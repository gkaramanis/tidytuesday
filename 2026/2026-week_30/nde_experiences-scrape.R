# Near-Death Experience (NDE) data scraped from the NDERF Search site
# Source: https://search.nderf.org/
#
# The Near Death Experience Research Foundation (NDERF) collects firsthand NDE
# accounts submitted online. Their search site embeds structured JSON metadata
# for each experience page. This script extracts that metadata (no narrative
# text is reproduced, respecting NDERF's copyright).
#
# Each page lives at https://search.nderf.org/en/exp/<id> and embeds a JSON
# object in a <script> tag: `var exp = {...}`. Pages with no structured record
# contain `var exp = null`.
#
# ID range: valid records were found empirically up to ~33,369; probing
# 34,000-70,000 returned nothing, so the collection ceiling is ~34,000. We scan
# 1:35000 with a margin. Valid density is high in the low IDs (~half of pages
# below 10,100) and sparse above, but there is no site index/API, so every ID
# must be requested. Bump `max_id` if NDERF adds newer experiences.
#
# Note on completeness: many (especially older/legacy) records genuinely have
# no POSTDATE and sometimes no EXPDATE - `experiences[[1]]` can hold only
# {ENTRYNUM, greyson}. Those NAs are faithful to the source, not scrape errors.

library(httr2)
library(jsonlite)
library(stringr)
library(tibble)
library(purrr)
library(dplyr)
library(readr)

# --- Column schema ---
# Declared once so the CSV checkpoint round-trips with stable types (prevents
# readr from guessing an all-NA column as logical and clashing on append).
col_spec <- cols(
  entry_id         = col_integer(),
  gender           = col_character(),
  classification   = col_character(),
  country          = col_character(),
  category         = col_character(),
  language         = col_character(),
  greyson_score    = col_integer(),
  post_date        = col_character(),
  exp_date         = col_character(),
  narrative_length = col_integer(),
  ai_obe           = col_logical(),
  ai_unity         = col_logical(),
  ai_hellish       = col_logical(),
  ai_clinical      = col_logical(),
  ai_esp           = col_logical(),
  ai_past_lives    = col_logical(),
  ai_world_future  = col_logical(),
  ai_aliens        = col_logical()
)

# --- Scraping function ---
# Returns a one-row tibble for a valid record, or NULL for empty/failed pages.
# All types are enforced so bind_rows() never coerces across batches.
fetch_nderf_record <- function(id) {
  url <- paste0("https://search.nderf.org/en/exp/", id)

  resp <- tryCatch(
    request(url) |>
      req_retry(max_tries = 3, backoff = ~2) |>
      req_throttle(rate = 3 / 1) |>
      req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp)) return(NULL)

  html <- resp_body_string(resp)

  # The JSON sits on a single line: `var exp = {...};` (or `var exp = null;`).
  json_match <- str_match(html, 'var exp = (\\{.+?\\});\\s*\\n')
  if (is.na(json_match[1, 2])) return(NULL)

  d <- tryCatch(
    fromJSON(json_match[1, 2], simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(d)) return(NULL)
  if (is.null(d$GENDER)) return(NULL)

  tibble(
    entry_id         = as.integer(id),
    gender           = as.character(d$GENDER %||% NA),
    classification   = as.character(paste(d$CLASSIFICATION %||% "NA", collapse = ";")),
    country          = as.character(d$COUNTRY_AI %||% NA),
    category         = as.character(d$Category %||% NA),
    language         = as.character(d$LANGUAGE %||% NA),
    greyson_score    = as.integer(d$experiences[[1]]$greyson %||% NA),
    post_date        = as.character(d$experiences[[1]]$POSTDATE %||% NA),
    exp_date         = as.character(d$experiences[[1]]$EXPDATE %||% NA),
    narrative_length = as.integer(d$EXPLEN %||% NA),
    ai_obe           = as.logical(d$ai_categories$OBE_AI %||% NA),
    ai_unity         = as.logical(d$ai_categories$UNITY_AI %||% NA),
    ai_hellish       = as.logical(d$ai_categories$HELLISH_AI %||% NA),
    ai_clinical      = as.logical(d$ai_categories$CLINICAL_AI %||% NA),
    ai_esp           = as.logical(d$ai_categories$ESP_AI %||% NA),
    ai_past_lives    = as.logical(d$ai_categories$PASTLIVES_AI %||% NA),
    ai_world_future  = as.logical(d$ai_categories$WORLDFUTURE_AI %||% NA),
    ai_aliens        = as.logical(d$ai_categories$ALIENS_AI %||% NA)
  )
}

# --- Scrape in batches, appending to a CSV checkpoint so a crash can resume ---
# Save into a `data/` subdirectory next to this script. here::here() resolves
# from the repo root, so this works regardless of the working directory.
data_dir <- here::here("2026", "2026-week_30", "data")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
cache_path    <- file.path(data_dir, "nde_experiences.csv")
progress_path <- file.path(data_dir, "nde_progress.txt")

max_id     <- 35000
batch_size <- 250

# Valid records already saved: prevents re-fetching / duplicating them.
scraped_ids <- if (file.exists(cache_path)) {
  read_csv(cache_path, col_types = col_spec)$entry_id
} else {
  integer(0)
}

# Highest ID whose batch has been fully scanned (valid AND invalid). This is
# what makes resume correct: most IDs return no record and are never saved, so
# skipping on `scraped_ids` alone would re-probe every empty ID on each run.
done_through <- if (file.exists(progress_path)) {
  as.integer(readLines(progress_path, n = 1))
} else if (length(scraped_ids) > 0) {
  # Legacy checkpoint from before this file existed: assume whole batches up to
  # the last batch boundary at/below the highest saved id are done; the final
  # partial batch is re-scanned (its valid ids are skipped via scraped_ids, and
  # any duplicate rows are removed by the final distinct()).
  (max(scraped_ids) %/% batch_size) * batch_size
} else {
  0L
}

for (batch_start in seq(1, max_id, by = batch_size)) {
  batch_end <- min(batch_start + batch_size - 1, max_id)
  if (batch_end <= done_through) next          # whole batch already scanned

  batch_ids <- setdiff(batch_start:batch_end, scraped_ids)
  valid_results <- list()

  if (length(batch_ids) > 0) {
    batch_results <- map(batch_ids, \(id) {
      tryCatch(fetch_nderf_record(id), error = function(e) NULL)
    })
    valid_results <- compact(batch_results)

    if (length(valid_results) > 0) {
      new_batch_df <- bind_rows(valid_results)
      # append = TRUE skips the header once the file exists
      write_csv(new_batch_df, cache_path, append = file.exists(cache_path))
      scraped_ids <- c(scraped_ids, new_batch_df$entry_id)
    }
  }

  # Record progress only after the batch's writes have flushed, so a crash
  # re-does at most this one batch.
  done_through <- batch_end
  writeLines(as.character(done_through), progress_path)

  cat(sprintf("[%s] IDs %d-%d | %d valid | Total: %d\n",
              format(Sys.time(), "%H:%M:%S"),
              batch_start, batch_end,
              length(valid_results), length(scraped_ids)))
}

# --- Load, de-duplicate and order the final dataset ---
nde_experiences <- read_csv(cache_path, col_types = col_spec) |>
  distinct(entry_id, .keep_all = TRUE) |>
  arrange(entry_id)

write_csv(nde_experiences, cache_path)

cat("\nFinal dataset:", nrow(nde_experiences), "records x",
    ncol(nde_experiences), "columns\n")
