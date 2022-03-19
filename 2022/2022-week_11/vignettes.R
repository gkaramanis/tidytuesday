library(tidyverse)
library(camcorder)
library(lubridate)
library(textreadr)
library(shadowtext)
library(ggshadow)

gg_record(dir = "temp", device = "png", width = 12, height = 10, units = "in", dpi = 320)

# My packages
# get full paths of scripts and year
full_paths <- data.frame(
  path = dir(path = here::here(), pattern = "\\.R", recursive = TRUE)
) %>% 
  mutate(
    document = str_remove(base_name(path), ".R"),
    year = parse_number(path)
  )

used_packages <- read_dir(path = here::here(), pattern = "\\.R", recursive = TRUE) %>%
  left_join(full_paths) %>% 
  filter(str_detect(content, "library|::")) %>% 
  mutate(
    package = case_when(
      str_detect(content, "library") ~ str_remove(str_remove(content, "library\\("), "\\)"),
      str_detect(content, "::") ~ str_extract(content, "\\w+(?=::)")
    )
  ) %>% 
  distinct(package) %>% 
  filter(str_detect(package, "#|'", negate = TRUE)) %>% 
  mutate(used = TRUE)


cran <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/cran.csv')

cran_updates <- cran %>% 
  mutate(version_dot = str_replace_all(version, "-", ".")) %>% 
  filter(date != 0) %>% 
  mutate(
    date_ymd_utc = parse_date(date, format = c("%Y-%m-%d %H:%M:%S UTC")),
    date_ymd_cdt = parse_date(date, format = c("%Y-%m-%d %H:%M:%OS CDT")),
    date_dmy = parse_date(date, format = c("%a %b %d %H:%M:%S %Y")),
    date_ymd = parse_date(date, format = c("%Y-%m-%d"))
  ) %>% 
  pivot_longer(starts_with("date_"), values_to = "date_short") %>%
  # remove one package ("PIN") that has date in 1987:
  filter(date_short > "2000-12-31") %>% 
  group_by(package) %>% 
  arrange(date_short) %>% 
  mutate(package_idx = cur_group_id()) %>% 
  arrange(package_idx) %>% 
  mutate(
    version_idx = row_number()
  ) %>% 
  ungroup() %>% 
  left_join(used_packages)

package_labels <- cran_updates %>% 
  filter(used) %>% 
  group_by(package) %>% 
  slice_max(order_by = version_idx, n = 1)

f1 <- "Porpora"
bg_col <- "#DAE7EE"
col1 <- "#712D1A"
col2 <- "#2D699C"

ggplot() +
  # Lines - Not used packages
  geom_line(data = cran_updates %>% filter(is.na(used)), aes(x = date_short, y = version_idx, group = package), color = col2, size = 0.1, alpha = 0.5) +
  # Lines - Used packages 
  geom_shadowline(data = cran_updates %>% filter(used), aes(x = date_short, y = version_idx, group = package), color = col1, size = 0.5, alpha = 0.7, shadowsize = 0.8) +
  geom_point(data = package_labels, aes(x = date_short, y = version_idx), size = 1, color = col1) +
  geom_shadowtext(data = package_labels, aes(x = date_short, y = version_idx, label = package, size = version_idx), hjust = 0, vjust = 0.2, nudge_x = 70, check_overlap = TRUE, bg.color = bg_col, color = col1, bg.r = 0.08, family = f1) +
  # Title and caption
  annotate("text", x = as.Date("2000-10-01"), y = 193, label = "Number of versions of packages in CRAN", hjust = 0, size = 7, family = f1) +
  annotate("text", x = as.Date("2000-10-01"), y = 187, label = "Highlighted are the packages that I have used at least once in a script\nSource: Robert Flight · Graphic: Georgios Karamanis", hjust = 0, vjust = 1, size = 4.5, family = f1, lineheight = 1) +
  scale_x_date(limits = c(as.Date("2000-10-01"), as.Date("2021-11-30")), date_breaks = "2 years", date_labels = "%Y") +
  scale_size_continuous(range = c(2.5, 6)) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_col, color = NA),
    plot.margin = margin(20, 40, 20, 20),
    axis.title = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    panel.grid = element_line(color = "grey97")
  )
  