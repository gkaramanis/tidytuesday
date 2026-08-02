# popgrid_py.R
# Run the Python popgrid package (https://github.com/databites-tech/popgrid)
# from R and get the cells back as sf. Geometry only — no plotting.
#
# Geometry crosses the R/Python boundary as a GeoPackage on disk rather than
# as objects, which avoids converting between sf and geopandas in memory.
#
# NOTE: the Python package exposes no public accessor for the generated
# cells. This helper reads them out of the private PopGrid._landmasses
# attribute, which upstream may rename or remove without warning. If it
# breaks, that is the first place to look.

library(reticulate)
library(sf)

POPGRID_ENV <- "popgrid"

# ── Interpreter discovery ─────────────────────────────────────────────────
# popgrid requires Python >= 3.10. Very new releases are avoided because
# geopandas and shapely binary wheels lag behind them by months, and without
# a wheel pip falls back to building from source.
.popgrid_py_find <- function(min_minor = 10L, max_minor = 13L) {
  named <- paste0("python3.", max_minor:min_minor)
  cands <- unique(c(
    unname(Sys.which(named)),
    file.path("/opt/homebrew/bin", named),
    unname(Sys.which("python3"))
  ))
  cands <- cands[nzchar(cands) & file.exists(cands)]

  for (p in cands) {
    v <- tryCatch(
      system2(p, "-c 'import sys; print(sys.version_info.minor)'",
              stdout = TRUE, stderr = FALSE),
      error = function(e) character(0)
    )
    minor <- suppressWarnings(as.integer(v[1]))
    if (!is.na(minor) && minor >= min_minor && minor <= max_minor) return(p)
  }

  # uv, if present, manages prebuilt interpreters: seconds to fetch, versus
  # reticulate's pyenv fallback which compiles CPython from source.
  uv <- unname(Sys.which("uv"))
  if (nzchar(uv)) {
    uv_find <- function(ver) {
      p <- suppressWarnings(tryCatch(
        system2(uv, c("python", "find", ver), stdout = TRUE, stderr = FALSE),
        error = function(e) character(0)
      ))
      if (length(p) && nzchar(p[1]) && file.exists(p[1])) p[1] else NULL
    }

    for (ver in paste0("3.", max_minor:min_minor)) {
      p <- uv_find(ver)
      if (!is.null(p)) return(p)
    }

    message("Installing Python 3.12 via uv - first run only.")
    system2(uv, c("python", "install", "3.12"))
    p <- uv_find("3.12")
    if (!is.null(p)) return(p)
  }

  message("No Python 3.", min_minor, "-3.", max_minor,
          " found; building one from source (first run only, several minutes).")
  reticulate::install_python(version = "3.12")
}

# ── One-time environment setup ────────────────────────────────────────────
# Safe to re-run: does nothing unless the environment is missing.
popgrid_py_setup <- function(envname = POPGRID_ENV, python = NULL,
                             force = FALSE) {
  if (force || !virtualenv_exists(envname)) {
    if (is.null(python)) python <- .popgrid_py_find()
    message("Creating virtualenv '", envname, "' from ", python,
            " - first run only.")
    virtualenv_create(envname, python = python)
    virtualenv_install(envname, c(
      "geopandas",
      "matplotlib",   # required by popgrid's own imports
      "git+https://github.com/databites-tech/popgrid"
    ))
  }
  use_virtualenv(envname, required = TRUE)
  invisible(envname)
}

# ── Python side ───────────────────────────────────────────────────────────
.popgrid_py_code <- '
import geopandas as gpd
import pandas as pd
import matplotlib
matplotlib.use("Agg")   # popgrid imports matplotlib; keep it headless
from popgrid import PopGrid


def popgrid_build(in_path, out_path, region_col, weight_col, n,
                  cluster_distance_km):
    gdf = gpd.read_file(in_path)

    pg = PopGrid.from_geodataframe(
        gdf,
        region_col=region_col,
        weight_col=weight_col,
        n=int(n),
        cluster_distance_km=float(cluster_distance_km),
    )
    pg.build()

    frames = []
    for i, lm in enumerate(pg._landmasses):
        cells = lm.get("cells")
        if cells is None or len(cells) == 0:
            continue
        cells = cells.copy()
        cells["landmass_id"] = i
        frames.append(cells)

    if not frames:
        raise RuntimeError("popgrid produced no cells - try a larger n")

    out = gpd.GeoDataFrame(pd.concat(frames, ignore_index=True),
                           crs=frames[0].crs)
    out.to_file(out_path, driver="GPKG")
    return len(out)
'

# ── R wrapper ─────────────────────────────────────────────────────────────
#' Build a mosaic cartogram with the Python popgrid package.
#'
#' @param x           sf polygon data frame.
#' @param region_col  Name of the column identifying regions.
#' @param weight_col  Name of the numeric column giving each region's weight.
#' @param n           Target number of cells.
#' @param cluster_distance_km Landmass clustering threshold.
#'
#' @return sf data frame of square cells, with `region_name`, `landmass_id`
#'   and a projected (LAEA) CRS.
popgrid_py <- function(x,
                       region_col,
                       weight_col,
                       n                   = 1000,
                       cluster_distance_km = 200) {
  stopifnot(inherits(x, "sf"))
  for (col in c(region_col, weight_col)) {
    if (!col %in% names(x)) {
      stop("Column '", col, "' not found. Available: ",
           paste(setdiff(names(x), attr(x, "sf_column")), collapse = ", "))
    }
  }

  popgrid_py_setup()
  py_run_string(.popgrid_py_code)

  fin  <- tempfile(fileext = ".gpkg")
  fout <- tempfile(fileext = ".gpkg")
  on.exit(unlink(c(fin, fout)), add = TRUE)

  st_write(x, fin, quiet = TRUE)

  # as.integer() matters: a bare R number arrives in Python as a float, and
  # popgrid indexes with n.
  py$popgrid_build(
    in_path             = fin,
    out_path            = fout,
    region_col          = region_col,
    weight_col          = weight_col,
    n                   = as.integer(n),
    cluster_distance_km = as.numeric(cluster_distance_km)
  )

  st_read(fout, quiet = TRUE)
}
