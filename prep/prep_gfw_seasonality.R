# Fleet seasonality from GFW daily fishing-effort CSVs.
# Aggregates fishing hours by FAO major fishing area, flag state and week,
# plus a coarse monthly grid for mapping.
# Run once locally, then build the app / figures from the two output files.

library(data.table)
library(sf)
library(arrow)

sf_use_s2(FALSE)   # 2005 FAO polygons have self-intersections that s2 rejects

daily_dir   <- "raw/mmsi-daily-csvs-10-v2-2020"
vessels_csv <- "raw/fishing-vessels-v2.csv"
fao_shp     <- "raw/fao/World_Fao_Zones.shp"
out_dir     <- "app/data"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- vessel lookup -------------------------------------------------------
v <- fread(vessels_csv)
# Column names have changed between GFW releases: check names(v) if this fails.
setnames(v,
  old = intersect(c("flag_gfw", "vessel_class_gfw"), names(v)),
  new = c("flag", "gear")[seq_along(intersect(c("flag_gfw", "vessel_class_gfw"), names(v)))])
lookup <- unique(v[, .(mmsi, flag, gear)])

# ---- pass over the daily files ------------------------------------------
files <- list.files(daily_dir, pattern = "\\.csv$", full.names = TRUE)
cat(length(files), "daily files\n")

acc <- vector("list", length(files))

for (i in seq_along(files)) {
  d <- fread(files[i], showProgress = FALSE)
  d <- d[fishing_hours > 0]
  if (!nrow(d)) next
  d <- lookup[d, on = "mmsi"]
  d[is.na(flag), flag := "UNK"]
  d[, `:=`(week  = as.integer(format(as.Date(date), "%V")),
           month = as.integer(format(as.Date(date), "%m")))]
  acc[[i]] <- d[, .(fishing_hours = sum(fishing_hours),
                    vessels = uniqueN(mmsi)),
                by = .(cell_ll_lat, cell_ll_lon, flag, gear, week, month)]
  if (i %% 25 == 0) cat("  ", i, "files\n")
}

eff <- rbindlist(acc, use.names = TRUE)
rm(acc); gc()
cat("aggregated rows:", nrow(eff), "\n")

# ---- assign FAO major fishing area to each cell -------------------------
# This shapefile holds the 19 major areas in a single `zone` field:
# no F_LEVEL column, so no subareas to filter out.
cells <- unique(eff[, .(cell_ll_lat, cell_ll_lon)])
pts <- st_as_sf(cells,
                coords = c("cell_ll_lon", "cell_ll_lat"),
                crs = 4326, remove = FALSE)

fao <- st_make_valid(st_read(fao_shp, quiet = TRUE))

# Cells on a shared boundary match two polygons, so deduplicate before joining.
hits <- as.data.table(st_join(pts, fao["zone"], join = st_intersects))
hits <- unique(hits, by = c("cell_ll_lat", "cell_ll_lon"))
cells <- hits[cells, on = c("cell_ll_lat", "cell_ll_lon")]
setnames(cells, "zone", "fao_area")
cells[, geometry := NULL]

# Cells just outside the 2005 polygons (coastlines, enclosed waters) fall back
# to the nearest area rather than being dropped.
na_idx <- which(is.na(cells$fao_area))
cat("cells with no direct match:", length(na_idx),
    sprintf("(%.1f%%)\n", 100 * length(na_idx) / nrow(cells)))
if (length(na_idx)) {
  near <- st_nearest_feature(pts[na_idx, ], fao)
  cells[na_idx, fao_area := fao$zone[near]]
}

eff <- cells[eff, on = c("cell_ll_lat", "cell_ll_lon")]
eff <- eff[!is.na(fao_area)]

# ---- output 1: weekly seasonality by FAO area and flag ------------------
seasonality <- eff[, .(fishing_hours = round(sum(fishing_hours), 1),
                       vessels = sum(vessels)),
                   by = .(fao_area, flag, gear, week)]
write_parquet(seasonality, file.path(out_dir, "seasonality.parquet"),
              compression = "zstd")

# ---- output 2: coarse monthly grid for mapping --------------------------
grid <- eff[, .(fishing_hours = round(sum(fishing_hours), 1)),
            by = .(lat = floor(cell_ll_lat * 2) / 2,
                   lon = floor(cell_ll_lon * 2) / 2,
                   fao_area, flag, month)]
write_parquet(grid, file.path(out_dir, "grid_monthly.parquet"),
              compression = "zstd")

cat("seasonality:", nrow(seasonality), "rows,",
    round(file.size(file.path(out_dir, "seasonality.parquet")) / 1e6, 1), "MB\n")
cat("grid:", nrow(grid), "rows,",
    round(file.size(file.path(out_dir, "grid_monthly.parquet")) / 1e6, 1), "MB\n")
