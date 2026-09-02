# Fleet seasonality from GFW daily fishing-effort CSVs.
# Aggregates fishing hours by FAO major fishing area, flag state and week,
# plus a coarse monthly grid for mapping.
#
# No vessel metadata file needed: the flag state is derived from the first
# three digits of the MMSI (the Maritime Identification Digits, or MID).
#
# Run once locally from the project root.

library(data.table)
library(sf)
library(arrow)

sf_use_s2(FALSE)   # 2005 FAO polygons have self-intersections that s2 rejects

daily_dir <- "raw/mmsi-daily-csvs-10-v2-2020"
fao_shp   <- "raw/fao/World_Fao_Zones.shp"
out_dir   <- "app/data"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- MID -> flag state ---------------------------------------------------
# Covers the major fishing nations. Any MID not listed keeps its raw code,
# so nothing is silently lost.
mid_map <- data.table(
  mid = c("412","413","414","416","431","432","440","441","224","225",
          "226","227","228","247","263","232","233","234","235","257",
          "258","259","273","251","219","220","244","245","246","211",
          "218","250","261","265","266","760","725","701","735","345",
          "338","366","367","368","369","316","525","548","574","567",
          "533","419","242","663","654","601","659","627","271","237",
          "239","240","241","238","272","351","352","353","354","355",
          "356","357","370","371","372","373","312","576","577","664",
          "645","461","512","503","710","770","740","276","275","277",
          "231","518","529","520","553","557","510","538","306","667",
          "613","619","632","630","603","674","677","634","647","650",
          "417","405","463","422","622","672","642","605"),
  flag = c("China","China","China","Taiwan","Japan","Japan","South Korea","South Korea","Spain","Spain",
           "France","France","France","Italy","Portugal","UK","UK","UK","UK","Norway",
           "Norway","Norway","Russia","Iceland","Denmark","Denmark","Netherlands","Netherlands","Netherlands","Germany",
           "Germany","Ireland","Poland","Sweden","Sweden","Peru","Chile","Argentina","Ecuador","Mexico",
           "USA","USA","USA","USA","USA","Canada","Indonesia","Philippines","Vietnam","Thailand",
           "Malaysia","India","Morocco","Senegal","Mauritania","South Africa","Namibia","Ghana","Turkey","Greece",
           "Greece","Greece","Greece","Croatia","Ukraine","Panama","Panama","Panama","Panama","Panama",
           "Panama","Panama","Panama","Panama","Panama","Panama","Belize","Vanuatu","Vanuatu","Seychelles",
           "Mauritius","Oman","New Zealand","Australia","Brazil","Uruguay","Falkland Is.","Estonia","Latvia","Lithuania",
           "Faroe Is.","Cook Is.","Kiribati","Fiji","Papua New Guinea","Solomon Is.","Micronesia","Marshall Is.","Curacao","Sierra Leone",
           "Cameroon","Cote d'Ivoire","Guinea","Guinea-Bissau","Angola","Tanzania","Tanzania","Kenya","Madagascar","Mozambique",
           "Sri Lanka","Bangladesh","Pakistan","Iran","Egypt","Tunisia","Libya","Algeria")
)

# ---- pass over the daily files ------------------------------------------
files <- list.files(daily_dir, pattern = "\\.csv$", full.names = TRUE)
cat(length(files), "daily files\n")

acc <- vector("list", length(files))

for (i in seq_along(files)) {
  d <- fread(files[i], showProgress = FALSE)
  d <- d[fishing_hours > 0]
  if (!nrow(d)) next
  d[, mid := substr(as.character(mmsi), 1, 3)]
  d[, `:=`(week  = as.integer(format(as.Date(date), "%V")),
           month = as.integer(format(as.Date(date), "%m")))]
  acc[[i]] <- d[, .(fishing_hours = sum(fishing_hours),
                    vessels = uniqueN(mmsi)),
                by = .(cell_ll_lat, cell_ll_lon, mid, week, month)]
  if (i %% 25 == 0) cat("  ", i, "files\n")
}

eff <- rbindlist(acc, use.names = TRUE)
rm(acc); gc()
cat("aggregated rows:", nrow(eff), "\n")

# attach flag names; unmatched MIDs keep their code
eff <- mid_map[eff, on = "mid"]
eff[is.na(flag), flag := paste0("MID ", mid)]

# ---- assign FAO major fishing area to each cell -------------------------
# This shapefile holds the 19 major areas in a single `zone` field.
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

# Cells just outside the 2005 polygons fall back to the nearest area.
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
                   by = .(fao_area, flag, week)]

# ---- output 2: coarse monthly grid for mapping --------------------------
# This file ships to the browser, so it has to stay small: 1 degree cells,
# and only the top 12 flags per area kept separately, the rest bucketed.
top_by_area <- eff[, .(h = sum(fishing_hours)), by = .(fao_area, flag)][
  order(fao_area, -h), head(.SD, 12), by = fao_area]

keep <- paste(top_by_area$fao_area, top_by_area$flag)
eff[, flag_map := fifelse(paste(fao_area, flag) %in% keep, flag, "Other")]

grid <- eff[, .(fishing_hours = round(sum(fishing_hours), 1)),
            by = .(lat = floor(cell_ll_lat), lon = floor(cell_ll_lon),
                   fao_area, flag = flag_map, month)]

# ---- write both as RDS (webR reads these without the arrow package) -----
saveRDS(as.data.frame(seasonality), file.path(out_dir, "seasonality.rds"),
        compress = "xz")
saveRDS(as.data.frame(grid), file.path(out_dir, "grid_monthly.rds"),
        compress = "xz")

cat("seasonality:", nrow(seasonality), "rows,",
    round(file.size(file.path(out_dir, "seasonality.rds")) / 1e6, 2), "MB\n")
cat("grid:", nrow(grid), "rows,",
    round(file.size(file.path(out_dir, "grid_monthly.rds")) / 1e6, 2), "MB\n")
cat("Aim for the grid under ~150,000 rows. If it is larger, change\n",
    "floor(cell_ll_lat) to floor(cell_ll_lat / 2) * 2 for 2 degree cells.\n")
