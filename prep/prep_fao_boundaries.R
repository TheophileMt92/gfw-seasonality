# Simplify the FAO major fishing area polygons into a small file the app can
# draw without sf. Run once locally from the project root.

library(sf)

fao_shp  <- "raw/fao/World_Fao_Zones.shp"
out_file <- "app/data/fao_boundaries.rds"

sf_use_s2(FALSE)   # 2005 polygons have self-intersections that s2 rejects

fao <- st_make_valid(st_read(fao_shp, quiet = TRUE))
fao <- st_cast(st_simplify(fao, dTolerance = 0.25, preserveTopology = TRUE),
               "MULTIPOLYGON")

co <- st_coordinates(fao)

# L1 = ring, L2 = polygon, L3 = feature. Combine the first three into a ring id
# so each closed outline is drawn separately, and map L3 back to the area code.
bnd <- data.frame(
  lon      = as.numeric(co[, "X"]),
  lat      = as.numeric(co[, "Y"]),
  ring     = paste(co[, "L1"], co[, "L2"], co[, "L3"], sep = "-"),
  fao_area = as.integer(as.character(fao$zone))[co[, "L3"]]
)

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
saveRDS(bnd, out_file, compress = "xz")

cat(nrow(bnd), "vertices across", length(unique(bnd$ring)), "rings,",
    round(file.size(out_file) / 1e6, 2), "MB\n")
cat("If that is over ~1 MB, raise dTolerance to 0.5 and rerun.\n")
