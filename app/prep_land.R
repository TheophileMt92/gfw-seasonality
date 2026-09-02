# Coarse world coastline for the static map export, stored as a plain data
# frame so the app needs neither sf nor maps. Each vertex carries its country,
# so the app can label whatever falls inside the current view.
# Run once locally from the project root.

land <- maps::map("world", plot = FALSE, fill = TRUE)

seg <- cumsum(is.na(land$x))
df <- data.frame(
  lon     = land$x,
  lat     = land$y,
  group   = seg,
  country = sub(":.*$", "", land$names)[seg + 1]   # drop island suffixes
)
df <- df[!is.na(df$lon), ]
df <- df[seq(1, nrow(df), by = 2), ]   # thin; plenty for a regional map

saveRDS(df, "app/data/land.rds", compress = "xz")
cat("land:", nrow(df), "vertices,",
    length(unique(df$country)), "countries,",
    round(file.size("app/data/land.rds") / 1e6, 2), "MB\n")
