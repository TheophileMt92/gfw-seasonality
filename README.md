# Fishing fleet seasonality

An interactive look at **when** different national fleets fish, and **where**, using
Global Fishing Watch's 2020 apparent fishing effort data aggregated by FAO major
fishing area.

**[Open the app](https://theophilemt92.github.io/gfw-seasonality/)** — runs entirely
in your browser, no server.

## What it shows

Pick an FAO major fishing area and a set of flag states. The chart gives weekly (or
cumulative) fishing hours per fleet across 2020, with the legend ordered by each
fleet's peak week. The map shows where that effort falls, month by month or
accumulating through the year.

The pattern that prompted this: in FAO Area 34 (Eastern Central Atlantic), the
distant-water fleets and the West African coastal fleets peak at opposite ends of
the year. Chinese and Taiwanese effort peaks around week 7, Moroccan effort in week
19, Spanish in week 28, Senegalese in week 35.

## Data

- **Fishing effort**: [Global Fishing Watch](https://globalfishingwatch.org/data-download/),
  daily fishing effort by MMSI at 0.1°, 2020 (CC BY 4.0). Apparent fishing effort
  inferred from AIS, so it under-represents vessels that do not carry or transmit AIS.
- **FAO major fishing areas**: FAO/Marine Regions boundary polygons.
- **Flag state**: derived from the first three digits of each MMSI (the Maritime
  Identification Digits). About 11% of global fishing hours in this dataset carry a
  prefix outside the assigned MID range and are excluded rather than guessed at.
  GFW's own vessel metadata resolves many of these; this project deliberately keeps
  the dependency list short instead.

## How it is built

The spatial join is a **build step, not a runtime step**. The prep scripts run once
locally over the ~2.2 GB of raw daily CSVs and write two small aggregated files that
the app ships with. This is what makes a serverless deployment possible: `sf` is not
available in WebAssembly, so the browser could never do the join itself.

```
prep/
  prep_gfw_seasonality.R   # daily CSVs -> effort by FAO area, flag, week + monthly grid
  prep_fao_boundaries.R    # FAO polygons -> simplified outlines as a plain data frame
  prep_land.R              # coastline + country labels as a plain data frame
app/
  app.R
  data/                    # the aggregated outputs (a few MB total)
docs/                      # shinylive build, served by GitHub Pages
```

To rebuild from scratch: download the GFW daily CSVs and the FAO polygons into
`raw/`, then run the three prep scripts from the project root, then
`shinylive::export("app", "docs")`.

## Stack

R, Shiny, plotly, leaflet, dplyr, ggplot2. Deployed as
[shinylive](https://posit-dev.github.io/r-shinylive/) — the app is compiled to
WebAssembly and runs client-side, so there is no server to pay for and no usage
limit. First load pulls down the R runtime and takes a few seconds.

## Caveats

- Apparent fishing effort is a model output, not observed fishing.
- AIS coverage varies by region and vessel size, so absolute comparisons between
  fleets or areas should be treated carefully. The seasonal *shape* within a fleet is
  more robust than the level.
- Flag state from MMSI is the vessel's registration, not necessarily beneficial
  ownership.

## Licence

Code MIT. Data remains under its original licences, Global Fishing Watch data under
CC BY 4.0.
