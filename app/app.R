library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(leaflet)

seasonality  <- readRDS("data/seasonality.rds")
grid_monthly <- readRDS("data/grid_monthly.rds")
fao_bnd      <- readRDS("data/fao_boundaries.rds")

# Drop flags that could not be resolved from the MMSI prefix
seasonality  <- filter(seasonality,  !grepl("^MID ", flag))
grid_monthly <- filter(grid_monthly, !grepl("^MID ", flag))

# The pipeline aggregates to ISO week, so recover an approximate start date for
# each week. ISO week 1 of 2020 began on 30 December 2019.
seasonality$date <- as.Date("2019-12-30") + (seasonality$week - 1) * 7

fao_names <- c(
  "18" = "18 - Arctic Sea",            "21" = "21 - Atlantic, Northwest",
  "27" = "27 - Atlantic, Northeast",   "31" = "31 - Atlantic, W Central",
  "34" = "34 - Atlantic, E Central",   "37" = "37 - Mediterranean & Black Sea",
  "41" = "41 - Atlantic, Southwest",   "47" = "47 - Atlantic, Southeast",
  "48" = "48 - Atlantic, Antarctic",   "51" = "51 - Indian Ocean, Western",
  "57" = "57 - Indian Ocean, Eastern", "58" = "58 - Indian Ocean, Antarctic",
  "61" = "61 - Pacific, Northwest",    "67" = "67 - Pacific, Northeast",
  "71" = "71 - Pacific, W Central",    "77" = "77 - Pacific, E Central",
  "81" = "81 - Pacific, Southwest",    "87" = "87 - Pacific, Southeast",
  "88" = "88 - Pacific, Antarctic"
)

areas <- sort(unique(seasonality$fao_area))
lbl <- fao_names[as.character(areas)]
lbl[is.na(lbl)] <- paste("Area", areas[is.na(lbl)])
area_choices <- setNames(areas, lbl)

month_names <- c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")

palette8 <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A",
              "#66A61E", "#E6AB02", "#A6761D", "#666666")

ui <- page_sidebar(
  title = "Who fishes where, and when: global fishing effort in 2020",
  theme = bs_theme(version = 5, preset = "flatly"),
  sidebar = sidebar(
    width = 320,
    selectInput("area", "FAO major fishing area",
                choices = area_choices, selected = 34),
    selectizeInput("flags", "Fleets (annual fishing hours)",
                   choices = NULL, multiple = TRUE,
                   options = list(placeholder = "Select flag states")),
    radioButtons("view", "Effort shown as",
                 choices = c("Weekly" = "weekly", "Cumulative" = "cumulative"),
                 selected = "weekly", inline = TRUE),
    sliderInput("month", "Month mapped", min = 1, max = 12, value = 1, step = 1,
                animate = animationOptions(interval = 900, loop = TRUE),
                ticks = FALSE),
    hr(),
    p(class = "text-muted small",
      "Apparent fishing effort from AIS, Global Fishing Watch (2020), ",
      "0.1 degree daily grid. Flag state is derived from the MMSI prefix, so ",
      "vessels with unresolved prefixes are excluded. Fleets under 100 hours ",
      "a year in an area are not listed.")
  ),
  card(
    card_header(textOutput("season_title", inline = TRUE)),
    plotlyOutput("season_plot", height = "340px")
  ),
  card(
    card_header(textOutput("map_title", inline = TRUE)),
    leafletOutput("map", height = "420px")
  )
)

server <- function(input, output, session) {

  area_label <- reactive({
    l <- names(area_choices)[area_choices == input$area]
    if (length(l)) l else paste("Area", input$area)
  })

  area_season <- reactive({
    filter(seasonality, fao_area == as.integer(input$area))
  })

  # Repopulate the fleet selector when the area changes, ranked by effort
  observeEvent(input$area, {
    ranked <- area_season() |>
      group_by(flag) |>
      summarise(h = sum(fishing_hours), .groups = "drop") |>
      filter(h >= 100) |>
      arrange(desc(h))

    choices <- setNames(
      ranked$flag,
      paste0(ranked$flag, "  (", format(round(ranked$h), big.mark = ","), " h)")
    )
    updateSelectizeInput(session, "flags", choices = choices,
                         selected = head(ranked$flag, 6), server = TRUE)
  })

  output$season_title <- renderText({
    paste0("Weekly fishing effort by fleet: ", area_label())
  })

  output$season_plot <- renderPlotly({
    req(input$flags)
    w <- area_season() |>
      filter(flag %in% input$flags) |>
      group_by(flag, week, date) |>
      summarise(hours = sum(fishing_hours), .groups = "drop") |>
      arrange(flag, week)
    validate(need(nrow(w) > 0, "No effort recorded for this selection."))

    # Order the legend by peak week so the seasonal sequence reads in order
    ord <- w |>
      group_by(flag) |>
      summarise(peak = week[which.max(hours)], .groups = "drop") |>
      arrange(peak) |>
      pull(flag)
    w$flag <- factor(w$flag, levels = ord)

    # Cumulative view: running total per fleet. Legend order is still set by
    # the weekly peak above, computed before the transform.
    cumulative <- identical(input$view, "cumulative")
    if (cumulative) {
      w <- w |> arrange(flag, week) |> group_by(flag) |>
        mutate(hours = cumsum(hours)) |> ungroup()
    }

    y_lab <- if (cumulative) "Cumulative fishing hours" else "Fishing hours"
    w$tip <- paste0("<b>", w$flag, "</b><br>Week of ",
                    format(w$date, "%d %B"), "<br>",
                    format(round(w$hours), big.mark = ","), " fishing hours",
                    if (cumulative) " to date" else "")

    plot_ly(w, x = ~date, y = ~hours, color = ~flag,
            colors = palette8[seq_len(nlevels(w$flag))],
            type = "scatter", mode = "lines",
            line = list(width = 2),
            text = ~tip, hoverinfo = "text") |>
      layout(
        xaxis = list(title = "2020", type = "date",
                     tickformat = "%b", dtick = "M1", zeroline = FALSE),
        yaxis = list(title = y_lab, rangemode = "tozero"),
        hovermode = "closest",
        legend = list(orientation = "h", x = 0, y = -0.18),
        margin = list(t = 20, r = 10)
      ) |>
      config(displayModeBar = FALSE)
  })

  output$map_title <- renderText({
    n <- length(input$flags)
    paste0("Fishing effort in ", month_names[input$month], " 2020: ",
           area_label(),
           if (n) paste0(" (", n, " fleet", if (n > 1) "s" else "", ")") else "")
  })

  # Base map: outline the selected FAO area and zoom to its full extent
  output$map <- renderLeaflet({
    b <- filter(fao_bnd, fao_area == as.integer(input$area))
    m <- leaflet() |>
      addProviderTiles("Esri.OceanBasemap") |>
      fitBounds(min(b$lon), min(b$lat), max(b$lon), max(b$lat))
    for (r in unique(b$ring)) {
      rr <- b[b$ring == r, ]
      m <- addPolylines(m, lng = rr$lon, lat = rr$lat,
                        color = "#1f4e5f", weight = 1.5, opacity = 0.9,
                        fill = FALSE)
    }
    m
  })

  # Effort layer, redrawn on month or fleet change
  observe({
    req(input$flags)
    g <- grid_monthly |>
      filter(fao_area == as.integer(input$area),
             month == input$month,
             flag %in% input$flags) |>
      group_by(lat, lon) |>
      summarise(hours = sum(fishing_hours), .groups = "drop")

    proxy <- leafletProxy("map") |> clearGroup("effort")
    if (!nrow(g)) return(invisible(NULL))

    pal <- colorNumeric("YlOrRd", domain = log10(g$hours + 1))
    proxy |>
      addRectangles(
        lng1 = g$lon, lat1 = g$lat,
        lng2 = g$lon + 0.5, lat2 = g$lat + 0.5,
        fillColor = pal(log10(g$hours + 1)),
        fillOpacity = 0.8, weight = 0,
        group = "effort",
        label = paste0(round(g$hours), " fishing hours")
      )
  })
}

shinyApp(ui, server)
