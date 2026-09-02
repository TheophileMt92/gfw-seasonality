library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(ggplot2)
library(leaflet)

seasonality  <- readRDS("data/seasonality.rds")
grid_monthly <- readRDS("data/grid_monthly.rds")
fao_bnd      <- readRDS("data/fao_boundaries.rds")
land         <- readRDS("data/land.rds")

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
    div(class = "d-flex gap-2 mb-2",
        actionButton("all_flags", "Select all", class = "btn-sm btn-outline-secondary flex-fill"),
        actionButton("top_flags", "Top 6", class = "btn-sm btn-outline-secondary flex-fill"),
        actionButton("no_flags", "Clear", class = "btn-sm btn-outline-secondary flex-fill")),
    p(class = "text-muted small mb-3",
      "Both panels below use this area and these fleets."),

    hr(class = "my-2"),
    tags$strong(class = "d-block mb-2 text-uppercase small", "Chart"),
    radioButtons("view", "Effort shown as",
                 choices = c("Weekly" = "weekly", "Cumulative" = "cumulative"),
                 selected = "weekly", inline = TRUE),
    p(class = "text-muted small mb-0",
      "Use the camera icon on the chart to save it as PNG."),

    hr(class = "my-2"),
    tags$strong(class = "d-block mb-2 text-uppercase small", "Map"),
    sliderInput("month", "Month mapped", min = 1, max = 12, value = 1, step = 1,
                animate = animationOptions(interval = 900, loop = TRUE),
                ticks = FALSE),
    radioButtons("map_view", "Effort shown as",
                 choices = c("That month" = "single",
                             "Cumulative to date" = "cumulative"),
                 selected = "single"),
    checkboxInput("labels", "Country names on downloaded map", TRUE),
    downloadButton("dl_map", "Download map (PNG)", class = "btn-sm w-100"),

    hr(class = "my-2"),
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

  # Fleets present in the selected area, ranked by annual effort
  ranked_flags <- reactive({
    area_season() |>
      group_by(flag) |>
      summarise(h = sum(fishing_hours), .groups = "drop") |>
      filter(h >= 100) |>
      arrange(desc(h))
  })

  flag_choices <- reactive({
    r <- ranked_flags()
    setNames(r$flag,
             paste0(r$flag, "  (", format(round(r$h), big.mark = ","), " h)"))
  })

  # Repopulate the fleet selector when the area changes
  observeEvent(input$area, {
    updateSelectizeInput(session, "flags", choices = flag_choices(),
                         selected = head(ranked_flags()$flag, 6), server = TRUE)
  })

  observeEvent(input$all_flags, {
    updateSelectizeInput(session, "flags", choices = flag_choices(),
                         selected = ranked_flags()$flag, server = TRUE)
  })

  observeEvent(input$top_flags, {
    updateSelectizeInput(session, "flags", choices = flag_choices(),
                         selected = head(ranked_flags()$flag, 6), server = TRUE)
  })

  observeEvent(input$no_flags, {
    updateSelectizeInput(session, "flags", choices = flag_choices(),
                         selected = character(0), server = TRUE)
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

    n <- nlevels(w$flag)
    cols <- if (n <= 8) palette8[seq_len(n)] else
      grDevices::colorRampPalette(palette8)(n)

    plot_ly(w, x = ~date, y = ~hours, color = ~flag,
            colors = cols,
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
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d",
                                        "hoverClosestCartesian",
                                        "hoverCompareCartesian"),
             toImageButtonOptions = list(format = "png", scale = 2,
                                         filename = "fishing_effort_chart"))
  })

  output$map_title <- renderText({
    n <- length(input$flags)
    period <- if (identical(input$map_view, "cumulative")) {
      if (input$month == 1) "January 2020"
      else paste0("January to ", month_names[input$month], " 2020")
    } else {
      paste0(month_names[input$month], " 2020")
    }
    paste0("Fishing effort, ", period, ": ", area_label(),
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

  mapped_grid <- reactive({
    req(input$flags)
    months_shown <- if (identical(input$map_view, "cumulative")) {
      seq_len(input$month)
    } else {
      input$month
    }
    cells <- grid_monthly |>
      filter(fao_area == as.integer(input$area),
             month %in% months_shown,
             flag %in% input$flags) |>
      group_by(lat, lon, flag) |>
      summarise(hours = sum(fishing_hours), .groups = "drop")

    # Per-cell total, plus a breakdown of the top fleets in that cell
    breakdown <- cells |>
      arrange(lat, lon, desc(hours)) |>
      group_by(lat, lon) |>
      summarise(
        hours = sum(hours),
        detail = paste0(
          paste0(head(flag, 5), ": ",
                 format(round(head(hours, 5)), big.mark = ","), " h",
                 collapse = "<br>"),
          if (n() > 5) paste0("<br>+ ", n() - 5, " more") else ""
        ),
        .groups = "drop"
      )
    breakdown
  })

  # Effort layer, redrawn on month or fleet change
  observe({
    g <- mapped_grid()
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
        label = lapply(
          paste0("<b>", format(round(g$hours), big.mark = ","),
                 " fishing hours</b><br>", g$detail),
          htmltools::HTML
        ),
        labelOptions = labelOptions(textsize = "12px", direction = "auto")
      )
  })
  # Static map export: ggplot cropped to the FAO area bounding box
  output$dl_map <- downloadHandler(
    filename = function() {
      paste0("fishing_effort_area", input$area, "_",
             if (identical(input$map_view, "cumulative")) "jan_to_" else "",
             tolower(month_names[input$month]), "_2020.png")
    },
    content = function(file) {
      g <- mapped_grid()
      b <- filter(fao_bnd, fao_area == as.integer(input$area))
      xr <- range(b$lon); yr <- range(b$lat)

      lnd <- filter(land, lon >= xr[1] - 5, lon <= xr[2] + 5,
                          lat >= yr[1] - 5, lat <= yr[2] + 5)

      # Label only the landmasses actually in view, ranked by how much of each
      # is visible, so the map is not swamped by small or distant countries.
      lab <- lnd |>
        filter(lon >= xr[1], lon <= xr[2], lat >= yr[1], lat <= yr[2]) |>
        group_by(country) |>
        summarise(lon = mean(lon), lat = mean(lat), n = n(), .groups = "drop") |>
        filter(n >= 8) |>
        arrange(desc(n)) |>
        head(12)

      period <- if (identical(input$map_view, "cumulative")) {
        if (input$month == 1) "January 2020"
        else paste0("January to ", month_names[input$month], " 2020")
      } else {
        paste0(month_names[input$month], " 2020")
      }

      p <- ggplot() +
        geom_tile(data = g, aes(lon + 0.25, lat + 0.25, fill = log10(hours + 1)),
                  width = 0.5, height = 0.5) +
        geom_polygon(data = lnd, aes(lon, lat, group = group),
                     fill = "#2a2f36", colour = "#6b757e", linewidth = 0.25) +
        {if (isTRUE(input$labels))
          geom_text(data = lab, aes(lon, lat, label = country),
                    colour = "grey72", size = 2.5, alpha = 0.9,
                    check_overlap = TRUE)
         else NULL} +
        scale_fill_gradientn(
          colours = c("#0b0724", "#3b0f70", "#8c2981", "#de4968",
                      "#fe9f6d", "#fcfdbf"),
          breaks = 0:4,
          labels = c("1", "10", "100", "1,000", "10,000"),
          name = "Fishing hours") +
        coord_fixed(xlim = xr, ylim = yr, expand = FALSE) +
        labs(title = paste0("Fishing effort, ", period),
             subtitle = area_label(),
             caption = "Apparent fishing effort from AIS, Global Fishing Watch (2020)",
             x = NULL, y = NULL) +
        theme_minimal(base_size = 13) +
        theme(panel.background = element_rect(fill = "#0b0724", colour = NA),
              panel.grid = element_blank(),
              plot.caption = element_text(colour = "grey40", size = 8))

      ggsave(file, p, width = 10, height = 7, dpi = 150, bg = "white")
    }
  )
}

shinyApp(ui, server)
