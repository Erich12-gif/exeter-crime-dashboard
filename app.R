library(shiny)
library(tidyverse)
library(jsonlite)
library(leaflet)
library(htmltools)

# --- Local crime summary CSV ---------------------------------
Exeter_stats.csv <- read.csv("Exeter_stats.csv")

crime <- Exeter_stats.csv |>
  rename(crime_type = Crime.type, total = Total, pct = Percentage) |>
  mutate(
    pct = as.numeric(gsub("%", "", pct)) / 100,
    crime_type = as.factor(crime_type)
  )

# --- Live crime data for maps --------------------------------
exeter_raw <- fromJSON(
  "https://data.police.uk/api/crimes-street/all-crime?lat=50.726&lng=-3.527"
)

crime_map <- exeter_raw |>
  unnest_wider(location) |>
  unnest_wider(street, names_sep = "_") |>
  select(
    category,
    street_name,
    lat = latitude,
    lon = longitude
  ) |>
  drop_na(lat, lon) |>
  mutate(
    lat = as.numeric(lat),
    lon = as.numeric(lon),
    category = as.factor(category)
  )

crime_types <- sort(unique(crime_map$category))

pal <- colorFactor(
  palette = c(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
  ),
  domain = levels(crime_map$category)
)

popup <- ~paste(
  "<b>Crime:</b>", category, "<br>",
  "<b>Street:</b>", street_name
)

# --- Auto-updating 12-month trend data -----------------------

bbox <- list(
  lat_min = 50.70,
  lat_max = 50.76,
  lon_min = -3.55,
  lon_max = -3.48
)

fetch_month <- function(date_str) {
  url <- paste0(
    "https://data.police.uk/api/crimes-street/all-crime?date=",
    date_str,
    "&lat=",
    bbox$lat_min,
    "&lng=",
    bbox$lon_min
  )
  
  dat <- tryCatch(fromJSON(url), error = function(e) NULL)
  
  if (is.null(dat)) return(0)
  if (length(dat) == 0) return(0)
  
  nrow(dat)
}

today <- Sys.Date()
last_month <- seq(as.Date(format(today, "%Y-%m-01")), length.out = 2, by = "-1 month")[2]

months_seq <- seq(from = last_month, length.out = 12, by = "-1 month")
months_str <- format(months_seq, "%Y-%m")

monthly_counts <- sapply(months_str, fetch_month)

monthly_data <- data.frame(
  month = as.Date(paste0(months_str, "-01")),
  count = monthly_counts
) |>
  arrange(month)

# --- GRID + HOTSPOTS + LEADERBOARD HELPERS -------------------

make_grid <- function(dat, cell_size) {
  lat_seq <- seq(min(dat$lat), max(dat$lat), by = cell_size)
  lon_seq <- seq(min(dat$lon), max(dat$lon), by = cell_size)
  
  grid <- expand.grid(
    lat_min = lat_seq[-length(lat_seq)],
    lon_min = lon_seq[-length(lon_seq)]
  )
  
  grid <- grid |>
    mutate(
      lat_max = lat_min + cell_size,
      lon_max = lon_min + cell_size
    )
  
  grid$count <- apply(grid, 1, function(row) {
    sum(
      dat$lat >= row["lat_min"] &
        dat$lat <  row["lat_max"] &
        dat$lon >= row["lon_min"] &
        dat$lon <  row["lon_max"]
    )
  })
  
  grid
}

make_hotspots <- function(grid, n = 10) {
  grid |>
    arrange(desc(count)) |>
    slice_head(n = n) |>
    mutate(
      lat_center = (lat_min + lat_max) / 2,
      lon_center = (lon_min + lon_max) / 2
    )
}

make_leaderboard <- function(dat) {
  base <- dat |>
    group_by(street_name) |>
    summarise(
      total_crimes = n(),
      .groups = "drop"
    )
  
  by_type <- dat |>
    group_by(street_name, category) |>
    summarise(n = n(), .groups = "drop") |>
    pivot_wider(
      names_from = category,
      values_from = n,
      values_fill = 0
    )
  
  full <- base |>
    left_join(by_type, by = "street_name") |>
    mutate(
      pct_of_all = round(100 * total_crimes / sum(total_crimes), 1)
    ) |>
    arrange(desc(total_crimes))
  
  full
}

# --- UI ------------------------------------------------------

ui <- fluidPage(
  titlePanel("Exeter Crime Dashboard"),
  
  checkboxGroupInput(
    inputId = "crime_filter",
    label = "Filter crime types:",
    choices = crime_types,
    selected = crime_types,
    inline = TRUE
  ),
  
  sliderInput(
    inputId = "grid_resolution",
    label = "Grid resolution (approx metres)",
    min = 100,
    max = 500,
    value = 250,
    step = 50
  ),
  
  tabsetPanel(
    
    tabPanel(
      "Home",
      h2("Exeter Crime Dashboard"),
      p("Created by: Ewan Richards"),
      tags$hr(),
      h3("About this project"),
      p("This dashboard explores recorded crime around Exeter using live data from the UK Police API."),
      h3("Data sources"),
      tags$ul(
        tags$li("UK Police API (street-level crime)"),
        tags$li("Local summary CSV: Exeter_stats.csv")
      ),
      h3("Methods"),
      tags$ul(
        tags$li("Spatial visualisation with leaflet (cluster map, grid intensity, hotspots)"),
        tags$li("Categorical summaries and trend analysis with ggplot2"),
        tags$li("Automatic monthly data updates"),
        tags$li("Street-level aggregation and hotspot analysis")
      ),
      h3("Visualisations"),
      tags$ul(
        tags$li("Crime Map: clustered and colour-coded markers"),
        tags$li("Grid Crime Intensity Map with resolution slider"),
        tags$li("Hotspot labels: top 10 high-intensity cells"),
        tags$li("Crime Categories: bar chart of totals"),
        tags$li("Trend Line: 12-month time series"),
        tags$li("Top Streets: analytical leaderboard table"),
        tags$li("Summary: tabular overview of crime types")
      ),
      h3("Contact"),
      p("Portfolio project by Ewan Richards.")
    ),
    
    tabPanel(
      "Crime Map",
      leafletOutput("crimeMap", height = "700px")
    ),
    
    tabPanel(
      "Grid Intensity + Hotspots",
      leafletOutput("gridMap", height = "700px")
    ),
    
    tabPanel(
      "Crime Categories",
      plotOutput("barChart", height = "600px")
    ),
    
    tabPanel(
      "Trend Line",
      uiOutput("trendUI"),
      plotOutput("trendLine", height = "600px")
    ),
    
    tabPanel(
      "Top Streets",
      h3("Top Streets Leaderboard"),
      tableOutput("leaderTable")
    ),
    
    tabPanel(
      "Summary",
      h3("Crime Summary"),
      tableOutput("summaryTable")
    )
  )
)

# --- SERVER --------------------------------------------------

server <- function(input, output, session) {
  
  filtered_map <- reactive({
    req(input$crime_filter)
    crime_map |>
      filter(category %in% input$crime_filter)
  })
  
  output$crimeMap <- renderLeaflet({
    dat <- filtered_map()
    
    leaflet(dat) |>
      addProviderTiles(providers$OpenStreetMap.Mapnik) |>
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = 5,
        color = ~pal(category),
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = popup,
        group = "Markers"
      ) |>
      addMarkers(
        lng = ~lon, lat = ~lat,
        clusterOptions = markerClusterOptions(),
        group = "Clusters"
      ) |>
      addLayersControl(
        overlayGroups = c("Markers", "Clusters"),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~category,
        title = "Crime Type",
        opacity = 1
      )
  })
  
  output$gridMap <- renderLeaflet({
    dat <- filtered_map()
    
    # convert slider metres to approx degrees (~111km per degree)
    cell_size <- input$grid_resolution / 111000
    
    grid <- make_grid(dat, cell_size = cell_size)
    hotspots <- make_hotspots(grid, n = 10)
    
    leaflet() |>
      addProviderTiles(providers$OpenStreetMap.Mapnik) |>
      addRectangles(
        lng1 = grid$lon_min,
        lat1 = grid$lat_min,
        lng2 = grid$lon_max,
        lat2 = grid$lat_max,
        fillOpacity = 0.6,
        stroke = FALSE,
        color = sapply(grid$count, function(c) {
          if (c > 20) return("red")
          if (c > 10) return("orange")
          if (c > 5)  return("yellow")
          return("green")
        })
      ) |>
      addLabelOnlyMarkers(
        lng = hotspots$lon_center,
        lat = hotspots$lat_center,
        label = paste0("Hotspot (", hotspots$count, ")"),
        labelOptions = labelOptions(
          noHide = TRUE,
          textsize = "12px",
          textOnly = TRUE,
          style = list(
            "color" = "black",
            "font-weight" = "bold",
            "background-color" = "rgba(255,255,255,0.7)",
            "padding" = "2px",
            "border-radius" = "3px"
          )
        )
      )
  })
  
  output$barChart <- renderPlot({
    ggplot(crime, aes(x = crime_type, y = total, fill = crime_type)) +
      geom_col() +
      theme_minimal() +
      labs(title = "Crime totals by category") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$trendUI <- renderUI({
    if (all(monthly_data$count == 0)) {
      h4("No monthly data available from Police.uk")
    } else {
      h4("Monthly crime counts (last 12 completed months)")
    }
  })
  
  output$trendLine <- renderPlot({
    if (all(monthly_data$count == 0)) {
      plot.new()
      text(0.5, 0.5, "No data available", cex = 1.5)
    } else {
      ggplot(monthly_data, aes(x = month, y = count)) +
        geom_line(color = "#1f77b4", linewidth = 1.2) +
        geom_point(color = "#d62728", size = 2) +
        theme_minimal() +
        labs(
          x = "Month",
          y = "Number of crimes"
        )
    }
  })
  
  output$leaderTable <- renderTable({
    dat <- filtered_map()
    make_leaderboard(dat)
  })
  
  output$summaryTable <- renderTable({
    crime
  })
}

# --- RUN APP -------------------------------------------------
shinyApp(ui, server)

