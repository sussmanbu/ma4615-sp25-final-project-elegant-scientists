# Load all required packages
library(shiny)
library(leaflet)
library(plotly)
library(DT)
library(tidyverse)
library(readr)

# Load data once at the start to populate UI
data_file <- "simpson_pm.rds"
simpson_pm_data <- readRDS(data_file)

# UI definition
ui <- fluidPage(
  titlePanel("Air Quality Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("site", "Select Monitoring Site:", 
                  choices = c("All", unique(simpson_pm_data$site_name))),
      dateRangeInput("dates", "Date Range:",
                     start = min(as.Date(simpson_pm_data$date), na.rm = TRUE),
                     end = max(as.Date(simpson_pm_data$date), na.rm = TRUE)),
      selectInput("metric", "Air Quality Metric:",
                  choices = c("PM2.5 Daily Mean" = "pm2.5_dailymean",
                              "Air Quality Index" = "aqi")),
      checkboxGroupInput("demographics", "Show Demographic Variables:",
                         choices = c("Race Proportions", "Income", "Poverty Rate", "Diversity Index"),
                         selected = "Diversity Index"),
      actionButton("update", "Update View")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map", height = "600px")),
        tabPanel("Time Series", plotlyOutput("timeseries")),
        tabPanel("Statistics", DTOutput("stats_table")),
        tabPanel("Data Explorer", DTOutput("data_table"))
      )
    )
  )
)

# Server definition
server <- function(input, output, session) {
  # Load and preprocess data
  simpson_pm <- reactive({
    # Read the RDS file with error handling
    df <- tryCatch({
      readRDS(data_file)
    }, error = function(e) {
      stop("Failed to load data: ", e$message)
    })
    
    # Convert data types
    df <- df %>%
      mutate(
        date = as.Date(date),
        neighborhood = as.factor(neighborhood),
        site_name = as.factor(site_name),
        pm2.5_dailymean = as.numeric(pm2.5_dailymean),
        aqi = as.numeric(aqi),
        pct_white = as.numeric(pct_white),
        pct_black = as.numeric(pct_black),
        median_incomeE = as.numeric(median_incomeE),
        pct_poverty = as.numeric(pct_poverty),
        simpson_diversity = as.numeric(simpson_diversity)
      )
    
    # Filter based on user inputs
    if (input$site != "All") {
      df <- df %>% filter(site_name == input$site)
    }
    
    df <- df %>% filter(date >= input$dates[1] & date <= input$dates[2])
    
    # Validate the filtered data frame
    if (nrow(df) == 0) {
      stop("No data available for the selected filters.")
    }
    
    df
  })
  
  # Interactive map
  output$map <- renderLeaflet({
    df <- simpson_pm() %>%
      group_by(site_name, lat, long) %>%
      summarise(
        avg_pm = mean(pm2.5_dailymean, na.rm = TRUE),
        avg_aqi = mean(aqi, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Validate summarized data
    if (nrow(df) == 0 || all(is.na(df$avg_pm)) || all(is.na(df$avg_aqi))) {
      return(leaflet() %>% addTiles() %>% addLabelOnlyMarkers(label = "No data to display"))
    }
    
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~long, 
        lat = ~lat,
        radius = ~ifelse(input$metric == "pm2.5_dailymean", 
                         scales::rescale(avg_pm, to = c(5, 20)), 
                         scales::rescale(avg_aqi, to = c(5, 20))),
        color = ~ifelse(input$metric == "pm2.5_dailymean",
                        colorNumeric("YlOrRd", avg_pm)(avg_pm),
                        colorNumeric("YlOrRd", avg_aqi)(avg_aqi)),
        stroke = FALSE, 
        fillOpacity = 0.8,
        popup = ~paste0("<b>", site_name, "</b><br>",
                        "Avg PM2.5: ", round(avg_pm, 2), " µg/m³<br>",
                        "Avg AQI: ", round(avg_aqi, 1))
      ) %>%
      addLegend(
        pal = colorNumeric("YlOrRd", ifelse(input$metric == "pm2.5_dailymean", df$avg_pm, df$avg_aqi)),
        values = ifelse(input$metric == "pm2.5_dailymean", df$avg_pm, df$avg_aqi),
        title = ifelse(input$metric == "pm2.5_dailymean", "PM2.5 (µg/m³)", "AQI"),
        opacity = 1
      )
  })
  
  # Time series plot
  output$timeseries <- renderPlotly({
    df <- simpson_pm() %>%
      group_by(date, site_name) %>%
      summarise(
        metric_value = mean(get(input$metric), na.rm = TRUE),
        .groups = "drop"
      )
    
    # Validate data
    if (nrow(df) == 0 || all(is.na(df$metric_value)) || all(is.na(df$date))) {
      return(plot_ly() %>% add_text(text = "No data to display", x = 0.5, y = 0.5))
    }
    
    p <- ggplot(df, aes(x = date, y = metric_value, color = site_name)) +
      geom_line() +
      geom_point() +
      labs(
        x = "Date", 
        y = ifelse(input$metric == "pm2.5_dailymean", "PM2.5 (µg/m³)", "Air Quality Index"),
        color = "Site"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Summary statistics table
  output$stats_table <- renderDT({
    selected_cols <- c("site_name", input$metric)
    if ("Race Proportions" %in% input$demographics) {
      selected_cols <- c(selected_cols, starts_with("pct_"))
    }
    if ("Income" %in% input$demographics) {
      selected_cols <- c(selected_cols, "median_incomeE")
    }
    if ("Poverty Rate" %in% input$demographics) {
      selected_cols <- c(selected_cols, "pct_poverty")
    }
    if ("Diversity Index" %in% input$demographics) {
      selected_cols <- c(selected_cols, "simpson_diversity")
    }
    
    df <- simpson_pm() %>%
      select(all_of(selected_cols)) %>%
      group_by(site_name) %>%
      summarise(across(
        where(is.numeric), 
        list(
          mean = ~mean(., na.rm = TRUE), 
          sd = ~sd(., na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      ))
    
    datatable(df, options = list(scrollX = TRUE))
  })
  
  # Data explorer table
  output$data_table <- renderDT({
    datatable(
      simpson_pm(), 
      options = list(scrollX = TRUE, pageLength = 10)
    )
  })
}

# Run the app
shinyApp(ui, server)