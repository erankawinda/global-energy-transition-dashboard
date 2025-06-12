# app.R

library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(readr)
library(sf)
library(scales)
library(tidyr)

# ─── 1. DATA PREP ─────────────────────────────────

# Load and prepare data
energy_raw <- read_csv("data/owid-energy-data.csv", show_col_types = FALSE)
world <- st_read("data/world-countries.json", quiet = TRUE)

region_map <- world %>%
  st_drop_geometry() %>%
  select(iso = iso_a3, region = region_un)

energy <- energy_raw %>%
  select(
    country, iso_code, year,
    renewables_share_elec, renewables_electricity,
    fossil_electricity, nuclear_electricity,
    energy_per_capita, greenhouse_gas_emissions,
    gdp, population
  ) %>%
  left_join(region_map, by = c("iso_code" = "iso")) %>%
  mutate(
    total_electricity = fossil_electricity + renewables_electricity + 
      coalesce(nuclear_electricity, 0),
    co2_intensity = case_when(
      total_electricity > 0 ~ greenhouse_gas_emissions * 1e6 / total_electricity,
      TRUE ~ NA_real_
    ),
    renewables_share_elec = pmin(renewables_share_elec, 100)
  ) %>%
  filter(!is.na(renewables_share_elec), !is.na(region), year >= 2000)

# ─── 2. UI ─────────────────────────────────

ui <- dashboardPage(
  title = "Global Energy Transition",
  skin = "black",
  
  # Header
  dashboardHeader(
    title = "Global Energy Transition",
    titleWidth = 250
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("Current State", tabName = "current", icon = icon("globe-americas")),
      menuItem("Evolution", tabName = "evolution", icon = icon("chart-line")),
      menuItem("Impact Analysis", tabName = "impact", icon = icon("chart-area"))
    ),
    
    # Year selector
    div(class = "year-container",
        h5("Select Year"),
        sliderInput("year", NULL,
                    min = 2000,
                    max = max(energy$year),
                    value = max(energy$year),
                    sep = "",
                    animate = animationOptions(interval = 2000, loop = FALSE),
                    width = "100%",
                    ticks = FALSE)
    ),
    
    # Global metric
    div(class = "metric-container",
        h5("Global Average"),
        div(class = "metric-value", textOutput("globalAvgText")),
        div(class = "metric-label", "Renewable Energy")
    )
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$script(HTML("
        $(document).ready(function() {
          $('body').css('overflow', 'hidden');
        });
      "))
    ),
    
    tabItems(
      # TAB 1: Current State
      tabItem(
        tabName = "current",
        fluidRow(
          column(12,
                 div(class = "section-header",
                     h3("Global Renewable Energy Distribution"),
                     p("Percentage of electricity generated from renewable sources by country")
                 )
          )
        ),
        fluidRow(
          column(12,
                 div(class = "main-viz-container",
                     plotlyOutput("worldMap", height = "calc(100vh - 280px)")
                 )
          )
        ),
        fluidRow(
          column(4,
                 div(class = "stat-box",
                     div(class = "stat-value", textOutput("statCountries")),
                     div(class = "stat-label", "Countries Tracked")
                 )
          ),
          column(4,
                 div(class = "stat-box",
                     div(class = "stat-value", textOutput("statLeaders")),
                     div(class = "stat-label", "Above 50% Renewable")
                 )
          ),
          column(4,
                 div(class = "stat-box",
                     div(class = "stat-value", textOutput("statGrowth")),
                     div(class = "stat-label", "Year-over-Year Change")
                 )
          )
        )
      ),
      
      # TAB 2: Evolution
      tabItem(
        tabName = "evolution",
        fluidRow(
          column(12,
                 div(class = "section-header",
                     h3("Energy Transition Timeline"),
                     p("Tracking renewable energy adoption in major economies from 2000 to present")
                 )
          )
        ),
        fluidRow(
          column(8,
                 div(class = "viz-container",
                     plotlyOutput("timeSeriesPlot", height = "calc(100vh - 240px)")
                 )
          ),
          column(4,
                 div(class = "viz-container",
                     h4("Progress Since 2000"),
                     plotlyOutput("progressPlot", height = "calc(100vh - 280px)")
                 )
          )
        )
      ),
      
      # TAB 3: Impact Analysis
      tabItem(
        tabName = "impact",
        fluidRow(
          column(12,
                 div(class = "section-header",
                     h3("Renewable Energy and CO₂ Emissions"),
                     p("Examining the relationship between renewable energy adoption and carbon intensity of electricity generation"),
                     div(class = "note-box",
                         icon("info-circle"),
                         "Countries with higher renewable shares typically show lower CO₂ intensity. 
                Bubble size represents per capita energy consumption."
                     )
                 )
          )
        ),
        fluidRow(
          column(8,
                 div(class = "viz-container",
                     plotlyOutput("scatterPlot", height = "calc(100vh - 300px)")
                 )
          ),
          column(4,
                 div(class = "viz-container",
                     h4("Regional Averages"),
                     plotlyOutput("regionalPlot", height = "calc(100vh - 300px)")
                 )
          )
        )
      )
    )
  )
)

# ─── 3. SERVER ─────────────────────────────────

server <- function(input, output, session) {
  
  # Reactive data
  yearData <- reactive({
    filter(energy, year == input$year)
  })
  
  # Global average text
  output$globalAvgText <- renderText({
    avg <- yearData() %>%
      summarise(avg = mean(renewables_share_elec, na.rm = TRUE)) %>%
      pull(avg)
    paste0(round(avg, 1), "%")
  })
  
  # Statistics
  output$statCountries <- renderText({
    nrow(yearData())
  })
  
  output$statLeaders <- renderText({
    sum(yearData()$renewables_share_elec > 50, na.rm = TRUE)
  })
  
  output$statGrowth <- renderText({
    if (input$year > 2000) {
      prev_year <- filter(energy, year == input$year - 1)
      curr_year <- yearData()
      
      prev_avg <- mean(prev_year$renewables_share_elec, na.rm = TRUE)
      curr_avg <- mean(curr_year$renewables_share_elec, na.rm = TRUE)
      growth <- curr_avg - prev_avg
      
      paste0(ifelse(growth > 0, "+", ""), round(growth, 1), "%")
    } else {
      "N/A"
    }
  })
  
  # World map
  output$worldMap <- renderPlotly({
    df <- yearData()
    
    # Simple grayscale color palette
    colors <- list(
      c(0, 0.2, 0.4, 0.6, 0.8, 1),
      c("#f7f7f7", "#d9d9d9", "#bdbdbd", "#969696", "#636363", "#252525")
    )
    
    plot_geo(data = df, locationmode = 'ISO-3') %>%
      add_trace(
        type = 'choropleth',
        locations = ~iso_code,
        z = ~renewables_share_elec,
        text = ~paste0(
          country, "\n",
          "Renewable: ", round(renewables_share_elec, 1), "%"
        ),
        hoverinfo = "text",
        colorscale = colors,
        reversescale = TRUE,
        marker = list(line = list(width = 0.5, color = '#ffffff')),
        colorbar = list(
          title = list(text = "% Renewable", font = list(family = "Arial, sans-serif", size = 12)),
          thickness = 12,
          len = 0.6,
          x = 0.98,
          tickfont = list(family = "Arial, sans-serif", size = 10)
        )
      ) %>%
      layout(
        geo = list(
          projection = list(type = "natural earth"),
          showframe = FALSE,
          showcoastlines = TRUE,
          coastlinecolor = "#cccccc",
          bgcolor = "#ffffff"
        ),
        margin = list(l = 0, r = 0, t = 0, b = 0),
        font = list(family = "Arial, sans-serif")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Time series
  output$timeSeriesPlot <- renderPlotly({
    countries <- c("China", "United States", "Germany", "India", "Brazil", "Norway")
    df <- filter(energy, country %in% countries)
    
    # Simple color palette
    colors <- c("#000000", "#404040", "#666666", "#808080", "#999999", "#b3b3b3")
    
    p <- plot_ly()
    
    for(i in 1:length(countries)) {
      df_country <- filter(df, country == countries[i])
      p <- p %>%
        add_trace(
          data = df_country,
          x = ~year,
          y = ~renewables_share_elec,
          name = countries[i],
          type = 'scatter',
          mode = 'lines',
          line = list(width = 2, color = colors[i]),
          hovertemplate = paste0(
            countries[i], "<br>",
            "Year: %{x}<br>",
            "Renewable: %{y:.1f}%<br>",
            "<extra></extra>"
          )
        )
    }
    
    # Add event markers
    p %>%
      add_annotations(
        x = c(2011, 2015),
        y = c(25, 35),
        text = c("Fukushima", "Paris Agreement"),
        showarrow = FALSE,
        font = list(size = 10, color = "#666666", family = "Arial, sans-serif")
      ) %>%
      layout(
        xaxis = list(
          title = "Year",
          gridcolor = "#f0f0f0",
          font = list(family = "Arial, sans-serif", size = 12)
        ),
        yaxis = list(
          title = "Renewable Energy Share (%)",
          range = c(0, 100),
          gridcolor = "#f0f0f0",
          font = list(family = "Arial, sans-serif", size = 12)
        ),
        legend = list(
          x = 0.02,
          y = 0.98,
          font = list(family = "Arial, sans-serif", size = 11)
        ),
        margin = list(l = 60, r = 20, t = 20, b = 40),
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        font = list(family = "Arial, sans-serif")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Progress plot
  output$progressPlot <- renderPlotly({
    countries <- c("China", "United States", "Germany", "India", "Brazil", "Japan")
    
    df_comparison <- energy %>%
      filter(country %in% countries, year %in% c(2000, input$year)) %>%
      select(country, year, renewables_share_elec) %>%
      pivot_wider(names_from = year, values_from = renewables_share_elec, names_prefix = "year_") %>%
      mutate(
        change = get(paste0("year_", input$year)) - year_2000,
        country = factor(country, levels = country[order(change)])
      )
    
    plot_ly(df_comparison) %>%
      add_trace(
        x = ~change,
        y = ~country,
        type = 'bar',
        orientation = 'h',
        marker = list(color = "#666666"),
        text = ~paste0(round(change, 1), "%"),
        textposition = "outside",
        textfont = list(size = 10, family = "Arial, sans-serif"),
        hovertemplate = paste0(
          "%{y}<br>",
          "Change: %{x:+.1f}%<br>",
          "<extra></extra>"
        )
      ) %>%
      layout(
        xaxis = list(
          title = "Percentage Point Change",
          gridcolor = "#f0f0f0",
          font = list(family = "Arial, sans-serif", size = 11)
        ),
        yaxis = list(
          title = "",
          font = list(family = "Arial, sans-serif", size = 11)
        ),
        margin = list(l = 100, r = 50, t = 20, b = 40),
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        font = list(family = "Arial, sans-serif")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Scatter plot
  output$scatterPlot <- renderPlotly({
    df <- yearData() %>%
      filter(!is.na(co2_intensity), co2_intensity > 0) %>%
      filter(co2_intensity < quantile(co2_intensity, 0.95, na.rm = TRUE))
    
    # Fit trend line
    fit <- lm(log10(co2_intensity) ~ renewables_share_elec, data = df)
    x_range <- seq(0, 100, by = 1)
    y_pred <- 10^predict(fit, newdata = data.frame(renewables_share_elec = x_range))
    
    plot_ly() %>%
      # Trend line
      add_trace(
        x = x_range,
        y = y_pred,
        type = 'scatter',
        mode = 'lines',
        line = list(color = '#cccccc', width = 2, dash = 'dash'),
        showlegend = FALSE,
        hoverinfo = 'skip'
      ) %>%
      # Points
      add_trace(
        data = df,
        x = ~renewables_share_elec,
        y = ~co2_intensity,
        type = 'scatter',
        mode = 'markers',
        marker = list(
          size = ~sqrt(energy_per_capita),
          sizemode = 'area',
          sizeref = 2,
          color = ~region,
          colors = c("#000000", "#404040", "#666666", "#999999", "#cccccc"),
          opacity = 0.6,
          line = list(color = '#ffffff', width = 1)
        ),
        text = ~paste0(
          country, "\n",
          "Renewable: ", round(renewables_share_elec, 1), "%\n",
          "CO₂: ", round(co2_intensity, 0), " t/TWh"
        ),
        hoverinfo = "text",
        name = ~region
      ) %>%
      layout(
        xaxis = list(
          title = "Renewable Energy Share (%)",
          range = c(-5, 105),
          gridcolor = "#f0f0f0",
          font = list(family = "Arial, sans-serif", size = 12)
        ),
        yaxis = list(
          title = "CO₂ Intensity (tonnes per TWh) - Log Scale",
          type = "log",
          gridcolor = "#f0f0f0",
          font = list(family = "Arial, sans-serif", size = 12)
        ),
        legend = list(
          title = list(text = "Region", font = list(family = "Arial, sans-serif", size = 11)),
          font = list(family = "Arial, sans-serif", size = 10)
        ),
        margin = list(l = 80, r = 20, t = 20, b = 60),
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        font = list(family = "Arial, sans-serif")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Regional plot
  output$regionalPlot <- renderPlotly({
    df <- yearData() %>%
      group_by(region) %>%
      summarise(
        renewable_avg = mean(renewables_share_elec, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(desc(renewable_avg))
    
    plot_ly(df, 
            x = ~renewable_avg,
            y = ~reorder(region, renewable_avg),
            type = 'bar',
            orientation = 'h',
            marker = list(color = "#666666"),
            text = ~paste0(round(renewable_avg, 1), "%"),
            textposition = "outside",
            textfont = list(size = 10, family = "Arial, sans-serif"),
            hovertemplate = paste0(
              "%{y}<br>",
              "Average: %{x:.1f}%<br>",
              "<extra></extra>"
            )) %>%
      layout(
        xaxis = list(
          title = "Average Renewable %",
          range = c(0, 60),
          gridcolor = "#f0f0f0",
          font = list(family = "Arial, sans-serif", size = 11)
        ),
        yaxis = list(
          title = "",
          font = list(family = "Arial, sans-serif", size = 11)
        ),
        margin = list(l = 80, r = 40, t = 20, b = 40),
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        font = list(family = "Arial, sans-serif")
      ) %>%
      config(displayModeBar = FALSE)
  })
}

# ─── 4. RUN APP ─────────────────────────────────
shinyApp(ui, server)