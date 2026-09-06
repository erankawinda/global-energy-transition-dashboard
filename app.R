# Global Energy Transition Dashboard
# Author: Eran Dodampe Gamage
# Data Source: Our World in Data

library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(readr)
library(sf)
library(tidyr)

# Data Preparation

energy_raw <- read_csv("data/owid-energy-data.csv", show_col_types = FALSE)
world <- st_read("data/world-countries.json", quiet = TRUE)

required_energy_columns <- c(
  "country", "iso_code", "year", "renewables_share_elec",
  "carbon_intensity_elec"
)
missing_energy_columns <- setdiff(required_energy_columns, names(energy_raw))
if (length(missing_energy_columns) > 0) {
  stop(
    "The energy snapshot is missing required columns: ",
    paste(missing_energy_columns, collapse = ", ")
  )
}

required_map_columns <- c("iso_a3", "region_un")
missing_map_columns <- setdiff(required_map_columns, names(world))
if (length(missing_map_columns) > 0) {
  stop(
    "The boundary file is missing required properties: ",
    paste(missing_map_columns, collapse = ", ")
  )
}

region_map <- world %>%
  st_drop_geometry() %>%
  select(iso = iso_a3, region = region_un) %>%
  filter(iso != "-99") %>%
  distinct(iso, .keep_all = TRUE)

energy <- energy_raw %>%
  select(
    country, iso_code, year,
    renewables_share_elec, carbon_intensity_elec
  ) %>%
  left_join(region_map, by = c("iso_code" = "iso")) %>%
  mutate(
    co2_intensity = carbon_intensity_elec
  ) %>%
  filter(!is.na(renewables_share_elec), !is.na(region), year >= 2000)

if (anyDuplicated(energy[c("country", "year")])) {
  stop("Country-year rows are not unique after the region join.")
}

paired_country_mean_change <- function(data, selected_year) {
  if (selected_year <= min(data$year)) {
    return(NA_real_)
  }

  current <- data %>%
    filter(year == selected_year) %>%
    select(iso_code, current_share = renewables_share_elec)
  previous <- data %>%
    filter(year == selected_year - 1) %>%
    select(iso_code, previous_share = renewables_share_elec)

  matched <- inner_join(current, previous, by = "iso_code")
  if (nrow(matched) == 0) {
    return(NA_real_)
  }
  mean(matched$current_share - matched$previous_share, na.rm = TRUE)
}

# User interface

ui <- dashboardPage(
  title = "Global Energy Transition",
  skin = "black",
  
  dashboardHeader(
    title = "Global Energy Transition Dashboard",
    titleWidth = 250
  ),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("Selected Year", tabName = "current", icon = icon("globe-americas")),
      menuItem("Evolution", tabName = "evolution", icon = icon("chart-line")),
      menuItem("Renewables and Emissions", tabName = "impact", icon = icon("chart-area"))
    ),
    
    div(class = "dashboard-description",
        p("Explore changes in renewable electricity and their descriptive relationship with emissions since 2000.")
    ),
    
    div(class = "year-container",
        h5("Select Year"),
        sliderInput("year", NULL,
                    min = 2000,
                    max = max(energy$year),
                    value = max(energy$year),
                    sep = "",
                    animate = FALSE,
                    width = "100%",
                    ticks = TRUE),
        actionButton("playBtn", 
                     label = "", 
                     icon = icon("play"),
                     class = "btn-play-slider")
    ),
    
    div(class = "metric-container global-average",
        h5("Country Average"),
        div(class = "metric-value", textOutput("globalAvgText")),
        div(class = "metric-label", "Mean renewable electricity share")
    ),
    
    div(class = "sidebar-footer",
        p("Data: Our World in Data"),
        p("Dashboard: Eran Dodampe Gamage")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$script(HTML("
        $(document).ready(function() {
          // Play button functionality
          var playing = false;
          var interval;
          
          $(document).on('click', '#playBtn', function(e) {
            e.preventDefault();
            
            if (!playing) {
              playing = true;
              $(this).find('i').removeClass('fa-play').addClass('fa-pause');
              
              // Get slider instance
              var slider = $('#year').data('ionRangeSlider');
              if (!slider) return;
              
              var currentVal = slider.result.from;
              var maxVal = slider.result.max;
              var minVal = slider.result.min;
              
              // Reset to start if at end
              if (currentVal >= maxVal) {
                slider.update({from: minVal});
              }
              
              // Animate through years
              interval = setInterval(function() {
                var current = slider.result.from;
                if (current < maxVal) {
                  slider.update({from: current + 1});
                } else {
                  clearInterval(interval);
                  playing = false;
                  $('#playBtn').find('i').removeClass('fa-pause').addClass('fa-play');
                }
              }, 2000);
            } else {
              // Pause
              playing = false;
              clearInterval(interval);
              $(this).find('i').removeClass('fa-pause').addClass('fa-play');
            }
          });
        });
      "))
    ),
    
    tabItems(
      # Current State Tab
      tabItem(
        tabName = "current",
        fluidRow(
          column(12,
                 div(class = "section-header",
                     h3("Renewable Electricity Share by Country"),
                     div(class = "section-subtitle",
                         "Percentage of electricity generated from renewable sources by country"
                     ),
                     div(class = "section-description", 
                         "The map shows the values available in the committed dataset for the selected year. Darker green indicates a higher renewable share.")
                 )
          )
        ),
        fluidRow(
          style = "margin-bottom: 30px;",
          column(12,
                 div(class = "main-viz-container",
                     plotlyOutput("worldMap", height = "600px")
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
                     div(class = "stat-label", "Paired-Country Annual Change (pp)")
                 )
          )
        )
      ),
      
      # Evolution Tab
      tabItem(
        tabName = "evolution",
        fluidRow(
          style = "margin-bottom: 15px;",
          column(12,
                 div(class = "section-header",
                     h3("Energy Transition Timeline"),
                     p("Tracking renewable electricity shares in selected economies across the available years"),
                     p(class = "section-description",
                       "The lines are descriptive and do not identify the causes of changes over time.")
                 )
          )
        ),
        fluidRow(
          style = "margin-bottom: 15px;",
          column(8,
                 div(class = "viz-container",
                     plotlyOutput("timeSeriesPlot", height = "350px")
                 )
          ),
          column(4,
                 div(class = "viz-container compact",
                     h4("Progress Since 2000"),
                     plotlyOutput("progressPlot", height = "200px")
                 )
          )
        ),
        fluidRow(
          column(12,
                 div(class = "note-box",
                     icon("info-circle"),
                     "Differences in data coverage between years can affect the country mean and comparisons."
                 )
          )
        )
      ),
      
      # Impact Analysis Tab
      tabItem(
        tabName = "impact",
        fluidRow(
          column(12,
                 div(class = "section-header",
                     h3("Renewable Electricity and Carbon Intensity"),
                     p("Comparing renewable electricity share with the carbon intensity of electricity generation"),
                     div(class = "note-box",
                         icon("info-circle"),
                         "This is a descriptive cross-country comparison. Each marker is one country, and the fitted line summarises association rather than causation."
                     )
                 )
          )
        ),
        fluidRow(
          column(8,
                 div(class = "viz-container",
                     plotlyOutput("scatterPlot", height = "450px")
                 )
          ),
          column(4,
                 div(class = "viz-container",
                     h4("Regional Averages"),
                     plotlyOutput("regionalPlot", height = "450px")
                 )
          )
        ),
        fluidRow(
          column(12,
                 div(class = "insights-container",
                     h4("Reading the Charts"),
                     div(class = "insight-box",
                         p("• ", strong("Coverage:"), " Country and regional summaries use the observations available for the selected year."),
                         p("• ", strong("Averages:"), " Country averages are unweighted and should not be interpreted as global electricity-weighted estimates."),
                         p("• ", strong("Association:"), " The renewables-and-emissions chart is descriptive and does not identify a causal effect.")
                     )
                 )
          )
        )
      )
    )
  )
)

# Server Logic

server <- function(input, output, session) {
  
  # Color palettes
  country_colors <- c(
    "China" = "#FF998080",
    "United States" = "#99CCFF80", 
    "Germany" = "#99FF9980",
    "India" = "#FFCC9980",
    "Brazil" = "#FF99CC80",
    "Norway" = "#CCCCFF80",
    "Japan" = "#FFFF9980"
  )
  
  # Reactive data
  yearData <- reactive({
    filter(energy, year == input$year)
  })
  
  # Unweighted mean across countries with data in the selected year
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
    growth <- paired_country_mean_change(energy, input$year)
    if (!is.na(growth)) {
      paste0(ifelse(growth > 0, "+", ""), round(growth, 1), " pp")
    } else {
      "N/A"
    }
  })
  
  # World map
  output$worldMap <- renderPlotly({
    df <- yearData()
    
    colors <- list(
      c(0, 0.2, 0.4, 0.6, 0.8, 1),
      c("#FFE5CC", "#FFD4A3", "#B8E6B8", "#8FD68F", "#66C266", "#2E8B2E")
    )
    
    plot_geo(data = df, locationmode = 'ISO-3') %>%
      add_trace(
        type = 'choropleth',
        locations = ~iso_code,
        z = ~renewables_share_elec,
        text = ~paste0(country, "\n", "Renewable: ", round(renewables_share_elec, 1), "%"),
        hoverinfo = "text",
        colorscale = colors,
        reversescale = FALSE,
        marker = list(line = list(width = 0.5, color = 'rgba(255,255,255,0.5)')),
        colorbar = list(
          title = list(text = "% Renewable", font = list(family = "Gill Sans, sans-serif", size = 12)),
          thickness = 12,
          len = 0.6,
          x = 0.98,
          tickfont = list(family = "Gill Sans, sans-serif", size = 10)
        )
      ) %>%
      layout(
        geo = list(
          projection = list(type = "natural earth"),
          showframe = FALSE,
          showcoastlines = TRUE,
          coastlinecolor = "rgba(100,100,100,0.3)",
          bgcolor = "rgba(245,245,245,0.3)",
          showocean = TRUE,
          oceancolor = "rgba(173,216,230,0.3)",
          showlakes = TRUE,
          lakecolor = "rgba(173,216,230,0.3)"
        ),
        margin = list(l = 0, r = 0, t = 0, b = 0),
        font = list(family = "Gill Sans, sans-serif")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Time series plot
  output$timeSeriesPlot <- renderPlotly({
    countries <- c("China", "United States", "Germany", "India", "Brazil", "Norway")
    df <- filter(energy, country %in% countries)
    
    p <- plot_ly()
    
    # Country lines
    last_year_data <- df %>%
      group_by(country) %>%
      filter(year == max(year)) %>%
      ungroup()
    
    for(i in seq_along(countries)) {
      df_country <- filter(df, country == countries[i])
      p <- p %>%
        add_trace(
          data = df_country,
          x = ~year,
          y = ~renewables_share_elec,
          name = countries[i],
          type = 'scatter',
          mode = 'lines',
          line = list(width = 3, color = country_colors[countries[i]]),
          opacity = 0.9,
          hovertemplate = paste0(countries[i], "<br>Year: %{x}<br>Renewable: %{y:.1f}%<br><extra></extra>"),
          showlegend = FALSE
        )
    }
    
    # End labels
    p <- p %>%
      add_trace(
        data = last_year_data,
        x = ~year,
        y = ~renewables_share_elec,
        text = ~country,
        type = 'scatter',
        mode = 'text',
        textposition = 'middle right',
        textfont = list(
          size = 11,
          color = sapply(last_year_data$country, function(c) country_colors[c]),
          family = "Gill Sans, sans-serif"
        ),
        showlegend = FALSE,
        hoverinfo = 'skip'
      )
    
    p %>%
      layout(
        xaxis = list(
          title = "Year",
          gridcolor = "rgba(240,240,240,0.5)",
          font = list(family = "Gill Sans, sans-serif", size = 12),
          range = c(2000, max(df$year) + 2)
        ),
        yaxis = list(
          title = "Renewable Electricity Share (%)",
          range = c(0, 100),
          gridcolor = "rgba(240,240,240,0.5)",
          font = list(family = "Gill Sans, sans-serif", size = 12)
        ),
        margin = list(l = 60, r = 100, t = 20, b = 40),
        plot_bgcolor = "rgba(255,255,255,0.8)",
        paper_bgcolor = "white",
        font = list(family = "Gill Sans, sans-serif")
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
    
    bar_colors <- sapply(as.character(df_comparison$country), function(c) country_colors[c])
    
    plot_ly(df_comparison) %>%
      add_trace(
        x = ~change,
        y = ~country,
        type = 'bar',
        orientation = 'h',
        marker = list(
          color = bar_colors,
          line = list(color = 'rgba(255,255,255,0.5)', width = 1)
        ),
        opacity = 0.9,
        text = ~paste0(round(change, 1), " pp"),
        textposition = "outside",
        textfont = list(size = 10, family = "Gill Sans, sans-serif"),
        hovertemplate = paste0("%{y}<br>Change: %{x:+.1f} percentage points<br><extra></extra>")
      ) %>%
      layout(
        xaxis = list(
          title = "Change (percentage points)",
          gridcolor = "rgba(240,240,240,0.5)",
          font = list(family = "Gill Sans, sans-serif", size = 10)
        ),
        yaxis = list(
          title = "",
          font = list(family = "Gill Sans, sans-serif", size = 10)
        ),
        margin = list(l = 80, r = 40, t = 10, b = 30),
        plot_bgcolor = "rgba(255,255,255,0.8)",
        paper_bgcolor = "white",
        font = list(family = "Gill Sans, sans-serif")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Scatter plot
  output$scatterPlot <- renderPlotly({
    df <- yearData() %>%
      filter(!is.na(co2_intensity), co2_intensity > 0)
    
    fit <- lm(log10(co2_intensity) ~ renewables_share_elec, data = df)
    x_range <- seq(0, 100, by = 1)
    y_pred <- 10^predict(fit, newdata = data.frame(renewables_share_elec = x_range))
    
    # Define watercolor palette for regions
    region_colors <- c(
      "Africa" = "rgba(255, 179, 186, 0.7)",      # Soft pink
      "Americas" = "rgba(186, 225, 255, 0.7)",    # Soft blue
      "Asia" = "rgba(255, 223, 186, 0.7)",        # Soft peach
      "Europe" = "rgba(186, 255, 201, 0.7)",      # Soft mint green
      "Oceania" = "rgba(221, 186, 255, 0.7)"      # Soft lavender
    )
    
    p <- plot_ly() %>%
      add_trace(
        x = x_range,
        y = y_pred,
        type = 'scatter',
        mode = 'lines',
        line = list(color = 'rgba(100,100,100,0.5)', width = 2, dash = 'dash'),
        showlegend = FALSE,
        hoverinfo = 'skip'
      )
    
    # Add each region as a separate trace to maintain consistent colors
    for(region_name in names(region_colors)) {
      df_region <- df %>% filter(region == region_name)
      
      if(nrow(df_region) > 0) {
        p <- p %>%
          add_trace(
            data = df_region,
            x = ~renewables_share_elec,
            y = ~co2_intensity,
            type = 'scatter',
            mode = 'markers',
            name = region_name,
            marker = list(
              size = 10,
              color = region_colors[region_name],
              line = list(color = 'rgba(255,255,255,0.8)', width = 1)
            ),
            text = ~paste0(country, "\n", 
                           "Renewable: ", round(renewables_share_elec, 1), "%\n", 
                           "Carbon intensity: ", round(co2_intensity, 0), " g CO₂e/kWh"),
            hoverinfo = "text"
          )
      }
    }
    
    p %>%
      layout(
        xaxis = list(
          title = "Renewable Electricity Share (%)",
          range = c(-5, 105),
          gridcolor = "rgba(240,240,240,0.5)",
          font = list(family = "Gill Sans, sans-serif", size = 12)
        ),
        yaxis = list(
          title = "Carbon Intensity (g CO₂e/kWh) - Log Scale",
          type = "log",
          gridcolor = "rgba(240,240,240,0.5)",
          font = list(family = "Gill Sans, sans-serif", size = 12)
        ),
        legend = list(
          title = list(text = "Region", font = list(family = "Gill Sans, sans-serif", size = 11)),
          font = list(family = "Gill Sans, sans-serif", size = 10),
          bgcolor = "rgba(255,255,255,0.8)",
          bordercolor = "rgba(200,200,200,0.5)",
          borderwidth = 1
        ),
        margin = list(l = 80, r = 20, t = 20, b = 60),
        plot_bgcolor = "rgba(255,255,255,0.8)",
        paper_bgcolor = "white",
        font = list(family = "Gill Sans, sans-serif")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Regional plot
  output$regionalPlot <- renderPlotly({
    df <- yearData() %>%
      group_by(region) %>%
      summarise(
        renewable_avg = mean(renewables_share_elec, na.rm = TRUE),
        n_countries = n(),
        .groups = 'drop'
      ) %>%
      arrange(desc(renewable_avg))
    
    # Use same watercolor palette as scatter plot
    region_colors <- c(
      "Africa" = "rgba(255, 179, 186, 0.7)",      # Soft pink
      "Americas" = "rgba(186, 225, 255, 0.7)",    # Soft blue
      "Asia" = "rgba(255, 223, 186, 0.7)",        # Soft peach
      "Europe" = "rgba(186, 255, 201, 0.7)",      # Soft mint green
      "Oceania" = "rgba(221, 186, 255, 0.7)"      # Soft lavender
    )
    
    plot_ly(df, 
            x = ~renewable_avg,
            y = ~reorder(region, renewable_avg),
            type = 'bar',
            orientation = 'h',
            marker = list(
              color = sapply(df$region, function(r) region_colors[r]),
              line = list(color = 'rgba(255,255,255,0.5)', width = 1)
            ),
            opacity = 0.9,
            text = ~paste0(round(renewable_avg, 1), "%"),
            customdata = ~n_countries,
            textposition = "outside",
            textfont = list(size = 10, family = "Gill Sans, sans-serif"),
            hovertemplate = paste0(
              "%{y}<br>Country mean: %{x:.1f}%<br>",
              "Countries: %{customdata}<br><extra></extra>"
            )
    ) %>%
      layout(
        xaxis = list(
          title = "Mean Renewable Electricity Share (%)",
          range = c(0, min(100, max(60, max(df$renewable_avg) * 1.15))),
          gridcolor = "rgba(240,240,240,0.5)",
          font = list(family = "Gill Sans, sans-serif", size = 11)
        ),
        yaxis = list(
          title = "",
          font = list(family = "Gill Sans, sans-serif", size = 11)
        ),
        margin = list(l = 80, r = 40, t = 20, b = 40),
        plot_bgcolor = "rgba(255,255,255,0.8)",
        paper_bgcolor = "white",
        font = list(family = "Gill Sans, sans-serif")
      ) %>%
      config(displayModeBar = FALSE)
  })
}

# Run Application
shinyApp(ui, server)
