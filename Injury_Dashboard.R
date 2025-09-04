# DATA*6200 Final Project: Severe Injuries and Occupational Illness Tracking Dashboard for understanding Injury Trends

# Author: Karanvir Virdi, Saritha Kumari Krishna Reddy, Stephanie Ajah

# Date: Dec 12, 2024

# Load the libraries
library(shiny)
library(shinyjs)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(maps)
library(tidyr)
library(DT)

# Load the dataset
setwd("/Users/sarithakumarik/Documents/DATA6200/FinalProject/Dataset")
severe_injuries_data <- read.csv("critical_injuries_data.csv", stringsAsFactors = FALSE)
occ_injuries_data <- read.csv("occupational_illness_data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  useShinyjs(),
  
  # Welcome Page
  div(
    id = "welcome-page",
    style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100vh; background-color: #4682b4;",
    tags$h1(
      style = "font-family: 'Playfair Display', serif; font-size: 6rem; font-weight: 700; color: #ffffff; margin-bottom: 2rem;",
      "Welcome to Injury Data Dashboard!"
    ),
    tags$p(
      style = "font-family: 'Playfair Display', serif; font-size: 2.5rem; color: #333; margin-bottom: 2rem;",
      "Let's do some analysis."
    ),
    actionButton(
      inputId = "get_started_btn",
      label = "Get Started",
      class = "btn btn-primary btn-lg",
      style = "font-family: 'Playfair Display', serif; font-size: 3rem; font-weight: 600; padding: 1rem 2rem; background-color: #303030; color: #ffffff;"
    )
  ),
  
  # Dashboard Page
  hidden(
    div(
      id = "dashboard-page",
      style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100vh; background-color: #4682b4;",
      tags$h1(
        style = "font-family: 'Playfair Display', serif; font-size: 6rem; font-weight: 700; color: #ffffff; margin-bottom: 8rem;",
        "Injury Data Dashboard"
      ),
      fluidRow(
        actionButton(
          inputId = "severe_btn",
          label = "Severe Injuries",
          class = "btn btn-primary btn-lg",
          style = "font-family: 'Playfair Display', serif; font-size: 2.5rem; font-weight: 600; padding: 1rem 2rem; margin-right: 1rem; background-color: #303030; color: #ffffff;"
        ),
        actionButton(
          inputId = "occupational_btn",
          label = "Occupational Injuries/Illnesses",
          class = "btn btn-primary btn-lg",
          style = "font-family: 'Playfair Display', serif; font-size: 2.5rem; font-weight: 600; padding: 1rem 2rem; background-color: #303030; color: #ffffff;"
        )
      )
    )
  ),
  # Severe Injuries Page
  hidden(
    div(
      id = "severe-page",
      dashboardPage(
        dashboardHeader(
          title = div(
            actionButton(
              inputId = "back_button",
              label = NULL,
              icon = icon("arrow-left"),
              style = "margin-right: 0px; border: none; background-color: transparent; color: white;"
            ),
            span("Severe Injuries", style = "font-size: 19px; padding-left: 0px;")
          )
        ),
        dashboardSidebar(
          sidebarMenu(
            menuItem("Injuries by State", tabName = "state", icon = icon("map-marker-alt")),
            menuItem("Top Industries", tabName = "industries", icon = icon("building")),
            menuItem("Top States", tabName = "states", icon = icon("flag-checkered")),
            menuItem("Event Categories", tabName = "events", icon = icon("calendar-alt")),
            menuItem("Source Categories", tabName = "source", icon = icon("bullseye")),
            menuItem("Body Part Analysis", tabName = "bodypart", icon = icon("stethoscope")),
            menuItem("Trend Overview", tabName = "trend", icon = icon("chart-line")),
            menuItem("Interactive Filters", tabName = "filtering", icon = icon("filter"))
          )
        ),
        dashboardBody(
          tabItems(
            # Injuries by State Tab
            tabItem(tabName = "state",
                    h2("Injuries by State Analysis",
                       style = "font-weight: bold; font-size: 3.5rem; text-align: center;"),
                    sliderInput(
                      "year",
                      "Select Year:",
                      min = 2015,
                      max = 2024,
                      value = 2024,
                      step = 1,
                      animate = FALSE,
                      sep = ""
                    ),
                    plotlyOutput("state_plot"),
                    plotOutput("hospitalization_amputation_plot")
            ),
            
            # Top Industries Tab
            tabItem(tabName = "industries",
                    h2("Top Industries Analysis",
                       style = "font-weight: bold; font-size: 3.5rem; text-align: center;"),
                    sliderInput(
                      "year_industry",
                      "Select Year:",
                      min = 2015,
                      max = 2024,
                      value = 2024,
                      step = 1,
                      animate = FALSE,
                      sep = ""
                    ),
                    fluidRow(
                      column(12, style = "margin-top: 20px;", 
                             downloadButton("download_industry_plot", "Download Filtered Data", 
                                            style = "float: right;"))
                    ),
                    plotOutput("industry_pie_chart"),
                    h4("Complete List of all Industries", style = "margin-top: 20px; font-weight: bold; text-align: left;"),
                    tableOutput("industry_table")
            ),
            
            # Top States Tab
            tabItem(
              tabName = "states",
              h2("Top States Analysis",
                 style = "font-weight: bold; font-size: 3.5rem; text-align: center;"),
              sliderInput(
                "year_state",
                "Select Year:",
                min = 2015,
                max = 2024,
                value = 2024,
                step = 1,
                animate = FALSE,
                sep = ""
              ),
              fluidRow(
                column(12, style = "margin-top: 20px;", 
                       downloadButton("download_top_states_plot", "Download Filtered Data", 
                                      style = "float: right;"))
              ),
              plotOutput("top_states_plot"),
              plotOutput("top_states_amputation_hospitalization"),
              h4("Complete List of States", style = "margin-top: 20px; font-weight: bold; text-align: left;"),
              tableOutput("state_table")
            ),
            
            # Event Categories Tab
            tabItem(
              tabName = "events",
              h2("Event Categories Analysis",
                 style = "font-weight: bold; font-size: 3.5rem; text-align: center;"),
              sliderInput(
                "year_event",
                "Select Year:",
                min = 2015,
                max = 2024,
                value = 2024,
                step = 1,
                animate = FALSE,
                sep = ""
              ),
              selectInput(
                "industry_event",
                "Select Industry:",
                choices = c("All Industries", unique(severe_injuries_data$industry)),
                selected = "All Industries"
              ),
              fluidRow(
                column(12, style = "margin-top: 20px;", 
                       downloadButton("download_event_category_plot", "Download Filtered Data", 
                                      style = "float: right;"))
              ),
              plotOutput("event_category_pie_chart"),
              h4("Complete List of Events leading to Injuries", style = "margin-top: 20px; font-weight: bold; text-align: left;"),
              tableOutput("event_category_table")
            ),
            
            # Source Analysis Tab
            tabItem(
              tabName = "source",
              h2("Source Categories Analysis",
                 style = "font-weight: bold; font-size: 3.5rem; text-align: center;"),
              sliderInput(
                "year_source",
                "Select Year:",
                min = 2015,
                max = 2024,
                value = 2024,
                step = 1,
                animate = FALSE,
                sep = "" 
              ),
              selectInput(
                "industry_source",
                "Select Industry:",
                choices = c("All Industries", unique(severe_injuries_data$industry)),
                selected = "All Industries"
              ),
              fluidRow(
                column(12, style = "margin-top: 20px;", 
                       downloadButton("download_source_category_plot", "Download Filtered Data", 
                                      style = "float: right;"))
              ),
              plotOutput("source_category_pie_chart"),
              h4("Complete List of all Injury Sources", style = "margin-top: 20px; font-weight: bold; text-align: left;"),
              tableOutput("source_category_table")
                    
            ),
            # Body Part Analysis Tab
            tabItem(tabName = "bodypart",
                    h2("Body Part Analysis",
                       style = "font-weight: bold; font-size: 3.5rem; text-align: center;"),
                    sliderInput(
                      "year_body_part",
                      "Select Year:",
                      min = 2015,
                      max = 2024,
                      value = 2024,
                      step = 1,
                      animate = FALSE,
                      sep = ""
                    ),
                    selectInput(
                      "industry_body_part",
                      "Select Industry:",
                      choices = c("All Industries", unique(severe_injuries_data$industry)),
                      selected = "All Industries"
                    ),
                    fluidRow(
                      column(12, style = "margin-top: 20px;", 
                             downloadButton("download_body_part_plot", "Download Filtered Data", 
                                            style = "float: right;"))
                    ),
                    plotOutput("body_part_pie_chart"),
                    h4("Complete List of all Body Parts Injured", style = "margin-top: 20px; font-weight: bold; text-align: left;"),
                    tableOutput("body_part_table")
                    
            ),
            # Trend Overview Tab
            tabItem(
              tabName = "trend",
              h2("Trend Overview",
                 style = "font-weight: bold; font-size: 3.5rem; text-align: center;"),
              selectInput(
                "industry_trend",
                "Select Industry:",
                choices = c("All Industries", unique(severe_injuries_data$industry)),
                selected = "All Industries"
              ),
              fluidRow(
                column(12, style = "margin-top: 20px;", 
                       downloadButton("download_trend_plot", "Download Filtered Data", 
                                      style = "float: right;"))
              ),
              plotlyOutput("line_plot")
            ),
            
            # Interactive Filter Tab
            tabItem(
              tabName = "filtering",
              h2("Interactive Filters for Severe Injuries",
                 style = "font-weight: bold; font-size: 3.5rem; text-align: center;"),
              
              # Filters
              fluidRow(
                column(4, sliderInput("year_filter", "Select Year:", 
                                      min = 2015,
                                      max = 2024,
                                      value = 2024,
                                      step = 1,
                                      animate = FALSE,
                                      sep = "")),
                column(4, selectInput("state_filter", "Select State:", 
                                      choices = c("All States", unique(severe_injuries_data$state)), 
                                      selected = "All States")),
                column(4, selectInput("industry_filter", "Select Industry:", 
                                      choices = c("All Industries", unique(severe_injuries_data$industry)), 
                                      selected = "All Industries"))
              ),
              fluidRow(
                column(4, selectInput("hospitalized_filter", "Hospitalized:", 
                                      choices = c("Yes", "No"), 
                                      selected = "Yes")),
                column(4, selectInput("amputation_filter", "Amputation:", 
                                      choices = c("Yes", "No"), 
                                      selected = "Yes")),
                column(4, selectInput("nature_filter", "Nature Category:", 
                                      choices = c("All", unique(severe_injuries_data$nature_category)), 
                                      selected = "All"))
              ),
              fluidRow(
                column(4, selectInput("body_part_filter", "Body Part:", 
                                      choices = c("All", unique(severe_injuries_data$body_part)), 
                                      selected = "All")),
                column(4, selectInput("event_filter", "Event Category:", 
                                      choices = c("All", unique(severe_injuries_data$event_category)), 
                                      selected = "All")),
                column(4, selectInput("source_filter", "Source Category:", 
                                      choices = c("All", unique(severe_injuries_data$source_category)), 
                                      selected = "All"))
              ),
              fluidRow(
                column(12, style = "margin-top: 20px;", 
                       downloadButton("download_data", "Download Filtered Data", 
                                      style = "float: right;"))
              ),
              DTOutput("filtered_table")
            )
          )
        )
      )
    )
  ),
  # Occupational Illness Page
  hidden(
    div(
      id = "occ-page",
      dashboardPage(
        dashboardHeader(
          title = div(
            actionButton(
              inputId = "back_button_occ",
              label = NULL,
              icon = icon("arrow-left"),
              style = "margin-right: 0px; border: none; background-color: transparent; color: white;"
            ),
            span("Occupational Illness", style = "font-size: 19px; padding-left: 0px;")
          )
        ),
        dashboardSidebar(
          sidebarMenu(
            menuItem("Illness by State", tabName = "state_occ", icon = icon("map-marker-alt")),
            menuItem("Top Industries", tabName = "industries_occ", icon = icon("building")),
            menuItem("Top States", tabName = "states_occ", icon = icon("flag-checkered")),
            menuItem("Key Injury Metrics by Industry", tabName = "filter_industry_occ", icon = icon("industry")),
            menuItem("Interactive Filters", tabName = "filtering_occ", icon = icon("filter"))
          )
        ),
        dashboardBody(
          tabItems(
            # Injuries by State Tab
            tabItem(tabName = "state_occ",
                    div(
                      h2("Illnesses by State Analysis", 
                         style = "font-weight: bold; font-size: 3.5rem; text-align: center;")
                    ),
                    sliderInput(
                      "year_occ",
                      "Select Year:",
                      min = 2016,
                      max = 2022,
                      value = 2022,
                      step = 1,
                      animate = FALSE,
                      sep = ""
                    ),
                    plotlyOutput("state_plot_occ")
            ),
            # Top Industries Tab
            tabItem(
              tabName = "industries_occ",
              h2("Top Industries Analysis",
                 style = "font-weight: bold; font-size: 3.5rem; text-align: center;"),
              sliderInput(
                "year_industry_occ",
                "Select Year:",
                min = 2016,
                max = 2022,
                value = 2022,
                step = 1,
                animate = FALSE,
                sep = ""
              ),
              fluidRow(
                column(12, style = "margin-top: 20px;", 
                       downloadButton("download_industry_plot_occ", "Download Filtered Data", 
                                      style = "float: right;"))
              ),
              plotOutput("industry_pie_chart_occ"),
              h4("Complete List of all Industries", style = "margin-top: 20px; font-weight: bold; text-align: left;"),
              tableOutput("industry_table_occ")
            ),
            
            # Top States Tab
            tabItem(
              tabName = "states_occ",
              h2("Top States Analysis",
                 style = "font-weight: bold; font-size: 3.5rem; text-align: center;"),
              sliderInput(
                "year_state_occ",
                "Select Year:",
                min = 2016,
                max = 2022,
                value = 2022,
                step = 1,
                animate = FALSE,
                sep = ""
              ),
              fluidRow(
                column(12, style = "margin-top: 20px;", 
                       downloadButton("download_top_states_plot_occ", "Download Filtered Data", 
                                      style = "float: right;"))
              ),
              plotOutput("top_states_plot_occ"),
              h4("Complete List of States", style = "margin-top: 20px; font-weight: bold; text-align: left;"),
              tableOutput("states_table_occ")
                    
            ),
            # Key Injury Metrics by Industry Tab
            tabItem(
              tabName = "filter_industry_occ",
              h2("Key Injury Metrics by Industry",
                 style = "font-weight: bold; font-size: 3.5rem; text-align: center;"),
              sliderInput(
                "year_filter_industry_occ",
                "Select Year:",
                min = 2016,
                max = 2022,
                value = 2022,
                step = 1,
                animate = FALSE,
                sep = ""
              ),
              selectInput(
                "select_industry_occ",
                "Select Industry:",
                choices = c("All Industries", unique(occ_injuries_data$industry)),
                selected = "All Industries",
                multiple = FALSE
              ),
              uiOutput("dynamic_title"),
              fluidRow(
                column(12, style = "margin-top: 20px;", 
                       downloadButton("download_industry_interactive_plots", "Download Filtered Data", 
                                      style = "float: right;"))
              ),
              fluidRow(
                column(4, plotOutput("illness_employees_plot_occ")),
                column(4, plotOutput("illness_deaths_plot_occ")),
                column(4, plotOutput("illness_cases_plot_occ"))
              ),
              fluidRow(
                column(4, plotOutput("illness_days_plot_occ")),
                column(8, plotOutput("illness_categories_plot_occ"))
              )
                    
            ),
            # Interactive Filters Tab
            tabItem(
              tabName = "filtering_occ",
              h2("Interactive Filters for Occupational Illness",
                 style = "font-weight: bold; font-size: 3.5rem; text-align: center;"),
              
              # Filters for the page
              fluidRow(
                column(
                  4,
                  sliderInput(
                    "year_filter_occ",
                    "Select Year:",
                    min = 2016,
                    max = 2022,
                    value = 2022,
                    step = 1,
                    sep = ""
                  )
                ),
                column(
                  4,
                  selectInput(
                    "industry_filter_occ",
                    "Select Industry:",
                    choices = c("All Industries", unique(occ_injuries_data$industry)),
                    selected = "All Industries"
                  )
                ),
                column(
                  4,
                  selectInput(
                    "state_filter_occ",
                    "Select State:",
                    choices = c("All States", unique(occ_injuries_data$state)),
                    selected = "All States"
                  )
                )
              ),
              fluidRow(
                column(
                  4,
                  selectInput(
                    "illness_injuries_filter_occ",
                    "Illness:",
                    choices = c("Yes", "No"),
                    selected = "Yes"
                  )
                ),
                column(
                  4,
                  selectInput(
                    "respiratory_conditions_filter_occ",
                    "Respiratory Conditions:",
                    choices = c("No", "Yes"),
                    selected = "No"
                  )
                ),
                column(
                  4,
                  selectInput(
                    "skin_disorder_filter_occ",
                    "Skin Disorder:",
                    choices = c("Yes", "No"),
                    selected = "No"
                  )
                )
              ),
              fluidRow(
                column(
                  4,
                  selectInput(
                    "hearing_loss_filter_occ",
                    "Hearing Loss:",
                    choices = c("Yes", "No"),
                    selected = "No"
                  )
                ),
                column(
                  4,
                  selectInput(
                    "other_illness_filter_occ",
                    "Other Illness:",
                    choices = c("Yes", "No"),
                    selected = "No"
                  )
                )
              ),
              fluidRow(
                column(12, style = "margin-top: 20px;", 
                       downloadButton("download_occ_data", "Download Filtered Data", 
                                      style = "float: right;"))
              ),
              DTOutput("filtered_table_occ")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Show dashboard page when 'Get Started' button is clicked
  observeEvent(input$get_started_btn, {
    hide("welcome-page")
    show("dashboard-page")
  })
  
  # Show severe injuries page when 'Severe Injuries' button is clicked
  observeEvent(input$severe_btn, {
    hide("dashboard-page")
    show("severe-page")
  })
  
  # Show occupational injuries page when 'Occupational Injuries' button is clicked
  observeEvent(input$occupational_btn, {
    hide("dashboard-page")
    show("occ-page")
  })
  
  # Handle Back Button on Severe Injuries page
  observeEvent(input$back_button, {
    show("dashboard-page")
    hide("severe-page")
  })
  
  # Handle Back Button on Occupational Injuries page
  observeEvent(input$back_button_occ, {
    show("dashboard-page")
    hide("occ-page")
  })
  
  # SEVERE INJURIES
  
  # Injuries by State Analysis
  
  # Map plot for total injuries across all the states in US 
  output$state_plot <- renderPlotly({
    # Filter and summarize data by state and year
    state_summary <- severe_injuries_data |>
      filter(year == input$year) |>
      group_by(state) |>
      summarise(Total_Injuries = n(), .groups = "drop")
    
    # Convert state names to lowercase for map matching
    state_summary$state <- tolower(state_summary$state)
    
    # Load US state map data
    us_states <- map_data("state")
    
    # Combine map data with injury data
    map_data_combined <- us_states |>
      left_join(state_summary, by = c("region" = "state"))
    
    # Create hover text for map
    map_data_combined$hover_text <- paste(
      "State:", map_data_combined$region,
      "<br>Total Injuries:", map_data_combined$Total_Injuries
    )
    
    # Create and customize the map plot
    p <- ggplot(map_data_combined, aes(
      x = long, y = lat, group = group, fill = Total_Injuries, text = hover_text
    )) +
      geom_polygon(color = "white") +
      coord_fixed(1.3) +
      scale_fill_gradient(low = "#336699", high = "#003366", na.value = "grey50") +
      labs(
        title = paste("Total Severe Injuries by State for", input$year),
        fill = "Total Injuries"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      )
    
    # Convert ggplot to Plotly for interactive features
    ggplotly(p, tooltip = "text") |>
      layout(
        hoverlabel = list(bgcolor = "white", font = list(size = 12)),
        hovermode = "closest"
      ) |>
      config(
        modeBarButtonsToRemove = c(
          "pan2d", "select2d", "lasso2d", "autoScale2d", "resetScale2d", 
          "hoverClosestCartesian", "hoverCompareCartesian", 
          "zoom2d", "sendDataToCloud"
        ),
        displaylogo = FALSE 
      )
  })
  
  # Visualization for hospitalization and amputation
  output$hospitalization_amputation_plot <- renderPlot({
    # Summarize data for hospitalizations and amputations based on the selected year
    summary_data <- severe_injuries_data |>
      filter(year == input$year) |>
      summarise(
        `Total Hospitalizations` = sum(hospitalization %in% c(1, 2, 3, 4, 5, 6), na.rm = TRUE),
        `No Hospitalization` = sum(hospitalization == 0, na.rm = TRUE),
        `Total Amputations` = sum(amputation %in% c(1, 2), na.rm = TRUE),
        `No Amputation` = sum(amputation == 0, na.rm = TRUE)
      ) |>
      tidyr::pivot_longer(cols = c(`Total Hospitalizations`, `No Hospitalization`, `Total Amputations`, `No Amputation`), 
                          names_to = "Type", 
                          values_to = "Count")
    
    # Create bar plot for hospitalizations and amputations with counts for each category
    ggplot(summary_data, aes(x = Type, y = Count, fill = Type)) +
      geom_col(width = 0.6, show.legend = FALSE, alpha = 0.9) +
      geom_text(aes(label = Count), vjust = -0.5, size = 5, color = "black", fontface = "bold") +
      theme_minimal() +
      labs(
        title = paste("Total Hospitalizations and Amputations in", input$year),
        x = NULL,
        y = "Total Count"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      scale_fill_manual(values = c(
        "Total Hospitalizations" = "#003366", 
        "No Hospitalization" = "#336699", 
        "Total Amputations" = "#4d4d4d", 
        "No Amputation" = "#99ccff"
      )) +
      coord_cartesian(clip = "off")
  })
  
  # Top Industries Analysis
  
  # Render pie chart of the top 5 industries with the highest number of severe injuries
  output$industry_pie_chart <- renderPlot({
    # Filter data based on selected year, group by industry, and calculate total injuries per industry
    industry_data <- severe_injuries_data |>
      filter(year == input$year_industry) |>
      group_by(industry) |>
      summarise(Total_Injuries = n()) |>
      arrange(desc(Total_Injuries)) |>
      mutate(Percentage = round((Total_Injuries / sum(Total_Injuries)) * 100, 1)) |>
      slice(1:5)
    
    custom_colors <- c("#336699", "#003366", "#4d4d4d", "#99ccff", "#1a1a1a")
    
    # Create pie chart with custom colors and labels
    ggplot(industry_data, aes(x = "", y = Total_Injuries, fill = industry)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar(theta = "y") +
      geom_text(aes(label = paste0(Percentage, "%")),
                position = position_stack(vjust = 0.5), size = 5, color = "white") +
      labs(
        title = paste("Top 5 Industries for Severe Injuries in", input$year_industry),
        fill = "Industry"
      ) +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 10)
      ) +
      scale_fill_manual(values = custom_colors)
  })
  
  # Render table showing total injuries and percentage of injuries by industry
  output$industry_table <- renderTable({
    # Calculate total number of injuries for the selected year
    total_injuries <- severe_injuries_data |>
      filter(year == input$year_industry) |>
      summarise(Total_Injuries = n()) |>
      pull(Total_Injuries)
    
    # Summarize injuries by industry, calculate percentage for each industry
    industry_data <- severe_injuries_data |>
      filter(year == input$year_industry) |>
      group_by(industry) |>
      summarise(Total_Injuries = n()) |>
      arrange(desc(Total_Injuries)) |>
      mutate(Percentage = round((Total_Injuries / total_injuries) * 100, 1)) |>
      rename(
        `Industry` = industry,
        `Total Injuries` = Total_Injuries,
        `Percentage (%)` = Percentage
      )
    # Return the summarized data as a table
    industry_data
  })
  
  # Download the top industries pie chart as a PNG image
  output$download_industry_plot <- downloadHandler(
    # Define filename format for the downloaded image
    filename = function() {
      paste("top_industries_pie_chart_", Sys.time(), ".png", sep = "")
    },
    content = function(file) {
      # Generate the top 5 industries data for the pie chart
      industry_data <- severe_injuries_data |>
        filter(year == input$year_industry) |>
        group_by(industry) |>
        summarise(Total_Injuries = n()) |>
        arrange(desc(Total_Injuries)) |>
        mutate(Percentage = round((Total_Injuries / sum(Total_Injuries)) * 100, 1)) |>
        slice(1:5)
      
      custom_colors <- c("#336699", "#003366", "#4d4d4d", "#99ccff", "#1a1a1a")
      
      # Create pie chart plot
      p <- ggplot(industry_data, aes(x = "", y = Total_Injuries, fill = industry)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar(theta = "y") +
        geom_text(aes(label = paste0(Percentage, "%")),
                  position = position_stack(vjust = 0.5), size = 5, color = "white") +
        labs(
          title = paste("Top 5 Industries for Severe Injuries in", input$year_industry),
          fill = "Industry"
        ) +
        theme_void() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
          legend.title = element_text(face = "bold"),
          legend.text = element_text(size = 10)
        ) +
        scale_fill_manual(values = custom_colors)
      
      # Save the plot as a PNG file
      ggsave(file, plot = p, width = 8, height = 6, dpi = 300)
    }
  )
  
  # Top States Analysis
  
  # Render a horizontal bar plot showing the top 5 states with the highest total injuries
  output$top_states_plot <- renderPlot({
    # Summarize the data to get the top 5 states with the highest total injuries for the selected year
    top_states <- severe_injuries_data |>
      filter(year == input$year_state) |>
      group_by(state) |>
      summarise(Total_Injuries = n()) |>
      arrange(desc(Total_Injuries)) |>
      slice(1:5) |>
      mutate(state = stringr::str_to_title(state)) # Capitalize the first letter of each state
    
    # Create a horizontal bar plot of the top 5 states with the highest total injuries
    ggplot(top_states, aes(x = reorder(state, Total_Injuries), y = Total_Injuries, fill = state)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(
        title = paste("Top 5 States by Total Injuries in", input$year_state),
        x = "State",
        y = "Total Injuries"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      scale_fill_manual(values = c("#336699", "#003366", "#4d4d4d", "#99ccff", "#1a1a1a"))
  })
  
  # Render a horizontal bar plot showing total amputations and hospitalizations by state
  output$top_states_amputation_hospitalization <- renderPlot({
    # Summarize data for total amputations and hospitalizations by state for the selected year
    states_amputation_hospitalization <- severe_injuries_data |>
      filter(year == input$year_state) |>
      group_by(state) |>
      summarise(
        Total_Amputations = sum(amputation %in% c(1, 2), na.rm = TRUE),
        Total_Hospitalizations = sum(hospitalization > 0, na.rm = TRUE)
      ) |>
      arrange(desc(Total_Hospitalizations)) |>
      slice(1:5) |>
      mutate(state = stringr::str_to_title(state))
    
    # Reshape the data to long format for plotting
    melted_data <- states_amputation_hospitalization |>
      pivot_longer(cols = c(Total_Amputations, Total_Hospitalizations), names_to = "Type", values_to = "Count")
    
    # Create a side-by-side bar plot comparing total amputations and hospitalizations across the top 5 states
    ggplot(melted_data, aes(x = reorder(state, Count), y = Count, fill = Type)) +
      geom_col(position = "dodge") +
      coord_flip() +
      labs(
        title = paste("Top States for Amputations and Hospitalizations in", input$year_state),
        x = "State",
        y = "Count",
        fill = "Type"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      scale_fill_manual(values = c(
        "Total_Amputations" = "#1f77b4",
        "Total_Hospitalizations" = "#595959"
      ))
  })
  
  # Render a table for total injuries, amputations, and hospitalizations by state
  output$state_table <- renderTable({
    # Summarize data for total injuries, amputations, and hospitalizations by state for the selected year
    states_table <- severe_injuries_data |>
      filter(year == input$year_state) |>
      group_by(state) |>
      summarise(
        Total_Injuries = n(),
        Total_Amputations = sum(amputation %in% c(1, 2), na.rm = TRUE),
        Total_Hospitalizations = sum(hospitalization > 0, na.rm = TRUE)
      ) |>
      arrange(desc(Total_Injuries)) |>
      mutate(state = stringr::str_to_title(state)) |>
      rename(
        `State` = state,
        `Total Injuries` = Total_Injuries,
        `Total Amputations` = Total_Amputations,
        `Total Hospitalizations` = Total_Hospitalizations
      )
    
    # Display the summarized data as a table
    states_table
  })
  
  #Body Part Analysis
  
  #Plot the top 5 body parts with the most injuries for the selected year and industry (if specified)
  output$body_part_pie_chart <- renderPlot({
    # Filter data based on selected year and industry, then group by body part
    body_part_data <- severe_injuries_data |>
      filter(
        year == input$year_body_part,
        if (input$industry_body_part != "All Industries") industry == input$industry_body_part else TRUE
      ) |>
      group_by(body_part) |>
      summarise(Total = n()) |>
      arrange(desc(Total)) |>
      mutate(Percentage = round((Total / sum(Total)) * 100, 1)) |>
      slice(1:5)
    
    custom_colors <- c("#336699", "#003366", "#4d4d4d", "#99ccff", "#1a1a1a")
    
    # Create a pie chart displaying the top 5 body parts and their respective percentages
    ggplot(body_part_data, aes(x = "", y = Total, fill = body_part)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      geom_text(aes(label = paste0(Percentage, "%")),
                position = position_stack(vjust = 0.5), size = 5, color = "white") +
      labs(
        title = paste("Top 5 Body Parts in", input$year_body_part,
                      if (input$industry_body_part != "All Industries") paste("for", input$industry_body_part) else ""),
        fill = "Body Part"
      ) +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 10)
      ) +
      scale_fill_manual(values = custom_colors)
  })
  
  # Create and display a table showing the total injuries and percentage for the top 5 body parts
  output$body_part_table <- renderTable({
    body_part_data <- severe_injuries_data |>
      filter(
        year == input$year_body_part,
        if (input$industry_body_part != "All Industries") industry == input$industry_body_part else TRUE
      ) |>
      group_by(body_part) |>
      summarise(Total = n()) |>
      arrange(desc(Total)) |>
      mutate(Percentage = round((Total / sum(Total)) * 100, 1)) |>
      rename(
        `Body Part` = body_part,
        `Total Injuries` = Total,
        `Percentage (%)` = Percentage
      )
    
    # Return the summarized data as a table
    body_part_data
  })
  
  # Download the body part pie chart as a PNG file
  output$download_body_part_plot <- downloadHandler(
    filename = function() {
      paste("body_part_pie_chart_", Sys.time(), ".png", sep = "")
    },
    content = function(file) {
      # Filter and summarize the data for the pie chart
      body_part_data <- severe_injuries_data |>
        filter(
          year == input$year_body_part,
          if (input$industry_body_part != "All Industries") industry == input$industry_body_part else TRUE
        ) |>
        group_by(body_part) |>
        summarise(Total = n()) |>
        arrange(desc(Total)) |>
        mutate(Percentage = round((Total / sum(Total)) * 100, 1)) |>
        slice(1:5)
      
      custom_colors <- c("#336699", "#003366", "#4d4d4d", "#99ccff", "#1a1a1a")
      
      # Create the pie chart plot
      p <- ggplot(body_part_data, aes(x = "", y = Total, fill = body_part)) +
        geom_col(width = 1) +
        coord_polar(theta = "y") +
        geom_text(aes(label = paste0(Total, " (", Percentage, "%)")),
                  position = position_stack(vjust = 0.5), size = 5, color = "white") +
        labs(
          title = paste("Top 5 Body Parts in", input$year_body_part,
                        if (input$industry_body_part != "All Industries") paste("for", input$industry_body_part) else ""),
          fill = "Body Part"
        ) +
        theme_void() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
          legend.title = element_text(face = "bold"),
          legend.text = element_text(size = 10)
        ) +
        scale_fill_manual(values = custom_colors)
      
      # Save the plot as a PNG file
      ggsave(file, plot = p, width = 8, height = 6, dpi = 300)
    }
  )
  
  # Event Categories Analysis
  
  # Plot the top 5 event categories with the most injuries for the selected year and industry (if specified)
  output$event_category_pie_chart <- renderPlot({
    # Filter data based on selected year and industry, then group by event category
    event_data <- severe_injuries_data |>
      filter(
        year == input$year_event,
        if (input$industry_event != "All Industries") industry == input$industry_event else TRUE
      ) |>
      group_by(event_category) |>
      summarise(Total = n()) |>
      arrange(desc(Total)) |>
      mutate(Percentage = round((Total / sum(Total)) * 100, 1)) |>
      slice(1:5)
    
    custom_colors <- c("#336699", "#003366", "#4d4d4d", "#99ccff", "#1a1a1a")
    
    # Create a pie chart displaying the top 5 event categories and their respective percentages
    ggplot(event_data, aes(x = "", y = Total, fill = event_category)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      geom_text(aes(label = paste0(Percentage, "%")),
                position = position_stack(vjust = 0.5), size = 5, color = "white") +
      labs(
        title = paste("Top 5 Event Categories in", input$year_event,
                      if (input$industry_event != "All Industries") paste("for", input$industry_event) else ""),
        fill = "Event Category"
      ) +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 10)
      ) +
      scale_fill_manual(values = custom_colors)
  })
  
  # Create and display a table showing the total injuries and percentage for the top 5 event categories
  output$event_category_table <- renderTable({
    event_data <- severe_injuries_data |>
      filter(
        year == input$year_event,
        if (input$industry_event != "All Industries") industry == input$industry_event else TRUE
      ) |>
      group_by(event_category) |>
      summarise(Total = n()) |>
      arrange(desc(Total)) |>
      mutate(Percentage = round((Total / sum(Total)) * 100, 1)) |>
      rename(
        `Event Category` = event_category,
        `Total Injuries` = Total,
        `Percentage (%)` = Percentage
      )
    
    # Return the summarized data as a table
    event_data
  })
  
  # Download the event category pie chart as a PNG file
  output$download_event_category_plot <- downloadHandler(
    filename = function() {
      paste("event_category_pie_chart_", Sys.time(), ".png", sep = "")
    },
    content = function(file) {
      # Filter and summarize the data for the pie chart
      event_data <- severe_injuries_data |>
        filter(
          year == input$year_event,
          if (input$industry_event != "All Industries") industry == input$industry_event else TRUE
        ) |>
        group_by(event_category) |>
        summarise(Total = n()) |>
        arrange(desc(Total)) |>
        mutate(Percentage = round((Total / sum(Total)) * 100, 1)) |>
        slice(1:5)
      
      custom_colors <- c("#336699", "#003366", "#4d4d4d", "#99ccff", "#1a1a1a")
      
      # Create the pie chart plot
      p <- ggplot(event_data, aes(x = "", y = Total, fill = event_category)) +
        geom_col(width = 1) +
        coord_polar(theta = "y") +
        geom_text(aes(label = paste0(Total, " (", Percentage, "%)")),
                  position = position_stack(vjust = 0.5), size = 5, color = "white") +
        labs(
          title = paste("Top 5 Event Categories in", input$year_event,
                        if (input$industry_event != "All Industries") paste("for", input$industry_event) else ""),
          fill = "Event Category"
        ) +
        theme_void() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
          legend.title = element_text(face = "bold"),
          legend.text = element_text(size = 10)
        ) +
        scale_fill_manual(values = custom_colors)
      
      # Save the plot as a PNG file
      ggsave(file, plot = p, width = 8, height = 6, dpi = 300)
    }
  )
  
  # Source Categories Analysis
  
  # Plot the top 5 source categories with the most injuries for the selected year and industry (if specified)
  output$source_category_pie_chart <- renderPlot({
    # Filter data based on selected year and industry, then group by source category
    source_data <- severe_injuries_data |>
      filter(
        year == input$year_source,
        if (input$industry_source != "All Industries") industry == input$industry_source else TRUE
      ) |>
      group_by(source_category) |>
      summarise(Total = n()) |>
      arrange(desc(Total)) |>
      mutate(Percentage = round((Total / sum(Total)) * 100, 1)) |>
      slice(1:5)
    
    custom_colors <- c("#336699", "#003366", "#4d4d4d", "#99ccff", "#1a1a1a")
    
    # Create a pie chart displaying the top 5 source categories and their respective percentages
    ggplot(source_data, aes(x = "", y = Total, fill = source_category)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      geom_text(aes(label = paste0(Percentage, "%")),
                position = position_stack(vjust = 0.5), size = 5, color = "white") +
      labs(
        title = paste("Top 5 Source Categories in", input$year_source,
                      if (input$industry_source != "All Industries") paste("for", input$industry_source) else ""),
        fill = "Source Category"
      ) +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 10)
      ) +
      scale_fill_manual(values = custom_colors)
  })
  
  # Create and display a table showing the total injuries and percentage for the top 5 source categories
  output$source_category_table <- renderTable({
    # Filter data based on selected year and industry, then group by source category
    source_data <- severe_injuries_data |>
      filter(
        year == input$year_source,
        if (input$industry_source != "All Industries") industry == input$industry_source else TRUE
      ) |>
      group_by(source_category) |>
      summarise(Total = n()) |>
      arrange(desc(Total)) |>
      mutate(Percentage = round((Total / sum(Total)) * 100, 1)) |>
      rename(
        `Source Category` = source_category,
        `Total Injuries` = Total,
        `Percentage (%)` = Percentage
      )
    
    # Return the summarized data as a table
    source_data
  })
  
  # Download the source category pie chart as a PNG file
  output$download_source_category_plot <- downloadHandler(
    filename = function() {
      paste("source_category_pie_chart_", Sys.time(), ".png", sep = "")
    },
    content = function(file) {
      source_data <- severe_injuries_data |>
        filter(
          year == input$year_source,
          if (input$industry_source != "All Industries") industry == input$industry_source else TRUE
        ) |>
        group_by(source_category) |>
        summarise(Total = n()) |>
        arrange(desc(Total)) |>
        mutate(Percentage = round((Total / sum(Total)) * 100, 1)) |>
        slice(1:5)
      
      custom_colors <- c("#336699", "#003366", "#4d4d4d", "#99ccff", "#1a1a1a")
      
      # Create the pie chart plot
      p <- ggplot(source_data, aes(x = "", y = Total, fill = source_category)) +
        geom_col(width = 1) +
        coord_polar(theta = "y") +
        geom_text(aes(label = paste0(Total, " (", Percentage, "%)")),
                  position = position_stack(vjust = 0.5), size = 5, color = "white") +
        labs(
          title = paste("Top 5 Source Categories in", input$year_source,
                        if (input$industry_source != "All Industries") paste("for", input$industry_source) else ""),
          fill = "Source Category"
        ) +
        theme_void() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
          legend.title = element_text(face = "bold"),
          legend.text = element_text(size = 10)
        ) +
        scale_fill_manual(values = custom_colors)
      
      # Save the plot as a PNG file
      ggsave(file, plot = p, width = 8, height = 6, dpi = 300)
    }
  )
  
  # Trend Overview
  
  # Plot trend overview of injuries over years
  output$line_plot <- renderPlotly({
    # Filter data based on selected year and industry, then group by year
    trend_data <- severe_injuries_data |>
      filter(if (input$industry_trend != "All Industries") industry == input$industry_trend else TRUE) |>
      group_by(year) |>
      summarise(
        Total_Injuries = n(),
        Total_Hospitalized = sum(hospitalization > 0, na.rm = TRUE),
        Total_Amputations = sum(amputation > 0, na.rm = TRUE)
      ) |>
      pivot_longer(
        cols = c(Total_Injuries, Total_Hospitalized, Total_Amputations), 
        names_to = "Metric", 
        values_to = "Count"
      )
    
    trend_data$year <- as.factor(trend_data$year)
    
    # Create a plot displaying the trend overview of injuries over the years
    p <- ggplot(trend_data, aes(x = year, y = Count, color = Metric, group = Metric)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      theme_minimal(base_size = 14) +
      labs(
        title = paste("Trend Over Years", 
                      if (input$industry_trend != "All Industries") paste("for", input$industry_trend) else "for All Industries"),
        x = "Year",
        y = "Count",
        color = "Metric"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(face = "bold", size = 8),
        legend.text = element_text(size = 8)
      ) +
      scale_color_manual(values = c(
        "Total_Injuries" = "#336699",
        "Total_Hospitalized" = "#4d4d4d",
        "Total_Amputations" = "#003366"
      ))
    
    ggplotly(p, tooltip = c("x", "y", "color")) |>
      config(displayModeBar = FALSE)
  })
  
  # Download the trend overview line plot as a PNG file
  output$download_trend_plot <- downloadHandler(
    filename = function() {
      # Generate the file name with timestamp
      paste("trend_overview_", Sys.time(), ".png", sep = "")
    },
    content = function(file) {
      # Filter and summarize the data for the line graph
      trend_data <- severe_injuries_data |>
        filter(if (input$industry_trend != "All Industries") industry == input$industry_trend else TRUE) |>
        group_by(year) |>
        summarise(
          Total_Injuries = n(),
          Total_Hospitalized = sum(hospitalization > 0, na.rm = TRUE),
          Total_Amputations = sum(amputation > 0, na.rm = TRUE)
        ) |>
        pivot_longer(
          cols = c(Total_Injuries, Total_Hospitalized, Total_Amputations), 
          names_to = "Metric", 
          values_to = "Count"
        )
      
      trend_data$year <- as.factor(trend_data$year)
      
      # Create the line graph
      p <- ggplot(trend_data, aes(x = year, y = Count, color = Metric, group = Metric)) +
        geom_line(size = 0.3) +
        geom_point(size = 0.5) +
        theme_minimal(base_size = 8) +
        labs(
          title = paste("Trend Over Years", 
                        if (input$industry_trend != "All Industries") paste("for", input$industry_trend) else ""),
          x = "Year",
          y = "Count",
          color = "Metric"
        ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          axis.title.x = element_text(face = "bold", size = 10),
          axis.title.y = element_text(face = "bold", size = 10),
          axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 8),
          legend.title = element_text(face = "bold", size = 8),
          legend.text = element_text(size = 8) 
        )+
        scale_color_manual(values = c(
          "Total_Injuries" = "#336699",
          "Total_Hospitalized" = "#4d4d4d",
          "Total_Amputations" = "#003366"
        ))
      
      # Save the plot as a PNG file
      ggsave(file, plot = p, width = 10, height = 6)
    }
  )
  
  # Interactive Filters for Severe Injuries
  
  # Reactive expression for filtering the data based on user inputs
  filtered_data <- reactive({
    data <- severe_injuries_data
    
    # Filter data by year
    data <- data |> filter(year == input$year_filter)
    
    # Filter by state if a specific state is selected
    if (input$state_filter != "All States") {
      data <- data |> filter(state == input$state_filter)
    }
    
    # Filter by industry if a specific industry is selected
    if (input$industry_filter != "All Industries") {
      data <- data |> filter(industry == input$industry_filter)
    }
    
    # Filter based on hospitalization status
    if (input$hospitalized_filter == "Yes") {
      data <- data |> filter(hospitalization %in% 1:6)
    } else {
      data <- data |> filter(hospitalization == 0)
    }
    
    # Filter based on amputation status
    if (input$amputation_filter == "Yes") {
      data <- data |> filter(amputation %in% 1:2)
    } else {
      data <- data |> filter(amputation == 0)
    }
    
    # Filter by nature of the injury if a specific category is selected
    if (input$nature_filter != "All") {
      data <- data |> filter(nature_category == input$nature_filter)
    }
    
    # Filter by affected body part if a specific category is selected
    if (input$body_part_filter != "All") {
      data <- data |> filter(body_part == input$body_part_filter)
    }
    
    # Filter by event category if a specific category is selected
    if (input$event_filter != "All") {
      data <- data |> filter(event_category == input$event_filter)
    }
    
    # Filter by source category if a specific category is selected
    if (input$source_filter != "All") {
      data <- data |> filter(source_category == input$source_filter)
    }
    
    # Format the state and city columns to have only the first letter in uppercase
    data <- data |> 
      mutate(
        state = stringr::str_to_title(state),
        city = stringr::str_to_title(city)
      )
    
    # Return the filtered dataset
    data
  })
  
  # Render a data table with the filtered data
  output$filtered_table <- renderDT({
    datatable(
      filtered_data(), 
      options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Download handler for exporting the filtered data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("filtered_data_", Sys.time(), ".csv", sep = "")
    },
    content = function(file) {
      # Write the filtered data to a CSV file
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # OCCUPATIONAL ILLNESS
  
  # Illnesses by State Analysis
  output$state_plot_occ <- renderPlotly({
    # Filter the data by the selected year and calculate the total injuries for each state
    state_summary_occ <- occ_injuries_data |>
      filter(year == input$year_occ) |>
      group_by(state) |>
      summarise(Total_Injuries = sum(total_injuries, na.rm = TRUE), .groups = "drop")
      
    # Load US state map data
    us_states <- map_data("state")
    
    # Merge the summarized injury data with the US map data by matching the "state" and "region" columns
    map_data_combined <- us_states |>
      left_join(state_summary_occ, by = c("region" = "state"))
    
    # Create hover text for displaying additional information when hovering over a state in the map
    map_data_combined$hover_text <- paste(
      "State:", map_data_combined$region,
      "<br>Total Injuries:", map_data_combined$Total_Injuries
    )
    
    # Create the map plot using ggplot
    p <- ggplot(map_data_combined, aes(
      x = long, y = lat, group = group, fill = Total_Injuries, text = hover_text
    )) +
      geom_polygon(color = "white") +
      coord_fixed(1.3) +
      scale_fill_gradient(low = "#336699", high = "#003366", na.value = "grey50") +
      labs(
        title = paste("Total Occupational Illness by State for", input$year_occ),
        fill = "Total Injuries"
      ) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(
          hjust = 0.5,
          size = 16,
          face = "bold"
        )
      )
    # Convert the ggplot to interactive plotly plot
    ggplotly(p, tooltip = "text") |>
      layout(
        hoverlabel = list(bgcolor = "white", font = list(size = 12)),
        hovermode = "closest"
      ) |>
      config(
        modeBarButtonsToRemove = c(
          "pan2d", "select2d", "lasso2d", "autoScale2d", "resetScale2d", 
          "hoverClosestCartesian", "hoverCompareCartesian", 
          "zoom2d", "sendDataToCloud"
        ),
        displaylogo = FALSE  # Hide the Plotly logo
      )
  })
  
  # Top Occupational Industries Analysis
  
  # Render pie chart of the top 5 industries with the highest number of occupational injuries
  output$industry_pie_chart_occ <- renderPlot({
    # Filter data based on selected year, group by industry, and calculate total injuries per industry
    industry_data <- occ_injuries_data |>
      filter(year == input$year_industry_occ) |>
      group_by(industry) |>
      summarise(Total_Injuries = n()) |>
      arrange(desc(Total_Injuries)) |>
      mutate(Percentage = round((Total_Injuries / sum(Total_Injuries)) * 100, 1)) |>
      slice(1:5)
    
    custom_colors <- c("#336699", "#003366", "#4d4d4d", "#99ccff", "#1a1a1a")
    
    # Create pie chart with custom colors and labels
    ggplot(industry_data, aes(x = "", y = Total_Injuries, fill = industry)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar(theta = "y") +
      geom_text(aes(label = paste0(Percentage, "%")),
                position = position_stack(vjust = 0.5), size = 5, color = "white") +
      labs(
        title = paste("Top 5 Industries for Occupational Injuries in", input$year_industry_occ),
        fill = "Industry"
      ) +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 10)
      ) +
      scale_fill_manual(values = custom_colors)
  })
  
  # Render table showing total injuries and percentage of injuries by industry
  output$industry_table_occ <- renderTable({
    # Calculate total number of injuries for the selected year
    total_injuries <- occ_injuries_data |>
      filter(year == input$year_industry_occ) |>
      summarise(Total_Injuries = n()) |>
      pull(Total_Injuries)
    
    # Summarize injuries by industry, calculate percentage for each industry
    industry_data <- occ_injuries_data |>
      filter(year == input$year_industry_occ) |>
      group_by(industry) |>
      summarise(Total_Injuries = n()) |>
      arrange(desc(Total_Injuries)) |>
      mutate(Percentage = round((Total_Injuries / total_injuries) * 100, 1)) |>
      rename(
        `Industry` = industry,
        `Total Injuries` = Total_Injuries,
        `Percentage (%)` = Percentage
      )
    # Return the summarized data as a table
    industry_data
  })
  
  # Download the top industries pie chart as a PNG image
  output$download_industry_plot_occ <- downloadHandler(
    # Define filename format for the downloaded image
    filename = function() {
      paste("top_occ_industries_pie_chart_", Sys.time(), ".png", sep = "")
    },
    content = function(file) {
      # Generate the top 5 industries data for the pie chart
      industry_data <- occ_injuries_data |>
        filter(year == input$year_industry_occ) |>
        group_by(industry) |>
        summarise(Total_Injuries = n()) |>
        arrange(desc(Total_Injuries)) |>
        mutate(Percentage = round((Total_Injuries / sum(Total_Injuries)) * 100, 1)) |>
        slice(1:5)
      
      custom_colors <- c("#336699", "#003366", "#4d4d4d", "#99ccff", "#1a1a1a")
      
      # Create pie chart plot
      p <- ggplot(industry_data, aes(x = "", y = Total_Injuries, fill = industry)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar(theta = "y") +
        geom_text(aes(label = paste0(Percentage, "%")),
                  position = position_stack(vjust = 0.5), size = 5, color = "white") +
        labs(
          title = paste("Top 5 Industries for Occupational Injuries in", input$year_industry_occ),
          fill = "Industry"
        ) +
        theme_void() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
          legend.title = element_text(face = "bold"),
          legend.text = element_text(size = 10)
        ) +
        scale_fill_manual(values = custom_colors)
      
      # Save the plot as a PNG file
      ggsave(file, plot = p, width = 8, height = 6, dpi = 300)
    }
  )
  
  
  # Top States Analysis
  
  # Render the plot for the top 5 states by total occupational illness
  output$top_states_plot_occ <- renderPlot({
    # Filter data by the selected year and calculate total injuries for each state
    top_states <- occ_injuries_data |>
      filter(year == input$year_state_occ) |>
      group_by(state) |>
      summarise(Total_Injuries = n()) |>
      mutate(state = stringr::str_to_title(state)) |>
      arrange(desc(Total_Injuries)) |>
      slice(1:5)
    custom_colors <- c("#336699", "#003366", "#4d4d4d", "#99ccff", "#1a1a1a")
    
    # Create a bar chart for the top 5 states by total injuries
    ggplot(top_states, aes(x = reorder(state, Total_Injuries), y = Total_Injuries, fill = state)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(
        title = paste("Top 5 States by Total Occupational Illness in", input$year_state_occ),
        x = "State",
        y = "Total Illness"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        axis.title.x = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold",size = 14),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12)
      ) +
      scale_fill_manual(values = custom_colors) +
      scale_y_continuous(labels = scales::comma)
  })
  
  # Render a table for all the states
  output$states_table_occ <- renderTable({
    all_states <- occ_injuries_data |>
      filter(year == input$year_state_occ) |>
      group_by(state) |>
      summarise(
        Total_Injuries = n()) |>
      mutate(state = stringr::str_to_title(state)) |>
      arrange(desc(Total_Injuries)) |>
      rename(
        `State` = state,
        `Total Injuries` = Total_Injuries
      )
    # Return the table for all the states
    all_states
  })
  
  # Download handler for the top states plot
  output$download_top_states_plot_occ <- downloadHandler(
    filename = function() {
      paste("top_states_plot_", Sys.time(), ".png", sep = "")
    },
    content = function(file) {
      top_states <- occ_injuries_data |>
        filter(year == input$year_state_occ) |>
        group_by(state) |>
        summarise(Total_Injuries = n()) |>
        mutate(state = stringr::str_to_title(state)) |>
        arrange(desc(Total_Injuries)) |>
        slice(1:5)
      custom_colors <- c("#336699", "#003366", "#4d4d4d", "#99ccff", "#1a1a1a")
      
      # Create the plot for the top 5 states
      p <- ggplot(top_states, aes(x = reorder(state, Total_Injuries), y = Total_Injuries, fill = state)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        labs(
          title = paste("Top 5 States by Total Occupational Illness in", input$year_state_occ),
          x = "State",
          y = "Total Illness"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
          axis.title.x = element_text(face = "bold", size = 14),
          axis.title.y = element_text(face = "bold",size = 14),
          axis.text.x = element_text(face = "bold", size = 12),
          axis.text.y = element_text(face = "bold", size = 12)
        ) +
        scale_fill_manual(values = custom_colors) +
        scale_y_continuous(labels = scales::comma)
      
      # Save the plot as a PNG file
      ggsave(file, plot = p, width = 8, height = 6, dpi = 300)
    }
  )
  
  # Key Injury Metric by Industry
  
  # Render dynamic title for the occupational illness visualization based on the selected industry
  output$dynamic_title <- renderUI({
    title <- paste("Occupational Illness Visualization for", input$select_industry_occ, "in", input$year_filter_industry_occ)
    
    div(
      style = "text-align: center; font-size: 24px; font-weight: bold; margin-bottom: 5px;",
      title
    )
  })
  
  # Reactive data filtering based on the selected industry and year
  filtered_data_occ <- reactive({
    data <- occ_injuries_data |>
      filter(year == input$year_filter_industry_occ)
    
    # Filter the data by industry if selected
    if (input$select_industry_occ != "All Industries") {
      data <- data |> filter(industry == input$select_industry_occ)
    }
    
    return(data)
  })

  # Render plot for employee details such as average employees and hours worked
  output$illness_employees_plot_occ <- renderPlot({
    data <- filtered_data_occ() |>
      summarise(
        Avg_Employees = sum(annual_average_employees, na.rm = TRUE),
        Hours_Worked = sum(total_hours_worked, na.rm = TRUE)
      )
    
    bar_data <- gather(data, key = "Metric", value = "Value")
    custom_colors <- c("#336699", "#003366")
    
    # Create a bar plot with labels and formatting
    ggplot(bar_data, aes(x = Metric, y = Value, fill = Metric)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      geom_text(aes(label = scales::comma(Value)),
                vjust = -0.5, size = 4, fontface = "bold") +
      scale_y_continuous(labels = scales::comma) +
      labs(title = "Employee Details", x = NULL, y = "Value") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, vjust = 1, face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.text = element_text(face = "bold", size = 12),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),
        plot.margin = margin(20, 20, 20, 20)
      ) +
      scale_fill_manual(values = custom_colors)
  })
  
  # Render plot for illness and death statistics
  output$illness_deaths_plot_occ <- renderPlot({
    data <- filtered_data_occ() |>
      summarise(
        Deaths = sum(total_deaths, na.rm = TRUE),
        Illnesses = sum(total_injuries, na.rm = TRUE)
      )
    
    bar_data <- gather(data, key = "Metric", value = "Value")
    custom_colors <- c("#336699", "#003366")
    
    # Create a bar plot with illness and death counts
    ggplot(bar_data, aes(x = Metric, y = Value, fill = Metric)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      geom_text(aes(label = scales::comma(Value)),
                vjust = -0.5, size = 4, fontface = "bold") +
      scale_y_continuous(labels = scales::comma) +
      labs(title = "Count of Illness", x = NULL, y = "Value") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, vjust = 1, face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.text = element_text(face = "bold", size = 12),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),
        plot.margin = margin(20, 20, 20, 20)
      ) +
      scale_fill_manual(values = custom_colors)
  })
  
  # Render plot for various case types such as DAFW, DJTR, and others
  output$illness_cases_plot_occ <- renderPlot({
    data <- filtered_data_occ() |>
      summarise(
        DAFW_Cases = sum(total_dafw_cases, na.rm = TRUE),
        DJTR_Cases = sum(total_djtr_cases, na.rm = TRUE),
        Other_Cases = sum(total_other_cases, na.rm = TRUE)
      )
    
    bar_data <- gather(data, key = "Case_Type", value = "Value")
    custom_colors <- c("#336699", "#003366", "#4d4d4d")
    
    # Create a bar plot with case type counts
    ggplot(bar_data, aes(x = Case_Type, y = Value, fill = Case_Type)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      geom_text(aes(label = scales::comma(Value)),
                vjust = -0.5, size = 4, fontface = "bold") +
      scale_y_continuous(labels = scales::comma) +
      labs(title = "Different Cases", x = NULL, y = "Value") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, vjust = 1, face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.text = element_text(face = "bold", size = 12),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),
        plot.margin = margin(20, 20, 20, 20)
      ) +
      scale_fill_manual(values = custom_colors)
  })
  
  # Render plot for time duration statistics (DAFW and DJTR days)
  output$illness_days_plot_occ <- renderPlot({
    data <- filtered_data_occ() |>
      summarise(
        DAFW_Days = sum(total_dafw_days, na.rm = TRUE),
        DJTR_Days = sum(total_djtr_days, na.rm = TRUE)
      )
    
    bar_data <- gather(data, key = "Days_Type", value = "Value")
    custom_colors <- c("#336699", "#003366")
    
    # Create a bar plot with time duration counts
    ggplot(bar_data, aes(x = Days_Type, y = Value, fill = Days_Type)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      geom_text(aes(label = scales::comma(Value)),
                vjust = -0.5, size = 4, fontface = "bold") +
      scale_y_continuous(labels = scales::comma) +
      labs(title = "Time Duration", x = NULL, y = "Days") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, vjust = 1, face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.text = element_text(face = "bold", size = 12),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),
        plot.margin = margin(20, 20, 20, 20)
      ) +
      scale_fill_manual(values = custom_colors)
  })
  
  # Render plot for illness categories and total illness cases
  output$illness_categories_plot_occ <- renderPlot({
    data <- filtered_data_occ() |>
      summarise(
        Poisonings = sum(total_poisonings, na.rm = TRUE),
        Respiratory = sum(total_respiratory_conditions, na.rm = TRUE),
        Skin_Disorders = sum(total_skin_disorders, na.rm = TRUE),
        Hearing_Loss = sum(total_hearing_loss, na.rm = TRUE),
        Other = sum(total_other_illnesses, na.rm = TRUE)
      )
    
    bar_data <- gather(data, key = "Illness_Type", value = "Value")
    custom_colors <- c("#336699", "#003366", "#4d4d4d", "#99ccff", "#1a1a1a")
    
    # Create a bar plot for illness categories and illness cases
    ggplot(bar_data, aes(x = Illness_Type, y = Value, fill = Illness_Type)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      geom_text(aes(label = scales::comma(Value)),
                vjust = -0.5, size = 4, fontface = "bold") +
      scale_y_continuous(labels = scales::comma) +
      labs(title = "Illness Categories vs Illness", x = NULL, y = "Total Illness") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, vjust = 1, face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.text = element_text(face = "bold", size = 12),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),
        plot.margin = margin(20, 20, 20, 20)
      ) +
      scale_fill_manual(values = custom_colors)
  })
  
  output$download_industry_interactive_plots <- downloadHandler(
    # Define the file name for the downloaded PDF
    filename = function() {
      paste("Summary_Plots", Sys.time(), ".pdf", sep = "")
    },
    content = function(file) {
      # Open a PDF device to save the plots
      pdf(file, width = 8, height = 6)  # Open a PDF device
      custom_colors <- c("#336699", "#003366", "#4d4d4d", "#99ccff", "#1a1a1a")
      
      # Render dynamic title for the report
      output$dynamic_title <- renderUI({
        title <- paste("Occupational Illness Visualization for", input$select_industry_occ, "in", input$year_filter_industry_occ)
        
        div(
          style = "text-align: center; font-size: 24px; font-weight: bold; margin-bottom: 5px;",
          title
        )
      })
      
      # Plot 1: Annual Average Employees, Total Hours Worked
      data1 <- filtered_data_occ() |>
        summarise(
          Avg_Employees = sum(annual_average_employees, na.rm = TRUE),
          Hours_Worked = sum(total_hours_worked, na.rm = TRUE)
        )
      bar_data1 <- gather(data1, key = "Metric", value = "Value")
      plot1 <- ggplot(bar_data1, aes(x = Metric, y = Value, fill = Metric)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        geom_text(aes(label = scales::comma(Value)), vjust = -0.5, size = 3) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Employee Details", x = NULL, y = "Value") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, vjust = 1, face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 14),
          axis.text = element_text(face = "bold", size = 12),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),
          plot.margin = margin(20, 20, 20, 20)
        ) +
        scale_fill_manual(values = custom_colors)
      print(plot1)
      
      # Plot 2: Total Deaths and Total Injuries
      data2 <- filtered_data_occ() |>
        summarise(
          Deaths = sum(total_deaths, na.rm = TRUE),
          Injuries = sum(total_injuries, na.rm = TRUE)
        )
      bar_data2 <- gather(data2, key = "Metric", value = "Value")
      plot2 <- ggplot(bar_data2, aes(x = Metric, y = Value, fill = Metric)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        geom_text(aes(label = scales::comma(Value)), vjust = -0.5, size = 3) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Count of Illness", x = NULL, y = "Value") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, vjust = 1, face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 14),
          axis.text = element_text(face = "bold", size = 12),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),
          plot.margin = margin(20, 20, 20, 20)
        ) +
        scale_fill_manual(values = custom_colors)
      print(plot2)
      
      # Plot 3: DAFW Cases, DJTR Cases, Other Cases
      data3 <- filtered_data_occ() |>
        summarise(
          DAFW_Cases = sum(total_dafw_cases, na.rm = TRUE),
          DJTR_Cases = sum(total_djtr_cases, na.rm = TRUE),
          Other_Cases = sum(total_other_cases, na.rm = TRUE)
        )
      bar_data3 <- gather(data3, key = "Case_Type", value = "Value")
      plot3 <- ggplot(bar_data3, aes(x = Case_Type, y = Value, fill = Case_Type)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        geom_text(aes(label = scales::comma(Value)), vjust = -0.5, size = 3) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Different Cases", x = NULL, y = "Value") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, vjust = 1, face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 14),
          axis.text = element_text(face = "bold", size = 12),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),
          plot.margin = margin(20, 20, 20, 20)
        ) +
        scale_fill_manual(values = custom_colors)
      print(plot3)
      
      # Plot 4: DAFW Days, DJTR Days
      data4 <- filtered_data_occ() |>
        summarise(
          DAFW_Days = sum(total_dafw_days, na.rm = TRUE),
          DJTR_Days = sum(total_djtr_days, na.rm = TRUE)
        )
      bar_data4 <- gather(data4, key = "Days_Type", value = "Value")
      plot4 <- ggplot(bar_data4, aes(x = Days_Type, y = Value, fill = Days_Type)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        geom_text(aes(label = scales::comma(Value)), vjust = -0.5, size = 3) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Time Duration", x = NULL, y = "Days") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, vjust = 1, face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 14),
          axis.text = element_text(face = "bold", size = 12),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),
          plot.margin = margin(20, 20, 20, 20)
        ) +
        scale_fill_manual(values = custom_colors)
      print(plot4)
      
      # Plot 5: Illness Categories
      data5 <- filtered_data_occ() |>
        summarise(
          Poisonings = sum(total_poisonings, na.rm = TRUE),
          Respiratory = sum(total_respiratory_conditions, na.rm = TRUE),
          Skin_Disorders = sum(total_skin_disorders, na.rm = TRUE),
          Hearing_Loss = sum(total_hearing_loss, na.rm = TRUE),
          Other = sum(total_other_illnesses, na.rm = TRUE)
        )
      bar_data5 <- gather(data5, key = "Illness_Type", value = "Value")
      plot5 <- ggplot(bar_data5, aes(x = Illness_Type, y = Value, fill = Illness_Type)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        geom_text(aes(label = scales::comma(Value)), vjust = -0.5, size = 3) +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Illness Categories vs Illness", x = NULL, y = "Total Illness") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, vjust = 1, face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 14),
          axis.text = element_text(face = "bold", size = 12),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),
          plot.margin = margin(20, 20, 20, 20)
        ) +
        scale_fill_manual(values = custom_colors)
      print(plot5)
      
      # Close the PDF device to finish saving the plots
      dev.off()
    }
  )
  
  # Interactive Filters for Occupational Illness
  
  # Reactive expression to filter the dataset based on user inputs
  filtered_data_table_occ <- reactive({
    # Start with the full dataset of occupational injuries
    data <- occ_injuries_data
    
    # Filter data by the selected year
    data <- data |> filter(year == input$year_filter_occ)
    
    # Filter the data by industry if selected
    if (input$industry_filter_occ != "All Industries") {
      data <- data |> filter(industry == input$industry_filter_occ)
    }
    
    # Filter the data by state if selected
    if (input$state_filter_occ != "All States") {
      data <- data |> filter(state == input$state_filter_occ)
    }
    
    # Filter data based on whether the injury/illness type is selected as 'Yes' (1) or 'No' (2)
    if (input$illness_injuries_filter_occ == "Yes") {
      data <- data |> filter(injury_illness == 1)
    } else {
      data <- data |> filter(injury_illness == 2)
    }
    
    # Filter data based on respiratory conditions
    if (input$respiratory_conditions_filter_occ == "No") {
      data <- data |> filter(total_respiratory_conditions == 0)
    } else {
      data <- data |> filter(total_respiratory_conditions > 0)
    }
    
    # Filter data based on skin disorders
    if (input$skin_disorder_filter_occ == "Yes") {
      data <- data |> filter(total_skin_disorders > 1)
    } else {
      data <- data |> filter(total_skin_disorders == 0)
    }
    
    # Filter data based on hearing loss
    if (input$hearing_loss_filter_occ == "Yes") {
      data <- data |> filter(total_hearing_loss > 1)
    } else {
      data <- data |> filter(total_hearing_loss == 0)
    }
    
    # Filter data based on other illnesses
    if (input$other_illness_filter_occ == "Yes") {
      data <- data |> filter(total_other_illnesses > 1)
    } else {
      data <- data |> filter(total_other_illnesses == 0)
    }
    
    # Format the state column to have only the first letter in uppercase
    data <- data |> 
      mutate(state = stringr::str_to_title(state))
    
    # Return the filtered dataset
    return(data)
  })
  
  # Render the filtered data table based on the user-selected filters
  output$filtered_table_occ <- renderDT({
    datatable(
      # Render the datatable with filtered data
      filtered_data_table_occ(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # Download handler to export the filtered data as a CSV file
  output$download_occ_data <- downloadHandler(
    filename = function() {
      paste("filtered_data_", Sys.time(), ".csv", sep = "")
    },
    # Write the filtered data to the CSV file
    content = function(file) {
      write.csv(filtered_data_table_occ(), file, row.names = FALSE)
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


# OSHA Injury Risk Dashboard with modelling
# -------------------------------------------------------------------------

# ---- Packages ----
packages <- c(
  "shiny","shinythemes","bslib","tidyverse","lubridate","janitor","scales",
  "DT","plotly","leaflet","sf","tidymodels","textrecipes","tidytext",
  "topicmodels","widyr","ggrepel","gt","gtExtras","cluster","factoextra",
  "randomForest","glmnet","ranger"
)
to_install <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)

options(shiny.maxRequestSize = 200*1024^2)

# ---- UI ----
ui <- navbarPage(
  title = "OSHA Injury Risk Insights",
  theme = shinytheme("flatly"),

  # Upload
  tabPanel("Upload & Filters",
    sidebarLayout(
      sidebarPanel(width = 3,
        fileInput("file", "Upload OSHA CSV", accept = c(".csv")),
        checkboxInput("has_header", "Header row", TRUE),
        selectInput("dt_format", "Date column format", 
                    choices = c("ymd","mdy","dmy"), selected = "ymd"),
        sliderInput("year_filter", "Year range", min = 2015, max = year(Sys.Date()),
                    value = c(2019, year(Sys.Date())), step = 1, sep = ""),
        selectizeInput("state_filter", "States", choices = NULL, multiple = TRUE),
        selectizeInput("naics_filter", "NAICS", choices = NULL, multiple = TRUE),
        selectizeInput("employer_filter", "Employers", choices = NULL, multiple = TRUE),
        textInput("keyword", "Narrative keyword filter (optional)", ""),
        helpText("Expected columns: EventDate, Employer, City, State, NAICS, Hospitalized, Amputation, Narrative, Latitude, Longitude")
      ),
      mainPanel(
        h4("Data snapshot"),
        DTOutput("table_head"),
        br(),
        fluidRow(
          column(4, gt_output("kpi_total")),
          column(4, gt_output("kpi_hosp")),
          column(4, gt_output("kpi_ampt"))
        )
      )
    )
  ),

  tabPanel("EDA",
    fluidRow(
      column(6, plotlyOutput("ts_trend")),
      column(6, plotlyOutput("by_state"))
    ),
    fluidRow(
      column(6, plotlyOutput("by_naics")),
      column(6, plotlyOutput("repeat_employers"))
    )
  ),

  tabPanel("NLP (TF-IDF & Keywords)",
    fluidRow(
      column(6, plotlyOutput("tfidf_terms")),
      column(6, DTOutput("keywords_tbl"))
    )
  ),

  tabPanel("Topics (LDA)",
    sidebarLayout(
      sidebarPanel(width = 3,
        sliderInput("n_topics", "Number of topics (LDA)", min = 3, max = 12, value = 6, step = 1),
        numericInput("top_n_terms", "Top terms per topic", value = 8, min = 5, max = 20),
        actionButton("run_lda", "Run / Refresh LDA")
      ),
      mainPanel(
        plotlyOutput("lda_terms_plot"),
        DTOutput("doc_topics_tbl")
      )
    )
  ),

  tabPanel("Clustering",
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput("cluster_level", "Cluster by", choices = c("Incidents","Employers"), selected = "Employers"),
        sliderInput("k", "k (clusters)", min = 2, max = 10, value = 4),
        checkboxGroupInput("cluster_features", "Features",
                           choices = c("HospRate","AmputationRate","Incidents","UniqueNAICS"),
                           selected = c("HospRate","Incidents"))
      ),
      mainPanel(
        plotOutput("cluster_plot"),
        DTOutput("cluster_centers")
      )
    )
  ),

  # Modeling
  tabPanel("Modeling (Predict Hospitalization)",
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput("model_type", "Model", choices = c("Logistic Regression","Random Forest"), selected = "Random Forest"),
        sliderInput("train_prop", "Train proportion", min = 0.5, max = 0.9, value = 0.8, step = 0.05),
        numericInput("rf_trees", "Random Forest trees", value = 300, min = 50, max = 1000),
        actionButton("train_model", "Train / Evaluate")
      ),
      mainPanel(
        h5("Performance"),
        verbatimTextOutput("model_metrics"),
        plotOutput("roc_plot"),
        DTOutput("feat_importance")
      )
    )
  ),

  # Map
  tabPanel("Map (Hotspots)",
    fluidRow(
      column(12, leafletOutput("map", height = 600))
    )
  ),

  # High-Risk Employers
  tabPanel("High-Risk Employers",
    fluidRow(
      column(12, DTOutput("employer_rank"))
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {

  # Load & Clean
  raw <- reactive({
    req(input$file)
    df <- readr::read_csv(input$file$datapath, show_col_types = FALSE)
    df <- janitor::clean_names(df)
    date_fun <- switch(input$dt_format, ymd = lubridate::ymd, mdy = lubridate::mdy, dmy = lubridate::dmy)
    if ("eventdate" %in% names(df)) df$event_date <- df$eventdate
    if (!"event_date" %in% names(df)) stop("Missing event_date/EventDate column.")
    df <- df %>%
      mutate(
        event_date = date_fun(event_date),
        year = year(event_date),
        hospitalized = if_else(tolower(as.character(hospitalized)) %in% c("1","yes","y","true","t"), 1, 0),
        amputation   = if_else(tolower(as.character(amputation))   %in% c("1","yes","y","true","t"), 1, 0),
        narrative = as.character(narrative)
      ) %>% filter(!is.na(event_date))
    df
  })

  # Update filters
  observeEvent(raw(), {
    df <- raw()
    updateSelectizeInput(session, "state_filter", choices = sort(unique(df$state)), server = TRUE)
    updateSelectizeInput(session, "naics_filter", choices = sort(unique(df$naics)), server = TRUE)
    updateSelectizeInput(session, "employer_filter", choices = sort(unique(df$employer)), server = TRUE)
    updateSliderInput(session, "year_filter", min = min(df$year, na.rm = TRUE), max = max(df$year, na.rm = TRUE),
                      value = c(max(min(df$year), 2018), max(df$year)))
  })

  # Apply filters
  filtered <- reactive({
    df <- req(raw())
    df <- df %>% filter(between(year, input$year_filter[1], input$year_filter[2]))
    if (length(input$state_filter)) df <- df %>% filter(state %in% input$state_filter)
    if (length(input$naics_filter)) df <- df %>% filter(naics %in% input$naics_filter)
    if (length(input$employer_filter)) df <- df %>% filter(employer %in% input$employer_filter)
    if (nzchar(input$keyword)) df <- df %>% filter(str_detect(tolower(narrative), tolower(input$keyword)))
    validate(need(nrow(df) > 0, "No rows after filters. Adjust filters."))
    df
  })

  # KPIs
  kpi_card <- function(title, value) {
    gt(data.frame(Title = title, Value = value)) |> gt_theme_guardian() |> fmt_number(columns = "Value", decimals = 0)
  }
  output$kpi_total <- render_gt({ kpi_card("Incidents", nrow(filtered())) })
  output$kpi_hosp  <- render_gt({ kpi_card("Hospitalizations", sum(filtered()$hospitalized, na.rm = TRUE)) })
  output$kpi_ampt  <- render_gt({ kpi_card("Amputations", sum(filtered()$amputation, na.rm = TRUE)) })
  output$table_head <- renderDT({ datatable(head(filtered(), 50), options = list(pageLength = 10, scrollX = TRUE)) })

  # === EDA ===
  output$ts_trend <- renderPlotly({
    df <- filtered() %>% count(date = floor_date(event_date, "month"))
    ggplotly(ggplot(df, aes(date, n)) + geom_line() + geom_point() + labs(title = "Incidents over time"))
  })
  output$by_state <- renderPlotly({
    df <- filtered() %>% count(state, sort = TRUE) %>% slice_max(n, n = 25)
    ggplotly(ggplot(df, aes(reorder(state, n), n)) + geom_col() + coord_flip())
  })
  output$by_naics <- renderPlotly({
    df <- filtered() %>% count(naics, sort = TRUE) %>% slice_max(n, n = 25)
    ggplotly(ggplot(df, aes(reorder(naics, n), n)) + geom_col() + coord_flip())
  })
  output$repeat_employers <- renderPlotly({
    df <- filtered() %>% count(employer, sort = TRUE) %>% slice_max(n, n = 25)
    ggplotly(ggplot(df, aes(reorder(employer, n), n)) + geom_col() + coord_flip())
  })

  # === NLP: TF-IDF ===
  tokens <- reactive({
    df <- filtered() %>% select(id = row_number(), narrative)
    df %>% unnest_tokens(word, narrative) %>% anti_join(stop_words, by = "word") %>% filter(str_detect(word, "^[a-z]+$"))
  })
  output$tfidf_terms <- renderPlotly({
    tfidf <- tokens() %>% count(id, word, sort = TRUE) %>% bind_tf_idf(word, id, n) %>%
      group_by(word) %>% summarize(tf_idf = max(tf_idf), .groups = "drop") %>% slice_max(tf_idf, n = 25)
    ggplotly(ggplot(tfidf, aes(tf_idf, fct_inorder(word))) + geom_col())
  })
  output$keywords_tbl <- renderDT({
    tfidf <- tokens() %>% count(id, word, sort = TRUE) %>% bind_tf_idf(word, id, n) %>% arrange(desc(tf_idf))
    datatable(slice_max(tfidf, tf_idf, n = 200))
  })

  # === Topics: LDA ===
  lda_results <- eventReactive(input$run_lda, {
    tidy <- tokens()
    doc_term <- tidy %>% count(id, word) %>% cast_dtm(id, word, n)
    m <- LDA(doc_term, k = input$n_topics, control = list(seed = 123))
    list(model = m, dtm = doc_term)
  }, ignoreInit = TRUE)
  output$lda_terms_plot <- renderPlotly({
    m <- req(lda_results())$model
    terms <- tidy(m, matrix = "beta") %>% group_by(topic) %>% slice_max(beta, n = input$top_n_terms)
    ggplotly(ggplot(terms, aes(beta, fct_inorder(term))) + geom_col() + facet_wrap(~ topic, scales = "free_y"))
  })
  output$doc_topics_tbl <- renderDT({
    m <- req(lda_results())$model
    docs <- tidy(m, matrix = "gamma") %>% group_by(document) %>% slice_max(gamma, n = 1)
    datatable(docs)
  })

  # === Clustering ===
  cluster_data <- reactive({
    df <- filtered()
    if (input$cluster_level == "Employers") {
      agg <- df %>% group_by(employer) %>%
        summarise(Incidents = n(), HospRate = mean(hospitalized), AmputationRate = mean(amputation), UniqueNAICS = n_distinct(naics))
      rownames(agg) <- agg$employer; agg
    } else {
      df %>% mutate(Incidents = 1, HospRate = hospitalized, AmputationRate = amputation, UniqueNAICS = as.numeric(as.factor(naics))) %>%
        select(Incidents, HospRate, AmputationRate, UniqueNAICS)
    }
  })
  output$cluster_plot <- renderPlot({
    dat <- cluster_data(); feats <- intersect(input$cluster_features, names(dat))
    km <- kmeans(scale(dat[,feats]), centers = input$k, nstart = 20)
    fviz_cluster(km, data = as.data.frame(scale(dat[,feats])))
  })
  output$cluster_centers <- renderDT({
    dat <- cluster_data(); feats <- intersect(input$cluster_features, names(dat))
    km <- kmeans(scale(dat[,feats]), centers = input$k, nstart = 20)
    datatable(as.data.frame(km$centers))
  })

  # === Modeling ===
  model_obj <- eventReactive(input$train_model, {
    df <- filtered() %>% mutate(severe = factor(if_else(hospitalized == 1,"Yes","No")))
    split <- initial_split(df, prop = input$train_prop, strata = severe)
    tr <- training(split); te <- testing(split)
    rec <- recipe(severe ~ ., data = tr) |> step_zv(all_predictors()) |>
      step_text_normalization(narrative) |> step_tokenize(narrative) |> step_stopwords(narrative) |>
      step_tfidf(narrative)
    if (input$model_type == "Logistic Regression") {
      mod <- logistic_reg(mode = "classification", penalty = tune(), mixture = 1) |> set_engine("glmnet")
      grid <- grid_regular(penalty(), levels = 10)
    } else {
      mod <- rand_forest(mode = "classification", trees = input$rf_trees, mtry = tune(), min_n = tune()) |> set_engine("ranger", importance = "impurity")
      grid <- grid_regular(mtry(range = c(2, 20)), min_n(), levels = 5)
    }
    wf <- workflow() |> add_model(mod) |> add_recipe(rec)
    folds <- vfold_cv(tr, v = 5, strata = severe)
    tuned <- tune_grid(wf, resamples = folds, grid = grid, metrics = metric_set(roc_auc, accuracy))
    best <- select_best(tuned, "roc_auc")
    final_wf <- finalize_workflow(wf, best); final_fit <- fit(final_wf, tr)
    preds <- predict(final_fit, te, type = "prob") %>% bind_cols(te %>% select(severe))
    list(fit = final_fit, preds = preds)
  })
  output$model_metrics <- renderPrint({
    preds <- req(model_obj())$preds
    preds <- preds %>% mutate(.pred_class = if_else(.pred_Yes >= 0.5, "Yes","No"))
    list(Accuracy = accuracy(preds, truth = severe, estimate = .pred_class)$.estimate,
         ROC_AUC = roc_auc(preds, truth = severe, .pred_Yes)$.estimate)
  })
  output$roc_plot <- renderPlot({
    preds <- req(model_obj())$preds
    roc <- roc_curve(preds, truth = severe, .pred_Yes)
    ggplot(roc, aes(1 - specificity, sensitivity)) + geom_path() + geom_abline(lty = 3)
  })
  output$feat_importance <- renderDT({
    fit <- model_obj()$fit; eng <- extract_fit_engine(fit)
    if ("variable.importance" %in% names(eng)) {
      imp <- tibble(term = names(eng$variable.importance), importance = eng$variable.importance)
      datatable(arrange(imp, desc(importance)))
    } else datatable(tibble())
  })

  # === Map ===
  output$map <- renderLeaflet({
    df <- filtered() %>% filter(!is.na(latitude), !is.na(longitude))
    leaflet(df) %>% addTiles() %>% addCircleMarkers(~longitude, ~latitude, popup = ~paste(employer, city, state), radius = 5)
  })

  # === Employer Ranking ===
  output$employer_rank <- renderDT({
    df <- filtered() %>% group_by(employer) %>%
      summarise(Incidents = n(), HospRate = mean(hospitalized), AmputationRate = mean(amputation)) %>%
      mutate(RiskScore = Incidents*0.5 + HospRate*35 + AmputationRate*15) %>% arrange(desc(RiskScore))
    datatable(df)
  })
}

shinyApp(ui, server)
