library(shiny)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(plotly)

movies_data <- read.csv("movies.csv")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .header-banner {
        background-color: #5A00B8;
        color: #fff;
        padding: 10px;
        height: 100px;
        width: 180vh;
        font-family: 'Karla', sans-serif;
        font-weight: bold;
        text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5);
        font-size: 35px;
        margin-left: -20px;
        margin-right: -20px;
      }
      .header-text {
        margin-left: 50px;
        margin-top: -500px;
      }
      .content {
        display: flex;
        position: relative;
      }
      .sidebar {
        background-color: #00BDAA;
        color: #fff;
        font-family: 'Karla', sans-serif;
        font-weight: 600;
        width: 200px;
        height: 180vh;
        display: flex;
        flex-direction: column;
        margin-left: -20px;
        margin-top: -60px;
        justify-content: flex-start;
      }
      .sidebar-btn {
        display: block;
        background-color: inherit;
        border: none;
        color: #fff;
        padding: 10px 30px;
        margin-bottom: 5px;
        margin-top: 20px;
        width: 100%;
      }
      .title-dashboard {
        background-color: #fffff;
        font-family: 'Karla', sans-serif;
        font-weight: 600;
        padding: 10px;
        margin-left: -20px; 
        margin-top: -50px; 
      }
      .subtitle-dashboard {
        background-color: #fffff;
        font-family: 'Karla', sans-serif;
        padding: 10px;
        margin-left: -20px; 
        margin-top: -45px; 
      }
      .title-span {
        font-size: 20px; 
      }
      .subtitle-span {
        font-size: 14px; 
      }
      .main-content {
        flex: 1;
        padding: 10px;
      }
    "))
  ),
  div(class = "header-banner",
      tags$span(class = "header-text", "Movie Metadata")  
  ),
  div(class = "content",
      absolutePanel(
        div(class = "sidebar",
            actionButton("btn_dashboard", "Dashboard", class = "sidebar-btn"),
            actionButton("btn_analytics", "Analytics", class = "sidebar-btn"),
            actionButton("btn_about", "About", class = "sidebar-btn")
        ),
        top = 60, left = 0, width = 200, height = "100%"
      ),
      absolutePanel(
        id = "dashboardPanel",
        top = 60, left = 220, width = "calc(100% - 220px)", height = "100%",
        tags$div(id = "dashboardContent", class = "main-content"),
        uiOutput("dashboardContent"),
      )
  )
)

server <- function(input, output, session) {
  
  movies_data <- movies_data %>%
    arrange(desc(release_date)) %>%
    filter(status == "Released") %>%
    na.omit()
  
  # Function to generate dashboard content
  generateDashboardContent <- function(title, desc) {
    div(
      class = "dashboard-content",
      div(class = "title-dashboard",
          span(class = "title-span", title)),
      br(),
      div(class = "subtitle-dashboard",
          span(class = "subtitle-span", desc))
    )
  }
  
  # Initial rendering of charts upon app startup and when btn_dashboard is pressed
  output$dashboardContent <- renderUI({
    div(
      generateDashboardContent("Movies Data", "Overview of Different movies over the years."),
      # Additional elements below the dashboard content
      selectInput("year_dropdown", label = "Select year:", choices = unique(substr(movies_data$release_date, 1, 4))),
      # Card
      div(class = "genre-revenue-card",
          style = "background-color: #E0E0E0;
        height: 500px;
        width: 600;
        border-radius: 30px;
        box-shadow: 0 4px 4px rgba(0, 0, 0, 0.3)",
          div(class = "card-body",
              h4(class = "card-title", "Average Revenue for Each Genre",
                 style = "padding: 10px 15px;"),
              div(
                style = "background-color: #E0E0E0;", # Set background color of the plot area
                plotlyOutput("piechart", height = "400px")
              )
          )
      ),
      div(class = "monthly-revenue-card",
          style = "background-color: #E0E0E0;
        height: 500px;
        width: 600;
        border-radius: 30px;
        box-shadow: 0 4px 4px rgba(0, 0, 0, 0.3)",
          div(class = "card-body",
              h4(class = "card-title", "Monthly Movie Revenue",
                 style = "padding: 10px 15px;"),
              div(
                style = "background-color: #E0E0E0;", # Set background color of the plot area
                plotlyOutput("linechart", height = "400px")
              )
          )
      ),
      div(class = "genre-popularity-card",
          style = "background-color: #E0E0E0;
        height: 500px;
        width: 600;
        border-radius: 30px;
        box-shadow: 0 4px 4px rgba(0, 0, 0, 0.3)",
          div(class = "card-body",
              h4(class = "card-title", "Monthly Movie Revenue",
                 style = "padding: 10px 15px;"),
              div(
                style = "background-color: #E0E0E0;", # Set background color of the plot area
                plotlyOutput("barchart", height = "400px")
              )
          )
      )
    )
  })
  
  # Render all charts when btn_dashboard is pressed
  observeEvent(input$btn_dashboard, {
    output$dashboardContent <- renderUI({
      div(
        generateDashboardContent("Movies Data", "Overview of Different movies over the years."),
        # Additional elements below the dashboard content
        selectInput("year_dropdown", label = "Select year:", choices = unique(substr(movies_data$release_date, 1, 4))),
        # Card
        div(class = "genre-revenue-card",
            style = "background-color: #E0E0E0;
          height: 500px;
          width: 600;
          border-radius: 30px;
          box-shadow: 0 4px 4px rgba(0, 0, 0, 0.3)",
            div(class = "card-body",
                h4(class = "card-title", "Average Revenue for Each Genre",
                   style = "padding: 10px 15px;"),
                div(
                  style = "background-color: #E0E0E0;", # Set background color of the plot area
                  plotlyOutput("piechart", height = "400px")
                )
            )
        ),
        div(class = "monthly-revenue-card",
            style = "background-color: #E0E0E0;
          height: 500px;
          width: 600;
          border-radius: 30px;
          box-shadow: 0 4px 4px rgba(0, 0, 0, 0.3)",
            div(class = "card-body",
                h4(class = "card-title", "Monthly Movie Revenue",
                   style = "padding: 10px 15px;"),
                div(
                  style = "background-color: #E0E0E0;", # Set background color of the plot area
                  plotlyOutput("linechart", height = "400px")
                )
            )
        ),
        div(class = "genre-popularity-card",
            style = "background-color: #E0E0E0;
          height: 500px;
          width: 600;
          border-radius: 30px;
          box-shadow: 0 4px 4px rgba(0, 0, 0, 0.3)",
            div(class = "card-body",
                h4(class = "card-title", "Monthly Movie Revenue",
                   style = "padding: 10px 15px;"),
                div(
                  style = "background-color: #E0E0E0;", # Set background color of the plot area
                  plotlyOutput("barchart", height = "400px")
                )
            )
        )
      )
    })
  })
  
  observeEvent(input$btn_analytics, {
    output$dashboardContent <- renderUI({
      generateDashboardContent("Movie Revenue Prediction Model",  "Overview of Different movies over the years.")
    })
  })
  
  observeEvent(input$btn_about, {
    output$dashboardContent <- renderUI({
      div(
        generateDashboardContent("About", "This is an about page.")
      )
    })
  })
  
  # Function to render the pie chart
  output$piechart <- renderPlotly({
    render_piechart()
  })
  
  # Function to render the line chart
  output$linechart <- renderPlotly({
    render_linechart()
  })
  
  # Function to render the bar chart
  output$barchart <- renderPlotly({
    render_barchart()
  })
  
  # Function to render the pie chart
  render_piechart <- function() {
    # Filter the data based on the selected year
    filtered_data <- movies_data %>%
      filter(substr(release_date, 1, 4) == input$year_dropdown & revenue != 0 & !is.na(revenue)) %>%
      na.omit()
    
    # Unnest the genre column to have one row per genre
    unnested_data <- filtered_data %>%
      mutate(genres = strsplit(genres, "-")) %>%
      unnest(genres)
    
    # Calculate the average revenue for each unique genre
    average_revenue <- unnested_data %>%
      group_by(genres) %>%
      summarise(average_revenue = mean(revenue, na.rm = TRUE))
    
    # Plot the pie chart
    plot_ly(average_revenue, labels = ~genres, values = ~average_revenue, type = "pie") %>%
      layout( 
        paper_bgcolor = "rgba(0,0,0,0)",  # Set background color to transparent
        plot_bgcolor = "rgba(0,0,0,0)"  # Set plot area background color to transparent
      )
  }
  
  # Function to render the line chart
  render_linechart <- function() {
    # Filter the data based on the selected year
    filtered_data <- movies_data %>%
      filter(substr(release_date, 1, 4) == input$year_dropdown & revenue != 0 & !is.na(revenue)) %>%
      na.omit()
    
    # Extract the month from the release date
    filtered_data$month <- format(as.Date(filtered_data$release_date), "%m")
    
    # Calculate the average revenue for each month
    average_revenue_per_month <- filtered_data %>%
      group_by(month) %>%
      summarise(average_revenue = mean(revenue, na.rm = TRUE))
    
    # Convert numeric month values to month names and sort them
    average_revenue_per_month$month <- factor(month.name[as.integer(average_revenue_per_month$month)], levels = month.name)
    
    # Plot the line chart
    plot_ly(average_revenue_per_month, x = ~month, y = ~average_revenue, type = "scatter", mode = "lines") %>%
      layout(
        xaxis = list(title = "Month"),
        yaxis = list(title = "Average Revenue"),
        paper_bgcolor = "rgba(0,0,0,0)",  # Set background color to transparent
        plot_bgcolor = "rgba(0,0,0,0)"    # Set plot area background color to transparent
      )
  }
  
  # Function to render the bar chart
  render_barchart <- function() {
    # Filter the data based on the selected year
    filtered_data <- movies_data %>%
      filter(substr(release_date, 1, 4) == input$year_dropdown & popularity != 0 & !is.na(popularity)) %>%
      na.omit()
    
    # Unnest the genre column to have one row per genre
    unnested_data <- filtered_data %>%
      mutate(genres = strsplit(genres, "-")) %>%
      unnest(genres)
    
    # Calculate the average popularity for each unique genre
    average_popularity <- unnested_data %>%
      group_by(genres) %>%
      summarise(average_popularity = mean(popularity, na.rm = TRUE))
    
    # Plot the bar chart
    plot_ly(average_popularity, x = ~genres, y = ~average_popularity, type = "bar") %>%
      layout( 
        xaxis = list(title = "Genre"),
        yaxis = list(title = "Average Popularity"),
        paper_bgcolor = "rgba(0,0,0,0)",  # Set background color to transparent
        plot_bgcolor = "rgba(0,0,0,0)"    # Set plot area background color to transparent
      )
  }
}

shinyApp(ui = ui, server = server)
