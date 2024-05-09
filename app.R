library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .header-banner {
        background-color: #5A00B8;
        color: #fff;
        padding: 10px;
        font-family: 'Karla', sans-serif;
        font-weight: bold;
        text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5);
        font-size: 35px;
        margin-left: -20px;
        margin-right: -20px;
      }
      .header-text {
        margin-left: 20px; 
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
        height: 100vh;
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
        margin-left: -15px; 
        margin-top: -50px; 
      }
      .subtitle-dashboard {
        background-color: #fffff;
        font-family: 'Karla', sans-serif;
        padding: 10px;
        margin-left: -15px; 
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
        tags$div(id = "dashboardContent", class = "main-content",
                 textOutput("output_text"))
      )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$btn_dashboard, {
    output$output_text <- renderText({
      "Movies Data"
    })
    
    # Update the content dynamically using tags$div
    output$dashboardContent <- renderUI({
      tags$div(
        tags$div(class = "title-dashboard",
                 tags$span(class = "title-span", "Movies Data")),
        tags$div(class = "subtitle-dashboard",
                 tags$span(class = "subtitle-span", "Overview of Different movies over the years."))
      )
    })
  })
  
  observeEvent(input$btn_analytics, {
    output$output_text <- renderText({
      "Analytics Content"
    })
  })
  
  observeEvent(input$btn_about, {
    output$output_text <- renderText({
      "About Content"
    })
  })
}

shinyApp(ui = ui, server = server)
