library(shiny)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  
  tags$style(HTML("
    /* Title Container */
    .title-container {
      display: flex;
      justify-content: center;
      align-items: center;
      margin: 20px auto;
    }

    .title-text {
      font-size: 38px;
      font-weight: bold;
      color: black; /* Changed title color to black */
      text-align: center;
    }

    .title-icon {
      width: 50px;
      height: 50px;
      background-color: white;
      border-radius: 50%;
      display: flex;
      justify-content: center;
      align-items: center;
      box-shadow: 0px 3px 8px rgba(0, 128, 0, 0.2);
      margin: 0 15px;
    }

    .title-icon img {
      width: 30px;
      height: 30px;
    }

    /* Floating Board Animation */
    @keyframes floating {
      0% { transform: translateY(0px); }
      50% { transform: translateY(-4px); }
      100% { transform: translateY(0px); }
    }

    .result-box {
      border: 3px solid #5a9ecb; 
      background-color: #f0f8ff;
      padding: 20px;
      border-radius: 15px;
      display: flex;
      align-items: center;
      font-size: 20px;
      margin-bottom: 15px;
      position: relative;
      box-shadow: 0px 4px 10px rgba(0, 0, 0, 0.2);
      animation: floating 3s infinite ease-in-out;
    }

    /* Number Block Styling */
    .num-block {
      background-color: #f4e8d4;
      padding: 10px 15px;
      border-radius: 8px;
      font-weight: bold;
      display: inline-block;
      font-size: 22px;
      margin-right: 15px;
      animation: fadeIn 0.5s ease-in-out;
    }

    .text-bold {
      font-weight: bold;
      font-size: 18px;
      color: #333;
    }

    .icon {
      margin-left: auto;
      font-size: 26px;
    }

    /* Tree Section Pulsating */
    .tree-icon {
      animation: growTree 3s infinite ease-in-out;
    }
  ")),
  
  div(class = "title-container",
      div(class = "title-icon", style = "font-size: 50px; background: none; box-shadow: none;", "🌿"), # Bigger left icon, no bubble
      h1("Greenhouse Gas Equivalencies Calculator", class = "title-text"), # Title in black
      div(class = "title-icon", style = "font-size: 50px; background: none; box-shadow: none;", "🌳") # Bigger right icon, no bubble
  )
  ,
  
  sidebarLayout(
    sidebarPanel(
      numericInput("co2_input", 
                   "Enter CO₂ emissions (metric tons):", 
                   value = NULL, min = 0),
      actionButton("convert_btn", "Convert")
    ),
    
    mainPanel(
      h3("Equivalent Measures"),
      
      div(uiOutput("vehicles_output"), class = "result-box"),
      div(uiOutput("gasoline_output"), class = "result-box"),
      div(uiOutput("trees_output"), class = "result-box")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$convert_btn, {
    co2 <- input$co2_input
    
    vehicles_eq <- co2 * 2547  
    gasoline_eq <- co2 * 113
    trees_eq <- co2 * 16.5
    
    output$vehicles_output <- renderUI({
      tagList(
        tags$span(class = "num-block", round(vehicles_eq, 4)), 
        tags$span(class = "text-bold", "miles driven by an average gasoline-powered passenger vehicle"),
        tags$span(class = "icon", "🚗")
      )
    })
    
    output$gasoline_output <- renderUI({
      tagList(
        tags$span(class = "num-block", round(gasoline_eq, 2)), 
        tags$span(class = "text-bold", "gallons of gasoline consumed"),
        tags$span(class = "icon", "⛽")
      )
    })
    
    output$trees_output <- renderUI({
      tagList(
        tags$span(class = "num-block", round(trees_eq, 1)), 
        tags$span(class = "text-bold", "tree seedlings grown for 10 years"),
        tags$span(class = "icon tree-icon", "🌱")
      )
    })
  })
}

shinyApp(ui = ui, server = server)

