# Load required libraries
library(shiny)
library(ggplot2)

# Load the Iris dataset
data(iris)

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Iris Dataset Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "species",
        "Choose Species:",
        choices = c("All", unique(iris$Species)),
        selected = "All"
      ),
      
      selectInput(
        "x_var",
        "X-axis Variable:",
        choices = names(iris)[1:4],
        selected = "Sepal.Length"
      ),
      
      selectInput(
        "y_var",
        "Y-axis Variable:",
        choices = names(iris)[1:4],
        selected = "Petal.Length"
      )
    ),
    
    mainPanel(
      plotOutput("scatterPlot"),
      tableOutput("summaryTable")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Filter data based on species selection
  filteredData <- reactive({
    if (input$species == "All") {
      iris
    } else {
      subset(iris, Species == input$species)
    }
  })
  
  # Create scatter plot
  output$scatterPlot <- renderPlot({
    ggplot(filteredData(), aes_string(x = input$x_var, y = input$y_var, color = "Species")) +
      geom_point(size = 3) +
      theme_minimal() +
      labs(
        title = "Scatter Plot of Iris Dataset",
        x = input$x_var,
        y = input$y_var
      )
  })
  
  # Show summary statistics
  output$summaryTable <- renderTable({
    summary(filteredData())
  })
}

# Run the application
shinyApp(ui = ui, server = server)

