#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
 
library(shiny)

ui <- fluidPage(
  titlePanel("Interactive Slope Field Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("equation", 
                label = "Enter differential equation dy/dx = f(x, y):", 
                value = "x - y"),
      numericInput("xrange", "X range (+/-):", 5, min = 1, max = 20),
      numericInput("yrange", "Y range (+/-):", 5, min = 1, max = 20),
      numericInput("density", "Field density (points per axis):", 20, min = 5, max = 50),
      actionButton("plotBtn", "Plot Slope Field")
    ),
    
    mainPanel(
      plotOutput("slopePlot", height = "600px")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$plotBtn, {
    output$slopePlot <- renderPlot({
      
      # Create grid
      x <- seq(-input$xrange, input$xrange, length.out = input$density)
      y <- seq(-input$yrange, input$yrange, length.out = input$density)
      grid <- expand.grid(x = x, y = y)
      
      # Define slope function safely
      f <- function(x, y) {
        eval(parse(text = input$equation))
      }
      
      # Compute slope values
      slopes <- mapply(f, grid$x, grid$y)
      dx <- 1 / sqrt(1 + slopes^2)
      dy <- slopes / sqrt(1 + slopes^2)
      
      # Plot slope field
      plot(grid$x, grid$y, type = "n",
           xlab = "x", ylab = "y",
           main = paste("Slope Field for dy/dx =", input$equation))
      
      # Draw arrows for slope field
      arrows(grid$x - dx/2, grid$y - dy/2,
             grid$x + dx/2, grid$y + dy/2,
             length = 0.05, col = "steelblue")
      
      # Add thick black lines at x=0 and y=0
      abline(h = 0, v = 0, col = "black", lwd = 2)
    })
  })
}

shinyApp(ui = ui, server = server)

