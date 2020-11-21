source("R/packages.R")
source("R/functions.R")

ui <- semanticPage(
  title = "My page",
  textInput(
    'k',
    'Enter a vector of zeros and ones',
    placeholder = "For example: 0101"
    ),
  plotOutput("plot")
)

server <- function(input, output) {
  k <- reactive({
    req(input$k)
    string2vec(input$k)
  })
  
  output$plot <- renderPlot({
    # move this to separate function
    populate_posteriors(k(), rep(1, 1000)) %>% 
      ggplot(aes(p_grid, posterior, color = id, group = id)) +
          geom_line() + 
        labs(title = "Posteriors after each data point")
  })
}

shinyApp(ui, server)





