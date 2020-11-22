# alpha of lines?
# slider for confidence intervals
# only possible characters for text input

ui <- semanticPage(
  title = "My page",
  textInput(
    "k",
    "Enter a vector of zeros and ones",
    placeholder = "For example: 0101"
  ),
  selectInput(
    "prior",
    "Prior:",
    list(
      "uniform",
      "step",
      "normal"
    )
  ),
  conditionalPanel(
    condition = "input.prior == 'step'",
    numericInput(
      "step_start",
      "Start value:",
      value = 0,
      min = 0,
      step = 1
    ),
    # slider_input(
    #   "step_border",
    #   value = 0.5,
    #   min = 0,
    #   max = 1,
    #   step = 0.01
    # ),
    numericInput(
      "step_border",
      "Border value(between 0 and 1):",
      value = 0.5,
      min = 0,
      max = 1,
      step = 0.1
    ),
    numericInput(
      "step_end",
      "End value:",
      value = 1,
      min = 0,
      step = 1
    )
  ),
  conditionalPanel(
    condition = "input.prior == 'normal'",
    numericInput(
      "normal_mean",
      "Mean:",
      value = 0.5,
      min = 0,
      max = 1,
      step = 0.1
    ),
    numericInput(
      "normal_sd",
      "Standard deviation:",
      value = 0.1,
      min = 0,
      max = 1,
      step = 0.1
    )
  ),
  plotOutput("plot")
)

server <- function(input, output) {
  k <- reactive({
    req(input$k)
    string2vec(input$k)
  })

  prior <- reactive({
    req(input$prior)
    n_grid <- 100
    prior <- if (input$prior == "uniform") {
      rep(1, n_grid)
    } else if (input$prior == "step") {
      border_index <- floor(input$step_border * n_grid)
      c(
        rep(input$step_start, border_index),
        rep(input$step_end, n_grid - border_index)
      )
    } else if (input$prior == "normal") {
      grid <- seq(0, 1, length.out = n_grid)
      dnorm(grid, x = input$normal_mean, sd = input$normal_sd)
    }
    
    return(prior)
  })

  output$plot <- renderPlot({
    # move this to a separate function
    populate_posteriors(k(), prior()) %>%
      ggplot(aes(p_grid, posterior, color = id, group = id)) +
      geom_line() +
      labs(title = "Posteriors after each data point")
  })
}

shinyApp(ui, server)
