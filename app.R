ui <- semanticPage(
  title = "Bayesian updating",
  header(
    "Bayesian updating for binomial trials", 
    HTML("<p style='font-size:10px'>Author: <a href='https://www.mihagazvoda.com'>Miha Gazvoda</a></p>")
    ),
  textInput(
    "outcome",
    "Vector of outcomes (only 0 and 1 allowed):",
    value = "011",
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
    numericInput(
      "step_border",
      "Border value (between 0 and 1):",
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
  outcome <- reactive({
    req(input$outcome)
    validate(
      need(are_outcomes_valid(input$outcome), "Possible outcomes are only 0 and 1!")
    )
    string2vec(input$outcome)
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

    prior
  })

  output$plot <- renderPlot({
    populate_posteriors(outcome(), prior()) %>%
      plot_posteriors()
  })
}

shinyApp(ui, server)
