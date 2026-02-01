
library(ggplot2)

ui <- fluidPage(
  titlePanel("Funcții de Repartiție - Vizualizare Interactivă"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("distribution", "Selectează Distribuția:",
                  choices = c("Normal Standard N(0,1)" = "norm_std",
                              "Normal N(μ, σ²)" = "norm_general",
                              "Exponențială Exp(λ)" = "exp",
                              "Poisson Pois(λ)" = "pois",
                              "Binomială Binom(r, p)" = "binom")),
      
      conditionalPanel(
        condition = "input.distribution == 'norm_general'",
        numericInput("mu", "μ (media):", value = 0, step = 0.5),
        numericInput("sigma", "σ (deviere standard):", value = 1, min = 0.1, step = 0.1)
      ),
      
      conditionalPanel(
        condition = "input.distribution == 'exp'",
        numericInput("lambda_exp", "λ (rată):", value = 1, min = 0.1, step = 0.1)
      ),
      
      conditionalPanel(
        condition = "input.distribution == 'pois'",
        numericInput("lambda_pois", "λ (medie):", value = 3, min = 0.1, step = 0.5)
      ),
      
      conditionalPanel(
        condition = "input.distribution == 'binom'",
        numericInput("r", "r (număr încercări):", value = 10, min = 1, step = 1),
        numericInput("p", "p (probabilitate succes):", value = 0.5, min = 0, max = 1, step = 0.05)
      ),
      
      numericInput("n", "n (dimensiune eșantion pentru sume):", value = 10, min = 1, step = 1),
      
      selectInput("transformation", "Selectează Transformarea:",
                  choices = c("X" = "x",
                              "Transformare liniară" = "linear",
                              "X²" = "x2",
                              "X³" = "x3",
                              "Σ(Xi)" = "sum",
                              "Σ(Xi²)" = "sum_sq"))
    ),
    
    mainPanel(
      plotOutput("cdfPlot", height = "500px"),
      verbatimTextOutput("statistics")
    )
  )
)

server <- function(input, output) {
  

  generate_data <- reactive({
    n <- input$n
    
    data <- switch(input$distribution,
                   "norm_std" = rnorm(10000),
                   "norm_general" = rnorm(10000, mean = input$mu, sd = input$sigma),
                   "exp" = rexp(10000, rate = input$lambda_exp),
                   "pois" = rpois(10000, lambda = input$lambda_pois),
                   "binom" = rbinom(10000, size = input$r, prob = input$p))
    

    # apply transf
    transformed_data <- switch(input$transformation,
                                "x" = data,
                                "linear" = apply_linear_transform(data, input$distribution),
                                "x2" = data^2,
                                "x3" = data^3,
                                "sum" = compute_sums(data, n),
                                "sum_sq" = compute_sum_squares(data, n))
    
    transformed_data
  })
  
# linear transf
  apply_linear_transform <- function(data, dist) {
    switch(dist,
           "norm_std" = 3 + 2*data,
           "norm_general" = 3 + 2*data,
           "exp" = 2 - 5*data,
           "pois" = 3*data + 2,
           "binom" = 5*data + 4)
  }
  
# calc sums
  compute_sums <- function(data, n) {
    matrix_data <- matrix(data[1:(length(data) - length(data) %% n)], ncol = n)
    rowSums(matrix_data)
  }
  
# calc squares sum
  compute_sum_squares <- function(data, n) {
    matrix_data <- matrix(data[1:(length(data) - length(data) %% n)], ncol = n)
    rowSums(matrix_data^2)
  }
  
#  repart function
  output$cdfPlot <- renderPlot({
    data <- generate_data()
    
    # calc ecdf
    ecdf_func <- ecdf(data)
    x_vals <- seq(min(data), max(data), length.out = 1000)
    y_vals <- ecdf_func(x_vals)
    
    df <- data.frame(x = x_vals, y = y_vals)
    
    ggplot(df, aes(x = x, y = y)) +
      geom_line(color = "blue", size = 1.2) +
      labs(title = "Funcția de Repartiție Empirică",
           x = "Valoare",
           y = "F(x) = P(X ≤ x)") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })


# descriptive stats
  output$statistics <- renderText({
    data <- generate_data()
    
    paste0(
      "Statistici Descriptive:\n",
      "------------------------\n",
      "Media: ", round(mean(data), 4), "\n",
      "Devierea Standard: ", round(sd(data), 4), "\n",
      "Min: ", round(min(data), 4), "\n",
      "Max: ", round(max(data), 4), "\n",
      "Mediana: ", round(median(data), 4), "\n",
      "Q1: ", round(quantile(data, 0.25), 4), "\n",
      "Q3: ", round(quantile(data, 0.75), 4)
    )
  })
}


shinyApp(ui = ui, server = server)