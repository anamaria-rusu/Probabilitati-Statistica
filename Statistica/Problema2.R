library(shiny)
library(bslib)

ui <- fluidPage(
  titlePanel("Reprezentarea grafica a functiilor de repartitie"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "tipVA",
        label = "Tipul variabilei aleatoare",
        choices = list(
          "Normala" = "normala",
          "Normala parametrizata" = "normalaParametrizata",
          "Exponentiala" = "exponentiala",
          "Poisson" = "poisson",
          "Binomiala" = "binomiala"
        )
      ),
      numericInput(
        inputId = "n",
        label = "Numarul n:",
        value = 5,
        min = 1
      ),
      conditionalPanel(
        condition = "input.tipVA == 'normalaParametrizata'",
        numericInput(
          inputId = "miu",
          label = "Media:",
          value = 0
        ),
        numericInput(
          inputId = "sigma",
          label = "Deviatia standard",
          value = 1,
          min = 0.00001
        )
      ),
      conditionalPanel(
        condition = "input.tipVA == 'poisson' || input.tipVA == 'exponentiala'",
        numericInput(
          inputId = "lambda",
          label = "Lambda:",
          value = 1,
          min = 0.00001
        )
      ),
      conditionalPanel(
        condition = "input.tipVA == 'binomiala'",
        numericInput(
          inputId = "r",
          label = "Numarul de repetari:",
          value = 5,
          min = 1
        ),
        numericInput(
          inputId = "p",
          label = "Probabilitatea realizarii evenimentului:",
          value = 0.5,
          min = 0,
          max = 1
        )
      )
    ),
    
    mainPanel(
      plotOutput("repartitie")
    )
  )
)

server <- function(input, output) {
  
  output$repartitie <- renderPlot({
    n <- input$n
    tipVariabila <- input$tipVA
    
    if (tipVariabila == "binomiala") {
      r <- input$r
      p <- input$p
      x1 <- rbinom(n = n, size = r, prob = p)
      x2 <- 5 * x1 + 4
      x3 <- x1^3
      x4 <- cumsum(x1)

      par(mfrow = c(2, 2))
    
      plot(
        ecdf(x1),
        main = "Functia de repartitie pentru X",
        xlab = "X",
        ylab = "F(X)",
        col = "red"
      )
      plot(
        ecdf(x2),
        main = "Functia de repartitie pentru 5X+4",
        xlab = "X",
        ylab = "F(X)",
        col = "purple"
      )
      plot(
        ecdf(x3),
        main = "Functia de repartitie pentru X^3",
        xlab = "X",
        ylab = "F(X)",
        col = "violet"
      )
      plot(
        ecdf(x4),
        main = "Functia de repartitie pentru Σ(X)",
        xlab = "X",
        ylab = "F(X)",
        col = "blue"
      )
      
    } else if (tipVariabila == "poisson") {
      lambda <- input$lambda
      x1 <- rpois(n = n, lambda = lambda)
      x2 <- 3 * x1 + 2
      x3 <- x1^2
      x4 <- cumsum(x1)

      par(mfrow = c(2, 2))
    
      plot(
        ecdf(x1),
        main = "Functia de repartitie pentru X",
        xlab = "X",
        ylab = "F(X)",
        col = "red"
      )
      plot(
        ecdf(x2),
        main = "Functia de repartitie pentru 3X+2",
        xlab = "X",
        ylab = "F(X)",
        col = "purple"
      )
      plot(
        ecdf(x3),
        main = "Functia de repartitie pentru X^2",
        xlab = "X",
        ylab = "F(X)",
        col = "violet"
      )
      plot(
        ecdf(x4),
        main = "Functia de repartitie pentru Σ(X)",
        xlab = "X",
        ylab = "F(X)",
        col = "blue"
      )
      
    } else if (tipVariabila == "exponentiala") {
      lambda <- input$lambda
      x1 <- rexp(n = n, rate = lambda)
      x2 <- 2 - 5 * x1
      x3 <- x1^2
      x4 <- cumsum(x1)

      par(mfrow = c(2, 2))
    
      plot(
        ecdf(x1),
        main = "Functia de repartitie pentru X",
        xlab = "X",
        ylab = "F(X)",
        col = "red"
      )
      plot(
        ecdf(x2),
        main = "Functia de repartitie pentru 2-5X",
        xlab = "X",
        ylab = "F(X)",
        col = "purple"
      )
      plot(
        ecdf(x3),
        main = "Functia de repartitie pentru X^2",
        xlab = "X",
        ylab = "F(X)",
        col = "violet"
      )
      plot(
        ecdf(x4),
        main = "Functia de repartitie pentru Σ(X)",
        xlab = "X",
        ylab = "F(X)",
        col = "blue"
      )
      
    } else if (tipVariabila == "normala") {
      x1 <- rnorm(n = n, mean = 0, sd = 1)
      x2 <- 3 + 2 * x1
      x3 <- x1^2
      x4 <- cumsum(x1^2)

      par(mfrow = c(2, 2))
    
      plot(
        ecdf(x1),
        main = "Functia de repartitie pentru X",
        xlab = "X",
        ylab = "F(X)",
        col = "red"
      )
      plot(
        ecdf(x2),
        main = "Functia de repartitie pentru 3+2X",
        xlab = "X",
        ylab = "F(X)",
        col = "purple"
      )
      plot(
        ecdf(x3),
        main = "Functia de repartitie pentru X^2",
        xlab = "X",
        ylab = "F(X)",
        col = "violet"
      )
      plot(
        ecdf(x4),
        main = "Functia de repartitie pentru Σ(X^2)",
        xlab = "X",
        ylab = "F(X)",
        col = "blue"
      )
      
    } else if (tipVariabila == "normalaParametrizata") {
      media <- input$miu
      sigma <- input$sigma
      x1 <- rnorm(n = n, mean = media, sd = sigma)
      x2 <- 3 + 2 * x1
      x3 <- x1^2
      x4 <- cumsum(x1^2)

      par(mfrow = c(2, 2))
    
      plot(
        ecdf(x1),
        main = "Functia de repartitie pentru X",
        xlab = "X",
        ylab = "F(X)",
        col = "red"
      )
      plot(
        ecdf(x2),
        main = "Functia de repartitie pentru 3+2X",
        xlab = "X",
        ylab = "F(X)",
        col = "purple"
      )
      plot(
        ecdf(x3),
        main = "Functia de repartitie pentru X^2 sau X^3",
        xlab = "X",
        ylab = "F(X)",
        col = "violet"
      )
      plot(
        ecdf(x4),
        main = "Functia de repartitie pentru Σ(X^2)",
        xlab = "X",
        ylab = "F(X)",
        col = "blue"
      )
    }
  })
}

shinyApp(ui = ui, server = server)
