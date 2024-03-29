if (!require("shiny")) install.packages("shiny")
if (!require("shinyWidgets")) install.packages("shinyWidgets")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("MASS")) install.packages("MASS")
if (!require("pwr")) install.packages("pwr")

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(MASS)
library(pwr)

ui <- fluidPage(
  titlePanel("Statistical Power Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("mean1", "Mean 1", 10),
      numericInput("mean2", "Mean 2", 20),
      numericInput("sd", "Common SD", 20),
      sliderInput("power", "Power", min = 0.1, max = 0.99, value = 0.9),
      sliderInput("sigLevel", "Significance Level", min = 0.01, max = 0.1, value = 0.05),
      radioButtons("alternative", "Alternative",
                   choices = list("One-sided" = "one.sided", "Two-sided" = "two.sided")),
      actionButton("calcBtn", "Calculate")
    ),
    mainPanel(
      verbatimTextOutput("sampleSize"),
      plotOutput("sampleSizeDist"),
      verbatimTextOutput("summaryStats")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$calcBtn, {
    # Calculate sample size for the specified parameters
    sample.size <- reactive({
      power.t.test(delta = input$mean2 - input$mean1,
                   sd = input$sd,
                   type = "two.sample",
                   sig.level = input$sigLevel,
                   power = input$power,
                   alternative = input$alternative)
    })
    
    output$sampleSize <- renderPrint({
      sample.size()
    })
    
    # Sample size distribution based on effect sizes
    output$sampleSizeDist <- renderPlot({
      set.seed(123) # For reproducibility
      B <- 10000
      lower.ES <- 0.2
      upper.ES <- 1
      ES <- runif(B, lower.ES, upper.ES)
      n.star <- numeric(B)
      
      for(b in 1:B) {
        n.star[b] <- power.t.test(delta = ES[b], 
                                  sd = 1, 
                                  type = "two.sample", 
                                  sig.level = input$sigLevel, 
                                  power = input$power, 
                                  alternative = input$alternative)$n
      }
      
      hist(n.star, breaks = 50, main = "Sample Size Distribution", xlab = "n")
    })
    
    # Summary statistics of the sample size distribution
    output$summaryStats <- renderPrint({
      set.seed(123) # For reproducibility
      B <- 10000
      lower.ES <- 0.2
      upper.ES <- 1
      ES <- runif(B, lower.ES, upper.ES)
      n.star <- numeric(B)
      
      for(b in 1:B) {
        n.star[b] <- power.t.test(delta = ES[b], 
                                  sd = 1, 
                                  type = "two.sample", 
                                  sig.level = input$sigLevel, 
                                  power = input$power, 
                                  alternative = input$alternative)$n
      }
      
      summary(n.star)
    })
  })
}

shinyApp(ui = ui, server = server)
