library(shiny)
library(pander)
library(tidyverse)
library(MASS)
library(broom)
library(ggplot2)
library(survival)
library(plotly)

# UI
ui <- fluidPage(
  titlePanel("Data Simulation and Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataChoice", "Choose outcome variable Type", 
                  choices = c("Continuous", "Discrete", "Time to Event")),
      actionButton("btn", "Perform Analysis")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data", tableOutput("table")),
        tabPanel("Plot", plotlyOutput("plot")),  # Modified to plotlyOutput
        tabPanel("Analysis", verbatimTextOutput("analysisOutput"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Sample size calculation moved outside reactive to avoid re-calculation
  sd.common = 20
  mean1 = 10
  mean2 = 20
  
  sample.size = power.t.test(delta = mean1-mean2, sd = sd.common, 
                             type = "two.sample", 
                             sig.level = 0.05,
                             power = 0.9,
                             alternative = "two.sided")
  n1 = n2 = round(sample.size$n, 0)
  
  # Simulated data based on user choice
  simulatedData <- reactive({
    if (input$dataChoice == "Continuous") {
      set.seed(123456)
      y.1.A = rnorm(n1, mean = mean1, sd = sd.common)
      y.1.B = rnorm(n2, mean = mean2, sd = sd.common)
      rbind(data.frame(y.1 = y.1.A, devise = "A"),
            data.frame(y.1 = y.1.B, devise = "B"))
      
    } else if (input$dataChoice == "Discrete") {
      set.seed(123)
      y.2.A = rbinom(n1, size = 1, prob = 0.75)
      y.2.A = factor(y.2.A, labels=c("no", "yes"))
      y.2.B = rbinom(n2, size = 1, prob = 0.6)
      y.2.B = factor(y.2.B, labels=c("no", "yes"))
      rbind(data.frame(y.2 = y.2.A, devise = "A"),
            data.frame(y.2 = y.2.B, devise = "B"))
      
    } else if (input$dataChoice == "Time to Event") {
      y.3.A = rexp(n1, rate = 1)
      y.3.B = rexp(n1, rate = 1.5)
      rbind(data.frame(y.3 = y.3.A, devise = "A"),
            data.frame(y.3 = y.3.B, devise = "B"))
    }
  })
  
  # Data download
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataChoice, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(simulatedData(), file, row.names = FALSE)
    }
  )
  
 
  # Analysis and Visualization
  observeEvent(input$btn, {
    
    data <- simulatedData()
    
    if (input$dataChoice == "Continuous") {
      
      output$analysisOutput <- renderText({
        result <- t.test(y.1 ~ devise, data = data)
        ttest_result <- paste("t-test result:", result$statistic)
        
        # Statistical analysis of variable y.1
        summary_stats <- data %>%
          group_by(devise) %>%
          summarize(mean = mean(y.1),
                    sd = sd(y.1),
                    min = min(y.1),
                    max = max(y.1),
                    N = n())
        
        stats_output <- capture.output(pander(summary_stats))
        paste(ttest_result, paste(stats_output, collapse = "\n"), sep = "\n\n")
      })
      
      output$plot <- renderPlotly({
        p <- ggplot(data, aes(x=devise, y=y.1, color=devise)) + geom_boxplot()
        ggplotly(p)
      })
      
    } else if (input$dataChoice == "Discrete") {
      
      output$analysisOutput <- renderText({
        tab <- table(data$y.2, data$devise)
        test <- chisq.test(tab)
        paste("Chi-squared test result:", test$statistic)
      })
      
    } else if (input$dataChoice == "Time to Event") {
      
      output$analysisOutput <- renderText({
        test <- survdiff(Surv(y.3) ~ devise, data = data)
        paste("Survival difference test result:", test$chisq)
      })
      
      m.surv <- survfit(Surv(y.3) ~ devise, data = data)
      
      output$plot <- renderPlotly({
        surv_data <- broom::tidy(m.surv)
        p <- ggplot(data = surv_data, aes(x=time, y=estimate, color=strata)) + 
          geom_step() + 
          labs(color = "Devise")
        ggplotly(p)
      })
    }
  })
  
  # Moved outside the observeEvent
  output$table <- renderTable({
    head(simulatedData(), 10) # Display top 10 rows of the simulated data
  })
}

shinyApp(ui = ui, server = server)
