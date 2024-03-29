library(broom)
library(ggplot2)
library(survival)
library(plotly)
library(shiny)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("One and Two Group Assessment"),
  
  # Create a tabset that contains both the assessment and sample size calculation
  tabsetPanel(
    # First tab for assessment
    tabPanel("One and Two Group Assessment", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("trial_type", "Define the type of trial:", 
                             choices = c("1-group design", "2-group (parallel) design")),
                 fileInput("file1", "Choose CSV file for Group 1", accept = ".csv"),
                 conditionalPanel(
                   condition = "input.trial_type == '2-group (parallel) design'",
                   fileInput("file2", "Choose CSV file for Group 2", accept = ".csv")
                 ),
                 selectInput("variableName", "Select Variable Name", 
                             choices = c("Select a variable", "y.1", "y.2", "y.3")),
                 selectInput("dataChoice", "Choose outcome variable Type", 
                             choices = c("Select an outcome type", "Continuous", "Discrete", "Time to Event")),
                 actionButton("btn", "Perform Analysis")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Data", tableOutput("table")),
                   tabPanel("Plot", plotlyOutput("plot")),
                   tabPanel("Analysis", verbatimTextOutput("analysisOutput"))
                 )
               )
             )
    ),
    
    # Second tab for sample size calculation
    tabPanel("Sample Size Calculation", 
             sidebarLayout(
               sidebarPanel(
                 numericInput("mean1", "Mean of device 1:", 10),
                 numericInput("mean2", "Mean of device 2:", 20),
                 numericInput("sd", "Common standard deviation:", 20),
                 numericInput("alpha", "Significance Level (alpha):", value = 0.05, min = 0, max = 1, step = 0.01),
                 sliderInput("power", "Power:", min = 0, max = 1, value = 0.9, step = 0.1),
                 radioButtons("alternative", "Hypothesis type:", c("One-sided" = "one.sided", "Two-sided" = "two.sided")),
                 numericInput("lowerES", "Lower bound for Effect Size:", 0.2),
                 numericInput("upperES", "Upper bound for Effect Size:", 1),
                 sliderInput("B", "Number of scenarios for uncertainty (0 for no uncertainty):", min = 0, max = 10000, value = 0)
               ),
               mainPanel(
                 textOutput("sampleSizeText"),
                 plotOutput("sampleSizePlot"),
                 textOutput("summaryStatsText"),
                 textOutput("probabilityPowerText")
               )
             )
    )
  )
)

server <- function(input, output) {
  
  # Reactive values for storing the uploaded data
  uploadedData1 <- reactiveVal()
  uploadedData2 <- reactiveVal()
  
  # Observe changes in file input for file1
  observe({
    req(input$file1)
    file1 <- input$file1
    if (is.null(file1)) {
      return(NULL)
    }
    uploadedData1(read.csv(file1$datapath))
  })
  
  # Observe changes in file input for file2
  observe({
    req(input$file2)
    file2 <- input$file2
    if (is.null(file2)) {
      return(NULL)
    }
    uploadedData2(read.csv(file2$datapath))
  })
  
  # Combine or select data based on the trial type
  combinedData <- reactive({
    if (input$trial_type == "2-group (parallel) design") {
      data1 <- uploadedData1()
      data2 <- uploadedData2()
      if (is.null(data1) || is.null(data2)) {
        return(NULL)
      }
      return(rbind(data1, data2))
    } else {
      return(uploadedData1())
    }
  })
  
  # Display table of data
  output$table <- renderTable({
    head(combinedData(), 10) 
  })
  
  # Analysis and Visualization
  observeEvent(input$btn, {
    data <- combinedData()
    req(data)
    
    
    # Analysis and Visualization for Continuous outcome variable
    if (input$dataChoice == "Continuous") {
      
      # Retrieve the selected variable 'y.1' from the data
      variable_selected <- data$y.1
      
      # Check if it is a one-group design or two-group design
      if (input$trial_type == "1-group design") {
        # One-group design: Perform t.test without grouping
        result <- t.test(variable_selected ~ 1)
      } else if (input$trial_type == "2-group (parallel) design") {
        # Two-group design: Perform t.test with grouping
        result <- t.test(variable_selected ~ data$device)
      }
      
      # Extracting the t-test results and formatting them
      t_value <- formatC(result$statistic, format = "f", digits = 4)
      df_value <- formatC(result$parameter, format = "f", digits = 2)
      
      # Handling p-value for better representation
      if(result$p.value < 0.0001) {
        p_value <- formatC(result$p.value, format = "e", digits = 4)
      } else {
        p_value <- formatC(result$p.value, format = "f", digits = 4)
      }
      
      # Creating the result string
      ttest_result <- paste("t =", t_value, ", df =", df_value, ", p-value =", p_value)
      
      # Summary statistics
      if (input$trial_type == "1-group design") {
        # One-group design: Compute summary statistics for the uploaded data
        summary_stats <- data %>%
          summarize(mean = mean(variable_selected, na.rm = TRUE),
                    sd = sd(variable_selected, na.rm = TRUE),
                    min = min(variable_selected, na.rm = TRUE),
                    max = max(variable_selected, na.rm = TRUE),
                    N = sum(!is.na(variable_selected)))
      } else {
        # Two-group design: Compute summary statistics for each group
        summary_stats <- data %>%
          group_by(device) %>%
          summarize(mean = mean(variable_selected, na.rm = TRUE),
                    sd = sd(variable_selected, na.rm = TRUE),
                    min = min(variable_selected, na.rm = TRUE),
                    max = max(variable_selected, na.rm = TRUE),
                    N = sum(!is.na(variable_selected)))
      }
      
      stats_output <- capture.output(pander(summary_stats))
      output$analysisOutput <- renderText({
        paste(ttest_result, paste(stats_output, collapse = "\n"), sep = "\n\n")
      })
      
      output$plot <- renderPlotly({
        p <- ggplot(data, aes(x=factor(device), y=y.1, color=factor(device))) + geom_boxplot()
        ggplotly(p)
      })
      
      
    } else if (input$dataChoice == "Discrete") {
      output$analysisOutput <- renderText({
        # Assuming 'y.2' is your variable of interest and 'device' differentiates the groups
        tab <- table(data$y.2, data$device)
        test <- chisq.test(tab)
        
        # Formatting chi-squared test result
        chi_value <- formatC(test$statistic, format = "f", digits = 4)
        df_value <- formatC(test$parameter, format = "f", digits = 0)
        
        # Handling p-value for better representation
        p_value <- ifelse(test$p.value < 0.0001, 
                          formatC(test$p.value, format = "e", digits = 4), 
                          formatC(test$p.value, format = "f", digits = 4))
        
        # Creating the result string
        chi_result <- paste("X-squared =", chi_value, ", df =", df_value, ", p-value =", p_value)
        
        chi_result
      })
      
      output$plot <- renderPlotly({
        p <- ggplot(data, aes(x=device, fill=y.2)) +
          geom_bar(position = "dodge") +
          labs(x = "Device", y = "Count", fill = "Response") +
          theme_minimal()
        ggplotly(p)
      })
      
    } else if (input$dataChoice == "Time to Event") {
      
      
      # Determine if it's a one-group or two-group design and execute the code accordingly
      if (input$trial_type == "1-group design") {
        # One-group design
        output$analysisOutput <- renderText({
          
          # Assuming 'y.3' represents the time to event and 'device' is the grouping factor
          test <- survfit(Surv(y.3) ~ 1, data = data)
          print(test)
        })
        
        
        
        output$plot <- renderPlotly({
          m.surv <- survfit(Surv(get(input$variableName)) ~ 1, data = data)
          surv_data <- broom::tidy(m.surv)
          
          p <- ggplot(surv_data, aes(x=time, y=estimate)) + 
            geom_step(color="#00BFC4") + # This sets the color of the survival curve
            labs(x = "Time", y = "Survival Probability") +
            theme_minimal()
          ggplotly(p)
        })
        
        
        
      } else if (input$trial_type == "2-group (parallel) design") {
        # Two-group design
        output$analysisOutput <- renderText({
          test <- survdiff(Surv(get(input$variableName)) ~ device, data = data)
          chisq_result <- round(test$chisq, 1) # Rounding the test result to 1 decimal place
          p_value <- round(1 - pchisq(test$chisq, df=1), 3) # Calculate the p-value and round it to 3 decimal places
          paste("Chisq=", chisq_result, "on 1 degrees of freedom, p=", p_value)
        })
        
        output$plot <- renderPlotly({
          m.surv <- survfit(Surv(get(input$variableName)) ~ device, data = data)
          surv_data <- broom::tidy(m.surv)
          
          p <- ggplot(data = surv_data, aes(x=time, y=estimate, color=strata)) + 
            geom_step() + 
            labs(color = "Device")
          ggplotly(p)
        })
      }
    }
  })
  #sample_size_calculation
  # Reactive expression for sample size calculation
  sample_size_calculation <- reactive({
    power.t.test(delta = input$mean2 - input$mean1, 
                 sd = input$sd,
                 sig.level = input$alpha, 
                 power = input$power, 
                 type = "two.sample", 
                 alternative = input$alternative)$n
  })
  
  # Output the reactive expression directly
  output$sampleSizeText <- renderText({
    paste("Sample size per group is", round(sample_size_calculation(), 2), "for", input$alternative, "test.")
  })
  
  # Reactive for calculating sample size with uncertainty
  reactive_sample_size_with_uncertainty <- reactive({
    if (input$B > 0) {
      ES <- runif(input$B, input$lowerES, input$upperES) # Effect sizes are drawn from user-defined uniform distribution
      n.star <- sapply(ES, function(e) {
        power.t.test(delta = e, 
                     sd = input$sd, 
                     sig.level = input$alpha, 
                     power = input$power, 
                     type = "two.sample", 
                     alternative = "two.sided")$n
      })
      n.star # Return the sample sizes
    }
  })
  
  # Render the histogram plot of the sample size distribution
  output$sampleSizePlot <- renderPlot({
    n.star <- reactive_sample_size_with_uncertainty()
    if (!is.null(n.star)) {
      hist(n.star, breaks = 50, main = "Sample size distribution", xlab = "n")
    }
  })
  
  # Output for summary statistics text
  output$summaryStatsText <- renderText({n.star <- reactive_sample_size_with_uncertainty()
  if (!is.null(n.star)) {
    summary_stats <- summary(n.star)
    paste("Summary Statistics - Min:", summary_stats["Min."], 
          "1st Qu.:", summary_stats["1st Qu."], 
          "Median:", summary_stats["Median"], 
          "Mean:", summary_stats["Mean"], 
          "3rd Qu.:", summary_stats["3rd Qu."], 
          "Max:", summary_stats["Max."])
  }
  })
  output$probabilityPowerText <- renderText({
    n.star <- reactive_sample_size_with_uncertainty()
    if (!is.null(n.star)) {
      # Define sample sizes for which to calculate power
      sample_sizes <- c(85, 106, 129)
      # Initialize vector to store probabilities
      probabilities <- numeric(length(sample_sizes))
      # Calculate probabilities for each sample size
      for (i in seq_along(sample_sizes)) {
        # Vectorized calculation of power for the current sample size across all effect sizes
        powers <- sapply(n.star, function(n) {
          power.t.test(n = n, 
                       delta = input$mean2 - input$mean1, 
                       sd = input$sd, 
                       sig.level = input$alpha, 
                       type = "two.sample", 
                       alternative = "two.sided")$power
        })
        # Calculate the probability of power being less than 0.9
        probabilities[i] <- mean(powers < 0.9) * 100
      }
      
      # Create output text with the probabilities
      output_text <- paste("Probability that the power is less than 0.9:",
                           paste(sapply(seq_along(sample_sizes), function(i) {
                             paste("for n =", sample_sizes[i], "is", round(probabilities[i], 2), "%")
                           }), collapse = ", "))
      output_text
    }
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
