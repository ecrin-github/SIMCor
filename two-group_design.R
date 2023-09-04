library(shiny)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)

# Save the data to a CSV file
save_data <- function(data, file_name) {
  file_path <- file.path("data", paste0(file_name, ".csv"))
  write.csv(data, file_path, row.names = FALSE)
}

# Read the data from a CSV file
read_data <- function(file_name) {
  file_path <- file.path("data", paste0(file_name, ".csv"))
  if (file.exists(file_path)) {
    read.csv(file_path)
  } else {
    NULL
  }
}

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML('
    body {
      background-color: #f2f2f2;
    }
    .navbar-default {
      background-color: #6c757d;
      border-color: #6c757d;
    }
    .navbar-default .navbar-brand,
    .navbar-default .navbar-brand:hover,
    .navbar-default .navbar-brand:focus,
    .navbar-default .navbar-nav > li > a {
      color: #f8f9fa;
    }
  ')))
  ,
  titlePanel(title = div(img(src = "simcor_logo.jpg", height = "6%", width = "6%", align = "right"), "Statistical Analysis")),
  sidebarPanel(
    fileInput("file1", "Select real dataset (shapeFeatures_real.csv)"),
    fileInput("file2", "Select synthetic dataset (shapeFeatures_synthetic.csv)"),
    selectInput("sampling_type1", "Select sampling type for Dataset 1:", choices = c("Full dataset", "Random sampling")),
    selectInput("sampling_type2", "Select sampling type for Dataset 2:", choices = c("Full dataset", "Random sampling")),
    conditionalPanel(
      condition = "input.sampling_type1 != 'Full dataset'",
      numericInput("sample_size1", "Choose the size of the sample for Dataset 1:", min = 1, value = 5, step = 1),
      numericInput("replications1", "Specify the number of replications for Dataset 1:", min = 1, value = 1, step = 1)
    ),
    conditionalPanel(
      condition = "input.sampling_type2 != 'Full dataset'",
      numericInput("sample_size2", "Choose the size of the sample for Dataset 2:", min = 1, value = 5, step = 1),
      numericInput("replications2", "Specify the number of replications for Dataset 2:", min = 1, value = 1, step = 1)
    ),
    selectInput("variable", "Choose a variable:", choices = c("D_lvot", "D_an", "D_sinus", "D_stj", "AVA", "angle", "ratio_lvot_over_an_area", "H_sinus"))
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("View Data",
               DT::dataTableOutput("dataset1_table"),
               DT::dataTableOutput("dataset2_table")),
      tabPanel("Summary Statistics",
               tableOutput("summary_table1"),
               tableOutput("summary_table2")),
      tabPanel("Group Comparison",
               plotlyOutput("group_comparison_plot")
      )
    )
  )
  
)

server <- function(input, output, session) {
  
  # Read datasets
  realData <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, stringsAsFactors = FALSE)
  })
  
  syntheticData <- reactive({
    req(input$file2)
    read.csv(input$file2$datapath, stringsAsFactors = FALSE)
  })
  
  
  # Initialize reactiveValues object to store identification code or name
  id_code <- reactiveValues(code = NULL)
  
  # Initialize reactiveValues object to store random samples
  samples <- reactiveValues(data = NULL)
  
  # Update id_code when Submit button is clicked, only when Random Sampling is selected
  observeEvent(c(input$dataset, input$sampling_type, input$sample_size, input$replications),
               {
                 if(input$sampling_type == "Full dataset") {
                   id_code$code <- NULL
                 } else {
                   id <- paste(input$dataset, input$replications, input$sample_size, seq(input$replications), sep = ".")
                   id_code$code <- id[1]
                   
                   # Generate random samples with replacement
                   samples$data <- random.samples(realData(), input$sample_size, input$replications)
                 }
               }
  )
  # Generate data tables
  output$dataset1_table <- DT::renderDataTable({
    if (input$sampling_type1 == "Full dataset") {
      data_subset <- realData()
      page_length <- nrow(data_subset)
      caption <- "Full dataset"
    } else {
      # Get stored random samples if available
      data_subset <- samples$data
      page_length <- nrow(data_subset)
      caption <- paste0(input$file1$name, " (", input$replications1, " replicates of ", input$sample_size1, " samples)")
    }
    
    datatable(
      data_subset,
      caption = caption,
      rownames = FALSE,
      filter = "top",
      extensions = "Buttons",
      options = list(
        dom = "Bfrtip",
        pageLength = page_length,
        buttons = list("pdf", "excel")
      )
    )
  })
  
  output$dataset2_table <- DT::renderDataTable({
    if (input$sampling_type2 == "Full dataset") {
      data_subset <- syntheticData()
      page_length <- nrow(data_subset)
      caption <- "Full dataset"
    } else {
      # Get stored random samples if available
      data_subset <- samples$data
      page_length <- nrow(data_subset)
      caption <- paste0(input$file2$name, " (", input$replications2, " replicates of ", input$sample_size2, " samples)")
    }
    
    datatable(
      data_subset,
      caption = caption,
      rownames = FALSE,
      filter = "top",
      extensions = "Buttons",
      options = list(
        dom = "Bfrtip",
        pageLength = page_length,
        buttons = list("pdf", "excel")
      )
    )
  })
  
  
  # Generate summary statistics
  output$summary_table1 <- renderTable({
    data_subset <- realData()
    
    if (input$sampling_type1 == "Full dataset") {
      caption <- "Summary statistics for full dataset"
      
      numeric_vars <- sapply(data_subset, is.numeric)
      
      # Exclude "id" variable from summary statistics
      numeric_vars <- numeric_vars & !(names(data_subset) %in% c("id"))
      
      summary_table <- data.frame(
        Variable = names(data_subset)[numeric_vars],
        Mean = round(sapply(data_subset[numeric_vars], mean, na.rm = TRUE), 2),
        SD = round(sapply(data_subset[numeric_vars], sd, na.rm = TRUE), 2),
        Min = sapply(data_subset[numeric_vars], min, na.rm = TRUE),
        Max = sapply(data_subset[numeric_vars], max, na.rm = TRUE)
      )
    } else {
      data_subset <- realData()
      caption <- paste0("Summary statistics for ", input$replications1, " replicates of ", input$sample_size1, " samples")
      
      numeric_vars <- sapply(data_subset, is.numeric)
      
      summary_tables <- lapply(1:input$replications1, function(replication) {
        subset <- data_subset[sample(nrow(data_subset), input$sample_size1), ]
        subset_summary <- data.frame(
          Variable = names(subset)[numeric_vars],
          Mean = round(sapply(subset[numeric_vars], mean, na.rm = TRUE), 2),
          SD = round(sapply(subset[numeric_vars], sd, na.rm = TRUE), 2),
          Min = sapply(subset[numeric_vars], min, na.rm = TRUE),
          Max = sapply(subset[numeric_vars], max, na.rm = TRUE)
        )
        
        subset_summary <- subset_summary[!(subset_summary$Variable %in% c("id", "sample.id")), ]
        subset_summary <- subset_summary[c("Variable", "Mean", "SD", "Min", "Max")]
        colnames(subset_summary) <- c("Variable", "Mean", "SD", "Min", "Max")
        
        # Add a row for the Sample ID title with replication number
        subset_summary <- rbind(data.frame(Variable = paste0("Replication (", replication, ")"), Mean = "", SD = "", Min = "", Max = ""), subset_summary)
        
        subset_summary
      })
      
      # Add space between summary tables
      summary_tables_with_space <- lapply(summary_tables, function(table) {
        rbind(table, rep("", ncol(table)))
      })
      
      # Combine the summary tables into a single table
      summary_table <- do.call(rbind, summary_tables_with_space)
    }
    
    summary_table
  })
  
  output$summary_table2 <- renderTable({
    data_subset <- syntheticData()
    
    if (input$sampling_type2 == "Full dataset") {
      caption <- "Summary statistics for full dataset"
      
      numeric_vars <- sapply(data_subset, is.numeric)
      
      # Exclude "id" variable from summary statistics
      numeric_vars <- numeric_vars & !(names(data_subset) %in% c("id"))
      
      summary_table <- data.frame(
        Variable = names(data_subset)[numeric_vars],
        Mean = round(sapply(data_subset[numeric_vars], mean, na.rm = TRUE), 2),
        SD = round(sapply(data_subset[numeric_vars], sd, na.rm = TRUE), 2),
        Min = sapply(data_subset[numeric_vars], min, na.rm = TRUE),
        Max = sapply(data_subset[numeric_vars], max, na.rm = TRUE)
      )
    } else {
      data_subset <- syntheticData()
      caption <- paste0("Summary statistics for ", input$replications2, " replicates of ", input$sample_size2, " samples")
      
      numeric_vars <- sapply(data_subset, is.numeric)
      
      summary_tables <- lapply(1:input$replications2, function(replication) {
        subset <- data_subset[sample(nrow(data_subset), input$sample_size2), ]
        subset_summary <- data.frame(
          Variable = names(subset)[numeric_vars],
          Mean = round(sapply(subset[numeric_vars], mean, na.rm = TRUE), 2),
          SD = round(sapply(subset[numeric_vars], sd, na.rm = TRUE), 2),
          Min = sapply(subset[numeric_vars], min, na.rm = TRUE),
          Max = sapply(subset[numeric_vars], max, na.rm = TRUE)
        )
        
        subset_summary <- subset_summary[!(subset_summary$Variable %in% c("id", "sample.id")), ]
        subset_summary <- subset_summary[c("Variable", "Mean", "SD", "Min", "Max")]
        colnames(subset_summary) <- c("Variable", "Mean", "SD", "Min", "Max")
        
        # Add a row for the Sample ID title with replication number
        subset_summary <- rbind(data.frame(Variable = paste0("Replication (", replication, ")"), Mean = "", SD = "", Min = "", Max = ""), subset_summary)
        
        subset_summary
      })
      
      # Add space between summary tables
      summary_tables_with_space <- lapply(summary_tables, function(table) {
        rbind(table, rep("", ncol(table)))
      })
      
      # Combine the summary tables into a single table
      summary_table <- do.call(rbind, summary_tables_with_space)
    }
    
    summary_table
  })
  
  
  # Generate group comparison boxplots
  output$group_comparison_plot <- renderPlotly({
    data_subset1 <- realData()
    data_subset2 <- syntheticData()
    
    variable_data1 <- data_subset1[[input$variable]]
    variable_data2 <- data_subset2[[input$variable]]
    
    if (is.numeric(variable_data1) && is.numeric(variable_data2)) {
      # Both variables are numeric
      
      # Create a combined data frame for boxplot
      boxplot_data <- data.frame(
        Dataset = c(rep("Real Cohort", length(variable_data1)), rep("Synthetic Cohort", length(variable_data2))),
        Value = c(variable_data1, variable_data2)
      )
      
      # Generate boxplot using plotly
      plot_ly(boxplot_data, x = ~Dataset, y = ~Value, type = "box",
              marker = list(color = c("seagreen2", "gold2")),
              ylab = input$variable, main = "Group Comparison")
    } else {
      # At least one variable is non-numeric
      if (!is.numeric(variable_data1)) {
        message <- paste("Variable", input$variable, "in the Real Cohort is not numeric.")
      }
      if (!is.numeric(variable_data2)) {
        message <- paste("Variable", input$variable, "in the Synthetic Cohort is not numeric.")
      }
      message
    }
  })
  
  
  
  
  
  
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)