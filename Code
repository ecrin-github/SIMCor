library(shiny)
library(dplyr)
library(tidyr)
library(DT)
library(shinydashboard)
library(rmarkdown)
library(knitr)
library(echarts4r)
library(plotly)

options(digits = 4)

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
  
  titlePanel(
    title = div(
      img(src = "simcor_logo.jpg", height = "6%", width = "6%", align = "right"),
      "Statistical Analysis"
    )
  ),
  sidebarPanel(
    fileInput("file", "Choose CSV file", accept = ".csv"),
    selectInput("trial_type", "Define the type of trial:", choices = c("1-group design", "2-group (parallel) design")),
    selectInput("sampling_type", "Select full dataset or random sampling:", choices = c("Full dataset", "Random sampling")),
    conditionalPanel(
      condition = "input.sampling_type != 'Full dataset'",
      numericInput("sample_size", "Choose the size of the sample:", min = 1, value = 5, step = 1),
      numericInput("replications", "Specify the number of replications:", min = 1, value = 1, step = 1)
    ),
    # Disable subsample inputs if full dataset is selected
    conditionalPanel(
      condition = "input.sampling_type == 'Full dataset'",
      uiOutput("disable_subsampling")
    ),
    selectInput("variable", "Choose a variable:", choices = NULL)
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel(
        "View Data ",
        DT::dataTableOutput("dataset_table")
      ),
      
      tabPanel(
        "Summary Statistics",
        DT::dataTableOutput("summary_table")
      ),
      
      tabPanel(
        "Plots",
        plotlyOutput("variable_plot")
      )
    )
  )
)

server <- function(input, output, session) {
  
  random.samples <- function(dataset, sample.size, number.of.samples) {
    N <- dim(dataset)[1]
    dat.sample.all <- NULL
    
    for (s in 1:number.of.samples) {
      id.sample <- sample(1:N, replace = TRUE, size = sample.size)
      dat.sample <- dataset[id.sample, ]
      dat.sample$sample.id <- s
      dat.sample.all <- rbind(dat.sample.all, dat.sample)
    }
    
    # Move the "sample.id" column to the first position
    dat.sample.all <- dat.sample.all[, c("sample.id", names(dat.sample.all)[-1])]
    
    return(dat.sample.all[, -grep("sample.id\\.1", names(dat.sample.all))])
  }
  
  # Load data
  dataset <- reactive({
    req(input$file)
    read.csv(input$file$datapath, header = TRUE)
  })
  
  # Initialize reactiveValues object to store identification code or name
  id_code <- reactiveValues(code = NULL)
  
  # Initialize reactiveValues object to store random samples
  samples <- reactiveValues(data = NULL)
  
  # Update id_code when Submit button is clicked, only when Random Sampling is selected
  observeEvent(
    c(input$dataset, input$sampling_type, input$sample_size, input$replications),
    {
      if (input$sampling_type == "Full dataset") {
        id_code$code <- NULL
      } else {
        id <- paste(input$dataset, input$replications, input$sample_size, seq(input$replications), sep = ".")
        id_code$code <- id[1]
        
        # Generate random samples with replacement
        samples$data <- random.samples(dataset(), input$sample_size, input$replications)
      }
    }
  )
  
  # Generate data table
  output$dataset_table <- DT::renderDataTable({
    if (input$sampling_type == "Full dataset") {
      data_subset <- dataset()
      page_length <- nrow(data_subset)
      caption <- "Full dataset"
    } else {
      # Get stored random samples if available
      data_subset <- samples$data
      page_length <- nrow(data_subset)
      caption <- paste0(input$file$name, " (", input$replications, " replicates of ", input$sample_size, " samples)")
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
  output$summary_table <- DT::renderDataTable({
    if (is.null(dataset()))
      return(NULL)
    
    data_subset <- dataset()
    
    if (input$sampling_type == "Full dataset") {
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
      data_subset <- samples$data
      caption <- paste0("Summary statistics for ", input$replications, " replicates of ", input$sample_size, " samples")
      
      numeric_vars <- sapply(data_subset, is.numeric)
      
      summary_tables <- lapply(split(data_subset, data_subset$sample.id), function(subset) {
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
        
        # Add a row for the Sample ID title with sample.id number
        subset_summary <- rbind(data.frame(Variable = paste0("Sample ID (", unique(subset$sample.id), ")"), Mean = "", SD = "", Min = "", Max = ""), subset_summary)
        
        subset_summary
      })
      
      # Add space between summary tables
      summary_tables_with_space <- lapply(summary_tables, function(table) {
        rbind(table, rep("", ncol(table)))
      })
      
      # Combine the summary tables into a single table
      summary_table <- do.call(rbind, summary_tables_with_space)
    }
    
    datatable(
      summary_table,
      caption = caption,
      rownames = FALSE,
      filter = "top",
      extensions = "Buttons",
      options = list(
        dom = "Bfrtip",
        pageLength = nrow(summary_table),
        buttons = list("pdf", "excel")
      )
    )
  })
  
  # Generate variable choices
  observeEvent(dataset(), {
    variables <- names(dataset())
    updateSelectInput(session, "variable", choices = variables)
  })
  
  # Generate graph or boxplot
  output$variable_plot <- renderPlot({
    if (!is.null(input$variable)) {
      variable_data <- dataset()[[input$variable]]
      
      # Determine the plot type based on the variable type
      if (is.numeric(variable_data)) {
        # Numeric variable - create a boxplot
        plot_ly(y = variable_data, type = "box", name = input$variable)
      } else {
        # Non-numeric variable - create a bar chart
        variable_counts <- table(variable_data)
        plot_ly(labels = names(variable_counts), values = variable_counts, type = "pie") %>%
          layout(title = input$variable)
      }
    }
  })
  output$plot_output <- renderUI({
    variable_data <- dataset()[[input$variable]]
    
    if (is.numeric(variable_data)) {
      plotlyOutput("variable_plot")
    } else {
      echarts4rOutput("variable_pie_chart")
    }
  })
  observeEvent(input$variable, {
    if (!is.null(input$variable)) {
      variable_data <- dataset()[[input$variable]]
      
      if (is.numeric(variable_data)) {
        output$variable_plot <- renderPlotly({
          plot_ly(y = variable_data, type = "box", name = input$variable)
        })
      } else {
        output$variable_pie_chart <- renderEcharts4r({
          variable_counts <- table(variable_data)
          pie_chart <- pie(variable_counts, radius = "50%")
          pie_chart$set_global_opts(title_opts = opts_title(text = input$variable))
          pie_chart
        })
      }
    }
  })
}
 


# Run the application
shinyApp(ui = ui, server = server)
