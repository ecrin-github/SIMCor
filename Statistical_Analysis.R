library(shiny)
library(dplyr)
library(tidyr)
library(JMbayes2)
library(DT)
library(shinydashboard)
library(rmarkdown)
library(knitr)
library(echarts4r)

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
  '))
  ),
  
  titlePanel(title = div(img(src = "simcor_logo.jpg", height = "6%", width = "6%", align = "right"), "Statistical Analysis")),
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
    
    br(),
    downloadButton("download_report", "Download Report")
    
  ),
  
  
  mainPanel(
    tabsetPanel(
      tabPanel("Full Dataset Table ",
               DT::dataTableOutput("dataset_table")),
      tabPanel("Random Sample Table",
               DT::dataTableOutput("sample_table")),
      tabPanel("Summary Statistics",
               plotOutput("summary_table"))
    )
  )
)


server <- function(input, output, session) {
  
  random.samples <- function(dataset, sample.size, number.of.samples) {
    
    N <- dim(dataset)[1]
    dat.sample.all <- NULL
    
    for(s in 1:number.of.samples) {
      id.sample <- sample(1:N, replace = TRUE, size = sample.size)
      dat.sample <- dataset[id.sample, ]
      dat.sample$sample.id <- s
      dat.sample.all <- rbind(dat.sample.all, dat.sample)
    }
    
    return(dat.sample.all)
    
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
  observeEvent(c(input$dataset, input$sampling_type, input$sample_size, input$replications),
               {
                 if(input$sampling_type == "Full dataset") {
                   id_code$code <- NULL
                 } else {
                   id <- paste(input$dataset, input$replications, input$sample_size, seq(input$replications), sep = ".")
                   id_code$code <- id[1]
                   
                   # Generate random samples with replacement
                   samples$data <- random.samples(dataset(), input$sample_size, input$replications)
                 }
               }
  )
  
  output$id_code <- renderText({
    id_code$code
  })
  
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
      caption <- paste0("Random sample table (", input$replications, " replicates of ", input$sample_size, " samples)")
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
        buttons = list("pdf", "excel", "word")
      )
    )
  })
  
  
}


# Run the application
shinyApp(ui = ui, server = server)
