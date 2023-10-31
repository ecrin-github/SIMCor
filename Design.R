library(shiny)
library(readxl)
library(data.table)
library(ggplot2)
library(plotly)
library(reshape2)
library(corrplot)
library(GGally)
library(boot)
library(dplyr)
library(shinydashboard)



ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@400;500;700&display=swap');
      body {
        font-family: 'Poppins', sans-serif;
      }
    "))
  ),
  
  titlePanel("SIMCor"),
  div(style = "display: flex; justify-content: space-around; align-items: center; height: 80vh;",
      
      # Validation of Virtual Cohorts Module
      tags$div(
        style = "width: 45%; height: 60%; background-color: #34B4C6; color: white; text-align: center; line-height: 300px; border-radius: 15px; transition: transform .2s, box-shadow .2s; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);",
        tags$a(href = "#", 
               "Validation of Virtual Cohorts",
               style = "color: white; text-decoration: none; font-size: 24px; font-weight: bold;",
               onclick = "Shiny.setInputValue('tab', 'validation', {priority: 'event'});",
               title = "Navigate to Validation of Virtual Cohorts",
               onmouseover = "$(this).css('box-shadow', '0 8px 16px rgba(0, 0, 0, 0.15)');",
               onmouseout = "$(this).css('box-shadow', '0 4px 8px rgba(0, 0, 0, 0.1)');"
        )
      ),
      
      # Application of Virtual Cohorts Module
      tags$div(
        style = "width: 45%; height: 60%; background-color: #1E6A9C; color: white; text-align: center; line-height: 300px; border-radius: 15px; transition: transform .2s, box-shadow .2s; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);",
        tags$a(href = "application_ui", 
               "Application of Virtual Cohorts",
               style = "color: white; text-decoration: none; font-size: 24px; font-weight: bold;",
               onclick = "navigateTo('application_ui')",
               title = "Navigate to Application of Virtual Cohorts",
               onmouseover = "$(this).css('box-shadow', '0 8px 16px rgba(0, 0, 0, 0.15)');",
               onmouseout = "$(this).css('box-shadow', '0 4px 8px rgba(0, 0, 0, 0.1)');"
        )
      )
  )
)

# Define the validation UI
validation_ui <- fluidPage(
  titlePanel("Validation of Virtual Cohorts"),
  tabsetPanel(
    tabPanel("Context of Use (CuO)",
             textAreaInput("cou_text", label = NULL, placeholder = "Enter CoU text here..."),
             actionButton("submit_cou", "Submit CoU")
    ),
    tabPanel("Question of Interest (QoI)",
             textAreaInput("qoi_text", label = NULL, placeholder = "Enter QoI text here..."),
             actionButton("submit_qoi", "Submit QoI")
    ),
    conditionalPanel(
      condition = "input.submit_cou && input.submit_qoi",
      div(
        id = "import_dataset_section",
        h4("Import Virtual Dataset"),
        fileInput("virtual_dataset", "Choose a Virtual Dataset (CSV file)")
      ),
      div(
        id = "qoi_validation_section",
        h4("QoI for Validation"),
        selectInput("selected_variable", "Select a Variable for QoI Validation", choices = NULL)
      ),
      conditionalPanel(
        condition = "input.selected_variable",
        div(
          id = "import_real_dataset_section",
          h4("Import Real Dataset"),
          fileInput("real_dataset", "Choose a Real Dataset (CSV file)")
        )
      )
    )
  )
)


# Define the analysis UI
analysis_ui <- fluidPage(
  titlePanel("Analysis"),
  tabsetPanel(
    tabPanel("Univariate Comparison",
             h4("Summary statistics for the real dataset"),
             textOutput("dataset_size_real"),
             tableOutput("summary_real"),
             
             
             h4("Summary statistics for the virtual dataset"),
             textOutput("dataset_size_virtual"),
             tableOutput("summary_virtual"),
             h4("Boxplots Comparison"),
             div(
               style = "display: flex; justify-content: space-between;",
               plotlyOutput("boxplot_real", height = "400px", width = "48%"),
               plotlyOutput("boxplot_virtual", height = "400px", width = "48%")
             ),
             
             h4("Scatter plots comparison"),
             div(
               style = "display: flex; justify-content: space-between;",
               plotOutput("pairplot_real", height = "600px", width = "48%"),
               plotOutput("pairplot_virtual", height = "600px", width = "48%")
             )
    ),
    
    tabPanel("Bivariate Comparison",
             h4("Bivariate Comparison for Real Dataset"),
             plotlyOutput("heatmap_real", height = "500px", width = "48%"),
             h4("Bivariate Comparison for Virtual Dataset"),
             plotlyOutput("heatmap_virtual", height = "500px", width = "48%")
    ),
    
    tabPanel("Multivariate Comparison",
             tags$p("To simplify the multivariate comparison, we evaluate standardized observations using quadratic forms:"),
             tags$p(HTML("q<sub>r</sub> = (y<sub>r</sub>-&mu;<sub>r</sub>)<sup>T</sup> &times; &Sigma;<sub>r</sub><sup>-1</sup> &times; (y<sub>r</sub>-&mu;<sub>r</sub>)")),
             tags$p("and"),
             tags$p(HTML("q<sub>v</sub> = (y<sub>v</sub>-&mu;<sub>v</sub>)<sup>T</sup> &times; &Sigma;<sub>v</sub><sup>-1</sup> &times; (y<sub>v</sub>-&mu;<sub>v</sub>)")),
             tags$p(HTML("Both q<sub>r</sub> and q<sub>v</sub> are one-dimensional. If y<sub>r</sub> and y<sub>v</sub> follow a multivariate normal distribution, then q<sub>r</sub> and q<sub>v</sub> follow a chi-squared distribution with (p) degrees of freedom. We use a qq-plot to compare the univariate distributions of q<sub>r</sub> and q<sub>v</sub>, reflecting the multivariate distributions of y<sub>r</sub> and y<sub>v</sub>.")),
             tags$p(HTML("<span style='color:darkgray; font-size:0.9em;'><strong>Reference:</strong> Multivariate comparison using qq-plot: Applied Multivariate Statistical Analysis. Johnson R. A. and Wichern, D.W. Prentice-Hall International Editions. 1992, Third Edition, Chapter 4, pages 126-171.</span>")),
             mainPanel(
               plotOutput("histVirtual"),
               plotOutput("histReal"),
               plotOutput("qqPlot")
             )
    ),
    
    
    
    
    tabPanel("Variability Assessment",
             sidebarPanel(
               fileInput('file1', 'Choose CSV File for Real Data', accept=c('text/csv', '.csv')),
               fileInput('file2', 'Choose CSV File for Virtual Data', accept=c('text/csv', '.csv')),
               uiOutput("varSelect"),
               numericInput('sample_size', 'Sample Size (n)', value = 90, min = 1),
               numericInput('alpha', 'Discounting Factor (alpha)', value = 0.8),
               numericInput('B', 'Number of Bootstrap Samples (B)', value = 2000, min = 1),
               actionButton("submit", "Submit")
             ),
             
             mainPanel(
               plotOutput("densityPlot"),
               plotOutput("bootstrapPlot"),
               plotlyOutput("ggplot"),
               br(),  # Line break
               HTML("<span style='color:black; font-weight:bold;'>References:</span><br>"),
               HTML("<span style='color:darkgray; font-size:0.9em'>Efron, B. & Tibshirani, R. (1993). Bootstrap Analysis: An Introduction to the Bootstrap. Chapman & Hall. First Edition, Chapter 16, pages 220-236.</span><br><br>"),
               HTML("<span style='color:darkgray; font-size:0.9em'>Tarek Haddad, Adam Himes, Laura Thompson, Telba Irony, Rajesh Nair & on Behalf of MDIC Computer Modeling and Simulation Working Group Participants (2017). Incorporation of stochastic engineering models as prior information in Bayesian medical device trials, Journal of Biopharmaceutical Statistics, 27:6, 1089-1103, DOI: 10.1080/10543406.2017.1300907</span>")
             )
    )
  )
)


server <- function(input, output, session) {
  
  observe({
    if (!is.null(input$tab)) {
      if (input$tab == 'validation') {
        showModal(
          modalDialog(
            validation_ui,
            size = "l",
            title = "Validation of Virtual Cohorts",
            footer = modalButton("Close")
          )
        )
      }
    }
  })
  
  observeEvent(input$virtual_dataset, {
    if (!is.null(input$virtual_dataset)) {
      dataset <- read.csv(input$virtual_dataset$datapath)
      updateSelectInput(session, "selected_variable", choices = names(dataset))
    }
  })
  
  observeEvent(input$real_dataset, {
    showModal(
      modalDialog(
        analysis_ui,
        size = "l",
        title = "Analysis",
        footer = modalButton("Close")
      )
    )
  })
  
  realData <- reactive({
    req(input$real_dataset)
    read.csv(input$real_dataset$datapath)
  })
  
  virtualData <- reactive({
    req(input$virtual_dataset)
    read.csv(input$virtual_dataset$datapath)
  })
  
  output$dataset_size_real <- renderText({
    data_subset <- realData()
    paste("The sample size is : ", nrow(data_subset), "observations")
  })
  
  output$summary_real <- renderTable({
    data_subset <- realData()
    numeric_vars <- sapply(data_subset, is.numeric)
    
    summary_table <- data.frame(
      Variable = names(data_subset)[numeric_vars],
      Mean = round(sapply(data_subset[numeric_vars], mean, na.rm = TRUE), 2),
      SD = round(sapply(data_subset[numeric_vars], sd, na.rm = TRUE), 2),
      Min = sapply(data_subset[numeric_vars], min, na.rm = TRUE),
      Max = sapply(data_subset[numeric_vars], max, na.rm = TRUE)
    )
    return(summary_table)
  })
  
  output$dataset_size_virtual <- renderText({
    data_subset <- virtualData()
    paste("The sample size is: ", nrow(data_subset), "observations")
  })
  
  output$summary_virtual <- renderTable({
    data_subset <- virtualData()
    numeric_vars <- sapply(data_subset, is.numeric)
    
    summary_table <- data.frame(
      Variable = names(data_subset)[numeric_vars],
      Mean = round(sapply(data_subset[numeric_vars], mean, na.rm = TRUE), 2),
      SD = round(sapply(data_subset[numeric_vars], sd, na.rm = TRUE), 2),
      Min = sapply(data_subset[numeric_vars], min, na.rm = TRUE),
      Max = sapply(data_subset[numeric_vars], max, na.rm = TRUE)
    )
    return(summary_table)
  })
  
  output$boxplot_real <- renderPlotly({
    req(input$selected_variable, realData())
    plot_ly(data = realData(), y = ~get(input$selected_variable), type = "box", name = "Real Data") %>%
      layout(title = paste("Boxplot of", input$selected_variable, "in Real Data"))
  })
  
  # Create the boxplots for the virtual data
  output$boxplot_virtual <- renderPlotly({
    req(input$selected_variable, virtualData())
    plot_ly(data = virtualData(), y = ~get(input$selected_variable), type = "box", name = "Virtual Data") %>%
      layout(title = paste("Boxplot of", input$selected_variable, "in Virtual Data"))
  })
  
  generate_pair_plot <- function(data) {
    # Select only numeric variables
    numeric_vars <- dplyr::select_if(data, is.numeric)
    
    # Create pair plot
    plot <- GGally::ggpairs(numeric_vars)
    
    return(plot)
  }
  
  output$pairplot_real <- renderPlot({
    generate_pair_plot(realData())
  })
  
  output$pairplot_virtual <- renderPlot({
    generate_pair_plot(virtualData())
  })
  
  
  # Create heatmap for real dataset
  output$heatmap_real <- renderPlotly({
    req(realData())
    data_subset <- realData()
    numeric_vars <- sapply(data_subset, is.numeric)
    corr_matrix <- cor(data_subset[numeric_vars], use = "complete.obs", method = "spearman")
    heatmap_data <- reshape2::melt(corr_matrix)
    
    plot_ly(data = heatmap_data, x = ~Var1, y = ~Var2, z = ~value, type = "heatmap", colors = "RdBu", zmin=-1, zmax=1, text = ~sprintf("%.2f", value), hoverinfo = "text") %>%
      layout(
        title = "Spearman Correlation Heatmap (Real Data)",
        xaxis = list(tickangle = 45),
        yaxis = list(tickangle = -45),
        margin = list(l = 50, r = 10, b = 100, t = 100),
        width = 1000,
        height = 900
      )
  })
  
  # Create heatmap for virtual dataset
  output$heatmap_virtual <- renderPlotly({
    req(virtualData())
    data_subset <- virtualData()
    numeric_vars <- sapply(data_subset, is.numeric)
    corr_matrix <- cor(data_subset[numeric_vars], use = "complete.obs", method = "spearman")
    heatmap_data <- reshape2::melt(corr_matrix)
    
    plot_ly(data = heatmap_data, x = ~Var1, y = ~Var2, z = ~value, type = "heatmap", colors = "RdBu", zmin=-1, zmax=1, text = ~sprintf("%.2f", value), hoverinfo = "text") %>%
      layout(
        title = "Spearman Correlation Heatmap (Virtual Data)",
        xaxis = list(tickangle = 45),
        yaxis = list(tickangle = -45),
        margin = list(l = 50, r = 10, b = 100, t = 100),
        width = 1000,
        height = 900
      )
  })
  
  
  # multivariate comparison
  observeEvent(c(input$real_dataset, input$virtual_dataset), {
    req(input$real_dataset, input$virtual_dataset)
    
    data_real <- fread(input$real_dataset$datapath)
    data_virtual <- fread(input$virtual_dataset$datapath)
    
    # Calculations for virtual data
    n.virtual = dim(data_virtual)[1]
    mean.virtual = colMeans(data_virtual)
    Sigma.virtual = var(data_virtual)
    Sigma.virtual.inverse = solve(Sigma.virtual)
    q.virtual = apply(data_virtual, 1,
                      function(x) t(x-mean.virtual) %*% Sigma.virtual.inverse %*% (x - mean.virtual))
    
    # Calculations for real data
    n.real = dim(data_real)[1]
    mean.real = colMeans(data_real)
    Sigma.real = var(data_real)
    Sigma.real.inverse = solve(Sigma.real)
    q.real = apply(data_real, 1,
                   function(x) t(x-mean.real) %*% Sigma.real.inverse %*% (x - mean.real))
    
    # Comparison of quadratic form vs. a chi-squared distribution
    p = dim(data_real)[2]
    max.q.virtual = max(q.virtual)
    max.q.real = max(q.real)
    
    output$histVirtual <- renderPlot({
      hist(q.virtual, breaks = 40, probability = TRUE, ylim = c(0, 0.14))
      curve(dchisq(x, df = p), from = 0, to = max.q.virtual, lwd = 2, col = "blue", add = TRUE)
    })
    
    output$histReal <- renderPlot({
      hist(q.real, breaks = 40, probability = TRUE, ylim = c(0, 0.14))
      curve(dchisq(x, df = p), from = 0, to = max.q.real, lwd = 2, col = "red", add = TRUE)
    })
    
    output$qqPlot <- renderPlot({
      qqplot(q.virtual, q.real)
      abline(a = 0, b =1, lwd = 2)
    })
  })
  
  # Variability assessment
  
  shapeFeatures_real <- reactive({
    if (is.null(input$file1)) return(NULL)
    read.csv(input$file1$datapath)
  })
  
  shapeFeatures_virtual <- reactive({
    if (is.null(input$file2)) return(NULL)
    read.csv(input$file2$datapath)
  })
  
  observe({
    df <- shapeFeatures_real()
    if (is.null(df)) return()
    updateSelectInput(session, "selected_var", "Choose a variable:", choices = names(df), selected = names(df)[1])
  })
  
  output$varSelect <- renderUI({
    if (is.null(input$file1)) return(NULL)
    selectInput("selected_var", "Select Variable:", choices = names(shapeFeatures_real()))
  })
  
  dataPlots <- reactive({
    if (is.null(input$file1) || is.null(input$file2)) return(NULL)
    
    y.real <- shapeFeatures_real()[[input$selected_var]][1:input$sample_size]
    y.virtual <- shapeFeatures_virtual()[[input$selected_var]][1:input$sample_size]
    
    n <- input$sample_size
    alpha <- input$alpha
    B <- input$B
    m <- round(n * alpha, 0)
    
    set.seed(1509)
    length.of.the.grid <- 512
    
    boot.mat.x <- matrix(0, nrow = B, ncol = length.of.the.grid)
    boot.mat.y <- matrix(0, nrow = B, ncol = length.of.the.grid)
    boot.yvals <- vector("list", 50)
    
    for(b in 1:B) {
      y.star <- sample(y.virtual, size = m, replace = TRUE)
      boot.mat.x[b, ] <- density(y.star)$x
      boot.mat.y[b, ] <- density(y.star)$y
      if (b <= 50) {
        boot.yvals[[b]] <- density(y.star)$y
      }
    }
    
    d.virtual <- density(y.virtual)
    d.real <- density(y.real)
    
    ylim.range <- range(c(d.virtual$y, d.real$y, unlist(boot.yvals)))
    
    list(y.real = y.real, y.virtual = y.virtual, boot.mat.x = boot.mat.x, boot.mat.y = boot.mat.y, ylim.range = ylim.range)
  })
  
  output$densityPlot <- renderPlot({
    plotData <- dataPlots()
    if (is.null(plotData)) return(NULL)
    
    d.virtual <- density(plotData$y.virtual)
    d.real <- density(plotData$y.real)
    plot(d.virtual, ylim = plotData$ylim.range, lwd = 2, col = "#1E6A9C", xlab = paste("Variable:", input$selected_var), main = "Comparison: Real vs. Virtual Cohort")
    lines(d.real, lwd = 2, col = "red")
    legend("topright", legend = c("Real", "Virtual"), col = c("red", "#1E6A9C"), lwd = c(3, 3))
  })
  
  output$bootstrapPlot <- renderPlot({
    plotData <- dataPlots()
    if (is.null(plotData)) return(NULL)
    
    d.virtual <- density(plotData$y.virtual)
    d.real <- density(plotData$y.real)
    plot(d.virtual, ylim = plotData$ylim.range, lwd = 2, col = "#1E6A9C", xlab = paste("Variable:", input$selected_var), main = "Comparison: Real vs. Virtual Cohort with Bootstrap")
    lines(d.real, lwd = 2, col = "red")
    for(b in 1:50) {
      y.star <- sample(plotData$y.virtual, size = input$sample_size, replace = TRUE)
      lines(density(y.star), col = "grey", lty = 3)
    }
    legend("topright", legend = c("Real", "Virtual", "Bootstrap"), col = c("red", "#1E6A9C", "grey"), lwd = c(2, 2, 2))
  })
  
  output$ggplot <- renderPlotly({
    plotData <- dataPlots()
    if (is.null(plotData)) return(NULL)
    
    d.real <- density(plotData$y.real)
    d.virtual <- density(plotData$y.virtual)
    dat <- data.frame(
      x.boot = apply(plotData$boot.mat.x, 2, mean),
      d.real = d.real$y,
      x.real = d.real$x,
      d.virtual = d.virtual$y,
      x.virtual = d.virtual$x,
      y.boot.05 = apply(plotData$boot.mat.y, 2, quantile, 0.025),
      y.boot.95 = apply(plotData$boot.mat.y, 2, quantile, 0.975)
    )
    p <- ggplot(dat, aes(x = x.boot)) + 
      geom_line(aes(y = y.boot.95, color = "Bootstrap 95% CI"), linetype = "solid") +
      geom_line(aes(x = x.real, y = d.real, color = "Real"), linetype = "solid") +
      geom_line(aes(x = x.virtual, y = d.virtual, color = "Virtual"), linetype = "solid") +
      ggtitle("Bootstrap validation") +
      xlab("x") + ylab("density") +
      coord_cartesian(ylim = plotData$ylim.range) +
      theme(legend.position = "bottom", panel.background = element_blank(), plot.background = element_rect(fill = "white")) + 
      labs(color = "Legend") +
      scale_color_manual(values = c("Bootstrap 95%" = "grey", "Bootstrap 5%" = "grey", "Real" = "red", "Virtual" = "#1E6A9C"))
    ggplotly(p)
  })
  
  
  output$report <- downloadHandler(
    filename = function() {
      paste("Variability_Assessment_Report_", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      # Save the current plots into a PDF
      pdf(file, width = 10, height = 8)
      
      plotData <- dataPlots()
      
      if (!is.null(plotData)) {
        # 1st Plot: Density Plot
        d.virtual <- density(plotData$y.virtual)
        d.real <- density(plotData$y.real)
        plot(d.virtual, ylim = plotData$ylim.range, lwd = 2, col = "#1E6A9C", xlab = paste("Variable:", isolate(input$selected_var)),
             main = "Comparison: Real vs. Virtual Cohort")
        lines(d.real, lwd = 2, col = "red")
        legend("topright", legend = c("Real", "Virtual"), col = c("red", "#1E6A9C"), lwd = c(3, 3))
        
        # 2nd Plot: Bootstrap Plot
        d.virtual <- density(plotData$y.virtual)
        d.real <- density(plotData$y.real)
        plot(d.virtual, ylim = plotData$ylim.range, lwd = 2, col = "#1E6A9C", xlab = paste("Variable:", isolate(input$selected_var)),
             main = "Comparison: Real vs. Virtual Cohort with Bootstrap")
        lines(d.real, lwd = 2, col = "red")
        for(b in 1:50) {
          y.star <- sample(plotData$y.virtual, size = isolate(input$sample_size), replace = TRUE)
          lines(density(y.star), col = "grey", lty = 3)
        }
        legend("topright", legend = c("Real", "Virtual", "Bootstrap"), col = c("red", "#1E6A9C", "grey"), lwd = c(2, 2, 2))
        
        # 3rd Plot: ggplot Bootstrap validation
        # (Note: plotly-specific features won't work in a static PDF plot, so we will just print the ggplot object.)
        d.real <- density(plotData$y.real)
        d.virtual <- density(plotData$y.virtual)
        dat <- data.frame(
          x.boot = apply(plotData$boot.mat.x, 2, mean),
          d.real = d.real$y,
          x.real = d.real$x,
          d.virtual = d.virtual$y,
          x.virtual = d.virtual$x,
          y.boot.05 = apply(plotData$boot.mat.y, 2, quantile, 0.025),
          y.boot.95 = apply(plotData$boot.mat.y, 2, quantile, 0.975)
        )
        p <- ggplot(dat, aes(x = x.boot)) + 
          geom_line(aes(y = y.boot.95, color = "Bootstrap 95% CI"), linetype = "solid") +
          geom_line(aes(x = x.real, y = d.real, color = "Real"), linetype = "solid") +
          geom_line(aes(x = x.virtual, y = d.virtual, color = "Virtual"), linetype = "solid") +
          ggtitle("Bootstrap validation") +
          xlab("x") + ylab("density") +
          coord_cartesian(ylim = plotData$ylim.range) +
          theme(legend.position = "bottom", # Change legend position
                panel.background = element_blank(), # Set panel background to blank
                plot.background = element_rect(fill = "white")) + # Set plot background to white
          labs(color = "Legend") +
          scale_color_manual(values = c("Bootstrap 95% CI" = "grey", 
                                        "Real" = "red", 
                                        "Virtual" = "#1E6A9C"))
        print(p)
      }
      
      dev.off()
    }
  )
}









shinyApp(ui = ui, server = server)