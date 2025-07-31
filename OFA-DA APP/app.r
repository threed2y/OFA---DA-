library(shiny)
library(shinyjs)
library(DT)
library(ggplot2)
library(dplyr)
library(caret)
library(readr)
library(openxlsx)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Comprehensive Data Analytics Tool"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Data Input"),
      fileInput("file", "Upload Data (CSV/Excel)", accept = c(".csv", ".xlsx")),
      
      conditionalPanel(
        condition = "output.fileUploaded",
        h4("Data Cleaning"),
        checkboxInput("remove_na", "Remove NA rows", FALSE),
        checkboxInput("remove_duplicates", "Remove duplicates", FALSE),
        selectizeInput("cols_to_keep", "Select columns to keep", choices = NULL, multiple = TRUE),
        
        h4("Data Validation"),
        checkboxInput("check_numeric", "Validate numeric columns", FALSE),
        checkboxInput("check_factor", "Validate factor levels", FALSE),
        numericInput("min_value", "Minimum allowed value", value = NA),
        numericInput("max_value", "Maximum allowed value", value = NA),
        
        h4("Model Fitting"),
        selectInput("model_type", "Model Type", 
                    choices = c("Linear Regression" = "lm", 
                               "Logistic Regression" = "logit",
                               "Random Forest" = "rf",
                               "Decision Tree" = "rpart")),
        selectInput("response_var", "Response Variable", choices = NULL),
        selectizeInput("predictor_vars", "Predictor Variables", choices = NULL, multiple = TRUE),
        actionButton("run_model", "Run Model", class = "btn-primary"),
        
        h4("Save Results"),
        textInput("save_name", "File name prefix", value = "analysis_results"),
        radioButtons("save_format", "Format", 
                     choices = c("CSV" = "csv", "Excel" = "xlsx", "RData" = "rdata")),
        downloadButton("download_results", "Download Results")
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Raw Data", DTOutput("raw_data")),
        tabPanel("Cleaned Data", DTOutput("cleaned_data")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Visualizations",
                 selectInput("plot_type", "Plot Type",
                             choices = c("Histogram" = "hist",
                                        "Scatter Plot" = "scatter",
                                        "Box Plot" = "box",
                                        "Bar Plot" = "bar")),
                 conditionalPanel(
                   condition = "input.plot_type == 'hist' || input.plot_type == 'box' || input.plot_type == 'bar'",
                   selectInput("hist_var", "Select Variable", choices = NULL)
                 ),
                 conditionalPanel(
                   condition = "input.plot_type == 'scatter'",
                   selectInput("scatter_x", "X Variable", choices = NULL),
                   selectInput("scatter_y", "Y Variable", choices = NULL)
                 ),
                 plotOutput("plot"),
                 downloadButton("download_plot", "Download Plot")
        ),
        tabPanel("Model Results",
                 verbatimTextOutput("model_summary"),
                 plotOutput("model_plot"),
                 DTOutput("model_coefs"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value to store data
  rv <- reactiveValues(
    raw_data = NULL,
    cleaned_data = NULL,
    model = NULL,
    plots = list()
  )
  
  # Check if file is uploaded for conditional panels
  output$fileUploaded <- reactive({
    return(!is.null(rv$raw_data))
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  # Read uploaded file
  observeEvent(input$file, {
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    if (ext == "csv") {
      rv$raw_data <- read.csv(input$file$datapath)
    } else if (ext == "xlsx") {
      rv$raw_data <- read.xlsx(input$file$datapath)
    }
    
    # Update UI elements with column names
    updateSelectizeInput(session, "cols_to_keep", choices = names(rv$raw_data))
    updateSelectInput(session, "response_var", choices = names(rv$raw_data))
    updateSelectInput(session, "predictor_vars", choices = names(rv$raw_data))
    updateSelectInput(session, "hist_var", choices = names(rv$raw_data))
    updateSelectInput(session, "scatter_x", choices = names(rv$raw_data))
    updateSelectInput(session, "scatter_y", choices = names(rv$raw_data))
    
    # Initialize cleaned data with raw data
    rv$cleaned_data <- rv$raw_data
  })
  
  # Data cleaning
  observe({
    req(rv$raw_data)
    
    cleaned <- rv$raw_data
    
    # Remove NA rows if selected
    if (input$remove_na) {
      cleaned <- cleaned[complete.cases(cleaned), ]
    }
    
    # Remove duplicates if selected
    if (input$remove_duplicates) {
      cleaned <- distinct(cleaned)
    }
    
    # Keep only selected columns
    if (!is.null(input$cols_to_keep)) {
      cleaned <- cleaned[, input$cols_to_keep, drop = FALSE]
    }
    
    # Data validation
    if (input$check_numeric) {
      num_cols <- sapply(cleaned, is.numeric)
      if (any(num_cols)) {
        if (!is.na(input$min_value)) {
          cleaned[, num_cols] <- lapply(cleaned[, num_cols, drop = FALSE], function(x) {
            x[x < input$min_value] <- NA
            x
          })
        }
        if (!is.na(input$max_value)) {
          cleaned[, num_cols] <- lapply(cleaned[, num_cols, drop = FALSE], function(x) {
            x[x > input$max_value] <- NA
            x
          })
        }
      }
    }
    
    rv$cleaned_data <- cleaned
    
    # Update response and predictor variables based on cleaned data
    updateSelectInput(session, "response_var", choices = names(cleaned))
    updateSelectizeInput(session, "predictor_vars", choices = names(cleaned))
  })
  
  # Model fitting
  observeEvent(input$run_model, {
    req(rv$cleaned_data, input$response_var, input$predictor_vars)
    
    tryCatch({
      formula <- as.formula(paste(input$response_var, "~", paste(input$predictor_vars, collapse = "+")))
      
      if (input$model_type == "lm") {
        rv$model <- lm(formula, data = rv$cleaned_data)
      } else if (input$model_type == "logit") {
        rv$cleaned_data[[input$response_var]] <- as.factor(rv$cleaned_data[[input$response_var]])
        rv$model <- glm(formula, data = rv$cleaned_data, family = "binomial")
      } else if (input$model_type == "rf") {
        rv$model <- train(formula, data = rv$cleaned_data, method = "rf")
      } else if (input$model_type == "rpart") {
        rv$model <- train(formula, data = rv$cleaned_data, method = "rpart")
      }
    }, error = function(e) {
      showNotification(paste("Model error:", e$message), type = "error")
    })
  })
  
  # Generate plots based on selected type
  output$plot <- renderPlot({
    req(rv$cleaned_data)
    
    if (input$plot_type == "hist") {
      req(input$hist_var)
      ggplot(rv$cleaned_data, aes_string(x = input$hist_var)) + 
        geom_histogram(fill = "blue", color = "black", bins = 30) +
        theme_minimal()
    } else if (input$plot_type == "scatter") {
      req(input$scatter_x, input$scatter_y)
      ggplot(rv$cleaned_data, aes_string(x = input$scatter_x, y = input$scatter_y)) + 
        geom_point(color = "blue") + 
        theme_minimal()
    } else if (input$plot_type == "box") {
      req(input$hist_var)
      ggplot(rv$cleaned_data, aes_string(y = input$hist_var)) + 
        geom_boxplot(fill = "blue") + 
        theme_minimal()
    } else if (input$plot_type == "bar") {
      req(input$hist_var)
      ggplot(rv$cleaned_data, aes_string(x = input$hist_var)) + 
        geom_bar(fill = "blue") + 
        theme_minimal()
    }
  })
  
  # Download plot
  output$download_plot <- downloadHandler(
    filename = function() {
      paste(input$plot_type, "_plot.png", sep = "")
    },
    content = function(file) {
      ggsave(file, device = "png", width = 8, height = 6)
    }
  )
  
  # Download results
  output$download_results <- downloadHandler(
    filename = function() {
      paste(input$save_name, ".", input$save_format, sep = "")
    },
    content = function(file) {
      if (input$save_format == "csv") {
        write.csv(rv$cleaned_data, file, row.names = FALSE)
      } else if (input$save_format == "xlsx") {
        write.xlsx(list(
          "Raw Data" = rv$raw_data,
          "Cleaned Data" = rv$cleaned_data,
          "Model Summary" = if(!is.null(rv$model)) capture.output(summary(rv$model)) else NULL
        ), file)
      } else if (input$save_format == "rdata") {
        save(rv$raw_data, rv$cleaned_data, rv$model, file = file)
      }
    }
  )
  
  # Output renderers
  output$raw_data <- renderDT({
    datatable(rv$raw_data, options = list(scrollX = TRUE))
  })
  
  output$cleaned_data <- renderDT({
    datatable(rv$cleaned_data, options = list(scrollX = TRUE))
  })
  
  output$summary <- renderPrint({
    req(rv$cleaned_data)
    summary(rv$cleaned_data)
  })
  
  output$model_summary <- renderPrint({
    req(rv$model)
    summary(rv$model)
  })
  
  output$model_coefs <- renderDT({
    req(rv$model)
    if ("finalModel" %in% names(rv$model)) {
      # For caret models
      coefs <- as.data.frame(rv$model$finalModel$importance)
    } else {
      # For lm/glm models
      coefs <- as.data.frame(summary(rv$model)$coefficients)
    }
    datatable(coefs)
  })
  
  output$model_plot <- renderPlot({
    req(rv$model)
    if (input$model_type %in% c("lm", "logit")) {
      par(mfrow = c(2, 2))
      plot(rv$model)
    } else if (input$model_type == "rf") {
      varImpPlot(rv$model$finalModel)
    } else if (input$model_type == "rpart") {
      plot(rv$model$finalModel)
      text(rv$model$finalModel)
    }
  })
}

shinyApp(ui, server)