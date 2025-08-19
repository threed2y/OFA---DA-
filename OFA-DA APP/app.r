# --- Load Required Libraries ---
# Ensure all packages are installed. You can run:
install.packages(c("shiny", "shinyjs", "DT", "ggplot2", "plotly", "dplyr", 
                   "caret", "readr", "openxlsx", "shinythemes", "shinycssloaders", "randomForest", "e1071"))

library(shiny)
library(shinyjs)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(caret)
library(readr)
library(openxlsx)
library(shinythemes)
library(shinycssloaders)
library(randomForest) # Explicitly load for varImpPlot

# --- UI Definition ---
# Defines the user interface of the application.
ui <- fluidPage(
  theme = shinytheme("cerulean"), # Apply a modern theme for a better look and feel
  useShinyjs(), # Initialize shinyjs for enabling/disabling UI elements
  titlePanel(" VIZORD "),
  
  # Main layout with a sidebar and a main panel
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # Organize sidebar options into tabs for a cleaner interface
      tabsetPanel(
        id = "sidebar_tabs",
        
        # --- Tab 1: Data Input & Setup ---
        tabPanel("1. Setup",
                 br(),
                 fileInput("file", "Upload Data (CSV/Excel)", accept = c(".csv", ".xlsx")),
                 hr(),
                 # This panel only appears after a file is uploaded
                 conditionalPanel(
                   condition = "output.fileUploaded",
                   h4("Train/Test Split"),
                   sliderInput("split_ratio", "Training Set Ratio", min = 0.5, max = 0.9, value = 0.7, step = 0.05),
                   helpText("The model will be trained on the training set and evaluated on the unseen test set.")
                 )
        ),
        
        # --- Tab 2: Pre-processing ---
        tabPanel("2. Pre-process",
                 br(),
                 conditionalPanel(
                   condition = "output.fileUploaded",
                   h4("Column Selection"),
                   selectizeInput("cols_to_keep", "Select columns to keep", choices = NULL, multiple = TRUE),
                   hr(),
                   h4("Data Cleaning"),
                   checkboxInput("remove_na", "Remove NA rows", TRUE),
                   checkboxInput("remove_duplicates", "Remove duplicates", TRUE),
                   hr(),
                   h4("Data Validation"),
                   checkboxInput("check_numeric", "Validate numeric column ranges", FALSE),
                   numericInput("min_value", "Minimum allowed value", value = NA),
                   numericInput("max_value", "Maximum allowed value", value = NA)
                 )
        ),
        
        # --- Tab 3: Modeling ---
        tabPanel("3. Modeling",
                 br(),
                 conditionalPanel(
                   condition = "output.fileUploaded",
                   h4("Model Configuration"),
                   selectInput("model_type", "Model Type",
                               choices = c("Linear Regression" = "lm",
                                           "Logistic Regression" = "glm",
                                           "Random Forest" = "rf",
                                           "Decision Tree" = "rpart")),
                   selectInput("response_var", "Response Variable (Y)", choices = NULL),
                   selectizeInput("predictor_vars", "Predictor Variables (X)", choices = NULL, multiple = TRUE),
                   hr(),
                   h4("Preprocessing for Model"),
                   checkboxInput("scale_center", "Scale and Center Numeric Predictors", FALSE),
                   hr(),
                   actionButton("run_model", "Train & Evaluate Model", icon = icon("robot"), class = "btn-primary btn-block")
                 )
        )
      )
    ),
    
    # --- Main Panel for Outputs ---
    mainPanel(
      width = 9,
      tabsetPanel(
        # --- Data Inspector Tab ---
        tabPanel("Data Inspector",
                 br(),
                 tabsetPanel(
                   tabPanel("Raw Data", DTOutput("raw_data") %>% withSpinner()),
                   tabPanel("Cleaned Training Data", DTOutput("train_data_table") %>% withSpinner()),
                   tabPanel("Cleaned Test Data", DTOutput("test_data_table") %>% withSpinner())
                 )),
        
        # --- Visualization Tab ---
        tabPanel("Summary & Visualization",
                 br(),
                 h3("Data Summary"),
                 verbatimTextOutput("summary") %>% withSpinner(),
                 hr(),
                 h3("Interactive Visualizations"),
                 fluidRow(
                   column(3,
                          selectInput("plot_type", "Plot Type",
                                      choices = c("Histogram" = "hist",
                                                  "Scatter Plot" = "scatter",
                                                  "Box Plot" = "box",
                                                  "Bar Plot" = "bar"))
                   ),
                   column(3,
                          # Dynamic UI for selecting plot variables based on plot type
                          uiOutput("plot_variable_ui")
                   )
                 ),
                 plotlyOutput("interactive_plot") %>% withSpinner(), # Interactive plots with plotly
                 downloadButton("download_plot", "Download Plot")
        ),
        
        # --- Model Results Tab ---
        tabPanel("Model Results",
                 br(),
                 fluidRow(
                   column(6,
                          h3("Model Training Summary"),
                          # Shows cross-validation results from caret
                          verbatimTextOutput("model_summary") %>% withSpinner()
                   ),
                   column(6,
                          h3("Model Evaluation on Test Data"),
                          # Shows performance on the held-out test set
                          verbatimTextOutput("model_evaluation") %>% withSpinner()
                   )
                 ),
                 hr(),
                 h3("Diagnostic & Importance Plots"),
                 plotOutput("model_plot") %>% withSpinner()
        ),
        
        # --- Downloads Tab ---
        tabPanel("Downloads",
                 br(),
                 h3("Download Processed Data & Results"),
                 textInput("save_name", "File name prefix", value = "analysis_results"),
                 radioButtons("save_format", "Format",
                              choices = c("CSV" = "csv", "Excel" = "xlsx"), inline = TRUE),
                 downloadButton("download_results", "Download Processed Data")
        )
      )
    )
  )
)

# --- Server Logic ---
# Contains all the backend logic for data processing, modeling, and rendering outputs.
server <- function(input, output, session) {
  
  # --- Reactive Values Store ---
  # A centralized place to store data that can change and trigger updates.
  rv <- reactiveValues(
    raw_data = NULL,
    cleaned_data = NULL,
    train_data = NULL,
    test_data = NULL,
    model = NULL,
    evaluation = NULL,
    current_plot = NULL
  )
  
  # --- UI Logic & Updates ---
  
  # Conditional panel trigger: Returns TRUE if data has been uploaded.
  output$fileUploaded <- reactive(!is.null(rv$raw_data))
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  # Dynamically render plot variable selectors based on the selected plot type.
  output$plot_variable_ui <- renderUI({
    req(rv$cleaned_data)
    cols <- names(rv$cleaned_data)
    
    if (input$plot_type == "scatter") {
      list(
        selectInput("scatter_x", "X Variable", choices = cols, selected = cols[1]),
        selectInput("scatter_y", "Y Variable", choices = cols, selected = cols[2])
      )
    } else {
      selectInput("hist_var", "Select Variable", choices = cols)
    }
  })
  
  # --- Data Loading and Initial Processing ---
  observeEvent(input$file, {
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    
    tryCatch({
      if (ext == "csv") {
        rv$raw_data <- read_csv(input$file$datapath)
      } else if (ext == "xlsx") {
        rv$raw_data <- read.xlsx(input$file$datapath)
      }
      # Update UI elements with column names from the raw data, selecting all by default
      updateSelectizeInput(session, "cols_to_keep", choices = names(rv$raw_data), selected = names(rv$raw_data))
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
    })
  })
  
  # --- Reactive Data Cleaning and Splitting Pipeline ---
  
  # 1. This reactive expression cleans data based on user selections.
  # It re-runs automatically whenever an input it depends on changes.
  cleaned_data_reactive <- reactive({
    req(rv$raw_data)
    
    cleaned <- rv$raw_data
    
    # Keep only selected columns
    if (!is.null(input$cols_to_keep)) {
      cleaned <- cleaned[, input$cols_to_keep, drop = FALSE]
    }
    
    if (input$remove_duplicates) {
      cleaned <- distinct(cleaned)
    }
    
    # Data validation for numeric ranges
    if (input$check_numeric) {
      num_cols <- sapply(cleaned, is.numeric)
      if (any(num_cols)) {
        min_val <- input$min_value
        max_val <- input$max_value
        cleaned[, num_cols] <- lapply(cleaned[, num_cols, drop = FALSE], function(x) {
          if (!is.na(min_val)) x[x < min_val] <- NA
          if (!is.na(max_val)) x[x > max_val] <- NA
          x
        })
      }
    }
    
    if (input$remove_na) {
      cleaned <- na.omit(cleaned)
    }
    
    rv$cleaned_data <- cleaned
    return(cleaned)
  })
  
  # 2. This observer splits the cleaned data into training and testing sets.
  # It runs whenever the cleaned data changes.
  observe({
    df <- cleaned_data_reactive()
    req(df)
    
    # Update model variable choices based on the columns in the cleaned data
    updateSelectInput(session, "response_var", choices = names(df))
    updateSelectizeInput(session, "predictor_vars", choices = names(df))
    
    # Perform the train/test split
    set.seed(123) # for reproducibility
    train_index <- createDataPartition(df[[1]], p = input$split_ratio, list = FALSE)
    rv$train_data <- df[train_index, ]
    rv$test_data <- df[-train_index, ]
  })
  
  # --- Modeling ---
  # This observer runs only when the "Run Model" button is clicked.
  observeEvent(input$run_model, {
    req(rv$train_data, rv$test_data, input$response_var, input$predictor_vars)
    
    # --- Input Validation ---
    # Logistic regression requires a binary factor response variable
    if (input$model_type == "glm") {
      response_levels <- unique(rv$train_data[[input$response_var]])
      if (length(response_levels) != 2) {
        showNotification("Logistic Regression requires a response variable with exactly 2 unique values.", type = "error")
        return() # Stop execution
      }
      # Ensure response is a factor for classification models
      rv$train_data[[input$response_var]] <- as.factor(rv$train_data[[input$response_var]])
      rv$test_data[[input$response_var]] <- as.factor(rv$test_data[[input$response_var]])
    }
    
    showNotification("Training model... this may take a moment.", type = "message", duration = 3)
    
    tryCatch({
      formula <- as.formula(paste(input$response_var, "~", paste(input$predictor_vars, collapse = "+")))
      
      # Define preprocessing steps (scaling/centering) if selected
      preProc_args <- if (input$scale_center) c("center", "scale") else NULL
      
      # Use caret for a unified modeling interface with cross-validation
      train_control <- trainControl(method = "cv", number = 5) # 5-fold cross-validation
      
      model <- train(
        form = formula,
        data = rv$train_data,
        method = input$model_type,
        trControl = train_control,
        preProcess = preProc_args,
        na.action = na.omit # Handle any remaining NAs
      )
      
      rv$model <- model
      
      # --- Evaluation on the Test Set ---
      predictions <- predict(model, newdata = rv$test_data)
      
      if (is.factor(rv$test_data[[input$response_var]])) { # Classification evaluation
        rv$evaluation <- confusionMatrix(predictions, rv$test_data[[input$response_var]])
      } else { # Regression evaluation
        rv$evaluation <- postResample(pred = predictions, obs = rv$test_data[[input$response_var]])
      }
      
      showNotification("Model trained and evaluated successfully!", type = "success")
      
    }, error = function(e) {
      showNotification(paste("Model Error:", e$message), type = "error")
    })
  })
  
  # --- Output Rendering ---
  
  # Data Tables
  output$raw_data <- renderDT(datatable(rv$raw_data, options = list(scrollX = TRUE, pageLength = 5)))
  output$train_data_table <- renderDT(datatable(rv$train_data, options = list(scrollX = TRUE, pageLength = 5)))
  output$test_data_table <- renderDT(datatable(rv$test_data, options = list(scrollX = TRUE, pageLength = 5)))
  
  # Summary Text
  output$summary <- renderPrint({
    req(rv$cleaned_data)
    summary(rv$cleaned_data)
  })
  
  # Interactive Plot using Plotly
  output$interactive_plot <- renderPlotly({
    req(rv$cleaned_data)
    
    p <- switch(input$plot_type,
                "hist" = ggplot(rv$cleaned_data, aes_string(x = input$hist_var)) + geom_histogram(aes(text = ..count..), fill = "#2c3e50", color = "white", bins = 30),
                "scatter" = ggplot(rv$cleaned_data, aes_string(x = input$scatter_x, y = input$scatter_y)) + geom_point(color = "#2c3e50", alpha = 0.7),
                "box" = ggplot(rv$cleaned_data, aes_string(y = input$hist_var)) + geom_boxplot(fill = "#2c3e50"),
                "bar" = ggplot(rv$cleaned_data, aes_string(x = input$hist_var)) + geom_bar(fill = "#2c3e50")
    ) + theme_minimal()
    
    rv$current_plot <- p # Store the ggplot object for downloading
    ggplotly(p, tooltip = if(input$plot_type == "hist") "text" else "all")
  })
  
  # Model Outputs
  output$model_summary <- renderPrint({
    req(rv$model)
    print(rv$model)
  })
  
  output$model_evaluation <- renderPrint({
    req(rv$evaluation)
    print(rv$evaluation)
  })
  
  output$model_plot <- renderPlot({
    req(rv$model)
    if (input$model_type %in% c("lm", "glm")) {
      par(mfrow = c(2, 2))
      plot(rv$model$finalModel)
    } else if (input$model_type == "rf") {
      plot(varImp(rv$model), main="Variable Importance")
    } else if (input$model_type == "rpart") {
      plot(rv$model$finalModel, margin = 0.1)
      text(rv$model$finalModel, use.n = TRUE, pretty = TRUE, cex = 0.8)
    }
  })
  
  # --- Downloads ---
  
  # Download Plot
  output$download_plot <- downloadHandler(
    filename = function() { paste(input$plot_type, "_plot.png", sep = "") },
    content = function(file) {
      req(rv$current_plot)
      ggsave(file, plot = rv$current_plot, device = "png", width = 8, height = 6)
    }
  )
  
  # Download Data
  output$download_results <- downloadHandler(
    filename = function() {
      paste(input$save_name, "_", Sys.Date(), ".", input$save_format, sep = "")
    },
    content = function(file) {
      if (input$save_format == "csv") {
        write.csv(rv$cleaned_data, file, row.names = FALSE)
      } else if (input$save_format == "xlsx") {
        # Create a new Excel workbook
        wb <- createWorkbook()
        addWorksheet(wb, "Raw Data")
        addWorksheet(wb, "Cleaned Training Data")
        addWorksheet(wb, "Cleaned Test Data")
        writeData(wb, "Raw Data", rv$raw_data)
        writeData(wb, "Cleaned Training Data", rv$train_data)
        writeData(wb, "Cleaned Test Data", rv$test_data)
        
        # Add model results if they exist
        if (!is.null(rv$model)) {
          addWorksheet(wb, "Model Summary")
          writeData(wb, "Model Summary", capture.output(print(rv$model)))
          addWorksheet(wb, "Model Evaluation")
          writeData(wb, "Model Evaluation", capture.output(print(rv$evaluation)))
        }
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    }
  )
}

# --- Run the App ---
shinyApp(ui, server)
