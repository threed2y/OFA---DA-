# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(corrplot)
library(VIM)
library(mice)
library(caret)
library(randomForest)
library(readr)
library(readxl)
library(writexl)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Data Analytics Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Data Cleaning", tabName = "cleaning", icon = icon("broom")),
      menuItem("Data Validation", tabName = "validation", icon = icon("check-circle")),
      menuItem("Visualization", tabName = "visualization", icon = icon("chart-bar")),
      menuItem("Model Fitting", tabName = "modeling", icon = icon("cogs")),
      menuItem("Results & Export", tabName = "results", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Data Upload Tab
      tabItem(tabName = "upload",
        fluidRow(
          box(
            title = "Upload Data", status = "primary", solidHeader = TRUE, width = 12,
            fileInput("file", "Choose CSV/Excel File",
                     accept = c(".csv", ".xlsx", ".xls")),
            checkboxInput("header", "Header", TRUE),
            checkboxInput("stringsAsFactors", "Strings as factors", FALSE),
            radioButtons("sep", "Separator",
                        choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                        selected = ","),
            radioButtons("quote", "Quote",
                        choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                        selected = '"')
          )
        ),
        fluidRow(
          box(
            title = "Data Preview", status = "info", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("preview")
          )
        ),
        fluidRow(
          box(
            title = "Data Summary", status = "info", solidHeader = TRUE, width = 12,
            verbatimTextOutput("summary")
          )
        )
      ),
      
      # Data Cleaning Tab
      tabItem(tabName = "cleaning",
        fluidRow(
          box(
            title = "Data Cleaning Options", status = "primary", solidHeader = TRUE, width = 4,
            h4("Missing Data Treatment"),
            radioButtons("missing_action", "Handle Missing Values:",
                        choices = list(
                          "Keep as is" = "none",
                          "Remove rows with NA" = "remove",
                          "Fill with mean/mode" = "fill",
                          "Use MICE imputation" = "mice"
                        )),
            br(),
            h4("Duplicate Handling"),
            checkboxInput("remove_duplicates", "Remove duplicate rows", FALSE),
            br(),
            h4("Outlier Detection"),
            checkboxInput("remove_outliers", "Remove outliers (IQR method)", FALSE),
            br(),
            actionButton("apply_cleaning", "Apply Cleaning", class = "btn-primary")
          ),
          box(
            title = "Cleaned Data Preview", status = "success", solidHeader = TRUE, width = 8,
            DT::dataTableOutput("cleaned_data")
          )
        ),
        fluidRow(
          box(
            title = "Missing Data Pattern", status = "info", solidHeader = TRUE, width = 6,
            plotOutput("missing_pattern")
          ),
          box(
            title = "Cleaning Summary", status = "info", solidHeader = TRUE, width = 6,
            verbatimTextOutput("cleaning_summary")
          )
        )
      ),
      
      # Data Validation Tab
      tabItem(tabName = "validation",
        fluidRow(
          box(
            title = "Data Validation", status = "primary", solidHeader = TRUE, width = 12,
            h4("Data Quality Checks"),
            verbatimTextOutput("validation_results")
          )
        ),
        fluidRow(
          box(
            title = "Variable Types", status = "info", solidHeader = TRUE, width = 6,
            DT::dataTableOutput("var_types")
          ),
          box(
            title = "Data Completeness", status = "info", solidHeader = TRUE, width = 6,
            plotOutput("completeness_plot")
          )
        )
      ),
      
      # Visualization Tab
      tabItem(tabName = "visualization",
        fluidRow(
          box(
            title = "Visualization Controls", status = "primary", solidHeader = TRUE, width = 3,
            selectInput("plot_type", "Plot Type:",
                       choices = list(
                         "Histogram" = "hist",
                         "Boxplot" = "box",
                         "Scatter Plot" = "scatter",
                         "Correlation Matrix" = "corr",
                         "Bar Plot" = "bar"
                       )),
            conditionalPanel(
              condition = "input.plot_type != 'corr'",
              selectInput("x_var", "X Variable:", choices = NULL)
            ),
            conditionalPanel(
              condition = "input.plot_type == 'scatter'",
              selectInput("y_var", "Y Variable:", choices = NULL),
              selectInput("color_var", "Color Variable:", choices = c("None" = "none"))
            ),
            conditionalPanel(
              condition = "input.plot_type == 'box'",
              selectInput("group_var", "Group Variable:", choices = c("None" = "none"))
            ),
            br(),
            downloadButton("download_plot", "Download Plot", class = "btn-info")
          ),
          box(
            title = "Visualization", status = "success", solidHeader = TRUE, width = 9,
            plotlyOutput("main_plot", height = "500px")
          )
        )
      ),
      
      # Model Fitting Tab
      tabItem(tabName = "modeling",
        fluidRow(
          box(
            title = "Model Configuration", status = "primary", solidHeader = TRUE, width = 4,
            selectInput("target_var", "Target Variable:", choices = NULL),
            selectInput("model_type", "Model Type:",
                       choices = list(
                         "Linear Regression" = "lm",
                         "Random Forest" = "rf",
                         "Logistic Regression" = "glm"
                       )),
            numericInput("train_split", "Training Split (%):", value = 80, min = 50, max = 95),
            checkboxInput("cross_validation", "Use Cross-Validation", TRUE),
            br(),
            actionButton("fit_model", "Fit Model", class = "btn-primary")
          ),
          box(
            title = "Model Results", status = "success", solidHeader = TRUE, width = 8,
            verbatimTextOutput("model_summary")
          )
        ),
        fluidRow(
          box(
            title = "Model Performance", status = "info", solidHeader = TRUE, width = 6,
            verbatimTextOutput("model_performance")
          ),
          box(
            title = "Feature Importance", status = "info", solidHeader = TRUE, width = 6,
            plotOutput("feature_importance")
          )
        )
      ),
      
      # Results & Export Tab
      tabItem(tabName = "results",
        fluidRow(
          box(
            title = "Export Options", status = "primary", solidHeader = TRUE, width = 12,
            h4("Data Export"),
            downloadButton("download_cleaned", "Download Cleaned Data (CSV)", class = "btn-info"),
            br(), br(),
            downloadButton("download_excel", "Download Results (Excel)", class = "btn-success"),
            br(), br(),
            h4("Model Export"),
            downloadButton("download_model", "Download Model (RDS)", class = "btn-warning"),
            br(), br(),
            h4("Report Export"),
            downloadButton("download_report", "Download Analysis Report (HTML)", class = "btn-primary")
          )
        ),
        fluidRow(
          box(
            title = "Session Summary", status = "info", solidHeader = TRUE, width = 12,
            verbatimTextOutput("session_summary")
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive values to store data
  values <- reactiveValues(
    raw_data = NULL,
    cleaned_data = NULL,
    model = NULL,
    model_performance = NULL
  )
  
  # Data Upload
  observeEvent(input$file, {
    req(input$file)
    
    ext <- tools::file_ext(input$file$datapath)
    
    if(ext == "csv") {
      values$raw_data <- read.csv(input$file$datapath,
                                 header = input$header,
                                 sep = input$sep,
                                 quote = input$quote,
                                 stringsAsFactors = input$stringsAsFactors)
    } else if(ext %in% c("xlsx", "xls")) {
      values$raw_data <- read_excel(input$file$datapath)
    }
    
    # Update variable choices
    numeric_vars <- names(select_if(values$raw_data, is.numeric))
    all_vars <- names(values$raw_data)
    
    updateSelectInput(session, "x_var", choices = all_vars)
    updateSelectInput(session, "y_var", choices = numeric_vars)
    updateSelectInput(session, "color_var", choices = c("None" = "none", all_vars))
    updateSelectInput(session, "group_var", choices = c("None" = "none", all_vars))
    updateSelectInput(session, "target_var", choices = all_vars)
  })
  
  # Data Preview
  output$preview <- DT::renderDataTable({
    req(values$raw_data)
    DT::datatable(values$raw_data, options = list(scrollX = TRUE))
  })
  
  # Data Summary
  output$summary <- renderPrint({
    req(values$raw_data)
    summary(values$raw_data)
  })
  
  # Data Cleaning
  observeEvent(input$apply_cleaning, {
    req(values$raw_data)
    
    cleaned <- values$raw_data
    
    # Handle missing values
    if(input$missing_action == "remove") {
      cleaned <- na.omit(cleaned)
    } else if(input$missing_action == "fill") {
      numeric_cols <- sapply(cleaned, is.numeric)
      cleaned[numeric_cols] <- lapply(cleaned[numeric_cols], function(x) {
        ifelse(is.na(x), mean(x, na.rm = TRUE), x)
      })
      
      factor_cols <- sapply(cleaned, function(x) is.factor(x) || is.character(x))
      cleaned[factor_cols] <- lapply(cleaned[factor_cols], function(x) {
        mode_val <- names(sort(table(x), decreasing = TRUE))[1]
        ifelse(is.na(x), mode_val, x)
      })
    } else if(input$missing_action == "mice") {
      if(sum(is.na(cleaned)) > 0) {
        mice_result <- mice(cleaned, m = 1, method = 'pmm', printFlag = FALSE)
        cleaned <- complete(mice_result)
      }
    }
    
    # Remove duplicates
    if(input$remove_duplicates) {
      cleaned <- distinct(cleaned)
    }
    
    # Remove outliers
    if(input$remove_outliers) {
      numeric_cols <- sapply(cleaned, is.numeric)
      for(col in names(cleaned)[numeric_cols]) {
        Q1 <- quantile(cleaned[[col]], 0.25, na.rm = TRUE)
        Q3 <- quantile(cleaned[[col]], 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        lower <- Q1 - 1.5 * IQR
        upper <- Q3 + 1.5 * IQR
        cleaned <- cleaned[cleaned[[col]] >= lower & cleaned[[col]] <= upper, ]
      }
    }
    
    values$cleaned_data <- cleaned
  })
  
  # Cleaned Data Preview
  output$cleaned_data <- DT::renderDataTable({
    req(values$cleaned_data)
    DT::datatable(values$cleaned_data, options = list(scrollX = TRUE))
  })
  
  # Missing Data Pattern
  output$missing_pattern <- renderPlot({
    req(values$raw_data)
    VIM::aggr(values$raw_data, col = c('navyblue', 'red'), numbers = TRUE, sortVars = TRUE)
  })
  
  # Cleaning Summary
  output$cleaning_summary <- renderPrint({
    req(values$raw_data)
    raw_rows <- nrow(values$raw_data)
    raw_cols <- ncol(values$raw_data)
    
    if(!is.null(values$cleaned_data)) {
      clean_rows <- nrow(values$cleaned_data)
      clean_cols <- ncol(values$cleaned_data)
      
      cat("Original Data:\n")
      cat(paste("Rows:", raw_rows, "\n"))
      cat(paste("Columns:", raw_cols, "\n"))
      cat(paste("Missing values:", sum(is.na(values$raw_data)), "\n\n"))
      
      cat("Cleaned Data:\n")
      cat(paste("Rows:", clean_rows, "\n"))
      cat(paste("Columns:", clean_cols, "\n"))
      cat(paste("Missing values:", sum(is.na(values$cleaned_data)), "\n"))
      cat(paste("Rows removed:", raw_rows - clean_rows, "\n"))
    } else {
      cat("No cleaning applied yet.\n")
      cat(paste("Rows:", raw_rows, "\n"))
      cat(paste("Columns:", raw_cols, "\n"))
      cat(paste("Missing values:", sum(is.na(values$raw_data)), "\n"))
    }
  })
  
  # Data Validation
  output$validation_results <- renderPrint({
    data_to_validate <- if(!is.null(values$cleaned_data)) values$cleaned_data else values$raw_data
    req(data_to_validate)
    
    cat("DATA QUALITY REPORT\n")
    cat("==================\n\n")
    
    # Basic statistics
    cat("Dataset Dimensions:", nrow(data_to_validate), "rows x", ncol(data_to_validate), "columns\n\n")
    
    # Missing data
    missing_count <- sum(is.na(data_to_validate))
    missing_percent <- round(missing_count / (nrow(data_to_validate) * ncol(data_to_validate)) * 100, 2)
    cat("Missing Values:", missing_count, paste0("(", missing_percent, "%)\n\n"))
    
    # Duplicate rows
    duplicate_count <- sum(duplicated(data_to_validate))
    cat("Duplicate Rows:", duplicate_count, "\n\n")
    
    # Variable types
    cat("Variable Types:\n")
    type_summary <- sapply(data_to_validate, class)
    for(i in 1:length(type_summary)) {
      cat(paste("-", names(type_summary)[i], ":", type_summary[i], "\n"))
    }
  })
  
  # Variable Types Table
  output$var_types <- DT::renderDataTable({
    data_to_validate <- if(!is.null(values$cleaned_data)) values$cleaned_data else values$raw_data
    req(data_to_validate)
    
    var_info <- data.frame(
      Variable = names(data_to_validate),
      Type = sapply(data_to_validate, function(x) class(x)[1]),
      `Missing Count` = sapply(data_to_validate, function(x) sum(is.na(x))),
      `Missing %` = round(sapply(data_to_validate, function(x) sum(is.na(x))/length(x)*100), 2),
      `Unique Values` = sapply(data_to_validate, function(x) length(unique(x[!is.na(x)])))
    )
    
    DT::datatable(var_info, options = list(pageLength = 15))
  })
  
  # Completeness Plot
  output$completeness_plot <- renderPlot({
    data_to_validate <- if(!is.null(values$cleaned_data)) values$cleaned_data else values$raw_data
    req(data_to_validate)
    
    completeness <- data_to_validate %>%
      summarise_all(~sum(!is.na(.))) %>%
      gather(key = "Variable", value = "Complete_Count") %>%
      mutate(Completeness = Complete_Count / nrow(data_to_validate) * 100)
    
    ggplot(completeness, aes(x = reorder(Variable, Completeness), y = Completeness)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Data Completeness by Variable",
           x = "Variable",
           y = "Completeness (%)") +
      theme_minimal()
  })
  
  # Main Visualization
  output$main_plot <- renderPlotly({
    data_to_plot <- if(!is.null(values$cleaned_data)) values$cleaned_data else values$raw_data
    req(data_to_plot, input$plot_type)
    
    if(input$plot_type == "hist") {
      req(input$x_var)
      p <- ggplot(data_to_plot, aes_string(x = input$x_var)) +
        geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Histogram of", input$x_var))
        
    } else if(input$plot_type == "box") {
      req(input$x_var)
      if(input$group_var != "none") {
        p <- ggplot(data_to_plot, aes_string(x = input$group_var, y = input$x_var)) +
          geom_boxplot(fill = "steelblue", alpha = 0.7)
      } else {
        p <- ggplot(data_to_plot, aes_string(y = input$x_var)) +
          geom_boxplot(fill = "steelblue", alpha = 0.7)
      }
      p <- p + theme_minimal() + labs(title = paste("Boxplot of", input$x_var))
      
    } else if(input$plot_type == "scatter") {
      req(input$x_var, input$y_var)
      if(input$color_var != "none") {
        p <- ggplot(data_to_plot, aes_string(x = input$x_var, y = input$y_var, color = input$color_var)) +
          geom_point(alpha = 0.7)
      } else {
        p <- ggplot(data_to_plot, aes_string(x = input$x_var, y = input$y_var)) +
          geom_point(color = "steelblue", alpha = 0.7)
      }
      p <- p + theme_minimal() + labs(title = paste("Scatter plot:", input$x_var, "vs", input$y_var))
      
    } else if(input$plot_type == "corr") {
      numeric_data <- select_if(data_to_plot, is.numeric)
      if(ncol(numeric_data) > 1) {
        cor_matrix <- cor(numeric_data, use = "complete.obs")
        p <- ggplot(expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix)),
                   aes(Var1, Var2, fill = as.vector(cor_matrix))) +
          geom_tile() +
          scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = "Correlation Matrix", fill = "Correlation")
      } else {
        p <- ggplot() + geom_text(aes(x = 1, y = 1, label = "Not enough numeric variables")) + theme_void()
      }
      
    } else if(input$plot_type == "bar") {
      req(input$x_var)
      p <- ggplot(data_to_plot, aes_string(x = input$x_var)) +
        geom_bar(fill = "steelblue", alpha = 0.7) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = paste("Bar plot of", input$x_var))
    }
    
    ggplotly(p)
  })
  
  # Model Fitting
  observeEvent(input$fit_model, {
    data_for_model <- if(!is.null(values$cleaned_data)) values$cleaned_data else values$raw_data
    req(data_for_model, input$target_var)
    
    # Prepare data
    set.seed(123)
    train_index <- sample(1:nrow(data_for_model), 
                         size = round(input$train_split/100 * nrow(data_for_model)))
    train_data <- data_for_model[train_index, ]
    test_data <- data_for_model[-train_index, ]
    
    # Fit model
    if(input$model_type == "lm") {
      formula_str <- paste(input$target_var, "~ .")
      values$model <- lm(as.formula(formula_str), data = train_data)
      predictions <- predict(values$model, test_data)
      
      if(is.numeric(data_for_model[[input$target_var]])) {
        rmse <- sqrt(mean((test_data[[input$target_var]] - predictions)^2, na.rm = TRUE))
        r_squared <- cor(test_data[[input$target_var]], predictions, use = "complete.obs")^2
        values$model_performance <- list(RMSE = rmse, R_squared = r_squared)
      }
      
    } else if(input$model_type == "rf") {
      formula_str <- paste(input$target_var, "~ .")
      values$model <- randomForest(as.formula(formula_str), data = train_data, ntree = 100)
      predictions <- predict(values$model, test_data)
      
      if(is.numeric(data_for_model[[input$target_var]])) {
        rmse <- sqrt(mean((test_data[[input$target_var]] - predictions)^2, na.rm = TRUE))
        r_squared <- cor(test_data[[input$target_var]], predictions, use = "complete.obs")^2
        values$model_performance <- list(RMSE = rmse, R_squared = r_squared)
      } else {
        accuracy <- mean(test_data[[input$target_var]] == predictions, na.rm = TRUE)
        values$model_performance <- list(Accuracy = accuracy)
      }
      
    } else if(input$model_type == "glm") {
      formula_str <- paste(input$target_var, "~ .")
      if(is.factor(data_for_model[[input$target_var]]) || is.character(data_for_model[[input$target_var]])) {
        values$model <- glm(as.formula(formula_str), data = train_data, family = binomial())
        predictions <- predict(values$model, test_data, type = "response")
        predicted_class <- ifelse(predictions > 0.5, levels(as.factor(train_data[[input$target_var]]))[2], 
                                levels(as.factor(train_data[[input$target_var]]))[1])
        accuracy <- mean(test_data[[input$target_var]] == predicted_class, na.rm = TRUE)
        values$model_performance <- list(Accuracy = accuracy)
      }
    }
  })
  
  # Model Summary
  output$model_summary <- renderPrint({
    req(values$model)
    summary(values$model)
  })
  
  # Model Performance
  output$model_performance <- renderPrint({
    req(values$model_performance)
    cat("MODEL PERFORMANCE\n")
    cat("=================\n\n")
    for(metric in names(values$model_performance)) {
      cat(paste(metric, ":", round(values$model_performance[[metric]], 4), "\n"))
    }
  })
  
  # Feature Importance
  output$feature_importance <- renderPlot({
    req(values$model)
    
    if(input$model_type == "rf") {
      importance_data <- data.frame(
        Feature = rownames(importance(values$model)),
        Importance = importance(values$model)[, 1]
      )
      
      ggplot(importance_data, aes(x = reorder(Feature, Importance), y = Importance)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        theme_minimal() +
        labs(title = "Feature Importance", x = "Features", y = "Importance")
    } else if(input$model_type %in% c("lm", "glm")) {
      coef_data <- data.frame(
        Feature = names(coef(values$model))[-1],
        Coefficient = coef(values$model)[-1]
      )
      
      ggplot(coef_data, aes(x = reorder(Feature, abs(Coefficient)), y = Coefficient)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        theme_minimal() +
        labs(title = "Model Coefficients", x = "Features", y = "Coefficient Value")
    }
  })
  
  # Session Summary
  output$session_summary <- renderPrint({
    cat("ANALYSIS SESSION SUMMARY\n")
    cat("========================\n\n")
    
    if(!is.null(values$raw_data)) {
      cat("✓ Data uploaded successfully\n")
      cat(paste("  - Dimensions:", nrow(values$raw_data), "rows x", ncol(values$raw_data), "columns\n"))
    }
    
    if(!is.null(values$cleaned_data)) {
      cat("✓ Data cleaning applied\n")
      cat(paste("  - Final dimensions:", nrow(values$cleaned_data), "rows x", ncol(values$cleaned_data), "columns\n"))
    }
    
    if(!is.null(values$model)) {
      cat("✓ Model fitted successfully\n")
      cat(paste("  - Model type:", input$model_type, "\n"))
      cat(paste("  - Target variable:", input$target_var, "\n"))
    }
    
    cat("\nTimestamp:", Sys.time(), "\n")
  })
  
  # Download Handlers
  output$download_cleaned <- downloadHandler(
    filename = function() {
      paste("cleaned_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data_to_export <- if(!is.null(values$cleaned_data)) values$cleaned_data else values$raw_data
      write.csv(data_to_export, file, row.names = FALSE)
    }
  )
  
  output$download_excel <- downloadHandler(
    filename = function() {
      paste("analysis_results_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data_list <- list()
      
      if(!is.null(values$raw_data)) {
        data_list$"Raw Data" <- values$raw_data
      }
      
      if(!is.null(values$cleaned_data)) {
        data_list$"Cleaned Data" <- values$cleaned_data
      }
      
      write_xlsx(data_list, file)
    }
  )
  
  output$download_model <- downloadHandler(
    filename = function() {
      paste("fitted_model_", Sys.Date(), ".rds", sep = "")
    },
    content = function(file) {
      req(values$model)
      saveRDS(values$model, file)
    }
  )
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("plot_", input$plot_type, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # This would need the current plot to be saved
      # Implementation depends on the specific plot being displayed
      ggsave(file, width = 10, height = 8, dpi = 300)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)