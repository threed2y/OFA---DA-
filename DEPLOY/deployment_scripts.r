# deploy/deploy_shinyapps.R
# Script to deploy the Shiny app to ShinyApps.io

# Install and load required packages
if (!require("rsconnect")) {
  install.packages("rsconnect")
  library(rsconnect)
}

# Function to install all required packages
install_required_packages <- function() {
  packages <- c(
    "shiny", "shinydashboard", "DT", "plotly", "dplyr", "ggplot2",
    "corrplot", "VIM", "mice", "caret", "randomForest", "readr",
    "readxl", "writexl", "tidyr"
  )
  
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  if(length(new_packages)) {
    cat("Installing packages:", paste(new_packages, collapse = ", "), "\n")
    install.packages(new_packages, dependencies = TRUE)
  } else {
    cat("All required packages are already installed.\n")
  }
}

# Install packages
install_required_packages()

# Set up ShinyApps.io account (you need to fill in your credentials)
setup_shinyapps_account <- function() {
  cat("Setting up ShinyApps.io account...\n")
  cat("Please visit https://www.shinyapps.io/admin/#/tokens to get your token\n")
  cat("Then run:\n")
  cat("rsconnect::setAccountInfo(name='YOUR_ACCOUNT_NAME',\n")
  cat("                         token='YOUR_TOKEN',\n")
  cat("                         secret='YOUR_SECRET')\n")
}

# Deploy function
deploy_to_shinyapps <- function(app_name = "data-analytics-app") {
  
  # Check if account is configured
  accounts <- rsconnect::accounts()
  
  if (nrow(accounts) == 0) {
    setup_shinyapps_account()
    return()
  }
  
  cat("Deploying to ShinyApps.io...\n")
  
  # Deploy the app
  rsconnect::deployApp(
    appName = app_name,
    appTitle = "Data Analytics Dashboard",
    launch.browser = TRUE,
    forceUpdate = TRUE
  )
  
  cat("Deployment completed!\n")
}

# Run deployment
if (interactive()) {
  cat("Ready to deploy to ShinyApps.io\n")
  cat("Run: deploy_to_shinyapps('your-app-name')\n")
} else {
  # For non-interactive deployment
  deploy_to_shinyapps()
}

# -------------------------------------------------------------------
# deploy/install_packages.R
# Script to install all required packages for local development

install_packages <- function() {
  # List of required packages
  packages <- c(
    "shiny", "shinydashboard", "DT", "plotly", "dplyr", "ggplot2",
    "corrplot", "VIM", "mice", "caret", "randomForest", "readr",
    "readxl", "writexl", "tidyr", "rsconnect"
  )
  
  # Check which packages are not installed
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  if(length(new_packages)) {
    cat("Installing the following packages:\n")
    cat(paste(new_packages, collapse = ", "), "\n\n")
    
    # Install packages
    install.packages(new_packages, dependencies = TRUE)
    
    cat("\nPackage installation completed!\n")
  } else {
    cat("All required packages are already installed.\n")
  }
  
  # Load main packages to verify installation
  cat("\nVerifying package installation...\n")
  
  essential_packages <- c("shiny", "shinydashboard", "DT", "plotly")
  
  for(pkg in essential_packages) {
    if(require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("✓", pkg, "loaded successfully\n")
    } else {
      cat("✗", pkg, "failed to load\n")
    }
  }
  
  cat("\nSetup complete! You can now run the app with: shiny::runApp()\n")
}

# Run the installation
install_packages()