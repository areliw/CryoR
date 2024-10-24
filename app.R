# app.R

# Error handling for package loading
load_required_packages <- function() {
  required_packages <- c(
    "shiny",
    "shinydashboard",
    "shinyjs",
    "DT",
    "googlesheets4",
    "ggplot2",
    "data.table",
    "Rcpp",
    "memoise",
    "pwr",
    "htmltools"
  )
  
  packages_status <- sapply(required_packages, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(paste("Installing package:", pkg))
      install.packages(pkg, repos = "https://cloud.r-project.org/")
      return(FALSE)
    }
    return(TRUE)
  })
  
  if (!all(packages_status)) {
    packages_status <- sapply(required_packages, function(pkg) {
      requireNamespace(pkg, quietly = TRUE)
    })
    if (!all(packages_status)) {
      missing_packages <- required_packages[!packages_status]
      stop("Failed to install packages: ", 
           paste(missing_packages, collapse = ", "))
    }
  }
  
  # Load all packages
  sapply(required_packages, library, character.only = TRUE)
}

# Initialize error handling
initialize_error_handling <- function() {
  options(shiny.error = function() {
    stop.function <- function() {
      tags$div(
        class = "shiny-output-error-validation",
        "เกิดข้อผิดพลาด กรุณาลองใหม่อีกครั้งหรือติดต่อผู้ดูแลระบบ"
      )
    }
    stop.function
  })
}

# Load all module files
load_modules <- function() {
  modules_dir <- "modules"
  if (!dir.exists(modules_dir)) {
    stop("Modules directory not found")
  }
  
  module_files <- list.files(
    modules_dir, 
    pattern = "\\.R$", 
    full.names = TRUE
  )
  
  if (length(module_files) == 0) {
    stop("No module files found")
  }
  
  # Load each module file
  sapply(module_files, function(file) {
    tryCatch({
      source(file, local = TRUE)
    }, error = function(e) {
      stop("Error loading module: ", file, "\n", e$message)
    })
  })
}

# Load utility functions
load_utils <- function() {
  utils_file <- "R/utils.R"
  if (!file.exists(utils_file)) {
    stop("Utils file not found")
  }
  
  tryCatch({
    source(utils_file, local = TRUE)
  }, error = function(e) {
    stop("Error loading utils: ", e$message)
  })
}

# Main initialization function
initialize_app <- function() {
  # Load and check packages
  load_required_packages()
  
  # Set up error handling
  initialize_error_handling()
  
  # Load required files
  required_files <- c("global.R", "ui.R", "server.R")
  missing_files <- required_files[!file.exists(required_files)]
  
  if (length(missing_files) > 0) {
    stop("Missing required files: ", 
         paste(missing_files, collapse = ", "))
  }
  
  # Load global variables and functions
  source("global.R")
  
  # Load utility functions
  load_utils()
  
  # Load all modules
  load_modules()
  
  # Load UI and server
  source("ui.R")
  source("server.R")
  
  # Return the Shiny app
  shinyApp(ui = ui, server = server)
}

# Execute the initialization and run the app
tryCatch({
  initialize_app()
}, error = function(e) {
  message("Error initializing application: ", e$message)
  quit(status = 1)
})
