# global.R
# ฟังก์ชันตรวจสอบระบบ
check_system_requirements <- function() {
  tryCatch({
    # ตรวจสอบเวอร์ชัน R
    if(getRversion() < "4.4.1") {
      stop("ต้องการ R เวอร์ชัน 4.4.1 หรือสูงกว่า")
    }
    
    # ตรวจสอบพื้นที่ว่าง
    if(.Platform$OS.type == "unix") {
      free_space <- as.numeric(system("df -k / | tail -1 | awk '{print $4}'", 
                                    intern = TRUE))
      if(free_space < 1000000) {
        warning("พื้นที่ว่างในระบบเหลือน้อย (น้อยกว่า 1GB)")
      }
    }
    
    # ตรวจสอบการเชื่อมต่ออินเทอร์เน็ต
    if(!curl::has_internet()) {
      stop("ไม่สามารถเชื่อมต่ออินเทอร์เน็ตได้")
    }
  }, error = function(e) {
    stop(paste("เกิดข้อผิดพลาดในการตรวจสอบระบบ:", e$message))
  })
}

# ฟังก์ชันติดตั้งและตรวจสอบแพ็คเกจ
check_and_install_packages <- function() {
  required_packages <- list(
    shiny = "1.9.1",
    shinydashboard = "0.7.2",
    shinyjs = "2.1.0",
    DT = "0.27",
    googlesheets4 = "1.1.0",
    ggplot2 = "3.4.0",
    data.table = "1.14.8",
    Rcpp = "1.0.10",
    memoise = "2.0.1",
    pwr = "1.3.0",
    htmltools = "0.5.8.1"
  )
  
  for(pkg in names(required_packages)) {
    if(!requireNamespace(pkg, quietly = TRUE)) {
      message(paste("กำลังติดตั้งแพ็คเกจ:", pkg))
      install.packages(pkg, 
                      version = required_packages[[pkg]],
                      repos = "https://cloud.r-project.org")
    }
    
    pkg_version <- packageVersion(pkg)
    if(pkg_version != required_packages[[pkg]]) {
      warning(paste(
        "เวอร์ชันของแพ็คเกจ", pkg,
        "ไม่ตรงตามที่กำหนด\n",
        "ต้องการ:", required_packages[[pkg]],
        "ติดตั้งอยู่:", pkg_version
      ))
    }
  }
  
  invisible(lapply(names(required_packages), library, character.only = TRUE))
}

# ฟังก์ชันตั้งค่าการจัดการข้อผิดพลาด
setup_error_handling <- function() {
  options(shiny.error = function() {
    function(e) {
      # บันทึกข้อผิดพลาด
      message(paste("เกิดข้อผิดพลาด:", e$message))
      
      # แสดงข้อความแจ้งเตือน
      tags$div(
        class = "alert alert-danger",
        icon("exclamation-triangle"),
        "เกิดข้อผิดพลาดในระบบ กรุณาลองใหม่อีกครั้งหรือติดต่อผู้ดูแลระบบ"
      )
    }
  })
  
  # ตั้งค่าขนาดไฟล์สูงสุด
  options(shiny.maxRequestSize = 30 * 1024^2)
}

# ฟังก์ชันตั้งค่าระบบแคช
setup_caching <- function() {
  cache_dir <- "cache"
  if(!dir.exists(cache_dir)) {
    dir.create(cache_dir)
  }
  
  # ตั้งค่า memoise สำหรับการอ่านข้อมูล
  get_sheet_data <- memoise::memoise(
    googlesheets4::read_sheet,
    cache = cache_filesystem(cache_dir)
  )
  
  # ล้างแคชอัตโนมัติ
  cache_cleanup <- function() {
    unlink(file.path(cache_dir, "*"))
  }
  
  later::later(cache_cleanup, 24 * 60 * 60)
}

# ฟังก์ชันโหลดไฟล์ที่จำเป็น
load_required_files <- function() {
  required_files <- c(
    "ui.R",
    "server.R",
    "www/custom.css"
  )
  
  missing_files <- required_files[!file.exists(required_files)]
  if(length(missing_files) > 0) {
    stop(paste("ไม่พบไฟล์ที่จำเป็น:", paste(missing_files, collapse = ", ")))
  }
  
  # โหลดโมดูลทั้งหมด
  module_files <- list.files("modules", pattern = "\\.R$", full.names = TRUE)
  for(module in module_files) {
    source(module)
  }
  
  # โหลด utility functions
  source("R/utils.R")
}
