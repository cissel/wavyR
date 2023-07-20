#required packages
require(tidyverse)
require(rvest)

# surf report
surfReport <- function() {
  
  url <- "https://www.surfguru.com/jacksonville-beach-pier-surf-report"
  
  html <- read_html(url) |>
    html_nodes("#report-text > div.col-12.col-sm-9 > div.report-block.surf-report.mt-0") |>
    html_text() 
  
  report <- strsplit(html, "\r\n")[[1]][2:4] |> unlist()
  
  df <- data.frame("timestamp" = report[[1]],
                   "location" = report[[2]],
                   "report" = report[[3]])
  
  return(df)
  
}

# surf forecast
surfFcst <- function() {
  
  url <- "https://www.surfguru.com/jacksonville-beach-pier-surf-report"
  
  html <- read_html(url) |>
    html_nodes("#report-text > div.col-12.col-sm-9 > div.report-block.surf-forecast") |>
    html_text()
  
  forecast <- strsplit(html, "\r\n")[[1]][c(1, 3, 5)]
  
  df <- data.frame("location" = forecast[[1]],
                   "tomorrow" = forecast[[2]],
                   "future" = forecast[[3]])
  
  return(df)
  
}