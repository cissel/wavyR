# Wavy.R by JHCV

##### Required Packages #####

require(tidyverse)
require(rvest)
require(plotly)

#####

##### Plot Appearance Theme #####

myTheme <- theme(legend.position = "none",
                 plot.background = element_rect(fill = "#02233F"),
                 panel.background = element_rect(fill = "#02233F"),
                 panel.grid = element_line(color = "#274066"),
                 axis.ticks = element_line(color = "#274066"),
                 axis.text = element_text(color = "white"),
                 axis.title = element_text(color = "white"),
                 plot.title = element_text(color = "white",
                                           hjust = .5),
                 plot.subtitle = element_text(color = "white",
                                              hjust = .5))

#####

##### Legend Appearance Theme #####

myLegend <- theme(legend.position = "right",
                  legend.background = element_rect(fill = "#02233F"),
                  legend.text = element_text(color = "white"),
                  legend.title = element_text(color = "white"))#,
#legend.key.height = unit(100, "cm"))

#####

##### Text Surf Report #####

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

#####

##### Text Surf Forecast #####
surfFcstTxt <- function() {
  
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

#####

##### Surf Forecast Dataframe #####

surfFcstDf <- function() {
  
  url <- "https://www.surfguru.com/jacksonville-beach-pier-surf-report"
  
  html <- read_html(url) |>
    
    html_nodes("div.surf-range") |>
    
    html_text()
  
  fcst <- strsplit(html, "\r\n")
  
  fcstDf <- data.frame("day" = 0:6)
  
  fcstDf$date <- Sys.Date()
  
  fcstDf$fcst <- ""
  
  for (i in 1:nrow(fcstDf)) {
    
    fcstDf$date[i] <- as.Date(Sys.Date()+fcstDf$day[i])
    fcstDf$fcst[i] <- fcst[[i]]
    
  }
  
  fcstDf$weekday <- ""
  
  for (i in 1:nrow(fcstDf)) {
    
    fcstDf$weekday[i] <- weekdays(fcstDf$date[i])
    
  }
  
  fcstDf$low <- ""
  fcstDf$high <- ""
  
  for (i in 1:nrow(fcstDf)) {
    
    fcstDf$low[i] <- strsplit(fcstDf$fcst[i], " - ")[[1]][1]
    fcstDf$high[i] <- strsplit(fcstDf$fcst[i], " - ")[[1]][2]
    
  }
  
  gsub("ft", "", fcstDf$low)
  gsub("ft", "", fcstDf$high)
  
  fcstDf$low <- parse_number(fcstDf$low)
  fcstDf$high <- parse_number(fcstDf$high)
  
  fcstDf <- fcstDf |> 
    
    select(weekday,
           date,
           fcst,
           low,
           high) 
  
  return(fcstDf)
  
}

#####

##### Surf Forecast Plot #####

plotSurfFcst <- function() {
  
  sfdf <- surfFcstDf()
  
  sfp <- ggplot(sfdf,
                aes(x = date)) +
    
    geom_bar(aes(weight = low),
             fill = "white",
             alpha = .75) +
    
    geom_bar(aes(weight = high),
             fill = "white",
             alpha = .5) +
    
    scale_x_date(breaks = sfdf$date,
                 labels = sfdf$weekday) +
    
    labs(x = "Day",
         y = "Wave Height (ft)",
         title = paste("Jax Beach Surf Forecast // ",
                       Sys.Date(),
                       sep = "")) +
    
    myTheme +
    theme(panel.grid.major.x = element_blank(),
          axis.ticks.x = element_blank())
  
  ggplotly(sfp)
  
}

#####
