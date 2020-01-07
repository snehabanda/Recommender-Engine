library(shiny)
library(ggmap)
library(ggplot2)


geo_key #Using personal google API 

register_google(key = geo_key)
location_data <- read.csv("loc4.csv")

dallas <- get_map(location = c(lon = mean(location_data$round_flon), lat = mean(location_data$round_flat)), zoom = 13,
                  maptype = "roadmap", source = "google")

shinyServer(
  function(input,output){
    
    output$timesave_plot <- renderPlot({
      hist(subset(location_data, Freq >= input$freq_range[1] & Freq <= input$freq_range[2])$save_ratio,
           breaks=input$hist_breaks,
           xlab = "Percentage of time saved by using Car over Transit",
           main = "Histogram of Time Saved Ratios")
    })
    output$map_plot <- renderPlot({
      ggmap(dallas) + geom_segment(data = subset(location_data, Freq >= input$freq_range[1] & Freq <= input$freq_range[2]) ,
                                   aes(x=from_lon, y=from_lat, xend=to_lon, yend=to_lat, size=Freq, color = save_ratio))
    }, height = 1000, width = 1000)
  }
)