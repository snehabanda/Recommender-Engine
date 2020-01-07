library(shiny)

ui <- shinyUI(
  fluidPage(
    titlePanel("Travel Distance Analytics Dashboard"),
    sidebarLayout(
      sidebarPanel(h3("Enter Parameters"),
                   sliderInput("hist_breaks", "Number of breaks in Histogram",
                               3, 10, 5, step = 1),
                   sliderInput("freq_range", "Range of Frequency",
                               1, 40, c(2,25), step = 1),
                   dateInput("FROM_date",  # Input ID
                             label = tags$b(style = "font-size: 17px;", "FROM DATE: "), # label
                             value = "2018-08-26", # date value that shows up initially
                             min = Sys.Date() - 1000,  # set the minimin date
                             max = Sys.Date(), # set the maximum date
                             width = "100px") # set the width of widget)
                   ),
      mainPanel(h3("The Dashboard"),
                # plotOutput("timesave_plot", width = "50%"),
                # plotOutput("map_plot", width = "100%"),
                textOutput("test_date")
                )
    )
  )
)




library(ggmap)
library(ggplot2)


geo_key #Personal google API is to be used

register_google(key = geo_key)
location_data <- read.csv("loc4.csv")

dallas <- get_map(location = c(lon = mean(location_data$round_flon), lat = mean(location_data$round_flat)), zoom = 13,
                  maptype = "roadmap", source = "google")

server <- shinyServer(
  function(input,output){
    
    # output$timesave_plot <- renderPlot({
    #   hist(subset(location_data,Freq >= input$freq_range[1] & Freq <= input$freq_range[2])$save_ratio,
    #        breaks=input$hist_breaks,
    #        xlab = "Percentage of time saved by using Car over Transit",
    #        main = "Histogram of Time Saved Ratios")
    # })
    # output$map_plot <- renderPlot({
    #   ggmap(dallas) + geom_segment(data = subset(location_data, Freq >= input$freq_range[1] & Freq <= input$freq_range[2]) ,
    #                                aes(x=from_lon, y=from_lat, xend=to_lon, yend=to_lat, size=Freq, color = save_ratio))
    # }, height = 1000, width = 1000)
    output$test_date <- reactive(as.character(input$FROM_date))
  }
)


shinyApp(ui,server)
