suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(tidyverse))

## UI
ui <- fluidPage(
  titlePanel("Time and location distribution of rides"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "user_type",
                  label = "Choose a user type:",
                  choices = c("Subscriber", "Customer")),
      sliderInput(inputId = "start_time",
                  label = "Start time range",
                  min = 0.0,
                  max = 24.0,
                  value = c(8.0, 18.0)),
      sliderInput(inputId = "nsam_loc",
                  label = "Sampled location points",
                  min = 0,
                  max = 10000,
                  value = 5000)
    ),
    mainPanel(
      verbatimTextOutput(outputId = "data_count"),
      verbatimTextOutput(outputId = "time_summary"),
      plotOutput(outputId = "locations")
    )
  )
)

## Server
server <- function(input, output) {

  ## static data
  dat <-
    read.csv("citibike_2014-07.csv", header = T, stringsAsFactors = F) %>%
    # convert time to week day and hour
    mutate(time = as.POSIXct(starttime, tz = "EST"),
           dur = tripduration/60) %>%
    mutate(wday = as.integer(format(time, "%w")),
           hour = as.integer(format(time, "%H"))) %>%
    select(wday,
           hour,
           dur,
           id_i = start.station.id,
           sta_i = start.station.name,
           lat_i = start.station.latitude,
           lon_i = start.station.longitude,
           id_f = end.station.id,
           sta_f = end.station.name,
           lat_f = end.station.latitude,
           lon_f = end.station.longitude,
           bike = bikeid,
           user_type = usertype,
           birth = birth.year,
           gender)

  ## all stations
  stations <-
    rbind(
      dat %>% select(id = id_i, sta = sta_i, lat = lat_i, lon = lon_i),
      dat %>% select(id = id_f, sta = sta_f, lat = lat_f, lon = lon_f)
    ) %>%
    unique()

  ## reactive object
  datasetInput <- reactive({
    dat %>%
      filter(user_type == input$user_type,
             hour >= input$start_time[1],
             hour <= input$start_time[2])
  })

  ## output a summary of start time and duration in text format
  output$data_count <- renderPrint({
    sprintf("Total data count after filter = %d", nrow(datasetInput()))
  })

  ## output a summary of start time and duration in text format
  output$time_summary <- renderPrint({
    dataset_time <- datasetInput() %>% select(wday, hour, dur)
    summary(dataset_time)
  })

  ## output a plot of the starting locations
  output$locations <- renderPlot({
    dataset_loc <- datasetInput() %>% select(lat_i, lon_i)
    dataset_loc <- sample_n(dataset_loc,
                            size = input$nsam_loc,
                            replace = (input$nsam_loc > nrow(dataset_loc)))
    ggplot(dataset_loc) +
      stat_density2d(aes(x = lon_i, y = lat_i, fill = ..level.., alpha = ..level..),
                     size = 0.01, bins = 16, geom = "polygon") +
      geom_density2d(aes(x = lon_i, y = lat_i), size = 0.3) +
      scale_fill_gradient(low = "green", high = "red") +
      scale_alpha(range = c(0, 0.3), guide = FALSE) +
      geom_point(data = stations, aes(x = lon, y = lat), col = "black", size = 1) +
      labs(x = 'Longitude', y = 'Latitude', title = 'Fig.3 Start location')
  })
}

## Shiny App
shinyApp(ui, server)
