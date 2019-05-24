suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(tidyverse))

## UI
citi_ui <- fluidPage(
  titlePanel("Time and location distribution of rides"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "user_type",
                  label = "Choose a user type:",
                  choices = c("All", "Subscriber", "Customer")),
      checkboxGroupInput(inputId = "start_wday",
                         label = "Day in a week",
                         choices = c("Sun" = 0,
                                     "Mon" = 1,
                                     "Tue" = 2,
                                     "Wed" = 3,
                                     "Thu" = 4,
                                     "Fri" = 5,
                                     "Sat" = 6),
                         selected = c(0, 1, 2, 3, 4, 5, 6),
                         inline = TRUE),
      sliderInput(inputId = "start_time",
                  label = "Start time range",
                  min = 0.0,
                  max = 24.0,
                  step = 0.5, # step is 30 mins
                  value = c(0.0, 24.0)),
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

## Server function
citi_server <- function(input, output, session) {

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
    ## filter by user type
    if (input$user_type != "All") {
      dat <- dat %>% filter(user_type == input$user_type)
    }
    ## filter by start time in a day
    dat %>%
      filter(wday %in% input$start_wday,
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
    dataset_loc <- datasetInput() %>% select(lat_i, lon_i, lat_f, lon_f)
    dataset_loc <- sample_n(dataset_loc,
                            size = input$nsam_loc,
                            replace = (input$nsam_loc > nrow(dataset_loc)))
    dataset_loc <-
      rbind(
        dataset_loc %>%
          select(lat = lat_i, lon = lon_i) %>%
          mutate(label = "1. start"),
        dataset_loc %>%
          select(lat = lat_f, lon = lon_f) %>%
          mutate(label = "2. end")
      )

    ggplot(dataset_loc) +
      stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
                     size = 0.01, bins = 16, geom = "polygon") +
      geom_density2d(aes(x = lon, y = lat), size = 0.3) +
      scale_fill_gradient(low = "green", high = "red") +
      scale_alpha(range = c(0, 0.3), guide = FALSE) +
      geom_point(data = stations, aes(x = lon, y = lat), col = "black", size = 1) +
      labs(x = 'Longitude', y = 'Latitude', title = 'Fig.3 Start location') +
      coord_fixed(ratio = 1) +
      facet_grid(. ~ label)
  })
}

## Shiny App
shinyApp(ui = citi_ui, server = citi_server)
