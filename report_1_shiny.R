suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(tidyverse))

## UI
citi_ui <- fluidPage(
  titlePanel("Time and location distribution of rides"),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      h2("Filter"),
      selectInput(inputId = "user_type",
                  label = "Choose a user type:",
                  choices = c("All", "Subscriber", "Customer")),
      checkboxGroupInput(inputId = "start_wday",
                         label = "Day in a week",
                         choices = c("Sun" = 0, "Mon" = 1, "Tue" = 2, "Wed" = 3,
                                     "Thu" = 4, "Fri" = 5, "Sat" = 6),
                         selected = c(0, 1, 2, 3, 4, 5, 6),
                         inline = FALSE),
      sliderInput(inputId = "start_time",
                  label = "Start time range",
                  min = 0, max = 24, step = 1, value = c(0, 24))
    ),
    mainPanel(
      width = 10,
      h2("Filtered Data"),
      verbatimTextOutput(outputId = "data_count"),
      splitLayout(
        plotOutput(height = 600, outputId = "start_locations", brush = "brushed_starts"),
        plotOutput(height = 600, outputId = "end_locations", brush = "brushed_ends")
      ),
      dataTableOutput(outputId = "brushed_table"),
      downloadButton(outputId = "brushed_download", label = "Download Table")
    )
  )
)

## Server function
citi_server <- function(input, output, session) {

  ## static data
  dat <-
    read.csv("citibike_2014-07.csv",
    #read.csv("small.csv",
             header = T, stringsAsFactors = F) %>%
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

  ## filter data
  filtered_data <- reactive({
    ## filter by user type
    if (input$user_type != "All") {
      d = dat <- dat %>% filter(user_type == input$user_type)
    } else {
      d = dat
    }
    ## filter by start time in a day
    d %>%
      filter(wday %in% input$start_wday,
             hour >= input$start_time[1],
             hour <= input$start_time[2]) %>%
      mutate(id = row_number())
  })

  ## brushed data
  brushed_data <- reactive({
    brushed_starts <- brushedPoints(filtered_data(), input$brushed_starts,
                                    xvar = "lon_i", yvar = "lat_i")
    brushed_ends <- brushedPoints(filtered_data(), input$brushed_ends,
                                  xvar = "lon_f", yvar = "lat_f")
    brushed_both <- intersect(brushed_starts, brushed_ends)
    return(brushed_both)
  })

  ## output a summary of start time and duration in text format
  output$data_count <- renderPrint({
    sprintf("Total data count after filter = %d", nrow(filtered_data()))
  })

  ## general plot function for locations
  plot_locs <- function(d, nsam = 5000, title_string) {
    sampled_data <- sample_n(d, size = nsam, replace = (nsam > nrow(d)))

    ggplot(sampled_data) +
      stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
                     size = 0.01, bins = 16, geom = "polygon") +
      geom_density2d(aes(x = lon, y = lat), size = 0.3) +
      scale_fill_gradient(low = "green", high = "red") +
      scale_alpha(range = c(0, 0.3), guide = FALSE) +
      geom_point(data = stations, aes(x = lon, y = lat), col = "black", size = 1) +
      labs(x = 'Longitude', y = 'Latitude', title = title_string) +
      coord_fixed(ratio = 1)
  }

  ## output a plot of the starting locations
  output$start_locations <- renderPlot({
    start_data <- filtered_data() %>% select(lat = lat_i, lon = lon_i)
    plot_locs(d = start_data, title_string = "Start locations")
  })

  ## output a plot of the ending locations
  output$end_locations <- renderPlot({
    end_data <- filtered_data() %>% select(lat = lat_f, lon = lon_f)
    plot_locs(d = end_data, title_string = "End locations")
  })

  ## output the brushed area to a table
  output$brushed_table <- renderDataTable(brushed_data())

  ## download the brushed area to csv
  output$brushed_download <- downloadHandler(
    filename = "plot_extract.csv",
    content = function(file) {
      write.csv(brushed_data(), file)
    }
  )
}

## Shiny App
shinyApp(ui = citi_ui, server = citi_server)
