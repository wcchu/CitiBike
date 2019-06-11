suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(tidyverse))

## UI
citi_ui <- fluidPage(
  titlePanel("Time and location distribution of rides"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h2("Filter"),
      # input user type
      selectInput(inputId = "user_type",
                  label = "Choose a user type:",
                  choices = c("All", "Subscriber", "Customer")),
      # input weekdays
      checkboxGroupInput(inputId = "start_wday",
                         label = "Day in a week",
                         choices = c("Sun" = 0, "Mon" = 1, "Tue" = 2, "Wed" = 3,
                                     "Thu" = 4, "Fri" = 5, "Sat" = 6),
                         selected = c(0, 1, 2, 3, 4, 5, 6),
                         inline = FALSE),
      # input start time of day
      sliderInput(inputId = "start_time",
                  label = "Start time range",
                  min = 0, max = 24, step = 1, value = c(0, 24)),
      # data count before & after filter
      textOutput(outputId = "data_count")
    ),
    mainPanel(
      width = 9,
      h2("Filtered Data"),
      plotOutput(outputId = "start_times", height = 500),
      splitLayout(
        plotOutput(outputId = "start_locations", height = 600, brush = "brushed_starts"),
        plotOutput(outputId = "end_locations", height = 600, brush = "brushed_ends")
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
           hour = as.integer(format(time, "%H")) + as.integer(format(time, "%M")) / 60) %>%
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

  ## label whether the data passes filter or not
  all_data <- reactive({
    dat %>%
      mutate(
        pass = (
          ifelse(input$user_type == "All", TRUE, user_type == input$user_type) &
          wday %in% input$start_wday &
          hour >= input$start_time[1] &
          hour <= input$start_time[2]
        )
      )
  })

  ## sample data
  sampled_all_data <- reactive({
    nsam = 5000
    sample_n(all_data(), size = nsam, replace = (nsam > nrow(all_data())))
  })

  ## filter data
  filtered_data <- reactive({
    all_data() %>% filter(pass == TRUE)
  })

  ## sample filtered data
  sampled_filtered_data <- reactive({
    nsam = 5000
    sample_n(filtered_data(), size = nsam, replace = (nsam > nrow(filtered_data())))
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

  ## output data count
  output$data_count <- renderPrint({
    sprintf("Data count after/before filter = %d/%d", nrow(filtered_data()), nrow(dat))
  })

  ## output a start time distribution including original and filtered data
  output$start_times <- renderPlot({
    d <-
      sampled_all_data() %>%
      mutate(time = wday + hour/24) %>%
      select(time, pass)
    ggplot(d) +
      geom_histogram(aes(x = time, fill = pass),
                     position = "identity", binwidth = 0.05, alpha = 0.3) +
      xlim(-0.1, 7.1) +
      labs(title = "Distribution of start times",
           x = "Time in a week (day)", y = "Count")
  })

  ## general plot function for locations
  plot_locs <- function(d, title_string) {
    ggplot(d) +
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
    start_data <- sampled_filtered_data() %>% select(lat = lat_i, lon = lon_i)
    plot_locs(d = start_data, title_string = "Start locations")
  })

  ## output a plot of the ending locations
  output$end_locations <- renderPlot({
    end_data <- sampled_filtered_data() %>% select(lat = lat_f, lon = lon_f)
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
