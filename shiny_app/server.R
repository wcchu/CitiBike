suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(tidyverse))

## static data
dat <-
  read.csv("data.csv", header = T, stringsAsFactors = F) %>%
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
         gender) %>%
  mutate(wdaytime = wday + hour/24)

## all stations
stations <-
  rbind(
    dat %>% select(id = id_i, sta = sta_i, lat = lat_i, lon = lon_i),
    dat %>% select(id = id_f, sta = sta_f, lat = lat_f, lon = lon_f)
  ) %>%
  unique()

## general plot function for locations
plot_locs <- function(d, n, s, t) {
  r <- (n > nrow(d)) # replace if sample size larger than data
  ggplot(d %>% sample_n(size = n, replace = r)) +
    stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
                   size = 0.01, bins = 16, geom = "polygon") +
    geom_density2d(aes(x = lon, y = lat), size = 0.3) +
    scale_fill_gradient(low = "green", high = "red") +
    scale_alpha(range = c(0, 0.3), guide = FALSE) +
    geom_point(data = s, aes(x = lon, y = lat), col = "black", size = 1) +
    labs(x = 'Longitude', y = 'Latitude', title = t) +
    coord_fixed(ratio = 1)
}

server <- function(input, output, session) {
  ## label whether the data passes filter or not
  all_data <- reactive({
    req(input$user_type)
    req(input$start_wday)
    req(input$start_time)
    dat %>%
      mutate(
        pass =
          (input$user_type == "All" | user_type == input$user_type) &
          (wday %in% input$start_wday) &
          (hour >= input$start_time[1] & hour <= input$start_time[2])
      )
  })


  ## Filtering in the left panel
  ## filter data
  filtered_data <- reactive({
    all_data() %>%
      filter(pass == TRUE) %>%
      select(-pass)
  })
  ## output data count
  output$data_count <- renderPrint({
    sprintf("Data count after/before filter = %d/%d", nrow(filtered_data()), nrow(dat))
  })


  ## Time page
  ## output a start time distribution including original and filtered data
  output$start_times <- renderPlot({
    ggplot() +
      geom_histogram(data = all_data(), aes(x = wdaytime),
                     position = "identity", binwidth = 0.05, alpha = 0.3) +
      geom_histogram(data = filtered_data(), aes(x = wdaytime),
                     position = "identity", binwidth = 0.05, alpha = 0.3, fill = "red") +
      xlim(-0.1, 7.1) +
      labs(title = "Distribution of start times",
           x = "Time in a week (day)", y = "Count")
  })
  ## brushed time
  br_time_data <- reactive({
    brushedPoints(filtered_data(), input$br_start_time, xvar = "wdaytime")
  })
  ## output the brushed time to a table
  output$br_time_table <- renderDataTable(br_time_data())
  ## download the brushed area to csv
  output$br_time_download <- downloadHandler(
    filename = "brushed_times.csv",
    content = function(file) {
      write.csv(br_time_data(), file)
    }
  )


  ## Location page
  ## output a plot of the starting locations
  output$start_locations <- renderPlot({
    start_data <- filtered_data() %>% select(lat = lat_i, lon = lon_i)
    plot_locs(d = start_data, n = 5000, s = stations, t = "Start locations")
  })
  ## output a plot of the ending locations
  output$end_locations <- renderPlot({
    end_data <- filtered_data() %>% select(lat = lat_f, lon = lon_f)
    plot_locs(d = end_data, n = 5000, s = stations, t = "End locations")
  })
  ## brushed locations
  br_loc_data <- reactive({
    intersect(
      brushedPoints(filtered_data(), input$br_start_locs,
                    xvar = "lon_i", yvar = "lat_i"),
      brushedPoints(filtered_data(), input$br_end_locs,
                    xvar = "lon_f", yvar = "lat_f"))
  })
  ## output the brushed area to a table
  output$br_loc_table <- renderDataTable(br_loc_data())
  ## download the brushed area to csv
  output$br_loc_download <- downloadHandler(
    filename = "brushed_locations.csv",
    content = function(file) {
      write.csv(br_loc_data(), file)
    }
  )
}
