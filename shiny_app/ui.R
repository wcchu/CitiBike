suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(tidyverse))

ui <- fluidPage(
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
      tabsetPanel(
        tabPanel(
          "Time",
          plotOutput(outputId = "start_times", height = 500)),
        tabPanel(
          "Location",
          splitLayout(
            plotOutput(outputId = "start_locations", height = 600, brush = "brushed_starts"),
            plotOutput(outputId = "end_locations", height = 600, brush = "brushed_ends")
          ),
          "Choose the area of start locations and area of end locations to generate data table",
          dataTableOutput(outputId = "brushed_table"),
          downloadButton(outputId = "brushed_download", label = "Download Table"))
      )
    )
  )
)
