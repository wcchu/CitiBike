# CitiBike rental system

We study how riders used the CitiBike bike rental system in New York City in July 2014.

## Data Exploration

We explore the basic properties of the data and draw some basic facts in `explore.Rmd` (rendered to `explore.pdf`). The data is visualized and summarized with basic time and location information. This analysis provides an intuitive understanding of the details in the prediction model later. Possible extension of this work is also discussed.

## Shiny app

In the shiny app, we filter the data by the starting time/weekday and the customer type (subscriber or not) and plot the starting time histogram and the start/end location map, with the tabs "Time" and "Location" respectively. We can then brush the time histogram or map to extract data into a viewable table and download it to a csv file. Run `R -e "shiny::runApp('shiny_app')"` to start the app locally. The app has also been deployed through shinyapps.io at https://wcchu.shinyapps.io/shiny_app/.

## Prediction Models

For a trip with given starting location and time, we want to predict (1) the user type of the renter--is it a "subscriber" (with a annual pass) or a "customer" (with a daily or weekly pass), (2) the duration of the ride. For (1), we build random forest and RuleFit classifiers, and for (2), we use neural networks. Such questions are important because they could help prepare for the trip, promote events, and/or plan infrastructure updates.

## Availability

(In progress) we investigate the issues of insufficient available bikes and insufficient parking spaces. The manual relocation (shipping by trucks) of the bikes is identified, and its adjustment is suggested.
