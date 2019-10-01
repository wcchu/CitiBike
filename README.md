# CitiBike rental system

We study how riders used the CitiBike bike rental system in New York City in July 2014.

## Data Exploration

We explore the basic properties of the data and draw some basic facts in `explore.Rmd` (rendered to `explore.pdf`). The data is visualized and summarized with basic time and location information. This analysis provides an intuitive understanding of the details in the prediction model later. Possible extension of this work is also discussed.

## Shiny app

In the shiny app, we filter the data by the starting time/weekday and the customer type (subscriber or not) and plot the starting time histogram and the start/end location map, with the tabs "Time" and "Location" respectively. We can then brush the time histogram or map to extract data into a viewable table and download it to a csv file. Run `R -e "shiny::runApp('shiny_app')"` to start the app locally. The app has also been deployed through shinyapps.io at https://wcchu.shinyapps.io/shiny_app/.

## Predict the user type

In "Part 2: Predict the user and the trip", we ask the question whether a station can predict (1) if a user is a subscriber (with a annual pass) or not (with a daily or weekly pass) and (2) the duration and end location of the trip, given the location and time of the renting. We build random forest and RuleFit classifiers for (1) and random forest, rulefit, and tensorflow regressors for (2). Such predictions could help Citi to prepare for the trip, to promote events, and to expand the customer base.

## Availability

In "Part 3: Availabilities of bikes and parking spaces", we investigate the issues of insufficient available bikes and insufficient parking spaces. The manual relocation (shipping by trucks) of the bikes is identified, and its adjustment is suggested.
