# CitiBike rental system

We study how riders used the CitiBike bike rental system in New York City in July 2014.

In "Part 1: Exploratory data analysis", we find the basic facts of how people used this system. The data is visually explored and summarized with basic time and location information. This analysis provides an intuitive understanding of the details in Part 2 and Part 3. Possible extension of this work is also discussed.

In "Part 2: Predict the user and the trip", we ask the question whether a station can predict (1) if a user is a subscriber (with a annual pass) or not (with a daily or weekly pass) and (2) the duration and end location of the trip, given the location and time of the renting. We build random forest and RuleFit classifiers for (1) and random forest, rulefit, and tensorflow regressors for (2). Such predictions could help Citi to prepare for the trip, to promote events, and to expand the customer base.

In "Part 3: Availabilities of bikes and parking spaces", we investigate the issues of insufficient available bikes and insufficient parking spaces. The manual relocation (shipping by trucks) of the bikes is identified, and its adjustment is suggested.

-----

The data file is compressed in citibike_2014-07.csv and should be extracted before running the Rmd files.
