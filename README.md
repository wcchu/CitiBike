# CitiBike rental system

We study how riders used the CitiBike bike rental system in New York City in July 2014.

In "Part 1: Exploratory data analysis", we find the basic facts of how people used this system. The data is visually explored and summarized with basic time and location information. This analysis provides an intuitive understanding of the details in Part 2 and Part 3. Possible extension of this work is also discussed.

In "Part 2: Subscribers and customers", we focus on the differece between subscribers (with annual passes) and customers (with daily or weekly passes). We build random forest, RuleFit, and boosted decision tree models to predict a certain behavior (location and time of ride) belonging to a subscriber or customer. We suggest how to expand the subscriber base and how to attract more short-term users

In "Part 3: Availabilities of bikes and parking spaces", we investigate the issues of insufficient available bikes and insufficient parking spaces. The manual relocation (shipping by trucks) of the bikes is identified, and its adjustment is suggested.

-----

The data file is compressed in citibike_2014-07.csv and should be extracted before running the Rmd files.
