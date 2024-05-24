global_confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/4360e50239b4eb6b22f3a1759323748f36752177/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

# List of unique countries in the dataset
countries <- unique(global_confirmed$`Country/Region`)

# Initialize empty lists to store summed counts
summed_global_confirmed <- vector("numeric", length(countries))

# Sum counts for each country
for (i in 1:length(countries)) {
  country <- countries[i]
  summed_global_confirmed[i] <- sum(global_confirmed[global_confirmed$`Country/Region` == country, 5:ncol(global_confirmed)])
}

# Create data frames from summed counts
sum_global_confirmed_dataframe <- data.frame(Country = countries, Confirmed = summed_global_confirmed)

# Sort the data frames in descending order based on counts
sum_global_confirmed_dataframe <- sum_global_confirmed_dataframe[order(-sum_global_confirmed_dataframe$Confirmed), ]

# Table for countries confirmations
sum_global_confirmed_dataframe %>%
  kbl() %>%
  kable_material(c("striped", "hover", "condensed")) %>%
  add_header_above(c("Table of Top Countries", " ", " "))

# Print tables
print(sum_global_confirmed_dataframe)