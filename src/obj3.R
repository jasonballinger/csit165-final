# Load data
national_confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

# FIRST GRAPH
# Filter data to create a new data frame with only data from California 
california_confirmed <- national_confirmed %>%
  filter(Province_State == "California")

# Calculate column sums for the selected rows
california_sum <- colSums(california_confirmed[, 12:ncol(california_confirmed)], na.rm = TRUE)

# Create a new row with the sums and bind it to the dataframe keeping the initial columns
california_sum <- c(california_confirmed[1, 1:11], california_sum) 
california_confirmed <- rbind(california_confirmed, california_sum)

# Select the last row which contains the summed data
totaled_california_confirmed <- california_confirmed[nrow(california_confirmed), ]

# Convert data from wide to long format
totaled_california_confirmed <- totaled_california_confirmed %>%
  pivot_longer(cols = 12:ncol(totaled_california_confirmed), names_to = "date", values_to = "cases")

# Convert date column to the date format
totaled_california_confirmed$date <- as.Date(totaled_california_confirmed$date, format = "%m/%d/%y")

# Plot COVID-19 Confirmed Cases in California vs Date
ggplot(totaled_california_confirmed, aes(x = date, y = cases)) +
  geom_point() +
  labs(title = "COVID-19 Confirmed Cases: California",
       x = "Date",
       y = "Confirmed Cases") +
  theme_minimal()

# SECOND GRAPH
# create a new column in confirmed with the total confirmed cases for each country
california_confirmed_second_graph <- national_confirmed %>%
  filter(Province_State == "California") %>%
  mutate(total_confirmed = rowSums(select(., 12:length(.)), na.rm = TRUE)) %>%
  arrange(desc(total_confirmed))

# find the top 3 cities with the most confirmed cases
top_3_cities <- california_confirmed_second_graph[1:3, ]
print(top_3_cities$Admin2)

# convert top 3 cities data from wide to long format
top_3_cities <- top_3_cities %>%
  pivot_longer(cols = 12:ncol(national_confirmed), names_to = "date", values_to = "cases")

# convert top 3 cities date column to the date format
top_3_cities$date <- as.Date(gsub("X", "", top_3_cities$date), format = "%m/%d/%y")

# plot COVID-19 Confirmed Cases in the Top 3 Cities vs Date with DElta and Omicron Lines
ggplot(top_3_cities, aes(x = date, y = cases, color = `Admin2`)) +
  geom_point() +
  scale_color_manual(values = c("red", "orange", "yellow")) +
  labs(title = "COVID-19 Confirmed Cases: Top 3 Cities",
       x = "Date",
       y = "Confirmed Cases",
       color = "City") +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_vline(xintercept = as.Date("2021-05-11"), linetype = "dashed", color = "blue") +
  annotate("text", x = as.Date("2021-05-11"), y = Inf, label = "Delta", vjust = 1, hjust = 1) +
  geom_vline(xintercept = as.Date("2021-11-26"), linetype = "dashed", color = "green") +
  annotate("text", x = as.Date("2021-11-26"), y = Inf, label = "Omicron", vjust = 1, hjust = 1)