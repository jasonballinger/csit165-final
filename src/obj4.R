# Load the data
merged <- merge(globalCasesByCountry, globalDeathsByCountry, by="Country.Region")
names(merged)[names(merged) == "X3.9.23.x"] <- "cases"
names(merged)[names(merged) == "X3.9.23.y"] <- "deaths"

# Log transformation of confirmed counts and death counts for the second plot
merged <- merged %>%
  mutate(log_total_cases = log(cases + 1),  # Adding 1 to avoid log(0)
         log_total_deaths = log(deaths + 1))  # Adding 1 to avoid log(0)

# Scatter plot for total cases vs. total deaths
plot1 <- ggplot(merged, aes(x = cases, y = deaths)) +
  geom_point() +
  labs(title = "Total Confirmed Cases vs. Total Death Counts", x = "Total Confirmed Cases", y = "Total Death Counts") +
  theme_minimal()

# Scatter plot for log-transformed total cases vs. log-transformed total deaths
plot2 <- ggplot(merged, aes(x = log_total_cases, y = log_total_deaths)) +
  geom_point() +
  labs(title = "Confirmed Cases vs. Death Counts (Log Transformed)", 
       x = "Log Transformed Confirmed Cases", y = "Log Transformed Death Counts") +
  theme_minimal()

# Display the plots side by side
grid.draw(grid.arrange(plot1, plot2, ncol=2))