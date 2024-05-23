# Manipulate data: group by country, average Latitude and Longitude, summarise case totals
globalCasesByCountry <- covid_global_confirmed %>%
  filter(!is.na(Lat)) %>%
  group_by(Country.Region) %>%
  summarise(across(Lat:Long, mean), across(c(starts_with("X")), sum)) %>%
  select(Country.Region, Lat, Long, X3.9.23)

globalDeathsByCountry <- covid_global_deaths %>%
  group_by(Country.Region) %>%
  summarise(across(c(starts_with("X")), sum)) %>%
  select(Country.Region, X3.9.23)

# Get values for every 20% of cases
quartiles <- quantile(globalCasesByCountry$X3.9.23, probs = c(0.2, 0.4, 0.6, 0.8))

# Initialize the map
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = 0.0, lat = 10.0, zoom = 1.5)

# Iterate over every country
for (i in 1:nrow(globalCasesByCountry)) {
  cases <- globalCasesByCountry$X3.9.23[i]
  deaths <- globalDeathsByCountry$X3.9.23[i]
  
  # Create the popup message
  popupMessage <- paste(
    "<b>", globalCasesByCountry$Country.Region[i], "</b>",
    "<br>Cases:", format(cases, big.mark = ","),
    "<br>Deaths:", format(deaths, big.mark = ",")
  )
  
  # Add marker to the map given latitude and longitude, set color according to quantile function
  map <- map %>%
    addCircleMarkers(lng = globalCasesByCountry$Long[i], lat = globalCasesByCountry$Lat[i], popup = popupMessage, label = globalCasesByCountry$Country.Region[i], radius = 5, color = case_when(cases > quartiles[4] ~ rgb(1,0,0), cases > quartiles[3] ~ rgb(0.8, 0.4, 0.4), cases > quartiles[2] ~ rgb(0.5, 0.5, 0.5), cases > quartiles[1] ~ rgb(0.4, 0.4, 0.8), TRUE ~ rgb(0, 0, 1)))
}

# Display the map
map