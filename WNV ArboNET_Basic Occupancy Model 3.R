# Install packages
install.packages(c("unmarked", "dplyr", "ggplot2", "sf", "tmap", "tidyr", "stars", "dichromat"))

# Load packages
library(unmarked)
library(dplyr)
library(ggplot2)
library(sf)
library(tmap)
library(tidyr)
library(stars)
library(dichromat)

# Load shapefile
shapefile_path <- "/Users/torrelavelle/downloads/usa_counties.shp"
shp <- st_read(shapefile_path)

# Load ArboNET data for WNV cases
arbonet_data_path <- "/Users/torrelavelle/downloads/spillovers_arbonet (1).csv"
arbonet_data <- read.csv(arbonet_data_path)

# Display structure of ArboNET data to verify columns
str(arbonet_data)

# Create full data frame with all years and locations
data_full <- expand.grid(unique(arbonet_data$ADMcode), 2004:2020) %>%
  as.data.frame() %>%
  rename("ADMcode" = 1, "Year" = 2) %>%
  left_join(
    arbonet_data %>%
      filter(Disease == "West Nile virus disease") %>%
      select(ADMcode, Year, NumCases),
    by = c("ADMcode", "Year")
  ) %>%
  mutate(NumCases = replace(NumCases, is.na(NumCases), 0), # Set NAs to zero
         Present = ifelse(NumCases > 0, 1, 0)) # Create presence-absence column

# Load covariate data for population density, healthcare access, stagnant water proximity, and precipitation

# Load population density data
population_density <- read.csv("/Users/torrelavelle/downloads/population_density.csv")
population_density <- population_density %>%
  rename(ADMcode = county_code) %>%
  select(ADMcode, year, pop_density)

# Load healthcare access data (insurance coverage)
insurance_coverage <- read.csv("/Users/torrelavelle/downloads/insurance_coverage.csv")
insurance_coverage <- insurance_coverage %>%
  rename(ADMcode = county_code) %>%
  select(ADMcode, year, insurance_coverage)

# Load proximity to stagnant water data
stagnant_water_proximity <- read.csv("/Users/torrelavelle/downloads/stagnant_water_proximity.csv")
stagnant_water_proximity <- stagnant_water_proximity %>%
  rename(ADMcode = county_code) %>%
  select(ADMcode, year, proximity_index)

# Load precipitation data
precipitation_data <- read.csv("/Users/torrelavelle/downloads/precipitation_data.csv")
precipitation_data <- precipitation_data %>%
  rename(ADMcode = county_code) %>%
  select(ADMcode, year, total_precipitation)

# Merge all covariates with main dataset
covariate_data <- full_join(data_full, population_density, by = c("ADMcode", "Year")) %>%
  full_join(insurance_coverage, by = c("ADMcode", "Year")) %>%
  full_join(stagnant_water_proximity, by = c("ADMcode", "Year")) %>%
  full_join(precipitation_data, by = c("ADMcode", "Year"))

# Subset data for California and Arizona
ca_az_shp <- shp %>%
  filter(STATE_NAME %in% c("California", "Arizona"))

# Filter covariate data for two states
ca_az_data <- data_full %>%
  filter(ADMcode %in% ca_az_shp$ADMcode)

# Create detection history matrix
detection_history <- ca_az_data %>%
  spread(key = Year, value = Present, fill = 0)

# Ensure detection history matches site covariates
detection_history <- detection_history[order(detection_history$ADMcode),]
ca_az_shp <- ca_az_shp[order(ca_az_shp$ADMcode),]

# Create unmarked frame for occupancy modeling
detection_matrix <- as.matrix(detection_history[, -1])  # Removing ADMcode column
umf <- unmarkedFrameOccu(y = detection_matrix, siteCovs = ca_az_shp)

# Fit occupancy model
# Replace covariate names (e.g., pop_density, insurance_coverage, etc.) with actual column names in your data
fm <- occu(~ 1 ~ NumCases, data = umf)

# Summary of fitted model
summary(fm)

# Predict detection probabilities
detection_probs <- predict(fm, type = "det")

# Add detection probabilities to shapefile data
ca_az_shp$detection_prob <- detection_probs$Predicted

# Plot map
tm_shape(ca_az_shp) +
  tm_polygons("detection_prob", palette = "RdYlBu", title = "Detection Probability") +
  tm_layout(title = "Detection Probability of West Nile Virus (2004-2020)",
            legend.outside = TRUE)

# Save map using tmap_save
tmap_save(
  tm_shape(ca_az_shp) +
    tm_polygons("detection_prob", palette = "RdYlBu", title = "Detection Probability") +
    tm_layout(title = "Detection Probability of West Nile Virus (2004-2020)",
              legend.outside = TRUE),
  filename = "WNV_detection_probability_map.png"
)
