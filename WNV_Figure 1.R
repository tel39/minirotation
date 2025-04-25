# Load libraries
library(unmarked)
library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)
library(gridExtra)
library(readr)

# Load covariate data
population_density <- read_csv("/Users/torrelavelle/Dropbox/cleaned_population_density_data.csv", show_col_types = FALSE) %>%
  rename(ADMcode = CountyFIPS) %>%
  mutate(ADMcode = as.numeric(ADMcode))

insurance_coverage <- read_csv("/Users/torrelavelle/Dropbox/updated_cleaned_insurance_coverage.csv", show_col_types = FALSE) %>%
  rename(ADMcode = CountyFIPS) %>%
  mutate(ADMcode = as.numeric(ADMcode))

stagnant_water_proximity <- read_csv("/Users/torrelavelle/Dropbox/combined_bodies_of_water_data.csv", show_col_types = FALSE) %>%
  rename(ADMcode = CountyFIPS) %>%
  mutate(ADMcode = as.numeric(ADMcode))

precipitation_data <- read_csv("/Users/torrelavelle/Dropbox/updated_annual_precipitation_summary.csv", show_col_types = FALSE) %>%
  rename(ADMcode = CountyFIPS) %>%
  mutate(ADMcode = as.numeric(ADMcode))

# Set and load shapefile and ArboNET data
shapefile_path <- "/Users/torrelavelle/Dropbox/arbo_occupancy/usa_counties.shp"

shp <- st_read(shapefile_path) %>%
  mutate(ADMcode = as.numeric(ADMcode)) %>%
  filter(!STATEFP %in% c("02", "15"))  # Remove Alaska and Hawaii

arbonet_data_path <- "/Users/torrelavelle/Dropbox/arbo_occupancy/spillovers_arbonet.csv"
arbonet_data <- read.csv(arbonet_data_path)

# Create dataset with all years and locations in U.S.
data_full <- expand.grid(unique(shp$ADMcode), 2004:2020) %>%
  as.data.frame() %>%
  rename("ADMcode" = 1, "Year" = 2) %>%
  left_join(
    arbonet_data %>%
      filter(Disease == "West Nile virus disease" & Presentation == "All cases") %>%
      select(ADMcode, Year, NumCases),
    by = c("ADMcode", "Year")
  ) %>%
  mutate(NumCases = replace(NumCases, is.na(NumCases), 0),
         Present = ifelse(NumCases > 0, 1, 0))

# Merge covariates with main dataset
covariate_data <- data_full %>%
  left_join(population_density, by = c("ADMcode", "Year")) %>%
  left_join(insurance_coverage, by = c("ADMcode", "Year")) %>%
  left_join(stagnant_water_proximity, by = c("ADMcode", "Year")) %>%
  left_join(precipitation_data, by = c("ADMcode", "Year"))

summary(covariate_data)

# Matrix of detections by ADM/year
covariate_data %>%
  ggplot() +
  geom_tile(aes(x = Year, y = factor(ADMcode), fill = factor(Present))) +
  theme_classic() +
  scale_fill_viridis_d()

covariate_data %>%
  ggplot() +
  geom_tile(aes(x = Year, y = factor(ADMcode), fill = log(NumCases + 1))) +
  theme_classic() +
  scale_fill_viridis_c()

detection_history <- covariate_data %>%
  select(-NumCases) %>%
  pivot_wider(names_from = "Year", values_from = "Present")

numcase_history <- covariate_data %>%
  mutate(NumCases = log(NumCases + 1)) %>%
  select(-Present) %>%
  pivot_wider(names_from = "Year", values_from = "NumCases")

detection_history <- detection_history[order(detection_history$ADMcode), ]
numcase_history <- numcase_history[order(numcase_history$ADMcode), ]
shp <- shp[order(shp$ADMcode), ]

stopifnot(all(detection_history$ADMcode == numcase_history$ADMcode))
stopifnot(all(detection_history$ADMcode == shp$ADMcode))

detection_matrix <- as.matrix(detection_history[, -1])
umf <- unmarkedFrameOccu(
  y = detection_matrix,
  siteCovs = shp %>%
    st_drop_geometry() %>%
    select(ADMcode) %>% 
    mutate(across(everything(), scale)),
  obsCovs = list(logCases = numcase_history[, -1])
)

fm_null <- occu(~ 1 ~ 1, data = umf)
fm1 <- occu(~ 1 ~ PopulationDensity + InsuranceCoverage + WaterBodyProximity + TotalAnnualPrecipitation, data = umf)

fl <- fitList(null = fm_null, fm1 = fm1)
modSel(fl)

summary(fm1)

pred_occ <- predict(fm1, type = "state", newdata = shp %>%
                      st_drop_geometry() %>%
                      mutate(across(everything(), scale)))
shp$occ_prob <- pred_occ$Predicted

# Plot map
map_occ <- shp %>%
  ggplot() +
  geom_sf(aes(fill = occ_prob), color = NA) +
  theme_classic() +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  ggtitle("Predicted Occupancy")
print(map_occ)
