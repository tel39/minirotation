# Figure 4

# Load libraries
library(unmarked)
library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)
library(readr)
library(viridis)

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

# Load shapefile and ArboNET data
shapefile_path <- "/Users/torrelavelle/Dropbox/arbo_occupancy/usa_counties.shp"
shp <- st_read(shapefile_path) %>%
  mutate(ADMcode = as.numeric(ADMcode)) %>%
  filter(!STATEFP %in% c("02", "15"))

arbonet_data <- read.csv("/Users/torrelavelle/Dropbox/arbo_occupancy/spillovers_arbonet.csv")

# Create full dataset
full_data <- expand.grid(unique(shp$ADMcode), 2004:2020) %>%
  rename(ADMcode = Var1, Year = Var2) %>%
  left_join(arbonet_data %>% filter(Disease == "West Nile virus disease", Presentation == "All cases") %>% select(ADMcode, Year, NumCases),
            by = c("ADMcode", "Year")) %>%
  mutate(NumCases = replace_na(NumCases, 0),
         Present = ifelse(NumCases > 0, 1, 0))

# Merge covariates
covariate_data <- full_data %>%
  left_join(population_density, by = c("ADMcode", "Year")) %>%
  left_join(insurance_coverage, by = c("ADMcode", "Year")) %>%
  left_join(stagnant_water_proximity, by = c("ADMcode", "Year")) %>%
  left_join(precipitation_data, by = c("ADMcode", "Year"))

heatmap_detection <- ggplot(covariate_data) +
  geom_tile(aes(x = Year, y = factor(ADMcode), fill = factor(Present))) +
  theme_classic() +
  scale_fill_viridis_d(name = "Detection") +
  ggtitle("WNV Detection Heatmap")
print(heatmap_detection)

heatmap_cases <- ggplot(covariate_data) +
  geom_tile(aes(x = Year, y = factor(ADMcode), fill = log(NumCases + 1))) +
  theme_classic() +
  scale_fill_viridis_c(name = "Log(Cases + 1)") +
  ggtitle("WNV Case Heatmap")
print(heatmap_cases)

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

detection_matrix <- as.matrix(detection_history[, -1])
site_covs <- covariate_data %>% 
  group_by(ADMcode) %>% 
  summarise(PopDensity = mean(PopulationDensity, na.rm = TRUE),
            Insurance = mean(InsuranceCoverage, na.rm = TRUE),
            Water = mean(WaterBodyProximity, na.rm = TRUE),
            Precip = mean(TotalAnnualPrecipitation, na.rm = TRUE)) %>%
  mutate(across(-ADMcode, scale))

umf <- unmarkedFrameOccu(
  y = detection_matrix,
  siteCovs = site_covs %>% select(-ADMcode),
  obsCovs = list(logCases = as.matrix(numcase_history[, -1]))
)

fm_full <- occu(~ logCases ~ PopDensity + Insurance + Water + Precip, data = umf)
fm_null <- occu(~ 1 ~ 1, data = umf)

model_list <- fitList(full = fm_full, null = fm_null)
print(modSel(model_list))

print(summary(fm_full))

pred_occ <- predict(fm_full, type = "state")
shp$occ_prob <- pred_occ$Predicted

occ_map <- ggplot(shp) +
  geom_sf(aes(fill = occ_prob), color = NA) +
  scale_fill_viridis_c(option = "plasma", name = "Occupancy") +
  theme_classic() +
  ggtitle("Predicted Occupancy from Full Model")
print(occ_map)
