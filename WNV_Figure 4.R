# Figure 4

install.packages("biscale")

# Load libraries
library(unmarked)
library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)
library(readr)
library(viridis)
library(patchwork)
library(biscale)

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

arbonet_data <- read_csv("/Users/torrelavelle/Dropbox/arbo_occupancy/spillovers_arbonet.csv") %>%
  mutate(ADMcode = as.numeric(ADMcode))

# Create dataset
full_data <- expand.grid(ADMcode = unique(shp$ADMcode), Year = 2004:2020) %>%
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

site_covs <- covariate_data %>%
  group_by(ADMcode) %>%
  summarise(PopDensity = mean(PopulationDensity, na.rm = TRUE),
            Insurance = mean(InsuranceCoverage, na.rm = TRUE),
            Water = mean(WaterBodyProximity, na.rm = TRUE),
            Precip = mean(TotalAnnualPrecipitation, na.rm = TRUE),
            .groups = 'drop') %>%
  mutate(across(-ADMcode, scale))

covariate_data <- covariate_data %>% mutate(logCases = log(NumCases + 1))

covariate_data <- covariate_data %>%
  group_by(ADMcode, Year) %>%
  summarise(Present = max(Present, na.rm = TRUE),
            logCases = max(logCases, na.rm = TRUE),
            .groups = 'drop')

detection_history <- covariate_data %>%
  pivot_wider(names_from = "Year", values_from = "Present", values_fill = 0)

numcase_history <- covariate_data %>%
  pivot_wider(names_from = "Year", values_from = "logCases", values_fill = 0)

detection_history <- detection_history[order(detection_history$ADMcode), ]
numcase_history <- numcase_history[order(numcase_history$ADMcode), ]
shp <- shp[order(shp$ADMcode), ]

common_adm <- Reduce(intersect, list(detection_history$ADMcode, numcase_history$ADMcode, site_covs$ADMcode))
detection_history <- detection_history %>% filter(ADMcode %in% common_adm)
numcase_history <- numcase_history %>% filter(ADMcode %in% common_adm)
site_covs <- site_covs %>% filter(ADMcode %in% common_adm)
shp <- shp %>% filter(ADMcode %in% common_adm)

num_years <- ncol(detection_history) - 1  # subtract ADMcode column
obs_matrix <- as.matrix(numcase_history[, -1])
obs_long <- as.vector(t(obs_matrix))  # stack row-wise

stopifnot(nrow(site_covs) * num_years == length(obs_long))

umf <- unmarkedFrameOccu(
  y = as.matrix(detection_history[, -1]),
  siteCovs = site_covs %>% select(-ADMcode),
  obsCovs = data.frame(logCases = obs_long)
)

fm_full <- occu(~ logCases ~ PopDensity + Insurance + Water + Precip, data = umf)

pred_occ <- predict(fm_full, type = "state")
pred_det <- predict(fm_full, type = "det")

shp$occ_prob <- pred_occ$Predicted
shp$det_prob <- pred_det$Predicted

# Classify into high/low categories
occ_thresh <- median(shp$occ_prob, na.rm = TRUE)
det_thresh <- median(shp$det_prob, na.rm = TRUE)

shp <- shp %>%
  mutate(occ_class = ifelse(occ_prob >= occ_thresh, "HighOcc", "LowOcc"),
         det_class = ifelse(det_prob >= det_thresh, "HighDet", "LowDet"),
         bivar_class = paste0(occ_class, "_", det_class))

# Choose bivariate color scale
bivar_colors <- c("HighOcc_HighDet" = "#762a83",
                  "HighOcc_LowDet" = "#af8dc3",
                  "LowOcc_HighDet" = "#7fbf7b",
                  "LowOcc_LowDet" = "#1b7837")

# Plot bivariate map
bivar_map <- ggplot(shp) +
  geom_sf(aes(fill = bivar_class), color = NA) +
  scale_fill_manual(values = bivar_colors, name = "Bivariate Class") +
  theme_classic() +
  ggtitle("Bivariate Map: Occupancy vs Detection")

legend_plot <- bi_legend(pal = "GrPink", dim = 2, 
                         xlab = "Higher Occupancy", 
                         ylab = "Higher Detection", 
                         size = 8)

final_plot <- bivar_map + inset_element(legend_plot, left = 0.65, bottom = 0.1, right = 0.95, top = 0.4)

print(final_plot)
