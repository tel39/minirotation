# Figure 3

# Load libraries
library(ggplot2)
library(readr)
library(dplyr)
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

adjusted_time_series_plot <- function(data, value_column, title, fill_label, title_hjust = 0.5, reduce_y_axis = FALSE) {
  p <- ggplot(data, aes(x = Year, y = as.factor(ADMcode), fill = !!sym(value_column))) +
    geom_tile() +
    scale_fill_viridis_c(option = "plasma", name = fill_label) +
    theme_classic(base_size = 14) +
    labs(title = title, x = "Year", y = "County FIPS") +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = title_hjust),
      axis.ticks.y = element_blank(),
      legend.position = "right"
    )
  
  if (reduce_y_axis) {
    p <- p + scale_y_discrete(breaks = scales::pretty_breaks(n = 5))
  } else {
    p <- p + theme(axis.text.y = element_blank())
  }
  
  return(p)
}

p1 <- adjusted_time_series_plot(population_density, "PopulationDensity", "Population Density Over Time", "Population Density
(people per sq mi)", title_hjust = 0.1)
print(p1)

p2 <- adjusted_time_series_plot(insurance_coverage, "InsuranceCoverage", "Healthcare Access Over Time", "Insurance Coverage
(percent)", title_hjust = 0.1)
print(p2)

p3 <- adjusted_time_series_plot(stagnant_water_proximity, "WaterBodyProximity", "Proximity to Stagnant Water
Over Time", "Water Body Proximity
(index)", title_hjust = 0.1, reduce_y_axis = TRUE)
print(p3)

p4 <- adjusted_time_series_plot(precipitation_data, "TotalAnnualPrecipitation", "Precipitation Over Time", "Precipitation
(mm per year)", title_hjust = 0.5)
print(p4)
