# Figure 2

# Load libraries
library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)
library(gridExtra)
library(grid)
library(cowplot)
library(viridis)

wnv_summary <- covariate_data %>%
  group_by(ADMcode) %>%
  summarise(TotalDetections = sum(Present),
            YearsPresent = n()) %>%
  mutate(AvgOccupancy = TotalDetections / YearsPresent)

shp_occ <- shp %>%
  left_join(wnv_summary, by = "ADMcode")

map_core <- ggplot(shp_occ) +
  geom_sf(aes(fill = AvgOccupancy), color = NA) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    limits = c(0, 0.2),
    name = "Average Occupancy",
    guide = guide_colorbar(
      direction = "vertical",
      title.position = "top",
      title.hjust = 0.5,
      barheight = unit(2, "cm"),
      barwidth = unit(0.4, "cm"),
      label.theme = element_text(size = 7),
      title.theme = element_text(size = 8, face = "bold")
    )
  ) +
  theme_void(base_size = 28) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
  theme(legend.position = c(0.85, -0.3), # move legend to top-left corner
        legend.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(0, 0, 0, 0))

plot_title <- ggdraw() +
  draw_label("Average West Nile Virus\nOccupancy (2004-2020)", fontface = 'bold', size = 11, x = 0.5, y = 0.3, hjust = 0.5)

final_plot <- cowplot::plot_grid(
  plot_title,
  map_core,
  ncol = 1,
  rel_heights = c(0.1, 0.9)  # Closer title
)

print(final_plot)

cowplot::save_plot("WNV_Average_Occupancy_Map_FINAL_FIXED.pdf", plot = final_plot, base_width = 24, base_height = 18)

