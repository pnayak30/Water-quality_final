library(terra)
library(tidyverse)
library(lubridate)
library(readxl)

# ----------------------- 1) Load site coordinates -----------------------
site_file <- file.choose()
sites <- read_excel(site_file, sheet = "MMRB_sites")

# Convert to spatial object
sites_vect <- vect(
  sites,
  geom = c("Longitude", "Latitude"),
  crs = "EPSG:4326"
)

# ----------------------- 2) Load CHIRPS NetCDF -----------------------
rain_file <- file.choose()
rain <- rast(rain_file)

# ----------------------- 3) Extract rainfall at sites ------------------
rain_sites <- extract(rain, sites_vect)
rain_df <- cbind(sites, rain_sites)

# ----------------------- 4) Reshape to long format ---------------------
rain_long <- rain_df %>%
  pivot_longer(
    cols = starts_with("precip"),
    names_to = "layer",
    values_to = "rain_mm"
  ) %>%
  mutate(
    date  = time(rain)[as.integer(gsub("precip_", "", layer))],
    Year  = year(date),
    Month = month(date, label = TRUE, abbr = TRUE)
  ) %>%
  filter(Year %in% c(2021, 2022, 2023, 2024))

# ----------------------- 5) Order months correctly ---------------------
rain_long <- rain_long %>%
  mutate(
    Month = factor(
      Month,
      levels = c("Jan","Feb","Mar","Apr","May","Jun",
                 "Jul","Aug","Sep","Oct","Nov","Dec")
    )
  )

# ----------------------- 6) Monthly rainfall box plots -----------------
ggplot(rain_long, aes(x = Month, y = rain_mm)) +
  geom_boxplot(
    fill = "#56B4E9",
    colour = "black",
    outlier.size = 0.8
  ) +
  facet_wrap(~ Year, ncol = 2) +
  labs(
    x = "Month",
    y = "Monthly rainfall (mm)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.title   = element_text(face = "bold"),
    panel.border = element_rect(colour = "black")
  )

# ============================================================
# End of script
# ============================================================

