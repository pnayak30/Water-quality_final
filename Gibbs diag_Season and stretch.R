
# ==========================================================
# Gibbs diagram: Na⁺/(Na⁺ + Ca²⁺) vs TDS
# Interactive CSV selection for zones + dashed boundaries
# ==========================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(scales)
})

# ---------- 1) Load main data ----------
excel_path <- file.choose()  # Choose your Excel with Sheet1
df <- read_excel(excel_path, sheet = "Sheet1") %>%
  mutate(
    Season  = factor(Season,  levels = c("Pre-monsoon","Monsoon","Post-monsoon")),
    Stretch = factor(Stretch, levels = c("Upstream","Midstream","Downstream")),
    Na_ratio = Sodium / (Sodium + Calcium)
  ) %>%
  filter(is.finite(Na_ratio), is.finite(TDS))

# ---------- 2) Choose and load zone polygons ----------
message("Select the Precipitation-dominance zone CSV"); precip_csv <- file.choose()
message("Select the Rock-dominance zone CSV");         rock_csv   <- file.choose()
message("Select the Evaporation-dominance zone CSV");  sea_csv    <- file.choose()

read_zone <- function(path) {
  z <- readr::read_csv(path, show_col_types = FALSE)
  num_cols <- which(vapply(z, is.numeric, TRUE))
  stopifnot(length(num_cols) >= 2)
  tibble(x = z[[num_cols[1]]], y = z[[num_cols[2]]])
}

precip <- read_zone(precip_csv)
rock   <- read_zone(rock_csv)
sea    <- read_zone(sea_csv)

# ---------- 3) Styling ----------
okabe_stretch <- c("Upstream"="#E69F00","Midstream"="#56B4E9","Downstream"="#009E73")
okabe_season  <- c("Pre-monsoon"="#E69F00","Monsoon"="#56B4E9","Post-monsoon"="#009E73")

theme_gibbs <- theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "Arial"),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid = element_blank()
  )

# Label positions (tweak if needed for your polygons)
lab_precip <- list(x = 0.08, y = 30,  txt = "Precipitation\ndominance")
lab_rock   <- list(x = 0.25, y = 120, txt = "Rock\ndominance")
lab_sea    <- list(x = 0.78, y = 300, txt = "Evaporation\ndominance")

zone_polys <- list(
  geom_polygon(data = precip, aes(x, y),
               fill = "#F4F4F4", alpha = 0.6,
               color = "black", linetype = "dashed", linewidth = 0.5),
  geom_polygon(data = rock,   aes(x, y),
               fill = "#E9E9E9", alpha = 0.6,
               color = "black", linetype = "dashed", linewidth = 0.5),
  geom_polygon(data = sea,    aes(x, y),
               fill = "#DFDFDF", alpha = 0.6,
               color = "black", linetype = "dashed", linewidth = 0.5)
)

zone_labels <- list(
  annotate("text", x = lab_rock$x,   y = lab_rock$y,   label = lab_rock$txt,   fontface="bold"),
  annotate("text", x = lab_sea$x,    y = lab_sea$y,    label = lab_sea$txt,    fontface="bold"),
  annotate("text", x = lab_precip$x, y = lab_precip$y, label = lab_precip$txt, fontface="bold")
)

# ---------- 4) Plot by STRETCH ----------
p_stretch <- ggplot() +
  zone_polys +
  geom_point(data = df, aes(x = Na_ratio, y = TDS, color = Stretch), size = 2.2, alpha = 0.9) +
  scale_color_manual(values = okabe_stretch) +
  scale_y_log10(labels = comma_format()) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(x = expression("Na"^'+' / ("Na"^'+' + "Ca"^'2+')),
       y = expression("TDS (mg/L)"),
       color = "Stretch") +
  zone_labels +
  theme_gibbs

ggsave("Gibbs_Stretch.svg", p_stretch, width = 6.9, height = 4.7, dpi = 600)
ggsave("Gibbs_Stretch.png", p_stretch, width = 6.9, height = 4.7, dpi = 600)

# ---------- 5) Plot by SEASON ----------
p_season <- ggplot() +
  zone_polys +
  geom_point(data = df, aes(x = Na_ratio, y = TDS, color = Season, shape = Season), size = 2.2, alpha = 0.9) +
  scale_color_manual(values = okabe_season) +
  scale_y_log10(labels = comma_format()) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(x = expression("Na"^'+' / ("Na"^'+' + "Ca"^'2+')),
       y = expression("TDS (mg/L)"),
       color = "Season", shape = "Season") +
  zone_labels +
  theme_gibbs

ggsave("Gibbs_Season.svg", p_season, width = 6.9, height = 4.7, dpi = 600)
ggsave("Gibbs_Season.png", p_season, width = 6.9, height = 4.7, dpi = 600)

