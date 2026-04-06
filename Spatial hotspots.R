# =========================================================
# SITE-WISE BOXPLOTS ONLY FOR PARAMETERS EXCEEDING BIS
# Okabe-Ito palette + dotted BIS exceedance lines
# =========================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
})

# -----------------------------
# 1. Load data
# -----------------------------
file_path <- file.choose()
df <- read_excel(file_path, sheet = "Sheet1")

# -----------------------------
# 2. Clean metadata
# -----------------------------
df <- df %>%
  mutate(
    Site_ID  = as.character(Site_ID),
    Stretch  = factor(as.character(Stretch),
                      levels = c("Upstream", "Midstream", "Downstream")),
    Season   = factor(as.character(Season),
                      levels = c("Pre-monsoon", "Monsoon", "Post-monsoon"))
  )

# -----------------------------
# 3. Order sites by stretch
# -----------------------------
site_order <- df %>%
  distinct(Site_ID, Stretch) %>%
  arrange(Stretch, Site_ID) %>%
  pull(Site_ID)

df$Site_ID <- factor(df$Site_ID, levels = site_order)

# -----------------------------
# 4. Define BIS standards
# -----------------------------
bis_limits <- tibble(
  Parameter = c("pH", "TDS", "EC", "Sodium", "Calcium", "Magnesium",
                "Fluoride", "Chloride", "Nitrate", "Sulphate",
                "Iron", "Zinc", "Lead"),
  BIS_value = c(8.5, 500, 400, 200, 75, 30,
                1.0, 250, 45, 200,
                0.3, 5, 0.01),
  BIS_lower = c(6.5, NA, NA, NA, NA, NA,
                NA, NA, NA, NA,
                NA, NA, NA)
)

# -----------------------------
# 5. Parameter labels
# -----------------------------
param_labels <- c(
  pH        = "pH",
  TDS       = "TDS (mg/L)",
  EC        = "EC",
  Sodium    = "Na+",
  Calcium   = "Ca2+",
  Magnesium = "Mg2+",
  Fluoride  = "F-",
  Chloride  = "Cl-",
  Nitrate   = "NO3-",
  Sulphate  = "SO4 2-",
  Iron      = "Fe",
  Zinc      = "Zn",
  Lead      = "Pb"
)

# -----------------------------
# 6. Keep only relevant columns
# -----------------------------
params_present <- intersect(bis_limits$Parameter, names(df))

plot_df <- df %>%
  select(Site_ID, Stretch, all_of(params_present)) %>%
  mutate(across(all_of(params_present), ~ suppressWarnings(as.numeric(.)))) %>%
  pivot_longer(
    cols = all_of(params_present),
    names_to = "Parameter",
    values_to = "Value"
  ) %>%
  left_join(bis_limits, by = "Parameter") %>%
  filter(!is.na(Value))

# -----------------------------
# 7. Keep only parameters exceeding BIS at least once
# -----------------------------
exceed_params <- plot_df %>%
  group_by(Parameter) %>%
  summarise(
    exceeded = if (first(Parameter) == "pH") {
      any(Value < first(BIS_lower) | Value > first(BIS_value), na.rm = TRUE)
    } else {
      any(Value > first(BIS_value), na.rm = TRUE)
    },
    .groups = "drop"
  ) %>%
  filter(exceeded) %>%
  pull(Parameter)

plot_df_exceed <- plot_df %>%
  filter(Parameter %in% exceed_params)

# -----------------------------
# 8. Okabe-Ito palette
# -----------------------------
okabe_ito <- c(
  "Upstream"   = "#E69F00",
  "Midstream"  = "#56B4E9",
  "Downstream" = "#009E73"
)

# -----------------------------
# 9. Create facet labels
# -----------------------------
facet_labs <- as_labeller(param_labels, default = label_value)

# -----------------------------
# 10. Base plot
# -----------------------------
p <- ggplot(plot_df_exceed, aes(x = Site_ID, y = Value, fill = Stretch)) +
  geom_boxplot(width = 0.65, outlier.shape = NA, alpha = 0.9) +
  geom_jitter(aes(color = Stretch),
              width = 0.15, size = 1.2, alpha = 0.8, show.legend = FALSE) +
  scale_fill_manual(values = okabe_ito, drop = FALSE) +
  scale_color_manual(values = okabe_ito, drop = FALSE) +
  facet_wrap(~ Parameter, scales = "free_y", ncol = 2, labeller = facet_labs) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "Arial"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "top",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  labs(
    x = "Site",
    y = "Concentration",
    fill = "Stretch"
  )

# -----------------------------
# 11. Add BIS dotted lines
# -----------------------------
p <- p +
  geom_hline(
    data = bis_limits %>% filter(Parameter %in% exceed_params, !is.na(BIS_value)),
    aes(yintercept = BIS_value),
    inherit.aes = FALSE,
    linetype = "dotted",
    linewidth = 0.6,
    color = "black"
  )

if ("pH" %in% exceed_params) {
  p <- p +
    geom_hline(
      data = bis_limits %>% filter(Parameter == "pH", !is.na(BIS_lower)),
      aes(yintercept = BIS_lower),
      inherit.aes = FALSE,
      linetype = "dotted",
      linewidth = 0.6,
      color = "black"
    )
}

# -----------------------------
# 12. Print plot
# -----------------------------
print(p)

# -----------------------------
# 13. Print exceeded parameters
# -----------------------------
print(exceed_params)
