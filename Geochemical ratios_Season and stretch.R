# Geochemical ratios vs TDS (Season & Stretch)
# mg/L ratios: Na+/Cl-, Mg2+/Ca2+, Na+/Ca2+
# meq/L ratio: (Na+ + K+)/TZ+
# TZ+ = Na+ + K+ + Ca2+ + Mg2+ (meq/L)

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
})

# ---- Load data ----
file_path <- file.choose()
df <- read_excel(file_path, sheet = "Sheet1")

# ---- Required columns ----
req <- c("TDS", "Sodium", "Chloride", "Magnesium",
         "Calcium", "Potassium", "Season", "Stretch")
miss <- setdiff(req, names(df))
if (length(miss) > 0) {
  stop("Missing columns in Sheet1: ", paste(miss, collapse = ", "))
}

# ---- Factor ordering ----
df <- df %>%
  mutate(
    Season  = factor(Season,  levels = c("Pre-monsoon", "Monsoon", "Post-monsoon")),
    Stretch = factor(Stretch, levels = c("Upstream", "Midstream", "Downstream"))
  )

# ---- Helper functions ----
safe_div <- function(num, den) ifelse(is.na(den) | den == 0, NA_real_, num / den)

# Equivalent weights (g/eq) for mg/L -> meq/L
eqw <- list(
  Na = 22.989,
  K  = 39.098,
  Ca = 40.078 / 2,
  Mg = 24.305 / 2
)

to_meq <- function(mgL, eqw) ifelse(is.na(mgL), NA_real_, mgL / eqw)

# ---- Compute ratios ----
df <- df %>%
  mutate(
    Ratio_Na_Cl = safe_div(Sodium,   Chloride),
    Ratio_Mg_Ca = safe_div(Magnesium, Calcium),
    Ratio_Na_Ca = safe_div(Sodium,   Calcium),
    
    Na_meq = to_meq(Sodium,   eqw$Na),
    K_meq  = to_meq(Potassium, eqw$K),
    Ca_meq = to_meq(Calcium,   eqw$Ca),
    Mg_meq = to_meq(Magnesium, eqw$Mg),
    
    TZpos  = Na_meq + K_meq + Ca_meq + Mg_meq,
    Ratio_NaK_TZ = safe_div(Na_meq + K_meq, TZpos)
  )

# ---- Okabe–Ito palette ----
okabe_season  <- c(
  "Pre-monsoon" = "#E69F00",
  "Monsoon"     = "#56B4E9",
  "Post-monsoon"= "#009E73"
)

okabe_stretch <- c(
  "Upstream"  = "#E69F00",
  "Midstream" = "#56B4E9",
  "Downstream"= "#009E73"
)

# ---- Global theme ----
theme_set(
  theme_classic(base_size = 12) +
    theme(
      text = element_text(family = "Arial"),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      plot.margin = margin(6, 6, 6, 6)
    )
)

# ---- Plot function ----
make_singlepanel <- function(data, ratio_col, ylab_expr,
                             legend_var = c("Season", "Stretch"),
                             out_stub,
                             width = 6.2, height = 4.5, dpi = 600) {
  
  legend_var <- match.arg(legend_var)
  aes_col <- if (legend_var == "Season") "Season" else "Stretch"
  pal     <- if (legend_var == "Season") okabe_season else okabe_stretch
  
  p <- ggplot(data, aes(x = TDS, y = .data[[ratio_col]], color = .data[[aes_col]])) +
    geom_point(size = 2, alpha = 0.85) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 1.0) +
    scale_color_manual(values = pal) +
    labs(
      x = expression("TDS (mg/L)"),
      y = ylab_expr,
      color = legend_var
    ) +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.08)))
  
  ggsave(paste0(out_stub, "_", legend_var, ".svg"),
         p, width = width, height = height, dpi = dpi)
  ggsave(paste0(out_stub, "_", legend_var, ".png"),
         p, width = width, height = height, dpi = dpi)
  
  p
}

# ---- Generate plots ----

# Na+/Cl-
p_NaCl_season  <- make_singlepanel(
  df, "Ratio_Na_Cl",
  expression(Na^"+" / Cl^"-"),
  "Season",
  "Ratio_NaCl_vs_TDS"
)

p_NaCl_stretch <- make_singlepanel(
  df, "Ratio_Na_Cl",
  expression(Na^"+" / Cl^"-"),
  "Stretch",
  "Ratio_NaCl_vs_TDS"
)

# Mg2+/Ca2+
p_MgCa_season  <- make_singlepanel(
  df, "Ratio_Mg_Ca",
  expression(Mg^"2+" / Ca^"2+"),
  "Season",
  "Ratio_MgCa_vs_TDS"
)

p_MgCa_stretch <- make_singlepanel(
  df, "Ratio_Mg_Ca",
  expression(Mg^"2+" / Ca^"2+"),
  "Stretch",
  "Ratio_MgCa_vs_TDS"
)

# Na+/Ca2+
p_NaCa_season  <- make_singlepanel(
  df, "Ratio_Na_Ca",
  expression(Na^"+" / Ca^"2+"),
  "Season",
  "Ratio_NaCa_vs_TDS"
)

p_NaCa_stretch <- make_singlepanel(
  df, "Ratio_Na_Ca",
  expression(Na^"+" / Ca^"2+"),
  "Stretch",
  "Ratio_NaCa_vs_TDS"
)

# (Na+ + K+)/TZ+
p_NaK_TZ_season <- make_singlepanel(
  df, "Ratio_NaK_TZ",
  expression((Na^"+" + K^"+") / TZ^"+"),
  "Season",
  "Ratio_NaK_over_TZ_vs_TDS"
)

p_NaK_TZ_stretch <- make_singlepanel(
  df, "Ratio_NaK_TZ",
  expression((Na^"+" + K^"+") / TZ^"+"),
  "Stretch",
  "Ratio_NaK_over_TZ_vs_TDS"
)
