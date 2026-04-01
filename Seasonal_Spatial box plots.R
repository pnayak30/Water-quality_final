Worked R Code: Significant Boxplots (PNG + SVG, mg/L units)
# ==========================================================
#  SIGNIFICANT boxplots (Season & Stretch) using EXACT headers
#  Units: mg/L (Water Research standard)
#  Outputs: PNG (600 dpi) + SVG (vector)
# ==========================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(rstatix)
})

# ---- Factor orders ----
season_levels  <- c("Pre-monsoon", "Monsoon", "Post-monsoon")
stretch_levels <- c("Upstream", "Midstream", "Downstream")
year_levels    <- c("2022", "2023", "2024")

# ---- Okabe–Ito palette ----
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00",
               "#CC79A7", "#999999")

# ---- Parameter groups ----
physicochem_params <- c("Temperature", "pH", "DO", "EC", "TDS")
cations_params     <- c("Lithium", "Sodium", "Ammonium", "Potassium", "Magnesium", "Calcium")
anions_params      <- c("Fluoride", "Chloride", "Nitrite", "Bromide", "Nitrate", "Phosphate", "Sulphate")
metals_params      <- c("Manganese", "Iron", "Cobalt", "Copper", "Zinc", "Lead")
all_params         <- c(physicochem_params, cations_params, anions_params, metals_params)

# ---- Axis labels (symbols + mg/L) ----
param_labels <- list(
  Temperature = expression("Temperature (°C)"),
  "pH"        = "pH",
  DO          = expression("DO (mg/L)"),
  EC          = expression("EC ("*mu*"S/cm)"),
  TDS         = expression("TDS (mg/L)"),
  
  Lithium   = expression("Li"^"+"*" (mg/L)"),
  Sodium    = expression("Na"^"+"*" (mg/L)"),
  Ammonium  = expression("NH"["4"]^"+"*" (mg/L)"),
  Potassium = expression("K"^"+"*" (mg/L)"),
  Magnesium = expression("Mg"^"2+"*" (mg/L)"),
  Calcium   = expression("Ca"^"2+"*" (mg/L)"),
  
  Fluoride  = expression("F"^"-"*" (mg/L)"),
  Chloride  = expression("Cl"^"-"*" (mg/L)"),
  Nitrite   = expression("NO"["2"]^"-"*" (mg/L)"),
  Bromide   = expression("Br"^"-"*" (mg/L)"),
  Nitrate   = expression("NO"["3"]^"-"*" (mg/L)"),
  Phosphate = expression("PO"["4"]^"3-"*" (mg/L)"),
  Sulphate  = expression("SO"["4"]^"2-"*" (mg/L)"),
  
  Manganese = expression("Mn (mg/L)"),
  Iron      = expression("Fe (mg/L)"),
  Cobalt    = expression("Co (mg/L)"),
  Copper    = expression("Cu (mg/L)"),
  Zinc      = expression("Zn (mg/L)"),
  Lead      = expression("Pb (mg/L)")
)

# ---------- Load data ----------
file_path <- file.choose()
dat <- read_excel(file_path, sheet = 1)

# Factor orders
dat <- dat %>%
  mutate(
    Season  = factor(Season,  levels = season_levels),
    Stretch = factor(Stretch, levels = stretch_levels),
    Year    = factor(as.character(Year), levels = year_levels)
  )

# Ensure numeric columns
num_present <- intersect(all_params, names(dat))
dat <- dat %>%
  mutate(across(all_of(num_present), ~ suppressWarnings(as.numeric(.))))

# ---------- Helper functions ----------
find_significant <- function(df, group_col, params) {
  res <- purrr::map_df(params, function(p) {
    if (!p %in% names(df)) return(NULL)
    tmp <- df %>% select(all_of(c(group_col, p))) %>% drop_na()
    if (nrow(tmp) < 3 || n_distinct(tmp[[group_col]]) < 2) return(NULL)
    kt <- rstatix::kruskal_test(tmp, as.formula(paste(p, "~", group_col)))
    tibble(Parameter = p, Pvalue = kt$p)
  })
  if (nrow(res) == 0) return(character(0))
  res %>% filter(Pvalue < 0.05) %>% pull(Parameter)
}

make_box <- function(df, group_col, param, out_dir) {
  ylab <- if (!is.null(param_labels[[param]])) param_labels[[param]] else param
  g <- ggplot(df, aes(x = .data[[group_col]], y = .data[[param]], fill = .data[[group_col]])) +
    geom_boxplot(width = 0.6, outlier.shape = NA, alpha = 0.9) +
    geom_jitter(width = 0.15, alpha = 0.6, size = 1.2) +
    scale_fill_manual(values = okabe_ito) +
    theme_classic(base_size = 12) +
    theme(
      axis.text = element_text(color = "black"),
      axis.title.x = element_blank(),
      axis.title.y = element_text(face = "bold"),
      text = element_text(family = "Arial")
    ) +
    labs(y = ylab)
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  # --- Save both PNG and SVG ---
  ggsave(file.path(out_dir, paste0(param, "_", group_col, ".png")),
         g, width = 6.9, height = 4.7, dpi = 600)
  ggsave(file.path(out_dir, paste0(param, "_", group_col, ".svg")),
         g, width = 6.9, height = 4.7, device = "svg")
}

run_block <- function(df, params, category, group_col, year_tag) {
  pp  <- intersect(params, names(df))
  sig <- find_significant(df, group_col, pp)
  message("(", group_col, " | ", category, " | ", year_tag, ") Significant: ",
          ifelse(length(sig)==0, "None", paste(sig, collapse = ", ")))
  if (length(sig) == 0) return()
  out_dir <- file.path("Boxplots_Significant", group_col, year_tag, category)
  for (p in sig) make_box(df, group_col, p, out_dir)
}

# ---------- Run pooled + year-wise ----------
category_list <- list(
  Physicochemical = physicochem_params,
  Cations         = cations_params,
  Anions          = anions_params,
  Metals          = metals_params
)

# Pooled
for (cat in names(category_list)) {
  run_block(dat, category_list[[cat]], cat, "Season",  "Pooled")
  run_blockWorked R Code: Significant Boxplots (PNG + SVG, mg/L units)
  # ==========================================================
  #  SIGNIFICANT boxplots (Season & Stretch) using EXACT headers
  #  Units: mg/L (Water Research standard)
  #  Outputs: PNG (600 dpi) + SVG (vector)
  # ==========================================================
  
  suppressPackageStartupMessages({
    library(tidyverse)
    library(readxl)
    library(rstatix)
  })
  
  # ---- Factor orders ----
  season_levels  <- c("Pre-monsoon", "Monsoon", "Post-monsoon")
  stretch_levels <- c("Upstream", "Midstream", "Downstream")
  year_levels    <- c("2022", "2023", "2024")
  
  # ---- Okabe–Ito palette ----
  okabe_ito <- c("#E69F00", "#56B4E9", "#009E73",
                 "#F0E442", "#0072B2", "#D55E00",
                 "#CC79A7", "#999999")
  
  # ---- Parameter groups ----
  physicochem_params <- c("Temperature", "pH", "DO", "EC", "TDS")
  cations_params     <- c("Lithium", "Sodium", "Ammonium", "Potassium", "Magnesium", "Calcium")
  anions_params      <- c("Fluoride", "Chloride", "Nitrite", "Bromide", "Nitrate", "Phosphate", "Sulphate")
  metals_params      <- c("Manganese", "Iron", "Cobalt", "Copper", "Zinc", "Lead")
  all_params         <- c(physicochem_params, cations_params, anions_params, metals_params)
  
  # ---- Axis labels (symbols + mg/L) ----
  param_labels <- list(
    Temperature = expression("Temperature (°C)"),
    "pH"        = "pH",
    DO          = expression("DO (mg/L)"),
    EC          = expression("EC ("*mu*"S/cm)"),
    TDS         = expression("TDS (mg/L)"),
    
    Lithium   = expression("Li"^"+"*" (mg/L)"),
    Sodium    = expression("Na"^"+"*" (mg/L)"),
    Ammonium  = expression("NH"["4"]^"+"*" (mg/L)"),
    Potassium = expression("K"^"+"*" (mg/L)"),
    Magnesium = expression("Mg"^"2+"*" (mg/L)"),
    Calcium   = expression("Ca"^"2+"*" (mg/L)"),
    
    Fluoride  = expression("F"^"-"*" (mg/L)"),
    Chloride  = expression("Cl"^"-"*" (mg/L)"),
    Nitrite   = expression("NO"["2"]^"-"*" (mg/L)"),
    Bromide   = expression("Br"^"-"*" (mg/L)"),
    Nitrate   = expression("NO"["3"]^"-"*" (mg/L)"),
    Phosphate = expression("PO"["4"]^"3-"*" (mg/L)"),
    Sulphate  = expression("SO"["4"]^"2-"*" (mg/L)"),
    
    Manganese = expression("Mn (mg/L)"),
    Iron      = expression("Fe (mg/L)"),
    Cobalt    = expression("Co (mg/L)"),
    Copper    = expression("Cu (mg/L)"),
    Zinc      = expression("Zn (mg/L)"),
    Lead      = expression("Pb (mg/L)")
  )
  
  # ---------- Load data ----------
  file_path <- file.choose()
  dat <- read_excel(file_path, sheet = 1)
  
  # Factor orders
  dat <- dat %>%
    mutate(
      Season  = factor(Season,  levels = season_levels),
      Stretch = factor(Stretch, levels = stretch_levels),
      Year    = factor(as.character(Year), levels = year_levels)
    )
  
  # Ensure numeric columns
  num_present <- intersect(all_params, names(dat))
  dat <- dat %>%
    mutate(across(all_of(num_present), ~ suppressWarnings(as.numeric(.))))
  
  # ---------- Helper functions ----------
  find_significant <- function(df, group_col, params) {
    res <- purrr::map_df(params, function(p) {
      if (!p %in% names(df)) return(NULL)
      tmp <- df %>% select(all_of(c(group_col, p))) %>% drop_na()
      if (nrow(tmp) < 3 || n_distinct(tmp[[group_col]]) < 2) return(NULL)
      kt <- rstatix::kruskal_test(tmp, as.formula(paste(p, "~", group_col)))
      tibble(Parameter = p, Pvalue = kt$p)
    })
    if (nrow(res) == 0) return(character(0))
    res %>% filter(Pvalue < 0.05) %>% pull(Parameter)
  }
  
  make_box <- function(df, group_col, param, out_dir) {
    ylab <- if (!is.null(param_labels[[param]])) param_labels[[param]] else param
    g <- ggplot(df, aes(x = .data[[group_col]], y = .data[[param]], fill = .data[[group_col]])) +
      geom_boxplot(width = 0.6, outlier.shape = NA, alpha = 0.9) +
      geom_jitter(width = 0.15, alpha = 0.6, size = 1.2) +
      scale_fill_manual(values = okabe_ito) +
      theme_classic(base_size = 12) +
      theme(
        axis.text = element_text(color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold"),
        text = element_text(family = "Arial")
      ) +
      labs(y = ylab)
    
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    # --- Save both PNG and SVG ---
    ggsave(file.path(out_dir, paste0(param, "_", group_col, ".png")),
           g, width = 6.9, height = 4.7, dpi = 600)
    ggsave(file.path(out_dir, paste0(param, "_", group_col, ".svg")),
           g, width = 6.9, height = 4.7, device = "svg")
  }
  
  run_block <- function(df, params, category, group_col, year_tag) {
    pp  <- intersect(params, names(df))
    sig <- find_significant(df, group_col, pp)
    message("(", group_col, " | ", category, " | ", year_tag, ") Significant: ",
            ifelse(length(sig)==0, "None", paste(sig, collapse = ", ")))
    if (length(sig) == 0) return()
    out_dir <- file.path("Boxplots_Significant", group_col, year_tag, category)
    for (p in sig) make_box(df, group_col, p, out_dir)
  }
  
  # ---------- Run pooled + year-wise ----------
  category_list <- list(
    Physicochemical = physicochem_params,
    Cations         = cations_params,
    Anions          = anions_params,
    Metals          = metals_params
  )
  
  # Pooled
  for (cat in names(category_list)) {
    run_block(dat, category_list[[cat]], cat, "Season",  "Pooled")
    run_block(dat, category_list[[cat]], cat, "Stretch", "Pooled")
  }
  
  # Year-wise
  for (yr in year_levels) {
    dy <- dat %>% filter(!is.na(Year), Year == yr)
    if (nrow(dy) == 0) next
    for (cat in names(category_list)) {
      run_block(dy, category_list[[cat]], cat, "Season",  yr)
      run_block(dy, category_list[[cat]], cat, "Stretch", yr)
    }
  }
  
  message("\n✅ Done. PNG + SVG files saved in 'Boxplots_Significant/' (mg/L units, Water Research formatting).")
  
  (dat, category_list[[cat]], cat, "Stretch", "Pooled")
}

# Year-wise
for (yr in year_levels) {
  dy <- dat %>% filter(!is.na(Year), Year == yr)
  if (nrow(dy) == 0) next
  for (cat in names(category_list)) {
    run_block(dy, category_list[[cat]], cat, "Season",  yr)
    run_block(dy, category_list[[cat]], cat, "Stretch", yr)
  }
}

message("\n✅ Done. PNG + SVG files saved in 'Boxplots_Significant/' (mg/L units, Water Research formatting).")

