# ============================================================
# 1) WQI Excel (Assigned-weights; DO special; new 5-class scheme)
# ============================================================
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(openxlsx)
})

# ---------- 1) Load your data ----------
# Choose your input file (must contain columns for the parameters + IDs)
file_path <- file.choose()
df <- read_excel(file_path, sheet = 1) %>% clean_names()

# ---------- 2) Define standards (Si), ideal (Ii), and assigned weights (wi) ----------
Si <- c(           # standards / permissible limits
  do = 7,          # mg/L
  ec = 400,        # µS/cm
  ca = 75,         # mg/L
  mg = 30,         # mg/L
  k  = 12,         # mg/L
  na = 200,        # mg/L
  fe = 0.3,        # mg/L
  zn = 5,          # mg/L
  pb = 0.01,       # mg/L
  cl = 250         # mg/L
)
Ii <- c(do = 14.6) # ideal for DO (mg/L)

wi_raw <- c(       # assigned raw weights (Sahu & Sikdar 2008)
  do = 4, ec = 3, ca = 2, mg = 2, k = 2, na = 2, fe = 3, zn = 2, pb = 5, cl = 3
)
Wi <- wi_raw / sum(wi_raw)  # normalized weights

# ---------- 3) Map columns robustly ----------
aliases <- list(
  do = c("do","d_o","dissolved_oxygen"),
  ec = c("ec","conductivity","electrical_conductivity"),
  ca = c("ca","calcium","ca2","ca2_plus"),
  mg = c("mg","magnesium","mg2","mg2_plus"),
  k  = c("k","potassium","k_plus"),
  na = c("na","sodium","na_plus"),
  fe = c("fe","iron"),
  zn = c("zn","zinc"),
  pb = c("pb","lead"),
  cl = c("cl","chloride","cl_minus")
)

find_col <- function(df_names, cands) {
  cand <- tolower(cands)
  hit  <- intersect(df_names, cand)
  if (length(hit)) hit[1] else NA_character_
}

dfn <- names(df)
param_cols <- setNames(rep(NA_character_, length(aliases)), names(aliases))
for (p in names(aliases)) {
  param_cols[p] <- find_col(dfn, aliases[[p]])
}

present <- names(param_cols)[!is.na(param_cols)]
missing <- setdiff(names(Si), present)
if (length(missing)) {
  message("⚠️ Missing parameters in data (skipped): ", paste(missing, collapse = ", "))
}

params <- intersect(names(Si), present)
stopifnot(length(params) > 0)

Si_use <- Si[params]
Wi_use <- Wi[params]

# ---------- 4) Build numeric matrix of parameter values ----------
X <- df %>%
  transmute(across(all_of(unname(param_cols[params])), as.numeric)) %>%
  as.matrix()
colnames(X) <- params

# ---------- 5) Compute qi ----------
qi <- matrix(
  NA_real_,
  nrow = nrow(X),
  ncol = ncol(X),
  dimnames = list(NULL, params)
)

for (j in seq_along(params)) {
  p  <- params[j]
  Ci <- X[, j]
  Sj <- Si_use[[p]]
  if (p == "do") {
    Ij <- Ii[["do"]]
    qi[, j] <- 100 * (Ij - Ci) / (Ij - Sj)      # DO: inverse (higher is better)
  } else {
    qi[, j] <- (Ci / Sj) * 100                  # others: direct (higher is worse)
  }
}
qi[qi < 0] <- 0  # cap negatives (e.g., super-saturated DO)

# ---------- 6) Compute SIi and WQI (missing-aware renormalization) ----------
Wi_vec <- as.numeric(Wi_use)
names(Wi_vec) <- names(Wi_use)

SI <- sweep(qi, 2, Wi_vec, `*`)   # SIi = Wi * qi (pre-renorm for reporting)

# Recompute WQI per sample using only non-NA parameters (renormalize Wi over available ones)
WQI <- apply(qi, 1, function(qrow) {
  ok <- !is.na(qrow)
  if (!any(ok)) return(NA_real_)
  Wi_v <- Wi_vec[ok]
  Wi_v <- Wi_v / sum(Wi_v)
  sum(Wi_v * qrow[ok])
})

# ---------- 7) Classify (new 5-class scheme) ----------
classify_wqi <- function(x) {
  cut(
    x,
    breaks = c(-Inf, 25, 50, 75, 100, Inf),
    labels = c("Excellent","Good","Poor","Very Poor","Unsuitable for drinking"),
    right = TRUE,
    ordered_result = TRUE
  )
}
WQI_class <- classify_wqi(WQI)

# ---------- 8) Assemble output table ----------
id_cols <- intersect(
  c("sample_id","site_id","stretch","season","year","river","site"),
  names(df)
)

out <- tibble::as_tibble(df[, id_cols, drop = FALSE]) %>%
  bind_cols(
    tibble(WQI = WQI, WQI_class = WQI_class),
    as_tibble(qi, .name_repair = "check_unique") %>%
      rename_with(~ paste0(.x, "_qi")),
    as_tibble(SI, .name_repair = "check_unique") %>%
      rename_with(~ paste0(.x, "_SI"))
  )

# ---------- 9) Save Excel to Desktop ----------
wb <- createWorkbook()
addWorksheet(wb, "WQI results")
writeData(wb, "WQI results", out)
saveWorkbook(
  wb,
  file.path("~/Desktop", "WQI_results_assigned_weights_NEW_CLASSES.xlsx"),
  overwrite = TRUE
)
cat("\n✅ WQI Excel saved to Desktop.\n")


# ======================================================
# 2) Spatial Box Plot for WQI (new classes, sheet = "WQI results")
# ======================================================
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(janitor)
})

file_path <- file.choose()   # choose the Excel produced above
wqi_df <- read_excel(file_path, sheet = "WQI results") %>% clean_names()

# Identify columns
guess_col <- function(x, candidates) {
  hit <- intersect(names(x), candidates)
  if (length(hit)) hit[1] else NA_character_
}
site_col    <- guess_col(wqi_df, c("site_id","site","siteid"))
stretch_col <- guess_col(wqi_df, c("stretch"))
wqi_col     <- guess_col(wqi_df, c("wqi","water_quality_index"))
stopifnot(!is.na(site_col), !is.na(stretch_col), !is.na(wqi_col))

# Rebuild classes (0–25, 26–50, 51–75, 76–100, >100)
wqi_df <- wqi_df %>%
  mutate(
    !!stretch_col := factor(.data[[stretch_col]],
                            levels = c("Upstream","Midstream","Downstream")),
    WQI_class = cut(
      .data[[wqi_col]],
      breaks = c(-Inf, 25, 50, 75, 100, Inf),
      labels = c("Excellent", "Good", "Poor", "Very Poor", "Unsuitable for drinking"),
      right = TRUE,
      ordered_result = TRUE
    )
  )

# Okabe–Ito
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00",
               "#CC79A7", "#999999")

p_box <- ggplot(
  wqi_df,
  aes(x = .data[[site_col]], y = .data[[wqi_col]], fill = .data[[stretch_col]])
) +
  geom_boxplot(alpha = 0.9, outlier.shape = NA) +
  geom_jitter(aes(color = .data[[stretch_col]]),
              width = 0.2, size = 1.2, alpha = 0.6) +
  geom_hline(yintercept = c(25, 50, 75, 100),
             linetype = "dotted", color = "black", linewidth = 0.6) +
  annotate("text", x = -0.3, y = 12.5,  label = "Excellent",
           hjust = 0, vjust = 0, size = 3.5, color = "black", fontface = "italic") +
  annotate("text", x = -0.3, y = 37.5,  label = "Good",
           hjust = 0, vjust = 0, size = 3.5, color = "black", fontface = "italic") +
  annotate("text", x = -0.3, y = 62.5,  label = "Poor",
           hjust = 0, vjust = 0, size = 3.5, color = "black", fontface = "italic") +
  annotate("text", x = -0.3, y = 87.5,  label = "Very Poor",
           hjust = 0, vjust = 0, size = 3.5, color = "black", fontface = "italic") +
  annotate("text", x = -0.3, y = 112,   label = "Unsuitable",
           hjust = 0, vjust = 0, size = 3.5, color = "black", fontface = "italic") +
  scale_fill_manual(values = okabe_ito[1:3]) +
  scale_color_manual(values = okabe_ito[1:3]) +
  facet_wrap(reformulate(stretch_col), scales = "free_x", nrow = 1) +
  labs(
    title = "Spatial Distribution of Water Quality Index (WQI) across Sites",
    x = "Site", y = "Water Quality Index (WQI)",
    fill = "Stretch", color = "Stretch"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    strip.background = element_rect(fill = "white", colour = "black"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(p_box)
ggsave(
  "~/Desktop/WQI_Spatial_Boxplot_NEW_CLASSES.svg",
  p_box, width = 8, height = 5, dpi = 600
)


# ======================================================
# 3) Stacked Bar Plot by Stretch (no black; light purple for 'Unsuitable')
# ======================================================
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(janitor)
})

file_path <- file.choose()   # choose the same Excel
wqi_df <- read_excel(file_path, sheet = "WQI results") %>% clean_names()

# Identify columns
guess_col <- function(x, candidates) {
  hit <- intersect(names(x), candidates)
  if (length(hit)) hit[1] else NA_character_
}
stretch_col <- guess_col(wqi_df, c("stretch"))
wqi_col     <- guess_col(wqi_df, c("wqi","water_quality_index"))
stopifnot(!is.na(stretch_col), !is.na(wqi_col))

# Rebuild classes
wqi_df <- wqi_df %>%
  mutate(
    !!stretch_col := factor(.data[[stretch_col]],
                            levels = c("Upstream","Midstream","Downstream")),
    WQI_class = cut(
      .data[[wqi_col]],
      breaks = c(-Inf, 25, 50, 75, 100, Inf),
      labels = c("Excellent", "Good", "Poor", "Very Poor", "Unsuitable for drinking"),
      right = TRUE,
      ordered_result = TRUE
    )
  )

# Class colors (no black)
class_cols <- c(
  "Excellent"               = "#009E73",
  "Good"                    = "#56B4E9",
  "Poor"                    = "#E69F00",
  "Very Poor"               = "#D55E00",
  "Unsuitable for drinking" = "#A676CF"  # lighter lavender-purple
)

wqi_prop <- wqi_df %>%
  filter(!is.na(.data[[stretch_col]]), !is.na(WQI_class)) %>%
  count(Stretch = .data[[stretch_col]], WQI_class, name = "n") %>%
  group_by(Stretch) %>%
  mutate(perc = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(
    WQI_class = factor(
      WQI_class,
      levels = c("Excellent","Good","Poor","Very Poor","Unsuitable for drinking")
    )
  )

p_stack <- ggplot(wqi_prop, aes(x = Stretch, y = perc, fill = WQI_class)) +
  geom_col(color = "white", linewidth = 0.3) +
  geom_text(
    aes(label = paste0(round(perc, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  scale_fill_manual(values = class_cols, drop = FALSE) +
  labs(
    title = "WQI Class Composition by River Stretch",
    x = "Stretch", y = "Percentage (%)", fill = "WQI Class"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(p_stack)
ggsave(
  "~/Desktop/WQI_StackedBar_Stretch_NEW_CLASSES_LightPurple.svg",
  p_stack, width = 6.5, height = 4.5, dpi = 600
)


# ======================================================
# 4) Relative Weights (Wi) from assigned weights (wi) and standards (Si)
# ======================================================
suppressPackageStartupMessages(library(tidyverse))

param_data <- tribble(
  ~Parameter, ~wi, ~Si,
  "DO",   4, 7,
  "EC",   3, 400,
  "Ca2+", 2, 75,
  "Mg2+", 2, 30,
  "K+",   2, 12,
  "Na+",  2, 200,
  "Fe",   3, 0.3,
  "Zn",   2, 5,
  "Pb",   5, 0.01,
  "Cl-",  3, 250
)

param_data <- param_data %>%
  mutate(
    inv_si = wi / Si,
    Wi     = inv_si / sum(inv_si)
  ) %>%
  arrange(desc(Wi))

param_data %>%
  mutate(across(where(is.numeric), ~ round(., 6))) %>%
  rename(
    `Assigned weight (wi)` = wi,
    `Standard value (Si)`  = Si,
    `Relative weight (Wi)` = Wi
  ) %>%
  print(n = Inf)

write_csv(param_data, "~/Desktop/WQI_Relative_Weights.csv")
cat("\n✅ Relative weights saved to Desktop as 'WQI_Relative_Weights.csv'\n")
