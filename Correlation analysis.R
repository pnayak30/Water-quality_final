# =====================================================
#  Spearman Correlation — grouped, clean, no gridlines
#  (Removes TDS, Latitude, Longitude, Lithium, Nitrite, Year)
#  Water Research–quality figure (SVG + PNG @ 600 dpi)
# =====================================================

suppressPackageStartupMessages({
  library(readxl)
  library(tidyverse)
  library(ggcorrplot)
  library(Hmisc)
})

# ---- 1) Load data ----
file_path <- file.choose()
df <- read_excel(file_path)

# ---- 2) Remove unwanted parameters ----
drop_list <- c(
  "TDS",
  "Latitude",
  "Longitude",
  "Lithium",
  "Nitrite",
  "Year"
)

df <- df %>% select(-any_of(drop_list))

# ---- 3) Keep only numeric columns ----
df_num <- df %>% select(where(is.numeric))

stopifnot(
  "Need >=2 numeric columns after removals" =
    ncol(df_num) >= 2
)

# ---- 4) Spearman correlation + p-values ----
rc      <- Hmisc::rcorr(as.matrix(df_num), type = "spearman")
cor_mat <- rc$r
p_mat   <- rc$P

# ---- 5) Keep only variables with ≥1 significant correlation ----
sig_any   <- apply(p_mat < 0.05 & !is.na(p_mat), 1, any)
keep_vars <- names(sig_any[sig_any])

if (length(keep_vars) < 2) {
  stop("After pruning, fewer than 2 variables remain with significant correlations.")
}

# ---- 6) Define groups and custom plotting order ----
physicochem <- c("Temperature", "pH", "DO", "EC")
anions      <- c("Chloride", "Sulphate", "Nitrate",
                 "Fluoride", "Bromide", "Phosphate")
cations     <- c("Sodium", "Potassium", "Magnesium",
                 "Calcium", "Ammonium")
trace_metals<- c("Iron", "Manganese", "Zinc",
                 "Lead", "Copper", "Cobalt")

ord_phys   <- intersect(physicochem, keep_vars)
ord_anion  <- intersect(anions, keep_vars)
ord_cation <- intersect(cations, keep_vars)
ord_trace  <- intersect(trace_metals, keep_vars)

ordered_vec <- c(
  ord_phys,
  ord_anion,
  ord_cation,
  ord_trace,
  setdiff(keep_vars,
          c(ord_phys, ord_anion, ord_cation, ord_trace))
)

# ---- 7) Reorder matrices ----
cor_mat <- cor_mat[ordered_vec, ordered_vec, drop = FALSE]
p_mat   <- p_mat  [ordered_vec, ordered_vec, drop = FALSE]

# ---- 8) Round correlations for labels ----
cor_mat_round <- round(cor_mat, 2)

# ---- 9) Plot settings (Okabe–Ito) ----
okabe_sky  <- "#56B4E9"  # negative correlations
okabe_orng <- "#E69F00"  # positive correlations

n_vars   <- length(ordered_vec)
lab_size <- if (n_vars <= 14) 3.2 else if (n_vars <= 22) 2.6 else 2.2
ax_size  <- if (n_vars <= 16) 10  else if (n_vars <= 24) 9   else 8

w_in <- max(6.9, min(14, 0.40 * n_vars + 2.5))
h_in <- max(5.0, min(12, 0.40 * n_vars + 2.0))

# ---- 10) Build correlation heatmap ----
p_corr <- ggcorrplot(
  cor_mat_round,
  type = "lower",
  hc.order = FALSE,
  lab = TRUE,
  lab_size = lab_size,
  p.mat = p_mat,
  sig.level = 0.05,
  insig = "blank",
  outline.col = "white",
  colors = c(okabe_sky, "white", okabe_orng)
) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "Arial"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = ax_size),
    axis.text.y = element_text(size = ax_size),
    axis.title = element_blank()
  )

print(p_corr)

# ---- 11) Save outputs ----
write.csv(cor_mat,
          "Correlation_Spearman_SignificantOnly_matrix.csv")
write.csv(p_mat,
          "Correlation_Spearman_SignificantOnly_pvalues.csv")
write.csv(
  data.frame(
    Excluded = setdiff(colnames(df_num), ordered_vec)
  ),
  "Correlation_Excluded_Variables.csv",
  row.names = FALSE
)

ggsave(
  "Correlation_Spearman_SignificantOnly.svg",
  p_corr,
  width = w_in,
  height = h_in,
  units = "in"
)

ggsave(
  "Correlation_Spearman_SignificantOnly.png",
  p_corr,
  width = w_in,
  height = h_in,
  units = "in",
  dpi = 600
)
