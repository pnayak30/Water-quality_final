suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(ggrepel)
  library(grid)
})

# ----------------------- 0) Load -----------------------
file_path  <- file.choose()
sheet_name <- 1
dat_full   <- read_excel(file_path, sheet = sheet_name)

# Filter ONLY 2022–2024
dat <- dat_full %>% filter(Year %in% c(2022, 2023, 2024))

# ----------------------- 1) Parameter Columns -----------------------
params_all <- c(
  "pH","DO","EC",
  "Sodium","Calcium",
  "Fluoride","Chloride","Nitrate",
  "Phosphate","Sulphate"
)

meta_cols <- c("Stretch","Season","Year")
meta <- dat %>% select(any_of(meta_cols))

# ----------------------- 2) Build numeric matrix -----------------------
X <- dat %>%
  select(any_of(params_all)) %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.)))) %>%
  mutate(across(everything(), ~ replace(., is.infinite(.) | is.nan(.), NA_real_)))

# Drop all-NA and zero-variance columns
all_na_cols <- names(X)[vapply(X, function(v) all(is.na(v)), logical(1))]
zero_var_cols <- names(X)[vapply(X, function(v){
  v2 <- v[is.finite(v)]
  if (length(v2)==0) TRUE else sd(v2, na.rm=TRUE)==0
}, logical(1))]

if (length(all_na_cols)) message("Dropped all-NA columns: ", paste(all_na_cols, collapse=", "))
if (length(zero_var_cols)) message("Dropped zero-variance columns: ", paste(zero_var_cols, collapse=", "))

X <- X %>% select(-any_of(c(all_na_cols, zero_var_cols)))
stopifnot(ncol(X) >= 2)

# ----------------------- 3) Median Impute -----------------------
X_imp <- X %>% mutate(across(everything(), ~ {
  m <- median(., na.rm = TRUE)
  replace(., is.na(.), m)
}))

# ----------------------- 4) Z-score -----------------------
X_z <- scale(X_imp)
if (!all(is.finite(X_z))) stop("Non-finite values remain.")

# ----------------------- 5) PCA (ALL VARIABLES) -----------------------
pca <- prcomp(X_z, center = FALSE, scale. = FALSE)

# Variance explained
var_exp <- (pca$sdev^2) / sum(pca$sdev^2)
pc1_lab <- paste0("PC1 (", round(var_exp[1]*100, 1), "%)")
pc2_lab <- paste0("PC2 (", round(var_exp[2]*100, 1), "%)")

# ----------------------- 6) Scores -----------------------
scores_df <- as.data.frame(pca$x[, 1:2]) %>%
  rename(PC1 = 1, PC2 = 2) %>%
  bind_cols(meta) %>%
  mutate(
    Stretch = factor(Stretch, levels=c("Upstream","Midstream","Downstream")),
    Season  = factor(Season,  levels=c("Pre-monsoon","Monsoon","Post-monsoon")),
    Year    = factor(as.character(Year), levels=c("2022","2023","2024"))
  )

# ----------------------- 7) Loadings -----------------------
load_df <- as.data.frame(pca$rotation[,1:2])
colnames(load_df) <- c("L1","L2")
load_df$Variable <- rownames(load_df)

# Scale arrows
r1 <- range(scores_df$PC1, na.rm=TRUE)
r2 <- range(scores_df$PC2, na.rm=TRUE)
sx <- (r1[2]-r1[1]) / max(abs(load_df$L1))
sy <- (r2[2]-r2[1]) / max(abs(load_df$L2))
mult <- 0.70 * min(sx, sy)

load_df <- load_df %>%
  mutate(
    x0 = 0, y0 = 0,
    x1 = L1 * mult,
    y1 = L2 * mult
  )

# ----------------------- 8) Plot function -----------------------
okabe_ito <- c("#E69F00","#56B4E9","#009E73","#F0E442",
               "#0072B2","#D55E00","#CC79A7","#999999")

plot_pca_group <- function(df, group_col, title_text) {
  
  g <- ggplot(df, aes(PC1, PC2, color=.data[[group_col]])) +
    geom_hline(yintercept=0, linetype="dotted") +
    geom_vline(xintercept=0, linetype="dotted") +
    
    stat_ellipse(type="t", level=0.95, linewidth=0.6, linetype=2) +
    
    geom_point(size=2.3, alpha=0.9) +
    scale_color_manual(values = okabe_ito) +
    
    geom_segment(
      data = load_df,
      aes(x=x0, y=y0, xend=x1, yend=y1),
      inherit.aes=FALSE,
      arrow = arrow(length = unit(0.25, "cm")),
      linewidth=0.65, color="black"
    ) +
    
    geom_text_repel(
      data = load_df,
      aes(x=x1, y=y1, label=Variable),
      inherit.aes=FALSE,
      size=3.2, fontface="bold"
    ) +
    
    labs(title=title_text, x=pc1_lab, y=pc2_lab, color=NULL) +
    
    coord_equal() +
    
    theme_classic(base_size=12) +
    theme(
      axis.text  = element_text(color="black"),
      axis.title = element_text(face="bold"),
      legend.position = "top",
      panel.border = element_rect(color="black", fill=NA, linewidth=0.6)
    )
  
  print(g)
}

# ----------------------- 9) PRINT plots -----------------------
plot_pca_group(scores_df, "Stretch", "PCA grouped by Stretch")
plot_pca_group(scores_df, "Season",  "PCA grouped by Season")
plot_pca_group(scores_df, "Year",    "PCA grouped by Year")

# ----------------------- 10) Print loadings -----------------------
print(pca$rotation[,1:2])

# ----------------------- 11) Print summary -----------------------
print(summary(pca))