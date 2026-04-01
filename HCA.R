suppressPackageStartupMessages({
  library(tidyverse)
  library(vegan)
  library(dendextend)
  library(readxl)
})

# ---- 1. Choose master dataset ----
file_path <- file.choose()
master_data <- read_excel(file_path)

# ---- 2. Define FINAL PCA variable set (FULL NAMES) ----
pca_vars <- c(
  "pH",
  "DO",
  "EC",
  "Sodium",
  "Calcium",
  "Fluoride",
  "Chloride",
  "Nitrate",
  "Phosphate",
  "Sulphate"
)

# ---- 3. Build site-wise median matrix ----
hca_data <- master_data %>%
  select(Site_ID, all_of(pca_vars)) %>%
  group_by(Site_ID) %>%
  summarise(across(all_of(pca_vars), median, na.rm = TRUE)) %>%
  ungroup()

# ---- 4. Convert to matrix and impute remaining NAs ----
hca_mat <- hca_data %>%
  column_to_rownames("Site_ID")

hca_mat_imp <- apply(hca_mat, 2, function(x) {
  x[is.na(x)] <- median(x, na.rm = TRUE)
  return(x)
}) %>% as.data.frame()

# ---- 5. Z-score standardisation ----
hca_scaled <- scale(hca_mat_imp)

# ---- 6. Bray–Curtis dissimilarity and clustering ----
bc_dist <- vegdist(hca_scaled, method = "bray")
hc <- hclust(bc_dist, method = "ward.D2")

# ---- 7. Prepare dendrogram ----
dend_pub <- as.dendrogram(hc) |>
  set("branches_col", "black") |>
  set("branches_lwd", 1.1) |>
  set("labels_col", "black") |>
  set("labels_cex", 1.0)

# ---- 8. Figure dimensions (Water Research) ----
w_in <- 7.09
h_in <- 5.5
plot_par <- list(mar = c(5, 20, 1, 2), family = "sans")

# ---- 9. Export SVG ----
svg("HCA_SiteWise_BrayCurtis_WR.svg",
    width = w_in, height = h_in, family = "sans")
par(plot_par)
plot(dend_pub, horiz = TRUE, leaflab = "perpendicular")
axis(side = 1, lwd = 0.8, cex.axis = 0.8)
title(xlab = "Bray–Curtis dissimilarity", cex.lab = 1.0)
rect.dendrogram(dend_pub, k = 3, border = "black", lwd = 1.1, horiz = TRUE)
dev.off()

# ---- 10. Export PDF ----
pdf("HCA_SiteWise_BrayCurtis_WR.pdf",
    width = w_in, height = h_in, family = "sans")
par(plot_par)
plot(dend_pub, horiz = TRUE, leaflab = "perpendicular")
axis(side = 1, lwd = 0.8, cex.axis = 0.8)
title(xlab = "Bray–Curtis dissimilarity", cex.lab = 1.0)
rect.dendrogram(dend_pub, k = 3, border = "black", lwd = 1.1, horiz = TRUE)
dev.off()

# ---- 11. Export PNG (600 dpi) ----
png("HCA_SiteWise_BrayCurtis_WR.png",
    width = w_in * 600,
    height = h_in * 600,
    res = 600,
    family = "sans")
par(plot_par)
plot(dend_pub, horiz = TRUE, leaflab = "perpendicular")
axis(side = 1, lwd = 0.8, cex.axis = 0.8)
title(xlab = "Bray–Curtis dissimilarity", cex.lab = 1.0)
rect.dendrogram(dend_pub, k = 3, border = "black", lwd = 1.1, horiz = TRUE)
dev.off()
