library(dplyr)
library(tidyr)
library(cluster)

all_dat <- readRDS("./data/derived/04_combined_all.RDS") %>% filter(!is.na(region_name))

vars_insee <- c("densite_pop_2022","niveau_vie_median_annuel",
                "intensite_pauvrete_pct","pct_non_scol15","total_population")

vars_sepsis <- c("nb_sejours","age_moyen","dms_nuitees","pct_deces","tx_sejours_sepsis_100k")


# Base INSEE vars (one row per region)
base <- 
  all_dat %>%
  distinct(region_name, .keep_all = TRUE) %>%
  select(region_name, all_of(vars_insee))

# Wide format by sepsis type
wide_sepsis <-
  all_dat %>%
  select(region_name, type_sejour_sepsis, all_of(vars_sepsis)) %>%
  pivot_wider(names_from = type_sejour_sepsis, values_from =
                all_of(vars_sepsis))


# Helper to fetch type columns (adapt exact names if needed)
nb1 <- wide_sepsis$`nb_sejours_1 - Sepsis sans choc septique`
nb2 <- wide_sepsis$`nb_sejours_2 - Choc septique`
nbT <- nb1 + nb2

ageT <- (wide_sepsis$`age_moyen_1 - Sepsis sans choc septique` * nb1 +
           wide_sepsis$`age_moyen_2 - Choc septique` * nb2) / nbT

dmsT <- (wide_sepsis$`dms_nuitees_1 - Sepsis sans choc septique` * nb1 +
           wide_sepsis$`dms_nuitees_2 - Choc septique` * nb2) / nbT

deathsT <- (wide_sepsis$`pct_deces_1 - Sepsis sans choc septique` * nb1 +
              wide_sepsis$`pct_deces_2 - Choc septique`* nb2)

pctT <- deathsT / nbT

## Include the total sepsis variables in the base dataset
df_total <- base %>%
  left_join(wide_sepsis %>% select(region_name), by="region_name") %>%
  mutate(
    nb_sejours = nbT,
    age_moyen = ageT,
    dms_nuitees = dmsT,
    pct_deces = pctT,
    tx_sejours_sepsis_100k = 1e5 * nbT / total_population)

## define main variables for clustering (adapt as needed)
features <-  c("nb_sejours","age_moyen","dms_nuitees","pct_deces","tx_sejours_sepsis_100k",
    "densite_pop_2022","niveau_vie_median_annuel","intensite_pauvrete_pct","pct_non_scol15")

X <- df_total %>% select(all_of(features))

# log1p on skewed features
X$nb_sejours <- log1p(X$nb_sejours)
X$densite_pop_2022 <- log1p(X$densite_pop_2022)    


# median imputation
for (j in seq_along(X)) {
  X[[j]][is.na(X[[j]])] <- median(X[[j]], na.rm=TRUE)
}

# z-score
Xs <- scale(X)

# Ward hierarchical clustering
hc <- hclust(dist(Xs), method="ward.D2")


## Silhuette values
for (k in 2:6) {
  cl_k <- cutree(hc, k = k)
  sil <- silhouette(cl_k, dist(Xs))
  cat("k =", k, " mean silhouette =", mean(sil[, "sil_width"]), "\n")
}


# choose k
k <- 5
cluster <- cutree(hc, k = k)
df_total$cluster <- factor(cluster)

# --- Summaries: original scale means ---
summary_orig <- 
  df_total %>%
  group_by(cluster) %>%
  summarise(
    n_regions = n(),
    across(all_of(features), ~mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

write.csv(summary_orig, "./outputs/tables/05_septic_all_cluster_summary_means_original_k5.csv",
          row.names = FALSE)


# --- Summaries: z-centroids ---
Xs_df <- as.data.frame(Xs)
Xs_df$cluster <- factor(cluster)

summary_z <- Xs_df %>%
  group_by(cluster) %>%
  summarise(n_regions = n(), across(where(is.numeric), mean), .groups = "drop")

write.csv(summary_z, "./outputs/tables/05_septic_all_cluster_summary_means_z_k5.csv", row.names =
            FALSE)


# --- Export assignments ---
write.csv(df_total, "./outputs/tables/05_septic_all_cluster_assignments_k5.csv", row.names =
            FALSE)

# --- Plots ---
png("./outputs/figures/05_dendrogram_septic_all_ward.png", width = 1400, height = 700, res = 150)
plot(hc, hang = -1, cex = 0.7, main = "Ward dendrogram")
rect.hclust(hc, k = k_final)
dev.off()


pca <- prcomp(Xs, center = FALSE, scale. = FALSE)

pca_df <- data.frame(pca$x[,1:2], cluster = factor(cluster), region =
                       df_total$region_name)


png("./outputs/figures/05_pca_septic_all_clusters.png", width = 900, height = 700, res = 150)
ggplot(pca_df, aes(x = PC1, y = PC2, label = region, color = cluster)) +
  geom_point() + geom_text(size = 2.5, vjust = -0.4) +
  theme_minimal() + ggtitle("PCA of standardized septic-all profiles")
dev.off()


##### Using only sepsis with chock ------

# --- Filter septic shock rows ---
shock <- 
  all_dat %>%
  filter(type_sejour_sepsis == "2 - Choc septique") %>%
  filter(!is.na(region_name)) %>% 
  filter(region_name!="MAYOTTE") # Exclude Mayotte due to missing INSEE data (documented choice)


# --- Aggregate to one row per region ---
shock_reg <- 
  shock %>%
  group_by(region_name, region, code_region) %>%
  summarise(
    total_population = first(total_population),
    # INSEE covariates
    densite_pop_2022 = first(densite_pop_2022),
    niveau_vie_median_annuel = first(niveau_vie_median_annuel),
    intensite_pauvrete_pct = first(intensite_pauvrete_pct),
    pct_non_scol15 = first(pct_non_scol15),
    # septic shock metrics
    nb_sejours = sum(nb_sejours),
    age_moyen = weighted.mean(age_moyen, w = nb_sejours),
    dms_nuitees = weighted.mean(dms_nuitees, w = nb_sejours),
    deaths_est = sum(pct_deces * nb_sejours),
    pct_deces = deaths_est / nb_sejours,
    tx_sejours_sepsis_100k = 1e5 * nb_sejours / total_population,
    .groups = "drop"
  )

# --- Features used for clustering ---
features <-  c("nb_sejours","age_moyen","dms_nuitees","pct_deces","tx_sejours_sepsis_100k",
    "densite_pop_2022","niveau_vie_median_annuel","intensite_pauvrete_pct","pct_non_scol15")

X_shock <- shock_reg %>% select(all_of(features))

# --- log1p on skewed vars (as specified) ---
X_shock$nb_sejours <- log1p(X_shock$nb_sejours)
X_shock$densite_pop_2022 <- log1p(X_shock$densite_pop_2022)

# --- Standardize (z-scores) ---
Xs_shock <- scale(X_shock)

# --- Ward hierarchical clustering ---
hc_shock <- hclust(dist(Xs_shock), method = "ward.D2")

# --- k selection: silhouette + stability vs k-means ---

## Silhuette values
for (k in 2:6) {
  cl_k <- cutree(hc_shock, k = k)
  sil <- silhouette(cl_k, dist(Xs_shock))
  cat("k =", k, " mean silhouette =", mean(sil[, "sil_width"]), "\n")
}

# --- Choose k (example: k=5) ---
k_final <- 4
cluster <- cutree(hc_shock, k = k_final)
shock_reg$cluster <- factor(cluster)

# --- Summaries: original scale means ---
summary_orig <- 
  shock_reg %>%
  group_by(cluster) %>%
  summarise(
    n_regions = n(),
    across(all_of(features), ~mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

write.csv(summary_orig, "./outputs/tables/05_septic_shock_cluster_summary_means_original_k5.csv",
          row.names = FALSE)


# --- Summaries: z-centroids ---
Xs_df <- as.data.frame(Xs_shock)
Xs_df$cluster <- factor(cluster)

summary_z <- Xs_df %>%
  group_by(cluster) %>%
  summarise(n_regions = n(), across(where(is.numeric), mean), .groups = "drop")

write.csv(summary_z, "./outputs/tables/05_septic_shock_cluster_summary_means_z_k5.csv", row.names =
            FALSE)


# --- Export assignments ---
write.csv(shock_reg, "./outputs/tables/05_septic_shock_cluster_assignments_k5.csv", row.names =
            FALSE)

# --- Plots ---
png("./outputs/figures/05_dendrogram_septic_shock_ward.png", width = 1400, height = 700, res = 150)
plot(hc, hang = -1, cex = 0.7, main = "Ward dendrogram (septic shock)")
rect.hclust(hc, k = k_final)
dev.off()


pca <- prcomp(Xs_shock, center = FALSE, scale. = FALSE)

pca_df <- data.frame(pca$x[,1:2], cluster = factor(cluster), region =
                       shock_reg$region_name)


png("./outputs/figures/05_pca_septic_shock_clusters.png", width = 900, height = 700, res = 150)
ggplot(pca_df, aes(x = PC1, y = PC2, label = region, color = cluster)) +
  geom_point() + geom_text(size = 2.5, vjust = -0.4) +
  theme_minimal() + ggtitle("PCA of standardized septic-shock profiles")
dev.off()


# Heatmap of z-centroids (simple base R version)
centroids <- summary_z %>% select(-n_regions) %>% as.data.frame()

row.names(centroids) <- centroids$cluster
centroids$cluster <- NULL

png("heatmap_septic_shock_cluster_centroids.png", width = 1400, height = 500,
    res = 150)
heatmap(as.matrix(centroids), Rowv = NA, Colv = NA, scale = "none",
        main = "Cluster z-centroids (means of standardized features)")
dev.off()
