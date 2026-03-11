## septic shock hospitalisation rates links with INSEE indicators across French regions
## OLS + SAR + BYM (via R-INLA)----

library(sf)
library(ggplot2)
library(dplyr)
library(INLA) # Bayesian spatial models (BYM2)
library(corrplot)


# data --------------------------------------------------------------------
# Load spatial data
shock_combined <- readRDS("./data/derived/04_combined_chock.RDS")
spatial_data <- readRDS("./data/derived/04_spatial_dat.RDS")



# Filtering and transforming data -----------------------------------------
overseas <- c("GUADELOUPE", "MARTINIQUE", "GUYANE", "LA REUNION", "MAYOTTE")


## Join spatial data with the analytical dataset ------------------------------------------------------
dat_sf <- 
  spatial_data |>
  mutate(region_name = NOM_M) |>
  filter(!region_name %in% overseas & !is.na(region_name)) |>
  left_join(shock_combined, by = "region_name")


shock_combined <- 
  shock_combined |>
  filter(!region_name %in% overseas & !is.na(region_name))

insee_vars <- 
  shock_combined |>
  select(intensite_pauvrete_pct, niveau_vie_median_annuel, 
         pct_non_scol15, densite_pop_2022) |>
  distinct()

cor(insee_vars, use = "complete.obs")


# Filtering and transforming data -----------------------------------------
overseas <- c("GUADELOUPE", "MARTINIQUE", "GUYANE", "LA REUNION", "MAYOTTE")

shock_combined <- 
  shock_combined |>
  filter(!region_name %in% overseas & !is.na(region_name))


# Scale predictors
model_dat_scaled <- 
  shock_combined |>
  mutate(across(c(niveau_vie_median_annuel,pct_non_scol15, densite_pop_2022), scale))

# Fit OLS
ols_fit <- lm(tx_sejours_sepsis_100k ~ intensite_pauvrete_pct + 
                pct_non_scol15 + densite_pop_2022, 
              data = model_dat_scaled)

summary(ols_fit)

# Test residuals for spatial autocorrelation
model_dat_scaled$resid <- residuals(ols_fit)

# Attach geometry back
shock_combined_resid <- 
  dat_sf %>%
  distinct(region_name, .keep_all = TRUE) %>%
  left_join(model_dat_scaled %>% select(region_name, resid), by = "region_name")

moran.test(metro_resid$resid, w_listw)

