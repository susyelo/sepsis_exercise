## Spatial analysis of septic shock hospitalisation rates across French regions
## Moran's I + SAR + BYM (via R-INLA)----


# libs --------------------------------------------------------------------
library(spdep)
library(sf)
library(geodata)
library(ggplot2)
library(dplyr)
library(scales)
library(patchwork)
library(INLA) # Bayesian spatial models (BYM2)
library(regclass)
library(spatialreg)


## Data prep -----------------------------------------------------------
# Load spatial data
shock_combined <- readRDS("./data/derived/04_combined_chock.RDS")
spatial_data <- readRDS("./data/derived/04_spatial_dat.RDS")


# Filtering and transforming data -----------------------------------------
overseas <- c("GUADELOUPE", "MARTINIQUE", "GUYANE", "LA REUNION", "MAYOTTE")

shock_combined <- 
  shock_combined |>
  filter(!region_name %in% overseas & !is.na(region_name))


## Variable transformations -------------------------------------------

## Population density (densite_pop_2022):
##   Right-skewed distribution — IDF at 1,031 inhab/km², Guyane at 3.5.
##   Log-transform before scaling to reduce leverage of extreme values.

## Log of total population — used as an offset in the Poisson BYM model.

shock_combined <- 
  shock_combined |>
  mutate(log_dens = log(densite_pop_2022)) |>
  mutate(log_pop = log(total_population))


## Standardise covariates to z-scores ---------------------------------
## response variables are not scale to keep the interpretability of the coefficients in the spatial models, 
# but I am standardising the covariates to make them comparable and to reduce the influence of outliers in the regression models.

shock_combined <- 
  shock_combined |>
  mutate(
    niveau_vie_z   = as.numeric(scale(niveau_vie_median_annuel)),
    pct_non_scol_z = as.numeric(scale(pct_non_scol15)),
    log_dens_z     = as.numeric(scale(log_dens))
  )


## Quick check: variances of pct_non_scol15 are narrow. This
## variable may contribute little to the model — watch its coefficient.
summary(shock_combined[, c("niveau_vie_z", "pct_non_scol_z", "log_dens_z",
                     "tx_sejours_sepsis_100k", "nb_sejours")])



## Join spatial data with the analytical dataset ------------------------------------------------------
dat_sf <- 
  spatial_data |>
  mutate(region_name = NOM_M) |>
  filter(!region_name %in% overseas & !is.na(region_name)) |>
  left_join(shock_combined, by = "region_name")

## Reorder rows to match a consistent region index (needed for INLA)
dat_sf <- dat_sf |> arrange(region_name)
dat_main_ordered <- shock_combined |> arrange(region_name)



## Build the spatial weight matrix ------------------------------------
## I use Queen contiguity (shared border OR shared corner).
## Queen contiguity is standard for administrative regions that share edges.

nb_queen <- poly2nb(dat_sf, queen = TRUE)


## Corse has no land border with the mainland. poly2nb() will return it as
## an isolate (zero neighbours), which breaks the ICAR model.
## We assign its nearest mainland neighbour by geographic proximity.
## PACA (Provence-Alpes-Côte d'Azur) is the closest region.

corse_idx <- which(dat_sf$region_name == "CORSE")
paca_idx  <- which(dat_sf$region_name == "PROVENCE-ALPES-COTE D'AZUR")

## Add Corse–PACA as reciprocal neighbours
nb_queen[[corse_idx]] <- as.integer(paca_idx)
nb_queen[[paca_idx]]  <- sort(as.integer(c(nb_queen[[paca_idx]], corse_idx)))

## Verify no isolates remain
summary(nb_queen)

## Convert to row-standardised weights (each region's neighbours sum to 1).
## Row standardisation is standard for Moran's I and SAR models.
## For BYM2 binary weights).
w_listw <- nb2listw(nb_queen, style = "W")


### Save adjacency graph for INLA (binary, not row-standardised)
nb2INLA("./data/derived/06_france_regions_nb.graph", nb_queen)
g_inla <- inla.read.graph("./data/derived/06_france_regions_nb.graph")


## MORAN'S I -----------------------------------------------------------
## 1. global Moran's
## Moran's I measures global spatial autocorrelation.
## do regions with high septic shock rates tend to cluster geographically, or are high-rate and low-rate regions scattered
## randomly across the map?
##
## I ranges from -1 (perfect dispersion) to +1 (perfect clustering).
## A significant positive I means nearby regions look more alike than expected
## by chance — which would justify using a spatial model.

## I use tx_sejours_sepsis_100k (the rate) for Moran's I, since the test is statistic is more interpretable on the rate scale.
moran_result <- moran.test(
  x       = dat_sf$tx_sejours_sepsis_100k,
  listw   = w_listw,
  alternative = "greater"  # one-sided: test for positive clustering
)

print(moran_result)


## Monte Carlo permutation test — more robust with small n (13 regions).
## It resamples the observed rates across regions 999 times and asks:
## how often would we get an I as large as observed purely by chance?
moran_mc <- moran.mc(
  x     = dat_sf$tx_sejours_sepsis_100k,
  listw = w_listw,
  nsim  = 999,
  alternative = "greater"
)
print(moran_mc)

p <- plot(moran_mc, main = "Moran's I permutation distribution (septic shock rate)")

saveRDS(moran_mc, "data/derived/06_moran_mc_result.RDS")

# 
#. Local Moran's I (LISA) ----
lisa <- localmoran(dat_sf$tx_sejours_sepsis_100k, w_listw)
dat_sf$local_I   <- lisa[, "Ii"]
dat_sf$local_pval <- lisa[, "Pr(z != E(Ii))"]

# Classify into quadrants
z <- scale(dat_sf$tx_sejours_sepsis_100k)[,1]
lag_z <- lag.listw(w_listw, z)

dat_sf$quad <- case_when(
  z > 0 & lag_z > 0 ~ "HH",
  z < 0 & lag_z < 0 ~ "LL",
  z > 0 & lag_z < 0 ~ "HL",  # potential spatial outlier
  z < 0 & lag_z > 0 ~ "LH"
)

# Only flag significant ones (p < 0.05)
dat_sf$quad_sig <- ifelse(dat_sf$local_pval < 0.05, 
                          dat_sf$quad, "Not significant")

# Quick look at Île-de-France specifically
LISA_ile_de_france <-
  dat_sf %>% 
  st_drop_geometry() %>%
  filter(grepl("ILE-DE-FRANCE", NOM_M, ignore.case = TRUE)) %>%
  select(NOM_M, quad, tx_sejours_sepsis_100k, local_I, local_pval, quad_sig)

saveRDS(LISA_ile_de_france, "data/derived/06_LISA_ile_de_france.RDS")



# OLS ---------------------------------------------------------------------
insee_vars <- 
  shock_combined |>
  select(intensite_pauvrete_pct, niveau_vie_median_annuel, 
         pct_non_scol15, densite_pop_2022, log_dens) |>
  distinct()

cor(insee_vars, use = "complete.obs")

# Fit OLS
model_dat <- 
  dat_sf %>%
  st_drop_geometry() 

ols_fit <- lm(tx_sejours_sepsis_100k ~ niveau_vie_z +  log_dens_z, 
              data = model_dat)

summary(ols_fit)
VIF(ols_fit)

# Test residuals for spatial autocorrelation
model_dat_scaled$resid <- residuals(ols_fit)

 

# Attach geometry back
shock_combined_resid <- 
  dat_sf %>%
  distinct(region_name, .keep_all = TRUE) %>%
  left_join(model_dat_scaled %>% select(region_name, resid), by = "region_name")


## mapping residuals to check for spatial patterns in the residuals (which would suggest that the OLS model is missing spatially structured predictors or that a spatial model is needed)

ggplot(shock_combined_resid) +
  geom_sf(aes(fill = resid)) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0,
    name = "Residual"
  ) +
  labs(title = "OLS Residuals — Sepsis hospitalisation rate",
       subtitle = "Red = higher than predicted, Blue = lower than predicted") +
  theme_minimal()



moran.test(shock_combined_resid$resid, w_listw)

# fitting SEM model -------------------------------------------------------
sem_fit <- errorsarlm(tx_sejours_sepsis_100k ~ niveau_vie_z + log_dens_z,
                      data = model_dat,
                      listw = w_listw)

summary(sem_fit)


