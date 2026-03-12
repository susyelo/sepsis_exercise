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
library(glmnet)


## Data prep -----------------------------------------------------------
# Load spatial data
shock_combined <- readRDS("./data/derived/04_combined_chock.RDS")
spatial_data <- readRDS("./data/derived/04_spatial_dat.RDS")
INSEE_data <- readRDS("./data/derived/04_all_insee_dat.RDS")


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
    log_dens_z     = as.numeric(scale(log_dens)),
    intensite_pauvrete_pct_z = as.numeric(scale(intensite_pauvrete_pct))
)


## Quick check: variances of pct_non_scol15 are narrow. This
## variable may contribute little to the model — watch its coefficient.
summary(shock_combined[, 
                       c("niveau_vie_z", "pct_non_scol_z", "log_dens_z", "intensite_pauvrete_pct_z","tx_sejours_sepsis_100k", "nb_sejours")])



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
LISA_results <-
  dat_sf %>% 
  st_drop_geometry() %>%
  filter(grepl("ILE-DE-FRANCE|PAYS DE LA LOIRE", NOM_M, ignore.case = TRUE)) %>%
  select(NOM_M, quad, tx_sejours_sepsis_100k, local_I, local_pval, quad_sig)

saveRDS(LISA_results, "data/derived/06_LISA_results.RDS")



# OLS ---------------------------------------------------------------------
## check correlation among variables to watch for multicollinearity in the OLS model. High correlation among predictors can inflate standard errors and make it hard to interpret coefficients. If I find high correlations, I may consider removing or combining variables, or using a regularisation method like Ridge regression.

insee_vars <- 
  shock_combined |>
  select(intensite_pauvrete_pct, niveau_vie_median_annuel, 
         pct_non_scol15, log_dens) |>
  distinct()

cor_dat <- cor(insee_vars, use = "complete.obs")

saveRDS(cor_dat, "data/derived/06_insee_vars_correlation.RDS")

# Fit OLS

ols_fit <- lm(tx_sejours_sepsis_100k ~ niveau_vie_z +  log_dens_z + pct_non_scol_z + intensite_pauvrete_pct_z, data = shock_combined)

summary(ols_fit)
VIF(ols_fit)

saveRDS(ols_fit, "./data/derived/06_ols_model.RDS")



# fitting SEM model -------------------------------------------------------
lm.RStests(ols_fit, listw = w_listw, test = "all")

sem_fit <- 
  errorsarlm(tx_sejours_sepsis_100k ~ niveau_vie_z +  log_dens_z + pct_non_scol_z + intensite_pauvrete_pct_z, data = model_dat, listw = w_listw)

summary(sem_fit)


# fitting Ridge regression -------------------------------------------------------

# Prepare matrix - include all predictors now
# since Ridge handles multicollinearity
x_dat <- 
  shock_combined %>%
  select(niveau_vie_z, intensite_pauvrete_pct_z,
         pct_non_scol_z, log_dens_z) %>%
  as.matrix()

y_dat <- shock_combined$tx_sejours_sepsis_100k

# Cross-validated Ridge (alpha = 0 for Ridge, 1 for LASSO)
set.seed(123)

# Leave-one-out CV for small samples
#cv.glmnet defaults to 10-fold cross-validation — but with n=13, some folds will have only 1-2 observations. I am therefore leaving one out cross-validation instead
  
ridge_cv <- cv.glmnet(x_dat, y_dat, alpha = 0, nfolds = 13)

# Optimal lambda
plot(ridge_cv)
best_lambda <- ridge_cv$lambda.min
cat("Best lambda:", best_lambda)

# Final coefficients
coef(ridge_cv, s = "lambda.min")

saveRDS(ridge_cv, "./data/derived/06_ridge_model.RDS")



# BYM2 model --------------------------------------------------------------
# Prepare data for INLA

# The response variable is the count of septic shock hospitalisations (nb_sejours).
# I use log(total_population) as an offset to model rates.

## standard Bayesian spatial model for disease mapping.
##
## It decomposes regional variability into two components:
##   - phi (structured): spatially correlated random effect (ICAR)
##     = nearby regions share similar unexplained variation
##   - theta (unstructured): region-specific noise, not spatially patterned
##     = like individual-level random effects in a mixed model
## A mixing parameter (phi proportion) controls the balance between the two.
##
## Why Poisson + offset?
##   - nb_sejours is a count — Poisson is the natural distribution for counts.
##   - The offset log(total_population) standardises for population size,
##     so the model estimates log-rates, not absolute counts.
##
## Region index: INLA needs a numeric ID (1, 2, ..., n) matching the graph.

dat_sf <- 
  dat_sf |>
  mutate(region_idx = 1:nrow(dat_sf))


## ---- 6a. Prior specification ------------------------------------------------
## We use the recommended PC-priors (Penalised Complexity) for BYM2.
## PC-priors penalise complexity away from a simpler base model:
##   - hyperpar[1] controls marginal SD of the spatial field
##     P(sigma > 1) = 0.01 means we expect the spatial SD to be < 1 on
##     the log-rate scale
##   - hyperpar[2] controls the structured/unstructured mixing proportion
##     P(phi_prop < 0.5) = 0.5 is a neutral prior — no strong assumption
##     about whether variation is primarily spatial or region-specific

hyper_bym2 <- list(
  prec = list(prior = "pc.prec", param = c(1, 0.01)),
  phi  = list(prior = "pc",      param = c(0.5, 0.5))
)


# Fit model ---------------------------------------------------------------


formula_bym2 <- nb_sejours ~
  niveau_vie_z +
  pct_non_scol_z +
  log_dens_z +
  f(region_idx,
    model        = "bym2",
    graph        = g_inla,
    scale.model  = TRUE,      # scale ICAR for identifiability (Sørbye 2014)
    hyper        = hyper_bym2
  )

bym2_main <- inla(
  formula  = formula_bym2,
  family   = "poisson",
  data     = as.data.frame(dat_sf),
  offset   = log_pop,         # log(total_population) as offset
  control.compute = list(
    dic     = TRUE,           # Deviance Information Criterion (model fit)
    waic    = TRUE,           # Watanabe-AIC (alternative fit measure)
    cpo     = TRUE,           # Conditional Predictive Ordinate (leave-one-out)
    config  = TRUE            # needed for posterior sampling if required later
  ),
  control.predictor = list(compute = TRUE, link = 1)
)

## Fixed effects are the regression coefficients for the covariates.
## Exponentiate to get Rate Ratios (RR): the multiplicative change in the
## expected septic shock rate per 1 SD increase in each predictor.

summary(bym2_main)

fe <- bym2_main$summary.fixed
fe_rr <- exp(fe[, c("mean", "0.025quant", "0.975quant")])
colnames(fe_rr) <- c("RR", "RR_lower95", "RR_upper95")
print(round(fe_rr, 3))


## The hyperparameters tell us:
##   - marginal SD (sigma): total unexplained regional variation
##   - phi proportion: how much of that variation is spatially structured
##     (phi close to 1 = mostly spatial; phi close to 0 = mostly noise)

print(bym2_main$summary.hyperpar)


## The fitted values from INLA are on the linear predictor scale (log-rate).
## We exponentiate and scale by 100,000 to get estimated rates per 100,000.

dat_sf$fitted_rate <- bym2_main$summary.fitted.values$mean /
  dat_sf$total_population * 100000


#  Compute Standardised Morbidity Ratios (SMR) ------------------------
## SMR = observed counts / expected counts
## Expected counts assume a uniform national rate applied to each region's pop.
national_rate <- sum(dat_sf$nb_sejours) / sum(dat_sf$total_population)
dat_sf$expected    <- national_rate * dat_sf$total_population
dat_sf$smr_raw     <- dat_sf$nb_sejours / dat_sf$expected

## Smoothed SMR from BYM2 posterior (shrinks unstable estimates toward mean)
dat_sf$smr_smooth  <- bym2_main$summary.fitted.values$mean / dat_sf$expected


## -xtract spatial random effects -------------------------------------
## The BYM2 spatial random effect (structured component) reveals which regions
## have higher or lower rates than expected AFTER accounting for covariates.
## Positive values = more cases than covariates predict (residual hot spot).

n_regions <- nrow(dat_sf)

## In BYM2, INLA stacks the spatial (ICAR) and unstructured effects.
## Indices 1:n are the combined (total) random effects.
## Indices (n+1):(2n) are the ICAR (structured) component alone.
re_summary <- bym2_main$summary.random$region_idx

dat_sf$re_total      <- re_summary$mean[1:n_regions]
dat_sf$re_structured <- re_summary$mean[(n_regions + 1):(2 * n_regions)]


## VISUALISATION -------------------------------------------------------

## Map: observed vs. smoothed septic shock rate -----------------------

p_obs <- ggplot(dat_sf) +
  geom_sf(aes(fill = tx_sejours_sepsis_100k), colour = "white", linewidth = 0.3) +
  scale_fill_viridis_c(
    name   = "Rate\n(per 100k)",
    option = "plasma",
    direction = -1
  ) +
  labs(title = "Observed septic shock rate") +
  theme_void(base_size = 11) +
  theme(legend.position = "right")

p_smooth <- ggplot(dat_sf) +
  geom_sf(aes(fill = fitted_rate), colour = "white", linewidth = 0.3) +
  scale_fill_viridis_c(
    name   = "Rate\n(per 100k)",
    option = "plasma",
    direction = -1
  ) +
  labs(title = "BYM2 smoothed rate (posterior mean)") +
  theme_void(base_size = 11) +
  theme(legend.position = "right")

p_obs + p_smooth +
  plot_annotation(
    title    = "Septic shock: observed vs. spatially smoothed rates",
    subtitle = "Mainland France + Corse | Main analysis (IDF included)"
  )


## Regions with positive values have unexplained excess risk that clusters
## spatially — i.e. not explained by the three covariates.

ggplot(dat_sf) +
  geom_sf(aes(fill = re_structured), colour = "white", linewidth = 0.3) +
  scale_fill_distiller(
    palette  = "RdBu",
    direction = -1,
    name     = "Spatial RE\n(log scale)"
  ) +
  labs(
    title    = "BYM2 structured spatial random effect",
    subtitle = "Positive = residual excess; Negative = residual deficit"
  ) +
  theme_void(base_size = 11)


##Forest plot: fixed effects (Rate Ratios) ---------------------------

fe_plot_dat <- data.frame(
  predictor = rownames(fe_rr)[-1],  # remove intercept
  RR        = fe_rr[-1, "RR"],
  lo        = fe_rr[-1, "RR_lower95"],
  hi        = fe_rr[-1, "RR_upper95"]
)

## Clean predictor labels
fe_plot_dat$predictor <- recode(fe_plot_dat$predictor,
                                "niveau_vie_z"   = "Median living standard\n(1 SD increase, €/year)",
                                "pct_non_scol_z" = "% pop. ≥15 not in education\n(1 SD increase)",
                                "log_dens_z"     = "Population density (log)\n(1 SD increase)"
)

ggplot(fe_plot_dat, aes(x = RR, y = predictor)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.15, linewidth = 0.8) +
  geom_point(size = 3, colour = "#2166ac") +
  scale_x_log10(breaks = c(0.7, 0.85, 1, 1.15, 1.3)) +
  labs(
    title    = "BYM2 fixed effects: Rate Ratios for septic shock",
    subtitle = "Posterior mean + 95% credible intervals | Main analysis (IDF included)",
    x        = "Rate Ratio (log scale)",
    y        = NULL
  ) +
  theme_bw(base_size = 11)