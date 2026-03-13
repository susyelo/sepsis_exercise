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

model_dat <- 
  readRDS("./data/derived/06_spatial_all_dat.RDS") %>% 
  mutate(region_idx = 1:nrow(dat_sf))

### load adjacency graph
g_inla <- inla.read.graph("./data/derived/06_france_regions_nb.graph")


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
  intensite_pauvrete_pct_z+
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
  data     = as.data.frame(model_dat),
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

# --- Build the table ---
r
# Build table
results_table <- data.frame(
  Predictor    = rownames(fe),
  Mean_logRR   = round(fe$mean, 3),
  CI_lower     = round(fe$`0.025quant`, 3),
  CI_upper     = round(fe$`0.975quant`, 3),
  RR           = round(exp(fe$mean), 3)
) %>%
  filter(Predictor != "(Intercept)") %>%
  mutate(
    `95% Credible Interval` = paste0("[", CI_lower, " – ", CI_upper, "]"),
    Predictor = recode(Predictor,
                       niveau_vie_z             = "Median living standard (z)",
                       intensite_pauvrete_pct_z = "Poverty intensity (z)",
                       pct_non_scol_z           = "Low educational attainment (z)",
                       log_dens_z               = "Population density log (z)"
    )
  ) %>%
  select(Predictor, Mean_logRR, `95% Credible Interval`, RR)

##save results

saveRDS(results_table, "./data/derived/07_BYM2_model_results.RDS")
saveRDS(bym2_main, "./data/derived/07_BYM2_model.RDS")


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

