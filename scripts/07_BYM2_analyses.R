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



# ------------------------------------------------------------
# EXTRACT POSTERIOR SUMMARIES FROM BYM2
# ------------------------------------------------------------
bym2_fit <- bym2_main

# Fitted values (on the response scale — expected counts)
fitted_df <- data.frame(
  region_idx = seq_len(nrow(bym2_fit$summary.fitted.values)),
  fitted_mean  = bym2_fit$summary.fitted.values$mean,
  fitted_lower = bym2_fit$summary.fitted.values$`0.025quant`,
  fitted_upper = bym2_fit$summary.fitted.values$`0.975quant`
)

# Random effects (BYM2 spatial + unstructured combined)
re_df <- data.frame(
  region_idx = seq_len(nrow(bym2_fit$summary.random$region_idx)),
  re_mean    = bym2_fit$summary.random$region_idx$mean,
  re_lower   = bym2_fit$summary.random$region_idx$`0.025quant`,
  re_upper   = bym2_fit$summary.random$region_idx$`0.975quant`
) %>%
  # BYM2 returns 2*n rows: first n = combined, second n = spatial only
  # Keep only first n rows (combined random effect)
  slice(seq_len(nrow(shock_combined)))

# Compute posterior relative risk per region
# RR = exp(random effect) — deviation from overall mean
re_df <- re_df %>%
  mutate(
    RR       = exp(re_mean),
    RR_lower = exp(re_lower),
    RR_upper = exp(re_upper),
    # Exceedance probability: P(RR > 1) — computed from marginals
    # Approximated here as proportion of CI above 0 on log scale
    above_null = as.numeric(re_lower > 0)  # 1 if 95% CI excludes 0
  )

# Attach region names from your data
re_df$region_name <- model_dat$region_name


# Attach region names from your data
re_df$region_name <- model_dat$region_name

# Join to spatial object
map_df <- 
  model_dat %>%
  left_join(re_df, by = c("region_name" = "region_name"))


saveRDS(map_df, "./data/derived/07_bym2_spatial.RDS")
saveRDS(re_df, "./data/derived/07_bym2_re.RDS")

## VISUALISATION -------------------------------------------------------
# ------------------------------------------------------------
# PLOT 1 — Choropleth map of posterior RR
# THE standard plot in disease mapping papers
# ------------------------------------------------------------

p1 <- ggplot(map_df) +
  geom_sf(aes(fill = RR), colour = "white", linewidth = 0.3) +
  scale_fill_gradient2(
    low      = "#2166AC",   # blue = lower than average
    mid      = "#F7F7F7",   # white = average
    high     = "#D6604D",   # red = higher than average
    midpoint = 1,
    name     = "Posterior\nRR",
    limits   = c(
      floor(min(map_df$RR, na.rm = TRUE) * 10) / 10,
      ceiling(max(map_df$RR, na.rm = TRUE) * 10) / 10
    )
  ) +
  labs(
    title    = "Posterior relative risk of sepsis hospitalisation",
    subtitle = "BYM2 model — metropolitan France. RR > 1 (red) = above average; RR < 1 (blue) = below average",
    caption  = "Overseas territories excluded. Spatial weights: queen contiguity."
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(colour = "grey50", size = 10),
    plot.caption  = element_text(colour = "grey60", size = 8),
    legend.position = "right"
  )



# ------------------------------------------------------------
# PLOT 2 — Exceedance probability map P(RR > 1)
# Shows where risk is above average
# ------------------------------------------------------------

# Compute P(RR > 1) = P(random effect > 0) from posterior marginals
exceed_prob <- sapply(
  seq_len(nrow(model_dat)),
  function(i) {
    marg <- bym2_fit$marginals.random$region_idx[[i]]
    # inla.pmarginal gives P(x < threshold)
    # P(x > 0) = 1 - P(x < 0)
    1 - INLA::inla.pmarginal(0, marg)
  }
)

map_df$exceed_prob <- exceed_prob

p2 <- ggplot(map_df) +
  geom_sf(aes(fill = exceed_prob), colour = "white", linewidth = 0.3) +
  scale_fill_gradient(
    low  = "#F7F7F7",
    high = "#D6604D",
    name = "P(RR > 1)",
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 0.8, 0.95, 1)
  ) +
  # Add hatching for regions where P(RR>1) > 0.8
  geom_sf(
    data   = filter(map_df, exceed_prob > 0.8),
    fill   = NA,
    colour = "grey30",
    linewidth = 0.8,
    linetype  = "solid"
  ) +
  labs(
    title    = "Posterior exceedance probability P(RR > 1)",
    subtitle = "BYM2 model — probability that a region's sepsis rate exceeds the national average.\nBold borders = P(RR > 1) > 0.80",
    caption  = "Overseas territories excluded."
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(colour = "grey50", size = 10),
    plot.caption  = element_text(colour = "grey60", size = 8),
    legend.position = "right"
  )


# ------------------------------------------------------------
# PLOT 3 — Caterpillar plot of posterior RR by region
# Shows all regions ranked by RR with uncertainty intervals
# ------------------------------------------------------------

re_plot_df <- re_df %>%
  mutate(
    region_name = reorder(region_name, RR),
    sig = case_when(
      RR_lower > 1 ~ "Above average",
      RR_upper < 1 ~ "Below average",
      TRUE         ~ "Inconclusive"
    )
  )

p3 <- ggplot(re_plot_df,
             aes(x = RR, y = region_name, colour = sig)) +
  geom_vline(xintercept = 1,
             linetype = "dashed", colour = "grey50", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = RR_lower, xmax = RR_upper),
                 height = 0.3, linewidth = 0.7) +
  geom_point(size = 3) +
  scale_colour_manual(
    values = c(
      "Above average" = "#D6604D",
      "Below average" = "#2166AC",
      "Inconclusive"  = "grey50"
    )
  ) +
  labs(
    title    = "Posterior relative risk by region — BYM2 model",
    subtitle = "Points = posterior mean RR; bars = 95% credible intervals.\nDashed line = national average (RR = 1)",
    x        = "Relative Risk",
    y        = NULL,
    colour   = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(colour = "grey50", size = 10),
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.y      = element_text(size = 10)
  )



# ------------------------------------------------------------
# PLOT 4 — Posterior distributions of fixed effects
# Shows full uncertainty for each predictor — Bayesian equivalent
# of the OLS/Ridge coefficient forest plot
# ------------------------------------------------------------

# Extract marginals for each fixed effect
fe_names <- rownames(bym2_main$summary.fixed)
fe_names <- fe_names[fe_names != "(Intercept)"]

fe_marginals <- lapply(fe_names, function(nm) {
  marg <- bym2_fit$marginals.fixed[[nm]]
  df   <- as.data.frame(marg)
  names(df) <- c("x", "density")
  df$Predictor <- nm
  df
})

fe_df <- bind_rows(fe_marginals) %>%
  mutate(
    Predictor = recode(Predictor,
                       niveau_vie_z             = "Median living standard",
                       intensite_pauvrete_pct_z = "Poverty intensity",
                       pct_non_scol_z           = "Low educational attainment",
                       log_dens_z               = "Population density (log)"
    ),
    # Flag area under curve to left or right of zero
    side = ifelse(x < 0, "negative", "positive")
  )

p4 <- ggplot(fe_df, aes(x = x, y = density)) +
  geom_area(aes(fill = side), alpha = 0.4) +
  geom_line(linewidth = 0.7, colour = "grey30") +
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  scale_fill_manual(
    values = c("negative" = "#2166AC", "positive" = "#D6604D"),
    guide  = "none"
  ) +
  facet_wrap(~ Predictor, scales = "free", ncol = 2) +
  labs(
    title    = "Posterior distributions of fixed effects — BYM2 model",
    subtitle = "Blue = negative effect region; red = positive effect region.\nDashed line = null (log RR = 0)",
    x        = "log Rate Ratio",
    y        = "Posterior density"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(colour = "grey50", size = 10),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold", size = 10)
  )


# ------------------------------------------------------------
# SAVE ALL PLOTS
# ------------------------------------------------------------
ggsave("./outputs/figures/bym2_rr_map.png",
       plot = p1, width = 8, height = 7, dpi = 150)

ggsave("./outputs/figures/bym2_exceedance_map.png",
       plot = p2, width = 8, height = 7, dpi = 150)

ggsave("./outputs/figures/bym2_caterpillar.png",
       plot = p3, width = 7, height = 5, dpi = 150)

ggsave("./outputs/figures/bym2_posterior_fe.png",
       plot = p4, width = 8, height = 6, dpi = 150)

ggsave("./outputs/figures/bym2_combined_maps.png",
       plot = combined_map, width = 14, height = 7, dpi = 150)