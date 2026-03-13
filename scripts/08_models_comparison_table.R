library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# ============================================================
# ASSUMPTIONS ABOUT YOUR OBJECT NAMES
# Replace these with your actual object names if different:
#   moran_result   <- moran.mc(...)
#   lisa_result    <- your LISA tibble (13 x 6)
#   ols_fit        <- lm(...)
#   lm_tests       <- lm.RStests(ols_fit, listw = w_listw, test = "all")
#   ridge_cv       <- cv.glmnet(...)
#   bym2_fit       <- inla(...)
# ============================================================

moran_result <- readRDS("./data/derived/06_moran_mc_result.RDS")
lisa_result <- readRDS("./data/derived/06_LISA_results.RDS")
ols_fit <- readRDS("./data/derived/06_ols_model.RDS")
lm_tests <- readRDS("./data/derived/06_lm_tests.RDS")
ridge_cv <- readRDS("./data/derived/06_ridge_model.RDS")
bym2_fit <- readRDS("./data/derived/07_BYM2_model.RDS")

# ------------------------------------------------------------
# 1. GLOBAL MORAN'S I
# ------------------------------------------------------------
morans_stat <- round(moran_result$statistic, 3)
morans_pval <- round(moran_result$p.value, 3)
morans_n    <- length(moran_result$res)  # number of simulations

moran_row <- data.frame(
  Method     = "Global Moran's I",
  Purpose    = "Test for global spatial clustering in sepsis rates",
  Key_Result = paste0(
    "I = ", morans_stat,
    ", p = ", morans_pval,
    " (Monte Carlo, ", morans_n, " permutations)"
  )
)


# ------------------------------------------------------------
# 2. LISA
# ------------------------------------------------------------
# Extract significant clusters only
lisa_sig <- lisa_result %>%
  filter(quad_sig != "Not significant") %>%
  mutate(summary = paste0(NOM_M, ": ", quad_sig, " (p = ", round(local_pval, 3), ")"))

# Extract borderline cases (non-significant but noteworthy)
lisa_border <- lisa_result %>%
  filter(NOM_M == "ILE-DE-FRANCE") %>%
  mutate(summary = paste0(NOM_M, ": ", quad_sig, " pattern (p = ", round(local_pval, 3), ", ns)"))

lisa_summary <- paste(
  c(lisa_sig$summary, lisa_border$summary),
  collapse = "; "
)

lisa_row <- data.frame(
  Method     = "LISA",
  Purpose    = "Detect local spatial clusters or outliers",
  Key_Result = lisa_summary
)


# ------------------------------------------------------------
# 3. OLS REGRESSION
# ------------------------------------------------------------
ols_summary  <- summary(ols_fit)
ols_fstat    <- round(ols_summary$fstatistic["value"], 2)
ols_fdf1     <- ols_summary$fstatistic["numdf"]
ols_fdf2     <- ols_summary$fstatistic["dendf"]
ols_fpval    <- round(pf(
  ols_summary$fstatistic["value"],
  ols_summary$fstatistic["numdf"],
  ols_summary$fstatistic["dendf"],
  lower.tail = FALSE
), 3)
ols_adjr2    <- round(ols_summary$adj.r.squared, 3)

# Extract individual predictor p-values
ols_coefs <- as.data.frame(ols_summary$coefficients) %>%
  filter(rownames(.) != "(Intercept)") %>%
  mutate(
    term    = rownames(.),
    p_label = paste0(term, ": p = ", round(`Pr(>|t|)`, 3))
  )

ols_coef_summary <- paste(ols_coefs$p_label, collapse = "; ")

ols_row <- data.frame(
  Method     = "OLS Regression",
  Purpose    = "Estimate effect of socioeconomic predictors on sepsis rates",
  Key_Result = paste0(
    "F(", ols_fdf1, ",", ols_fdf2, ") = ", ols_fstat,
    ", p = ", ols_fpval,
    "; Adjusted R² = ", ols_adjr2,
    ". Coefficients — ", ols_coef_summary
  )
)


# ------------------------------------------------------------
# 4. LAGRANGE MULTIPLIER DIAGNOSTICS
# ------------------------------------------------------------
# lm.RStests returns a list — extract each test by name
lm_err     <- round(lm_tests$RSerr$statistic, 3)
lm_err_p   <- round(lm_tests$RSerr$p.value, 3)
lm_lag     <- round(lm_tests$RSlag$statistic, 3)
lm_lag_p   <- round(lm_tests$RSlag$p.value, 3)
lm_rerr    <- round(lm_tests$adjRSerr$statistic, 3)
lm_rerr_p  <- round(lm_tests$adjRSerr$p.value, 3)
lm_rlag    <- round(lm_tests$adjRSlag$statistic, 3)
lm_rlag_p  <- round(lm_tests$adjRSlag$p.value, 3)

lm_row <- data.frame(
  Method     = "LM Diagnostics",
  Purpose    = "Test for residual spatial dependence after OLS",
  Key_Result = paste0(
    "LM-Error = ", lm_err, " (p = ", lm_err_p, "); ",
    "LM-Lag = ", lm_lag, " (p = ", lm_lag_p, "); ",
    "Robust LM-Error = ", lm_rerr, " (p = ", lm_rerr_p, "); ",
    "Robust LM-Lag = ", lm_rlag, " (p = ", lm_rlag_p, ")"
  )
)


# ------------------------------------------------------------
# 5. RIDGE REGRESSION
# ------------------------------------------------------------
best_lambda   <- round(ridge_cv$lambda.min, 3)
ridge_coefs   <- as.matrix(coef(ridge_cv, s = "lambda.min"))
ridge_df      <- data.frame(
  term  = rownames(ridge_coefs),
  coef  = round(ridge_coefs[, 1], 4)
) %>%
  filter(term != "(Intercept)") %>%
  mutate(label = paste0(term, " = ", coef))

ridge_coef_summary <- paste(ridge_df$label, collapse = "; ")

ridge_row <- data.frame(
  Method     = "Ridge Regression",
  Purpose    = "Assess whether multicollinearity explains weak OLS signal",
  Key_Result = paste0(
    "Optimal λ = ", best_lambda,
    ". Coefficients — ", ridge_coef_summary
  )
)


# ------------------------------------------------------------
# 6. BYM2 MODEL
# ------------------------------------------------------------
fe      <- bym2_fit$summary.fixed
hyper   <- bym2_fit$summary.hyperpar
waic    <- round(bym2_fit$waic$waic, 2)
dic     <- round(bym2_fit$dic$dic, 2)

# Fixed effects as Rate Ratios
fe_table <- fe %>%
  filter(rownames(.) != "(Intercept)") %>%
  mutate(
    term  = rownames(.),
    label = paste0(
      term,
      ": RR = ", round(exp(mean), 3),
      " [", round(exp(`0.025quant`), 3),
      " – ", round(exp(`0.975quant`), 3), "]"
    )
  )

fe_summary <- paste(fe_table$label, collapse = "; ")

# Hyperparameters
precision_mean <- round(hyper["Precision for region_idx", "mean"], 2)
phi_mean       <- round(hyper["Phi for region_idx", "mean"], 3)
phi_lower      <- round(hyper["Phi for region_idx", "0.025quant"], 3)
phi_upper      <- round(hyper["Phi for region_idx", "0.975quant"], 3)

bym2_row <- data.frame(
  Method     = "BYM2 Model",
  Purpose    = "Bayesian spatial model with Poisson likelihood and uncertainty quantification",
  Key_Result = paste0(
    "Fixed effects (RR, 95% CI) — ", fe_summary,
    ". Precision = ", precision_mean,
    "; φ = ", phi_mean,
    " (95% CI: ", phi_lower, " – ", phi_upper, ")",
    "; WAIC = ", waic,
    "; DIC = ", dic
  )
)


# ============================================================
# ASSEMBLE FULL TABLE
# ============================================================
comparison_table <- bind_rows(
  moran_row,
  lisa_row,
  ols_row,
  lm_row,
  ridge_row,
  bym2_row
)

saveRDS(comparison_table, "./data/derived/08_model_comparison_table.RDS")
