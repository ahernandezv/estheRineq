# Econometric Need Standardization & Horizontal Inequity Index (`HI`, Gravelle 2003 / O'Donnell et al. 2008)

.eval_ci_allow_overshoot <- function(y_is, r_sub, w_vec, method, bounds, rank_order) {
  rank_res <- compute_fractional_ranks(r_sub, weights = w_vec, rank_order = rank_order)
  w_n <- rank_res$w_norm
  mu <- sum(w_n * y_is)
  rf <- .influence_rank_functional_raw(y_is, rank_res, scheme = "standard")
  theta <- rf$theta
  if (method == "standard") return(theta / mu)
  if (method == "generalized") return(theta)
  if (method == "erreygers") {
    b <- if (is.null(bounds)) c(0.0, 1.0) else bounds
    return(4.0 * theta / (b[2L] - b[1L]))
  }
  if (method == "wagstaff") {
    b <- if (is.null(bounds)) c(0.0, 1.0) else bounds
    return((b[2L] - b[1L]) * theta / ((b[2L] - mu) * (mu - b[1L])))
  }
  influence_concentration_index(y_is, r_sub, weights = w_vec, method = method, bounds = bounds, rank_order = rank_order)$estimate
}

standardize_health_need <- function(
  data,
  outcome,
  need_vars,
  non_need_vars = NULL,
  categorical = NULL,
  family = c("auto", "gaussian", "binomial", "poisson"),
  link = NULL,
  weights = NULL
) {
  family <- match.arg(family)
  df <- .ensure_data_frame(data)
  w_full <- if (inherits(data, "esther_survey")) .get_design_arrays(data)$weights else if (!is.null(weights)) .to_clean_numeric(df[[weights]], weights) else rep(1.0, nrow(df))

  all_covs <- c(need_vars, non_need_vars)
  dm <- .build_design_matrix(
    df, outcome = outcome, covariates = all_covs, categorical = categorical, add_intercept = TRUE
  )
  valid <- dm$valid_mask
  y_sub <- dm$y
  w_sub <- w_full[valid]
  w_norm <- w_sub / sum(w_sub)

  fam <- if (family == "auto") {
    if (all(y_sub %in% c(0.0, 1.0))) "binomial" else "gaussian"
  } else {
    family
  }

  fit <- .fit_survey_glm(y = y_sub, X = dm$X, weights = w_sub, family = fam, link = link)
  X_cf <- dm$X
  if (!is.null(non_need_vars)) {
    for (nn_var in non_need_vars) {
      if (nn_var %in% names(dm$factor_groups)) {
        for (c_idx in dm$factor_groups[[nn_var]]) {
          X_cf[, c_idx] <- sum(w_norm * dm$X[, c_idx])
        }
      }
    }
  }

  y_need <- fit$inv_link(as.double(X_cf %*% fit$params))
  mean_need <- sum(w_norm * y_need)
  y_is <- y_sub - y_need + mean_need

  df$y_need_expected <- NA_real_
  df$y_need_standardized <- NA_real_
  df$y_need_expected[valid] <- y_need
  df$y_need_standardized[valid] <- y_is
  df
}

horizontal_inequity <- function(
  data,
  outcome,
  rank,
  need_vars,
  non_need_vars = NULL,
  categorical = NULL,
  method = "standard",
  bounds = NULL,
  family = c("auto", "gaussian", "binomial", "poisson"),
  link = NULL,
  rank_order = c("asc", "desc"),
  favorable = NULL,
  bootstrap_reps = 200L,
  confidence_level = 0.95,
  seed = 42L,
  weights = NULL,
  strata = NULL,
  psu = NULL,
  fpc = NULL,
  lonely_psu = "remove"
) {
  family <- match.arg(family)
  rank_order <- match.arg(rank_order)
  svy <- .coerce_survey(data, weights = weights, strata = strata, psu = psu, fpc = fpc, lonely_psu = lonely_psu)
  df <- svy$df
  if (!(rank %in% names(df))) stop(sprintf("`rank = '%s'` not found in `data`.", rank), call. = FALSE)

  all_covs <- c(need_vars, non_need_vars)
  dm <- .build_design_matrix(
    df, outcome = outcome, covariates = all_covs, categorical = categorical,
    add_intercept = TRUE, extra_cols = rank
  )
  valid <- dm$valid_mask
  y_sub <- dm$y
  r_sub <- .to_rank_numeric(df[[rank]][valid], rank)
  d_arr <- .get_design_arrays(svy)
  w_sub <- d_arr$weights[valid]

  fam <- if (family == "auto") {
    if (all(y_sub %in% c(0.0, 1.0))) "binomial" else "gaussian"
  } else {
    family
  }
  mc <- .canonical_ci_method(method)
  eff_b <- if (mc %in% c("wagstaff", "erreygers") && is.null(bounds)) c(0.0, 1.0) else bounds

  nn_cols <- integer(0)
  if (!is.null(non_need_vars)) {
    for (nn_v in non_need_vars) {
      if (nn_v %in% names(dm$factor_groups)) {
        nn_cols <- c(nn_cols, dm$factor_groups[[nn_v]])
      }
    }
  }

  eval_triple <- function(w_vec) {
    w_n <- w_vec / sum(w_vec)
    fit <- .fit_survey_glm(y = y_sub, X = dm$X, weights = w_vec, family = fam, link = link)
    X_cf <- dm$X
    for (c_i in nn_cols) {
      X_cf[, c_i] <- sum(w_n * dm$X[, c_i])
    }
    y_need <- fit$inv_link(as.double(X_cf %*% fit$params))
    y_is <- y_sub - y_need + sum(w_n * y_need)

    ci_u <- influence_concentration_index(y_sub, r_sub, weights = w_vec, method = mc, bounds = eff_b, rank_order = rank_order)$estimate
    ci_n <- influence_concentration_index(y_need, r_sub, weights = w_vec, method = mc, bounds = eff_b, rank_order = rank_order)$estimate
    hi_v <- .eval_ci_allow_overshoot(y_is, r_sub, w_vec, method = mc, bounds = eff_b, rank_order = rank_order)
    c(ci_u, ci_n, hi_v)
  }

  pt <- eval_triple(w_sub)
  if (bootstrap_reps >= 10L) {
    bsum <- rao_wu_survey_bootstrap(
      estimator_fn = eval_triple,
      weights = w_sub,
      strata = if (is.null(d_arr$strata)) NULL else d_arr$strata[valid],
      psu = if (is.null(d_arr$psu)) NULL else d_arr$psu[valid],
      n_boot = bootstrap_reps,
      seed = seed
    )
    se_vec <- bsum$se
    df_deg <- bsum$df
    n_st <- bsum$n_strata
    n_ps <- bsum$n_psu
  } else {
    psi_hi <- influence_concentration_index(y_sub, r_sub, weights = w_sub, method = mc, bounds = eff_b, rank_order = rank_order)$psi
    se_ap <- sqrt(sum(((w_sub / sum(w_sub)) * psi_hi)^2))
    se_vec <- c(se_ap, NA_real_, se_ap)
    df_deg <- as.double(max(length(y_sub) - 1L, 1L))
    n_st <- 1L; n_ps <- length(y_sub)
  }

  crit <- stats::qt(1.0 - 0.5 * (1.0 - confidence_level), df = df_deg)
  lbls <- c(
    sprintf("Unstandardized %s concentration index", mc),
    sprintf("Need-expected %s concentration index", mc),
    sprintf("Horizontal inequity index, %s", mc)
  )
  ci_l <- pt - crit * se_vec
  ci_h <- pt + crit * se_vec
  t_v <- ifelse(se_vec > 0, pt / se_vec, NA_real_)
  p_v <- 2.0 * stats::pt(-abs(t_v), df = df_deg)
  interps <- vapply(1:3, function(k) .interpret_gradient_sign(pt[k], ci_l[k], ci_h[k], 0.0, rank_order), character(1L))

  tbl <- data.frame(
    measure = lbls,
    estimate = pt,
    se = se_vec,
    ci_low = ci_l,
    ci_high = ci_h,
    p_value = p_v,
    n_obs = rep(sum(valid), 3L),
    sum_weights = rep(sum(w_sub), 3L),
    interpretation = interps,
    stringsAsFactors = FALSE
  )

  new_esther_result(
    table = tbl,
    title = sprintf("Need standardization and horizontal inequity of %s by %s", outcome, rank),
    metadata = list(
      outcome = outcome,
      rank = rank,
      measures = lbls,
      need_vars = need_vars,
      non_need_vars = non_need_vars,
      favorable = favorable,
      rank_order = rank_order,
      data_mode = "survey",
      vce_method = "bootstrap",
      n_obs = sum(valid),
      sum_weights = sum(w_sub),
      n_strata = n_st,
      n_psu = n_ps,
      df = df_deg,
      confidence_level = confidence_level
    )
  )
}
