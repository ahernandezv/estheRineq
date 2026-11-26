# Inequality Decompositions: Wagstaff (2003), Bivariate RIF-I-OLS (Heckley 2016), and RIF-Oaxaca (Rios-Avila 2020)

decompose_wagstaff <- function(
  data,
  rank,
  formula = NULL,
  outcome = NULL,
  covariates = NULL,
  categorical = NULL,
  method = "erreygers",
  bounds = NULL,
  family = "gaussian",
  link = NULL,
  marginal = c("ame", "mem"),
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
  marginal <- match.arg(marginal)
  rank_order <- match.arg(rank_order)
  svy <- .coerce_survey(data, weights = weights, strata = strata, psu = psu, fpc = fpc, lonely_psu = lonely_psu)
  df <- svy$df
  if (!(rank %in% names(df))) stop(sprintf("`rank = '%s'` not found in `data`.", rank), call. = FALSE)

  dm <- .build_design_matrix(
    df, formula = formula, outcome = outcome, covariates = covariates,
    categorical = categorical, add_intercept = TRUE, extra_cols = rank
  )
  if (is.null(dm$y) || is.null(dm$outcome_name)) {
    stop("Specify outcome via `formula = y ~ ...` or `outcome = 'y'`.", call. = FALSE)
  }

  valid <- dm$valid_mask
  y_sub <- dm$y
  X_sub <- dm$X
  r_sub <- .to_rank_numeric(df[[rank]][valid], rank)
  d_arr <- .get_design_arrays(svy)
  w_sub <- d_arr$weights[valid]

  mc <- .canonical_ci_method(method)
  eff_b <- if (mc %in% c("wagstaff", "erreygers") && is.null(bounds)) c(0.0, 1.0) else bounds
  factor_names <- setdiff(names(dm$factor_groups), "Intercept")
  k_fac <- length(factor_names)

  eval_decomp <- function(w_vec) {
    w_n <- w_vec / sum(w_vec)
    mu_y <- sum(w_n * y_sub)
    tot_ci <- influence_concentration_index(
      y_sub, r_sub, weights = w_vec, method = mc, bounds = eff_b, rank_order = rank_order
    )$estimate

    fit <- .fit_survey_glm(y = y_sub, X = X_sub, weights = w_vec, family = family, link = link)
    is_linear <- (fit$family == "gaussian" && fit$link == "identity")
    if (is_linear) {
      slopes <- fit$params
    } else if (marginal == "ame") {
      avg_dmu <- sum(w_n * fit$dmu_deta)
      slopes <- fit$params * avg_dmu
    } else {
      x_bar <- colSums(X_sub * w_n)
      eta_bar <- sum(x_bar * fit$params)
      mu_bar <- fit$inv_link(eta_bar)
      dmu_bar <- fit$deriv_inv_link(eta_bar, mu_bar)
      slopes <- fit$params * dmu_bar
    }

    if (mc == "standard") {
      scale_m <- 1.0 / mu_y
    } else if (mc == "generalized") {
      scale_m <- 1.0
    } else if (mc == "modified") {
      scale_m <- 1.0 / (mu_y - eff_b[1L])
    } else if (mc == "erreygers") {
      scale_m <- 4.0 / (eff_b[2L] - eff_b[1L])
    } else if (mc == "wagstaff") {
      scale_m <- (eff_b[2L] - eff_b[1L]) / ((eff_b[2L] - mu_y) * (mu_y - eff_b[1L]))
    } else {
      scale_m <- 1.0 / mu_y
    }

    rank_res <- compute_fractional_ranks(r_sub, weights = w_vec, rank_order = rank_order)
    contribs <- numeric(k_fac)
    elasts <- numeric(k_fac)
    fac_cis <- numeric(k_fac)

    for (j in seq_len(k_fac)) {
      cols_j <- dm$factor_groups[[factor_names[j]]]
      s_f <- as.double(X_sub[, cols_j, drop = FALSE] %*% slopes[cols_j])
      mean_sf <- sum(w_n * s_f)
      theta_sf <- .influence_rank_functional_raw(s_f, rank_res, scheme = "standard")$theta
      contribs[j] <- scale_m * theta_sf
      elasts[j] <- if (abs(mu_y) > 1e-15) (mean_sf / mu_y) else 0.0
      fac_cis[j] <- if (abs(mean_sf) > 1e-15) (theta_sf / mean_sf) else 0.0
    }

    explained <- sum(contribs)
    residual <- tot_ci - explained
    c(tot_ci, explained, residual, contribs, elasts, fac_cis)
  }

  pt <- eval_decomp(w_sub)
  if (bootstrap_reps >= 10L) {
    bsum <- rao_wu_survey_bootstrap(
      estimator_fn = eval_decomp,
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
    vce_used <- "bootstrap"
  } else {
    se_vec <- rep(NA_real_, length(pt))
    df_deg <- as.double(max(sum(valid) - ncol(X_sub), 1L))
    n_st <- 1L; n_ps <- sum(valid)
    vce_used <- "point_only"
  }

  tot_est <- pt[1L]
  exp_est <- pt[2L]
  res_est <- pt[3L]
  crit <- stats::qt(1.0 - 0.5 * (1.0 - confidence_level), df = df_deg)

  c_vals <- pt[4:(3 + k_fac)]
  e_vals <- pt[(4 + k_fac):(3 + 2 * k_fac)]
  fc_vals <- pt[(4 + 2 * k_fac):(3 + 3 * k_fac)]
  se_c <- se_vec[4:(3 + k_fac)]
  t_c <- ifelse(is.finite(se_c) & se_c > 0, c_vals / se_c, NA_real_)
  p_c <- 2.0 * stats::pt(-abs(t_c), df = df_deg)

  fac_df <- data.frame(
    factor = c(factor_names, "Residual"),
    elasticity = c(e_vals, NA_real_),
    factor_ci = c(fc_vals, NA_real_),
    contribution = c(c_vals, res_est),
    pct_share = if (abs(tot_est) > 1e-15) (c(c_vals, res_est) / tot_est) * 100.0 else rep(NA_real_, k_fac + 1L),
    se = c(se_c, se_vec[3L]),
    ci_low = c(c_vals - crit * se_c, res_est - crit * se_vec[3L]),
    ci_high = c(c_vals + crit * se_c, res_est + crit * se_vec[3L]),
    p_value = c(p_c, NA_real_),
    stringsAsFactors = FALSE
  )

  ov_df <- data.frame(
    measure = c(
      sprintf("Total %s", .ci_method_label(mc)),
      "Explained by model variables",
      "Unexplained residual"
    ),
    estimate = c(tot_est, exp_est, res_est),
    pct_share = c(100.0, if (abs(tot_est) > 1e-15) (exp_est / tot_est) * 100.0 else NA_real_, if (abs(tot_est) > 1e-15) (res_est / tot_est) * 100.0 else NA_real_),
    se = se_vec[1:3],
    stringsAsFactors = FALSE
  )

  new_esther_decomp(
    contributions = fac_df,
    overall_summary = ov_df,
    title = sprintf("Wagstaff inequality decomposition of %s by %s", dm$outcome_name, rank),
    metadata = list(
      outcome = dm$outcome_name,
      rank = rank,
      measures = sprintf("Wagstaff decomposition, %s", mc),
      favorable = favorable,
      rank_order = rank_order,
      data_mode = "survey",
      vce_method = vce_used,
      n_obs = sum(valid),
      sum_weights = sum(w_sub),
      n_strata = n_st,
      n_psu = n_ps,
      df = df_deg,
      confidence_level = confidence_level
    )
  )
}

decompose_rif <- function(
  data,
  rank,
  formula = NULL,
  outcome = NULL,
  covariates = NULL,
  categorical = NULL,
  method = "erreygers",
  bounds = NULL,
  rank_order = c("asc", "desc"),
  favorable = NULL,
  vce = c("if", "bootstrap"),
  bootstrap_reps = 200L,
  confidence_level = 0.95,
  seed = 42L,
  weights = NULL,
  strata = NULL,
  psu = NULL,
  fpc = NULL,
  lonely_psu = "remove"
) {
  rank_order <- match.arg(rank_order)
  vce <- match.arg(vce)
  svy <- .coerce_survey(data, weights = weights, strata = strata, psu = psu, fpc = fpc, lonely_psu = lonely_psu)
  df <- svy$df
  if (!(rank %in% names(df))) stop(sprintf("`rank = '%s'` not found in `data`.", rank), call. = FALSE)

  dm <- .build_design_matrix(
    df, formula = formula, outcome = outcome, covariates = covariates,
    categorical = categorical, add_intercept = TRUE, extra_cols = rank
  )
  valid <- dm$valid_mask
  y_sub <- dm$y
  X_sub <- dm$X
  r_sub <- .to_rank_numeric(df[[rank]][valid], rank)
  d_arr <- .get_design_arrays(svy)
  w_full <- d_arr$weights
  w_sub <- w_full[valid]
  w_norm <- w_sub / sum(w_sub)

  mc <- .canonical_ci_method(method)
  eff_b <- if (mc %in% c("wagstaff", "erreygers") && is.null(bounds)) c(0.0, 1.0) else bounds

  ci_res <- influence_concentration_index(
    y_sub, r_sub, weights = w_sub, method = mc, bounds = eff_b, rank_order = rank_order
  )
  idx_est <- ci_res$estimate
  rif_vec <- compute_rif(idx_est, ci_res$psi)

  psi_idx_full <- numeric(nrow(df))
  psi_idx_full[valid] <- ci_res$psi
  vsum_idx <- ultimate_cluster_variance(
    psi = psi_idx_full, weights = w_full, strata = d_arr$strata, psu = d_arr$psu,
    fpc = d_arr$fpc, domain_mask = valid, lonely_psu = svy$lonely_psu
  )

  fit_rif <- .fit_survey_glm(
    y = rif_vec, X = X_sub, weights = w_sub, family = "gaussian", link = "identity",
    vcov_fn = function(psi_sub) {
      p_full <- matrix(0.0, nrow = nrow(df), ncol = ncol(psi_sub))
      p_full[valid, ] <- psi_sub
      ultimate_cluster_variance(
        psi = p_full, weights = w_full, strata = d_arr$strata, psu = d_arr$psu,
        fpc = d_arr$fpc, domain_mask = valid, lonely_psu = svy$lonely_psu
      )$vcov
    }
  )

  if (vce == "bootstrap") {
    bsum <- rao_wu_survey_bootstrap(
      estimator_fn = function(wv) {
        c_b <- influence_concentration_index(y_sub, r_sub, weights = wv, method = mc, bounds = eff_b, rank_order = rank_order)
        r_b <- compute_rif(c_b$estimate, c_b$psi)
        .fit_survey_glm(r_b, X_sub, weights = wv, family = "gaussian", link = "identity")$params
      },
      weights = w_sub,
      strata = if (is.null(d_arr$strata)) NULL else d_arr$strata[valid],
      psu = if (is.null(d_arr$psu)) NULL else d_arr$psu[valid],
      n_boot = bootstrap_reps,
      seed = seed
    )
    cov_beta <- bsum$vcov
    se_beta <- bsum$se
    vce_used <- "bootstrap"
  } else {
    cov_beta <- fit_rif$cov_params
    se_beta <- fit_rif$se
    vce_used <- "if"
  }

  df_deg <- vsum_idx$df
  crit <- stats::qt(1.0 - 0.5 * (1.0 - confidence_level), df = df_deg)
  x_bar <- colSums(X_sub * w_norm)
  b_hat <- fit_rif$params
  t_v <- ifelse(se_beta > 0, b_hat / se_beta, NA_real_)
  p_v <- 2.0 * stats::pt(-abs(t_v), df = df_deg)
  mean_c <- x_bar * b_hat

  contribs <- data.frame(
    factor = dm$col_names,
    rif_marginal_effect = b_hat,
    se = se_beta,
    ci_low = b_hat - crit * se_beta,
    ci_high = b_hat + crit * se_beta,
    p_value = p_v,
    covariate_mean = x_bar,
    contribution = mean_c,
    pct_share = if (abs(idx_est) > 1e-15) (mean_c / idx_est) * 100.0 else rep(NA_real_, length(b_hat)),
    stringsAsFactors = FALSE
  )

  ov_nms <- sprintf("Target %s index", mc)
  ov_est <- idx_est
  ov_se <- vsum_idx$se[1L]
  for (fnm in names(dm$factor_groups)) {
    cols_f <- dm$factor_groups[[fnm]]
    g_vec <- numeric(length(b_hat))
    g_vec[cols_f] <- x_bar[cols_f]
    ov_nms <- c(ov_nms, sprintf("Factor contribution: %s", fnm))
    ov_est <- c(ov_est, sum(g_vec * b_hat))
    ov_se <- c(ov_se, sqrt(max(as.double(t(g_vec) %*% cov_beta %*% g_vec), 0.0)))
  }

  new_esther_decomp(
    contributions = contribs,
    overall_summary = data.frame(component = ov_nms, estimate = ov_est, se = ov_se, stringsAsFactors = FALSE),
    title = sprintf("RIF regression decomposition of %s by %s", dm$outcome_name, rank),
    metadata = list(
      outcome = dm$outcome_name,
      rank = rank,
      measures = sprintf("RIF regression decomposition, %s", mc),
      favorable = favorable,
      rank_order = rank_order,
      data_mode = "survey",
      vce_method = vce_used,
      n_obs = sum(valid),
      sum_weights = sum(w_sub),
      n_strata = vsum_idx$n_strata,
      n_psu = vsum_idx$n_psu,
      df = df_deg,
      confidence_level = confidence_level
    )
  )
}

decompose_oaxaca_rif <- function(
  data,
  rank,
  group,
  formula = NULL,
  outcome = NULL,
  covariates = NULL,
  categorical = NULL,
  group_levels = NULL,
  method = "erreygers",
  bounds = NULL,
  reweight = FALSE,
  rank_order = c("asc", "desc"),
  favorable = NULL,
  bootstrap_reps = 200L,
  confidence_level = 0.95,
  seed = 42L,
  weights = NULL,
  strata = NULL,
  psu = NULL,
  fpc = NULL
) {
  rank_order <- match.arg(rank_order)
  svy <- .coerce_survey(data, weights = weights, strata = strata, psu = psu, fpc = fpc)
  df <- svy$df
  for (col in c(rank, group)) {
    if (!(col %in% names(df))) stop(sprintf("Column `%s` not found in `data`.", col), call. = FALSE)
  }

  dm <- .build_design_matrix(
    df, formula = formula, outcome = outcome, covariates = covariates,
    categorical = categorical, add_intercept = TRUE, extra_cols = c(rank, group)
  )
  valid <- dm$valid_mask
  y_sub <- dm$y
  X_sub <- dm$X
  r_sub <- .to_rank_numeric(df[[rank]][valid], rank)
  g_raw <- as.character(df[[group]][valid])
  d_arr <- .get_design_arrays(svy)
  w_sub <- d_arr$weights[valid]

  if (!is.null(group_levels)) {
    g0 <- as.character(group_levels[1L]); g1 <- as.character(group_levels[2L])
  } else {
    uq <- sort(unique(g_raw))
    if (length(uq) != 2L) {
      stop(sprintf("`group = '%s'` has %d levels; provide binary group or `group_levels = c(g0, g1)`.", group, length(uq)), call. = FALSE)
    }
    g0 <- uq[1L]; g1 <- uq[2L]
  }

  m0 <- g_raw == g0
  m1 <- g_raw == g1
  if (sum(m0) < 5L || sum(m1) < 5L) stop("Insufficient observations in comparison groups.", call. = FALSE)

  mc <- .canonical_ci_method(method)
  eff_b <- if (mc %in% c("wagstaff", "erreygers") && is.null(bounds)) c(0.0, 1.0) else bounds
  factor_names <- setdiff(names(dm$factor_groups), "Intercept")
  k_fac <- length(factor_names)

  eval_oax <- function(w_vec) {
    w0 <- w_vec[m0]; w1 <- w_vec[m1]
    c0 <- influence_concentration_index(y_sub[m0], r_sub[m0], weights = w0, method = mc, bounds = eff_b, rank_order = rank_order)
    c1 <- influence_concentration_index(y_sub[m1], r_sub[m1], weights = w1, method = mc, bounds = eff_b, rank_order = rank_order)
    rif0 <- compute_rif(c0$estimate, c0$psi)
    rif1 <- compute_rif(c1$estimate, c1$psi)

    X0 <- X_sub[m0, , drop = FALSE]; X1 <- X_sub[m1, , drop = FALSE]
    fit0 <- .fit_survey_glm(rif0, X0, weights = w0, family = "gaussian", link = "identity")
    fit1 <- .fit_survey_glm(rif1, X1, weights = w1, family = "gaussian", link = "identity")
    xb0 <- colSums(X0 * (w0 / sum(w0)))
    xb1 <- colSums(X1 * (w1 / sum(w1)))

    if (isTRUE(reweight)) {
      mb <- m0 | m1
      d_ind <- as.double(m1[mb])
      ps_fit <- .fit_survey_glm(d_ind, X_sub[mb, , drop = FALSE], weights = w_vec[mb], family = "binomial", link = "logit")
      p_hat0 <- pmin(pmax(ps_fit$inv_link(as.double(X0 %*% ps_fit$params)), 1e-4), 1.0 - 1e-4)
      w0_cf <- w0 * (p_hat0 / (1.0 - p_hat0))
      c_cf <- influence_concentration_index(y_sub[m0], r_sub[m0], weights = w0_cf, method = mc, bounds = eff_b, rank_order = rank_order)
      rif_cf <- compute_rif(c_cf$estimate, c_cf$psi)
      fit_cf <- .fit_survey_glm(rif_cf, X0, weights = w0_cf, family = "gaussian", link = "identity")
      xb_cf <- colSums(X0 * (w0_cf / sum(w0_cf)))
      b_ref <- fit_cf$params
      tot_diff <- c1$estimate - c0$estimate
      tot_exp <- sum((xb_cf - xb0) * fit0$params) + sum(xb_cf * (fit_cf$params - fit0$params))
      tot_unexp <- tot_diff - tot_exp
    } else {
      b_ref <- fit0$params
      tot_diff <- c1$estimate - c0$estimate
      tot_exp <- sum((xb1 - xb0) * b_ref)
      tot_unexp <- sum(xb1 * (fit1$params - b_ref))
    }

    exp_f <- numeric(k_fac)
    unexp_f <- numeric(k_fac)
    for (j in seq_len(k_fac)) {
      cols_j <- dm$factor_groups[[factor_names[j]]]
      exp_f[j] <- sum((xb1[cols_j] - xb0[cols_j]) * b_ref[cols_j])
      unexp_f[j] <- sum(xb1[cols_j] * (fit1$params[cols_j] - fit0$params[cols_j]))
    }
    unexp_int <- if (dm$has_intercept) (fit1$params[1L] - fit0$params[1L]) else 0.0
    c(c0$estimate, c1$estimate, tot_diff, tot_exp, tot_unexp, exp_f, unexp_f, unexp_int)
  }

  pt <- eval_oax(w_sub)
  if (bootstrap_reps >= 10L) {
    bsum <- rao_wu_survey_bootstrap(
      estimator_fn = eval_oax,
      weights = w_sub,
      strata = if (is.null(d_arr$strata)) NULL else d_arr$strata[valid],
      psu = if (is.null(d_arr$psu)) NULL else d_arr$psu[valid],
      n_boot = bootstrap_reps,
      seed = seed
    )
    se_vec <- bsum$se
    df_deg <- bsum$df
    n_st <- bsum$n_strata; n_ps <- bsum$n_psu
  } else {
    se_vec <- rep(NA_real_, length(pt))
    df_deg <- as.double(max(sum(m0 | m1) - 2L, 1L))
    n_st <- 1L; n_ps <- sum(m0 | m1)
  }

  crit <- stats::qt(1.0 - 0.5 * (1.0 - confidence_level), df = df_deg)
  tot_d <- pt[3L]
  ex_v <- pt[6:(5 + k_fac)]
  se_ex <- se_vec[6:(5 + k_fac)]
  un_v <- pt[(6 + k_fac):(5 + 2 * k_fac)]
  se_un <- se_vec[(6 + k_fac):(5 + 2 * k_fac)]
  un_int <- pt[length(pt)]
  se_int <- se_vec[length(pt)]

  fac_df <- data.frame(
    factor = c(factor_names, "Intercept"),
    explained = c(ex_v, 0.0),
    explained_se = c(se_ex, 0.0),
    unexplained = c(un_v, un_int),
    unexplained_se = c(se_un, se_int),
    contribution = c(ex_v + un_v, un_int),
    pct_share = if (abs(tot_d) > 1e-15) (c(ex_v + un_v, un_int) / tot_d) * 100.0 else rep(NA_real_, k_fac + 1L),
    ci_low = c(ex_v - crit * se_ex, NA_real_),
    ci_high = c(ex_v + crit * se_ex, NA_real_),
    stringsAsFactors = FALSE
  )

  ov_df <- data.frame(
    component = c(
      sprintf("Group %s %s index", g0, mc),
      sprintf("Group %s %s index", g1, mc),
      sprintf("Total difference: %s minus %s", g1, g0),
      "Explained composition effect",
      "Unexplained structure effect"
    ),
    estimate = pt[1:5],
    se = se_vec[1:5],
    stringsAsFactors = FALSE
  )

  new_esther_decomp(
    contributions = fac_df,
    overall_summary = ov_df,
    title = sprintf("RIF-Oaxaca inequality decomposition of %s between %s and %s", dm$outcome_name, g1, g0),
    metadata = list(
      outcome = dm$outcome_name,
      rank = rank,
      dimension = group,
      measures = sprintf("RIF-Oaxaca decomposition, %s", mc),
      favorable = favorable,
      rank_order = rank_order,
      data_mode = "survey",
      vce_method = "bootstrap",
      n_obs = sum(m0 | m1),
      sum_weights = sum(w_sub[m0 | m1]),
      n_strata = n_st,
      n_psu = n_ps,
      df = df_deg,
      confidence_level = confidence_level
    )
  )
}
