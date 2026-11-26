# Modern Epidemiological Slope & Relative Indices of Inequality (`SII` & `RII`)

sii_rii <- function(
  data,
  outcome = NULL,
  rank = NULL,
  method = c("logit_marginal", "moreno_betancur", "linear"),
  covariates = NULL,
  categorical = NULL,
  strata_for_ridit = NULL,
  rank_order = c("asc", "desc"),
  favorable = NULL,
  scale = 1.0,
  confidence_level = 0.95,
  n_sim = 2000L,
  seed = 42L,
  weights = NULL,
  strata = NULL,
  psu = NULL,
  fpc = NULL,
  lonely_psu = "remove"
) {
  method <- match.arg(method)
  rank_order <- match.arg(rank_order)

  # Disaggregated subgroup table path
  if (inherits(data, "esther_grouped")) {
    if (!isTRUE(data$ordered)) {
      stop("`sii_rii` requires an ordered stratification dimension (`ordered = TRUE`).", call. = FALSE)
    }
    df_g <- data$df
    y_g <- .to_clean_numeric(df_g[[data$estimate]], data$estimate)
    eff_sc <- if (missing(scale)) data$scale else as.double(scale)
    if (eff_sc != data$scale) y_g <- y_g * (eff_sc / data$scale)
    pop_g <- if (is.null(data$population)) rep(1.0, nrow(df_g)) else .to_clean_numeric(df_g[[data$population]], data$population)
    r_res <- compute_fractional_ranks(seq_len(nrow(df_g)) - 1.0, weights = pop_g, rank_order = rank_order)
    rid <- r_res$ridit

    eval_grp <- function(yv) {
      yp <- yv / eff_sc
      if (method == "logit_marginal" && all(yp > 0.0 & yp < 1.0)) {
        fit <- .fit_survey_glm(yp, cbind(1.0, rid), weights = pop_g, family = "binomial", link = "logit")
        p1 <- stats::plogis(fit$params[1L] + fit$params[2L]) * eff_sc
        p0 <- stats::plogis(fit$params[1L]) * eff_sc
        return(c(p1 - p0, p1 / p0))
      }
      fit <- .fit_survey_glm(yv, cbind(1.0, rid), weights = pop_g, family = "gaussian", link = "identity")
      p1 <- fit$params[1L] + fit$params[2L]
      p0 <- fit$params[1L]
      c(p1 - p0, if (abs(p0) > 1e-15) p1 / p0 else NA_real_)
    }

    pt <- eval_grp(y_g)
    se_v <- c(NA_real_, NA_real_)
    ci_l <- c(NA_real_, NA_real_)
    ci_h <- c(NA_real_, NA_real_)
    p_v <- c(NA_real_, NA_real_)

    if (!is.null(data$se) && n_sim >= 20L) {
      se_sub <- .to_clean_numeric(df_g[[data$se]], data$se) * (eff_sc / data$scale)
      old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
      on.exit({
        if (is.null(old_seed)) { if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv) } else assign(".Random.seed", old_seed, envir = .GlobalEnv)
      }, add = TRUE)
      set.seed(as.integer(seed))

      draws <- matrix(NA_real_, nrow = n_sim, ncol = 2L)
      for (s_i in seq_len(n_sim)) {
        ys <- pmin(pmax(stats::rnorm(length(y_g), mean = y_g, sd = se_sub), 1e-5 * eff_sc), (1.0 - 1e-5) * eff_sc)
        draws[s_i, ] <- eval_grp(ys)
      }
      z_c <- stats::qnorm(1.0 - 0.5 * (1.0 - confidence_level))
      se_v[1L] <- stats::sd(draws[, 1L], na.rm = TRUE)
      ci_l[1L] <- pt[1L] - z_c * se_v[1L]
      ci_h[1L] <- pt[1L] + z_c * se_v[1L]
      p_v[1L] <- 2.0 * stats::pnorm(-abs(pt[1L] / se_v[1L]))

      l_rii <- log(draws[draws[, 2L] > 0, 2L])
      se_l <- stats::sd(l_rii, na.rm = TRUE)
      se_v[2L] <- pt[2L] * se_l
      ci_l[2L] <- exp(log(pt[2L]) - z_c * se_l)
      ci_h[2L] <- exp(log(pt[2L]) + z_c * se_l)
      p_v[2L] <- 2.0 * stats::pnorm(-abs(log(pt[2L]) / se_l))
    }

    tbl <- data.frame(
      measure = c("Slope index of inequality", "Relative index of inequality"),
      estimate = pt,
      se = se_v,
      ci_low = ci_l,
      ci_high = ci_h,
      p_value = p_v,
      n_obs = rep(nrow(df_g), 2L),
      sum_weights = rep(sum(pop_g), 2L),
      interpretation = c(
        .interpret_gradient_sign(pt[1L], ci_l[1L], ci_h[1L], null_value = 0.0, rank_order = rank_order),
        .interpret_gradient_sign(pt[2L], ci_l[2L], ci_h[2L], null_value = 1.0, rank_order = rank_order)
      ),
      stringsAsFactors = FALSE
    )
    return(new_esther_result(
      table = tbl,
      title = sprintf("Slope and relative indices of inequality of %s by %s", data$estimate, data$subgroup),
      metadata = list(
        outcome = data$estimate,
        rank = data$subgroup,
        measures = tbl$measure,
        favorable = if (is.null(favorable)) data$favorable else favorable,
        rank_order = rank_order,
        data_mode = "grouped",
        vce_method = "monte_carlo",
        confidence_level = confidence_level
      )
    ))
  }

  if (is.null(outcome) || is.null(rank)) {
    stop("`outcome` and `rank` are required for microdata `sii_rii`.", call. = FALSE)
  }

  svy <- .coerce_survey(data, weights = weights, strata = strata, psu = psu, fpc = fpc, lonely_psu = lonely_psu)
  df <- svy$df
  extra <- unique(c(rank, strata_for_ridit))
  dm <- .build_design_matrix(
    df, outcome = outcome, covariates = covariates, categorical = categorical,
    add_intercept = TRUE, extra_cols = extra
  )
  valid <- dm$valid_mask
  y_sub <- dm$y
  d_arr <- .get_design_arrays(svy)
  w_full <- d_arr$weights
  w_sub <- w_full[valid]
  w_norm <- w_sub / sum(w_sub)
  r_sub <- .to_rank_numeric(df[[rank]][valid], rank)

  # Compute overall or stratum-specific ridit scores
  if (!is.null(strata_for_ridit)) {
    st_r <- as.character(df[[strata_for_ridit]][valid])
    ridit <- numeric(length(r_sub))
    for (g_val in unique(st_r)) {
      idx <- which(st_r == g_val)
      ridit[idx] <- compute_fractional_ranks(r_sub[idx], weights = w_sub[idx], rank_order = rank_order)$ridit
    }
  } else {
    ridit <- compute_fractional_ranks(r_sub, weights = w_sub, rank_order = rank_order)$ridit
  }

  # Design matrix: [Intercept, ridit, covariates...]
  X_cov <- if (ncol(dm$X) > 1L) dm$X[, -1L, drop = FALSE] else NULL
  X_full <- cbind(`(Intercept)` = 1.0, ridit = ridit, X_cov)
  X_r1 <- X_full; X_r1[, 2L] <- 1.0
  X_r0 <- X_full; X_r0[, 2L] <- 0.0

  is_binary <- all(y_sub >= 0.0 & y_sub <= 1.0)
  eff_method <- if (method == "logit_marginal" && !is_binary) "moreno_betancur" else method

  if (eff_method == "logit_marginal") {
    fit <- .fit_survey_glm(y_sub, X_full, weights = w_sub, family = "binomial", link = "logit")
    eta1 <- as.double(X_r1 %*% fit$params)
    eta0 <- as.double(X_r0 %*% fit$params)
    p1_i <- stats::plogis(eta1)
    p0_i <- stats::plogis(eta0)
    mu1 <- sum(w_norm * p1_i)
    mu0 <- sum(w_norm * p0_i)

    dmu1_db <- colSums(X_r1 * (w_norm * p1_i * (1.0 - p1_i)))
    dmu0_db <- colSums(X_r0 * (w_norm * p0_i * (1.0 - p0_i)))

    psi_mu1 <- as.double(fit$psi_params %*% dmu1_db) + (p1_i - mu1)
    psi_mu0 <- as.double(fit$psi_params %*% dmu0_db) + (p0_i - mu0)

    sii_est <- (mu1 - mu0) * scale
    psi_sii <- (psi_mu1 - psi_mu0) * scale
    rii_est <- mu1 / mu0
    psi_log_rii <- psi_mu1 / mu1 - psi_mu0 / mu0
    psi_rii <- rii_est * psi_log_rii
  } else if (eff_method == "moreno_betancur") {
    fam_id <- if (is_binary) "binomial" else "gaussian"
    fit_id <- .fit_survey_glm(y_sub, X_full, weights = w_sub, family = fam_id, link = "identity")
    sii_est <- fit_id$params[2L] * scale
    psi_sii <- fit_id$psi_params[, 2L] * scale

    fit_log <- .fit_survey_glm(pmax(y_sub, 0.0), X_full, weights = w_sub, family = "poisson", link = "log")
    rii_est <- exp(fit_log$params[2L])
    psi_log_rii <- fit_log$psi_params[, 2L]
    psi_rii <- rii_est * psi_log_rii
  } else {
    fit_lin <- .fit_survey_glm(y_sub, X_full, weights = w_sub, family = "gaussian", link = "identity")
    mu1 <- sum(w_norm * as.double(X_r1 %*% fit_lin$params))
    mu0 <- sum(w_norm * as.double(X_r0 %*% fit_lin$params))
    dmu1_db <- colSums(X_r1 * w_norm)
    dmu0_db <- colSums(X_r0 * w_norm)
    psi_mu1 <- as.double(fit_lin$psi_params %*% dmu1_db) + (as.double(X_r1 %*% fit_lin$params) - mu1)
    psi_mu0 <- as.double(fit_lin$psi_params %*% dmu0_db) + (as.double(X_r0 %*% fit_lin$params) - mu0)

    sii_est <- (mu1 - mu0) * scale
    psi_sii <- (psi_mu1 - psi_mu0) * scale
    rii_est <- mu1 / mu0
    psi_log_rii <- psi_mu1 / mu1 - psi_mu0 / mu0
    psi_rii <- rii_est * psi_log_rii
  }

  psi_mat <- matrix(0.0, nrow = nrow(df), ncol = 3L)
  psi_mat[valid, 1L] <- psi_sii
  psi_mat[valid, 2L] <- psi_rii
  psi_mat[valid, 3L] <- psi_log_rii

  vsum <- ultimate_cluster_variance(
    psi = psi_mat,
    weights = w_full,
    strata = d_arr$strata,
    psu = d_arr$psu,
    fpc = d_arr$fpc,
    domain_mask = valid,
    lonely_psu = svy$lonely_psu
  )
  crit <- stats::qt(1.0 - 0.5 * (1.0 - confidence_level), df = vsum$df)

  se_sii <- vsum$se[1L]
  ci_sii <- c(sii_est - crit * se_sii, sii_est + crit * se_sii)
  p_sii <- if (se_sii > 0) 2.0 * stats::pt(-abs(sii_est / se_sii), df = vsum$df) else NA_real_

  se_rii <- vsum$se[2L]
  se_log_r <- vsum$se[3L]
  ci_rii <- if (rii_est > 0 && is.finite(se_log_r)) {
    c(exp(log(rii_est) - crit * se_log_r), exp(log(rii_est) + crit * se_log_r))
  } else {
    c(rii_est - crit * se_rii, rii_est + crit * se_rii)
  }
  p_rii <- if (rii_est > 0 && se_log_r > 0) 2.0 * stats::pt(-abs(log(rii_est) / se_log_r), df = vsum$df) else NA_real_

  tbl <- data.frame(
    measure = c("Slope index of inequality", "Relative index of inequality"),
    estimate = c(sii_est, rii_est),
    se = c(se_sii, se_rii),
    ci_low = c(ci_sii[1L], ci_rii[1L]),
    ci_high = c(ci_sii[2L], ci_rii[2L]),
    p_value = c(p_sii, p_rii),
    n_obs = rep(sum(valid), 2L),
    sum_weights = rep(sum(w_sub), 2L),
    deff = vsum$deff[1:2],
    interpretation = c(
      .interpret_gradient_sign(sii_est, ci_sii[1L], ci_sii[2L], null_value = 0.0, rank_order = rank_order),
      .interpret_gradient_sign(rii_est, ci_rii[1L], ci_rii[2L], null_value = 1.0, rank_order = rank_order)
    ),
    stringsAsFactors = FALSE
  )

  new_esther_result(
    table = tbl,
    title = sprintf("Slope and relative indices of inequality of %s by %s", outcome, rank),
    metadata = list(
      outcome = outcome,
      rank = rank,
      measures = tbl$measure,
      favorable = favorable,
      rank_order = rank_order,
      data_mode = "survey",
      vce_method = "if",
      n_obs = sum(valid),
      sum_weights = sum(w_sub),
      n_strata = vsum$n_strata,
      n_psu = vsum$n_psu,
      df = vsum$df,
      mean_deff = mean(vsum$deff[1:2], na.rm = TRUE),
      confidence_level = confidence_level
    ),
    vcov = vsum$vcov[1:2, 1:2, drop = FALSE]
  )
}
