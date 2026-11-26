# Classical Univariate Inequality Indices (`Gini`, `S-Gini`, `Atkinson`, `GE`, `Palma`, `S80/S20`) & Subgroup GE Decomposition

classical_inequality <- function(
  data,
  outcome,
  indices = c("gini", "atkinson", "ge", "palma", "s80s20"),
  atkinson_eps = 1.0,
  ge_alpha = c(0.0, 1.0),
  sgini_v = 2.0,
  confidence_level = 0.95,
  weights = NULL,
  strata = NULL,
  psu = NULL,
  fpc = NULL,
  lonely_psu = "remove"
) {
  svy <- .coerce_survey(data, weights = weights, strata = strata, psu = psu, fpc = fpc, lonely_psu = lonely_psu)
  df <- svy$df
  if (!(outcome %in% names(df))) {
    stop(sprintf("`outcome = '%s'` not found in `data`.", outcome), call. = FALSE)
  }

  d_arr <- .get_design_arrays(svy)
  w_full <- d_arr$weights
  y_raw <- .to_clean_numeric(df[[outcome]], outcome)
  valid <- !is.na(y_raw) & is.finite(y_raw)
  y_sub <- y_raw[valid]
  w_sub <- w_full[valid]

  req_idx <- tolower(trimws(indices))
  eps_vec <- as.double(atkinson_eps)
  alpha_vec <- as.double(ge_alpha)

  nms <- character(0)
  ests <- numeric(0)
  psi_list <- list()

  for (idx_nm in req_idx) {
    if (idx_nm == "gini") {
      res_g <- influence_concentration_index(y_sub, y_sub, weights = w_sub, method = "standard", rank_order = "asc")
      nms <- c(nms, "Gini coefficient")
      ests <- c(ests, res_g$estimate)
      psi_list[[length(psi_list) + 1L]] <- res_g$psi
    } else if (idx_nm %in% c("sgini", "s-gini")) {
      res_sg <- influence_concentration_index(y_sub, y_sub, weights = w_sub, method = "extended", v = sgini_v, rank_order = "asc")
      nms <- c(nms, sprintf("S-Gini index, v = %g", sgini_v))
      ests <- c(ests, res_sg$estimate)
      psi_list[[length(psi_list) + 1L]] <- res_sg$psi
    } else if (idx_nm == "atkinson") {
      for (ep in eps_vec) {
        res_a <- influence_atkinson(y_sub, weights = w_sub, epsilon = ep)
        nms <- c(nms, sprintf("Atkinson index, epsilon = %g", ep))
        ests <- c(ests, res_a$estimate)
        psi_list[[length(psi_list) + 1L]] <- res_a$psi

        mu_y <- .weighted_mean(y_sub, w_sub)
        psi_ede <- (1.0 - res_a$estimate) * (y_sub - mu_y) - mu_y * res_a$psi
        nms <- c(nms, sprintf("Atkinson equally distributed equivalent, epsilon = %g", ep))
        ests <- c(ests, res_a$ede)
        psi_list[[length(psi_list) + 1L]] <- psi_ede
      }
    } else if (idx_nm %in% c("ge", "theil", "mld")) {
      t_alphas <- if (idx_nm == "theil") 1.0 else if (idx_nm == "mld") 0.0 else alpha_vec
      for (al in t_alphas) {
        res_ge <- influence_generalized_entropy(y_sub, weights = w_sub, alpha = al)
        lbl <- if (abs(al) < 1e-12) "Mean log deviation" else if (abs(al - 1.0) < 1e-12) "Theil index" else sprintf("Generalized entropy, alpha = %g", al)
        nms <- c(nms, lbl)
        ests <- c(ests, res_ge$estimate)
        psi_list[[length(psi_list) + 1L]] <- res_ge$psi
      }
    } else if (idx_nm == "palma") {
      co <- influence_curve_ordinates(y_sub, NULL, weights = w_sub, grid = c(0.4, 0.9), curve_type = "relative")
      s_bot40 <- co$ordinates[1L]
      s_top10 <- 1.0 - co$ordinates[2L]
      if (s_bot40 <= 1e-15) stop("Bottom 40% share is zero; Palma ratio is undefined.", call. = FALSE)
      palma <- s_top10 / s_bot40
      psi_p <- (-co$psi_mat[, 2L] * s_bot40 - s_top10 * co$psi_mat[, 1L]) / (s_bot40^2)
      nms <- c(nms, "Palma ratio")
      ests <- c(ests, palma)
      psi_list[[length(psi_list) + 1L]] <- psi_p
    } else if (idx_nm %in% c("s80s20", "qsr")) {
      co <- influence_curve_ordinates(y_sub, NULL, weights = w_sub, grid = c(0.2, 0.8), curve_type = "relative")
      s_bot20 <- co$ordinates[1L]
      s_top20 <- 1.0 - co$ordinates[2L]
      if (s_bot20 <= 1e-15) stop("Bottom 20% share is zero; S80/S20 ratio is undefined.", call. = FALSE)
      qsr <- s_top20 / s_bot20
      psi_q <- (-co$psi_mat[, 2L] * s_bot20 - s_top20 * co$psi_mat[, 1L]) / (s_bot20^2)
      nms <- c(nms, "Quintile share ratio S80/S20")
      ests <- c(ests, qsr)
      psi_list[[length(psi_list) + 1L]] <- psi_q
    } else {
      stop(sprintf("Unknown classical index `%s`.", idx_nm), call. = FALSE)
    }
  }

  psi_full <- matrix(0.0, nrow = nrow(df), ncol = length(psi_list))
  psi_full[valid, ] <- do.call(cbind, psi_list)
  vsum <- ultimate_cluster_variance(
    psi = psi_full,
    weights = w_full,
    strata = d_arr$strata,
    psu = d_arr$psu,
    fpc = d_arr$fpc,
    domain_mask = valid,
    lonely_psu = svy$lonely_psu
  )

  crit <- stats::qt(1.0 - 0.5 * (1.0 - confidence_level), df = vsum$df)
  tbl <- data.frame(
    measure = nms,
    estimate = ests,
    se = vsum$se,
    ci_low = ests - crit * vsum$se,
    ci_high = ests + crit * vsum$se,
    n_obs = rep(sum(valid), length(nms)),
    sum_weights = rep(sum(w_sub), length(nms)),
    deff = vsum$deff,
    stringsAsFactors = FALSE
  )

  new_esther_result(
    table = tbl,
    title = sprintf("Univariate inequality indices of %s", outcome),
    metadata = list(
      outcome = outcome,
      dimension = outcome,
      measures = nms,
      data_mode = "survey",
      vce_method = "if",
      n_obs = sum(valid),
      sum_weights = sum(w_sub),
      n_strata = vsum$n_strata,
      n_psu = vsum$n_psu,
      df = vsum$df,
      mean_deff = mean(vsum$deff, na.rm = TRUE),
      confidence_level = confidence_level
    ),
    vcov = vsum$vcov
  )
}

decompose_subgroup_ge <- function(
  data,
  outcome,
  by,
  alpha = 1.0,
  bootstrap_reps = 200L,
  confidence_level = 0.95,
  seed = 42L,
  weights = NULL,
  strata = NULL,
  psu = NULL,
  fpc = NULL
) {
  svy <- .coerce_survey(data, weights = weights, strata = strata, psu = psu, fpc = fpc)
  df <- svy$df
  for (col in c(outcome, by)) {
    if (!(col %in% names(df))) stop(sprintf("Column `%s` not found in `data`.", col), call. = FALSE)
  }

  d_arr <- .get_design_arrays(svy)
  valid <- stats::complete.cases(df[, c(outcome, by), drop = FALSE])
  y_sub <- .to_clean_numeric(df[[outcome]][valid], outcome)
  g_sub <- as.character(df[[by]][valid])
  w_sub <- d_arr$weights[valid]
  groups <- sort(unique(g_sub))

  eval_fn <- function(w_vec) {
    w_tot <- sum(w_vec)
    mu_tot <- sum(w_vec * y_sub) / w_tot
    ge_tot <- influence_generalized_entropy(y_sub, weights = w_vec, alpha = alpha)$estimate

    ge_within <- 0.0
    v_g <- numeric(length(groups))
    mu_g <- numeric(length(groups))
    ge_g <- numeric(length(groups))

    for (j in seq_along(groups)) {
      m_g <- g_sub == groups[j]
      w_j <- w_vec[m_g]
      v_g[j] <- sum(w_j) / w_tot
      mu_g[j] <- sum(w_j * y_sub[m_g]) / sum(w_j)
      s_j <- v_g[j] * (mu_g[j] / mu_tot)
      ge_g[j] <- influence_generalized_entropy(y_sub[m_g], weights = w_j, alpha = alpha)$estimate
      ge_within <- ge_within + ((v_g[j]^(1.0 - alpha)) * (s_j^alpha)) * ge_g[j]
    }
    ge_between <- influence_generalized_entropy(mu_g, weights = v_g, alpha = alpha)$estimate
    c(ge_tot, ge_within, ge_between, ge_g)
  }

  pt <- eval_fn(w_sub)
  se_vec <- rep(NA_real_, length(pt))
  if (bootstrap_reps >= 10L) {
    bsum <- rao_wu_survey_bootstrap(
      estimator_fn = eval_fn,
      weights = w_sub,
      strata = if (is.null(d_arr$strata)) NULL else d_arr$strata[valid],
      psu = if (is.null(d_arr$psu)) NULL else d_arr$psu[valid],
      n_boot = bootstrap_reps,
      seed = seed
    )
    se_vec <- bsum$se
  }

  z_c <- stats::qnorm(1.0 - 0.5 * (1.0 - confidence_level))
  ge_tot <- pt[1L]
  contribs <- data.frame(
    factor = c("Within-group inequality", "Between-group inequality"),
    contribution = pt[2:3],
    pct_share = if (ge_tot > 0) (pt[2:3] / ge_tot) * 100.0 else c(0.0, 0.0),
    se = se_vec[2:3],
    ci_low = pt[2:3] - z_c * se_vec[2:3],
    ci_high = pt[2:3] + z_c * se_vec[2:3],
    stringsAsFactors = FALSE
  )

  ov <- data.frame(
    metric = c(sprintf("Total generalized entropy, alpha = %g", alpha), paste("Subgroup", groups)),
    estimate = c(ge_tot, pt[4:length(pt)]),
    se = c(se_vec[1L], se_vec[4:length(pt)]),
    stringsAsFactors = FALSE
  )

  new_esther_decomp(
    contributions = contribs,
    overall_summary = ov,
    title = sprintf("Generalized entropy subgroup decomposition of %s by %s", outcome, by),
    metadata = list(
      outcome = outcome,
      dimension = by,
      measures = sprintf("Generalized entropy subgroup decomposition, alpha = %g", alpha),
      data_mode = "survey",
      vce_method = "bootstrap",
      n_obs = sum(valid),
      sum_weights = sum(w_sub),
      confidence_level = confidence_level
    )
  )
}
