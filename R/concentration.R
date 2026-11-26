# Concentration Indices, Concentration/Lorenz Curves, and Survey Means (`concentration_index`, `concentration_curve`, `survey_mean`)

.canonical_ci_method <- function(method) {
  m <- tolower(trimws(method))
  map <- c(
    standard = "standard", ci = "standard", rci = "standard", c = "standard",
    generalized = "generalized", gc = "generalized", aci = "generalized",
    modified = "modified", mc = "modified",
    wagstaff = "wagstaff", w = "wagstaff",
    erreygers = "erreygers", e = "erreygers", cci = "erreygers",
    extended = "extended", eci = "extended",
    symmetric = "symmetric", sci = "symmetric"
  )
  if (!(m %in% names(map))) {
    stop(sprintf("Unknown concentration index `method = '%s'`.", method), call. = FALSE)
  }
  unname(map[[m]])
}

.ci_method_label <- function(mc, v = 2.0, beta = 2.0) {
  if (mc == "standard") return("Standard concentration index")
  if (mc == "generalized") return("Generalized concentration index")
  if (mc == "modified") return("Modified concentration index")
  if (mc == "wagstaff") return("Wagstaff concentration index")
  if (mc == "erreygers") return("Erreygers concentration index")
  if (mc == "extended") return(sprintf("Extended concentration index, v = %g", v))
  if (mc == "symmetric") return(sprintf("Symmetric concentration index, beta = %g", beta))
  mc
}

concentration_index <- function(
  data,
  outcome = NULL,
  rank = NULL,
  method = "erreygers",
  bounds = NULL,
  v = 2.0,
  beta = 2.0,
  rank_order = c("asc", "desc"),
  favorable = NULL,
  by = NULL,
  vce = c("if", "convenient", "newey", "replicate", "bootstrap"),
  newey_lags = 1L,
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
  mc <- .canonical_ci_method(method)
  m_lbl <- .ci_method_label(mc, v = v, beta = beta)

  # Grouped disaggregated surveillance path
  if (inherits(data, "esther_grouped")) {
    if (!isTRUE(data$ordered)) {
      stop("Concentration index requires an ordered stratification dimension (`ordered = TRUE`).", call. = FALSE)
    }
    df_g <- data$df
    y_g <- .to_clean_numeric(df_g[[data$estimate]], data$estimate)
    pop_g <- if (is.null(data$population)) rep(1.0, nrow(df_g)) else .to_clean_numeric(df_g[[data$population]], data$population)
    r_g <- as.double(seq_len(nrow(df_g)) - 1L)
    eff_b <- bounds
    if (mc %in% c("wagstaff", "erreygers") && is.null(eff_b)) {
      eff_b <- c(0.0, data$scale)
    }
    pt_res <- influence_concentration_index(
      y_g, r_g, weights = pop_g, method = mc, bounds = eff_b, v = v, beta = beta, rank_order = rank_order
    )
    est <- pt_res$estimate

    se_val <- NA_real_
    ci_l <- NA_real_
    ci_h <- NA_real_
    p_val <- NA_real_
    if (!is.null(data$se)) {
      se_sub <- .to_clean_numeric(df_g[[data$se]], data$se)
      old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
      on.exit({
        if (is.null(old_seed)) { if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv) } else assign(".Random.seed", old_seed, envir = .GlobalEnv)
      }, add = TRUE)
      set.seed(as.integer(seed))
      draws <- numeric(bootstrap_reps)
      for (b_i in seq_len(bootstrap_reps)) {
        y_sim <- stats::rnorm(length(y_g), mean = y_g, sd = se_sub)
        if (!is.null(eff_b)) {
          eps_b <- 1e-6 * (eff_b[2L] - eff_b[1L])
          y_sim <- pmin(pmax(y_sim, eff_b[1L] + eps_b), eff_b[2L] - eps_b)
        }
        draws[b_i] <- tryCatch(
          influence_concentration_index(y_sim, r_g, weights = pop_g, method = mc, bounds = eff_b, v = v, beta = beta, rank_order = rank_order)$estimate,
          error = function(e) NA_real_
        )
      }
      draws <- draws[is.finite(draws)]
      if (length(draws) >= 5L) {
        se_val <- stats::sd(draws)
        z_c <- stats::qnorm(1.0 - 0.5 * (1.0 - confidence_level))
        ci_l <- est - z_c * se_val
        ci_h <- est + z_c * se_val
        p_val <- 2.0 * stats::pnorm(-abs(est / se_val))
      }
    }

    tbl <- data.frame(
      measure = m_lbl,
      estimate = est,
      se = se_val,
      ci_low = ci_l,
      ci_high = ci_h,
      p_value = p_val,
      n_obs = nrow(df_g),
      sum_weights = sum(pop_g),
      interpretation = .interpret_gradient_sign(est, ci_l, ci_h, null_value = 0.0, rank_order = rank_order),
      stringsAsFactors = FALSE
    )
    return(new_esther_result(
      table = tbl,
      title = sprintf("%s of %s by %s", m_lbl, data$estimate, data$subgroup),
      metadata = list(
        outcome = data$estimate,
        rank = data$subgroup,
        measures = m_lbl,
        favorable = if (is.null(favorable)) data$favorable else favorable,
        rank_order = rank_order,
        data_mode = "grouped",
        vce_method = "monte_carlo",
        confidence_level = confidence_level
      )
    ))
  }

  if (is.null(outcome) || is.null(rank)) {
    stop("`outcome` and `rank` are required for microdata `concentration_index`.", call. = FALSE)
  }

  svy <- .coerce_survey(data, weights = weights, strata = strata, psu = psu, fpc = fpc, lonely_psu = lonely_psu)
  df <- svy$df
  for (col in c(outcome, rank, by)) {
    if (!is.null(col) && !(col %in% names(df))) {
      stop(sprintf("Column `%s` not found in `data`.", col), call. = FALSE)
    }
  }

  d_arr <- .get_design_arrays(svy)
  w_full <- d_arr$weights
  req_cols <- c(outcome, rank, by)
  valid <- stats::complete.cases(df[, req_cols, drop = FALSE])
  y_all <- .to_clean_numeric(df[[outcome]], outcome)
  r_all <- .to_rank_numeric(df[[rank]], rank)

  eff_b <- bounds
  if (mc %in% c("wagstaff", "erreygers") && is.null(eff_b)) {
    eff_b <- c(0.0, 1.0)
  }

  # Subgroup / domain comparison path
  if (!is.null(by)) {
    by_raw <- df[[by]]
    dom_levels <- if (is.factor(by_raw)) {
      lv <- levels(by_raw)
      lv[lv %in% as.character(by_raw[valid])]
    } else {
      sort(unique(as.character(by_raw[valid])))
    }
    g_cnt <- length(dom_levels)
    if (g_cnt < 1L) stop("No valid observations in `by` subgroups.", call. = FALSE)

    psi_mat <- matrix(0.0, nrow = nrow(df), ncol = g_cnt)
    ests <- numeric(g_cnt)
    n_vec <- integer(g_cnt)
    sw_vec <- numeric(g_cnt)
    deff_vec <- numeric(g_cnt)

    for (j in seq_len(g_cnt)) {
      m_j <- valid & (as.character(by_raw) == dom_levels[j])
      n_vec[j] <- sum(m_j)
      w_j <- w_full[m_j]
      sw_vec[j] <- sum(w_j)
      ci_j <- influence_concentration_index(
        y_all[m_j], r_all[m_j], weights = w_j, method = mc, bounds = eff_b, v = v, beta = beta, rank_order = rank_order
      )
      ests[j] <- ci_j$estimate
      # Scale domain IF so (w_i / W_valid) * psi_scaled == (w_i / W_domain) * psi_domain
      psi_mat[m_j, j] <- (sum(w_full[valid]) / sw_vec[j]) * ci_j$psi
      v_dom <- ultimate_cluster_variance(
        psi = ci_j$psi,
        weights = w_full[m_j],
        strata = if (is.null(d_arr$strata)) NULL else d_arr$strata[m_j],
        psu = if (is.null(d_arr$psu)) NULL else d_arr$psu[m_j],
        fpc = if (is.null(d_arr$fpc)) NULL else d_arr$fpc[m_j],
        lonely_psu = svy$lonely_psu
      )
      deff_vec[j] <- v_dom$deff[1L]
    }

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
    se_v <- vsum$se
    ci_l <- ests - crit * se_v
    ci_h <- ests + crit * se_v
    t_v <- ifelse(se_v > 0, ests / se_v, NA_real_)
    p_v <- 2.0 * stats::pt(-abs(t_v), df = vsum$df)

    # Joint complex-survey omnibus Wald F-test H0: I_1 = ... = I_G
    omnibus <- NULL
    if (g_cnt >= 2L) {
      c_mat <- cbind(-1.0, diag(1.0, g_cnt - 1L))
      diff_v <- as.double(c_mat %*% ests)
      v_diff <- c_mat %*% vsum$vcov %*% t(c_mat)
      f_stat <- tryCatch({
        w_chi2 <- as.double(t(diff_v) %*% solve(v_diff, diff_v))
        w_chi2 / (g_cnt - 1.0)
      }, error = function(e) NA_real_)
      if (is.finite(f_stat) && f_stat >= 0) {
        p_omni <- stats::pf(f_stat, df1 = g_cnt - 1.0, df2 = vsum$df, lower.tail = FALSE)
        omnibus <- list(stat = f_stat, df1 = g_cnt - 1L, df2 = vsum$df, p_value = p_omni)
      }
    }

    interps <- vapply(seq_len(g_cnt), function(j) {
      .interpret_gradient_sign(ests[j], ci_l[j], ci_h[j], null_value = 0.0, rank_order = rank_order)
    }, character(1L))

    tbl <- data.frame(
      measure = rep(m_lbl, g_cnt),
      domain = dom_levels,
      estimate = ests,
      se = se_v,
      ci_low = ci_l,
      ci_high = ci_h,
      p_value = p_v,
      n_obs = n_vec,
      sum_weights = sw_vec,
      deff = deff_vec,
      interpretation = interps,
      stringsAsFactors = FALSE
    )

    return(new_esther_result(
      table = tbl,
      title = sprintf("%s of %s by %s", m_lbl, outcome, by),
      metadata = list(
        outcome = outcome,
        rank = rank,
        dimension = by,
        measures = m_lbl,
        favorable = favorable,
        rank_order = rank_order,
        data_mode = "survey",
        vce_method = "if",
        n_obs = sum(valid),
        sum_weights = sum(w_full[valid]),
        n_strata = vsum$n_strata,
        n_psu = vsum$n_psu,
        df = vsum$df,
        mean_deff = mean(deff_vec, na.rm = TRUE),
        confidence_level = confidence_level
      ),
      vcov = vsum$vcov,
      omnibus_test = omnibus
    ))
  }

  # Single overall population estimate
  y_sub <- y_all[valid]
  r_sub <- r_all[valid]
  w_sub <- w_full[valid]

  ci_res <- influence_concentration_index(
    y_sub, r_sub, weights = w_sub, method = mc, bounds = eff_b, v = v, beta = beta, rank_order = rank_order
  )
  est <- ci_res$estimate

  if (vce == "if" && is.null(svy$rep_weights)) {
    psi_full <- numeric(nrow(df))
    psi_full[valid] <- ci_res$psi
    vsum <- ultimate_cluster_variance(
      psi = psi_full,
      weights = w_full,
      strata = d_arr$strata,
      psu = d_arr$psu,
      fpc = d_arr$fpc,
      domain_mask = valid,
      lonely_psu = svy$lonely_psu
    )
    vce_used <- "if"
  } else if (vce %in% c("convenient", "newey")) {
    mu <- ci_res$mu
    sc_fac <- if (mc == "standard") 1.0 / mu else if (mc == "generalized") 1.0 else if (mc == "modified") 1.0 / (mu - eff_b[1L]) else if (mc == "wagstaff") (eff_b[2L] - eff_b[1L]) / ((eff_b[2L] - mu) * (mu - eff_b[1L])) else if (mc == "erreygers") 4.0 / (eff_b[2L] - eff_b[1L]) else 1.0 / mu
    vsum <- convenient_regression_variance(
      y = y_sub,
      ridit = ci_res$ridit,
      weights = w_sub,
      strata = if (is.null(d_arr$strata)) NULL else d_arr$strata[valid],
      psu = if (is.null(d_arr$psu)) NULL else d_arr$psu[valid],
      fpc = if (is.null(d_arr$fpc)) NULL else d_arr$fpc[valid],
      scale_factor = sc_fac,
      newey_lags = if (vce == "newey") as.integer(newey_lags) else 0L,
      lonely_psu = svy$lonely_psu
    )
    vce_used <- vce
  } else if (vce == "replicate" || !is.null(svy$rep_weights)) {
    if (is.null(svy$rep_weights)) stop("`rep_weights` not configured in `survey_health_data`.", call. = FALSE)
    rw_mat <- as.matrix(df[valid, svy$rep_weights, drop = FALSE])
    vsum <- replicate_weights_variance(
      estimator_fn = function(wv) influence_concentration_index(y_sub, r_sub, weights = wv, method = mc, bounds = eff_b, v = v, beta = beta, rank_order = rank_order)$estimate,
      full_weights = w_sub,
      rep_weights_mat = rw_mat,
      rep_type = svy$rep_type,
      rep_scale = svy$rep_scale
    )
    vce_used <- "replicate"
  } else {
    vsum <- rao_wu_survey_bootstrap(
      estimator_fn = function(wv) influence_concentration_index(y_sub, r_sub, weights = wv, method = mc, bounds = eff_b, v = v, beta = beta, rank_order = rank_order)$estimate,
      weights = w_sub,
      strata = if (is.null(d_arr$strata)) NULL else d_arr$strata[valid],
      psu = if (is.null(d_arr$psu)) NULL else d_arr$psu[valid],
      n_boot = bootstrap_reps,
      seed = seed
    )
    vce_used <- "bootstrap"
  }

  crit <- stats::qt(1.0 - 0.5 * (1.0 - confidence_level), df = vsum$df)
  se_k <- vsum$se[1L]
  ci_l <- est - crit * se_k
  ci_h <- est + crit * se_k
  p_k <- if (se_k > 0) 2.0 * stats::pt(-abs(est / se_k), df = vsum$df) else NA_real_

  tbl <- data.frame(
    measure = m_lbl,
    estimate = est,
    se = se_k,
    ci_low = ci_l,
    ci_high = ci_h,
    p_value = p_k,
    n_obs = sum(valid),
    sum_weights = sum(w_sub),
    deff = vsum$deff[1L],
    interpretation = .interpret_gradient_sign(est, ci_l, ci_h, null_value = 0.0, rank_order = rank_order),
    stringsAsFactors = FALSE
  )

  new_esther_result(
    table = tbl,
    title = sprintf("%s of %s by %s", m_lbl, outcome, rank),
    metadata = list(
      outcome = outcome,
      rank = rank,
      measures = m_lbl,
      favorable = favorable,
      rank_order = rank_order,
      data_mode = "survey",
      vce_method = vce_used,
      n_obs = sum(valid),
      sum_weights = sum(w_sub),
      n_strata = vsum$n_strata,
      n_psu = vsum$n_psu,
      df = vsum$df,
      mean_deff = vsum$deff[1L],
      confidence_level = confidence_level
    ),
    vcov = vsum$vcov
  )
}

concentration_curve <- function(
  data,
  outcome,
  rank = NULL,
  by = NULL,
  curve_type = c("relative", "generalized", "absolute"),
  n_grid = 21L,
  simultaneous_bands = FALSE,
  rank_order = c("asc", "desc"),
  confidence_level = 0.95,
  weights = NULL,
  strata = NULL,
  psu = NULL,
  fpc = NULL,
  lonely_psu = "remove"
) {
  curve_type <- match.arg(curve_type)
  rank_order <- match.arg(rank_order)
  svy <- .coerce_survey(data, weights = weights, strata = strata, psu = psu, fpc = fpc, lonely_psu = lonely_psu)
  df <- svy$df
  for (col in c(outcome, rank, by)) {
    if (!is.null(col) && !(col %in% names(df))) {
      stop(sprintf("Column `%s` not found in `data`.", col), call. = FALSE)
    }
  }

  d_arr <- .get_design_arrays(svy)
  w_full <- d_arr$weights
  req_cols <- unique(c(outcome, rank, by))
  valid <- stats::complete.cases(df[, req_cols, drop = FALSE])
  y_all <- .to_clean_numeric(df[[outcome]], outcome)
  r_all <- if (is.null(rank)) y_all else .to_rank_numeric(df[[rank]], rank)
  p_grid <- seq(0.0, 1.0, length.out = as.integer(n_grid))

  groups <- if (is.null(by)) {
    list(Overall = valid)
  } else {
    by_raw <- df[[by]]
    lv <- if (is.factor(by_raw)) levels(by_raw)[levels(by_raw) %in% as.character(by_raw[valid])] else sort(unique(as.character(by_raw[valid])))
    stats::setNames(lapply(lv, function(g) valid & (as.character(by_raw) == g)), lv)
  }

  rows_list <- list()
  for (grp_nm in names(groups)) {
    m_g <- groups[[grp_nm]]
    ord_res <- influence_curve_ordinates(
      y = y_all[m_g],
      rank = r_all[m_g],
      weights = w_full[m_g],
      grid = p_grid,
      curve_type = curve_type,
      rank_order = rank_order
    )
    psi_full <- matrix(0.0, nrow = nrow(df), ncol = length(p_grid))
    psi_full[m_g, ] <- ord_res$psi_mat
    vsum <- ultimate_cluster_variance(
      psi = psi_full,
      weights = w_full,
      strata = d_arr$strata,
      psu = d_arr$psu,
      fpc = d_arr$fpc,
      domain_mask = m_g,
      lonely_psu = svy$lonely_psu
    )

    interior <- (p_grid > 0.0) & (p_grid < 1.0) & (vsum$se > 1e-12)
    m_int <- max(sum(interior), 1L)
    alpha_eff <- if (isTRUE(simultaneous_bands)) (1.0 - confidence_level) / m_int else (1.0 - confidence_level)
    crit <- stats::qt(1.0 - 0.5 * alpha_eff, df = vsum$df)

    ci_l <- ord_res$ordinates - crit * vsum$se
    ci_h <- ord_res$ordinates + crit * vsum$se
    if (curve_type == "relative") {
      ci_l <- pmin(pmax(ci_l, 0.0), 1.0)
      ci_h <- pmin(pmax(ci_h, 0.0), 1.0)
    }

    df_grp <- data.frame(
      group = grp_nm,
      p = p_grid,
      ordinate = ord_res$ordinates,
      equality = if (curve_type == "relative") p_grid else if (curve_type == "generalized") p_grid * .weighted_mean(y_all[m_g], w_full[m_g]) else 0.0,
      se = vsum$se,
      ci_low = ci_l,
      ci_high = ci_h,
      stringsAsFactors = FALSE
    )
    rows_list[[length(rows_list) + 1L]] <- df_grp
  }

  out_ords <- do.call(rbind, rows_list)
  rownames(out_ords) <- NULL
  title_str <- if (is.null(rank)) {
    sprintf("Lorenz curve of %s", outcome)
  } else {
    sprintf("Concentration curve of %s by %s", outcome, rank)
  }
  new_esther_curve(
    ordinates = out_ords,
    title = title_str,
    metadata = list(
      outcome = outcome,
      rank = rank,
      by = by,
      curve_type = curve_type,
      simultaneous_bands = simultaneous_bands,
      confidence_level = confidence_level
    )
  )
}

survey_mean <- function(
  data,
  outcome,
  by = NULL,
  scale = 1.0,
  proportion_ci = TRUE,
  confidence_level = 0.95,
  weights = NULL,
  strata = NULL,
  psu = NULL,
  fpc = NULL,
  lonely_psu = "remove"
) {
  svy <- .coerce_survey(data, weights = weights, strata = strata, psu = psu, fpc = fpc, lonely_psu = lonely_psu)
  df <- svy$df
  for (col in c(outcome, by)) {
    if (!is.null(col) && !(col %in% names(df))) {
      stop(sprintf("Column `%s` not found in `data`.", col), call. = FALSE)
    }
  }

  d_arr <- .get_design_arrays(svy)
  w_full <- d_arr$weights
  req <- c(outcome, by)
  valid <- stats::complete.cases(df[, req, drop = FALSE])
  y_raw <- .to_clean_numeric(df[[outcome]], outcome)
  is_prop <- isTRUE(proportion_ci) && all(y_raw[valid] >= 0.0 & y_raw[valid] <= 1.0)

  groups <- if (is.null(by)) {
    list(Overall = valid)
  } else {
    by_raw <- df[[by]]
    lv <- if (is.factor(by_raw)) levels(by_raw)[levels(by_raw) %in% as.character(by_raw[valid])] else sort(unique(as.character(by_raw[valid])))
    stats::setNames(lapply(lv, function(g) valid & (as.character(by_raw) == g)), lv)
  }

  rows <- list()
  for (g_nm in names(groups)) {
    m_g <- groups[[g_nm]]
    w_g <- w_full[m_g]
    y_g <- y_raw[m_g]
    mu_raw <- .weighted_mean(y_g, w_g)
    est <- mu_raw * scale

    psi_full <- numeric(nrow(df))
    psi_full[m_g] <- (y_g - mu_raw) * scale
    vsum <- ultimate_cluster_variance(
      psi = psi_full,
      weights = w_full,
      strata = d_arr$strata,
      psu = d_arr$psu,
      fpc = d_arr$fpc,
      domain_mask = m_g,
      lonely_psu = svy$lonely_psu
    )
    se_s <- vsum$se[1L]
    crit <- stats::qt(1.0 - 0.5 * (1.0 - confidence_level), df = vsum$df)

    if (is_prop && mu_raw > 0.0 && mu_raw < 1.0 && se_s > 0.0) {
      se_raw <- se_s / scale
      se_logit <- se_raw / (mu_raw * (1.0 - mu_raw))
      lg <- log(mu_raw / (1.0 - mu_raw))
      ci_l <- stats::plogis(lg - crit * se_logit) * scale
      ci_h <- stats::plogis(lg + crit * se_logit) * scale
    } else {
      ci_l <- est - crit * se_s
      ci_h <- est + crit * se_s
    }
    cv_pct <- if (abs(est) > 1e-15) (se_s / abs(est)) * 100.0 else NA_real_

    row_df <- data.frame(
      measure = sprintf("Survey mean of %s", outcome),
      estimate = est,
      se = se_s,
      ci_low = ci_l,
      ci_high = ci_h,
      cv_pct = cv_pct,
      n_obs = sum(m_g),
      sum_weights = sum(w_g),
      deff = vsum$deff[1L],
      stringsAsFactors = FALSE
    )
    if (!is.null(by)) {
      row_df <- cbind(data.frame(measure = row_df$measure, domain = g_nm, stringsAsFactors = FALSE), row_df[, -1L, drop = FALSE])
    }
    rows[[length(rows) + 1L]] <- row_df
  }

  tbl <- do.call(rbind, rows)
  rownames(tbl) <- NULL
  new_esther_result(
    table = tbl,
    title = if (is.null(by)) sprintf("Survey mean of %s", outcome) else sprintf("Survey mean of %s by %s", outcome, by),
    metadata = list(
      outcome = outcome,
      dimension = if (is.null(by)) "Overall" else by,
      measures = sprintf("Survey mean of %s", outcome),
      data_mode = "survey",
      vce_method = "if",
      n_obs = sum(valid),
      sum_weights = sum(w_full[valid]),
      confidence_level = confidence_level
    )
  )
}
