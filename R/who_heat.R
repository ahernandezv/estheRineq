# Complete Suite of all 21 WHO Health Inequality Monitor (`healthequal` / HEAT) Summary Measures

ALL_ORDERED_MEASURES <- c(
  "d", "r", "par", "paf", "aci", "rci", "sii", "rii",
  "bgv", "bgsd", "covar", "mdmw", "mdmu", "mdbw", "mdbu",
  "mdrw", "mdru", "idisw", "idisu", "ti", "mld"
)

ALL_UNORDERED_MEASURES <- c(
  "d", "r", "par", "paf", "bgv", "bgsd", "covar", "mdmw", "mdmu",
  "mdbw", "mdbu", "mdrw", "mdru", "idisw", "idisu", "ti", "mld"
)

WHO_MEASURE_NAMES <- c(
  d = "Difference",
  r = "Ratio",
  par = "Population attributable risk",
  paf = "Population attributable fraction (%)",
  aci = "Absolute concentration index",
  rci = "Relative concentration index (%)",
  sii = "Slope index of inequality",
  rii = "Relative index of inequality",
  bgv = "Between-group variance",
  bgsd = "Between-group standard deviation",
  covar = "Between-group coefficient of variation (%)",
  mdmw = "Weighted mean difference from mean",
  mdmu = "Unweighted mean difference from mean",
  mdbw = "Weighted mean difference from best subgroup",
  mdbu = "Unweighted mean difference from best subgroup",
  mdrw = "Weighted mean difference from reference subgroup",
  mdru = "Unweighted mean difference from reference subgroup",
  idisw = "Weighted index of disparity (%)",
  idisu = "Unweighted index of disparity (%)",
  ti = "Theil index (per 1,000)",
  mld = "Mean log deviation (per 1,000)"
)

.normalize_who_code <- function(code) {
  c_low <- tolower(trimws(code))
  aliases <- c(parisk = "par", cov = "covar", theil = "ti")
  if (c_low %in% names(aliases)) unname(aliases[[c_low]]) else c_low
}

.resolve_who_measures <- function(measures, ordered, has_ref) {
  if (identical(measures, "all")) {
    base <- if (isTRUE(ordered)) ALL_ORDERED_MEASURES else ALL_UNORDERED_MEASURES
    return(base)
  }
  codes <- unique(vapply(measures, .normalize_who_code, character(1L)))
  bad <- setdiff(codes, ALL_ORDERED_MEASURES)
  if (length(bad) > 0L) {
    stop(sprintf("Unknown WHO measure code(s): %s.", paste(bad, collapse = ", ")), call. = FALSE)
  }
  if (!isTRUE(ordered) && any(codes %in% c("aci", "rci", "sii", "rii"))) {
    stop("Ordered measures (`aci`, `rci`, `sii`, `rii`) require `ordered = TRUE`.", call. = FALSE)
  }
  codes
}

.eval_single_who_code <- function(
  code, y, p, ordered, favorable, ref_idx, scale, heat_scaling
) {
  k <- length(y)
  mu <- sum(p * y)

  # Identify advantaged/best vs disadvantaged/worst subgroup
  if (isTRUE(ordered)) {
    adv_val <- y[k]
    dis_val <- y[1L]
    best_val <- if (identical(favorable, FALSE)) y[k] else y[k]
    # Note: for ordered WHO HEAT measures, PAR = y_advantaged - mu for favorable, or y_advantaged - mu (negative) / |PAR|
    # Match exact WHO `healthequal`: for ordered favorable, ref_par = y[K]; for ordered adverse, ref_par = y[K] (most advantaged)
    # Wait: for adverse unordered, best = min(y); for favorable unordered, best = max(y)!
    if (is.null(favorable) || isTRUE(favorable)) {
      best_sub <- if (isTRUE(ordered)) y[k] else max(y)
      hi_val <- if (isTRUE(ordered)) y[k] else max(y)
      lo_val <- if (isTRUE(ordered)) y[1L] else min(y)
    } else {
      best_sub <- if (isTRUE(ordered)) y[k] else min(y)
      hi_val <- if (isTRUE(ordered)) y[1L] else max(y)
      lo_val <- if (isTRUE(ordered)) y[k] else min(y)
    }
  } else {
    if (is.null(favorable)) {
      stop("`favorable = TRUE` or `FALSE` is required for unordered WHO summary measures.", call. = FALSE)
    }
    best_sub <- if (isTRUE(favorable)) max(y) else min(y)
    hi_val <- max(y)
    lo_val <- min(y)
  }

  # For ordered dimensions, D = y_advantaged - y_disadvantaged when favorable != FALSE, or |max - min| on unordered
  if (code == "d") {
    if (isTRUE(ordered)) return(y[k] - y[1L])
    return(hi_val - lo_val)
  }
  if (code == "r") {
    if (isTRUE(ordered)) return(if (abs(y[1L]) > 1e-15) y[k] / y[1L] else NA_real_)
    return(if (abs(lo_val) > 1e-15) hi_val / lo_val else NA_real_)
  }
  if (code == "par") {
    if (is.null(favorable) || isTRUE(favorable)) {
      return(best_sub - mu)
    } else {
      return(best_sub - mu)
    }
  }
  if (code == "paf") {
    par_v <- best_sub - mu
    return(if (abs(mu) > 1e-15) (par_v / mu) * 100.0 else NA_real_)
  }
  if (code == "aci") {
    r_res <- compute_fractional_ranks(seq_len(k) - 1.0, weights = p, rank_order = "asc")
    return(sum(p * y * (2.0 * r_res$ridit - 1.0)))
  }
  if (code == "rci") {
    r_res <- compute_fractional_ranks(seq_len(k) - 1.0, weights = p, rank_order = "asc")
    aci_v <- sum(p * y * (2.0 * r_res$ridit - 1.0))
    mult <- if (isTRUE(heat_scaling)) 100.0 else 1.0
    return(if (abs(mu) > 1e-15) (aci_v / mu) * mult else NA_real_)
  }
  if (code %in% c("sii", "rii")) {
    r_res <- compute_fractional_ranks(seq_len(k) - 1.0, weights = p, rank_order = "asc")
    rid <- r_res$ridit
    y_prop <- y / scale
    if (all(y_prop > 0.0 & y_prop < 1.0)) {
      fit <- .fit_survey_glm(y_prop, cbind(1.0, rid), weights = p, family = "binomial", link = "logit")
      p1 <- stats::plogis(fit$params[1L] + fit$params[2L]) * scale
      p0 <- stats::plogis(fit$params[1L]) * scale
    } else {
      fit <- .fit_survey_glm(y, cbind(1.0, rid), weights = p, family = "gaussian", link = "identity")
      p1 <- fit$params[1L] + fit$params[2L]
      p0 <- fit$params[1L]
    }
    return(if (code == "sii") (p1 - p0) else (if (abs(p0) > 1e-15) p1 / p0 else NA_real_))
  }
  if (code == "bgv") {
    return(sum(p * ((y - mu)^2)))
  }
  if (code == "bgsd") {
    return(sqrt(sum(p * ((y - mu)^2))))
  }
  if (code == "covar") {
    return(if (abs(mu) > 1e-15) (sqrt(sum(p * ((y - mu)^2))) / mu) * 100.0 else NA_real_)
  }
  if (code == "mdmw") {
    return(sum(p * abs(y - mu)))
  }
  if (code == "mdmu") {
    return(mean(abs(y - mu)))
  }
  best_abs <- if (is.null(favorable) || isTRUE(favorable)) max(y) else min(y)
  if (code == "mdbw") {
    return(sum(p * abs(y - best_abs)))
  }
  if (code == "mdbu") {
    return(mean(abs(y - best_abs)))
  }
  r_val <- y[ref_idx]
  if (code == "mdrw") {
    return(sum(p * abs(y - r_val)))
  }
  if (code == "mdru") {
    return(mean(abs(y - r_val)))
  }
  if (code == "idisw") {
    return(if (abs(mu) > 1e-15) (sum(p * abs(y - best_abs)) / mu) * 100.0 else NA_real_)
  }
  if (code == "idisu") {
    denom_k <- if (k > 1L && abs(best_abs) > 1e-15) (k - 1.0) * best_abs else NA_real_
    return(if (is.finite(denom_k)) (sum(abs(y - best_abs)) / denom_k) * 100.0 else NA_real_)
  }
  if (code == "ti") {
    if (any(y <= 0) || mu <= 0) return(NA_real_)
    mult <- if (isTRUE(heat_scaling)) 1000.0 else 1.0
    return(sum(p * (y / mu) * log(y / mu)) * mult)
  }
  if (code == "mld") {
    if (any(y <= 0) || mu <= 0) return(NA_real_)
    mult <- if (isTRUE(heat_scaling)) 1000.0 else 1.0
    return(sum(p * log(mu / y)) * mult)
  }
  NA_real_
}

.compute_who_grouped <- function(
  gdata, target_codes, ordered, favorable, reference_group, scale, heat_scaling, confidence_level, n_sim, seed
) {
  df <- gdata$df
  y <- .to_clean_numeric(df[[gdata$estimate]], gdata$estimate)
  pop <- if (is.null(gdata$population)) rep(1.0, nrow(df)) else .to_clean_numeric(df[[gdata$population]], gdata$population)
  p <- pop / sum(pop)
  sg_names <- as.character(df[[gdata$subgroup]])

  ref_idx <- if (!is.null(reference_group)) {
    idx <- match(as.character(reference_group), sg_names)
    if (is.na(idx)) stop(sprintf("`reference_group = '%s'` not found in subgroups.", reference_group), call. = FALSE)
    idx
  } else {
    if (is.null(favorable) || isTRUE(favorable)) which.max(y) else which.min(y)
  }

  eval_vec <- function(y_vec) {
    vapply(target_codes, function(cd) {
      .eval_single_who_code(cd, y_vec, p, ordered, favorable, ref_idx, scale, heat_scaling)
    }, numeric(1L))
  }

  ests <- eval_vec(y)
  se_vec <- rep(NA_real_, length(target_codes))
  ci_l <- rep(NA_real_, length(target_codes))
  ci_h <- rep(NA_real_, length(target_codes))

  if (!is.null(gdata$se) && n_sim >= 20L) {
    se_sub <- .to_clean_numeric(df[[gdata$se]], gdata$se)
    old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
    on.exit({
      if (is.null(old_seed)) { if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv) } else assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }, add = TRUE)
    set.seed(as.integer(seed))

    sim_mat <- matrix(NA_real_, nrow = n_sim, ncol = length(target_codes))
    for (s_i in seq_len(n_sim)) {
      y_sim <- stats::rnorm(length(y), mean = y, sd = se_sub)
      if (any(target_codes %in% c("ti", "mld", "r", "sii", "rii"))) {
        y_sim <- pmax(y_sim, 1e-6)
      }
      sim_mat[s_i, ] <- eval_vec(y_sim)
    }
    alpha <- 1.0 - confidence_level
    z_c <- stats::qnorm(1.0 - 0.5 * alpha)
    for (j in seq_along(target_codes)) {
      col_j <- sim_mat[, j]
      col_j <- col_j[is.finite(col_j)]
      if (length(col_j) >= 10L) {
        se_vec[j] <- stats::sd(col_j)
        if (target_codes[j] %in% c("r", "rii", "ti", "mld")) {
          ci_l[j] <- stats::quantile(col_j, probs = 0.5 * alpha, names = FALSE)
          ci_h[j] <- stats::quantile(col_j, probs = 1.0 - 0.5 * alpha, names = FALSE)
        } else {
          ci_l[j] <- ests[j] - z_c * se_vec[j]
          ci_h[j] <- ests[j] + z_c * se_vec[j]
        }
      }
    }

    # Exact closed-form analytical SEs for D and R when independent subgroup SEs are available
    for (j in seq_along(target_codes)) {
      cd <- target_codes[j]
      if (cd == "d" && isTRUE(ordered)) {
        se_d <- sqrt(se_sub[length(y)]^2 + se_sub[1L]^2)
        se_vec[j] <- se_d
        ci_l[j] <- ests[j] - z_c * se_d
        ci_h[j] <- ests[j] + z_c * se_d
      } else if (cd == "r" && isTRUE(ordered) && y[1L] > 0 && y[length(y)] > 0) {
        se_log_r <- sqrt((se_sub[length(y)] / y[length(y)])^2 + (se_sub[1L] / y[1L])^2)
        se_vec[j] <- ests[j] * se_log_r
        ci_l[j] <- exp(log(ests[j]) - z_c * se_log_r)
        ci_h[j] <- exp(log(ests[j]) + z_c * se_log_r)
      }
    }
  }

  small_cells <- 0L
  n_tot <- nrow(df)
  if (!is.null(gdata$sample_size) && (gdata$sample_size %in% names(df))) {
    n_sub <- .to_clean_numeric(df[[gdata$sample_size]], gdata$sample_size)
    small_cells <- sum(n_sub < 25, na.rm = TRUE)
    n_tot <- sum(n_sub, na.rm = TRUE)
  }

  tbl <- data.frame(
    measure = unname(WHO_MEASURE_NAMES[target_codes]),
    estimate = unname(ests),
    se = se_vec,
    ci_low = ci_l,
    ci_high = ci_h,
    n_obs = rep(as.integer(n_tot), length(target_codes)),
    sum_weights = rep(sum(pop), length(target_codes)),
    stringsAsFactors = FALSE
  )

  new_esther_result(
    table = tbl,
    title = sprintf("Summary inequality measures of %s by %s", gdata$estimate, gdata$subgroup),
    metadata = list(
      outcome = gdata$estimate,
      dimension = gdata$subgroup,
      measures = tbl$measure,
      favorable = favorable,
      data_mode = "grouped",
      vce_method = "monte_carlo",
      n_obs = as.integer(n_tot),
      sum_weights = sum(pop),
      small_cells_under_25 = as.integer(small_cells),
      confidence_level = confidence_level
    )
  )
}

who_summary <- function(
  data,
  outcome = NULL,
  dimension = NULL,
  measures = "all",
  ordered = NULL,
  favorable = NULL,
  reference_group = NULL,
  scale = NULL,
  heat_scaling = TRUE,
  confidence_level = 0.95,
  n_sim = 2000L,
  seed = 42L,
  weights = NULL,
  strata = NULL,
  psu = NULL,
  fpc = NULL,
  lonely_psu = "remove"
) {
  polarity_req <- c("d", "r", "par", "paf", "mdbw", "mdbu", "idisw", "idisu")

  if (inherits(data, "esther_grouped")) {
    eff_ord <- if (is.null(ordered)) data$ordered else isTRUE(ordered)
    eff_fav <- if (is.null(favorable)) data$favorable else favorable
    eff_ref <- if (is.null(reference_group)) data$reference_group else reference_group
    eff_sc <- if (is.null(scale)) data$scale else as.double(scale)
    t_codes <- .resolve_who_measures(measures, eff_ord, !is.null(eff_ref))
    if (!eff_ord && is.null(eff_fav) && any(t_codes %in% polarity_req)) {
      stop("`favorable = TRUE` or `FALSE` is required for unordered WHO summary measures.", call. = FALSE)
    }
    return(.compute_who_grouped(
      data, t_codes, eff_ord, eff_fav, eff_ref, eff_sc, heat_scaling, confidence_level, n_sim, seed
    ))
  }

  if (is.null(outcome) || is.null(dimension)) {
    stop("`outcome` and `dimension` are required when `data` is microdata.", call. = FALSE)
  }

  svy <- .coerce_survey(data, weights = weights, strata = strata, psu = psu, fpc = fpc, lonely_psu = lonely_psu)
  df <- svy$df
  for (col in c(outcome, dimension)) {
    if (!(col %in% names(df))) stop(sprintf("Column `%s` not found in `data`.", col), call. = FALSE)
  }

  eff_ord <- if (is.null(ordered)) TRUE else isTRUE(ordered)
  eff_sc <- if (is.null(scale)) 1.0 else as.double(scale)
  t_codes <- .resolve_who_measures(measures, eff_ord, !is.null(reference_group))
  if (!eff_ord && is.null(favorable) && any(t_codes %in% polarity_req)) {
    stop("`favorable = TRUE` or `FALSE` is required for unordered WHO summary measures.", call. = FALSE)
  }

  d_arr <- .get_design_arrays(svy)
  w_full <- d_arr$weights
  valid <- stats::complete.cases(df[, c(outcome, dimension), drop = FALSE])
  y_full <- numeric(nrow(df))
  y_full[valid] <- .to_clean_numeric(df[[outcome]][valid], outcome)
  w_valid_sum <- sum(w_full[valid])

  dim_raw <- df[[dimension]]
  subgroups <- if (is.factor(dim_raw)) {
    lv <- levels(dim_raw)
    lv[lv %in% as.character(dim_raw[valid])]
  } else if (is.numeric(dim_raw)) {
    as.character(sort(unique(dim_raw[valid])))
  } else {
    sort(unique(as.character(dim_raw[valid])))
  }

  j_len <- length(subgroups)
  if (j_len < 2L) stop(sprintf("Dimension `%s` has fewer than 2 observed subgroups.", dimension), call. = FALSE)

  psi_means <- matrix(0.0, nrow = nrow(df), ncol = j_len)
  y_j <- numeric(j_len)
  pop_j <- numeric(j_len)
  n_j <- integer(j_len)

  for (j in seq_len(j_len)) {
    m_j <- valid & (as.character(dim_raw) == subgroups[j])
    n_j[j] <- sum(m_j)
    pop_j[j] <- sum(w_full[m_j])
    mu_j <- (sum(w_full[m_j] * y_full[m_j]) / pop_j[j]) * eff_sc
    y_j[j] <- mu_j
    psi_means[m_j, j] <- (w_valid_sum / pop_j[j]) * (y_full[m_j] * eff_sc - mu_j)
  }

  vsum_means <- ultimate_cluster_variance(
    psi = psi_means,
    weights = w_full,
    strata = d_arr$strata,
    psu = d_arr$psu,
    fpc = d_arr$fpc,
    domain_mask = valid,
    lonely_psu = svy$lonely_psu
  )
  small_cells <- sum(n_j < 25L)

  g_df <- data.frame(
    subgroup = subgroups,
    est = y_j,
    se = vsum_means$se,
    pop = pop_j,
    n = n_j,
    stringsAsFactors = FALSE
  )
  names(g_df)[1L] <- dimension
  gdata <- grouped_health_data(
    g_df, subgroup = dimension, estimate = "est", se = "se", population = "pop",
    ordered = eff_ord, favorable = favorable, reference_group = reference_group,
    scale = eff_sc, sample_size = "n"
  )

  sub_codes <- setdiff(t_codes, c("aci", "rci", "sii", "rii"))
  grouped_rows <- list()
  if (length(sub_codes) > 0L) {
    g_res <- .compute_who_grouped(
      gdata, sub_codes, eff_ord, favorable, reference_group, eff_sc, heat_scaling, confidence_level, n_sim, seed
    )
    for (idx_c in seq_along(sub_codes)) {
      rw <- g_res$table[idx_c, , drop = FALSE]
      rw$n_obs <- sum(valid)
      rw$sum_weights <- w_valid_sum
      grouped_rows[[sub_codes[idx_c]]] <- rw
    }
  }

  micro_rows <- list()
  if (any(t_codes %in% c("aci", "rci", "sii", "rii"))) {
    df_tmp <- df
    df_tmp[["..who_rank.."]] <- match(as.character(dim_raw), subgroups) - 1.0
    df_tmp[["..who_y_sc.."]] <- y_full * eff_sc
    svy_tmp <- survey_health_data(
      df_tmp, weights = svy$weights, strata = svy$strata, psu = svy$psu, fpc = svy$fpc,
      rep_weights = svy$rep_weights, rep_type = svy$rep_type, rep_scale = svy$rep_scale, lonely_psu = svy$lonely_psu
    )
    if ("aci" %in% t_codes) {
      r_aci <- concentration_index(svy_tmp, outcome = "..who_y_sc..", rank = "..who_rank..", method = "generalized", confidence_level = confidence_level)
      rw <- r_aci$table[1L, c("measure", "estimate", "se", "ci_low", "ci_high", "n_obs", "sum_weights")]
      rw$measure <- WHO_MEASURE_NAMES[["aci"]]
      micro_rows[["aci"]] <- rw
    }
    if ("rci" %in% t_codes) {
      r_rci <- concentration_index(svy_tmp, outcome = "..who_y_sc..", rank = "..who_rank..", method = "standard", confidence_level = confidence_level)
      rw <- r_rci$table[1L, c("measure", "estimate", "se", "ci_low", "ci_high", "n_obs", "sum_weights")]
      mult <- if (isTRUE(heat_scaling)) 100.0 else 1.0
      rw$measure <- WHO_MEASURE_NAMES[["rci"]]
      rw$estimate <- rw$estimate * mult
      rw$se <- rw$se * mult
      rw$ci_low <- rw$ci_low * mult
      rw$ci_high <- rw$ci_high * mult
      micro_rows[["rci"]] <- rw
    }
    if (any(t_codes %in% c("sii", "rii"))) {
      r_sr <- sii_rii(svy_tmp, outcome = outcome, rank = "..who_rank..", scale = eff_sc, confidence_level = confidence_level)
      if ("sii" %in% t_codes) {
        rw <- r_sr$table[1L, c("measure", "estimate", "se", "ci_low", "ci_high", "n_obs", "sum_weights")]
        rw$measure <- WHO_MEASURE_NAMES[["sii"]]
        micro_rows[["sii"]] <- rw
      }
      if ("rii" %in% t_codes) {
        rw <- r_sr$table[2L, c("measure", "estimate", "se", "ci_low", "ci_high", "n_obs", "sum_weights")]
        rw$measure <- WHO_MEASURE_NAMES[["rii"]]
        micro_rows[["rii"]] <- rw
      }
    }
  }

  out_list <- lapply(t_codes, function(cd) {
    if (!is.null(micro_rows[[cd]])) micro_rows[[cd]] else grouped_rows[[cd]]
  })
  out_tbl <- do.call(rbind, out_list)
  rownames(out_tbl) <- NULL

  new_esther_result(
    table = out_tbl,
    title = sprintf("Summary inequality measures of %s by %s", outcome, dimension),
    metadata = list(
      outcome = outcome,
      rank = dimension,
      dimension = dimension,
      measures = out_tbl$measure,
      favorable = favorable,
      data_mode = "survey",
      vce_method = "if",
      n_obs = sum(valid),
      sum_weights = w_valid_sum,
      n_strata = vsum_means$n_strata,
      n_psu = vsum_means$n_psu,
      df = vsum_means$df,
      mean_deff = mean(vsum_means$deff, na.rm = TRUE),
      small_cells_under_25 = as.integer(small_cells),
      confidence_level = confidence_level
    )
  )
}
