# Complex Survey Variance Engines: Ultimate-Cluster Taylor, Convenient/Newey, Replicate, and Rao-Wu Bootstrap

ultimate_cluster_variance <- function(
  psi,
  weights,
  strata = NULL,
  psu = NULL,
  fpc = NULL,
  domain_mask = NULL,
  lonely_psu = c("remove", "adjust", "certainty", "average")
) {
  lonely_psu <- match.arg(lonely_psu)
  psi_mat <- if (is.matrix(psi)) psi else matrix(as.double(psi), ncol = 1L)
  n <- nrow(psi_mat)
  q <- ncol(psi_mat)
  w <- as.double(weights)

  if (!is.null(domain_mask)) {
    dm <- as.logical(domain_mask)
    psi_eff <- psi_mat * as.double(dm)
    w_dom_sum <- sum(w[dm])
  } else {
    dm <- rep(TRUE, n)
    psi_eff <- psi_mat
    w_dom_sum <- sum(w)
  }

  if (w_dom_sum <= 0) {
    stop("Zero total sampling weight inside target subpopulation.", call. = FALSE)
  }

  z_obs <- (w / w_dom_sum) * psi_eff

  # SRS baseline variance on domain for Kish/survey DEFF
  n_dom <- sum(dm)
  v_srs_diag <- rep(NA_real_, q)
  if (n_dom > 1L) {
    w_d <- w[dm] / w_dom_sum
    p_d <- psi_mat[dm, , drop = FALSE]
    p_bar <- colSums(p_d * w_d)
    centered <- sweep(p_d, 2L, p_bar, "-")
    s2_d <- colSums((centered^2) * w_d) * (n_dom / (n_dom - 1.0))
    v_srs_diag <- s2_d / n_dom
  }

  st_vec <- if (is.null(strata)) rep("__S0__", n) else as.character(strata)
  psu_vec <- if (is.null(psu)) paste0("__OBS_", seq_len(n)) else as.character(psu)

  uniq_st <- unique(st_vec)
  v_total <- matrix(0.0, nrow = q, ncol = q)
  total_psus <- 0L
  non_lonely_strata <- 0L
  lonely_totals <- list()
  non_lonely_contribs <- list()

  for (st in uniq_st) {
    idx_h <- which(st_vec == st)
    psus_h <- psu_vec[idx_h]
    z_h <- rowsum(z_obs[idx_h, , drop = FALSE], psus_h, reorder = FALSE)
    m_h <- nrow(z_h)
    total_psus <- total_psus + m_h

    f_h <- 0.0
    if (!is.null(fpc)) {
      f_val <- as.double(fpc[idx_h[1L]])
      if (is.finite(f_val) && f_val > 0) {
        f_h <- if (f_val <= 1.0) f_val else min(m_h / f_val, 1.0)
      }
    }
    fpc_mult <- max(0.0, 1.0 - f_h)

    if (m_h >= 2L) {
      non_lonely_strata <- non_lonely_strata + 1L
      z_bar_h <- colMeans(z_h)
      diff_h <- sweep(z_h, 2L, z_bar_h, "-")
      s_h <- (m_h / (m_h - 1.0)) * fpc_mult * crossprod(diff_h)
      v_total <- v_total + s_h
      non_lonely_contribs[[length(non_lonely_contribs) + 1L]] <- s_h
    } else if (m_h == 1L) {
      lonely_totals[[length(lonely_totals) + 1L]] <- list(z = as.double(z_h[1L, ]), fpc = fpc_mult)
    }
  }

  if (length(lonely_totals) > 0L) {
    if (lonely_psu == "adjust") {
      z_grand <- colSums(z_obs) / max(total_psus, 1L)
      for (lt in lonely_totals) {
        d1 <- lt$z - z_grand
        v_total <- v_total + lt$fpc * tcrossprod(d1)
      }
    } else if (lonely_psu == "average" && length(non_lonely_contribs) > 0L) {
      avg_s <- Reduce(`+`, non_lonely_contribs) / length(non_lonely_contribs)
      v_total <- v_total + length(lonely_totals) * avg_s
    }
  }

  df_deg <- as.double(max(total_psus - max(non_lonely_strata, 1L), 1L))
  se <- sqrt(pmax(diag(v_total), 0.0))
  deff <- ifelse(!is.na(v_srs_diag) & v_srs_diag > 1e-24, diag(v_total) / v_srs_diag, NA_real_)

  list(
    vcov = v_total,
    se = se,
    df = df_deg,
    n_strata = length(uniq_st),
    n_psu = total_psus,
    deff = deff
  )
}

convenient_regression_variance <- function(
  y,
  ridit,
  weights,
  strata = NULL,
  psu = NULL,
  fpc = NULL,
  scale_factor = 1.0,
  newey_lags = 0L,
  lonely_psu = "remove"
) {
  y_arr <- as.double(y)
  r_arr <- as.double(ridit)
  n <- length(y_arr)
  w_norm <- as.double(weights) / sum(weights)

  r_bar <- sum(w_norm * r_arr)
  sigma2_r <- sum(w_norm * ((r_arr - r_bar)^2))
  if (sigma2_r <= 1e-15) {
    stop("Zero variance in fractional ranks; convenient regression undefined.", call. = FALSE)
  }

  lhs <- 2.0 * sigma2_r * y_arr * scale_factor
  x_mat <- cbind(1.0, r_arr)
  xtwx <- crossprod(x_mat, x_mat * w_norm)
  beta <- solve(xtwx, crossprod(x_mat, lhs * w_norm))
  resid <- lhs - as.double(x_mat %*% beta)

  if (newey_lags > 0L) {
    ord <- order(r_arr)
    e_s <- resid[ord]
    r_s <- r_arr[ord] - r_bar
    w_s <- w_norm[ord]
    u_s <- w_s * r_s * e_s
    gamma_0 <- sum(u_s^2)
    gamma_sum <- 0.0
    for (l_idx in seq_len(newey_lags)) {
      if (l_idx < n) {
        w_bart <- 1.0 - l_idx / (newey_lags + 1.0)
        gamma_l <- sum(u_s[(l_idx + 1L):n] * u_s[1L:(n - l_idx)])
        gamma_sum <- gamma_sum + 2.0 * w_bart * gamma_l
      }
    }
    var_b1 <- max((gamma_0 + gamma_sum) / (sigma2_r^2), 0.0)
    return(list(
      vcov = matrix(var_b1, 1L, 1L),
      se = sqrt(var_b1),
      df = as.double(max(n - 2L, 1L)),
      n_strata = 1L,
      n_psu = n,
      deff = NA_real_
    ))
  }

  if (is.null(strata) && is.null(psu)) {
    dof_hc1 <- if (n > 2L) n / (n - 2.0) else 1.0
    meat <- crossprod(x_mat, x_mat * ((w_norm * resid)^2)) * dof_hc1
    cov_beta <- solve(xtwx, t(solve(xtwx, meat)))
    var_b1 <- max(cov_beta[2L, 2L], 0.0)
    return(list(
      vcov = matrix(var_b1, 1L, 1L),
      se = sqrt(var_b1),
      df = as.double(max(n - 2L, 1L)),
      n_strata = 1L,
      n_psu = n,
      deff = NA_real_
    ))
  }

  psi_b1 <- ((r_arr - r_bar) * resid) / sigma2_r
  ultimate_cluster_variance(
    psi = psi_b1,
    weights = weights,
    strata = strata,
    psu = psu,
    fpc = fpc,
    lonely_psu = lonely_psu
  )
}

replicate_weights_variance <- function(
  estimator_fn,
  full_weights,
  rep_weights_mat,
  rep_type = "BRR",
  rep_scale = NULL,
  fay_rho = 0.5
) {
  theta_hat <- as.double(estimator_fn(as.double(full_weights)))
  r_reps <- ncol(rep_weights_mat)
  q <- length(theta_hat)
  rep_ests <- matrix(0.0, nrow = r_reps, ncol = q)
  for (r in seq_len(r_reps)) {
    rep_ests[r, ] <- as.double(estimator_fn(as.double(rep_weights_mat[, r])))
  }

  diffs <- sweep(rep_ests, 2L, theta_hat, "-")
  rt <- toupper(trimws(rep_type))
  if (!is.null(rep_scale)) {
    scale <- as.double(rep_scale)
  } else if (rt == "BRR") {
    scale <- 1.0 / r_reps
  } else if (rt == "FAY") {
    scale <- 1.0 / (r_reps * ((1.0 - fay_rho)^2))
  } else if (rt %in% c("JK1", "JKN", "JACKKNIFE")) {
    scale <- (r_reps - 1.0) / r_reps
  } else {
    scale <- 1.0 / max(r_reps - 1L, 1L)
  }

  vcov_mat <- scale * crossprod(diffs)
  list(
    vcov = vcov_mat,
    se = sqrt(pmax(diag(vcov_mat), 0.0)),
    df = as.double(max(r_reps - 1L, 1L)),
    n_strata = 1L,
    n_psu = r_reps,
    deff = rep(NA_real_, q)
  )
}

rao_wu_survey_bootstrap <- function(
  estimator_fn,
  weights,
  strata = NULL,
  psu = NULL,
  n_boot = 200L,
  seed = 42L
) {
  if (!is.null(seed)) {
    old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    } else {
      NULL
    }
    on.exit({
      if (is.null(old_seed)) {
        if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      } else {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(as.integer(seed))
  }

  w <- as.double(weights)
  n <- length(w)
  st_vec <- if (is.null(strata)) rep("__S0__", n) else as.character(strata)
  psu_vec <- if (is.null(psu)) paste0("__OBS_", seq_len(n)) else as.character(psu)

  strata_groups <- list()
  total_psus <- 0L
  uniq_st <- unique(st_vec)
  for (st in uniq_st) {
    idx_h <- which(st_vec == st)
    psus_h <- psu_vec[idx_h]
    psu_split <- split(idx_h, psus_h)
    strata_groups[[length(strata_groups) + 1L]] <- psu_split
    total_psus <- total_psus + length(psu_split)
  }

  pt_est <- as.double(estimator_fn(w))
  q <- length(pt_est)
  boot_draws <- matrix(NA_real_, nrow = n_boot, ncol = q)

  for (b in seq_len(n_boot)) {
    mult <- rep(1.0, n)
    for (psu_list in strata_groups) {
      n_h <- length(psu_list)
      if (n_h <= 1L) next
      m_h <- n_h - 1L
      sampled <- sample.int(n_h, size = m_h, replace = TRUE)
      counts <- tabulate(sampled, nbins = n_h)
      factors <- counts * (n_h / m_h)
      for (c_idx in seq_len(n_h)) {
        mult[psu_list[[c_idx]]] <- factors[c_idx]
      }
    }
    w_b <- w * mult
    if (sum(w_b) <= 0) w_b <- w
    val_b <- tryCatch(as.double(estimator_fn(w_b)), error = function(e) rep(NA_real_, q))
    boot_draws[b, ] <- val_b
  }

  ok <- apply(is.finite(boot_draws), 1L, all)
  valid_draws <- boot_draws[ok, , drop = FALSE]
  if (nrow(valid_draws) < 2L) {
    vcov_mat <- matrix(NA_real_, q, q)
  } else {
    vcov_mat <- stats::cov(valid_draws)
  }
  df_deg <- as.double(max(total_psus - length(uniq_st), 1L))
  list(
    pt = pt_est,
    vcov = vcov_mat,
    se = sqrt(pmax(diag(vcov_mat), 0.0)),
    df = df_deg,
    n_strata = length(uniq_st),
    n_psu = total_psus,
    deff = rep(NA_real_, q)
  )
}
