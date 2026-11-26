# Exact O(N log N) Gateaux Influence Functions (IF) & Recentered Influence Functions (RIF)

.influence_rank_functional_raw <- function(y, rank_res, scheme = "standard", param = 2.0) {
  y_arr <- as.double(y)
  w_norm <- rank_res$w_norm
  pk <- rank_res$group_share
  inv <- rank_res$inv_idx
  k_tot <- rank_res$n_groups

  tw <- .compute_tie_integrated_weights(rank_res, scheme = scheme, param = param)
  omega_obs <- tw$omega_obs
  omega_k <- tw$omega_k
  w_low_k <- tw$w_low_k
  w_high_k <- tw$w_high_k

  rs_wy <- rowsum(w_norm * y_arr, inv, reorder = FALSE)
  wy_k <- numeric(k_tot)
  wy_k[unique(inv)] <- as.double(rs_wy[, 1L])
  hbar_k <- ifelse(pk > 0, wy_k / pk, 0.0)
  theta <- sum(wy_k * omega_k)

  c_0 <- -sum(hbar_k * (w_high_k * rank_res$f_high - w_low_k * rank_res$f_low))
  term_jump <- hbar_k * (w_high_k - w_low_k)
  rev_cum <- rev(cumsum(rev(term_jump)))
  suffix_after_k <- if (k_tot > 1L) c(rev_cum[-1L], 0.0) else 0.0

  group_base <- c_0 + hbar_k * w_high_k + suffix_after_k
  psi_theta <- group_base[inv] + (y_arr - hbar_k[inv]) * omega_k[inv]
  list(theta = theta, psi_theta = psi_theta)
}

influence_concentration_index <- function(
  y,
  rank,
  weights = NULL,
  method = "standard",
  bounds = NULL,
  v = 2.0,
  beta = 2.0,
  rank_order = c("asc", "desc")
) {
  rank_order <- match.arg(rank_order)
  y_arr <- as.double(y)
  rank_res <- compute_fractional_ranks(rank, weights = weights, rank_order = rank_order)
  w_norm <- rank_res$w_norm
  mu <- sum(w_norm * y_arr)
  psi_mu <- y_arr - mu

  m <- tolower(trimws(method))
  if (m %in% c("standard", "ci", "rci", "c")) {
    if (abs(mu) < 1e-15) {
      stop("Standard concentration index is undefined when outcome mean `mu == 0`.", call. = FALSE)
    }
    rf <- .influence_rank_functional_raw(y_arr, rank_res, scheme = "standard")
    est <- rf$theta / mu
    psi <- (rf$psi_theta - est * psi_mu) / mu
  } else if (m %in% c("generalized", "gc", "aci")) {
    rf <- .influence_rank_functional_raw(y_arr, rank_res, scheme = "standard")
    est <- rf$theta
    psi <- rf$psi_theta
  } else if (m %in% c("modified", "mc")) {
    if (is.null(bounds)) {
      stop("`bounds = c(a, b)` is required for the modified concentration index.", call. = FALSE)
    }
    a <- as.double(bounds[1L])
    denom <- mu - a
    if (abs(denom) < 1e-15) stop("`mu - a == 0` in modified concentration index.", call. = FALSE)
    rf <- .influence_rank_functional_raw(y_arr, rank_res, scheme = "standard")
    est <- rf$theta / denom
    psi <- (rf$psi_theta - est * psi_mu) / denom
  } else if (m %in% c("wagstaff", "w")) {
    eff_b <- if (is.null(bounds)) c(0.0, 1.0) else as.double(bounds)
    a <- eff_b[1L]; b <- eff_b[2L]
    if (b <= a) stop("Invalid `bounds = c(a, b)`: require `b > a`.", call. = FALSE)
    if (any(y_arr < a - 1e-9 | y_arr > b + 1e-9)) {
      stop(sprintf("Outcome values outside `bounds = c(%g, %g)` for Wagstaff index.", a, b), call. = FALSE)
    }
    denom <- (b - mu) * (mu - a)
    if (denom <= 1e-15) {
      stop("Outcome mean lies on boundary `a` or `b`; Wagstaff index is undefined.", call. = FALSE)
    }
    rf <- .influence_rank_functional_raw(y_arr, rank_res, scheme = "standard")
    scale_w <- b - a
    est <- scale_w * rf$theta / denom
    d_denom <- b + a - 2.0 * mu
    psi <- (scale_w * rf$psi_theta * denom - scale_w * rf$theta * d_denom * psi_mu) / (denom^2)
  } else if (m %in% c("erreygers", "e", "cci")) {
    eff_b <- if (is.null(bounds)) c(0.0, 1.0) else as.double(bounds)
    a <- eff_b[1L]; b <- eff_b[2L]
    if (b <= a) stop("Invalid `bounds = c(a, b)`: require `b > a`.", call. = FALSE)
    if (any(y_arr < a - 1e-9 | y_arr > b + 1e-9)) {
      stop(sprintf("Outcome values outside `bounds = c(%g, %g)` for Erreygers index.", a, b), call. = FALSE)
    }
    rf <- .influence_rank_functional_raw(y_arr, rank_res, scheme = "standard")
    factor_e <- 4.0 / (b - a)
    est <- factor_e * rf$theta
    psi <- factor_e * rf$psi_theta
  } else if (m %in% c("extended", "eci")) {
    rf <- .influence_rank_functional_raw(y_arr, rank_res, scheme = "extended", param = v)
    if (!is.null(bounds)) {
      a <- as.double(bounds[1L]); b <- as.double(bounds[2L])
      scale_ev <- (2.0^(v - 2.0)) * v / (b - a)
      est <- scale_ev * rf$theta
      psi <- scale_ev * rf$psi_theta
    } else {
      if (abs(mu) < 1e-15) stop("Extended CI without `bounds` requires `mu != 0`.", call. = FALSE)
      est <- rf$theta / mu
      psi <- (rf$psi_theta - est * psi_mu) / mu
    }
  } else if (m %in% c("symmetric", "sci")) {
    rf <- .influence_rank_functional_raw(y_arr, rank_res, scheme = "symmetric", param = beta)
    if (!is.null(bounds)) {
      a <- as.double(bounds[1L]); b <- as.double(bounds[2L])
      factor_s <- 4.0 / (b - a)
      est <- factor_s * rf$theta
      psi <- factor_s * rf$psi_theta
    } else {
      if (abs(mu) < 1e-15) stop("Symmetric CI without `bounds` requires `mu != 0`.", call. = FALSE)
      est <- 2.0 * rf$theta / mu
      psi <- 2.0 * (rf$psi_theta - 0.5 * est * psi_mu) / mu
    }
  } else {
    stop(sprintf("Unknown concentration index `method = '%s'`.", method), call. = FALSE)
  }

  list(estimate = est, psi = psi, mu = mu, ridit = rank_res$ridit)
}

compute_rif <- function(estimate, psi) {
  as.double(estimate) + as.double(psi)
}

influence_generalized_entropy <- function(y, weights = NULL, alpha = 1.0) {
  y_arr <- as.double(y)
  n <- length(y_arr)
  w <- if (is.null(weights)) rep(1.0, n) else as.double(weights)
  w_norm <- w / sum(w)

  if (any(y_arr <= 0)) {
    stop(sprintf("Generalized entropy GE(%g) requires strictly positive values (`y > 0`).", alpha), call. = FALSE)
  }
  mu <- sum(w_norm * y_arr)

  if (abs(alpha) < 1e-12) {
    log_y <- log(y_arr)
    m_log <- sum(w_norm * log_y)
    ge <- log(mu) - m_log
    psi <- (y_arr - mu) / mu - (log_y - m_log)
  } else if (abs(alpha - 1.0) < 1e-12) {
    y_log_y <- y_arr * log(pmax(y_arr, 1e-300))
    m_ylog <- sum(w_norm * y_log_y)
    ge <- m_ylog / mu - log(mu)
    psi <- (y_log_y - m_ylog) / mu - (m_ylog / (mu^2) + 1.0 / mu) * (y_arr - mu)
  } else {
    c_f <- 1.0 / (alpha * (alpha - 1.0))
    y_pow <- y_arr^alpha
    m_pow <- sum(w_norm * y_pow)
    mu_pow <- mu^alpha
    ge <- c_f * (m_pow / mu_pow - 1.0)
    psi <- c_f * ((y_pow - m_pow) / mu_pow - alpha * m_pow * (y_arr - mu) / (mu^(alpha + 1.0)))
  }

  list(estimate = ge, psi = psi)
}

influence_atkinson <- function(y, weights = NULL, epsilon = 1.0) {
  if (epsilon < 0) stop("Atkinson inequality aversion `epsilon` must be >= 0.", call. = FALSE)
  y_arr <- as.double(y)
  n <- length(y_arr)
  w <- if (is.null(weights)) rep(1.0, n) else as.double(weights)
  w_norm <- w / sum(w)

  if (any(y_arr <= 0)) stop("Atkinson index requires strictly positive values (`y > 0`).", call. = FALSE)
  mu <- sum(w_norm * y_arr)

  if (abs(epsilon) < 1e-12) {
    return(list(estimate = 0.0, psi = rep(0.0, n), ede = mu))
  }
  if (abs(epsilon - 1.0) < 1e-12) {
    log_y <- log(y_arr)
    m_log <- sum(w_norm * log_y)
    ede <- exp(m_log)
    atk <- 1.0 - ede / mu
    psi_ede <- ede * (log_y - m_log)
    psi <- -(psi_ede * mu - ede * (y_arr - mu)) / (mu^2)
    return(list(estimate = atk, psi = psi, ede = ede))
  }

  p_exp <- 1.0 - epsilon
  y_pow <- y_arr^p_exp
  m_pow <- sum(w_norm * y_pow)
  ede <- m_pow^(1.0 / p_exp)
  atk <- 1.0 - ede / mu
  psi_ede <- (1.0 / p_exp) * (m_pow^(1.0 / p_exp - 1.0)) * (y_pow - m_pow)
  psi <- -(psi_ede * mu - ede * (y_arr - mu)) / (mu^2)
  list(estimate = atk, psi = psi, ede = ede)
}

influence_curve_ordinates <- function(
  y,
  rank = NULL,
  weights = NULL,
  grid = NULL,
  curve_type = c("relative", "generalized", "absolute"),
  rank_order = c("asc", "desc")
) {
  curve_type <- match.arg(curve_type)
  rank_order <- match.arg(rank_order)
  y_arr <- as.double(y)
  r_arr <- if (is.null(rank)) y_arr else rank
  rank_res <- compute_fractional_ranks(r_arr, weights = weights, rank_order = rank_order)
  w_norm <- rank_res$w_norm
  mu <- sum(w_norm * y_arr)

  p_grid <- if (is.null(grid)) seq(0.0, 1.0, length.out = 21L) else as.double(grid)
  fl <- rank_res$f_low
  fh <- rank_res$f_high
  pk <- rank_res$group_share
  inv <- rank_res$inv_idx
  k_tot <- rank_res$n_groups

  rs_wy <- rowsum(w_norm * y_arr, inv, reorder = FALSE)
  wy_k <- numeric(k_tot)
  wy_k[unique(inv)] <- as.double(rs_wy[, 1L])
  hbar_k <- ifelse(pk > 0, wy_k / pk, 0.0)
  cum_wy_before <- c(0.0, cumsum(wy_k)[-k_tot])

  n_obs <- length(y_arr)
  m_pts <- length(p_grid)
  ordinates <- numeric(m_pts)
  psi_mat <- matrix(0.0, nrow = n_obs, ncol = m_pts)

  for (j in seq_len(m_pts)) {
    p <- p_grid[j]
    if (p <= 0.0) {
      ordinates[j] <- 0.0
      next
    }
    if (p >= 1.0) {
      if (curve_type == "relative") {
        ordinates[j] <- 1.0
      } else if (curve_type == "generalized") {
        ordinates[j] <- mu
        psi_mat[, j] <- y_arr - mu
      } else {
        ordinates[j] <- 0.0
      }
      next
    }

    k_idx <- findInterval(p, fh, left.open = TRUE) + 1L
    if (k_idx > k_tot) k_idx <- k_tot
    frac_in_k <- if (pk[k_idx] > 0) (p - fl[k_idx]) / pk[k_idx] else 0.0
    frac_in_k <- min(max(frac_in_k, 0.0), 1.0)

    gl_p <- cum_wy_before[k_idx] + frac_in_k * wy_k[k_idx]
    ind_obs <- ifelse(inv < k_idx, 1.0, ifelse(inv == k_idx, frac_in_k, 0.0))

    # Jann (2016) local conditional expectation smoothing across continuous rank neighborhoods
    if (pk[k_idx] >= 0.02) {
      hbar_q <- hbar_k[k_idx]
    } else {
      bw <- max(0.03, min(0.10, 2.5 / sqrt(max(n_obs, 25L))))
      mid_k <- 0.5 * (fl + fh)
      in_win <- abs(mid_k - p) <= bw
      if (sum(pk[in_win]) > 0) {
        hbar_q <- sum(wy_k[in_win]) / sum(pk[in_win])
      } else {
        hbar_q <- hbar_k[k_idx]
      }
    }

    psi_gl <- ind_obs * (y_arr - hbar_q) + p * hbar_q - gl_p

    if (curve_type == "generalized") {
      ordinates[j] <- gl_p
      psi_mat[, j] <- psi_gl
    } else if (curve_type == "relative") {
      if (abs(mu) < 1e-15) stop("Relative concentration curve requires `mu != 0`.", call. = FALSE)
      c_p <- gl_p / mu
      ordinates[j] <- c_p
      psi_mat[, j] <- (psi_gl - c_p * (y_arr - mu)) / mu
    } else {
      ordinates[j] <- gl_p - p * mu
      psi_mat[, j] <- psi_gl - p * (y_arr - mu)
    }
  }

  list(grid = p_grid, ordinates = ordinates, psi_mat = psi_mat)
}
