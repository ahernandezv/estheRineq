# Weighted Fractional Ranks (Ridit Scores), Tie-Exact Antiderivatives, and `xtile`

compute_fractional_ranks <- function(x, weights = NULL, rank_order = c("asc", "desc")) {
  rank_order <- match.arg(rank_order)
  r_num <- .to_rank_numeric(x, name = "x")
  n <- length(r_num)
  if (n == 0L) {
    stop("Cannot compute fractional ranks on an empty vector.", call. = FALSE)
  }
  if (any(is.na(r_num) | !is.finite(r_num))) {
    stop("Rank vector contains missing or non-finite values; filter complete cases first.", call. = FALSE)
  }

  w <- if (is.null(weights)) rep(1.0, n) else as.double(weights)
  if (length(w) != n) {
    stop("`weights` must have the same length as `x`.", call. = FALSE)
  }
  if (any(w < 0) || sum(w) <= 0) {
    stop("`weights` must be non-negative with strictly positive sum.", call. = FALSE)
  }
  w_norm <- w / sum(w)

  r_eff <- if (rank_order == "desc") -r_num else r_num
  ord <- order(r_eff)
  r_sorted <- r_eff[ord]
  w_sorted <- w_norm[ord]

  is_new <- c(TRUE, r_sorted[-1L] != r_sorted[-n])
  grp_sorted <- cumsum(is_new)
  k_total <- grp_sorted[n]

  group_share <- as.double(rowsum(w_sorted, grp_sorted, reorder = FALSE)[, 1L])
  f_high <- cumsum(group_share)
  f_high[k_total] <- 1.0
  f_low <- c(0.0, f_high[-k_total])
  midpoints <- f_low + 0.5 * group_share

  inv_idx <- integer(n)
  inv_idx[ord] <- grp_sorted
  ridit <- midpoints[inv_idx]

  list(
    ridit = ridit,
    f_low = f_low,
    f_high = f_high,
    group_share = group_share,
    inv_idx = inv_idx,
    w_norm = w_norm,
    n_groups = k_total
  )
}

# Antiderivative G_ext(u) = -(1 - u)^v for extended CI w(u) = v * (1 - u)^(v - 1)
.antideriv_extended <- function(u, v) {
  -((pmax(0.0, 1.0 - u))^v)
}

.weight_extended <- function(u, v) {
  v * ((pmax(0.0, 1.0 - u))^(v - 1.0))
}

# Antiderivative G_sym(u) matching Erreygers, Clarke & Van Ourti (2012) & Stata `conindex`
.antideriv_symmetric <- function(u, beta) {
  dist <- ((u - 0.5)^2 + 0.25)^(0.5 * (beta - 2.0))
  (u - 0.5) * dist * (2.0^(beta - 2.0))
}

.weight_symmetric <- function(u, beta) {
  s <- (u - 0.5)^2 + 0.25
  (2.0^(beta - 2.0)) * (s^(0.5 * (beta - 4.0))) * ((beta - 1.0) * ((u - 0.5)^2) + 0.25)
}

.compute_tie_integrated_weights <- function(rank_res, scheme = c("standard", "extended", "symmetric"), param = 2.0) {
  scheme <- match.arg(scheme)
  fl <- rank_res$f_low
  fh <- rank_res$f_high
  pk <- rank_res$group_share
  inv <- rank_res$inv_idx
  pos <- pk > 0

  omega_k <- numeric(length(pk))
  w_low_k <- numeric(length(pk))
  w_high_k <- numeric(length(pk))

  if (scheme == "standard") {
    omega_k <- 2.0 * (fl + 0.5 * pk) - 1.0
    w_low_k <- 2.0 * fl - 1.0
    w_high_k <- 2.0 * fh - 1.0
  } else if (scheme == "extended") {
    if (param <= 1.0) stop("Extended CI aversion parameter `v` must satisfy `v > 1`.", call. = FALSE)
    g_hi <- .antideriv_extended(fh, param)
    g_lo <- .antideriv_extended(fl, param)
    omega_k[pos] <- 1.0 - (g_hi[pos] - g_lo[pos]) / pk[pos]
    w_low_k <- 1.0 - .weight_extended(fl, param)
    w_high_k <- 1.0 - .weight_extended(fh, param)
  } else {
    if (param <= 1.0) stop("Symmetric index parameter `beta` must satisfy `beta > 1`.", call. = FALSE)
    g_hi <- .antideriv_symmetric(fh, param)
    g_lo <- .antideriv_symmetric(fl, param)
    omega_k[pos] <- (g_hi[pos] - g_lo[pos]) / pk[pos]
    w_low_k <- .weight_symmetric(fl, param)
    w_high_k <- .weight_symmetric(fh, param)
  }

  list(
    omega_obs = omega_k[inv],
    omega_k = omega_k,
    w_low_k = w_low_k,
    w_high_k = w_high_k
  )
}

xtile <- function(x, weights = NULL, n_quantiles = 5L, labels = NULL) {
  n_quantiles <- as.integer(n_quantiles)
  if (n_quantiles < 2L) {
    stop("`n_quantiles` must be at least 2.", call. = FALSE)
  }
  if (!is.null(labels) && length(labels) != n_quantiles) {
    stop(sprintf("`labels` length (%d) must equal `n_quantiles` (%d).", length(labels), n_quantiles), call. = FALSE)
  }

  v <- .to_clean_numeric(x, name = "x")
  n <- length(v)
  w <- if (is.null(weights)) rep(1.0, n) else .to_clean_numeric(weights, name = "weights")
  valid <- !is.na(v) & is.finite(v) & !is.na(w) & is.finite(w) & (w > 0)

  codes <- rep(NA_integer_, n)
  if (any(valid)) {
    v_sub <- v[valid]
    w_sub <- w[valid]
    ord <- order(v_sub)
    v_sort <- v_sub[ord]
    w_sort <- w_sub[ord]

    n_sub <- length(v_sort)
    is_new <- c(TRUE, v_sort[-1L] != v_sort[-n_sub])
    grp_id <- cumsum(is_new)
    grp_w <- as.double(rowsum(w_sort, grp_id, reorder = FALSE)[, 1L])
    cum_share_hi <- cumsum(grp_w) / sum(w_sub)

    eps <- 1e-12
    q_grp <- as.integer(ceiling((cum_share_hi - eps) * n_quantiles))
    q_grp <- pmin(pmax(q_grp, 1L), n_quantiles)

    q_sub <- integer(n_sub)
    q_sub[ord] <- q_grp[grp_id]
    codes[valid] <- q_sub
  }

  if (!is.null(labels)) {
    return(factor(codes, levels = seq_len(n_quantiles), labels = labels, ordered = TRUE))
  }
  codes
}
