# Epidemiological Direct & Indirect Rate Standardization (`DSR`, `SMR`, `ISR`)

STANDARD_POPULATIONS <- list(
  who_2000_2025 = c(
    `0-4` = 8860, `5-9` = 8690, `10-14` = 8600, `15-19` = 8470, `20-24` = 8220,
    `25-29` = 7930, `30-34` = 7610, `35-39` = 7150, `40-44` = 6590, `45-49` = 6040,
    `50-54` = 5370, `55-59` = 4550, `60-64` = 3720, `65-69` = 2960, `70-74` = 2210,
    `75-79` = 1520, `80-84` = 910, `85+` = 630
  ),
  segi_1960 = c(
    `0-4` = 12000, `5-9` = 10000, `10-14` = 9000, `15-19` = 9000, `20-24` = 8000,
    `25-29` = 8000, `30-34` = 6000, `35-39` = 6000, `40-44` = 6000, `45-49` = 6000,
    `50-54` = 5000, `55-59` = 4000, `60-64` = 4000, `65-69` = 3000, `70-74` = 2000,
    `75-79` = 1000, `80-84` = 500, `85+` = 500
  ),
  european_2013 = c(
    `0-4` = 5000, `5-9` = 5500, `10-14` = 5500, `15-19` = 5500, `20-24` = 6000,
    `25-29` = 6000, `30-34` = 6500, `35-39` = 7000, `40-44` = 7000, `45-49` = 7000,
    `50-54` = 7000, `55-59` = 6500, `60-64` = 6000, `65-69` = 5500, `70-74` = 5000,
    `75-79` = 4000, `80-84` = 2500, `85+` = 2500
  ),
  who_broad_5 = c(
    `0-14` = 26150, `15-29` = 24620, `30-44` = 21350, `45-59` = 15960, `60+` = 11950
  )
)

get_standard_population <- function(name = "who_2000_2025") {
  key <- tolower(trimws(name))
  aliases <- c(who = "who_2000_2025", segi = "segi_1960", esp2013 = "european_2013", paho = "who_2000_2025")
  if (key %in% names(aliases)) key <- unname(aliases[[key]])
  if (!(key %in% names(STANDARD_POPULATIONS))) {
    stop(sprintf("Unknown standard population `%s`. Available: %s.", name, paste(names(STANDARD_POPULATIONS), collapse = ", ")), call. = FALSE)
  }
  vec <- STANDARD_POPULATIONS[[key]]
  data.frame(
    age_group = names(vec),
    std_pop = as.double(vec),
    std_weight = as.double(vec) / sum(vec),
    stringsAsFactors = FALSE
  )
}

.exact_poisson_limits <- function(count, alpha) {
  low <- if (count <= 0.0) 0.0 else 0.5 * stats::qchisq(0.5 * alpha, df = 2.0 * count)
  high <- 0.5 * stats::qchisq(1.0 - 0.5 * alpha, df = 2.0 * (count + 1.0))
  c(low, high)
}

.byar_poisson_limits <- function(count, alpha) {
  if (count <= 0.0) return(.exact_poisson_limits(count, alpha))
  z <- stats::qnorm(1.0 - 0.5 * alpha)
  low <- count * ((1.0 - 1.0 / (9.0 * count) - z / (3.0 * sqrt(count)))^3)
  cp <- count + 1.0
  high <- cp * ((1.0 - 1.0 / (9.0 * cp) + z / (3.0 * sqrt(cp)))^3)
  c(max(0.0, low), high)
}

direct_standardize <- function(
  data,
  events,
  population,
  stratum,
  group = NULL,
  standard_pop = "who_2000_2025",
  std_pop_col = NULL,
  multiplier = 100000.0,
  ci_method = c("dobson", "byar", "normal"),
  confidence_level = 0.95
) {
  ci_method <- match.arg(ci_method)
  df <- .ensure_data_frame(data)
  for (col in c(events, population, stratum, group, std_pop_col)) {
    if (!is.null(col) && !(col %in% names(df))) {
      stop(sprintf("Column `%s` not found in `data`.", col), call. = FALSE)
    }
  }

  if (!is.null(std_pop_col)) {
    sp_df <- unique(df[, c(stratum, std_pop_col), drop = FALSE])
    std_map <- stats::setNames(as.double(sp_df[[std_pop_col]]), as.character(sp_df[[stratum]]))
  } else if (is.character(standard_pop) && length(standard_pop) == 1L) {
    sp_df <- get_standard_population(standard_pop)
    std_map <- stats::setNames(sp_df$std_pop, sp_df$age_group)
  } else if (is.numeric(standard_pop) && !is.null(names(standard_pop))) {
    std_map <- as.double(standard_pop)
    names(std_map) <- names(standard_pop)
  } else if (is.data.frame(standard_pop) && ncol(standard_pop) >= 2L) {
    std_map <- stats::setNames(as.double(standard_pop[[2L]]), as.character(standard_pop[[1L]]))
  } else {
    stop("Unsupported `standard_pop` specification.", call. = FALSE)
  }

  alpha <- 1.0 - confidence_level
  grp_list <- if (is.null(group)) {
    list(Overall = df)
  } else {
    g_vals <- unique(as.character(df[[group]]))
    stats::setNames(lapply(g_vals, function(g) df[as.character(df[[group]]) == g, , drop = FALSE]), g_vals)
  }

  rows <- list()
  for (g_nm in names(grp_list)) {
    sub_df <- grp_list[[g_nm]]
    st_keys <- as.character(sub_df[[stratum]])
    if (any(!(st_keys %in% names(std_map)))) {
      miss <- setdiff(st_keys, names(std_map))
      stop(sprintf("Strata %s missing from `standard_pop`.", paste(miss, collapse = ", ")), call. = FALSE)
    }
    raw_w <- as.double(std_map[st_keys])
    pi_a <- raw_w / sum(raw_w)

    d_a <- .to_clean_numeric(sub_df[[events]], events)
    n_a <- .to_clean_numeric(sub_df[[population]], population)
    if (any(n_a <= 0)) stop(sprintf("Zero or negative population in group `%s`.", g_nm), call. = FALSE)

    tot_d <- sum(d_a)
    tot_n <- sum(n_a)
    crude <- (tot_d / tot_n) * multiplier
    dsr <- sum(pi_a * (d_a / n_a)) * multiplier
    var_unsc <- sum(((pi_a / n_a)^2) * d_a)
    se_dsr <- multiplier * sqrt(max(var_unsc, 0.0))

    lims <- if (ci_method == "byar") .byar_poisson_limits(tot_d, alpha) else .exact_poisson_limits(tot_d, alpha)
    if (tot_d == 0.0) {
      ci_l <- 0.0
      ci_h <- multiplier * max(pi_a / n_a) * lims[2L]
    } else if (ci_method %in% c("dobson", "byar")) {
      scale_d <- sqrt((multiplier^2 * var_unsc) / tot_d)
      ci_l <- max(0.0, dsr + scale_d * (lims[1L] - tot_d))
      ci_h <- dsr + scale_d * (lims[2L] - tot_d)
    } else {
      z <- stats::qnorm(1.0 - 0.5 * alpha)
      ci_l <- max(0.0, dsr - z * se_dsr)
      ci_h <- dsr + z * se_dsr
    }

    rows[[length(rows) + 1L]] <- data.frame(
      measure = sprintf("Directly standardized rate per %s", format(multiplier, big.mark = ",", scientific = FALSE)),
      domain = g_nm,
      events = tot_d,
      population = tot_n,
      crude_rate = crude,
      estimate = dsr,
      se = se_dsr,
      ci_low = ci_l,
      ci_high = ci_h,
      stringsAsFactors = FALSE
    )
  }

  out_tbl <- do.call(rbind, rows)
  rownames(out_tbl) <- NULL
  new_esther_result(
    table = out_tbl,
    title = sprintf("Directly standardized rates of %s by %s", events, if (is.null(group)) "total" else group),
    metadata = list(
      outcome = events,
      dimension = if (is.null(group)) "Overall" else group,
      measures = "Directly standardized rate",
      data_mode = "grouped",
      vce_method = paste0("poisson_gamma:", ci_method),
      confidence_level = confidence_level
    )
  )
}

indirect_standardize <- function(
  data,
  events,
  population,
  stratum,
  group,
  ref_rates = NULL,
  multiplier = 100000.0,
  output_type = c("both", "smr", "isr"),
  ci_method = c("exact", "byar"),
  confidence_level = 0.95
) {
  output_type <- match.arg(output_type)
  ci_method <- match.arg(ci_method)
  df <- .ensure_data_frame(data)
  for (col in c(events, population, stratum, group)) {
    if (!(col %in% names(df))) stop(sprintf("Column `%s` not found in `data`.", col), call. = FALSE)
  }

  if (is.null(ref_rates)) {
    ev_tot <- tapply(.to_clean_numeric(df[[events]], events), as.character(df[[stratum]]), sum)
    pop_tot <- tapply(.to_clean_numeric(df[[population]], population), as.character(df[[stratum]]), sum)
    rate_map <- as.double(ev_tot / pop_tot)
    names(rate_map) <- names(ev_tot)
    ref_crude <- sum(ev_tot) / sum(pop_tot)
  } else if (is.numeric(ref_rates) && !is.null(names(ref_rates))) {
    rate_map <- as.double(ref_rates)
    names(rate_map) <- names(ref_rates)
    ref_crude <- mean(rate_map)
  } else if (is.data.frame(ref_rates) && ncol(ref_rates) >= 2L) {
    rate_map <- stats::setNames(as.double(ref_rates[[2L]]), as.character(ref_rates[[1L]]))
    ref_crude <- mean(rate_map)
  } else {
    stop("Unsupported `ref_rates` format.", call. = FALSE)
  }

  alpha <- 1.0 - confidence_level
  g_vals <- unique(as.character(df[[group]]))
  rows <- list()

  for (g_nm in g_vals) {
    sub_df <- df[as.character(df[[group]]) == g_nm, , drop = FALSE]
    st_k <- as.character(sub_df[[stratum]])
    r_ref <- as.double(rate_map[st_k])
    d_a <- .to_clean_numeric(sub_df[[events]], events)
    n_a <- .to_clean_numeric(sub_df[[population]], population)

    obs <- sum(d_a)
    exp_cnt <- sum(n_a * r_ref)
    if (exp_cnt <= 0.0) stop(sprintf("Expected events `E == 0` in group `%s`.", g_nm), call. = FALSE)

    smr <- obs / exp_cnt
    se_smr <- sqrt(max(obs, 0.0)) / exp_cnt
    lims <- if (ci_method == "byar") .byar_poisson_limits(obs, alpha) else .exact_poisson_limits(obs, alpha)
    smr_l <- lims[1L] / exp_cnt
    smr_h <- lims[2L] / exp_cnt

    if (output_type %in% c("smr", "both")) {
      rows[[length(rows) + 1L]] <- data.frame(
        measure = "Indirectly standardized ratio",
        domain = g_nm,
        observed = obs,
        expected = exp_cnt,
        estimate = smr,
        se = se_smr,
        ci_low = smr_l,
        ci_high = smr_h,
        stringsAsFactors = FALSE
      )
    }
    if (output_type %in% c("isr", "both")) {
      c_scale <- ref_crude * multiplier
      rows[[length(rows) + 1L]] <- data.frame(
        measure = sprintf("Indirectly standardized rate per %s", format(multiplier, big.mark = ",", scientific = FALSE)),
        domain = g_nm,
        observed = obs,
        expected = exp_cnt,
        estimate = smr * c_scale,
        se = se_smr * c_scale,
        ci_low = smr_l * c_scale,
        ci_high = smr_h * c_scale,
        stringsAsFactors = FALSE
      )
    }
  }

  out_tbl <- do.call(rbind, rows)
  rownames(out_tbl) <- NULL
  new_esther_result(
    table = out_tbl,
    title = sprintf("Indirectly standardized estimates of %s by %s", events, group),
    metadata = list(
      outcome = events,
      dimension = group,
      measures = unique(out_tbl$measure),
      data_mode = "grouped",
      vce_method = paste0("poisson_exact:", ci_method),
      confidence_level = confidence_level
    )
  )
}
