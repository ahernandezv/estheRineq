# S3 Survey & Disaggregated Surveillance Containers (`esther_survey` & `esther_grouped`)

survey_health_data <- function(
  data,
  weights = NULL,
  strata = NULL,
  psu = NULL,
  fpc = NULL,
  rep_weights = NULL,
  rep_type = "BRR",
  rep_scale = NULL,
  lonely_psu = c("remove", "adjust", "certainty", "average")
) {
  lonely_psu <- match.arg(lonely_psu)

  if (inherits(data, "esther_survey")) {
    return(data)
  }

  # Native adapter for `survey::svydesign` and `survey::svyrep.design`
  if (inherits(data, "svyrep.design")) {
    df <- as.data.frame(data$variables, stringsAsFactors = FALSE)
    df[["..esther_w.."]] <- as.double(data$pweights)
    rw_mat <- as.matrix(data$repweights)
    rw_cols <- paste0("..esther_rw_", seq_len(ncol(rw_mat)), "..")
    for (j in seq_len(ncol(rw_mat))) {
      df[[rw_cols[j]]] <- as.double(rw_mat[, j])
    }
    return(survey_health_data(
      df,
      weights = "..esther_w..",
      rep_weights = rw_cols,
      rep_type = if (!is.null(data$type)) as.character(data$type) else rep_type,
      rep_scale = if (!is.null(data$scale)) as.double(data$scale) else rep_scale,
      lonely_psu = lonely_psu
    ))
  }

  if (inherits(data, "survey.design")) {
    df <- as.data.frame(data$variables, stringsAsFactors = FALSE)
    w_vec <- if (!is.null(data$prob)) 1.0 / as.double(data$prob) else rep(1.0, nrow(df))
    df[["..esther_w.."]] <- w_vec
    st_col <- NULL
    if (!is.null(data$strata) && ncol(data$strata) >= 1L && isTRUE(data$has.strata)) {
      df[["..esther_strata.."]] <- as.character(data$strata[[1L]])
      st_col <- "..esther_strata.."
    }
    psu_col <- NULL
    if (!is.null(data$cluster) && ncol(data$cluster) >= 1L) {
      df[["..esther_psu.."]] <- as.character(data$cluster[[1L]])
      psu_col <- "..esther_psu.."
    }
    fpc_col <- NULL
    if (!is.null(data$fpc) && !is.null(data$fpc$popsize)) {
      df[["..esther_fpc.."]] <- as.double(data$fpc$popsize[, 1L])
      fpc_col <- "..esther_fpc.."
    }
    return(survey_health_data(
      df,
      weights = "..esther_w..",
      strata = st_col,
      psu = psu_col,
      fpc = fpc_col,
      lonely_psu = lonely_psu
    ))
  }

  df <- .ensure_data_frame(data)
  for (col in c(weights, strata, psu, fpc, rep_weights)) {
    if (!is.null(col) && !(col %in% names(df))) {
      stop(sprintf("Survey design column `%s` not found in `data`.", col), call. = FALSE)
    }
  }

  if (!is.null(weights)) {
    w_arr <- .to_clean_numeric(df[[weights]], name = weights)
    if (any(is.na(w_arr) | !is.finite(w_arr))) {
      stop(sprintf("Weights column `%s` contains missing or non-finite values.", weights), call. = FALSE)
    }
    if (any(w_arr < 0)) {
      stop(sprintf("Weights column `%s` contains negative values.", weights), call. = FALSE)
    }
    if (sum(w_arr) <= 0) {
      stop("Sum of sampling weights must be strictly positive.", call. = FALSE)
    }
  }

  structure(
    list(
      df = df,
      weights = weights,
      strata = strata,
      psu = psu,
      fpc = fpc,
      rep_weights = rep_weights,
      rep_type = toupper(trimws(rep_type)),
      rep_scale = rep_scale,
      lonely_psu = lonely_psu
    ),
    class = "esther_survey"
  )
}

.coerce_survey <- function(
  data,
  weights = NULL,
  strata = NULL,
  psu = NULL,
  fpc = NULL,
  lonely_psu = "remove"
) {
  if (inherits(data, "esther_survey")) {
    return(data)
  }
  survey_health_data(
    data = data,
    weights = weights,
    strata = strata,
    psu = psu,
    fpc = fpc,
    lonely_psu = lonely_psu
  )
}

.get_design_arrays <- function(svy) {
  n <- nrow(svy$df)
  w <- if (is.null(svy$weights)) rep(1.0, n) else .to_clean_numeric(svy$df[[svy$weights]], svy$weights)
  st <- if (is.null(svy$strata)) NULL else as.character(svy$df[[svy$strata]])
  ps <- if (is.null(svy$psu)) NULL else as.character(svy$df[[svy$psu]])
  fp <- if (is.null(svy$fpc)) NULL else .to_clean_numeric(svy$df[[svy$fpc]], svy$fpc)
  list(weights = w, strata = st, psu = ps, fpc = fp)
}

print.esther_survey <- function(x, ...) {
  d <- .get_design_arrays(x)
  n_st <- if (is.null(d$strata)) 1L else length(unique(d$strata))
  n_ps <- if (is.null(d$psu)) nrow(x$df) else length(unique(paste(if (is.null(d$strata)) "1" else d$strata, d$psu, sep = "::")))
  cat(sprintf(
    "<esther_survey: n = %s, sum_weights = %s, strata = %d, PSUs = %d>\n",
    format(nrow(x$df), big.mark = ","),
    format(round(sum(d$weights), 1), big.mark = ","),
    n_st,
    n_ps
  ))
  invisible(x)
}

grouped_health_data <- function(
  data,
  subgroup,
  estimate = NULL,
  se = NULL,
  population = NULL,
  events = NULL,
  subgroup_order = NULL,
  ordered = TRUE,
  favorable = NULL,
  reference_group = NULL,
  scale = 1.0,
  sample_size = NULL
) {
  if (inherits(data, "esther_grouped")) {
    return(data)
  }
  df <- .ensure_data_frame(data)
  if (!(subgroup %in% names(df))) {
    stop(sprintf("`subgroup = '%s'` not found in `data`.", subgroup), call. = FALSE)
  }
  if (is.null(estimate) && (is.null(events) || is.null(population))) {
    stop("Provide either `estimate` or both `events` and `population`.", call. = FALSE)
  }

  if (is.null(estimate)) {
    ev <- .to_clean_numeric(df[[events]], events)
    pop <- .to_clean_numeric(df[[population]], population)
    if (any(pop <= 0, na.rm = TRUE)) {
      stop("`population` denominators must be strictly positive.", call. = FALSE)
    }
    rate <- (ev / pop) * scale
    df[["..computed_estimate.."]] <- rate
    est_col <- "..computed_estimate.."
    if (is.null(se)) {
      p_raw <- ev / pop
      if (all(p_raw >= 0 & p_raw <= 1, na.rm = TRUE)) {
        se_vals <- sqrt(pmax(p_raw * (1 - p_raw), 1e-12) / pop) * scale
      } else {
        se_vals <- (sqrt(pmax(ev, 0.5)) / pop) * scale
      }
      df[["..computed_se.."]] <- se_vals
      se_col <- "..computed_se.."
    } else {
      se_col <- se
    }
  } else {
    if (!(estimate %in% names(df))) {
      stop(sprintf("`estimate = '%s'` not found in `data`.", estimate), call. = FALSE)
    }
    est_col <- estimate
    se_col <- se
    if (!is.null(se_col) && !(se_col %in% names(df))) {
      stop(sprintf("`se = '%s'` not found in `data`.", se_col), call. = FALSE)
    }
  }

  if (!is.null(population) && !(population %in% names(df))) {
    stop(sprintf("`population = '%s'` not found in `data`.", population), call. = FALSE)
  }

  if (is.character(subgroup_order) && length(subgroup_order) == 1L && (subgroup_order %in% names(df))) {
    ord_idx <- order(df[[subgroup_order]])
    df <- df[ord_idx, , drop = FALSE]
    rownames(df) <- NULL
  } else if (!is.null(subgroup_order) && length(subgroup_order) > 1L) {
    f_ord <- factor(df[[subgroup]], levels = subgroup_order, ordered = TRUE)
    df <- df[order(f_ord), , drop = FALSE]
    rownames(df) <- NULL
  }

  structure(
    list(
      df = df,
      subgroup = subgroup,
      estimate = est_col,
      se = se_col,
      population = population,
      events = events,
      ordered = isTRUE(ordered),
      favorable = favorable,
      reference_group = reference_group,
      scale = as.double(scale),
      sample_size = sample_size
    ),
    class = "esther_grouped"
  )
}

print.esther_grouped <- function(x, ...) {
  cat(sprintf(
    "<esther_grouped: %d subgroups (%s), ordered = %s, scale = %g>\n",
    nrow(x$df),
    x$subgroup,
    x$ordered,
    x$scale
  ))
  invisible(x)
}
