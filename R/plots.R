# Five Editorial Visualization Families (`equiplot`, `plot_curve`, `plot_sii_gradient`, `plot_decomposition`, `funnel_plot`)
# Strict sentence-case typography, dynamic variable names, zero parenthetical clutter.
# Supports `ggplot2` (when installed) and crisp publication-grade base/grid rendering out of the box.

PALETTES <- list(
  pelotas = c("#D97706", "#EA580C", "#94A3B8", "#2563EB", "#0F172A"),
  lancet = c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F", "#FDAF91"),
  okabe_ito = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
  who = c("#008DC9", "#005392", "#F26829", "#A6228C", "#5B2C86", "#00A76D")
)

get_palette <- function(name = "pelotas", n = 5L) {
  key <- tolower(trimws(name))
  base_cols <- if (key %in% names(PALETTES)) PALETTES[[key]] else PALETTES$pelotas
  if (n <= length(base_cols)) return(base_cols[seq_len(n)])
  grDevices::colorRampPalette(base_cols)(n)
}

classify_inequality_pattern <- function(values, favorable = TRUE, lang = "en") {
  v <- as.double(values)
  if (length(v) < 3L) return(.tr("linear_inequality", lang))
  span <- abs(v[length(v)] - v[1L])
  if (span < 1e-8) return(.tr("near_equality", lang))
  gap_bot <- abs(v[2L] - v[1L])
  gap_top <- abs(v[length(v)] - v[length(v) - 1L])
  if (gap_top >= 0.42 * span && gap_top >= 1.6 * max(gap_bot, 1e-9)) return(.tr("top_inequality", lang))
  if (gap_bot >= 0.42 * span && gap_bot >= 1.6 * max(gap_top, 1e-9)) return(.tr("bottom_inequality", lang))
  .tr("linear_inequality", lang)
}

.resolve_plot_backend <- function(backend) {
  b <- tolower(trimws(backend))
  if (b == "auto") {
    if (requireNamespace("ggplot2", quietly = TRUE)) return("ggplot2")
    return("base")
  }
  if (b == "ggplot2" && !requireNamespace("ggplot2", quietly = TRUE)) {
    stop("`ggplot2` is not installed; use `backend = 'base'` or install `ggplot2`.", call. = FALSE)
  }
  b
}

.prepare_equiplot_df <- function(data, outcome, subgroup, by, columns, scale, weights) {
  if (!is.null(columns)) {
    if (length(columns) < 2L) stop("`columns` for wide-format `equiplot` requires at least 2 columns.", call. = FALSE)
    df_w <- .ensure_data_frame(data)
    for (col in c(columns, by)) {
      if (!is.null(col) && !(col %in% names(df_w))) stop(sprintf("Column `%s` not found in `data`.", col), call. = FALSE)
    }
    out_lbl <- if (is.null(outcome)) "estimate" else outcome
    sub_lbl <- if (is.null(subgroup)) "subgroup" else subgroup
    rows <- list()
    for (i in seq_len(nrow(df_w))) {
      r_nm <- if (!is.null(by)) as.character(df_w[[by]][i]) else as.character(i)
      for (col in columns) {
        val <- as.double(df_w[[col]][i]) * scale
        if (is.finite(val)) {
          rows[[length(rows) + 1L]] <- data.frame(row = r_nm, sub = col, val = val, stringsAsFactors = FALSE)
        }
      }
    }
    return(list(df = do.call(rbind, rows), sub_levels = as.character(columns), out_var = out_lbl, sub_var = sub_lbl, by_var = by))
  }

  if (inherits(data, "esther_grouped")) {
    df_g <- data$df
    sg_col <- if (is.null(subgroup)) data$subgroup else subgroup
    est_col <- if (is.null(outcome)) data$estimate else outcome
    out_lbl <- if (startsWith(est_col, "..")) "estimate" else est_col
    vals <- .to_clean_numeric(df_g[[est_col]], est_col)
    if (scale != 1.0 && scale != data$scale) vals <- vals * (scale / data$scale)
    has_by <- !is.null(by) && (by %in% names(df_g))
    r_col <- if (has_by) as.character(df_g[[by]]) else rep(out_lbl, nrow(df_g))
    sub_lv <- unique(as.character(df_g[[sg_col]]))
    return(list(
      df = data.frame(row = r_col, sub = as.character(df_g[[sg_col]]), val = vals, stringsAsFactors = FALSE),
      sub_levels = sub_lv, out_var = out_lbl, sub_var = sg_col, by_var = if (has_by) by else NULL
    ))
  }

  if (is.null(outcome) || is.null(subgroup)) {
    stop("`outcome` and `subgroup` are required for microdata `equiplot`.", call. = FALSE)
  }
  svy <- .coerce_survey(data, weights = weights)
  df <- svy$df
  req <- unique(c(outcome, subgroup, by))
  valid <- stats::complete.cases(df[, req, drop = FALSE])
  sub_df <- df[valid, , drop = FALSE]
  w_sub <- .get_design_arrays(svy)$weights[valid]
  y_sub <- .to_clean_numeric(sub_df[[outcome]], outcome) * scale

  sg_raw <- sub_df[[subgroup]]
  sub_lv <- if (is.factor(sg_raw)) {
    lv <- levels(sg_raw); lv[lv %in% as.character(sg_raw)]
  } else if (is.numeric(sg_raw)) {
    as.character(sort(unique(sg_raw)))
  } else {
    sort(unique(as.character(sg_raw)))
  }

  rows <- list()
  if (is.null(by)) {
    for (sg in sub_lv) {
      m <- as.character(sg_raw) == sg
      if (any(m)) {
        rows[[length(rows) + 1L]] <- data.frame(row = outcome, sub = sg, val = .weighted_mean(y_sub[m], w_sub[m]), stringsAsFactors = FALSE)
      }
    }
  } else {
    by_lv <- unique(as.character(sub_df[[by]]))
    for (bv in by_lv) {
      for (sg in sub_lv) {
        m <- (as.character(sub_df[[by]]) == bv) & (as.character(sg_raw) == sg)
        if (any(m)) {
          rows[[length(rows) + 1L]] <- data.frame(row = bv, sub = sg, val = .weighted_mean(y_sub[m], w_sub[m]), stringsAsFactors = FALSE)
        }
      }
    }
  }
  list(df = do.call(rbind, rows), sub_levels = sub_lv, out_var = outcome, sub_var = subgroup, by_var = by)
}

equiplot <- function(
  data,
  outcome = NULL,
  subgroup = NULL,
  by = NULL,
  columns = NULL,
  scale = 1.0,
  favorable = TRUE,
  diagnose_pattern = FALSE,
  sort_by = NULL,
  ascending = TRUE,
  palette = "pelotas",
  title = NULL,
  xlabel = NULL,
  ylabel = NULL,
  lang = "en",
  backend = "auto",
  weights = NULL
) {
  prep <- .prepare_equiplot_df(data, outcome, subgroup, by, columns, scale, weights)
  sdf <- prep$df
  sub_lv <- prep$sub_levels
  rows_lv <- unique(sdf$row)

  if (!is.null(sort_by) && length(rows_lv) > 1L) {
    sb <- tolower(trimws(sort_by))
    keys <- vapply(rows_lv, function(rn) {
      vs <- sdf$val[sdf$row == rn]
      if (sb == "gap") return(max(vs) - min(vs))
      if (sb == "min") return(min(vs))
      if (sb == "max") return(max(vs))
      if (sb == "mean") return(mean(vs))
      if (sb == "first") return(vs[1L])
      if (sb == "last") return(vs[length(vs)])
      stop("`sort_by` must be `'gap'`, `'min'`, `'max'`, `'mean'`, `'first'`, or `'last'`.", call. = FALSE)
    }, numeric(1L))
    rows_lv <- rows_lv[order(keys, decreasing = !isTRUE(ascending))]
  }

  cols <- get_palette(palette, length(sub_lv))
  p_title <- if (!is.null(title)) title else .tr("equiplot_title", lang, outcome = prep$out_var, subgroup = prep$sub_var)
  x_lbl <- if (!is.null(xlabel)) xlabel else prep$out_var
  y_lbl <- if (!is.null(ylabel)) ylabel else if (!is.null(prep$by_var)) prep$by_var else ""

  bk <- .resolve_plot_backend(backend)
  if (bk == "ggplot2") {
    sdf$row_f <- factor(sdf$row, levels = rev(rows_lv))
    sdf$sub_f <- factor(sdf$sub, levels = sub_lv)
    seg_df <- do.call(rbind, lapply(rows_lv, function(rn) {
      vs <- sdf$val[sdf$row == rn]
      data.frame(
        row_f = factor(rn, levels = rev(rows_lv)),
        xmin = min(vs), xmax = max(vs),
        diag = classify_inequality_pattern(vs, favorable = favorable, lang = lang),
        stringsAsFactors = FALSE
      )
    }))
    names(cols) <- sub_lv
    p <- ggplot2::ggplot(sdf, ggplot2::aes(x = .data$val, y = .data$row_f)) +
      ggplot2::geom_segment(
        data = seg_df,
        ggplot2::aes(x = .data$xmin, xend = .data$xmax, y = .data$row_f, yend = .data$row_f),
        color = "#94A3B8", linewidth = 1.2
      ) +
      ggplot2::geom_point(ggplot2::aes(color = .data$sub_f), size = 3.6) +
      ggplot2::scale_color_manual(values = cols, name = prep$sub_var) +
      ggplot2::labs(title = p_title, x = x_lbl, y = y_lbl) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        legend.position = "top",
        plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
        panel.grid.minor = ggplot2::element_blank()
      )
    if (isTRUE(diagnose_pattern)) {
      span_x <- max(sdf$val) - min(sdf$val)
      p <- p + ggplot2::geom_text(
        data = seg_df,
        ggplot2::aes(x = .data$xmax, y = .data$row_f, label = .data$diag),
        hjust = -0.1, size = 3.1, fontface = "italic", color = "#475569"
      ) + ggplot2::xlim(min(sdf$val), max(sdf$val) + 0.30 * max(span_x, 1.0))
    }
    return(p)
  }

  # Crisp editorial base-R renderer
  n_r <- length(rows_lv)
  y_pos <- rev(seq_len(n_r))
  names(y_pos) <- rows_lv
  left_in <- max(1.1, min(2.4, max(nchar(rows_lv)) * 0.095 + 0.55))
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mai = c(0.85, left_in, 1.22, 0.35), bg = "white", col.axis = "#1E293B", col.lab = "#0F172A", las = 1)

  x_rng <- range(sdf$val, na.rm = TRUE)
  x_pad <- max(diff(x_rng), 1.0)
  x_lim <- c(x_rng[1L] - 0.04 * x_pad, x_rng[2L] + (if (isTRUE(diagnose_pattern)) 0.34 else 0.05) * x_pad)
  y_lim <- if (n_r > 1L) c(0.5, n_r + 0.5) else c(0.5, 1.5)

  graphics::plot(NA, xlim = x_lim, ylim = y_lim, xlab = x_lbl, ylab = "", yaxt = "n", bty = "l", main = "")
  graphics::grid(nx = NULL, ny = NA, col = "#CBD5E1", lty = "dotted")
  graphics::abline(h = y_pos, col = "#E2E8F0", lty = "dotted")
  graphics::axis(2, at = y_pos, labels = rows_lv, tick = TRUE, cex.axis = 0.88)
  if (nzchar(y_lbl)) graphics::mtext(y_lbl, side = 2, line = left_in * 3.6, las = 0, cex = 0.95, col = "#0F172A")
  graphics::mtext(p_title, side = 3, line = 3.4, font = 2, cex = 1.05, col = "#0F172A")

  for (rn in rows_lv) {
    vs <- sdf$val[sdf$row == rn]
    yp <- y_pos[[rn]]
    graphics::segments(min(vs), yp, max(vs), yp, col = "#94A3B8", lwd = 2.8)
    if (isTRUE(diagnose_pattern)) {
      d_txt <- classify_inequality_pattern(vs, favorable = favorable, lang = lang)
      graphics::text(max(vs) + 0.02 * x_pad, yp, labels = d_txt, pos = 4, cex = 0.82, font = 3, col = "#475569")
    }
  }
  for (j in seq_along(sub_lv)) {
    sg <- sub_lv[j]
    sub_j <- sdf[sdf$sub == sg, , drop = FALSE]
    graphics::points(sub_j$val, y_pos[sub_j$row], pch = 21, bg = cols[j], col = "#FFFFFF", lwd = 1.2, cex = 1.45)
  }
  usr <- graphics::par("usr")
  y_top_in <- graphics::grconvertY(usr[4L], from = "user", to = "inches")
  y_leg_usr <- graphics::grconvertY(y_top_in + 0.28, from = "inches", to = "user")
  graphics::legend(
    x = mean(usr[1:2]), y = y_leg_usr, xjust = 0.5, yjust = 0.5,
    xpd = TRUE, horiz = TRUE, bty = "n",
    legend = sub_lv, pt.bg = cols, col = "#FFFFFF", pch = 21, pt.cex = 1.35,
    title = prep$sub_var, cex = 0.86
  )
  invisible(sdf)
}

plot_curve <- function(
  curve_res,
  show_ci = TRUE,
  palette = "lancet",
  title = NULL,
  xlabel = NULL,
  ylabel = NULL,
  lang = "en",
  backend = "auto"
) {
  if (!inherits(curve_res, "esther_curve")) stop("`curve_res` must be an `esther_curve` object.", call. = FALSE)
  df <- curve_res$ordinates
  meta <- curve_res$metadata
  ctype <- if (!is.null(meta$curve_type)) meta$curve_type else "relative"
  out_v <- if (!is.null(meta$outcome)) meta$outcome else "outcome"
  rk_v <- meta$rank
  by_v <- meta$by
  groups <- unique(df$group)
  cols <- get_palette(palette, length(groups))

  x_lbl <- if (!is.null(xlabel)) xlabel else .tr("cum_pop_ranked_by", lang, rank = if (!is.null(rk_v)) rk_v else out_v)
  y_lbl <- if (!is.null(ylabel)) ylabel else if (ctype == "relative") .tr("cum_share_of", lang, outcome = out_v) else if (ctype == "generalized") .tr("gen_ordinate_of", lang, outcome = out_v) else .tr("abs_gap_of", lang, outcome = out_v)
  p_title <- if (!is.null(title)) title else if (is.null(rk_v)) .tr("lorenz_curve_title", lang, outcome = out_v) else .tr("conc_curve_title", lang, outcome = out_v, rank = rk_v)
  eq_lbl <- .tr("line_of_equality", lang)

  bk <- .resolve_plot_backend(backend)
  if (bk == "ggplot2") {
    names(cols) <- groups
    eq_df <- df[df$group == groups[1L], c("p", "equality"), drop = FALSE]
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$p, y = .data$ordinate, color = .data$group, fill = .data$group))
    if (isTRUE(show_ci)) {
      p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$ci_low, ymax = .data$ci_high), alpha = 0.16, color = NA)
    }
    p <- p +
      ggplot2::geom_line(data = eq_df, ggplot2::aes(x = .data$p, y = .data$equality), inherit.aes = FALSE, linetype = "dashed", color = "#475569", linewidth = 0.8) +
      ggplot2::geom_line(linewidth = 1.1) +
      ggplot2::scale_color_manual(values = cols, name = by_v) +
      ggplot2::scale_fill_manual(values = cols, name = by_v) +
      ggplot2::labs(title = p_title, x = x_lbl, y = y_lbl) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5), panel.grid.minor = ggplot2::element_blank())
    return(p)
  }

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mai = c(0.85, 0.88, 0.72, 0.25), bg = "white", col.axis = "#1E293B", col.lab = "#0F172A", las = 1)
  y_rng <- if (ctype == "relative") c(0.0, 1.0) else range(c(df$ci_low, df$ci_high, df$equality), na.rm = TRUE)

  graphics::plot(NA, xlim = c(0.0, 1.0), ylim = y_rng, xlab = x_lbl, ylab = y_lbl, bty = "l", main = "")
  graphics::grid(col = "#CBD5E1", lty = "dotted")
  graphics::mtext(p_title, side = 3, line = 1.2, font = 2, cex = 1.05, col = "#0F172A")

  eq_df <- df[df$group == groups[1L], , drop = FALSE]
  graphics::lines(eq_df$p, eq_df$equality, lty = 2, lwd = 1.6, col = "#475569")

  for (j in seq_along(groups)) {
    sub_j <- df[df$group == groups[j], , drop = FALSE]
    if (isTRUE(show_ci)) {
      rgb_v <- grDevices::col2rgb(cols[j]) / 255.0
      fill_c <- grDevices::rgb(rgb_v[1L], rgb_v[2L], rgb_v[3L], alpha = 0.16)
      graphics::polygon(c(sub_j$p, rev(sub_j$p)), c(sub_j$ci_low, rev(sub_j$ci_high)), col = fill_c, border = NA)
    }
    graphics::lines(sub_j$p, sub_j$ordinate, col = cols[j], lwd = 2.5)
  }

  leg_lbls <- c(eq_lbl, if (length(groups) == 1L && groups[1L] == "Overall") out_v else groups)
  graphics::legend(
    "topleft", bty = "n",
    legend = leg_lbls,
    col = c("#475569", cols),
    lty = c(2, rep(1, length(groups))),
    lwd = c(1.6, rep(2.5, length(groups))),
    title = by_v, cex = 0.88
  )
  invisible(df)
}

plot_sii_gradient <- function(
  data,
  outcome = NULL,
  subgroup = NULL,
  scale = 1.0,
  method = c("logit_marginal", "linear"),
  title = NULL,
  lang = "en",
  backend = "auto",
  weights = NULL
) {
  method <- match.arg(method)
  if (inherits(data, "esther_grouped")) {
    df_g <- data$df
    sub_lbls <- as.character(df_g[[data$subgroup]])
    eff_sc <- if (missing(scale)) data$scale else as.double(scale)
    y_vals <- .to_clean_numeric(df_g[[data$estimate]], data$estimate) * (eff_sc / data$scale)
    se_vals <- if (!is.null(data$se)) .to_clean_numeric(df_g[[data$se]], data$se) * (eff_sc / data$scale) else rep(0.0, nrow(df_g))
    pop <- if (is.null(data$population)) rep(1.0, nrow(df_g)) else .to_clean_numeric(df_g[[data$population]], data$population)
    out_nm <- if ("indicator" %in% names(df_g)) as.character(df_g$indicator[1L]) else data$estimate
    sub_nm <- if ("dimension" %in% names(df_g)) as.character(df_g$dimension[1L]) else data$subgroup
  } else {
    if (is.null(outcome) || is.null(subgroup)) stop("`outcome` and `subgroup` required for microdata `plot_sii_gradient`.", call. = FALSE)
    svy <- .coerce_survey(data, weights = weights)
    df <- svy$df
    valid <- stats::complete.cases(df[, c(outcome, subgroup), drop = FALSE])
    sub_df <- df[valid, , drop = FALSE]
    w_sub <- .get_design_arrays(svy)$weights[valid]
    y_raw <- .to_clean_numeric(sub_df[[outcome]], outcome)
    sg_raw <- sub_df[[subgroup]]
    lv <- if (is.factor(sg_raw)) levels(sg_raw)[levels(sg_raw) %in% as.character(sg_raw)] else if (is.numeric(sg_raw)) as.character(sort(unique(sg_raw))) else sort(unique(as.character(sg_raw)))
    sub_lbls <- lv
    y_vals <- numeric(length(lv)); se_vals <- numeric(length(lv)); pop <- numeric(length(lv))
    for (j in seq_along(lv)) {
      m <- as.character(sg_raw) == lv[j]
      pop[j] <- sum(w_sub[m])
      y_vals[j] <- .weighted_mean(y_raw[m], w_sub[m]) * scale
      se_vals[j] <- stats::sd(y_raw[m]) / sqrt(max(sum(m), 1L)) * scale
    }
    out_nm <- outcome; sub_nm <- subgroup
  }

  r_res <- compute_fractional_ranks(seq_along(y_vals) - 1.0, weights = pop, rank_order = "asc")
  rid <- r_res$ridit
  r_grid <- seq(0.0, 1.0, length.out = 101L)
  yp <- y_vals / scale
  if (method == "logit_marginal" && all(yp > 0.0 & yp < 1.0)) {
    fit <- .fit_survey_glm(yp, cbind(1.0, rid), weights = pop, family = "binomial", link = "logit")
    y_line <- stats::plogis(fit$params[1L] + fit$params[2L] * r_grid) * scale
  } else {
    fit <- .fit_survey_glm(y_vals, cbind(1.0, rid), weights = pop, family = "gaussian", link = "identity")
    y_line <- fit$params[1L] + fit$params[2L] * r_grid
  }

  p_title <- if (!is.null(title)) title else .tr("gradient_title", lang, outcome = out_nm, subgroup = sub_nm)
  x_lbl <- .tr("fractional_rank_of", lang, subgroup = sub_nm)
  fit_lbl <- .tr("fitted_gradient", lang)
  obs_lbl <- .tr("observed_mean", lang)

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mai = c(0.85, 0.88, 0.72, 0.25), bg = "white", col.axis = "#1E293B", col.lab = "#0F172A", las = 1)
  y_all_r <- c(y_vals - 1.96 * se_vals, y_vals + 1.96 * se_vals, y_line)
  pad_y <- 0.12 * max(diff(range(y_all_r)), 1.0)
  ylim <- c(min(y_all_r) - 0.5 * pad_y, max(y_all_r) + pad_y)

  graphics::plot(NA, xlim = c(-0.03, 1.03), ylim = ylim, xlab = x_lbl, ylab = out_nm, bty = "l", main = "")
  graphics::grid(col = "#CBD5E1", lty = "dotted")
  graphics::mtext(p_title, side = 3, line = 1.2, font = 2, cex = 1.05, col = "#0F172A")

  graphics::lines(r_grid, y_line, col = "#009E73", lwd = 2.8)
  if (any(se_vals > 0)) {
    graphics::arrows(rid, y_vals - 1.96 * se_vals, rid, y_vals + 1.96 * se_vals, angle = 90, code = 3, length = 0.05, col = "#334155", lwd = 1.4)
  }
  pt_cex <- 1.6 + 1.6 * (pop / max(pop))
  graphics::points(rid, y_vals, pch = 21, bg = "#0072B2CC", col = "#FFFFFF", lwd = 1.3, cex = pt_cex)
  graphics::text(rid, y_vals + 0.062 * diff(ylim), labels = sub_lbls, cex = 0.86, col = "#0F172A")
  graphics::legend(
    "topleft", bty = "n",
    legend = c(fit_lbl, obs_lbl),
    col = c("#009E73", "#FFFFFF"), pt.bg = c(NA, "#0072B2CC"),
    lty = c(1, NA), lwd = c(2.8, NA), pch = c(NA, 21), pt.cex = c(NA, 1.8), cex = 0.88
  )
  invisible(data.frame(subgroup = sub_lbls, ridit = rid, estimate = y_vals, se = se_vals, stringsAsFactors = FALSE))
}

plot_decomposition <- function(
  decomp_res,
  metric = c("contribution", "pct_share"),
  title = NULL,
  lang = "en",
  backend = "auto"
) {
  metric <- match.arg(metric)
  if (!inherits(decomp_res, "esther_decomp")) stop("`decomp_res` must be an `esther_decomp` object.", call. = FALSE)
  df <- decomp_res$contributions
  meta <- decomp_res$metadata
  out_nm <- if (!is.null(meta$outcome)) meta$outcome else "outcome"
  rk_nm <- if (!is.null(meta$rank)) meta$rank else if (!is.null(meta$dimension)) meta$dimension else "rank"
  p_title <- if (!is.null(title)) title else .tr("decomp_title", lang, outcome = out_nm, rank = rk_nm)

  facs <- as.character(df$factor)
  vals <- as.double(df[[metric]])
  n_f <- length(facs)
  y_pos <- rev(seq_len(n_f))
  bar_cols <- ifelse(vals >= 0, "#2563EB", "#009E73")
  x_lbl <- .tr(metric, lang)

  has_ci <- (metric == "contribution") && all(c("ci_low", "ci_high") %in% names(df))
  x_all <- vals
  if (has_ci) x_all <- c(x_all, df$ci_low[is.finite(df$ci_low)], df$ci_high[is.finite(df$ci_high)], 0.0) else x_all <- c(x_all, 0.0)
  x_rng <- range(x_all, na.rm = TRUE)
  pad_x <- 0.08 * max(diff(x_rng), 0.05)

  left_in <- max(1.3, min(2.6, max(nchar(facs)) * 0.095 + 0.55))
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mai = c(0.85, left_in, 0.72, 0.25), bg = "white", col.axis = "#1E293B", col.lab = "#0F172A", las = 1)

  graphics::plot(NA, xlim = c(x_rng[1L] - pad_x, x_rng[2L] + pad_x), ylim = c(0.4, n_f + 0.6), xlab = x_lbl, ylab = "", yaxt = "n", bty = "l", main = "")
  graphics::grid(nx = NULL, ny = NA, col = "#CBD5E1", lty = "dotted")
  graphics::axis(2, at = y_pos, labels = facs, cex.axis = 0.88)
  graphics::mtext(p_title, side = 3, line = 1.2, font = 2, cex = 1.05, col = "#0F172A")

  for (i in seq_len(n_f)) {
    yp <- y_pos[i]
    graphics::rect(min(0.0, vals[i]), yp - 0.30, max(0.0, vals[i]), yp + 0.30, col = bar_cols[i], border = NA)
    if (has_ci && is.finite(df$ci_low[i]) && is.finite(df$ci_high[i])) {
      graphics::arrows(df$ci_low[i], yp, df$ci_high[i], yp, angle = 90, code = 3, length = 0.04, col = "#0F172A", lwd = 1.3)
    }
  }
  graphics::abline(v = 0.0, col = "#0F172A", lwd = 1.2)
  invisible(df)
}

funnel_plot <- function(
  data,
  estimate = "estimate",
  se = "se",
  denominator = "population",
  label = "domain",
  benchmark = NULL,
  overdispersion = FALSE,
  label_outliers = TRUE,
  title = NULL,
  xlabel = NULL,
  ylabel = NULL,
  lang = "en",
  backend = "auto"
) {
  if (inherits(data, "esther_result")) {
    df <- data$table
    meta_out <- data$metadata$outcome
  } else {
    df <- .ensure_data_frame(data)
    meta_out <- NULL
  }

  y <- .to_clean_numeric(df[[estimate]], estimate)
  s <- .to_clean_numeric(df[[se]], se)
  n_den <- if (denominator %in% names(df)) .to_clean_numeric(df[[denominator]], denominator) else 1.0 / pmax(s^2, 1e-12)
  lbls <- if (label %in% names(df)) as.character(df[[label]]) else as.character(seq_len(nrow(df)))

  w_iv <- 1.0 / pmax(s^2, 1e-12)
  theta <- if (!is.null(benchmark)) as.double(benchmark) else sum(w_iv * y) / sum(w_iv)

  phi <- 1.0
  if (isTRUE(overdispersion) && length(y) > 2L) {
    z_raw <- (y - theta) / pmax(s, 1e-9)
    win <- stats::quantile(z_raw, probs = c(0.10, 0.90), names = FALSE)
    z_win <- pmin(pmax(z_raw, win[1L]), win[2L])
    phi <- max(1.0, sum(z_win^2) / max(length(y) - 1.0, 1.0))
  }

  c_var <- stats::median((s^2) * n_den)
  n_grid <- seq(max(min(n_den) * 0.90, 1.0), max(n_den) * 1.06, length.out = 200L)
  se_grid <- sqrt((c_var / n_grid) * phi)

  z95 <- stats::qnorm(0.975)
  z998 <- stats::qnorm(0.999)
  se_eff_obs <- s * sqrt(phi)
  is_out <- abs(y - theta) > z998 * se_eff_obs

  y_nm <- if (!is.null(ylabel)) ylabel else if (!is.null(meta_out)) meta_out else estimate
  x_nm <- if (!is.null(xlabel)) xlabel else denominator
  p_title <- if (!is.null(title)) title else .tr("funnel_title", lang, estimate = y_nm, denominator = x_nm)

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mai = c(0.85, 0.88, 0.72, 0.35), bg = "white", col.axis = "#1E293B", col.lab = "#0F172A", las = 1)

  y_all <- c(y, theta + z998 * se_grid, theta - z998 * se_grid)
  y_rng <- range(y_all, na.rm = TRUE)
  pad_y <- max(diff(y_rng), 1.0)

  graphics::plot(NA, xlim = range(n_grid), ylim = c(y_rng[1L] - 0.08 * pad_y, y_rng[2L] + 0.24 * pad_y), xlab = x_nm, ylab = y_nm, bty = "l", main = "")
  graphics::grid(col = "#CBD5E1", lty = "dotted")
  graphics::mtext(p_title, side = 3, line = 1.2, font = 2, cex = 1.05, col = "#0F172A")

  graphics::abline(h = theta, col = "#0F172A", lwd = 1.8)
  graphics::lines(n_grid, theta + z95 * se_grid, col = "#334155", lty = 2, lwd = 1.4)
  graphics::lines(n_grid, theta - z95 * se_grid, col = "#334155", lty = 2, lwd = 1.4)
  graphics::lines(n_grid, theta + z998 * se_grid, col = "#64748B", lty = 3, lwd = 1.4)
  graphics::lines(n_grid, theta - z998 * se_grid, col = "#64748B", lty = 3, lwd = 1.4)

  if (any(!is_out)) {
    graphics::points(n_den[!is_out], y[!is_out], pch = 21, bg = "#0072B2D9", col = "#FFFFFF", cex = 1.35)
  }
  if (any(is_out)) {
    graphics::points(n_den[is_out], y[is_out], pch = 21, bg = "#DC2626EB", col = "#FFFFFF", cex = 1.55)
    if (isTRUE(label_outliers)) {
      pos_v <- ifelse(n_den[is_out] > 0.80 * max(n_grid), 2L, 4L)
      for (k_i in seq_along(pos_v)) {
        graphics::text(n_den[is_out][k_i], y[is_out][k_i], labels = lbls[is_out][k_i], pos = pos_v[k_i], offset = 0.48, cex = 0.78, col = "#991B1B")
      }
    }
  }

  graphics::legend(
    "topright", bty = "n", cex = 0.82,
    legend = c(
      .tr("benchmark", lang, val = sprintf("%.2f", theta)),
      .tr("limit_95", lang),
      .tr("limit_998", lang),
      .tr("within_limits", lang),
      .tr("outside_limits", lang)
    ),
    col = c("#0F172A", "#334155", "#64748B", "#FFFFFF", "#FFFFFF"),
    pt.bg = c(NA, NA, NA, "#0072B2D9", "#DC2626EB"),
    lty = c(1, 2, 3, NA, NA), lwd = c(1.8, 1.4, 1.4, NA, NA),
    pch = c(NA, NA, NA, 21, 21), pt.cex = c(NA, NA, NA, 1.35, 1.5)
  )
  invisible(data.frame(label = lbls, denominator = n_den, estimate = y, se = s, outlier_998 = is_out, stringsAsFactors = FALSE))
}
