# Sentence-Case Trilingual (`en`, `es`, `pt`) Dictionary & STROBE-Equity (2025) Methodological Notes

.I18N_LABELS <- list(
  en = c(
    measure = "Measure",
    domain = "Subgroup",
    estimate = "Estimate",
    se = "Std. error",
    ci_low = "95% CI lower",
    ci_high = "95% CI upper",
    cv_pct = "CV (%)",
    p_value = "p-value",
    n_obs = "Sample n",
    sum_weights = "Weighted population",
    deff = "DEFF",
    interpretation = "Interpretation",
    factor = "Variable",
    elasticity = "Elasticity",
    factor_ci = "Factor CI",
    contribution = "Contribution",
    pct_share = "Share (%)",
    explained = "Explained",
    unexplained = "Unexplained",
    total_diff = "Total difference",
    residual = "Residual",
    cum_pop_ranked_by = "Cumulative population share ranked by {rank}",
    cum_share_of = "Cumulative share of {outcome}",
    gen_ordinate_of = "Generalized ordinate of {outcome}",
    abs_gap_of = "Absolute concentration gap of {outcome}",
    line_of_equality = "Line of equality",
    conc_curve_title = "Concentration curve of {outcome} by {rank}",
    lorenz_curve_title = "Lorenz curve of {outcome}",
    equiplot_title = "Inequality profile of {outcome} by {subgroup}",
    funnel_title = "Funnel plot of {estimate} by {denominator}",
    gradient_title = "Socioeconomic gradient of {outcome} across {subgroup}",
    fractional_rank_of = "Fractional rank of {subgroup}",
    fitted_gradient = "Fitted gradient",
    observed_mean = "Subgroup estimate",
    decomp_title = "Inequality decomposition of {outcome} by {rank}",
    benchmark = "Benchmark = {val}",
    limit_95 = "95% control limits",
    limit_998 = "99.8% control limits",
    within_limits = "Within 99.8% limits",
    outside_limits = "Outside 99.8% limits",
    pro_advantaged = "Concentrated among more advantaged groups",
    pro_disadvantaged = "Concentrated among more disadvantaged groups",
    near_equality = "No statistically clear socioeconomic gradient",
    favorable_yes = "favorable indicator, higher values are better",
    favorable_no = "adverse indicator, lower values are better",
    small_cell_warn = "Caution: {k} subgroup(s) have unweighted sample size n < 25.",
    omnibus_test = "Subgroup equality Wald test",
    top_inequality = "Top inequality",
    bottom_inequality = "Bottom inequality",
    linear_inequality = "Linear gradient"
  ),
  es = c(
    measure = "Medida",
    domain = "Subgrupo",
    estimate = "Estimaci\u00f3n",
    se = "Error est\u00e1ndar",
    ci_low = "IC 95% inf.",
    ci_high = "IC 95% sup.",
    cv_pct = "CV (%)",
    p_value = "Valor p",
    n_obs = "n muestral",
    sum_weights = "Poblaci\u00f3n ponderada",
    deff = "DEFF",
    interpretation = "Interpretaci\u00f3n",
    factor = "Variable",
    elasticity = "Elasticidad",
    factor_ci = "IC de la variable",
    contribution = "Contribuci\u00f3n",
    pct_share = "Contribuci\u00f3n (%)",
    explained = "Explicado",
    unexplained = "No explicado",
    total_diff = "Diferencia total",
    residual = "Residuo",
    cum_pop_ranked_by = "Proporci\u00f3n acumulada de poblaci\u00f3n seg\u00fan {rank}",
    cum_share_of = "Proporci\u00f3n acumulada de {outcome}",
    gen_ordinate_of = "Ordenada generalizada de {outcome}",
    abs_gap_of = "Brecha absoluta de concentraci\u00f3n de {outcome}",
    line_of_equality = "L\u00ednea de igualdad",
    conc_curve_title = "Curva de concentraci\u00f3n de {outcome} seg\u00fan {rank}",
    lorenz_curve_title = "Curva de Lorenz de {outcome}",
    equiplot_title = "Perfil de equidad de {outcome} seg\u00fan {subgroup}",
    funnel_title = "Gr\u00e1fico de embudo de {estimate} seg\u00fan {denominator}",
    gradient_title = "Gradiente socioecon\u00f3mico de {outcome} seg\u00fan {subgroup}",
    fractional_rank_of = "Rango fraccional de {subgroup}",
    fitted_gradient = "Gradiente ajustado",
    observed_mean = "Estimaci\u00f3n por subgrupo",
    decomp_title = "Descomposici\u00f3n de desigualdad de {outcome} seg\u00fan {rank}",
    benchmark = "Referencia = {val}",
    limit_95 = "L\u00edmites de control 95%",
    limit_998 = "L\u00edmites de control 99.8%",
    within_limits = "Dentro de l\u00edmites 99.8%",
    outside_limits = "Fuera de l\u00edmites 99.8%",
    pro_advantaged = "Concentrado en grupos m\u00e1s aventajados",
    pro_disadvantaged = "Concentrado en grupos m\u00e1s desfavorecidos",
    near_equality = "Sin gradiente socioecon\u00f3mico estad\u00edsticamente claro",
    favorable_yes = "indicador favorable, mayor valor es mejor",
    favorable_no = "indicador adverso, menor valor es mejor",
    small_cell_warn = "Precauci\u00f3n: {k} subgrupo(s) con tama\u00f1o muestral no ponderado n < 25.",
    omnibus_test = "Prueba de Wald de igualdad entre subgrupos",
    top_inequality = "Desigualdad superior",
    bottom_inequality = "Desigualdad inferior",
    linear_inequality = "Gradiente lineal"
  ),
  pt = c(
    measure = "Medida",
    domain = "Subgrupo",
    estimate = "Estimativa",
    se = "Erro padr\u00e3o",
    ci_low = "IC 95% inf.",
    ci_high = "IC 95% sup.",
    cv_pct = "CV (%)",
    p_value = "Valor p",
    n_obs = "n amostral",
    sum_weights = "Popula\u00e7\u00e3o ponderada",
    deff = "DEFF",
    interpretation = "Interpreta\u00e7\u00e3o",
    factor = "Vari\u00e1vel",
    elasticity = "Elasticidade",
    factor_ci = "IC da vari\u00e1vel",
    contribution = "Contribui\u00e7\u00e3o",
    pct_share = "Participa\u00e7\u00e3o (%)",
    explained = "Explicado",
    unexplained = "N\u00e3o explicado",
    total_diff = "Diferen\u00e7a total",
    residual = "Res\u00edduo",
    cum_pop_ranked_by = "Propor\u00e7\u00e3o acumulada da popula\u00e7\u00e3o segundo {rank}",
    cum_share_of = "Propor\u00e7\u00e3o acumulada de {outcome}",
    gen_ordinate_of = "Ordenada generalizada de {outcome}",
    abs_gap_of = "Hiato absoluto de concentra\u00e7\u00e3o de {outcome}",
    line_of_equality = "Linha de igualdade",
    conc_curve_title = "Curva de concentra\u00e7\u00e3o de {outcome} segundo {rank}",
    lorenz_curve_title = "Curva de Lorenz de {outcome}",
    equiplot_title = "Perfil de equidade de {outcome} segundo {subgroup}",
    funnel_title = "Gr\u00e1fico de funil de {estimate} segundo {denominator}",
    gradient_title = "Gradiente socioecon\u00f4mico de {outcome} segundo {subgroup}",
    fractional_rank_of = "Posto fracion\u00e1rio de {subgroup}",
    fitted_gradient = "Gradiente ajustado",
    observed_mean = "Estimativa por subgrupo",
    decomp_title = "Decomposi\u00e7\u00e3o da desigualdade de {outcome} segundo {rank}",
    benchmark = "Refer\u00eancia = {val}",
    limit_95 = "Limites de controle 95%",
    limit_998 = "Limites de controle 99.8%",
    within_limits = "Dentro dos limites 99.8%",
    outside_limits = "Fora dos limites 99.8%",
    pro_advantaged = "Concentrado nos grupos mais favorecidos",
    pro_disadvantaged = "Concentrado nos grupos mais desfavorecidos",
    near_equality = "Sem gradiente socioecon\u00f4mico estatisticamente claro",
    favorable_yes = "indicador favor\u00e1vel, maior valor \u00e9 melhor",
    favorable_no = "indicador adverso, menor valor \u00e9 melhor",
    small_cell_warn = "Aten\u00e7\u00e3o: {k} subgrupo(s) com tamanho amostral n\u00e3o ponderado n < 25.",
    omnibus_test = "Teste de Wald de igualdade entre subgrupos",
    top_inequality = "Desigualdade no topo",
    bottom_inequality = "Desigualdade na base",
    linear_inequality = "Gradiente linear"
  )
)

.tr <- function(.key, .lang = "en", ...) {
  lg <- tolower(trimws(.lang))
  if (!(lg %in% names(.I18N_LABELS))) {
    stop(sprintf("Unsupported language `%s`. Choose `'en'`, `'es'`, or `'pt'`.", .lang), call. = FALSE)
  }
  dict <- .I18N_LABELS[[lg]]
  tmpl <- if (.key %in% names(dict)) unname(dict[[.key]]) else if (.key %in% names(.I18N_LABELS$en)) unname(.I18N_LABELS$en[[.key]]) else .key
  args <- list(...)
  if (length(args) > 0L) {
    for (nm in names(args)) {
      tmpl <- gsub(paste0("\\{", nm, "\\}"), as.character(args[[nm]]), tmpl)
    }
  }
  tmpl
}

.interpret_gradient_sign <- function(estimate, ci_low = NA_real_, ci_high = NA_real_, null_value = 0.0, rank_order = "asc", lang = "en") {
  if (!is.finite(estimate)) return("")
  if (is.finite(ci_low) && is.finite(ci_high) && ci_low <= null_value && null_value <= ci_high) {
    return(.tr("near_equality", lang))
  }
  is_pos <- estimate > null_value
  if (rank_order == "desc") is_pos <- !is_pos
  if (is_pos) .tr("pro_advantaged", lang) else .tr("pro_disadvantaged", lang)
}

strobe_equity_note <- function(object, lang = "en") {
  meta <- object$metadata
  if (is.null(meta)) meta <- list()
  lg <- tolower(trimws(lang))
  if (!(lg %in% c("en", "es", "pt"))) stop("`lang` must be `'en'`, `'es'`, or `'pt'`.", call. = FALSE)

  outcome <- if (!is.null(meta$outcome)) meta$outcome else "health_indicator"
  dim_nm <- if (!is.null(meta$dimension)) meta$dimension else if (!is.null(meta$rank)) meta$rank else "socioeconomic_rank"
  rank_order <- if (!is.null(meta$rank_order)) meta$rank_order else "asc"
  favorable <- meta$favorable
  vce <- if (!is.null(meta$vce_method)) meta$vce_method else "if"
  data_mode <- if (!is.null(meta$data_mode)) meta$data_mode else "survey"
  n_obs <- meta$n_obs
  sum_w <- meta$sum_weights
  n_st <- meta$n_strata
  n_psu <- meta$n_psu
  df_deg <- meta$df
  deff <- meta$mean_deff
  small_cells <- if (!is.null(meta$small_cells_under_25)) meta$small_cells_under_25 else 0L
  conf_pct <- as.integer(round(100.0 * (if (!is.null(meta$confidence_level)) meta$confidence_level else 0.95)))
  measures_str <- if (!is.null(meta$measures)) paste(meta$measures, collapse = ", ") else "Inequality index"

  pol_txt <- if (isTRUE(favorable)) {
    .tr("favorable_yes", lg)
  } else if (identical(favorable, FALSE)) {
    .tr("favorable_no", lg)
  } else {
    c(en = "polarity not specified", es = "polaridad no especificada", pt = "polaridade n\u00e3o especificada")[[lg]]
  }

  order_txt <- if (rank_order == "asc") {
    c(en = "ascending from most disadvantaged at 0 to most advantaged at 1",
      es = "ascendente desde el m\u00e1s desfavorecido en 0 hasta el m\u00e1s aventajado en 1",
      pt = "ascendente do mais desfavorecido em 0 ao mais favorecido em 1")[[lg]]
  } else {
    c(en = "descending from most advantaged at 0 to most disadvantaged at 1",
      es = "descendente desde el m\u00e1s aventajado en 0 hasta el m\u00e1s desfavorecido en 1",
      pt = "descendente do mais favorecido em 0 ao mais desfavorecido em 1")[[lg]]
  }

  vce_map <- list(
    `if` = c(
      en = "influence-function linearization with stratified ultimate-cluster variance",
      es = "linealizaci\u00f3n por funciones de influencia con varianza estratificada de conglomerados primarios",
      pt = "lineariza\u00e7\u00e3o por fun\u00e7\u00f5es de influ\u00eancia com vari\u00e2ncia estratificada de conglomerados prim\u00e1rios"
    ),
    convenient = c(
      en = "Kakwani-Wagstaff-van Doorslaer convenient regression",
      es = "regresi\u00f3n conveniente de Kakwani-Wagstaff-van Doorslaer",
      pt = "regress\u00e3o conveniente de Kakwani-Wagstaff-van Doorslaer"
    ),
    newey = c(
      en = "Newey-West rank-autocorrelation variance",
      es = "varianza de Newey-West robusta a autocorrelaci\u00f3n de rango",
      pt = "vari\u00e2ncia de Newey-West robusta \u00e0 autocorrela\u00e7\u00e3o de posto"
    ),
    replicate = c(
      en = "survey replicate weights",
      es = "pesos replicados de dise\u00f1o muestral",
      pt = "pesos replicados do plano amostral"
    ),
    bootstrap = c(
      en = "Rao-Wu-Yue rescaled PSU survey bootstrap",
      es = "bootstrap muestral reescalado de Rao-Wu-Yue por UPM",
      pt = "bootstrap amostral reescalonado de Rao-Wu-Yue por UPA"
    ),
    monte_carlo = c(
      en = "subgroup parametric Monte Carlo propagation",
      es = "propagaci\u00f3n param\u00e9trica Monte Carlo sobre estimaciones desagregadas",
      pt = "propaga\u00e7\u00e3o param\u00e9trica de Monte Carlo sobre estimativas desagregadas"
    )
  )
  vce_txt <- if (vce %in% names(vce_map)) unname(vce_map[[vce]][[lg]]) else vce

  parts <- character(0)
  if (lg == "en") {
    parts <- c(parts, sprintf(
      "STROBE-Equity note: Outcome `%s`; %s. Stratifier `%s` using tie-adjusted midpoint fractional ranks (%s). Reported measures (%d%% CI): %s.",
      outcome, pol_txt, dim_nm, order_txt, conf_pct, measures_str
    ))
    if (data_mode == "survey") {
      ds_bits <- character(0)
      if (!is.null(n_obs)) ds_bits <- c(ds_bits, sprintf("n = %s", format(as.integer(n_obs), big.mark = ",")))
      if (!is.null(sum_w)) ds_bits <- c(ds_bits, sprintf("weighted population = %s", format(round(sum_w, 1), nsmall = 1, big.mark = ",")))
      if (!is.null(n_st)) ds_bits <- c(ds_bits, sprintf("strata = %d", as.integer(n_st)))
      if (!is.null(n_psu)) ds_bits <- c(ds_bits, sprintf("PSUs = %d", as.integer(n_psu)))
      if (!is.null(df_deg)) ds_bits <- c(ds_bits, sprintf("df = %.0f", df_deg))
      if (!is.null(deff) && is.finite(deff)) ds_bits <- c(ds_bits, sprintf("mean DEFF = %.2f", deff))
      parts <- c(parts, sprintf("Survey design: %s; standard errors via %s.", paste(ds_bits, collapse = ", "), vce_txt))
    } else {
      parts <- c(parts, sprintf("Disaggregated subgroup data; uncertainty via %s.", vce_txt))
    }
  } else if (lg == "es") {
    parts <- c(parts, sprintf(
      "Nota STROBE-Equity: Desenlace `%s`; %s. Estratificador `%s` con rangos fraccionales de punto medio ajustados por empates (%s). Medidas reportadas (IC %d%%): %s.",
      outcome, pol_txt, dim_nm, order_txt, conf_pct, measures_str
    ))
    if (data_mode == "survey") {
      ds_bits <- character(0)
      if (!is.null(n_obs)) ds_bits <- c(ds_bits, sprintf("n = %s", format(as.integer(n_obs), big.mark = ",")))
      if (!is.null(sum_w)) ds_bits <- c(ds_bits, sprintf("poblaci\u00f3n ponderada = %s", format(round(sum_w, 1), nsmall = 1, big.mark = ",")))
      if (!is.null(n_st)) ds_bits <- c(ds_bits, sprintf("estratos = %d", as.integer(n_st)))
      if (!is.null(n_psu)) ds_bits <- c(ds_bits, sprintf("UPM = %d", as.integer(n_psu)))
      if (!is.null(df_deg)) ds_bits <- c(ds_bits, sprintf("gl = %.0f", df_deg))
      if (!is.null(deff) && is.finite(deff)) ds_bits <- c(ds_bits, sprintf("DEFF medio = %.2f", deff))
      parts <- c(parts, sprintf("Dise\u00f1o muestral: %s; errores est\u00e1ndar v\u00eda %s.", paste(ds_bits, collapse = ", "), vce_txt))
    } else {
      parts <- c(parts, sprintf("Datos agregados por subgrupo; incertidumbre v\u00eda %s.", vce_txt))
    }
  } else {
    parts <- c(parts, sprintf(
      "Nota STROBE-Equity: Desfecho `%s`; %s. Estratificador `%s` com postos fracion\u00e1rios ajustados para empates (%s). Medidas reportadas (IC %d%%): %s.",
      outcome, pol_txt, dim_nm, order_txt, conf_pct, measures_str
    ))
    if (data_mode == "survey") {
      ds_bits <- character(0)
      if (!is.null(n_obs)) ds_bits <- c(ds_bits, sprintf("n = %s", format(as.integer(n_obs), big.mark = ",")))
      if (!is.null(sum_w)) ds_bits <- c(ds_bits, sprintf("popula\u00e7\u00e3o ponderada = %s", format(round(sum_w, 1), nsmall = 1, big.mark = ",")))
      if (!is.null(n_st)) ds_bits <- c(ds_bits, sprintf("estratos = %d", as.integer(n_st)))
      if (!is.null(n_psu)) ds_bits <- c(ds_bits, sprintf("UPAs = %d", as.integer(n_psu)))
      if (!is.null(df_deg)) ds_bits <- c(ds_bits, sprintf("gl = %.0f", df_deg))
      if (!is.null(deff) && is.finite(deff)) ds_bits <- c(ds_bits, sprintf("DEFF m\u00e9dio = %.2f", deff))
      parts <- c(parts, sprintf("Plano amostral: %s; erros padr\u00e3o via %s.", paste(ds_bits, collapse = ", "), vce_txt))
    } else {
      parts <- c(parts, sprintf("Dados agregados por subgrupo; incerteza via %s.", vce_txt))
    }
  }

  if (small_cells > 0L) {
    parts <- c(parts, .tr("small_cell_warn", lg, k = small_cells))
  }
  paste(parts, collapse = " ")
}
