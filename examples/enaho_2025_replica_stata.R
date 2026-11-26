# Complex-survey inequality analysis replicating Stata & Python workflows in R with `estheRineq`

library(estheRineq)

run_enaho_example <- function(df, fig_dir = NULL) {
  df$cloro <- ifelse(df$p110a == 1, 1.0, ifelse(df$p110a %in% c(2, 3), 0.0, NA_real_))
  df$quintile <- xtile(df$gasto_mens_pc, weights = df$factor07, n_quantiles = 5L, labels = paste0("Q", 1:5))

  svy <- survey_health_data(
    data = df,
    weights = "factor07",
    strata = "estrato",
    psu = "conglome"
  )

  cat(to_markdown(survey_mean(svy, "cloro", scale = 100), include_strobe = FALSE), "\n\n")
  cat(to_markdown(survey_mean(svy, "cloro", by = "quintile", scale = 100), include_strobe = FALSE), "\n\n")
  cat(to_markdown(who_summary(
    svy,
    outcome = "cloro",
    dimension = "quintile",
    measures = c("d", "r", "par", "paf", "sii", "rii", "aci", "rci"),
    ordered = TRUE,
    favorable = TRUE,
    scale = 100
  )), "\n\n")
  cat(to_markdown(concentration_index(
    svy,
    outcome = "cloro",
    rank = "gasto_mens_pc",
    method = "erreygers",
    by = "regionnat",
    favorable = TRUE
  )), "\n\n")

  if (!is.null(fig_dir)) {
    dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
    cres <- concentration_curve(svy, outcome = "cloro", rank = "gasto_mens_pc", by = "regionnat", n_grid = 41L)
    grDevices::png(file.path(fig_dir, "enaho_2025_curves_r.png"), width = 1260, height = 1150, res = 180)
    plot_curve(cres, palette = "lancet", backend = "base")
    grDevices::dev.off()

    grDevices::png(file.path(fig_dir, "enaho_2025_equiplot_r.png"), width = 1650, height = 2000, res = 180)
    equiplot(svy, outcome = "cloro", subgroup = "quintile", by = "dpto", scale = 100, favorable = TRUE, palette = "pelotas", backend = "base")
    grDevices::dev.off()
  }
}
