# Comprehensive Unit & Golden Verification Suite for `estheRineq` (executed by R CMD check)

library(estheRineq)

assert_true <- function(cond, msg = "Assertion failed") {
  if (!isTRUE(cond)) stop(msg, call. = FALSE)
}

assert_near <- function(a, b, tol = 1e-8, msg = "Values not equal within tolerance") {
  diff_v <- max(abs(as.double(a) - as.double(b)))
  if (!is.finite(diff_v) || diff_v > tol) {
    stop(sprintf("%s (max |diff| = %.3e > %.3e)", msg, diff_v, tol), call. = FALSE)
  }
}

cat("1. Testing exact World Bank benchmark, row-order tie invariance & Gateaux IF derivatives...\n")
y_wb <- c(0.10, 0.15, 0.20, 0.25, 0.30)
r_wb <- c(1, 2, 3, 4, 5)
w_wb <- c(0.25, 0.20, 0.20, 0.20, 0.15)
ci_wb <- influence_concentration_index(y_wb, r_wb, weights = w_wb, method = "standard")
# Analytical manual check
rk_wb <- compute_fractional_ranks(r_wb, weights = w_wb)
mu_wb <- sum((w_wb / sum(w_wb)) * y_wb)
exact_ci <- sum((w_wb / sum(w_wb)) * y_wb * (2 * rk_wb$ridit - 1)) / mu_wb
assert_near(ci_wb$estimate, exact_ci, tol = 1e-12, msg = "World Bank exact CI")

# Row permutation invariance under heavy ties
set.seed(123)
y_tie <- c(1, 0, 1, 1, 0, 0, 1, 0, 1, 1)
r_tie <- c(2, 1, 2, 3, 1, 2, 3, 3, 1, 2)
w_tie <- c(1.5, 2.0, 0.8, 1.2, 1.1, 3.0, 0.5, 1.7, 2.2, 0.9)
perm <- sample(seq_along(y_tie))
for (m in c("standard", "generalized", "wagstaff", "erreygers", "extended", "symmetric")) {
  b_opt <- if (m %in% c("wagstaff", "erreygers", "extended", "symmetric")) c(0, 1) else NULL
  a1 <- influence_concentration_index(y_tie, r_tie, weights = w_tie, method = m, bounds = b_opt, v = 3, beta = 3)
  a2 <- influence_concentration_index(y_tie[perm], r_tie[perm], weights = w_tie[perm], method = m, bounds = b_opt, v = 3, beta = 3)
  assert_near(a1$estimate, a2$estimate, tol = 1e-12, msg = paste("Permutation invariance:", m))
}

# Finite-difference Gateaux derivative verification
eps_fd <- 1e-6
w_norm_t <- w_tie / sum(w_tie)
for (m in c("standard", "generalized", "wagstaff", "erreygers", "extended", "symmetric")) {
  b_opt <- if (m %in% c("wagstaff", "erreygers")) c(0, 1) else NULL
  res0 <- influence_concentration_index(y_tie, r_tie, weights = w_norm_t, method = m, bounds = b_opt, v = 2.5, beta = 2.5)
  fd_psi <- numeric(length(y_tie))
  for (i in seq_along(y_tie)) {
    wp <- (1.0 - eps_fd) * w_norm_t; wp[i] <- wp[i] + eps_fd
    wm <- (1.0 + eps_fd) * w_norm_t; wm[i] <- wm[i] - eps_fd
    ip <- influence_concentration_index(y_tie, r_tie, weights = wp, method = m, bounds = b_opt, v = 2.5, beta = 2.5)$estimate
    im <- influence_concentration_index(y_tie, r_tie, weights = wm, method = m, bounds = b_opt, v = 2.5, beta = 2.5)$estimate
    fd_psi[i] <- (ip - im) / (2 * eps_fd)
  }
  assert_near(res0$psi, fd_psi, tol = 1e-5, msg = paste("Gateaux IF exactness:", m))
}

cat("2. Testing complex survey CI, domain Wald test, classical indices & GE subgroup decomposition...\n")
df_mch <- load_survey_mch(n = 400L, seed = 2026L)
svy <- survey_health_data(df_mch, weights = "sample_weight", strata = "stratum_id", psu = "cluster_id")
res_dom <- concentration_index(svy, outcome = "stunting", rank = "wealth_score", method = "erreygers", by = "area", favorable = FALSE)
assert_true(nrow(res_dom$table) == 2L, "2 domain rows")
assert_true(!is.null(res_dom$omnibus_test) && is.finite(res_dom$omnibus_test$stat), "Omnibus Wald test computed")

res_cls <- classical_inequality(svy, outcome = "household_income", indices = c("gini", "sgini", "atkinson", "ge", "palma", "s80s20"), atkinson_eps = c(0.5, 1.0), ge_alpha = c(0, 1, 2), sgini_v = 2.5)
assert_true(nrow(res_cls$table) == 11L && all(is.finite(res_cls$table$estimate)), "Classical indices finite")

ge_dec <- decompose_subgroup_ge(svy, outcome = "household_income", by = "area", alpha = 1.0, bootstrap_reps = 20L)
assert_near(sum(ge_dec$contributions$contribution), ge_dec$overall_summary$estimate[1L], tol = 1e-12, msg = "Shorrocks GE_W + GE_B == GE_Total")

cat("3. Testing all 21 WHO Health Inequality Monitor measures & guardrails...\n")
df_w <- load_who_disaggregated("wealth")
gd_w <- grouped_health_data(df_w, subgroup = "subgroup", estimate = "estimate", se = "se", population = "population", ordered = TRUE, favorable = TRUE, scale = 100, sample_size = "sample_size")
res_who21 <- who_summary(gd_w, measures = "all", n_sim = 200L)
assert_true(nrow(res_who21$table) == 21L && all(is.finite(res_who21$table$estimate)), "All 21 grouped WHO measures finite")

res_who_svy <- who_summary(svy, outcome = "full_immunization", dimension = "wealth_quintile", ordered = TRUE, favorable = TRUE, scale = 100, n_sim = 150L)
assert_true(nrow(res_who_svy$table) == 21L && all(is.finite(res_who_svy$table$estimate)), "All 21 survey WHO measures finite")

cat("4. Testing SII/RII asc/desc symmetry & ordered factor quintiles...\n")
for (m_sr in c("logit_marginal", "moreno_betancur", "linear")) {
  ra <- sii_rii(svy, outcome = "full_immunization", rank = "wealth_quintile", method = m_sr, rank_order = "asc", scale = 100)
  rd <- sii_rii(svy, outcome = "full_immunization", rank = "wealth_quintile", method = m_sr, rank_order = "desc", scale = 100)
  assert_near(ra$table$estimate[1L], -rd$table$estimate[1L], tol = 1e-8, msg = paste("SII symmetry:", m_sr))
  assert_near(ra$table$estimate[2L], 1.0 / rd$table$estimate[2L], tol = 1e-8, msg = paste("RII symmetry:", m_sr))
}
df_q <- df_mch
df_q$q_num <- xtile(df_q$wealth_score, weights = df_q$sample_weight, n_quantiles = 5L)
df_q$q_cat <- xtile(df_q$wealth_score, weights = df_q$sample_weight, n_quantiles = 5L, labels = paste0("Q", 1:5))
r_qn <- sii_rii(df_q, outcome = "full_immunization", rank = "q_num", weights = "sample_weight", scale = 100)
r_qc <- sii_rii(df_q, outcome = "full_immunization", rank = "q_cat", weights = "sample_weight", scale = 100)
assert_near(r_qn$table$estimate, r_qc$table$estimate, tol = 1e-12, msg = "Ordered factor labels match numeric quintiles")

cat("5. Testing DSR, SMR/ISR, Need Standardization & Horizontal Inequity...\n")
df_dist <- load_district_mortality()
res_dsr <- direct_standardize(df_dist, events = "deaths", population = "population", stratum = "age_group", group = "district", standard_pop = "who_broad_5")
res_smr <- indirect_standardize(df_dist, events = "deaths", population = "population", stratum = "age_group", group = "district")
assert_true(nrow(res_dsr$table) == 24L && all(res_dsr$table$ci_low < res_dsr$table$estimate), "DSR Dobson CIs valid")
assert_true(nrow(res_smr$table) == 48L, "SMR & ISR rows valid")

df_ns <- standardize_health_need(svy, outcome = "outpatient_visits", need_vars = c("chronic_morbidity", "child_age_mo", "female"), non_need_vars = c("wealth_score", "mother_edu"), family = "poisson")
w_ns <- df_ns$sample_weight / sum(df_ns$sample_weight)
assert_near(sum(w_ns * df_ns$outpatient_visits), sum(w_ns * df_ns$y_need_standardized), tol = 1e-10, msg = "Gravelle mean preservation in Poisson need standardization")

res_hi <- horizontal_inequity(svy, outcome = "outpatient_visits", rank = "wealth_score", need_vars = c("chronic_morbidity", "child_age_mo"), non_need_vars = c("mother_edu"), family = "poisson", bootstrap_reps = 20L)
assert_true(nrow(res_hi$table) == 3L && all(is.finite(res_hi$table$estimate)), "Horizontal inequity valid")

cat("6. Testing reference-invariant Wagstaff, RIF-I-OLS, and RIF-Oaxaca decompositions...\n")
df_ref1 <- df_mch; df_ref1$mother_edu <- factor(df_ref1$mother_edu, levels = c("None/Primary", "Secondary", "Higher"))
df_ref2 <- df_mch; df_ref2$mother_edu <- factor(df_ref2$mother_edu, levels = c("Higher", "Secondary", "None/Primary"))
dw1 <- decompose_wagstaff(df_ref1, formula = stunting ~ mother_edu + area + improved_sanitation, rank = "wealth_score", weights = "sample_weight", bootstrap_reps = 0L)
dw2 <- decompose_wagstaff(df_ref2, formula = stunting ~ mother_edu + area + improved_sanitation, rank = "wealth_score", weights = "sample_weight", bootstrap_reps = 0L)
assert_near(dw1$contributions$contribution, dw2$contributions$contribution, tol = 1e-12, msg = "Wagstaff reference-category invariance")

drif <- decompose_rif(svy, formula = stunting ~ mother_edu + area + improved_sanitation, rank = "wealth_score")
doax <- decompose_oaxaca_rif(svy, formula = stunting ~ mother_edu + improved_sanitation, rank = "wealth_score", group = "survey_wave", bootstrap_reps = 15L)
assert_true(nrow(drif$contributions) >= 4L && nrow(doax$overall_summary) == 5L, "RIF and Oaxaca-RIF valid")

cat("7. Testing all 5 plot families & trilingual Markdown/LaTeX/HTML/Excel/Word exporters...\n")
tmp_png <- tempfile(fileext = ".png")
grDevices::png(tmp_png, width = 800, height = 500)
equiplot(svy, outcome = "full_immunization", subgroup = "wealth_quintile", by = "region", scale = 100, diagnose_pattern = TRUE, sort_by = "gap", backend = "base")
cres <- concentration_curve(svy, outcome = "stunting", rank = "wealth_score", by = "area", simultaneous_bands = TRUE)
plot(cres, backend = "base")
plot_sii_gradient(gd_w, backend = "base")
plot(dw1, backend = "base")
funnel_plot(res_dsr, backend = "base")
grDevices::dev.off()
assert_true(file.exists(tmp_png) && file.info(tmp_png)$size > 1000, "Plots rendered cleanly")

for (lg in c("en", "es", "pt")) {
  md <- to_markdown(res_dom, lang = lg)
  lx <- to_latex(res_dom, lang = lg)
  ht <- to_html(res_dom, lang = lg)
  assert_true(nzchar(md) && grepl("\\\\toprule", lx) && grepl("<table", ht), paste("Text exporters:", lg))
}
tmp_xlsx <- tempfile(fileext = ".xlsx")
tmp_docx <- tempfile(fileext = ".docx")
to_excel(res_dom, tmp_xlsx)
to_word(res_dom, tmp_docx)
assert_true(file.exists(tmp_xlsx) && file.info(tmp_xlsx)$size > 500, "OpenXML .xlsx created")
assert_true(file.exists(tmp_docx) && file.info(tmp_docx)$size > 500, "OpenXML .docx created")

cat("ALL ESTHERINEQ GOLDEN TESTS PASSED!\n")
