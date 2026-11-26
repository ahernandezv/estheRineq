# Built-in Epidemiological & Complex-Survey Benchmark Datasets for `estheRineq`

load_survey_mch <- function(n = 1200L, seed = 2026L) {
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit({
    if (is.null(old_seed)) { if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv) } else assign(".Random.seed", old_seed, envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(as.integer(seed))

  regions <- c("Coast", "Highlands", "Jungle")
  areas <- c("Urban", "Rural")
  strata_grid <- expand.grid(area = areas, region = regions, stringsAsFactors = FALSE)
  n_strata <- nrow(strata_grid)
  psus_per_st <- 10L
  total_psus <- n_strata * psus_per_st
  base_per_psu <- max(as.integer(n) %/% total_psus, 4L)

  rows <- list()
  obs_id <- 0L

  for (s_i in seq_len(n_strata)) {
    reg <- strata_grid$region[s_i]
    ar <- strata_grid$area[s_i]
    st_code <- sprintf("S%d_%s_%s", s_i, substr(reg, 1, 3), substr(ar, 1, 3))
    w_shift <- (if (reg == "Coast") 0.75 else if (reg == "Highlands") -0.45 else -0.60) + (if (ar == "Urban") 0.65 else -0.65)
    b_wgt <- (if (reg == "Coast") 1.35 else if (reg == "Highlands") 0.95 else 0.70) * (if (ar == "Urban") 1.15 else 0.85)

    for (p_loc in seq_len(psus_per_st)) {
      psu_id <- (s_i - 1L) * psus_per_st + p_loc
      re_w <- stats::rnorm(1L, 0.0, 0.35)
      re_h <- stats::rnorm(1L, 0.0, 0.28)
      m_obs <- base_per_psu + (if (obs_id + (total_psus - psu_id + 1L) * base_per_psu < n) 1L else 0L)

      waves <- sample(c(2018L, 2025L), size = m_obs, replace = TRUE)
      wave_sh <- ifelse(waves == 2025L, 0.20, 0.0)
      w_obs <- round(b_wgt * exp(stats::rnorm(m_obs, 0.0, 0.22)) * 100.0, 3)
      wealth <- round(w_shift + wave_sh + re_w + stats::rnorm(m_obs, 0.0, 0.85), 5)
      income <- round(exp(6.2 + 0.58 * wealth + stats::rnorm(m_obs, 0.0, 0.25)), 2)

      edu_lat <- wealth + stats::rnorm(m_obs, 0.0, 0.75)
      edu <- ifelse(edu_lat < -0.45, "None/Primary", ifelse(edu_lat < 0.75, "Secondary", "Higher"))
      edu_num <- ifelse(edu == "None/Primary", 0.0, ifelse(edu == "Secondary", 1.0, 2.0))

      sanit <- as.integer(stats::runif(m_obs) < stats::plogis(0.2 + 0.95 * wealth + (if (ar == "Urban") 0.6 else -0.4)))
      age_mo <- sample(6L:59L, size = m_obs, replace = TRUE)
      female <- sample(0L:1L, size = m_obs, replace = TRUE)
      morb <- as.integer(stats::runif(m_obs) < stats::plogis(-1.1 - 0.30 * wealth + 0.012 * (36 - abs(age_mo - 24))))

      eta_st <- -1.25 - 0.62 * wealth - 0.35 * edu_num - 0.40 * sanit - ifelse(waves == 2025L, 0.25, 0.0) + 0.008 * age_mo + re_h
      stunt <- as.integer(stats::runif(m_obs) < stats::plogis(eta_st))

      eta_im <- 0.55 + 0.58 * wealth + 0.38 * edu_num + (if (ar == "Urban") 0.30 else -0.20) + ifelse(waves == 2025L, 0.35, 0.0) - 0.5 * re_h
      full_im <- as.integer(stats::runif(m_obs) < stats::plogis(eta_im))

      mu_vis <- exp(0.15 + 0.85 * morb - 0.006 * age_mo + 0.12 * female + 0.28 * wealth + 0.18 * edu_num)
      visits <- stats::rpois(m_obs, lambda = mu_vis)

      ids <- seq.int(obs_id + 1L, obs_id + m_obs)
      obs_id <- obs_id + m_obs

      rows[[length(rows) + 1L]] <- data.frame(
        hh_id = ids,
        survey_wave = waves,
        stratum_id = rep(st_code, m_obs),
        cluster_id = rep(sprintf("PSU_%03d", psu_id), m_obs),
        sample_weight = w_obs,
        region = rep(reg, m_obs),
        area = rep(ar, m_obs),
        wealth_score = wealth,
        household_income = income,
        mother_edu = edu,
        improved_sanitation = sanit,
        child_age_mo = age_mo,
        female = female,
        chronic_morbidity = morb,
        stunting = stunt,
        full_immunization = full_im,
        outpatient_visits = visits,
        stringsAsFactors = FALSE
      )
    }
  }

  df <- do.call(rbind, rows)
  df$wealth_quintile <- xtile(df$wealth_score, weights = df$sample_weight, n_quantiles = 5L)
  rownames(df) <- NULL
  df
}

load_who_disaggregated <- function(dimension = c("both", "wealth", "region")) {
  dimension <- match.arg(dimension)
  wealth_df <- data.frame(
    indicator = rep("dtp3_coverage", 5L),
    dimension = rep("wealth_quintile", 5L),
    ordered = rep(TRUE, 5L),
    favorable = rep(TRUE, 5L),
    scale = rep(100.0, 5L),
    subgroup = c("Q1", "Q2", "Q3", "Q4", "Q5"),
    subgroup_order = 1:5,
    estimate = c(68.4, 76.2, 83.5, 89.1, 94.6),
    se = c(2.1, 1.8, 1.6, 1.4, 1.1),
    population = c(225000, 210000, 198000, 187000, 180000),
    sample_size = c(1420L, 1380L, 1310L, 1240L, 1150L),
    stringsAsFactors = FALSE
  )
  region_df <- data.frame(
    indicator = rep("dtp3_coverage", 5L),
    dimension = rep("region", 5L),
    ordered = rep(FALSE, 5L),
    favorable = rep(TRUE, 5L),
    scale = rep(100.0, 5L),
    subgroup = c("Metropolitan", "North Coast", "Central Valley", "South Highlands", "Amazon Basin"),
    subgroup_order = 1:5,
    estimate = c(93.2, 86.5, 82.0, 74.8, 65.1),
    se = c(1.2, 1.7, 1.9, 2.3, 2.8),
    population = c(340000, 195000, 160000, 175000, 130000),
    sample_size = c(1900L, 1120L, 950L, 1040L, 790L),
    stringsAsFactors = FALSE
  )
  if (dimension == "wealth") return(wealth_df)
  if (dimension == "region") return(region_df)
  rbind(wealth_df, region_df)
}

load_district_mortality <- function(seed = 2026L) {
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit({
    if (is.null(old_seed)) { if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv) } else assign(".Random.seed", old_seed, envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(as.integer(seed))

  age_bands <- c("0-14", "15-29", "30-44", "45-59", "60+")
  base_rates <- c(0.00045, 0.00075, 0.00160, 0.00480, 0.02450)
  young_s <- c(0.33, 0.27, 0.19, 0.13, 0.08)
  old_s <- c(0.17, 0.20, 0.22, 0.21, 0.20)

  rows <- list()
  for (d_i in 1:24) {
    q <- ((d_i - 1L) %% 5L) + 1L
    tot_pop <- sample(28000L:240000L, 1L)
    mix <- (q - 1.0) / 4.0
    shares <- (1.0 - mix) * young_s + mix * old_s
    shares <- shares / sum(shares)
    out_mult <- if (d_i == 2L) 1.48 else if (d_i == 24L) 0.66 else exp(stats::rnorm(1L, 0.0, 0.06))
    rr_dep <- (1.36 - 0.14 * (q - 1.0)) * out_mult

    pop_a <- pmax(as.integer(round(tot_pop * shares)), 200L)
    deaths_a <- stats::rpois(5L, lambda = pop_a * base_rates * rr_dep)
    rows[[d_i]] <- data.frame(
      district = rep(sprintf("District_%02d", d_i), 5L),
      deprivation_quintile = rep(q, 5L),
      age_group = age_bands,
      deaths = deaths_a,
      population = pop_a,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}
