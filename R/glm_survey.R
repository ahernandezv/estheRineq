# Survey-Weighted IRLS Generalized Linear Model Engine & Design Matrix Builder

.build_design_matrix <- function(
  df,
  formula = NULL,
  outcome = NULL,
  covariates = NULL,
  categorical = NULL,
  add_intercept = TRUE,
  extra_cols = NULL
) {
  df_work <- df
  if (!is.null(categorical)) {
    for (cat_col in categorical) {
      if (cat_col %in% names(df_work) && !is.factor(df_work[[cat_col]])) {
        df_work[[cat_col]] <- factor(df_work[[cat_col]])
      }
    }
  }

  if (!is.null(formula)) {
    f_obj <- if (inherits(formula, "formula")) formula else stats::as.formula(formula)
    vars_f <- all.vars(f_obj)
    for (v in vars_f) {
      if (!(v %in% names(df_work))) {
        stop(sprintf("Variable `%s` in formula not found in dataset.", v), call. = FALSE)
      }
      if (is.character(df_work[[v]])) {
        df_work[[v]] <- factor(df_work[[v]])
      }
    }
    req_cols <- unique(c(vars_f, extra_cols))
    valid <- stats::complete.cases(df_work[, req_cols, drop = FALSE])
    sub_df <- df_work[valid, , drop = FALSE]
    mf <- stats::model.frame(f_obj, data = sub_df, na.action = stats::na.pass)
    mt <- attr(mf, "terms")
    if (!add_intercept) attr(mt, "intercept") <- 0L
    X <- stats::model.matrix(mt, data = mf)
    assign_vec <- attr(X, "assign")
    term_labels <- attr(mt, "term.labels")

    y_resp <- stats::model.response(mf)
    y_vec <- if (is.null(y_resp)) NULL else .to_clean_numeric(y_resp, vars_f[1L])
    out_name <- if (attr(mt, "response") == 1L) vars_f[1L] else outcome

    factor_groups <- list()
    if (any(assign_vec == 0L)) {
      factor_groups[["Intercept"]] <- which(assign_vec == 0L)
    }
    for (t_idx in seq_along(term_labels)) {
      cols_t <- which(assign_vec == t_idx)
      if (length(cols_t) > 0L) {
        # Strip C(...) wrapper if user wrote C(var)
        clean_lbl <- sub("^C\\(([^,)]+).*\\)$", "\\1", term_labels[t_idx])
        factor_groups[[clean_lbl]] <- cols_t
      }
    }

    return(list(
      y = y_vec,
      X = X,
      col_names = colnames(X),
      factor_groups = factor_groups,
      valid_mask = valid,
      outcome_name = out_name,
      has_intercept = any(assign_vec == 0L)
    ))
  }

  covs <- if (is.null(covariates)) character(0) else as.character(covariates)
  for (v in c(outcome, covs)) {
    if (!is.null(v) && !(v %in% names(df_work))) {
      stop(sprintf("Variable `%s` not found in dataset.", v), call. = FALSE)
    }
    if (!is.null(v) && (v %in% covs) && is.character(df_work[[v]])) {
      df_work[[v]] <- factor(df_work[[v]])
    }
  }
  req_cols <- unique(c(outcome, covs, extra_cols))
  valid <- stats::complete.cases(df_work[, req_cols, drop = FALSE])
  sub_df <- df_work[valid, , drop = FALSE]

  rhs <- if (length(covs) == 0L) "1" else paste(covs, collapse = " + ")
  if (!add_intercept) rhs <- paste0("0 + ", rhs)
  f_str <- if (!is.null(outcome)) paste(outcome, "~", rhs) else paste("~", rhs)
  f_obj <- stats::as.formula(f_str)
  mf <- stats::model.frame(f_obj, data = sub_df, na.action = stats::na.pass)
  mt <- attr(mf, "terms")
  X <- stats::model.matrix(mt, data = mf)
  assign_vec <- attr(X, "assign")
  term_labels <- attr(mt, "term.labels")

  factor_groups <- list()
  if (any(assign_vec == 0L)) {
    factor_groups[["Intercept"]] <- which(assign_vec == 0L)
  }
  for (t_idx in seq_along(term_labels)) {
    cols_t <- which(assign_vec == t_idx)
    if (length(cols_t) > 0L) factor_groups[[term_labels[t_idx]]] <- cols_t
  }

  y_vec <- if (is.null(outcome)) NULL else .to_clean_numeric(sub_df[[outcome]], outcome)
  list(
    y = y_vec,
    X = X,
    col_names = colnames(X),
    factor_groups = factor_groups,
    valid_mask = valid,
    outcome_name = outcome,
    has_intercept = any(assign_vec == 0L)
  )
}

.fit_survey_glm <- function(
  y,
  X,
  weights = NULL,
  family = "gaussian",
  link = NULL,
  max_iter = 100L,
  tol = 1e-9,
  vcov_fn = NULL
) {
  y_arr <- as.double(y)
  X_mat <- as.matrix(X)
  n <- nrow(X_mat)
  p <- ncol(X_mat)
  w_raw <- if (is.null(weights)) rep(1.0, n) else as.double(weights)
  w_norm <- w_raw / sum(w_raw)

  fam <- tolower(trimws(family))
  default_links <- c(gaussian = "identity", binomial = "logit", poisson = "log")
  if (!(fam %in% names(default_links))) {
    stop(sprintf("Unsupported GLM family `%s`.", family), call. = FALSE)
  }
  lnk <- if (is.null(link)) default_links[[fam]] else tolower(trimws(link))

  inv_link <- function(eta) {
    if (lnk == "identity") return(eta)
    if (lnk == "log") return(exp(pmin(pmax(eta, -40.0), 40.0)))
    if (lnk == "logit") return(stats::plogis(pmin(pmax(eta, -35.0), 35.0)))
    if (lnk == "probit") return(stats::pnorm(pmin(pmax(eta, -8.0), 8.0)))
    stop(sprintf("Unsupported link `%s`.", lnk), call. = FALSE)
  }

  deriv_inv_link <- function(eta, mu) {
    if (lnk == "identity") return(rep(1.0, length(eta)))
    if (lnk == "log") return(pmax(mu, 1e-12))
    if (lnk == "logit") return(pmax(mu * (1.0 - mu), 1e-12))
    if (lnk == "probit") return(pmax(stats::dnorm(pmin(pmax(eta, -8.0), 8.0)), 1e-12))
    stop(sprintf("Unsupported link `%s`.", lnk), call. = FALSE)
  }

  var_func <- function(mu) {
    if (fam == "gaussian") return(rep(1.0, length(mu)))
    if (fam == "binomial") return(pmax(mu * (1.0 - mu), 1e-9))
    if (fam == "poisson") return(pmax(mu, 1e-9))
    rep(1.0, length(mu))
  }

  # Exact weighted OLS closed-form shortcut for Gaussian + identity
  if (fam == "gaussian" && lnk == "identity") {
    xtwx <- crossprod(X_mat, X_mat * w_norm)
    xtwx_inv <- tryCatch(solve(xtwx), error = function(e) qr.solve(xtwx + diag(1e-12, p)))
    params <- as.double(xtwx_inv %*% crossprod(X_mat, y_arr * w_norm))
    eta <- as.double(X_mat %*% params)
    resid <- y_arr - eta
    psi_params <- (X_mat * resid) %*% t(xtwx_inv)
    cov_p <- if (!is.null(vcov_fn)) {
      vcov_fn(psi_params)
    } else {
      dof <- if (n > p) n / (n - p) else 1.0
      crossprod(psi_params * w_norm) * dof
    }
    return(list(
      params = params,
      cov_params = cov_p,
      se = sqrt(pmax(diag(cov_p), 0.0)),
      linear_predictor = eta,
      fitted_values = eta,
      dmu_deta = rep(1.0, n),
      psi_params = psi_params,
      family = fam,
      link = lnk,
      inv_link = inv_link,
      deriv_inv_link = deriv_inv_link
    ))
  }

  # Initialize mu & eta safely inside domain
  if (fam == "binomial") {
    mu <- pmin(pmax((w_norm * y_arr + 0.5 / n) / (w_norm + 1.0 / n), 0.02), 0.98)
  } else {
    mu <- pmax(y_arr, 0.1)
  }
  eta <- if (lnk == "identity") mu else if (lnk == "log") log(pmax(mu, 1e-6)) else if (lnk == "logit") stats::qlogis(pmin(pmax(mu, 1e-5), 1 - 1e-5)) else stats::qnorm(pmin(pmax(mu, 1e-5), 1 - 1e-5))

  params <- rep(0.0, p)
  xtwx_inv <- diag(1.0, p)

  for (it in seq_len(max_iter)) {
    dmu <- deriv_inv_link(eta, mu)
    v_mu <- var_func(mu)
    wt_irls <- w_norm * (dmu^2) / v_mu
    wt_irls <- pmax(wt_irls, 1e-14)
    z_work <- eta + (y_arr - mu) / dmu

    xtwx <- crossprod(X_mat, X_mat * wt_irls)
    xtwx_inv <- tryCatch(solve(xtwx), error = function(e) qr.solve(xtwx + diag(1e-10, p)))
    new_params <- as.double(xtwx_inv %*% crossprod(X_mat, z_work * wt_irls))
    new_eta <- as.double(X_mat %*% new_params)
    new_mu <- inv_link(new_eta)

    # Step-halving for identity/log-binomial boundary feasibility
    step <- 1.0
    while (fam == "binomial" && lnk %in% c("identity", "log") && any(new_mu <= 1e-6 | new_mu >= 1.0 - 1e-6) && step > 1e-4) {
      step <- step * 0.5
      cand_p <- params + step * (new_params - params)
      new_eta <- as.double(X_mat %*% cand_p)
      new_mu <- inv_link(new_eta)
      new_params <- cand_p
    }
    if (fam == "binomial" && lnk %in% c("identity", "log")) {
      new_mu <- pmin(pmax(new_mu, 1e-6), 1.0 - 1e-6)
    }

    delta <- max(abs(new_params - params) / (abs(params) + 1e-4))
    params <- new_params
    eta <- new_eta
    mu <- new_mu
    if (it > 1L && delta < tol) break
  }

  dmu <- deriv_inv_link(eta, mu)
  v_mu <- var_func(mu)
  wt_irls <- pmax(w_norm * (dmu^2) / v_mu, 1e-14)
  xtwx <- crossprod(X_mat, X_mat * wt_irls)
  xtwx_inv <- tryCatch(solve(xtwx), error = function(e) qr.solve(xtwx + diag(1e-10, p)))

  score_scalar <- (dmu / v_mu) * (y_arr - mu)
  psi_params <- (X_mat * score_scalar) %*% t(xtwx_inv)

  cov_p <- if (!is.null(vcov_fn)) {
    vcov_fn(psi_params)
  } else {
    dof <- if (n > p) n / (n - p) else 1.0
    crossprod(psi_params * w_norm) * dof
  }

  list(
    params = params,
    cov_params = cov_p,
    se = sqrt(pmax(diag(cov_p), 0.0)),
    linear_predictor = eta,
    fitted_values = mu,
    dmu_deta = dmu,
    psi_params = psi_params,
    family = fam,
    link = lnk,
    inv_link = inv_link,
    deriv_inv_link = deriv_inv_link
  )
}
