# S3 Result Classes (`esther_result`, `esther_decomp`, `esther_curve`) & Standard Methods

new_esther_result <- function(table, title = "", metadata = list(), vcov = NULL, omnibus_test = NULL) {
  structure(
    list(
      table = as.data.frame(table, stringsAsFactors = FALSE),
      title = as.character(title),
      metadata = metadata,
      vcov = vcov,
      omnibus_test = omnibus_test
    ),
    class = "esther_result"
  )
}

new_esther_decomp <- function(contributions, overall_summary, title = "", metadata = list()) {
  structure(
    list(
      contributions = as.data.frame(contributions, stringsAsFactors = FALSE),
      overall_summary = as.data.frame(overall_summary, stringsAsFactors = FALSE),
      title = as.character(title),
      metadata = metadata
    ),
    class = "esther_decomp"
  )
}

new_esther_curve <- function(ordinates, title = "", metadata = list()) {
  structure(
    list(
      ordinates = as.data.frame(ordinates, stringsAsFactors = FALSE),
      title = as.character(title),
      metadata = metadata
    ),
    class = "esther_curve"
  )
}

as.data.frame.esther_result <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$table
}

as.data.frame.esther_decomp <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$contributions
}

as.data.frame.esther_curve <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$ordinates
}

coef.esther_result <- function(object, ...) {
  nms <- if ("domain" %in% names(object$table)) {
    paste(object$table$measure, object$table$domain, sep = ":")
  } else if ("measure" %in% names(object$table)) {
    as.character(object$table$measure)
  } else {
    seq_len(nrow(object$table))
  }
  stats::setNames(as.double(object$table$estimate), nms)
}

vcov.esther_result <- function(object, ...) {
  object$vcov
}

confint.esther_result <- function(object, parm, level = 0.95, ...) {
  if (all(c("ci_low", "ci_high") %in% names(object$table))) {
    mat <- cbind(`2.5 %` = object$table$ci_low, `97.5 %` = object$table$ci_high)
    rownames(mat) <- names(stats::coef(object))
    return(mat)
  }
  NULL
}

print.esther_result <- function(x, digits = 4L, lang = "en", ...) {
  cat(to_markdown(x, lang = lang, digits = digits, include_strobe = TRUE), "\n")
  invisible(x)
}

summary.esther_result <- function(object, digits = 4L, lang = "en", ...) {
  cat(to_markdown(object, lang = lang, digits = digits, include_strobe = TRUE), "\n")
  invisible(object)
}

print.esther_decomp <- function(x, digits = 4L, lang = "en", ...) {
  cat(to_markdown(x, lang = lang, digits = digits, include_strobe = TRUE), "\n")
  invisible(x)
}

summary.esther_decomp <- function(object, digits = 4L, lang = "en", ...) {
  cat(to_markdown(object, lang = lang, digits = digits, include_strobe = TRUE), "\n")
  invisible(object)
}

print.esther_curve <- function(x, ...) {
  cat(sprintf("<esther_curve: %s (%d coordinate rows)>\n", x$title, nrow(x$ordinates)))
  print(utils::head(x$ordinates, 8L))
  invisible(x)
}

plot.esther_curve <- function(x, ...) {
  plot_curve(x, ...)
}

plot.esther_decomp <- function(x, ...) {
  plot_decomposition(x, ...)
}
