# Publication Exporters: Markdown, LaTeX (booktabs), HTML, and Pure-R OpenXML Excel (.xlsx) & Word (.docx)

.localize_table <- function(df, lang = "en") {
  out <- df
  if ("interpretation" %in% names(out) && lang != "en") {
    map_i <- c(
      "Concentrated among more advantaged groups" = .tr("pro_advantaged", lang),
      "Concentrated among more disadvantaged groups" = .tr("pro_disadvantaged", lang),
      "No statistically clear socioeconomic gradient" = .tr("near_equality", lang)
    )
    hit <- out$interpretation %in% names(map_i)
    out$interpretation[hit] <- unname(map_i[out$interpretation[hit]])
  }
  known_cols <- c(
    "measure", "domain", "estimate", "se", "ci_low", "ci_high", "cv_pct",
    "p_value", "n_obs", "sum_weights", "deff", "interpretation",
    "factor", "elasticity", "factor_ci", "contribution", "pct_share",
    "explained", "unexplained", "total_diff", "residual"
  )
  new_nms <- vapply(names(out), function(cn) {
    if (cn %in% known_cols) .tr(cn, lang) else cn
  }, character(1L))
  names(out) <- new_nms
  out
}

.format_cell_df <- function(df, digits = 4L) {
  out <- df
  int_hdrs <- c("Sample n", "n muestral", "n amostral")
  pop_hdrs <- c("Weighted population", "Poblaci\u00f3n ponderada", "Popula\u00e7\u00e3o ponderada")
  for (cn in names(out)) {
    col <- out[[cn]]
    if (is.numeric(col)) {
      if (cn %in% int_hdrs) {
        out[[cn]] <- ifelse(is.finite(col), format(round(col), big.mark = ",", trim = TRUE), "\u2014")
      } else if (cn %in% pop_hdrs) {
        out[[cn]] <- ifelse(is.finite(col), format(round(col, 1), nsmall = 1, big.mark = ",", trim = TRUE), "\u2014")
      } else {
        out[[cn]] <- ifelse(is.finite(col), sprintf(paste0("%.", digits, "f"), col), "\u2014")
      }
    } else {
      out[[cn]] <- ifelse(is.na(col), "\u2014", as.character(col))
    }
  }
  out
}

.df_to_md_grid <- function(fmt_df) {
  headers <- names(fmt_df)
  widths <- vapply(seq_along(headers), function(j) {
    max(nchar(headers[j]), max(nchar(fmt_df[[j]]), 0L))
  }, integer(1L))

  hdr_line <- paste0("| ", paste(mapply(function(h, w) format(h, width = w), headers, widths), collapse = " | "), " |")
  sep_line <- paste0("| ", paste(vapply(widths, function(w) paste(rep("-", w), collapse = ""), character(1L)), collapse = " | "), " |")
  body_lines <- vapply(seq_len(nrow(fmt_df)), function(i) {
    cells <- mapply(function(val, w) format(val, width = w), as.character(fmt_df[i, ]), widths)
    paste0("| ", paste(cells, collapse = " | "), " |")
  }, character(1L))
  paste(c(hdr_line, sep_line, body_lines), collapse = "\n")
}

.omnibus_line <- function(obj, lang = "en") {
  om <- obj$omnibus_test
  if (is.null(om)) return("")
  sprintf("%s: F(%d, %.0f) = %.4f (p = %.5f).", .tr("omnibus_test", lang), om$df1, om$df2, om$stat, om$p_value)
}

to_markdown <- function(object, lang = "en", digits = 4L, include_strobe = TRUE) {
  if (inherits(object, "esther_decomp")) {
    ov_fmt <- .format_cell_df(.localize_table(object$overall_summary, lang), digits)
    co_fmt <- .format_cell_df(.localize_table(object$contributions, lang), digits)
    blocks <- c(
      sprintf("### %s \u2014 overview\n\n%s", object$title, .df_to_md_grid(ov_fmt)),
      sprintf("### %s \u2014 variables\n\n%s", object$title, .df_to_md_grid(co_fmt))
    )
    if (isTRUE(include_strobe)) {
      blocks <- c(blocks, paste0("*", strobe_equity_note(object, lang = lang), "*"))
    }
    return(paste(blocks, collapse = "\n\n"))
  }

  tbl_fmt <- .format_cell_df(.localize_table(object$table, lang), digits)
  blocks <- character(0)
  if (nzchar(object$title)) blocks <- c(blocks, paste0("### ", object$title))
  blocks <- c(blocks, .df_to_md_grid(tbl_fmt))

  foot <- character(0)
  om_str <- .omnibus_line(object, lang)
  if (nzchar(om_str)) foot <- c(foot, om_str)
  if (isTRUE(include_strobe)) foot <- c(foot, strobe_equity_note(object, lang = lang))
  if (length(foot) > 0L) blocks <- c(blocks, paste0("*", paste(foot, collapse = "\n"), "*"))
  paste(blocks, collapse = "\n\n")
}

.esc_latex <- function(s) {
  s <- gsub("\\\\", "\\\\textbackslash{}", s)
  s <- gsub("&", "\\\\&", s)
  s <- gsub("%", "\\\\%", s)
  s <- gsub("_", "\\\\_", s)
  s <- gsub("#", "\\\\#", s)
  s
}

to_latex <- function(object, lang = "en", digits = 4L, caption = NULL, label = NULL) {
  raw_df <- if (inherits(object, "esther_decomp")) object$contributions else object$table
  loc_df <- .localize_table(raw_df, lang)
  fmt_df <- .format_cell_df(loc_df, digits)
  align <- paste(vapply(loc_df, function(col) if (is.numeric(col)) "r" else "l", character(1L)), collapse = "")
  cap <- if (!is.null(caption)) caption else object$title

  lines <- c("\\begin{table}[htbp]", "\\centering")
  if (nzchar(cap)) lines <- c(lines, sprintf("\\caption{%s}", .esc_latex(cap)))
  if (!is.null(label) && nzchar(label)) lines <- c(lines, sprintf("\\label{%s}", label))
  lines <- c(
    lines,
    sprintf("\\begin{tabular}{%s}", align),
    "\\toprule",
    paste0(paste(.esc_latex(names(fmt_df)), collapse = " & "), " \\\\"),
    "\\midrule"
  )
  for (i in seq_len(nrow(fmt_df))) {
    row_str <- paste(.esc_latex(as.character(fmt_df[i, ])), collapse = " & ")
    lines <- c(lines, paste0(row_str, " \\\\"))
  }
  lines <- c(lines, "\\bottomrule", "\\end{tabular}", "\\end{table}")
  paste(lines, collapse = "\n")
}

.esc_xml <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  s <- gsub(">", "&gt;", s, fixed = TRUE)
  s <- gsub("\"", "&quot;", s, fixed = TRUE)
  s <- gsub("'", "&apos;", s, fixed = TRUE)
  s
}

to_html <- function(object, lang = "en", digits = 4L, include_strobe = TRUE) {
  raw_df <- if (inherits(object, "esther_decomp")) object$contributions else object$table
  fmt_df <- .format_cell_df(.localize_table(raw_df, lang), digits)
  ths <- paste(sprintf("<th style='padding:6px 10px;border-bottom:2px solid #334155;text-align:left;'>%s</th>", .esc_xml(names(fmt_df))), collapse = "")
  trs <- vapply(seq_len(nrow(fmt_df)), function(i) {
    tds <- paste(sprintf("<td style='padding:5px 10px;border-bottom:1px solid #e2e8f0;'>%s</td>", .esc_xml(as.character(fmt_df[i, ]))), collapse = "")
    paste0("<tr>", tds, "</tr>")
  }, character(1L))
  foot <- if (isTRUE(include_strobe)) {
    sprintf("<p style='font-size:0.85em;color:#475569;margin-top:8px;'>%s</p>", .esc_xml(strobe_equity_note(object, lang)))
  } else ""
  sprintf(
    "<div class='esther-ineq-report'><h4 style='margin-bottom:6px;'>%s</h4><table style='border-collapse:collapse;font-family:sans-serif;font-size:0.92em;'><thead><tr>%s</tr></thead><tbody>%s</tbody></table>%s</div>",
    .esc_xml(object$title), ths, paste(trs, collapse = ""), foot
  )
}

.col_letter <- function(idx) {
  out <- ""
  n <- as.integer(idx)
  while (n > 0L) {
    rem <- (n - 1L) %% 26L
    out <- paste0(LETTERS[rem + 1L], out)
    n <- (n - 1L) %/% 26L
  }
  out
}

to_excel <- function(object, path, lang = "en", include_strobe = TRUE) {
  raw_df <- if (inherits(object, "esther_decomp")) object$contributions else object$table
  loc_df <- .localize_table(raw_df, lang)
  note_txt <- if (isTRUE(include_strobe)) strobe_equity_note(object, lang) else ""

  rows_xml <- character(0)
  # Row 1: Title
  rows_xml <- c(rows_xml, sprintf("<row r=\"1\"><c r=\"A1\" t=\"inlineStr\"><is><t>%s</t></is></c></row>", .esc_xml(object$title)))
  # Row 3: Column headers
  hdr_cells <- vapply(seq_along(names(loc_df)), function(j) {
    sprintf("<c r=\"%s3\" t=\"inlineStr\"><is><t>%s</t></is></c>", .col_letter(j), .esc_xml(names(loc_df)[j]))
  }, character(1L))
  rows_xml <- c(rows_xml, paste0("<row r=\"3\">", paste(hdr_cells, collapse = ""), "</row>"))

  # Data rows starting at row 4
  for (i in seq_len(nrow(loc_df))) {
    r_num <- i + 3L
    cells <- vapply(seq_along(loc_df), function(j) {
      ref <- paste0(.col_letter(j), r_num)
      val <- loc_df[[j]][i]
      if (is.numeric(val) && is.finite(val)) {
        sprintf("<c r=\"%s\"><v>%.10g</v></c>", ref, val)
      } else {
        txt <- if (is.na(val)) "" else as.character(val)
        sprintf("<c r=\"%s\" t=\"inlineStr\"><is><t>%s</t></is></c>", ref, .esc_xml(txt))
      }
    }, character(1L))
    rows_xml <- c(rows_xml, sprintf("<row r=\"%d\">%s</row>", r_num, paste(cells, collapse = "")))
  }

  if (nzchar(note_txt)) {
    fn_r <- nrow(loc_df) + 5L
    rows_xml <- c(rows_xml, sprintf("<row r=\"%d\"><c r=\"A%d\" t=\"inlineStr\"><is><t>%s</t></is></c></row>", fn_r, fn_r, .esc_xml(note_txt)))
  }

  sheet_xml <- paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
    "<worksheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\"><sheetData>",
    paste(rows_xml, collapse = ""),
    "</sheetData></worksheet>"
  )
  wb_xml <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><workbook xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\"><sheets><sheet name=\"Inequality_Estimates\" sheetId=\"1\" r:id=\"rId1\"/></sheets></workbook>"
  wb_rels <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet\" Target=\"worksheets/sheet1.xml\"/></Relationships>"
  root_rels <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument\" Target=\"xl/workbook.xml\"/></Relationships>"
  ctypes <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\"><Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/><Default Extension=\"xml\" ContentType=\"application/xml\"/><Override PartName=\"/xl/workbook.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml\"/><Override PartName=\"/xl/worksheets/sheet1.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml\"/></Types>"

  .write_zip_entries(path, list(
    `[Content_Types].xml` = ctypes,
    `_rels/.rels` = root_rels,
    `xl/workbook.xml` = wb_xml,
    `xl/_rels/workbook.xml.rels` = wb_rels,
    `xl/worksheets/sheet1.xml` = sheet_xml
  ))
}

to_word <- function(object, path, lang = "en", digits = 4L, include_strobe = TRUE) {
  raw_df <- if (inherits(object, "esther_decomp")) object$contributions else object$table
  fmt_df <- .format_cell_df(.localize_table(raw_df, lang), digits)
  note_txt <- if (isTRUE(include_strobe)) strobe_equity_note(object, lang) else ""

  hdr_cells <- paste(sprintf(
    "<w:tc><w:p><w:r><w:rPr><w:b/></w:rPr><w:t>%s</w:t></w:r></w:p></w:tc>",
    .esc_xml(names(fmt_df))
  ), collapse = "")
  body_rows <- vapply(seq_len(nrow(fmt_df)), function(i) {
    tcs <- paste(sprintf("<w:tc><w:p><w:r><w:t>%s</w:t></w:r></w:p></w:tc>", .esc_xml(as.character(fmt_df[i, ]))), collapse = "")
    paste0("<w:tr>", tcs, "</w:tr>")
  }, character(1L))

  doc_xml <- paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
    "<w:document xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\"><w:body>",
    sprintf("<w:p><w:r><w:rPr><w:b/><w:sz w:val=\"26\"/></w:rPr><w:t>%s</w:t></w:r></w:p>", .esc_xml(object$title)),
    "<w:tbl><w:tblPr><w:tblBorders><w:top w:val=\"single\" w:sz=\"8\" w:color=\"334155\"/><w:bottom w:val=\"single\" w:sz=\"8\" w:color=\"334155\"/></w:tblBorders></w:tblPr>",
    "<w:tr>", hdr_cells, "</w:tr>",
    paste(body_rows, collapse = ""),
    "</w:tbl>",
    if (nzchar(note_txt)) sprintf("<w:p><w:r><w:rPr><w:i/><w:sz w:val=\"18\"/></w:rPr><w:t>%s</w:t></w:r></w:p>", .esc_xml(note_txt)) else "",
    "</w:body></w:document>"
  )
  ctypes <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\"><Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/><Default Extension=\"xml\" ContentType=\"application/xml\"/><Override PartName=\"/word/document.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml\"/></Types>"
  rels <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument\" Target=\"word/document.xml\"/></Relationships>"

  .write_zip_entries(path, list(
    `[Content_Types].xml` = ctypes,
    `_rels/.rels` = rels,
    `word/document.xml` = doc_xml
  ))
}
