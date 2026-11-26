# Internal data coercion, numeric rank helpers, and pure-R ZIP writer for OpenXML (.xlsx / .docx)

.ensure_data_frame <- function(data) {
  if (inherits(data, "esther_survey") || inherits(data, "esther_grouped")) {
    return(data$df)
  }
  if (inherits(data, "survey.design") || inherits(data, "svyrep.design")) {
    return(as.data.frame(data$variables, stringsAsFactors = FALSE))
  }
  if (is.data.frame(data)) {
    return(as.data.frame(data, stringsAsFactors = FALSE))
  }
  if (is.list(data)) {
    return(as.data.frame(data, stringsAsFactors = FALSE))
  }
  stop("`data` must be a data.frame, survey_health_data, grouped_health_data, or survey.design object.", call. = FALSE)
}

.to_clean_numeric <- function(x, name = "variable") {
  if (is.logical(x)) {
    return(as.numeric(x))
  }
  if (is.numeric(x)) {
    return(as.double(x))
  }
  suppressWarnings(out <- as.double(as.character(x)))
  if (all(is.na(out)) && any(!is.na(x))) {
    stop(sprintf("Column `%s` cannot be coerced to numeric.", name), call. = FALSE)
  }
  out
}

.to_rank_numeric <- function(x, name = "rank") {
  if (is.factor(x)) {
    codes <- as.integer(x)
    out <- as.double(codes - 1L)
    out[is.na(codes)] <- NA_real_
    return(out)
  }
  if (is.logical(x) || is.numeric(x)) {
    return(as.double(x))
  }
  suppressWarnings(num <- as.double(as.character(x)))
  if (all(is.na(num)) && any(!is.na(x))) {
    f <- factor(x, levels = sort(unique(as.character(x[!is.na(x)]))), ordered = TRUE)
    out <- as.double(as.integer(f) - 1L)
    out[is.na(x)] <- NA_real_
    return(out)
  }
  num
}

.weighted_mean <- function(x, w) {
  sum(x * w) / sum(w)
}

# Pure base-R CRC32 computation and uncompressed PKZIP writer (zero external CLI/package deps!)
.crc32_table <- local({
  tbl <- numeric(256L)
  for (n in 0:255) {
    c_val <- as.double(n)
    for (k in 1:8) {
      if ((c_val %% 2) >= 1) {
        c_val <- bitwXor(as.integer(floor(c_val / 2)), -306674912L) # 0xEDB88320 signed 32-bit
        if (c_val < 0) c_val <- c_val + 4294967296
      } else {
        c_val <- floor(c_val / 2)
      }
    }
    tbl[n + 1L] <- c_val
  }
  tbl
})

.crc32_raw <- function(bytes) {
  crc <- 4294967295 # 0xFFFFFFFF
  if (length(bytes) > 0L) {
    ints <- as.integer(bytes)
    for (b in ints) {
      low8 <- as.integer(crc %% 256)
      idx <- bitwXor(low8, b) + 1L
      high24 <- floor(crc / 256)
      tval <- .crc32_table[idx]
      # 32-bit unsigned XOR via 16-bit halves to avoid signed 32-bit NA in R
      h_hi <- as.integer(floor(high24 / 65536))
      h_lo <- as.integer(high24 %% 65536)
      t_hi <- as.integer(floor(tval / 65536))
      t_lo <- as.integer(tval %% 65536)
      crc <- as.double(bitwXor(h_hi, t_hi)) * 65536 + as.double(bitwXor(h_lo, t_lo))
    }
  }
  h_hi <- as.integer(floor(crc / 65536))
  h_lo <- as.integer(crc %% 65536)
  as.double(bitwXor(h_hi, 65535L)) * 65536 + as.double(bitwXor(h_lo, 65535L))
}

.u16_le <- function(x) {
  v <- as.double(x) %% 65536
  as.raw(c(as.integer(v %% 256), as.integer(floor(v / 256) %% 256)))
}

.u32_le <- function(x) {
  v <- as.double(x) %% 4294967296
  b0 <- as.integer(v %% 256)
  v1 <- floor(v / 256)
  b1 <- as.integer(v1 %% 256)
  v2 <- floor(v1 / 256)
  b2 <- as.integer(v2 %% 256)
  b3 <- as.integer(floor(v2 / 256) %% 256)
  as.raw(c(b0, b1, b2, b3))
}

.write_zip_entries <- function(path, entries) {
  # entries: named list of character or raw vectors (names are archive paths)
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)

  central_records <- list()
  offset <- 0

  for (nm in names(entries)) {
    val <- entries[[nm]]
    data_raw <- if (is.raw(val)) val else charToRaw(enc2utf8(paste(val, collapse = "\n")))
    nm_raw <- charToRaw(enc2utf8(nm))
    crc <- .crc32_raw(data_raw)
    sz <- length(data_raw)

    local_hdr <- c(
      as.raw(c(0x50, 0x4b, 0x03, 0x04)), # local file header signature
      .u16_le(20),                       # version needed (2.0)
      .u16_le(0),                        # flags
      .u16_le(0),                        # compression = 0 (store)
      .u16_le(0), .u16_le(0x0021),       # mod time / date
      .u32_le(crc),
      .u32_le(sz),
      .u32_le(sz),
      .u16_le(length(nm_raw)),
      .u16_le(0),
      nm_raw
    )
    writeBin(local_hdr, con)
    if (sz > 0L) writeBin(data_raw, con)

    central_hdr <- c(
      as.raw(c(0x50, 0x4b, 0x01, 0x02)), # central file header signature
      .u16_le(20),                       # version made by
      .u16_le(20),                       # version needed
      .u16_le(0),
      .u16_le(0),
      .u16_le(0), .u16_le(0x0021),
      .u32_le(crc),
      .u32_le(sz),
      .u32_le(sz),
      .u16_le(length(nm_raw)),
      .u16_le(0),
      .u16_le(0),
      .u16_le(0),
      .u16_le(0),
      .u32_le(0),
      .u32_le(offset),
      nm_raw
    )
    central_records[[length(central_records) + 1L]] <- central_hdr
    offset <- offset + length(local_hdr) + sz
  }

  central_blob <- unlist(central_records, use.names = FALSE)
  if (length(central_blob) > 0L) writeBin(central_blob, con)

  eocd <- c(
    as.raw(c(0x50, 0x4b, 0x05, 0x06)),
    .u16_le(0),
    .u16_le(0),
    .u16_le(length(entries)),
    .u16_le(length(entries)),
    .u32_le(length(central_blob)),
    .u32_le(offset),
    .u16_le(0)
  )
  writeBin(eocd, con)
  invisible(normalizePath(path, winslash = "/", mustWork = FALSE))
}
