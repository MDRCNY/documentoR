# R/file_handlers.R
# File handlers for documentoR
# Provides per-file-type extraction logic that returns safe text snippets
# to include in LLM prompts.

#' Detect whether a file appears binary (contains NUL bytes in the first chunk)
#'
#' @param path Path to file
#' @param n Number of bytes to inspect (default 1024)
#' @return Logical scalar
is_likely_binary <- function(path, n = 1024) {
  if (!file.exists(path)) return(FALSE)
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  bytes <- tryCatch(readBin(con, what = "raw", n = n), error = function(e) raw(0))
  length(bytes) > 0 && any(bytes == as.raw(0))
}

#' Safe file metadata
#' @noRd
file_meta <- function(path) {
  info <- tryCatch(fs::file_info(path), error = function(e) NULL)
  list(size = if (!is.null(info)) as.numeric(info$size) else NA,
       mtime = if (!is.null(info)) as.character(info$modification_time) else NA)
}

#' Truncate text to max_chars with marker
#' @noRd
truncate_text <- function(txt, max_chars = 16000) {
  if (is.null(txt)) return("")
  if (!is.character(txt)) txt <- as.character(txt)
  if (nchar(txt, type = "chars") > max_chars) {
    paste0(substr(txt, 1, max_chars), "\n\n--- TRUNCATED ---")
  } else {
    txt
  }
}

# -------------------------
# Text handler (generic)
# -------------------------
#' Handle plain text-like files (R, Rmd, qmd, md, csv, sql, json, yaml, etc.)
#'
#' @param path file path
#' @param max_chars integer truncate length
#' @return list(type, content, meta, raw_preview)
#' @export
handle_text_file <- function(path, max_chars = 16000) {
  if (is_likely_binary(path)) {
    return(handle_binary_file(path, max_chars = 1000))
  }
  content <- tryCatch(readr::read_file(path), error = function(e) paste0("<<UNREADABLE: ", conditionMessage(e), ">>"))
  content_trunc <- truncate_text(content, max_chars = max_chars)
  list(
    type = "text",
    content = content_trunc,
    meta = c(file_meta(path)),
    raw_preview = substr(content_trunc, 1, min(nchar(content_trunc), 500))
  )
}

# -------------------------
# HTML handler
# -------------------------
#' Handle HTML files (extract visible text)
#' @export
handle_html <- function(path, max_chars = 16000) {
  if (is_likely_binary(path)) return(handle_binary_file(path, max_chars = 1000))
  if (!requireNamespace("xml2", quietly = TRUE) || !requireNamespace("rvest", quietly = TRUE)) {
    # fallback: return raw file (truncated)
    content <- tryCatch(readr::read_file(path), error = function(e) paste0("<<UNREADABLE HTML: ", conditionMessage(e), ">>"))
    return(list(type = "html", content = truncate_text(content, max_chars = max_chars), meta = file_meta(path), raw_preview = substr(content, 1, 500)))
  }
  txt <- tryCatch({
    doc <- xml2::read_html(path)
    # extract visible text, collapse spaces
    body <- rvest::html_nodes(doc, "body")
    if (length(body) == 0) body_text <- rvest::html_text(doc) else body_text <- rvest::html_text(body)
    gsub("[ \t\r\n]+", " ", body_text)
  }, error = function(e) paste0("<<UNREADABLE HTML: ", conditionMessage(e), ">>"))
  txt <- truncate_text(txt, max_chars = max_chars)
  list(type = "html", content = txt, meta = c(file_meta(path)), raw_preview = substr(txt, 1, 500))
}

# -------------------------
# PDF handler
# -------------------------
#' Handle PDF files using pdftools (if available)
#' @export
handle_pdf <- function(path, max_chars = 16000) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    return(list(type = "pdf", content = paste0("<<PDF: pdftools not installed; cannot extract text from ", basename(path), ">>"), meta = file_meta(path), raw_preview = ""))
  }
  txt_pages <- tryCatch(pdftools::pdf_text(path), error = function(e) { warning("pdftools::pdf_text failed: ", conditionMessage(e)); character(0) })
  txt <- paste(txt_pages, collapse = "\n\n")
  txt_trunc <- truncate_text(txt, max_chars = max_chars)
  list(type = "pdf", content = txt_trunc, meta = c(file_meta(path), list(pages = length(txt_pages))), raw_preview = substr(txt_trunc, 1, 500))
}

# -------------------------
# DOCX handler
# -------------------------
#' Handle .docx (requires textreadr or fallback to unreadable placeholder)
#' @export
handle_docx <- function(path, max_chars = 16000) {
  if (is_likely_binary(path)) return(handle_binary_file(path, max_chars = 1000))
  if (!requireNamespace("textreadr", quietly = TRUE)) {
    return(list(type = "docx", content = paste0("<<DOCX: textreadr not installed; cannot extract text from ", basename(path), ">>"), meta = file_meta(path), raw_preview = ""))
  }
  txt <- tryCatch({
    # textreadr::read_docx returns a character vector of paragraphs
    pieces <- textreadr::read_docx(path)
    paste(pieces, collapse = "\n\n")
  }, error = function(e) paste0("<<UNREADABLE DOCX: ", conditionMessage(e), ">>"))
  txt <- truncate_text(txt, max_chars = max_chars)
  list(type = "docx", content = txt, meta = c(file_meta(path)), raw_preview = substr(txt, 1, 500))
}

# -------------------------
# Jupyter notebook handler (.ipynb)
# -------------------------
#' Handle Jupyter notebooks (.ipynb) by extracting code + markdown cell sources
#' @export
handle_ipynb <- function(path, max_chars = 16000) {
  if (is_likely_binary(path)) return(handle_binary_file(path, max_chars = 1000))
  content_raw <- tryCatch(readr::read_file(path), error = function(e) NULL)
  if (is.null(content_raw)) return(list(type = "ipynb", content = "<<UNREADABLE ipynb>>", meta = file_meta(path), raw_preview = ""))
  j <- tryCatch(jsonlite::fromJSON(content_raw, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(j) || is.null(j$cells)) {
    # fallback: return raw truncated file
    return(list(type = "ipynb", content = truncate_text(content_raw, max_chars = max_chars), meta = file_meta(path), raw_preview = substr(content_raw, 1, 500)))
  }
  # Extract text from code and markdown cells, ignore outputs
  pieces <- vapply(j$cells, FUN = function(cell) {
    if (!is.null(cell$cell_type) && cell$cell_type %in% c("markdown", "code")) {
      src <- cell$source
      if (is.null(src)) return("") 
      # cell$source may be vector of lines
      paste(unlist(src), collapse = "")
    } else {
      ""
    }
  }, FUN.VALUE = character(1))
  txt <- paste(pieces[pieces != ""], collapse = "\n\n")
  txt <- truncate_text(txt, max_chars = max_chars)
  list(type = "ipynb", content = txt, meta = c(file_meta(path), list(n_cells = length(j$cells))), raw_preview = substr(txt, 1, 500))
}

# -------------------------
# RData / rda handler
# -------------------------
#' Inspect .RData/.rda files and return object summary (do not attach to global env)
#' @export
handle_rdata <- function(path, max_chars = 2000) {
  # Avoid loading into global env. Use new env.
  env <- new.env(parent = emptyenv())
  res <- tryCatch({
    load(path, envir = env)
    objs <- ls(envir = env)
    meta_info <- lapply(objs, function(nm) {
      obj <- get(nm, envir = env)
      list(class = paste0(class(obj), collapse = ","), size = as.numeric(object.size(obj)))
    })
    content <- paste0("RData file containing objects: ", paste(objs, collapse = ", "), ".")
    list(type = "rdata", content = truncate_text(content, max_chars = max_chars), meta = c(file_meta(path), list(objects = meta_info)), raw_preview = substr(content, 1, 400))
  }, error = function(e) {
    list(type = "rdata", content = paste0("<<UNREADABLE RData: ", conditionMessage(e), ">>"), meta = file_meta(path), raw_preview = "")
  })
  res
}

# -------------------------
# rds handler
# -------------------------
#' Inspect .rds by reading object metadata (class, length) but avoid printing full object
#' @export
handle_rds <- function(path, max_chars = 2000) {
  res <- tryCatch({
    obj <- readRDS(path)
    cl <- if (!is.null(obj)) class(obj)[1] else "unknown"
    size_bytes <- as.numeric(object.size(obj))
    desc <- paste0("rds object: class=", cl, ", size=", size_bytes)
    list(type = "rds", content = truncate_text(desc, max_chars = max_chars), meta = c(file_meta(path), list(object_class = cl, object_size = size_bytes)), raw_preview = substr(desc, 1, 400))
  }, error = function(e) {
    list(type = "rds", content = paste0("<<UNREADABLE rds: ", conditionMessage(e), ">>"), meta = file_meta(path), raw_preview = "")
  })
  res
}

# -------------------------
# Image handler (jpg/png/gif)
# -------------------------
#' Handle images by returning metadata only (avoid sending binary content)
#' @export
handle_image <- function(path, max_chars = 1000) {
  info <- file_meta(path)
  # try to get dimensions via magick if available
  dims <- tryCatch({
    if (requireNamespace("magick", quietly = TRUE)) {
      img <- magick::image_read(path)
      magick::image_info(img)[c("width","height")]
    } else NULL
  }, error = function(e) NULL)
  content <- paste0("IMAGE FILE: ", basename(path), " | size=", info$size, if (!is.null(dims)) paste0(" | width=", dims$width, " height=", dims$height) else "", ".")
  list(type = "image", content = content, meta = c(info, list(dimensions = dims)), raw_preview = content)
}

# -------------------------
# Generic binary fallback
# -------------------------
#' Handle unknown or binary files: return metadata-only placeholder
#' @export
handle_binary_file <- function(path, max_chars = 1000) {
  info <- file_meta(path)
  content <- paste0("<<BINARY FILE: ", basename(path), " | size=", info$size, " | mtime=", info$mtime, ">>\n\n(Contents omitted.)")
  list(type = "binary", content = content, meta = info, raw_preview = substr(content, 1, 400))
}

# -------------------------
# Registry & chooser
# -------------------------
#' Registry of file handlers (extension -> function)
#' Add or override entries as needed.
#' @export
FILE_HANDLERS <- list(
  # text-like
  "r"     = handle_text_file,
  "rmd"   = handle_text_file,
  "qmd"   = handle_text_file,
  "md"    = handle_text_file,
  "csv"   = handle_text_file,
  "sql"   = handle_text_file,
  "json"  = handle_text_file,
  "yml"   = handle_text_file,
  "yaml"  = handle_text_file,
  "R"     = handle_text_file,
  # markup / web
  "html"  = handle_html,
  "htm"   = handle_html,
  # notebooks, docs
  "ipynb" = handle_ipynb,
  "pdf"   = handle_pdf,
  "docx"  = handle_docx,
  # R binary formats
  "rds"   = handle_rds,
  "rda"   = handle_rdata,
  "rdata" = handle_rdata,
  # images
  "png"   = handle_image,
  "jpg"   = handle_image,
  "jpeg"  = handle_image,
  "gif"   = handle_image
)

#' Choose an appropriate handler for a file path
#'
#' @param path File path
#' @return A function(path, max_chars, ...) -> list(...) (the handler)
#' @export
choose_handler_for_file <- function(path) {
  if (is.null(path) || !nzchar(path)) return(handle_binary_file)
  ext <- tolower(fs::path_ext(path))
  if (ext == "" && grepl("\\.RData$", basename(path), ignore.case = TRUE)) ext <- "rdata"
  h <- FILE_HANDLERS[[ext]]
  if (is.null(h)) {
    # if file is likely binary, return binary handler, otherwise text fallback
    if (is_likely_binary(path)) return(handle_binary_file)
    return(handle_text_file)
  }
  h
}
