# R/documentoR.R
# documentoR - orchestration: scanning, summarizing (via handlers), indexing, single-file output.
#
# Dependencies (suggested): fs, purrr, tibble, dplyr, readr, stringr, jsonlite, glue
# Optional handler packages: xml2, rvest, pdftools, textreadr, magick
#
# NOTE: This file will call choose_handler_for_file(file) if implemented in R/file_handlers.R.
# The implementation below falls back to plain text reading if no handler exists.

# ----------------------
# small helpers / operators
# ----------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Fallback file_meta in case handlers file isn't loaded yet
file_meta <- function(path) {
  info <- tryCatch(fs::file_info(path), error = function(e) NULL)
  list(size = if (!is.null(info)) as.numeric(info$size) else NA,
       mtime = if (!is.null(info)) as.character(info$modification_time) else NA)
}

# Robust SUMMARY extractor (used for README lines)
extract_summary_section <- function(md_text) {
  if (is.null(md_text) || md_text == "") return("")
  anchors <- c("KEY_ITEMS","FUNCTIONS/EXPORTS","DEPENDENCIES/MIGHT_USE","TODO / WARNINGS","SUGGESTED_TESTS or EXAMPLES","SHORT_RECOMMENDATION")
  anchors_escaped <- gsub("/", "\\\\/", anchors)
  anchor_alt <- paste0("(?=\\n(?:", paste0(anchors_escaped, collapse = "|"), ")\\b)|$")
  pattern <- paste0("(?s)SUMMARY\\s*\\n\\s*(.*?)", anchor_alt)
  m <- regexpr(pattern, md_text, perl = TRUE)
  if (m[1] != -1L) {
    match_text <- regmatches(md_text, m)[[1]]
    cap <- sub(pattern, "\\1", match_text, perl = TRUE)
    cap <- trimws(cap)
    if (nzchar(cap)) return(cap)
  }
  lines <- strsplit(md_text, "\n", fixed = TRUE)[[1]]
  lines_trim <- trimws(lines)
  idx <- which(toupper(lines_trim) == "SUMMARY")
  if (length(idx) > 0 && idx + 1 <= length(lines_trim)) {
    nxt <- lines_trim[idx + 1]
    if (nzchar(nxt)) return(nxt)
  }
  nonempty <- lines_trim[lines_trim != ""]
  if (length(nonempty) > 0) return(nonempty[1])
  ""
}

# ----------------------
# Exclude file parsing
# ----------------------
read_exclude_file <- function(path) {
  if (is.null(path) || !file.exists(path)) return(list(dirs = character(0), patterns = character(0)))
  lines <- readr::read_lines(path, skip_empty_rows = TRUE)
  lines <- trimws(gsub("#.*$", "", lines))
  lines <- lines[lines != ""]
  dirs <- character(0); patterns <- character(0)
  for (ln in lines) {
    if (grepl("/$", ln)) {
      dirs <- c(dirs, gsub("/$", "", ln))
    } else if (!grepl("[*?\\[\\]|()\\\\]", ln) && !grepl("\\.", ln) && !grepl("/", ln)) {
      dirs <- c(dirs, ln)
    } else {
      patterns <- c(patterns, glob_to_regex(ln))
    }
  }
  list(dirs = unique(dirs), patterns = unique(patterns))
}

glob_to_regex <- function(glob) {
  if (grepl("^\\^|\\\\|\\(|\\)$", glob)) return(glob)
  esc <- gsub("([.|^$+{}\\[\\]\\\\()])", "\\\\\\1", glob, perl = TRUE)
  esc <- gsub("\\*", ".*", esc, perl = TRUE)
  esc <- gsub("\\?", ".", esc, perl = TRUE)
  paste0("^", esc, "$")
}

# ----------------------
# Scan project files (honoring excludes)
# ----------------------
#' Scan project for files to document (honoring excludes)
#' @export
scan_project_files <- function(path = ".",
                               include = c("R","r","Rmd","rmd","qmd","qmarkdown","md","markdown","html","htm","ipynb","pdf","docx","doc","pptx","rds","rda","rdata","csv","sql","json","yml","yaml"),
                               exclude_dirs = c("packrat", "renv", "node_modules", ".git", "demo"),
                               exclude_patterns = c("\\.Rproj$", "^README\\.md$")) {
  root <- fs::path_real(path)
  patterns <- paste0("\\.(", paste0(tolower(include), collapse = "|"), ")$")
  all_files <- fs::dir_ls(root, recurse = TRUE, type = "file", all = FALSE)
  # exclude directories by name anywhere in path
  if (length(exclude_dirs) > 0) {
    excl_dirs_pattern <- paste0("/", exclude_dirs, collapse = "|")
    all_files <- all_files[!grepl(excl_dirs_pattern, as.character(all_files))]
  }
  # exclude by regex patterns
  if (length(exclude_patterns) > 0) {
    pat_combined <- paste0("(", paste0(exclude_patterns, collapse = "|"), ")")
    all_files <- all_files[!grepl(pat_combined, as.character(all_files))]
  }
  # match extensions ignoring case
  hits <- all_files[grepl(patterns, tolower(as.character(all_files)))]
  if (length(hits) == 0) {
    return(tibble::tibble(file = character(0), size = numeric(0), mtime = character(0)))
  }
  df <- tibble::tibble(
    file = as.character(hits),
    size = fs::file_info(hits)$size,
    mtime = as.character(fs::file_info(hits)$modification_time)
  )
  dplyr::arrange(df, dplyr::desc(size))
}

# ----------------------
# Fallback read_file_snippet (only used if handlers not available)
# ----------------------
#' Read a file safely into a truncated string
#' @export
read_file_snippet <- function(file, max_chars = 16000) {
  tryCatch({
    txt <- readr::read_file(file)
    if (nchar(txt) > max_chars) paste0(substr(txt, 1, max_chars), "\n\n--- TRUNCATED ---") else txt
  }, error = function(e) {
    paste0("<<UNREADABLE: ", conditionMessage(e), ">>")
  })
}

# ----------------------
# Prompt template
# ----------------------
#' Default prompt template for per-file summarization
#' @export
file_summary_prompt <- function(file_path, file_content, meta = list()) {
  glue::glue(
    "\
You are an experienced R developer and documentation assistant.

Task: produce a concise, actionable summary of the following file. Format the answer as Markdown with the sections exactly: SUMMARY, KEY_ITEMS, FUNCTIONS/EXPORTS (if any), DEPENDENCIES/MIGHT_USE, TODO / WARNINGS, SUGGESTED_TESTS or EXAMPLES, and SHORT_RECOMMENDATION (1-2 lines).

File path: {file_path}
File size (bytes): {meta$size}
Last modified: {meta$mtime}

----- FILE CONTENT START -----
{file_content}
----- FILE CONTENT END -----

Rules:
- SUMMARY: 2-6 sentences describing purpose.
- KEY_ITEMS: bullet list of important lines: exported function names, dataset names, SQL tables, major code blocks.
- FUNCTIONS/EXPORTS: list functions with one-liner description if obvious.
- DEPENDENCIES/MIGHT_USE: list R packages, external files or system tools referenced.
- TODO / WARNINGS: point out possible issues (unhandled errors, TODO comments, tests missing).
- SUGGESTED_TESTS or EXAMPLES: give 1-3 concrete test ideas or example usage.
- SHORT_RECOMMENDATION: 1-2 lines what author should do next.

Be concise and factual. Use code backticks where appropriate.
")
}

# ----------------------
# Summarize a single file using the handler + chat_fn
# ----------------------
#' Summarize a single file using a user-supplied chat function and file handler
#' @export
summarize_file <- function(file, chat_fn, max_chars = 16000, verbose = TRUE) {
  # Prefer the handler from file_handlers.R if available
  handler <- tryCatch(choose_handler_for_file(file), error = function(e) NULL)
  if (!is.null(handler) && is.function(handler)) {
    handled <- tryCatch(handler(file, max_chars = max_chars), error = function(e) list(content = read_file_snippet(file, max_chars = max_chars), meta = file_meta(file)))
    content <- handled$content %||% ""
    meta <- handled$meta %||% file_meta(file)
  } else {
    # fallback to naive read
    content <- read_file_snippet(file, max_chars = max_chars)
    meta <- file_meta(file)
  }
  prompt <- file_summary_prompt(file_path = as.character(file), file_content = content, meta = meta)
  if (verbose) message("Summarizing: ", as.character(file))
  res <- tryCatch(chat_fn(prompt), error = function(e) paste0("<<CHAT ERROR: ", conditionMessage(e), ">>"))
  if (!is.character(res)) res <- as.character(res)
  list(file = as.character(file), summary = res, meta = meta)
}

# ----------------------
# README creation (uses SUMMARY extraction)
# ----------------------
#' Create README.md content (use SUMMARY section)
#' @export
create_readme_text_fixed <- function(project_path, project_summary = NULL, file_summaries = list(), readme_title = NULL) {
  project_name <- if (!is.null(readme_title)) readme_title else fs::path_file(fs::path_norm(project_path))
  high <- project_summary %||% paste0("Auto-generated documentation for `", project_name, "`.")
  outline <- character(0)
  if (length(file_summaries) > 0) {
    outline <- vapply(file_summaries, FUN.VALUE = character(1), FUN = function(x) {
      s <- x$summary %||% ""
      sum_txt <- extract_summary_section(s)
      first_line <- ""
      if (nzchar(sum_txt)) {
        first_line <- strsplit(sum_txt, "\n", fixed = TRUE)[[1]][1]
        first_line <- trimws(first_line)
      }
      if (!nzchar(first_line)) {
        all_lines <- trimws(strsplit(s, "\n", fixed = TRUE)[[1]])
        nonempty <- all_lines[all_lines != ""]
        first_line <- ifelse(length(nonempty) > 0, nonempty[1], "")
      }
      paste0("- `", fs::path_rel(x$file, start = project_path), "` — ", first_line)
    })
  }
  readme <- c(
    glue::glue("# {project_name}"), "",
    glue::glue("> {high}"), "",
    "## Quick facts", "",
    glue::glue("- Project path: `{project_path}`"),
    glue::glue("- Files documented: {length(file_summaries)}"), "",
    "## Folder / File summaries (auto-generated)", "",
    outline, "",
    "## How to use this project", "",
    "1. Installation (if an R package): `devtools::install()` or `remotes::install_local()`",
    "2. Example usage: see `examples/` or `vignettes/` (if present).", "",
    "## Notes / next steps", "",
    "- Please review the per-file summaries in `documentation/` for more details.", "",
    "## License & Authors", "",
    "- Add license and author information here."
  )
  paste(readme, collapse = "\n")
}

# ----------------------
# Create single combined markdown
# ----------------------
#' Create a single combined documentation markdown file
#' @export
create_single_documentation <- function(summaries, output_file = "documentation/documentation.md", include_frontmatter = TRUE) {
  if (is.null(summaries) || length(summaries) == 0) stop("No summaries provided")
  out_lines <- c("<!-- Auto-generated by documentoR -->", paste0("Generated: ", as.character(Sys.time())), "")
  for (s in summaries) {
    file_path <- s$file %||% ""
    meta <- s$meta %||% list()
    header <- paste0("## File: ", file_path)
    out_lines <- c(out_lines, header)
    if (include_frontmatter) {
      fm <- c("", "---", paste0("source: \"", file_path, "\""), paste0("size: ", ifelse(!is.null(meta$size), meta$size, NA)), paste0("mtime: \"", ifelse(!is.null(meta$mtime), meta$mtime, ""),"\""), "---", "")
      out_lines <- c(out_lines, fm)
    } else {
      out_lines <- c(out_lines, "")
    }
    body <- if (!is.null(s$summary) && nzchar(s$summary)) s$summary else if (!is.null(s$md) && file.exists(as.character(s$md))) readr::read_file(as.character(s$md)) else "<!-- no summary available -->"
    out_lines <- c(out_lines, body, "\n")
  }
  out_file_char <- as.character(output_file)
  d <- dirname(out_file_char)
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  writeLines(out_lines, con = out_file_char)
  invisible(normalizePath(out_file_char))
}

# ----------------------
# Helper to resolve output base (accepts "path", "project", or explicit path)
# ----------------------
resolve_output_base <- function(output_base, root) {
  # if explicit NULL -> use getwd()
  if (is.null(output_base)) return(fs::path_real(getwd()))
  ob <- output_base
  # if user passed a length>1 (legacy) take first
  if (length(ob) > 1) ob <- ob[[1]]
  # if ob equals "path" or "project" -> return appropriate base
  if (identical(ob, "path")) return(fs::path_real(root))
  if (identical(ob, "project")) return(fs::path_real(getwd()))
  # otherwise treat as a path string (relative or absolute)
  ob_char <- as.character(ob)
  # if empty, fallback to getwd()
  if (!nzchar(ob_char)) return(fs::path_real(getwd()))
  # resolve relative to current working dir
  tryCatch(fs::path_real(ob_char), error = function(e) fs::path_norm(ob_char))
}

# ----------------------
# Main: document_project (with change_only support and flexible output_base)
# ----------------------
#' Document an R project (single_output supported)
#' @param output_base "path" | "project" | path string. If omitted, defaults to getwd().
#' @export
document_project <- function(path = ".",
                             chat_fn = NULL,
                             output_dir = NULL,
                             output_base = getwd(),
                             max_chars = 16000,
                             verbose = TRUE,
                             generate_docs = TRUE,
                             generate_readme = TRUE,
                             overwrite_readme = FALSE,
                             single_output = FALSE,
                             single_output_file = "documentation/documentation.md",
                             exclude_file = NULL,
                             exclude_dirs = NULL,
                             exclude_patterns = NULL,
                             change_only = FALSE) {
  root <- fs::path_real(path)
  base_for_output <- resolve_output_base(output_base, root)
  
  # If output_dir is NULL, default to './documentation' under the resolved base
  if (is.null(output_dir)) {
    output_dir <- fs::path(base_for_output, "documentation")
  }
  # resolve output_dir wrt the base (unless absolute)
  output_dir <- if (fs::is_absolute_path(output_dir)) as.character(output_dir) else as.character(fs::path(base_for_output, output_dir))
  
  default_exclude_file <- fs::path(output_dir, "exclude.txt")
  exclude_file <- exclude_file %||% (if (file.exists(default_exclude_file)) default_exclude_file else NULL)
  base_exclude_dirs <- c("packrat", "renv", "node_modules", ".git", "demo")
  base_exclude_patterns <- c("\\.Rproj$", "^README\\.md$")
  exclude_from_file <- read_exclude_file(exclude_file)
  all_exclude_dirs <- unique(c(base_exclude_dirs, exclude_from_file$dirs, exclude_dirs %||% character(0)))
  all_exclude_patterns <- unique(c(base_exclude_patterns, exclude_from_file$patterns, exclude_patterns %||% character(0)))
  
  if (generate_docs && is.null(chat_fn)) stop("`chat_fn` is required when `generate_docs = TRUE`.")
  
  summaries <- NULL; index <- NULL; readme_written <- NULL
  
  if (generate_docs) {
    files_df <- scan_project_files(root, exclude_dirs = all_exclude_dirs, exclude_patterns = all_exclude_patterns)
    # permanent exclusions
    files_df <- files_df[!grepl("(?i)^README\\.md$", fs::path_file(files_df$file), perl = TRUE), ]
    if (single_output) {
      single_output_file_resolved <- if (fs::is_absolute_path(single_output_file)) single_output_file else as.character(fs::path(base_for_output, single_output_file))
      out_basename <- fs::path_file(as.character(single_output_file_resolved))
      files_df <- files_df[!fs::path_file(files_df$file) %in% out_basename, ]
    }
    if (nrow(files_df) == 0) {
      message("No files found matching defaults. You can change include/exclude in scan_project_files().")
    } else {
      # ensure output_dir exists before per-file md writes
      fs::dir_create(output_dir, recurse = TRUE)
      # load previous index if change_only
      index_path <- fs::path(output_dir, "index.json")
      prev_index <- if (change_only && fs::file_exists(index_path)) jsonlite::read_json(as.character(index_path), simplifyVector = TRUE) else NULL
      prev_map <- list()
      if (!is.null(prev_index) && !is.null(prev_index$files)) {
        # prev_index$files is a list of lists with $file, $md, $meta
        for (f in prev_index$files) {
          prev_map[[f$file]] <- f
        }
      }
      
      summaries_list <- list()
      # iterate in file order to preserve deterministic output
      for (i in seq_len(nrow(files_df))) {
        f <- as.character(files_df$file[i])
        mtime <- as.character(fs::file_info(f)$modification_time)
        reuse <- FALSE
        if (!is.null(prev_map[[f]]) && !is.null(prev_map[[f]]$meta) && identical(as.character(prev_map[[f]]$meta$mtime), mtime)) {
          # attempt to reuse previous markdown file
          mdpath <- prev_map[[f]]$md %||% ""
          if (nzchar(mdpath) && fs::file_exists(as.character(mdpath))) {
            stext <- tryCatch(readr::read_file(as.character(mdpath)), error = function(e) "")
            summaries_list[[length(summaries_list) + 1]] <- list(file = f, md = as.character(mdpath), summary = stext, meta = prev_map[[f]]$meta)
            reuse <- TRUE
            if (verbose) message("Reusing summary for (unchanged): ", f)
          }
        }
        if (!reuse) {
          out <- summarize_file(f, chat_fn = chat_fn, max_chars = max_chars, verbose = verbose)
          rel <- fs::path_rel(out$file, start = root)
          rel_noext <- fs::path_ext_remove(rel)
          safe_name <- gsub("[^A-Za-z0-9._-]", "_", rel_noext)
          safe_name <- stringr::str_replace_all(safe_name, "/", "__")
          md_path <- fs::path(output_dir, paste0(safe_name, ".md"))
          # write per-file .md (unless single_output)
          if (!single_output) {
            tryCatch(readr::write_file(out$summary, as.character(md_path)), error = function(e) warning("Failed writing md file: ", as.character(md_path), " : ", conditionMessage(e)))
          }
          summaries_list[[length(summaries_list) + 1]] <- list(file = out$file, md = as.character(md_path), summary = out$summary, meta = out$meta)
        }
      } # end for files
      
      # write index.json
      index <- list(project = fs::path_file(root), root = as.character(root), generated_at = as.character(Sys.time()), files = summaries_list)
      json_path <- as.character(fs::path(output_dir, "index.json"))
      tryCatch(jsonlite::write_json(index, json_path, pretty = TRUE, auto_unbox = TRUE),
               error = function(e) warning("Failed writing index.json: ", conditionMessage(e)))
      if (verbose) message("Written documentation index to: ", json_path)
      
      # single combined output if requested
      if (single_output) {
        single_output_file_resolved <- if (fs::is_absolute_path(single_output_file)) as.character(single_output_file) else as.character(fs::path(base_for_output, single_output_file))
        outp <- create_single_documentation(summaries_list, output_file = as.character(single_output_file_resolved), include_frontmatter = TRUE)
        if (verbose) message("Written single combined documentation to: ", outp)
      }
      
      summaries <- summaries_list
    }
  } else {
    # generate_docs = FALSE: potentially load existing index.json for README generation
    json_path <- as.character(fs::path(output_dir, "index.json"))
    if (generate_readme) {
      if (!fs::file_exists(json_path)) {
        stop("`generate_docs = FALSE` and no documentation/index.json found at ", json_path, ". Run with generate_docs = TRUE first.")
      }
      index <- jsonlite::read_json(json_path, simplifyVector = TRUE)
      summaries <- purrr::map(index$files, function(x) {
        mdfile <- x$md
        stext <- if (fs::file_exists(as.character(mdfile))) readr::read_file(as.character(mdfile)) else ""
        list(file = x$file, md = mdfile, summary = stext, meta = x$meta)
      })
    }
  }
  
  # README generation -> write README next to the resolved output base (default getwd())
  if (generate_readme) {
    readme_dest <- as.character(fs::path(base_for_output, "README.md"))
    if (fs::file_exists(readme_dest) && !overwrite_readme) {
      if (verbose) message("README.md exists at ", readme_dest, " and overwrite_readme = FALSE. Skipping.")
      readme_written <- as.character(readme_dest)
    } else {
      if (is.null(summaries)) summaries <- list()
      readme_text <- create_readme_text_fixed(root, project_summary = NULL, file_summaries = summaries, readme_title = fs::path_file(root))
      tryCatch(readr::write_file(readme_text, readme_dest), error = function(e) warning("Failed writing README.md: ", conditionMessage(e)))
      if (verbose) message("README written to: ", readme_dest)
      readme_written <- as.character(readme_dest)
    }
  }
  
  invisible(list(index = index, summaries = summaries, readme = if (!is.null(readme_written)) readme_written else NULL))
}

# ----------------------
# make_ellmer_chat_fn (robust wrapper)
# ----------------------
#' Create chat wrapper for an ellmer chat object or similar
#' @export
make_ellmer_chat_fn <- function(chat_obj, response_field = NULL) {
  # If a function is provided just return it
  if (is.function(chat_obj)) return(chat_obj)
  
  function(prompt) {
    res <- tryCatch({
      # common method names: $chat, $call, $invoke, function-like
      if (!is.null(chat_obj$chat) && is.function(chat_obj$chat)) {
        chat_obj$chat(prompt)
      } else if (!is.null(chat_obj$call) && is.function(chat_obj$call)) {
        chat_obj$call(prompt)
      } else if (!is.null(chat_obj$invoke) && is.function(chat_obj$invoke)) {
        chat_obj$invoke(prompt)
      } else if (is.function(chat_obj)) {
        chat_obj(prompt)
      } else {
        stop("Unknown chat object interface. Provide a function or object with $chat(prompt).")
      }
    }, error = function(e) e)
    
    # If an error object returned, convert to message
    if (inherits(res, "error")) return(paste0("<<CHAT ERROR: ", conditionMessage(res), ">>"))
    
    # normalize response to character
    if (!is.null(response_field) && is.list(res) && !is.null(res[[response_field]])) {
      return(as.character(res[[response_field]]))
    }
    if (is.character(res)) return(paste(res, collapse = "\n"))
    if (is.list(res)) {
      # OpenAI-like: choices[[1]]$message$content or choices[[1]]$text
      if (!is.null(res$choices) && length(res$choices) >= 1) {
        ch <- res$choices[[1]]
        if (!is.null(ch$message) && !is.null(ch$message$content)) return(as.character(ch$message$content))
        if (!is.null(ch$text)) return(as.character(ch$text))
      }
      # direct message/content
      if (!is.null(res$message) && !is.null(res$message$content)) return(as.character(res$message$content))
      if (!is.null(res$content) && is.character(res$content)) return(as.character(res$content))
      if (!is.null(res$text) && is.character(res$text)) return(as.character(res$text))
      # fallback: collapse any character-like elements
      txts <- unlist(lapply(res, function(x) {
        if (is.character(x)) return(x)
        if (is.list(x)) return(paste(unlist(x), collapse = " "))
        tryCatch(as.character(x), error = function(e) "")
      }))
      txts <- txts[txts != ""]
      if (length(txts) > 0) return(paste(txts, collapse = "\n\n"))
      return(paste0("<<UNHANDLED CHAT RESPONSE: ", paste(capture.output(str(res)), collapse = " "), ">>"))
    }
    as.character(res)
  }
}

# ----------------------
# stub chat for testing
# ----------------------
#' Tiny stub chat function for testing
#' @export
stub_chat <- function(prompt) {
  paste0("SUMMARY\n\nThis is a stub summary (documentoR). Prompt preview: ", substr(prompt, 1, 200), "\n\nKEY_ITEMS\n\n- (stub)\n")
}

# ----------------------
# Convenience: single-file only writer (no index.json) with dry_run + README options
# ----------------------
#' Create a single combined documentation file only (no index.json)
#'
#' Summarizes project files via `chat_fn` and writes exactly one markdown file.
#'
#' @param path Project root (default ".")
#' @param chat_fn Function(prompt)->character used to summarize each file. Required unless dry_run = TRUE.
#' @param output_file Path to the single combined markdown to write (default "documentation/documentation.md")
#' @param output_base "path" | "project" | path string. If omitted, defaults to getwd().
#' @param include File extensions to include
#' @param exclude_dirs Directories to exclude (simple names)
#' @param exclude_patterns Regex patterns to exclude (file names/paths)
#' @param max_chars Max characters of source to include in prompt
#' @param verbose Logical
#' @param exclude_file Optional path to an exclude.txt (see read_exclude_file)
#' @param dry_run Logical. If TRUE, only list files that would be summarized (no LLM calls, no writes).
#' @param generate_readme Logical. If TRUE, also create README.md from summaries.
#' @param overwrite_readme Logical. If TRUE, overwrite existing README.md when generate_readme = TRUE.
#' @param change_only Logical. If TRUE and output_dir/index.json exists, reuse unchanged summaries.
#' @return Invisibly the path(s) to the written documentation file (and README if requested).
#' @export
document_project_single_file <- function(path = ".",
                                         chat_fn,
                                         output_file = "documentation/documentation.md",
                                         output_base = getwd(),
                                         include = c("R","r","Rmd","rmd","qmd","qmarkdown","md","markdown","html","htm","ipynb","pdf","docx","doc","pptx","rds","rda","rdata","csv","sql","json","yml","yaml"),
                                         exclude_dirs = c("packrat", "renv", "node_modules", ".git", "demo"),
                                         exclude_patterns = c("\\.Rproj$", "^README\\.md$"),
                                         max_chars = 16000,
                                         verbose = TRUE,
                                         exclude_file = NULL,
                                         dry_run = FALSE,
                                         generate_readme = FALSE,
                                         overwrite_readme = FALSE,
                                         change_only = FALSE) {
  root <- fs::path_real(path)
  base_for_output <- resolve_output_base(output_base, root)
  
  if (!dry_run && (missing(chat_fn) || !is.function(chat_fn))) stop("Please supply chat_fn(prompt) as a function unless dry_run = TRUE.")
  
  # Determine resolved absolute output_file path (resolve relative to base_for_output)
  output_file_resolved <- if (fs::is_absolute_path(output_file)) as.character(output_file) else as.character(fs::path(base_for_output, output_file))
  output_dir <- dirname(output_file_resolved)
  
  # ensure output dir exists before per-file md writes
  fs::dir_create(output_dir, recurse = TRUE)
  
  # merge exclude file if present
  if (!is.null(exclude_file) && file.exists(exclude_file)) {
    ex <- read_exclude_file(exclude_file)
    exclude_dirs <- unique(c(exclude_dirs, ex$dirs))
    exclude_patterns <- unique(c(exclude_patterns, ex$patterns))
  } else {
    default_exclude_file <- fs::path(root, "documentation", "exclude.txt")
    if (file.exists(default_exclude_file)) {
      ex <- read_exclude_file(default_exclude_file)
      exclude_dirs <- unique(c(exclude_dirs, ex$dirs))
      exclude_patterns <- unique(c(exclude_patterns, ex$patterns))
    }
  }
  
  # always exclude the output dir name and README
  exclude_dirs <- unique(c(exclude_dirs, fs::path_file(output_dir)))
  exclude_patterns <- unique(c(exclude_patterns, "^README\\.md$"))
  
  # scan BEFORE writing
  files_df <- scan_project_files(root, include = include, exclude_dirs = exclude_dirs, exclude_patterns = exclude_patterns)
  
  # permanent exclusions: README and the output file itself
  files_df <- files_df[!grepl("(?i)^README\\.md$", fs::path_file(files_df$file), perl = TRUE), ]
  out_basename <- fs::path_file(as.character(output_file_resolved))
  files_df <- files_df[!fs::path_file(files_df$file) %in% out_basename, ]
  
  if (nrow(files_df) == 0) {
    if (dry_run) {
      message("Dry run: no files would be documented after applying excludes.")
      return(invisible(files_df))
    }
    stop("No files found to document after applying excludes.")
  }
  
  # dry_run: just list files and return
  if (dry_run) {
    message("Dry run: the following files would be summarized (no LLM calls will be made):")
    print(files_df$file)
    return(invisible(files_df))
  }
  
  # change_only support (load index if present)
  prev_index <- NULL
  index_path <- fs::path(output_dir, "index.json")
  if (change_only && fs::file_exists(index_path)) {
    prev_index <- jsonlite::read_json(as.character(index_path), simplifyVector = TRUE)
  }
  prev_map <- list()
  if (!is.null(prev_index) && !is.null(prev_index$files)) {
    for (f in prev_index$files) prev_map[[f$file]] <- f
  }
  
  # Summarize files (reuse unchanged if possible)
  summaries <- list()
  for (i in seq_len(nrow(files_df))) {
    f <- as.character(files_df$file[i])
    mtime <- as.character(fs::file_info(f)$modification_time)
    reused <- FALSE
    if (!is.null(prev_map[[f]]) && !is.null(prev_map[[f]]$meta) && identical(as.character(prev_map[[f]]$meta$mtime), mtime)) {
      mdpath <- prev_map[[f]]$md %||% ""
      if (nzchar(mdpath) && fs::file_exists(as.character(mdpath))) {
        stext <- tryCatch(readr::read_file(as.character(mdpath)), error = function(e) "")
        summaries[[length(summaries) + 1]] <- list(file = f, md = as.character(mdpath), summary = stext, meta = prev_map[[f]]$meta)
        reused <- TRUE
        if (verbose) message("Reusing summary for (unchanged): ", f)
      }
    }
    if (!reused) {
      out <- summarize_file(f, chat_fn = chat_fn, max_chars = max_chars, verbose = verbose)
      rel <- fs::path_rel(out$file, start = root)
      rel_noext <- fs::path_ext_remove(rel)
      safe_name <- gsub("[^A-Za-z0-9._-]", "_", rel_noext)
      safe_name <- stringr::str_replace_all(safe_name, "/", "__")
      md_path <- fs::path(output_dir, paste0(safe_name, ".md"))
      tryCatch(readr::write_file(out$summary, as.character(md_path)), error = function(e) warning("Failed writing md file: ", as.character(md_path), " : ", conditionMessage(e)))
      summaries[[length(summaries) + 1]] <- list(file = out$file, md = as.character(md_path), summary = out$summary, meta = out$meta)
    }
  }
  
  # write single combined doc (already ensured parent dir exists)
  outp <- create_single_documentation(summaries, output_file = as.character(output_file_resolved), include_frontmatter = TRUE)
  if (verbose) message("Wrote single documentation file: ", outp)
  
  # optionally create README.md in output base using extracted SUMMARY content
  readme_written <- NULL
  if (isTRUE(generate_readme)) {
    readme_dest <- as.character(fs::path(base_for_output, "README.md"))
    if (fs::file_exists(readme_dest) && !isTRUE(overwrite_readme)) {
      message("README.md exists at ", readme_dest, " and overwrite_readme = FALSE. Skipping README generation.")
      readme_written <- as.character(readme_dest)
    } else {
      readme_text <- create_readme_text_fixed(root, project_summary = NULL, file_summaries = summaries, readme_title = fs::path_file(root))
      tryCatch(readr::write_file(readme_text, readme_dest), error = function(e) warning("Failed writing README.md: ", conditionMessage(e)))
      if (verbose) message("README written to: ", readme_dest)
      readme_written <- as.character(readme_dest)
    }
  }
  
  out <- list(documentation = normalizePath(as.character(outp)))
  if (!is.null(readme_written)) out$readme <- normalizePath(readme_written)
  invisible(out)
}

# ----------------------
# Examples / usage (commented)
# ----------------------
# devtools::load_all()
# # test with stub:
# chat_fn <- documentoR::stub_chat
# # basic single-file documentation, outputs to ./documentation/documentation.md (resolved relative to getwd())
# documentoR::document_project_single_file(
#   path = "docs",
#   chat_fn = chat_fn,
#   output_file = "documentation/documentation.md",
#   output_base = getwd(),
#   dry_run = TRUE
# )
#
# # real run (writes combined doc into ./documentation/documentation.md and README.md into getwd()/README.md)
# documentoR::document_project_single_file(
#   path = "docs",
#   chat_fn = chat_fn,
#   output_file = "documentation/documentation.md",
#   output_base = getwd(),
#   dry_run = FALSE,
#   generate_readme = TRUE,
#   overwrite_readme = TRUE
# )
#
# # Using an ellmer-like chat object:
# # chat <- ellmer::chat_aws_bedrock(system_prompt = "You are a concise R doc assistant.")
# # chat_fn <- documentoR::make_ellmer_chat_fn(chat)
# # documentoR::document_project(path = ".", chat_fn = chat_fn, output_base = getwd(), single_output = TRUE, single_output_file = "documentation/documentation.md")
#
# End of file
