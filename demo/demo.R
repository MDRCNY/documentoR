# 0) load package (run from package source root)
devtools::load_all()

# 1) quick dry-run using the tiny stub chat (no LLM calls; shows which files would be summarized)
chat_fn <- documentoR::stub_chat

documentoR::document_project_single_file(
  path = "docs",                                 # folder to scan
  chat_fn = chat_fn,                             # stub chat (no external LLM)
  output_file = "documentation/documentation.md",# where to write combined doc (relative to output_base)
  output_base = getwd(),                         # base used to resolve output_file and README (defaults to getwd())
  dry_run = TRUE,                                # only list files, do not call LLM or write files
  verbose = TRUE
)

# 2) Real run with stub chat: writes a combined documentation file + README at output_base
documentoR::document_project_single_file(
  path = "docs",
  chat_fn = chat_fn,
  output_file = "documentation/documentation.md",
  output_base = getwd(),
  dry_run = FALSE,            # actually call chat_fn for each file
  generate_readme = TRUE,     # also create README.md (at output_base)
  overwrite_readme = TRUE,    # overwrite existing README if present
  verbose = TRUE
)

# 3) Use a real LLM via ellmer (uncommented); configure ellmer/credentials before running.
#    This example creates an ellmer chat object and wraps it for documentoR.
chat <- ellmer::chat_aws_bedrock(
  model = "anthropic.claude-3-5-sonnet-20240620-v1:0",
  system_prompt = "Answer concisely and use prior context."
)

chat_fn_llm <- documentoR::make_ellmer_chat_fn(chat)

# Run documentoR using the ellmer-backed chat function
documentoR::document_project_single_file(
  path = "docs",
  chat_fn = chat_fn_llm,
  output_file = "documentation/documentation.md",
  output_base = getwd(),      # resolves output_file/README relative to current working dir
  dry_run = FALSE,
  generate_readme = TRUE,
  overwrite_readme = TRUE,
  verbose = TRUE
)

# ---- Notes ----
# - Make sure ellmer is installed and configured (credentials, region, etc.) before using the ellmer chat call.
# - If you prefer the combined documentation to live in a different base directory, change output_base
#   to a directory path (e.g. output_base = "/path/to/project") or "path"/"project" depending on your function variant.
