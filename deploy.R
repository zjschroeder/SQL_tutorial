# =============================================================================
# deploy.R — push the "SQL, step by step" app to shinyapps.io
#
# Run from the project root *after* setting your shinyapps.io credentials
# (one-time; see SETUP). The script:
#   1. Verifies CRAN dependencies are installed.
#   2. Confirms the database exists.
#   3. Checks that app.R sources cleanly.
#   4. Calls rsconnect::deployApp() with a stable appName.
#
# Files matching patterns in .rscignore (e.g. Example app/, docs/) are excluded
# from the upload bundle.
# =============================================================================

# ---- SETUP (do once) --------------------------------------------------------
# 1. Create a free shinyapps.io account: https://www.shinyapps.io/admin/#/signup
# 2. From the dashboard, copy your Token + Secret (Account -> Tokens).
# 3. Run once interactively, replacing placeholders:
#       rsconnect::setAccountInfo(name   = "<your-account>",
#                                 token  = "<token>",
#                                 secret = "<secret>")

# ---- DEPLOY -----------------------------------------------------------------
required_pkgs <- c("shiny", "bslib", "DBI", "RSQLite", "rsconnect")
missing <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing)) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing)
}

# The database must ship with the app.
if (!file.exists(file.path("data", "institute.sqlite"))) {
  stop("data/institute.sqlite not found. Build it first:  Rscript data/build_db.R")
}

# Sanity-check: confirm app.R sources cleanly before uploading.
message("Sourcing app.R to catch errors before deploy...")
src <- readLines("app.R")
end <- which(grepl("^shinyApp\\(", src))[1]
src <- src[seq_len(end - 1)]
tmp <- tempfile(fileext = ".R"); writeLines(src, tmp)
e <- try(sys.source(tmp, envir = new.env(parent = globalenv())), silent = TRUE)
unlink(tmp)
if (inherits(e, "try-error")) {
  stop("app.R failed to source:\n", attr(e, "condition")$message)
}
message("[OK] app.R sources OK")

# Deploy. appName is the URL slug under <account>.shinyapps.io/<appName>.
rsconnect::deployApp(
  appDir   = ".",
  appName  = "sql-step-by-step",
  appTitle = "SQL, step by step",
  forceUpdate    = TRUE,
  launch.browser = TRUE
)
