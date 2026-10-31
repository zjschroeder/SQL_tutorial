# =============================================================================
# shinylive_export.R — build the browser-only (static HTML) version of the app
#
# Produces a `docs/` directory containing the R interpreter (webR), the CRAN
# packages the app depends on (incl. DBI + RSQLite, compiled to WebAssembly),
# the app source, and the shipped database — a complete, self-contained website
# that needs no server. The query editor runs real SQL entirely in the browser.
#
# Run from the project root:
#     Rscript shinylive_export.R
# =============================================================================

required_pkgs <- c("shinylive")
missing <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing)) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}

suppressMessages(library(shinylive))
message("shinylive version: ", as.character(packageVersion("shinylive")))

# Sanity-check: the database must exist, or the exported app loads with no data.
if (!file.exists(file.path("data", "institute.sqlite"))) {
  stop("data/institute.sqlite not found. Build it first:  Rscript data/build_db.R")
}
if (!file.exists(file.path("data", "district.sqlite"))) {
  stop("data/district.sqlite not found. Build it first:  Rscript data/build_district.R")
}

# Stage only the app's own files into a temp dir, so the export bundle never
# picks up the bundled reference app (Example app/), a previous docs/ build,
# RStudio cruft, or the build-time-only data (frozen CSVs, the build and
# generate scripts). Only the two .sqlite files are needed at runtime.
stage <- file.path(tempdir(), "sqlapp_stage")
unlink(stage, recursive = TRUE, force = TRUE)
dir.create(stage)
dir.create(file.path(stage, "data"))
file.copy("app.R", stage)
file.copy("R", stage, recursive = TRUE)
file.copy(file.path("data", "institute.sqlite"), file.path(stage, "data"))
file.copy(file.path("data", "district.sqlite"),  file.path(stage, "data"))
message("Staged app for export:\n  ", paste(list.files(stage, recursive = TRUE), collapse = "\n  "))

# Clean any previous build so we don't ship stale assets.
if (dir.exists("docs")) {
  message("Removing previous docs/ build...")
  unlink("docs", recursive = TRUE, force = TRUE)
}

# Build the static Shinylive bundle. The webR interpreter + WASM package
# binaries (DBI, RSQLite, and their dependency closure) are downloaded from
# repo.r-wasm.org at export time and bundled into docs/.
message("Building static Shinylive bundle into docs/ ...")
# Pinned assets. shinylive 0.5.0 defaults to assets 0.10.12 (webR 0.6.0, R 4.6),
# and the RSQLite build for R 4.6 on repo.r-wasm.org fails to load there
# ("unable to load shared object .../RSQLite.so"), so the app never starts.
# Assets 0.10.8 bundle webR 0.5.5 (R 4.5.1), where RSQLite loads and runs.
# Override with the SHINYLIVE_ASSETS_VERSION environment variable to try a
# newer version; open the browser console after deploying to confirm RSQLite loads.
assets <- Sys.getenv("SHINYLIVE_ASSETS_VERSION", "0.10.8")
message("Using Shinylive assets ", assets)
shinylive::export(appdir = stage, destdir = "docs", quiet = FALSE, assets_version = assets)

# GitHub Pages reads .nojekyll to skip Jekyll, which would otherwise ignore the
# underscore-prefixed directories Shinylive emits (e.g. _modules).
file.create("docs/.nojekyll")

message("\n[OK] Static bundle ready in docs/")
message("    Preview locally:  Rscript -e 'httpuv::runStaticServer(\"docs\")'")
message("    Then deploy:      git add docs && git commit -m 'rebuild' && git push")
