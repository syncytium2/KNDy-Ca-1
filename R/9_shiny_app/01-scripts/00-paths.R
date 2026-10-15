# 00-paths.R
#
# Resolves dataFolder and outputFolder from environment variables, with
# platform-aware WSL/Windows switching and an in-repo fallback.
# Sourced by 01-set-up.R; also sourceable standalone by pipeline scripts
# that do not need the full setup (packages, functions, etc.).
#
# Sets: dataFolder, outputFolder

library(here)

.resolveFolder <- function(envName, default) {
  suffix <- if (nzchar(Sys.getenv("WSL_DISTRO_NAME"))) "_WSL" else "_WIN"
  raw <- Sys.getenv(paste0(envName, suffix), unset = "")
  if (!nzchar(raw)) raw <- Sys.getenv(envName, unset = "")
  if (!nzchar(raw)) return(default)
  candidate <- tryCatch(
    normalizePath(raw, winslash = "/", mustWork = FALSE),
    error = function(e) raw
  )
  if (dir.exists(candidate)) candidate else default
}

dataFolder   <- .resolveFolder("DATA_FOLDER",   here::here("04-data"))
outputFolder <- .resolveFolder("OUTPUT_FOLDER", here::here("output"))
