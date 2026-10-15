
isManuscript <- FALSE


# Self-locate this script ------------------------------------------------

# Figure out where this file lives on disk, so paths work regardless of
# the current working directory and however the script is invoked
# (sourced from an Rmd, Source button in RStudio, line-by-line, or
# Rscript). Avoids depending on `here()` because here() walks up from
# the wd and can land on the wrong project root.
.thisFile <- function() {
  # 1) When the file is being sourced: sys.frames() carries $ofile.
  frames <- sys.frames()
  for (i in seq_along(frames)) {
    ofile <- frames[[i]]$ofile
    if (!is.null(ofile) && nzchar(ofile)) {
      return(normalizePath(ofile, winslash = "/"))
    }
  }
  # 2) When run via Rscript: --file=<path> appears in commandArgs().
  args <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", args, value = TRUE)
  if (length(m) > 0) {
    return(normalizePath(sub("^--file=", "", m[1]), winslash = "/"))
  }
  # 3) When in RStudio (Source button or running line-by-line).
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    ctx <- rstudioapi::getSourceEditorContext()
    if (!is.null(ctx) && nzchar(ctx$path)) {
      return(normalizePath(ctx$path, winslash = "/"))
    }
  }
  NA_character_
}

.scriptFile <- .thisFile()
if (is.na(.scriptFile)) {
  stop(
    "Cannot locate 01-set-up.R automatically. ",
    "Set the working directory to the project's R/ folder and re-run, ",
    "or open the .Rproj before running."
  )
}

# 01-set-up.R lives at <projectRoot>/01-scripts/01-set-up.R, so the
# project root is two levels up.
projectRoot <- normalizePath(
  file.path(dirname(.scriptFile), ".."),
  winslash = "/"
)


# Script folder -----------------------------------------------------------

# Where R script files are saved
scriptsFolder <- file.path(projectRoot, "01-scripts")


# Load packages -----------------------------------------------------------

source(file.path(scriptsFolder, "00-load-packages.R"))


# Read environment --------------------------------------------------------

# Pick the right .Renviron for this machine. Override the auto-pick by
# setting the env var LME_RENVIRON before sourcing this file.
renvFile <- Sys.getenv("LME_RENVIRON", unset = "")
if (!nzchar(renvFile)) {
  candidates <- c(
    home = file.path(projectRoot, "TD_home.Renviron"),
    work = file.path(projectRoot, "TD_work_65.Renviron")
  )
  # Default per OS, then fall back to whichever exists.
  renvFile <- if (.Platform$OS.type == "windows") {
    candidates[["work"]]
  } else {
    candidates[["home"]]
  }
  if (!file.exists(renvFile)) {
    found <- candidates[file.exists(candidates)]
    if (length(found) > 0) renvFile <- found[[1]]
  }
}
if (!file.exists(renvFile)) {
  stop(
    "No Renviron file found. Looked at: ",
    paste(file.path(projectRoot,
                    c("TD_home.Renviron", "TD_work_65.Renviron")),
          collapse = ", "),
    ". Set the LME_RENVIRON env var to override."
  )
}
readRenviron(renvFile)

Sys.getenv("RENV_FILE")

# Store today's date, to help with versioned output
dateToday <- Sys.Date()


# Define input ------------------------------------------------------------

source(here::here("01-scripts", "00-paths.R"))  # sets dataFolder, outputFolder

# get the CSV file name
csvFileName <- Sys.getenv("DATA_FILE_NAME")

# Get the Excel file name
excelFileName <- Sys.getenv("EXCEL_FILE_NAME")


# Define output -----------------------------------------------------------

# This gets prepended to the file names when saving
filePrefix <- Sys.getenv("FILE_PREFIX")

# type of images to save for plots.
# .png, .svg, or .pdf, for example
imgTypePlots <- Sys.getenv("PLOT_OUTPUT_FORMAT")

# save individual plot files?
savePlots <- Sys.getenv("SAVE_PLOTS")

# Current Computer Type
# Some functions and outputs need to be formatted differently
# depending on Windows or Mac. Detect rather than relying on Renviron.
currentCompType <- if (.Platform$OS.type == "windows") "windows" else "mac"

# (outputFolder set by 00-paths.R above)

dataOutputFolder   <- file.path(outputFolder, "data")
plotOutputFolder   <- file.path(outputFolder, "plots")
reportOutputFolder <- file.path(outputFolder, "reports")

# Make sure each output subfolder exists before normalizing — otherwise
# normalizePath() warns/errors on a fresh checkout where they don't yet
# exist (notably on Windows).
for (d in c(outputFolder, dataOutputFolder, plotOutputFolder, reportOutputFolder)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

dataOutputFolder   <- normalizePath(dataOutputFolder,   winslash = "/", mustWork = FALSE)
plotOutputFolder   <- normalizePath(plotOutputFolder,   winslash = "/", mustWork = FALSE)
reportOutputFolder <- normalizePath(reportOutputFolder, winslash = "/", mustWork = FALSE)


# Source function files ---------------------------------------------------


# Where Function files are saved
functionsFolder <- file.path(projectRoot, "02-functions")

functionFiles <- list.files(
  functionsFolder,
  full.names = TRUE,
  recursive = TRUE,
  pattern = "\\.R$"
)

invisible(lapply(functionFiles, source))
