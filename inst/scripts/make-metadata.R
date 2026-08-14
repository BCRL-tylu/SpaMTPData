#!/usr/bin/env Rscript

if (!requireNamespace("ExperimentHubData", quietly = TRUE)) {
    stop("Install ExperimentHubData before validating Hub metadata.")
}
script_arg <- grep("^--file=", commandArgs(), value = TRUE)[1L]
script_path <- sub("^--file=", "", script_arg)
package_root <- normalizePath(
    file.path(dirname(script_path), "..", ".."), mustWork = TRUE
)
ExperimentHubData::makeExperimentHubMetadata(package_root)
