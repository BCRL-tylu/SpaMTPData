.spamtpdata_manifest <- function() {
    path <- system.file("extdata", "resource_manifest.csv", package = "SpaMTPData")
    if (!nzchar(path)) {
        stop("SpaMTPData resource manifest is unavailable.", call. = FALSE)
    }
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

.spamtpdata_resolve_version <- function(manifest, version) {
    versions <- unique(as.character(manifest$version))
    if (is.null(version) || identical(version, "latest")) {
        return(utils::tail(sort(package_version(versions)), 1L) |> as.character())
    }
    version <- as.character(version)[1L]
    if (!version %in% versions) {
        stop(
            "SpaMTPData version '", version, "' is unavailable. Available: ",
            paste(versions, collapse = ", "),
            call. = FALSE
        )
    }
    version
}

.spamtpdata_local_dir <- function(local_dir = NULL) {
    if (!is.null(local_dir)) {
        return(normalizePath(local_dir, mustWork = FALSE))
    }
    configured <- getOption("SpaMTPData.resource_dir", "")
    if (!nzchar(configured)) {
        configured <- Sys.getenv("SPAMTPDATA_RESOURCE_DIR", "")
    }
    if (!nzchar(configured)) NULL else normalizePath(configured, mustWork = FALSE)
}

.spamtpdata_local_file <- function(row, local_dir) {
    if (is.null(local_dir)) return(NULL)
    candidates <- unique(c(
        file.path(local_dir, row$file_name),
        file.path(local_dir, row$resource, row$file_name),
        file.path(local_dir, paste0(row$resource, ".rds")),
        file.path(local_dir, paste0(row$resource, ".RDS"))
    ))
    found <- candidates[file.exists(candidates)]
    if (length(found)) found[[1L]] else NULL
}

.spamtpdata_read_local <- function(path, dispatch_class) {
    if (tolower(dispatch_class) %in% c("rds", "rda")) {
        if (tolower(dispatch_class) == "rds") return(readRDS(path))
        environment <- new.env(parent = emptyenv())
        loaded <- load(path, envir = environment)
        if (length(loaded) != 1L) {
            stop("Local Rda resource must contain exactly one object.", call. = FALSE)
        }
        return(environment[[loaded]])
    }
    normalizePath(path, mustWork = TRUE)
}

.spamtpdata_cache_dir <- function(cache_dir = NULL) {
    if (is.null(cache_dir)) {
        cache_dir <- getOption("SpaMTPData.cache_dir", "")
    }
    if (!nzchar(cache_dir)) {
        cache_dir <- Sys.getenv("SPAMTPDATA_CACHE_DIR", "")
    }
    if (!nzchar(cache_dir)) {
        cache_dir <- tools::R_user_dir("SpaMTPData", which = "cache")
    }
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    normalizePath(cache_dir, mustWork = TRUE)
}

.spamtpdata_file_valid <- function(path, row) {
    if (!file.exists(path)) return(FALSE)
    expected_bytes <- suppressWarnings(as.numeric(row$bytes[[1L]]))
    if (is.finite(expected_bytes) && file.info(path)$size != expected_bytes) {
        return(FALSE)
    }
    expected_md5 <- tolower(as.character(row$md5[[1L]]))
    if (nzchar(expected_md5)) {
        observed_md5 <- unname(tools::md5sum(path))
        if (!identical(tolower(observed_md5), expected_md5)) return(FALSE)
    }
    TRUE
}

.spamtpdata_download <- function(row, cache_dir = NULL, timeout = 1800,
                                 retries = 3L) {
    cache_dir <- .spamtpdata_cache_dir(cache_dir)
    destination <- file.path(cache_dir, as.character(row$file_name[[1L]]))
    if (.spamtpdata_file_valid(destination, row)) return(destination)

    url <- paste0(
        as.character(row$location_prefix[[1L]]),
        as.character(row$rdata_path[[1L]])
    )
    retries <- suppressWarnings(as.integer(retries)[1L])
    if (is.na(retries) || retries < 1L) retries <- 1L
    timeout <- suppressWarnings(as.numeric(timeout)[1L])
    if (!is.finite(timeout) || timeout < 1) timeout <- 1800
    old_timeout <- getOption("timeout")
    old_timeout_numeric <- suppressWarnings(as.numeric(old_timeout)[1L])
    if (!is.finite(old_timeout_numeric)) old_timeout_numeric <- 60
    options(timeout = max(old_timeout_numeric, timeout))
    on.exit(options(timeout = old_timeout), add = TRUE)

    last_error <- NULL
    for (attempt in seq_len(retries)) {
        partial <- paste0(destination, ".part-", Sys.getpid())
        on.exit(unlink(partial), add = TRUE)
        result <- tryCatch(
            {
                utils::download.file(
                    url,
                    destfile = partial,
                    method = "libcurl",
                    mode = "wb",
                    quiet = TRUE
                )
                if (!.spamtpdata_file_valid(partial, row)) {
                    stop("downloaded file failed its size or MD5 check")
                }
                if (file.exists(destination)) unlink(destination)
                if (!file.rename(partial, destination)) {
                    stop("could not move the verified file into the cache")
                }
                destination
            },
            error = function(error) {
                last_error <<- conditionMessage(error)
                unlink(partial)
                NULL
            }
        )
        if (!is.null(result)) return(result)
    }
    stop(
        "Failed to download verified SpaMTPData resource '", row$resource,
        "' after ", retries, " attempt(s): ", last_error,
        call. = FALSE
    )
}

#' List SpaMTP experiment resources
#'
#' @param version Data release version. `NULL` returns all versions.
#' @param category Optional resource category.
#'
#' @return A data frame describing the registered resources.
#' @export
#'
#' @examples
#' SpaMTPDataResources()
SpaMTPDataResources <- function(version = NULL, category = NULL) {
    manifest <- .spamtpdata_manifest()
    if (!is.null(version)) {
        version <- .spamtpdata_resolve_version(manifest, version)
        manifest <- manifest[manifest$version == version, , drop = FALSE]
    }
    if (!is.null(category)) {
        manifest <- manifest[manifest$category %in% category, , drop = FALSE]
    }
    rownames(manifest) <- NULL
    manifest
}

#' Retrieve a SpaMTP experiment resource
#'
#' @param resource Resource name; see [SpaMTPDataResources()].
#' @param version Data release version or `"latest"`.
#' @param local_dir Optional directory containing downloaded source files.
#' @param hub Optional pre-created `ExperimentHub` object.
#' @param metadata Return only the registry row.
#' @param offline If `TRUE`, never query ExperimentHub.
#' @param fallback_url If `TRUE`, use the immutable source URL when the resource
#'   has not yet been ingested into ExperimentHub.
#' @param cache_dir Cache directory for source-URL downloads. Defaults to the
#'   platform-specific user cache returned by [tools::R_user_dir()].
#' @param timeout Download timeout in seconds for the source-URL fallback.
#' @param retries Number of verified download attempts.
#'
#' @return The requested experiment object or local file path. With
#'   `metadata = TRUE`, returns one registry row.
#' @export
#'
#' @examples
#' SpaMTPData("mouse_brain_dhb_striatum", metadata = TRUE)
#' \dontrun{
#' striatum <- SpaMTPData("mouse_brain_dhb_striatum")
#' }
SpaMTPData <- function(resource, version = "latest", local_dir = NULL,
                       hub = NULL, metadata = FALSE, offline = FALSE,
                       fallback_url = TRUE, cache_dir = NULL, timeout = 1800,
                       retries = 3L) {
    manifest <- .spamtpdata_manifest()
    version <- .spamtpdata_resolve_version(manifest, version)
    key <- tolower(as.character(resource)[1L])
    rows <- manifest[
        tolower(manifest$resource) == key & manifest$version == version,
        , drop = FALSE
    ]
    if (nrow(rows) != 1L) {
        stop(
            "Unknown SpaMTPData resource '", resource, "' for version ",
            version, ". Use SpaMTPDataResources() to list valid names.",
            call. = FALSE
        )
    }
    if (isTRUE(metadata)) return(rows)

    local_file <- .spamtpdata_local_file(rows, .spamtpdata_local_dir(local_dir))
    if (!is.null(local_file)) {
        return(.spamtpdata_read_local(local_file, rows$dispatch_class))
    }
    if (isTRUE(offline)) {
        stop(
            "Resource '", resource, "' is not present in the configured local ",
            "directory and offline = TRUE.",
            call. = FALSE
        )
    }

    hub_error <- NULL
    value <- tryCatch(
        {
            if (is.null(hub)) hub <- ExperimentHub::ExperimentHub()
            hits <- AnnotationHub::query(hub, c("SpaMTPData", rows$title))
            hit_metadata <- as.data.frame(S4Vectors::mcols(hits))
            exact <- which(as.character(hit_metadata$title) == rows$title)
            if (!length(exact)) {
                stop("resource has not yet been ingested into ExperimentHub")
            }
            hits[[exact[[1L]]]]
        },
        error = function(error) {
            hub_error <<- conditionMessage(error)
            NULL
        }
    )
    if (!is.null(value)) return(value)
    if (isTRUE(fallback_url)) {
        path <- .spamtpdata_download(
            rows,
            cache_dir = cache_dir,
            timeout = timeout,
            retries = retries
        )
        return(.spamtpdata_read_local(path, rows$dispatch_class))
    }
    stop(
        "ExperimentHub could not provide '", rows$title, "': ", hub_error,
        ". Configure a local resource directory or set fallback_url = TRUE.",
        call. = FALSE
    )
}

#' Alias for explicit SpaMTPData resource retrieval
#'
#' @inheritParams SpaMTPData
#' @return The value returned by [SpaMTPData()].
#' @export
SpaMTPDataResource <- function(resource, version = "latest", local_dir = NULL,
                               hub = NULL, metadata = FALSE, offline = FALSE,
                               fallback_url = TRUE, cache_dir = NULL,
                               timeout = 1800, retries = 3L) {
    SpaMTPData(
        resource = resource,
        version = version,
        local_dir = local_dir,
        hub = hub,
        metadata = metadata,
        offline = offline,
        fallback_url = fallback_url,
        cache_dir = cache_dir,
        timeout = timeout,
        retries = retries
    )
}

#' Report the available SpaMTPData releases
#'
#' @return A character vector of data releases, newest first.
#' @export
#'
#' @examples
#' SpaMTPDataVersion()
SpaMTPDataVersion <- function() {
    versions <- unique(as.character(.spamtpdata_manifest()$version))
    rev(as.character(sort(package_version(versions))))
}
