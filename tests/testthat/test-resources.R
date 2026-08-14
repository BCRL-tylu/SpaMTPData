test_that("resource registry is versioned and categorized", {
    resources <- SpaMTPDataResources()
    expect_true(all(c("resource", "version", "category") %in% names(resources)))
    expect_true("mouse_brain_dhb_striatum" %in% resources$resource)
    expect_true(all(resources$bytes > 0))
    expect_true(all(grepl("^[0-9a-f]{32}$", resources$md5)))
    expect_identical(SpaMTPDataVersion(), "1.0.0")
})

test_that("local RDS resources load without a Hub connection", {
    path <- tempfile("spamtpdata-")
    dir.create(path)
    on.exit(unlink(path, recursive = TRUE), add = TRUE)
    fixture <- list(dataset = "mouse brain")
    saveRDS(fixture, file.path(path, "striatum.dhb.data.RDS"))
    observed <- SpaMTPData(
        "mouse_brain_dhb_striatum",
        local_dir = path,
        offline = TRUE
    )
    expect_identical(observed, fixture)
})

test_that("metadata lookup never downloads a resource", {
    metadata <- SpaMTPData("simulated_xenium", metadata = TRUE)
    expect_identical(metadata$category, "simulated_multiomics")
    expect_identical(metadata$dispatch_class, "Rds")
})

test_that("source fallback verifies and reuses its cache", {
    source_dir <- tempfile("spamtpdata-source-")
    cache_dir <- tempfile("spamtpdata-cache-")
    dir.create(source_dir)
    dir.create(cache_dir)
    on.exit(unlink(c(source_dir, cache_dir), recursive = TRUE), add = TRUE)

    source_file <- file.path(source_dir, "fixture.rds")
    saveRDS(list(value = 42), source_file)
    row <- data.frame(
        resource = "fixture",
        file_name = "fixture.rds",
        location_prefix = paste0("file://", normalizePath(source_dir), "/"),
        rdata_path = "fixture.rds",
        bytes = file.info(source_file)$size,
        md5 = unname(tools::md5sum(source_file)),
        stringsAsFactors = FALSE
    )

    first <- .spamtpdata_download(row, cache_dir = cache_dir, retries = 1L)
    second <- .spamtpdata_download(row, cache_dir = cache_dir, retries = 1L)
    expect_identical(first, second)
    expect_true(.spamtpdata_file_valid(second, row))
})
