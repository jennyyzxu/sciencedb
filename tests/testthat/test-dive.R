# =========================================================================
# Happy Path :)
# =========================================================================
# Happy Path 1: Successfully return cleaned metadata dataframe
test_that("sdb_dive returns cleaned metadata by default", {
  doi <- "10.57760/sciencedb.28090"
  mocked_rawinfo <- readRDS(paste0(
    "~/Documents/sciencedb/tests/testthat",
    "/fixtures/sdb_dive_mocked_rawinfo.rds"
  ))

  local_mocked_bindings(
    .sdb_cookie = function(user_agent) {
      "test-cookie"
    },
    .sdb_datasetid = function(doi, user_agent, joint_cookies) {
      "test-datasetid"
    },
    .sdb_rawinfo = function(dataset_id, user_agent, joint_cookies) {
      mocked_rawinfo
    }
  )

  result <- sdb_dive(doi = doi, raw = FALSE)

  # Verify returned structure
  expect_s3_class(result, "data.frame")

  # Verify several cleaned values
  expect_equal(result$id, "68c8ca4c26b8ca4876f6e5c7")
  expect_equal(result$dataset_id, "f1eb696bfb3445c8b18f1c42fb0fec0e")
  expect_equal(result$dataset_type, "personal")
  expect_equal(result$title_en, paste0(
    "Research on the impact of childhood ",
    "stress on exercise commitment ",
    "in adulthood"
  ))
  expect_equal(result$doi, doi)
})

# Happy Path 2: Successfully return raw metadata by request
test_that("sdb_dive returns raw metadata by request", {
  doi <- "10.57760/sciencedb.28090"
  mocked_rawinfo <- readRDS(paste0(
    "~/Documents/sciencedb/tests/testthat",
    "/fixtures/sdb_dive_mocked_rawinfo.rds"
  ))

  local_mocked_bindings(
    .sdb_cookie = function(user_agent) {
      "test-cookie"
    },
    .sdb_datasetid = function(doi, user_agent, joint_cookies) {
      "test-datasetid"
    },
    .sdb_rawinfo = function(dataset_id, user_agent, joint_cookies) {
      mocked_rawinfo
    },
    .sdb_clean = function(raw_info) {
      stop("Cleaning Function should not be called")
    }
  )

  result <- sdb_dive(doi = doi, raw = TRUE)

  # Verify original metadata is preserved
  expect_identical(result, mocked_rawinfo)
})

# =========================================================================
# Wrong Path :(
# =========================================================================
test_that("sdb_search rejects invalid keyword before any API requests", {
  api_calls <- 0L

  local_mocked_bindings(
    .sdb_cookie = function(user_agent) {
      api_calls <<- api_calls + 1L
      stop("Cookie Function should not be called")
    },
    .sdb_datasetid = function(doi, user_agent, joint_cookies) {
      api_calls <<- api_calls + 1L
      stop("DatasetID Function should not be called")
    },
    .sdb_rawinfo = function(dataset_id, user_agent, joint_cookies) {
      api_calls <<- api_calls + 1L
      stop("Rawinfo Function should not be called")
    },
    .sdb_clean = function(raw_info) {
      api_calls <<- api_calls + 1L
      stop("Cleaning Function should not be called")
    }
  )

  expect_error(
    sdb_dive(doi = ""),
    paste0(
      "Invalid input. ",
      "Input must be a single non-empty character string."
    )
  )
  expect_error(
    sdb_dive(doi = NULL),
    paste0(
      "Invalid input. ",
      "Input must be a single non-empty character string."
    )
  )
  expect_equal(api_calls, 0L)
})
