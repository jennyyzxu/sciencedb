# =========================================================================
# Happy Paths :)
# =========================================================================
# Happy Path 1: Successfuly Integrate all private functions
test_that("sdb_download saves files in a temporary directory", {
  ## Prepare variables for what we expect to see (functions/directory path)
  download <- NULL
  result <- NULL

  ## Clean up temporary directory after test finishes
  withr::defer({
    if (!is.null(result)) {
      unlink(result, recursive = TRUE)
    }
  })

  # Replace private helper functions with mocked responses
  local_mocked_bindings(
    .sdb_cookie = function(user_agent) {
      "test-cookies"
    },
    .sdb_datasetid = function(doi, user_agent, joint_cookies) {
      "test-datasetID"
    },
    .sdb_rawinfo = function(dataset_id, user_agent, joint_cookies) {
      list(
        list(
          version = "V1",
          shareStatus = "PUBLIC"
        )
      )
    },
    .sdb_filetree = function(user_agent, joint_cookies, dataset_id,
                             ver, path) {
      list(
        list(
          file_id = "file1",
          file_path = "/V1/data.csv",
          file_name = "data.csv"
        )
      )
    },
    .sdb_download = function(file_id, file_path, file_name,
                             dataset_id, user_agent, joint_cookies,
                             destination) {
      # Update whatever master function passes
      download <<- list(
        file_id = file_id,
        file_path = file_path,
        file_name = file_name,
        dataset_id = dataset_id,
        destination = destination
      )
    }
  )

  expect_message(
    result <- sdb_download(doi = "test-doi"),
    "Found 1 file\\(s\\) to download."
  )
  expect_true(dir.exists(result))
  expect_equal(download$file_id, "file1")
  expect_equal(download$file_path, "/V1/data.csv")
  expect_equal(download$file_name, "data.csv")
  expect_equal(download$dataset_id, "test-datasetID")
  expect_identical(
    download$destination,
    file.path(result, "data.csv")
  )
})

# Happy Path 2:  zero file
test_that("sdb_download successfully handle dataset with no file", {
  local_mocked_bindings(
    .sdb_cookie = function(user_agent) {
      "test-cookies"
    },
    .sdb_datasetid = function(doi, user_agent, joint_cookies) {
      "test-datasetID"
    },
    .sdb_rawinfo = function(dataset_id, user_agent, joint_cookies) {
      list(
        list(
          version = "V1",
          shareStatus = "PUBLIC"
        )
      )
    },
    .sdb_filetree = function(user_agent, joint_cookies, dataset_id,
                             ver, path) {
      list()
    }
  )

  expect_message(
    result <- sdb_download(doi = "test-doi"),
    "No files found to download for this dataset."
  )
  expect_null(result)
})

# Happy Path 3:  RESTRICTED/EMBARGO Datasets
test_that("sdb_download warns for restricted/embargo share status", {
  result <- NULL

  ## Clean up temporary directory after test finishes
  withr::defer({
    if (!is.null(result)) {
      unlink(result, recursive = TRUE)
    }
  })

  statuses <- c("RESTRICTED", "EMBARGO")

  for (status in statuses) {
    download <- NULL

    local_mocked_bindings(
      .sdb_cookie = function(user_agent) {
        "test-cookies"
      },
      .sdb_datasetid = function(doi, user_agent, joint_cookies) {
        "test-datasetID"
      },
      .sdb_rawinfo = function(dataset_id, user_agent, joint_cookies) {
        list(
          list(
            version = "V1",
            shareStatus = status
          )
        )
      },
      .sdb_filetree = function(user_agent, joint_cookies, dataset_id,
                               ver, path) {
        list(
          list(
            file_id = "file1",
            file_path = "/V1/data.csv",
            file_name = "data.csv"
          )
        )
      },
      .sdb_download = function(file_id, file_path, file_name,
                               dataset_id, user_agent, joint_cookies,
                               destination) {
        # capture whatever master function passes
        download <<- list(
          file_id = file_id,
          file_path = file_path,
          file_name = file_name,
          dataset_id = dataset_id,
          destination = destination
        )
      }
    )

    expect_message(
      result <- sdb_download(doi = "test-doi"),
      paste0(
        "Notice: this dataset contains restricted or ",
        "embargoed files. Some downloads may be ",
        "unavailable."
      )
    )
    expect_true(dir.exists(result))
    expect_equal(download$file_id, "file1")
    expect_equal(download$file_name, "data.csv")
    expect_identical(
      download$destination,
      file.path(result, "data.csv")
    )
  }
})
