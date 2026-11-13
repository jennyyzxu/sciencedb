# =========================================================================
# Helper function
# =========================================================================
error_codes <- c(
  400L, # Bad request: the server considers request invalid
  401L, # Unauthorized: authentication is missing or invalid
  403L, # Forbidden: the server understands but refuses the request
  404L, # Not found: the requested endpoint does not exist
  429L, # Too many requests: the client has exceeded a rate limit
  500L, # Internal server error: an unexpected server-side failure occurred
  502L, # Bad gateway: an upstream server returned an invalid response
  503L, # Service unavailable: the service is temporarily unavailable
  504L
) # Gateway timeout: an upstream server did not respond in time

expect_http_errors <- function(run_function) {
  for (status in error_codes) {
    httr2::with_mocked_responses(
      mock = function(req) {
        httr2::response(status_code = status)
      },
      code = expect_error(
        run_function(),
        paste0("HTTP ", status)
      )
    )
  }
}
# =========================================================================
# sdb_filetree
# =========================================================================
# Happy Path 1: Single top-level file
test_that(".sdb_filetree identifies a top-level file", {
  httr2::with_mocked_responses(
    mock = function(req) {
      httr2::response_json(
        status_code = 200,
        body = list(
          data = list(
            list(
              id = "file1",
              path = "/V1/data.csv",
              fileName = "data.csv",
              dir = FALSE
            )
          )
        )
      )
    },
    code = {
      result <- .sdb_filetree(
        user_agent = "test-agent",
        joint_cookies = "test-cookies",
        dataset_id = "test-datasetID",
        ver = "V1",
        path = "/V1"
      )

      expect_equal(
        result,
        list(
          list(
            file_id = "file1",
            file_path = "/V1/data.csv",
            file_name = "data.csv"
          )
        )
      )
    }
  )
})

# Happy Path 2: Multiple top-level files
test_that(".sdb_filetree identifies multiple top-level files", {
  httr2::with_mocked_responses(
    mock = function(req) {
      httr2::response_json(
        status_code = 200,
        body = list(
          data = list(
            list(
              id = "file1",
              path = "/V1/data.csv",
              fileName = "data.csv",
              dir = FALSE
            ),
            list(
              id = "file2",
              path = "/V1/code.R",
              fileName = "code.R",
              dir = FALSE
            ),
            list(
              id = "file3",
              path = "/V1/readme.txt",
              fileName = "readme.txt",
              dir = FALSE
            )
          )
        )
      )
    },
    code = {
      result <- .sdb_filetree(
        user_agent = "test-agent",
        joint_cookies = "test-cookies",
        dataset_id = "test-datasetID",
        ver = "V1",
        path = "/V1"
      )

      expect_equal(
        result,
        list(
          list(
            file_id = "file1",
            file_path = "/V1/data.csv",
            file_name = "data.csv"
          ),
          list(
            file_id = "file2",
            file_path = "/V1/code.R",
            file_name = "code.R"
          ),
          list(
            file_id = "file3",
            file_path = "/V1/readme.txt",
            file_name = "readme.txt"
          )
        )
      )
    }
  )
})

# Happy Path 3: A folder with one file
test_that(".sdb_filetree identifies a folder and a file", {
  num_request <- 0

  httr2::with_mocked_responses(
    mock = function(req) {
      num_request <<- num_request + 1

      if (num_request == 1) {
        httr2::response_json(
          status_code = 200,
          body = list(
            data = list(
              list(
                id = "folder1",
                path = "/V1/subfolder",
                fileName = "subfolder",
                dir = TRUE
              )
            )
          )
        )
      } else {
        httr2::response_json(
          status_code = 200,
          body = list(
            data = list(
              list(
                id = "file1",
                path = "/V1/subfolder/data.csv",
                fileName = "data.csv",
                dir = FALSE
              )
            )
          )
        )
      }
    },
    code = {
      result <- .sdb_filetree(
        user_agent = "test-agent",
        joint_cookies = "test-cookies",
        dataset_id = "test-datasetID",
        ver = "V1",
        path = "/V1"
      )

      expect_equal(
        result,
        list(
          list(
            file_id = "file1",
            file_path = "/V1/subfolder/data.csv",
            file_name = "data.csv"
          )
        )
      )
    }
  )
})

# Happy Path 4: One folder with multiple files
test_that(".sdb_filetree identifies a folder and multiple files", {
  num_request <- 0

  httr2::with_mocked_responses(
    mock = function(req) {
      num_request <<- num_request + 1

      if (num_request == 1) {
        httr2::response_json(
          status_code = 200,
          body = list(
            data = list(
              list(
                id = "folder1",
                path = "/V1/subfolder",
                fileName = "subfolder",
                dir = TRUE
              )
            )
          )
        )
      } else {
        httr2::response_json(
          status_code = 200,
          body = list(
            data = list(
              list(
                id = "file1",
                path = "/V1/subfolder/data.csv",
                fileName = "data.csv",
                dir = FALSE
              ),
              list(
                id = "file2",
                path = "/V1/subfolder/code.R",
                fileName = "code.R",
                dir = FALSE
              ),
              list(
                id = "file3",
                path = "/V1/subfolder/readme.txt",
                fileName = "readme.txt",
                dir = FALSE
              )
            )
          )
        )
      }
    },
    code = {
      result <- .sdb_filetree(
        user_agent = "test-agent",
        joint_cookies = "test-cookies",
        dataset_id = "test-datasetID",
        ver = "V1",
        path = "/V1"
      )

      expect_equal(
        result,
        list(
          list(
            file_id = "file1",
            file_path = "/V1/subfolder/data.csv",
            file_name = "data.csv"
          ),
          list(
            file_id = "file2",
            file_path = "/V1/subfolder/code.R",
            file_name = "code.R"
          ),
          list(
            file_id = "file3",
            file_path = "/V1/subfolder/readme.txt",
            file_name = "readme.txt"
          )
        )
      )
    }
  )
})

# Happy Path 5: Multiple folders with multiple files
test_that(".sdb_filetree identifies multiple folders", {
  num_request <- 0

  httr2::with_mocked_responses(
    mock = function(req) {
      num_request <<- num_request + 1

      if (num_request == 1) {
        httr2::response_json(
          status_code = 200,
          body = list(
            data = list(
              list(
                id = "folder1",
                path = "/V1/subfolder1",
                fileName = "subfolder1",
                dir = TRUE
              ),
              list(
                id = "folder2",
                path = "/V1/subfolder2",
                fileName = "subfolder2",
                dir = TRUE
              )
            )
          )
        )
      } else if (num_request == 2) {
        httr2::response_json(
          status_code = 200,
          body = list(
            data = list(
              list(
                id = "file1",
                path = "/V1/subfolder1/data.csv",
                fileName = "data.csv",
                dir = FALSE
              ),
              list(
                id = "file2",
                path = "/V1/subfolder1/code.R",
                fileName = "code.R",
                dir = FALSE
              ),
              list(
                id = "file3",
                path = "/V1/subfolder1/readme.txt",
                fileName = "readme.txt",
                dir = FALSE
              )
            )
          )
        )
      } else {
        httr2::response_json(
          status_code = 200,
          body = list(
            data = list(
              list(
                id = "file4",
                path = "/V1/subfolder2/paper.pdf",
                fileName = "paper.pdf",
                dir = FALSE
              ),
              list(
                id = "file5",
                path = "/V1/subfolder2/video.mp3",
                fileName = "video.mp3",
                dir = FALSE
              )
            )
          )
        )
      }
    },
    code = {
      result <- .sdb_filetree(
        user_agent = "test-agent",
        joint_cookies = "test-cookies",
        dataset_id = "test-datasetID",
        ver = "V1",
        path = "/V1"
      )

      expect_equal(
        result,
        list(
          list(
            file_id = "file1",
            file_path = "/V1/subfolder1/data.csv",
            file_name = "data.csv"
          ),
          list(
            file_id = "file2",
            file_path = "/V1/subfolder1/code.R",
            file_name = "code.R"
          ),
          list(
            file_id = "file3",
            file_path = "/V1/subfolder1/readme.txt",
            file_name = "readme.txt"
          ),
          list(
            file_id = "file4",
            file_path = "/V1/subfolder2/paper.pdf",
            file_name = "paper.pdf"
          ),
          list(
            file_id = "file5",
            file_path = "/V1/subfolder2/video.mp3",
            file_name = "video.mp3"
          )
        )
      )
    }
  )
})

# Happy Path 6: Nested folders
test_that(".sdb_filetree identifies nested folders", {
  num_request <- 0

  httr2::with_mocked_responses(
    mock = function(req) {
      num_request <<- num_request + 1

      if (num_request == 1) {
        httr2::response_json(
          status_code = 200,
          body = list(
            data = list(
              list(
                id = "folder1",
                path = "/V1/folder1",
                fileName = "folder1",
                dir = TRUE
              )
            )
          )
        )
      } else if (num_request == 2) {
        httr2::response_json(
          status_code = 200,
          body = list(
            data = list(
              list(
                id = "file1",
                path = "/V1/folder1/subfolder",
                fileName = "subfolder",
                dir = TRUE
              )
            )
          )
        )
      } else {
        httr2::response_json(
          status_code = 200,
          body = list(
            data = list(
              list(
                id = "file1",
                path = "/V1/folder1/subfolder/paper.pdf",
                fileName = "paper.pdf",
                dir = FALSE
              ),
              list(
                id = "file2",
                path = "/V1/folder1/subfolder/video.mp3",
                fileName = "video.mp3",
                dir = FALSE
              )
            )
          )
        )
      }
    },
    code = {
      result <- .sdb_filetree(
        user_agent = "test-agent",
        joint_cookies = "test-cookies",
        dataset_id = "test-datasetID",
        ver = "V1",
        path = "/V1"
      )

      expect_equal(
        result,
        list(
          list(
            file_id = "file1",
            file_path = "/V1/folder1/subfolder/paper.pdf",
            file_name = "paper.pdf"
          ),
          list(
            file_id = "file2",
            file_path = "/V1/folder1/subfolder/video.mp3",
            file_name = "video.mp3"
          )
        )
      )
    }
  )
})

# Wrong Path 1: non-JSON response body
test_that(".sdb_filetree rejects non-JSON response", {
  httr2::with_mocked_responses(
    mock = function(req) {
      httr2::response(
        status_code = 200L,
        body = charToRaw(
          "<html><body>ScienceDB is under maintenance</body></html>"
        )
      )
    },
    code = {
      expect_error(
        .sdb_filetree(
          dataset_id = "test_id",
          user_agent = "test_agent",
          joint_cookies = "test_cookies",
          ver = "V1",
          path = "/V1"
        )
      )
    }
  )
})
# Wrong Path 2: Valid JSON with invalid $data
test_that(".sdb_filetree rejects missing data response", {
  httr2::with_mocked_responses(
    mock = function(req) {
      httr2::response_json(
        status_code = 200L,
        body = list(
          code = 20000,
          message = "成功",
          messageEn = "success"
        )
      )
    },
    code = {
      expect_error(
        .sdb_filetree(
          dataset_id = "test_id",
          user_agent = "test_agent",
          joint_cookies = "test_cookies",
          ver = "V1",
          path = "/V1"
        ),
        "ScienceDB file-tree response is missing or contains invalid data."
      )
    }
  )
})
# Wrong Path 3: Valid Json, Valid $data, Invalid terminal file ID
test_that(".sdb_filetree rejects malformed terminal file", {
  httr2::with_mocked_responses(
    mock = function(req) {
      httr2::response_json(
        status_code = 200L,
        body = list(
          code = 20000,
          message = "成功",
          messageEn = "success",
          data = list(
            list(
              id = NULL,
              path = "/V1/data.csv",
              fileName = "data.csv",
              dir = FALSE
            )
          )
        )
      )
    },
    code = {
      expect_error(
        .sdb_filetree(
          dataset_id = "test_id",
          user_agent = "test_agent",
          joint_cookies = "test_cookies",
          ver = "V1",
          path = "/V1"
        ),
        "ScienceDB returned malformed terminal file metadata."
      )
    }
  )
})

# Wrong Path 4: Valid Json, Valid $data, Invalid folder path
test_that(".sdb_filetree rejects invalid folder path", {
  httr2::with_mocked_responses(
    mock = function(req) {
      httr2::response_json(
        status_code = 200L,
        body = list(
          code = 20000,
          message = "成功",
          messageEn = "success",
          data = list(
            list(
              id = "folder1",
              path = NULL,
              fileName = "subfolder",
              dir = TRUE
            )
          )
        )
      )
    },
    code = {
      expect_error(
        .sdb_filetree(
          dataset_id = "test_id",
          user_agent = "test_agent",
          joint_cookies = "test_cookies",
          ver = "V1",
          path = "/V1"
        ),
        "ScienceDB returned malformed folder metadata."
      )
    }
  )
})

# Wrong Path 5. HTTP Error Status Code
test_that(".sdb_filetree returns error status code", {
  expect_http_errors(function() {
    .sdb_filetree(
      user_agent = "test_agent",
      joint_cookies = "test_cookies",
      dataset_id = "test_datasetID",
      ver = "test_version",
      path = "test_path"
    )
  })
})
# =========================================================================
# sdb_rename
# =========================================================================
# Happy Path 1: new file, destination does not exist
test_that(".sdb_rename keeps available file name", {
  temporary_directory <- withr::local_tempdir()
  destination <- file.path(temporary_directory, "data.csv")

  result <- .sdb_rename(destination)

  expect_identical(
    result,
    destination
  )
})

# Happy Path 2: existing file, destination exists
test_that(".sdb_rename renames duplicate file", {
  temporary_directory <- withr::local_tempdir()
  destination <- file.path(temporary_directory, "data.csv")
  file.create(destination)

  result <- .sdb_rename(destination)

  expect_message(
    .sdb_rename(destination),
    "File already exists."
  )

  expect_identical(
    result,
    file.path(temporary_directory, "data(1).csv")
  )
})

# Happy Path 3: two existing files, destination exists
test_that(".sdb_rename renames duplicate file", {
  temporary_directory <- withr::local_tempdir()
  original_destination <- file.path(temporary_directory, "data.csv")
  duplicate_destination <- file.path(temporary_directory, "data(1).csv")

  file.create(original_destination)
  file.create(duplicate_destination)

  expect_message(
    result <- .sdb_rename(original_destination),
    "File already exists\\. Saving as: data\\(2\\)\\.csv"
  )

  expect_identical(
    result,
    file.path(temporary_directory, "data(2).csv")
  )
})

# Happy Path 4: existing file, name without extension (e.g., .csv, .xlsx)
test_that(".sdb_rename handles file names without extensions", {
  temporary_directory <- withr::local_tempdir()
  destination <- file.path(temporary_directory, "README")
  file.create(destination)

  expect_message(
    result <- .sdb_rename(destination),
    "File already exists\\. Saving as: README\\(1\\)"
  )

  expect_identical(
    result,
    file.path(temporary_directory, "README(1)")
  )
})

# Happy Path 5: existing file, name with multiple periods (e.g. final.data.csv)
test_that(".sdb_rename handles file names with multiple periods", {
  temporary_directory <- withr::local_tempdir()
  destination <- file.path(temporary_directory, "final.data.csv")
  file.create(destination)

  expect_message(
    result <- .sdb_rename(destination),
    "File already exists\\. Saving as: final.data\\(1\\)\\.csv"
  )

  expect_identical(
    result,
    file.path(temporary_directory, "final.data(1).csv")
  )
})
# =========================================================================
# sdb_download
# =========================================================================
# Happy Path 1: Downloads one file to intended location
test_that(".sdb_download downloads one file to intended location", {
  temporary_directory <- withr::local_tempdir()
  destination <- file.path(temporary_directory, "data.csv")

  httr2::with_mocked_responses(
    mock = function(req) {
      httr2::response(
        status_code = 200
      )
    },
    code = {
      result <- .sdb_download(
        file_id = "test-file",
        file_path = "test-path",
        file_name = "data.csv",
        dataset_id = "test-datasetID",
        user_agent = "test-agent",
        joint_cookies = "test-cookies",
        destination = destination
      )

      expect_equal(result, destination)
    }
  )
})

# Wrong Path 1. HTTP Error Status Code
test_that(".sdb_download returns error status code", {
  expect_http_errors(function() {
    .sdb_download(
      file_id = "test_id",
      file_path = "test_path",
      file_name = "test_name",
      user_agent = "test_agent",
      joint_cookies = "test_cookies",
      dataset_id = "test_datasetID",
      destination = "test_destination"
    )
  })
})
