# =========================================================================
# Helper Function for Testing
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

expect_valid_dive <- function(result) {
  # Is result a list?
  expect_type(result, "list")

  # Is result a list with multiple elements?
  expect_gt(length(result), 0)

  # Is result a nested list?
  expect_type(result[[1]], "list")

  # Does list contain key information? (Maybe try different queries)
  # What if a different dataset have a different set of keys?
  key <- c(
    "id", "dataSetId", "dataSetType", "dataSetTypeCode", "onLineUrl",
    "titleZh", "titleEn", "author", "taxonomy", "keywordEn",
    "introductionEn", "doi", "version", "status", "fileStatus",
    "dataSetCreateDate", "dataSetUpdateDate", "dataSetPublishDate",
    "language", "publisher", "shareStatus"
  )

  result_key <- names(result[[1]])
  for (k in key) {
    expect_true(k %in% result_key)
  }
}
# =========================================================================
# sdb_validate_input
# =========================================================================
# Wrong Path 1: Invalid User Inputs
test_that(".sdb_validate_input rejects invalid inputs", {
  invalid_inputs <- list(
    na_logical = NA,
    na_char = NA_character_,
    vector = c("10.57760/sciencedb.15594", "10.57760/sciencedb.psych.00291"),
    number = 12345,
    boolean = TRUE,
    list = list(
      "10.57760/sciencedb.15594", "10.57760/sciencedb.psych.00291",
      "10.57760/sciencedb.07525"
    ),
    df = data.frame("10.57760/sciencedb.15594"),
    factor = factor("10.57760/sciencedb.15594"),
    empty_vector = character(0),
    empty_string = "",
    whitespace = " "
  )

  for (input in invalid_inputs) {
    expect_error(
      .sdb_validate_input(input),
      paste0(
        "Invalid input. ",
        "Input must be a single non-empty character string."
      )
    )
  }
})
# =========================================================================
# sdb_cookie
# =========================================================================
# Happy Path 1: Standard
test_that(".sdb_cookie extracts required cookies", {
  httr2::with_mocked_responses(
    mock = function(req) {
      httr2::response(
        status_code = 200L,
        headers = c(
          "server: Tengine",
          "content-type: text/html; charset=utf-8",
          "set-cookie: acw_tc=hello123;path=/;HttpOnly;Max-Age=3600",
          "set-cookie: cdn_sec_tc=goodbye456;path=/;HttpOnly;Max-Age=3600"
        )
      )
    },
    code = {
      expect_identical(
        .sdb_cookie(user_agent = "test_agent"),
        "acw_tc=hello123; cdn_sec_tc=goodbye456"
      )
    }
  )
})
# Happy Path 2: Reordered
test_that(".sdb_cookie handles reordered cookie headers", {
  httr2::with_mocked_responses(
    mock = function(req) {
      httr2::response(
        status_code = 200L,
        headers = c(
          "set-cookie: cdn_sec_tc=goodbye456;path=/;HttpOnly;Max-Age=3600",
          "set-cookie: acw_tc=hello123;path=/;HttpOnly;Max-Age=3600",
          "content-type: text/html; charset=utf-8",
          "server: Tengine"
        )
      )
    },
    code = {
      expect_identical(
        .sdb_cookie(user_agent = "test_agent"),
        "acw_tc=hello123; cdn_sec_tc=goodbye456"
      )
    }
  )
})
# Wrong Path 1: HTTP Error in Initial Cookie Request
test_that(".sdb_cookie returns error status code", {
  expect_http_errors(function() {
    .sdb_cookie(user_agent = "test_agent")
  })
})

# Wrong Path 2: Missing cookies
test_that(".sdb_cookie rejects missing cookie headers", {
  httr2::with_mocked_responses(
    mock = function(req) {
      httr2::response(
        status_code = 200L,
        headers = c(
          "server: Tengine",
          "content-type: text/html; charset=utf-8"
        )
      )
    },
    code = {
      expect_error(
        .sdb_cookie(user_agent = "test_agent"),
        "ScienceDB did not return any cookies"
      )
    }
  )
})
# Wrong Path 3: Incomplete ACW cookie
test_that(".sdb_cookie rejects incomplete ACW cookie headers", {
  httr2::with_mocked_responses(
    mock = function(req) {
      httr2::response(
        status_code = 200L,
        headers = c(
          "server: Tengine",
          "content-type: text/html; charset=utf-8",
          "set-cookie: acw_tc=;path=/;HttpOnly;Max-Age=3600",
          "set-cookie: cdn_sec_tc=hello123;path=/;HttpOnly;Max-Age=3600"
        )
      )
    },
    code = {
      expect_error(
        .sdb_cookie(user_agent = "test_agent"),
        "ScienceDB did not return a valid ACW cookie"
      )
    }
  )
})
# Wrong Path 4: Incomplete CDN cookie
test_that(".sdb_cookie rejects incomplete CDN cookie headers", {
  httr2::with_mocked_responses(
    mock = function(req) {
      httr2::response(
        status_code = 200L,
        headers = c(
          "server: Tengine",
          "content-type: text/html; charset=utf-8",
          "set-cookie: acw_tc=hello123;path=/;HttpOnly;Max-Age=3600",
          "set-cookie: cdn_sec_tc=;path=/;HttpOnly;Max-Age=3600"
        )
      )
    },
    code = {
      expect_error(
        .sdb_cookie(user_agent = "test_agent"),
        "ScienceDB did not return a valid CDN cookie"
      )
    }
  )
})
# =========================================================================
# sdb_datasetid
# =========================================================================
# Happy Path 1: Successful DOI --> dataset ID translation
test_that(".sdb_datasetid translates DOI into datasetID", {
  httr2::with_mocked_responses(
    mock = function(req) {
      httr2::response(
        status_code = 302L,
        headers = c(
          paste0(
            "location: ",
            "https://www.scidb.cn/detail?",
            "dataSetId=709f48315df2446d9f1f481fa6740f37"
          )
        )
      )
    },
    code = {
      expect_identical(
        .sdb_datasetid(
          doi = "test_doi",
          user_agent = "test_agent",
          joint_cookies = "test_cookies"
        ),
        "709f48315df2446d9f1f481fa6740f37"
      )
    }
  )
})
# Wrong Path 1: HTTP Error in DOI to DatasetID Request
test_that(".sdb_datasetid returns error status code", {
  expect_http_errors(function() {
    .sdb_datasetid(
      doi = "test_doi",
      user_agent = "test_agent",
      joint_cookies = "test_cookies"
    )
  })
})

# Wrong Path 2: Missing Destination URL
test_that(".sdb_datasetid rejects a redirect without Location header", {
  httr2::with_mocked_responses(
    mock = function(req) httr2::response(status_code = 302L),
    code = {
      expect_error(
        .sdb_datasetid(
          doi = "test_doi",
          user_agent = "test_agent",
          joint_cookies = "test_cookies"
        ),
        paste0(
          "Could not resolve DOI: ",
          "the DOI service did not provide a destination URL."
        )
      )
    }
  )
})

# Wrong Path 3: Destination URL exists BUT datasetID is missing or empty
test_that(".sdb_datasetid rejects a destination without a usable datasetID", {
  locations <- c(
    missing = "https://www.scidb.cn/detail?potato=goodcarb",
    empty = "https://www.scidb.cn/detail?dataSetId="
  )

  for (case in locations) {
    httr2::with_mocked_responses(
      mock = function(req) {
        httr2::response(
          status_code = 302,
          headers = paste0("location:", case)
        )
      },
      code = {
        expect_error(
          .sdb_datasetid(
            doi = "test_doi",
            user_agent = "test_agent",
            joint_cookies = "test_cookies"
          ),
          paste0(
            "Could not resolve DOI: ",
            "The destination URL did not contain a dataset ID."
          )
        )
      }
    )
  }
})
# =========================================================================
# sdb_rawinfo
# =========================================================================
# Happy Path 1: Successful Metadata Extraction
test_that(".sdb_rawinfo extracts metadata from the API response", {
  rawinfo_response <- readRDS(
    test_path("fixtures", "sdb_rawinfo_internal.rds")
  )
  httr2::with_mocked_responses(
    mock = list(rawinfo_response),
    code = {
      raw_info <- .sdb_rawinfo(
        dataset_id = "test_datasetID",
        user_agent = "test_agent",
        joint_cookies = "test_cookies"
      )

      expect_valid_dive(raw_info)
    }
  )
})

# Wrong Path 1: Invalid JSON response
test_that(".sdb_rawinfo rejects non-JSON response", {
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
        .sdb_rawinfo(
          dataset_id = "test_id",
          user_agent = "test_agent",
          joint_cookies = "test_cookies"
        )
      )
    }
  )
})
# Wrong Path 2: Valid JSON but data is missing
test_that(".sdb_rawinfo rejects non-JSON response", {
  httr2::with_mocked_responses(
    mock = function(req) {
      httr2::response(
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
        .sdb_rawinfo(
          dataset_id = "test_id",
          user_agent = "test_agent",
          joint_cookies = "test_cookies"
        )
      )
    }
  )
})
# Wrong Path 3: Valid JSON and data but data$list is missing
test_that(".sdb_rawinfo rejects non-JSON response", {
  httr2::with_mocked_responses(
    mock = function(req) {
      httr2::response(
        status_code = 200L,
        body = list(
          code = 20000,
          message = "成功",
          messageEn = "success",
          data = list()
        )
      )
    },
    code = {
      expect_error(
        .sdb_rawinfo(
          dataset_id = "test_id",
          user_agent = "test_agent",
          joint_cookies = "test_cookies"
        )
      )
    }
  )
})
