# =========================================================================
# sdb_keyword
# =========================================================================
# Happy Path 1: Successful API request
test_that(".sdb_keyword returns valid JSON response", {
  mocked_response <- list(
    data = list(
      data = list(
        list(
          titleEn = "Relationship between Salmon and Potato",
          titleZh = "三文鱼与土豆之间的关系",
          author = list(
            list(nameEn = "Jenny"),
            list(nameEn = "Chris")
          ),
          keywordEn = list("fish", "potato", "dinner"),
          doi = "10.12345/sciencedb.67890",
          cstr = "33333.22.sciencedb.67890",
          visit = "5604",
          dataSetPublishDate = "2023-03-08T05:55:30.800Z",
          download = "0",
          taxonomy = list(
            list(nameEn = "Nutrition"),
            list(nameEn = "Agriculture")
          ),
          introductionEn = paste0(
            "<p> This is a dataset about ",
            "salmon and mashed potato </p>"
          ),
          introductionZh = "<p>这是一个三文鱼土豆泥测试数据集。</p>",
          shareStatus = "PUBLIC"
        )
      )
    )
  )

  httr2::with_mocked_responses(
    mock = function(req) {
      httr2::response_json(
        status_code = 200L,
        body = mocked_response
      )
    },
    code = {
      result <- .sdb_keyword(
        keyword = "test-keyword",
        user_agent = "test-agent",
        joint_cookies = "test-cookies"
      )

      # Returned object is R list
      expect_type(result, "list")

      # Entire returned object matches expected JSON
      expect_equal(result, mocked_response)

      # Nested dataset field contains exactly one record
      expect_length(result$data$data, 1)
    }
  )
})

# Wrong Path 1: HTTP Error Code
test_that(".sdb_keyword returns error status code", {
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

  for (status in error_codes) {
    httr2::with_mocked_responses(
      mock = function(req) {
        httr2::response(status_code = status)
      },
      code = expect_error(
        .sdb_keyword(
          keyword = "attachment",
          user_agent = "test_agent",
          joint_cookies = "acw_tc=hello123; cdn_sec_tc=goodbye456"
        ),
        paste0("HTTP ", status),
        info = paste("Failed for HTTP status:", status)
      )
    )
  }
})

# Wrong Path 2: Non-Json Response Body
test_that(".sdb_keyword rejects non-JSON response", {
  httr2::with_mocked_responses(
    mock = function(req) {
      httr2::response(
        status_code = 200L,
        headers = c(
          "server: Tengine",
          "content-type: text/html; charset=utf-8",
          "set-cookie: acw_tc=hello123;path=/;HttpOnly;Max-Age=3600",
          "set-cookie: cdn_sec_tc=goodbye456;path=/;HttpOnly;Max-Age=3600"
        ),
        body = charToRaw(
          "<html><body>ScienceDB is under maintenance</body></html>"
        )
      )
    },
    code = {
      expect_error(
        .sdb_keyword(
          keyword = "attachment",
          user_agent = "test_agent",
          joint_cookies = "acw_tc=hello123; cdn_sec_tc=goodbye456"
        ),
        paste0(
          "ScienceDB returned an invalid JSON response; ",
          "<html><body>ScienceDB is under maintenance</body></html>"
        ),
        fixed = TRUE
      )
    }
  )
})

# Wrong Path 3: Missing nested data$data field
test_that(".sdb_keyword rejects missing nested dataset data", {
  httr2::with_mocked_responses(
    mock = function(req) {
      httr2::response_json(
        status_code = 200L,
        body = list(
          data = list(message = "Results temporarily unavailable")
        )
      )
    },
    code = {
      expect_error(
        .sdb_keyword(
          keyword = "attachment",
          user_agent = "test_agent",
          joint_cookies = "acw_tc=hello123; cdn_sec_tc=goodbye456"
        ),
        "The dataset field data$data is missing or invalid",
        fixed = TRUE
      )
    }
  )
})

# =========================================================================
# sdb_clean_html
# =========================================================================
# Happy Path 1: Convert Missing Input to NA
test_that(".sdb_clean_html converts missing input to NA", {
  expect_identical(.sdb_clean_html(NULL), NA_character_)
  expect_identical(.sdb_clean_html(character(0)), NA_character_)
})

# Happy Path 2: Remove HTML tags and trailing whtiespaces
test_that(".sdb_clean_html removes HTML tags and whitespaces", {
  inputs <- c(
    "<p>Salmon and mashed potato</p>",
    "<div><b>Psychology and stress<b></div>",
    "  Personality and Stress  ",
    "this is a dataset about attachement and stress."
  )

  expected <- c(
    "Salmon and mashed potato",
    "Psychology and stress",
    "Personality and Stress",
    "this is a dataset about attachement and stress."
  )

  expect_equal(.sdb_clean_html(inputs), expected)
})

# =========================================================================
# sdb_extract
# =========================================================================
# Happy Path 1: Successful Metadata Extraction
test_that(".sdb_extract successful extracts metadata from list", {
  mocked_datasets <- list(
    list(
      titleEn = "Relationship between Salmon and Potato",
      titleZh = "三文鱼与土豆之间的关系",
      author = list(
        list(nameEn = "Jenny"),
        list(nameEn = "Chris")
      ),
      keywordEn = list("fish", "potato", "dinner"),
      doi = "10.12345/sciencedb.67890",
      cstr = "33333.22.sciencedb.67890",
      visit = "5604",
      dataSetPublishDate = "2023-03-08T05:55:30.800Z",
      download = "0",
      taxonomy = list(
        list(nameEn = "Nutrition"),
        list(nameEn = "Agriculture")
      ),
      introductionEn = paste0(
        "<p> This is a dataset about ",
        "salmon and mashed potato </p>"
      ),
      introductionZh = "<p>这是一个三文鱼土豆泥测试数据集。</p>",
      shareStatus = "PUBLIC"
    )
  )

  expect_equal(
    .sdb_extract(mocked_datasets, "titleEn",
      optional_field = "titleZh"
    ),
    "Relationship between Salmon and Potato"
  )

  expect_equal(
    .sdb_extract(mocked_datasets, "author",
      sub_key = "nameEn"
    ),
    "Jenny; Chris"
  )

  expect_equal(
    .sdb_extract(mocked_datasets, "keywordEn"),
    "fish; potato; dinner"
  )

  expect_equal(
    .sdb_extract(mocked_datasets, "dataSetPublishDate"),
    "2023-03-08T05:55:30.800Z"
  )

  expect_equal(
    .sdb_extract(mocked_datasets, "doi"),
    "10.12345/sciencedb.67890"
  )

  expect_equal(
    .sdb_extract(mocked_datasets, "taxonomy",
      sub_key = "nameEn"
    ),
    "Nutrition; Agriculture"
  )

  expect_equal(
    .sdb_extract(mocked_datasets, "introductionEn",
      optional_field = "introductionZh"
    ),
    paste0(
      "This is a dataset about ",
      "salmon and mashed potato"
    )
  )

  expect_equal(
    .sdb_extract(mocked_datasets, "shareStatus"),
    "PUBLIC"
  )

  expect_equal(
    .sdb_extract(mocked_datasets, "visit"),
    "5604"
  )

  expect_equal(
    .sdb_extract(mocked_datasets, "download"),
    "0"
  )

  expect_equal(
    .sdb_extract(mocked_datasets, "cstr"),
    "33333.22.sciencedb.67890"
  )
  expect_equal(
    .sdb_extract(mocked_datasets, "language"),
    NA_character_
  )
})

# =========================================================================
# sdb_dataframe
# =========================================================================
test_that(".sdb_dataframe successfully asseembles metadata", {
  mocked_datasets <- list(
    list(
      titleEn = NULL,
      titleZh = "三文鱼与土豆之间的关系",
      author = list(
        list(nameEn = "Jenny"),
        list(nameEn = "Chris")
      ),
      keywordEn = list("fish", "potato", "dinner"),
      taxonomy = list(
        list(nameEn = "Nutrition"),
        list(nameEn = "Agriculture")
      ),
      introductionEn = NULL,
      introductionZh = "<p>这是一个三文鱼土豆泥测试数据集。</p>"
    )
  )

  result <- .sdb_dataframe(mocked_datasets)

  # Returns expected structure
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 12)

  # Successful English - Chinese title/intro replacement
  expect_equal(
    result$Title[1],
    "三文鱼与土豆之间的关系"
  )

  # Successfully removing HTML sign
  expect_equal(
    result$Introduction,
    "这是一个三文鱼土豆泥测试数据集。"
  )

  # Successful handling missing metadata
  missing <- c(
    "Publish", "Doi", "Language",
    "Access", "Views", "Downloads", "Cstr"
  )
  for (field in missing) {
    expect_true(is.na(result[[field]]))
  }

  # Successfully collapsing multiple authors/taxonomy/keywords
  expect_equal(
    result$Author,
    "Jenny; Chris"
  )

  expect_equal(
    result$Keyword,
    "fish; potato; dinner"
  )

  expect_equal(
    result$Taxonomy,
    "Nutrition; Agriculture"
  )
})
