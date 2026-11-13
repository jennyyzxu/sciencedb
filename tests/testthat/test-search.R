### Helper function
expect_valid <- function(result, min_row = 1) {
  # Is result a data frame?
  expect_s3_class(result, "data.frame")

  # Does data frame contain 12 columns with exact names?
  columns <- c(
    "Title", "Author", "Keyword", "Publish", "Doi",
    "Taxonomy", "Language", "Introduction", "Access",
    "Views", "Downloads", "Cstr"
  )
  expect_named(result, columns)

  # Is every column character type?
  character <- vapply(result, is.character, logical(1))
  expect_true(all(character))

  # Is row count equal to 0?
  if (min_row > 0) {
    expect_gt(nrow(result), 0)
  } else {
    expect_equal(nrow(result), 0)
  }
}

# =========================================================================
# Happy Paths :)
# =========================================================================
# 1. Valid Query
test_that("sdb_search returns expected dataframe for valid keyword", {
  # Standard, Multiple, Special, Chinese
  keywords <- c(
    "stress",
    "ecological momentary assessment",
    "climate & psychology",
    "心理学"
  )

  # Initialize an empty vector
  received_keywords <- character()

  # Mocked Keyword Search API Request
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

  local_mocked_bindings(
    .sdb_cookie = function(user_agent) {
      "test-cookies"
    },
    .sdb_keyword = function(keyword, user_agent, joint_cookies) {
      received_keywords <<- c(received_keywords, keyword)
      mocked_response
    }
  )

  for (keyword in keywords) {
    result <- sdb_search(keyword = keyword)
    expect_valid(result)
  }

  # Verify every keyword was passed correctly
  expect_equal(received_keywords, keywords)
})

# 2. No Results Query
test_that("sdb_search returns empty data frame with no results", {
  mocked_empty_response <- list(
    data = list(
      data = list()
    )
  )

  local_mocked_bindings(
    .sdb_cookie = function(user_agent) {
      "test-cookies"
    },
    .sdb_keyword = function(keyword, user_agent, joint_cookies) {
      mocked_empty_response
    }
  )

  result <- sdb_search(keyword = "no-result-query")
  expect_valid(result, min_row = 0)
})

# =========================================================================
# Wrong Paths :(
# =========================================================================
# Wrong Path 1: Reject invalid input before API requests
test_that("sdb_search rejects invalid keyword before any API requests", {
  api_calls <- 0L

  local_mocked_bindings(
    .sdb_cookie = function(user_agent) {
      api_calls <<- api_calls + 1L
      "test-cookies"
    },
    .sdb_keyword = function(keyword, user_agent, joint_cookies) {
      stop("Unexpected API call")
    }
  )

  expect_error(
    sdb_search(keyword = ""),
    paste0(
      "Invalid input. ",
      "Input must be a single non-empty character string."
    )
  )
  expect_error(
    sdb_search(keyword = NULL),
    paste0(
      "Invalid input. ",
      "Input must be a single non-empty character string."
    )
  )
  expect_equal(api_calls, 0L)
})
