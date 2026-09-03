skip("Entire test suite pending meeting updates")

# Happy Path
test_that("sdb_search returns expected dataframe of dataset title and ID", {
  result <- sdb_search(keyword = "stress", display_pages = 1, display_size = 10)
  expect_s3_class(result, "data.frame")
  expected_fields <- c("title", "doi", "cstr")
  expect_true(all(expected_fields %in% colnames(result)))
})

test_that("sdb_search returns expected value given string integer",{
  result <- sdb_search(keyword = "stress", display_pages = "1", display_size = "10")
  expect_s3_class(result, "data.frame")
  expected_fields <- c("title", "doi", "cstr")
  expect_true(all(expected_fields %in% colnames(result)))
})

# Wrong Path
test_that("sdb_search returns an error if invalid input", {
  expect_error(sdb_search(keyword = NULL, display_pages = 1, display_size = 10))
  expect_error(sdb_search(keyword = "",display_pages = 1, display_size = 10))

  expect_error(sdb_search(keyword = "stress", display_pages = 0, display_size = 10))
  expect_error(sdb_search(keyword = "stress", display_pages = -1, display_size = 10))
  expect_error(sdb_search(keyword = "stress", display_pages = NULL, display_size = 10))
  expect_error(sdb_search(keyword = "stress", display_pages = 1.23, display_size = 10))
  expect_error(sdb_search(keyword = "stress", display_pages = "stress", display_size = NULL))

  expect_error(sdb_search(keyword = "stress", display_pages = 0, display_size = 0))
  expect_error(sdb_search(keyword = "stress", display_pages = -1, display_size = -1))
  expect_error(sdb_search(keyword = "stress", display_pages = 1, display_size = NULL))
  expect_error(sdb_search(keyword = "stress", display_pages = 1, display_size = 5.67))
  expect_error(sdb_search(keyword = "stress", display_pages = 1, display_size = "happy"))
})
