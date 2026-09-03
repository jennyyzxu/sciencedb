skip("Entire test suite pending meeting updates")

# Correct Path (Test Output)
# Look into file path and check if file path exists
# maybe a list of files in paths and check
# cache, compare fingerprints
test_that("sdb_download returns a character string representing file path", {
  file.path <- tempdir()
  result <- sdb_download(id = "10.3886/icpsr02760.v18", path = file.path)
  expect_equal(result, file.path)
  expect_type(result, "character")
})

# Wrong Path (Test Input)
# Other potential input: A list of DOI
# Directory path OR file path
# tempfile path?
# User should pick the file name so not colliding with existing one
# what if file already exists in their files? error, overwrite (but document), warning (but skip)
test_that("sdb_download returns an error if invalid input", {
  expect_error(sdb_download(id = NULL, path = tempdir()))
  expect_error(sdb_download(id = "", path = tempdir()))
  expect_error(sdb_download(id = "title", path = tempdir()))
  expect_error(sdb_download(id = "10.3886/icpsr02760.v18", path = NULL))
  expect_error(sdb_download(id = "10.3886/icpsr02760.v18", path = ""))
})
