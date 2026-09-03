skip("Entire test suite pending meeting updates")

# Happy Path
# check expect equal for lists
test_that("sdb_browse returns expected list of metadata", {
  result1 <- sdb_browse(doi = "10.57760/sciencedb.26336")
  expect_type(result1, "list")

  expected_fields <- c("Title", "Authors", "Keyword", "Publish", "Version",
                       "Doi", "License", "Access", "SizeBytes", "Taxonomy",
                       "Language", "Introduction", "Clicks","ReferenceNum", "Cstr")
  expect_named(result1, expected_fields)

  expected_types <- list(
    Title = "character",
    Authors = "character",
    Keyword = "character",
    Publish = "Date",
    Version = "character",
    Doi = "character",
    License = "character",
    Access = "character",
    SizeBytes = "numeric",
    Taxonomy = "character",
    Language = "character",
    Introduction = "character",
    Clicks = "numeric",
    ReferenceNum = "numeric",
    Cstr = "character"
  )

  for (field in expected_fields) {
    target <- expected_types[[field]]
    value <- result1[[field]]

    if (target == "Date"){
      expect_s3_class(value, "Date")
    } else if (target == "numeric"){
      expect_true(is.numeric(value))
    }else{
      expect_type(value, target)
    }
  }
})

# Wrong Path
test_that("sdb_browse returns an error for invalid id", {
  invalid_input <- list(
    NULL,
    "",
    " ",
    12345,
    c("10.123", "10.456"),
    list("10.3886/icpsr02760.v18"),
    "random_thing"
  )

  for(input in invalid_input){
    expect_error(
      sdb_browse(doi = input)
    )
  }
})
