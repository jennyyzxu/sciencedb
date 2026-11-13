# =========================================================================
# sdb_scalar
# =========================================================================
# Happy Path 1: Preserve Legitimate Values
test_that(".sdb_scalar preserves legitimate input values", {
  expect_identical(.sdb_scalar("stress"), "stress")
  expect_identical(.sdb_scalar("压力"), "压力")
  expect_identical(.sdb_scalar(0), 0)
  expect_identical(.sdb_scalar(1), 1)
  expect_identical(.sdb_scalar(FALSE), FALSE)
  expect_identical(.sdb_scalar(TRUE), TRUE)
})
# Happy Path 2: Convert NULL, Empty, and White-space only inputs in NA
test_that(".sdb_scalar converts invalid inputs into NA", {
  expect_identical(.sdb_scalar(NULL), NA)
  expect_identical(.sdb_scalar(character(0)), NA)
  expect_identical(.sdb_scalar(numeric(0)), NA)
  expect_identical(.sdb_scalar(logical(0)), NA)
  expect_identical(.sdb_scalar(""), NA_character_)
  expect_identical(.sdb_scalar(" "), NA_character_)
  expect_identical(.sdb_scalar("   "), NA_character_)
  expect_identical(.sdb_scalar("\t"), NA_character_)
  expect_identical(.sdb_scalar(NA), NA)
  expect_identical(.sdb_scalar(NA_character_), NA_character_)
  expect_identical(.sdb_scalar(NA_real_), NA_real_)
  expect_identical(.sdb_scalar(NA_integer_), NA_integer_)
})
# =========================================================================
# sdb_list
# =========================================================================
# Happy path 1: Convert Null/Empty inputs into NA
test_that(".sdb_list converts missing and empty inputs into NA", {
  expect_identical(.sdb_list(NULL), NA_character_)
  expect_identical(.sdb_list(character(0)), NA_character_)
  expect_identical(.sdb_list(list()), NA_character_)
  expect_identical(.sdb_list(list(NULL)), NA_character_)
  expect_identical(.sdb_list(list(NA_character_, "", "   ")),
                   NA_character_)
  expect_identical(.sdb_list(list(" ", "\t")),
                   NA_character_)
})
# Happy Path 2: Flatten list
test_that(".sdb_list flattens and combines multiple values", {
  expect_identical(
    .sdb_list(list("salmon", "potato", "dinner")),
    "salmon; potato; dinner"
  )

  expect_identical(
    .sdb_list(list(c("terriyaki chicken", "potato"), "dinner")),
    "terriyaki chicken; potato; dinner"
  )

  expect_identical(
    .sdb_list(list("food science")),
    "food science"
  )

  expect_identical(
    .sdb_list(list(" poke bowl", NA_integer_, "", "   ", "lunch")),
    "poke bowl; lunch"
  )

  expect_identical(
    .sdb_list(list(0, FALSE, 123)),
    "0; FALSE; 123"
  )

  expect_identical(
    .sdb_list(list("三文鱼", "土豆", "dinner")),
    "三文鱼; 土豆; dinner"
  )
})
# =========================================================================
# sdb_intro
# =========================================================================
# Happy Path 1: Convert missing/empty inputs to NA
test_that(".sdb_intro converts missing and empty inputs to NA", {
  expect_identical(.sdb_intro(NULL), NA_character_)
  expect_identical(.sdb_intro(character(0)), NA_character_)
  expect_identical(.sdb_intro(numeric(0)), NA_character_)
  expect_identical(.sdb_intro(logical(0)), NA_character_)
  expect_identical(.sdb_intro(""), NA_character_)
  expect_identical(.sdb_intro(" "), NA_character_)
  expect_identical(.sdb_intro("   "), NA_character_)
  expect_identical(.sdb_intro("\t"), NA_character_)
})
# Happy Path 2: Trim surrounding white space
test_that(".sdb_intro trims surrounding whitespace", {
  expect_identical(.sdb_intro("This is a dataset about salmon and potato."),
                   "This is a dataset about salmon and potato.")

  expect_identical(.sdb_intro(" 这是关于三文鱼和土豆的数据集。 "),
                   "这是关于三文鱼和土豆的数据集。")

  expect_identical(.sdb_intro(" <p>dinner dataset introduction</p>"),
                   "dinner dataset introduction")

  expect_identical(.sdb_intro("\n<p>晚餐数据集简介</p>\n"),
                   "晚餐数据集简介")
})
# Happy Path 3: Remove HTML tags
test_that(".sdb_intro removes HTML tags", {
  expect_identical(.sdb_intro("<p>how to make a good poke bowl?<p>"),
                   "how to make a good poke bowl?")

  expect_identical(.sdb_intro(paste0("<p>how to make a <strong>good</strong>",
                                     " poke bowl?<p>")),
                   "how to make a good poke bowl?")

  expect_identical(.sdb_intro(paste0("<div><span>怎么做好吃的波奇碗？",
                                     "</span><div>")),
                   "怎么做好吃的波奇碗？")
})
# =========================================================================
# sdb_date
# =========================================================================
# Happy Path 1: Convert missing and empty inputs into POSIXct NA
test_that(".sdb_date converts missing and empty inputs into POSIXct NA", {
  invalid_inputs <- list(NULL, numeric(0), NA,
                         NA_real_, NA_integer_, NA_character_)

  for (input in invalid_inputs){
    result <- .sdb_date(input)
    expect_s3_class(result, "POSIXct")
    expect_length(result, 1)
    expect_true(is.na(result))
  }
})
# Happy Path 2: Convert milliseconds into POSIXct dates
test_that(".sdb_date converts milliseconds into POSIXct dates", {
  expect_identical(.sdb_date(1793318400000),
                   as.POSIXct("2026-10-30 00:00:00", tz = "UTC"))

  expect_identical(.sdb_date(0),
                   as.POSIXct("1970-01-01 00:00:00", tz = "UTC"))

  expect_identical(.sdb_date(1000),
                   as.POSIXct("1970-01-01 00:00:01", tz = "UTC"))
})
# Happy Path 3: Use UTC timezone
test_that(".sdb_date returns timestamps in UTC", {
  result <- .sdb_date(1793318400000)
  expect_identical(attr(result, "tzone"), "UTC")
})
# =========================================================================
# sdb_author
# =========================================================================
# Happy Path 1: Convert missing and empty list into empty data frame
test_that(".sdb_author converts missing and empty lists into empty dataframe", {
  expect_identical(.sdb_author(NULL), data.frame())
  expect_identical(.sdb_author(list()), data.frame())
})
# Happy Path 2: Extract information from a single author
test_that(".sdb_author extracts info from a single author", {
  author <- list(
    nameEn = "Jenny Xu",
    nameZh = "徐珍妮",
    email = "jenny@promptly.org",
    orcId = "0000-0730-2004-6666",
    country_name = list("China"),
    organizations = list(
      list(nameEn = "University of Poke Bowl")
    )
  )

  result <- .sdb_author(list(author))

  expected <- data.frame(
    name_en = "Jenny Xu",
    name_zh = "徐珍妮",
    email = "jenny@promptly.org",
    orcid = "0000-0730-2004-6666",
    country = "China",
    affiliation = "University of Poke Bowl"
  )

  expect_identical(result, expected)
})
# Happy Path 3: Extract information from multiple authors
test_that(".sdb_author extracts info from a single author", {
  authors <- list(
    list(
      nameEn = "Jenny Xu",
      nameZh = "徐珍妮",
      email = "jenny@promptly.org",
      orcId = "0000-0730-2004-6666",
      country_name = list("China"),
      organizations = list(
        list(nameEn = "University of Poke Bowl")
      )
    ),
    list(
      nameEn = "Chris",
      nameZh = "Chris",
      email = "chris@promptly.org",
      orcId = "1234-5678-9012-8888",
      country_name = list("United States"),
      organizations = list(
        list(nameEn = "University of Pizza")
      )
    )
  )

  result <- .sdb_author(authors)

  expect_equal(nrow(result), 2)
  expect_identical(result$name_en, c("Jenny Xu", "Chris"))
  expect_identical(result$name_zh, c("徐珍妮", "Chris"))
  expect_identical(result$email,
                   c("jenny@promptly.org", "chris@promptly.org"))
  expect_identical(result$country, c("China", "United States"))
  expect_identical(result$affiliation,
                   c("University of Poke Bowl",
                     "University of Pizza"))
})
# =========================================================================
# sdb_taxonomy
# =========================================================================
# Happy path 1: convert missing and empty list into empty dataframe
test_that(".sdb_taxonomy handles NULL and empty inputs", {
  expected <- data.frame(
    code = character(),
    name_en = character(),
    name_zh = character()
  )

  expect_identical(.sdb_taxonomy(NULL), expected)
  expect_identical(.sdb_taxonomy(list()), expected)
})

# Happy path 2: Extract Single Taxonomy
test_that(".sdb_taxonomy extracts a single taxonomy entry", {
  taxonomy <- list(
    code = "630",
    nameEn = "Psychology",
    nameZh = "心理学"
  )

  result <- .sdb_taxonomy(list(taxonomy))

  expected <- data.frame(
    code = "630",
    name_en = "Psychology",
    name_zh = "心理学"
  )

  expect_identical(result, expected)
})
# Happy path 3: Extract Multiple Taxonomy
test_that(".sdb_taxonomy combines multiple taxonomy entries", {
  taxonomies <- list(
    list(
      code = "630",
      nameEn = "Psychology",
      nameZh = "心理学"
    ),
    list(
      code = "710",
      nameEn = "Statistics",
      nameZh = "统计学"
    )
  )

  result <- .sdb_taxonomy(taxonomies)

  expected <- data.frame(
    code = c("630", "710"),
    name_en = c("Psychology", "Statistics"),
    name_zh = c("心理学", "统计学")
  )

  expect_identical(result, expected)
})
# =========================================================================
# sdb_clean
# =========================================================================
example_info <- list(
  # Dataset information
  id = 101,
  dataSetId = "dataset-001",

  # Basic information
  titleEn = "Poke Bowl Dataset",
  titleZh = "波奇碗数据",
  keywordEn = list("salmon", "potato"),
  introductionEn = "<p>How to make the best poke bowl?</p>",
  version = "V1",

  # Copyright information
  copyRight = list(
    code = "CC BY"
  ),

  # Date information
  dataSetCreateDate = 1704067200000,

  # Status
  shareStatus = "PUBLIC",

  # Flags
  createFileFlag = FALSE,

  # Author information
  author = list(
    list(
      nameEn = "Jenny Xu",
      nameZh = "徐珍妮",
      email = "jenny@example.org",
      orcId = "0000-0002-1234-5678",
      country_name = list("China"),
      organizations = list(
        list(nameEn = "University of Poke Bowl")
      )
    )
  ),

  # Taxonomy information
  taxonomy = list(
    list(
      code = "630",
      nameEn = "Poke Food Science",
      nameZh = "波奇碗食品科学"
    )
  )
)

# Happy Path 1: Empty raw metadata
test_that(".sdb_clean handles NULL and empty raw metadata", {
  expect_identical(.sdb_clean(NULL), data.frame())
  expect_identical(.sdb_clean(list()), data.frame())
})

# Happy Path 2: Single dataset
test_that(".sdb_clean extracts and cleans a single version", {
  result <- .sdb_clean(list(example_info))

  # Check data frame structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)

  # Check basic information
  expect_identical(result$id, 101)
  expect_identical(result$dataset_id, "dataset-001")
  expect_identical(result$title_en, "Poke Bowl Dataset")
  expect_identical(result$title_zh, "波奇碗数据")

  # Check list and HTML cleaning
  expect_identical(result$keyword_en, "salmon; potato")
  expect_identical(result$introduction_en,
                   "How to make the best poke bowl?")

  # Check copyright information
  expect_identical(result$copyright_code, "CC BY")

  # Check date conversion
  expect_identical(
    result$dataset_create_date,
    as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  )

  # Check status and flags
  expect_identical(result$share_status, "PUBLIC")
  expect_identical(result$file_flag, FALSE)

  # Check version row name
  expect_identical(rownames(result), "Version 1")

  # Check nested author and taxonomy data
  expect_s3_class(result$author[[1]], "data.frame")
  expect_s3_class(result$taxonomy[[1]], "data.frame")

  expect_identical(result$author[[1]]$name_en, "Jenny Xu")
  expect_identical(result$taxonomy[[1]]$name_en,
                   "Poke Food Science")
})
