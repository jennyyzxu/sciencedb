# =========================================================================
.sdb_keyword <- function(keyword, user_agent, joint_cookies) {
  search_response <- httr2::request(paste0(
    "https://www.scidb.cn/api/",
    "sdb-query-service/query"
  )) |>
    httr2::req_url_query(
      queryCode = "",
      q = keyword
    ) |>
    httr2::req_headers(
      "User-Agent" = user_agent,
      "Cookie" = joint_cookies,
      "Origin" = "https://www.scidb.cn",
      "Referer" = "https://www.scidb.cn/en/list",
      "Content-Type" = "application/json;charset=UTF-8"
    ) |>
    httr2::req_body_json(list(
      copyrightCode = list(),
      dataSetStatus = list(),
      fileType = list(),
      journalNameEn = list(),
      ordernum = "6",
      page = 1,
      publishDate = list(),
      ror = "",
      rorId = list(),
      size = 10000,
      taxonomyEn = list()
    )) |>
    httr2::req_perform()

  json <- tryCatch(
    httr2::resp_body_json(search_response),
    error = function(error) {
      raw_response <- httr2::resp_body_string(search_response)
      stop(
        paste0(
          "ScienceDB returned an invalid JSON response; ",
          raw_response
        ),
        call. = FALSE
      )
    }
  )

  if (!is.list(json)
      || is.null(json$data)
      || !is.list(json$data)
      || is.null(json$data$data)
      || !is.list(json$data$data)) {
    stop("The dataset field data$data is missing or invalid", call. = FALSE)
  }

  json
}
# =========================================================================
.sdb_clean_html <- function(text) {
  if (is.null(text) || length(text) == 0) {
    return(NA_character_)
  }
  cleaned <- gsub("<[^>]+>", "", text)
  trimws(cleaned)
}
# =========================================================================
.sdb_extract <- function(list, field, optional_field = NULL, sub_key = NULL) {
  vapply(list, function(x) {
    value <- x[[field]]

    if ((is.null(value) || length(value) == 0) && !is.null(optional_field)) {
      value <- x[[optional_field]]
    }

    ### Return missing value (NA) if field doesn't exist in JSON
    if (is.null(value) || length(value) == 0) {
      return(NA_character_)
    }

    if (!is.null(sub_key) && is.list(value)) {
      extracted_keys <- sapply(value, function(item) {
        response <- item[[sub_key]]
        if (is.null(response)) {
          return(NA_character_)
        }
        as.character(response)
      })

      extracted_keys <- extracted_keys[!is.na(extracted_keys)]
      if (length(extracted_keys) == 0) {
        return(NA_character_)
      }
      return(paste(extracted_keys, collapse = "; "))
    }

    if (length(value) > 1 || is.list(value)) {
      return(paste(.sdb_clean_html(unlist(value)), collapse = "; "))
    }

    result <- as.character(value)
    .sdb_clean_html(result)
  }, FUN.VALUE = character(1))
}
# =========================================================================
.sdb_dataframe <- function(final_datasets) {
  dataset_names <- data.frame(
    Title = .sdb_extract(final_datasets, "titleEn",
      optional_field = "titleZh"
    ),
    Author = .sdb_extract(final_datasets, "author",
      sub_key = "nameEn"
    ),
    Keyword = .sdb_extract(final_datasets, "keywordEn"),
    Publish = .sdb_extract(final_datasets, "dataSetPublishDate"),
    Doi = .sdb_extract(final_datasets, "doi"),
    Taxonomy = .sdb_extract(final_datasets, "taxonomy",
      sub_key = "nameEn"
    ),
    Language = .sdb_extract(final_datasets, "language"),
    Introduction = .sdb_extract(final_datasets, "introductionEn",
      optional_field = "introductionZh"
    ),
    Access = .sdb_extract(final_datasets, "shareStatus"),
    Views = .sdb_extract(final_datasets, "visit"),
    Downloads = .sdb_extract(final_datasets, "download"),
    Cstr = .sdb_extract(final_datasets, "cstr")
  )

  dataset_names
}
