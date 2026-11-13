.sdb_validate_input <- function(input) {
  if (
    !is.character(input) ||
      length(input) != 1L ||
      is.na(input) ||
      !nzchar(trimws(input))
  ) {
    stop(
      "Invalid input. Input must be a single non-empty character string.",
      call. = FALSE
    )
  }

  input <- trimws(input)
  input
}
# =========================================================================
.sdb_cookie <- function(user_agent) {
  initial_response <- httr2::request("https://www.scidb.cn/en") |>
    httr2::req_headers(
      "User-Agent" = user_agent,
      "Accept" = paste0(
        "text/html,application/xhtml+xml,application/",
        "xml;q=0.9,*/*;q=0.8"
      ),
      "Accept-Language" = "en"
    ) |>
    httr2::req_perform()


  headers <- httr2::resp_headers(initial_response)
  set_cookie_headers <- unname(
    headers[tolower(names(headers)) == "set-cookie"]
  )

  if (length(set_cookie_headers) == 0L) {
    stop(
      "ScienceDB did not return any cookies",
      call. = FALSE
    )
  }

  set_cookie_headers <- trimws(sub(";.*$", "", set_cookie_headers))

  # (ACW) Application Control Web Traffic Cookie: basic firewall session
  acw_cookies <- set_cookie_headers[startsWith(
    set_cookie_headers,
    "acw_tc="
  )]
  acw <- acw_cookies[nzchar(sub("^acw_tc=", "", acw_cookies))]

  if (length(acw) == 0L) {
    stop("ScienceDB did not return a valid ACW cookie",
      call. = FALSE
    )
  }

  # (CDN) Content Delivery Network Security Traffic Cookie:
  # authorize high-bandwidth data transfers
  cdn_cookies <- set_cookie_headers[startsWith(
    set_cookie_headers,
    "cdn_sec_tc="
  )]
  cdn <- cdn_cookies[nzchar(sub("^cdn_sec_tc=", "", cdn_cookies))]

  if (length(cdn) == 0L) {
    stop("ScienceDB did not return a valid CDN cookie",
      call. = FALSE
    )
  }

  joint_cookies <- paste(acw, cdn, sep = "; ")
  joint_cookies
}
# =========================================================================
.sdb_datasetid <- function(doi, user_agent, joint_cookies) {
  doi_response <- httr2::request(paste0("https://doi.org/", doi)) |>
    httr2::req_headers(
      "User-Agent" = user_agent,
      "Cookie" = joint_cookies
    ) |>
    httr2::req_options(followlocation = FALSE) |>
    httr2::req_perform()

  location <- httr2::resp_header(doi_response, "location")

  if (!is.character(location)
      || is.null(location)
      || length(location) != 1
      || is.na(location)
      || !nzchar(trimws(location))) {
    stop(
      paste0(
        "Could not resolve DOI: ",
        "the DOI service did not provide a destination URL."
      ),
      call. = FALSE
    )
  }

  match <- regexec("[?&]dataSetId=([^&#]+)",
    location,
    ignore.case = TRUE
  )

  check_match <- regmatches(location, match)[[1]]
  if (length(check_match) < 2 || !nzchar(trimws(check_match[2]))) {
    stop(paste0(
      "Could not resolve DOI: ",
      "The destination URL did not contain a dataset ID."
    ))
  }

  dataset_id <- check_match[2]
  dataset_id
}
# =========================================================================
.sdb_rawinfo <- function(dataset_id, user_agent, joint_cookies) {
  rawinfo_response <- httr2::request(paste0(
    "https://www.scidb.cn/api/",
    "sdb-dataset-version-service/",
    "versionList"
  )) |>
    httr2::req_url_query(
      dataSetType = "personal",
      dataSetId = dataset_id,
      mode = "front"
    ) |>
    httr2::req_headers(
      "User-Agent" = user_agent,
      "Cookie" = joint_cookies,
      "Origin" = "https://www.scidb.cn",
      "Referer" = paste0(
        "https://www.scidb.cn/en/detail?dataSetID=",
        dataset_id
      )
    ) |>
    httr2::req_perform()

  json <- httr2::resp_body_json(rawinfo_response)
  raw_info <- json$data$list

  raw_info
}
