#' Retrieve Rich Raw Metadata for a ScienceDB Dataset
#'
#' @description
#' Queries ScienceDB website by DOI (Digital Object Identifier) to retrieve the complete raw backend metadata. This includes dataset metrics, raw author details, file structures, licenses, and internal identifiers.
#'
#' @param doi A character string specifying the DOI (e.g., "10.57760/sciencedb.15594").
#'
#' @returns A nested list containing the full API metadata response directly from ScienceDB's backend service.
#'
#' @importFrom httr2 request req_headers req_url_query req_body_json req_perform resp_body_json req_options req_timeout resp_header
#' @export
#'
#' @examples
#' \dontrun{
#' # Fetch complete rwa metadata for a dataset
#' raw_info <- sdb_dive("10.57760/sciencedb.15594")
#'
#' # Access raw backend elements
#' id <- raw_info[[1]]$id
#' access <- raw_info[[1]]$shareStatus}

sdb_dive <- function(doi){
  # =========================================================================
  # Step 1: Check Cookies
  # =========================================================================
  user_agent <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/152.0.0.0 Safari/537.36"

  initial_response <- httr2::request("https://www.scidb.cn/en") |>
    httr2::req_headers(
      'User-Agent' = user_agent,
      'Accept' = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      'Accept-Language' = "en"
    ) |>
    httr2::req_perform()

  # =========================================================================
  # Step 2: Extract Cookies
  # =========================================================================
  # (ACW) Application Control Web Traffic Cookie: basic firewall session
  acw_cookies <- initial_response[["headers"]][[3]]
  acw <- sub(";.*$", "", acw_cookies)

  # (CDN) Content Delivery Network Security Traffic Cookie: authorize high-bandwidth data transfers
  cdn_cookies <- initial_response[["headers"]][[4]]
  cdn <- sub(";.*$", "", cdn_cookies)

  joint_cookies <- paste(acw, cdn, sep = "; ")

  # =========================================================================
  # Step 3: Extract datafile ID, name, and path from Dataset ID
  # =========================================================================
  doi_response <- httr2::request(paste0("https://doi.org/", doi)) |>
    httr2::req_headers(
      'User-Agent' = user_agent,
      'Cookie' = joint_cookies
    ) |>
    httr2::req_options(followlocation = FALSE) |>
    httr2::req_perform()

  status <- httr2::resp_status(doi_response)
  location <- httr2::resp_header(doi_response, "location")

  match <- regexec("[?&]dataSetId=([^&#]+)",
                   location, ignore.case = TRUE)
  datasetID <- regmatches(location, match)[[1]][2]

  # =========================================================================
  # Step 4: Obtain raw information
  # =========================================================================
  doi_response <- httr2::request("https://www.scidb.cn/api/sdb-dataset-version-service/versionList") |>
    httr2::req_url_query(
      dataSetType = "personal",
      dataSetId = datasetID,
      mode = "front") |>
    httr2::req_headers(
      'User-Agent' = user_agent,
      'Cookie'     = joint_cookies,
      'Origin'     = "https://www.scidb.cn",
      'Referer'    = paste0("https://www.scidb.cn/en/detail?dataSetID=", datasetID)
    ) |>
    httr2::req_perform()

  json <- httr2::resp_body_json(doi_response)
  raw_info <- json$data$list

  return(raw_info)
}
