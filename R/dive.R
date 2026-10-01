#' Retrieve Rich Raw Metadata for a ScienceDB Dataset
#'
#' @description
#' Queries ScienceDB website by DOI (Digital Object Identifier) to retrieve the
#' complete raw backend metadata. This includes dataset metrics, 
#' raw author details, file structures, licenses, and internal identifiers.
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
#' # Fetch complete raw metadata for a dataset
#' raw_info <- sdb_dive(doi = "10.57760/sciencedb.15594")
#'
#' # Access raw backend elements
#' id <- raw_info[[1]]$id
#' access <- raw_info[[1]]$shareStatus}

sdb_dive <- function(doi, raw = FALSE){
  # =========================================================================
  # Step 1: Check user input
  # =========================================================================
  doi <- trimws(doi)
  
  if (
    !is.character(doi) ||
    length(doi) != 1L ||
    is.na(doi) ||
    !nzchar(doi)
  ) {
    stop(
      "Invalid DOI. DOI must a single non-empty character string.", 
      call. = FALSE
    )
  }
  
  # =========================================================================
  # Step 2: Extract cookies
  # =========================================================================
  user_agent <- paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ", 
                       "AppleWebKit/537.36 (KHTML, like Gecko) ",
                       "Chrome/152.0.0.0 Safari/537.36")
  
  joint_cookies <- sdb_cookie_internal(user_agent)
  
  # =========================================================================
  # Step 3: Translate DOI into Dataset ID
  # =========================================================================
  datasetID <- sdb_datasetid_internal(
    doi = doi,
    user_agent = user_agent,
    joint_cookies = joint_cookies
  )
  
  # =========================================================================
  # Step 4: Extract raw meta-information
  # =========================================================================
  raw_info <- sdb_rawinfo_internal(
    datasetID = datasetID,
    user_agent = user_agent,
    joint_cookies = joint_cookies
  )
  if (raw) {return(raw_info)}
  
  # =========================================================================
  # Step 5: Return clean meta-information by default
  # =========================================================================
  clean_info <- sdb_clean_internal(raw_info)
  return(clean_info)
  }
