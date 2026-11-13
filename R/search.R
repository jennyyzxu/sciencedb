#' Search ScienceDB Datasets by Keyword
#'
#' @description
#' Executes a programmatic search against the ScienceDB platform
#' using internal query endpoints.
#' It returns a cleaned and structured data frame of dataset metadata.
#'
#' @param keyword A non-empty character string specifying the search query.
#'
#' @returns A data frame containing the following columns:
#' \describe{
#'   \item{Title}{English dataset title (Chinese title if English is missing).}
#'   \item{Author}{Semicolon-separated list of author names.}
#'   \item{Keyword}{Semicolon-separated list of English keywords.}
#'   \item{Publish}{Publish date string.}
#'   \item{Doi}{Digital Object Identifier associated with the dataset.}
#'   \item{Taxonomy}{Semicolon-separated taxonomy categories.}
#'   \item{Language}{Dataset language code/string.}
#'   \item{Introduction}{Dataset abstract or description.}
#'   \item{Access}{Access conditions (PUBLIC/RESTRICTED/EMBARGO).}
#'   \item{Views}{Total page view count.}
#'   \item{Downloads}{Total file download count.}
#'   \item{Cstr}{China Science and Technology Resource identifier.}}
#'
#' @importFrom httr2 request req_headers req_url_query
#'             req_body_json req_perform resp_body_json req_options req_timeout
#'             resp_header
#' @export
#'
#' @examples
#' \dontrun{
#' results <- sdb_search("personality")
#' head(results)
#' }

sdb_search <- function(keyword) {
  # =========================================================================
  # Step 1: Validate User input: keyword
  # =========================================================================
  keyword <- .sdb_validate_input(keyword)

  # =========================================================================
  # Step 2: Check Cookies
  # =========================================================================
  user_agent <- paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
                       "AppleWebKit/537.36 (KHTML, like Gecko) ",
                       "Chrome/152.0.0.0 Safari/537.36")

  joint_cookies <- .sdb_cookie(user_agent)

  # =========================================================================
  # Step 3: Search keywords
  # =========================================================================
  json <- .sdb_keyword(keyword, user_agent, joint_cookies)
  final_datasets <- json$data$data

  # =========================================================================
  # Step 4: Return search results
  # =========================================================================
  dataset_names <- .sdb_dataframe(final_datasets)
  dataset_names
}
