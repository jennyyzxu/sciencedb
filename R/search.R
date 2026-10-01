#' Search ScienceDB Datasets by Keyword
#'
#' @description
#' Executes a programmatic search against the ScienceDB platform using internal query endpoints. It returns a cleaned and structured data frame of dataset metadata.
#'
#' @param keyword A character string specifying the search query or topic (e.g., 'personality', 'economics'). Default is 'NULL', which returns top default results.
#'
#' @returns A data frame containing the following columns:
#' \describe{
#'   \item{Title}{English dataset title (falls back to Chinese title if English is missing).}
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
#' @importFrom httr2 request req_headers req_url_query req_body_json req_perform resp_body_json req_options req_timeout resp_header
#' @export
#'
#' @examples
#' \dontrun{
#' results <- sdb_search("personality")
#' head(results)
#' }

sdb_search <- function(keyword = ""){
  # =========================================================================
  # Step 1,2,3: Cookies Extraction in Internal Helper
  # =========================================================================
  if(is.null(keyword) || trimws(keyword) == ""){
    keyword = ""
  } else{
    if (
      !is.character(keyword) ||
      length(keyword) != 1L ||
      is.na(keyword)
    ){
      stop(
        "Invalid keyword. Keyword must be NULL or a single, non-missing character string."
      )
    }
    keyword = trimws(keyword)
  }

  json <- sdb_search_internal(keyword)
  final_datasets <- json$data$data

  # =========================================================================
  # Step 4: Create clean data frame
  # =========================================================================
  clean_html <- function(text){
    if (is.null(text) || length(text) == 0) return(NA_character_)
    cleaned <- gsub("<[^>]+>", "", text)
    trimws(cleaned)
  }

  ## Define an internal helper function to extract info from master list
  extract <- function(list, field, optional_field=NULL, sub_key = NULL){
    vapply(list, function(x){
      value <- x[[field]]

      if((is.null(value) || length(value) == 0) && !is.null(optional_field)){
        value <- x[[optional_field]]
      }

      ### Return missing value (NA) if field doesn't exist in JSON
      if (is.null(value) || length(value) == 0){
        return(NA_character_)
      }

      if(!is.null(sub_key)&& is.list(value)){
        extracted_keys <- sapply(value, function(item){
          response <- item[[sub_key]]
          if(is.null(response)) return(NA_character_)
          return(as.character(response))
        })

        extracted_keys <- extracted_keys[!is.na(extracted_keys)]
        if (length(extracted_keys) == 0) return(NA_character_)
        return(paste(extracted_keys, collapse = "; "))
      }

      if(length(value) >1||is.list(value)){
        return(paste(clean_html(unlist(value)), collapse = "; "))
      }

      result <- as.character(value)
      return(clean_html(result))

    }, FUN.VALUE = character(1)
    )}

  ## Define a final data frame by extracting name column from datasets
  dataset.names <- data.frame(
    Title = extract(final_datasets, "titleEn", optional_field = "titleZh"),
    Author = extract(final_datasets, "author", sub_key = "nameEn"),
    Keyword = extract(final_datasets, "keywordEn"),
    Publish = extract(final_datasets, "dataSetPublishDate"),
    Doi = extract(final_datasets, "doi"),
    Taxonomy = extract(final_datasets, "taxonomy", sub_key = "nameEn"),
    Language = extract(final_datasets, "language"),
    Introduction = extract(final_datasets, "introductionEn", optional_field = "introductionZh"),
    Access = extract(final_datasets, "shareStatus"),
    Views = extract(final_datasets, "visit"),
    Downloads = extract(final_datasets, "download"),
    Cstr = extract(final_datasets, "cstr"))

  return(dataset.names)
}
