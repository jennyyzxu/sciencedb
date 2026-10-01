#' Internal API-request helper function for sdb_search()
#' @param keyword A single character search string.
#' @param user_agent A character string containing the HTTP user agent.
#' @param joint_cookies A character string containing the required cookies.
#' @return The parsed ScienceDB JSON response as an R list
#' @noRd

sdb_cookie_internal <- function(user_agent){
  # =========================================================================
  # Step 1: Check Cookies
  # =========================================================================
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
  headers <- httr2::resp_headers(initial_response)
  set_cookie_headers <- unname(
    headers[tolower(names(headers)) == "set-cookie"]
  )
  
  if (length(set_cookie_headers) == 0L){
    stop(
      "ScienceDB did not return any cookies",
      call. = FALSE
    )
  }
  
  set_cookie_headers <- trimws(sub(";.*$", "", set_cookie_headers))

  # (ACW) Application Control Web Traffic Cookie: basic firewall session
  acw_cookies <- set_cookie_headers[startsWith(set_cookie_headers, "acw_tc=")]
  acw <- acw_cookies[nzchar(sub("^acw_tc=","", acw_cookies))]
  
  if(length(acw)==0L){
    stop("ScienceDB did not return a valid ACW cookie",
    call. = FALSE)
  }
  
  # (CDN) Content Delivery Network Security Traffic Cookie: authorize high-bandwidth data transfers
  cdn_cookies <- set_cookie_headers[startsWith(set_cookie_headers, "cdn_sec_tc=")]
  cdn <- cdn_cookies[nzchar(sub("^cdn_sec_tc=","", cdn_cookies))]
  
  if(length(cdn)==0L){
    stop("ScienceDB did not return a valid CDN cookie",
         call. = FALSE)
  }
  
  joint_cookies <- paste(acw, cdn, sep = "; ")
  return(joint_cookies)
}
# =========================================================================
#' Title
#'
#' @param keyword 
#' @param user_agent 
#' @param joint_cookies 
#'
#' @returns
#'
#' @examples
sdb_keyword_internal <- function(keyword, user_agent, joint_cookies){
  # =========================================================================
  # Step 3: Search keyword
  # Print error codes and responses
  # =========================================================================
  search_response <- httr2::request("https://www.scidb.cn/api/sdb-query-service/query") |>
    httr2::req_url_query(
      queryCode = "",
      q = keyword
    ) |>
    httr2::req_headers(
      'User-Agent' = user_agent,
      'Cookie' = joint_cookies,
      'Origin' = "https://www.scidb.cn",
      'Referer' = "https://www.scidb.cn/en/list",
      'Content-Type' = "application/json;charset=UTF-8"
    ) |>
    httr2::req_body_json(list(
      copyrightCode=list(),
      dataSetStatus=list(),
      fileType=list(),
      journalNameEn=list(),
      ordernum="6",
      page = 1,
      publishDate=list(),
      ror="",
      rorId=list(),
      size= 10000,
      taxonomyEn=list()
    ))|>
    httr2::req_perform()
  
  json <- tryCatch(
    httr2::resp_body_json(search_response),
    error = function(error){
      raw_response <- resp_body_raw(search_response)
      stop(paste0("ScienceDB returned an invalid JSON response; ", raw_response),
           call. = FALSE)
    })
  
  if (!is.list(json) ||
      is.null(json$data) ||
      !is.list(json$data) ||
      is.null(json$data$data) ||
      !is.list(json$data$data)){
    stop("The dataset field data$data is missing or invalid", call. = FALSE)
  }
  
  return(json)
}

# =========================================================================
#' Title
#'
#' @param keyword 
#'
#' @returns
#'
#' @examples
sdb_search_internal <- function(keyword){
  user_agent <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/152.0.0.0 Safari/537.36"
  joint_cookies <- sdb_cookie_internal(user_agent)
  sdb_keyword_internal(
    keyword = keyword,
    user_agent = user_agent,
    joint_cookies = joint_cookies
  )
}  
  
  