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
sdb_datasetid_internal <- function(doi,user_agent, joint_cookies){
  doi_response <- httr2::request(paste0("https://doi.org/", doi)) |>
    httr2::req_headers(
      'User-Agent' = user_agent,
      'Cookie' = joint_cookies
    ) |>
    httr2::req_options(followlocation = FALSE) |>
    httr2::req_perform()
  
  location <- httr2::resp_header(doi_response, "location")
  
  if(!is.character(location) || is.null(location) || length(location) != 1 || is.na(location) || !nzchar(trimws(location))){
    stop("Could not resolve DOI: the DOI service did not provide a destination URL.", call. = FALSE)
  }
  
  match <- regexec("[?&]dataSetId=([^&#]+)",
                   location, ignore.case = TRUE)
  
  check_match <- regmatches(location, match)[[1]]
  if (length(check_match) < 2 ||!nzchar(trimws(check_match[2]))){
    stop("Could not resolve DOI: The destination URL did not contain a dataset ID.")
  }
  
  datasetID <- check_match[2]
  return(datasetID)
}
# =========================================================================
sdb_rawinfo_internal <- function(datasetID, user_agent, joint_cookies){
  rawinfo_response <- httr2::request("https://www.scidb.cn/api/sdb-dataset-version-service/versionList") |>
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
  
  json <- httr2::resp_body_json(rawinfo_response)
  raw_info <- json$data$list
  
  return(raw_info)
}
# =========================================================================
sdb_scalar_internal <- function(x){
  # Convert NULL, length-0 object into NA
  if (is.null(x) || length(x) == 0){
    return(NA)
  }
  
  # Convert empty and whitespae-only string to NA
  if(is.character(x) && !nzchar(trimws(x))){
    return(NA_character_)
  }
  
  # Preserve legitimate value like 0 and FALSE
  x
}
# =========================================================================
sdb_list_internal <- function(x){
  # Convert NULL, length-0 object into NA
  if (is.null(x) || length(x) == 0){
    return(NA_character_)
  }
  
  # Flatten list into vector
  values <- unlist(x, use.names = FALSE)
  # Trim whitespace
  values <- trimws(as.character(values))
  # Remove missing/empty entries
  values <- values[!is.na(values)&nzchar(values)]
  # Return NA if nothing usable remains
  if (length(values) == 0){
    return(NA_character_)
  }
  
  # Combine remaining values
  paste(values, collapse = "; ")
}
# =========================================================================
sdb_intro_internal <- function(x){
  # Convert NULL, length-0 object into NA
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(x))){
    return(NA_character_)
  }
  
  text <- gsub("<[^>]+>", "", x)
  trimws(text)
}
# =========================================================================
sdb_date_internal <- function(x){
  # Convert NULL, length-0 object, and missing in NA
  if (is.null(x) || length(x) == 0 || is.na(x)){
    return(as.POSIXct(NA))
  }
  
  as.POSIXct(
    # Convert milliseconds in seconds
    x/1000,
    # Unix epoch: number of seconds that have passed since January 1, 1970
    origin = "1970-01-01",
    # Coordinated Universal Time
    tz = "UTC"
  )
}
# =========================================================================
sdb_author_internal <- function(x){
  if (is.null(x) || length(x) == 0){
    return(data.frame())
  }
  
  # Process one author at a time
  authors <- lapply(x, function(author){
    organization <- author$organizations[[1]]
    
    data.frame(
      name_en = sdb_scalar_internal(author$nameEn),
      name_zh = sdb_scalar_internal(author$nameZh),
      email = sdb_scalar_internal(author$email),
      orcid = sdb_scalar_internal(author$orcid),
      country = sdb_scalar_internal(author$country_name[[1]]),
      affiliation = sdb_scalar_internal(organization$nameEn)
    )
  })
  
  # Combine rows
  do.call(rbind, authors)
}
# =========================================================================
sdb_taxonomy_internal <- function(x){
  # Convert NULL, length-0 object into NA
  if (is.null(x) || length(x) == 0){
    return(data.frame(
      code = character(),
      name_en = character(),
      name_zh = character()))
  }
  
  taxonomy_values <- lapply(x, function(taxonomy){
    data.frame(
      code = sdb_scalar_internal(x[[1]]$code),
      name_en = sdb_scalar_internal(x[[1]]$nameEn),
      name_zh = sdb_scalar_internal(x[[1]]$nameZh))
  })
  
  do.call(rbind, taxonomy_values)
}
# =========================================================================
sdb_clean_internal <- function(raw_info){
  if (is.null(raw_info) || length(raw_info) == 0){
    return(data.frame())
  }
  
  rows <- lapply(raw_info, function(version){
    data.frame(
      # Dataset Info
      id = sdb_scalar_internal(version$id),
      dataset_id = sdb_scalar_internal(version$dataSetId),
      dataset_type = sdb_scalar_internal(version$dataSetType),
      dataset_typecode = sdb_scalar_internal(version$dataSetTypeCode),
      
      # Basic Info
      title_en = sdb_scalar_internal(version$titleEn),
      title_zh = sdb_scalar_internal(version$titleZh),
      correspondent = sdb_list_internal(version$correspondent),
      keyword_en = sdb_list_internal(version$keywordEn),
      keyword_zh = sdb_list_internal(version$keywordZh),
      introduction_en = sdb_intro_internal(version$introductionEn),
      introduction_zh = sdb_intro_internal(version$introductionZh),
      version = sdb_scalar_internal(version$version),
      language = sdb_scalar_internal(version$language),
      funding = sdb_list_internal(version$funding),
      
      # Copyright Info
      copyright_id = sdb_scalar_internal(version$copyRight$did),
      copyright_code = sdb_scalar_internal(version$copyRight$code),
      copyright_name = sdb_scalar_internal(version$copyRight$name),
      copyright_image = sdb_scalar_internal(version$copyRight$img),
      copyright_url = sdb_scalar_internal(version$copyRight$url),
      copyright_explain_zh = sdb_scalar_internal(version$copyRight$explain),
      copyright_explain_en = sdb_scalar_internal(version$copyRight$explainEn),
      
      # Identifier Info
      doi = sdb_scalar_internal(version$doi),
      pid = sdb_scalar_internal(version$pid),
      cstr = sdb_scalar_internal(version$cstr),
      
      # Timestamps
      dataset_create_date = sdb_date_internal(version$dataSetCreateDate),
      dataset_update_date = sdb_date_internal(version$dataSetUpdateDate),
      dataset_publish_date = sdb_date_internal(version$dataSetPublishDate),
      version_create_date = sdb_date_internal(version$versionCreateDate),
      version_update_date = sdb_date_internal(version$versionUpdateDate),
      version_publish_date = sdb_date_internal(version$versionPublishDate),
      archive_date = sdb_date_internal(version$archiveDate),
      audit_date = sdb_date_internal(version$auditDate),
      submit_date = sdb_date_internal(version$submitDate),
      
      # Status
      status = sdb_scalar_internal(version$status),
      file_status = sdb_scalar_internal(version$fileStatus),
      doi_status = sdb_scalar_internal(version$doiStatus),
      share_status = sdb_scalar_internal(version$shareStatus),
      private_status = sdb_scalar_internal(version$privateState),
    
      # file info
      file_type = sdb_scalar_internal(version$fileType),
      size = sdb_scalar_internal(version$size),
      count = sdb_scalar_internal(version$count),
      record = sdb_scalar_internal(version$record),
      record_unit = sdb_scalar_internal(version$recordUnit),
      source = sdb_scalar_internal(version$source),
      publisher = sdb_scalar_internal(version$publisher),
      
      # URL
      short_url = sdb_scalar_internal(version$shortUrl),
      cover_url = sdb_scalar_internal(version$coverUrl),
      archive_link = sdb_scalar_internal(version$archiveLink),
      online_url = sdb_scalar_internal(version$onLineUrl),
      
      # Flags
      file_flag = sdb_scalar_internal(version$createFileFlag),
      archive_flag = sdb_scalar_internal(version$archiveFlag),
      doi_flag = sdb_scalar_internal(version$doiFlag)
    )
  })
  
  clean_info <- do.call(rbind, rows)
  rownames(clean_info) <- paste("Version", 
                                sub("^V", "", clean_info$version))
  
  clean_info$author <- lapply(raw_info, function(version){
    sdb_author_internal(version$author)
  })
  
  clean_info$taxonomy <- lapply(raw_info, function(version){
    sdb_taxonomy_internal(version$taxonomy)
  })
  
  return(clean_info)
}