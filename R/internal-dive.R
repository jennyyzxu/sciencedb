# =========================================================================
.sdb_scalar <- function(x) {
  # Convert NULL, length-0 object into NA
  if (is.null(x) || length(x) == 0) {
    return(NA)
  }

  # Convert empty and whitespae-only string to NA
  if (is.character(x) && !nzchar(trimws(x))) {
    return(NA_character_)
  }

  # Preserve legitimate value like 0 and FALSE
  x
}
# =========================================================================
.sdb_list <- function(x) {
  # Convert NULL, length-0 object into NA
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }

  # Flatten list into vector
  values <- unlist(lapply(x, as.character), use.names = FALSE)
  # Trim whitespace
  values <- trimws(values)
  # Remove missing/empty entries
  values <- values[!is.na(values) & nzchar(values)]
  # Return NA if nothing usable remains
  if (length(values) == 0) {
    return(NA_character_)
  }

  # Combine remaining values
  paste(values, collapse = "; ")
}
# =========================================================================
.sdb_intro <- function(x) {
  # Convert NULL, length-0 object into NA
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(x))) {
    return(NA_character_)
  }

  text <- gsub("<[^>]+>", "", x)
  trimws(text)
}
# =========================================================================
.sdb_date <- function(x) {
  # Convert NULL, length-0 object, and missing in NA
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(as.POSIXct(NA))
  }

  as.POSIXct(
    # Convert milliseconds in seconds
    x / 1000,
    # Unix epoch: number of seconds that have passed since January 1, 1970
    origin = "1970-01-01",
    # Coordinated Universal Time
    tz = "UTC"
  )
}
# =========================================================================
.sdb_author <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(data.frame())
  }

  # Process one author at a time
  authors <- lapply(x, function(author) {
    organization <- author$organizations[[1]]

    data.frame(
      name_en = .sdb_scalar(author$nameEn),
      name_zh = .sdb_scalar(author$nameZh),
      email = .sdb_scalar(author$email),
      orcid = .sdb_scalar(author$orcId),
      country = .sdb_scalar(author$country_name[[1]]),
      affiliation = .sdb_scalar(organization$nameEn)
    )
  })

  # Combine rows
  do.call(rbind, authors)
}
# =========================================================================
.sdb_taxonomy <- function(x) {
  # Convert NULL, length-0 object into NA
  if (is.null(x) || length(x) == 0) {
    return(data.frame(
      code = character(),
      name_en = character(),
      name_zh = character()
    ))
  }

  taxonomy_values <- lapply(x, function(taxonomy) {
    data.frame(
      code = .sdb_scalar(taxonomy$code),
      name_en = .sdb_scalar(taxonomy$nameEn),
      name_zh = .sdb_scalar(taxonomy$nameZh)
    )
  })

  do.call(rbind, taxonomy_values)
}
# =========================================================================
.sdb_clean <- function(raw_info) {
  if (is.null(raw_info) || length(raw_info) == 0) {
    return(data.frame())
  }

  rows <- lapply(raw_info, function(version) {
    data.frame(
      # Dataset Info
      id = .sdb_scalar(version$id),
      dataset_id = .sdb_scalar(version$dataSetId),
      dataset_type = .sdb_scalar(version$dataSetType),
      dataset_typecode = .sdb_scalar(version$dataSetTypeCode),

      # Basic Info
      title_en = .sdb_scalar(version$titleEn),
      title_zh = .sdb_scalar(version$titleZh),
      correspondent = .sdb_list(version$correspondent),
      keyword_en = .sdb_list(version$keywordEn),
      keyword_zh = .sdb_list(version$keywordZh),
      introduction_en = .sdb_intro(version$introductionEn),
      introduction_zh = .sdb_intro(version$introductionZh),
      version = .sdb_scalar(version$version),
      language = .sdb_scalar(version$language),
      funding = .sdb_list(version$funding),

      # Copyright Info
      copyright_id = .sdb_scalar(version$copyRight$did),
      copyright_code = .sdb_scalar(version$copyRight$code),
      copyright_name = .sdb_scalar(version$copyRight$name),
      copyright_image = .sdb_scalar(version$copyRight$img),
      copyright_url = .sdb_scalar(version$copyRight$url),
      copyright_explain_zh = .sdb_scalar(version$copyRight$explain),
      copyright_explain_en = .sdb_scalar(version$copyRight$explainEn),

      # Identifier Info
      doi = .sdb_scalar(version$doi),
      pid = .sdb_scalar(version$pid),
      cstr = .sdb_scalar(version$cstr),

      # Timestamps
      dataset_create_date = .sdb_date(version$dataSetCreateDate),
      dataset_update_date = .sdb_date(version$dataSetUpdateDate),
      dataset_publish_date = .sdb_date(version$dataSetPublishDate),
      version_create_date = .sdb_date(version$versionCreateDate),
      version_update_date = .sdb_date(version$versionUpdateDate),
      version_publish_date = .sdb_date(version$versionPublishDate),
      archive_date = .sdb_date(version$archiveDate),
      audit_date = .sdb_date(version$auditDate),
      submit_date = .sdb_date(version$submitDate),

      # Status
      status = .sdb_scalar(version$status),
      file_status = .sdb_scalar(version$fileStatus),
      doi_status = .sdb_scalar(version$doiStatus),
      share_status = .sdb_scalar(version$shareStatus),
      private_status = .sdb_scalar(version$privateState),

      # file info
      file_type = .sdb_scalar(version$fileType),
      size = .sdb_scalar(version$size),
      count = .sdb_scalar(version$count),
      record = .sdb_scalar(version$record),
      record_unit = .sdb_scalar(version$recordUnit),
      source = .sdb_scalar(version$source),
      publisher = .sdb_scalar(version$publisher),

      # URL
      short_url = .sdb_scalar(version$shortUrl),
      cover_url = .sdb_scalar(version$coverUrl),
      archive_link = .sdb_scalar(version$archiveLink),
      online_url = .sdb_scalar(version$onLineUrl),

      # Flags
      file_flag = .sdb_scalar(version$createFileFlag),
      archive_flag = .sdb_scalar(version$archiveFlag),
      doi_flag = .sdb_scalar(version$doiFlag)
    )
  })

  clean_info <- do.call(rbind, rows)
  rownames(clean_info) <- paste(
    "Version",
    sub("^V", "", clean_info$version)
  )

  clean_info$author <- lapply(raw_info, function(version) {
    .sdb_author(version$author)
  })

  clean_info$taxonomy <- lapply(raw_info, function(version) {
    .sdb_taxonomy(version$taxonomy)
  })

  clean_info
}
