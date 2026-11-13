# =========================================================================
.sdb_filetree <- function(user_agent,
                          joint_cookies,
                          dataset_id,
                          ver,
                          path) {
  sub_response <- httr2::request(paste0("https://www.scidb.cn/api/",
                                        "gin-sdb-filetree/public/file/",
                                        "childrenFileListByPath")) |>
    httr2::req_headers(
      "User-Agent" = user_agent,
      "Cookie" = joint_cookies,
      "Origin" = "https://www.scidb.cn",
      "Referer" = paste0("https://www.scidb.cn/en/detail?dataSetId=",
                         dataset_id)
    ) |>
    httr2::req_body_json(
      list(
        dataSetId = dataset_id,
        lastIndex = 0,
        pageSize = 2000,
        path = path,
        version = ver
      )
    ) |>
    httr2::req_perform()

  json <- httr2::resp_body_json(sub_response)

  if (is.null(json$data) || !is.list(json$data)) {
    stop("ScienceDB file-tree response is missing or contains invalid data.",
      call. = FALSE
    )
  }

  top_level_files <- json$data
  all_files <- list()

  # Recursion: R creates a brand new, independent copy of function
  # while original copy wait for new copy to finish before resuming
  for (file in top_level_files) {
    # Does each item has a valid dir?
    if (is.null(file$dir) || length(file$dir) != 1) {
      stop("ScienceDB returned malformed directory metadata.",
        call. = FALSE
      )
    }
    # Is this item a folder? (Yes)
    if (isTRUE(file$dir)) {
      # Does each folder has a usable path for recursion?
      if (is.null(file$path)
          || length(file$path) != 1
          || !nzchar(trimws(file$path))) {
        stop("ScienceDB returned malformed folder metadata.")
      }
      # Pause, launch a new request with subfolder path and discover files
      nested_files <- .sdb_filetree(
        user_agent = user_agent,
        joint_cookies = joint_cookies,
        dataset_id = dataset_id,
        ver = ver,
        path = file$path
      )
      # Merge discovered files into master list
      all_files <- c(all_files, nested_files)
      # Is this item a folder? (No)
    } else {
      # Does each terminal file has valid id, path, and filename?
      if (
        is.null(file$id) || length(file$id) != 1 ||
          is.null(file$path) || length(file$path) != 1 ||
          is.null(file$fileName) || length(file$fileName) != 1
      ) {
        stop("ScienceDB returned malformed terminal file metadata.",
          call. = FALSE
        )
      }
      # Append sub-list containing datafile info
      all_files[[length(all_files) + 1]] <- list(
        file_id = file$id,
        file_path = file$path,
        file_name = file$fileName
      )
    }
  }

  all_files
}

# =========================================================================
.sdb_rename <- function(destination_path) {
  if (!file.exists(destination_path)) {
    return(destination_path)
  }

  dir_name <- dirname(destination_path)
  file_name <- basename(destination_path)

  extension <- tools::file_ext(file_name)
  extension_str <- if (nzchar(extension)) paste0(".", extension) else ""
  base_name <- tools::file_path_sans_ext(file_name)

  counter <- 1
  while (file.exists(destination_path)) {
    new_name <- sprintf("%s(%d)%s", base_name, counter, extension_str)
    destination_path <- file.path(dir_name, new_name)
    counter <- counter + 1
  }

  message(sprintf("File already exists. Saving as: %s",
                  basename(destination_path)))
  destination_path
}
# =========================================================================
.sdb_download <- function(file_id,
                          file_path,
                          file_name,
                          dataset_id,
                          user_agent,
                          joint_cookies,
                          destination) {
  httr2::request("https://download.scidb.cn/download") |>
    httr2::req_url_query(
      fileId = file_id,
      path = file_path,
      fileName = file_name
    ) |>
    httr2::req_headers(
      "User-Agent" = user_agent,
      "Cookie" = joint_cookies,
      "Origin" = "https://www.scidb.cn",
      "Referer" = paste0("https://www.scidb.cn/en/detail?dataSetId=",
                         dataset_id)
    ) |>
    httr2::req_timeout(3600) |>
    httr2::req_perform(path = destination)

  destination
}
