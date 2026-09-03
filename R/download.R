#' Download All Dataset Files to Local Directory by DOI
#'
#' @param doi A character string specifying the DOI (e.g., "10.57760/sciencedb.15594").
#' @param user_path A character string specifying the local directory path where downloaded files will be saved.
#'
#' @returns Invisible. Files are saved directly to the specified user path.
#'
#' @importFrom httr2 request req_headers req_url_query req_body_json req_perform resp_body_json req_options req_timeout resp_header
#' @export
#'
#' @examples
#' \dontrun{
#' sdb_download(doi = "10.57760/sciencedb.15594",
#'              user_path = "~/Desktop")
#' }

sdb_download <- function(doi, user_path){
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
  # Step 3: Test DOI to Dataset ID
  # =========================================================================
  response <- httr2::request(paste0("https://doi.org/", doi)) |>
    httr2::req_headers(
      'User-Agent' = user_agent,
      'Cookie' = joint_cookies
    ) |>
    httr2::req_options(followlocation = FALSE) |>
    httr2::req_perform()

  location <- httr2::resp_header(response, "location")
  match <- regexec("[?&]dataSetId=([^&#]+)",
                   location, ignore.case = TRUE)
  datasetID <- regmatches(location, match)[[1]][2]

  # =========================================================================
  # Step 4: Extract datafile ID, name, and path from Dataset ID
  # =========================================================================
  meta_info <- sdb_dive(doi)
  ver <- meta_info[[1]]$version
  current_path <- paste("/", ver, sep = "")

  get_terminal_files <- function(path){
    sub_response <- httr2::request(
      "https://www.scidb.cn/api/gin-sdb-filetree/public/file/childrenFileListByPath"
    ) |>
      httr2::req_headers(
        'User-Agent' = user_agent,
        'Cookie' = joint_cookies,
        'Origin' = "https://www.scidb.cn",
        'Referer' = paste0("https://www.scidb.cn/en/detail?dataSetId=", datasetID)
      ) |>
      httr2::req_body_json(
        list(
          dataSetId = datasetID,
          lastIndex = 0,
          pageSize = 2000,
          path = path,
          version = ver
        )
      ) |>
      httr2::req_perform()

    top_level_files <- httr2::resp_body_json(sub_response)$data
    all_files <- list()

    # Recursion: R creates a brand new, independent copy of function while original copy wait for new copy to finish before resuming
    for (file in top_level_files){
      # Is this item a folder? (Yes)
      if (isTRUE(file$dir)){
        # Pause, launch a new request with subfolder path, and discover a list of files
        nested_files <- get_terminal_files(file$path)
        # Merge discovered files into master list
        all_files <- c(all_files, nested_files)
        # Is this item a folder? (No)
      } else {
        # Append sub-list containing datafile info
        all_files[[length(all_files) + 1]] <- list(
          file_id = file$id,
          file_path = file$path,
          file_name = file$fileName
        )
      }
    }

    return(all_files)
  }

  datafiles_info <- get_terminal_files(current_path)

  # =========================================================================
  # Step 5: Check file duplicates
  # =========================================================================
  rename <- function(destination_path){
    if(!file.exists(destination_path)){
      return(destination_path)
    }

    dir_name <- dirname(destination_path)
    file_name <- basename(destination_path)

    extension <- tools::file_ext(file_name)
    extension_str <- if(nzchar(extension)) paste0(".", extension) else ""
    base_name <- tools::file_path_sans_ext(file_name)

    counter <- 1
    while (file.exists(destination_path)){
      new_name <- sprintf("%s(%d)%s", base_name, counter, extension_str)
      destination_path <- file.path(dir_name, new_name)
      counter <- counter+1
    }

    message(sprintf("File already exists. Saving as: %s", basename(destination_path)))
    return(destination_path)
  }


  # =========================================================================
  # Step 6: Download All Data Files to User's File Directory
  # =========================================================================
  num_datafiles <- length(datafiles_info)

  if (num_datafiles == 0){
    message("No files found to download for this dataset.")
    return(invisible(NULL))
  }

  message(sprintf("Found %d file(s) to download.", num_datafiles))

  for (i in seq_along(datafiles_info)){
    current_file <- datafiles_info[[i]]
    target_path <- file.path(user_path, current_file$file_name)
    destination <- rename(target_path)

    message(sprintf("[%d/%d] Downloading: %s ...", i, num_datafiles, current_file$file_name))

    download_res <- httr2::request("https://download.scidb.cn/download") |>
      httr2::req_url_query(
        fileId = current_file$file_id,
        path = current_file$file_path,
        fileName = current_file$file_name
      ) |>
      httr2::req_headers(
        'User-Agent' = user_agent,
        'Cookie' = joint_cookies,
        'Origin' = "https://www.scidb.cn",
        'Referer' = paste0("https://www.scidb.cn/en/detail?dataSetId=", datasetID)
      )|>
      httr2::req_timeout(3600) |>
      httr2::req_perform(path = destination)

    Sys.sleep(1)
  }

  message("All downloads completed successfully!")
}
