#' Download All Dataset Files to Local Directory by DOI
#'
#' @param doi A character string specifying the DOI.
#'
#' @returns The path to the temporary directory with downloaded files.
#' Users should copy any files they want before the ending R session.
#'
#' @importFrom httr2 request req_headers req_url_query req_body_json
#'             req_perform resp_body_json req_options req_timeout resp_header
#' @export
#'
#' @examples
#' \dontrun{
#' sdb_download(doi = "10.57760/sciencedb.15594")
#' }

sdb_download <- function(doi) {
  # =========================================================================
  # Step 1: Validate User Inputs: DOI
  # =========================================================================
  doi <- .sdb_validate_input(doi)

  # =========================================================================
  # Step 2: Check Cookies
  # =========================================================================
  user_agent <- paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
                       "AppleWebKit/537.36 (KHTML, like Gecko) ",
                       "Chrome/152.0.0.0 Safari/537.36")

  joint_cookies <- .sdb_cookie(user_agent)

  # =========================================================================
  # Step 3: Test DOI to Dataset ID
  # =========================================================================
  dataset_id <- .sdb_datasetid(
    doi = doi,
    user_agent = user_agent,
    joint_cookies = joint_cookies
  )

  # =========================================================================
  # Step 4: Extract datafile ID, name, and path from Dataset ID
  # =========================================================================
  meta_info <- .sdb_rawinfo(
    dataset_id = dataset_id,
    user_agent = user_agent,
    joint_cookies = joint_cookies
  )
  ver <- meta_info[[1]]$version
  current_path <- paste("/", ver, sep = "")

  datafiles_info <- .sdb_filetree(user_agent = user_agent,
                                  joint_cookies = joint_cookies,
                                  dataset_id = dataset_id,
                                  ver = ver,
                                  path = current_path)

  # =========================================================================
  # Step 5: Check File Number & Access Condition
  # =========================================================================
  num_datafiles <- length(datafiles_info)

  if (num_datafiles == 0) {
    message("No files found to download for this dataset.")
    return(invisible(NULL))
  }

  access <- meta_info[[1]]$shareStatus
  if (access %in% c("RESTRICTED", "EMBARGO")) {
    message(paste0("Notice: this dataset contains restricted or embargoed",
                   " files. Some downloads may be unavailable."))
  }

  # =========================================================================
  # Step 6: Create temporary directory
  # =========================================================================
  temporary_dir <- tempfile("sdb-", tmpdir = tempdir())
  if (!dir.create(temporary_dir, mode = "0700")) {
    stop("Could not create a temporary download directory.", call. = FALSE)
  }

  completed <- FALSE
  on.exit({
    if (!completed) {
      unlink(temporary_dir, recursive = TRUE)
    }
  }, add = TRUE)

  # =========================================================================
  # Step 7: Download All Data Files to Temporary Directory
  # =========================================================================
  message(sprintf("Found %d file(s) to download.", num_datafiles))

  for (i in seq_along(datafiles_info)){
    current_file <- datafiles_info[[i]]
    target_path <- file.path(temporary_dir, current_file$file_name)
    destination <- .sdb_rename(target_path)

    message(sprintf("[%d/%d] Downloading: %s ...", i, num_datafiles,
                    current_file$file_name))

    .sdb_download(file_id = current_file$file_id,
                  file_path = current_file$file_path,
                  file_name = current_file$file_name,
                  user_agent = user_agent,
                  dataset_id = dataset_id,
                  joint_cookies = joint_cookies,
                  destination = destination)

    Sys.sleep(1)
  }

  completed <- TRUE
  message("Downloads saved to: ", temporary_dir)
  temporary_dir
}
