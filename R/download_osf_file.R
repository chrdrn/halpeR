#' Download OSF File
#'
#' This function downloads data from the Open Science Framework (OSF) by specifying a file ID.
#'
#' @param local_folder A character vector specifying the local folder(s) where the data should be downloaded.
#' @param osf_file_id A character vector specifying the OSF file ID(s). Default is NULL.
#' @param local_directory A character string specifying the local directory where the data should be stored. Default is "local_data/osf_pull".
#'
#' @details
#' You must specify `osf_file_id`. The function will create the necessary local directories if they do not exist.
#'
#' @return None. The function is called for its side effects (downloading data).
#' If an error occurs during the download of a file, a message will be printed with the file ID and the error message.
#'
#' @examples
#' \dontrun{
#' # Download data from an OSF file
#' download_osf_file(local_folder = c("folder1", "folder2"), osf_file_id = c("file_id1", "file_id2"))
#' }
#'
#' @importFrom here here
#' @importFrom osfr osf_download osf_retrieve_file
#' @export

download_osf_file <- function(
    local_folder,
    osf_file_id = NULL,
    local_directory = "local_data/osf_pull") {

    if (is.null(osf_file_id)) {
        stop("You must specify 'osf_file_id'.")
    }

    purrr::walk2(local_folder, osf_file_id, function(download_folder, file_id) {
        # Check if directory exists
        download_dir_folder <- here::here(local_directory, download_folder)
        if (!dir.exists(download_dir_folder)) {
            dir.create(download_dir_folder, recursive = TRUE)
        }

        # Download data
        tryCatch({
            osfr::osf_download(
                osfr::osf_retrieve_file(file_id),
                path = download_dir_folder,
                conflicts = "overwrite",
                verbose = TRUE
            )
        }, error = function(e) {
            message(paste("Error downloading file:", file_id, ":", e$message))
        })
    })
}
