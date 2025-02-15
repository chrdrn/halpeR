
#' Download OSF Component
#'
#' This function downloads components from the Open Science Framework (OSF) to a specified local directory.
#'
#' @param local_folder A character vector of local folder paths where the OSF components will be downloaded.
#' @param osf_component_id A character vector of OSF component IDs corresponding to the local folders.
#' @param local_directory A character string specifying the base local directory for the OSF pull. Default is "local_data/osf_pull".
#'
#' @details
#' The function uses `purrr::walk2` to iterate over the `local_folder` and `osf_component_id` vectors, downloading each component to the corresponding local folder.
#' If the specified local folder does not exist, it will be created. The function handles errors gracefully, printing a message if a download fails.
#'
#' @importFrom purrr walk2
#' @importFrom osfr osf_retrieve_node osf_ls_files osf_download
#'
#' @examples
#' \dontrun{
#' local_folders <- c("data/project1", "data/project2")
#' osf_component_ids <- c("abc123", "def456")
#' download_osf_component(local_folders, osf_component_ids)
#' }
#'
#' @export

download_osf_component <- function(local_folder,
                                   osf_component_id,
                                   local_directory = "local_data/osf_pull") {
  purrr::walk2(local_folder, osf_component_id, function(folder, component_id) {
    download_component <- function(download_dir_folder, component_id) {
      tryCatch({
        # Check if directory exists
        if (!dir.exists(download_dir_folder)) {
          dir.create(download_dir_folder, recursive = TRUE)
        }
        node <- osfr::osf_retrieve_node(component_id)
        # Download data
        files <- osfr::osf_ls_files(node)
        osfr::osf_download(
          files,
          path = download_dir_folder,
          recurse = TRUE,
          conflicts = "overwrite"
        )
      }, error = function(e) {
        message(sprintf("Error downloading %s: %s", component_id, e$message))
      })
    }

    download_component(folder, component_id)
  })
}