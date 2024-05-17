#' Download data folder from OSF
#'
#' This function is design to download all files from (at least) one OSF component.
#'
#' @param local_folder A character vector specifying the folder(s) to save the downloaded data into. Will be used to add to argument 'local_directory'.
#' @param local_directory The directory where data will be downloaded. Default is "local_data/osf_pull".
#' @param osf_component A character vector specifying the corresponding node ID(s) for each component.
#' @param osf_node An OSF project or (super-)component ID, which is the parent of the component(s) to download.
#'
#' @return No explicit return value. Data will be downloaded to the specified directory.
#' @importFrom osfr osf_download osf_ls_nodes osf_ls_files
#' @importFrom here here
#' @export
#' @examples
#' \dontrun{
#' local_folder <- c("base")
#' osf_component <- c("Base data")
#' osf_node <- osfr::osf_retrieve_node("Your OSF project/component")
#' download_osf_data(local_folder, osf_component, osf_node)
#' }

download_osf_component <- function(
    local_folder,
    osf_component,
    osf_node,
    # optional
    local_directory = "local_data/osf_pull") {

  for (i in seq_along(local_folder)) {
    download_folder <- local_folder[i]
    osf_node_component <- osf_component[i]

    # Check if directory exists
    download_dir_folder <- here::here(local_directory, download_folder)
    if (!dir.exists(download_dir_folder)) {
      dir.create(download_dir_folder, recursive = TRUE)
    }

    # Download data
    osfr::osf_download(
      osf_node |>
        osfr::osf_ls_nodes(osf_node_component) |>
        osfr::osf_ls_files(n_max = 100),
      path = download_dir_folder,
      recurse = TRUE,
      conflicts = "overwrite",
      verbose = TRUE
    )
  }
}
