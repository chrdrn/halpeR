#' Send a Pushover Notification for Job Status
#'
#' This function sends a Pushover notification indicating the status of a job.
#'
#' @param pushover_title A character string specifying the title of the notification.
#' @param pushover_job A character string specifying the name of the job.
#' @param status A character string specifying the status of the job. 
#'               Must be either "started" or "finished".
#'
#' @return Sends a Pushover notification and returns the response object.
#' 
#' @examples
#' \dontrun{
#' pushover_status("Job Notification", "Data Analysis", "started")
#' pushover_status("Job Notification", "Data Analysis", "finished")
#' }
pushover_status <- function(pushover_title, pushover_job, status) {
    # Check if the status is valid
    if (!status %in% c("started", "finished")) {
        stop("Invalid status. Status must be either 'started' or 'finished'.")
    }
    
    # Create the message
    pushover_message <- glue::glue("{pushover_job} {status} @ {Sys.time()}")
    
    # Send the pushover notification
    pushoverr::pushover(
        title = pushover_title,
        message = pushover_message
    )
}