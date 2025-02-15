#' Create Prevalence Data Frames from a Topic Model
#'
#' This function generates three data frames from a given topic model: 
#' a document-topic matrix, a wide-format document-topic matrix with dummy variables, 
#' and a data frame with the top topic for each document.
#'
#' @param model A topic model object.
#' @param sort A logical value indicating whether to sort the resulting data frames by document. Default is FALSE.
#'
#' @return A list containing three data frames:
#' \item{td_gamma}{A data frame with the document-topic matrix.}
#' \item{gamma_dummies}{A wide-format data frame with dummy variables for each topic.}
#' \item{top_gamma}{A data frame with the top topic for each document and its corresponding gamma value.}
#'
#' @examples
#' \dontrun{
#' model <- stm::stm(documents, vocab, K = 5, max.em.its = 75)
#' prevalence_data <- createPrevalence(model, sort = TRUE)
#' }
#' 
#' @import dplyr
#' @import tidytext
#' @import tidyr
#' @export
createPrevalence <- function(model, sort = FALSE) { 

  # Create document-topic matrix (gamma)
  td_gamma <- model %>%
      tidytext::tidy(matrix = "gamma")

  gamma_dummies <- td_gamma %>%
    dplyr::group_by(document) %>% 
    tidyr::pivot_wider(
      id_cols = document, 
      names_from = "topic", 
      names_prefix = "gamma_topic_",
      values_from = "gamma") %>% 
    dplyr::rename(doc_id_gamma = document)
    
  top_gamma <- td_gamma %>% 
    dplyr::group_by(document) %>% 
    dplyr::slice_max(gamma) %>% 
    dplyr::mutate(main_topic = ifelse(gamma > 0.5, topic, NA)) %>% 
    dplyr::rename(
      top_topic = topic,
      top_gamma = gamma)

  # Sort the data frames if the sort parameter is TRUE
  if (sort) {
    td_gamma <- td_gamma %>% dplyr::arrange(document)
    gamma_dummies <- gamma_dummies %>% dplyr::arrange(doc_id_gamma)
    top_gamma <- top_gamma %>% dplyr::arrange(document)
  }

  # Create a list to store the resulting data frames
  result_list <- list(
    td_gamma = td_gamma,
    gamma_dummies = gamma_dummies,
    top_gamma = top_gamma
  )

  return(result_list)
}