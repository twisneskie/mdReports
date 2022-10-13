#' General search
#'
#' Searches for records that contain a specified term in any part of the
#' metadata.
#'
#' @param data Imported mdJSON data
#' @param term Term or regex string to search for
#' @param col Column(s) to subset the search in. Select from: project,
#' resourceType, title, citation, abstract, shortAbstract, purpose, timePeriod,
#' status, pointOfContact, extent, taxonomy, keyword, constraint,
#' resourceMaintenance, resourceLineage, resourceDistribution,
#' associatedResource, or funding. Default selects all columns.
#'
#' @return A subset of resources that contain the specified term
#' @export
search_general <- function(data, term, col = "everything") {

  # Determine which resources contain the term
  if(col == "everything"){

    containsTerm <- data

  } else {

    containsTerm <- data %>% dplyr::select(col)

  }

  containsTerm <- containsTerm %>%

    purrr::map(as.character) %>%

    purrr::map(stringr::str_detect, stringr::regex(term, ignore_case = TRUE)) %>%

    tibble::as_tibble() %>%

    dplyr::rowwise() %>%

    dplyr::mutate(match = any(str_detect(c_across(), 'TRUE'))) %>%

    ungroup() %>%

    select(match)

  # Filter resources
  dmp <- data %>%

    dplyr::filter(containsTerm)

}
