#' General search
#'
#' Searches for records that contain a specified term in any part of the
#' metadata.
#'
#' @param data
#' @param term
#'
#' @return A subset of resources that contain the specified term
#' @export
search_general <- function(data, term) {

  # Determine which resources contain the term
  containsTerm <- data %>%

    purrr::map(as.character) %>%

    purrr::map(stringr::str_detect, stringr::regex(term, ignore_case = TRUE)) %>%

    purrr::pluck("metadata") %>%

    tibble::as_tibble()

  # Filter resources
  data %>%

    dplyr::filter(containsTerm)

}

#' Search for a taxon
#'
#' Searches for resources that contain a specific taxon.
#'
#' @param data
#' @param taxon
#'
#' @return A subset of resources that contain a specific taxon.
#' @export
search_taxon <- function(data, taxon){

  #Make a list of which resources contain the taxon
  expandTaxonomy <- data %>%

    tidyr::hoist("metadata",
                 "resourceInfo") %>%

    tidyr::hoist("resourceInfo",
                 "taxonomy") %>%

    tidyr::unnest("taxonomy", keep_empty = TRUE) %>%

    tidyr::unnest("taxonomy", keep_empty = TRUE)

  containsTaxon <- dplyr::select(expandTaxonomy,
                                 "taxonomicClassification") %>%

    purrr::map(as.character) %>%

    purrr::map(stringr::str_detect, stringr::regex(taxon, ignore_case = T)) %>%

    tibble::as_tibble()

  #Filter the resources and return important resource info
  data %>%

    dplyr::filter(containsTaxon)

}

# Search by time periods

# Search for update needed
