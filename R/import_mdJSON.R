#' Import mdJSON metadata
#'
#' Converts metadata files using the mdJSON metadata standard into a tibble
#' for further analysis in R. This function may take several minutes to run,
#' depending on the number of folders and JSON files in the given directory.
#'
#' @param path Path to directory that will be searched for metadata files.
#' @return A tibble containing imported mdJSON metadata. Tibble
#' columns include metadata identifier and the top level metadata elements.
#' @export

import_mdJSON <- function(path) {

  # Import JSON files from the selected directory into a tibble of the top
  # level elements: schema, contact, metadata, metadata repository, and
  # data dictionary
  json <- purrr::map(list.files(path,
                                pattern = "*.json",
                                recursive = TRUE,
                                full.names = TRUE),
                     jsonlite::fromJSON) %>%

    tibble::tibble() %>%

    dplyr::rename(resources = ".") %>%

    tidyr::hoist("resources",
                 "metadata",
                 "schema",
                 "contact",
                 "metadataRepository",
                 "dataDictionary")

  # Drop rows that aren't from mdJSON files
  # (i.e. mdEditor files, JSON data products)
  dplyr::filter(json, json$metadata != "NULL") %>%

    # Extract out metadata identifier from metadata
    tidyr::hoist("metadata", "metadataInfo") %>%

    tidyr::hoist("metadataInfo", "metadataIdentifier") %>%

    tidyr::unnest_wider("metadataIdentifier") %>%

    # Give the identifier a meaningful column name
    dplyr::rename(metaId = "identifier") %>%

    # Drop unnecessary columns
    dplyr::select(-dplyr::any_of(c("namespace",
                                   "resources")))

}
