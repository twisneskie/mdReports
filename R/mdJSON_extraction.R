#' Flatten metadata
#'
#' @description
#' Flattens a useful subset of metadata fields from imported mdJSON files to be
#' used in analysis and reports.
#'
#' @param data A tibble of mdJSON files
#'
#' @return A partially flattened tibble of useful metadata fields for each
#' resource: title, responsible parties, resource type, dates, file location,
#' data dictionary, directory name, and associated resources.
#' @export
flatten_mdJSON <- function(data) {

  # Run helper functions
  flat <- data %>%

    # Extract all fields under "metadata"
    tidyr::hoist("metadata",
                 "resourceInfo",
                 "resourceLineage",
                 "resourceDistribution",
                 "associatedResource") %>%

    # Extract all fields under "resourceInfo"
    tidyr::hoist("resourceInfo",
                 "resourceType",
                 "citation",
                 "pointOfContact",
                 "abstract",
                 "status",
                 "timePeriod") %>%

    # Expand some fields needed for reports
    tidyr::unnest(c("resourceType",
                    "timePeriod",
                    "pointOfContact",
                    "status",
                    "resourceDistribution"),
                  keep_empty = TRUE) %>%

    tidyr::unnest(c("resourceType",
                    "pointOfContact",
                    "status"),
                  keep_empty = TRUE) %>%

    # Give "name" a unique and meaningful name
    dplyr::rename(resourceName = "name")

  # Add columns for startDate and endDate
  flat <- dplyr::mutate(flat,
                        startDate = as.Date(flat$startDateTime),
                        endDate = as.Date(flat$endDateTime),
                        keep = "unused")

  # Reformat personnel
  roles <- unique(flat$role)

  contacts <- get_contacts(data)

  flat <- flat %>%

    dplyr::group_by(dplyr::across(-"party")) %>%

    tidyr::nest() %>%

    tidyr::pivot_wider(names_from = "role",
                       values_from = "data") %>%

    dplyr::ungroup()

  flat <- reformat_personnel(flat, contacts, roles)

  # Extract file location
  possibly_extract <- purrr::possibly(extract_resourceDistribution,
                                      otherwise = dplyr::mutate(flat,
                                                                uri = "NULL"))
  possibly_extract(flat)

}

# Helper Functions -------------------------------------------------------------

#' Extract resourceDistribution column
#'
#' @param mdFiles The `data` argument from the `flatten_mdJSON()`
#' function.
#'
#' @return The input tibble with `resourceDistribtuion`, `contact`,
#' `uri`, `onlineResourceName`, `function`, and
#' `transferSize` as top level columns.
extract_resourceDistribution <- function(mdFiles) {

  mdFiles %>%

    tidyr::hoist("resourceDistribution", "distributor") %>%

    tidyr::unnest("distributor", keep_empty = TRUE) %>%

    tidyr::hoist("distributor",
                 "transferOption") %>%

    tidyr::unnest("transferOption", keep_empty = TRUE) %>%

    tidyr::hoist("transferOption",
                 "onlineOption") %>%

    tidyr::unnest("onlineOption", keep_empty = TRUE) %>%

    tidyr::hoist("onlineOption", "uri")

}

#' Get metadata contacts
#'
#' Creates a table of contacts associated with metadata resources
#'
#' @param data A tibble of mdJSON files
#'
#' @return A tibble containing contact ID, contact name and email address for
#' contacts found in imported metadata.
#' @export
get_contacts <- function(data) {

  data %>%

    tidyr::unnest("contact") %>%

    tidyr::unnest("contact") %>%

    dplyr::select("contactId",
                  "name",
                  "electronicMailAddress") %>%

    #Get rid of duplicate entries
    dplyr::distinct()

}


