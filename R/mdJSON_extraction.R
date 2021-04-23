##################
# mdJSON Extraction
# Functions and helpers that extract information from imported mdJSON files
# Created by Theresa Wisneskie (theresa_wisneskie@fws.gov)
##################
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

#' Flatten metadata
#'
#' @description
#'`r lifecycle::badge("deprecated")`
#'
#'Flattens a useful subset of metadata fields from imported mdJSON files to be
#' used in analysis and reports.
#'
#' @param data A tibble of mdJSON files
#'
#' @return A partially flattened tibble of useful metadata fields for each
#' resource: title, responsible parties, resource type, dates, file location,
#' data dictionary, directory name, and associated resources.
#' @export
flatten_mdJSON <- function(data) {

  # Run all helper functions
  data %>%

    extract_resourceInfo %>%

    extract_abstract %>%

    extract_status %>%

    extract_citation %>%

    extract_people %>%

    extract_type %>%

    extract_extent %>%

    extract_taxonomy %>%

    extract_dates %>%

    extract_associatedResource %>%

    extract_resourceDistribution %>%

    extract_metadataInfo %>%

    extract_dataDictionary

}

# Helper Functions -------------------------------------------------------------

#' Extract metadataInfo column
#'
#' @param mdFiles The `data` argument from the `flatten_mdJSON()`
#' function.
#'
#' @return The input tibble with `metadataInfo`, `metaId`, and
#' `namespace` as top level columns.
extract_metadataInfo <- function(mdFiles){

  test <- data %>%

    tidyr::hoist("metadata", "metadataInfo") %>%

    tidyr::hoist("metadataInfo", "metadataIdentifier") %>%

    # Unnest into identifier and namespace
    tidyr::unnest_wider("metadataIdentifier") %>%

    # Give identifier a unique and meaningful name
    dplyr::rename(metaId = "identifier") %>%

    dplyr::select(-"namespace")

}

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

    tidyr::hoist("metadata", "resourceDistribution") %>%

    tidyr::unnest_longer("resourceDistribution") %>%

    tidyr::hoist("resourceDistribution", "distributor") %>%

    tidyr::unnest_longer("distributor") %>%

    # Unnest contact and transferOption
    tidyr::unnest_wider("distributor") %>%

    tidyr::unnest_longer("transferOption") %>%

    # Unnest onlineOption and transferSize
    tidyr::unnest_wider("transferOption") %>%

    tidyr::unnest_longer("onlineOption") %>%

    # Unnest uri, name, and function
    tidyr::unnest_wider("onlineOption") %>%

    # Give "name" a unique and meaningful name
    dplyr::rename(onlineResourceName = "name")

}

#' Extract associatedResource column
#'
#' @param mdFiles The `data` argument from the `flatten_mdJSON()`
#' function.
#'
#' @return The input tibble with `resourceDistribtuion`, `contact`,
#' `uri`, `onlineResourceName`, `function`, and
#' `transferSize` as top level columns.
extract_associatedResource <- function(mdFiles) {

  tidyr::hoist(mdFiles,
               "metadata",
               "associatedResource")
}

#' Extract dataDictionary column
#'
#' @param mdFiles The `data` argument from the `flatten_mdJSON()`
#' function.
#'
#' @return The input tibble with `dataDictionary` as a top level column.
extract_dataDictionary <- function(mdFiles) {

  tidyr::hoist(mdFiles,
               "resources",
               "dataDictionary")

}

#' Extract resourceInfo column
#'
#' @param mdFiles The `data` argument from the `flatten_mdJSON()`
#' function.
#'
#' @return The input tibble with `resourceInfo` as a top level column.
extract_resourceInfo <- function(mdFiles) {

  tidyr::hoist(mdFiles,
               "metadata",
               "resourceInfo")

}

#' Extract abstract column
#'
#' @param resourceInfo The output from the from the
#' `extract_resourceInfo()` function.
#'
#' @return The input tibble with `abstract` as a top level column.
extract_abstract <- function(resourceInfo) {

  tidyr::hoist(resourceInfo,
               "resourceInfo",
               "abstract")

}

#' Extract resource type column
#'
#' @param resourceInfo The output from the from the
#' `extract_resourceInfo()` function.
#'
#' @return The input tibble with `type` and `resourceName` as top
#' level columns.
extract_type <- function(resourceInfo) {

  resourceInfo %>%

    tidyr::hoist("resourceInfo", "resourceType") %>%

    tidyr::unnest_longer("resourceType") %>%

    # Unnest into type and name
    tidyr::unnest_wider("resourceType") %>%

    # Give "name" a unique and meaningful name
    dplyr::rename(resourceName = "name")

}

#' Extract taxonomy column
#'
#' @param resourceInfo The output from the from the
#' `extract_resourceInfo()` function.
#'
#' @return The input tibble with `taxonomicClassification` and
#' `taxonomicSystem` as top level columns.
extract_taxonomy <- function(resourceInfo) {

  resourceInfo %>%

    tidyr::hoist("resourceInfo", "taxonomy") %>%

    tidyr::unnest_longer("taxonomy") %>%

    # Unnest taxonomicClassification and taxonomicSystem
    tidyr::unnest_wider("taxonomy")

}

#' Extract extent column
#'
#' @param resourceInfo The output from the from the
#' `extract_resourceInfo()` function.
#'
#' @return The input tibble with `extent` as a top level column.
extract_extent <- function(resourceInfo) {

  resourceInfo %>%

    tidyr::hoist("resourceInfo", "extent") %>%

    tidyr::unnest("extent", keep_empty = TRUE)
}

#' Extract dates columns
#'
#' @param resourceInfo The output from the from the
#' `extract_resourceInfo()` function.
#'
#' @return The input tibble with `startDateTime` and `endDateTime`
#' as top level columns. Creates columns `startDate` and `endDate`.
extract_dates <- function(resourceInfo) {

  dates <- resourceInfo %>%

    tidyr::hoist("resourceInfo", "timePeriod") %>%

    # Unnest startDateTime, endDateTime, timeInterval, and description
    tidyr::unnest("timePeriod", keep_empty = TRUE)

    # Create new columns with the start and end datetimes as just dates
  dates <- dplyr::mutate(dates,
                         startDate = as.Date(dates$startDateTime))

  dplyr::mutate(dates,
                endDate = as.Date(dates$endDateTime))

}

#' Extract pointOfContact column
#'
#' @param resourceInfo The output from the from the
#' `extract_resourceInfo()` function.
#'
#' @return The input tibble with all personnel roles as top level columns.
extract_people <- function(resourceInfo) {

  resourceInfo %>%

    tidyr::hoist("resourceInfo", "pointOfContact") %>%

    tidyr::unnest_longer("pointOfContact")%>%

    # Unnest party and role
    tidyr::unnest("pointOfContact", keep_empty = TRUE) %>%

    # Make each role its own column containing party
    dplyr::group_by(dplyr::across(-"party")) %>%

    tidyr::nest() %>%

    tidyr::spread("role", "data") %>%

    dplyr::ungroup()

}

#' Extract status column
#'
#' @param resourceInfo The output from the from the
#' `extract_resourceInfo()` function.
#'
#' @return The input tibble with `status` as a top level column.
extract_status <- function(resourceInfo) {

  resourceInfo %>%

    tidyr::hoist("resourceInfo", "status") %>%

    # Unnest status
    tidyr::unnest_longer("status")

}

#' Extract citation column
#'
#' @param resourceInfo The output from the from the
#' `extract_resourceInfo()` function.
#'
#' @return The input tibble with `citation` and `title` as top level
#' columns.
extract_citation <- function(resourceInfo) {

  resourceInfo %>%

    tidyr::hoist("resourceInfo", "citation") %>%

    tidyr::hoist("citation", "title")

}
