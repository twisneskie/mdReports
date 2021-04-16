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

    tidyr::hoist(resources, contacts = "contact") %>%

    tidyr::unnest(contacts) %>%

    tidyr::unnest(contacts) %>%

    dplyr::select(contactId,
                  name,
                  electronicMailAddress) %>%

    #Get rid of duplicate entries
    dplyr::distinct()

}

#' Flatten metadata
#'
#' \code{flatten_mdJSON} flattens a useful subset of metadata fields from
#' imported mdJSON files to be used in analysis and reports.
#'
#' @param data A tibble of mdJSON files
#'
#' @return A partially flattened tibble of useful metadata fields for each
#' resource: title, responsible parties, resource type, dates, file location,
#' data dictionary, directory name, and associated resources.
#' @export
#'
#' @examples
flatten_mdJSON <- function(data) {

  # Run all helper functions
  data %>%

    extract_resourceInfo %>%

    extract_abstract %>%

    extract_status %>%

    extract_title %>%

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

extract_metadataInfo <- function(mdFiles){

  mdFiles %>%

    tidyr::hoist(metadata, metadataInfo = "metadataInfo") %>%

    tidyr::hoist(metadataInfo, metadataIdentifier = "metadataIdentifier") %>%

    # Unnest into identifier and namespace
    tidyr::unnest_wider(metadataIdentifier) %>%

    # Give identifier a unique and meaningful name
    dplyr::rename(metaId = "identifier")

}

extract_resourceDistribution <- function(mdFiles) {

  mdFiles %>%

    tidyr::hoist(metadata, resourceDistribution = "resourceDistribution") %>%

    tidyr::unnest_longer(resourceDistribution) %>%

    tidyr::hoist(resourceDistribution, distributor = "distributor") %>%

    tidyr::unnest_longer(distributor) %>%

    # Unnest contact and transferOption
    tidyr::unnest_wider(distributor) %>%

    tidyr::unnest_longer(transferOption) %>%

    # Unnest onlineOption and transferSize
    tidyr::unnest_wider(transferOption) %>%

    tidyr::unnest_longer(onlineOption) %>%

    # Unnest uri, name, and function
    tidyr::unnest_wider(onlineOption) %>%

    # Give "name" a unique and meaningful name
    dplyr::rename(onlineResourceName = name)

}

extract_associatedResource <- function(mdFiles) {

  tidyr::hoist(mdFiles,
               metadata,
               associatedResource = "associatedResource")
}

extract_dataDictionary <- function(mdFiles) {

  tidyr::hoist(mdFiles,
        resources,
        dataDictionary = "dataDictionary")

}

extract_resourceInfo <- function(mdFiles) {

  mdFiles %>%

    tidyr::hoist(metadata, resourceInfo = "resourceInfo")
}

# The following helper functions extract data from the resourceInfo column
# They must be run after running extract_resourceInfo

extract_abstract <- function(resourceInfo) {

  tidyr::hoist(resourceInfo,
               resourceInfo,
               abstract = "abstract")

}

extract_type <- function(resourceInfo) {

  resourceInfo %>%

    tidyr::hoist(resourceInfo, resourceType = "resourceType") %>%

    tidyr::unnest_longer(resourceType) %>%

    # Unnest into type and name
    tidyr::unnest_wider(resourceType) %>%

    # Give "name" a unique and meaningful name
    dplyr::rename(resourceName = name)

}

extract_taxonomy <- function(resourceInfo) {

  resourceInfo %>%

    tidyr::hoist(resourceInfo, taxonomy = "taxonomy") %>%

    tidyr::unnest_longer(taxonomy) %>%

    # Unnest taxonomicClassification and taxonomicSystem
    tidyr::unnest_wider(taxonomy)

}

extract_extent <- function(resourceInfo) {

  resourceInfo %>%

    tidyr::hoist(resourceInfo, extent = "extent") %>%

    tidyr::unnest(extent, keep_empty = TRUE)
}

extract_dates <- function(resourceInfo) {

  resourceInfo %>%

    tidyr::hoist(resourceInfo, timePeriod = "timePeriod") %>%

    # Unnest startDateTime, endDateTime, timeInterval, and description
    tidyr::unnest("timePeriod", keep_empty = TRUE) %>%

    # Create new columns with the start and end datetimes as just dates
    dplyr::mutate(startDate = as.Date(startDateTime)) %>%

    dplyr::mutate(endDate = as.Date(endDateTime))

}

extract_people <- function(resourceInfo) {

  resourceInfo %>%

    tidyr::hoist(resourceInfo, pointOfContact = "pointOfContact") %>%

    tidyr::unnest_longer(pointOfContact)%>%

    # Unnest party and role
    tidyr::unnest(pointOfContact, keep_empty = TRUE) %>%

    # Make each role its own column containing party
    dplyr::group_by(dplyr::across(-party)) %>%

    tidyr::nest() %>%

    tidyr::spread(role, data) %>%

    dplyr::ungroup()

}

extract_status <- function(resourceInfo) {

  resourceInfo %>%

    tidyr::hoist(resourceInfo, status = "status") %>%

    # Unnest status
    tidyr::unnest_longer("status")

}

extract_title <- function(resourceInfo) {

  resourceInfo %>%

    tidyr::hoist(resourceInfo, citation = "citation") %>%

    tidyr::hoist(citation, title = "title")

}
