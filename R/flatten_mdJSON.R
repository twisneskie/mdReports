#' Flatten metadata
#'
#' Flattens a useful subset of metadata fields from imported mdJSON files to be
#' used in analysis and reports.
#'
#' @param data A tibble of mdJSON files
#'
#' @return A partially flattened tibble of useful metadata fields for each
#' resource: title, responsible parties, resource type, dates, file location,
#' data dictionary, directory name, and associated resources.
#'
#' @importFrom rlang :=
#' @export
flatten_mdJSON <- function(data) {

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
    tidyr::unnest(dplyr::any_of(c("resourceType",
                                  "timePeriod",
                                  "pointOfContact",
                                  "status",
                                  "resourceDistribution")),
                  keep_empty = TRUE) %>%

    tidyr::unnest(dplyr::any_of(c("resourceType",
                                  "pointOfContact",
                                  "status")),
                  keep_empty = TRUE) %>%

    # Give resource "name" a unique and meaningful name
    dplyr::rename(resourceName = "name")

  # Add columns for startDate and endDate
  flat <- dplyr::mutate(flat,
                        startDate = as.Date(flat$startDateTime),
                        endDate = as.Date(flat$endDateTime))

  # Reformat personnel so that each role is a column that contains people in
  # that role as a comma separated list with their emails.
  roles <- unique(flat$role)

  contacts <- get_contacts(data)

  flat <- flat %>%

    dplyr::group_by(dplyr::across(-"party")) %>%

    tidyr::nest() %>%

    tidyr::pivot_wider(names_from = "role",
                       values_from = "data") %>%

    dplyr::ungroup()

  flat <- reformat_personnel(flat, contacts, roles)

  # Extract file location, if it exists
  possibly_extract <- purrr::possibly(extract_resourceDistribution,
                                      otherwise = dplyr::mutate(flat,
                                                                uri = "NULL"))
  flat <- possibly_extract(flat)

}

# Helper Functions -------------------------------------------------------------

# Extracts the resource URI from the resourceDistribution section
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

# Creates a table of unique contacts found in the metadata
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

# Reformat personnel so that each role is a column that contains people in
# that role as a comma separated list with their emails.
reformat_personnel <- function(data,
                               contacts,
                               personnel) {

  # Separate out the selected personnel roles from the rest of the data
  wholeData <- dplyr::select(data,
                             c(-personnel))

  reformatted <- dplyr::select(data,
                               c("metaId", personnel))

  # Unnest each role
  for(role in personnel) {

    reformatted <- unnest_personnel(reformatted,
                                    role,
                                    contacts)

  }

  # Reduce the data frame to just the title and roles
  reformatted <- reformatted %>%

    tidyr::gather(key = "role",
                  value = "person",
                  dplyr::any_of(personnel)) %>%

    dplyr::distinct(dplyr::across(c("metaId",
                                    "role",
                                    "person"))) %>%

    dplyr::group_by(dplyr::across(c("metaId",
                                    "role"))) %>%

    tidyr::nest() %>%

    tidyr::spread("role", "data") %>%

    dplyr::ungroup()

  # Put multiple personnel with the same role on the same line, comma separated
  for(role in personnel) {

    reformatted <- condense_personnel(reformatted, role)
  }

  # Add the personnel back to the main tibble
  reformatted <- dplyr::left_join(wholeData,
                                  reformatted,
                                  by = "metaId")

}

# Unnests personnel for a role and joins them with their email address(es), in
# parentheses
unnest_personnel <- function(reformatted, role, contacts) {

  reformatted <- reformatted %>%

    tidyr::unnest_wider(role) %>%

    tidyr::unnest_longer("party") %>%

    tidyr::unnest_wider("party") %>%

    tidyr::unnest_longer("contactId") %>%

    dplyr::left_join(contacts, by = "contactId") %>%

    dplyr::select(-"contactId") %>%

    tidyr::unnest_wider("electronicMailAddress",
                        names_sep = ".") %>%

    tidyr::unite("electronicMailAddress",
                 tidyr::contains("."),
                 sep = ", ",
                 na.rm = TRUE)

  # Get rid of leading/trailing spaces in emails
  # and add a closing parentheses
  reformatted <- dplyr::mutate(reformatted,
                               electronicMailAddress =
                                 reformatted$electronicMailAddress %>%
                                 stringr::str_trim() %>%
                                 stringr::str_c(")") %>%
                                 dplyr::na_if(")")) %>%

    dplyr::group_by(dplyr::across(-c("name",
                                     "electronicMailAddress"))) %>%

    tidyr::unite(!!role,
                 c("name", "electronicMailAddress"),
                 sep = " (",
                 na.rm = TRUE) %>%

    tidyr::nest() %>%

    tidyr::unnest_wider("data") %>%

    tidyr::unnest_wider(!!role, names_sep = ".") %>%

    tidyr::unite(!!role,
                 tidyr::contains("."),
                 sep = ", ",
                 na.rm = TRUE) %>%

    dplyr::ungroup()

}

# Condenses multiple people with the same data management role one comma
# separated line for display
condense_personnel <- function(reformatted, role) {

  reformatted %>%

    tidyr::unnest_wider(role) %>%

    tidyr::unnest_wider("person", names_sep = ".") %>%

    tidyr::unite("person",
                 dplyr::contains("."),
                 sep = ", ",
                 na.rm = TRUE) %>%

    dplyr::rename(!!role := "person")

}
