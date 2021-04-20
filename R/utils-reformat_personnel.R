#' Reformat personnel
#'
#' For each data management role requested, changes the data from a nested list
#' to a comma separated list of personnel with their email addresses.
#'
#' @param data Tibble of flattened mdJSON resources
#' @param contacts Tibble of personnel extracted from the metadata
#' @param personnel List of personnel roles to be reformatted
#'
#' @importFrom rlang :=
#'
#' @return A tibble with reformatted personnel columns
reformat_personnel <- function(data,
                               contacts,
                               personnel) {

  # Separate out the selected personnel roles from the rest of the data
  wholeData <- dplyr::select(data,
                             c(-personnel))

  reformatted <- dplyr::select(data,
                               c("title", personnel))

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

    dplyr::distinct(dplyr::across(c("title",
                                    "role",
                                    "person"))) %>%

    dplyr::group_by(dplyr::across(c("title",
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
                                  by = "title")

}

# Helper functions -------------------------------------------------------------

unnest_personnel <- function(reformatted, role, contacts) {

  reformatted %>%

    tidyr::unnest_wider(role) %>%

    tidyr::unnest_longer("party") %>%

    tidyr::unnest_wider("party") %>%

    tidyr::unnest_longer("contactId") %>%

    dplyr::left_join(contacts, by = "contactId") %>%

    dplyr::select(-"contactId") %>%

    dplyr::group_by(dplyr::across(c(-"name",
                                    -"electronicMailAddress"))) %>%

    tidyr::unnest_wider("electronicMailAddress",
                        names_sep = ".") %>%

    tidyr::unite("electronicMailAddress",
                 tidyr::contains("."),
                 sep = ", ",
                 na.rm = TRUE) %>%

    # Get rid of leading/trailing spaces in emails
    # and add a closing parentheses
    dplyr::mutate(electronicMailAddress = "electronicMailAddress" %>%
                    stringr::str_trim() %>%
                    stringr::str_c(")") %>%
                    dplyr::na_if(")")) %>%

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

# Sub-function that condenses multiple people with the same
# data management role one comma separated line for display
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
