#' Get Contacts
#'
#' @param data Any number of mdJSON metadata records that have been flattened
#' into a tidy tibble
#'
#' @return A tibble containing all contacts found within the metadata records,
#' including their contactIds, names, and email addresses.
#' @export

get_contacts <- function(data) {

  # Unnest data in the "Contacts" field and make it its own table.
  # "Contacts" is a top-level mdJSON property that holds all contacts in each
  # mdJSON record.
  data %>%

    tidyr::unnest("contact") %>%

    tidyr::unnest("contact") %>%

    dplyr::select("contactId",
                  "name",
                  "electronicMailAddress") %>%

    # Get rid of duplicate entries
    dplyr::distinct()

}
