#' Condense projects
#'
#' Takes in flat table of metadata and extracts a subset of associated resource
#' information by project
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
condense_projects <- function(data) {


  # Filter results to projects and select important project information
  dplyr::filter(data,
                data$type == "project",
                data$projectName %in% project_list) %>%

    tidyr::nest(projectPersonnel = essential_personnel) %>%

    dplyr::select(dplyr::any_of(c("projectName",
                                  projectTitle = "title",
                                  "abstract",
                                  "projectPersonnel",
                                  "associatedResource"))) %>%

    # Get information from resources associated with the projects
    tidyr::unnest_longer("associatedResource") %>%

    tidyr::hoist("associatedResource",
                 "resourceCitation",
                 "metadataCitation",
                 "resourceType") %>%

    tidyr::hoist("metadataCitation",
                 metadataCitationIdentifier = "identifier") %>%

    tidyr::unnest(c("resourceCitation",
                    "metadataCitationIdentifier",
                    "resourceType")) %>%

    # Get metadata identifier for the associated product
    tidyr::hoist("metadataCitationIdentifier", metaId = "identifier") %>%

    # Get the associated resource type
    tidyr::hoist("resourceType", productType = "type") %>%


    dplyr::select("projectName",
                  "projectTitle",
                  "abstract",
                  "projectPersonnel",
                  "metaId",
                  "title",
                  "productType") %>%

    dplyr::left_join(dplyr::select(data,
                                   dplyr::any_of(c("metaId",
                                                   "status",
                                                   "uri",
                                                   "dataDictionary")),
                                   dplyr::contains("originator")),
                     by = "metaId") %>%

    # Turn any blank fields into NAs before merging originator fields to
    # prevent commas where there shouldn't be any after joining originator
    # fields
    dplyr::mutate_all(dplyr::na_if,"") %>%

    tidyr::unite(col = "dataOriginator",
                 dplyr::contains("originator"),
                 sep = ", ",
                 na.rm = TRUE) %>%

    # Nest associated resources so there is one project per row
    dplyr::group_by(dplyr::across(c("projectName",
                                    "projectTitle",
                                    "abstract",
                                    "projectPersonnel"))) %>%

    tidyr::nest() %>%

    dplyr::ungroup()
}
