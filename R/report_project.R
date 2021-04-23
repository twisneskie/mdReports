#' Report project details
#'
#' Generates reports for individual projects.
#'
#' @param data Flattened metadata created by the function
#' `flatten_mdJSON()`.
#' @param contacts List of contacts from the metadata with their metadata IDs.
#' @param project_list List of projects that individual reports will be
#' generated for.
#' @param output_dir Directory that reports will be saved to.
#'
#' @return An html report containing project details.
#' @export
report_projects <- function(data,
                            contacts,
                            project_list = data$projectName,
                            output_dir) {

  # Establish which template to use
  directory <- "inst\\rmarkdown\\templates\\projects-individual\\skeleton"

  file <- "projects-individual.Rmd"

  template <- paste(directory,
                    file,
                    sep = "\\")

  # Isolate ongoing projects
  projects <- project_summary(data,
                              contacts,
                              project_list)

  # Create a report for each ongoing project
  for(i in 1:nrow(projects)) {

    project <- projects[i,]

    rmarkdown::render(template,
                      params = list(project = project,
                                    contacts = contacts,
                                    metaSubset = data),
                      output_file = paste0(project$projectName,
                                           "_",
                                           as.Date(Sys.Date()),
                                           ".html"),
                      output_dir = output_dir)

  }

}
# Helper Functions -------------------------------------------------------------

# Takes sub-setted metadata for projects
# Extracts a subset of useful associated resource information,
# then renests the information by project
project_summary <- function(flat_data,
                            contacts,
                            project_list,
                            personnel = c("AKRegionDataTrustee",
                                          "AKRegionDataSteward",
                                          "AKRegionDataCustodian")) {

  projSum <- dplyr::filter(flat_data,
                           flat_data$type == "project")

  dplyr::filter(projSum,
                projSum$projectName %in% project_list) %>%

    reformat_personnel(contacts, personnel) %>%

    tidyr::nest(projPersonnel = personnel) %>%

    dplyr::select("projectName",
                  projTitle = "title",
                  "abstract",
                  "extent",
                  "projPersonnel",
                  "associatedResource") %>%

    tidyr::unnest_longer("associatedResource") %>%

    tidyr::hoist("associatedResource",
                 "resourceCitation",
                 "associationType",
                 "metadataCitation",
                 "resourceType") %>%

    tidyr::hoist("metadataCitation", "identifier") %>%

    tidyr::unnest(c("resourceCitation",
                    "associationType",
                    "identifier",
                    "resourceType")) %>%

    tidyr::hoist("identifier", metaId = "identifier") %>%

    tidyr::hoist("resourceType", prodType = "type") %>%

    dplyr::select("projectName",
                  "projTitle",
                  "abstract",
                  "extent",
                  "projPersonnel",
                  "title",
                  "responsibleParty",
                  "date",
                  "onlineResource",
                  "associationType",
                  "prodType",
                  "metaId") %>%

    tidyr::unnest("responsibleParty", keep_empty = TRUE) %>%

    dplyr::group_by(dplyr::across(c(-"party"))) %>%

    tidyr::nest() %>%

    dplyr::ungroup() %>%

    tidyr::spread(key = "role",
                  value = "data") %>%

    dplyr::select(dplyr::any_of(c("projectName",
                                  "projTitle",
                                  "abstract",
                                  "projPersonnel",
                                  "extent",
                                  "title",
                                  "originator",
                                  "date",
                                  "onlineResource",
                                  "associationType",
                                  "prodType",
                                  "metaId"))) %>%

    dplyr::group_by(dplyr::across(c("projectName",
                                    "projTitle",
                                    "abstract",
                                    "projPersonnel"))) %>%

    tidyr::nest() %>%

    dplyr::ungroup() %>%

    tidyr::unnest_wider("projPersonnel")

}

# Function that generates the table for chunk 3: all.resources
all_resources <- function(project, metaSubset, contacts) {

  allResources <- project %>%

    tidyr::unnest("data") %>%

    dplyr::select(dplyr::any_of(c("title",
                                  "originator",
                                  "prodType",
                                  "metaId")))

  reformat_possibly <- purrr::possibly(reformat_personnel,
                                       otherwise = dplyr::mutate(allResources,
                                                                 originator =
                                                                   "NULL"))

  reformat_possibly(allResources,
                    contacts,
                    personnel = c("originator")) %>%

    dplyr::left_join(dplyr::select(metaSubset,
                                   "metaId",
                                   "status",
                                   "uri",
                                   "dataDictionary"),
                     by = "metaId") %>%

    dplyr::mutate(hasMetadata = dplyr::case_when(!is.na(status) ~
                                                   "Completed",
                                                 is.na(status) ~
                                                   "Initiated")) %>%

    dplyr::mutate(originator = dplyr::case_when(originator == "NULL" ~
                                                  "None assigned",
                                                originator != "" ~
                                                  originator)) %>%

    tidyr::replace_na(list(uri = "File Not Archived")) %>%

    dplyr::select(-c("metaId", "status")) %>%

    dplyr::select("title",
                  "prodType",
                  "hasMetadata",
                  "originator",
                  "uri")

}

# Function that generates the table for chunk 4: data.dictionaries
data_dictionary <- function(project, metaSubset) {

  dictionaryTable <- project %>%

    tidyr::unnest("data") %>%

    dplyr::select("title",
                  "prodType",
                  "metaId") %>%

    dplyr::left_join(dplyr::select(metaSubset,
                                   "metaId",
                                   "status",
                                   "uri",
                                   "dataDictionary"),
                     by = "metaId")

    dplyr::filter(dictionaryTable,
                  dictionaryTable$dataDictionary != "NULL") %>%

      dplyr::select("title")

}
