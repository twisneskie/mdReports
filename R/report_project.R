#' Report project details
#'
#' Generates reports for individual projects.
#'
#' @param data Flattened metadata created by the function
#' \code{flatten_mdJSON()}.
#' @param contacts List of contacts from the metadata with their metadata IDs.
#' @param project_list List of projects that individual reports will be
#' generated for.
#'
#' @return An html report containing project details.
#' @export
report_projects <- function(data,
                            contacts,
                            project_list = data$projectName) {

  #Isolate ongoing projects
  projects <- project_summary(data,
                              contacts,
                              project_list)

  # Create a report for each ongoing project
  for(i in 1:nrow(projects)) {

    project <- projects[i,]

    rmarkdown::render("reports\\02_project-report.Rmd",
                      params = list(project = project,
                                    contacts = contacts,
                                    metaSubset = data),
                      output_file = paste0(project$projectName,
                                           "_",
                                           as_date(Sys.Date()),
                                           ".html"))

  }

}
# Helper Functions -------------------------------------------------------------

# Takes sub-setted metadata for projects
# Extracts a subset of useful associated resource information,
# then renests the information by project
project_summary <- function(metaSubset,
                            contacts,
                            projectList,
                            personnel = c("AKRegionDataTrustee",
                                          "AKRegionDataSteward",
                                          "AKRegionDataCustodian")) {

  metaSubset %>%

    filter(type == "project") %>%

    filter(dirName %in% projectList) %>%

    list_personnel(contacts, otherColumns = c("dirName",
                                              "abstract",
                                              "extent",
                                              "associated")) %>%

    nest(projPersonnel = personnel) %>%

    select(dirName,
           projTitle = title,
           abstract,
           extent,
           projPersonnel,
           associated) %>%

    unnest_longer(associated) %>%

    hoist(associated,
          resourceCitation = "resourceCitation",
          associationType = "associationType",
          metadataCitation = "metadataCitation",
          resourceType = "resourceType") %>%

    hoist(metadataCitation, identifier = "identifier") %>%

    unnest(c(resourceCitation,
             associationType,
             identifier,
             resourceType)) %>%

    hoist(identifier, metaId = "identifier") %>%

    hoist(resourceType, prodType = "type") %>%

    select(dirName,
           projTitle,
           abstract,
           extent,
           projPersonnel,
           title,
           responsibleParty,
           date,
           onlineResource,
           associationType,
           prodType,
           metaId) %>%

    unnest(responsibleParty, keep_empty = TRUE) %>%

    group_by(across(c(-party))) %>%

    nest %>%

    spread(key = role,
           value = data) %>%

    select(any_of(c("dirName",
                    "projTitle",
                    "abstract",
                    "extent",
                    "projPersonnel",
                    "title",
                    "originator",
                    "date",
                    "onlineResource",
                    "associationType",
                    "prodType",
                    "metaId"))) %>%

    group_by(dirName,
             projTitle,
             abstract,
             extent,
             projPersonnel) %>%

    nest %>%

    ungroup %>%

    unnest_wider(projPersonnel)

}

# Create Report Tables for 02_project-report -----------------------------------
# Creates functions to generate tables for project reports

# Function that generates the table for chunk 3: all.resources
all_resources <- function(project, metaSubset, contacts) {

  allResources <- project %>%

    unnest(data) %>%

    select(any_of(c("title",
                    "originator",
                    "prodType",
                    "metaId")))

  list_personnel_possibly <- possibly(list_personnel,
                                      otherwise =
                                        mutate(allResources,
                                               originator = "NULL"))

  list_personnel_possibly(allResources,
                          contacts,
                          personnel = c("originator"),
                          otherColumns = c("prodType", "identifier")) %>%

    left_join(select(metaSubset,
                     metaId,
                     status,
                     uri,
                     dictionary),
              by = "metaId") %>%

    mutate(hasMetadata = case_when(!is.na(status) ~ "Completed",
                                   is.na(status) ~ "Initiated")) %>%

    mutate(originator = case_when(originator == "NULL" ~ "None assigned",
                                  originator != "" ~ originator)) %>%

    mutate(uri = replace_na(uri, "File Not Archived")) %>%

    select(-c(metaId, status)) %>%

    select(title,
           "prodType",
           "hasMetadata",
           "originator",
           "uri")

}

# Function that generates the table for chunk 4: data.dictionaries
data_dictionary <- function(project, metaSubset) {

  project %>%

    tidyr::unnest(data) %>%

    dplyr::select(title,
           prodType,
           metaId) %>%

    dplyr::left_join(select(metaSubset,
                     metaId,
                     status,
                     uri,
                     dictionary),
              by = "metaId") %>%

    dplyr::filter(dictionary != "NULL") %>%

    dplyr::select(title)

}

