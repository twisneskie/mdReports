#' Report overview of project metadata
#'
#' Generates an html report showing details on all imported projects.
#'
#' Reports contain a list of all projects, the number and types of data
#' products, a searchable table of all ongoing project details, and a table of
#' critical project personnel.
#'
#' @param data Flattened metadata created by the function
#' `flatten_mdJSON()`.
#' @param contacts List of contacts from the metadata with their metadata IDs.
#' @param output_name Name to be given to the report generated.
#' @param output_dir Directory that the report should be generated in.
#'
#' @return An html report containing project details.
#' @export
report_overview <- function(data,
                            contacts,
                            output_name,
                            output_dir){

  directory <- "inst\\rmarkdown\\templates\\projects-overview\\skeleton"

  file <- "report_overview.Rmd"

  template <- paste(directory,
                     file,
                     sep = "\\")

  rmarkdown::render(template,
                    params = list(data = data,
                                  contacts = contacts),
                    output_file = paste0(output_name,
                                         as.Date(Sys.Date()),
                                         ".html"),
                    output_dir = output_dir)
}

# Helper functions -------------------------------------------------------------

# Table for chunk 2: list.projects
listProjects <- function(data, contacts) {

  projectList <- data %>%

    reformat_personnel(contacts,
                       personnel = c("principalInvestigator"))

  dplyr::filter(projectList,
                projectList$type == "project") %>%

    dplyr::select("title",
                  "projectName",
                  "principalInvestigator")

}

# Table for chunk 3: types.of.products
typesProducts <- function(data) {

  productTable <- data %>%

    dplyr::select("type")


  productTable <- dplyr::filter(productTable,
                                productTable$type != "project")


  productTable <- productTable %>%

    dplyr::group_by(dplyr::across(c("type"))) %>%

    dplyr::count() %>%

    dplyr::ungroup()


  tibble::add_row(productTable,
                  type = "Total",
                  n = sum(productTable$n))

}

# Table for chunk 4: ongoing.projects
ongoingProjects <- function(data, contacts) {

  ongoingTable <- data %>%

    reformat_personnel(contacts,
                       personnel = c("principalInvestigator"))

  dplyr::filter(ongoingTable,
                ongoingTable$type == "project",
                ongoingTable$status == "onGoing") %>%

    dplyr::select("title",
                  "projectName",
                  "startDate",
                  "endDate",
                  "principalInvestigator")

}

# Table for chunk 5: dm.personnel
dmPersonnel <- function(data, contacts) {

  personnelTable <- reformat_personnel(data,
                                       contacts,
                                       personnel = c("AKRegionDataTrustee",
                                                     "AKRegionDataSteward",
                                                     "AKRegionDataCustodian"))

    dplyr::filter(personnelTable,
                  personnelTable$type == "project",
                  personnelTable$status == "onGoing") %>%

    dplyr::select("title",
                  "AKRegionDataTrustee",
                  "AKRegionDataSteward",
                  "AKRegionDataCustodian")

}
