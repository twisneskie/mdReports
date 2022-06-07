#' Report overview of project metadata
#'
#' Generates an html report showing details on all projects for a single
#' program.
#'
#' Reports contain a list of all projects, the number and types of data
#' products, a searchable table of all ongoing project details, and a table of
#' critical project personnel.
#'
#' @param data Imported mdJSON files.
#' @param contact_person The role for the project's point of contact
#' @param essential_personnel Essential personnel titles for projects.
#' @param output_name Name to be given to the report generated.
#' @param output_dir Directory that the report should be generated in.
#'
#' @return An html report containing project details.
#' @export
report_overview <- function(data,
                            contact_person = "principalInvestigator",
                            essential_personnel = c("AKRegionDataTrustee",
                                                    "AKRegionDataSteward",
                                                    "AKRegionDataCustodian"),
                            output_name,
                            output_dir){

  # Construct file path to report template
  directory <- "inst\\rmarkdown\\templates\\projects-overview\\skeleton"

  file <- "report_overview.Rmd"

  template <- paste(directory,
                     file,
                     sep = "\\")

  # Partially flatten mdJSON tibble
  data <- flatten_mdJSON(data)

  # Render html reports
  rmarkdown::render(template,
                    params = list(data = data,
                                  contact_person = contact_person,
                                  essential_personnel = essential_personnel),
                    output_file = paste0(output_name,
                                         "_",
                                         as.Date(Sys.Date()),
                                         ".html"),
                    output_dir = output_dir)
}

#' Report project details
#'
#' Generates reports for individual projects.
#'
#' @param data Imported mdJSON files.
#' @param essential_personnel Essential personnel titles for projects.
#' @param project_list List of projects that individual reports will be
#' generated for.
#' @param output_dir Directory that reports will be saved to.
#'
#' @return An html report containing project details.
#' @export
report_projects <- function(data,
                            essential_personnel = c("AKRegionDataTrustee",
                                                    "AKRegionDataSteward",
                                                    "AKRegionDataCustodian"),
                            project_list = data$projectName,
                            output_dir) {

  # Construct file path to report template
  directory <- "inst\\rmarkdown\\templates\\projects-individual\\skeleton"

  file <- "projects-individual.Rmd"

  template <- paste(directory,
                    file,
                    sep = "\\")

  # Partially flatten mdJSON tibble
  data <- flatten_mdJSON(data)

  # Isolate ongoing projects
  projects <- project_summary(data,
                              project_list,
                              essential_personnel)

  # Create an html report for each ongoing project
  for(i in 1:nrow(projects)) {

    project <- projects[i,]

    rmarkdown::render(template,
                      params = list(project = project,
                                    metaSubset = data),
                      output_file = paste0(project$projectName,
                                           "_",
                                           as.Date(Sys.Date()),
                                           ".html"),
                      output_dir = output_dir)

  }

}
