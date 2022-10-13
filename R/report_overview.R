#' Overview report of all projects
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
                            output_name,
                            output_dir){

  # Construct file path to report template
  directory <- "inst\\rmarkdown\\templates\\projects-overview\\skeleton"

  file <- "report_overview.Rmd"

  template <- paste(directory,
                     file,
                     sep = "\\")

  # Render html reports
  rmarkdown::render(template,
                    params = list(data = data),
                    output_file = paste0(output_name,
                                         "_",
                                         as.Date(Sys.Date()),
                                         ".html"),
                    output_dir = output_dir)
}
