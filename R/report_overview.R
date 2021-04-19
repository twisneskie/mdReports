#' Report overview of project metadata
#'
#' @param data Subset of metadata created by \code{subset_mdJSON}.
#' @param contacts List of contacts from the metadata with their metadata IDs.
#' @param output_name Name to be given to the report generated.
#' @param output_dir Directory that the report should be generated in.
#'
#' @return
#' @export
report_overview <- function(data,
                            contacts,
                            output_name,
                            output_dir){

  template <- "inst\\rmarkdown\\templates\\projects-overview\\skeleton\\skeleton.Rmd"

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

  data %>%

    dplyr::filter(type == "project") %>%

    reformat_personnel(contacts,
                       personnel = c("principalInvestigator"),
                       other_columns = c("projectName"))

}

# Table for chunk 3: types.of.products
typesProducts <- function(data) {

  data %>%

    dplyr::select(type) %>%

    dplyr::filter(type != "project") %>%

    dplyr::group_by(type) %>%

    dplyr::count() %>%

    dplyr::ungroup() %>%

    tibble::add_row(type = "Total",
                    n = sum(.$n))

}

# Table for chunk 4: ongoing.projects
ongoingProjects <- function(data, contacts) {

  data %>%

    dplyr::filter(type == "project",
                  status == "onGoing") %>%

    reformat_personnel(contacts,
                       personnel = c("principalInvestigator"),
                       other_columns = c("projectName",
                                         "startDate",
                                         "endDate"))

}

# Table for chunk 5: dm.personnel
dmPersonnel <- function(data, contacts) {

  data %>%

    reformat_personnel(contacts, other_columns = c("type", "status")) %>%

    dplyr::filter(type == "project",
                  status == "onGoing")

}
