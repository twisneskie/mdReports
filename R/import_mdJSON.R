#' Import mdJSON files
#'
#' Converts mdJSON files into a tibble for further analysis in R.
#' The \code{import_mdJSON()} function can also add a column for project names,
#' derived from the name of the top level folders in the file path given to
#' import mdJSON files.
#'
#' @param path Path to directory that will be searched for mdJSON files.
#' @param project_names If \code{project_names = TRUE}, a column will be added
#' with the top level directory after the path given as project names.
#'
#' @return A tibble containing imported JSON and,
#' if \code{project_names = TRUE}, projects names for imported JSON files.
#' @export

import_mdJSON <- function(path = getwd(), project_names = TRUE) {

  # Get list of JSON files in the selected directory
  files <- list.files(path,
                      pattern = "*.json",
                      recursive = TRUE,
                      full.names = TRUE)

  # Import JSON files
  json <- files %>%

    purrr::map(jsonlite::fromJSON) %>%

    tibble::tibble() %>%

    dplyr::rename(resources = ".")


  if (project_names == TRUE) {

    # Get the project name from the top level directory after the file path
    pathRemove <- paste0(stringr::str_replace_all(path,
                                                  "\\\\",
                                                  "\\\\\\\\"))

    projName <- stringr::str_remove(files, pathRemove) %>%

      stringr::str_extract("(?<=[//])(.*?)(?=[//])") %>%

      tibble::tibble() %>%

      dplyr::rename(projectName = ".") %>%

      tibble::rownames_to_column(var = "row")


    json <- tibble::rownames_to_column(json, var = "row") %>%

      dplyr::left_join(projName, by = "row") %>%

      dplyr::select(-row)

  }

    # Most reporting needs will be from the "metadata" section,
    # so we will isolate that section to save lines of code in other functions
    json <- tidyr::hoist(json,
                         "resources",
                         "metadata")

    # Drop items that don't have the metadata section
    # (i.e. non-mdJSON files, such as mdEditor files)
    dplyr::filter(json,
                  json$metadata != "NULL")

}
