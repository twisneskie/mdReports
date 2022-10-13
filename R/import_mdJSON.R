#' Import mdJSON metadata
#'
#' Gathers metadata files using the mdJSON metadata standard from a chosen
#' program directory (or all program directories) in the Alaska Regional Data
#' Repository into a tibble for further analysis in R. This function may take
#' several minutes to run, depending on the number of folders and JSON files in
#' the given directory.
#'
#' @param program The program the report will be generated for. Choose from
#' "fes", "mbm", "nwrs", "osm", "sa", or "all".
#' @param path Path to directory that will be searched for metadata files
#' (for USFWS Alaska Region, use link to the RDR).
#' @return A tibble containing imported mdJSON metadata records. Each row is a
#' unique record
#' @export

import_mdJSON <- function(program,
                          path) {

  #### STEP 1: IMPORT mdJSON RECORDS ####
  # Create a path to the specified program directory, if necessary
  if (program != "all") {

    full_path <- paste0(path,
                        "\\",
                        program)

  } else {

    full_path <- paste0(path)

  }

  # Create regex to extract program the file is from
  # Mostly useful for when all programs are selected
  prog_regexp <- paste0("(?<=",
                        stringr::str_replace_all(path,
                                                 "\\\\",
                                                 "\\\\\\\\"),
                        "[:punct:])[:alpha:]*")

  # Create regex to extract the folder name
  proj_regexp <- paste0("(?<=",
                        stringr::str_replace_all(path,
                                                 "\\\\",
                                                 "\\\\\\\\"),
                        "[:punct:][:alpha:]{2,4}[:punct:])",
                        "(.*?)",
                        "(?=[//])")

  # Create regex to extract the sub-program code
  subprog_regexp <- paste0("(?<=",
                           stringr::str_replace_all(path,
                                                    "\\\\",
                                                    "\\\\\\\\"),
                           "[:punct:][:alpha:]{2,4}[:punct:])",
                           "(.*?)",
                           "(?=[_])")

  # Get a tibble of JSON files from the specified folder
  json <- list.files(full_path,
                     pattern = "*.json",
                     recursive = TRUE,
                     full.names = TRUE) %>%

    tibble::tibble() %>%

    dplyr::rename(filePath = ".") %>%

    # Extract the program name from the file path
    dplyr::mutate(program = stringr::str_extract(filePath,
                                                 prog_regexp)) %>%

    # Toss rows that are from the contacts or extent folder
    dplyr::filter(program != "contacts" & program != "extents") %>%

    # Pull out project folder name and subprogram names
    dplyr::mutate(project = stringr::str_extract(filePath,
                                                 proj_regexp)) %>%

    dplyr::mutate(subprogram = stringr::str_extract(filePath,
                                                    subprog_regexp) %>%
                    stringr::str_extract(paste0("(?<=",
                                                program,
                                                ")",
                                                "[:alpha:]*")))

  # Import JSON files from the selected directory and separate out some of the
  # top level elements: contact, metadata, and data dictionary
  json$resources <- purrr::map(json$filePath,
                               jsonlite::fromJSON)

  json <- tidyr::hoist(json,
                       "resources",
                       "metadata",
                       "contact",
                       "metadataRepository",
                       "dataDictionary")

  # Drop rows that aren't from mdJSON files
  # (i.e. mdEditor files, JSON data products)
  data <- dplyr::filter(json, json$metadata != "NULL") %>%

    # Extract out metadata identifier from metadata
    tidyr::hoist("metadata", "metadataInfo") %>%

    tidyr::hoist("metadataInfo", "metadataIdentifier") %>%

    tidyr::unnest_wider("metadataIdentifier") %>%

    # Give the identifier a meaningful column name
    dplyr::rename(metaId = "identifier") %>%

    # Drop unnecessary columns
    dplyr::select(-dplyr::any_of(c("namespace",
                                   "resources",
                                   "filePath")))

  #### STEP 2: FLATTEN FIELDS INTO A TABLE ####
  data %>%

    # Unpack top level elements of metadata
    tidyr::hoist("metadata",
                 "resourceInfo",
                 "resourceLineage",
                 "resourceDistribution",
                 "associatedResource",
                 "funding") %>%

    # Unpack top level elements of resourceInfo
    tidyr::hoist("resourceInfo",
                 "resourceType",
                 "citation",
                 "abstract",
                 "shortAbstract",
                 "purpose",
                 "timePeriod",
                 "status",
                 "pointOfContact",
                 "extent",
                 "taxonomy",
                 "keyword",
                 "constraint",
                 "resourceMaintenance") %>%

    tidyr::hoist("citation",
                 "title")

}
