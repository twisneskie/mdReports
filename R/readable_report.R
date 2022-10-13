#' Generate metadata printout
#'
#' Creates a human-readable document containing an overview of a single metadata
#' record.
#'
#' @param resource A single row of a tibble of mdJSON files (as created by the
#' import_mdJSON function) to be turned into a document.
#' @param output_name A name for the generated document. Default is
#' "Metadata_Report".
#' @param output_dir The output directory for the created file.
#'
#' @return
#' @export
readable_report <- function(resource,
                            output_name = "Metadata_Report",
                            output_dir){

  # The R Markdown template to be used to generate the document
  directory <- "inst\\rmarkdown\\templates\\readable-report\\skeleton"

  rmd_file <- "readable-report.Rmd"

  rmd <- paste(directory,
               rmd_file,
               sep = "\\")

  # The Word template the document will use for formatting
  word_file <- "readable-report.docx"

  word_template <- paste(directory,
                         word_file,
                         sep = "\\")

  # Render the document in markdown
  rmarkdown::render(rmd,
                    params = list(resource = resource),
                    output_file = paste0(output_name,
                                         "_",
                                         as.Date(Sys.Date()),
                                         ".docx"),
                    output_dir = output_dir)

}
