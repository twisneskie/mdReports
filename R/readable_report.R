readable_report <- function(resource,
                            output_name = "Metadata_Report",
                            output_dir = "C:\\Users\\twisneskie\\Documents"){

  directory <- "inst\\rmarkdown\\templates\\readable-report\\skeleton"

  rmd_file <- "readable-report.Rmd"

  word_file <- "readable-report.docx"

  rmd <- paste(directory,
               rmd_file,
               sep = "\\")

  word_template <- paste(directory,
                         word_file,
                         sep = "\\")

  # Readable metadata output
  rmarkdown::render(rmd,
                    params = list(resource = resource),
                    output_file = paste0(output_name,
                                         "_",
                                         as.Date(Sys.Date()),
                                         ".docx"),
                    output_dir = output_dir)

}
