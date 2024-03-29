% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_overview.R
\name{report_projects}
\alias{report_projects}
\title{Report project details}
\usage{
report_projects(
  data,
  essential_personnel = c("AKRegionDataTrustee", "AKRegionDataSteward",
    "AKRegionDataCustodian"),
  project_list = data$projectName,
  output_dir
)
}
\arguments{
\item{data}{Imported mdJSON files.}

\item{essential_personnel}{Essential personnel titles for projects.}

\item{project_list}{List of projects that individual reports will be
generated for.}

\item{output_dir}{Directory that reports will be saved to.}
}
\value{
An html report containing project details.
}
\description{
Generates reports for individual projects.
}
