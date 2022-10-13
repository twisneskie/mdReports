
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mdReports

<!-- badges: start -->
<!-- badges: end -->

The goal of mdReports is to offer tools to import, search, and create
reports from mdJSON metadata files created by mdEditor.

The web application mdEditor provides a user interface for authoring and
editing metadata for projects and datasets without requiring extensive
knowledge of metadata standards. The mdEditor application creates mdJSON
records, a JSON standard for metadata created by the Alaska Data
Integration Working Group, which can be exported as-is or translated
into a variety of metadata standards.

Functions provided by the mdReports package can be used to query
information stored in mdJSON records and generate reports by individual
project, a subset of projects, or for all projects.

## Current Functions

Currently, the package can import mdJSON files into R, generate a human
readable metadata summary for projects and data products, and create a
report of all projects within a program.

## Installation

You can install the released version of mdReports from
[Github](https://github.com/) by installing devtools, if necessary, with
`install.packages("devtools")` followed by running
`devtools::install_github("twisneskie/mdReports")`.
