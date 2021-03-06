% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_data.R
\name{qread_charinet}
\alias{qread_charinet}
\title{Read in character interaction network data from standardised input format}
\usage{
qread_charinet(events_file, nodes_file = NULL, from = 3, check_errors = TRUE)
}
\arguments{
\item{events_file}{The event list, containing at least a sender ID column and
columns containing binary dummy variables for each character.}

\item{nodes_file}{The node list, containing as many rows as there are unique
characters in the events list. If not supplied, it will be automatically
generated from the event list, with character names being inferred from how
they appear in the event list.}

\item{from}{The column containing the numeric sender IDs, followed by dummy
variables for each character.}

\item{check_errors}{If TRUE, the function checks for common data input
errors. It looks for characters with self-ties, empty rows where no
recipient was inputted, and any values other than 1 and 0 in the recipient
dummy columns. Returns a message reporting the results of the checks.}
}
\value{
A named list containing the event list, node list and an adjacency
  matrix derived from the event list.
}
\description{
A helper function for quickly reading in data recorded in a
  standardised format wherein certain column orders and variable names are
  used. For data not meeting this format, read the data in manually.

  More specifically, in order to work, the event list should be structured
  consistently with the format specified elsewhere in this package
  documentation (at least a sender ID column followed immediately by columns
  containing binary dummy variables for each character).
}
