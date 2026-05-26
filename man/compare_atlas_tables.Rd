\name{compare_atlas_tables}
\alias{compare_atlas_tables}
\title{Compare Two Atlas Tables}
\usage{compare_atlas_tables(old_data, new_data, id_col = "id")}
\arguments{
\item{old_data}{Previous Atlas data frame.}
\item{new_data}{Updated Atlas data frame.}
\item{id_col}{Row identifier column. Defaults to \code{"id"}.}
}
\value{A one-row data frame with row and ID counts.}
\description{Summarizes added, removed, and shared Atlas row IDs.}
