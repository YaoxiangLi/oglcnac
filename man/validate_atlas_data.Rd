\name{validate_atlas_data}
\alias{validate_atlas_data}
\title{Validate O-GlcNAcAtlas Data}
\usage{validate_atlas_data(data, dataset = NULL)}
\arguments{
\item{data}{A data frame containing Atlas records.}
\item{dataset}{Optional dataset label: \code{"unambiguous"} for dataset-I or \code{"ambiguous"} for dataset-II.}
}
\value{A list with \code{valid}, \code{errors}, \code{warnings}, and \code{summary}.}
\description{Checks that an Atlas table has the columns needed by the public website and that dataset labels are not mixed accidentally.}
