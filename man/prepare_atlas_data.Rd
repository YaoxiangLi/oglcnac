\name{prepare_atlas_data}
\alias{prepare_atlas_data}
\title{Prepare O-GlcNAcAtlas Data for Export}
\usage{prepare_atlas_data(data, dataset = NULL)}
\arguments{
\item{data}{A data frame containing Atlas records.}
\item{dataset}{Optional dataset label: \code{"unambiguous"} or \code{"ambiguous"}.}
}
\value{A data frame ready to export.}
\description{Adds or normalizes the \code{ambiguous} field used by the public static website.}
