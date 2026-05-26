\name{export_atlas_csv}
\alias{export_atlas_csv}
\title{Export O-GlcNAcAtlas Data as CSV}
\usage{export_atlas_csv(data, file, dataset = NULL)}
\arguments{
\item{data}{A data frame containing Atlas records.}
\item{file}{Output CSV path.}
\item{dataset}{Optional dataset label: \code{"unambiguous"} or \code{"ambiguous"}.}
}
\value{The output file path, invisibly.}
\description{Writes a validated Atlas table to CSV with blank cells for missing values.}
