\name{process_tibble_uniprot_cached}
\alias{process_tibble_uniprot_cached}
\title{Process UniProt Data with a Local Cache}
\usage{
process_tibble_uniprot_cached(
  data,
  cache_path = NULL,
  accession_col = "accession",
  accession_source_col = "accession_source",
  entry_name_col = "entry_name",
  protein_name_col = "protein_name",
  gene_name_col = "gene_name"
)
}
\arguments{
\item{data}{A data frame containing accession and accession source columns.}
\item{cache_path}{Optional RDS file path for cached parsed UniProt records.}
\item{accession_col}{The accession column name.}
\item{accession_source_col}{The accession source column name.}
\item{entry_name_col}{The entry name column name.}
\item{protein_name_col}{The protein name column name.}
\item{gene_name_col}{The gene name column name.}
}
\value{A data frame with UniProt fields filled where available.}
\description{Enriches a data frame like \code{process_tibble_uniprot()} while avoiding repeated UniProt requests for accessions already present in a local RDS cache.}
