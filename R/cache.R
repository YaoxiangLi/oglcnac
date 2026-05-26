#' Process UniProt Data with a Local Cache
#'
#' Enriches a data frame like `process_tibble_uniprot()` while avoiding repeated
#' UniProt requests for accessions already present in a local RDS cache.
#'
#' @param data A data frame containing accession and accession source columns.
#' @param cache_path Optional RDS file path for cached parsed UniProt records.
#' @param accession_col The accession column name.
#' @param accession_source_col The accession source column name.
#' @param entry_name_col The entry name column name.
#' @param protein_name_col The protein name column name.
#' @param gene_name_col The gene name column name.
#' @return A data frame with UniProt fields filled where available.
#' @export
process_tibble_uniprot_cached <- function(data,
                                          cache_path = NULL,
                                          accession_col = "accession",
                                          accession_source_col = "accession_source",
                                          entry_name_col = "entry_name",
                                          protein_name_col = "protein_name",
                                          gene_name_col = "gene_name") {
  required_cols <- c(accession_col, accession_source_col)
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Missing required columns: {missing_cols}")
  }

  if (!entry_name_col %in% colnames(data)) data[[entry_name_col]] <- NA
  if (!protein_name_col %in% colnames(data)) data[[protein_name_col]] <- NA
  if (!gene_name_col %in% colnames(data)) data[[gene_name_col]] <- NA

  cache <- read_uniprot_cache(cache_path)
  is_uniprot <- tolower(trimws(as.character(data[[accession_source_col]]))) == "uniprot"
  accessions <- unique(trimws(as.character(data[[accession_col]][is_uniprot])))
  accessions <- accessions[!is.na(accessions) & nzchar(accessions)]

  for (accession in accessions) {
    if (!is.null(cache[[accession]])) {
      next
    }

    cli::cli_inform("Fetching UniProt accession: {accession}")
    uniprot_data <- retrieve_uniprot_data(accession)
    if (is.null(uniprot_data)) {
      cache[[accession]] <- list(entry_name = NA, protein_name = NA, gene_name = NA)
      next
    }
    cache[[accession]] <- parse_uniprot_data(uniprot_data)
  }

  for (i in which(is_uniprot)) {
    accession <- trimws(as.character(data[[accession_col]][i]))
    parsed <- cache[[accession]]
    if (is.null(parsed)) next

    if (!is.null(parsed$entry_name) && !is.na(parsed$entry_name)) data[[entry_name_col]][i] <- parsed$entry_name
    if (!is.null(parsed$protein_name) && !is.na(parsed$protein_name)) data[[protein_name_col]][i] <- parsed$protein_name
    if (!is.null(parsed$gene_name) && !is.na(parsed$gene_name)) data[[gene_name_col]][i] <- parsed$gene_name
  }

  write_uniprot_cache(cache, cache_path)
  data
}

read_uniprot_cache <- function(cache_path) {
  if (is.null(cache_path) || !nzchar(cache_path) || !file.exists(cache_path)) {
    return(list())
  }
  cache <- readRDS(cache_path)
  if (!is.list(cache)) {
    cli::cli_abort("UniProt cache must be an RDS list.")
  }
  cache
}

write_uniprot_cache <- function(cache, cache_path) {
  if (is.null(cache_path) || !nzchar(cache_path)) {
    return(invisible(NULL))
  }
  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(cache, cache_path)
  invisible(cache_path)
}
