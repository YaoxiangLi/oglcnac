#' Validate O-GlcNAcAtlas Data
#'
#' Checks that an Atlas table has the columns needed by the public website and
#' that dataset labels are not mixed accidentally.
#'
#' @param data A data frame containing Atlas records.
#' @param dataset Optional dataset label. Use `"unambiguous"` for dataset-I or
#'   `"ambiguous"` for dataset-II.
#' @return A list with `valid`, `errors`, `warnings`, and `summary` fields.
#' @export
validate_atlas_data <- function(data, dataset = NULL) {
  if (!is.data.frame(data)) {
    cli::cli_abort("`data` must be a data frame.")
  }

  dataset <- normalize_atlas_dataset(dataset)
  required_cols <- c(
    "id", "species", "sample_type", "accession", "accession_source",
    "entry_name", "protein_name", "gene_name", "peptide_seq",
    "site_residue", "position_in_peptide", "position_in_protein",
    "method", "analytical_throughput", "pmid"
  )

  errors <- character()
  warnings <- character()
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    errors <- c(errors, paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  if ("ambiguous" %in% names(data)) {
    values <- unique(tolower(trimws(as.character(data$ambiguous))))
    values <- values[!is.na(values) & nzchar(values)]
    bad_values <- setdiff(values, c("ambiguous", "unambiguous"))
    if (length(bad_values) > 0) {
      errors <- c(errors, paste("Invalid ambiguous values:", paste(bad_values, collapse = ", ")))
    }
    if (!is.null(dataset) && length(values) > 0 && any(values != dataset)) {
      errors <- c(errors, paste0("Dataset is ", dataset, " but `ambiguous` contains other values."))
    }
  }

  if ("accession" %in% names(data) && any(is.na(data$accession) | !nzchar(trimws(as.character(data$accession))))) {
    warnings <- c(warnings, "Some rows have empty accession values.")
  }

  for (col in intersect(c("id", "position_in_peptide", "position_in_protein", "pmid"), names(data))) {
    suppressWarnings(numeric_values <- as.numeric(data[[col]]))
    bad_rows <- which(!is.na(data[[col]]) & nzchar(trimws(as.character(data[[col]]))) & is.na(numeric_values))
    if (length(bad_rows) > 0) {
      message <- paste0("Column `", col, "` has non-numeric values.")
      if (col == "id") {
        errors <- c(errors, message)
      } else {
        warnings <- c(warnings, message)
      }
    }
  }

  if (is.null(dataset) && !"ambiguous" %in% names(data)) {
    warnings <- c(warnings, "No dataset was selected and no `ambiguous` column exists.")
  }

  summary <- data.frame(
    rows = nrow(data),
    columns = ncol(data),
    dataset = if (is.null(dataset)) "" else dataset,
    stringsAsFactors = FALSE
  )

  list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings,
    summary = summary
  )
}

#' Prepare O-GlcNAcAtlas Data for Export
#'
#' Adds or normalizes the `ambiguous` field used by the public static website.
#' Use `"unambiguous"` for dataset-I and `"ambiguous"` for dataset-II.
#'
#' @param data A data frame containing Atlas records.
#' @param dataset Optional dataset label: `"unambiguous"` or `"ambiguous"`.
#' @return A data frame ready to export.
#' @export
prepare_atlas_data <- function(data, dataset = NULL) {
  result <- data
  dataset <- normalize_atlas_dataset(dataset)
  validation <- validate_atlas_data(result, dataset = dataset)
  if (!validation$valid) {
    cli::cli_abort(validation$errors)
  }

  if (!is.null(dataset)) {
    result$ambiguous <- dataset
  } else if ("ambiguous" %in% names(result)) {
    result$ambiguous <- tolower(trimws(as.character(result$ambiguous)))
  }

  result
}

#' Export O-GlcNAcAtlas Data as CSV
#'
#' Writes a validated Atlas table to CSV. Empty values are written as blank
#' cells so the output is easy to review and commit.
#'
#' @param data A data frame containing Atlas records.
#' @param file Output CSV path.
#' @param dataset Optional dataset label: `"unambiguous"` or `"ambiguous"`.
#' @return The output file path, invisibly.
#' @export
export_atlas_csv <- function(data, file, dataset = NULL) {
  prepared <- prepare_atlas_data(data, dataset = dataset)
  utils::write.csv(prepared, file, row.names = FALSE, na = "")
  invisible(file)
}

#' Compare Two Atlas Tables
#'
#' Summarizes added, removed, and shared Atlas row IDs.
#'
#' @param old_data Previous Atlas data frame.
#' @param new_data Updated Atlas data frame.
#' @param id_col Row identifier column. Defaults to `"id"`.
#' @return A one-row data frame with row and ID counts.
#' @export
compare_atlas_tables <- function(old_data, new_data, id_col = "id") {
  if (!id_col %in% names(old_data) || !id_col %in% names(new_data)) {
    cli::cli_abort("Both tables must contain `{id_col}`.")
  }

  old_ids <- unique(as.character(old_data[[id_col]]))
  new_ids <- unique(as.character(new_data[[id_col]]))

  data.frame(
    old_rows = nrow(old_data),
    new_rows = nrow(new_data),
    added_ids = length(setdiff(new_ids, old_ids)),
    removed_ids = length(setdiff(old_ids, new_ids)),
    shared_ids = length(intersect(old_ids, new_ids)),
    stringsAsFactors = FALSE
  )
}

normalize_atlas_dataset <- function(dataset) {
  if (is.null(dataset) || identical(dataset, "") || identical(dataset, "none")) {
    return(NULL)
  }

  value <- tolower(trimws(dataset))
  aliases <- c(
    "dataset-i" = "unambiguous",
    "dataset i" = "unambiguous",
    "i" = "unambiguous",
    "unambiguous" = "unambiguous",
    "dataset-ii" = "ambiguous",
    "dataset ii" = "ambiguous",
    "ii" = "ambiguous",
    "ambiguous" = "ambiguous"
  )
  if (!value %in% names(aliases)) {
    cli::cli_abort("Dataset must be `unambiguous`/dataset-I or `ambiguous`/dataset-II.")
  }
  aliases[[value]]
}
