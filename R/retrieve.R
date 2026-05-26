#' Retrieve Data from UniProt API
#'
#' This function retrieves UniProt REST API data based on the provided UniProt
#' accession number.
#'
#' @param accession A character string representing the UniProt accession number.
#' @return A list containing the retrieved data in JSON format, or NULL if the request fails.
#' @import jsonlite cli
#' @examples
#' # Example usage
#' \donttest{
#' result <- retrieve_uniprot_data("O88737")
#' print(result)
#' }
#' @export
retrieve_uniprot_data <- function(accession) {
  # Base URL for UniProt API
  base_url <- paste0("https://rest.uniprot.org/uniprotkb/", accession, ".json")

  cli::cli_alert_info("Sending request to UniProt for accession: {accession}")
  tryCatch({
    json_data <- jsonlite::fromJSON(base_url)
    cli::cli_alert_success("Successfully retrieved data for {accession}")
    json_data
  }, error = function(e) {
    cli::cli_alert_danger("Failed to retrieve data for {accession}: {e$message}")
    return(NULL)
  })
}
