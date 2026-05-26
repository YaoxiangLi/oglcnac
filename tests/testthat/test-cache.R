test_that("process_tibble_uniprot_cached uses cached parsed UniProt records", {
  cache_file <- tempfile(fileext = ".rds")
  saveRDS(list(
    P12345 = list(
      entry_name = "TEST_HUMAN",
      protein_name = "Cached protein",
      gene_name = "GENE"
    )
  ), cache_file)

  data <- tibble::tibble(
    accession = c("P12345", "X00000"),
    accession_source = c("UniProt", "OtherDB"),
    entry_name = c(NA, NA),
    protein_name = c(NA, NA),
    gene_name = c(NA, NA)
  )

  result <- process_tibble_uniprot_cached(data, cache_path = cache_file)
  expect_equal(result$entry_name[1], "TEST_HUMAN")
  expect_true(is.na(result$entry_name[2]))
})
