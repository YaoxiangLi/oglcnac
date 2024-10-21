# tests/testthat/test-retrieve.R
test_that("retrieve_uniprot_data handles a valid accession correctly", {
  # Skip this test on CRAN or if an internet connection is not available
  skip_on_cran()
  if (!curl::has_internet()) {
    skip("No internet connection available.")
  }

  # Test with a known valid accession (this relies on UniProt's availability)
  result <- retrieve_uniprot_data("O88737")

  # We expect a list result, though we may not know the exact structure
  expect_type(result, "list")
  expect_true(!is.null(result$uniProtkbId)) # Check that an ID is present
})
