test_that("validate_atlas_data accepts unambiguous and ambiguous Atlas tables", {
  atlas <- tibble::tibble(
    id = c(10000001, 10000002),
    species = c("human", "human"),
    sample_type = c("cells", "cells"),
    accession = c("P12345", "Q12345"),
    accession_source = c("UniProt", "UniProt"),
    entry_name = c("TEST_HUMAN", "TEST2_HUMAN"),
    protein_name = c("Protein 1", "Protein 2"),
    gene_name = c("GENE1", "GENE2"),
    peptide_seq = c("AAAASTAAAA", "AAAASAAAAA"),
    site_residue = c("S", "S"),
    position_in_peptide = c(5, 5),
    position_in_protein = c(100, 101),
    method = c("MS", "MS"),
    analytical_throughput = c("HTP", "HTP"),
    pmid = c(1, 2)
  )

  result <- validate_atlas_data(atlas, dataset = "unambiguous")
  expect_true(result$valid)
  expect_equal(result$summary$dataset, "unambiguous")

  prepared <- prepare_atlas_data(atlas, dataset = "ambiguous")
  expect_true("ambiguous" %in% names(prepared))
  expect_true(all(prepared$ambiguous == "ambiguous"))
})

test_that("validate_atlas_data rejects mixed dataset labels", {
  atlas <- tibble::tibble(
    id = 1,
    species = "human",
    sample_type = "cells",
    accession = "P12345",
    accession_source = "UniProt",
    entry_name = "TEST_HUMAN",
    protein_name = "Protein",
    gene_name = "GENE",
    peptide_seq = "AAAASTAAAA",
    site_residue = "S",
    position_in_peptide = 5,
    position_in_protein = 100,
    method = "MS",
    analytical_throughput = "HTP",
    pmid = 1,
    ambiguous = "ambiguous"
  )

  result <- validate_atlas_data(atlas, dataset = "unambiguous")
  expect_false(result$valid)
  expect_true(any(grepl("Dataset is unambiguous", result$errors)))
})

test_that("compare_atlas_tables summarizes changed row ids", {
  old_data <- data.frame(id = c(1, 2), accession = c("A", "B"))
  new_data <- data.frame(id = c(2, 3), accession = c("B", "C"))

  summary <- compare_atlas_tables(old_data, new_data)
  expect_equal(summary$added_ids, 1)
  expect_equal(summary$removed_ids, 1)
  expect_equal(summary$shared_ids, 1)
})
