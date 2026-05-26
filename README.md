# oglcnac

`oglcnac` helps curate O-GlcNAcAtlas data files before they are published on
oglcnac.org. The package keeps the workflow simple: work with CSV, TSV, or
Excel files, validate the required Atlas columns, enrich UniProt fields, and
export clean CSV files for the static website.

## Main Workflow

```r
library(oglcnac)

atlas <- read.csv("Atlas_unambiguous.csv")

validation <- validate_atlas_data(atlas, dataset = "unambiguous")
validation$valid

atlas <- process_tibble_uniprot_cached(
  atlas,
  cache_path = "~/.cache/oglcnac/uniprot-cache.rds"
)

export_atlas_csv(
  atlas,
  "atlas-records-unambiguous.csv",
  dataset = "unambiguous"
)
```

Use `dataset = "unambiguous"` for unambiguous sites, also called dataset-I.
Use `dataset = "ambiguous"` for ambiguous sites, also called dataset-II.
The package writes this value to the `ambiguous` column used by the public
website, so the two Atlas datasets are not mixed.

## Shiny App

```r
oglcnac::launch_app()
```

The app supports:

- CSV, TSV, and Excel upload
- Atlas dataset selection
- Atlas schema validation
- cached UniProt enrichment
- processed CSV download

## Website Export

The website source can generate static JSON directly from CSV files:

```bash
python3 frontend/scripts/generate_static_data.py \
  --atlas-unambiguous-csv atlas-records-unambiguous.csv \
  --atlas-ambiguous-csv atlas-records-ambiguous.csv \
  --ogt-pin-csv ogt-pin-records.csv
```

SQLite is still useful for legacy recovery, but CSV files are easier to review
and should be the normal update format.
