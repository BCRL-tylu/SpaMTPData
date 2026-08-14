# Contributing to SpaMTPData

Experiment resources must be immutable, publicly accessible, and accompanied
by provenance, a checksum, and an explicit software class. New or revised
datasets should receive a new Zenodo version before their Hub metadata changes.

For a data update:

1. Publish the processed resource and its generation recipe.
2. Record the stable URL, byte size, MD5 checksum, species, genome, and class.
3. Update `inst/extdata/resource_manifest.csv` and `metadata.csv`.
4. Run the metadata validator, `R CMD check`, and `BiocCheck::BiocCheck()`.

Do not commit large experiment files or credentials to Git.
