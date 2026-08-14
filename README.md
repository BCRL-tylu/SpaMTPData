# SpaMTPData

`SpaMTPData` provides ExperimentHub access to the datasets used by
[`SpaMTP`](https://github.com/GenomicsMachineLearning/SpaMTP) tutorials and
tests. Large Seurat, Cardinal and multi-omic objects remain outside Git and are
downloaded only when requested. ExperimentHub is preferred; before Hub
ingestion is complete, the same API falls back to immutable Zenodo URLs with a
30-minute timeout, retries, and size/MD5 validation.

```r
SpaMTPData::SpaMTPDataResources()
striatum <- SpaMTPData::SpaMTPData("mouse_brain_dhb_striatum")
```

For offline development, set `options(SpaMTPData.resource_dir = "...")` to a
directory containing files named as listed in the resource registry.

The registry currently contains 18 checksum-verified resources from eight
versioned Zenodo records, covering mouse brain, mouse bladder, import examples,
simulated single-cell multi-omics, annotation refinement, a large-data ROI, and
the human brain demonstration.

If you use these resources, cite the SpaMTP paper:
[Causer, Lu, Kriel *et al.*, Nature Methods (2026)](https://doi.org/10.1038/s41592-026-03140-8).
