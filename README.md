# Figure review materials for an ecological corridor study

This repository contains plotting scripts and derived inputs for selected figures in the revised manuscript. It is intended to support inspection of the figures and their plotted values. Figure numbers refer to the accompanying revision.

## Contents

- `scripts/`: R scripts for Figures 6, 8, 9 and 10 and shared plotting functions.
- `data/fig6/`: simulation-derived trajectories and loss components used for Figure 6.
- `data/fig8/`: derived data for the corridor selection map and response curves in Figure 8.
- `data/maps/`: derived spatial geometries, node attributes and protection-overlap data used by Figures 9 and 10.
- `checks/`: reference values for checking plotted summaries.
- `tables/`: selected supplementary tables used for comparison.
- `diagrams/`: the final image files used for the workflow and conceptual diagrams, Figures 2 and 3.
- `README.txt`: further detail about each figure and its checks.
- `SHA256SUMS.txt`: checksums of files in the original figure-review archive; this new `README.md` is not included in that original checksum list.

## Run the plotting scripts

Extract or clone the whole repository, retaining its directory structure. With R installed, run the following commands from the repository root:

```text
Rscript scripts/Fig6.R
Rscript scripts/Fig8.R
Rscript scripts/Fig9.R
Rscript scripts/Fig10.R
```

The scripts use R's `grid` package and require Cairo graphics support. A compatible Times New Roman font is recommended; another font may change the layout. Output PDFs and associated checks are written to `output/`.

The four scripts were run on Windows with R 4.6.1 using the supplied derived inputs. Replication on another system may differ in fonts or rendering. See `README.txt` for the figure-specific inputs and numerical checks.

## Scope and limits

These materials allow the listed figures to be redrawn from derived plotting inputs. They do not provide the individual service rasters, original source products, a complete reconstruction of the GIS and optimization workflows, or a complete authoring history for the original diagrams. The diagram files are final images, not editable drawing sources. No claim of full end-to-end reproducibility is made here.

Data and code may be reused only in accordance with the rights and conditions applicable to their sources. Verify the rights for the included derived spatial data and any third-party content before publishing the repository. No reuse license is specified in this draft.
