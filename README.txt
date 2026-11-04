Figure review materials

The scripts reproduce Figures 6, 8, 9 and 10 in the accompanying English manuscript from the derived data used to draw them. Figure numbers follow the accompanying revision. The diagrams/ folder contains the exact retained PNGs used for the workflow (Fig. 2) and the conceptual example (Fig. 3), checked against the embedded manuscript images. They identify the artwork used in this revision; they do not document the full authoring history of the original submission. These files cover selected figures and include the plotting data for Figs. 6, 8, 9 and 10. Individual service rasters, source products and a complete reconstruction of the GIS and optimization workflows are outside this archive.

Extract the whole archive. From the extracted directory run:
Rscript scripts/Fig6.R
Rscript scripts/Fig8.R
Rscript scripts/Fig9.R
Rscript scripts/Fig10.R

Use R with grid, Cairo PDF graphics and Times New Roman (or a compatible font). The scripts create output/figN/FigureN.pdf and relevant numerical outputs. Code uses relative paths. Alternative fonts may change typography.

Fig. 6a uses the 2025 trajectories in NestedRandom_Raw.csv. Fig. 6b uses the individual half-performance crossings in NestedRandom_HalfPerformance_ByRepeat.csv (100 trajectories for each of 2010, 2015, 2020 and 2025). At a crossing, the three components sum to 0.5 of baseline access. Multiply by 100 for the figure's vertical axis; divide by 0.5 to report the fraction of total loss at the crossing. Compare the mean components with Table S24 and the exported box statistics with checks/Fig6b_expected_boxplots.csv.

Fig. 8a uses the mapped endpoints and retained/replaced/added corridors. Fig. 8b uses differences between access-refined and binary-optimal portfolios at 21 stages. AUC checks reference the random-order batch used in Fig. 8b, not the independent evaluation in Appendix S8.6. The script compares integrated differences with the stored AUCs and writes a check against checks/Fig8b_expected_AUC.csv.

Fig. 9 maps source ranks using the same network geometries in both panels. Fig. 10 maps source area and corridor centerline overlap; the two inset views use panel b's layers. Projected drawing coordinates are in metres; the projection definition is in data/maps/Map_CRS.prj. Table S36 reports totals from the spatial overlay. Tables S37 and S38 report results from separate sensitivity analyses; drawing the maps does not rerun those analyses.

Tables are transcribed from the accompanying supplementary material. The scripts preserve the original plotting and calculation logic with portable input paths and PDF output. The checks/ directory provides reference values; SHA256SUMS.txt identifies the archived input, script and reference files.
