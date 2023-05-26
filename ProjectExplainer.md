# Living-Shorelines-NewHampshire

Short-Term Recovery of Pilot Living Shoreline Projects in New Hampshire

Date Last Updated: 05/25/2023

Created by: JG McKown (james.mckown@unh.edu)

Project Details:

Three pilot living shoreline projects were created in the Seacoast of New Hampshire (Portsmouth, Durham) to mitigate infrastructure impacts and restore valuable fringe salt marsh habitat in the Great Bay Estuary system. Living shorelines were monitored 2018 - 2022 (0 - 4 years post-restoration) for vegetation, pore water chemistry, and nekton and compared to nearby no action controls ('before' conditions) and reference fringe marshes to accurately assess the short-term recovery of the ecosytem structure, functions, and services. We applied to the Restoration Performance Index to objectively assess the short-term recovery. In the code, the monitoring results of each RPI metric was bootstrapped 1000 times (n = 10 per each resample).  


Input CSV Files:

Input data is:
(1) Monitoring data, modified to allow for the RPI calculations to proceed, for vegetation, pore water chemistry, and nekton. In each input CSV file, a column explains if monitoring data is "modified" and why it was modified for RPI calculations. Additionally, the monitoring data of the sills for each living shoreline is included for algae and macroinvertebrate colonization. 

(2) Weighted and unweighted group core RPI scores were calculated and compiled in Excel for the entire study and used for visualization purposes. Exclusion of nekton and adjustments to pore water chemistry RPI scores (reduction-oxidatino potential mismatches of Cutts Cove 2021 and North Mill Pond 2020 and 2020) 

(3) Descriptive Statistics of vegetation, pore water chemistry, nekton, and sill monitoring data for visualization purposes

Please read the README text file in the Input CSV Files for further metadata for files, especially the RPI Core Group csv

Note on Code:

The organization of the code is broken down by Book - Chapter - Page - Step. Each book represents three uniquely different aspects of the code - Code Prep, Data Prep, RPI Analysis, Data Visualization, etc. Each page represents a unique facet or series of steps within the chapter. Steps are individual code blocks. I organize my R code this way to allow for ease of understanding in bite-size chunks for the audience as well as myself. Code is heavily annotated to fully explain the steps, functions, and packages used in the analysis as well as reasoning for certain statistical analyses.
