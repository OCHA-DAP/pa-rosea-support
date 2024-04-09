# OCHA ROSEA & Related Country Offices Analysis - Droughts & Floods
This document serves as the main README for the repository containing R code used for analyses conducted in support of the OCHA Regional Office for Southern and Eastern Africa (ROSEA) and Country Offices within the region. The analyses primarily focus on drought and flood events.

## Content Structure

The repository is organized into subdirectories, each representing a specific analysis. These subdirectories typically contain:

 - R scripts (.R) for the analysis.
 - A concise description of the analysis performed in the script(s).
 - Details about the data used (sources, formats).
 - Expected outputs and their interpretation.
 - Instructions on running the code (dependencies, steps involved).

## Getting Started

1. Clone the Repository: Use git clone https://github.com/your-username/ocha-rosea-analysis.git to clone the repository locally.
2. Install Dependencies: Install any required R packages using install.packages("package_name") within R.
3. Run Analyses: Navigate to the relevant subdirectory and run the R script(s) using source("script_name.R") in R.

### Subdirectory Structure

pa-rosea-support/
├── R/                            # Scripts for running targets pipeline
├── drought/
│   ├── moz                        # Scripts for MOZ analyses
│   └── southern-africa            # Scripts for Southern Africa region
└── flood/                        # Scripts for flood risk mapping

