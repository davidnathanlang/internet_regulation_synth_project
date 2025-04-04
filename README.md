# Age Verification Law Analysis

This repository contains code to analyze the effects of age verification laws on internet search behavior. The analysis uses synthetic control methods to estimate treatment effects across multiple states and specifications.

The Manuscript can be found at the Open Science Foundation at this URL: https://osf.io/26jq4.

## System Requirements

### Software Dependencies
- R version 4.1.0 or higher
- RStudio (recommended for ease of use)

### Required R Packages

```R
# Core packages
library(tidyverse)  # v1.3.1 or higher
library(tidylog)    # v1.0.2 or higher
library(here)       # v1.0.1 or higher
library(glue)       # v1.4.2 or higher

# Data manipulation and processing
library(janitor)    # v2.1.0 or higher
library(vroom)      # v1.5.7 or higher
library(furrr)      # v0.2.3 or higher
library(unglue)     # v0.1.0 or higher
library(lubridate)  # v1.8.0 or higher

# Synthetic control packages
library(gsynth)     # v1.2.1 or higher
library(augsynth)   # v0.1.0 or higher

# Visualization
library(patchwork)  # v1.1.1 or higher
library(ggrepel)    # v0.9.1 or higher
library(geofacet)   # v0.2.0 or higher

# Census data
library(tidycensus) # v0.12.0 or higher
library(gtrendsR)   # v1.5.0 or higher
```

### Hardware Requirements
- Standard desktop or laptop computer
- Minimum 8GB RAM (16GB recommended for multiverse analysis)
- At least 10GB free disk space for data and outputs

### Operating Systems
- The code has been tested on:
  - Windows 10 and 11
  - macOS 11 (Big Sur) and later
  - Ubuntu 20.04 LTS and later

## Installation Guide

### Setting Up the Environment

1. Install R (version 4.1.0 or higher) from https://cran.r-project.org/
2. Install RStudio (optional but recommended) from https://posit.co/download/rstudio-desktop/
3. Clone this repository:
   ```
   git clone https://github.com/davidnathanlang/internet_regulation_synth_project.git
   cd internet_regulation_synth_project
   ```

4. Install required R packages:
   ```R
   # Run this in R or RStudio
   install.packages(c("tidyverse", "tidylog", "here", "glue", "janitor", 
                     "vroom", "furrr", "unglue", "lubridate", "patchwork", 
                     "ggrepel", "geofacet", "tidycensus", "gtrendsR"))
                     
   # For packages not on CRAN
   devtools::install_github("synth-inference/augsynth")
   devtools::install_github("xuyiqing/gsynth")
   ```

5. Create an `api_keys` file (no extension) in the root directory with your Census API key:
   ```
   # Get your key from https://api.census.gov/data/key_signup.html
   YOUR_CENSUS_API_KEY_HERE
   ```

## Demo

### Expected Runtime

If collecting the data directly from the Google Trends API, please allow for ~72 hours to retrieve all state-level time series for the search terms of interest. The multiverse analysis portion of the code should run in 1-2 hours depending on computing resources. For reference, the runtime is approximately 2 hours on an Apple M1 Max Macbook Pro with 64gb of memory.

### Running the Analysis

1. Execute the data collection scripts:
   ```R
   source("scripts/01a_pull_gtrend_data.R")
   source("scripts/01b_pull_census_data.R")
   source("scripts/01c_scrape_implementation_dates.R")
   source("scripts/02_assemble_data.R")
   ```

2. Run the initial analysis:
   ```R
   source("scripts/03_preregistered_hypotheses.R")
   source("scripts/03_single_state_example.R")
   ```

3. Check the output in `figures/` and `tables/` directories

### Expected Output
- Data files in the `data/` directory
- Visualization files in the `figures/` directory
- Tables in the `tables/` directory

### Expected Run Time
- Data collection: ~30-60 minutes (depending on internet connection)
- Initial analysis: ~10-20 minutes
- Full multiverse analysis: ~4-8 hours (depending on hardware)

## Instructions for Use

### Basic Usage

1. Open the project in RStudio:
   ```
   File > Open Project > [select the repository directory]
   ```

2. Run individual scripts from the `scripts/` folder in the order listed in "Execution Order" below
   
3. Alternatively, use the R console to source scripts:
   ```R
   source("scripts/01a_pull_gtrend_data.R")
   ```

4. View generated outputs in their respective directories

### Advanced Usage

To run the complete multiverse analysis:
```R
source("scripts/04_multiverse.R")
source("scripts/04b_multiverse-table_extraction.R")
source("scripts/04c_multiverse_plots.R")
source("scripts/06_multiverse_gsynth.R")
source("scripts/06b_multiverse_gsynth.R")
source("scripts/06c_multiverse_gsynth_plots.R")
source("scripts/07_combined_multiverse_plots.R")
```

### Reproducing Results

To reproduce the specific results from the paper:
1. Ensure all dependencies are installed
2. Run scripts 01a through 03_preregistered_hypotheses.R
3. Compare outputs in the `figures/` directory with those in the paper

### File Naming Patterns

#### Data Files
- Raw Google Trends data: `[STATE]_[KEYWORD]_[TIMESPAN].csv`
  - Example: `CA_porn_2022-01-01 2024-10-31.csv`

#### Multiverse Files
- Multiverse models: `[KEYWORD]_[TIMERANGE]_[COVARIATES]_scm[SCM]_fixedeff[FIXEDEFF]_leads[LEADS]_lags[LAGS]_treatment_[TREATMENT]_verification_method_[METHOD].rds`
  - Example: `pornhub_2022-01-01_2024-10-31_with_covariates_scmTRUE_fixedeffFALSE_leads52_lags13_treatment_post_treat_verification_method_government_id.rds`

#### GSynth Files
- GSynth models: `[KEYWORD]_[TIMERANGE]_treatment_[TREATMENT]_verification_[METHOD]_force_[FORCE]_estimator_[ESTIMATOR].rds`
  - Example: `pornhub_2022-01-01_2024-10-31_treatment_post_treat_verification_government_id_force_none_estimator_ife.rds`

## Project Structure

```
.
├── api_keys                              # Census API key file (user created)
│
├── data/                                 # Processed data directory
│   ├── census_data.csv                  # Generated by 01b_pull_census_data.R
│   ├── fsc_implementation_dates.csv     # Generated by 01c_scrape_implementation_dates.R
│   ├── porn.csv                        # Generated by 02_assemble_data.R
│   ├── pornhub.csv                     # Generated by 02_assemble_data.R
│   ├── vpn.csv                         # Generated by 02_assemble_data.R
│   ├── xvideos.csv                     # Generated by 02_assemble_data.R
│   └── analysis_data_[DATE].rds        # Generated by 02_assemble_data.R
│
├── data-raw/                            # Raw Google Trends data
│   ├── [STATE]_porn_[TIMESPAN].csv     # Generated by 01a_pull_gtrend_data.R
│   ├── [STATE]_pornhub_[TIMESPAN].csv
│   ├── [STATE]_vpn_[TIMESPAN].csv
│   └── [STATE]_xvideos_[TIMESPAN].csv
│
├── figures/                             # Generated visualizations
│   ├── [KEYWORD]_att_plot.png          # Generated by 03_preregistered_hypotheses.R
│   ├── [KEYWORD]_overall_plot.png
│   ├── [KEYWORD]_state_plots.png
│   ├── pre_registered_specification_plot.png
│   ├── pre-registered_cumulative_effects.png
│   ├── single_state_example.png        # Generated by 03_single_state_example.R
│   ├── multiverse_[STATE].png          # Generated by 04c_multiverse_plots.R
│   ├── multiverse_gsynth_[STATE].png   # Generated by 06c_multiverse_gsynth_plots.R
│   └── combined_multiverse.png         # Generated by 07_combined_multiverse_plots.R
│
├── tables/                              # Generated tables
│   └── single_synth_att.tex           # Generated by 03_single_state_example.R
│
├── model_weights/                       # Model weight outputs
│   └── single_synth_example.tex        # Generated by 03_single_state_example.R
│
├── multiverse_mod/                      # Multiverse analysis outputs
│   └── [PARAMS].rds                    # Generated by 04_multiverse.R
│
├── multiverse_tables/                   # Processed multiverse results
│   └── [INDEX].csv                     # Generated by 04b_multiverse-table_extraction.R
│
├── gsynth_output/                       # GSynth model outputs
│   └── [PARAMS].rds                    # Generated by 06_multiverse_gsynth.R
│
├── gsynth_tables/                       # Processed GSynth results
│   └── [PARAMS].csv                    # Generated by 06b_multiverse_gsynth.R
│
├── combined_multiverse/                 # Combined analysis results
│   ├── augsynth_multiverse.csv         # Generated by 04c_multiverse_plots.R
│   └── gsynth_results.csv              # Generated by 06c_multiverse_gsynth_plots.R
│
├── mods/                               # Additional model outputs
│   └── [KEYWORD]_model.rds            # Generated by 03_preregistered_hypotheses.R
│
└── scripts/                            # R scripts
    ├── 01a_pull_gtrend_data.R         # Google Trends data collection
    ├── 01b_pull_census_data.R         # Census data collection
    ├── 01c_scrape_implementation_dates.R # Implementation dates
    ├── 02_assemble_data.R             # Data assembly
    ├── 03_preregistered_hypotheses.R   # Initial analysis
    ├── 03_single_state_example.R       # Single state analysis
    ├── 04_multiverse.R                # Multiverse analysis
    ├── 04b_multiverse-table_extraction.R # Results extraction
    ├── 04c_multiverse_plots.R         # Multiverse visualization
    ├── 06_multiverse_gsynth.R         # GSynth analysis
    ├── 06b_multiverse_gsynth.R        # GSynth results processing
    ├── 06c_multiverse_gsynth_plots.R  # GSynth visualization
    └── 07_combined_multiverse_plots.R  # Combined results visualization
```

## Execution Order

1. **Data Collection and Processing**
   - `01a_pull_gtrend_data.R`: Pulls Google Trends data for specified search terms
   - `01b_pull_census_data.R`: Collects census demographic data
   - `01c_scrape_implementation_dates.R`: Compiles law implementation dates
   - `02_assemble_data.R`: Combines all data sources

2. **Initial Analysis**
   - `

## License

This software is licensed under the MIT License. See LICENSE file for details.

## Code Repository

The code is available at: https://github.com/username/age-verification-analysis

## Detailed Code Functionality

A complete description of the code's functionality can be found in the Methods section of the manuscript, which provides pseudocode and algorithm descriptions for the synthetic control methods implemented.

## Reproduction Instructions

The following steps will reproduce all quantitative results presented in the manuscript:

1. Set up the environment as described in the Installation Guide
2. Run data collection scripts (01a, 01b, 01c)
3. Run data assembly script (02)
4. Run preregistered hypothesis testing (03)
5. For robustness checks, run the multiverse analysis scripts (04, 06, 07)

Exact results may vary slightly due to random initialization in some models, but all substantive findings should be reproducible.

## Troubleshooting

- **Memory Issues**: Reduce the number of parallel workers in scripts using `furrr` or `parallel`
- **Census API Rate Limits**: Add delays between calls or request a higher rate limit
- **Missing Files Error**: Ensure all directories exist and have write permissions
- **Package Compatibility**: If you encounter package conflicts, create a dedicated R environment

For additional support, please open an issue on the GitHub repository.
