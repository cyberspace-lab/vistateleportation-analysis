# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an R-based analysis project for Vista Teleportation research using VR navigation data. The project analyzes spatial navigation, pointing accuracy, and movement patterns from VR experiments. Data comes from Unity/NavR logs and includes position tracking, session events, and pointing behavior.

## Architecture

### Core Structure
- `functions/` - Modular R functions organized by purpose
  - `loading.R` - Data ingestion from Unity log files
  - `processing.R` - Data transformation and feature engineering  
  - `getters.R` - Data access utilities
  - `analysis.R` - Statistical analysis functions
  - `visualisation.R` - Plotting and visualization functions
- `scripts/` - Main execution scripts
  - `process-data.R` - Primary data processing pipeline
  - `buffer-data.R` - Data buffering operations
- `reports/` - Quarto documents for analysis output
  - `analysis.qmd` - Main analysis report
- `temp/` - Temporary processing files and cached data
- `necessarydata/` - Required reference data (positions.RData)
- `example-data/` - Sample datasets for development

### Data Processing Pipeline
1. Raw Unity logs loaded via `load_participants()` in `loading.R`
2. Data processed through `analyze_participants()` in `analysis.R` 
3. Results written to `temp/processed/` as CSV files
4. Analysis report generated via Quarto from processed data

### Key Data Types
- **Pointing data**: Spatial accuracy measurements between target and pointed locations
- **Distance data**: Movement tracking and path analysis
- **Timing data**: Phase durations and temporal sequences
- **Trial data**: Aggregated trial-level metrics
- **Combined data**: Merged participant-level summaries

## Development Commands

### Data Processing
```r
# Main processing pipeline
source("scripts/process-data.R")

# Data buffering
source("scripts/buffer-data.R")
```

### Report Generation
```bash
# Render analysis report
quarto render reports/analysis.qmd

# Preview website
quarto preview
```

### R Environment
```r
# Load all functions
source("functions/loading.R")
source("functions/processing.R") 
source("functions/analysis.R")
source("functions/getters.R")
source("functions/visualisation.R")

# Interactive analysis
source("analysis-playground.R")
```

## Key Dependencies

- `navr` - Unity VR data processing
- `cyberframer` - Data manipulation utilities  
- `psychotronr` - Psychology research tools
- `tidyverse` - Data science workflow
- `lme4` - Mixed-effects modeling
- `here` - Path management

## Data File Patterns

- Unity logs: `NEO_GenericLogName_YYYYMMDD-HHMMSS.txt`
- Position logs: `NEO_None_position_YYYYMMDD-HHMMSS.txt` 
- Session logs: `NEO_Session_YYYYMMDD-HHMMSS.txt`
- Processed outputs: `*YYYYMMDD.csv` format in `temp/processed/`

## Important Notes

- All participant data is cached in `temp/participants.RData` to avoid reprocessing
- Object positions are loaded from `necessarydata/positions.RData`
- Project uses 2-space indentation (configured in `.Rproj`)
- Training trials are filtered out in analysis (`target != "OriginDoor"`)
- Run variants handled via participant naming patterns (`*run2*`)