# City Look Dissertation Project v2
## Urban Green Visibility Analysis in Inner London
# City Look: Urban Green Visibility and Social Inequality in Inner London

## Project Overview

This repository contains the complete analytical pipeline for dissertation: **"City Look: Urban Character Assessment Using Mapillary Street View Image Analysis"**. The research investigates the relationship between street-level green visibility and socioeconomic deprivation across Inner London's 1,731 Lower Layer Super Output Areas (LSOAs).

### Key Findings
- **Negative correlation** between green visibility and deprivation (r = -0.327, p < 0.001)
- **3.4 percentage point gap** in mean GVI between least and most deprived quintiles
- **Strong spatial clustering** of green inequality (Moran's I = 0.487)
- **Borough-level heterogeneity** in the GVI-deprivation relationship

## Research Questions

1. **RQ1**: What is the relationship between street-level green visibility and socioeconomic deprivation?
2. **RQ2**: How does this relationship vary spatially across Inner London?
3. **RQ3**: What urban form factors moderate the GVI-deprivation relationship?

## Repository Structure

```
city-look-dissertation/
│
├── R/                       
│   ├── config.R               # Project configuration
│
├── analysis/                   # Analysis scripts (numbered sequence)
│   ├── 01_lsoa_selection.R   # Study area definition
│   ├── 02_streetview_download.R # Image acquisition
│   ├── 03_gvi_calculation.R  # Green view index computation
│   ├── 04_RQ1_analysis.R     # Descriptive & correlation analysis
│   ├── 05_RQ2_spatial.R      # Spatial analysis (LISA, GWR)
│   └── 06_RQ3_mechanisms.R   # Mechanism investigation
│
├── data/                      # Data directory (not included)
│   ├── raw/                   # Original datasets
│   └── processed/             # Processed data
│
├── output/                    # Analysis results
│   ├── tables/               # Statistical tables
│   └── models/               # Model outputs
│
├── figures/                   # Visualizations
│   ├── descriptive/          # Descriptive plots
│   ├── spatial/              # Maps and spatial figures
│   └── mechanisms/           # Interaction plots
│
│
└── README.md                 # This file
```

## Getting Started

### Prerequisites

- R version ≥ 4.1.0
- Python 3.8+ (for image processing)
- Mapillary API token (free registration required)

### Installation

1. **Clone the repository**
```bash
git clone https://github.com/Yaxin0224/dissertation_final_submit.git
cd city-look-dissertation
```

2. **Install R dependencies**
```r
# Install required packages
install.packages(c(
  "tidyverse", "sf", "spdep", "GWmodel",
  "corrplot", "viridis", "here", "randomForest",
  "mgcv", "car", "performance", "interactions"
))

# For streetscape package (Mapillary integration)
install.packages("streetscape")
```

3. **Configure API access**
```r
# Edit R/config.R with your Mapillary token
MAPILLARY_TOKEN <- "MLY|9922859457805691|cef02444f32c339cf09761b104ca4bb5"
```

## Data Sources

| Dataset | Source | Year | Description |
|---------|--------|------|-------------|
| Street View Imagery | Mapillary API v4 | 2020-2024 | 138,000+ crowdsourced images |
| LSOA Boundaries | ONS Digital Boundaries | 2011 | 1,731 spatial units |
| Index of Multiple Deprivation | MHCLG | 2019 | Composite deprivation scores |
| Population Density | ONS Census | 2021 | Residents per km² |
| PTAL Scores | Transport for London | 2021 | Public transport accessibility |
| Building Density | Ordnance Survey | 2022 | Built environment coverage |

## Methodology

### 1. Data Collection
- **Adaptive spatial sampling** strategy for Mapillary images
- **Quality filtering** (resolution, date, angle)
- **Spherical projection** for accurate 360° GVI calculation

### 2. Green View Index Calculation
- **Semantic segmentation** using SegFormer model
- **Vegetation detection** across multiple classes
- **Aggregation** to LSOA level (mean values)

### 3. Missing Data Imputation
- **Random Forest** imputation for 71 LSOAs (4.1%)
- **Spatial predictors** from neighboring areas
- **Cross-validation** for accuracy assessment

### 4. Statistical Analysis
- **Correlation analysis** (Pearson, Spearman, Kendall)
- **Spatial autocorrelation** (Global & Local Moran's I)
- **Geographically Weighted Regression** (GWR)
- **Interaction models** with urban form variables

## Key Results

### Overall Statistics
- **Mean GVI**: 15.6% (SD = 8.2%)
- **Measured LSOAs**: 1,660 (95.9%)
- **Predicted LSOAs**: 71 (4.1%)

### By IMD Quintile
| Quintile | Mean GVI (%) | SD | N |
|----------|-------------|-----|-----|
| Q1 (Least Deprived) | 17.9 | 8.8 | 346 |
| Q2 | 16.5 | 8.4 | 346 |
| Q3 | 15.8 | 8.1 | 347 |
| Q4 | 14.8 | 7.7 | 346 |
| Q5 (Most Deprived) | 14.5 | 7.5 | 346 |

### Spatial Clustering (LISA)
- **High-High clusters**: 123 LSOAs (7.1%) - Affluent green areas
- **Low-Low clusters**: 1 LSOA (0.1%) - Deprived low-green area
- **Low-High outliers**: 5 LSOAs (0.3%) - Green inequality hotspots



## Contributing

Contributions are welcome! Please:
1. Fork the repository
2. Create a feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit changes (`git commit -m 'Add AmazingFeature'`)
4. Push to branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request


## Acknowledgments

- **UCL CASA** for academic support and resources
- **Mapillary** for providing open street view imagery
- **ONS** and **TfL** for open data provision
- **Dissertation supervisor** Palominos Ortega, Nicolas for guidance
- **R and Python communities** for open-source tools

## Contact

- **Author**: Yaxin Zhuang
- **Email**: ucfnyz2@ucl.ac.uk
- **Institution**: Centre for Advanced Spatial Analysis, UCL

## Related Resources

- [Mapillary API Documentation](https://www.mapillary.com/developer)
- [UCL CASA Research](https://www.ucl.ac.uk/bartlett/casa/)
- [London Datastore](https://data.london.gov.uk/)
- [streetscape R Package](https://cran.r-project.org/package=streetscape)

---

<p align="center"> <img src="figures/logo_ucl.png" alt="UCL Logo" width="200"/> <br> <em>Centre for Advanced Spatial Analysis</em> <br> <em>University College London</em> </p>
