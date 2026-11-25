# Heat Exposure Intelligence

An end-to-end data science project investigating historical heat exposure patterns across three major Nigerian cities: **Abuja, Ilorin and Lagos**, using ERA5 weather reanalysis data from 2021 to 2025.

## 1. Project Overview

Heat exposure presents challenges for public health, occupational safety, urban planning and environmental management. Understanding how heat conditions vary across locations and over time can support evidence-informed planning and risk awareness.

This project applies a practical data science workflow to historical weather data to examine heat-related conditions, identify temporal and geographical patterns, develop predictive models, evaluate model performance and communicate findings through analytical outputs.

### Project objectives

- Acquire and organise historical weather data.
- Audit data quality, completeness and consistency.
- Clean and prepare datasets for analysis.
- Explore weather patterns across locations and time.
- Conduct statistical analyses and feature engineering.
- Develop and evaluate predictive models.
- Examine historical heat exposure patterns.
- Translate analytical findings into decision-support insights.
- Consolidate project outputs through quality assurance.

## 2. Study Coverage and Data

| Attribute | Description |
|---|---|
| Locations | Abuja, Ilorin and Lagos, Nigeria |
| Study period | 2021–2025 |
| Data frequency | Hourly and daily |
| Primary data source | Open-Meteo Historical Weather API |
| Weather dataset | ERA5 reanalysis |
| Spatial resolution | Approximately 0.25° grid resolution |
| Main data format | CSV and JSON |

The repository includes individual location datasets, a combined Nigerian weather dataset and the original API response files.

**Important data qualification:** ERA5 is a gridded atmospheric reanalysis product, not direct measurements from weather stations. Its grid resolution is approximately 25–30 km. Consequently, results represent grid-derived weather conditions and should not be interpreted as precise measurements at individual workplaces, neighbourhoods or households.

## 3. Technical Workflow

The project is organised into sequential R scripts:

| Script | Purpose |
|---|---|
| `00_project_setup.R` | Project setup |
| `01_data_acquisition.R` | Data acquisition |
| `02_data_audit.R` | Data quality assessment |
| `03_data_cleaning.R` | Data cleaning |
| `04_exploratory_analysis.R` | Exploratory data analysis |
| `05_statistical_analysis.R` | Statistical analysis |
| `06_feature_engineering.R` | Feature engineering |
| `07_predictive_modelling.R` | Predictive modelling |
| `08_model_validation.R` | Model validation |
| `09_historical_heat_exposure.R` | Historical heat exposure analysis |
| `10_model_interpretation_decision_support.R` | Model interpretation and decision support |
| `11_project_quality_assurance_and_consolidation.R` | Project-wide quality assurance |
| `supp_assistance_codes.R` | Supplementary assistance and diagnostic code |

The workflow progresses from data acquisition and preparation to analysis, modelling, validation, interpretation and project-wide quality assurance.

## 4. Repository Structure

```text
Heat_Exposure_Intelligence/
│
├── R/
│   ├── 00_project_setup.R
│   ├── 01_data_acquisition.R
│   ├── 02_data_audit.R
│   ├── 03_data_cleaning.R
│   ├── 04_exploratory_analysis.R
│   ├── 05_statistical_analysis.R
│   ├── 06_feature_engineering.R
│   ├── 07_predictive_modelling.R
│   ├── 08_model_validation.R
│   ├── 09_historical_heat_exposure.R
│   ├── 10_model_interpretation_decision_support.R
│   ├── 11_project_quality_assurance_and_consolidation.R
│   └── supp_assistance_codes.R
│
├── data/
│   └── raw/
│       ├── api_responses/
│       ├── Abuja_Weather_2021_2025.csv
│       ├── Ilorin_Weather_2021_2025.csv
│       ├── Lagos_Weather_2021_2025.csv
│       └── Nigeria_Weather_2021_2025.csv
│
├── outputs/
│   ├── audit/
│   ├── figures/
│   ├── models/
│   ├── reports/
│   └── tables/
│
├── Heat_Exposure_Intelligence.Rproj
├── .gitignore
├── APPLY_RENUMBERING_PATCH.txt
├── VALIDATE_RENUMBERING.R
└── README.md
```

Some generated and intermediate files are intentionally excluded from version control, as specified in `.gitignore`. In particular, processed datasets and large saved model objects are not included in the initial repository release.

## 5. Outputs

The consolidated project includes:

- **68 analytical tables**
- **57 analytical figures**
- **6 methodological reports**
- Data quality and audit records
- Raw weather datasets and API response files
- Reproducible R scripts

These outputs document the analytical workflow and provide supporting evidence for the project's findings.

## 6. Reproducibility

The project was developed in R using RStudio.

To work with the project:

1. Clone or download this repository.
2. Open `Heat_Exposure_Intelligence.Rproj` in RStudio.
3. Review `R/00_project_setup.R` for project configuration and package requirements.
4. Review the scripts in numerical order.
5. Follow the data acquisition, preparation, analysis and modelling workflow.
6. Run the project quality assurance script after the preceding workflow has been completed.

The raw datasets and API response files are included to support inspection and reuse of the acquired data.

Some intermediate datasets and saved model objects are excluded to keep the repository manageable. Reproducing all outputs may therefore require rerunning the relevant scripts.

## 7. Interpretation and Limitations

The project analyses weather reanalysis data and develops analytical and predictive approaches to understanding historical heat-related conditions.

The following limitations should guide interpretation:

- ERA5 reanalysis values are grid-derived estimates, not direct local station observations.
- The three study locations do not represent every climatic zone or community in Nigeria.
- The analysis does not contain individual-level health outcomes or direct occupational exposure measurements.
- Observed associations and model predictions do not establish causation.
- Model performance should be interpreted within the study period, locations and available data.

The results should therefore support exploratory assessment and informed decision-making rather than be treated as direct clinical, occupational or public-health risk measurements.

## 8. Future Development

Potential extensions include:

- Reimplementation of the complete workflow in Python.
- Interactive dashboards and decision-support applications.
- Additional Nigerian locations and longer historical periods.
- Integration of relevant health, occupational and population datasets where reliable data become available.
- Further model deployment and monitoring.

## 9. Author and Project Purpose

**Heat Exposure Intelligence** is an applied data science portfolio project demonstrating an end-to-end workflow for transforming real-world environmental data into structured analytical outputs and decision-support insights.

The project emphasises reproducibility, data quality, methodological transparency, model evaluation and practical interpretation.

---

**Data source:** [Open-Meteo Historical Weather API](https://open-meteo.com/en/docs/historical-weather-api)

**Project:** Heat Exposure Intelligence  
**Study period:** 2021–2025  
**Geographical coverage:** Abuja, Ilorin and Lagos, Nigeria  
**Primary language:** R