# Time-Series-Analysis-of-Malaria

Clustering-based time-series analysis of malaria incidence and mortality trends across **46 Sub-Saharan African countries from 2000–2023**.

This repository contains the **R code, data-source documentation, and analytical outputs** supporting the manuscript. The study applies clustering-based time-series methods to characterize similarities and heterogeneity in malaria trends across countries and to examine their relationships with selected **sociodemographic, climatic, geographic, and health-system characteristics**.

## Methods

The analysis includes:

- Data cleaning, harmonization, and preparation of malaria and auxiliary datasets
- Time-series analysis of malaria incidence and mortality rates
- Pairwise **cross-correlation-based distance** to quantify similarity between country-level time series
- **Multidimensional scaling (MDS)** to visualize temporal similarities and differences
- **Hierarchical clustering** to identify groups of countries with similar malaria trajectories
- Assessment of clustering stability and correspondence using **cophenetic correlation** and **Baker's Gamma correlation**
- Network-based visualization of relationships among malaria indicators and complementary sociodemographic, climatic, geographic, and health-system variables
- Data visualization and generation of manuscript figures and tables

## Repository Structure

### `Data Sources/`

Documentation and links to the publicly available data sources used in the study, including malaria, sociodemographic, climatic, geographic, and health-system data.

### `Code/`

R scripts used for:

- Data cleaning and harmonization
- Time-series preparation and transformation
- Clustering and distance calculations
- Multidimensional scaling and hierarchical clustering
- Cluster comparison and validation
- Network analysis
- Statistical analysis
- Generation of figures and tables

### `Data/`

Processed datasets and analysis-ready data used by the R scripts, where applicable.

### `Output/`

Analysis outputs, including tables, statistical results, and other generated files supporting the manuscript.

### `Plots/`

Figures and visualizations generated from the analysis for the manuscript.

## Reproducibility

The analysis was conducted primarily in **R**. The repository is intended to provide a reproducible workflow from data preparation through statistical analysis and visualization.

The time-series clustering methodology was adapted from the framework developed by **Dr. Alexander Kirpich** and collaborators for comparing multivariate epidemiological dynamics:

> Kirpich et al. *Clustering-based methodology for comparing multi-characteristic epidemiological dynamics with application to COVID-19 epidemiology in Europe.*

Additional information about this methodological framework and related work is available through the [Kirpich Lab](https://alexanderkirpich.com/lab).

## Study Scope

The analysis focuses on **46 Sub-Saharan African countries** and covers the period **2000–2023**. Malaria incidence and mortality trends are analyzed alongside selected contextual variables to characterize geographic and temporal heterogeneity across the region.

The auxiliary variables are used primarily to **contextualize and compare observed malaria trajectories**, rather than to infer direct causal relationships.

## Citation

If you use this repository, please cite the associated manuscript:

> *Application of Clustering-Based Time-Series Analysis Methodology for Malaria Trends and Socio-Environmental, Climatic and Geographic Drivers in Sub-Saharan Africa, 2000–2023.*

## Author

**Israel Atobrhan**  
Georgia State University  
PhD Program in Public Health / Epidemiology
