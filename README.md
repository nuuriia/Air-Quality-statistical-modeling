# Air-Quality-statistical-modeling
This project was developed for the course Modelització Estadística in the Bachelor’s Degree in Artificial Intelligence at FIB (UPC). It provides a comprehensive statistical study on how environmental factors like temperature, humidity, and precipitation influence air pollution and its subsequent risks to public health.

Project Description
Using a dataset of urban air quality, this project applies various statistical methods to predict health risks and understand the underlying patterns of pollution. The workflow follows the standard data science pipeline: from raw data cleaning to advanced unsupervised learning.

Key Analysis Components:
- Exploratory Data Analysis (EDA): Univariate and bivariate descriptive statistics to understand variable distributions.
- Generalized Linear Models (GLM): Implementation of models for both numerical and binary responses to identify significant predictors of pollution.
- Time Series Analysis: Studying the temporal evolution of air quality metrics.
- Dimensionality Reduction: Using PCA (Principal Component Analysis) to simplify the feature space while retaining variance.
- Clustering & Profiling:
  Hierarchical Clustering: To find natural groupings in the data.
  Partitional Clustering (K-Means): To segment observations into distinct profiles.
  Cluster Profiling: Characterizing each group to extract actionable health insights.

Technical Structure
The project is implemented in R and includes the following logical blocks:
- Preprocessing: Handling missing values, outliers, and feature engineering.
- Predictive Modeling: GLM scripts for risk assessment.
- Advanced Statistics: Scripts for Time Series and PCA decomposition.
- Segmentation: Clustering algorithms and visualization of results.
- D5 - ENTREGA FINAL.pdf: The final report containing the full discussion, methodology, and conclusions.

How to Use:

Software Requirements:
  R (version 4.0 or higher).
  Recommended IDE: RStudio.
  Required Libraries: ggplot2, factoextra, cluster, forecast, MASS.

Dataset:
Download the "Urban Air Quality and Health" dataset from Kaggle.
Place the CSV file in the project root directory.

Execution:
Run the scripts sequentially as outlined in the report (Preprocessing -> GLM -> PCA -> Clustering).
