# To screen or not to screen: an interactive framework for comparing costs of mass malaria treatment interventions

This repository contains the data and R/RShiny code for running the analysis and creating the applications from "[To screen or not to screen: an interactive framework for comparing costs of mass malaria treatment interventions](https://rdcu.be/b43rC)", published in *BMC Medicine*. The article is Open Access, so please refer to the article for information on the underlying models and data. 

The core of this repository are the two Shiny applications. The `general-app` folder contains code for the [generalized framework application]( https://jjmillar.shinyapps.io/msat-general/ ) (Figure 1), which is solely based on user input data for RDT sensitivity and specificity. This app is not reliant on external data, and contained in a single `app.R` file. 

The `example-app` folder contains the data and R code for the ["data-driven" example application]( https://jjmillar.shinyapps.io/msat-example/ ) based of off DHS/MIS survey data. The Shiny app is contained in `app.R`, and uses model estimates (as CSV files) for malaria prevalence and diagnostic performance. More details on the contents of this folder are provided below.

## Example application using DHS/MIS data

The data for this application were collected from recent DHS and MIS surveys (details provided in Table 2), and were aggregated to region-level (Admin1). The `example-app\r\` folder contains R code used create the models for prevalence, sensitivity, and specificity. The core scripts are:

* `brms-models.R`: Creates the country-level mixed effect models for malaria prevalence and RDT Sn/Sp, saved as .RDS files.
* `sample-posterior.R`: Draws posterior samples from models, which are used for estimates in the Shiny application. 

The application, `app.R`, uses the outputs from these scripts, which are contained in the `data` subfolder. Finally, additional assets for the application are contained in the `shapefiles\` folder and the `functions.R` scripts. 