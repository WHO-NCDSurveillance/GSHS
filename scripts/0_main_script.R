rm(list =ls())
options(survey.lonely.psu="certainty")
options(survey.adjust.domain.lonely=TRUE)

# Setting working directory. The directory  should point to the downloaded folder
setwd("~/WHO/rstudio_interface")
# Loading all libraries
all.packages <- c(
  'haven', 'shinythemes', 'tidyverse', 'shiny', 'shinydashboard',
  'shinyWidgets', 'lubridate', 'stringr', 'readxl', 'plotly', 'zoo',
  'gridExtra', 'cowplot', 'anytime', 'data.table', 'DT', 'shinyjs',
  'flextable', 'officer', 'ggpubr', 'EpiReport', 'xlsx', 'zscorer',
  'Hmisc', 'survey', 'questionr', 'writexl', 'openxlsx', 'future.apply','future'
)

# Function to check and install packages
check_and_install_packages <- function(packages) {
  installed_packages <- rownames(installed.packages())
  
# Identify packages that are not installed
  missing_packages <- packages[!packages %in% installed_packages]
  
  if (length(missing_packages) > 0) {
    message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages)
  } else {
    message("All packages are already installed.")
  }
}
# Check and install the packages
check_and_install_packages(all.packages)
# loading libraries
eval(parse(text = paste0('library(',all.packages,')', sep='\n')))
# Allocation of cores for analyses
num_cores = as.numeric(availableCores())
num_cores = ifelse(num_cores>1, round(num_cores*0.6), num_cores)
# Plan and set up parallel processing using future
plan(multisession, workers = num_cores)
# Indicating country name and the year of the survey
site_name = 'Bahamas'
survey_year ='2023'
compute_BMI_indicators = TRUE 
weighted_analysis = TRUE
is_this_census = TRUE
language =c('ENGLISH','FRENCH', 'SPANISH','RUSSIAN','OTHER')[1]
#if (language =='OTHER'){language = 'SPECIFY'} else{}
weighting_grade_only = FALSE
weighting_sex_only = FALSE
no_post_strat_weighting = FALSE

# Clear content of temp_tables, reports, and weighted dataset folders
unlink(paste0(getwd(),"/temp_tables/*"))
unlink(paste0(getwd(),"/reports/*"))
unlink(paste0(getwd(),"/weighted dataset/*"))

# The scripts are structured into xxx sections as indicated below

##Section 1: Edit checks, cleaning, and mapping site specific survey questions to standard format
source(paste0(getwd(),'/scripts/1_cleaning_and_mapping.R'))
##Section 2: Weighting
source(paste0(getwd(),'/scripts/2_weighting.R'))
###Section 3: Preprocessing of datasets
source(paste0(getwd(),'/scripts/3_pre_report_processing.R'))
##Section 4: Primary codebook generation
source(paste0(getwd(),'/scripts/4_primary_codebook.R'))
##Section 5: Demographic Table
source(paste0(getwd(),'/scripts/5_demographic_table.R'))
##Section 6: Detailed tables
source(paste0(getwd(),'/scripts/6_detailed_tables.R'))
##Section 7: Binary codebook generation
source(paste0(getwd(),'/scripts/7_binary_codebook.R'))
##Section 8: Summary tables
source(paste0(getwd(),'/scripts/8_summary_tables.R'))
##Section 9: Fact sheet
source(paste0(getwd(),'/scripts/9_factsheet.R'))
##Section 10: Sample Description
source(paste0(getwd(),'/scripts/10_sample_description.R'))
###Stopping the cores
#stopCluster(cl)
