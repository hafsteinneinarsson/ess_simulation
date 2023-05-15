# Download data for ESS rounds 6-10, save these in folder /Data

# ESS contact form data https://www.europeansocialsurvey.org/data/download_contact_form.html
# ESS survey data https://ess-search.nsd.no

# Load packages
library("here")
library("tidyverse")
library("janitor")
library("haven")
library("lubridate")
library("labelled")
library("gt")
library("scales")
library("patchwork")
library("gridExtra")
library("broom")


# 1. Clean the data and create the dataset contacts.Rdata which will be used in the analysis

# source(here::here("01_contact_data.R")) OR
# readRDS(here("Data", "contacts.Rdata"))

# 2. Get R-indicator functions from the RISQ project

source(here::here("R-Indicator RISQ.R"))

# 3. Set seed

set.seed(12345)

# 4. Run main analysis code, 02_simulations

source(here::here("02_simulations.R"))

# 5. Summarize the response propensity models

source(here::here("03_responsepropensitymodels.R"))

# 6. Summarize R-indicator/CV 

source(here::here("04_R_summary.R"))

# 7. Explore relationship between interviewer observations and target variables

source(here::here("05_intobs_targetvars.R"))


















