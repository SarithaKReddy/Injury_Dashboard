# Data Cleaning for Occupational Illness

# Author: Saritha Kumari Krishna Reddy, Stephanie Ajah, Karanvir Virdi

# Date: Dec 12, 2024

# Load necessary libraries
library(tidyverse)
library(readxl)

# Read the Excel files
occ_data_2016 <- read_xlsx('/Users/sarithakumarik/Documents/DATA6200/FinalProject/Dataset_Occ/Data_2016.xlsx')
occ_data_2017 <- read_xlsx('/Users/sarithakumarik/Documents/DATA6200/FinalProject/Dataset_Occ/Data_2017.xlsx')
occ_data_2018 <- read_xlsx('/Users/sarithakumarik/Documents/DATA6200/FinalProject/Dataset_Occ/Data_2018.xlsx')
occ_data_2019 <- read_xlsx('/Users/sarithakumarik/Documents/DATA6200/FinalProject/Dataset_Occ/Data_2019.xlsx')
occ_data_2020 <- read_xlsx('/Users/sarithakumarik/Documents/DATA6200/FinalProject/Dataset_Occ/Data_2020.xlsx')
occ_data_2021 <- read_xlsx('/Users/sarithakumarik/Documents/DATA6200/FinalProject/Dataset_Occ/Data_2021.xlsx')
occ_data_2022 <- read_xlsx('/Users/sarithakumarik/Documents/DATA6200/FinalProject/Dataset_Occ/Data_2022.xlsx')

# Combine the data frames into a single dataframe
occ_data <- rbind(occ_data_2016, occ_data_2017, occ_data_2018, occ_data_2019, occ_data_2020, occ_data_2021, occ_data_2022)

# Industry classification using NAICS codes
occ_data <- occ_data |>
  rowwise() |>
  mutate(industry = case_when(
    str_detect(`naics_code`, regex("^11", ignore_case = TRUE)) ~ "Agriculture, Forestry, Fishing and Hunting",
    str_detect(`naics_code`, regex("^21", ignore_case = TRUE)) ~ "Mining, Quarrying, and Oil and Gas Extraction",
    str_detect(`naics_code`, regex("^22", ignore_case = TRUE)) ~ "Utilities",
    str_detect(`naics_code`, regex("^23", ignore_case = TRUE)) ~ "Construction",
    str_detect(`naics_code`, regex("^31|32|33", ignore_case = TRUE)) ~ "Manufacturing",
    str_detect(`naics_code`, regex("^42", ignore_case = TRUE)) ~ "Wholesale Trade",
    str_detect(`naics_code`, regex("^44|45", ignore_case = TRUE)) ~ "Retail Trade",
    str_detect(`naics_code`, regex("^48|49", ignore_case = TRUE)) ~ "Transportation and Warehousing",
    str_detect(`naics_code`, regex("^51", ignore_case = TRUE)) ~ "Information",
    str_detect(`naics_code`, regex("^52", ignore_case = TRUE)) ~ "Finance and Insurance",
    str_detect(`naics_code`, regex("^53", ignore_case = TRUE)) ~ "Real Estate and Rental and Leasing",
    str_detect(`naics_code`, regex("^54", ignore_case = TRUE)) ~ "Professional, Scientific, and Technical Services",
    str_detect(`naics_code`, regex("^56", ignore_case = TRUE)) ~ "Administrative and Support and Waste Management and Remediation Services",
    str_detect(`naics_code`, regex("^61", ignore_case = TRUE)) ~ "Educational Services",
    str_detect(`naics_code`, regex("^62", ignore_case = TRUE)) ~ "Health Care and Social Assistance",
    str_detect(`naics_code`, regex("^71", ignore_case = TRUE)) ~ "Arts, Entertainment, and Recreation",
    str_detect(`naics_code`, regex("^72", ignore_case = TRUE)) ~ "Accommodation and Food Services",
    str_detect(`naics_code`, regex("^81", ignore_case = TRUE)) ~ "Other Services (except Public Administration)",
    str_detect(`naics_code`, regex("^92", ignore_case = TRUE)) ~ "Public Administration",
    TRUE ~ "Other"
  ))

# Convert all states to lowercase
state_abbreviation_to_name <- c(
  "al" = "alabama", "ak" = "alaska", "az" = "arizona", "ar" = "arkansas",
  "ca" = "california", "co" = "colorado", "ct" = "connecticut", "de" = "delaware",
  "fl" = "florida", "ga" = "georgia", "hi" = "hawaii", "id" = "idaho",
  "il" = "illinois", "in" = "indiana", "ia" = "iowa", "ks" = "kansas",
  "ky" = "kentucky", "la" = "louisiana", "me" = "maine", "md" = "maryland",
  "ma" = "massachusetts", "mi" = "michigan", "mn" = "minnesota", "ms" = "mississippi",
  "mo" = "missouri", "mt" = "montana", "ne" = "nebraska", "nv" = "nevada",
  "nh" = "new hampshire", "nj" = "new jersey", "nm" = "new mexico", "ny" = "new york",
  "nc" = "north carolina", "nd" = "north dakota", "oh" = "ohio", "ok" = "oklahoma",
  "or" = "oregon", "pa" = "pennsylvania", "ri" = "rhode island", "sc" = "south carolina",
  "sd" = "south dakota", "tn" = "tennessee", "tx" = "texas", "ut" = "utah",
  "vt" = "vermont", "va" = "virginia", "wa" = "washington", "wv" = "west virginia",
  "wi" = "wisconsin", "wy" = "wyoming",
  "pr" = "puerto rico", "vi" = "virgin islands", "gu" = "guam", "pw" = "unknown",
  "mp" = "unknown", "as" = "unknown", "aa" = "unknown", "ae" = "unknown", "ap" = "unknown"
)

# Convert state abbreviations in the dataset to lowercase
occ_data$state <- tolower(occ_data$state)

# Map state abbreviations to full names
occ_data$state <- state_abbreviation_to_name[occ_data$state]

# Remove rows where state is "unknown"
occ_data <- occ_data[occ_data$state != "unknown", ]

# Dealing with the missing values in the column company_name
occ_data$company_name <- ifelse(is.na(occ_data$company_name), occ_data$establishment_name, occ_data$company_name)

# Selecting only the required columns and renaming the column names
occ_data <- occ_data |>
  select(
    year_filing_for,
    company_name,
    state,
    industry, 
    annual_average_employees, 
    total_hours_worked,
    no_injuries_illnesses, 
    total_deaths,
    total_dafw_cases,
    total_djtr_cases,
    total_other_cases,
    total_dafw_days, 
    total_djtr_days,
    total_injuries, 
    total_poisonings,
    total_respiratory_conditions,
    total_skin_disorders,
    total_hearing_loss,
    total_other_illnesses
  ) |>
  rename(
    year = year_filing_for,
    injury_illness = no_injuries_illnesses
  )

# Saving the clean data into a csv file
file_path <- "/Users/sarithakumarik/Documents/DATA6200/FinalProject/Dataset/occupational_illness_data.csv"
write.csv(occ_data, file_path, row.names = FALSE)