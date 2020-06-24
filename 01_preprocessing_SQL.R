############################################################################
# Preamble
############################################################################
source("../00_R_functions/preamble.R")
rm(list=ls())

### Load packages
library(tidyverse)
library(magrittr)

library(DBI) # GEneral R database interface
library(RPostgres) # PostgreSQL interface driver
library(dbplyr) # for dplyr with databases

### Database connection

# set up connection to existing PostgreSQL database, just plug in own details
con <- dbConnect(drv = RPostgres::Postgres(), 
                 dbname = "patstat2019",
                 host = "127.0.0.1", port = 5432,
                 user = "danieldb", password = "postgres2019")

# Inspect DB:
db_list_tables(con) %>% sort()

# Create main tables

# Create the relevant lazy tibbles
appln <-tbl(con, "tls201_appln") 
pat_ind_oecd <-tbl(con, "xtra_ind_oecd") 

# get the data
pat_data <- pat_ind_oecd %>%
  left_join(appln %>% select(appln_id, nb_applicants, nb_inventors), by = 'appln_id') %>%
  collect()
  
# restrict 
pat_data %<>% 
  #distinct(docdb_family_id, .keep_all = TRUE) %>%
  select(-docdb_family_id, -inpadoc_family_id)

#save
pat_data %>%
  saveRDS('../input/pat_data_main.rds')




