# -> Gerhard O
# -> 12/15/2020
# -> Data sets: 2a_HDS

library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(readxl)

# Initial Data Sets
raw_data_path <- 'C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Clean Data\\HDS\\2a_HDS'
raw_brmr <- '2a_fcmr.csv'

# Clean Data
clean_data_path <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Clean Data\\HDS\\2c_HDS-FCMR"

#------------------------------------------------------------------------#
#                       Load Household Diary Study Data                  #
#                                 Data: FCMR                             #
#------------------------------------------------------------------------#

# Load in HDS (FCMR) data
fcmr_raw <- read.csv(paste(raw_data_path, raw_brmr, sep = '\\'))