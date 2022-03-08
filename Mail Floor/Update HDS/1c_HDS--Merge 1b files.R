# -> Gerhard O
# -> 12/1/2021
# -> Data sets: 1b_HDS

library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(readxl)

# Set working directory 
setwd("C:/Users/BG4CN0/USPS/ARTSI Team Folder - Documents/Projects")

# Initial Data Sets
raw_data_path <- "./Mail Floor/Code/Clean Data/Updated HDS/1b--Build mail volume by ID year"
misc_files <- "./_Scripts"

# Clean Data
clean_data_path <- "./Mail Floor/Code/Clean Data/Updated HDS/1c--Merge 1b files"

#------------------------------------------------------------------------#
#                   Step 1: Merge Aggregate HDS data together            #
#                               Data: 1b_HDS                             #
#------------------------------------------------------------------------#

#1.1:  Import data sets

    #A. 2020 data
    file.name.csv <- dir(raw_data_path, pattern=".2020")
    csvs_1b = lapply(paste(raw_data_path, file.name.csv, sep = '/'), read.csv)

    #B. Use merge function that includes _merge
    source(paste(misc_files, "full_join_track.R", sep = '/'))

#1.2: Merge data sets together
    
    #A. Merge Standard Marketing Mail (BRMR) & First Class Mail Received (FCMR)
    merged <- csvs_1b[[1]] %>% full_join_track(csvs_1b[[2]], 
                                               by = c("SAMPLE.NUMBER", "SURVEY.YEAR"), 
                                               .merge = T) %>% 
      rename(.merge1_2 = .merge) %>%
      
        ##1. Merge in Non-profit Mail Received (NPMR)
        full_join_track(csvs_1b[[4]], 
                        by = c("SAMPLE.NUMBER", "SURVEY.YEAR"), 
                        .merge = T) %>% 
        rename(.merge1_2_4 = .merge) %>%
        
        ##2. Merge in Periodicals Received (PEMR)
        full_join_track(csvs_1b[[5]], 
                        by = c("SAMPLE.NUMBER", "SURVEY.YEAR"), 
                        .merge = T) %>% 
        rename(.merge1_2_4_5 = .merge) %>%
        
        ##3. Merge in Packages Received (PKGR)
        full_join_track(csvs_1b[[6]], 
                        by = c("SAMPLE.NUMBER", "SURVEY.YEAR"), 
                        .merge = T) %>% 
        rename(.merge1_2_4_5_6 = .merge) %>%
        
        ##4. Merge in Unaddressed Mail Received (UAMR)
        full_join_track(csvs_1b[[8]], 
                        by = c("SAMPLE.NUMBER", "SURVEY.YEAR"), 
                        .merge = T) %>% 
        rename(.merge1_2_4_5_6_8 = .merge) %>%
        
        ##5. Merge in First Class Mail Sent (FCMS)
        full_join_track(csvs_1b[[3]], 
                        by = c("SAMPLE.NUMBER", "SURVEY.YEAR"), 
                        .merge = T) %>% 
        rename(.merge1_2_4_5_6_8_3 = .merge) %>%
        
        ##6. Merge in Packages Sent (PKGS)
        full_join_track(csvs_1b[[7]], 
                        by = c("SAMPLE.NUMBER", "SURVEY.YEAR"), 
                        .merge = T) %>% 
        rename(.merge1_2_4_5_6_8_3_7 = .merge) 

#------------------------------------------------------------------------#
#                       Step 2: Convert NAs to Zero                      #
#                                                                        #
#------------------------------------------------------------------------#

#2.1: This indicates that ID didn't receive X type of mail
    
    #A. Change NAs to zero 
    merged[is.na(merged)] <- 0

#------------------------------------------------------------------------#
#                         Step 3: Export to CSV                          #
#                                                                        #
#------------------------------------------------------------------------#

#3.1: Export
    
    #A. Include merge indicator    
    df.new.name <- "1c_mail volume (_merge).csv"
    write_csv(merged, path = paste(clean_data_path, df.new.name, sep='/'),
              append=FALSE, col_names=TRUE)

    #B. Remove merge indicator  
    merged2 <- merged %>% select(-contains(".merge"))
    df.new.name <- "1c_mail volume.csv"
    write_csv(merged2, path = paste(clean_data_path, df.new.name, sep='/'),
              append=FALSE, col_names=TRUE)

