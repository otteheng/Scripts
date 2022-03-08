# -> Gerhard O
# -> 12/1/2021
# -> Data sets: Add 2020 data to HDS

# set working directory 
setwd("C:/Users/BG4CN0/USPS/ARTSI Team Folder - Documents/Projects")

# Load Packages in Library
library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(readxl)

# Data Paths
raw_data_path <- "./_Raw Data/Household Diary Survey/Unzipped files (CSV)/2020"
clean_data_path <- "./Mail Floor/Code/Clean Data/Updated HDS/1b--Build mail volume by ID year"

#------------------------------------------------------------------------#
#                 Step 1: Check that all dfs contain SAMPLE.NUMBER,      # 
#                             SURVEY.YEAR, MAIL.PIECE                    #
#                                 Data: 1a_HDS                           #
#------------------------------------------------------------------------#

#1.1: Print out column names for all variables

    #A. Get list of all CSVs in directory
    file.name.csv <- dir(raw_data_path, pattern=".csv")
    
    #B. Loop through files
    for (i in file.name.csv) {
      print(i)
      df <- read.csv(file= paste(raw_data_path, i, sep = '/'), 
                     sep=",", colClasses=c(rep(NA, 7), rep("NULL", 500)) )  
      print(colnames(df[, 1:5]))
      print()
    }

        ##1. Copy of results stored here: Mail Floor\Code\Script\Update HDS\_Misc    

#------------------------------------------------------------------------#
#                 Step 2: Only Keep ID, MAIL.PIECE, and Year             #
#           Aggregate so that each row represents one piece of mail      #
#                         Label each variable                            #
#------------------------------------------------------------------------#  

#2.1: Columns to be kept
    
    #A. Columns
    col1 <- c("SAMPLE.NUMBER", "MAIL.PIECE", "SURVEY.YEAR") 

#2.2: Loop through all HDS files minus RECTS, DRYTS, and RSTTS
    
    #A. Loop
    for (i in file.name.csv) {
      print(i)
      
      ##1. Files that do not contain contain MAIL.PIECE. 
      if (i == "1a_rects_final.csv" | i == "1a_dryts_final.csv" | i == "1a_rstts_final.csv") {
        print("      NO MAIL.PIECE")
      }
      
      ##2. Create new aggregated csv file
      else {
        print(" (Contains MAIL.PIECE)")
            
          ##3. Import Data and keep columns set in col1
          df <- read.csv(file= paste(raw_data_path, i, sep = '/'),
                         sep=",", colClasses=c(rep(NA, 7), rep("NULL", 500)) )
          names(df) <- toupper(colnames(df))
          df <- df[, col1]
          
          ##4. Create new MAIL.PIECE var to match data set and set value to 1 for aggregation
          a <- i
          a <- gsub("1a_", "", a)
          a <- paste0("MAIL.VOLUME.",toupper(gsub(".csv", "", a)))
          df[, "TOTAL"] <- 1    
          
          ##5. Aggregate
          df2 <- df %>% 
            group_by(SAMPLE.NUMBER, SURVEY.YEAR) %>%
            summarise(test = sum(TOTAL)) %>%
            select(SAMPLE.NUMBER, SURVEY.YEAR, test) %>% 
            setNames(c("SAMPLE.NUMBER", "SURVEY.YEAR", a)) 
          
          ##6. Export file
          df.new.name <- paste0("1b_", gsub("1a_", "", i))
          write_csv(df2, path = paste(clean_data_path, df.new.name, sep="/"),
                    append=FALSE, col_names=TRUE)
      }
      
    }  
    
#------------------------------------------------------------------------#
#                   Step 3: Filter: Only keep 2020 data                  #
#                                                                        #    
#------------------------------------------------------------------------#  


#3.2: Loop through all HDS files minus RECTS, DRYTS, and RSTTS
    
    #A. Get list of all CSVs in directory
    file.name.csv.new <- dir(clean_data_path, pattern=".csv")

    #B. Loop
    for (i in file.name.csv.new) {
      print(i)
        
          ##1. Import Data 
          df <- read.csv(file= paste(clean_data_path, i, sep = '/'))
        
          ##2. Filter
          df2 <- df %>% 
            filter(SURVEY.YEAR==2020)
          
          ##3. Export file
          df.new.name <- paste0(gsub(".csv", "", i), ".2020")
          df.new.name <- paste0(df.new.name, ".csv")
          write_csv(df2, path = paste(clean_data_path, df.new.name, sep="/"),
                    append=FALSE, col_names=TRUE)
      
    }  

#------------------------------------------------------------------------#
#                     Step 4: Conduct Data Checks                        #
#------------------------------------------------------------------------# 

#4.1: Check column names & number of unique IDs
    
    #A. Get list of all CSVs in directory
    file.name.csv <- dir(clean_data_path, pattern=".csv")
    
    #B. Loop
    for (i in file.name.csv) {
      print(i)
      df <- read.csv(file= paste(clean_data_path, i, sep = '\\'), sep="," )  
      print(colnames(df))
      print(length(unique(df$SAMPLE.NUMBER)))
      cat("\n")
    }
    
    