# -> Gerhard O
# -> 12/22/2020
# -> Data sets: 2a_HDS

library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(readxl)

# Initial Data Sets
raw_data_path <- 'C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Clean Data\\HDS\\2a_HDS'

# Clean Data
clean_data_path <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Clean Data\\HDS\\2b_HDS--Build mail volume by ID year"

#------------------------------------------------------------------------#
#   Check that all dfs contain SAMPLE.NUMBER, SURVEY.YEAR, MAIL.PIECE    #
#                               Data: 2a_HDS                             #
#------------------------------------------------------------------------#

# Print out column names for all variables
    file.name.csv <- dir(raw_data_path, pattern=".csv")
    
    for (i in file.name.csv) {
      print(i)
      df <- read.csv(file= paste(raw_data_path, i, sep = '\\'), 
                     sep=",", colClasses=c(rep(NA, 7), rep("NULL", 500)) )  
      print(colnames(df[, 1:5]))
      print(" ")
    }

#------------------------------------------------------------------------#
#                 Only Keep ID, MAIL.PIECE, and Year                     #
#           Aggregate so that each row represents one piece of mail      #
#                         Label each variable                            #
#------------------------------------------------------------------------#  

# Columns to be kept
col1 <- c("SAMPLE.NUMBER", "MAIL.PIECE", "SURVEY.YEAR") 
    
    # Loop through all HDS files minus RECTS
    for (i in file.name.csv) {
      print(i)
      
      # This is the Adult Roster File. Does not contain MAIL.PIECE. 
      if (i == "2a_rects_final.csv") {
        print("      RECTS")
      }
      
      # Create new aggregated csv file
      else {
        print(" Not RECTS")
        
        # Import Data and keep columns set in col1
        df <- read.csv(file= paste(raw_data_path, i, sep = '\\'),
                       sep=",", colClasses=c(rep(NA, 7), rep("NULL", 500)) )
        names(df) <- toupper(colnames(df))
        df <- df[, col1]
        
        # Create new MAIL.PIECE var to match data set and set value to 1 for aggregation
        a <- i
        a <- gsub("2a_", "", a)
        a <- paste0("MAIL.VOLUME.",toupper(gsub(".csv", "", a)))
        df[, "TOTAL"] <- 1    
        
        # Aggregate
        df2 <- df %>% 
          group_by(SAMPLE.NUMBER, SURVEY.YEAR) %>%
          summarise(test = sum(TOTAL)) %>%
          select(SAMPLE.NUMBER, SURVEY.YEAR, test) %>% 
          setNames(c("SAMPLE.NUMBER", "SURVEY.YEAR", a)) 
        
        # Export file
        df.new.name <- paste0("2b_", gsub("2a_", "", i))
        write_csv(df2, path = paste(clean_data_path, df.new.name, sep="\\"),
                  append=FALSE, col_names=TRUE)
      }

    }  
    
    #------------------------------------------------------------------------#
    #                         Conduct Data Checks                            #
    #------------------------------------------------------------------------# 
    
    # Check column names & number of unique IDs
        file.name.csv <- dir(clean_data_path, pattern=".csv")
        
        for (i in file.name.csv) {
          print(i)
          df <- read.csv(file= paste(clean_data_path, i, sep = '\\'), sep="," )  
          print(colnames(df))
          print(length(unique(df$SAMPLE.NUMBER)))
          cat("\n")
        }
        
        