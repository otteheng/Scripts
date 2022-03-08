# -> Gerhard O
# -> 12/07/2020
# -> Data sets: Raw Census data

library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)


# Raw Data
raw_data_path <- 'C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Raw Data\\Census'
test_raw_census1 <- "ACSDT5Y2012.B09019_data_with_overlays_2020-11-24T160318.csv"

# Clean Data
clean_data_path <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Clean Data\\Census\\1b_Census_Household"

#------------------------------------------------------------------------#
#                       Light Clean of Census Data                       #
#                           Prepare for export                           #
#------------------------------------------------------------------------#

# Only Household data (excludes: LIVING ARRANGEMENTS OF ADULTS 18 YEARS AND OVER 
# BY AGE - as there are only two years worth of this data)
file.name.household <- dir(raw_data_path, pattern=".B09019")

for (csv.file in file.name.household){
  print(csv.file)
  
  # Read in Raw data
  census1_raw <- read.csv(paste(raw_data_path, csv.file, sep="\\"))
  
  # Remove "estimates" variables 
  census2 <- census1_raw
  census2 <- census2 %>%
    select(ZCTA = which(str_detect(unlist(.[1,]), "Geo") == T), 
           Estimate = which(str_detect(unlist(.[1,]), 
                                       "Estimate") == T)) 
  
  # Replace "!" from first row
  census2[1, ] <- str_replace_all(census2[1, ],"!!",".")
  
  # Make the first row the column name
  header.true <- function(df) {
    names(df) <- as.character(unlist(df[1,]))
    df[-1,]
  }
  census2 <- header.true(census2)
  
  # Split ZCTA var (Keep as string)
  census2 <- separate(data = census2, col = `Geographic Area Name`, 
                      into = c("ZCTA.Label", "ZCTA"), sep = " ")
  
  # Check how many unique ZCTAs there are
  length(unique(census2$ZCTA)) # 33120
  
  # Keep columns 0f interest as long as df are same length
  if ( (ncol(census2) == 40) ) {
    print("  if: 1")
    print("    Columns = 40")
    census3 <- census2[ c(2:10, 16, 20, 26:37, 40) ]
  } 
  if (ncol(census2) != 40) {
    print("  if: 2")
    print("    Columns don't match")
    break
  } else if (ncol(census3) != 24) {
    print("  if: 3")
    break
  }
  
  # Add File name to df
  census3$CSV.Name <- csv.file
  
  #------------------------------------------------------------------------#
  #                           Export Clean File                            #
  #------------------------------------------------------------------------#
  
  # Put prefix indicating which file created data
  csv.file2 <- paste0("1b_", csv.file)

  # Export as CSV
  write_csv(census3, path = paste(clean_data_path, csv.file2, sep="\\"),
            append=FALSE, col_names=TRUE)
  
}

#------------------------------------------------------------------------#
#                       Make sure that all columns are                   #
#                         the same in each csv file                      #
#------------------------------------------------------------------------#

# Source: https://stackoverflow.com/questions/57799567/how-to-pop-out-non-matching-column-names-in-a-series-of-csv-files

# Getting all the files from directory. Update it as required
fnames <- list.files(clean_data_path ,pattern='1b_') 

# Reading all the files
csv <- lapply(paste(clean_data_path, fnames, sep="\\"), read.csv)  

# Place column names in data frame
csv_name <- sapply(csv, colnames)

mismatches <- list(integer())
for (i in seq_len(length(csv) - 1)) {
  different <- names(csv[[i]]) != names(csv[[i + 1]])
  mismatches[[i + 1]] <- which(different)
}

# No Issues found
mismatches 




