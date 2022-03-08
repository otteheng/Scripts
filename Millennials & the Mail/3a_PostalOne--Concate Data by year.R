# -> Gerhard O
# -> 12/08/2020
# -> Data sets: Raw Postal One Data (2019-2020)

library(tidyverse)

# Raw Data
raw_data_path <- 'C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Raw Data\\Postal One'

# Clean Data
clean_data_path <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Clean Data\\Postal One\\3a_Appended Data"
csv_name20 <- "PostalOne_2020.csv"
csv_name19 <- "PostalOne_2019.csv"

#------------------------------------------------------------------------#
#                         Append Postal One data                         #
#                             by Year (2020)                             #
#------------------------------------------------------------------------#

#--->      <---
#---> 2020 <---
#--->      <---


# Only Postal One Data (as 12/8 we only have 2019 & 2020)
file.postal <- dir(raw_data_path, pattern="2020")
print(file.postal)

# Full path to csv filenames
fullpath = file.path(raw_data_path, file.postal)
print(fullpath)

# Merge listed files from the path above
postal_2020 <- do.call("rbind",lapply(fullpath,FUN=function(files)
{ read.csv(files)}))

#------------------------------------------------------------------------#
#                               Data Checks                              #
#------------------------------------------------------------------------#

    # Check all months have been appended (January-October...don't have data for
    # November and December in 2020 yet)
    unique(postal_2020$MONTH)

    # Check format of columns (zip should be character, volume integer)
    str(postal_2020) # zip is character
    
    # Sort Volume - Zip codes with large volumes are not five digits. Normally 
    # since zip is stored as a character the leading zeros should be saved as 
    # well. Will need to follow up. 
    View(postal_2020)

#------------------------------------------------------------------------#
#                           Export Clean 2020 File                       #
#------------------------------------------------------------------------#

# Put prefix indicating which file created data
csv_name2 <- paste0("3a_", csv_name20)

write_csv(postal_2020, path = paste(clean_data_path, csv_name2, sep="/"),
          append=FALSE, col_names=TRUE)



#------------------------------------------------------------------------#
#                         Append Postal One data                         #
#                             by Year (2019)                             #
#------------------------------------------------------------------------#

#--->      <---
#---> 2019 <---
#--->      <---

    #----------------------------------#
    #   Not all column names match     #
    #----------------------------------#

    # Source: https://stackoverflow.com/questions/57799567/how-to-pop-out-non-matching-column-names-in-a-series-of-csv-files
    
    # Getting all the files from directory. Update it as required
    fnames <- list.files(raw_data_path ,pattern='2019') 
    
    # Reading all the files
    csv <- lapply(paste(raw_data_path, fnames, sep="\\"), read.csv)  
    
    # Place column names in data frame
    csv_colname <- sapply(csv, colnames)
    
    mismatches <- list(integer())
    for (i in seq_len(length(csv) - 1)) {
      different <- names(csv[[i]]) != names(csv[[i + 1]])
      mismatches[[i + 1]] <- which(different)
    }
    
    # PostalOne data (012019-042019) title column[1] as "Zip" vs "X5DZip"
    mismatches 
    
    # Change column names in df list
    right_colnames <- colnames(csv[[5]])
    csv2 <- lapply(csv, setNames, right_colnames)

    # Place column names in data frame
    csv_colname2 <- sapply(csv2, colnames)
    

#----------------------------------#
#   Issue resolved--Append         #
#----------------------------------#

# Only Postal One Data (as 12/8 we only have 2019 & 2020)
file.postal <- dir(raw_data_path, pattern="2019")
print(file.postal)

# Full path to csv filenames
fullpath = file.path(raw_data_path, file.postal)
print(fullpath)

# Merge listed files from the path above
postal_2019 <- do.call("rbind", csv2)

#------------------------------------------------------------------------#
#                               Data Checks                              #
#------------------------------------------------------------------------#

    # Check all months have been appended 
    unique(postal_2019$MONTH)

    # Check format of columns (zip should be character, volume integer)
    str(postal_2019) # zip is character
    
    # Sort Volume - Zip codes with large volumes are not five digits. Normally 
    # since zip is stored as a character the leading zeros should be saved as 
    # well. Will need to follow up. 
    View(postal_2019)

#------------------------------------------------------------------------#
#                           Export Clean 2020 File                       #
#------------------------------------------------------------------------#

# Put prefix indicating which file created data
csv_name2 <- paste0("3a_", csv_name19)

write_csv(postal_2019, path = paste(clean_data_path, csv_name2, sep="/"),
          append=FALSE, col_names=TRUE)

