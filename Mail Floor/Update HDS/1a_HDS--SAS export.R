# -> Gerhard O
# -> 12/1/2021
# -> Data sets: HDS SAS Files
# ->    Retrieved: 2000-2019 (12/4/2020)
# ->    Retrieved: 2020 (11/17/2021)

# set working directory 
setwd("C:/Users/BG4CN0/USPS/ARTSI Team Folder - Documents/Projects")

library(haven)
library(sjlabelled)
library(readr)

# Raw Data
raw_data_path <- './_Raw Data/Household Diary Survey/Zip files/2020'

# Clean Data
clean_data_path <- "./_Raw Data/Household Diary Survey/Unzipped files (CSV)/2020"

#------------------------------------------------------------------------#
#                         Load SAS Files and Export                      #
#                                 to CSV                                 #
#------------------------------------------------------------------------#

# Only SAS files 
file.name.sas <- dir(raw_data_path, pattern=".sas7bdat")

for (sas in file.name.sas) {
  print(sas)
  
  # Import SAS file
  print("1")
  sas_raw <- read_sas(paste(raw_data_path, sas, sep="\\"))
  
  # Convert Variable labels to column names
  print("2")
  sas_new <- sas_raw
  colnames(sas_new) <- colnames(label_to_colnames(sas_new))
  
  # Add prefix to file name and remove SAS file extension
  print("3")
  sas.new.name <- paste0("1a_", gsub(".sas7bdat", ".csv", sas))
  
  # Save memory
  sas_raw <- ""
  
  # Export as CSV
  write_csv(sas_new, path = paste(clean_data_path, sas.new.name, sep="\\"),
            append=FALSE, col_names=TRUE)
  
  sas_new <- ""
  print("Done")
}

  

