#------------------------------------------------------------------------#
#                           Miscellaneous Code                           #
#------------------------------------------------------------------------#

#_________________________________#

# Tried to create a list of the names of columns of data frames in order to 
#   check that they have the same number of columns. Would try to include 
#   in (1a_) in order to make sure that not only are the correct number of 
#   columns but that they are the same columns since the subset I removed
#   was done by indexing. 
#   Have not included in (1a_) yet. 

# **** Found a solution beginning on line 52

df_list <- list()

test1 <- census2[, 1:4]
test2 <- census2[, 1:4]
test3 <- census2[, 2:4]



a <- setdiff(colnames(test1), colnames(test2))
is_empty(a)


N <- 3
x <- list()
for(i in 1:N) {
  Ps <- i  ## where i is whatever your Ps is
  x[[paste0("census1", i)]] <- Ps
}

#_________________________________#

# Play around with finding ways to simulate Stata 'Tab' command
# https://stackoverflow.com/questions/13043817/mimic-tabulate-command-from-stata-in-r


library(gmodels)
set.seed(1)
Data <- data.frame(
  X = sample(1:10),
  Y = sample(c("yes", "no"), 10, replace = TRUE)
)
CrossTable(Data$Y, format="SAS") # Tab similar to Stata Tabulate




#------------------------------------------------------------------------#
#                       Make sure that all columns are                   #
#                         the same in each csv file                      #
#------------------------------------------------------------------------#

# Source: https://stackoverflow.com/questions/57799567/how-to-pop-out-non-matching-column-names-in-a-series-of-csv-files

# Getting all the files from directory. Update it as required
fnames <- list.files(clean_data_path ,pattern='1a_') 

# Reading all the files
csv <- lapply(paste(clean_data_path, fnames, sep="\\"), read.csv)  

# Place column names in data frame
csv_name <- sapply(csv, colnames)

mismatches <- list(integer())
for (i in seq_len(length(csv) - 1)) {
  different <- names(csv[[i]]) != names(csv[[i + 1]])
  mismatches[[i + 1]] <- which(different)
}

mismatches 

#_________________________________#