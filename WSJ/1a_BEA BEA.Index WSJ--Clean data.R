# -> Gerhard O
# -> 9/10/2021
#     Update: 1/21/2022
# -> Data sets: BEA, BEA Index, WSJ Survey

# set working directory 
setwd("C:/Users/BG4CN0/USPS/ARTSI Team Folder - Documents/Projects")

# Load Packages in Library
source('./library_GSO.R')

# Raw Data
raw_data_path <- "./_Raw Data/WSJ"

# New Data
new_data_path <- "./WSJ/4-Code and Analysis/Clean Data/1a"

# Names of files
n.bea_raw <- "BEA--GDP_2006Q1-2021Q3 (Jan 22).csv"
n.bea_index_raw <- "BEA--GDP Indexes_2006Q1-2021Q3 (Jan 22).csv"
n.wsj_raw <- "wsjecon0122.xlsx"

#------------------------------------------------------------------------#
#                           Step 1: Load in Data                         #
#                                                                        #
#------------------------------------------------------------------------#

bea_raw <- read.csv(paste(raw_data_path, "BEA/GDP/BEA--GDP_2006Q1-2021Q3 (Jan 22).csv", 
                          sep = "/"), skip = 6, header = F)

bea_index_raw <- read.csv(paste(raw_data_path, "BEA/GDP Indexes/BEA--GDP Indexes_2006Q1-2021Q3 (Jan 22).csv", 
                                sep = "/"), skip = 6, header = F)

wsj_raw <- read_xlsx(paste(raw_data_path, "WSJ Survey/wsjecon0122.xlsx",
                           sep = "/"), skip = 2, col_names = F, n_max = 76) # <- Check n_max as number of individuals surveyed may change
    
#------------------------------------------------------------------------#
#                            Step 2: Clean Data                          #
#                                 BEA (GDP)                              #
#------------------------------------------------------------------------#
    
#2.1: Fix column names

bea <- bea_raw
        
    #A. Remove columns with all NAs
    all_na <- function(x) any(!is.na(x))
    bea <- bea %>% dplyr::select(where(all_na))
    
    #B. Use combination of Year and Quarter as column names
    
        ##1. Take heading from CSV 
        new.col.names <- read.csv(paste(raw_data_path, "BEA/GDP/BEA--GDP_2006Q1-2021Q3 (Jan 22).csv", 
                                        sep = "/"), skip = 4, nrow = 2, header = F)
        
        ##2. Collapse rows into one row
        new.col.names <- new.col.names %>% 
          summarise_all(funs(trimws(paste(., collapse = '.'))))
        
        ##3. Make row into column name
        new.col.names <- new.col.names %>%
          row_to_names(row_number = 1) 
        
        ##4. Rename
        new.col.names <- colnames(new.col.names)
        colnames(bea) <- new.col.names
        
    #C. Rename columns
    bea <- bea %>%
      rename(Line = Line.Line,
             Items = NA.NA) %>%
      rename_with(~ gsub(".20", "YR.20", .x, fixed = TRUE))
    
#2.2: Fix issues with data
    
    #A. Convert necessary columns to numeric
    bea <- bea %>%
      
        ##1. Remove white space
        mutate_all(str_trim) %>%
      
        ##2. Remove extraneous characters
        mutate_all(funs(gsub("-", "", .))) %>%
    
        ##3. Convert to numeric
        mutate_at(vars(matches("YR")), as.numeric)
    
    
#------------------------------------------------------------------------#
#                            Step 3: Clean Data                          #
#                              BEA (GDP Index)                           #
#------------------------------------------------------------------------#

#3.1: Fix column names

bea_index <- bea_index_raw
        
    #A. Remove columns with all NAs
    all_na <- function(x) any(!is.na(x))
    bea_index <- bea_index %>% dplyr::select(where(all_na))
    
    #B. Use combination of Year and Quarter as column names
        
        ##1. Take heading from CSV 
        new.col.names <- read.csv(paste(raw_data_path, "BEA/GDP Indexes/BEA--GDP Indexes_2006Q1-2021Q3 (Jan 22).csv", 
                                        sep = "/"), skip = 4, nrow = 2, header = F)
        
        ##2. Collapse rows into one row
        new.col.names <- new.col.names %>% 
          summarise_all(funs(trimws(paste(., collapse = '.'))))
        
        ##3. Make row into column name
        new.col.names <- new.col.names %>%
          row_to_names(row_number = 1) 
        
        ##4. Rename
        new.col.names <- colnames(new.col.names)
        colnames(bea_index) <- new.col.names
    
    #C. Rename columns
    bea_index <- bea_index %>%
      rename(Line = Line.Line,
             Items = NA.NA) %>%
      rename_with(~ gsub(".20", "YR.20", .x, fixed = TRUE))
    
#3.2: Fix issues with data
    
    #A. Convert necessary columns to numeric
    bea_index <- bea_index %>%
      
        ##1. Remove white space
        mutate_all(str_trim) %>%
        
        ##2. Remove extraneous characters
        mutate_all(funs(gsub("-", "", .))) %>%
        
        ##3. Convert to numeric
        mutate_at(vars(matches("YR")), as.numeric)    
    
    
#------------------------------------------------------------------------#
#                            Step 4: Clean Data                          #
#                                WSJ Survey                              #
#------------------------------------------------------------------------#
    
#4.1: Fix column names
    
wsj <- wsj_raw
    
    #A. Remove columns with all NAs
    all_na <- function(x) any(!is.na(x))
    wsj <- wsj %>% dplyr::select(where(all_na))
    
    #B. Use combination of Year and Quarter as column names
    
        ##1. Take heading from CSV 
        new.col.names <- read_xlsx(paste(raw_data_path, "WSJ Survey/wsjecon0122.xlsx",
                                         sep = "/"), col_names = F, n_max = 2)
        
        ##2. Transpose, fill in values, and transpose back
        new.col.names <- t(new.col.names)
        new.col.names <- as.data.frame(new.col.names) %>%
            fill(V1) 
        new.col.names <- as.data.frame(t(new.col.names))
        
        ##3. Collapse rows into one row
        new.col.names <- new.col.names %>% 
            summarise_all(funs(trimws(paste(., collapse = '.'))))
        
        ##4. Make row into column name
        new.col.names <- new.col.names %>%
            row_to_names(row_number = 1) 
        
        ##4. Rename
        new.col.names <- colnames(new.col.names)
        colnames(wsj) <- new.col.names
    
    
# Since I don't need every column I'll change column types once I pull out the 
# columns I need to make the graphs
    
    
#------------------------------------------------------------------------#
#                            Step 5: Clean Data                          #
#                               Export Data                              #
#------------------------------------------------------------------------#

#5.1: Export

    #A. BEA data
    write.csv(bea, paste(new_data_path, 
                            paste0('1a_', "BEA--GDP_2006Q1-2021Q3 (Jan 22).csv"), 
                            sep = "/"), row.names = FALSE) 
    
    #B. BEA Index data
    write.csv(bea_index, paste(new_data_path, 
                         paste0('1a_', "BEA--GDP Indexes_2006Q1-2021Q3 (Jan 22).csv"), 
                         sep = "/"), row.names = FALSE) 
    
    #A. WSJ Survey data
    write.csv(wsj, paste(new_data_path, 
                         paste0('1a_', "WSJ--Survey Data January 2022.csv"), 
                         sep = "/"), row.names = FALSE) 
    
    
    
    