# -> Gerhard O
# -> 5/17/2021
# -> Data sets: FY2000 Q1 - FY2020 Q4
# -> Pull Totals (i.e. rows that have "total" in title)

# Load Packages in Library
source('C:\\Users\\bg4cn0\\USPS\\ARTSI Team Folder - Documents\\Projects\\library_GSO.R')

# Raw Data
raw_data_path <- "C:\\Users\\bg4cn0\\USPS\\ARTSI Team Folder - Documents\\Projects\\_Raw Data\\USPS Public\\Unaggregated data\\RPW"

# New Data
new_data_path <- "C:\\Users\\bg4cn0\\USPS\\ARTSI Team Folder - Documents\\Projects\\_Raw Data\\USPS Public"

#------------------------------------------------------------------------#
#                       Step 1:Review Weird Cases                        #
#                                                                        #
#------------------------------------------------------------------------#

raw_fy2001q4 <- read.csv(paste(raw_data_path, "fy2001-q4.csv", sep = '\\'), 
                         skip = 11, nrows = 47, na.strings = c(""),
                         header = F)

raw_fy2008q4 <- read.csv(paste(raw_data_path, "fy2008-q4.csv", sep = '\\'), 
                         skip = 13, nrows = 40, na.strings = c(""),
                         header = F)

raw_fy2012q4 <- read.csv(paste(raw_data_path, "fy2012-q1.csv", sep = '\\'), 
                         skip = 13, nrows = 40, na.strings = c(""),
                         header = F)

raw_fy2020q4 <- read.csv(paste(raw_data_path, "fy2020-q4.csv", sep = '\\'), 
                         skip = 13, nrows = 40, na.strings = c(""),
                         header = F)

#------------------------------------------------------------------------#
#                     Step 2: Grab directory of files                    #
#                   Divide into years that are similar                   #
#------------------------------------------------------------------------#

#2.1: File names
    
    #A. Grab all csv file names
    file.name.csv <- dir(raw_data_path, pattern=".csv")
    
    #B. Files from 2001-2008 maintain same structure
    file.name.2001.2008.q3 <- file.name.csv[1:31]
    
    #C. Files from 2008-2020 maintain same structure
    file.name.2008.2020 <- file.name.csv[32:length(file.name.csv)]

#------------------------------------------------------------------------#
#                             Step 3: Clean Data                         #
#                         Years: FY 2001 - FY 2006                       #
#------------------------------------------------------------------------#
    
for (csv.file in file.name.2001.2008.q3 ){
  
#3.1: Fix layout
    print(csv.file)  

    df <- read.csv(paste(raw_data_path, csv.file, sep = '\\'), 
                             skip = 11, nrows = 47, na.strings = c(""), 
                             header = FALSE)

    #A. Remove columns will all NAs
    all_na <- function(x) any(!is.na(x))
    df <- df %>% dplyr::select(where(all_na))

    #B. Rename columns
    
        ##1. Take heading from CSV 
        new.col.names <- read.csv(paste(raw_data_path, csv.file, sep = '\\'), 
                                  skip = 4, nrows = 6, header = FALSE)

        ##2. Remove extraneous rows
        new.col.names <- new.col.names %>% 
          filter(!grepl('=', V3)) %>%
          dplyr::select(where(all_na)) %>%
          replace(is.na(.), "")
    
        ##3. Collapse rows into one row
        new.col.names <- new.col.names %>%
          summarise(V1 = paste(V1, collapse = ""),
                    V3 = paste(V3, collapse = ""),
                    V5 = paste(V5, collapse = ""),
                    V7 = paste(V7, collapse = ""),
                    V9 = paste(V9, collapse = ""),
                    V11 = paste(V11, collapse = ""),
                    V13 = paste(V13, collapse = ""),
                    V15 = paste(V15, collapse = ""),
                    V17 = paste(V17, collapse = ""),
                    V19 = paste(V19, collapse = ""),
                    V21 = paste(V21, collapse = ""),
                    V23 = paste(V23, collapse = ""),
                    V25 = paste(V25, collapse = ""))
       
        ##4. Make row into column name
        new.col.names <- new.col.names %>%
          row_to_names(row_number = 1) %>%
          clean_names()
        
        ##5. Rename
        new.col.names <- colnames(new.col.names)
        colnames(df) <- new.col.names

    #C. Remove blank rows
    df <- df %>% 
      remove_empty("rows")
    
#3.2: Add variables
    
    #A. Add variable indicating FCM, MM, etc. 
    df <- df %>%
      mutate(topline_service_category = if_else(endsWith(service_category, ":"), 
                                                service_category, "")) %>%
      mutate(topline_service_category = na_if(topline_service_category, "")) %>%
      fill(topline_service_category, .direction = "down") %>%
      relocate(topline_service_category, .before = service_category)
    
    #B. Remove first element of group now that topline_service_category contains info
    df <- df %>%
      group_by(topline_service_category) %>%
      slice(-1)
    
    #B. Add file name to dataframe 
    df$csv.file.name <- csv.file
    
#3.3: Export CSVs
    
    #A. Export
    write.csv(df, paste(new_data_path, paste0('3_', csv.file), sep = "\\"), row.names = FALSE)
    
}

#------------------------------------------------------------------------#
#                 Step 4: Narrow Data down to include just totals        #
#                         Years: FY 2001 - FY 2006                       #
#------------------------------------------------------------------------#  

#4.1: Identify rows with misplaced headers
    
    #A. Import data sets
    file3.2001.2008.q3  <- dir(new_data_path, pattern="3_")
    csvs.new = lapply(paste(new_data_path, file3.2001.2008.q3 , sep = '\\'), read.csv)  
    
    #B. See which data frames contain extra rows and delete manually
    lapply(csvs.new, function(x) any(grepl("REVENUE, PIECES, AND WEIGHT BY CLASSES OF MAIL", 
                                           x[, "service_category"])))
    
        ##1. Print out which CSVs it is
        csvs.new[[4]][1, "csv.file.name"] #fy2001-q4
        csvs.new[[6]][1, "csv.file.name"] #fy2002-q2
        csvs.new[[18]][1, "csv.file.name"] #fy2005-q2
        csvs.new[[19]][1, "csv.file.name"] #fy2005-q3
        csvs.new[[20]][1, "csv.file.name"] #fy2005-q4   
        csvs.new[[26]][1, "csv.file.name"] #fy2007-q2 
        csvs.new[[27]][1, "csv.file.name"] #fy2007-q3 
        csvs.new[[28]][1, "csv.file.name"] #fy2007-q4 
        csvs.new[[29]][1, "csv.file.name"] #fy2008-q1 
        csvs.new[[30]][1, "csv.file.name"] #fy2008-q2 
        csvs.new[[31]][1, "csv.file.name"] #fy2008-q3 
        
#4.2: Reduce to just necessary columns and rows
        
    #A. Loop over all data frames
    csvs.4 = lapply(csvs.new, function(x) x %>% 
    
        ##1. Select certain columns
        dplyr::select(topline_service_category, service_category, csv.file.name,
                                    starts_with("revenue"), 
                                    starts_with("pieces")) %>%
        ##2. Only rows with totals
        filter(grepl("Total", service_category)) %>%
          
        ##3. Remove white space
        mutate_all(str_trim) %>%
          
        ##4. Change Marketing Mail B to MM
        mutate(service_category = gsub("\\(A)", "", service_category)) %>%
          
        ##5. Rename columns
        rename_at(vars(starts_with("revenue")), ~"revenue") %>%
        rename_at(vars(starts_with("pieces")), ~"pieces") %>%
          
        ##6. Split CSV File Name into Quarter and FY year
        mutate(fiscal_year = str_extract(csv.file.name, "\\d{4}")) %>%
        mutate(quarter = str_extract(csv.file.name, "q\\d"))   ) 
            
#------------------------------------------------------------------------#
#                           Step 5: Append Data                          #
#                         Years: FY 2001 - FY 2008                       #
#------------------------------------------------------------------------#

#5.1 Append data frames together
    
    #A. Append
    csvs.5 <- do.call(rbind, csvs.4)
    
    #B. Export
    write.csv(csvs.5, paste(new_data_path, paste0('5_', "fy2001-2008_totals.csv"), sep = "\\"), row.names = FALSE)    
    
#------------------------------------------------------------------------#
#                             Step 6: Clean Data                         #
#                         Years: FY 2008 - FY 2020                       #
#------------------------------------------------------------------------#

for (csv.file in file.name.2008.2020){
  
#6.1: Import
  print(csv.file)  
  
  df <- read.csv(paste(raw_data_path, csv.file, sep = '\\'), 
                 skip = 13, nrows = 40, na.strings = c(""), 
                 header = FALSE)
  
#6.2: Fix layout--Files after certain date are on column longer. Use condition. 
#     Length==25
  
  if (length(df) == 25) {
    print("    Length: 25")
  
      #A. Remove columns will all NAs
      all_na <- function(x) any(!is.na(x))
      df <- df %>% dplyr::select(where(all_na))
      
      #B. Rename columns
  
          ##1. Take heading from CSV 
          new.col.names <- read.csv(paste(raw_data_path, csv.file, sep = '\\'), 
                                    skip = 5, nrows = 6, header = FALSE)
          
          ##2. Remove extraneous rows
          new.col.names <- new.col.names %>% 
            filter(!grepl('=', V3)) %>%
            dplyr::select(where(all_na)) %>%
            replace(is.na(.), "")
          
          ##6. Collapse rows into one row
          new.col.names <- new.col.names %>%
            summarise(V1 = paste(V1, collapse = ""),
                      V3 = paste(V3, collapse = ""),
                      V5 = paste(V5, collapse = ""),
                      V7 = paste(V7, collapse = ""),
                      V9 = paste(V9, collapse = ""),
                      V11 = paste(V11, collapse = ""),
                      V13 = paste(V13, collapse = ""),
                      V15 = paste(V15, collapse = ""),
                      V17 = paste(V17, collapse = ""),
                      V19 = paste(V19, collapse = ""),
                      V21 = paste(V21, collapse = ""),
                      V23 = paste(V23, collapse = ""),
                      V25 = paste(V25, collapse = ""))
          
          ##4. Make row into column name
          new.col.names <- new.col.names %>%
            row_to_names(row_number = 1) %>%
            clean_names()
          
          ##5. Rename
          new.col.names <- colnames(new.col.names)
          colnames(df) <- new.col.names
  }  

#6.3: Fix layout--Files after certain date are on column longer. Use condition. 
#     Length==26  
  
  if (length(df) >= 26) {
    print("    Length> 26")
        
    #A. Remove columns will all NAs
    all_na <- function(x) any(!is.na(x))

        ##1. Change zeros to NA
        df[df == 0] <- NA   
        
        ##2. Remove
        df <- df %>% 
          dplyr::select(where(all_na))
   
    #B. Rename columns
    
        ##1. Take heading from CSV 
        new.col.names <- read.csv(paste(raw_data_path, csv.file, sep = '\\'), 
                                  skip = 5, nrows = 6, header = FALSE)
     
        ##2. Remove extraneous rows
        new.col.names <- new.col.names %>% 
          mutate_all(function (x) gsub("\\/", "", x)) %>%
          filter(!grepl('=', V4)) %>%
          dplyr::select(where(all_na)) %>%
          replace(is.na(.), "")
     
        ##3. Collapse rows into one row
        new.col.names <- new.col.names %>%
          summarise(V2 = paste(V2, collapse = ""),
                    V4 = paste(V4, collapse = ""),
                    V6 = paste(V6, collapse = ""),
                    V8 = paste(V8, collapse = ""),
                    V10 = paste(V10, collapse = ""),
                    V12 = paste(V12, collapse = ""),
                    V14 = paste(V14, collapse = ""),
                    V16 = paste(V16, collapse = ""),
                    V18 = paste(V18, collapse = ""),
                    V20 = paste(V20, collapse = ""),
                    V22 = paste(V22, collapse = ""),
                    V24 = paste(V24, collapse = ""),
                    V26 = paste(V26, collapse = ""))
         
        ##4. Make row into column name
        new.col.names <- new.col.names %>%
          row_to_names(row_number = 1) %>%
          clean_names()
      
        ##5. Rename
        new.col.names <- colnames(new.col.names)
        colnames(df) <- new.col.names
  }  
  
    #C. Remove blank rows
    df <- df %>% 
      remove_empty("rows")
  
#6.4: Add variables
  
    #A. Add variable indicating FCM, MM, etc. 
    df <- df %>%
      mutate(topline_service_category = if_else(endsWith(service_category, ":"), 
                                                service_category, "")) %>%
      mutate(topline_service_category = na_if(topline_service_category, "")) %>%
      fill(topline_service_category, .direction = "down") %>%
      relocate(topline_service_category, .before = service_category)
    
    #B. Remove first element of group now that topline_service_category contains info
    df <- df %>%
      group_by(topline_service_category) %>%
      slice(-1)
    
    #C. Add file name to dataframe 
    df$csv.file.name <- csv.file
  
#6.5: Export CSVs
  
    #A. Export
    write.csv(df, paste(new_data_path, paste0('6_', csv.file), sep = "\\"), row.names = FALSE)
    
}

#------------------------------------------------------------------------#
#                 Step 7: Narrow Data down to include just totals        #
#                         Years: FY 2008 - FY 2020                       #
#------------------------------------------------------------------------#  

#7.1: Identify rows with misplaced headers

    #A. Import data sets
    file6.2008.2020  <- dir(new_data_path, pattern="6_")
    csvs.new = lapply(paste(new_data_path, file6.2008.2020 , sep = '\\'), read.csv)  
    
    #B. See which data frames contain extra rows and delete manually
    lapply(csvs.new, function(x) any(grepl("REVENUE, PIECES, AND WEIGHT BY CLASSES OF MAIL", 
                                           x[, "service_category"])))

#7.2: Reduce to just necessary columns and rows
    
    #A. Loop over all data frames
    csvs.7 = lapply(csvs.new, function(x) x %>% 
        
        ##1. Select certain columns
        dplyr::select(topline_service_category, service_category, csv.file.name,
                      starts_with("revenue"), 
                      starts_with("pieces")) %>%
        ##2. Only rows with totals
        filter(grepl("Total", service_category)) %>%
        
        ##3. Remove white space
        mutate_all(str_trim) %>%
        
        ##4. Change Marketing Mail B to MM
        mutate(service_category = gsub("\\(A)", "", service_category)) %>%
        
        ##5. Rename columns
        rename_at(vars(starts_with("revenue")), ~"revenue") %>%
        rename_at(vars(starts_with("pieces")), ~"pieces") %>%
        
        ##6. Split CSV File Name into Quarter and FY year
        mutate(fiscal_year = str_extract(csv.file.name, "\\d{4}")) %>%
        mutate(quarter = str_extract(csv.file.name, "q\\d"))   ) 

#------------------------------------------------------------------------#
#                           Step 8: Append Data                          #
#                         Years: FY 2008 - FY 2020                       #
#------------------------------------------------------------------------#

#8.1 Append data frames together
    
    #A. Append
    csvs.8 <- do.call(rbind, csvs.7)
    
    #B. Export
    write.csv(csvs.8, paste(new_data_path, 
                            paste0('8_', "fy2008-2020_totals.csv"), 
                            sep = "\\"), row.names = FALSE)    

#------------------------------------------------------------------------#
#                           Step 9: Append Data                          #
#                         Years: FY 2001 - FY 2020                       #
#------------------------------------------------------------------------#

#9.1: Append both year sets (FY 2001 - 2008 Q3 and FY 2008 Q4 - FY 2020) together
        
    #A. Append
    csvs.9 <- rbind(csvs.5, csvs.8)

    #B. Export
    write.csv(csvs.9, paste(new_data_path, 
                            paste0('9_', "fy2001-2020_totals.csv"), 
                            sep = "\\"), row.names = FALSE)    
    
#------------------------------------------------------------------------#
#                           Step 10: Light Clean                         #
#                         Years: FY 2001 - FY 2020                       #
#------------------------------------------------------------------------#

#10.1: Check unique values
    
    #A. Service Category count all years
    csvs.9 %>% 
      group_by(service_category) %>%
      count()
    
    #B. Service Category count by year
    count10b <- csvs.9 %>% 
      group_by(service_category, fiscal_year) %>%
      count()
    
#10.2: New variables
    
    #A. Group Service Categories together
    csvs.10 <- csvs.9 %>%
      mutate(combined_service_category = case_when(
        service_category=="Total Package Services Mail" ~ "Total Package Services",
        service_category=="Total Periodical Mail" ~ "Total Periodicals Mail",
        service_category=="Total Presort Letters, Flats, & Parcels"  ~ "Total Presort Letters and Cards",
        service_category=="Total Presort Cards"  ~ "Total Presort Letters and Cards",
        service_category=="Total Standard Mail " ~ "Total USPS Marketing Mail",
        service_category=="Total Standard Mail" ~ "Total USPS Marketing Mail",
        service_category=="Total USPS Marketing Mail / Standard Mail" ~ "Total USPS Marketing Mail",
        TRUE ~ as.character(service_category)
      ))
    
    #B. Data Check
    
        ##1. Service Category count all years
        csvs.10 %>% 
          group_by(combined_service_category) %>%
          count()
        
        ##2. Service Category count by year
        count10.2b <- csvs.10 %>% 
          group_by(combined_service_category, fiscal_year) %>%
          count()  
        
    #C. Change order of column
    csvs.10 <- csvs.10 %>%
      relocate(combined_service_category, .after = service_category)
        
#10.3: Remove values
    
    #A. Negotiated Service Agreement
    csvs.10 <- subset(csvs.10, combined_service_category!="Total Negotiated Service Agreement Mail")  
    
    #B. Remove characters
    csvs.10 <- csvs.10 %>%
      mutate(quarter = gsub("\\q", "", quarter)) %>%
      mutate(revenue = gsub("\\,", "", revenue)) %>%
      mutate(pieces = gsub("\\,", "", pieces))
    
    #C. Change to numeric
    str(csvs.10) # quarter and fiscal year are characters
    csvs.10$quarter <- as.numeric(csvs.10$quarter)
    csvs.10$fiscal_year <- as.numeric(csvs.10$fiscal_year)
    csvs.10$revenue <- as.numeric(csvs.10$revenue)
    csvs.10$pieces <- as.numeric(csvs.10$pieces)

#10.4: Export 
    
    #A. Export
    write.csv(csvs.10, paste(new_data_path, 
                             paste0('10_', "Clean--fy2001-2020_totals.csv"), 
                             sep = "\\"), row.names = FALSE)    
    
    
    
    
    
    
        