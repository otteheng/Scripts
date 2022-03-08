# -> Gerhard O
# -> Data sets: CRA FY 2008 - FY 2018

# Load Packages in Library
source('C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Script\\_Package-Library.R')

# Raw Data
raw_data_path <- "S:\\Projects\\Mail Volume Project\\Code\\Economic Approach\\Raw Data\\CRA"
new_data_path <- "S:\\Projects\\Mail Volume Project\\Code\\Economic Approach\\Clean Data\\1b_CRA--Clean excel files_Volume"

#------------------------------------------------------------------------#
#                       Step 1:Review Weird Cases                        #
#                                                                        #
#------------------------------------------------------------------------#

raw_fy2008.v1 <- read_excel(paste(raw_data_path, "fy2008.xls", sep="\\"), 
                            sheet = "Volume1", skip = 7, col_names = T)

raw_fy2013.v1 <- read_excel(paste(raw_data_path, "fy2013.xls", sep="\\"), 
                            sheet = "Volume1", skip = 7, col_names = T)

raw_fy2018.v1 <- read_excel(paste(raw_data_path, "Public_FY18CRAReportRevFeb11.xlsx", sep="\\"), 
                               sheet = "Volume1", skip = 7, col_names = T)

#------------------------------------------------------------------------#
#                     Step 2: Grab directory of files                    #
#                                                                        #
#------------------------------------------------------------------------#

#2.1: File names
    
    #A. Grab all file names
    file.name.xls <- dir(raw_data_path, pattern=".xls")
    
#2.2: Check that excel only contains sheets "Volume1" and "Volume2"
    
    #A. Loop excel file list
    lapply(file.name.xls, function(x) excel_sheets(paste(raw_data_path, x, 
                                                         sep = "\\")))
    
#------------------------------------------------------------------------#
#                             Step 3: Clean Data                         #
#                         Years: FY 2008 - FY 2017                       #
#------------------------------------------------------------------------#

for (xlsx.file in file.name.xls){
  
#3.1: Excel Volume Sheets
  
  xlsx.sheets <- c("Volume1", "Volume2")
  
  for (sheets in xlsx.sheets){
    
#3.2: Fix layout
    print(xlsx.file)
    print(sheets)
    
    if (grepl("fy2", xlsx.file)) {
      df <- read_xls(paste(raw_data_path, xlsx.file, sep="\\"), 
                     sheet = sheets, skip = 7, col_names = F)
      new.col.names <- read_xls(paste(raw_data_path, xlsx.file, sep="\\"), 
                                sheet = sheets, skip = 4, n_max = 3,
                                col_names = F)
    } else {
      df <- read_xlsx(paste(raw_data_path, xlsx.file, sep="\\"), 
                       sheet = sheets, skip = 7, col_names = T)
      new.col.names <- read_excel(paste(raw_data_path, xlsx.file, sep="\\"), 
                                  sheet = sheets, skip = 4, n_max = 3,
                                  col_names = F)
    }
    
    #A. Remove columns with all NAs
    
        ##1. Convert zeros to NAs
        df[df == 0] <- NA
        df[df == "n/a"] <- NA
        
        ##2. Remove 
        all_na <- function(x) any(!is.na(x))
        df <- df %>% dplyr::select(where(all_na))
      
    #B. Rename columns
        
        ##1. Remove extraneous characters
        new.col.names <- new.col.names %>% 
          mutate_all(function (x) gsub("\\$", "", x)) %>%
          mutate_all(function (x) gsub("\\(note \\d)", "", x)) %>%
          mutate_all(function (x) gsub("\\(thousands)", "", x)) %>%
          mutate_all(function (x) gsub("\\(ounces)", "_ounces", x)) %>%
          mutate_all(str_trim) %>%
          dplyr::select(where(all_na)) %>%
          replace(is.na(.), "")
        
        ##2. Collapse rows into one row
        v.names <- c("V1", "V3", "V5", "V7")
        colnames(new.col.names) <- v.names
        new.col.names <- new.col.names %>%
          summarise(V1 = paste(V1, collapse = ""),
                    V3 = paste(V3, collapse = ""),
                    V5 = paste(V5, collapse = ""),
                    V7 = paste(V7, collapse = ""))
        
        ##3. Make row into column name
        new.col.names <- new.col.names %>%
          row_to_names(row_number = 1) %>%
          clean_names()
        
        ##4. Rename
        new.col.names <- colnames(new.col.names)
        colnames(df) <- new.col.names
    
    #C. Remove blank rows
    df <- df %>% 
      remove_empty("rows")
      
#3.3: Add variables
    
    #A. Add variable indicating FCM, MM, etc. 
    df <- df %>%
      mutate(topline_mail_class_and_products = if_else(endsWith(mail_classes_and_products, ":"), 
                                                       mail_classes_and_products, "")) %>%
      mutate(topline_mail_class_and_products = na_if(topline_mail_class_and_products, "")) %>%
      fill(topline_mail_class_and_products, .direction = "down") %>%
      relocate(topline_mail_class_and_products, .before = mail_classes_and_products) %>%
      mutate(topline_mail_class_and_products = gsub("\\:", "",topline_mail_class_and_products))
    
    #B. Remove first element of group now that topline_service_category contains info
    df <- df %>%
      group_by(topline_mail_class_and_products,) %>%
      slice(-1)
    
    #C. Add file name to dataframe 
    df$xlsx.file.name <- xlsx.file
      
#3.4: Convert numbers to thousands 
    
    #A. Correct Vector of Column Names
    correct.col.names <- c("topline_mail_class_and_products", 
                           "mail_classes_and_products","pieces", "weight_in_pounds", 
                           "weight_per_piece_ounces", "xlsx.file.name") 

    #B. Make change to columns if needed
    if (!all(correct.col.names==colnames(df))) {
      print("    Error: Col Names Incorrect")
        colnames(df) <- correct.col.names  
    }
      
    #C. Convert volume 
      
    df <- df %>%
      
        ##1. Pieces
        mutate(pieces = pieces*1000) %>%
        
        ##2. Weight in pounds
        mutate(weight_in_pounds = as.numeric(weight_in_pounds)*1000) %>%
      
        ##2. Weight in pounds
        mutate(weight_per_piece_ounces = as.numeric(weight_per_piece_ounces)*1000)
    
#3.6: Export CSVs
    
    #A. Export as CSV
    if (grepl("fy2", xlsx.file)) {
      csv.file <- gsub(".xls", ".csv", xlsx.file)
      csv.file <- paste(sheets, csv.file, sep = "--")
    } else {
      csv.file <- gsub(".xlsx", ".csv", xlsx.file)
      csv.file <- paste(sheets, csv.file, sep = "--")
    }
    
    #B. Export
    write.csv(df, paste(new_data_path, paste0('3_', csv.file), sep = "\\"), row.names = FALSE)
    csv.file <- ""
  } 
}
    
#------------------------------------------------------------------------#
#                 Step 4: Narrow Data down to include just totals        #
#                         Years: FY 2008 - FY 2017                       #
#------------------------------------------------------------------------#  

#4.1: Identify rows with misplaced headers

    #A. Import Volume1 data sets
    file.vol1  <- dir(new_data_path, pattern="Volume1")
    csvs.3.vol1 = lapply(paste(new_data_path, file.vol1 , sep = '\\'), read.csv) 
    
    #B. Import Volume2 data sets
    file.vol2  <- dir(new_data_path, pattern="Volume2")
    csvs.3.vol2 = lapply(paste(new_data_path, file.vol2 , sep = '\\'), read.csv)     


#4.2: Volume1: Reduce to just necessary columns and rows

    #A. Loop over all data frames
    csvs.4.vol1.totals = lapply(csvs.3.vol1, function(x) x %>% 
        
        ##1. Only rows with totals
        filter(grepl("Total", mail_classes_and_products)) %>%
        
        ##2. Remove "(note )" from category
        mutate(mail_classes_and_products = 
                gsub("\\(note \\d)", "", mail_classes_and_products)) %>%  
        
        ##3. Remove white space
        mutate_all(str_trim) %>%
          
        ##4. Add Volume Variable
        mutate(excel_sheet = "Volume1") %>%
        
        ##5. Split CSV File Name into FY year
        mutate(fiscal_year = str_extract(xlsx.file.name, "\\d{4}")) %>%
        mutate(fiscal_year = case_when(is.na(fiscal_year) & 
                                        xlsx.file.name=="Public_FY17CRAReport.xlsx" ~ 2017,
                                      is.na(fiscal_year) & 
                                        xlsx.file.name=="Public_FY18CRAReportRevFeb11.xlsx" ~ 2018,
                                      TRUE ~ as.double(fiscal_year))) %>%
        
        ##6. Rename mail classes so they match over time 
        mutate(mail_classes_and_products = case_when(
          mail_classes_and_products=="Total Domestic Market Dominant Mail" ~ "Total Market Dominant Mail",
          mail_classes_and_products=="Total Standard Mail" ~ "Total USPS Marketing Mail",
          TRUE ~ as.character(mail_classes_and_products)
        ))  )   
    
#4.2: Volume1: Reduce to just necessary columns and rows

    #A. Loop over all data frames
    csvs.4.vol2.totals = lapply(csvs.3.vol2, function(x) x %>% 
    
        ##1. Only rows with totals
        filter(grepl("Total", mail_classes_and_products)) %>%
        
        ##2. Remove "(note )" from category
        mutate(mail_classes_and_products = 
                 gsub("\\(note \\d)", "", mail_classes_and_products)) %>%  
        
        ##3. Remove white space
        mutate_all(str_trim) %>%
          
        ##4. Add Volume Variable
        mutate(excel_sheet = "Volume2") %>%
        
        ##5. Split CSV File Name into FY year
        mutate(fiscal_year = str_extract(xlsx.file.name, "\\d{4}")) %>%
        mutate(fiscal_year = case_when(is.na(fiscal_year) & 
                                         xlsx.file.name=="Public_FY17CRAReport.xlsx" ~ 2017,
                                       is.na(fiscal_year) & 
                                         xlsx.file.name=="Public_FY18CRAReportRevFeb11.xlsx" ~ 2018,
                                       TRUE ~ as.double(fiscal_year))) %>%  
        
        ##6. Rename mail classes so they match over time 
        mutate(mail_classes_and_products = case_when(
          mail_classes_and_products=="Total Domestic Market Dominant Mail" ~ "Total Market Dominant Mail",
          mail_classes_and_products=="Total Standard Mail" ~ "Total USPS Marketing Mail",
          mail_classes_and_products=="Total Market Dominant Mail (no serivces)" ~ "Total Market Dominant Mail (no services)",
          mail_classes_and_products=="Total All Mail (not including special services)" ~ "Total All Mail (no services)",
          mail_classes_and_products=="Total Standard Mail" ~ "Total USPS Marketing Mail",
          TRUE ~ as.character(mail_classes_and_products)
        ))   )     
    
#------------------------------------------------------------------------#
#                           Step 5: Append Data                          #
#                         Years: FY 2008 - FY 2018                       #
#------------------------------------------------------------------------#

#5.1 Append .Volume1 data frames together

    #A. Append (bind_rows allows for non-matching columns)
    csvs.5.vol1.totals <- do.call(bind_rows, csvs.4.vol1.totals)
    
    #B. Export
    write.csv(csvs.5.vol1.totals, paste(new_data_path, paste0('5_', "Volume1--fy2008-2018_totals.csv"), 
                                         sep = "\\"), row.names = FALSE)    

#5.2 Append .Volume2 data frames together

    #A. Append (bind_rows allows for non-matching columns)
    csvs.5.vol2.totals <- do.call(bind_rows, csvs.4.vol2.totals)
    
    #B. Export
    write.csv(csvs.5.vol2.totals, paste(new_data_path, paste0('5_', "Volume2--fy2008-2018_totals.csv"), 
                                         sep = "\\"), row.names = FALSE)  
    
#------------------------------------------------------------------------#
#                     Step 6: Append Data (Both Volume Sheets)           #
#                         Years: FY 2008 - FY 2018                       #
#------------------------------------------------------------------------#

#6.1 Append Volume data frames together
    
    #A. Append (bind_rows allows for non-matching columns)
    csvs.6.vol.totals <- bind_rows(csvs.5.vol1.totals, csvs.5.vol2.totals)
    
        ##1. Order columns
        csvs.6.vol.totals <- csvs.6.vol.totals %>%
          arrange(excel_sheet, 
                  mail_classes_and_products, fiscal_year, .by_group = T)
    
    #B. Export
    write.csv(csvs.6.vol.totals, paste(new_data_path, paste0('6_', "Volume All--fy2008-2018_totals.csv"), 
                                        sep = "\\"), row.names = FALSE) 
    
    
    
    
    