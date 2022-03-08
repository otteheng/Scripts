# -> Gerhard O
# -> Data sets: CRA FY2008 - FY 2018

# Load Packages in Library
source('C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Script\\_Package-Library.R')

# Raw Data
raw_data_path <- "S:\\Projects\\Mail Volume Project\\Code\\Economic Approach\\Raw Data\\CRA"
new_data_path <- "S:\\Projects\\Mail Volume Project\\Code\\Economic Approach\\Clean Data\\1a_CRA--Clean excel files_Cost"

#------------------------------------------------------------------------#
#                       Step 1:Review Weird Cases                        #
#                                                                        #
#------------------------------------------------------------------------#

raw_fy2018.cost1 <- read_excel(paste(raw_data_path, "Public_FY18CRAReportRevFeb11.xlsx", sep="\\"), 
                             sheet = "Cost1", skip = 7, col_names = T)

#------------------------------------------------------------------------#
#                     Step 2: Grab directory of files                    #
#                                                                        #
#------------------------------------------------------------------------#

#2.1: File names

    #A. Grab all xlsx file names
    file.name.xlsx <- dir(raw_data_path, pattern="Public_")
    
    #B. Grab all file names
    file.name.xls <- dir(raw_data_path, pattern=".xls")
    
#2.2: Excel Sheet names
    
    #A. Costing
    xlsx.sheets <- c("Cost1", "Cost2", "Cost3")

#------------------------------------------------------------------------#
#                             Step 3: Clean Data                         #
#                         Years: FY 2006 - FY 2017                       #
#------------------------------------------------------------------------#

for (xlsx.file in "fy2006.xls"){

#3.1: Define which excel files contain Cost1-3 or just Cost1-2
  
  sheet.list <- excel_sheets(paste(raw_data_path, xlsx.file, sep="\\"))
  
  if ("Cost3" %in% sheet.list) {
    xlsx.sheets <- c("Cost1", "Cost2", "Cost3")
  }
  if (!"Cost3" %in% sheet.list) {
    xlsx.sheets <- c("Cost1", "Cost2")
  }
  if (!"Cost1" %in% sheet.list) {
    xlsx.sheets <- c("Cost1")
  }

  for (sheets in xlsx.sheets){
    
#3.2: Fix layout
    print(xlsx.file)
    print(sheets)
    
    if (grepl("fy2", xlsx.file)) {
      df <- read_xls(paste(raw_data_path, xlsx.file, sep="\\"), 
                     sheet = sheets, skip = 9, col_names = F)
      new.col.names <- read_xls(paste(raw_data_path, xlsx.file, sep="\\"), 
                                  sheet = sheets, skip = 3, n_max = 3,
                                  col_names = F)
    } else {
      df <- read_excel(paste(raw_data_path, xlsx.file, sep="\\"), 
                       sheet = sheets, skip = 7, col_names = T)
      new.col.names <- read_excel(paste(raw_data_path, xlsx.file, sep="\\"), 
                                  sheet = sheets, skip = 3, n_max = 3,
                                  col_names = F)
    }
  
  
    #A. Remove columns with all NAs
    
        ##1. Convert zeros to NAs
        df[df == 0] <- NA
        
        ##2. Remove 
        all_na <- function(x) any(!is.na(x))
        df <- df %>% dplyr::select(where(all_na))
    
    #B. Rename columns
    
        ##1. Fix error in some headers
        if (grepl("Contribution", new.col.names[1,14])) {
          new.col.names[1,14] <- NA
          new.col.names[1,15] <- "Contributions"
        }
        if (grepl("Contribution", new.col.names[2,14])) {
          new.col.names[2,14] <- NA
          new.col.names[1,15] <- "Contributions"
        }
        if (grepl("Contribution", new.col.names[3,14])) {
          new.col.names[3,14] <- NA
          new.col.names[1,15] <- "Contributions"
        }
        if (grepl("MARKET DOMINANT MAIL", new.col.names[3,1])) {
          new.col.names[1,15] <- ""
        }
        
        ##2. Remove extraneous characters
        new.col.names <- new.col.names %>% 
          mutate_all(function (x) gsub("\\$", "", x)) %>%
          mutate_all(function (x) gsub("\\(note \\d)", "", x)) %>%
          mutate_all(function (x) gsub("\\(in millions)", "", x)) %>%
          mutate_all(function (x) gsub("\\(per piece)", "", x)) %>%
          mutate_all(str_trim) %>%
          dplyr::select(where(all_na)) %>%
          replace(is.na(.), "")
        
        ##3. Collapse rows into one row
        v.names <- c("V1", "V3", "V5", "V7", "V9", "V11", "V13", "V15", "V17")
        colnames(new.col.names) <- v.names
        new.col.names <- new.col.names %>%
          summarise(V1 = paste(V1, collapse = ""),
                    V3 = paste(V3, collapse = ""),
                    V5 = paste(V5, collapse = ""),
                    V7 = paste(V7, collapse = ""),
                    V9 = paste(V9, collapse = ""),
                    V11 = paste(V11, collapse = ""),
                    V13 = paste(V13, collapse = ""),
                    V15 = paste(V15, collapse = ""),
                    V17 = paste(V17, collapse = ""))
  
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
}}
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
      
#3.4: Convert numbers to millions if attributable cost column present
    
    #A. Vector of column names
    col.names.check <- colnames(df)
    
    if ("attributable_cost" %in% col.names.check) {
      
      df <- df %>%
        #B. Revenue
        mutate(revenue = revenue*1000000) %>%
        
        #C. Attributable Cost
        mutate(attributable_cost = attributable_cost*1000000) %>%
        
        #D. Variable Cost
        mutate(volume_variable_cost = volume_variable_cost*1000000)
    }
    
#3.5: Convert numbers to millions if attributable cost column is not in data frame (2016)
    
    else if (!"attributable_cost" %in% col.names.check) {
      
      df <- df %>%
        
        #A. Rename
        rename(attributable_cost = vol_var_prod_spec, 
               attributable_cost_2 = vol_var_prod_spec_2) %>%
        
        #B. Revenue
        mutate(revenue = revenue*1000000) %>%
        
        #C. Attributable Cost
        mutate(attributable_cost = attributable_cost*1000000) %>%
        
        #D. Variable Cost
        mutate(volume_variable_cost = volume_variable_cost*1000000)
    }
    
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

#4.1: Import Data sets into a list

    #A. Import Cost1 data sets
    file.cost1  <- dir(new_data_path, pattern="Cost1")
    csvs.3.cost1 = lapply(paste(new_data_path, file.cost1 , sep = '\\'), read.csv) 
    
    #B. Import Cost2 data sets
    file.cost2  <- dir(new_data_path, pattern="Cost2")
    csvs.3.cost2 = lapply(paste(new_data_path, file.cost2 , sep = '\\'), read.csv)  
    
    #C. Import Cost3 data sets
    file.cost3  <- dir(new_data_path, pattern="Cost3")
    csvs.3.cost3 = lapply(paste(new_data_path, file.cost3 , sep = '\\'), read.csv)  

#4.2: Cost1: Reduce to just necessary columns and rows

    #A. Loop over all data frames
    csvs.4.cost1.totals = lapply(csvs.3.cost1, function(x) x %>% 
                      
        ##1. Select certain columns
        dplyr::select(topline_mail_class_and_products, mail_classes_and_products, 
                      xlsx.file.name, starts_with("revenue"), 
                      starts_with("attributable_cost"),
                      starts_with("volume_variable_cost"), 
                      starts_with("contribution"),
                      cost_coverage) %>%
          
        ##2. Only rows with totals
        filter(grepl("Total", mail_classes_and_products)) %>%
          
        ##3. Remove "(note )" from category
        mutate(mail_classes_and_products = 
                 gsub("\\(note \\d)", "", mail_classes_and_products)) %>% 
          
        ##4. Add Cost Variable
        mutate(excel_sheet = "Cost1") %>%
        
        ##5. Remove white space
        mutate_all(str_trim) %>%
        
        ##6. Rename columns
        rename_at(vars(ends_with("_2")), ~ paste0(., "_per.piece")) %>%
        rename_at(vars(starts_with("contribution")), ~"contribution") %>%
        
        ##7. Split CSV File Name into FY year
        mutate(fiscal_year = str_extract(xlsx.file.name, "\\d{4}")) %>%
        mutate(fiscal_year = case_when(is.na(fiscal_year) & 
               xlsx.file.name=="Public_FY17CRAReport.xlsx" ~ 2017,
               is.na(fiscal_year) & 
               xlsx.file.name=="Public_FY18CRAReportRevFeb11.xlsx" ~ 2018,
               TRUE ~ as.double(fiscal_year)))  ) 
    
#4.3: Cost2: Reduce to just necessary columns and rows

    #A. Loop over all data frames
    csvs.4.cost2.totals = lapply(csvs.3.cost2, function(x) x %>% 
     
        ##1. Select certain columns
        dplyr::select(topline_mail_class_and_products, mail_classes_and_products, 
                     xlsx.file.name, starts_with("revenue"), 
                     starts_with("attributable_cost"),
                     starts_with("volume_variable_cost"), 
                     starts_with("contribution"),
                     cost_coverage) %>%
        
        ##2. Only rows with totals
        filter(grepl("Total", mail_classes_and_products)) %>%
          
        ##3. Remove "(note )" from category
        mutate(mail_classes_and_products = 
                 gsub("\\(note \\d)", "", mail_classes_and_products)) %>% 
          
        ##4. Add Cost Variable
        mutate(excel_sheet = "Cost2") %>%
        
        ##5. Remove white space
        mutate_all(str_trim) %>%
        
        ##6. Rename columns
        rename_at(vars(ends_with("_2")), ~ paste0(., "_per.piece")) %>%
        rename_at(vars(starts_with("contribution")), ~"contribution") %>%
        
        ##7. Split CSV File Name into Quarter and FY year
        mutate(fiscal_year = str_extract(xlsx.file.name, "\\d{4}")) %>%
        mutate(fiscal_year = case_when(is.na(fiscal_year) & 
                                        xlsx.file.name=="Public_FY17CRAReport.xlsx" ~ 2017,
                                      is.na(fiscal_year) & 
                                        xlsx.file.name=="Public_FY18CRAReportRevFeb11.xlsx" ~ 2018,
                                      TRUE ~ as.double(fiscal_year)))  ) 
    
#4.3: Cost3: Reduce to just necessary columns and rows

    #A. Loop over all data frames
    csvs.4.cost3.totals = lapply(csvs.3.cost3, function(x) x %>% 
    
        ##1. Select certain columns
        dplyr::select(topline_mail_class_and_products, mail_classes_and_products, 
                     xlsx.file.name, starts_with("revenue"), 
                     starts_with("attributable_cost"),
                     starts_with("volume_variable_cost"), 
                     starts_with("contribution"),
                     cost_coverage) %>%
        
        ##2. Only rows with totals
        filter(grepl("Total", mail_classes_and_products)) %>%
        
        ##3. Remove "(note )" from category
        mutate(mail_classes_and_products = 
                gsub("\\(note \\d)", "", mail_classes_and_products)) %>%
          
        ##4. Add Cost Variable
        mutate(excel_sheet = "Cost3") %>%
        
        ##5. Remove white space
        mutate_all(str_trim) %>%
        
        ##6. Rename columns
        rename_at(vars(ends_with("_2")), ~ paste0(., "_per.piece")) %>%
        rename_at(vars(starts_with("contribution")), ~"contribution") %>%
        
        ##7. Split CSV File Name into Quarter and FY year
        mutate(fiscal_year = str_extract(xlsx.file.name, "\\d{4}")) %>%
        mutate(fiscal_year = case_when(is.na(fiscal_year) & 
                                        xlsx.file.name=="Public_FY17CRAReport.xlsx" ~ 2017,
                                      is.na(fiscal_year) & 
                                        xlsx.file.name=="Public_FY18CRAReportRevFeb11.xlsx" ~ 2018,
                                      TRUE ~ as.double(fiscal_year)))  )     

#------------------------------------------------------------------------#
#                           Step 5: Append Data                          #
#                         Years: FY 2008 - FY 2018                       #
#------------------------------------------------------------------------#

#5.1 Append Cost1 data frames together

    #A. Append (bind_rows allows for non-matching columns)
    csvs.5.cost1.totals <- do.call(bind_rows, csvs.4.cost1.totals)
    
    #B. Export
    write.csv(csvs.5.cost1.totals, paste(new_data_path, paste0('5_', "Cost1--fy2008-2018_totals.csv"), 
                            sep = "\\"), row.names = FALSE)    

#5.2 Append Cost2 data frames together
    
    #A. Append (bind_rows allows for non-matching columns)
    csvs.5.cost2.totals <- do.call(bind_rows, csvs.4.cost2.totals)
    
    #B. Export
    write.csv(csvs.5.cost2.totals, paste(new_data_path, paste0('5_', "Cost2--fy2008-2018_totals.csv"), 
                                         sep = "\\"), row.names = FALSE)   

#5.2 Append Cost3 data frames together
    
    #A. Append (bind_rows allows for non-matching columns)
    csvs.5.cost3.totals <- do.call(bind_rows, csvs.4.cost3.totals)
    
    #B. Export
    write.csv(csvs.5.cost3.totals, paste(new_data_path, paste0('5_', "Cost3--fy2008-2018_totals.csv"), 
                                         sep = "\\"), row.names = FALSE)   
    
#------------------------------------------------------------------------#
#                     Step 6: Append Data (All Cost Sheets)              #
#                         Years: FY 2008 - FY 2018                       #
#------------------------------------------------------------------------#

#6.1 Append Volume data frames together

    #A. Append (bind_rows allows for non-matching columns)
    csvs.6.cost.totals <- bind_rows(csvs.5.cost1.totals, csvs.5.cost2.totals,
                                   csvs.5.cost3.totals)

    #B. Change variables
    csvs.6.cost.totals <- csvs.6.cost.totals %>%
          
        ##2. Rename mail classes so they match over time 
        mutate(mail_classes_and_products = case_when(
          mail_classes_and_products=="Total Domestic Market Dominant Mail" ~ "Total Market Dominant Mail",
          mail_classes_and_products=="Total Standard Mail" ~ "Total USPS Marketing Mail",
          mail_classes_and_products=="Total Market Dominant Mail (no serivces)" ~ "Total Market Dominant Mail (no services)",
          mail_classes_and_products=="Total All Mail (not including special services)" ~ "Total All Mail (no services)",
          mail_classes_and_products=="Total Standard Mail" ~ "Total USPS Marketing Mail",
          TRUE ~ as.character(mail_classes_and_products)
        )) %>%
      
        ##2. Order Columns
          arrange(excel_sheet, 
                  mail_classes_and_products, fiscal_year, .by_group = T) %>%
          relocate(xlsx.file.name, .after = fiscal_year) %>%
          relocate(fiscal_year, .after = mail_classes_and_products) %>%
      
        ##3. Remove duplicate rows in terms of all columns
        distinct()
    
    #C. Export
    write.csv(csvs.6.cost.totals, paste(new_data_path, paste0('6_', "Cost All--fy2008-2018_totals.csv"), 
                                       sep = "\\"), row.names = FALSE) 
    
    