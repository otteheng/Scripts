# -> Gerhard O
# -> 9/22/2021
# -> Data sets: HDS
# -> Clean data in order to get billing info
# Load Packages in Library
source('C:\\Users\\bg4cn0\\USPS\\ARTSI Team Folder - Documents\\Projects\\library_GSO.R')

# Data Paths
raw_data_path <- "C:\\Users\\bg4cn0\\USPS\\ARTSI Team Folder - Documents\\Projects\\Millennials & Mail\\Code\\Clean Data\\HDS"
graph_path <- "C:\\Users\\bg4cn0\\USPS\\ARTSI Team Folder - Documents\\Projects\\Mail Floor\\Code\\Tables & Figures\\1a"
clean_data_path <- "C:\\Users\\bg4cn0\\USPS\\ARTSI Team Folder - Documents\\Projects\\Mail Floor\\Code\\Clean Data\\2a"

#------------------------------------------------------------------------#
#                           Step 1: Load in Data                         #
#                                                                        #
#------------------------------------------------------------------------#

#1.1: Pull in partially cleaned data from "Millennials and the Mail" project. 

    #A. HDS Received
    hds_vol_raw <- read.csv(paste(raw_data_path, "2c_HDS--Merge 2b files\\2c_mail volume.csv",
                              sep = "\\"))
    
    #B. HDS Adult Roster (All questions asked in survey)
    rects_raw <- fread(paste(raw_data_path, "2a_HDS\\2a_rects_final.csv", sep = '\\'))
    
        ##1. Column Names as data frame
        rects_col <- as.data.frame(colnames(rects_raw))
        
    #C. HDS Adult Roster (Selection of billing questions)
    rects_bills_raw <- fread(paste(raw_data_path, "2a_HDS\\2a_rects_final.csv", sep = '\\'), 
                           select = c(1, 27:29, 137, 141:147, 155:166,
                                      179:211, 303:304, 316:333,
                                      247:248, 265:267))
        ##1. Column Names as data frame
        rects_bills_raw_col <- as.data.frame(colnames(rects_bills_raw))
        
    #D. HDS Adult Roster (Selection of Holiday and correspondence questions)
    rects_hol_raw <- fread(paste(raw_data_path, "2a_HDS\\2a_rects_final.csv", sep = '\\'), 
                           select = c(1, 27:29, 247:248, 265:267, 405:409))
        
#------------------------------------------------------------------------#
#                       Step 2: Merge with Volume data                   #
#                                                                        #
#------------------------------------------------------------------------#
        
#2.1: Clean for merge
        
    #A. Make columns unique
    rects_bills <- rects_bills_raw
    names(rects_bills) <- make.names(toupper(names(rects_bills)), unique = TRUE)
    
        ##1. Colnames
        rects_bills_col <- as.data.frame(colnames(rects_bills))
        
    #B. Make columns unique
    rects_hol <- rects_hol_raw
    names(rects_hol) <- make.names(toupper(names(rects_hol)), unique = TRUE)

#2.2: Merge
        
  #A. Merge billing data on survey year and sample number
  hds <- left_join(hds_vol_raw, rects_bills, 
                   by = c("SAMPLE.NUMBER", "SURVEY.YEAR"))  
  
  #B. Merge holiday data on survey and sample number
  hds.hol <- left_join(hds_vol_raw, rects_hol, 
                       by = c("SAMPLE.NUMBER", "SURVEY.YEAR"))
        
    
#------------------------------------------------------------------------#
#                            Step 3: Explore Data                        #
#                                                                        #
#------------------------------------------------------------------------#    
  
#3.1: Load in functions
    
    #A. Statistical Mode
    source("C:\\Users\\bg4cn0\\USPS\\ARTSI Team Folder - Documents\\Projects\\_Scripts\\Statistical Mode.R")
    
    #B. Merge function with Stata _merge option
    source("C:\\Users\\bg4cn0\\USPS\\ARTSI Team Folder - Documents\\Projects\\_Scripts\\full_join_track.R")
    
#3.2: Look at number of bills received
    
    #A. Most common answer by year
    
        ##1. Number of bills received
        hds %>% 
          dplyr::select(SURVEY.YEAR, Q27..NUMBER.OF.BILLS.RECEIVED.IN.THE.MAIL, 
                        BILLRECDMAIL..NUMBER.OF.BILLS.RECEIVED.VIA.MAIL,
                        X.INT28..HOW.MANY.BILLS.DOES.YOUR.HOUSEHOLD.RECEIVE.ON.LINE.AT.A.WEBSITE.OR.THOUGH.E.MAIL..EACH.MONTH..RANGE..0...80..99,
                        Q27A..DO.YOU.RECEIVE.ANY.BILLS.IN.THE.MAIL.) %>% 
          group_by(SURVEY.YEAR) %>% 
          summarise(m = Mode(Q27..NUMBER.OF.BILLS.RECEIVED.IN.THE.MAIL),
                    m2 = Mode(BILLRECDMAIL..NUMBER.OF.BILLS.RECEIVED.VIA.MAIL),
                    m3 = Mode(X.INT28..HOW.MANY.BILLS.DOES.YOUR.HOUSEHOLD.RECEIVE.ON.LINE.AT.A.WEBSITE.OR.THOUGH.E.MAIL..EACH.MONTH..RANGE..0...80..99),
                    m4 = Mode(Q27A..DO.YOU.RECEIVE.ANY.BILLS.IN.THE.MAIL.))
          # Q27 most common response not missing 2015-2019
          # BILLRECDMAIL most common response not missing 2007-2009
          # X.INT28 all NAs
          
        ##2. Q27AA. What are the main reasons you receive bills through the mail 
        #    instead of receiving them by Internet?
        hds %>% 
          dplyr::select(SURVEY.YEAR, Q27AA..WHAT.ARE.THE.MAIN.REASONS.YOU.RECEIVE.BILLS.THROUGH.THE.MAIL.INSTEAD.OF.RECEIVING.THEM.BY.INTERNET.) %>% 
          group_by(SURVEY.YEAR) %>% 
          summarise(m = Mode(Q27AA..WHAT.ARE.THE.MAIN.REASONS.YOU.RECEIVE.BILLS.THROUGH.THE.MAIL.INSTEAD.OF.RECEIVING.THEM.BY.INTERNET.))
          # Only contains responses from 2016-2019
    
    #B. Cross tab of year and billing questions
    yr.bill <- as.data.frame(table(hds$SURVEY.YEAR, hds$Q27..NUMBER.OF.BILLS.RECEIVED.IN.THE.MAIL))
    yr.bill2 <- as.data.frame(table(hds$SURVEY.YEAR, hds$BILLRECDMAIL..NUMBER.OF.BILLS.RECEIVED.VIA.MAIL))
    yr.bill3 <- as.data.frame(table(hds$SURVEY.YEAR, hds$Q27A..DO.YOU.RECEIVE.ANY.BILLS.IN.THE.MAIL.))
    
#3.3: Number of bills paid
    
    #A. Most common answer by year
    hds %>% 
      dplyr::select(SURVEY.YEAR, Q29..NUMBER.OF.BILLS.PAID.BY.HOUSEHOLD.PER.MONTH,
                    Q31A..NUMBER.OF.BILLS.PAID.BY.HOUSEHOLD.PER.MONTH.BY.MAIL) %>% 
      group_by(SURVEY.YEAR) %>% 
      summarise(m = Mode(Q29..NUMBER.OF.BILLS.PAID.BY.HOUSEHOLD.PER.MONTH),
                m2 = Mode(Q31A..NUMBER.OF.BILLS.PAID.BY.HOUSEHOLD.PER.MONTH.BY.MAIL))
      # Q29 most common response not missing 2000-2019 (Most common response is 10 in all years)
    
    #B. Cross tab of year and billing questions
    yr.bill.paid <- as.data.frame(table(hds$SURVEY.YEAR, hds$Q29..NUMBER.OF.BILLS.PAID.BY.HOUSEHOLD.PER.MONTH))
    yr.bill.paid2 <- as.data.frame(table(hds$SURVEY.YEAR, hds$Q31A..NUMBER.OF.BILLS.PAID.BY.HOUSEHOLD.PER.MONTH.BY.MAIL))
        
#3.4: Holiday cards sent
    
    #A. Most common answer by year
    hds.hol %>% 
      dplyr::select(SURVEY.YEAR, Q4..TOTAL.PERSONAL.LETTERS.HOUSEHOLD.WRITES.PER.MONTH,
                    Q5A..TOTAL.HOLIDAY.CARDS.SENT.LAST.YEAR,
                    Q5B..TOTAL.HOLIDAY.CARDS.RECEIVED.LAST.YEAR,
                    Q6A..TOTAL.NON.HOLIDAY.CARDS.SENT.IN.AVERAGE.MONTH,
                    Q6B..TOTAL.NON.HOLIDAY.CARDS.RECEIVED.IN.AVERAGE.MONTH) %>% 
      group_by(SURVEY.YEAR) %>% 
      summarise(q4 = Mode(Q4..TOTAL.PERSONAL.LETTERS.HOUSEHOLD.WRITES.PER.MONTH),
                q5a = Mode(Q5A..TOTAL.HOLIDAY.CARDS.SENT.LAST.YEAR),
                q5b = Mode(Q5B..TOTAL.HOLIDAY.CARDS.RECEIVED.LAST.YEAR),
                q6a = Mode(Q6A..TOTAL.NON.HOLIDAY.CARDS.SENT.IN.AVERAGE.MONTH),
                q6b = Mode(Q6B..TOTAL.NON.HOLIDAY.CARDS.RECEIVED.IN.AVERAGE.MONTH))
    
    #B. Cross tab of year and holiday questions
    yr.hol.q4 <- as.data.frame(table(hds$SURVEY.YEAR, hds.hol$Q4..TOTAL.PERSONAL.LETTERS.HOUSEHOLD.WRITES.PER.MONTH))
    
#------------------------------------------------------------------------#
#                         Step 4: Clean Variables                        #
#                               Billing Data                             #
#------------------------------------------------------------------------#      

#4.1: Drop variables that we don't need    
    
    #A. Drop
    hds <- hds %>%
      dplyr::select(-INCOM_OLD, -O_Q27AA, -CENSUS.REGION,
                    -Q28..NUMBER.OF.BILLS.RECEIVED.ON.LINE.AT.A.SITE.OR.VIA.E.MAIL,
                    -X.INT28..HOW.MANY.BILLS.DOES.YOUR.HOUSEHOLD.RECEIVE.ON.LINE.AT.A.WEBSITE.OR.THOUGH.E.MAIL..EACH.MONTH..RANGE..0...80..99,
                    -X.O_Q28D..WHAT.ARE.THE.REASONS.YOU.RECEIVE.BILLS.ONLINE..OTHER.SPECIFY,
                    -X.INT28B..FOR.HOW.MANY.OF.THESE.BILLS.AND.ACCOUNTS.DO.YOU.ALSO.RECEIVE.PAPER.STATEMENTS..RANGE..0...80..99)
        
#4.2: Label Non-billing variables
    
    #A. Respondent: Age Cohort
    hds$RESPONDENT..AGE.COHORT = 
      factor(hds$RESPONDENT..AGE.COHORT,
             levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 38, 98, 99),
             labels = c("DK/RF", "18-24", "18-24", "25-34", "35-44", 
                        "45-54", "55-64", "65-69", "70-74", "75+", 
                        "70+", "DK", "RF"), ordered = TRUE)
    
    #B. Respondent: Gender           
    hds$RESPONDENT..GENDER = 
      factor(hds$RESPONDENT..GENDER, 
             levels = c(-5, 0, 1, 2, 8, 9),
             labels = c("QUESTION NOT IN SURVEY", "?", "Male", "Female", "DF", 
                        "RF"))
    
    #C. Respondent: Marital Status
    hds$RESPONDENT..MARITAL.STATUS = 
      factor(hds$RESPONDENT..MARITAL.STATUS,
             levels = c(-1, 1, 2, 3, 4, 5, 6, 8, 9),
             labels = c("DK/RF", "Married", "Living as married", 
                        "Single, never been married", "Divorced", "Separated",
                        "Widowed", "DK", "RF"))
    
    
    #D. Income Level
    hds$ANNUAL.HOUSEHOLD.INCOME.LEVEL =
      factor(hds$ANNUAL.HOUSEHOLD.INCOME.LEVEL,
             levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 31, 98, 99),
             labels = c("Under $7,000", "$7,000 - $9,999", "$10,000 - $14,999", 
                        "$15,000 - $19,999","$20,000 - $24,999", 
                        "$25,000 - $34,999", "$35,000 - $49,999",
                        "$50,000 - $64,999", "$65,000 - $79,999", 
                        "$80,000 - $99,999", "$100,000 - $119,999", 
                        "$120,000 - $149,999", "$150,000 or more", 
                        "$100,000 or more", "DK", "RF"), ordered = TRUE)
            
        ##1. Recode 100,000+ incomes
        hds <- hds %>% 
          mutate(ANNUAL.HOUSEHOLD.INCOME.LEVEL = dplyr::recode(ANNUAL.HOUSEHOLD.INCOME.LEVEL,
                                    "$100,000 - $119,999" = "$100,000 or more",
                                    "$120,000 - $149,999" = "$100,000 or more",
                                    "$150,000 or more" = "$100,000 or more"))
        
#4.3: Label most important billing variables (can label others later if needed)
    
    #A. Number 
    hds <- hds %>%
      mutate(Q27AA..WHAT.ARE.THE.MAIN.REASONS.YOU.RECEIVE.BILLS.THROUGH.THE.MAIL.INSTEAD.OF.RECEIVING.THEM.BY.INTERNET. = 
               factor(Q27AA..WHAT.ARE.THE.MAIN.REASONS.YOU.RECEIVE.BILLS.THROUGH.THE.MAIL.INSTEAD.OF.RECEIVING.THEM.BY.INTERNET.,
                      levels = c(-1, 8,	14,	19,	20,	25,	26,	27,	97,	98,	99),
                      labels = c("QUESTION SKIPPED",	"SECURITY/PRIVACY",	
                                 "RECORD KEEPING",	"CONVENIENCE (EASE OF USE/SAVES TIME)",	
                                 "I HAVE ALWAYS DONE IT THIS WAY",	
                                 "HAVE NOT SET THINGS UP TO RECEIVE BILLS/STATEMENTS ONLINE",	
                                 "BILLER ONLY SENDS BILLS/STATEMENTS BY MAIL (NOT ONLINE)",	
                                 "NO INTERNET ACCESS TO RECEIVE BILLS/STATEMENTS ONLINE",	
                                 "Other",	"DK",	"RF")))    
   
    #B. Q31A1 Did you pay bills by mail last year (Format_library::NIU)
    hds <- hds %>%
      mutate(Q31A1..DID.THE.RESPONDENT.PAY.HOUSEHOLD.BILLS.BY.MAIL.THIS.TIME.LAST.YEAR. = 
               factor(Q31A1..DID.THE.RESPONDENT.PAY.HOUSEHOLD.BILLS.BY.MAIL.THIS.TIME.LAST.YEAR.,
                      levels = c(-5, 1, 2, 8, 9, 95),
                      labels = c("QUESTION NOT IN SURVEY", "Yes",	"No",	"DK",	
                                 "RF", "RF")))   
    
    #C. Q32A Did you pay bills by mail last year (Format_library::Q32AA)
    hds <- hds %>%
      mutate(Q32A..TYPES.OF.BILLS.PAID.BY.MAIL = 
               factor(Q32A..TYPES.OF.BILLS.PAID.BY.MAIL,
                      levels = c(-5, -1,	1, 2,	3, 4,	5, 6,	7, 8,	9, 10,	11,	
                                 12,	13,	14,	15,	16,	17,	18,	97,	98,	99),
                      labels = c("QUESTION NOT IN SURVEY", "QUESTION SKIPPED",	
                                 "Natural gas/propane/fuel oil etc.",	"Electric",	
                                 "Telephone",	"Water/sewer",	"Credit cards",	
                                 "Rent/mortgage",	"Cable TV",	"Insurance",	
                                 "Other loans(s)",	"Cell phone",	"Car payment",	
                                 "Medical or dental bills",	"Internet services",	
                                 "Alimony/child support",	"Taxes",	
                                 "Garbage/Solid Waste Services",	"Newspapers & Magazines",	
                                 "Homeowners Assoc Fees/Condo Assoc Fees",	
                                 "Other",	"DK",	"RF")))   

        
#4.4: Remove missing data
        
    #A. Drop from categorical variables
    hds[hds == 'RF'] <- NA
    hds[hds == 'DK'] <- NA 
    hds[hds == 'DK/RF'] <- NA 
    hds[hds == 'Other/DK/RF'] <- NA  
    hds[hds == 'QUESTION SKIPPED'] <- NA  
    hds[hds == 'QUESTION NOT IN SURVEY'] <- NA  
    hds[hds == '?'] <- NA  
    
    #B. Number of bills received in the mail
    hds <- hds %>%
      mutate(Q27..NUMBER.OF.BILLS.RECEIVED.IN.THE.MAIL = case_when(
        Q27..NUMBER.OF.BILLS.RECEIVED.IN.THE.MAIL < 0 ~ -5,
        Q27..NUMBER.OF.BILLS.RECEIVED.IN.THE.MAIL > 96 ~ -5,
        TRUE ~ as.numeric(Q27..NUMBER.OF.BILLS.RECEIVED.IN.THE.MAIL)
      )) %>%
      mutate(Q27..NUMBER.OF.BILLS.RECEIVED.IN.THE.MAIL = na_if(
        Q27..NUMBER.OF.BILLS.RECEIVED.IN.THE.MAIL, -5
      ))
    
    #C. Number of bills paid by household per month (Format_library::DKRFA)
    hds <- hds %>%
      mutate(Q29..NUMBER.OF.BILLS.PAID.BY.HOUSEHOLD.PER.MONTH = case_when(
        Q29..NUMBER.OF.BILLS.PAID.BY.HOUSEHOLD.PER.MONTH < 0 ~ -5,
        Q29..NUMBER.OF.BILLS.PAID.BY.HOUSEHOLD.PER.MONTH > 96 ~ -5,
        TRUE ~ as.numeric(Q29..NUMBER.OF.BILLS.PAID.BY.HOUSEHOLD.PER.MONTH)
      )) %>%
      mutate(Q29..NUMBER.OF.BILLS.PAID.BY.HOUSEHOLD.PER.MONTH = na_if(
        Q29..NUMBER.OF.BILLS.PAID.BY.HOUSEHOLD.PER.MONTH, -5
      ))
    
    #D. Q29 (If you don't know provide best estimate) (Format_library::DKRFA)
    hds <- hds %>%
      mutate(X.INT29..IF.YOU.DONT.KNOW..PLEASE.PROVIDE.YOUR.BEST.ESTIMATE. = case_when(
        X.INT29..IF.YOU.DONT.KNOW..PLEASE.PROVIDE.YOUR.BEST.ESTIMATE. < 0 ~ -5,
        X.INT29..IF.YOU.DONT.KNOW..PLEASE.PROVIDE.YOUR.BEST.ESTIMATE. > 96 ~ -5,
        TRUE ~ as.numeric(X.INT29..IF.YOU.DONT.KNOW..PLEASE.PROVIDE.YOUR.BEST.ESTIMATE.)
      )) %>%
      mutate(X.INT29..IF.YOU.DONT.KNOW..PLEASE.PROVIDE.YOUR.BEST.ESTIMATE. = na_if(
        X.INT29..IF.YOU.DONT.KNOW..PLEASE.PROVIDE.YOUR.BEST.ESTIMATE., -5
      ))
    
    #E. Q31A Number of bills per month by mail (Format_library::DKRFA)
    hds <- hds %>%
      mutate(Q31A..NUMBER.OF.BILLS.PAID.BY.HOUSEHOLD.PER.MONTH.BY.MAIL = case_when(
        Q31A..NUMBER.OF.BILLS.PAID.BY.HOUSEHOLD.PER.MONTH.BY.MAIL < 0 ~ -5,
        Q31A..NUMBER.OF.BILLS.PAID.BY.HOUSEHOLD.PER.MONTH.BY.MAIL > 96 ~ -5,
        TRUE ~ as.numeric(Q31A..NUMBER.OF.BILLS.PAID.BY.HOUSEHOLD.PER.MONTH.BY.MAIL)
      )) %>%
      mutate(Q31A..NUMBER.OF.BILLS.PAID.BY.HOUSEHOLD.PER.MONTH.BY.MAIL = na_if(
        Q31A..NUMBER.OF.BILLS.PAID.BY.HOUSEHOLD.PER.MONTH.BY.MAIL, -5
      ))
    
    #F. Number of bills received via the mail (Format_library::DKRFA)
    hds <- hds %>%
      mutate(BILLRECDMAIL..NUMBER.OF.BILLS.RECEIVED.VIA.MAIL = case_when(
        BILLRECDMAIL..NUMBER.OF.BILLS.RECEIVED.VIA.MAIL < 0 ~ -5,
        BILLRECDMAIL..NUMBER.OF.BILLS.RECEIVED.VIA.MAIL > 96 ~ -5,
        TRUE ~ as.numeric(BILLRECDMAIL..NUMBER.OF.BILLS.RECEIVED.VIA.MAIL)
      )) %>%
      mutate(BILLRECDMAIL..NUMBER.OF.BILLS.RECEIVED.VIA.MAIL = na_if(
        BILLRECDMAIL..NUMBER.OF.BILLS.RECEIVED.VIA.MAIL, -5
      ))

#------------------------------------------------------------------------#
#                     Step 5: Rename variable for easier                 #
#                       read in regression and Export                    #
#------------------------------------------------------------------------#      

#5.1: Rename 
    
    #A. Rename non-billing variables
    hds <- hds %>% 
      rename(INC.LEVEL = ANNUAL.HOUSEHOLD.INCOME.LEVEL,
             AGE.COHORT = RESPONDENT..AGE.COHORT,
             GENDER = RESPONDENT..GENDER,
             MARITAL.STATUS = RESPONDENT..MARITAL.STATUS)

    #B. Rename billing variables
    hds <- hds %>%
      rename(Q27AA.BILLS.MAIL.INSTEAD.INTERNET = Q27AA..WHAT.ARE.THE.MAIN.REASONS.YOU.RECEIVE.BILLS.THROUGH.THE.MAIL.INSTEAD.OF.RECEIVING.THEM.BY.INTERNET.,
             Q31A1.BILLS.PAID.MAIL.LAST.YEAR = Q31A1..DID.THE.RESPONDENT.PAY.HOUSEHOLD.BILLS.BY.MAIL.THIS.TIME.LAST.YEAR.,
             Q32A.TYPES.OF.BILLS.PAID.BY.MAIL = Q32A..TYPES.OF.BILLS.PAID.BY.MAIL,
             Q27.NUM.OF.BILLS.RECEIVED.MAIL = Q27..NUMBER.OF.BILLS.RECEIVED.IN.THE.MAIL,
             Q29.NUMB.BILLS.PAID.HOUSEHOLD.MONTH = Q29..NUMBER.OF.BILLS.PAID.BY.HOUSEHOLD.PER.MONTH,
             Q29.DONT.KNOW.ESTIMATE = X.INT29..IF.YOU.DONT.KNOW..PLEASE.PROVIDE.YOUR.BEST.ESTIMATE.,
             Q31A.NUM.BILLS.HOUSEHOLD.PER.MONTH.MAIL = Q31A..NUMBER.OF.BILLS.PAID.BY.HOUSEHOLD.PER.MONTH.BY.MAIL,
             NUM.OF.BILLS.RECEIVED.MAIL = BILLRECDMAIL..NUMBER.OF.BILLS.RECEIVED.VIA.MAIL)
    
    #C. Only keep variables of interest
    hds.1 <- hds %>%
      dplyr::select(SURVEY.YEAR, SAMPLE.NUMBER, starts_with("MAIL.VOLUME"),
                    INC.LEVEL, AGE.COHORT, GENDER, MARITAL.STATUS, Q27AA.BILLS.MAIL.INSTEAD.INTERNET,
                    Q31A1.BILLS.PAID.MAIL.LAST.YEAR, Q32A.TYPES.OF.BILLS.PAID.BY.MAIL, 
                    Q27.NUM.OF.BILLS.RECEIVED.MAIL, Q29.NUMB.BILLS.PAID.HOUSEHOLD.MONTH, 
                    Q29.DONT.KNOW.ESTIMATE, Q31A.NUM.BILLS.HOUSEHOLD.PER.MONTH.MAIL, 
                    NUM.OF.BILLS.RECEIVED.MAIL)
    
#5.2: Export
    
    #A. Save to CSV            
    df.new.name <- '2a_billing--selection.1.csv'       
    write_csv(hds.1, path = paste(clean_data_path, df.new.name, sep="\\"),
              append=FALSE, col_names=TRUE)
    
        
    