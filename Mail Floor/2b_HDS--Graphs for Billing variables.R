# -> Gerhard O
# -> 10/12/2021
# -> Data sets: HDS Billing
# -> Build figures from billing variables

# Load Packages in Library
source('C:\\Users\\bg4cn0\\USPS\\ARTSI Team Folder - Documents\\Projects\\library_GSO.R')

# Data Paths
raw_data_path <- "C:\\Users\\bg4cn0\\USPS\\ARTSI Team Folder - Documents\\Projects\\Mail Floor\\Code\\Clean Data\\2a"
graph_path <- "C:\\Users\\bg4cn0\\USPS\\ARTSI Team Folder - Documents\\Projects\\Mail Floor\\Code\\Tables & Figures\\2b"

#------------------------------------------------------------------------#
#                           Step 1: Load in Data                         #
#                                                                        #
#------------------------------------------------------------------------#

#1.1: Cleaned HDS that includes billing variables

    #A. HDS Received
    hds_raw <- read.csv(paste(raw_data_path, "2a_billing--selection.1.csv",
                              sep = "\\"))
    
#------------------------------------------------------------------------#
#                           Step 2: Light Clean                          #
#                                                                        #
#------------------------------------------------------------------------#

#2.1: Data checks
    
    #A. Number of NAs by column
    df.na.all <- hds_raw %>%
      summarise(across(everything(), ~ sum(is.na(.x))))
    
    #B. Number of NAs by column and year
    df.na.month.all <- hds_raw %>%
      group_by(SURVEY.YEAR) %>%
      summarise(across(everything(), ~ sum(is.na(.x))))
    
    #C. Number of NAs as a % of total observations by column and year
    df.na.month.perc.all <- hds_raw %>%
      group_by(SURVEY.YEAR) %>%
      summarise(across(everything(), ~ round(100*mean(is.na(.x)), 2) ) )
    
#2.2: Clean variables
    
    #A. Collapse age cohorts together 
    hds.age <- hds_raw %>%
      mutate(AGE.COHORT2 = case_when(
        AGE.COHORT=="18-24" ~ "18-34",
        AGE.COHORT=="25-34" ~ "18-34",
        AGE.COHORT=="65-69" ~ "65+",
        AGE.COHORT=="70-74" ~ "65+",
        AGE.COHORT=="70+" ~ "65+",
        AGE.COHORT=="75+" ~ "65+",
        TRUE ~ as.character(AGE.COHORT)
      )) 
    
    #B. Get average Bills paid per month
    
        ##1. By year
        mean.bill.month <- hds.age %>%
          group_by(SURVEY.YEAR) %>%
          summarise(Mean.bill = round(mean(Q29.NUMB.BILLS.PAID.HOUSEHOLD.MONTH, na.rm = T), 2),
                 Mean.bill.mail = round(mean(Q31A.NUM.BILLS.HOUSEHOLD.PER.MONTH.MAIL, na.rm = T), 2)) %>%
          mutate(Avg.all.bill.mail = round(100*(Mean.bill.mail/Mean.bill), 2))
        
        ##2. By year and age cohort
        mean.bill.month.age <- hds.age %>%
          group_by(SURVEY.YEAR, AGE.COHORT2) %>%
          summarise(Mean.bill = round(mean(Q29.NUMB.BILLS.PAID.HOUSEHOLD.MONTH, na.rm = T), 2),
                    Mean.bill.mail = round(mean(Q31A.NUM.BILLS.HOUSEHOLD.PER.MONTH.MAIL, na.rm = T), 2)) %>%
          mutate(Avg.all.bill.mail = round(100*(Mean.bill.mail/Mean.bill), 2)) %>%
          filter(!is.na(AGE.COHORT2))
        
    #C. Get average Bills received per month
        
        ##1. By year
        mean.bill.rec <- hds.age %>%
          group_by(SURVEY.YEAR) %>%
          summarise(Mean.bill.rec.1 = round(mean(Q27.NUM.OF.BILLS.RECEIVED.MAIL, na.rm = T), 2),
                    Mean.bill.rec.2 = round(mean(NUM.OF.BILLS.RECEIVED.MAIL, na.rm = T), 2)) %>%
          mutate(Mean.bill.rec = coalesce(Mean.bill.rec.1, Mean.bill.rec.2)) %>%
          dplyr::select(SURVEY.YEAR, Mean.bill.rec)
        mean.bill.rec$Mean.bill.rec[is.nan(mean.bill.rec$Mean.bill.rec)] <- NA
        
        ##2. Merge with bills paid by mail
        mean.bill.rec.paid <- mean.bill.month %>%
          dplyr::select(SURVEY.YEAR, Mean.bill.mail) %>%
          left_join(mean.bill.rec, by = c("SURVEY.YEAR")) 
        
    #D. Get percent of respondents who paid bills by mail last year
    
        ##1. By year (with NAs)
        perc.bill.last.year <- hds.age %>%
          group_by(SURVEY.YEAR) %>%
          mutate(bill.no =if_else(Q31A1.BILLS.PAID.MAIL.LAST.YEAR=="No", 1, 0),
                 bill.yes = if_else(Q31A1.BILLS.PAID.MAIL.LAST.YEAR=="Yes", 1, 0),
                 bill.na = if_else(is.na(Q31A1.BILLS.PAID.MAIL.LAST.YEAR), 1, 0)) %>%
          summarise(perc.no = round(100*(sum(bill.no, na.rm = T)/n()), 2),
                    perc.yes = round(100*(sum(bill.yes, na.rm = T)/n()),2),
                    perc.na = round(100*(sum(bill.na, na.rm = T)/n()),2)) %>%
          filter(SURVEY.YEAR > 2011)
        
        ##2. By year (without NAs)
        perc.bill.last.year_no.na <- hds.age %>%
          group_by(SURVEY.YEAR) %>%
          mutate(bill.no =if_else(Q31A1.BILLS.PAID.MAIL.LAST.YEAR=="No", 1, 0),
                 bill.yes = if_else(Q31A1.BILLS.PAID.MAIL.LAST.YEAR=="Yes", 1, 0)) %>%
          filter(!is.na(Q31A1.BILLS.PAID.MAIL.LAST.YEAR)) %>%
          summarise(perc.no = round(100*(sum(bill.no, na.rm = T)/n()), 2),
                    perc.yes = round(100*(sum(bill.yes, na.rm = T)/n()),2))
        
        
#2.3: Reshape data as needed
        
    #A. By year LONG
    mean.bill.month.long <- mean.bill.month %>%
      gather(billing.type, value, Mean.bill:Mean.bill.mail)
    
    #B. Bills received per month merged with paid by mail LONG
    mean.bill.rec.paid.long <- mean.bill.rec.paid %>%
      gather(billing.type, value, Mean.bill.mail:Mean.bill.rec)
    
    #C. Paid bills by mail last year (no NA)
    perc.bill.last.year_no.na.long <- perc.bill.last.year_no.na %>%
      gather(billing.type, value, perc.no:perc.yes)
        
#------------------------------------------------------------------------#
#                             Step 3: Graph                              #
#          Main var--> Number of bills paid by household per month       #
#------------------------------------------------------------------------#    

#3.1: Bar graph of "Number of bills paid by households per month"
#     Fiscal Year as x-axis

    #A. Graph
    graph.3.1 <- mean.bill.month.long %>% 
      mutate(billing.type.2 = case_when(
        billing.type=="Mean.bill" ~ "All Bills",
        billing.type=="Mean.bill.mail" ~ "Bills by\nMail",
      )) %>%
      mutate(Avg.all.bill.mail = ifelse(billing.type=="Mean.bill", NA, 
                                         Avg.all.bill.mail)) %>%
      
      ggplot(aes(x=SURVEY.YEAR)) +
      
        ##1. Bar Graph and text
        geom_bar(aes(fill=billing.type.2, y=value), position="identity", 
                 stat="identity", width = 0.7) +
        geom_text(aes(y=value, label=ifelse(is.na(Avg.all.bill.mail), "",
                                            sprintf("%1.0f%%", Avg.all.bill.mail))),
                  position=position_dodge(width=0.9), vjust=-0.25, size = 2,
                  hjust=.4, color = "white") +
        
        ##2. Graph title and Axis labels
        ggtitle(label = "Average Number of Bills Paid by Households Per Month",
                subtitle = "Bills by Mail as a Percent of All Bills") +
        labs(caption = 'Note: Years are fiscal years <br/> 
                        Source: Household Diary Survey') +
        ylab("Average Number of Bills Paid")+
        xlab("") +
        
        ##3. Scales
        scale_fill_manual(values = c(
          'All Bills' = "#014d64",
          'Bills by\nMail' = "#adadad")) +
        scale_x_continuous(breaks = seq(from = min(mean.bill.month.long$SURVEY.YEAR), 
                                     to = max(mean.bill.month.long$SURVEY.YEAR), by = 1)) +
        scale_y_continuous(breaks = seq(0, 16, by=2), limits = c(0, 14)) + 
        
        ##4. Theme
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = ggtext::element_markdown(color="black", size=14, hjust = .5),
              plot.subtitle = element_text(hjust = .5),
              axis.title.x = element_text(size = 9, color = "grey30"), 
              axis.title.y = element_text(color = "grey30", size=9),
              axis.title.y.right = element_text(color = "grey30", size=9),
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_blank(),
              legend.text = element_text(size = 7),
              legend.position = c(0.89, 0.89),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              panel.grid.minor.x = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D"))  
    
    #C. Save Graph
    ggsave(paste(graph_path, "2b_3.1--Bills by mail as a proportion of all bills.png",
                 sep="\\"), graph.3.1, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)
        
#3.2: Percentage Change in Marketing Mail Received by Age Cohort over time
            
    #A. Graph
    graph.3.2 <- ggplot() + 
      
        ##1. Line graph
        geom_hline(yintercept = 50, color = "#1C2833") +
        geom_line(data = mean.bill.month.age, 
                  aes(x=SURVEY.YEAR, y=Avg.all.bill.mail, group=AGE.COHORT2, 
                                     color=AGE.COHORT2), size = 1) +
        
        ##2. Graph title and Axis labels
        ggtitle(label = "Percent of All Bills Paid by **Mail**",
                subtitle = "Bills Paid by Households per Month") +
        labs(caption = 'Note: Percent = Average number of bills paid by **mail** 
                        by households per month / Average number of **all** bills paid by households
                        per month<br/>Source: Household Diary Survey',
             color = 'Age Cohort') +
        ylab("% of All Bills Paid by Mail") + 
        xlab("") +
        
        ##3. Scales
        scale_color_manual(values=c("#e3120b", "#336666", "#FB9851", "#acc8d4", 
                                    "#dbcc98")) + 
        scale_x_continuous(limits = c(2000, 2019) , breaks = (seq(2000, 2019, 2))) +
        scale_y_continuous(limits = c(0, 100), labels = percent_format(scale = 1),
                           breaks = seq(0, 100, by=10)) +
        
        ##4. Theme
        guides(col = guide_legend(ncol = 3, nrow = 2)) +
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = ggtext::element_markdown(color="black", size=14, hjust = .5),
              plot.subtitle = element_text(hjust = .5),
              axis.title.x = element_text(size = 9, color = "grey30"), 
              axis.title.y = element_text(size = 9, color = "grey30"), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 7),
              legend.position = c(0.15, 0.25),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D"))   
    
    #B Save Graph
    ggsave(paste(graph_path, "2b_3.2--Percent of All Bills Paid by Mail.png",
                 sep="\\"), graph.3.2, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)  
    
#------------------------------------------------------------------------#
#                             Step 4: Graph                              #
#             Main var--> Number of bills received by households         #
#------------------------------------------------------------------------#      

#4.1: Bar graph of "Average Number of Bills Received and Paid by Mail"
#     Fiscal Year as x-axis

    #A. Graph
    graph.4.1 <- mean.bill.rec.paid.long %>% 
      mutate(billing.type.2 = case_when(
        billing.type=="Mean.bill.rec" ~ "Bills Received\nby Mail",
        billing.type=="Mean.bill.mail" ~ "Bills Paid\nby Mail",
      )) %>% 
      mutate(billing.type.2 = replace(billing.type.2, SURVEY.YEAR 
                                      %in% c(2010, 2012), NA)) %>%
      
      ggplot(aes(x=SURVEY.YEAR)) +
        
        ##1. Bar Graph and text
        geom_bar(aes(fill=billing.type.2, y=value), position="dodge", 
                 stat="identity", width = 0.7) +
        
        ##2. Graph title and Axis labels
        ggtitle(label = "Average Number of Bills Received and Paid by Mail",
                subtitle = "") +
        labs(caption = 'Note: The question "Number of Bills Received by Mail" 
                        was not asked between 2000-2006 and 2010-2013.<br/> 
                        Source: Household Diary Survey') +
        ylab("Average Number of Bills Paid")+
        xlab("") +
        
        ##3. Scales
        scale_fill_manual(values = c(
          'Bills Received\nby Mail' = "#dbcc98",
          'Bills Paid\nby Mail' = "#adadad"),
          na.translate = F) +
        scale_x_continuous(breaks = seq(from = 2007, 
                                        to = max(mean.bill.month.long$SURVEY.YEAR), 
                                        by = 2), limits = c(2006, 2020)) +
        scale_y_continuous(breaks = seq(0, 12, by=2), limits = c(0, 12)) + 
        
        ##4. Theme
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = ggtext::element_markdown(color="black", size=14, hjust = .5),
              plot.subtitle = element_text(hjust = .5),
              axis.title.x = element_text(size = 9, color = "grey30"), 
              axis.title.y = element_text(color = "grey30", size=9),
              axis.title.y.right = element_text(color = "grey30", size=9),
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_blank(),
              legend.text = element_text(size = 7),
              legend.position = c(0.89, 0.85),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              panel.grid.minor.x = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D"))    
    
    #B Save Graph
    ggsave(paste(graph_path, "2b_4.1--Average Number of Bills Received and Paid by Mail.png",
                 sep="\\"), graph.4.1, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)  

#------------------------------------------------------------------------#
#                             Step 5: Graph                              #
#       Main var--> Did respondent pay a bill by the mail last year      #
#------------------------------------------------------------------------#      

#5.1: Bar graph of "Did respondent pay a bill by the mail last year"
#     Fiscal Year as x-axis  
    
    #A. Graph
    graph.5.1 <- perc.bill.last.year_no.na.long %>% 
      mutate(billing.type.2 = case_when(
        billing.type=="perc.yes" ~ "Yes",
        billing.type=="perc.no" ~ "No",
      )) %>% 
              
      ggplot(aes(x=SURVEY.YEAR)) +
      
        ##1. Bar Graph and text
        geom_bar(aes(fill=billing.type.2, y=value), position="dodge", 
                 stat="identity", width = 0.7) +
        
        ##2. Graph title and Axis labels
        ggtitle(label = "Percent of Respondents Who Paid a Bill by the Mail Last Year",
                subtitle = "") +
        labs(caption = 'Note: The question "Did you pay a bill by mail last year?" 
                          was not asked between 2000-2011.<br/> 
                          Source: Household Diary Survey',
             fill = 'Paid Bill by\nMail Last Year') +
        ylab("% Bills Paid Last Year")+
        xlab("") +
        
        ##3. Scales
        scale_fill_manual(values = c(
          'Yes' = "#475ED1",
          'No' = "#F6423C"),
          na.translate = F) +
      scale_x_continuous(limits = c(2011, 2020) , breaks = (seq(2012, 2019, 1))) +
      scale_y_continuous(limits = c(0, 100), labels = percent_format(scale = 1),
                         breaks = seq(0, 100, by=10)) + 
        
        ##4. Theme
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = ggtext::element_markdown(color="black", size=14, hjust = .5),
              plot.subtitle = element_text(hjust = .5),
              axis.title.x = element_text(size = 9, color = "grey30"), 
              axis.title.y = element_text(color = "grey30", size=9),
              axis.title.y.right = element_text(color = "grey30", size=9),
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 7),
              legend.position = c(0.89, 0.85),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              panel.grid.minor.x = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D"))      
      
    #B Save Graph
    ggsave(paste(graph_path, "2b_5.1--Percent of Respondents Who Paid a Bill by the Mail Last Year.png",
                 sep="\\"), graph.5.1, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)   
      
    
    
    