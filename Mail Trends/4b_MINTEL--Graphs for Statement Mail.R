# -> Gerhard O
# -> 10/29/2021
#       Update: 2.2.2022   
# -> Data sets: Cleaned mail by sector data (MINTEL)

# set working directory 
setwd("C:/Users/BG4CN0/USPS/ARTSI Team Folder - Documents/Projects")

# Load Packages in Library
source('./library_GSO.R')
library(forcats)

# Data Paths
clean_data_path <- "./Mail Trends/Sources/2022-02/Clean Data"
graph_path <- "./Mail Trends/Sources/2022-02/Charts and Figures"
  
  
#------------------------------------------------------------------------#
#                           Step 1: Load in Data                         #
#                                                                        #
#------------------------------------------------------------------------#
  
#1.1: Pull in data provided by Mintel (Comperemedia)
  
  # #A. September 2020-2021
  # statement.20.21_raw <- read_csv(paste(clean_data_path, "4a_mail.by.sector--statement mailing_2020-2021.csv",
  #                              sep = "/"))
  
  #A. September 2019-2021
  statement.19.21_raw <- read_csv(paste(clean_data_path, "4a_mail.by.sector--statement mailing_SEPT2019-DEC2021.csv",
                                  sep = "/"))

#------------------------------------------------------------------------#
#                             Step 2: Aggregate                          #
#                                                                        #
#------------------------------------------------------------------------#  
    
#2.1: Aggregate data (Sept 2019-Sept 2021)

    #A. Volume and Spend by month
    statement.19.21 <- statement.19.21_raw
    statement.19.21.agg <- statement.19.21 %>%
      group_by(Date) %>%
      summarise(volume = sum(Estimated.Mail.Volume),
                spend = sum(Spend))
    
    #B. Volume and Spend by month and industry
    statement.19.21.agg.ind <- statement.19.21 %>%
      group_by(Date, Industry) %>%
      summarise(volume = sum(Estimated.Mail.Volume),
                spend = sum(Spend)) %>%
      arrange(Date, volume)
    
        ##1. Create factor levels explicitly (https://stackoverflow.com/questions/69814998/ordering-a-stacked-bar-graph-by-second-variable-changing-over-time)
        statement.19.21.agg.ind <- 
          statement.19.21.agg.ind %>% 
          arrange(Date) %>%
          mutate(year_mo = fct_inorder(as.character(Date)))
        
        ##2. Split the new data by month and create different factor levels
        ls_statement.19.21.agg.ind <- 
          statement.19.21.agg.ind %>%
          split(., .$year_mo) %>%
          map(function(x) {x$Industry <- fct_reorder(x$Industry, x$volume); x})
        
        ##3. Make your geom_col list (geom_col is equivalent to geom_bar(stat= "identity")
        ls_statement.19.21.agg.ind_col <- map(ls_statement.19.21.agg.ind, function(x){
          geom_col(data = x, mapping = aes(x=as.Date(year_mo), y=volume, fill = Industry),
                   width = 15)
        })
        
    #C. Volume by company (top 3 each month)
    statement.19.21.agg.comp <- statement.19.21 %>%
      group_by(Date, Primary.Company) %>%
      summarise(volume = sum(Estimated.Mail.Volume)) %>%
      arrange(Date, -volume) %>%
      group_by(Date) %>%
      slice(1:3)
    
#2.3: Calculations
    
    #A. Average expenditure by month and industry
    statement.19.21 %>%
      group_by(year(Date), month(Date), Industry) %>%
      summarise(avg_month = mean(Spend))
    
    #B. Average volume by industry per year
    tab.2.3.B <- statement.19.21.agg.ind %>%
      group_by(year(Date), Industry) %>%
      summarise(avg_year = mean(volume)) %>%
      group_by(Industry) %>%
      mutate(percent.change = round(((avg_year - lag(avg_year)) / lag(avg_year))*100,2),
             First = head(avg_year, 1),
             BaselineChange = 
               case_when(avg_year != First ~ (avg_year - First) * 100 / First,
                         TRUE ~ 1 * NA)) %>%
      mutate(BaselineChange = round(BaselineChange, 2)) %>%
      mutate(avg_year = prettyNum(avg_year, big.mark = ",")) 
    
    #C. Percentage change month-to-month and from baseline year
    tab.2.3.C <- statement.19.21.agg %>%
      
        ##1. Expenditures
        mutate(perc.chg.spend = round(((spend - lag(spend)) / lag(spend))*100,2),
               First.spend = head(spend, 1),
               BlineChg.spend = 
                 case_when(spend != First.spend ~ (spend - First.spend) * 100 / First.spend,
                           TRUE ~ 1 * NA)) %>%
        mutate(BlineChg.spend = round(BlineChg.spend, 2)) %>%
        mutate(spend = prettyNum(spend, big.mark = ",")) %>%
      
        ##2. Volume
        mutate(perc.chg.volume = round(((volume - lag(volume)) / lag(volume))*100,2),
               First.vol = head(volume, 1),
               BlineChg.volume = 
                 case_when(volume != First.vol ~ (volume - First.vol) * 100 / First.vol,
                           TRUE ~ 1 * NA)) %>%
        mutate(BlineChg.volume = round(BlineChg.volume, 2)) %>%
        mutate(volume = prettyNum(volume, big.mark = ",")) %>%
      
        ##3. Remove variables
        dplyr::select(-starts_with("First"))
    
    #D. Percentage change year-to-year
    tab.2.3.D <- statement.19.21.agg %>%
      
        ##1. Expenditures (Year on Year)
        mutate(perc.chg.spend.yr = round(((spend - lag(spend, 12)) / lag(spend, 12))*100,2)) %>%
      
        ##2. Expenditures (Year on 2 Year)
        mutate(perc.chg.spend.2yr = round(((spend - lag(spend, 24)) / lag(spend, 24))*100,2)) %>%
        mutate(spend = prettyNum(spend, big.mark = ",")) %>%
        
        ##3. Volume (Year on Year)
        mutate(perc.chg.vol.yr = round(((volume - lag(volume, 12)) / lag(volume, 12))*100,2)) %>%
    
        ##4. Volume (Year on 2 Year)
        mutate(perc.chg.vol.2yr = round(((volume - lag(volume, 24)) / lag(volume, 24))*100,2)) %>%
        mutate(volume = prettyNum(volume, big.mark = ","))
    
#2.4: Export Calculations tables
    
    #A. Average volume by industry per year
    df.new.name <- '4b_Calculation Tables--tab.2.3B.csv'       
    write_csv(tab.2.3.B, path = paste(clean_data_path, df.new.name, sep="/"),
              append=FALSE, col_names=TRUE)
    
    #B. Percentage change month-to-month and from baseline year
    df.new.name <- '4b_Calculation Tables--tab.2.3C.csv'       
    write_csv(tab.2.3.C, path = paste(clean_data_path, df.new.name, sep="/"),
              append=FALSE, col_names=TRUE)
    
    #C. Percentage change year-to-year
    df.new.name <- '4b_Calculation Tables--tab.2.3D.csv'       
    write_csv(tab.2.3.D, path = paste(clean_data_path, df.new.name, sep="/"),
              append=FALSE, col_names=TRUE)
    
#------------------------------------------------------------------------#
#                             Step 3: Graphs                             #
#                                                                        #
#------------------------------------------------------------------------#

#3.1: Number of statements over time

    #A. Graph
    graph.3.1 <- statement.19.21.agg %>% 
      
      ggplot(aes(x=Date)) +
      
        ##1. Bar graph
        geom_bar(aes(x=Date, y=volume), stat="identity", fill = "#014d64") +
        annotate("text", x=as.Date("2019-09-30", "%Y-%m-%d"), color = "#014d64", 
                 y=48500000, label = "Volume", size = 3.5, fontface = 2) +
        
        ##2. Line Graph
        geom_line(aes(x=Date, y=spend), size=1.3, color="#adadad") +
        geom_point(aes(x=Date, y=spend), size=2, color="#3b3f40") +
        annotate("text", x=as.Date("2019-09-5", "%Y-%m-%d"), color = "#adadad",
                 y=20500000, label = "Spending", size = 3.5, fontface = 2) +
        
        ##3. Graph title and Axis labels
        ggtitle(label = "Statement Mail Volume and Spending",
                subtitle = "") +
        labs(caption = 'Source: MINTEL (Comperemedia) 
                          *December 2021: Mail Volume and Spend Report*') +
        ylab("Volume and Spending (Millions)") + 
        xlab("") +
        
        ##4. Scales
        scale_x_date(date_breaks = "2 month", labels = scales::label_date_short()) +
        scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6, 
                                                accuracy = 1),
                           limits = c(0, 50000000)) + 
        
        #5. Theme
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = element_markdown(color="black", size=14, hjust = .5),
              plot.subtitle = element_text(hjust = .5),
              axis.title.x = element_text(size = 9, color = "grey30"), 
              axis.title.y = element_text(size = 9, color = "grey30"), 
              legend.position = c(0.9, 0.85),
              legend.title = element_blank(),
              legend.text = ggtext::element_markdown(size = 10),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              panel.grid.minor.x = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 
    #C. Save Graph
    ggsave(paste(graph_path, "4b_3.1--Statement Mail Volume and Spending by Month.png", 
                 sep="\\"), graph.3.1, width=6, height=6) 

#3.2: Number of statements over time by industry

    #A. Graph
    graph.3.2 <- ggplot() +
      
        ##1. Bar graph(s)
        ls_statement.19.21.agg.ind_col +
        
        ##2. Graph title and Axis labels
        ggtitle(label = "Statement Mail Volume by Industry",
                subtitle = "Sorted from the largest to the smallest by volume and month") +
        labs(caption = 'Source: MINTEL (Comperemedia) 
                            *December 2021: Mail Volume and Spend Report*') +
        ylab("Volume (Millions)") + 
        xlab("") +
        
        ##3. Scales
        scale_fill_manual(values=c("#acc8d4","#dbcc98", "#36E2BD", "#e3120b", 
                                   "#336666", "#FB9851")) +
        scale_x_date(date_breaks = "2 months", labels = scales::label_date_short()) +
        scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6, 
                                                accuracy = 1)) + 
        
        #4. Theme
        guides(col = guide_legend(ncol = 2, nrow = 3)) +
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = element_markdown(color="black", size=14, hjust = .5),
              plot.subtitle = element_text(hjust = .5),
              axis.title.x = element_text(size = 9, color = "grey30"), 
              axis.title.y = element_text(size = 9, color = "grey30"), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_blank(),
              legend.text = element_text(size = 6),
              legend.position = "bottom",
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              panel.grid.minor.x = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 
    #C. Save Graph
    ggsave(paste(graph_path, "4b_3.2--Statement Mail Volume by Month and Industry.png", 
                 sep="\\"), graph.3.2, width=6, height=6) 


#3.3: Number of statements over time by industry

    #A. Graph
    graph.3.3 <- statement.19.21.agg.comp %>%
      
      ggplot(aes(x=Date)) +
      
        ##1. Bar graph(s)
        geom_bar(aes(x=Date, y=volume, fill = Primary.Company), stat="identity") + 
        
        ##2. Graph title and Axis labels
        ggtitle(label = "Statement Mail Volume by Company",
                subtitle = "Includes the three largest mailers within each month") +
        labs(caption = 'Source: MINTEL (Comperemedia) 
                              *December 2021: Mail Volume and Spend Report*') +
        ylab("Volume (Millions)") + 
        xlab("") +
        
        ##3. Scales
        scale_fill_brewer(palette = "Paired") +
        scale_x_date(date_breaks = "2 month", labels = scales::label_date_short()) +
        scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6, 
                                                accuracy = 1)) + 
        
        #4. Theme
        guides(col = guide_legend(ncol = 2, nrow = 3)) +
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = element_markdown(color="black", size=14, hjust = .5),
              plot.subtitle = element_text(hjust = .5),
              axis.title.x = element_text(size = 9, color = "grey30"), 
              axis.title.y = element_text(size = 9, color = "grey30"), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_blank(),
              legend.text = element_text(size = 6),
              legend.position = "bottom",
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              panel.grid.minor.x = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 
    #C. Save Graph
    ggsave(paste(graph_path, "4b_3.3--Statement Mail Volume by Month and Company.png", 
                 sep="\\"), graph.3.3, width=8, height=4.5)      
 