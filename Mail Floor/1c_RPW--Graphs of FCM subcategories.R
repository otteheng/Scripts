# -> Gerhard O
# -> 9/24/2021
# -> Data sets: RPW
# -> Build figures FCM subcategories (pre-sort, single piece)

# set working directory 
setwd("C:/Users/BG4CN0/USPS/ARTSI Team Folder - Documents/Projects")

# Load Packages in Library
source('./library_GSO.R')

# Data Paths
usps_public_path <- "./_Raw Data/USPS Public"
graph_path <- "./Mail Floor/Code/Tables & Figures/1c"
fiscal_calendar_path <- "./_Scripts"
mail_trends_path <- "./Mail Trends/Sources/November 2021"

#------------------------------------------------------------------------#
#                           Step 1: Load in Data                         #
#                                                                        #
#------------------------------------------------------------------------#

rpw_raw <- read.csv(paste(usps_public_path, "10_Clean Quarterly RPW--fy2001-2020_totals.csv",
                          sep = "/"))

mail_trends_raw <- read_excel(paste(mail_trends_path, "Mail Volume RPW.xlsx",
                                    sep = "/"))

source(paste(fiscal_calendar_path, "Fiscal-Calendar Year Crosswalk.R", sep = "/"))

#------------------------------------------------------------------------#
#                           Step 2: Light Clean                          #
#                                                                        #
#------------------------------------------------------------------------#

#2.1: Restructure data

    #A. Explore data
    tab.yr.srv <- as.data.frame(table(rpw_raw$fiscal_year, rpw_raw$service_category))
    
    #B. Filter data 
    rpw <- rpw_raw %>%
      filter(service_category=="Total Single-Piece Letters and Cards" |
             service_category=="Total Presort Letters and Cards" |
             service_category=="Total First-Class Mail") %>%
      filter(fiscal_year>2008) %>% 
      dplyr::select(service_category, revenue, pieces, fiscal_year, quarter)
    
    #C. Make data wide
    
        ##1. Pieces
        rpw.pieces <- rpw %>%
          dplyr::select(-revenue) %>%
          spread(service_category, pieces)
        
        ##2. Revenue
        rpw.revenue <- rpw %>%
          dplyr::select(-pieces) %>%
          spread(service_category, revenue)
        
#2.2: New variables
        
    #A. Presort and single piece as a percentage of FCM volume
    rpw.pieces <- rpw.pieces %>%
      mutate(perc.presort = (`Total Presort Letters and Cards`/`Total First-Class Mail`)*100,
             perc.presort = round(perc.presort, 2),
             perc.single = (`Total Single-Piece Letters and Cards`/`Total First-Class Mail`)*100, 
             perc.single = round(perc.single, 2)) %>%
      rename(FISCAL.QUARTER = quarter,
             FISCAL.YEAR = fiscal_year)
    
        ##1. Include percent "Other"
        rpw.pieces <- rpw.pieces %>%
          mutate(perc.other = 100-perc.presort-perc.single)
    
    #B Presort and single piece as a percentage of FCM revenue
    rpw.revenue <- rpw.revenue %>%
      mutate(perc.presort = (`Total Presort Letters and Cards`/`Total First-Class Mail`)*100,
             perc.presort = round(perc.presort, 2),
             perc.single = (`Total Single-Piece Letters and Cards`/`Total First-Class Mail`)*100, 
             perc.single = round(perc.single, 2)) %>%
      rename(FISCAL.QUARTER = quarter,
             FISCAL.YEAR = fiscal_year)
    
        ##1. Include percent "Other"
        rpw.revenue<- rpw.revenue %>%
          mutate(perc.other = 100-perc.presort-perc.single)
        
#2.3: Clean mail trends data
        
    #A. Reduce variables
        
        ##1. FCM
        mail.trends.fcm <- mail_trends_raw %>%
          filter(...1=="Total FCM") %>%
          dplyr::select(-`2010-2019 CAGR`) %>% 
          rename(category = ...1)
        
        ##2. Marketing Mail
        mail.trends.mm <- mail_trends_raw %>%
          filter(...1=="Total MM") %>%
          dplyr::select(-`2010-2019 CAGR`) %>% 
          rename(category = ...1)
    
    #B. Reshape FCM
    mail.trends.fcm <- mail.trends.fcm %>%
      gather("year", "pieces")
    
        ##1. Make first row column names
        header.true <- function(df) {
          names(df) <- as.character(unlist(df[1,]))
          df[-1,]
        }
        mail.trends.fcm <- header.true(mail.trends.fcm) 
        mail.trends.fcm <- mail.trends.fcm %>%
          rename(total.fcm = `Total FCM`, 
                 year = category)
        
    #C. Reshape Marketing mail
    mail.trends.mm <- mail.trends.mm %>%
      gather("year", "pieces")
        
        ##1. Make first row column names
        header.true <- function(df) {
          names(df) <- as.character(unlist(df[1,]))
          df[-1,]
        }
        mail.trends.mm <- header.true(mail.trends.mm)
        mail.trends.mm <- mail.trends.mm %>%
          rename(total.mm = `Total MM`,
                 year = category)
        
    #D. Merge together
    mail.trends <- inner_join(mail.trends.fcm, mail.trends.mm, by = c("year"))
    
    #E. Convert to numeric
    mail.trends <- mail.trends %>%
      mutate(total.fcm = as.numeric(total.fcm),
             total.mm = as.numeric(total.mm),
             year = as.numeric(year)) %>%
      
        ##1. Amount should be in billions
        mutate(total.fcm = total.fcm * 1000,
               total.mm = total.mm *1000)
    
    
#------------------------------------------------------------------------#
#                             Step 3: Merge                              #
#                                                                        #
#------------------------------------------------------------------------#   
 
#3.1: Merge with Fiscal/Calendar Year Crosswalk
    
    #A. Remove variables for merge
    df.1 <- df %>%
      dplyr::select(-start, -end, -MONTH, -MONTH.NUM) %>%
      distinct()
    
    #B. Merge
    
        ##1. Pieces
        merge.pieces <- rpw.pieces %>%
          left_join(df.1,  by = c("FISCAL.YEAR", "FISCAL.QUARTER"))
    
        ##2. Revenue
        merge.revenue <- rpw.revenue %>%
          left_join(df.1,  by = c("FISCAL.YEAR", "FISCAL.QUARTER"))
        
#3.2: Convert Year/Quarter to datetime needed for graph
        
    #A. Pieces
    merge.pieces <- merge.pieces %>%
      mutate(calendar.year.q = paste0(CALENDAR.YEAR, " Q", CALENDAR.QUARTER),
             calendar.year.q.1 = as.yearqtr(calendar.year.q),
             fiscal.year.q = paste0(FISCAL.YEAR, " Q", FISCAL.QUARTER),
             fiscal.year.q.1 = as.yearqtr(fiscal.year.q))
    
    #B. Revenue
    merge.revenue <- merge.revenue %>%
      mutate(calendar.year.q = paste0(CALENDAR.YEAR, " Q", CALENDAR.QUARTER),
             calendar.year.q.1 = as.yearqtr(calendar.year.q),
             fiscal.year.q = paste0(FISCAL.YEAR, " Q", FISCAL.QUARTER),
             fiscal.year.q.1 = as.yearqtr(fiscal.year.q))
    
    #C. Long data
    rpw.wide <- rpw %>%
      mutate(fiscal.year.q = as.yearqtr(paste0(fiscal_year, " Q", quarter)) )
    
#3.3: FCM Values are in RPW are in thousands. Change to actual number
    
    #A. Pieces
    merge.pieces <- merge.pieces %>%
      mutate(`Total First-Class Mail` = `Total First-Class Mail`*1000, 
             `Total Presort Letters and Cards` = `Total Presort Letters and Cards`*1000,
             `Total Single-Piece Letters and Cards` = `Total Single-Piece Letters and Cards`*1000)
    
    #B. Revenue
    merge.revenue <- merge.revenue %>%
      mutate(`Total First-Class Mail` = `Total First-Class Mail`*1000, 
             `Total Presort Letters and Cards` = `Total Presort Letters and Cards`*1000,
             `Total Single-Piece Letters and Cards` = `Total Single-Piece Letters and Cards`*1000)
    
    #C. Long data
    rpw.wide <- rpw.wide %>%
      mutate(pieces = pieces*1000,
             revenue = revenue*1000)
    
#3.4: Make data with percents long again
    
    #A. Make data long (again)
    merge.pieces.long <- merge.pieces %>%
      dplyr::select(perc.single, perc.presort, perc.other, fiscal.year.q.1) %>%
      gather(service.category, percent, perc.single:perc.other)
    
      
#------------------------------------------------------------------------#
#                             Step 4: Graph                              #
#                         Line Graphs of Volumes                         #
#------------------------------------------------------------------------#    
    
#4.1: Basic graph of volume for total FCM, presort and single piece
#     Calendar Year as x-axis

    #A. Graph
    graph.4.1 <- merge.pieces %>% 
      ggplot(aes(x=calendar.year.q.1)) +
      
        ##1. Line
        geom_line( aes(y=`Total First-Class Mail`, color = "Total FCM"), size=1) +
        geom_line( aes(y=`Total Presort Letters and Cards`, color = "Total FCM\nPresort"), 
                   size=1) +
        geom_line( aes(y=`Total Single-Piece Letters and Cards`,
                       color = "Total FCM\nSingle Piece"), size=1) +
        
        ##2. Graph title and Axis labels
        ggtitle(label = "First Class Mail and Subcategory Volumes",
                subtitle = "Calendar Years: 2008-2020") +
        labs(caption = 'Source: RPW',
             color = 'Key') +
        ylab("Volume (in Billions)")+
        xlab("") +
        
        ##3. Scales
        scale_color_manual(values = c(
          'Total FCM' = "#014d64",
          'Total FCM\nPresort' = "#adadad",
          'Total FCM\nSingle Piece' = "#01a2d9")) +
        scale_x_yearqtr(format = "%Y\nQ%q", 
                        limits = c(min(merge.pieces$calendar.year.q.1), 
                                   max(merge.pieces$calendar.year.q.1)),
                        breaks = seq(from = min(merge.pieces$calendar.year.q.1), 
                                     to = max(merge.pieces$calendar.year.q.1), by = 0.5)) +
        scale_y_continuous(labels = unit_format(unit = "B", scale = 1e-9, 
                                                accuracy = 1)) + 
        
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
              legend.position = c(0.9, 0.8),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 
    
    #C. Save Graph
    ggsave(paste(graph_path, "1c_4.1--First Class Mail and Subcategory Volumes.png", 
                 sep="/"), graph.4.1, scale = 1, width = 8.5, 
           height = (7.5/1.618), dpi = 300)  
    
#4.2: Basic graph of volume for total FCM, presort and single piece
#     Fiscal Year as x-axis
    
    #A. Graph
    graph.4.2 <- merge.pieces %>% 
      ggplot(aes(x=fiscal.year.q.1)) +
      
      ##1. Line
      geom_line( aes(y=`Total First-Class Mail`, color = "Total FCM"), size=1) +
      geom_line( aes(y=`Total Presort Letters and Cards`, color = "Total FCM\nPresort"), 
                 size=1) +
      geom_line( aes(y=`Total Single-Piece Letters and Cards`,
                     color = "Total FCM\nSingle Piece"), size=1) +
      
      ##2. Graph title and Axis labels
      ggtitle(label = "First Class Mail and Subcategory Volumes",
              subtitle = "Fiscal Years: 2009-2020") +
      labs(caption = 'Source: RPW',
           color = 'Key') +
      ylab("Volume (in Billions)")+
      xlab("") +
      
      ##3. Scales
      scale_color_manual(values = c(
        'Total FCM' = "#014d64",
        'Total FCM\nPresort' = "#adadad",
        'Total FCM\nSingle Piece' = "#01a2d9")) +
      scale_x_yearqtr(format = "%Y\nQ%q", 
                      limits = c(min(merge.pieces$fiscal.year.q.1), 
                                 max(merge.pieces$fiscal.year.q.1)),
                      breaks = seq(from = min(merge.pieces$fiscal.year.q.1), 
                                   to = max(merge.pieces$fiscal.year.q.1), by = 1)) +
      scale_y_continuous(labels = unit_format(unit = "B", scale = 1e-9, 
                                              accuracy = 1)) + 
      
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
            legend.position = c(0.9, 0.8),
            strip.background = element_rect(linetype="solid",),
            panel.grid.minor.y = element_line(color = NA),
            plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                    color = "#7B7D7D")) 
    
    #C. Save Graph
    ggsave(paste(graph_path, "1c_4.2--First Class Mail and Subcategory Volumes (Fiscal Yr).png", 
                 sep="/"), graph.4.2, scale = 1, width = 8.5, 
           height = (7.5/1.618), dpi = 300)  
    

#------------------------------------------------------------------------#
#                             Step 5: Graph                              #
#                       Bar Graphs of Percent of FCM                     #
#------------------------------------------------------------------------#    
    
#5.1: Bar graph of volume for total FCM, presort and single piece as a % of FCM
#     Fiscal Year as x-axis
    
    #A. Graph
    graph.5.1 <- merge.pieces.long %>% 
      filter(grepl("Q1", as.character(fiscal.year.q.1)) ) %>%
      mutate(service.category.2 = case_when(
        service.category=="perc.other" ~ "Other",
        service.category=="perc.single" ~ "Single-Piece\nLetters and Cards",
        service.category=="perc.presort" ~ "Presort\nLetters and Cards"
      )) %>%
      
      ggplot(aes(x=fiscal.year.q.1)) +
      
        ##1. Bar Graph and text
        geom_bar(aes(fill=service.category.2, y=percent), position="dodge", 
                 stat="identity") +
        geom_text(aes(fill=service.category.2, y=percent, label=paste0(floor(percent), "%")), 
                  position=position_dodge(width=0.9), vjust=-0.25, size = 2, 
                  hjust=.4) +
        
        ##2. Graph title and Axis labels
        ggtitle(label = "First Class Mail Subcategories as a Percent of First Class Mail",
                subtitle = "Fiscal Years: 2009-2020") +
        labs(caption = 'Note: Percentages in label rounded down to nearest whole number <br/>
                        Source: Quarterly Revenue, Pieces & Weight (RPW) Reports') +
        ylab("Percent of First Class Mail")+
        xlab("") +
        
        ##3. Scales
        scale_fill_manual(values = c(
          'Presort\nLetters and Cards' = "#014d64",
          'Single-Piece\nLetters and Cards' = "#adadad",
          'Other' = "#01a2d9")) +
        scale_x_yearqtr(format = "%Y\nQ%q",
                        breaks = seq(from = min(merge.pieces.long$fiscal.year.q.1), 
                                     to = max(merge.pieces.long$fiscal.year.q.1), by = 1)) +
        scale_y_continuous(labels = percent_format(scale = 1), breaks = seq(0, 70, by=10)) + 
        
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
              legend.position = "bottom",
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 
    
    #C. Save Graph
    ggsave(paste(graph_path, "1c_5.1--FCM Subcategories as a percent of FCM.png",
                 sep="/"), graph.5.1, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)
    
    
#------------------------------------------------------------------------#
#                             Step 6: Graph                              #
#                       Annual FCM and Marketing Mail                    #
#------------------------------------------------------------------------#  

#6.1: Basic line graph of FCM and Marketing mail 
        
    #A. Graph    
    graph.6.1 <- mail.trends %>% 
      ggplot(aes(x=year)) +
      
        ##1. Line
        geom_line( aes(y=total.fcm, color = "Total FCM"), size=1) +
        geom_line( aes(y=total.mm, color = "Total Marketing\nMail"), 
                   size=1) +
        
        ##2. Graph title and Axis labels
        ggtitle(label = "**Mail Volume**") +
        labs(caption = 'Source: Annual RPW Reports from USPS Financials',
             color = 'Key') +
        ylab("Volume (in Billions)")+
        xlab("") +
        
        ##3. Scales
        scale_color_manual(values = c(
          'Total FCM' = "#014d64",
          'Total Marketing\nMail' = "#adadad")) +
        scale_x_continuous(breaks = seq(from = 2001,to = 2021, by = 2)) +
        scale_y_continuous(labels = unit_format(unit = "B", scale = 1e-9, accuracy = 1),
                           limits = c(50000000000, 105000000000)) + 
        
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
              legend.position = c(0.9, 0.8),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 
    
    #C. Save Graph
    ggsave(paste(graph_path, "1c_6.1--First Class Mail and Marketing mail.png", 
                 sep="/"), graph.6.1, scale = 1, width = 7, 
           height = 6, dpi = 300)      
    
    
    