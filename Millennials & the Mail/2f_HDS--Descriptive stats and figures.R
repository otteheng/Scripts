# -> Gerhard O
# -> 1/08/2021
# -> Data sets: 2e_HDS

library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(readxl)
library(data.table)
library(janitor)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(ggsignif)

# Initial Data Sets
raw_data_path_2e <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Clean Data\\HDS\\2e_HDS--Cleaned data"

# Clean Data
clean_data_path <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Clean Data\\HDS\\2f_HDS--Regression & Descriptive Stats"
graph_path <- "C:\\Users\\bg4cn0\\Documents\\Gerhard\\R Projects\\Millennials & Mail\\Tables & Figures\\2f_HDS"

#------------------------------------------------------------------------#
#                         Descriptive Statistics                         #
#                           And Tables/Figures                           #
#------------------------------------------------------------------------#

# Load volume data (RDA)
load(file = paste(raw_data_path_2e, "2e_volume-rects-new_names.rda", sep = '\\'))  

#------------------------------------------------------------------------#
#                 Distribution of Dependent variable                     #
#------------------------------------------------------------------------#
    
    #1: Histogram
    
        #1a: Change margins
        par("mar")
        par(mar=c(1,1,1,1))
        
        #1b: Run Histogram  
        hist(vol.new.names$MAIL.VOLUME.SUM) # No labels for some reason
        
        #1c: ggplot histogram (count)
        hist1 <- ggplot(vol.new.names, aes(x=MAIL.VOLUME.SUM)) +
          geom_histogram(color="black", fill="white")
        hist2 <- hist1 + geom_vline(aes(xintercept=mean(MAIL.VOLUME.SUM)),
                           color="blue", linetype="dashed", size=1)
        
        #1c: ggplot histogram (density)
        hist3 <- ggplot(vol.new.names, aes(x=MAIL.VOLUME.SUM)) + 
          geom_histogram(aes(y=..density..), colour="black", fill="white")+
          geom_density(alpha=.2, fill="#FF6666") 
        
        #1d: Save plots
        ggsave(paste(graph_path, "Hist-Count (MAIL.VOL.SUM).png", 
                     sep="\\"), hist1, width=6, height=4.5)
        ggsave(paste(graph_path, "Hist-Count-Mean (MAIL.VOL.SUM).png", 
                     sep="\\"), hist2, width=6, height=4.5)
        ggsave(paste(graph_path, "Hist-density (MAIL.VOL.SUM).png", 
                     sep="\\"), hist3, width=6, height=4.5)
        
    #2: Descriptive Stats by Year
    mail.sum.desc <- vol.new.names %>%
      group_by(SURVEY.YEAR) %>%
      summarise(n = n(),
                Mean = mean(MAIL.VOLUME.SUM),
                SD = sd(MAIL.VOLUME.SUM),
                Q25 = quantile(MAIL.VOLUME.SUM, 0.25),
                Q50 = median(MAIL.VOLUME.SUM),
                Q75 = quantile(MAIL.VOLUME.SUM, 0.75),
                Min = min(MAIL.VOLUME.SUM),
                Max = max(MAIL.VOLUME.SUM))
    
        #2a: Save 
        df.new.name <- 'Descriptive Stats (MAIL.VOLUME.SUM).csv'       
        write_csv(mail.sum.desc, path = paste(graph_path, df.new.name, sep="\\"),
                  append=FALSE, col_names=TRUE)

#------------------------------------------------------------------------#
#                 Distribution of Independent variables                  #
#------------------------------------------------------------------------#
        
# Look at age cohorts and see if there are differences in marriage rates, 
# children, home ownership. These are the age cohorts that conform the best 
# with the generations. 
        
#--->
#---> Same Age Cohort
#--->
        
    #1a: Same Age cohort two different years
    same05.14 <- vol.new.names %>% filter(SURVEY.YEAR==2005 | 
                                          SURVEY.YEAR==2014, 
                                    AGE.COHORT=="18-24" | 
                                    AGE.COHORT=="25-34" |
                                    AGE.COHORT=="45-54")

    
    #1b: Observe Differences in Marriage Rates
    
        #1b.1: Marriage Rates data
        s.marr <- same05.14 %>%
          group_by(SURVEY.YEAR, AGE.COHORT, MARITAL.STATUS) %>%
          summarise(n = n() ) %>%
          group_by(SURVEY.YEAR, AGE.COHORT, .add = T) %>%
          mutate(percent = round(n/sum(n), 2)) %>%
          mutate(roll.sum = cumsum(percent))
        s.marr$SURVEY.YEAR <- as.character(s.marr$SURVEY.YEAR)
        
        #1b.2: Marriage rates graph
        s.marr.bar <- ggplot(s.marr, aes(x=SURVEY.YEAR, y=percent, 
                             fill=MARITAL.STATUS)) +
          geom_bar(stat = "identity", color = "white") +
          facet_wrap(~AGE.COHORT, nrow=1) +
          labs(title = "% Married by Year & Age Cohort") +
          theme(plot.title = element_text(hjust = .5))
        
        # Contains lines connecting between bar graphs
        s.marr.bar2 <- ggplot(s.marr, aes(x=SURVEY.YEAR, y=percent, 
                           fill=MARITAL.STATUS)) +
          geom_bar(stat = "identity", color = "white", width = 0.5) +
          facet_wrap(~AGE.COHORT, nrow=1) +
          labs(title = "% Married by Year & Age Cohort") +
          theme(plot.title = element_text(hjust = .5)) + 
          geom_area(aes(x = c("2005" = 1.25, "2014" = 1.75)[SURVEY.YEAR]), 
                    position = "fill", colour = "white", alpha = 0.5,
                    outline.type = "full")
    
        #1b.3: Save plots
        ggsave(paste(graph_path, "Same Cohort 2 Yrs (MARITAL.STATUS-2005.2014).png", 
                     sep="\\"), s.marr.bar, width=6, height=4.5)
        ggsave(paste(graph_path, "Same Cohort 2 Yrs (MARITAL.STATUS-2005.2014)(bars connected).png", 
                     sep="\\"), s.marr.bar2, width=6, height=4.5)
    
    #1c: Observe Differences in Marriage Rates
        
        #1c.1: Children Under 18 Rates data
        s.child <- same05.14 %>%
          group_by(SURVEY.YEAR, AGE.COHORT, NUM.UNDER18) %>%
          summarise(n = n() ) %>%
          group_by(SURVEY.YEAR, AGE.COHORT, .add = T) %>%
          mutate(percent = round(n/sum(n), 2)) %>%
          mutate(roll.sum = cumsum(percent))
        s.child$SURVEY.YEAR <- as.character(s.child$SURVEY.YEAR) 
        s.child$NUM.UNDER18 <- as.character(s.child$NUM.UNDER18)

        #1c.2: Children Under 18 rates graph
        s.child.bar <- ggplot(s.child, aes(x=SURVEY.YEAR, y=percent, 
                                         fill=NUM.UNDER18)) +
          geom_bar(stat = "identity", color = "white") +
          facet_wrap(~AGE.COHORT, nrow=1) +
          labs(title = "% w/ Children under 18 by Year & Age Cohort") +
          theme(plot.title = element_text(hjust = .5))

        #1c.3: Save plots
        ggsave(paste(graph_path, "Same Cohort 2 Yrs (NUM.UNDER18-2005.2014).png", 
                     sep="\\"), s.child.bar, width=6, height=4.5)

    #1d: Observe Differences in Home Ownership
        
        #1d.1: Home Ownership Rates data
        s.home <- same05.14 %>%
          group_by(SURVEY.YEAR, AGE.COHORT, HOME.OWNER) %>%
          summarise(n = n() ) %>%
          group_by(SURVEY.YEAR, AGE.COHORT, .add = T) %>%
          mutate(percent = round(n/sum(n), 2)) %>%
          mutate(roll.sum = cumsum(percent))
        s.home$SURVEY.YEAR <- as.character(s.home$SURVEY.YEAR) 
        
        #1d.2: Home Ownership graph
        s.home.bar <- ggplot(s.home, aes(x=SURVEY.YEAR, y=percent, 
                                           fill=HOME.OWNER)) +
          geom_bar(stat = "identity", color = "white") +
          facet_wrap(~AGE.COHORT, nrow=1) +
          labs(title = "% Home Owner/Renter by Year & Age Cohort") +
          theme(plot.title = element_text(hjust = .5))
        
        #1d.3: Save plots
        ggsave(paste(graph_path, "Same Cohort 2 Yrs (HOME.OWNER-2005.2014).png", 
                     sep="\\"), s.home.bar, width=6, height=4.5)
        
    #1e: Observe Differences in Home Ownership
        
        #1e.1: Home Ownership Rates data
        s.home.type <- same05.14 %>%
          group_by(SURVEY.YEAR, AGE.COHORT, HOME.TYPE) %>%
          summarise(n = n() ) %>%
          group_by(SURVEY.YEAR, AGE.COHORT, .add = T) %>%
          mutate(percent = round(n/sum(n), 2)) %>%
          mutate(roll.sum = cumsum(percent)) %>%
          mutate(HOME.TYPE = 
                 recode(HOME.TYPE, "Duplex or townhouse"="Duplex or townhouse*"))
        s.home.type$SURVEY.YEAR <- as.character(s.home.type$SURVEY.YEAR) 
        
        #1e.2: Home Ownership graph
        s.home.type.bar <- ggplot(s.home.type, aes(x=SURVEY.YEAR, y=percent, 
                           fill=HOME.TYPE)) +
          geom_bar(stat = "identity", color = "white") +
          facet_wrap(~AGE.COHORT, nrow=1) +
          labs(title = "% Home Owner/Renter by Year & Age Cohort", 
               caption = '* "Duplex or Townhouse" not asked in 2005') +
          theme(plot.title = element_text(hjust = .5))            
        
        #1e.3: Save plots
        ggsave(paste(graph_path, "Same Cohort 2 Yrs (HOME.TYPE-2005.2014).png", 
                     sep="\\"), s.home.type.bar, width=6, height=4.5)
        

#--->
#---> Different Age Cohort
#--->     
        
    #2a: Same Aged but 11 years in the future
    #    *25-34 in 2015 doesn't fit 100% due to different size of 18-24 bracket
    diff05 <- vol.new.names %>% 
      filter(SURVEY.YEAR==2005 & (AGE.COHORT=="45-54" |
                                   AGE.COHORT=="25-34" |
                                   AGE.COHORT=="18-24"))  
    diff15 <- vol.new.names %>% 
      filter(SURVEY.YEAR==2015 & (AGE.COHORT=="55-64" | 
                                  AGE.COHORT=="35-44" |
                                  AGE.COHORT=="25-34")) 
    diff05.15 <- rbind(diff05, diff15)
        
    #2b: Observe Differences in Marriage Rates
        
        #2b.1: Marriage Rates data
        d.marr <- diff05.15 %>%
          group_by(SURVEY.YEAR, AGE.COHORT, MARITAL.STATUS) %>%
          summarise(n = n() ) %>%
          group_by(SURVEY.YEAR, AGE.COHORT, .add = T) %>%
          mutate(percent = round(n/sum(n), 2)) %>%
          mutate(roll.sum = cumsum(percent)) %>%
          mutate(n.group.age = case_when(
              AGE.COHORT=="45-54" & SURVEY.YEAR==2005 ~ "45-54 10yrs 55-64",
              AGE.COHORT=="55-64" & SURVEY.YEAR==2015 ~ "45-54 10yrs 55-64",
              AGE.COHORT=="25-34" & SURVEY.YEAR==2005 ~ "25-34 10yrs 35-44",
              AGE.COHORT=="35-44" & SURVEY.YEAR==2015 ~ "25-34 10yrs 35-44",
              AGE.COHORT=="18-24" & SURVEY.YEAR==2005 ~ "18-24 10yrs 25-34",
              AGE.COHORT=="25-34" & SURVEY.YEAR==2015 ~ "18-24 10yrs 25-34"))
        d.marr$SURVEY.YEAR <- as.character(d.marr$SURVEY.YEAR)   
        
        #2b.2: Marriage rates graph
        d.marr.bar <- ggplot(d.marr, aes(x=SURVEY.YEAR, y=percent, 
                                         fill=MARITAL.STATUS)) +
          geom_bar(stat = "identity", color = "white") +
          facet_wrap(~n.group.age, nrow=1) +
          labs(title = "% Married by Year & Age Cohort \n (Same Aged 10 yr in Future)") +
          theme(plot.title = element_text(hjust = .5))
        
        #2b.3: Save plots
        ggsave(paste(graph_path, "Cohorts 10yrs later (MARITAL.STATUS-2005.2014).png", 
                     sep="\\"), d.marr.bar, width=6, height=4.5)

    #2c: Observe Differences in Marriage Rates
        
        #2c.1: Children Under 18 Rates data
        d.child <- diff05.15 %>%
          group_by(SURVEY.YEAR, AGE.COHORT, NUM.UNDER18) %>%
          summarise(n = n() ) %>%
          group_by(SURVEY.YEAR, AGE.COHORT, .add = T) %>%
          mutate(percent = round(n/sum(n), 2)) %>%
          mutate(roll.sum = cumsum(percent)) %>%
          mutate(n.group.age = case_when(
            AGE.COHORT=="45-54" & SURVEY.YEAR==2005 ~ "45-54 10yrs 55-64",
            AGE.COHORT=="55-64" & SURVEY.YEAR==2015 ~ "45-54 10yrs 55-64",
            AGE.COHORT=="25-34" & SURVEY.YEAR==2005 ~ "25-34 10yrs 35-44",
            AGE.COHORT=="35-44" & SURVEY.YEAR==2015 ~ "25-34 10yrs 35-44",
            AGE.COHORT=="18-24" & SURVEY.YEAR==2005 ~ "18-24 10yrs 25-34",
            AGE.COHORT=="25-34" & SURVEY.YEAR==2015 ~ "18-24 10yrs 25-34"))
        d.child$SURVEY.YEAR <- as.character(d.child$SURVEY.YEAR) 
        d.child$NUM.UNDER18 <- as.character(d.child$NUM.UNDER18)
        
        #2c.2: Children Under 18 rates graph
        d.child.bar <- ggplot(d.child, aes(x=SURVEY.YEAR, y=percent, 
                                           fill=NUM.UNDER18)) +
          geom_bar(stat = "identity", color = "white") +
          facet_wrap(~n.group.age, nrow=1) +
          labs(title = "% w/ Children under 18 by Year & Age Cohort \n (Same Aged 10 yr in Future)") +
          theme(plot.title = element_text(hjust = .5))
        
        #2c.3: Save plots
        ggsave(paste(graph_path, "Cohorts 10yrs later (NUM.UNDER18-2005.2014).png", 
                     sep="\\"), d.child.bar, width=6, height=4.5)            
        
    #2d: Observe Differences in Home Ownership
        
        #2d.1: Home Ownership Rates data
        d.home <- diff05.15 %>%
          group_by(SURVEY.YEAR, AGE.COHORT, HOME.OWNER) %>%
          summarise(n = n() ) %>%
          group_by(SURVEY.YEAR, AGE.COHORT, .add = T) %>%
          mutate(percent = round(n/sum(n), 2)) %>%
          mutate(roll.sum = cumsum(percent)) %>%
          mutate(n.group.age = case_when(
            AGE.COHORT=="45-54" & SURVEY.YEAR==2005 ~ "45-54 10yrs 55-64",
            AGE.COHORT=="55-64" & SURVEY.YEAR==2015 ~ "45-54 10yrs 55-64",
            AGE.COHORT=="25-34" & SURVEY.YEAR==2005 ~ "25-34 10yrs 35-44",
            AGE.COHORT=="35-44" & SURVEY.YEAR==2015 ~ "25-34 10yrs 35-44",
            AGE.COHORT=="18-24" & SURVEY.YEAR==2005 ~ "18-24 10yrs 25-34",
            AGE.COHORT=="25-34" & SURVEY.YEAR==2015 ~ "18-24 10yrs 25-34"))
        d.home$SURVEY.YEAR <- as.character(d.home$SURVEY.YEAR) 
        
        #2d.2: Home Ownership graph
        d.home.bar <- ggplot(d.home, aes(x=SURVEY.YEAR, y=percent, 
                                         fill=HOME.OWNER)) +
          geom_bar(stat = "identity", color = "white") +
          facet_wrap(~n.group.age, nrow=1) +
          labs(title = "% Home Owner/Renter by Year & Age Cohort \n (Same Aged 10 yr in Future)") +
          theme(plot.title = element_text(hjust = .5))
        
        #2d.3: Save plots
        ggsave(paste(graph_path, "Cohorts 10yrs later (HOME.OWNER-2005.2014).png", 
                     sep="\\"), d.home.bar, width=6, height=4.5)         
        
    #2e: Observe Differences in Home Ownership
            
        #2e.1: Home Ownership Rates data
        d.home.type <- diff05.15 %>%
          group_by(SURVEY.YEAR, AGE.COHORT, HOME.TYPE) %>%
          summarise(n = n() ) %>%
          group_by(SURVEY.YEAR, AGE.COHORT, .add = T) %>%
          mutate(percent = round(n/sum(n), 2)) %>%
          mutate(roll.sum = cumsum(percent)) %>%
          mutate(n.group.age = case_when(
            AGE.COHORT=="45-54" & SURVEY.YEAR==2005 ~ "45-54 10yrs 55-64",
            AGE.COHORT=="55-64" & SURVEY.YEAR==2015 ~ "45-54 10yrs 55-64",
            AGE.COHORT=="25-34" & SURVEY.YEAR==2005 ~ "25-34 10yrs 35-44",
            AGE.COHORT=="35-44" & SURVEY.YEAR==2015 ~ "25-34 10yrs 35-44",
            AGE.COHORT=="18-24" & SURVEY.YEAR==2005 ~ "18-24 10yrs 25-34",
            AGE.COHORT=="25-34" & SURVEY.YEAR==2015 ~ "18-24 10yrs 25-34")) %>%
          mutate(HOME.TYPE = 
            recode(HOME.TYPE, "Duplex or townhouse"="Duplex or townhouse*"))
        d.home.type$SURVEY.YEAR <- as.character(d.home.type$SURVEY.YEAR)
          
        #2e.2: Home Ownership graph
        d.home.type.bar <- ggplot(d.home.type, aes(x=SURVEY.YEAR, y=percent, 
                                                   fill=HOME.TYPE)) +
          geom_bar(stat = "identity", color = "white") +
          facet_wrap(~n.group.age, nrow=1) +
          labs(title = "% Home Owner/Renter by Year & Age Cohort \n (Same Aged 10 yr in Future)",
               caption = '* "Duplex or Townhouse" not asked in 2005') +
          theme(plot.title = element_text(hjust = .5))            
        
        #2e.3: Save plots
        ggsave(paste(graph_path, "Cohorts 10yrs later (HOME.TYPE-2005.2014).png", 
                     sep="\\"), d.home.type.bar, width=6, height=4.5)
            
            
#------------------------------------------------------------------------#
#                       Graph Mail Use Over Time by                      #
#                           Independent Variables                        #
#------------------------------------------------------------------------#   
        
    #3.1: Graph by Age group
        
        #A. Set up Data
        df.age1 <- vol.new.names[, c("SURVEY.YEAR", "MAIL.VOLUME.SUM", 
                                    "AGE.COHORT")]
        df.age1 <- df.age1 %>%
          filter(AGE.COHORT=="18-24" | AGE.COHORT=="25-34" | 
                 AGE.COHORT=="35-44" | AGE.COHORT=="45-54") 
        mean.data <- aggregate(x = df.age1$MAIL.VOLUME.SUM, 
                                by = list(df.age1$SURVEY.YEAR, df.age1$AGE.COHORT),
                                FUN = 'mean')
        colnames(mean.data) <- c("Year", "Age.Cohort", "Mean")
        median.data <- aggregate(x = df.age1$MAIL.VOLUME.SUM, 
                               by = list(df.age1$SURVEY.YEAR, df.age1$AGE.COHORT),
                               FUN = 'median')
        colnames(median.data) <- c("Year", "Age.Cohort", "Median")
        df.agg <- merge(mean.data, median.data, by = c("Year", "Age.Cohort"))
        df.agg$Mean.x <- round(df.agg$Mean.x, digits = 2)
        colnames(df.agg) <- c("Year", "Age.Cohort", "Mean", "Median")
        
        # Change order of variable for graph
        df.agg$Age.Cohort <- factor(df.agg$Age.Cohort, 
                                    levels = rev(levels(df.agg$Age.Cohort))) 
        
        #B. Mean Mail Volume by Age Cohort
        mean.by.age.graph <- df.agg %>%
          ggplot( aes(x=Year, y=Mean, group=Age.Cohort, color=Age.Cohort)) +
          scale_x_continuous(n.breaks = 10) +
          scale_y_continuous(limits = c(4,32), breaks = (seq(0, 30, 5))) +
          geom_line(size = 1) +
          theme_minimal() +
          ggtitle("Mean Mail Volume by Age Cohort") +
          ylab("Mean Mail Volume") + 
          scale_color_manual(values=c("#01a2d9","#76c0c1", "#6794a7", 
                                      "#014d64")) + 
          theme(plot.title = element_text(color="black", size=12, hjust = .5),
                axis.title.x = element_blank(), axis.title.y = 
                  element_text(size = 9), 
                legend.box.background = element_rect(color="black", size=.5),
                legend.title = element_text(size = 8),
                legend.text = element_text(size = 7)) +
          geom_signif(annotation="18-24 = Millennial", y_position=5, xmin=2005, 
                      xmax=2014, tip_length = c(0.04, 0.04), color = "#014d64", 
                      textsize = 3) + 
          geom_signif(annotation="25-34 = Millennial", y_position=5, xmin=2015, 
                      xmax=2019, tip_length = c(0.04, 0.04), color = "#6794a7", 
                      textsize = 3) +
          geom_signif(annotation="25-34 = Gen X", y_position=7, xmin=2000, 
                      xmax=2005, tip_length = c(0.04, 0.04), color = "#6794a7", 
                      textsize = 3) +
          geom_signif(annotation="35-44 = Gen X", y_position=7, xmin=2009, 
                    xmax=2015, tip_length = c(0.04, 0.04), color = "#76c0c1", 
                    textsize = 3) +
          geom_signif(annotation="45-54 = Boomer", y_position=9, xmin=2000, 
                      xmax=2009, tip_length = c(0.04, 0.04), color = "#01a2d9", 
                      textsize = 3)
        ggsave(paste(graph_path, 
                     "Mean Mail Volume by Age Cohort.png", 
                     sep="\\"), mean.by.age.graph, width=6, height=4.5)
      
        #C. Median Mail Volume by Age Cohort
        median.by.age.graph <- df.agg %>%
          ggplot( aes(x=Year, y=Median, group=Age.Cohort, color=Age.Cohort)) +
          scale_x_continuous(n.breaks = 10) +
          scale_y_continuous(limits = c(2,32), breaks = (seq(0, 30, 5))) +
          geom_line(size = 1) +
          theme_minimal() +
          ggtitle("Median Mail Volume by Age Cohort") +
          ylab("Median Mail Volume") + 
          scale_color_manual(values=c("#01a2d9","#76c0c1", "#6794a7", 
                                      "#014d64")) + 
          theme(plot.title = element_text(color="black", size=12, hjust = .5),
                axis.title.x = element_blank(), axis.title.y = 
                  element_text(size = 9), 
                legend.box.background = element_rect(color="black", size=.5),
                legend.title = element_text(size = 8),
                legend.text = element_text(size = 7)) +
          geom_signif(annotation="18-24 = Millennial", y_position=3, xmin=2005, 
                      xmax=2014, tip_length = c(0.04, 0.04), color = "#014d64", 
                      textsize = 3) + 
          geom_signif(annotation="25-34 = Millennial", y_position=3, xmin=2015, 
                      xmax=2019, tip_length = c(0.04, 0.04), color = "#6794a7", 
                      textsize = 3) +
          geom_signif(annotation="25-34 = Gen X", y_position=5, xmin=2000, 
                      xmax=2005, tip_length = c(0.04, 0.04), color = "#6794a7", 
                      textsize = 3) +
          geom_signif(annotation="35-44 = Gen X", y_position=5, xmin=2009, 
                      xmax=2015, tip_length = c(0.04, 0.04), color = "#76c0c1", 
                      textsize = 3) +
          geom_signif(annotation="45-54 = Boomer", y_position=7, xmin=2000, 
                      xmax=2009, tip_length = c(0.04, 0.04), color = "#01a2d9", 
                      textsize = 3)
        ggsave(paste(graph_path, 
                     "Median Mail Volume by Age Cohort.png", 
                     sep="\\"), median.by.age.graph, width=6, height=4.5)

    #3.2: Packages Sent       
        
        #A. Set up Data
        df.pkgr1 <- vol.new.names[, c("SURVEY.YEAR", "MAIL.VOLUME.PKGR_PRST_NP", 
                                     "AGE.COHORT")]
        df.pkgr1 <- df.pkgr1 %>%
          filter(AGE.COHORT=="18-24" | AGE.COHORT=="25-34" | 
                   AGE.COHORT=="35-44" | AGE.COHORT=="45-54") 
        mean.data <- aggregate(x = df.pkgr1$MAIL.VOLUME.PKGR_PRST_NP, 
                               by = list(df.pkgr1$SURVEY.YEAR, df.pkgr1$AGE.COHORT),
                               FUN = 'mean')
        colnames(mean.data) <- c("Year", "Age.Cohort", "Mean")
        median.data <- aggregate(x = df.pkgr1$MAIL.VOLUME.PKGR_PRST_NP, 
                                 by = list(df.pkgr1$SURVEY.YEAR, df.pkgr1$AGE.COHORT),
                                 FUN = 'median')
        colnames(median.data) <- c("Year", "Age.Cohort", "Mean")
        df.agg.pkgr <- merge(mean.data, median.data, by = c("Year", "Age.Cohort"))
        df.agg.pkgr$Mean.x <- round(df.agg.pkgr$Mean.x, digits = 2)
        colnames(df.agg.pkgr) <- c("Year", "Age.Cohort", "Mean", "Median")
        
        # Change order of variable for graph
        df.agg.pkgr$Age.Cohort <- factor(df.agg.pkgr$Age.Cohort, 
                                    levels = rev(levels(df.agg.pkgr$Age.Cohort))) 
        
        
        #B. Mean Package Volume by Age Cohort
        mean.pkgr.by.age.graph <- df.agg.pkgr %>%
          ggplot( aes(x=Year, y=Mean, group=Age.Cohort, color=Age.Cohort)) +
          scale_x_continuous(n.breaks = 10) +
          scale_y_continuous(limits = c(0,1.5), breaks = (seq(0, 1.5, .2))) +
          geom_line(size = 1) +
          theme_minimal() +
          ggtitle("Mean Package Volume by Age Cohort") +
          ylab("Mean Package Volume") + 
          scale_color_manual(values=c("#01a2d9","#76c0c1", "#6794a7", 
                                      "#014d64")) + 
          theme(plot.title = element_text(color="black", size=12, hjust = .5),
                axis.title.x = element_blank(), axis.title.y = 
                  element_text(size = 9), 
                legend.box.background = element_rect(color="black", size=.5),
                legend.title = element_text(size = 8),
                legend.text = element_text(size = 7)) +
          geom_signif(annotation="18-24 = Millennial", y_position=.05, xmin=2005, 
                      xmax=2014, tip_length = c(0.04, 0.04), color = "#014d64", 
                      textsize = 3) + 
          geom_signif(annotation="25-34 = Millennial", y_position=.05, xmin=2015, 
                      xmax=2019, tip_length = c(0.04, 0.04), color = "#6794a7", 
                      textsize = 3) +
          geom_signif(annotation="25-34 = Gen X", y_position=.14, xmin=2000, 
                      xmax=2005, tip_length = c(0.04, 0.04), color = "#6794a7", 
                      textsize = 3) +
          geom_signif(annotation="35-44 = Gen X", y_position=.14, xmin=2009, 
                      xmax=2015, tip_length = c(0.04, 0.04), color = "#76c0c1", 
                      textsize = 3) +
          geom_signif(annotation="45-54 = Boomer", y_position=.2, xmin=2000, 
                      xmax=2009, tip_length = c(0.04, 0.04), color = "#01a2d9", 
                      textsize = 3)
        ggsave(paste(graph_path, 
                     "Mean Package Volume by Age Cohort.png", 
                     sep="\\"), mean.pkgr.by.age.graph, width=6, height=4.5)