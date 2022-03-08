# -> Gerhard O
# -> 9/29/2021
# -> Data sets: HDS
# -> Build Figures for Jessica of Marketing mail and Non-Profit

# Load Packages in Library
source('C:\\Users\\bg4cn0\\USPS\\ARTSI Team Folder - Documents\\Projects\\library_GSO.R')

# Data Paths
raw_data_path <- "C:\\Users\\bg4cn0\\USPS\\ARTSI Team Folder - Documents\\Projects\\Millennials & Mail\\Code\\Clean Data\\HDS"
graph_path <- "C:\\Users\\bg4cn0\\USPS\\ARTSI Team Folder - Documents\\Projects\\Mail Floor\\Code\\Tables & Figures\\3a"

#------------------------------------------------------------------------#
#                           Step 1: Load in Data                         #
#                                                                        #
#------------------------------------------------------------------------#

#1.1: Final Clean HDS data created as part of "Millennials and the Mail" project. 

    #A. HDS Received
    hds_raw <- read.csv(paste(raw_data_path, "2e_HDS--Cleaned data\\2e_volume-rects-new_names.csv",
                              sep = "\\"))
#1.2: Uncleaned HDS roster data
    
    #A. HDS Adult Roster (Selection of internet questions)
    rects_internet_raw <- fread(paste(raw_data_path, "2a_HDS\\2a_rects_final.csv", sep = '\\'), 
                             select = c(1, 27, 114, 116))
    
        ##1. Column Names as data frame
        rects_internet_raw_col <- as.data.frame(colnames(rects_internet_raw))

#------------------------------------------------------------------------#
#                           Step 2: Light Clean                          #
#                              Final HDS Data                            #
#------------------------------------------------------------------------#

#2.1: Data checks

    #A. Number of NAs by column
    df.na.all <- hds_raw %>%
      summarise(across(everything(), ~ sum(is.na(.x))))
    
    #B. Number of NAs by column and year
    df.na.yr.all <- hds_raw %>%
      group_by(SURVEY.YEAR) %>%
      summarise(across(everything(), ~ sum(is.na(.x))))
    
    #C. Number of NAs as a % of total observations by column and year
    df.na.yr.perc.all <- hds_raw %>%
      group_by(SURVEY.YEAR) %>%
      summarise(across(everything(), ~ round(100*mean(is.na(.x)), 2) ) )

#2.2: New data sets

    #A. Marketing and Non-Project Received by Age cohort
    hds.r <- hds_raw %>%
      dplyr::select(SAMPLE.NUMBER, SURVEY.YEAR, MAIL.VOLUME.BRMR_PRST, 
                    MAIL.VOLUME.NPMR_PRST, AGE.COHORT, GENDER, INC.LEVEL,
                    EDUCATION, NUM.HOUSEHOLD)

#2.3: Get average by given variable(s)

    #A. Collapse variable into smaller categories
    
        ##1. Age cohort
        hds.r <- hds.r %>%
          mutate(AGE.COHORT2 = case_when(
            AGE.COHORT=="18-24" | AGE.COHORT=="25-34" ~ "18-34",
            AGE.COHORT=="65-69" ~ "65+",
            AGE.COHORT=="70-74" ~ "65+",
            AGE.COHORT=="70+" ~ "65+",
            AGE.COHORT=="75+" ~ "65+",
            TRUE ~ as.character(AGE.COHORT)
          ))
        
        ##2. Education
        hds.r <- hds.r %>%
          mutate(EDUCATION2 = case_when(
            EDUCATION=="8th grade or less" | EDUCATION == "Some high school" |
            EDUCATION=="High school graduate" ~ "HS or Less", 
            EDUCATION=="College graduate" | EDUCATION== "Post-graduate work" ~ 
              "BS or Higher",
            TRUE ~ EDUCATION
          ))
        hds.r$EDUCATION2 <- factor(hds.r$EDUCATION2, 
                                      levels = c("HS or Less", 
                                                 "Some college", 
                                                 "Technical school graduate",
                                                 "BS or Higher"))
        
        ##3. Education (fewer categories)
        hds.r <- hds.r %>%
          mutate(EDUCATION3 = case_when(
            EDUCATION=="8th grade or less" | EDUCATION == "Some high school" |
            EDUCATION=="High school graduate" | EDUCATION=="Some college" |
            EDUCATION=="Technical school graduate"  ~ "Less than a BS", 
            EDUCATION=="College graduate" | EDUCATION== "Post-graduate work" ~ 
              "BS or Higher",
            TRUE ~ EDUCATION
          ))
        
        ##4. Number in household
        hds.r <- hds.r %>%
          mutate(NUM.HOUSEHOLD2 = ifelse(NUM.HOUSEHOLD > 6, 7, NUM.HOUSEHOLD),
                 NUM.HOUSEHOLD2 = as.character(NUM.HOUSEHOLD2)
          )

    #B. Get average by Age cohort and Year
    
        ##1. Marketing Mail
        mean.r.brmr <- aggregate(x = hds.r$MAIL.VOLUME.BRMR_PRST, 
                            by = list(hds.r$SURVEY.YEAR, hds.r$AGE.COHORT2),
                            FUN = 'mean')
        colnames(mean.r.brmr) <- c("Year", "Age.Cohort", "Mean.Marketing")
        mean.r.brmr$Mean.Marketing <- round(mean.r.brmr$Mean.Marketing, 2)
        
        ##2. Non-Profit Mail
        mean.r.npmr <- aggregate(x = hds.r$MAIL.VOLUME.NPMR_PRST, 
                                 by = list(hds.r$SURVEY.YEAR, hds.r$AGE.COHORT2),
                                 FUN = 'mean')
        colnames(mean.r.npmr) <- c("Year", "Age.Cohort", "Mean.Nonprofit")
        mean.r.npmr$Mean.Nonprofit <- round(mean.r.npmr$Mean.Nonprofit, 2)
        
        ##3. Merge together
        mean.r <- merge(mean.r.brmr, mean.r.npmr, by = c("Year", "Age.Cohort"))
        
    #C. Get average by gender
        
        ##1. Non-Profit Mail
        hds.r[hds.r == 'DF'] <- NA
        mean.r.npmr.gender <- aggregate(x = hds.r$MAIL.VOLUME.NPMR_PRST, 
                                 by = list(hds.r$SURVEY.YEAR, hds.r$GENDER),
                                 FUN = 'mean')
        colnames(mean.r.npmr.gender) <- c("Year", "Gender", "Mean.Nonprofit")
        mean.r.npmr.gender$Mean.Nonprofit <- round(mean.r.npmr.gender$Mean.Nonprofit, 2)
        
    #D. Get average by gender and age cohort
        
        ##1. Non-profit Mail
        mean.r.npmr.gender.age <- aggregate(x = hds.r$MAIL.VOLUME.NPMR_PRST, 
                                        by = list(hds.r$SURVEY.YEAR, hds.r$AGE.COHORT2,
                                                  hds.r$GENDER),
                                        FUN = 'mean')
        colnames(mean.r.npmr.gender.age) <- c("Year", "Age.Cohort", "Gender", "Mean.Nonprofit")
        mean.r.npmr.gender.age$Mean.Nonprofit <- round(mean.r.npmr.gender.age$Mean.Nonprofit, 2)
        
    #E. Get average by income 
        
        ##1. Non-profit Mail
        mean.r.npmr.inc <- aggregate(x = hds.r$MAIL.VOLUME.NPMR_PRST, 
                                     by = list(hds.r$SURVEY.YEAR, hds.r$INC.LEVEL),
                                     FUN = 'mean')
        colnames(mean.r.npmr.inc) <- c("Year", "Income", "Mean.Nonprofit")
        mean.r.npmr.inc$Mean.Nonprofit <- round(mean.r.npmr.inc$Mean.Nonprofit, 2)
        mean.r.npmr.inc$Income <- factor(mean.r.npmr.inc$Income, 
                                      levels = c("$20,000 - $24,999", 
                                                 "$25,000 - $34,999", 
                                                 "$35,000 - $49,999",
                                                 "$50,000 - $64,999",
                                                 "$65,000 - $79,999",
                                                 "$80,000 - $99,999",
                                                 "$100,000 or more"))
        
    #F. Get average by education
        
        ##1. Non-profit Mail
        mean.r.npmr.educ <- aggregate(x = hds.r$MAIL.VOLUME.NPMR_PRST, 
                                            by = list(hds.r$SURVEY.YEAR, hds.r$EDUCATION2),
                                            FUN = 'mean')
        colnames(mean.r.npmr.educ) <- c("Year", "Education", "Mean.Nonprofit")
        mean.r.npmr.educ$Mean.Nonprofit <- round(mean.r.npmr.educ$Mean.Nonprofit, 2)
        
    #G. Get average by education and age
        
        ##1. Non-profit Mail
        mean.r.npmr.educ.age <- aggregate(x = hds.r$MAIL.VOLUME.NPMR_PRST, 
                                      by = list(hds.r$SURVEY.YEAR, hds.r$AGE.COHORT2,
                                                hds.r$EDUCATION3),
                                      FUN = 'mean')
        colnames(mean.r.npmr.educ.age) <- c("Year", "Age.Cohort", "Education", "Mean.Nonprofit")
        mean.r.npmr.educ.age$Mean.Nonprofit <- round(mean.r.npmr.educ.age$Mean.Nonprofit, 2)
        
    #G. Get average by size of household
    
        ##1. Non-profit Mail
        mean.r.npmr.house <- aggregate(x = hds.r$MAIL.VOLUME.NPMR_PRST, 
                                          by = list(hds.r$SURVEY.YEAR, hds.r$NUM.HOUSEHOLD2),
                                          FUN = 'mean')
        colnames(mean.r.npmr.house) <- c("Year", "Household.Size", "Mean.Nonprofit")
        mean.r.npmr.house$Mean.Nonprofit <- round(mean.r.npmr.house$Mean.Nonprofit, 2)
        

#2.4: Year to Year Percentage change of means

    #A. Marketing Mail
    mean.r <- mean.r %>%
      group_by(Age.Cohort, .add = T) %>%
      mutate(percent.change.brmr = round(((Mean.Marketing - lag(Mean.Marketing)) / 
                                            lag(Mean.Marketing))*100,2),
             percent.change.brmr = if_else(is.na(percent.change.brmr), 0, 
                                           percent.change.brmr)) 
    
    #B. Non-Profit Mail
    mean.r <- mean.r %>%
      group_by(Age.Cohort, .add = T) %>%
      mutate(percent.change.npmr = round(((Mean.Nonprofit - lag(Mean.Nonprofit)) / 
                                            lag(Mean.Nonprofit))*100,2),
             percent.change.npmr = if_else(is.na(percent.change.npmr), 0, 
                                           percent.change.npmr)) 

#2.5: Percentage change from the year 2000

    #A. Received
    mean.r <- mean.r %>%
      group_by(Age.Cohort, .add = T) %>%
      mutate(First = head(Mean.Marketing, 1),
             BaseChange.brmr = case_when(Mean.Marketing != First ~ 
                                        (Mean.Marketing - First) * 100 / First,
                                        TRUE ~ 1 * NA),
             BaseChange.brmr = round(BaseChange.brmr, 2),
             BaseChange.brmr = if_else(is.na(BaseChange.brmr), 0, BaseChange.brmr)) %>%
      dplyr::select(-First)
    
    #B. Sent
    mean.r <- mean.r %>%
      group_by(Age.Cohort, .add = T) %>%
      mutate(First = head(Mean.Nonprofit, 1),
             BaseChange.npmr = case_when(Mean.Nonprofit != First ~ 
                                           (Mean.Nonprofit - First) * 100 / First,
                                         TRUE ~ 1 * NA),
             BaseChange.npmr = round(BaseChange.npmr, 2),
             BaseChange.npmr = if_else(is.na(BaseChange.npmr), 0, BaseChange.npmr)) %>%
      dplyr::select(-First)
    
#------------------------------------------------------------------------#
#                             Step 3: Graph                              #
#                        Graphs of Volume Over time                      #
#------------------------------------------------------------------------#

#3.1: Marketing Mail Received by Age Cohort over time

    #A. Graph
    graph.3.1 <- ggplot() + 
    
        ##1. Line graph
        geom_line(data = mean.r, aes(x=Year, y=Mean.Marketing, group=Age.Cohort, 
                                     color=Age.Cohort), size = 1) +
        
        ##2. Graph title and Axis labels
        ggtitle(label = "Change in **Marketing Mail** Received Volume",
                subtitle = "Average of Respondents Weekly Totals") +
        labs(caption = 'Source: Household Diary Survey',
             color = 'Age Cohort') +
        ylab("Mean Volume Received") + 
        xlab("") +
        
        ##3. Scales
        scale_color_manual(values=c("#e3120b", "#336666", "#FB9851", "#acc8d4", 
                                    "#dbcc98")) + 
        scale_x_continuous(limits = c(2000, 2019) , breaks = (seq(2000, 2019, 2))) +
        scale_y_continuous(limits = c(0, 16), 
                           breaks = seq(0, 16, by=2)) +
        
        ##4. Theme
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
              legend.position = c(0.13, 0.25),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 
        
    #B Save Graph
    ggsave(paste(graph_path, "3a_3.1--Weekly Mean Marketing Mail Received by Age Cohort.png",
                 sep="\\"), graph.3.1, scale = 1, width = 8.5,
                 height = (7.5/1.618), dpi = 300)  
    
#3.2: Non-profit by Age Cohort over time
    
    #A. Graph
    graph.3.2 <- ggplot() + 
      
        ##1. Line graph
        geom_line(data = mean.r, aes(x=Year, y=Mean.Nonprofit, group=Age.Cohort, 
                                     color=Age.Cohort), size = 1) +
        
        ##2. Graph title and Axis labels
        ggtitle(label = "Change in **Non-Profit Mail** Received Volume",
                subtitle = "Average of Respondents Weekly Totals") +
        labs(caption = 'Source: Household Diary Survey',
             color = 'Age Cohort') +
        ylab("Mean Volume Received") + 
        xlab("") +
        
        ##3. Scales
        scale_color_manual(values=c("#e3120b", "#336666", "#FB9851", "#acc8d4", 
                                    "#dbcc98")) + 
        scale_x_continuous(limits = c(2000, 2019) , breaks = (seq(2000, 2019, 2))) +
        scale_y_continuous(limits = c(.5, 5), 
                           breaks = seq(.5, 5, by=.5)) +
        
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
              legend.position = c(0.85, 0.84),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 
    
    #B Save Graph
    ggsave(paste(graph_path, "3a_3.2--Weekly Mean Non-Profit Mail Received by Age Cohort.png",
                 sep="\\"), graph.3.2, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)    
    
#3.3: Non-profit by Gender over time

    #A. Graph
    graph.3.3 <- ggplot() + 
      
          ##1. Line graph
          geom_line(data = mean.r.npmr.gender, aes(x=Year, y=Mean.Nonprofit, 
                                                   group=Gender, 
                                       color=Gender), size = 1) +
          
          ##2. Graph title and Axis labels
          ggtitle(label = "Change in **Non-Profit Mail** Received Volume by Gender",
                  subtitle = "Average of Respondents Weekly Totals") +
          labs(caption = 'Source: Household Diary Survey',
               color = 'Gender') +
          ylab("Mean Volume Received") + 
          xlab("") +
          
          ##3. Scales
          scale_color_manual(values=c("#e3120b", "#336666")) + 
          scale_x_continuous(limits = c(2000, 2019) , breaks = (seq(2000, 2019, 2))) +
          scale_y_continuous(limits = c(2, 4), 
                             breaks = seq(2, 4, by=.25)) +
          
          ##4. Theme
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
                legend.position = c(0.85, 0.84),
                strip.background = element_rect(linetype="solid",),
                panel.grid.minor.y = element_line(color = NA),
                plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                        color = "#7B7D7D")) 
        
    #B Save Graph
    ggsave(paste(graph_path, "3a_3.3--Weekly Mean Non-Profit Mail Received by Gender.png",
                 sep="\\"), graph.3.3, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)    
    
    
#3.4: Non-profit by Gender and Age over time

    #A. Graph
    graph.3.4 <- ggplot() + 
      
        ##1. Line graph
        geom_line(data = mean.r.npmr.gender.age, aes(x=Year, y=Mean.Nonprofit, 
                                                 group=Age.Cohort, 
                                                 color=Age.Cohort), size = 1) +
        
        ##2. Graph title and Axis labels
        ggtitle(label = "Change in **Non-Profit Mail** Received Volume by Gender and Age Cohort",
                subtitle = "Average of Respondents Weekly Totals") +
        labs(caption = 'Source: Household Diary Survey',
             color = 'Age Cohort: ') +
        ylab("Mean Volume Received") + 
        xlab("") +
      
        ##3. Facets
        facet_rep_grid(. ~ Gender, repeat.tick.labels = 'left') +
        coord_capped_cart(bottom='both', left='both') +
        
        ##4. Scales
        scale_color_manual(values=c("#e3120b", "#336666", "#FB9851", "#acc8d4", 
                                    "#dbcc98")) + 
        scale_x_continuous(limits = c(2000, 2019) , breaks = (seq(2000, 2019, 2))) +
        scale_y_continuous(limits = c(0, 6), 
                           breaks = seq(0, 6, by=.5)) +
        
        ##5. Theme
        guides(col = guide_legend(ncol = 5, nrow = 1)) +
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
              legend.position = "bottom", 
              legend.margin = margin(2, 2, 2, 2),
              legend.spacing.x = unit(2, "mm"),
              legend.spacing.y = unit(2, "mm"),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 
    
    #B Save Graph
    ggsave(paste(graph_path, "3a_3.4--Weekly Mean Non-Profit Mail Received by Gender and Age.png",
                 sep="\\"), graph.3.4, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)   
    
#3.5: Non-profit by Gender and Age over time

    #A. Graph
    graph.3.5 <- ggplot(mean.r.npmr.gender.age, aes(x=Year, y=Mean.Nonprofit, group=Age.Cohort, 
                            color=Age.Cohort)) + 
      
        ##1. Line graph
        geom_line(data = mean.r.npmr.gender.age %>% filter(Gender=="Male"), 
                  size = 1) +
        geom_line(data = mean.r.npmr.gender.age %>% filter(Gender=="Female"), 
                  size = 1, linetype = "dashed") +
      
        ##2. Annotation
        annotate("label", x=2016, y=5.5, label = "Solid Line: Male\nDashed Line: Female", 
                 size = 3) +
        
        ##3. Graph title and Axis labels
        ggtitle(label = "Change in **Non-Profit Mail** Received Volume by Gender and Age Cohort",
                subtitle = "Average of Respondents Weekly Totals") +
        labs(caption = 'Source: Household Diary Survey',
             color = 'Age Cohort: ') +
        ylab("Mean Volume Received") + 
        xlab("") +
        
        ##4. Scales
        scale_color_manual(values=c("#e3120b", "#336666", "#FB9851", "#acc8d4", 
                                    "#dbcc98")) + 
        scale_x_continuous(limits = c(2000, 2019) , breaks = (seq(2000, 2019, 2))) +
        scale_y_continuous(limits = c(0, 6), 
                           breaks = seq(0, 6, by=.5)) +
        
        ##5. Theme
        guides(col = guide_legend(ncol = 5, nrow = 1)) +
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
              legend.position = "bottom", 
              legend.margin = margin(2, 2, 2, 2),
              legend.spacing.x = unit(2, "mm"),
              legend.spacing.y = unit(2, "mm"),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 
      
    #B Save Graph
    ggsave(paste(graph_path, "3a_3.5--Weekly Mean Non-Profit Mail Received by Gender and Age.png",
                 sep="\\"), graph.3.5, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)   
    
#3.6: Non-profit by Income over time

    #A. Graph
    graph.3.6 <- ggplot() + 
      
        ##1. Line graph
        geom_line(data = mean.r.npmr.inc, aes(x=Year, y=Mean.Nonprofit, 
                                                 group=Income, 
                                                 color=Income), size = 1) +
        
        ##2. Graph title and Axis labels
        ggtitle(label = "Change in **Non-Profit Mail** Received Volume by Income",
                subtitle = "Average of Respondents Weekly Totals") +
        labs(caption = 'Source: Household Diary Survey',
             color = 'Income Brackets') +
        ylab("Mean Volume Received") + 
        xlab("") +
        
        ##3. Scales
        scale_color_manual(values=c("#e3120b", "#336666", "#FB9851", "#acc8d4", 
                                    "#dbcc98", "#36E2BD", "#F9D2DB")) +
        scale_x_continuous(limits = c(2000, 2019) , breaks = (seq(2000, 2019, 2))) +
        scale_y_continuous(limits = c(1, 5), 
                           breaks = seq(1, 5, by=.5)) +
        
        ##4. Theme
        guides(col = guide_legend(ncol = 4, nrow = 2)) +
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
              legend.position = "bottom",
              legend.title.align=0.5,
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 
      
    #B Save Graph
    ggsave(paste(graph_path, "3a_3.6--Weekly Mean Non-Profit Mail Received by Income.png",
                 sep="\\"), graph.3.6, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)    
    

#3.7: Non-profit by Education over time

    #A. Graph
    graph.3.7 <- ggplot() + 
      
        ##1. Line graph
        geom_line(data = mean.r.npmr.educ, aes(x=Year, y=Mean.Nonprofit, 
                                              group=Education, 
                                              color=Education), size = 1) +
        
        ##2. Graph title and Axis labels
        ggtitle(label = "Change in **Non-Profit Mail** Received Volume by Education",
                subtitle = "Average of Respondents Weekly Totals") +
        labs(caption = 'Source: Household Diary Survey',
             color = '') +
        ylab("Mean Volume Received") + 
        xlab("") +
        
        ##3. Scales
        scale_color_manual(values=c("#e3120b", "#336666", "#FB9851", "#acc8d4")) +
        scale_x_continuous(limits = c(2000, 2019) , breaks = (seq(2000, 2019, 2))) +
        scale_y_continuous(limits = c(1, 4), 
                           breaks = seq(1, 4, by=.5)) +
        
        ##4. Theme
        guides(col = guide_legend(ncol = 4, nrow = 1)) +
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
              legend.position = "bottom",
              #legend.title.align=0.5,
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 
      
    #B Save Graph
    ggsave(paste(graph_path, "3a_3.7--Weekly Mean Non-Profit Mail Received by Education.png",
                 sep="\\"), graph.3.7, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)  
    
#3.8: Non-profit by Education and Age over time

    #A. Graph
    graph.3.8 <- ggplot(mean.r.npmr.educ.age, aes(x=Year, y=Mean.Nonprofit, 
                                                  group=Age.Cohort, 
                                                  color=Age.Cohort)) + 
      
        ##1. Line graph
        geom_line(data = mean.r.npmr.educ.age %>% filter(Education=="BS or Higher"), 
                  size = 1) +
        geom_line(data = mean.r.npmr.educ.age %>% filter(Education=="Less than a BS"), 
                  size = 1, linetype = "dashed") +
        
        ##2. Annotation
        annotate("label", x=2016.5, y=6, label = "Solid Line: BS or Higher\nDashed Line: Less than a BS", 
                 size = 3) +
        
        ##3. Graph title and Axis labels
        ggtitle(label = "Change in **Non-Profit Mail** Received Volume by Education and Age Cohort",
                subtitle = "Average of Respondents Weekly Totals") +
        labs(caption = 'Source: Household Diary Survey',
             color = 'Age Cohort: ') +
        ylab("Mean Volume Received") + 
        xlab("") +
        
        ##4. Scales
        scale_color_manual(values=c("#e3120b", "#336666", "#FB9851", "#acc8d4", 
                                    "#dbcc98")) + 
        scale_x_continuous(limits = c(2000, 2019) , breaks = (seq(2000, 2019, 2))) +
        scale_y_continuous(limits = c(0, 7), 
                           breaks = seq(0, 7, by=.5)) +
        
        ##5. Theme
        guides(col = guide_legend(ncol = 5, nrow = 1)) +
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
              legend.position = "bottom", 
              legend.margin = margin(2, 2, 2, 2),
              legend.spacing.x = unit(2, "mm"),
              legend.spacing.y = unit(2, "mm"),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 
      
    #B Save Graph
    ggsave(paste(graph_path, "3a_3.8--Weekly Mean Non-Profit Mail Received by Education and Age.png",
                 sep="\\"), graph.3.8, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)   

    
#3.9: Non-profit by Household Size over time

    #A. Graph
    graph.3.9 <- ggplot() + 
        
        ##1. Line graph
        geom_line(data = mean.r.npmr.house, aes(x=Year, y=Mean.Nonprofit, 
                                               group=Household.Size, 
                                               color=Household.Size), size = 1) +
        
        ##2. Graph title and Axis labels
        ggtitle(label = "Change in **Non-Profit Mail** Received Volume by Household Size",
                subtitle = "Average of Respondents Weekly Totals") +
        labs(caption = 'Source: Household Diary Survey',
             color = 'Household Size: ') +
        ylab("Mean Volume Received") + 
        xlab("") +
        
        ##3. Scales
        scale_color_manual(values=c("#e3120b", "#336666", "#FB9851", "#acc8d4", 
                                    "#dbcc98", "#36E2BD", "#F9D2DB")) +
        scale_x_continuous(limits = c(2000, 2019) , breaks = (seq(2000, 2019, 2))) +
        scale_y_continuous(limits = c(1, 4), 
                           breaks = seq(1, 4, by=.5)) +
        
        ##4. Theme
        guides(col = guide_legend(ncol = 7, nrow = 1)) +
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
              legend.position = "bottom",
              #legend.title.align=0.5,
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 
    
    #B Save Graph
    ggsave(paste(graph_path, "3a_3.9--Weekly Mean Non-Profit Mail Received by Household Size.png",
                 sep="\\"), graph.3.9, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)  

#------------------------------------------------------------------------#
#                             Step 4: Graph                              #
#                        Percentage Change Since 2000                    #
#------------------------------------------------------------------------#    
        
#4.1: Percentage Change in Marketing Mail Received by Age Cohort over time
    
    #A. Graph
    graph.4.1 <- ggplot() + 
      
        ##1. Line graph
        geom_line(data = mean.r, aes(x=Year, y=BaseChange.brmr, group=Age.Cohort, 
                                     color=Age.Cohort), size = 1) +
        geom_hline(yintercept = 0, color = "#1C2833") +
        
        ##2. Graph title and Axis labels
        ggtitle(label = "Percentage Change in **Marketing Mail** Volume Since 2000",
                subtitle = "Average of Respondents Weekly Totals") +
        labs(caption = 'Source: Household Diary Survey',
             color = 'Age Cohort') +
        ylab("%\u0394 in Average Weekly Volume") + 
        xlab("") +
        
        ##3. Scales
        scale_color_manual(values=c("#e3120b", "#336666", "#FB9851", "#acc8d4", 
                                    "#dbcc98")) + 
        scale_x_continuous(limits = c(2000, 2019) , breaks = (seq(2000, 2019, 2))) +
        scale_y_continuous(limits = c(-20, 65), labels = percent_format(scale = 1),
                           breaks = seq(-20, 65, by=10)) +
        
        ##4. Theme
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
              legend.position = c(0.85, 0.78),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D"))   
    
    
    #B Save Graph
    ggsave(paste(graph_path, "3a_4.1--Percentage change (since 2000) in Weekly Mean Marketing Mail by Age Cohort.png",
                 sep="\\"), graph.4.1, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)

    
#4.2: Percentage Change in Non-Profit Mail Received by Age Cohort over time

    #A. Graph
    graph.4.2 <- ggplot() + 
    
        ##1. Line graph
        geom_line(data = mean.r, aes(x=Year, y=BaseChange.npmr, group=Age.Cohort, 
                                     color=Age.Cohort), size = 1) +
        geom_hline(yintercept = 0, color = "#1C2833") +
        
        ##2. Graph title and Axis labels
        ggtitle(label = "Percentage Change in **Non-Profit Mail** Volume Since 2000",
                subtitle = "Average of Respondents Weekly Totals") +
        labs(caption = 'Source: Household Diary Survey',
             color = 'Age Cohort') +
        ylab("%\u0394 in Average Weekly Volume") + 
        xlab("") +
        
        ##3. Scales
        scale_color_manual(values=c("#e3120b", "#336666", "#FB9851", "#acc8d4", 
                                    "#dbcc98")) + 
        scale_x_continuous(limits = c(2000, 2019) , breaks = (seq(2000, 2019, 2))) +
        scale_y_continuous(limits = c(-40, 25), labels = percent_format(scale = 1),
                           breaks = seq(-40, 25, by=10)) +
        
        ##4. Theme
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
              legend.position = c(0.85, 0.78),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D"))   
    
    
    #B Save Graph
    ggsave(paste(graph_path, "3a_4.2--Percentage change (since 2000) in Weekly Mean Non-Profit by Age Cohort.png",
                 sep="\\"), graph.4.2, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)
    
#------------------------------------------------------------------------#
#                             Step 5: Graph                              #
#                     Percentage Change Year-to-Year                     #
#------------------------------------------------------------------------# 
    
#5.1: Percentage Change in Marketing Mail Received by Age Cohort over time
    
    #A. Graph
    graph.5.1 <- ggplot() + 
      
        ##1. Line graph
        geom_line(data = mean.r, aes(x=Year, y=percent.change.brmr, group=Age.Cohort, 
                                     color=Age.Cohort), size = 1) +
        geom_hline(yintercept = 0, color = "#1C2833") +
        
        ##2. Graph title and Axis labels
        ggtitle(label = "Percentage Change in **Marketing Mail** Volume Year-to-Year",
                subtitle = "Average of Respondents Weekly Totals") +
        labs(caption = 'Source: Household Diary Survey',
             color = 'Age Cohort') +
        ylab("%\u0394 in Average Weekly Volume") + 
        xlab("") +
        
        ##3. Scales
        scale_color_manual(values=c("#e3120b", "#336666", "#FB9851", "#acc8d4", 
                                    "#dbcc98")) + 
        scale_x_continuous(limits = c(2000, 2019) , breaks = (seq(2000, 2019, 2))) +
        scale_y_continuous(limits = c(-25, 20), labels = percent_format(scale = 1,
                                                                        accuracy = 1),
                           breaks = seq(-25, 20, by=5)) +
        
        ##4. Theme
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
              legend.position = c(0.1, 0.25),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D"))   
    
    
    #B Save Graph
    ggsave(paste(graph_path, "3a_5.1--Percentage change (Year-to-Year) in Weekly Mean Marketing Mail by Age Cohort.png",
                 sep="\\"), graph.5.1, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)    
    
#5.2: Percentage Change in Non-Profit Mail Received by Age Cohort over time
    
    #A. Graph
    graph.5.2 <- ggplot() + 
      
        ##1. Line graph
        geom_line(data = mean.r, aes(x=Year, y=percent.change.npmr, group=Age.Cohort, 
                                     color=Age.Cohort), size = 1) +
        geom_hline(yintercept = 0, color = "#1C2833") +
        
        ##2. Graph title and Axis labels
        ggtitle(label = "Percentage Change in **Non-Profit Mail** Volume Year-to-Year",
                subtitle = "Average of Respondents Weekly Totals") +
        labs(caption = 'Source: Household Diary Survey',
             color = 'Age Cohort') +
        ylab("%\u0394 in Average Weekly Volume") + 
        xlab("") +
        
        ##3. Scales
        scale_color_manual(values=c("#e3120b", "#336666", "#FB9851", "#acc8d4", 
                                    "#dbcc98")) + 
        scale_x_continuous(limits = c(2000, 2019) , breaks = (seq(2000, 2019, 2))) +
        scale_y_continuous(limits = c(-30, 30), labels = percent_format(scale = 1),
                           breaks = seq(-30, 30, by=10)) +
        
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
              legend.position = c(0.15, 0.15),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D"))   
      
    
    #B Save Graph
    ggsave(paste(graph_path, "3a_5.2--Percentage change (Year-to-Year) in Weekly Mean Non-Profit Mail by Age Cohort.png",
                 sep="\\"), graph.5.2, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)    
    
    
#------------------------------------------------------------------------#
#                           Step 6: Light Clean                          #
#                         HDS Internet Questions                         #
#------------------------------------------------------------------------#    
    
#6.1: Clean for merge
    
    #A. Make columns unique
    rects_internet <- rects_internet_raw
    names(rects_internet) <- make.names(toupper(names(rects_internet)), unique = TRUE)
    
        ##1. Colnames
        rects_internet_col <- as.data.frame(colnames(rects_internet))
        
    #B. Change name of variables
    rects_internet <- rects_internet %>%
      rename(Q209.ADULTS.INTERNET.AT.HOME = Q209..DO.ANY.OF.THE.ADULTS.AGE.18.OR.OLDER.IN.YOUR.HOUSEHOLDS.USE.THE.INTERNET.AT.HOME.,
             Q23.TYPE.OF.INTERNET = Q23..TYPE.OF.INTERNET.CONNECTION.AVAILABLE)
    
    #C. Label variables
    
        ##1. Q209 Do any adults use the internet at home (Format_library::YES)
        rects_internet <- rects_internet %>%
          mutate(Q209.ADULTS.INTERNET.AT.HOME = 
                   factor(Q209.ADULTS.INTERNET.AT.HOME,
                          levels = c(1, 2, 8, 9),
                          labels = c("Yes", "No", "DK", "RF"))) 
        
        ##2. Q23 Type of internet connection available (Format_library::YES)
        rects_internet <- rects_internet %>%
          mutate(Q23.TYPE.OF.INTERNET = 
                   factor(Q23.TYPE.OF.INTERNET,
                          levels = c(-5, -1, 1, 2, 3, 4, 5, 6, 7,	9, 97, 99),
                          labels = c("QUESTION NOT IN SURVEY", "QUESTION SKIPPED", 
                                     "Reg Dial-up Modem",	"Mobile Broadband",	
                                     "Cable Modem",	"Other Broadband",	"DSL",	
                                     "Other/DK/RF",	"Other/DK/RF", "Other/DK/RF",	
                                     "Other/DK/RF",	"Other/DK/RF")))  

    #D. Drop from categorical variables
    rects_internet[rects_internet == 'RF'] <- NA
    rects_internet[rects_internet == 'DK'] <- NA 
    rects_internet[rects_internet == 'DK/RF'] <- NA 
    rects_internet[rects_internet == 'Other/DK/RF'] <- NA  
    #rects_internet[rects_internet == 'QUESTION SKIPPED'] <- NA  
    rects_internet[rects_internet == 'QUESTION NOT IN SURVEY'] <- NA  
    rects_internet[rects_internet == '?'] <- NA  
    
#6.2: Data checks
    
    #A. Number of NAs by column
    df.na.all.internet <- rects_internet %>%
      summarise(across(everything(), ~ sum(is.na(.x))))
    
    #B. Number of NAs by column and year
    df.na.yr.all.internet <- rects_internet %>%
      group_by(SURVEY.YEAR) %>%
      summarise(across(everything(), ~ sum(is.na(.x))))
    
    #C. Number of NAs as a % of total observations by column and year
    df.na.yr.perc.all.internet <- rects_internet %>%
      group_by(SURVEY.YEAR) %>%
      summarise(across(everything(), ~ round(100*mean(is.na(.x)), 2) ) )
    
    #D. Cross tab
    
        ##1. Q23 Type of internet connection available
        cross.tab.q23 <- as.data.frame(table(hds.r$SURVEY.YEAR, hds.r$Q23.TYPE.OF.INTERNET))
    
#6.3: Merge
    
    #A. Merge billing data on survey year and sample number
    hds.r <- left_join(hds.r, rects_internet, 
                     by = c("SAMPLE.NUMBER", "SURVEY.YEAR"))  
    
#6.4: Get averages of internet use variables
    
    #A. Q23 Type of internet connection available 
    mean.r.npmr.type.internet <- aggregate(x = hds.r$MAIL.VOLUME.NPMR_PRST, 
                                  by = list(hds.r$SURVEY.YEAR, hds.r$Q23.TYPE.OF.INTERNET),
                                  FUN = 'mean')
    colnames(mean.r.npmr.type.internet) <- c("Year", "Type.Internet", "Mean.Nonprofit")
    mean.r.npmr.type.internet$Mean.Nonprofit <- round(mean.r.npmr.type.internet$Mean.Nonprofit, 2)
    
    
    
#------------------------------------------------------------------------#
#                             Step 7: Graph                              #
#                 Graphs of Internet Use Volume Over time                #
#------------------------------------------------------------------------#

#7.1: Marketing Mail Received by Age Cohort over time

    #A. Graph
    graph.7.1 <- ggplot() + 
      
        ##1. Line graph
        geom_line(data = mean.r.npmr.type.internet, 
                  aes(x=Year, y=Mean.Nonprofit, group=Type.Internet, 
                      color=Type.Internet, linetype=Type.Internet), size = 1) +
        
        ##2. Graph title and Axis labels
        ggtitle(label = "Change in **Non-Profit Mail** Received Volume by Type of Internet Connection",
                subtitle = "Average of Respondents Weekly Totals") +
        labs(caption = 'Note: The percentage of respondents using Dial-up falls 
                        from a high of 40% in 2004 to a low of 0.3% in 2019.
                        This may result in outliers skewing the average.<br/>
                        Between 10-30% of individuals in each year did not 
                        answer the question.<br/>
                        Source: Household Diary Survey') +
        ylab("Mean Volume Received") + 
        xlab("") +
        
        ##3. Scales
        scale_linetype_manual(values=c("longdash", "solid", "solid", "solid",
                                     "solid", "solid"), name = "Type.Internet") +
        scale_color_manual(values=c("#e3120b", "#336666", "#FB9851", "#acc8d4", 
                                    "#dbcc98", "#36E2BD"), name = "Type.Internet") + 
        scale_x_continuous(limits = c(2004, 2019) , breaks = (seq(2004, 2019, 2))) +
        scale_y_continuous(limits = c(1, 5), 
                           breaks = seq(1, 5, by=.5)) +
        
        ##4. Theme
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              panel.border=element_blank(), 
              axis.line=element_line(), 
              plot.title = ggtext::element_markdown(color="black", size=14, hjust = .5),
              plot.subtitle = element_text(hjust = .5),
              axis.title.x = element_text(size = 9, color = "grey30"), 
              axis.title.y = element_text(size = 9, color = "grey30"), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_blank(),
              legend.text = element_text(size = 7),
              legend.position = "bottom",
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 
      
    #B Save Graph
    ggsave(paste(graph_path, "3a_7.1--Weekly Mean Non-Profit Mail Received by Internet Connection.png",
                 sep="\\"), graph.7.1, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)     
    
    
    
