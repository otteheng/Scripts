# -> Gerhard O
# -> 9/22/2021
# -> Data sets: HDS
# -> Build figures for FCM Received and Sent 

# set working directory 
setwd("C:/Users/BG4CN0/USPS/ARTSI Team Folder - Documents/Projects")

# Load Packages in Library
source('./library_GSO.R')

# Data Paths
raw_data_path <- "./Millennials & Mail/Code/Clean Data/HDS"
graph_path <- "./Mail Floor/Code/Tables & Figures/1a"

#------------------------------------------------------------------------#
#                           Step 1: Load in Data                         #
#                                                                        #
#------------------------------------------------------------------------#

#1.1: Final Clean HDS data created as part of "Millennials and the Mail" project. 

    #A. HDS Received
    hds_raw <- read.csv(paste(raw_data_path, "2e_HDS--Cleaned data/2e_volume-rects-new_names.csv",
                                sep = "/"))

#------------------------------------------------------------------------#
#                           Step 2: Light Clean                          #
#                                                                        #
#------------------------------------------------------------------------#

#2.1: New data sets

    #A. FCM Received by Age cohort
    hds.r.age <- hds_raw %>%
      dplyr::select(SAMPLE.NUMBER, SURVEY.YEAR, MAIL.VOLUME.FCMR, AGE.COHORT)
    
    #B. FCM Sent by Age cohort
    hds.s.age <- hds_raw %>%
      dplyr::select(SAMPLE.NUMBER, SURVEY.YEAR, MAIL.VOLUME.FCMS, AGE.COHORT)
    
#2.2: Clean variables
    
    #A. Collapse age cohorts together 
    
        ##1. Received
        hds.r.age <- hds.r.age %>%
          mutate(AGE.COHORT2 = case_when(
            AGE.COHORT=="18-24" ~ "25-34",
            AGE.COHORT=="65-69" ~ "65+",
            AGE.COHORT=="70-74" ~ "65+",
            AGE.COHORT=="70+" ~ "65+",
            AGE.COHORT=="75+" ~ "65+",
            TRUE ~ as.character(AGE.COHORT)
          ))
        
        ##2. Sent
        hds.s.age <- hds.s.age %>%
          mutate(AGE.COHORT2 = case_when(
            AGE.COHORT=="18-24" ~ "25-34",
            AGE.COHORT=="65-69" ~ "65+",
            AGE.COHORT=="70-74" ~ "65+",
            AGE.COHORT=="70+" ~ "65+",
            AGE.COHORT=="75+" ~ "65+",
            TRUE ~ as.character(AGE.COHORT)
          ))
        
    #B. Get average FCM(S+R) by Age cohort and Year
        
        ##1. Received
        mean.r <- aggregate(x = hds.r.age$MAIL.VOLUME.FCMR, 
                               by = list(hds.r.age$SURVEY.YEAR, hds.r.age$AGE.COHORT2),
                               FUN = 'mean')
        colnames(mean.r) <- c("Year", "Age.Cohort", "Mean")
        mean.r$Mean <- round(mean.r$Mean, 2)
        
        ##2. Sent
        mean.s <- aggregate(x = hds.s.age$MAIL.VOLUME.FCMS, 
                            by = list(hds.s.age$SURVEY.YEAR, hds.s.age$AGE.COHORT2),
                            FUN = 'mean')
        colnames(mean.s) <- c("Year", "Age.Cohort", "Mean")
        mean.s$Mean <- round(mean.s$Mean, 2)
        
#2.3: Year to Year Percentage change of means
        
    #A. Received
    mean.r <- mean.r %>%
      group_by(Age.Cohort, .add = T) %>%
      mutate(percent.change = round(((Mean - lag(Mean)) / lag(Mean))*100,2),
             percent.change = if_else(is.na(percent.change), 0, percent.change)) 
    
    #B. Sent
    mean.s <- mean.s %>%
      group_by(Age.Cohort, .add = T) %>%
      mutate(percent.change = round(((Mean - lag(Mean)) / lag(Mean))*100,2),
             percent.change = if_else(is.na(percent.change), 0, percent.change))
    
#2.4: Percentage change from the year 2000
    
    #A. Received
    mean.r <- mean.r %>%
      group_by(Age.Cohort, .add = T) %>%
      mutate(First = head(Mean, 1),
             BaselineChange = case_when(Mean != First ~ (Mean - First) * 100 / First,
                                        TRUE ~ 1 * NA),
             BaselineChange = round(BaselineChange, 2),
             BaselineChange = if_else(is.na(BaselineChange), 0, BaselineChange)) %>%
      dplyr::select(-First)
    
    #B. Sent
    mean.s <- mean.s %>%
      group_by(Age.Cohort, .add = T) %>%
      mutate(First = head(Mean, 1),
             BaselineChange = case_when(Mean != First ~ (Mean - First) * 100 / First,
                                        TRUE ~ 1 * NA),
             BaselineChange = round(BaselineChange, 2),
             BaselineChange = if_else(is.na(BaselineChange), 0, BaselineChange)) %>%
      dplyr::select(-First)

#------------------------------------------------------------------------#
#                             Step 3: Graph                              #
#                                                                        #
#------------------------------------------------------------------------#

#3.1: FCM Received by Age Cohort over time
        
    #A. Graph
    graph.3.1 <- ggplot() + 
        
        ##1. Line graph
        geom_line(data = mean.r, aes(x=Year, y=Mean, group=Age.Cohort, 
                                  color=Age.Cohort), size = 1) +
        
        ##2. Graph title and Axis labels
        ggtitle(label = "Change in First Class Mail (**Received**) Volume",
                subtitle = "Average of Respondents Weekly Totals") +
        labs(caption = 'Source: Household Diary Survey',
             color = 'Age Cohort') +
        ylab("Mean Volume Received") + 
        xlab("") +
      
        ##3. Scales
        scale_color_manual(values=c("#e3120b", "#336666", "#8abbd0", "#acc8d4", 
                                    "#dbcc98")) + 
        scale_x_continuous(limits = c(2000, 2019) , breaks = (seq(2000, 2019, 2))) +
        scale_y_continuous(limits = c(0, 14), 
                           breaks = seq(0, 14, by=2)) +
      
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
              legend.position = c(0.1, 0.3),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 
    
    #B Save Graph
    ggsave(paste(graph_path, "1a_3.1--Weekly Mean FCM Received by Age Cohort.png",
                 sep="/"), graph.3.1, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)
    
#3.2: FCM Received by Age Cohort over time
    
    #A. Graph
    graph.3.2 <- ggplot() + 
      
        ##1. Line graph
        geom_line(data = mean.s, aes(x=Year, y=Mean, group=Age.Cohort, 
                                     color=Age.Cohort), size = 1) +
        
        ##2. Graph title and Axis labels
        ggtitle(label = "Change in First Class Mail (**Sent**) Volume",
                subtitle = "Average of Respondents Weekly Totals") +
        labs(caption = 'Source: Household Diary Survey',
             color = 'Age Cohort') +
        ylab("Mean Volume Received") + 
        xlab("") +
        
        ##3. Scales
        scale_color_manual(values=c("#e3120b", "#336666", "#8abbd0", "#acc8d4", 
                                    "#dbcc98")) + 
        scale_x_continuous(limits = c(2000, 2019) , breaks = (seq(2000, 2019, 2))) +
        scale_y_continuous(limits = c(0, 6), 
                           breaks = seq(0, 6, by=.5)) +
        
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
              legend.position = c(0.9, 0.75),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 
      
    #B Save Graph
    ggsave(paste(graph_path, "1a_3.2--Weekly Mean FCM Sent by Age Cohort.png",
                 sep="/"), graph.3.2, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)
    
#3.3: Percentage Change in FCM Received by Age Cohort over time
    
    #A. Graph
    graph.3.3 <- ggplot() + 
      
        ##1. Line graph
        geom_line(data = mean.r, aes(x=Year, y=BaselineChange, group=Age.Cohort, 
                                     color=Age.Cohort), size = 1) +
        
        ##2. Graph title and Axis labels
        ggtitle(label = "Percentage Change in First Class Mail (**Received**) Volume",
                subtitle = "Average of Respondents Weekly Totals") +
        labs(caption = 'Source: Household Diary Survey',
             color = 'Age Cohort') +
        ylab("%\u0394 in Average Weekly FCM Volume") + 
        xlab("") +
        
        ##3. Scales
        scale_color_manual(values=c("#e3120b", "#336666", "#8abbd0", "#acc8d4", 
                                    "#dbcc98")) + 
        scale_x_continuous(limits = c(2000, 2019) , breaks = (seq(2000, 2019, 2))) +
        scale_y_continuous(limits = c(-60, 20), 
                           breaks = seq(-60, 20, by=10)) +
        
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
              legend.position = c(0.85, 0.8),
              strip.background = element_rect(linetype="solid",),
              panel.grid.minor.y = element_line(color = NA),
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D"))   
    
    
    #B Save Graph
    ggsave(paste(graph_path, "1a_3.3--Percentage change (since 2000) in Weekly Mean FCM Received by Age Cohort.png",
                 sep="/"), graph.3.3, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)
    
#3.4: Percentage Change in FCM Sent by Age Cohort over time
    
    #A. Graph
    graph.3.4 <- ggplot() + 
      
      ##1. Line graph
      geom_line(data = mean.s, aes(x=Year, y=BaselineChange, group=Age.Cohort, 
                                   color=Age.Cohort), size = 1) +
      
      ##2. Graph title and Axis labels
      ggtitle(label = "Percentage Change in First Class Mail (**Sent**) Volume",
              subtitle = "Average of Respondents Weekly Totals") +
      labs(caption = 'Source: Household Diary Survey',
           color = 'Age Cohort') +
      ylab("%\u0394 in Average Weekly FCM Volume") + 
      xlab("") +
      
      ##3. Scales
      scale_color_manual(values=c("#e3120b", "#336666", "#8abbd0", "#acc8d4", 
                                  "#dbcc98")) + 
      scale_x_continuous(limits = c(2000, 2019) , breaks = (seq(2000, 2019, 2))) +
      scale_y_continuous(limits = c(-90, 40), 
                         breaks = seq(-90, 40, by=10)) +
      
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
            legend.position = c(0.85, 0.8),
            strip.background = element_rect(linetype="solid",),
            panel.grid.minor.y = element_line(color = NA),
            plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                    color = "#7B7D7D"))   
    
    
    #B Save Graph
    ggsave(paste(graph_path, "1a_3.4--Percentage change (since 2000) in Weekly Mean FCM Sent by Age Cohort.png",
                 sep="/"), graph.3.4, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)    