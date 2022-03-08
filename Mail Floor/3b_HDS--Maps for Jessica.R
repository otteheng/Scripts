# -> Gerhard O
# -> 10/20/2021
# -> Data sets: HDS
# -> Build Maps for Jessica of Non-Profit

# Load Packages in Library
source('C:\\Users\\bg4cn0\\USPS\\ARTSI Team Folder - Documents\\Projects\\library_GSO.R')
library(maps)

# Data Paths
raw_data_path <- "C:\\Users\\bg4cn0\\USPS\\ARTSI Team Folder - Documents\\Projects\\Millennials & Mail\\Code\\Clean Data\\HDS"
raw_census_path <- "C:\\Users\\bg4cn0\\USPS\\ARTSI Team Folder - Documents\\Projects\\_Raw Data\\Census"
graph_path <- "C:\\Users\\bg4cn0\\USPS\\ARTSI Team Folder - Documents\\Projects\\Mail Floor\\Code\\Tables & Figures\\3b"

#------------------------------------------------------------------------#
#                           Step 1: Load in Data                         #
#                                                                        #
#------------------------------------------------------------------------#

#1.1: Final Clean HDS data created as part of "Millennials and the Mail" project. 

    #A. HDS Received
    hds_raw <- read.csv(paste(raw_data_path, "2e_HDS--Cleaned data\\2e_volume-rects-new_names.csv",
                              sep = "\\"))
    
#1.2: Map data
    
    #A. United States state map data
    MainStates <- map_data("state")
    
#1.3: Census
    
    #A. State population data 2000-2009
    census_raw2000.2009 <- read.csv(paste(raw_census_path, "intercensal_estimate_state_population_2000-2010.csv",
                              sep = "\\"), skip = 3)
    
    #B. State population data 2010-2019
    census_raw2010.2019 <- read.csv(paste(raw_census_path, "intercensal_estimate_state_population_2010-2019.csv",
                                          sep = "\\"), skip = 3)

#------------------------------------------------------------------------#
#                           Step 2: Light Clean                          #
#                                   HDS                                  #
#------------------------------------------------------------------------#

#2.1: New data sets

    #A. Marketing and Non-Project Received by Age cohort
    hds.r <- hds_raw %>%
      dplyr::select(SAMPLE.NUMBER, SURVEY.YEAR, MAIL.VOLUME.BRMR_PRST, 
                    MAIL.VOLUME.NPMR_PRST, AGE.COHORT, GENDER, INC.LEVEL,
                    COUNTY, STATE.ABB, STATE, REGION, DIVISION)
    
#2.2: Data check
    
    #A. Number of respondents by state and year
    yr.state.count <- hds.r %>%
      group_by(STATE, SURVEY.YEAR) %>%
      mutate(num = 1) %>%
      summarise(count = sum(num)) %>%
      mutate(low.count.flag = if_else(count < 30, 1, 0))

#2.3: Get average by given variable(s)

    #A. Collapse age cohorts together 
    hds.r <- hds.r %>%
      mutate(AGE.COHORT2 = case_when(
        AGE.COHORT=="18-24" ~ "25-34",
        AGE.COHORT=="65-69" ~ "65+",
        AGE.COHORT=="70-74" ~ "65+",
        AGE.COHORT=="70+" ~ "65+",
        AGE.COHORT=="75+" ~ "65+",
        TRUE ~ as.character(AGE.COHORT)
      ))
    
    #B. Get average by Year and State
    
        ##1. Non-Profit Mail
        mean.r.npmr.state <- aggregate(x = hds.r$MAIL.VOLUME.NPMR_PRST, 
                                 by = list(hds.r$SURVEY.YEAR, hds.r$STATE),
                                 FUN = 'mean')
        colnames(mean.r.npmr.state) <- c("SURVEY.YEAR", "STATE", "Mean.Nonprofit")
        mean.r.npmr.state$Mean.Nonprofit <- round(mean.r.npmr.state$Mean.Nonprofit, 2)
        
#2.4: Merges (averages)
        
    #A. Merge with state count flag
    merge.nmpr.st.count <- left_join(mean.r.npmr.state, yr.state.count, 
                                      by = c("STATE", "SURVEY.YEAR"))
    
        ##1. Make "STATE" variable lowercase for merge with MainStates 
        merge.nmpr.st.count <- merge.nmpr.st.count %>%
          mutate(STATE = tolower(STATE)) %>%
          rename(region = STATE)
        
    #B. Merge state level data with MainStates for preparation with with maps
        
        ##1. Year 2000
        merge.nmpr.st.maps.2000 <- merge.nmpr.st.count %>%
          filter(SURVEY.YEAR == 2000)
        merge.nmpr.st.maps.2000 <- right_join(merge.nmpr.st.maps.2000, 
                                              MainStates, by = c("region"))
        
        ##2. Year 2019
        merge.nmpr.st.maps.2019 <- merge.nmpr.st.count %>%
          filter(SURVEY.YEAR == 2019)
        merge.nmpr.st.maps.2019 <- right_join(merge.nmpr.st.maps.2019, 
                                              MainStates, by = c("region"))
        
#2.5: Get sums by given variables
        
    #A. Create sum of non-profit mail by state
    sum.r.npmr.state <- aggregate(x = hds.r$MAIL.VOLUME.NPMR_PRST, 
                                  by = list(hds.r$SURVEY.YEAR, hds.r$STATE),
                                  FUN = 'sum')
    colnames(sum.r.npmr.state) <- c("SURVEY.YEAR", "STATE", "Sum.Nonprofit")
    
#2.6: Merges (sums)
    
    #A. Merge with state count flag
    merge.sum.nmpr.st.count <- left_join(sum.r.npmr.state, yr.state.count, 
                                     by = c("STATE", "SURVEY.YEAR"))
    
        ##1. Make "STATE" variable lowercase for merge with MainStates 
        merge.sum.nmpr.st.count <- merge.sum.nmpr.st.count %>%
          mutate(STATE = tolower(STATE)) %>%
          rename(region = STATE)
    
    #B. Merge state level data with MainStates for preparation with with maps
    
        ##1. Year 2000
        merge.sum.nmpr.st.maps.2000 <- merge.sum.nmpr.st.count %>%
          filter(SURVEY.YEAR == 2000)
        merge.sum.nmpr.st.maps.2000 <- right_join(merge.sum.nmpr.st.maps.2000, 
                                              MainStates, by = c("region"))
        
        ##2. Year 2019
        merge.sum.nmpr.st.maps.2019 <- merge.sum.nmpr.st.count %>%
          filter(SURVEY.YEAR == 2019)
        merge.sum.nmpr.st.maps.2019 <- right_join(merge.sum.nmpr.st.maps.2019, 
                                              MainStates, by = c("region"))
    
#------------------------------------------------------------------------#
#                             Step 3: Graph                              #
#                        Graphs of Volume Over time                      #
#------------------------------------------------------------------------#

#3.1: Non-profit received map in 2000
        
    #A. Graph
    graph.3.1 <- ggplot() + 
      
        ##1. Map 
        geom_polygon( data=merge.nmpr.st.maps.2000, 
                           aes(x=long, y=lat, group=group, fill = Mean.Nonprofit), 
                           color="#353935", size = .4)  + 
    
        ##2. Graph title and Axis labels
        ggtitle(label = "Non-Profit Mail Received in the United States, Year: **2000**",
                subtitle = "Average of Respondents Weekly Totals by State of Residence") +
          labs(caption = 'Note: States in grey did not have data for that year <br/>
                          Source: Household Diary Survey') +
          ylab("") + 
          xlab("") +
          
        ##3. Scales
        scale_fill_continuous(name="Mean\nNon-Profit\nMail", 
                              low = "lightblue", high = "darkblue",limits = c(1,4), 
                              breaks=c(1, 1.5, 2, 2.5, 3, 3.5, 4), 
                              na.value = "grey60") +
        
        ##4. Theme
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              axis.line=element_blank(), 
              plot.title = ggtext::element_markdown(color="black", size=14, hjust = .5),
              plot.subtitle = element_text(hjust = .5),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 7),
              strip.background = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill=NA, size=1), 
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 
  
    #B Save Graph
    ggsave(paste(graph_path, "3b_3.1--US Map of Non-profit mail 2000.png",
                 sep="\\"), graph.3.1, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)  

    
#3.2: Non-profit received map in 2019

    #A. Graph
    graph.3.2 <- ggplot() + 
      
        ##1. Map 
        geom_polygon( data=merge.nmpr.st.maps.2019, 
                      aes(x=long, y=lat, group=group, fill = Mean.Nonprofit), 
                      color="#353935", size = .4)  + 
        
        ##2. Graph title and Axis labels
        ggtitle(label = "Non-Profit Mail Received in the United States, Year: **2019**",
                subtitle = "Average of Respondents Weekly Totals by State of Residence") +
        labs(caption = 'Note: States in grey did not have data for that year <br/>
                            Source: Household Diary Survey') +
        ylab("") + 
        xlab("") +
        
        ##3. Scales
        scale_fill_continuous(name="Mean\nNon-Profit\nMail", 
                              low = "lightblue", high = "darkblue",limits = c(1,4), 
                              breaks=c(1, 1.5, 2, 2.5, 3, 3.5, 4), 
                              na.value = "grey60") +
        
        ##4. Theme
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              axis.line=element_blank(), 
              plot.title = ggtext::element_markdown(color="black", size=14, hjust = .5),
              plot.subtitle = element_text(hjust = .5),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 7),
              strip.background = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill=NA, size=1), 
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 
      
    #B Save Graph
    ggsave(paste(graph_path, "3b_3.2--US Map of Non-profit mail 2019.png",
                 sep="\\"), graph.3.2, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300) 
    
#------------------------------------------------------------------------#
#                               Step 4: Clean                            #
#                                Census Data                             #
#------------------------------------------------------------------------#  
    
#4.1: Clean 2000-2009 Census data
    
    #A. Select variables of interest
    census.2000.2009 <- census_raw2000.2009 %>%
      dplyr::select(X, starts_with("X2"))
    
    #B. Keep state level data only (Values starting with a period)
    census.2000.2009 <- census.2000.2009 %>%
      filter(str_detect(X, "^\\."))
    
    #C. Remove period from the beginning of state variable
    census.2000.2009 <- census.2000.2009 %>%
      mutate(X = gsub("\\.", "", X))
    
    #D. Make state lowercase to match map data
    census.2000.2009 <- census.2000.2009 %>%
      mutate(X = tolower(X))
    
    #D. Rename columns
    census.2000.2009 <- census.2000.2009 %>%
      rename(region = X, ) %>%
      rename_all(~str_replace(.,"^X","")) %>%
      rename_if(str_detect(names(.), "20"), ~ paste0("Yr.", .)) %>%
      rename_all(~str_replace_all(.,"`","")) 
    
#4.2: Clean 2009-2019 Census data
    
    #A. Select variables of interest
    census.2010.2019 <- census_raw2010.2019 %>%
      dplyr::select(X, starts_with("X2"))
    
    #B. Keep state level data only (Values starting with a period)
    census.2010.2019 <- census.2010.2019 %>%
      filter(str_detect(X, "^\\."))
    
    #C. Remove period from the beginning of state variable
    census.2010.2019 <- census.2010.2019 %>%
      mutate(X = gsub("\\.", "", X))
    
    #D. Make state lowercase to match map data
    census.2010.2019 <- census.2010.2019 %>%
      mutate(X = tolower(X))
    
    #D. Rename columns
    census.2010.2019 <- census.2010.2019 %>%
      rename(region = X, ) %>%
      rename_all(~str_replace(.,"^X","")) %>%
      rename_if(str_detect(names(.), "20"), ~ paste0("Yr.", .)) %>%
      rename_all(~str_replace_all(.,"`",""))     
    
#4.3: Make year level data
    
    #A. 2000
    census.2000 <- census.2000.2009 %>%
      dplyr::select(region, Yr.2000) %>%
      mutate(SURVEY.YEAR = 2000) %>%
      rename(population = Yr.2000) %>%
      mutate(population = as.numeric(gsub(",", "", population)))
    
    #A. 2019
    census.2019 <- census.2010.2019 %>%
      dplyr::select(region, Yr.2019) %>%
      mutate(SURVEY.YEAR = 2019) %>%
      rename(population = Yr.2019) %>%
      mutate(population = as.numeric(gsub(",", "", population)))
    
#4.4: Merges
    
    #A. Merge 2000 Census data with map data
    merge.census.sum.nmpr.st.maps.2000 <- left_join(merge.sum.nmpr.st.maps.2000, 
                                          census.2000, by = c("region", "SURVEY.YEAR"))
    
 
    #B. Merge 2019 Census data with map data
    merge.census.sum.nmpr.st.maps.2019 <- left_join(merge.sum.nmpr.st.maps.2019, 
                                                    census.2019, by = c("region", "SURVEY.YEAR"))
    
#4.5: Calculate per-capita non-profit mail 
    
    #A. 2000
    merge.census.sum.nmpr.st.maps.2000 <- merge.census.sum.nmpr.st.maps.2000 %>%
      mutate(per.capita = Sum.Nonprofit/population,
             per.capita.100000 = per.capita*100000)
    
    #A. 2019
    merge.census.sum.nmpr.st.maps.2019 <- merge.census.sum.nmpr.st.maps.2019 %>%
      mutate(per.capita = Sum.Nonprofit/population,
             per.capita.100000 = per.capita*100000)
    
    
#------------------------------------------------------------------------#
#                             Step 5: Graph                              #
#                        Graphs of Volume Over time                      #
#------------------------------------------------------------------------#

#5.1: Non-profit received map in 2000

    #A. Graph
    graph.5.1 <- ggplot() + 
      
        ##1. Map 
        geom_polygon( data=merge.census.sum.nmpr.st.maps.2000, 
                      aes(x=long, y=lat, group=group, fill = per.capita.100000), 
                      color="#353935", size = .4)  + 
        
        ##2. Graph title and Axis labels
        ggtitle(label = "Per Capita Non-Profit Mail Received in the United States, Year: **2000**",
                subtitle = "Non-Profit Mail Received per 100,000") +
        labs(caption = 'Note: States in grey did not have data for that year <br/>
                            Source: Household Diary Survey') +
        ylab("") + 
        xlab("") +
        
        ##3. Scales
        scale_fill_continuous(name="Per Capita\nNon-Profit\nMail", 
                              low = "lightblue", high = "darkblue",limits = c(0,30), 
                              breaks=c(0, 5, 10, 15, 20, 25, 30), 
                              na.value = "grey60") +
        
        ##4. Theme
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              axis.line=element_blank(), 
              plot.title = ggtext::element_markdown(color="black", size=14, hjust = .5),
              plot.subtitle = element_text(hjust = .5),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 7),
              strip.background = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill=NA, size=1), 
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 
      
    #B Save Graph
    ggsave(paste(graph_path, "3b_5.1--US Map of Non-profit mail per capita 2000.png",
                 sep="\\"), graph.5.1, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)      
    
    
#5.2: Non-profit received map in 2019

    #A. Graph
    graph.5.2 <- ggplot() + 
      
        ##1. Map 
        geom_polygon( data=merge.census.sum.nmpr.st.maps.2019, 
                      aes(x=long, y=lat, group=group, fill = per.capita.100000), 
                      color="#353935", size = .4)  + 
        
        ##2. Graph title and Axis labels
        ggtitle(label = "Per Capita Non-Profit Mail Received in the United States, Year: **2019**",
                subtitle = "Non-Profit Mail Received per 100,000") +
        labs(caption = 'Note: States in grey did not have data for that year <br/>
                              Source: Household Diary Survey') +
        ylab("") + 
        xlab("") +
        
        ##3. Scales
        scale_fill_continuous(name="Per Capita\nNon-Profit\nMail", 
                              low = "lightblue", high = "darkblue",limits = c(0,10), 
                              breaks=c(0, 2, 4, 6, 8, 10), 
                              na.value = "grey60") +
        
        ##4. Theme
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              axis.line=element_blank(), 
              plot.title = ggtext::element_markdown(color="black", size=14, hjust = .5),
              plot.subtitle = element_text(hjust = .5),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 7),
              strip.background = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill=NA, size=1), 
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 
      
    #B Save Graph
    ggsave(paste(graph_path, "3b_5.2--US Map of Non-profit mail per capita 2019.png",
                 sep="\\"), graph.5.2, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)   
    

#5.3: Non-profit received map in 2000

    #A. Convert to NA states with low response rates
    merge.census.sum.nmpr.st.maps.2000.2 <- merge.census.sum.nmpr.st.maps.2000 %>% 
      mutate(per.capita.100000 = ifelse(low.count.flag==1, NA,
                                        per.capita.100000))
    
    #B. Graph
    graph.5.3 <- ggplot() + 
      
        ##1. Map 
        geom_polygon( data=merge.census.sum.nmpr.st.maps.2000.2, 
                      aes(x=long, y=lat, group=group, fill = per.capita.100000), 
                      color="#353935", size = .4)  + 
        
        ##2. Graph title and Axis labels
        ggtitle(label = "Per Capita Non-Profit Mail Received in the United States, Year: **2000**",
                subtitle = "Non-Profit Mail Received per 100,000") +
        labs(caption = 'Note: States in grey did not have data for that year or
                              they had low response rates.<br/>
                              Source: Household Diary Survey') +
        ylab("") + 
        xlab("") +
        
        ##3. Scales
        scale_fill_continuous(name="Per Capita\nNon-Profit\nMail", 
                              low = "lightblue", high = "darkblue",limits = c(0,30), 
                              breaks=c(0, 5, 10, 15, 20, 25, 30), 
                              na.value = "grey60") +
        
        ##4. Theme
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              axis.line=element_blank(), 
              plot.title = ggtext::element_markdown(color="black", size=14, hjust = .5),
              plot.subtitle = element_text(hjust = .5),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 7),
              strip.background = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill=NA, size=1), 
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 
      
    #C Save Graph
    ggsave(paste(graph_path, "3b_5.3--US Map of Non-profit mail per capita 2000 (remove states with low response rates).png",
                 sep="\\"), graph.5.3, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)     
    
#5.4: Non-profit received map in 2019

    #A. Convert to NA states with low response rates
    merge.census.sum.nmpr.st.maps.2019.2 <- merge.census.sum.nmpr.st.maps.2019 %>% 
      mutate(per.capita.100000 = ifelse(low.count.flag==1, NA,
                                        per.capita.100000))
    
    #B. Graph
    graph.5.4 <- ggplot() + 
      
        ##1. Map 
        geom_polygon( data=merge.census.sum.nmpr.st.maps.2019.2, 
                      aes(x=long, y=lat, group=group, fill = per.capita.100000), 
                      color="#353935", size = .4)  + 
        
        ##2. Graph title and Axis labels
        ggtitle(label = "Per Capita Non-Profit Mail Received in the United States, Year: **2019**",
                subtitle = "Non-Profit Mail Received per 100,000") +
        labs(caption = 'Note: States in grey did not have data for that year or
                                they had low response rates.<br/>
                                Source: Household Diary Survey') +
        ylab("") + 
        xlab("") +
        
        ##3. Scales
        scale_fill_continuous(name="Per Capita\nNon-Profit\nMail", 
                              low = "lightblue", high = "darkblue",limits = c(1,8), 
                              breaks=c(1, 2, 3, 4, 5, 6, 7, 8), 
                              na.value = "grey60") +
        
        ##4. Theme
        theme_minimal() +
        theme(text = element_text(family = "Georgia"),
              axis.line=element_blank(), 
              plot.title = ggtext::element_markdown(color="black", size=14, hjust = .5),
              plot.subtitle = element_text(hjust = .5),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(), 
              legend.box.background = element_rect(color="black", size=.5),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 7),
              strip.background = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.border = element_rect(colour = "black", fill=NA, size=1), 
              plot.caption = ggtext::element_markdown(hjust = 1, size = 7, 
                                                      color = "#7B7D7D")) 
      
    #C Save Graph
    ggsave(paste(graph_path, "3b_5.4--US Map of Non-profit mail per capita 2019 (remove states with low response rates).png",
                 sep="\\"), graph.5.4, scale = 1, width = 8.5,
           height = (7.5/1.618), dpi = 300)     
    
    
    