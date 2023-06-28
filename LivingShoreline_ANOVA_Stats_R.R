#ANOVA Statistical Analysis of Living Shoreline RPI Metrics (Seacoast of New Hampshire)
#Author: Grant McKown, Research Associate of Coastal Habitat Restoration Team (CHaRT), 
#Contact: james.mckown@unh.edu | jgrantmck@gmail.com

#Created: June 2023

#Last Updated: June 2023

#R-script is broken down into 3 Chapters with Pages and Tasks:
# Chapter 1) Code Set Up
# Chapter 2) Data Input and Preparation
# Chapter 3) ANOVA of Vegetation Data
# Chapter 4) ANOVA of Pore water Data
# Chapter 5) ANOVA of Nekton Data

#-----------------------------------------------------------------------------------------------------------

#Chapter 1 - Code Set Up

#Page 1 - Load Library and Clear the Memory

#Reset the Global Environment and Load Library
rm(list = ls())

#Tidyr and dplyr are great data organization packages
library(tidyr)
library(dplyr)
library(pillar)
library(broom)
library(purrr)
library(rstatix)
library(multcomp)
library(FSA)

#Plotting plotting packages (making graphs...)
library(patchwork)
library(gridExtra)
library(ggfortify)
library(ggplot2)
library(ggsci)
library(ggforce)
library(viridis)
library(wesanderson)


#Lubridate is a special package that allows us to do some cool stuff with dates and time arithmetic
library(lubridate)

#DescTools is a special package that allows for some cool functions
library(DescTools)

#drc Package allows for the 3 Parameter Logistic Regression
library(drc)

#Following packages will allow for the mixed model effect of drc() package
library(nlme)
library(lme4)


set.seed(2023)



#______________________________________________________________________________________________________________


# Chapter 2 - Data Preparation

# OVERVIEW OF MONITORING AND DATA STRUCTURE

#The data sets for the data is divided between the three core groups of selected metrics:

#The core groups are broken down into selected metrics:
    # Vegetation - Halophyte Cover
    # Pore water Chemistry - Reduction-oxidation potential
    # Nekton - Mummichog Trap Catch Rate, Adult Mummichog Length

# Individual metrics for Vegetation and Pore water chemistry are monitored and calculated for both the
  # the low and high marsh for the three treatment shorelines: Living Shoreline, Reference, and No Action.

# Nekton was monitored and calculated for the marsh zones that are common across all three treatments:
    # Wagon Hill Farm - High Marsh
    # Cutts Cove - Low Marsh
    # North Mill Pond - Low Marsh

# Sites were monitored at different field seasons (and project ages):
    # Wagon Hill Farm - 2019 (0 year), 2020 (1 years), 2022 (3 years)
    # Cutts Cove - 2019 (1 year), 2020 (2 years), 2021 (3 years); Veg monitored in 2022
    # North Mill Pond - 2019 (3 years), 2020 (4 years)



#Page 2 - Loading Living Shoreline Data Sets

#The data sets for the data is divided between the three core groups of the RPI analysis: 
  # 1) Vegetation, 
  # 2) Pore water Chemistry 
  # 3) Nekton

#For each metric, the data set will be subsetted to the first and last year of monitoring for each site for
  #Vegetation and Pore water:
    # 1) Wagon Hill Farm - 2019, 2022
    # 2) Cutts Cove - 2019, 2021
    # 3) North Mill Pond - 2019, 2020

#For Nekton, the dates are slightly off due to not monitoring nekton in 2019 for all of the sites:
  # 1) Wagon Hill Farm - 2020, 2022
  # 2) Cutts Cove - 2020, 2021
  # 3) North Mill Pond - 2020



#Step 1- Vegetation Data Set

#Data Structure Note:
  # There are no NAs in the vegetation data set. If no halophyte species were recorded in the plot,
  # both halophyte cover and species richness were assigned zero in monitoring

#Read the Vegetation Data of all Living Shoreline Sites
#Note - the option na.strings is used to replace all blanks with NAs (will be used for the next two data sets)

Veg <- read.csv("E:\\Coastal Habitat Restoration Team\\Living Shorelines - New Hampshire\\Data Analysis\\Manuscript\\Input CSVs\\Vegetation_BaseData_2022.csv",
                na.strings = c("", "NA"))

Veg <- as.data.frame(Veg)

#Subset the Data sets for the Site, Field Season, and Marsh Zone
  # Additional site characteristics are selected including Treatment (Column = Type) and Plot (Column = Plot)
  # The vegetation metrics used in the RPI are selected: Halophyte Cover and Halophyte Species Richness
  # Note - Cutts Cove vegetation data in 2018 not included in the study due to not following similar protocols

Veg_subset <- Veg %>%
  mutate(Site_Year = paste(Site, Season, sep = " - ")) %>%
    filter(Site_Year != "Cutts Cove - 2018") %>%
      dplyr::select(Site, Season, Site_Year, LS_Age, Type, Transect, Plot, Percent.Halo) %>%
         rename("HaloCov" = Percent.Halo)

rm(Veg)


#Step 2 - Pore water Chemistry Data set

#Read the Pore water Data of all Living Shoreline Sites
  # Be sure to read in the "modified" pore water data set, since it was modified with the appropriate
  # data set to make the RPI function work

# The "modified" data set adds corresponding low or high marsh values when the low or high marsh is not present
  # for the no action control shoreline. For example, at Wagon Hill Farm, there is not a no action control low marsh
  # due to erosion. In the "modified" data set, the high marsh data set for each year is copied and relabelled
  # as high marsh to allow for RPI calculations to be completed. 


Porewater <- read.csv("E:\\Coastal Habitat Restoration Team\\Living Shorelines - New Hampshire\\Data Analysis\\Manuscript\\Input CSVs\\Porewater_BaseData_2022.csv",
                      na.strings = c("", "NA"))

Porewater <- as.data.frame(Porewater)

glimpse(Porewater)


#Subset the Data sets for the Site, Field Season, and Marsh Zone
  # Note - no need to filter any data from the larger data set

PW_subset <- Porewater %>%
  mutate(Site_Year = paste(Site, Season, sep = " - ")) %>%
    dplyr::select(Site, Season, Site_Year, Type, LS_Age, Transect, Plot, True_Redox_mV ) %>%
      rename("Redox" = True_Redox_mV)

rm(Porewater)

glimpse(PW_subset)


#Step 3 - Nekton Minnow Trap Data set

#Data structure notes:
  # If no mummichogs were caught in the trap, the trap catch value was assigned zero

#Read the nekton Data set of all living shoreline sites

Nekton <- read.csv("E:\\Coastal Habitat Restoration Team\\Living Shorelines - New Hampshire\\Data Analysis\\Manuscript\\Input CSVs\\Nekton_BaseData_2022.csv")

Nekton <- as.data.frame(Nekton)

#Subset the Data sets for the Site, Field Season, and Marsh Zone
  # Additional site characteristics are selected including replicate Year (Season) and Replicate (Trap)
  # Metrics selected for RPI: Trap catch rate of mummichogs and adult length of mummichogs
  # Note - No need to filter out any sites/seasons from the nekton data set


Nekton_subset <- Nekton %>%
  mutate(Site_Year = paste(Site, Season, sep = " - ")) %>%
      dplyr::select(Site, Season, Site_Year, LS_Age, Type, Transect, Replicate, Total_Mumm, Mumm_Adult_Length) %>%
        rename("Mumm_Count" = Total_Mumm,
               "Mumm_Length" = Mumm_Adult_Length)

rm(Nekton)

glimpse(Nekton_subset)




#_______________________________________________________________________________________________________________________

#CHAPTER 3: ANOVA ANALYSIS OF VEGETATION DATA

#GENERAL FRAMEWORK FOR ALL STATISTICAL ANALYSYIS FOR THE METRICS

# One-way ANOVAs with post-hoc Tukey's tests will be conducted on the vegetation data 
  #for halophyte cover in the low and high marsh, separately, for each site. If the 
  # no action control is non-existent for a marsh zone, paired t-tests are conducted between
  # the living shoreline and reference marsh. 

# No action controls are "missing" or non-existent for the following sites:
    # 1) Wagon Hill Farm - Low Marsh Missing
    # 2) Cutts Cove - High Marsh Missing
    # 3) North Mill Pond - High Marsh Missing


#One-way ANOVAs were selected for the following reasons:
    # 1) Not enough time for individual sites to conduct regressions
    # 2) Not enough sites to allow for a space-for-time substitution to conduct regressions

# Assumptions of one-way ANOVAs will be conducted for each marsh zone with all sites aggregated together. 
  # Assumptions will be visually verified using the autoplot() function





# Step 1 - Calculate the Mean and Standard Error of the Halophyte Cover in the Sites and Zones

Veg_subset <- Veg_subset %>%
  mutate(Type = as.factor(Type),
         Site_Zone_Year = paste(Site, Transect, Season, sep = " - "),
         Site_Zone = paste(Site, Transect, sep = " - "))



Veg_summ <- Veg_subset %>%
  group_by(Site, Season, Type, Transect, LS_Age) %>%
    summarise(Mean_Cover = mean(HaloCov),
              SE_Cover = sd(HaloCov)/sqrt(n())) %>%
      mutate( Mean_Cover = round(Mean_Cover, digits = 1),
              SE_Cover = round(SE_Cover, digits = 1)) %>%
ungroup()


#Step 2 - Verification of linear assumptions with autoplot() function

Veg_model <- lm(HaloCov ~ Type, data = Veg_subset)

autoplot(Veg_model, smooth.colour = NA)

anova(Veg_model)


# Step 3 - Conduct ANOVAs for each Site and Season using broom and purr functions

# Analysis will be completed for:
  # 1) Low Marsh - Cutts Cove, North Mill Pond
  # 2) High Marsh - Wagon Hill Farm

# The purr and broom packages, used within dplyr and tidyr, allows one to conduct 
  # multiple statistical tests for given groups simultaneously, removing redundant code and possible errors. 

# Post-hoc Tukey's tests will be used if significant p values (< 0.05) are found for a given Site - Year 
  #between the three treatments. 

# To complete the analysis, the following occurs in the code:
    # 1) Filter out Wagon Hill Farm (lack of low marsh for no action control, paired t-test later)
    # 2) Group the data by Site_Year (e.g., Cutts Cove - 2019)
    # 3) Nest the data by Site_Year to separate between each grouping
    # 4) Using the map() function, compute an ANOVA on each Site_Year data
    # 5) Within the map() function, tidy() function creates a tibble of the ANOVA stats
    # 6) The unnest() function creates a larger table of the ANOVA stats for all Site_Year together

# One special note about the code that I found out through sheer will and help from friends:
    # The function in the map() function after the tilda (~) needs to be in the aov() class for both
    # the purr function and later on tidy() function to work properly. Don't ask why. It is the law.
    # For post-hoc tests, using the glht() function from the multcomp package is key. 

Veg_anova <- Veg_subset %>%
  group_by(Site_Zone_Year) %>%
    filter(length(unique(Type)) == 3) %>%
    nest() %>%
      mutate(ANOVAs = map(.x = data, 
                      ~ aov(HaloCov ~ Type, data = .x) %>%
      tidy())) %>% 
        unnest(ANOVAs) %>%
          dplyr::select(-data) %>%
ungroup()

# Step 4 - Post-hoc Dunnett's test with broom and purr functions

# I tried to pipe the ANOVA stats into then calculating the Tukey post-hoc test, however, the table 
  # was unable to merge the table of ANOVA stats and Tukey stats together with the unnest functions.
  # Therefore, I recreated the analysis dplyr pipeline with the addition of the ghlt() function. 

# I recognize it is not the driest code, but definitely streamlines the code compared to individually
  # conducting the analyses per Site_Year

# Step 4 - To compute the post-hoc Tukey's tests, the following occurs in the code:
    # 1) Filter out the Site_Year without p-values < 0.05 (non-significant differences between treatments)
    # 2) Select only the Site_Year and the data (not nested in individual tibbles), removing the ANOVA stats
    # 3) Using the map() function, computes the ANOVA on each site and THEN computes the Tukey HSD test with
          # bonferonni corrections to p-value
    # 4) Tidy() and unnest() functions creates larger table of Tukey stats for all Site_Year together


Veg_dunnett <- Veg_subset %>%
  group_by(Site_Zone_Year) %>%
    filter(length(unique(Type)) == 3) %>%
    nest() %>%
      mutate(Dunnett_Compare = map(.x = data, 
                               ~ glht(lm(HaloCov ~ Type, data = .x), linfct = mcp(Type = "Dunnett")) %>%
      tidy())) %>%
        unnest(Dunnett_Compare) %>%
          dplyr::select(-data) %>%
ungroup()

# Step 5 - Paired t-tests will be conducted on the sites and marsh zone without no action controls
  # Similar code will be used for the paired t-tests with 
  # the function of t.test() and the arguments (1) paired = TRUE and (2) alternative = "two.sided"


Veg_ttest <- Veg_subset %>%
  group_by(Site_Zone_Year) %>%
    filter(length(unique(Type)) == 2) %>%
    nest() %>%
      mutate(ttests = map(.x = data, 
                        ~ t.test(HaloCov ~ Type, paired = TRUE, data = .x) %>%
      tidy())) %>%
          unnest(ttests) %>%
            dplyr::select(-data) %>%
ungroup()


# Step 6 - Export Results to CSVs

write.csv(Veg_summ, "Veg_SummaryStats.csv")

write.csv(Veg_anova, "Veg_ANOVA.csv")

write.csv(Veg_dunnett, "Veg_Dunnetts.csv")

write.csv(Veg_ttest, "Veg_Paired_Ttest.csv")

rm(Veg_anova, Veg_dunnett, Veg_ttest, Veg_model)





#_______________________________________________________________________________________________________________________

#CHAPTER 4: ANOVA ANALYSIS OF Porewater Data

#GENERAL FRAMEWORK FOR ALL STATISTICAL ANALYSYIS FOR THE METRICS

# Step 1 - Calculate the Mean and Standard Error of the Halophyte Cover in the Sites and Zones

# Sets up the table with reference columns of Site_Zone_Year and Site_Zone that will be used for
  # filtering the data sets later for different statistical results. 
  # Additionally, unlike the vegetation data, redox was not collected at certain plots. The NAs are
  # filtered out of the data set. 

PW_subset <- PW_subset %>%
  mutate(Type = as.factor(Type),
         Site_Zone_Year = paste(Site, Transect, Season, sep = " - "),
         Site_Zone = paste(Site, Transect, sep = " - ")) %>%
  filter(!is.na(Redox))


# Calculation of descriptive stats (mean +/- SE) for Redox

PW_summ <- PW_subset %>%
  group_by(Site, Season, Type, Transect, LS_Age) %>%
    summarise(Mean_Redox = mean(Redox),
              SE_Redox = sd(Redox)/sqrt(n())) %>%
      mutate( Mean_Redox = round(Mean_Redox, digits = 1),
              SE_Redox = round(SE_Redox, digits = 1)) %>%
ungroup()


#Step 2 - Verification of linear assumptions with autoplot() function

PW_model <- lm(Redox ~ Type, data = PW_subset)

autoplot(PW_model, smooth.colour = NA)

anova(PW_model)


# Step 3 - Conduct ANOVAs for each Site and Season using broom and purr functions

# Analysis will be completed for:
  # 1) Low Marsh - Cutts Cove, North Mill Pond
  # 2) High Marsh - Wagon Hill Farm

# The purr and broom packages, used within dplyr and tidyr, allows one to conduct 
  # multiple statistical tests for given groups simultaneously, removing redundant code and possible errors. 

# Post-hoc Tukey's tests will be used if significant p values (< 0.05) are found for a given Site - Year 
  #between the three treatments. 

# To complete the analysis, the following occurs in the code:
    # 1) Filter out all Site - Zone - Year groups that do not have 3 treatments
    # 2) Group the data by Site_Year (e.g., Cutts Cove - 2019)
    # 3) Nest the data by Site_Year to separate between each grouping
    # 4) Using the map() function, compute an ANOVA on each Site_Year data
    # 5) Within the map() function, tidy() function creates a tibble of the ANOVA stats
    # 6) The unnest() function creates a larger table of the ANOVA stats for all Site_Year together

# One special note about the code that I found out through sheer will and help from friends:
  # The function in the map() function after the tilda (~) needs to be in the aov() class for both
  # the purr function and later on tidy() function to work properly. Don't ask why. It is the law.
  # For post-hoc tests, using the glht() function from the multcomp package is key. 

PW_anova <- PW_subset %>%
  group_by(Site_Zone_Year) %>%
    filter(length(unique(Type)) == 3) %>%
    nest() %>%
      mutate(ANOVAs = map(.x = data, 
                      ~ aov(Redox ~ Type, data = .x) %>%
                        tidy())) %>% 
      unnest(ANOVAs) %>%
       dplyr::select(-data) %>%
ungroup()



# Step 4 - Post-hoc Dunnett's test with broom and purr functions

# I tried to pipe the ANOVA stats into then calculating the Tukey post-hoc test, however, the table 
  # was unable to merge the table of ANOVA stats and Tukey stats together with the unnest functions.
  # Therefore, I recreated the analysis dplyr pipeline with the addition of the ghlt() function. 

# I recognize it is not the driest code, but definitely streamlines the code compared to individually
  # conducting the analyses per Site_Year

# Step 4 - To compute the post-hoc Tukey's tests, the following occurs in the code:
    # 1) Filter out the Site_Year without p-values < 0.05 (non-significant differences between treatments)
    # 2) Select only the Site_Year and the data (not nested in individual tibbles), removing the ANOVA stats
    # 3) Using the map() function, computes the ANOVA on each site and THEN computes the Tukey HSD test with
        # bonferonni corrections to p-value
    # 4) Tidy() and unnest() functions creates larger table of Tukey stats for all Site_Year together


PW_dunnett <- PW_subset %>%
  group_by(Site_Zone_Year) %>%
  filter(length(unique(Type)) == 3) %>%
    nest() %>%
      mutate(Dunnett_Compare = map(.x = data, 
                               ~ glht(lm(Redox ~ Type, data = .x), linfct = mcp(Type = "Dunnett")) %>%
                                 tidy())) %>%
        unnest(Dunnett_Compare) %>%
          dplyr::select(-data) %>%
ungroup()

# Step 5 - Paired t-tests will be conducted on the sites and marsh zone without no action controls

# Similar code will be used for the paired t-tests with 
  # the function of t.test() and the arguments (1) paired = TRUE and (2) alternative = "two.sided"


PW_ttest <- PW_subset %>%
  group_by(Site_Zone_Year) %>%
    filter(length(unique(Type)) == 2) %>%
      nest() %>%
        mutate(ttests = map(.x = data, 
                      ~ t.test(Redox ~ Type, paired = FALSE, data = .x) %>%
                        tidy())) %>%
    unnest(ttests) %>%
      dplyr::select(-data) %>%
ungroup()


# Step 6 - Export Results to CSVs

write.csv(PW_summ, "PW_SummaryStats.csv")

write.csv(PW_anova, "PW_ANOVA.csv")

write.csv(PW_dunnett, "PW_Dunnetts.csv")

write.csv(PW_ttest, "PW_Paired_Ttest.csv")

rm(PW_anova, PW_dunnett, PW_ttest, PW_model)




#_______________________________________________________________________________________________________________________

#CHAPTER 4: ANOVA ANALYSIS OF Nekton Data Set

#GENERAL FRAMEWORK FOR ALL STATISTICAL ANALYSYIS FOR THE METRICS


# New notes to the nekton data analysis relevant to the design of the experimental design:
  # Nekton monitoring was only completed at the marsh zone representative across all three treatments.
  # Therefore we will only be completing ANOVAs and post hoc Dunnett's tests, not paired t-tests

# Step 1 - Calculate the Mean and Standard Error of the Trap Catch Rate in the Sites and Zones

# Sets up the table with reference columns of Site_Zone_Year and Site_Zone that will be used for
    # filtering the data sets later for different statistical results. 
    # Additionally, unlike the vegetation data, redox was not collected at certain plots. The NAs are
    # filtered out of the data set. 

Nekton_subset <- Nekton_subset %>%
  mutate(Type = as.factor(Type),
         Site_Zone_Year = paste(Site, Transect, Season, sep = " - "),
         Site_Zone = paste(Site, Transect, sep = " - ")) %>%
  filter(!is.na(Mumm_Count)) %>%
    group_by(Site_Zone_Year) %>%
      filter(length(unique(Type)) == 3) %>%
ungroup()


# Calculation of descriptive stats (mean +/- SE) for Mumm_Count

Nekton_summ <- Nekton_subset %>%
  group_by(Site, Season, Type, Transect, LS_Age) %>%
    summarise(Mean_Count = mean(Mumm_Count),
              SE_Count = sd(Mumm_Count)/sqrt(n()),
              
              Mean_Length = mean(Mumm_Length, na.rm = TRUE),
              SE_Length = sd(Mumm_Length, na.rm = TRUE)/sqrt(n())) %>%
      
    mutate( Mean_Count = round(Mean_Count, digits = 1),
            SE_Count = round(SE_Count, digits = 1),
              
            Mean_Length = round(Mean_Length, digits = 1),
            SE_Length = round(SE_Length, digits = 1)) %>%
ungroup()


#Step 2 - Verification of linear assumptions with autoplot() function

Nekton_model <- lm(Mumm_Count ~ Type, data = Nekton_subset)

autoplot(Nekton_model, smooth.colour = NA)

anova(Nekton_model)



# Step 3 - Conduct ANOVAs for each Site and Season using broom and purr functions

# Analysis will be completed for:
    # 1) Low Marsh - Cutts Cove, North Mill Pond
    # 2) High Marsh - Wagon Hill Farm

# The purr and broom packages, used within dplyr and tidyr, allows one to conduct 
  # multiple statistical tests for given groups simultaneously, removing redundant code and possible errors. 

# Post-hoc Tukey's tests will be used if significant p values (< 0.05) are found for a given Site - Year 
  #between the three treatments. 

# To complete the analysis, the following occurs in the code:
    # 1) Filter out Wagon Hill Farm (lack of low marsh for no action control, paired t-test later)
    # 2) Group the data by Site_Year (e.g., Cutts Cove - 2019)
    # 3) Nest the data by Site_Year to separate between each grouping
    # 4) Using the map() function, compute an ANOVA on each Site_Year data
    # 5) Within the map() function, tidy() function creates a tibble of the ANOVA stats
    # 6) The unnest() function creates a larger table of the ANOVA stats for all Site_Year together

# One special note about the code that I found out through sheer will and help from friends:
  # The function in the map() function after the tilda (~) needs to be in the aov() class for both
  # the purr function and later on tidy() function to work properly. Don't ask why. It is the law.
  # For post-hoc tests, using the glht() function from the multcomp package is key. 

Nekton_Kruskal <- Nekton_subset %>%
  group_by(Site_Zone_Year) %>%
       nest() %>%
         mutate(Kruskal = map(.x = data, 
                             ~ kruskal.test(Mumm_Count ~ Type, data = .x) %>%
          tidy())) %>% 
            unnest(Kruskal) %>%
              dplyr::select(-data) %>%
ungroup()



# Step 4 - Post-hoc Dunnett's test with broom and purr functions

# I tried to pipe the ANOVA stats into then calculating the Tukey post-hoc test, however, the table 
  # was unable to merge the table of ANOVA stats and Tukey stats together with the unnest functions.
  # Therefore, I recreated the analysis dplyr pipeline with the addition of the ghlt() function. 

# I recognize it is not the driest code, but definitely streamlines the code compared to individually
  # conducting the analyses per Site_Year

# Step 4 - To compute the post-hoc Tukey's tests, the following occurs in the code:
    # 1) Filter out the Site_Year without p-values < 0.05 (non-significant differences between treatments)
    # 2) Select only the Site_Year and the data (not nested in individual tibbles), removing the ANOVA stats
    # 3) Using the map() function, computes the ANOVA on each site and THEN computes the Tukey HSD test with
        # bonferonni corrections to p-value
    # 4) Tidy() and unnest() functions creates larger table of Tukey stats for all Site_Year together


Nekton_dunn <- Nekton_subset %>%
  group_by(Site_Zone_Year) %>%
      nest() %>%
        mutate(Dunn_Compare = map(.x = data, 
                                     ~ dunnTest(Mumm_Count ~ Type, method = "bonferroni", data = .x)$res)) %>%
  unnest(Dunn_Compare) %>%
  dplyr::select(-data) %>%
ungroup()



# Step 6 - Export Results to CSVs

write.csv(Nekton_summ, "Nekton_SummaryStats.csv")

write.csv(Nekton_Kruskal, "Nekton_ANOVA.csv")

write.csv(Nekton_dunn, "Nekton_Dunn.csv")

rm(Nekton_Kruskal, Nekton_dunn, Nekton_model)



#____________________________________________________________________________________________________________

# CHAPTER 6 - VISUAL GRAPH OF DESCRIPTIVE STATS


# To visualize the descriptive results, ggplots for low and high marsh for Halophyte Cover and Redox will 
  # be created individually and then general site measurements of Mummichog Trap Catch Rate and Adult Length

# For each metric, the sites and treatments will be factored to align colors and shapes across each ggplot.
  # Next, the living shoreline age will be shifted +/- 0.15 to space out each site at each year

# Lastly, all of the ggplots will be put together using the patchwork function.

# Page 1 - Graph halophyte cover

#Step 1 - Remove No Action Control, Adjust Living Shoreline Age, and set order for Site, Type, and Transect

Veg_summ <- Veg_summ %>%
  filter(Type != "No Action Control") %>%
   mutate(LS_Age = ifelse(Site == "Cutts Cove", LS_Age - 0.25, 
                   ifelse(Site == "Wagon Hill Farm", LS_Age + 0.25, LS_Age)))

Veg_summ$Site <- factor(Veg_summ$Site, levels = c("Cutts Cove", "North Mill Pond", "Wagon Hill Farm"))
        
Veg_summ$Type <- factor(Veg_summ$Type, levels = c("Living Shoreline", "Reference"))
         
Veg_summ$Transect <- factor(Veg_summ$Transect, levels = c("Low Marsh", "High Marsh"))


# Step 2 - Graph halophyte cover with low and high marsh factor_wrapped

Veg.figure <- ggplot(data = Veg_summ, aes(x = LS_Age, y = Mean_Cover)) +
  geom_errorbar(aes(y = Mean_Cover,
                    ymin = Mean_Cover - SE_Cover, ymax = Mean_Cover + SE_Cover),
                colour = "black", size = 0.75, width = 0.20) +   
  geom_point(aes(colour = Site, shape = Type),
             size = 6.5) +
  scale_colour_manual(values = wes_palette("FantasticFox1", n = 3)) +
  scale_shape_manual(values = c(16, 17))  +
  labs(x = "", y = "Halophyte Cover (%)") +
  theme_bw() +
  scale_x_continuous(limits = c(-0.5, 4.2),
                     breaks = seq(0, 4.2, 1)) +
  scale_y_continuous(limits = c(-0.5, 102),
                     breaks = seq(0, 100, 20)) + 
  theme(
    axis.title.x = element_text(size = 25, colour = "black"), 
    axis.title.y = element_text(size = 25, colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black"),
    axis.text.y = element_text(size = 20, colour = "black"),
    legend.text = element_text(size = 15, colour = "black"),
    strip.text.x = element_text(size = 27.5, colour = "black"),
    strip.background = element_blank(),
    legend.title = element_blank(),
    legend.position = "none",
    legend.background = element_rect(
      size = 0.5, linetype = "solid", 
      colour = "black")) + 
  facet_wrap(~Transect, nrow = 1, ncol = 2)


Veg.figure





# Page 2 - Graph the porewater redox 

# Step 1 - Remove No Action Control, Adjust Living Shoreline Age, and set order for Site, Type, and Transect

PW_summ <- PW_summ %>%
  filter(Type != "No Action Control") %>%
  mutate(LS_Age = ifelse(Site == "Cutts Cove", LS_Age - 0.25, 
                         ifelse(Site == "Wagon Hill Farm", LS_Age + 0.25, LS_Age)))

PW_summ$Site <- factor(PW_summ$Site, levels = c("Cutts Cove", "North Mill Pond", "Wagon Hill Farm"))
         
PW_summ$Type <- factor(PW_summ$Type, levels = c("Living Shoreline", "Reference"))
         
PW_summ$Transect <- factor(PW_summ$Transect, levels = c("Low Marsh", "High Marsh"))


# Step 2 - Graph the Redox Oxidation Potential in low and high marsh

PW.figure <- ggplot(data = PW_summ, aes(x = LS_Age, y = Mean_Redox)) +
  geom_errorbar(aes(x = LS_Age, y = Mean_Redox,
                    ymin = Mean_Redox - SE_Redox, ymax = Mean_Redox + SE_Redox),
                colour = "black", size = 0.75, width = 0.20) +   
  geom_point(aes(shape = Type,  colour = Site),
             size = 6.5) +
  scale_colour_manual(values = wes_palette("FantasticFox1", n = 3)) +
  scale_shape_manual(values = c(16, 17))  +
  labs(x = "", y = "Redox Potential (mV)") +
  theme_bw() +
  scale_x_continuous(limits = c(-0.5, 4.2),
                     breaks = seq(0, 4.2, 1)) +
  scale_y_continuous(limits = c(-400, 500),
                     breaks = seq(-400, 500, 200)) + 
  theme(
    axis.title.x = element_text(size = 25, colour = "black"), 
    axis.title.y = element_text(size = 25, colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black"),
    axis.text.y = element_text(size = 20, colour = "black"),
    legend.text = element_text(size = 15, colour = "black"),
    strip.text.x = element_text(size = 27.5, colour = "black"),
    strip.background = element_blank(),
    legend.title = element_blank(),
    legend.position = "none",
    legend.background = element_rect(
      size = 0.5, linetype = "solid", 
      colour = "black")) + 
  facet_wrap(~Transect, nrow = 1, ncol = 2)


PW.figure






# Page 3 - Graph the Mummichog trap catch rate and adult length

# Step 1 - # Step 1 - Remove No Action Control, Adjust Living Shoreline Age, and set order for Site, Type, and Transect

Nekton_summ <- Nekton_summ %>%
  filter(Type != "No Action Control") %>%
  mutate(LS_Age = ifelse(Site == "Cutts Cove", LS_Age - 0.25, 
                         ifelse(Site == "Wagon Hill Farm", LS_Age + 0.25, LS_Age)))

Nekton_summ$Site <- factor(Nekton_summ$Site, levels = c("Cutts Cove", "North Mill Pond", "Wagon Hill Farm"))

Nekton_summ$Type <- factor(Nekton_summ$Type, levels = c("Living Shoreline", "Reference"))

Nekton_summ$Transect <- factor(Nekton_summ$Transect, levels = c("Low Marsh", "High Marsh"))


# Step 2 - Mummichog Trap Catch Rate figure

Nekton.trap.figure <- ggplot(data = Nekton_summ, aes(x = LS_Age, y = Mean_Count)) +
  geom_errorbar(aes(x = LS_Age, y = Mean_Count,
                    ymin = Mean_Count - SE_Count, ymax = Mean_Count + SE_Count),
                colour = "grey25", size = 0.75, width = 0.20) +   
  geom_point(aes(shape = Type,  colour = Site),
             size = 6.5) +
  scale_colour_manual(values = wes_palette("FantasticFox1", n = 3)) +
  scale_shape_manual(values = c(16, 17))  +
  labs(x = "Project Age (yrs)", y = "Trap Catch Rate (# per trap)") +
  theme_bw() +
  scale_x_continuous(limits = c(-0.5, 4.2),
                     breaks = seq(0, 4.2, 1)) +
  scale_y_continuous(limits = c(0, 50),
                     breaks = seq(0, 50, 10)) + 
  theme(
    axis.title.x = element_text(size = 25, colour = "black"), 
    axis.title.y = element_text(size = 25, colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black"),
    axis.text.y = element_text(size = 20, colour = "black"),
    legend.text = element_text(size = 20, colour = "black"),
    strip.text.x = element_text(size = 27.5, colour = "black"),
    legend.title = element_blank(),
    legend.position = "none",
    legend.background = element_rect(size = 0.5, linetype = "solid", 
                                     colour = "black"))


Nekton.trap.figure



#Step 4 - Mummichog Adult Length graph

Nekton.length.figure <- ggplot(data = Nekton_summ, aes(x = LS_Age, y = Mean_Length)) +
  geom_errorbar(aes(x = LS_Age, y = Mean_Length,
                    ymin = Mean_Length - SE_Length, ymax = Mean_Length + SE_Length),
                colour = "black", size = 0.75, width = 0.20) +   
  geom_point(aes(shape = Type,  colour = Site),
             size = 6.5) +
  scale_colour_manual(values = wes_palette("FantasticFox1", n = 3)) +
  scale_shape_manual(values = c(16, 17))  +
  labs(x = "Project Age (yrs)", y = "Adult Length (mm)") +
  theme_bw() +
  scale_x_continuous(limits = c(0, 4.2),
                     breaks = seq(0, 4.2, 1)) +
  scale_y_continuous(limits = c(45, 65),
                     breaks = seq(45, 65, 5)) + 
  theme(
    axis.title.x = element_text(size = 25, colour = "black"), 
    axis.title.y = element_text(size = 25, colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black"),
    axis.text.y = element_text(size = 20, colour = "black"),
    legend.text = element_text(size = 20, colour = "black"),
    legend.title = element_blank(),
    legend.position = c(0.175, 0.225),
    legend.background = element_rect(
      size = 0.5, linetype = "solid", 
      colour = "black"))


Nekton.length.figure

#Step 6 - Compile the two Mummichog figures together with patchwork package

Nekton.figure <- Nekton.trap.figure + Nekton.length.figure

Nekton.figure

rm(Nekton.trap.figure, Nekton.length.figure)


#Page 4 - Create the entire figure for the manuscript with the patchwork package


Compilation.figure <- (PW.figure) / (Veg.figure) / (Nekton.figure)

Compilation.figure


ggsave(Compilation.figure, height = 20, width = 20, dpi = 600,
       limitsize = FALSE, units = "in",
       filename = "Merics_DescriptiveStats2.jpg")

