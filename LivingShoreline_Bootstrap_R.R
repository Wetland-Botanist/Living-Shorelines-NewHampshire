#Bootstrapping Analysis of Living Shoreline RPI Metrics (Seacoast of New Hampshire)
#Author: Grant McKown, Research Technician of Coastal Habitat Restoration Team (CHaRT), jgrantmck@gmail.com

#Created: June 2022

#Last Updated: March 2023

#Purpose: CHaRT submitted a manuscript, "Trajectories of Living Shorelines in New Hampshire for Salt Marsh Habitat:
#         If You Build It, Will They Come?" in Dec. 2021. It was accepted with revisions including re-completing
#         certain statistical analyses. This R-script addresses those two fundamental changes:
#         (1) Bootstrap the individual RPI metrics to quantify the 95% Confidence Interval
#         (2) Graph the Site RPI score of each living shoreline site
#         (3) Graph the core group RPI scores of each living shoreline site


#R-script is broken down into 3 Chapters with Pages and Tasks:
  # 1) Code Set Up
  # 2) Data Input and Preparation
  # 3) Bootstrap Analysis of Monitoring Data and RPI Calculations
  # 4) Graphing the RPI Analysis for Publication

#-----------------------------------------------------------------------------------------------------------

#Chapter 1 - Code Set Up

#Page 1 - Load Library and Clear the Memory

#Reset the Global Environment and Load Library
rm(list = ls())

#Tidyr and dplyr are great data organization packages
library(tidyr)
library(dplyr)
library(pillar)

#Plotting plotting packages (making graphs...)
library(patchwork)
library(gridExtra)
library(ggfortify)
library(ggplot2)
library(ggsci)
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

  #The data sets for the data is divided between the three core groups of the RPI analysis: 
    #Vegetation, Pore water Chemistry, and Nekton

  #The core groups are broken down into individual metrics:
    # Vegetation - Halophyte Cover, and Halophyte Species Richness
    # Pore water Chemistry - Salinity, Reduction - Oxidation Potential, pH, and Sulfide Content
    # Nekton - Mummichog Trap Catch Rate, Adult Mummichog Length

  # Individual metrics for Vegetation and Pore water chemistry are monitored and calculated for both the
    # the low and high marsh for the three treatment shorelines: Living Shoreline, Reference, and No Action.
   # Nekton was monitored and calculated for the marsh zones that are common across all three treatments:
      # Wagon Hill Farm - High Marsh
      # Cutts Cove - Low Marsh
      # North Mill Pond - Low Marsh

  # Sites were monitored at different field seasons (and project ages):
    # Wagon Hill Farm - 2019 (0 year), 2020 (1 years), 2022 (3 years)
    # Cutts Cove - 2019 (1 year), 2020 (2 years), 2021 (3 years)
    # North Mill Pond - 2019 (3 years), 2020 (4 years)




#Page 1 - Setting the Initial Data Parameters of Site & Season

  # To minimize complex code (in exchange for long, redundant code), bootstrapping and calculation of the
    #Restoration Performance Index will be conducted for each Site, Year, and Marsh Zone

  # Additionally, due to site or field season - specific conditions, unique pieces of code are required to
    # run for individual site - year combinations. Those small details are presented in the code as 
    # required in full explanation of when to apply them.

  # When each data set is inputted, it will be filtered to the proper selection. Note, all sites have both
    # low and high marsh zones. See above about the respective field seasons for each site. For marsh zone,
    # use either "Low" or "High" for low marsh and high marsh, respectively. 
             

  # In the data sets, the corresponding columns should be labelled as for the following characteristics:
      # Site = Shoreline
      # Season = Year
      # Transect = Year

Shoreline = "Cutts Cove"

Year = 2019

MarshZone = "High"



#Page 2 - Loading Living Shoreline Data Sets
  
  #The data sets for the data is divided between the three core groups of the RPI analysis: 
    #Vegetation, Pore water Chemistry, and Nekton

  # Each data set will be loaded, replace any blanks with NAs, and then subset to the designated
    # Site, Year, and Marsh Zone



#Step 1- Vegetation Data Set
  #Data Structure Note:
    # There are no NAs in the vegetation data set. If no halophyte species were recorded in the plot,
      # both halophyte cover and species richness were assigned zero in monitoring

  #Read the Vegetation Data of all Living Shoreline Sites
    #Note - the option na.strings is used to replace all blanks with NAs (will be used for the next two data sets)

Veg <- read.csv("E:\\Coastal Habitat Restoration Team\\Living Shorelines - New Hampshire\\Data Analysis\\Manuscript\\Input CSVs\\Vegetation_BaseData_2022_modified.csv",
                    na.strings = c("", "NA"))

Veg <- as.data.frame(Veg)

  #Subset the Data sets for the Site, Field Season, and Marsh Zone
    # Additional site characteristics are selected including Treatment (Column = Type) and Plot (Column = Plot)
    # The vegetation metrics used in the RPI are selected: Halophyte Cover and Halophyte Species Richness
    
Veg_subset <- Veg %>%
  filter(Site == Shoreline & Season == Year & Transect == MarshZone) %>%
    dplyr::select(Site, Season, Type, Transect, Plot, Percent.Halo, Richness.HALO) %>%
     rename("HaloCov" = Percent.Halo, "HaloRich" = Richness.HALO)

rm(Veg)
  




#Step 2 - Pore water Chemistry Data set
 
  #Read the Pore water Data of all Living Shoreline Sites
    # Be sure to read in the "modified" pore water data set, since it was modified with the appropriate
      # data set to make the RPI function work


Porewater <- read.csv("E:\\Coastal Habitat Restoration Team\\Living Shorelines - New Hampshire\\Data Analysis\\Manuscript\\Input CSVs\\Porewater_BaseData_2022_modified.csv",
                   na.strings = c("", "NA"))

Porewater <- as.data.frame(Porewater)


  #Subset the Data sets for the Site, Field Season, and Marsh Zone
    # No additional measures are taken for the pore water data set, since all needed "adjustments" to the 
      # data set were completed beforehand

PW_subset <- filter(Porewater, Site == Shoreline & Season == Year & Transect == MarshZone)

rm(Porewater)
  


#Step 3 - Nekton Minnow Trap Data set
    #Data structure notes:
      # If no mummichogs were caught in the trap, the trap catch value 
          # was assigned zero
      # If no mummichogs were caught in the trap, the adult length value was assigned NA 

  #Read the nekton Data set of all living shoreline sites

Nekton <- read.csv("E:\\Coastal Habitat Restoration Team\\Living Shorelines - New Hampshire\\Data Analysis\\Manuscript\\Input CSVs\\Nekton_BaseData_2022_modified.csv",
                       na.strings = c("", "NA"))

Nekton <- as.data.frame(Nekton)

  #Subset the Data sets for the Site, Field Season, and Marsh Zone
    # Additional site characteristics are selected including replicate Year (Season) and Replicate (Trap)
    # Metrics selected for RPI: Trap catch rate of mummichogs and adult length of mummichogs
  

Nekton_subset <- Nekton %>%
  filter(Site == Shoreline  & Season == Year & Transect == MarshZone) %>%
   dplyr::select(Site, Season, Type, Transect, Replicate, Total_Mumm, Mumm_Adult_Length) %>%
      rename("Mumm_Count" = Total_Mumm, "Mumm_Length" = Mumm_Adult_Length)

rm(Nekton)






#Page 3 - Creation of RPI Data Table for Export at end of Script
  
  # At the end of the script, all of the mean RPI values for each metric of each core group will be
      # compiled in a final RPI table. The final RPI table will be exported.

  # In Excel, outside of the R script, all of the individual RPI tables will be compiled into one table 
     # and imported for graphing purposes (Chapter 4 of this code)

  # Additionally, a second RPI table will be created to record the 95% confidence intervals of each metric

#Step 1 - Creation and Formatting of the Mean RPI Table
  #Using the dplyr pipe functions, a 1 x 10 Table is created with each column renamed to the proper metric
    #Next, the proper Marsh Zone and Site names are inputed into the first two cells
    #Lastly, outside3 of the pipe, the metric columns are re-formatted to numerics


RPI_Final <- data.frame(matrix(nrow = 1, ncol = 10)) %>%
  rename(  Site = X1, Marsh_Zone = X2,
          HaloCov = X3, HaloRich = X4,
          Salinity = X5, Redox = X6, pH = X7, Sulfide = X8,
          Nekton_Catch = X9, Nekton_Length = X10) %>%
  
  mutate(Marsh_Zone = MarshZone,
         Site = Shoreline)


RPI_Final[3:length(RPI_Final)] <- as.numeric(RPI_Final[3:length(RPI_Final)])

glimpse(RPI_Final)


#Page 5 - Creation of the 95% Confidence Interval for the final RPI Score
  # See above for description of how the data frame is created and formatted

RPI_CI <- data.frame(matrix(nrow = 1, ncol = 10)) %>%
  rename(  Site = X1, Marsh_Zone = X2,
           HaloCov = X3, HaloRich = X4,
           Salinity = X5, Redox = X6, pH = X7, Sulfide = X8,
           Nekton_Catch = X9, Nekton_Length = X10) %>%
  
  mutate(Marsh_Zone = MarshZone,
         Site = Shoreline)


RPI_CI[3:length(RPI_CI)] <- as.numeric(RPI_CI[3:length(RPI_CI)])

glimpse(RPI_CI)

  
#--------------------------------------------------------------------------------------------------------

#Chapter 2 - Bootstrap Analysis of the Individual RPI Metrics

#One of the main critiques of the manuscript was the small sample size (n  10) of each metric
  # Bootstrap analysis can allow for the resampling of the original data set and provide a larger data set to
  # conduct the RPI calculation

#Based on my research, it seems possible to bootstrap for the lower level individual metrics within each Marsh Zone:
#   Vegetation - Halophyte Cover, Species Richness
#   Pore water - Salinity, pH, Reduction-Oxidation Potential, and Sulfide Concentration
#   Nekton - Trap Catch Rate of Mummichogs, Adult length of Mummichogs

#To accomplish this, I used Dr. Isdell's Bootstrap Code to calculate the RPI value of each metric for each Marsh Zone
#   (1) Low Marsh
#   (2) High Marsh

#For the Bootstrap sample, the original data set will be resampled 1000 times. The RPI score is calculated
  #for each resample and then stored in a vector, "rpi".

#For each Bootstrap Analysis, the following will be calculated:
#   (1) Median RPI score 
#   (2) Standard Error of the RPI Score


#Data Structure Note:
  # For code to work, a value for a metric needs to be reported for every treatment shoreline
    # Vegetation - there is no need to modify the data, since all data have values or Zeroes 
                    # (absence of halophyte vegetation)
    
    #Nekton - Nekton monitoring was not conducted in 2019 for all sites and should be skipped for 2019 season.
                    # Nekton monitoring in the no action control of Cutts Cove 2020 was not completed.
                    # Solution: 2019 no action control data substituted in 2020 for RPI analysis of Cutts Cove

    # Pore water - there is a need to modify the data (ahead of this code) for run the code uniquely. 
                    #The issues for pore water in this study are four - fold:
                  
                    # First, pore water could not be obtained at all from living shoreline sites:
                      # Wagon Hill Farm - 2019, 2020 (both marsh zones)
                      # Cutts Cove - 2019, 2020 (both marshes)
                      # North Mill Pond - 2019 and 2020 (high marsh only)
                      # Solution: Do not run the RPI calculator for Salinity, pH, and Sulfides for the 
                                # above sites and years. They are assigned a zero.
                                # Do run the RPI calculator for Redox!
                  
                  # Second, pore water was not monitored for Wagon Hill Farm of 2021. Only soil redox was
                      # was obtained. 
                      # Solution: Do not run the RPI calculator for Salinity, pH, and Sulfides. Only run the  
                                  # RPI calculator for Redox. The other metrics will be ignored in site - year RPI.

                  #Third, for Wagon Hill Farm 2022 and North Mill Pond 2020, pore water was obtained in the 
                      # high marsh reference and living shoreline, but not in the no action control shoreline
                      # since the no action control shoreline does not exist! Without a no action control 
                      # shoreline, the RPI can not be calculated.
                      # Solution: The low marsh pore water values of no action control are substituted for the high marsh
                                  # analyses for each site. 

                  # Substitution of data will be completed prior to R analysis. Manual assignment of RPI metric
                      # values will be completed after R analysis.


  #Page 1 - Create the Basic RPI Function

calc.rpi <- function(ls, ref, nac){
  lsm = mean(ls, na.rm = TRUE)
  
  refm = mean(ref, na.rm = TRUE)
  
  nacm = mean(nac, na.rm = TRUE)
  
  val = (lsm - nacm)/(refm - nacm)
  
  if (val > 1) { val <- 1 }
  else  {val <- val }
  
  if (val < 0) { val <- 0 }
  else  {val <- val }
  
  
  return(val)
}

#Page 2 - Create the Resample Loop that then uses the calc.rpi function
  #The function resamples each shoreline type (n = 10) for 1000 times with the sample() function
    #Next, for each new sample, a new RPI value is calculated
    #Then, the median RPI score and standard deviation of all 1000 RPI calculations is quantified 


#After each Bootstrap analysis, the median RPI score and standard deviation will be calculated
    #and stored in a data frame of "rpi.bootstrap". After being stored, the data frame will be recycled
    # for the next metric. The content of the rpi.bootstrap data frame will be plugged into RPI_Final and
    # RPI_CI data frames before moving onto the next metric

Bootstrap_func <- function(ls, ref, nac) {
  rpi <- vector(mode = "numeric", length = 1000)
 
  Output <- data.frame(matrix(nrow = 1, ncol = 2))
  
  colnames(Output) <- c("Median", "Stan_Dev")
  
for(i in 1:length(rpi)) {
  nac.samp = sample(nac[!is.na(nac)], 10, replace = TRUE) #sampling with replacement is key
 
  ls.samp = sample(ls[!is.na(ls)], 10, replace = TRUE)
  
  ref.samp = sample(ref[!is.na(ref)], 10, replace = TRUE)
 
  rpi[i] <- calc.rpi(
    ls = ls.samp,
    ref = ref.samp,
    nac = nac.samp
  )
} 

#Calculate the Confidence Interval for Normal Distribution (n = 1000)  
SD.rpi <- sd(rpi)

mean.rpi <- mean(rpi, na.rm = TRUE)

ci.margin.rpi <- qnorm(0.975) * (SD.rpi/sqrt(length(rpi)))

#Output of the Function
hist(rpi)

Output[1] <- mean(rpi, na.rm = TRUE)

Output[2] <- ci.margin.rpi
return(Output)

}



  #Page 3 - Vegetation Bootstrap Analysis bootstrap analysis

  #One of the issues that was encountered in the sample() function of the Bootstrap_func was that metrics with
    # only zeroes in treatment data sets will be understood as integer(0) instead of a vector of zeroes.
  # The sample() function will not function properly, therefore, a work around was to create vectors of 
    # for each group core's metrics for each treatment. When read into the sample() function, it will process
    # as vectors of zeroes. 

#Task 1 - Halophyte Cover RPI Score

ls_veg <- Veg_subset %>%
  filter(Type == "Living Shoreline") %>%
    dplyr::select(HaloCov, HaloRich)

ref_veg <- Veg_subset %>%
  filter(Type == "Reference") %>%
    dplyr::select(HaloCov, HaloRich)

nac_veg <- Veg_subset %>%
  filter(Type == "No Action Control") %>%
    dplyr::select(HaloCov, HaloRich)



rpi.bootstrap <- Bootstrap_func(
  ls = dplyr::select(ls_veg, HaloCov),
  ref = dplyr::select(ref_veg, HaloCov),
  nac = dplyr::select(nac_veg, HaloCov))

RPI_Final$HaloCov[1] <- rpi.bootstrap[1,1]
RPI_CI$HaloCov[1] <- rpi.bootstrap[1,2]

rpi.bootstrap[1:length(rpi.bootstrap)] <- 0


#Task 2 - Halophyte Richness RPI Score
  #It should be noted that the Halophyte Richness is only used in the High Marsh

rpi.bootstrap <- Bootstrap_func(
  ls = dplyr::select(ls_veg, HaloRich),
  ref = dplyr::select(ref_veg, HaloRich),
  nac = dplyr::select(nac_veg, HaloRich))

RPI_Final$HaloRich[1] <- rpi.bootstrap[1,1]
RPI_CI$HaloRich[1] <- rpi.bootstrap[1,2]

rpi.bootstrap[1:length(rpi.bootstrap)] <- 0


rm(ls_veg, ref_veg, nac_veg)

RPI_Final


  #Page 3 - Pore water Chemistry RPI Score bootstrap analysis

ls_PW <- PW_subset %>%
  filter(Type == "Living Shoreline") %>%
  dplyr::select(Salinity, Redox, pH, Sulfide)

ref_PW <- PW_subset %>%
  filter(Type == "Reference") %>%
  dplyr::select(Salinity, Redox, pH, Sulfide)

nac_PW <- PW_subset %>%
  filter(Type == "No Action Control") %>%
  dplyr::select(Salinity, Redox, pH, Sulfide)




#Task 1 - Salinity RPI Score

rpi.bootstrap <- Bootstrap_func(
  ls = dplyr::select(ls_PW, Salinity),
  ref = dplyr::select(ref_PW, Salinity),
  nac = dplyr::select(nac_PW, Salinity))

RPI_Final$Salinity[1] <- rpi.bootstrap[1,1]
RPI_CI$Salinity[1] <- rpi.bootstrap[1,2]

rpi.bootstrap[1:length(rpi.bootstrap)] <- 0

#Task 2 - Reduction - Oxidation Potential

rpi.bootstrap <- Bootstrap_func(
  ls = dplyr::select(ls_PW, Redox),
  ref = dplyr::select(ref_PW, Redox),
  nac = dplyr::select(nac_PW, Redox))


RPI_Final$Redox[1] <- rpi.bootstrap[1,1]
RPI_CI$Redox[1] <- rpi.bootstrap[1,2]

rpi.bootstrap[1:length(rpi.bootstrap)] <- 0


#Task 3 - pH 

rpi.bootstrap <- Bootstrap_func(
  ls = dplyr::select(ls_PW, pH),
  ref = dplyr::select(ref_PW, pH),
  nac = dplyr::select(nac_PW, pH))


RPI_Final$pH[1] <- rpi.bootstrap[1,1]
RPI_CI$pH[1] <- rpi.bootstrap[1,2]

rpi.bootstrap[1:length(rpi.bootstrap)] <- 0


#Task 3 - Sulfide Concentration 

rpi.bootstrap <- Bootstrap_func(
  ls = dplyr::select(ls_PW, Sulfide),
  ref = dplyr::select(ref_PW, Sulfide),
  nac = dplyr::select(nac_PW, Sulfide))

RPI_Final$Sulfide[1] <- rpi.bootstrap[1,1]
RPI_CI$Sulfide[1] <- rpi.bootstrap[1,2]

rpi.bootstrap[1:length(rpi.bootstrap)] <- 0



rm(ls_PW, ref_PW, nac_PW)




#Page 5 - Nekton RPI Site Evaluation

ls_nekton <- Nekton_subset %>%
  filter(Type == "Living Shoreline") %>%
  dplyr::select(Mumm_Count, Mumm_Length)

ref_nekton <- Nekton_subset %>%
  filter(Type == "Reference") %>%
  dplyr::select(Mumm_Count, Mumm_Length)

nac_nekton <- Nekton_subset %>%
  filter(Type == "No Action Control") %>%
  dplyr::select(Mumm_Count, Mumm_Length)


#Task 1 - Mummichog Trap Catch Rate


rpi.bootstrap <- Bootstrap_func(
  ls = dplyr::select(ls_nekton, Mumm_Count),
  ref = dplyr::select(ref_nekton, Mumm_Count),
  nac = dplyr::select(nac_nekton, Mumm_Count))

RPI_Final$Nekton_Catch[1] <- rpi.bootstrap[1,1]
RPI_CI$Nekton_Catch[1] <- rpi.bootstrap[1,2]

rpi.bootstrap[1:length(rpi.bootstrap)] <- 0


head(RPI_Final)

#Task 2 - Mummichog Adult Length

rpi.bootstrap <- Bootstrap_func(
  ls = dplyr::select(ls_nekton, Mumm_Length),
  ref = dplyr::select(ref_nekton, Mumm_Length),
  nac = dplyr::select(nac_nekton, Mumm_Length))

RPI_Final$Nekton_Length[1] <- rpi.bootstrap[1,1]
RPI_CI$Nekton_Length[1] <- rpi.bootstrap[1,2]

rpi.bootstrap[1:length(rpi.bootstrap)] <- 0


rm(ls_nekton, ref_nekton, nac_nekton)


#Page 6 - Export the Mean and Standard Deviation RPI Tables to excel for further analysis

#Task 1 - Export the Mean RPI Table
  #For all values that are NA, they will be replaced with Zeroes.
  #All values will be rounded to 3 decimal places

RPI_Final[is.na(RPI_Final)] <- 0

RPI_Final[3:length(RPI_Final)] <- round(RPI_Final[3:length(RPI_Final)], 3)

RPI_Final$HaloRich <- ifelse(MarshZone == "Low", 0, RPI_Final$HaloRich)

RPI_Final


#Task 2 - Export the CI RPI Table

RPI_CI[is.na(RPI_CI)] <- 0

RPI_CI[3:length(RPI_CI)] <- round(RPI_CI[3:length(RPI_CI)], 3)

RPI_CI$HaloRich <- ifelse(MarshZone == "Low", 0, RPI_CI$HaloRich)

RPI_Final

#Step 2 - Output Excel Files
  #To work with the data after R-script, the Mean and 95% CI for the RPI Scores will be
  # outputed. The file path is automated to include the Site, Year, and Marsh Zone. 


RPI_Final.output <- paste("E:\\Coastal Habitat Restoration Team\\Living Shorelines - New Hampshire\\Data Analysis\\Manuscript\\RPI Calculations\\Bootstrap R Files March 23\\Mean\\", Shoreline,Year,MarshZone, ".csv", sep = "")

RPI_Final.output

RPI_CI.output <- paste("E:\\Coastal Habitat Restoration Team\\Living Shorelines - New Hampshire\\Data Analysis\\Manuscript\\RPI Calculations\\Bootstrap R Files March 23\\CI\\", Shoreline,Year,MarshZone, ".csv", sep = "")

RPI_CI.output

write.csv(RPI_Final, RPI_Final.output)

write.csv(RPI_CI, RPI_CI.output)


rm(Nekton_subset, PW_subset, Veg_subset, RPI_CI, RPI_Final, rpi.bootstrap)










#-------------------------------------------------------------------------------

#Chapter 3: Graph RPI Results of Bootstrap Analysis
  #In the original manuscript submission, the original RPI analysis was graphed as a
  # chronosequence of all the sites together as well as each site separately


#Page 1 - Chronosequence of unweighted RPI scores from the Bootstrap Analysis
  #After the Bootstrap Analysis, the RPI results were compiled in an Excel File

RPI.results <- read.csv("E:\\Coastal Habitat Restoration Team\\Living Shorelines - New Hampshire\\Data Analysis\\Manuscript\\Input CSVs\\RPI_CoreGroups_BootStrap__PWcorrected_March23.csv")


#Page 2 - Graph the Bootstrap Results of the Analysis

RPI.results$Group <- factor(RPI.results$Group, levels = c("Pore Water Chemistry", "Vegetation", "Nekton", "Total Site RPI"))

RPI.results <- RPI.results %>%
  mutate(LS_Age = ifelse(Site == "Cutts Cove", LS_Age - 0.15, 
                         ifelse(Site == "Wagon Hill Farm", LS_Age + 0.15, LS_Age)))



RPI.bootstrap.figure <- ggplot(data = filter(RPI.results, Group == "Total Site RPI"), 
                               aes(x = LS_Age, y = PW_Corrected_NoNekton)) +
  geom_point(aes(shape = Site,  fill = Site),
             size = 8) + 
  scale_shape_manual(values = c(22, 23, 24)) + 
  scale_fill_manual(values = wes_palette("FantasticFox1", n = 3)) +
  labs(x = "Project Age (yrs)", y = "RPI Score") +
  theme_bw() +
    scale_x_continuous(expand = c(0,0),
                       limits = c(-0.2, 5),
                       breaks = seq(0, 5, 1)) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0, 1, 0.20)) +
  theme_bw(base_family = "sans") + 
  theme(
    axis.title.x = element_text(size = 20, colour = "black"), 
    axis.title.y = element_text(size = 20, colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black"),
    axis.text.y = element_text(size = 20, colour = "black"),
    legend.text = element_text(size = 15, colour = "black"),
    strip.text.x = element_text(size = 22.5, colour = "black"),
    strip.background = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.10, 0.875),
    legend.background = element_rect(
      size = 0.5, linetype = "solid", 
      colour = "black"))
    facet_wrap(~Group, nrow = 2, ncol = 2)


RPI.bootstrap.figure

ggsave(RPI.bootstrap.figure, height = 10, width = 17, dpi = 600,
       limitsize = FALSE, units = "in",
       filename = "E:\\Coastal Habitat Restoration Team\\Living Shorelines - New Hampshire\\Data Analysis\\Manuscript\\Figures\\Third Resubmission\\UnweightedRPI_CoreGroups_Timeline_PWcorrected.jpg")





#Page 2 - Compilation of descriptive statistics of monitoring metrics

  # The metrics to be highlighted in the paper  by living shoreline and reference:
    #Vegetation - Halophyte Cover (Low Marsh), Halophyte Cover (High Marsh)
    # Pore water - Reduction Oxidation (Low Marsh), Reduction Oxidation (High Marsh)
    # Nekton - Trap Catch Rate, Adult Length

  # To do this, I'm going to create to 3 series of point graphs and then build them on top of eachother
    # as 2 columns and 3 rows 


# Step 1 - Load all of the data sets

Veg_sumstats <- read.csv("E:\\Coastal Habitat Restoration Team\\Living Shorelines - New Hampshire\\Data Analysis\\Manuscript\\Statistics\\Veg_SummStats.csv")

Veg_sumstats <- as.data.frame(Veg_sumstats)

PW_sumstats <- read.csv("E:\\Coastal Habitat Restoration Team\\Living Shorelines - New Hampshire\\Data Analysis\\Manuscript\\Statistics\\Porewater_SummStats.csv")

Nekton_sumstats <- read.csv("E:\\Coastal Habitat Restoration Team\\Living Shorelines - New Hampshire\\Data Analysis\\Manuscript\\Statistics\\Nekton_SummStats.csv")


# Step 1 - Halophyte Cover point graph


Veg_sumstats <- Veg_sumstats %>%
  filter(Type != "No Action Control")

Veg_sumstats$Type <- factor(Veg_sumstats$Type, levels = c("Living Shoreline", "Reference"))

Veg_sumstats$Site <- factor(Veg_sumstats$Site, levels = c("Cutts Cove", "North Mill Pond", "Wagon Hill Farm"))

Veg_sumstats$Transect <- factor(Veg_sumstats$Transect, levels = c("Low Marsh", "High Marsh"))


  #I was having terrible trouble preventing overlapping points and error bars in the vegetation and pore water
    # chemistry figures overlapping, thus making the graph unreadable. I tried postion_dodge() and jitter() options
    # and combinations, but the error bars and points would not align. I simply manually corrected the x-axis of LS_Age
    # with a two ifelse() statments to shift the sites +/- 0.15
Veg_sumstats <- Veg_sumstats %>%
  mutate(LS_Age = ifelse(Site == "Cutts Cove", LS_Age - 0.15, 
                         ifelse(Site == "Wagon Hill Farm", LS_Age + 0.15, LS_Age)))


Veg_sumstats

Veg.figure <- ggplot(data = Veg_sumstats, aes(x = LS_Age, y = Halophyte.m)) +
  geom_errorbar(aes(y = Halophyte.m,
                    ymin = Halophyte.m - Halophyte.se, ymax = Halophyte.m + Halophyte.se),
                    colour = "black", size = 0.75, width = 0.35, 
                    linetype = "longdash") +   
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


#Next, I want to calculate the halophyte cover of the low marsh living shoreline marshes across sites and years 
    # within 15 m and above 15 m of the sill

Veg_15m <- Veg %>%
  filter(Type  == "Living Shoreline", Transect == "Low", Season != 2018) %>%
  group_by(Site, Season, Less15m) %>%
    summarise(
      HaloCov.m = mean(Percent.Halo),
      HaloCov.se = sd(Percent.Halo)/sqrt(n())) %>%
ungroup()

Veg_15m[4:5] <- round(Veg_15m[4:5], 1)

Veg_15m_Table <- Veg_15m %>%
  mutate(HaloCov = paste(HaloCov.m, HaloCov.se, sep = " +/- "))


write.csv(Veg_15m_Table, "E:\\Coastal Habitat Restoration Team\\Living Shorelines - New Hampshire\\Data Analysis\\Manuscript\\Statistics\\Veg_15m_SummStast.csv")

#Step 3 - Reduction Oxidation Graph of Pore water chemistry



PW_sumstats <- PW_sumstats %>%
  filter(Type != "No Action Control")

PW_sumstats$Type <- factor(PW_sumstats$Type, levels = c("Living Shoreline", "Reference"))

PW_sumstats$Site <- factor(PW_sumstats$Site, levels = c("Cutts Cove", "North Mill Pond", "Wagon Hill Farm"))

PW_sumstats$Transect <- factor(PW_sumstats$Transect, levels = c("Low Marsh", "High Marsh"))


PW_sumstats <- PW_sumstats %>%
  mutate(LS_Age = ifelse(Site == "Cutts Cove", LS_Age - 0.15, 
                         ifelse(Site == "Wagon Hill Farm", LS_Age + 0.15, LS_Age)))


PW.figure <- ggplot(data = PW_sumstats, aes(x = LS_Age, y = Redox.m)) +
  geom_errorbar(aes(x = LS_Age, y = Redox.m,
                    ymin = Redox.m - Redox.se, ymax = Redox.m + Redox.se),
                    colour = "black", linetype = "longdash", size = 0.75, width = 0.35) +   
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




#Page 3 - Mummichog Trap Catch Rate Graph


# Step 1 - Mummichog Trap Catch Rate


Nekton_sumstats <- Nekton_sumstats %>%
  filter(Type != "No Action Control")


Nekton_sumstats$Type <- factor(Nekton_sumstats$Type, levels = c("Living Shoreline", "Reference"))

Nekton_sumstats$Site <- factor(Nekton_sumstats$Site, levels = c("Cutts Cove", "North Mill Pond", "Wagon Hill Farm"))

Nekton_sumstats <- Nekton_sumstats %>%
  mutate(LS_Age = ifelse(Site == "Cutts Cove", LS_Age - 0.15, 
                         ifelse(Site == "Wagon Hill Farm", LS_Age + 0.15, LS_Age)))


Nekton.trap.figure <- ggplot(data = Nekton_sumstats, aes(x = LS_Age, y = TrapCatch.m)) +
  geom_errorbar(aes(x = LS_Age, y = TrapCatch.m,
                    ymin = TrapCatch.m - TrapCatch.se, ymax = TrapCatch.m + TrapCatch.se),
                colour = "grey25", size = 0.75, width = 0.35, linetype = "longdash") +   
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



#Step 2 - Mummichog Adult Length



Nekton.length.figure <- ggplot(data = Nekton_sumstats, aes(x = LS_Age, y = AdultLength.m)) +
  geom_errorbar(aes(x = LS_Age, y = AdultLength.m,
                    ymin = AdultLength.m - AdultLength.se, ymax = AdultLength.m + AdultLength.se),
                colour = "black", linetype = "longdash", size = 0.75, width = 0.35) +   
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



Nekton.figure <- Nekton.trap.figure + Nekton.length.figure

Nekton.figure



Compilation.figure <- (PW.figure) / (Veg.figure) / (Nekton.figure)

Compilation.figure


ggsave(Compilation.figure, height = 20, width = 20, dpi = 600,
       limitsize = FALSE, units = "in",
       filename = "E:\\Coastal Habitat Restoration Team\\Living Shorelines - New Hampshire\\Data Analysis\\Manuscript\\Figures\\Third Resubmission\\Merics_DescriptiveStats.jpg")


#-----------------------------------------------------------------------------------

#Chapter 4 - Analysis and Visualization of the Algae and Invertebrate Data of Sills

  #In addition to monitoring vegetation plots throughout living shorelines, the sills at the lower edge
    # of the marsh were monitored for the development of algae cover and invertebrate colonization
  #10 randomly distributed plots were placed along the sills (rip rap = CC, WHF; coir fiber = NMP) and
    # the cover of algae was estimated and all macroinvertebrates were counted and identified


# Page 1 - Import the Summarized Sill Data set

Sill <- read.csv("E:\\Coastal Habitat Restoration Team\\Living Shorelines - New Hampshire\\Data Analysis\\Manuscript\\Input CSVs\\Sill_Cover_BaseData_2022.csv")

#Page 2 - Subset the Sill data set to the correct dates for each site


#Page 3 - Calculate Descriptive Statistics of the Sill data set

Sill_sumstats <- Sill %>%
  group_by(Site, Season, LS_Age, Type) %>%
    summarise(
      algae.m = mean(AlgaeCover),
      algae.se = sd(AlgaeCover)/sqrt(n()),
      
      invert.m = mean(InvertDensity),
      invert.se = sd(InvertDensity)/sqrt(n()),
      
      barnacle.m = mean(BarnacleDensity),
      barnacle.se = sd(BarnacleDensity)/sqrt(n()),
      
      NonFF.m = mean(NonFFDensity),
      NonFF.se = sd(NonFFDensity)/sqrt(n()))

Sill_sumstats[5:12] <- round(Sill_sumstats[5:12], 1)


write.csv(Sill_sumstats, "E:\\Coastal Habitat Restoration Team\\Living Shorelines - New Hampshire\\Data Analysis\\Manuscript\\Statistics\\Sill_SummStats.csv")


#Page 5 - Analysis of Sill Metrics
  
  #Comparisons for the Silly Analysis:
    # 1) Algae Cover over Time
    # 2) Snail & Crab Density over Time
    # 3) Barnacle Density over Time
    # 4) Algae Cover impact on Snail & Crab Density


#Step 1 - Build Model, Inspect Data, and Analyze Algae Cover Over Time


sill_analysis <- Sill %>%
  filter(Site != "North Mill Pond")


algae.time.figure <- ggplot(data = sill_analysis, aes(x = LS_Age, y = AlgaeCover)) +   
  geom_point(aes(fill = Site),
             size = 7.5, shape = 21, position = position_dodge(w = 0.2)) +
  scale_fill_manual(values = c("#DD8D29", "#46ACC8")) +
  xlab("Project Age (year)") +
  ylab(expression("Algae Cover (%)")) + 
  theme_bw() +
  scale_x_continuous(expand = c(0,0),
                     limits = c(-0.1, 4.2),
                     breaks = seq(0, 4.2, 1)) +
  scale_y_continuous(limits = c(-0.5, 100),
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
      colour = "black"))


algae.time.figure



#Step 2 - Build Model, Inspect Data, and Analyze Snail & Crab Density over Time

  # Snail and Crab Density were square - root transformed to meet visual inspection of heterogeneity &
    # normal distribution


nff.time.figure <- ggplot(data = sill_analysis, aes(x = LS_Age, y = NonFFDensity)) +   
  geom_point(aes(fill = Site),
             size = 6, shape = 21, position = position_dodge(w = 0.2)) +
  scale_fill_manual(values = c("#DD8D29", "#46ACC8"))+
  xlab("Snail & Crab Density (indiv." ~ m^-2 ~")") +
  ylab(expression("Algae Cover (%)")) + 
  theme_bw() +
  scale_x_continuous(expand = c(0,0),
                     limits = c(-0.1, 4.2),
                     breaks = seq(0, 4.2, 1)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(-1, 127),
                     breaks = seq(0, 125, 25)) + 
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
      colour = "black"))


nff.time.figure





# Step 3 - Build Model, Inspect Data, and Analyze Snail & Crab Density over Time


barnacle.time.figure <- ggplot(data = sill_analysis, aes(x = LS_Age, y = BarnacleDensity)) +   
  geom_point(aes(fill = Site),
             size = 7.5, shape = 21, position = position_dodge(w = 0.2)) +
  scale_fill_manual(values = c("#DD8D29", "#46ACC8")) +
  xlab("Project Age (year)") +
  ylab(expression("Barnacle Density (indiv." ~ m^-2 ~")")) + 
  theme_bw() +
  scale_x_continuous(expand = c(0,0),
                     limits = c(-0.1, 4.2),
                     breaks = seq(0, 4.2, 1)) +
  scale_y_continuous(limits = c(-0.5, 350),
                     breaks = seq(0, 350, 50)) + 
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
      colour = "black"))


barnacle.time.figure




#Step 4 - Build model, Inspect Data, and Analyze Snail and Crab Density to Algae Cover

#Step 1 - Build the Model & Assess the Model
  # Normality was improved with the square root transformation of non-filter feeder density
  # An outlier of the non-filter feeder density was identified (row 46, density = 206)

algae.nonff <- lm(sqrt(NonFFDensity) ~ poly(AlgaeCover, 2, raw = TRUE) + Site,
                  data = sill_analysis)


ff.outlier <- Outlier(sill_analysis$NonFFDensity, na.rm = TRUE)
algae.outlier <- Outlier(sill_analysis$AlgaeCover)

#Step 2 - Final Model and Analysis

algae.nonff <- lm(sqrt(NonFFDensity) ~ poly(AlgaeCover, 2, raw = TRUE) + Site,
                  data = filter(sill_analysis, NonFFDensity != ff.outlier))

autoplot(algae.nonff)

anova(algae.nonff)

summary(algae.nonff)


nff.algae.figure <- ggplot(data = sill_analysis, aes(x = AlgaeCover, y = NonFFDensity)) +   
  geom_point(aes(fill = Site),
             size = 7.5, shape = 21) +
  geom_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE),
              level = 0.95, colour = "black", alpha = 0.45) + 
  scale_fill_manual(values = c("#DD8D29", "#46ACC8"))+
  xlab("Algae Cover (%)") +
  ylab("Snail & Crab Density (indiv." ~ m^-2 ~")") + 
  theme_bw() +
  scale_x_continuous(expand = c(0,0),
                     limits = c(-1, 100),
                     breaks = seq(0, 125, 25)) +
  scale_y_continuous(limits = c(-15, 100),
                     breaks = seq(0, 100, 20)) + 
  theme(
    axis.title.x = element_text(size = 17.5, colour = "black"), 
    axis.title.y = element_text(size = 17.5, colour = "black"),
    axis.text.x = element_text(size = 17.5, colour = "black"),
    axis.text.y = element_text(size = 17.5, colour = "black"),
    legend.text = element_text(size = 15, colour = "black"),
    strip.text.x = element_text(size = 27.5, colour = "black"),
    strip.background = element_blank(),
    legend.title = element_blank(),
    legend.position = "none",
    legend.background = element_rect(
      size = 0.5, linetype = "solid", 
      colour = "black"))


nff.algae.figure







#Page 4 - Visualize the Descriptive Statistics of Algae Cover, Barnacle Density, and Snail + Crab Density

  # The figures for the sill analysis will be similar to the vegetation, pore water chemistry, and 
      # nekton figures from earlier in the code (Chapter 3)

Sill_sumstats$Site <- factor(Sill_sumstats$Site, levels = c("Cutts Cove", "North Mill Pond", "Wagon Hill Farm"))


Sill_sumstats <- Sill_sumstats %>%
  mutate(LS_Age = ifelse(Site == "Cutts Cove", LS_Age - 0.10, 
                         ifelse(Site == "Wagon Hill Farm", LS_Age + 0.10, LS_Age)))

# Step 1 - Algae Cover Descriptive Stats over Time
  
algae.figure <- ggplot(data = Sill_sumstats, aes(x = LS_Age, y = algae.m)) +
  geom_errorbar(aes(y = algae.m,
                    ymin = algae.m - algae.se, ymax = algae.m + algae.se),
                colour = "black", size = 0.75, width = 0.35, 
                linetype = "longdash") +   
  geom_point(aes(fill = Site, shape = Site),
             size = 7.5) +
  scale_fill_manual(values = wes_palette("FantasticFox1", n = 3)) +
  scale_shape_manual(values = c(22, 23, 24))  +
  xlab("") +
  ylab(expression("Algae Cover (%)")) + 
  theme_bw() +
  scale_x_continuous(expand = c(0,0),
                     limits = c(-0.1, 4.2),
                     breaks = seq(0, 4.2, 1)) +
  scale_y_continuous(limits = c(-0.5, 100),
                     breaks = seq(0, 100, 20)) + 
  theme(
    axis.title.x = element_text(size = 17.5, colour = "black"), 
    axis.title.y = element_text(size = 17.5, colour = "black"),
    axis.text.x = element_text(size = 17.5, colour = "black"),
    axis.text.y = element_text(size = 17.5, colour = "black"),
    legend.text = element_text(size = 15, colour = "black"),
    strip.text.x = element_text(size = 27.5, colour = "black"),
    strip.background = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.175, 0.80),
    legend.background = element_rect(
      size = 0.5, linetype = "solid", 
      colour = "black"))


algae.figure


#Step 2 - Filter Feeder (barnacle) Density

barnacle.figure <- ggplot(data = Sill_sumstats, aes(x = LS_Age, y = barnacle.m)) +
  geom_errorbar(aes(y = barnacle.m,
                    ymin = barnacle.m - barnacle.se, ymax = barnacle.m + barnacle.se),
                colour = "black", size = 0.75, width = 0.35, 
                linetype = "longdash") +   
  geom_point(aes(fill = Site, shape = Site),
             size = 7.5) +
  scale_fill_manual(values = wes_palette("FantasticFox1", n = 3)) +
  scale_shape_manual(values = c(22, 23, 24))  +
  xlab("Project Age (years)") +
  ylab(expression("Barnacle Density (indiv." ~ m^-2 ~")")) + 
  theme_bw() +
  scale_x_continuous(expand = c(0,0),
                     limits = c(-0.1, 4.2),
                     breaks = seq(0, 4.2, 1)) +
  scale_y_continuous(limits = c(-0.5, 200),
                     breaks = seq(0, 200, 50)) + 
  theme(
    axis.title.x = element_text(size = 17.5, colour = "black"), 
    axis.title.y = element_text(size = 17.5, colour = "black"),
    axis.text.x = element_text(size = 17.5, colour = "black"),
    axis.text.y = element_text(size = 17.5, colour = "black"),
    legend.text = element_text(size = 15, colour = "black"),
    strip.text.x = element_text(size = 27.5, colour = "black"),
    strip.background = element_blank(),
    legend.title = element_blank(),
    legend.position = "none",
    legend.background = element_rect(
      size = 0.5, linetype = "solid", 
      colour = "black"))


barnacle.figure



#Step 3 - Snail & Crab Density


NonFF.figure <- ggplot(data = Sill_sumstats, aes(x = LS_Age, y = NonFF.m)) +
  geom_errorbar(aes(y = NonFF.m,
                    ymin = NonFF.m - NonFF.se, ymax = NonFF.m + NonFF.se),
                colour = "black", size = 0.75, width = 0.35, 
                linetype = "longdash") +   
  geom_point(aes(fill = Site, shape = Site),
             size = 7.5) +
  scale_fill_manual(values = wes_palette("FantasticFox1", n = 3)) +
  scale_shape_manual(values = c(22, 23, 24))  +
  xlab("Project Age (years)") +
  ylab(expression("Snail & Crab Density (indiv." ~ m^-2 ~")")) + 
  theme_bw() +
  scale_x_continuous(expand = c(0,0),
                     limits = c(-0.1, 4.2),
                     breaks = seq(0, 4.2, 1)) +
  scale_y_continuous(limits = c(-0.5, 100),
                     breaks = seq(0, 100, 20)) + 
  theme(
    axis.title.x = element_text(size = 17.5, colour = "black"), 
    axis.title.y = element_text(size = 17.5, colour = "black"),
    axis.text.x = element_text(size = 17.5, colour = "black"),
    axis.text.y = element_text(size = 17.5, colour = "black"),
    legend.text = element_text(size = 15, colour = "black"),
    legend.title = element_blank(),
    legend.position = "none",
    legend.background = element_rect(
      size = 0.5, linetype = "solid", 
      colour = "black"))


NonFF.figure



# Page 3 - Creation of Sill Compilation Figure


Sill.figure <- (algae.figure + barnacle.figure) / (NonFF.figure + nff.algae.figure)

Sill.figure


ggsave(Sill.figure, height = 10, width = 17, dpi = 600,
       limitsize = FALSE, units = "in",
       filename = "E:\\Coastal Habitat Restoration Team\\Living Shorelines - New Hampshire\\Data Analysis\\Manuscript\\Figures\\Third Resubmission\\Sill_Compilation_Figure.jpg")


