
###################################################################################
### Pulling Data from Census Microdata (IPUMS) - Example Code
### Pulling 2021 Data for Chicago
###################################################################################

### Set wd
setwd("path/to/directory")

### Load Libraries

# Pulling Census Data
library(tidycensus)

# Data Manipulation
library(tidyverse)
library(data.table)
library(dplyr)

# Additional Useful Data Manipulation Packages
library(stringr)
library(reshape2)

# Plotting/Graphing
library(ggplot2)

# Mapping (not necessary)
library(sp)
library(rgdal)
library(sf)
library(rgeos)
library(tmap)
library(leaflet)

# Creating Confidence Intervals
library(survey)
library(srvyr)

### Install api key
census_api_key("<YOUR KEY HERE>")

### PUMS Variables <- Useful Reference for deciding variables to pull
pums_variables_list<-pums_variables %>% filter(year==2021)

pums_vars_2021 <- pums_variables %>%  ### Does not work
  filter(year == 2021, survey == "acs1")

pums_vars_person<-pums_variables %>% 
  distinct(var_code, var_label, data_type, level) %>% 
  filter(level == "person")

pums_vars_hh<-pums_variables %>% 
  distinct(var_code, var_label, data_type, level) %>% 
  filter(level == "housing")


############################################################
### Pull Data
############################################################

raw_2021_data <- get_pums(
  variables = c("SEX","RAC1P","HISP", "AGEP", "SCH","ESR","SCHL",
                "HICOV",
                "MIGPUMA","POWPUMA","PUMA","JWMNP",
                
                "BROADBND","HISPEED","LAPTOP","SMARTPHONE","TABLET"
  ),
  state = "IL",
  survey = "acs1",
  year = 2021
)

############################################################
### Clean Data <- The data does not come with intuitive labels so you need to clean it up
# Note! Codes from the codebook on the IPUMs website may be different than the codes here
############################################################

clean_2021_data <- raw_2021_data %>%
  mutate(
    year=2021,
    PUMA_numeric=as.numeric(PUMA),
    place_of_work_PUMA_numeric=as.numeric((PUMA)),
    migration_PUMA_numeric=as.numeric(MIGPUMA)
  ) %>%
  filter(
    between(PUMA_numeric,3500,3550), # filters to just Chicago
    between(AGEP,16,24) # filters to just youth
  ) %>%
  mutate(
    sex = case_when(
      SEX == 1 ~ "Male",
      SEX == 2 ~ "Female"
    ),
    
    race = case_when(
      as.numeric(HISP) == 1 & as.numeric(RAC1P) == 1 ~ "White",
      as.numeric(HISP) == 1 & as.numeric(RAC1P) == 2 ~ "Black",
      as.numeric(HISP) == 1 & as.numeric(RAC1P) > 2 ~ "Other",
      as.numeric(HISP) >= 2 ~ "Hispanic"
    ),
    
    school_enrollment_status = case_when(
      SCH == 1 ~ "Not in school",
      SCH  > 1 ~ "In School"
    ),
    
    employment_status = case_when(
      ESR %in% c(3,6) ~ "Not Working",
      ESR %in% c(1,2) ~ "Civilian Employed",
      ESR %in% c(4,5) ~ "Armed Forces"
    ),
    
    employment_status_detailed = case_when(
      ESR == 3 ~ "Unemployed",
      ESR == 6 ~ "Not in Labor Force",
      ESR %in% c(1,2) ~ "Civilian Employed",
      ESR %in% c(4,5) ~ "Armed Forces"
    ),
    
    educational_attainment = case_when(
      as.numeric(SCHL) %in% c(16,17) ~ "HS or equivalent",
      as.numeric(SCHL) %in% c(20:24) ~ "College Degree",
      as.numeric(SCHL) %in% c(18:19) ~ "Some College",
      as.numeric(SCHL) %in% c(1:15) ~ "Less than HS"
    ),

    health_insurance_coverage = case_when(
      HICOV == 1 ~ "Yes",
      HICOV == 2 ~ "No"
    ),
    
    broadband_access = case_when(
      BROADBND == 1 ~ "Yes",
      BROADBND == 2 ~ "No"
    ),
    
    highspeed_internet_access = case_when(
      HISPEED == 1 ~ "Yes",
      HISPEED == 2 ~ "No"
    ),
    
    computer_access = case_when(
      LAPTOP == 1 ~ "Yes",
      LAPTOP == 2 ~ "No"
    ),
    
    smartphone_access = case_when(
      SMARTPHONE == 1 ~ "Yes",
      SMARTPHONE == 2 ~ "No"
    ),
    
    tablet_access = case_when(
      SMARTPHONE == 1 ~ "Yes",
      SMARTPHONE == 2 ~ "No"
    )
    
  ) %>%
  
  rename( # for vars that don't need to be recoded
    travel_time_to_work=JWMNP
  ) %>%
  
  mutate( # Opportunity Youth Var
    
    oy_status = case_when(
      employment_status == "Not Working" & school_enrollment_status == "Not in school" ~ "OY",
      employment_status == "Civilian Employed" | school_enrollment_status == "In School" ~ "CY"
    )
  )

### Save Data for future reference so you don't need to pull the data each time
write.csv(clean_2021_data,"census_dat.csv",row.names = F)


############################################################
### Sample Analysis
### The key thing to be aware of is that you need to use the person weight to get the correct numbers
############################################################

### Number of OY in Chicago
clean_2021_data %>%
  filter(employment_status != "Armed Forces") %>%
  group_by(oy_status) %>%
  summarise(
    raw_count = n(), # unweighted number
    count = sum(PWGTP), # weighted number
    avg_age = weighted.mean(AGEP,w=PWGTP) # Average Age accounting for weights
            ) %>%
  mutate(
    percent = prop.table(count) # Getting rates
  )

### Number of OY by Sex with a bar chart
clean_2021_data %>%
  filter(employment_status != "Armed Forces") %>%
  group_by(sex,oy_status) %>%
  summarise(
    raw_count = n(), # unweighted number
    count = sum(PWGTP) # weighted number
  ) %>%
  mutate(
    percent = prop.table(count), # Getting rates
    percent_label = paste(round(percent*100,1),"%",sep="") # Making a better formatted percent for the bar chart
  ) %>%
  filter(oy_status == "OY") %>% # Just the disconnection rate
  ggplot(aes(sex,percent)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent_label),size = 5, vjust = 1.5,color = "white") +
  xlab("") + ylab("Percent %") +
  ggtitle("Disconnection Rate by Sex, 2021")
    

    