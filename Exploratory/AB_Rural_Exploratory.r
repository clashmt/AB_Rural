#libraries
library(tidyverse)
library(dplyr)
library(knitr)
library(tableone)
library(ggplot2)
options(scipen=999)
library(mediation)
library(MASS)
library(CMAverse)
library(mice)
library(stringr)
library(psych)
library(sjPlot)
library(gtools)
library(drc)
library(lm.beta)
library(apaTables)


#set working directory
setwd('/Users/michaelwilliams/Documents/MGH T32/Research/AB/Analysis/AB_Rural/Exploratory')
getwd()

#### data prep #####################################
#data import -- baseline participant data from Bob
data <- read.csv('/Users/michaelwilliams/Documents/MGH T32/Research/AB/Data/Baseline/Scored Baseline Data from Bob/Most Recent 2.4.26/AB-ExpandedScored-Formatted.dt2.4.2026 WITH RBANS AGE 6MWT.csv')
dim(data)
head(data)

#data import -- baseline participant data from Bob
rural <- read.csv('/Users/michaelwilliams/Documents/MGH T32/Research/AB/Analysis/AB_Rural/Analytic Dataset/AB_Rural_Analytic.csv')
colnames(rural)[colnames(rural) == "USER_record_id"] <- "record_id"
dim(rural)
#head(rural)
names(rural)

#Merge the data
merged_data <- merge(data, rural, by = "record_id", all.x = TRUE)

# Check that row counts match (data should equal merged_data)
nrow(data)
nrow(merged_data)

# Check for any duplicate record_ids created by the merge
sum(duplicated(merged_data$record_id))

# See how many records successfully matched
sum(!is.na(merged_data$PrimaryRUCADescription))

#Create binary urban/rural measure. 0 == urban, 1 == rural
merged_data$rural_binary <- ifelse(merged_data$PrimaryRUCA <= 3, 0, 1)

table(merged_data$rural_binary, useNA = "always")

table(merged_data$UrbanCoreType, useNA = "always")

#Two participants have an NA RUCA code. 1 is a PO box that likely didn't geocode properly, other is truly missing an address
merged_data %>%
  filter(is.na(rural_binary)) %>%
  glimpse()

data_final <- merged_data[!is.na(merged_data$rural_binary), ]
dim(data_final)
###############################################
#### descriptive statistics #####################################

# national % of folks in urban == 80.1, national % of folks in rural == 19.4 
#according to RUCA codes


#measures of interest
#demographics: age_consent, education
#geographic: rural_binary
#physical function: step_count, PhysFunc_Tscore
#emotional function: Toronto_Total_Score, Depression_Tscore, Anxiety_Tscore, MOCS_TotalImp_Score, GQ6_Total_Score
#cognition: rbans_tot
#pain: chronicpain_duration
#social: SSR_Tscore

###Manuscript Table 1
##MANUSCRIPT TABLE ONE 
#full table one for manuscript

table1vars <- c('age_consent', 
'education', 
'step_count', 
'PhysFunc_Tscore', 
'Toronto_Total_Score', 
'Depression_Tscore', 
'Anxiety_Tscore', 
'MOCS_TotalImp_Score', 
'GQ6_Total_Score', 
'rbans_tot',
'chronicpain_duration',
'SSR_Tscore'
)

table1factorvars <-  table1vars[table1vars %in% c("education", "chronicpain_duration")]

table1 <- CreateTableOne(data = data_final, addOverall = TRUE, vars = table1vars, factorVars = table1factorvars, strata = "rural_binary")

print_table1 <- print(table1, nonnormal = table1vars, quote = FALSE, format = 'fp', explain=TRUE)
write.csv(print_table1, file = 'table1.csv')


#### AIM 1 #####
#Compare our sample percentage of rural vs national rural
# First, get your sample info
n_rural <- sum(data_final$rural_binary == 1)
n_total <- nrow(data_final)
sample_prop <- n_rural / n_total

# National rural proportion (you'll need to specify this - it's around 14-20% depending on definition)
national_prop <- 0.194  # adjust to whatever definition you're using

# Proportion test
prop.test(x = n_rural, n = n_total, p = national_prop)
#p-value <0.0001
#ci = 6.3% - 13.8% -- this whole interval is WAY below 19.4% national average


#### AIM 2 #####
