# Data Wrangling with R - July 11, 2024
# Using the package 'tidyverse' 
# This is the updated file - updated on July 11, 2024

library(tidyverse)
setwd('C:/VANDANA_S/R_workshop')

# Download actual Dataset from the URL 
NHANES <- read_csv('https://raw.githubusercontent.com/GTPB/PSLS20/master/data/NHANES.csv')

#DATA EXPLORATION........
# Using "spec" to find the guessed column specifications and column names
spec(NHANES)
head(NHANES)

# find column and row names of the tibble
colnames(NHANES)
rownames(NHANES)


# Extract only the columns Age, BMI, and BPSysAve, SurveyYr, and Gender
#renamed BPSysAve as BloodPressure while subsetting data
nhanes <- NHANES %>% select(ID, Age, BMI, BloodPressure = BPSysAve, SurveyYr, Gender)
head(nhanes)

#rename multiple columns of the dataset after subsetting data -- using 'rename' function from dplyr
nhanes <- nhanes %>% 
  rename(
     BP = BloodPressure,
     Age_yr = Age
  )
head(nhanes)

# Ways to access columns
nhanes$ID     # same as nhanes[['ID]]
nhanes['ID']  # prints first 10 values, use print(colname, n=...) to see more rows
nhanes[['ID']]

# Accessing elements of dataset 
nhanes[2, "Gender"]  # a single element
nhanes[ , c("SurveyYr", "Gender")] # all rows, two columns
nhanes[1:6, ]             # rows 1-6, all columns
nhanes[ , ]               # everything

# Change the column type of "ID" from "double" to "character"
nhanes$ID <- as.character(nhanes$ID)

# Display the first few rows of the subset after converting column 'ID' to character
head(nhanes)
dim(nhanes) #dimension of the data (Total_rows x Total_columns)
str(nhanes) # Details of the data by columns

#find the class of the data object "nhanes"
class(nhanes)
typeof(nhanes)

# Using 'table' function for frequency table
table(nhanes$Gender)

# Find the summary statistics of the dataset and save the output in another file
sink('sumary_stats_before.txt')
print('Summary statistics of the data before cleaning:....')
summary(nhanes)
sink()

summary(nhanes)
# Using function 'unite' and 'separate' for column 'SurveyYr'
nhanes1 <- separate(nhanes, SurveyYr, into = c("Year1", "Year2"),
                    sep = '_', remove=TRUE) 
head(nhanes1)

nhanes <- unite(nhanes1, "SurveyYr", c("Year1", "Year2"), sep="_")
head(nhanes)

# CLEANING AND SUBSETTING THE DATA........
# Identify duplicate elements
print('Duplicated rows...')
duplicated(nhanes)

# count of duplicated data
print('count of duplicate rows')
sum(duplicated(nhanes))

# remove duplicate rows using both "unique" and "distinct"
data_uniq <- unique(nhanes)
data_dist <- distinct(nhanes)

head(data_uniq)
dim(data_uniq)

# remove duplicate rows using distinct -- needs tidyverse/dplyr
head(data_dist)
tail(data_dist)
dim(data_dist)

# CHECK -- Find the arguments of both "unique" and "distinct" function for enhanced application
# ?distinct, ?unique

# Replacing specific string in the whole column with another string
sub('5*','subj', data_dist$ID)
sub('subj','5', data_dist$ID)
tail(data_dist)

# Subsetting the data using condition on columns using "filter" function
# Condition -- Find all columns where Age is 34 and gender is 'male'
age_34_data <- filter(data_dist, Age_yr == 34, Gender == 'male')
age_34_data
dim(age_34_data)

#Condition --  find the data for age between 34 to 40
age_range_data <- data_dist %>% filter(between(data_dist$Age_yr, 34, 40), Gender == 'male')
age_range_data

#Condition --  find the data for age greater than 50
age_gt_data <- data_dist %>% filter(data_dist$Age_yr>50, Gender == 'male')
age_gt_data

#HANDLING MISSING DATA......
# Count the number of rows with column values as NA
cat('Number of missing values in column BloodPressure:', sum(is.na(data_dist$BP)))
cat('Number of missing values in column BMI:', sum(is.na(data_dist$BMI)))
cat('Number of missing values in column Age:',sum(is.na(data_dist$Age_yr)))
cat('Number of missing values in column ID:',sum(is.na(data_dist$ID)))

# Data imputation - replacing NA by some other values
# Replace NA by column 'mean'
impData_mean <- data_dist    #create a copy of original data
impData_mean$BP[is.na(impData_mean$BP)] <- mean(impData_mean$BP, na.rm = TRUE) # replace one column
impData_mean$BMI[is.na(impData_mean$BMI)] <- mean(impData_mean$BMI, na.rm = TRUE)
cat(' The number of NA in imputed dataframe', sum(is.na(impData_mean$BP)))

# Replace NA by some value (say 0)
impData_zero <- data_dist 
impData_zero$BP[is.na(impData_zero$BP)] <- 0
head(impData_zero)

# Remove all the rows with NA entries
dataNo_na <- data_dist
cat('Before deleting all NA rows', sum(is.na(dataNo_na)))
dataNo_na <- na.omit(dataNo_na)
cat('After deleting all NA rows', sum(is.na(dataNo_na)))

# Removing soecific columns with NA
rmCol_na <- data_dist %>% drop_na(BP)
sum(is.na(rmCol_na))

# USING APPLY FUNCTIONS FOR DATA WITH NO NA VALUES......
# Find min, length of ALL the columns
apply(dataNo_na, 2, min)   # MARGIN = 1 (for row), 2 (for column)
apply(dataNo_na, 2, length) # find the length of each column

#LAPPLY - Apply over a list or vector
num_cols <- list(dataNo_na$Age_yr, dataNo_na$BMI, dataNo_na$BP)
lapply(num_cols, mean)  # output as list
sapply(num_cols, mean) # output as vector

# USING GROUP_BY FOR GROUPING DATA BY COLUMNS
# By one column - 'Gender'
dataGrp.gender <- dataNo_na %>% group_by(Gender)  %>%
  summarise(mean_age = mean(Age_yr), 
            mean_BP = mean(BP), 
            mean_BMI = mean(BMI), 
            .groups = 'drop')
print('The mean for age, BP, and BMI by gender is:')
print(dataGrp.gender)

# By multiple columns - 'Gender' and 'SurveyYr'
dataGrp.multi <- dataNo_na %>% group_by(Gender, SurveyYr)  %>%
  summarise(mean_age = mean(Age_yr), 
            mean_BP = mean(BP), 
            mean_BMI = mean(BMI))
print('The mean for age, BP, and BMI by gender and year is:')
print(dataGrp.multi)

# similarly we can use min, max, sum, count function to aggregate

