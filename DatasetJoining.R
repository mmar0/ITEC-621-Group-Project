library(tidyverse)


standardizeUni <- function(universityCol){
  tempCol <- tolower(universityCol)
  tempCol <- gsub("\\s*\\([^\\)]+\\)","",as.character(tempCol))
  tempCol <- gsub(",","",as.character(tempCol))
  tempCol <- gsub(" - "," ",as.character(tempCol))
  tempCol <- gsub(" at "," ",as.character(tempCol))
  tempCol <- gsub(" in "," ",as.character(tempCol))
  tempCol <- gsub(" and "," & ",as.character(tempCol))
  tempCol <- nameReconciliation(tempCol)
  return(tempCol)
}

nameReconciliation <- function(tempCol) {
  tempCol <- gsub("state university of new york college ","state university of new york ",as.character(tempCol))
  tempCol <- gsub("american university washington d.c.", "american university", as.character(tempCol))
  tempCol <- gsub("california polytechnic state university san luis obispo", "cal poly san luis obispo", as.character(tempCol))
  tempCol <- gsub("cooper union for the advancement of science & art", "cooper union", as.character(tempCol))
  tempCol <- gsub("dowling college", "dowling college", as.character(tempCol))
  tempCol <- gsub("everest university tampa", "everest university", as.character(tempCol))
  tempCol <- gsub("fitchburg state college", "fitchburg state university", as.character(tempCol))
  tempCol <- gsub("florida metropolitan university", "everest university", as.character(tempCol))
  tempCol <- gsub("harvard college", "harvard university", as.character(tempCol))
  tempCol <- gsub("liu brooklyn", "long island university", as.character(tempCol))
  tempCol <- gsub("montana state university bozeman", "montana state university", as.character(tempCol))
  tempCol <- gsub("penn state university park", "pennsylvania state university", as.character(tempCol))
  tempCol <- gsub("robert morris college", "robert morris university", as.character(tempCol))
  tempCol <- gsub("rutgers the state university of new jersey", "rutgers university", as.character(tempCol))
  tempCol <- gsub("st. john's university new york", "st. john's university", as.character(tempCol))
  tempCol <- gsub("state university of new york albany", "university albany", as.character(tempCol))
  tempCol <- gsub("state university of new york buffalo", "university buffalo", as.character(tempCol))
  tempCol <- gsub("state university of new york farmingdale", "farmingdale state college", as.character(tempCol))
  tempCol <- gsub("university of alabama tuscaloosa", "university of alabama", as.character(tempCol))
  tempCol <- gsub("university of hawaii manoa", "university of hawaii", as.character(tempCol))
  tempCol <- gsub("university of maryland college park", "university of maryland", as.character(tempCol))
  tempCol <- gsub("university of minnesota twin cities", "university of minnesota", as.character(tempCol))
  tempCol <- gsub("university of missouri rolla", "missouri university of science & technology", as.character(tempCol))
  tempCol <- gsub("university of nebraska lincoln", "university of nebraska", as.character(tempCol))
  tempCol <- gsub("utah valley state college", "utah valley university", as.character(tempCol))
  tempCol <- gsub("embry-riddle aeronautical university daytona beach", "embry-riddle aeronautical university", as.character(tempCol))
  return(tempCol)
}

salaryUniType <- read_csv("salaries-by-college-type.csv", na = "N/A", col_types = cols("c", "c", "n", "n", "n", "n", "n", "n"))
salaryRegion <- read_csv("salaries-by-region.csv", na = "N/A", col_types = cols("c", "c", "n", "n", "n", "n", "n", "n"))
degreePayBack <- read_csv("degrees-that-pay-back.csv", na = "N/A", col_types = cols("c", "n", "n", "n", "n", "n", "n", "n"))
tuitionCosts <- read_csv("Costs of Tuition in the US 2018.csv", col_types = cols("c", "n", "n"))

head(salaryUniType)
head(salaryRegion)
head(degreePayBack)
head(tuitionCosts)

tuitionCosts$University <- standardizeUni(tuitionCosts$University)
salaryRegion$`School Name` <- standardizeUni(salaryRegion$`School Name`)
salaryUniType$`School Name` <- standardizeUni(salaryUniType$`School Name`)

#solve Embry Riddle in Region

embryRiddle <- salaryUniType %>% filter(`School Name` == "embry-riddle aeronautical university")
colnames(embryRiddle) <- c("School Name", "Region", "Starting Median Salary",
                           "Mid-Career Median Salary", "Mid-Career 10th Percentile Salary",
                           "Mid-Career 25th Percentile Salary", "Mid-Career 75th Percentile Salary", 
                           "Mid-Career 90th Percentile Salary")
embryRiddle$Region <- "Southern"
salaryRegion <- rbind(salaryRegion, embryRiddle)
rm(embryRiddle)

#Solved typeRegionDif <- anti_join(salaryUniType, salaryRegion, by = c("School Name" = "School Name"))
regionTypeDif <- anti_join(salaryRegion, salaryUniType, by = c("School Name" = "School Name"))
View(regionTypeDif)
typeCostDif <- anti_join(salaryUniType, tuitionCosts, by = c("School Name" = "University"))
View(typeCostDif)
regionCostDif <- anti_join(salaryRegion, tuitionCosts, by = c("School Name" = "University"))
View(regionCostDif)

