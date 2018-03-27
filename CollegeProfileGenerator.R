# Load rvest to webscrape tidyverse to handle strings
library(rvest)
library(tidyverse)


#Data Frame of schools cost of attendance
schoolProfile <- data.frame("University" = character(), "CollegeType" = character(), "Undergraduates" = numeric(), 
                            "EntranceDifficulty" = character(), "AdmitRate" = numeric(), "Applicants" = numeric(),
                            "City" = character(), "State" = character(), stringsAsFactors = FALSE)
#Data Frame of schools where scraping returned an NA or had more than 2 cost of attendance
schoolAnom <- data.frame("University" = character(), "IndexVal" = numeric(), "SchoolID" = numeric(), "Reason" = character(), stringsAsFactors = FALSE)

# indexer for schoolProfile
index <- 1
# indexer for schoolAnom
sAIndex <- 1
#Valid id nums from the website
IDno <- 6:2200
#Initializes attCostVal
collegeTypeValue <- NULL
numUndergradsValue <- NULL
entranceDifficultyValue <- NULL
admitRateValue <- NULL
schoolLocationValue <- NULL



#Scrape and return a cleansed value for cost of attending
collegeTypeFun <- function(url){
  
  collegeTypeNode <- url %>%
    read_html() %>%
    html_node('.overviewtext+ div tr:nth-child(2) td')
  #Creating a list of strings
  collegeTypeValue <<- str_extract_all(html_text(collegeTypeNode),boundary('word'))
  #Some lists of strings are nested in another list forces an unnest
  collegeTypeValue <<- collegeTypeValue[[1]]
  #Table name is changed in a few cases
  if(length(collegeTypeValue) == 0){
    return(NA)
  }
  if(is.na(collegeTypeValue)){
    collegeTypeNode <- url %>%
      read_html() %>%
      html_node('#cont_overview div:nth-child(1) tr:nth-child(2) td')
    #Creating a list of strings
    collegeTypeValue <<- str_extract_all(html_text(collegeTypeNode),boundary('word'))
    #Some lists of strings are nested in another list forces an unnest
    collegeTypeValue <<- collegeTypeValue[[1]]
  }
  #Removes commas dollar sign so numbers are condensed
  collegeTypeValue <<- gsub('\\$|,', '', collegeTypeValue)
  #Merges separated list of strings into single string
  collegeTypeValue <<- tolower(paste(collegeTypeValue, collapse = ' '))
  #Removes NA values 
  collegeTypeValue <<- na.omit(collegeTypeValue)
  return(collegeTypeValue)
}

numUndergradsFun <- function(url){
  
  numUndergradsNode <- url %>%
    read_html() %>%
    html_node('.overviewtext+ div tr:nth-child(4) td')
  
  #Creating a list of strings
  numUndergradsValue <<- str_extract_all(html_text(numUndergradsNode),boundary('word'))
  #Some lists of strings are nested in another list forces an unnest
  numUndergradsValue <<- numUndergradsValue[[1]]
  #Table name is changed in a few cases
  if(is.na(numUndergradsValue)){
    numUndergradsNode <- url %>%
      read_html() %>%
      html_node('#cont_overview div:nth-child(1) tr:nth-child(4) td')
    #Creating a list of strings
    numUndergradsValue <<- str_extract_all(html_text(numUndergradsNode),boundary('word'))
    #Some lists of strings are nested in another list forces an unnest
    numUndergradsValue <<- numUndergradsValue[[1]]
  }
  #Removes commas dollar sign so numbers are condensed
  numUndergradsValue <<- gsub('\\$|,', '', numUndergradsValue)
  #turns strings into numbers if string is words returns NA
  numUndergradsValue <<- as.numeric(gsub("([0-9]+).*$", "\\1", numUndergradsValue))
  #Removes NA values 
  numUndergradsValue <<- na.omit(numUndergradsValue)
  return(numUndergradsValue)
}

entranceDifficultyFun <- function(url){
  entranceDifficultyNode <- url %>%
    read_html() %>%
    html_node('#section0 tbody:nth-child(2) tr:nth-child(1) td')
  
  #Creating a list of strings
  entranceDifficultyValue <<- str_extract_all(html_text(entranceDifficultyNode),boundary('word'))
  #Some lists of strings are nested in another list forces an unnest
  entranceDifficultyValue <<- entranceDifficultyValue[[1]]
  #Removes commas dollar sign so numbers are condensed
  entranceDifficultyValue <<- gsub('\\$|,', '', entranceDifficultyValue)
  #Merges separated list of strings into single string
  entranceDifficultyValue <<- tolower(paste(entranceDifficultyValue, collapse = ' '))
  #turns strings into numbers if string is words returns NA
  #Removes NA values 
  entranceDifficultyValue <<- na.omit(entranceDifficultyValue)
  return(entranceDifficultyValue)
}

admitRateFun <- function(url){
  
  admitRateNode <- url %>%
    read_html() %>%
    html_node('#section0 tbody:nth-child(2) tr:nth-child(2) td')
  
  #Creating a list of strings
  admitRateValue <<- str_extract_all(html_text(admitRateNode),boundary('word'))
  #Some lists of strings are nested in another list forces an unnest
  admitRateValue <<- admitRateValue[[1]]
  #Removes commas dollar sign so numbers are condensed
  admitRateValue <<- gsub('\\$|,', '', admitRateValue)
  #turns strings into numbers if string is words returns NA
  admitRateValue <<- suppressWarnings(as.numeric(gsub("([0-9]+).*$", "\\1", admitRateValue)))
  #Removes NA values 
  admitRateValue <<- na.omit(admitRateValue)
  attr(admitRateValue, "na.action") <- NULL
  attr(admitRateValue, "class") <- NULL
  return(admitRateValue)
}


schoolLocationFun <- function(url){
  #If childNo 1 then cost of attendence if 2 then tuition
  schoolLocationNode <- url %>%
    read_html() %>%
    html_node('.citystate')
  
  #Creating a list of strings
  schoolLocationValue <<- str_extract_all(html_text(schoolLocationNode),boundary('word'))
  #Some lists of strings are nested in another list forces an unnest
  schoolLocationValue <<- schoolLocationValue[[1]]
  #Removes commas dollar sign so numbers are condensed
  schoolLocationValue <<- gsub('\\$|,', '', schoolLocationValue)
  schoolLocationValue <<- tolower(paste(schoolLocationValue, collapse = ' '))
  #Removes NA values 
  schoolLocationValue <<- na.omit(schoolLocationValue)
  return(schoolLocationValue)
}


#Looping over the website IDno vector
for(i in IDno){
  
  # Attaching the id to the end of the url to make it a valid url  
  url <- stringr::str_c("https://www.collegedata.com/cs/data/college/college_pg01_tmpl.jhtml?schoolId=", i)
  
  # Scrapes the header of the webpage which contains the name of the school
  name <- url %>%
    read_html() %>%
    html_node("h1")
  #If no page: h1 scraped is "Retrieve a Saved Search" will skip to next number
  if(is.na(html_text(name))){
    next()
  }
  if(str_detect(html_text(name), "Retrieve a Saved Search")){
    next()
  }
  schoolProfile[index,"University"] <- html_text(name)
  
  # Calls costAtFun() for cost of Attendance
  
  collegeTypeValue <- collegeTypeFun(url)
  numUndergradsValue <- numUndergradsFun(url)
  entranceDifficultyValue <- entranceDifficultyFun(url)
  admitRateValue <- admitRateFun(url)
  schoolLocationValue <- schoolLocationFun(url)
  

  if(length(collegeTypeValue)== 0){
    schoolProfile[index, "CollegeType"] <- NA
    schoolAnom[sAIndex,] <- c(schoolProfile[index,"University"], index, i, "CT Not Applicable")
    sAIndex = sAIndex + 1
  }else if(length(collegeTypeValue)== 1){
    schoolProfile[index, "CollegeType"] <- collegeTypeValue[1]
  }else{
    schoolProfile[index, "CollegeType"] <- NA
    schoolAnom[sAIndex,] <- c(schoolProfile[index,"University"], index, i, "CT Greater Than 1")
    sAIndex = sAIndex + 1
  }

  
  if(length(numUndergradsValue)== 0){
    schoolProfile[index, "Undergraduates"] <- NA
    schoolAnom[sAIndex,] <- c(schoolProfile[index,"University"], index, i, "NU Not Applicable")
    sAIndex = sAIndex + 1
  }else if(length(numUndergradsValue)== 1){
    schoolProfile[index, "Undergraduates"] <- numUndergradsValue[1]
  }else{
    schoolProfile[index, "Undergraduates"] <- NA
    schoolAnom[sAIndex,] <- c(schoolProfile[index,"University"], index, i, "NU Greater Than 1")
    sAIndex = sAIndex + 1
  }
  
  if(length(entranceDifficultyValue)== 0){
    schoolProfile[index, "EntranceDifficulty"] <- NA
    schoolAnom[sAIndex,] <- c(schoolProfile[index,"University"], index, i, "ED Not Applicable")
    sAIndex = sAIndex + 1
  }else if(length(entranceDifficultyValue)== 1){
    schoolProfile[index, "EntranceDifficulty"] <- entranceDifficultyValue[1]
  }else{
    schoolProfile[index, "EntranceDifficulty"] <- NA
    schoolAnom[sAIndex,] <- c(schoolProfile[index,"University"], index, i, "ED Greater Than 1")
    sAIndex = sAIndex + 1
  }
  
  if(length(admitRateValue)== 0){
    schoolProfile[index, "AdmitRate"] <- NA
    schoolProfile[index, "Applicants"] <- NA
    schoolAnom[sAIndex,] <- c(schoolProfile[index,"University"], index, i, "AR Not Applicable")
    sAIndex = sAIndex + 1
  }else if(length(admitRateValue)== 2){
    schoolProfile[index, "AdmitRate"] <- admitRateValue[1]/100
    schoolProfile[index, "Applicants"] <- admitRateValue[2]
  }else{
    schoolProfile[index, "AdmitRate"] <- NA
    schoolProfile[index, "Applicants"] <- NA
    schoolAnom[sAIndex,] <- c(schoolProfile[index,"University"], index, i, "AR Other Than 1")
    sAIndex = sAIndex + 1
  }
  
  if(length(schoolLocationValue)== 0){
    schoolProfile[index, "City"] <- NA
    schoolProfile[index, "State"] <- NA
    schoolAnom[sAIndex,] <- c(schoolProfile[index,"University"], index, i, "SL Not Applicable")
    sAIndex = sAIndex + 1
  }else if(length(schoolLocationValue)== 1){
    schoolProfile[index, "City"] <- word(schoolLocationValue, end = -2)
    schoolProfile[index, "State"] <- toupper(word(schoolLocationValue, -1))
  }else{
    schoolProfile[index, "City"] <- NA
    schoolProfile[index, "State"] <- NA
    schoolAnom[sAIndex,] <- c(schoolProfile[index,"University"], index, i, "SL Greater Than 1")
    sAIndex = sAIndex + 1
  }
  
  
  index = index + 1
  print(paste0("Number of Schools Remaining ",(1793-index-163)))
  
}
print("complete")
View(schoolProfile)
View(schoolAnom)
schoolProfile$University <- standardizeUni(schoolProfile$University)
tuitionCosts$University <- standardizeUni(tuitionCosts$University)
# test <- anti_join(tuitionCosts, schoolProfile)
write.csv(schoolProfile, file = "Profile of Colleges Minus Tuition.csv", row.names = FALSE)
fullSchoolProfile <- left_join(tuitionCosts, schoolProfile)
fullSchoolProfile <- unique(fullSchoolProfile)
write.csv(fullSchoolProfile, file = "School Profiles.csv", row.names = FALSE)


# View(uniCost)
# write.csv(uniCost, file = "Costs of Attending Universities in the US 2018.csv", row.names = FALSE)
# write.csv(schoolAnom, file = "CostOfAttendingAnomalies.csv", row.names = FALSE)
# write.csv(tuition, file = "Costs of Tuition in the US 2018.csv", row.names = FALSE)
# write.csv(tutAnom, file = "TuitionAnomalies.csv", row.names = FALSE)


#####Test/Debug ----------------------------
# i <- 1093 #UCLA
# i <- 475 #AU
# i <- 992
# i <- 12
# i <- 2016
# url <- stringr::str_c("https://www.collegedata.com/cs/data/college/college_pg01_tmpl.jhtml?schoolId=", i)
# 
# 
# costAtFun <- function(url, childNo){
#   #If childNo 1 then cost of attendence if 2 then tuition
#   attCost <- url %>%
#     read_html() %>%
#     html_node(stringr::str_c("#section9 tr:nth-child(",childNo,") td"))
# 
#   #Creating a list of strings
#   attCostVal <<- str_extract_all(html_text(attCost),boundary('word'))
#   #Some lists of strings are nested in another list forces an unnest
#   attCostVal <<- attCostVal[[1]]
#   #Removes commas dollar sign so numbers are condensed
#   attCostVal <<- gsub('\\$|,', '', attCostVal)
#   #turns strings into numbers if string is words returns NA
#   attCostVal <<- as.numeric(gsub("([0-9]+).*$", "\\1", attCostVal))
#   #Removes NA values
#   attCostVal <<- na.omit(attCostVal)
#   return(attCostVal)
# }
# 
# collegeTypeFun(url)
# numUndergradsFun(url)
# entranceDifficultyFun(url)
# admitRateFun(url)
# 
# attCost <- url %>%
#   read_html() %>%
#   html_node('.citystate')
# '.overviewtext+ div tr:nth-child(2) td'
# '.overviewtext+ div tr:nth-child(4) td'
# '#section0 tbody:nth-child(2) tr:nth-child(1) td'
# '#section0 tbody:nth-child(2) tr:nth-child(2) td'
# '.citystate'
