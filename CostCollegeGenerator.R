# Load rvest to webscrape tidyverse to handle strings
library(rvest)
library(tidyverse)

#Data Frame of schools cost of attendance
uniCost <- data.frame("University" = character(), "CostIState" = numeric(), "CostOState" = numeric(), stringsAsFactors = FALSE)
#Data Frame of schools where scraping returned an NA or had more than 2 cost of attendance
schoolAnom <- data.frame("University" = character(), "IndexVal" = numeric(), "SchoolID" = numeric(), "Reason" = character(), stringsAsFactors = FALSE)
#Data Frame of schools tuition and fees
tuition <- data.frame("University" = character(), "CostIState" = numeric(), "CostOState" = numeric(), stringsAsFactors = FALSE)
#Data Frame of schools where scraping returned an NA or had more than 2 tuition rates
tutAnom <- data.frame("University" = character(), "IndexVal" = numeric(), "SchoolID" = numeric(), "Reason" = character(), stringsAsFactors = FALSE)

# Cost of Attendance = Tuition and Fees + Room and Board + Books and Supplies + Other Expenses
# Tuition is Tuition and Fees

# indexer for uniCost
index <- 1
# indexer for schoolAnom
sAIndex <- 1
# indexer for tutAnom
tAIndex <- 1
#Valid id nums from the website
IDno <- 6:2200
#Initializes attCostVal
attCostVal <- NULL


#Scrape and return a cleansed value for cost of attending
costAtFun <- function(url, childNo){
  #If childNo 1 then cost of attendence if 2 then tuition
  attCost <- url %>%
    read_html() %>%
    html_node(stringr::str_c("#section9 tr:nth-child(",childNo,") td"))
  
  #Creating a list of strings
  attCostVal <<- str_extract_all(html_text(attCost),boundary('word'))
  #Some lists of strings are nested in another list forces an unnest
  attCostVal <<- attCostVal[[1]]
  #Removes commas dollar sign so numbers are condensed
  attCostVal <<- gsub('\\$|,', '', attCostVal)
  #turns strings into numbers if string is words returns NA
  attCostVal <<- as.numeric(gsub("([0-9]+).*$", "\\1", attCostVal))
  #Removes NA values 
  attCostVal <<- na.omit(attCostVal)
  return(attCostVal)
}




#Looping over the website IDno vector
for(i in IDno){

# Attaching the id to the end of the url to make it a valid url  
url <- stringr::str_c("https://www.collegedata.com/cs/data/college/college_pg03_tmpl.jhtml?schoolId=", i)

# Scrapes the header of the webpage which contains the name of the school
name <- url %>%
  read_html() %>%
  html_node("h1")
#If no page: h1 scraped is "Retrieve a Saved Search" will skip to next number
if(str_detect(html_text(name), "Retrieve a Saved Search")){
  next()
}
uniCost[index,1] <- html_text(name)
tuition[index,1] <- html_text(name)

# Calls costAtFun() for cost of Attendance
attCostVal <- costAtFun(url, 1)


#If length 0 the website normally said NA because it is something like military academy
#If length 1 the tuition rates are the same for instate and out of state
#If length 2 compare instate to out of state, instate will be lower
#If length >2 indexed on the anomally list and needs to be looked at further
if(length(attCostVal)== 0){
  uniCost[index, 2] <- NA
  uniCost[index, 3] <- NA
  schoolAnom[sAIndex,] <- c(uniCost[index,1], index, i, "Not Applicable")
  sAIndex = sAIndex + 1
}else if(length(attCostVal)== 1){
  uniCost[index, 2] <- attCostVal[1]
  uniCost[index, 3] <- attCostVal[1]
}else if(length(attCostVal)== 2){
  if(attCostVal[1] > attCostVal[2]){
  uniCost[index, 2] <- attCostVal[2]
  uniCost[index, 3] <- attCostVal[1]
  }else{
    uniCost[index, 2] <- attCostVal[1]
    uniCost[index, 3] <- attCostVal[2]
  }
}else{
  uniCost[index, 2] <- NA
  uniCost[index, 3] <- NA
  schoolAnom[sAIndex,] <- c(uniCost[index,1], index, i, "Greater Than 2")
  sAIndex = sAIndex + 1
}

#Calls costAtFun for tuition
attCostVal <- costAtFun(url, 2)

if(length(attCostVal)== 0){
  tuition[index, 2] <- NA
  tuition[index, 3] <- NA
  tutAnom[tAIndex,] <- c(tuition[index,1], index, i, "Not Applicable")
  tAIndex = tAIndex + 1
}else if(length(attCostVal)== 1){
  tuition[index, 2] <- attCostVal[1]
  tuition[index, 3] <- attCostVal[1]
}else if(length(attCostVal)== 2){
  if(attCostVal[1] > attCostVal[2]){
    tuitiont[index, 2] <- attCostVal[2]
    tuition[index, 3] <- attCostVal[1]
  }else{
    tuition[index, 2] <- attCostVal[1]  
    tuition[index, 3] <- attCostVal[2]
  }
}else{
  tuition[index, 2] <- NA
  tuition[index, 3] <- NA
  tutAnom[tAIndex,] <- c(tuition[index,1], index, i, "Greater Than 2")
  tAIndex = tAIndex + 1
}

index = index + 1
print(paste0("Number of Schools Remaining ",(1793-index-163)))

}

View(uniCost)
write.csv(uniCost, file = "Costs of Attending Universities in the US 2018.csv", row.names = FALSE)
write.csv(schoolAnom, file = "CostOfAttendingAnomalies.csv", row.names = FALSE)
write.csv(tuition, file = "Costs of Tuition in the US 2018.csv", row.names = FALSE)
write.csv(tutAnom, file = "TuitionAnomalies.csv", row.names = FALSE)

# oldUniCost <- read.csv("Costs of Attending Universities in the US 2018.csv", header = TRUE, stringsAsFactors = FALSE)
# oldschoolAnom <- read.csv("CostOfAttendingAnomalies.csv", header = TRUE, stringsAsFactors = FALSE)
# oldTuition <- read.csv("Costs of Tuition in the US 2018.csv", header = TRUE, stringsAsFactors = FALSE)
# oldTutAnom <- read.csv("TuitionAnomalies.csv", header = TRUE, stringsAsFactors = FALSE)
# 
# uniCost <- rbind(oldUniCost, uniCost) 
# schoolAnom <- rbind(oldschoolAnom, schoolAnom) 
# tuition <- rbind(oldTuition, tuition)
# tutAnom <- rbind(oldTutAnom, tutAnom)


# Use if needing to fix already written costs table
# oldUniCost<-uniCost[!(uniCost$University== "Retrieve a Saved Search"),]
# schoolAnom<-schoolAnom[!(schoolAnom$University== "Retrieve a Saved Search"),]
# 208 & 31


##Commented out code used to open webpages of anomalys of unreported 5 at a time press enter in console to continue
# for(i in 1:nrow(tutAnom)) {
#   url <- stringr::str_c("https://www.collegedata.com/cs/data/college/college_pg03_tmpl.jhtml?schoolId=", tutAnom[i, "SchoolID"])
#   browseURL(url)
#   if(i%%5 == 0){
#     readline()
#   }
#}


#####Test/Debug ----------------------------


# id nums 6:1799
# 
# IDno <- 1799
# url <- stringr::str_c("https://www.collegedata.com/cs/data/college/college_pg03_tmpl.jhtml?schoolId=", IDno)

# Cost of Attendance use: "#section9 tr:nth-child(1) td"
# Tuition and fees only use: "#section9 tr:nth-child(2) td"



#In state versus out of state tuition example UCLA id = 1093
#Air Force id = 700

# UCLAurl <- stringr::str_c("https://www.collegedata.com/cs/data/college/college_pg03_tmpl.jhtml?schoolId=", 1093)
# attCost <- UCLAurl %>%
#   read_html() %>%
#   html_node("#section9 tr:nth-child(1) td")
# html_text(attCost)
# attCostVal <- str_extract_all(html_text(attCost),boundary('word'))
# attCostVal[[1]] <- "31,916" 
# attCostVal <- attCostVal[[1]]
# attCostVal <- gsub('\\$|,', '', attCostVal)
# attCostVal <- as.numeric(gsub("([0-9]+).*$", "\\1", attCostVal))
# attCostVal <- na.omit(attCostVal)
# 
# length(attCostVal)
# #Returns 2
# attCostVal[1] > attCostVal[2]
# 
# 
# AirforceUrl <- stringr::str_c("https://www.collegedata.com/cs/data/college/college_pg03_tmpl.jhtml?schoolId=", 700)
# attCost <- AirforceUrl %>%
#   read_html() %>%
#   html_node("#section9 tr:nth-child(1) td")
# html_text(attCost)
# attCostVal <- str_extract_all(html_text(attCost),boundary('word'))
# attCostVal[[1]] <- "31,916" 
# attCostVal <- attCostVal[[1]]
# attCostVal <- gsub('\\$|,', '', attCostVal)
# attCostVal <- as.numeric(gsub("([0-9]+).*$", "\\1", attCostVal))
# attCostVal <- na.omit(attCostVal)
# length(attCostVal)
# #Returns 0
# 
# 
#  UCLAurl <- stringr::str_c("https://www.collegedata.com/cs/data/college/college_pg03_tmpl.jhtml?schoolId=", 17)
#  attCost <- UCLAurl %>%
#    read_html() %>%
#    html_node("#section9 tr:nth-child(1) td")
#  html_text(attCost)
#  attCostVal <- str_extract_all(html_text(attCost),boundary('word'))
#  attCostVal <- attCostVal[[1]]
#  attCostVal <- gsub('\\$|,', '', attCostVal)
#  attCostVal <- as.numeric(gsub("([0-9]+).*$", "\\1", attCostVal))
#  attCostVal <- na.omit(attCostVal)
# 
#  length(attCostVal)
#  #Returns 2
#  attCostVal[1] > attCostVal[2]

#angela check to see if git is good
#George check to see if git works