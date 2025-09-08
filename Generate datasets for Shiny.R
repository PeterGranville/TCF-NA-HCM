
#### Setup ####

library(scales)
library(tidyverse)

#### End ####

################################################
#### Load daatsets                          ####
################################################

#### Write function to remove columns ####

removeColumns <- function(data1){
  
  data1 <- data1 %>% select(
    
    # Remove variables we don't need 
    -(`Effy index`), 
    -(`Student index`), 
    -(`Effy-student index`), 
    -(`High school GPA >= 2.0`),
    -(`High school GPA >= 2.5`),
    -(`High school GPA >= 3.0`),
    -(`High school GPA >= 3.5`)
    
  ) %>% mutate(
    
    # Add a counting variable 
    `Count` = rep(1)
    
  )
  
  return(data1)
}

#### End #### 

# #### Load procesed output (LONG WAY: 20 MIN) ####
# 
# setwd("/Users/peter_granville/Fed State Modeling/Postprocessing data")
# 
# for(i in (1:176)){
# 
#   print(Sys.time())
# 
#   print(paste("Merging dataset ", i, ".", sep=""))
# 
#   if(i==1){
#     studentDF <- removeColumns(read.csv(
#       paste("Set-", i, ".csv", sep=""),
#       header=TRUE,
#       check.names=FALSE
#     )) %>% mutate(
#       `Source file number` = rep(i)
#     )
#   }else{
#     tempDF <- removeColumns(read.csv(
#       paste("Set-", i, ".csv", sep=""),
#       header=TRUE,
#       check.names=FALSE
#     )) %>% mutate(
#       `Source file number` = rep(i)
#     )
#     studentDF <- rbind(
#       studentDF,
#       tempDF
#     )
#     rm(tempDF)
#   }
# 
# }
# rm(i, removeColumns)
# 
# setwd("/Users/peter_granville/Fed State Modeling")
# 
# write.csv(studentDF, "All merged student data.csv", row.names=FALSE)
# 
# setwd("/Users/peter_granville/Fed State Modeling")
# 
# #### End ####

#### Load processed output (SHORT WAY: 2 MIN) ####

studentDF <- read.csv(
  "All merged student data.csv",
  header=TRUE,
  check.names=FALSE
)

#### End ####

################################################
#### Checking validity of studentDF         ####
################################################

#### Total/net variables ####

studentDF <- studentDF %>% mutate(
  
  # Calculate total grants, loans, and cost 
  `Total grants` = `Federal grant amount` + `VA/DOD grant amount` + `State grant amount` + `Institutional grant amount` + `Private grant amount`, 
  `Total loans` = `Federal loan amount` + `Parent loan amount`,
  `Total cost` = `Tuition and fees paid` + `Non-tuition expense budget`
  
) %>% mutate(
  
  # Calculate net price 
  `Net price` = pmax(0, `Total cost` - `Total grants`)
  
) %>% mutate(
  
  # Zero EFC status 
  `Zero EFC status` = ifelse(
    `EFC`==0, "Zero EFC", "Nonzero EFC"
  ), 
  
  # Receives any grants 
  `Receives any grants` = ifelse(
    `Total grants` > 0, "Yes", "No"
  ), 
  
  # Receives any loans 
  `Receives any loans` = ifelse(
    `Total loans` > 0, "Yes", "No"
  )
  
) 

#### End #### 

#### Aggregate sums #### 

dollar(sum(studentDF$`Federal grant amount`))
dollar(sum(studentDF$`VA/DOD grant amount`))
dollar(sum(studentDF$`State grant amount`))
dollar(sum(studentDF$`Institutional grant amount`))
dollar(sum(studentDF$`Private grant amount`))
dollar(sum(studentDF$`Federal loan amount`))
dollar(sum(studentDF$`Parent loan amount`))

#### End #### 

#### Write function to display distribution as percentages ####

showDistribution <- function(variableName){
  
  totalStudents <- sum(studentDF$`Count`)
  
  tempDF <- studentDF %>% select(
    all_of(variableName), 
    `Count`
  )
  names(tempDF)[1] <- "InterestVar"
  
  tempDF <- aggregate(
    data=tempDF, 
    `Count` ~ `InterestVar`,
    FUN=sum
  ) %>% mutate(
    `Share` = percent(`Count` / totalStudents, accuracy=0.1)
  ) %>% select(
    -(`Count`)
  ) %>% pivot_wider(
    names_from=`InterestVar`, 
    values_from=`Share`
  )
  
  print(as.data.frame(tempDF))
  
  rm(tempDF, totalStudents)
  
}

#### End #### 

#### Write function to display distribution as percentiles ####

showPercentiles <- function(variableName, removeZeros, USD){
  
  tempDF <- studentDF %>% select(
    all_of(variableName)
  )
  names(tempDF)[1] <- "InterestVar"
  
  if(removeZeros){
    tempDF <- tempDF %>% filter(
      `InterestVar` > 0
    )
  }
  
  if(USD){
    print(as.data.frame(dollar(quantile(tempDF$`InterestVar`, probs = seq(.1, .9, by = .1)), accuracy=1)))
  }else{
    print(as.data.frame(quantile(tempDF$`InterestVar`, probs = seq(.1, .9, by = .1))))
  }
  
  rm(tempDF)
  
}

#### End #### 

# #### Run function to display distributions #### 
# 
# showDistribution("Control")
# showDistribution("Region NPSAS")
# showDistribution("Race NPSAS")
# showDistribution("Carnegie NPSAS")
# showDistribution("Enrollment intensity NPSAS")
# showDistribution("Gender")
# showDistribution("Zero-EFC")
# showPercentiles("EFC", removeZeros=TRUE, USD=TRUE)
# showDistribution("Tuition jurisdiction")
# showPercentiles("Tuition and fees paid", removeZeros=TRUE, USD=TRUE)
# showPercentiles("Age", removeZeros=FALSE, USD=FALSE)
# showDistribution("Citizenship")
# showDistribution("Veteran status")
# showDistribution("Dependency status")
# showDistribution("Applied for federal aid")
# showPercentiles("Non-tuition expense budget", removeZeros=FALSE, USD=TRUE)
# showDistribution("Receives federal grants")
# showDistribution("Receives VA/DOD grants")
# showDistribution("Receives state grants")
# showDistribution("Receives institutional grants")
# showDistribution("Receives private grants")
# showDistribution("Receives federal loans")
# showDistribution("Receives parent loans")
# showPercentiles("Federal grant amount", removeZeros=TRUE, USD=TRUE)
# showPercentiles("VA/DOD grant amount", removeZeros=TRUE, USD=TRUE)
# showPercentiles("State grant amount", removeZeros=TRUE, USD=TRUE)
# showPercentiles("Institutional grant amount", removeZeros=TRUE, USD=TRUE)
# showPercentiles("Private grant amount", removeZeros=TRUE, USD=TRUE)
# showPercentiles("Federal loan amount", removeZeros=TRUE, USD=TRUE)
# showPercentiles("Parent loan amount", removeZeros=TRUE, USD=TRUE)
# showDistribution("Parent status")
# showDistribution("Parental education attainment")
# showDistribution("High school GPA")
# showPercentiles("Total cost", removeZeros=FALSE, USD=TRUE)
# showDistribution("Receives any grants")
# showPercentiles("Total grants", removeZeros=TRUE, USD=TRUE)
# showPercentiles("Net price", removeZeros=FALSE, USD=TRUE)
# showDistribution("Receives any loans")
# showPercentiles("Total loans", removeZeros=TRUE, USD=TRUE)
# 
# #### End #### 

#### Apply inflation adjustments ####

studentDF <- studentDF %>% mutate(
  
  # Adjust by inflation: January 2020 to January 2023 
  `EFC` = `EFC` * 1.159704, # Does it make sense to do this?
  `Tuition and fees paid` = `Tuition and fees paid` * 1.159704, 
  `Non-tuition expense budget` = `Non-tuition expense budget` * 1.159704, 
  `Federal grant amount` = `Federal grant amount` * 1.159704, # Does it make sense to do this?
  `VA/DOD grant amount` = `VA/DOD grant amount` * 1.159704, 
  `State grant amount` = `State grant amount` * 1.159704, 
  `Institutional grant amount` = `Institutional grant amount` * 1.159704, 
  `Private grant amount` = `Private grant amount` * 1.159704, 
  `Federal loan amount` = `Federal loan amount` * 1.159704, 
  `Parent loan amount` = `Parent loan amount` * 1.159704, 
  `Total grants` = `Total grants` * 1.159704, 
  `Total loans` = `Total loans` * 1.159704, 
  `Total cost` = `Total cost` * 1.159704, 
  `Net price` = `Net price` * 1.159704
  
) 

#### End #### 

#### Group by EFC ####

studentDF <- studentDF %>% mutate(
  `EFC group` = ifelse(
    floor(`EFC`)==0, "$0", ifelse(
      between(floor(`EFC`), 1, 5000), "$1 to $5,000", ifelse(
        between(floor(`EFC`), 5001, 10000), "$5,001 to $10,000", ifelse(
          between(floor(`EFC`), 10001, 20000), "$10,001 to $20,000", "Over $20,000"
        )
      )
    )
  )
)

#### End #### 

################################################
#### Simulation code                        ####
################################################

runSimulation <- function(
  
  #### List all inputs ####  
  
  data1, 
  program.goal,
  elig.carnegie, 
  elig.control, 
  elig.halftime, 
  elig.efc, 
  elig.outofstate, 
  elig.noncitizen, 
  elig.fafsa, 
  elig.gpa
  
  #### End #### 
  
){
  
  # TESTING ONLY
  # data1 <- studentDF
  # program.goal <- "Double current federal grant levels"
  # elig.carnegie <- "Associate's colleges only"
  # elig.control <- "Public only"
  # elig.halftime <- "Full-time only"
  # elig.efc <- "$10,000 or below"
  # elig.outofstate <- "In-state only" 
  # elig.noncitizen <- "U.S. citizens or eligible nonciizens only"
  # elig.fafsa <- "FAFSA completers only"
  # elig.gpa <- "2.5 or above"
  # TESTING ONLY
  
  #### Make a copy of the original data ####
  
  data2 <- data1
  
  #### End #### 
  
  #### Establish eligiblity vectors #### 
  
  # Carnegie Classification
  if(elig.carnegie=="Associate's colleges only"){
    vector.carnegie <- c("Associate's")
  }
  if(elig.carnegie=="Associate's and bachelor's colleges only"){
    vector.carnegie <- c("Associate's", "Baccalaureate")
  }
  if(elig.carnegie=="All institution types"){
    vector.carnegie <- c("Associate's", "Baccalaureate", "Research & Doctoral", "Master's", "Special Focus & other", "Not degree-granting")
  }

  # Control
  if(elig.control=="Public only"){
    vector.control <- c("Public")
  }
  if(elig.control=="Public and nonprofit"){
    vector.control <- c("Public", "Private nonprofit")
  }
  if(elig.control=="All controls"){
    vector.control <- c("Public", "Private nonprofit", "Private for-profit")
  }
  
  # Enrollment intensity
  if(elig.halftime=="Full-time only"){
    vector.intensity <- c("Full-time")
  }else{
    vector.intensity <- c("Full-time", "Part-time")
  }
  
  # EFC 
  if(elig.efc=="$0 only"){
    vector.efc <- c("$0")
  }
  if(elig.efc=="$5,000 or below"){
    vector.efc <- c("$0", "$1 to $5,000")
  }
  if(elig.efc=="$10,000 or below"){
    vector.efc <- c("$0", "$1 to $5,000", "$5,001 to $10,000")
  }
  if(elig.efc=="$20,000 or below"){
    vector.efc <- c("$0", "$1 to $5,000", "$5,001 to $10,000", "$10,001 to $20,000")
  }
  if(elig.efc=="All EFC groups"){
    vector.efc <- c("$0", "$1 to $5,000", "$5,001 to $10,000", "$10,001 to $20,000", "Over $20,000")
  }
  
  # In-state status 
  if(elig.outofstate=="In-state only"){
    vector.oos <- c("In-state tuition", "No differential tuition charged") # Double check this
  }else{
    vector.oos <- c("In-state tuition", "No differential tuition charged", "Out-of-state tuition")
  }
  
  # Citizenship
  if(elig.noncitizen=="U.S. citizens or eligible nonciizens only"){
    vector.cit <- c("Citizen or eligible non-citizen")
  }else{
    vector.cit <- c("Citizen or eligible non-citizen", "Non-citizen")
  }
  
  # FAFSA
  if(elig.fafsa=="FAFSA completers only"){
    vector.fafsa <- c("Yes")
  }else{
    vector.fafsa <- c("Yes", "No")
  }

  # GPA 
  if(elig.gpa=="3.0 or above"){
    vector.gpa <- c("Above 3.5", "Between 3.0 and 3.5")
  }
  if(elig.gpa=="2.5 or above"){
    vector.gpa <- c("Above 3.5", "Between 3.0 and 3.5", "Between 2.5 and 3.0")
  }
  if(elig.gpa=="All GPA groups"){
    vector.gpa <- c("Above 3.5", "Between 3.0 and 3.5", "Between 2.5 and 3.0", "Below 2.5")
  }
  
  #### End #### 
  
  #### Classify by eligibility ####
  
  data2 <- data2 %>% mutate(
    `Eligible for program` = ifelse(
      (`Carnegie NPSAS` %in% vector.carnegie) & (`Control` %in% vector.control) & (`Enrollment intensity` %in% vector.intensity) & (`EFC group` %in% vector.efc) & (`Tuition jurisdiction` %in% vector.oos) & (`Citizenship` %in% vector.cit) & (`Applied for federal aid` %in% vector.fafsa) & (`High school GPA` %in% vector.gpa), 
      "Eligible", 
      "Not eligible"
    )
  )
  
  #### End #### 
  
  #### Policy goal 1: Tuition set at $0 #### 
  
  if(program.goal=="Eliminate tuition"){
    
    data2 <- data2 %>% mutate(
      `Program amount` = pmax(0, `Tuition and fees paid`)
    )
    
  }
  
  #### End #### 
  
  #### Policy goal 2: Net tuition after all grants $0 #### 
  
  if(program.goal=="Cover remaining tuition after all other grants"){
    
    data2 <- data2 %>% mutate(
      `Program amount` = pmax(0, `Tuition and fees paid` - `Total grants`)
    )
    
  }
  
  #### End #### 
  
  #### Policy goal 3: Double current federal grant levels #### 
  
  if(program.goal=="Double current federal grant levels"){
    
    data2 <- data2 %>% mutate(
      `Program amount` = `Federal grant amount`
    )
    
  }
  
  #### End #### 
  
  #### Recalculate total grants, net price, total loans ####
  
  data2 <- data2 %>% mutate(
    
    # Program amount set to 0 if not eligible for program 
    `Program amount` = ifelse(
      `Eligible for program`=="Eligible", `Program amount`, 0
    )
    
  ) %>% mutate(
    
    # Increase total grants 
    `PP Total grants` = `Total grants` + `Program amount`
    
  ) %>% mutate(
    
    # Recalculate net price
    `PP Net price` = pmax(0, `Total cost` - `PP Total grants`)
    
  ) %>% mutate(
    
    # Reduce loans by the program amount 
    `PP Total loans` = pmax(0, `Total loans` - `Program amount`)
  )
  
  #### End #### 
  
  #### Table 1a: Total recipients, total cost ####
  
  table1 <- aggregate(
    data=data2, 
    cbind(`Count`, `Program amount`) ~ `Eligible for program`, 
    FUN=sum
  ) 
  
  if(("Eligible" %in% table1$`Eligible for program`)==FALSE){
    table1 <- table1 %>% add_row(
      `Eligible for program` = "Eligible", 
      `Count` = 0, 
      `Program amount` = 0
    )
  }
  if(("Not eligible" %in% table1$`Eligible for program`)==FALSE){
    table1 <- table1 %>% add_row(
      `Eligible for program` = "Not eligible", 
      `Count` = 0, 
      `Program amount` = 0
    )
  }
  
  table1 <- table1 %>% arrange(
    `Eligible for program`
  )
  
  table1.recipients <- table1$`Count`[1]
  table1.nonrecipients <- table1$`Count`[2]
  table1.totalamount <- table1$`Program amount`[1]
  
  rm(table1)
  
  #### End #### 
  
  #### Table 2: Net price deciles ####
  
  deciles.netprice <- quantile(data2$`Net price`, probs = seq(.1, .9, by = .1))
  deciles.pp.netprice <- quantile(data2$`PP Net price`, probs = seq(.1, .9, by = .1))
  
  table2.netprice.10 <- deciles.netprice[1]
  table2.netprice.20 <- deciles.netprice[2]
  table2.netprice.30 <- deciles.netprice[3]
  table2.netprice.40 <- deciles.netprice[4]
  table2.netprice.50 <- deciles.netprice[5]
  table2.netprice.60 <- deciles.netprice[6]
  table2.netprice.70 <- deciles.netprice[7]
  table2.netprice.80 <- deciles.netprice[8]
  table2.netprice.90 <- deciles.netprice[9]
  
  table2.pp.netprice.10 <- deciles.pp.netprice[1]
  table2.pp.netprice.20 <- deciles.pp.netprice[2]
  table2.pp.netprice.30 <- deciles.pp.netprice[3]
  table2.pp.netprice.40 <- deciles.pp.netprice[4]
  table2.pp.netprice.50 <- deciles.pp.netprice[5]
  table2.pp.netprice.60 <- deciles.pp.netprice[6]
  table2.pp.netprice.70 <- deciles.pp.netprice[7]
  table2.pp.netprice.80 <- deciles.pp.netprice[8]
  table2.pp.netprice.90 <- deciles.pp.netprice[9]
  
  rm(deciles.netprice, deciles.pp.netprice)
  
  #### End #### 
  
  #### Table 2: Total loans deciles ####
  
  deciles.totalloans <- quantile(data2$`Total loans`, probs = seq(.1, .9, by = .1))
  deciles.pp.totalloans <- quantile(data2$`PP Total loans`, probs = seq(.1, .9, by = .1))
  
  table2.totalloans.10 <- deciles.totalloans[1]
  table2.totalloans.20 <- deciles.totalloans[2]
  table2.totalloans.30 <- deciles.totalloans[3]
  table2.totalloans.40 <- deciles.totalloans[4]
  table2.totalloans.50 <- deciles.totalloans[5]
  table2.totalloans.60 <- deciles.totalloans[6]
  table2.totalloans.70 <- deciles.totalloans[7]
  table2.totalloans.80 <- deciles.totalloans[8]
  table2.totalloans.90 <- deciles.totalloans[9]
  
  table2.pp.totalloans.10 <- deciles.pp.totalloans[1]
  table2.pp.totalloans.20 <- deciles.pp.totalloans[2]
  table2.pp.totalloans.30 <- deciles.pp.totalloans[3]
  table2.pp.totalloans.40 <- deciles.pp.totalloans[4]
  table2.pp.totalloans.50 <- deciles.pp.totalloans[5]
  table2.pp.totalloans.60 <- deciles.pp.totalloans[6]
  table2.pp.totalloans.70 <- deciles.pp.totalloans[7]
  table2.pp.totalloans.80 <- deciles.pp.totalloans[8]
  table2.pp.totalloans.90 <- deciles.pp.totalloans[9]
  
  rm(deciles.totalloans, deciles.pp.totalloans)
  
  #### End #### 
  
  #### Table 3a: Beneficiaries by race ####
  
  table3a <- aggregate(
    data=data2, 
    `Count` ~ `Eligible for program` + `Race`, 
    FUN=sum
  ) %>% pivot_wider(
    id_cols=c(`Eligible for program`), 
    names_from=`Race`, 
    values_from=`Count`
  ) %>% mutate(
    `American Indian or Alaska Native` = ifelse(is.na(`American Indian or Alaska Native`), 0, `American Indian or Alaska Native`),
    `Asian` = ifelse(is.na(`Asian`), 0, `Asian`),
    `Black or African American` = ifelse(is.na(`Black or African American`), 0, `Black or African American`),
    `Hispanic or Latino` = ifelse(is.na(`Hispanic or Latino`), 0, `Hispanic or Latino`),
    `More than one race` = ifelse(is.na(`More than one race`), 0, `More than one race`),
    `Native Hawaiian/other Pacific Islander` = ifelse(is.na(`Native Hawaiian/other Pacific Islander`), 0, `Native Hawaiian/other Pacific Islander`),
    `Race/ethnicity unknown` = ifelse(is.na(`Race/ethnicity unknown`), 0, `Race/ethnicity unknown`),
    `U.S. Nonresident` = ifelse(is.na(`U.S. Nonresident`), 0, `U.S. Nonresident`),
    `White` = ifelse(is.na(`White`), 0, `White`)
  ) 
  
  if(("Eligible" %in% table3a$`Eligible for program`)==FALSE){
    table3a <- table3a %>% add_row(
      `Eligible for program` = "Eligible", 
      `Count` = 0
    )
  }
  if(("Not eligible" %in% table3a$`Eligible for program`)==FALSE){
    table3a <- table3a %>% add_row(
      `Eligible for program` = "Not eligible", 
      `Count` = 0
    )
  }
  
  table3a <- table3a %>% arrange(
    `Eligible for program`
  )
  
  table3.aian.eligible <- table3a$`American Indian or Alaska Native`[1]
  table3.asia.eligible <- table3a$`Asian`[1]
  table3.bkaa.eligible <- table3a$`Black or African American`[1]
  table3.hisp.eligible <- table3a$`Hispanic or Latino`[1]
  table3.2mor.eligible <- table3a$`More than one race`[1]
  table3.nhpi.eligible <- table3a$`Native Hawaiian/other Pacific Islander`[1]
  table3.unkn.eligible <- table3a$`Race/ethnicity unknown`[1]
  table3.nonr.eligible <- table3a$`U.S. Nonresident`[1]
  table3.whit.eligible <- table3a$`White`[1]
  
  table3.aian.ineligible <- table3a$`American Indian or Alaska Native`[2]
  table3.asia.ineligible <- table3a$`Asian`[2]
  table3.bkaa.ineligible <- table3a$`Black or African American`[2]
  table3.hisp.ineligible <- table3a$`Hispanic or Latino`[2]
  table3.2mor.ineligible <- table3a$`More than one race`[2]
  table3.nhpi.ineligible <- table3a$`Native Hawaiian/other Pacific Islander`[2]
  table3.unkn.ineligible <- table3a$`Race/ethnicity unknown`[2]
  table3.nonr.ineligible <- table3a$`U.S. Nonresident`[2]
  table3.whit.ineligible <- table3a$`White`[2]
  
  rm(table3a)
  
  #### End #### 
  
  #### Table 3b: Beneficiaries by gender ####
  
  table3b <- aggregate(
    data=data2, 
    `Count` ~ `Eligible for program` + `Gender`, 
    FUN=sum
  ) %>% pivot_wider(
    id_cols=c(`Eligible for program`), 
    names_from=`Gender`, 
    values_from=`Count`
  ) %>% mutate(
    `Male` = ifelse(is.na(`Male`), 0, `Male`),
    `Female` = ifelse(is.na(`Female`), 0, `Female`)
  ) 
  
  if(("Eligible" %in% table3b$`Eligible for program`)==FALSE){
    table3b <- table3b %>% add_row(
      `Eligible for program` = "Eligible", 
      `Count` = 0
    )
  }
  if(("Not eligible" %in% table3b$`Eligible for program`)==FALSE){
    table3b <- table3b %>% add_row(
      `Eligible for program` = "Not eligible", 
      `Count` = 0
    )
  }
  
  table3b <- table3b %>% arrange(
    `Eligible for program`
  )
  
  table3.male.eligible <- table3b$`Male`[1]
  table3.female.eligible <- table3b$`Female`[1]
  
  table3.male.ineligible <- table3b$`Male`[2]
  table3.female.ineligible <- table3b$`Female`[2]
  
  rm(table3b)
  
  #### End #### 
  
  #### Table 3c: Beneficiaries by first-gen status ####
  
  table3c <- aggregate(
    data=data2, 
    `Count` ~ `Eligible for program` + `Parental education attainment`, 
    FUN=sum
  ) %>% pivot_wider(
    id_cols=c(`Eligible for program`), 
    names_from=`Parental education attainment`, 
    values_from=`Count`
  ) %>% mutate(
    `Parents do not have a college degree` = ifelse(is.na(`Parents do not have a college degree`), 0, `Parents do not have a college degree`),
    `Parents have a college degree` = ifelse(is.na(`Parents have a college degree`), 0, `Parents have a college degree`)
  ) 
  
  if(("Eligible" %in% table3c$`Eligible for program`)==FALSE){
    table3c <- table3c %>% add_row(
      `Eligible for program` = "Eligible", 
      `Count` = 0
    )
  }
  if(("Not eligible" %in% table3c$`Eligible for program`)==FALSE){
    table3c <- table3c %>% add_row(
      `Eligible for program` = "Not eligible", 
      `Count` = 0
    )
  }
  
  table3c <- table3c %>% arrange(
    `Eligible for program`
  )
  
  table3.firstgen.eligible <- table3c$`Parents do not have a college degree`[1]
  table3.notfirstgen.eligible <- table3c$`Parents have a college degree`[1]
  
  table3.firstgen.ineligible <- table3c$`Parents do not have a college degree`[2]
  table3.notfirstgen.ineligible <- table3c$`Parents have a college degree`[2]
  
  rm(table3c)
  
  #### End #### 
  
  #### Table 3d: Beneficiaries by dependency status ####
  
  table3d <- aggregate(
    data=data2, 
    `Count` ~ `Eligible for program` + `Dependency status`, 
    FUN=sum
  ) %>% pivot_wider(
    id_cols=c(`Eligible for program`), 
    names_from=`Dependency status`, 
    values_from=`Count`
  ) %>% mutate(
    `Dependent` = ifelse(is.na(`Dependent`), 0, `Dependent`),
    `Independent` = ifelse(is.na(`Independent`), 0, `Independent`)
  ) 
  
  if(("Eligible" %in% table3d$`Eligible for program`)==FALSE){
    table3d <- table3d %>% add_row(
      `Eligible for program` = "Eligible", 
      `Count` = 0
    )
  }
  if(("Not eligible" %in% table3d$`Eligible for program`)==FALSE){
    table3d <- table3d %>% add_row(
      `Eligible for program` = "Not eligible", 
      `Count` = 0
    )
  }
  
  table3d <- table3d %>% arrange(
    `Eligible for program`
  )
  
  table3.dependent.eligible <- table3d$`Dependent`[1]
  table3.independent.eligible <- table3d$`Independent`[1]
  
  table3.dependent.ineligible <- table3d$`Dependent`[2]
  table3.independent.ineligible <- table3d$`Independent`[2]
  
  rm(table3d)
  
  #### End #### 
  
  #### Table 3e: Beneficiaries by zero-EFC status ####
  
  table3e <- aggregate(
    data=data2, 
    `Count` ~ `Eligible for program` + `Zero EFC status`, 
    FUN=sum
  ) %>% pivot_wider(
    id_cols=c(`Eligible for program`), 
    names_from=`Zero EFC status`, 
    values_from=`Count`
  ) %>% mutate(
    `Zero EFC` = ifelse(is.na(`Zero EFC`), 0, `Zero EFC`),
    `Nonzero EFC` = ifelse(is.na(`Nonzero EFC`), 0, `Nonzero EFC`)
  )
  
  if(("Eligible" %in% table3e$`Eligible for program`)==FALSE){
    table3e <- table3e %>% add_row(
      `Eligible for program` = "Eligible", 
      `Count` = 0
    )
  }
  if(("Not eligible" %in% table3e$`Eligible for program`)==FALSE){
    table3e <- table3e %>% add_row(
      `Eligible for program` = "Not eligible", 
      `Count` = 0
    )
  }
  
  table3e <- table3e %>% arrange(
    `Eligible for program`
  )
  
  table3.zeroEFC.eligible <- table3e$`Zero EFC`[1]
  table3.nonzeroEFC.eligible <- table3e$`Nonzero EFC`[1]
  
  table3.zeroEFC.ineligible <- table3e$`Zero EFC`[2]
  table3.nonzeroEFC.ineligible <- table3e$`Nonzero EFC`[2]
  
  rm(table3e)
  
  #### End #### 
  
  #### Table 3f: Beneficiaries by parent status ####
  
  table3f <- aggregate(
    data=data2, 
    `Count` ~ `Eligible for program` + `Parent status`, 
    FUN=sum
  ) %>% pivot_wider(
    id_cols=c(`Eligible for program`), 
    names_from=`Parent status`, 
    values_from=`Count`
  ) %>% mutate(
    `Has dependents` = ifelse(is.na(`Has dependents`), 0, `Has dependents`),
    `Does not have dependents` = ifelse(is.na(`Does not have dependents`), 0, `Does not have dependents`)
  )
  
  if(("Eligible" %in% table3f$`Eligible for program`)==FALSE){
    table3f <- table3f %>% add_row(
      `Eligible for program` = "Eligible", 
      `Count` = 0
    )
  }
  if(("Not eligible" %in% table3f$`Eligible for program`)==FALSE){
    table3f <- table3f %>% add_row(
      `Eligible for program` = "Not eligible", 
      `Count` = 0
    )
  }
  
  table3f <- table3f %>% arrange(
    `Eligible for program`
  )
  
  table3.parent.eligible <- table3f$`Has dependents`[1]
  table3.nonparent.eligible <- table3f$`Does not have dependents`[1]
  
  table3.parent.ineligible <- table3f$`Has dependents`[2]
  table3.nonparent.ineligible <- table3f$`Does not have dependents`[2]
  
  rm(table3f)
  
  #### End #### 
  
  #### Table 3g: Beneficiaries by Veteran status ####
  
  table3g <- aggregate(
    data=data2, 
    `Count` ~ `Eligible for program` + `Veteran status`, 
    FUN=sum
  ) %>% pivot_wider(
    id_cols=c(`Eligible for program`), 
    names_from=`Veteran status`, 
    values_from=`Count`
  ) %>% mutate(
    `Veteran` = ifelse(is.na(`Veteran`), 0, `Veteran`),
    `Not a veteran` = ifelse(is.na(`Not a veteran`), 0, `Not a veteran`)
  ) 
  
  if(("Eligible" %in% table3g$`Eligible for program`)==FALSE){
    table3g <- table3g %>% add_row(
      `Eligible for program` = "Eligible", 
      `Count` = 0
    )
  }
  if(("Not eligible" %in% table3g$`Eligible for program`)==FALSE){
    table3g <- table3g %>% add_row(
      `Eligible for program` = "Not eligible", 
      `Count` = 0
    )
  }
  
  table3g <- table3g %>% arrange(
    `Eligible for program`
  )
  
  table3.veteran.eligible <- table3g$`Veteran`[1]
  table3.nonveteran.eligible <- table3g$`Not a veteran`[1]
  
  table3.veteran.ineligible <- table3g$`Veteran`[2]
  table3.nonveteran.ineligible <- table3g$`Not a veteran`[2]
  
  rm(table3g)
  
  #### End #### 
  
  #### Table 4: Dollars by state ####
  
  table4 <- aggregate(
    data=data2, 
    `Program amount` ~ `STABBR`, 
    FUN=sum
  ) %>% mutate(
    `Program amount` = ifelse(is.na(`Program amount`), 0, `Program amount`)
  ) %>% pivot_wider(
    names_from=`STABBR`, 
    values_from=`Program amount`
  ) 
  
  table4.AK <- table4$`AK`[1]
  table4.AL <- table4$`AL`[1]
  table4.AR <- table4$`AR`[1]
  table4.AS <- table4$`AS`[1]
  table4.AZ <- table4$`AZ`[1]
  table4.CA <- table4$`CA`[1]
  table4.CO <- table4$`CO`[1]
  table4.CT <- table4$`CT`[1]
  table4.DC <- table4$`DC`[1]
  table4.DE <- table4$`DE`[1]
  table4.FL <- table4$`FL`[1]
  table4.FM <- table4$`FM`[1]
  table4.GA <- table4$`GA`[1]
  table4.GU <- table4$`GU`[1]
  table4.HI <- table4$`HI`[1]
  table4.IA <- table4$`IA`[1]
  table4.ID <- table4$`ID`[1]
  table4.IL <- table4$`IL`[1]
  table4.IN <- table4$`IN`[1]
  table4.KS <- table4$`KS`[1]
  table4.KY <- table4$`KY`[1]
  table4.LA <- table4$`LA`[1]
  table4.MA <- table4$`MA`[1]
  table4.MD <- table4$`MD`[1]
  table4.ME <- table4$`ME`[1]
  table4.MH <- table4$`MH`[1]
  table4.MI <- table4$`MI`[1]
  table4.MN <- table4$`MN`[1]
  table4.MO <- table4$`MO`[1]
  table4.MP <- table4$`MP`[1]
  table4.MS <- table4$`MS`[1]
  table4.MT <- table4$`MT`[1]
  table4.NC <- table4$`NC`[1]
  table4.ND <- table4$`ND`[1]
  table4.NE <- table4$`NE`[1]
  table4.NH <- table4$`NH`[1]
  table4.NJ <- table4$`NJ`[1]
  table4.NM <- table4$`NM`[1]
  table4.NV <- table4$`NV`[1]
  table4.NY <- table4$`NY`[1]
  table4.OH <- table4$`OH`[1]
  table4.OK <- table4$`OK`[1]
  table4.OR <- table4$`OR`[1]
  table4.PA <- table4$`PA`[1]
  table4.PR <- table4$`PR`[1]
  table4.PW <- table4$`PW`[1]
  table4.RI <- table4$`RI`[1]
  table4.SC <- table4$`SC`[1]
  table4.SD <- table4$`SD`[1]
  table4.TN <- table4$`TN`[1]
  table4.TX <- table4$`TX`[1]
  table4.UT <- table4$`UT`[1]
  table4.VA <- table4$`VA`[1]
  table4.VI <- table4$`VI`[1]
  table4.VT <- table4$`VT`[1]
  table4.WA <- table4$`WA`[1]
  table4.WI <- table4$`WI`[1]
  table4.WV <- table4$`WV`[1]
  table4.WY <- table4$`WY`[1]
  
  rm(table4)
  
  #### End #### 
  
  #### Return simulation results #### 
  
  simuResults <- data.frame(
    
    # Inputs 
    `elig.carnegie` = c(elig.carnegie),
    `elig.control` = c(elig.control),
    `elig.halftime` = c(elig.halftime),
    `elig.efc` = c(elig.efc),
    `elig.outofstate` = c(elig.outofstate),
    `elig.noncitizen` = c(elig.noncitizen),
    `elig.fafsa` = c(elig.fafsa),
    `elig.gpa` = c(elig.gpa),
    `program.goal` = c(program.goal),
    
    # Outputs: Table 1
    `table1.recipients` = c(table1.recipients),
    `table1.nonrecipients` = c(table1.nonrecipients),
    `table1.totalamount` = c(table1.totalamount),
    
    # Outputs: Table 2
    `table2.netprice.10` = c(`table2.netprice.10`),
    `table2.netprice.20` = c(`table2.netprice.20`),
    `table2.netprice.30` = c(`table2.netprice.30`),
    `table2.netprice.40` = c(`table2.netprice.40`),
    `table2.netprice.50` = c(`table2.netprice.50`),
    `table2.netprice.60` = c(`table2.netprice.60`),
    `table2.netprice.70` = c(`table2.netprice.70`),
    `table2.netprice.80` = c(`table2.netprice.80`),
    `table2.netprice.90` = c(`table2.netprice.90`),
    `table2.pp.netprice.10` = c(`table2.pp.netprice.10`),
    `table2.pp.netprice.20` = c(`table2.pp.netprice.20`),
    `table2.pp.netprice.30` = c(`table2.pp.netprice.30`),
    `table2.pp.netprice.40` = c(`table2.pp.netprice.40`),
    `table2.pp.netprice.50` = c(`table2.pp.netprice.50`),
    `table2.pp.netprice.60` = c(`table2.pp.netprice.60`),
    `table2.pp.netprice.70` = c(`table2.pp.netprice.70`),
    `table2.pp.netprice.80` = c(`table2.pp.netprice.80`),
    `table2.pp.netprice.90` = c(`table2.pp.netprice.90`),
    `table2.totalloans.10` = c(`table2.totalloans.10`),
    `table2.totalloans.20` = c(`table2.totalloans.20`),
    `table2.totalloans.30` = c(`table2.totalloans.30`),
    `table2.totalloans.40` = c(`table2.totalloans.40`),
    `table2.totalloans.50` = c(`table2.totalloans.50`),
    `table2.totalloans.60` = c(`table2.totalloans.60`),
    `table2.totalloans.70` = c(`table2.totalloans.70`),
    `table2.totalloans.80` = c(`table2.totalloans.80`),
    `table2.totalloans.90` = c(`table2.totalloans.90`),
    `table2.pp.totalloans.10` = c(`table2.pp.totalloans.10`),
    `table2.pp.totalloans.20` = c(`table2.pp.totalloans.20`),
    `table2.pp.totalloans.30` = c(`table2.pp.totalloans.30`),
    `table2.pp.totalloans.40` = c(`table2.pp.totalloans.40`),
    `table2.pp.totalloans.50` = c(`table2.pp.totalloans.50`),
    `table2.pp.totalloans.60` = c(`table2.pp.totalloans.60`),
    `table2.pp.totalloans.70` = c(`table2.pp.totalloans.70`),
    `table2.pp.totalloans.80` = c(`table2.pp.totalloans.80`),
    `table2.pp.totalloans.90` = c(`table2.pp.totalloans.90`),
    
    # Outputs: Table 3 (eligible)
    `table3.aian.eligible` = c(table3.aian.eligible),
    `table3.asia.eligible` = c(table3.asia.eligible),
    `table3.bkaa.eligible` = c(table3.bkaa.eligible),
    `table3.hisp.eligible` = c(table3.hisp.eligible),
    `table3.2mor.eligible` = c(table3.2mor.eligible),
    `table3.nhpi.eligible` = c(table3.nhpi.eligible),
    `table3.unkn.eligible` = c(table3.unkn.eligible),
    `table3.nonr.eligible` = c(table3.nonr.eligible),
    `table3.whit.eligible` = c(table3.whit.eligible),
    `table3.male.eligible` = c(table3.male.eligible),
    `table3.female.eligible` = c(table3.female.eligible),
    `table3.firstgen.eligible` = c(table3.firstgen.eligible),
    `table3.notfirstgen.eligible` = c(table3.notfirstgen.eligible),
    `table3.dependent.eligible` = c(table3.dependent.eligible),
    `table3.independent.eligible` = c(table3.independent.eligible),
    `table3.zeroEFC.eligible` = c(table3.zeroEFC.eligible),
    `table3.nonzeroEFC.eligible` = c(table3.nonzeroEFC.eligible),
    `table3.parent.eligible` = c(table3.parent.eligible),
    `table3.nonparent.eligible` = c(table3.nonparent.eligible),
    `table3.veteran.eligible` = c(table3.veteran.eligible),
    `table3.nonveteran.eligible` = c(table3.nonveteran.eligible),
    
    # Outputs: Table 3 (ineligible)
    `table3.aian.ineligible` = c(table3.aian.ineligible),
    `table3.asia.ineligible` = c(table3.asia.ineligible),
    `table3.bkaa.ineligible` = c(table3.bkaa.ineligible),
    `table3.hisp.ineligible` = c(table3.hisp.ineligible),
    `table3.2mor.ineligible` = c(table3.2mor.ineligible),
    `table3.nhpi.ineligible` = c(table3.nhpi.ineligible),
    `table3.unkn.ineligible` = c(table3.unkn.ineligible),
    `table3.nonr.ineligible` = c(table3.nonr.ineligible),
    `table3.whit.ineligible` = c(table3.whit.ineligible),
    `table3.male.ineligible` = c(table3.male.ineligible),
    `table3.female.ineligible` = c(table3.female.ineligible),
    `table3.firstgen.ineligible` = c(table3.firstgen.ineligible),
    `table3.notfirstgen.ineligible` = c(table3.notfirstgen.ineligible),
    `table3.dependent.ineligible` = c(table3.dependent.ineligible),
    `table3.independent.ineligible` = c(table3.independent.ineligible),
    `table3.zeroEFC.ineligible` = c(table3.zeroEFC.ineligible),
    `table3.nonzeroEFC.ineligible` = c(table3.nonzeroEFC.ineligible),
    `table3.parent.ineligible` = c(table3.parent.ineligible),
    `table3.nonparent.ineligible` = c(table3.nonparent.ineligible),
    `table3.veteran.ineligible` = c(table3.veteran.ineligible),
    `table3.nonveteran.ineligible` = c(table3.nonveteran.ineligible),
    
    # Table 4: Total dollars by state 
    `table4.AK` = c(table4.AK),
    `table4.AL` = c(table4.AL),
    `table4.AR` = c(table4.AR),
    `table4.AS` = c(table4.AS),
    `table4.AZ` = c(table4.AZ),
    `table4.CA` = c(table4.CA),
    `table4.CO` = c(table4.CO),
    `table4.CT` = c(table4.CT),
    `table4.DC` = c(table4.DC),
    `table4.DE` = c(table4.DE),
    `table4.FL` = c(table4.FL),
    `table4.FM` = c(table4.FM),
    `table4.GA` = c(table4.GA),
    `table4.GU` = c(table4.GU),
    `table4.HI` = c(table4.HI),
    `table4.IA` = c(table4.IA),
    `table4.ID` = c(table4.ID),
    `table4.IL` = c(table4.IL),
    `table4.IN` = c(table4.IN),
    `table4.KS` = c(table4.KS),
    `table4.KY` = c(table4.KY),
    `table4.LA` = c(table4.LA),
    `table4.MA` = c(table4.MA),
    `table4.MD` = c(table4.MD),
    `table4.ME` = c(table4.ME),
    `table4.MH` = c(table4.MH),
    `table4.MI` = c(table4.MI),
    `table4.MN` = c(table4.MN),
    `table4.MO` = c(table4.MO),
    `table4.MP` = c(table4.MP),
    `table4.MS` = c(table4.MS),
    `table4.MT` = c(table4.MT),
    `table4.NC` = c(table4.NC),
    `table4.ND` = c(table4.ND),
    `table4.NE` = c(table4.NE),
    `table4.NH` = c(table4.NH),
    `table4.NJ` = c(table4.NJ),
    `table4.NM` = c(table4.NM),
    `table4.NV` = c(table4.NV),
    `table4.NY` = c(table4.NY),
    `table4.OH` = c(table4.OH),
    `table4.OK` = c(table4.OK),
    `table4.OR` = c(table4.OR),
    `table4.PA` = c(table4.PA),
    `table4.PR` = c(table4.PR),
    `table4.PW` = c(table4.PW),
    `table4.RI` = c(table4.RI),
    `table4.SC` = c(table4.SC),
    `table4.SD` = c(table4.SD),
    `table4.TN` = c(table4.TN),
    `table4.TX` = c(table4.TX),
    `table4.UT` = c(table4.UT),
    `table4.VA` = c(table4.VA),
    `table4.VI` = c(table4.VI),
    `table4.VT` = c(table4.VT),
    `table4.WA` = c(table4.WA),
    `table4.WI` = c(table4.WI),
    `table4.WV` = c(table4.WV),
    `table4.WY` = c(table4.WY)
  )
  
  return(simuResults)
  rm(simuResults)
  
  #### End #### 
  
  #### Delete objects #### 
  
  rm(
    
    # Temporary dataframe 
    data2, 
    
    # Eligibility vectors 
    `vector.carnegie`, 
    `vector.cit`,
    `vector.control`, 
    `vector.efc`, 
    `vector.fafsa`, 
    `vector.gpa`, 
    `vector.intensity`, 
    `vector.oos`,
    
    # Outputs: Table 1
    `table1.recipients`,
    `table1.nonrecipients`,
    `table1.totalamount`,
    
    # Outputs: Table 2
    `table2.netprice.10`,
    `table2.netprice.20`,
    `table2.netprice.30`,
    `table2.netprice.40`,
    `table2.netprice.50`,
    `table2.netprice.60`,
    `table2.netprice.70`,
    `table2.netprice.80`,
    `table2.netprice.90`,
    `table2.pp.netprice.10`,
    `table2.pp.netprice.20`,
    `table2.pp.netprice.30`,
    `table2.pp.netprice.40`,
    `table2.pp.netprice.50`,
    `table2.pp.netprice.60`,
    `table2.pp.netprice.70`,
    `table2.pp.netprice.80`,
    `table2.pp.netprice.90`,
    `table2.totalloans.10`,
    `table2.totalloans.20`,
    `table2.totalloans.30`,
    `table2.totalloans.40`,
    `table2.totalloans.50`,
    `table2.totalloans.60`,
    `table2.totalloans.70`,
    `table2.totalloans.80`,
    `table2.totalloans.90`,
    `table2.pp.totalloans.10`,
    `table2.pp.totalloans.20`,
    `table2.pp.totalloans.30`,
    `table2.pp.totalloans.40`,
    `table2.pp.totalloans.50`,
    `table2.pp.totalloans.60`,
    `table2.pp.totalloans.70`,
    `table2.pp.totalloans.80`,
    `table2.pp.totalloans.90`,
    
    # Outputs: Table 3 (eligible)
    `table3.aian.eligible`,
    `table3.asia.eligible`,
    `table3.bkaa.eligible`,
    `table3.hisp.eligible`,
    `table3.2mor.eligible`,
    `table3.nhpi.eligible`,
    `table3.unkn.eligible`,
    `table3.nonr.eligible`,
    `table3.whit.eligible`,
    `table3.male.eligible`,
    `table3.female.eligible`,
    `table3.firstgen.eligible`,
    `table3.notfirstgen.eligible`,
    `table3.dependent.eligible`,
    `table3.independent.eligible`,
    `table3.zeroEFC.eligible`,
    `table3.nonzeroEFC.eligible`,
    `table3.parent.eligible`,
    `table3.nonparent.eligible`,
    `table3.veteran.eligible`,
    `table3.nonveteran.eligible`,
    
    # Outputs: Table 3 (ineligible)
    `table3.aian.ineligible`,
    `table3.asia.ineligible`,
    `table3.bkaa.ineligible`,
    `table3.hisp.ineligible`,
    `table3.2mor.ineligible`,
    `table3.nhpi.ineligible`,
    `table3.unkn.ineligible`,
    `table3.nonr.ineligible`,
    `table3.whit.ineligible`,
    `table3.male.ineligible`,
    `table3.female.ineligible`,
    `table3.firstgen.ineligible`,
    `table3.notfirstgen.ineligible`,
    `table3.dependent.ineligible`,
    `table3.independent.ineligible`,
    `table3.zeroEFC.ineligible`,
    `table3.nonzeroEFC.ineligible`,
    `table3.parent.ineligible`,
    `table3.nonparent.ineligible`,
    `table3.veteran.ineligible`,
    `table3.nonveteran.ineligible`,
    
    # Table 4: Total dollars by state 
    `table4.AK`,
    `table4.AL`,
    `table4.AR`,
    `table4.AS`,
    `table4.AZ`,
    `table4.CA`,
    `table4.CO`,
    `table4.CT`,
    `table4.DC`,
    `table4.DE`,
    `table4.FL`,
    `table4.FM`,
    `table4.GA`,
    `table4.GU`,
    `table4.HI`,
    `table4.IA`,
    `table4.ID`,
    `table4.IL`,
    `table4.IN`,
    `table4.KS`,
    `table4.KY`,
    `table4.LA`,
    `table4.MA`,
    `table4.MD`,
    `table4.ME`,
    `table4.MH`,
    `table4.MI`,
    `table4.MN`,
    `table4.MO`,
    `table4.MP`,
    `table4.MS`,
    `table4.MT`,
    `table4.NC`,
    `table4.ND`,
    `table4.NE`,
    `table4.NH`,
    `table4.NJ`,
    `table4.NM`,
    `table4.NV`,
    `table4.NY`,
    `table4.OH`,
    `table4.OK`,
    `table4.OR`,
    `table4.PA`,
    `table4.PR`,
    `table4.PW`,
    `table4.RI`,
    `table4.SC`,
    `table4.SD`,
    `table4.TN`,
    `table4.TX`,
    `table4.UT`,
    `table4.VA`,
    `table4.VI`,
    `table4.VT`,
    `table4.WA`,
    `table4.WI`,
    `table4.WV`,
    `table4.WY` 
  )
  
  #### End #### 
  
}

################################################
#### Run results                            ####
################################################

# #### Test simulation #### 
# 
# Sys.time()
# 
# test <- runSimulation(
#   data1=studentDF, 
#   program.goal="Double current federal grant levels",
#   elig.carnegie="Associate's colleges only", 
#   elig.control="Public only", 
#   elig.halftime="Full-time and part-time", 
#   elig.efc="All EFC groups", 
#   elig.outofstate="In-state only", 
#   elig.noncitizen="U.S. citizens or eligible nonciizens only", 
#   elig.fafsa="FAFSA completers only", 
#   elig.gpa="All GPA groups"
# )
# 
# Sys.time()
# 
# #### End #### 

#### Establish possible combinations ####

inputs.program.goal <- c(
  "Eliminate tuition", 
  "Cover remaining tuition after all other grants", 
  "Double current federal grant levels"
)

inputs.elig.carnegie <- c(
  "Associate's colleges only", 
  "Associate's and bachelor's colleges only", 
  "All institution types"
)

inputs.elig.control <- c(
  "Public only", 
  "Public and nonprofit", 
  "All controls"
)

inputs.elig.halftime <- c(
  "Full-time only", 
  "Full-time and part-time"
)

inputs.elig.efc <- c(
  "$0 only", 
  # "$5,000 or below", 
  "$10,000 or below", 
  # "$20,000 or below",
  "All EFC groups"
)

inputs.elig.outofstate <- c(
  "In-state only"
  # , 
  # "In-state and out-of-state"
)

inputs.elig.noncitizen <- c(
  "U.S. citizens or eligible noncitizens only"
  # , 
  # "All statuses"
)

inputs.elig.fafsa <- c(
  "FAFSA completers only"
  # ,
  # "Completers and non-completers"
)

inputs.elig.gpa <- c(
  # "3.0 or above", 
  "2.5 or above"
  # , 
  # "All GPA groups"
)

#### End #### 

#### Count total combinations ####

totalCombos <- length(
  inputs.program.goal
) * length(
  inputs.elig.carnegie
) * length(
  inputs.elig.control
) * length(
  inputs.elig.halftime
) * length(
  inputs.elig.efc
) * length(
  inputs.elig.outofstate
) * length(
  inputs.elig.noncitizen
) * length(
  inputs.elig.fafsa
) * length(
  inputs.elig.gpa
)

#### End #### 

#### Make an empty dataframe ####

simulationResults <- data.frame(
  
  # Inputs 
  `elig.carnegie` = character(), 
  `elig.control` = character(), 
  `elig.halftime` = character(), 
  `elig.efc` = character(), 
  `elig.outofstate` = character(), 
  `elig.noncitizen` = character(), 
  `elig.fafsa` = character(), 
  `elig.gpa` = character(), 
  `program.goal` = character(), 
  
  # Outputs: Table 1
  `table1.recipients` = numeric(), 
  `table1.nonrecipients` = numeric(), 
  `table1.totalamount` = numeric(), 
  
  # Outputs: Table 2
  `table2.netprice.10` = numeric(), 
  `table2.netprice.20` = numeric(), 
  `table2.netprice.30` = numeric(), 
  `table2.netprice.40` = numeric(), 
  `table2.netprice.50` = numeric(), 
  `table2.netprice.60` = numeric(), 
  `table2.netprice.70` = numeric(), 
  `table2.netprice.80` = numeric(), 
  `table2.netprice.90` = numeric(), 
  `table2.pp.netprice.10` = numeric(), 
  `table2.pp.netprice.20` = numeric(), 
  `table2.pp.netprice.30` = numeric(), 
  `table2.pp.netprice.40` = numeric(), 
  `table2.pp.netprice.50` = numeric(), 
  `table2.pp.netprice.60` = numeric(), 
  `table2.pp.netprice.70` = numeric(), 
  `table2.pp.netprice.80` = numeric(), 
  `table2.pp.netprice.90` = numeric(), 
  `table2.totalloans.10` = numeric(), 
  `table2.totalloans.20` = numeric(), 
  `table2.totalloans.30` = numeric(), 
  `table2.totalloans.40` = numeric(), 
  `table2.totalloans.50` = numeric(), 
  `table2.totalloans.60` = numeric(), 
  `table2.totalloans.70` = numeric(), 
  `table2.totalloans.80` = numeric(), 
  `table2.totalloans.90` = numeric(), 
  `table2.pp.totalloans.10` = numeric(), 
  `table2.pp.totalloans.20` = numeric(), 
  `table2.pp.totalloans.30` = numeric(), 
  `table2.pp.totalloans.40` = numeric(), 
  `table2.pp.totalloans.50` = numeric(), 
  `table2.pp.totalloans.60` = numeric(), 
  `table2.pp.totalloans.70` = numeric(), 
  `table2.pp.totalloans.80` = numeric(), 
  `table2.pp.totalloans.90` = numeric(), 
  
  # Outputs: Table 3 (eligible)
  `table3.aian.eligible` = numeric(), 
  `table3.asia.eligible` = numeric(), 
  `table3.bkaa.eligible` = numeric(), 
  `table3.hisp.eligible` = numeric(), 
  `table3.2mor.eligible` = numeric(), 
  `table3.nhpi.eligible` = numeric(), 
  `table3.unkn.eligible` = numeric(), 
  `table3.nonr.eligible` = numeric(), 
  `table3.whit.eligible` = numeric(), 
  `table3.male.eligible` = numeric(), 
  `table3.female.eligible` = numeric(), 
  `table3.firstgen.eligible` = numeric(), 
  `table3.notfirstgen.eligible` = numeric(), 
  `table3.dependent.eligible` = numeric(), 
  `table3.independent.eligible` = numeric(), 
  `table3.zeroEFC.eligible` = numeric(), 
  `table3.nonzeroEFC.eligible` = numeric(), 
  `table3.parent.eligible` = numeric(), 
  `table3.nonparent.eligible` = numeric(), 
  `table3.veteran.eligible` = numeric(), 
  `table3.nonveteran.eligible` = numeric(), 
  
  # Outputs: Table 3 (ineligible)
  `table3.aian.ineligible` = numeric(), 
  `table3.asia.ineligible` = numeric(), 
  `table3.bkaa.ineligible` = numeric(), 
  `table3.hisp.ineligible` = numeric(), 
  `table3.2mor.ineligible` = numeric(), 
  `table3.nhpi.ineligible` = numeric(), 
  `table3.unkn.ineligible` = numeric(), 
  `table3.nonr.ineligible` = numeric(), 
  `table3.whit.ineligible` = numeric(), 
  `table3.male.ineligible` = numeric(), 
  `table3.female.ineligible` = numeric(), 
  `table3.firstgen.ineligible` = numeric(), 
  `table3.notfirstgen.ineligible` = numeric(), 
  `table3.dependent.ineligible` = numeric(), 
  `table3.independent.ineligible` = numeric(), 
  `table3.zeroEFC.ineligible` = numeric(), 
  `table3.nonzeroEFC.ineligible` = numeric(), 
  `table3.parent.ineligible` = numeric(), 
  `table3.nonparent.ineligible` = numeric(), 
  `table3.veteran.ineligible` = numeric(), 
  `table3.nonveteran.ineligible` = numeric(), 
  
  # Table 4: Total dollars by state 
  `table4.AK` = numeric(), 
  `table4.AL` = numeric(), 
  `table4.AR` = numeric(), 
  `table4.AS` = numeric(), 
  `table4.AZ` = numeric(), 
  `table4.CA` = numeric(), 
  `table4.CO` = numeric(), 
  `table4.CT` = numeric(), 
  `table4.DC` = numeric(), 
  `table4.DE` = numeric(), 
  `table4.FL` = numeric(), 
  `table4.FM` = numeric(), 
  `table4.GA` = numeric(), 
  `table4.GU` = numeric(), 
  `table4.HI` = numeric(), 
  `table4.IA` = numeric(), 
  `table4.ID` = numeric(), 
  `table4.IL` = numeric(), 
  `table4.IN` = numeric(), 
  `table4.KS` = numeric(), 
  `table4.KY` = numeric(), 
  `table4.LA` = numeric(), 
  `table4.MA` = numeric(), 
  `table4.MD` = numeric(), 
  `table4.ME` = numeric(), 
  `table4.MH` = numeric(), 
  `table4.MI` = numeric(), 
  `table4.MN` = numeric(), 
  `table4.MO` = numeric(), 
  `table4.MP` = numeric(), 
  `table4.MS` = numeric(), 
  `table4.MT` = numeric(), 
  `table4.NC` = numeric(), 
  `table4.ND` = numeric(), 
  `table4.NE` = numeric(), 
  `table4.NH` = numeric(), 
  `table4.NJ` = numeric(), 
  `table4.NM` = numeric(), 
  `table4.NV` = numeric(), 
  `table4.NY` = numeric(), 
  `table4.OH` = numeric(), 
  `table4.OK` = numeric(), 
  `table4.OR` = numeric(), 
  `table4.PA` = numeric(), 
  `table4.PR` = numeric(), 
  `table4.PW` = numeric(), 
  `table4.RI` = numeric(), 
  `table4.SC` = numeric(), 
  `table4.SD` = numeric(), 
  `table4.TN` = numeric(), 
  `table4.TX` = numeric(), 
  `table4.UT` = numeric(), 
  `table4.VA` = numeric(), 
  `table4.VI` = numeric(), 
  `table4.VT` = numeric(), 
  `table4.WA` = numeric(), 
  `table4.WI` = numeric(), 
  `table4.WV` = numeric(), 
  `table4.WY` = numeric()
)

#### End #### 

#### Run simulation for all combinations ####

counter <- 1

for(a in (1:length(inputs.program.goal))){
  for(b in (1:length(inputs.elig.carnegie))){
    for(c in (1:length(inputs.elig.control))){
      for(d in (1:length(inputs.elig.halftime))){
        for(e in (1:length(inputs.elig.efc))){
          for(f in (1:length(inputs.elig.outofstate))){
            for(g in (1:length(inputs.elig.noncitizen))){
              for(h in (1:length(inputs.elig.fafsa))){
                for(i in (1:length(inputs.elig.gpa))){
                  print(paste("Number ", counter, " out of ", totalCombos, " at ", Sys.time(), ".", sep=""))
                  simulationResults <- rbind(
                    simulationResults, 
                    runSimulation(
                      data1 = studentDF, 
                      program.goal = inputs.program.goal[a],
                      elig.carnegie = inputs.elig.carnegie[b], 
                      elig.control = inputs.elig.control[c], 
                      elig.halftime = inputs.elig.halftime[d], 
                      elig.efc = inputs.elig.efc[e], 
                      elig.outofstate = inputs.elig.outofstate[f], 
                      elig.noncitizen = inputs.elig.noncitizen[g], 
                      elig.fafsa = inputs.elig.fafsa[h], 
                      elig.gpa = inputs.elig.gpa[i]
                    )
                  )
                  counter <- counter + 1
                }
              }
            }
          }
        }
      }
    }
  }
}

rm(counter)
rm(a, b, c, d, e, f, g, h, i, totalCombos)

#### End #### 

#### Save results for use by app #### 

setwd("/Users/peter_granville/Fed State Modeling/Model-V1")

write.csv(simulationResults, "Simulation results.csv", row.names=FALSE)

setwd("/Users/peter_granville/Fed State Modeling")

#### End #### 



