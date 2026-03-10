
#### Setup ####

library(readxl)
library(scales)
library(writexl)
library(tidyverse)
library(data.table)

quiet_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

#### End ####

################################################
#### Load datasets                          ####
################################################

#### Load processed output ####

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling")

studentDF <- read.csv(
  "All merged adjusted student data.csv",
  header=TRUE,
  check.names=FALSE
)

#### End ####

################################################
#### State government data                  ####
################################################

#### Load SHEEO data ####

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/IPEDS data")

stateDF <- read_excel(
  path="SHEEO_SHEF_FY24_Sector_Data.xlsx", 
  sheet="Sector Data"
) %>% filter(
  `FY`==2024
) %>% select(
  `State`,
  `Two-Year Education Appropriations`, 
  `Four-Year Education Appropriations`, 
  `Two-Year State Public Operating`, 
  `Four-Year State Public Operating`,
  `Two-Year Net Tuition and Fee Revenue`, 
  `Four-Year Net Tuition and Fee Revenue`,
  `Two-Year Total Education Revenue`, 
  `Four-Year Total Education Revenue`
) %>% mutate(
  `Education Appropriations` = `Two-Year Education Appropriations` + `Four-Year Education Appropriations`
)

#### End #### 

#### Load Census data ####

census <- read.csv(
  "ACSST5Y2024.S1501-2026-03-09T204116.csv", 
  header=TRUE, 
  check.names=FALSE
) 
census <- census[, grep("!!Total!!Estimate", names(census))]

census <- census[c(7, 8, 9, 10, 11, 12, 13, 14),]

census <- census %>% mutate(
  across(
    everything(), 
    as.numeric
  )
)

names(census) <- gsub("!!Total!!Estimate", "", names(census))

census_t <- transpose(census) %>% mutate(
  `State` = names(census)
)
rm(census)

census_t <- census_t %>% rename(
  `Population 25 years and over` = `V1`,
  `Less than 9th grade` = `V2`,
  `9th to 12th grade, no diploma` = `V3`,
  `High school graduate (includes equivalency)` = `V4`,
  `Some college, no degree` = `V5`,
  `Associate's degree` = `V6`,
  `Bachelor's degree` = `V7`,
  `Graduate or professional degree` = `V8`
) 

census_t <- census_t %>% mutate(
  `Population with an associate's degree or higher` = (`Associate's degree` + `Bachelor's degree` + `Graduate or professional degree`), 
  `Population with a bachelor's degree or higher` = (`Bachelor's degree` + `Graduate or professional degree`)
) %>% select(
  `State`,
  `Population 25 years and over`, 
  `Population with an associate's degree or higher`, 
  `Population with a bachelor's degree or higher`
)

stateDF <- left_join(x=stateDF, y=census_t, by="State")
rm(census_t)

#### End #### 

#### Load state abbreviations ####

stateDF <- stateDF %>% mutate(
  `STABBR` = state.abb[match(`State`, state.name)]
)
stateDF$STABBR[51] <- "US"
stateDF$STABBR[52] <- "DC"

#### End #### 

################################################
#### Institutional administration data      ####
################################################

#### Load IPEDS data ####

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/IPEDS data")

collegeDF <- read.csv("hd2024.csv", header=TRUE) %>% select(
  `UNITID`, 
  `INSTNM`, 
  `CITY`, 
  `STABBR`, 
  `CONTROL`,
  `C21BASIC`,
  `LONGITUD`, 
  `LATITUDE`, 
  `CARNEGIESIZE`
) %>% filter(
  `UNITID` %in% studentDF$`UNITID`
)

f1.2024 <- read.csv("f2324_f1a.csv", header=TRUE) %>% select(
  `UNITID`, 
  `F1B25`	  # Total all revenues and other additions
) %>% rename(
  `Total revenue` = `F1B25`
) %>% mutate(
  `Year` = rep(2024)
)
f2.2024 <- read.csv("f2324_f2.csv", header=TRUE) %>% select(
  `UNITID`, 
  `F2D16`  #	Total revenues and investment return - Total
) %>% rename(
  `Total revenue` = `F2D16`
) %>% mutate(
  `Year` = rep(2024)
)
f3.2024 <- read.csv("f2324_f3.csv", header=TRUE) %>% select(
  `UNITID`, 
  `F3D09`  #	Total revenues and investment return
) %>% rename(
  `Total revenue` = `F3D09`
) %>% mutate(
  `Year` = rep(2024)
)

f1.2023 <- read.csv("f2223_f1a.csv", header=TRUE) %>% select(
  `UNITID`, 
  `F1B25`	  # Total all revenues and other additions
) %>% rename(
  `Total revenue` = `F1B25`
) %>% mutate(
  `Year` = rep(2023)
)
f2.2023 <- read.csv("f2223_f2.csv", header=TRUE) %>% select(
  `UNITID`, 
  `F2D16`  #	Total revenues and investment return - Total
) %>% rename(
  `Total revenue` = `F2D16`
) %>% mutate(
  `Year` = rep(2023)
)
f3.2023 <- read.csv("f2223_f3.csv", header=TRUE) %>% select(
  `UNITID`, 
  `F3D09`  #	Total revenues and investment return
) %>% rename(
  `Total revenue` = `F3D09`
) %>% mutate(
  `Year` = rep(2023)
)

f1 <- rbind(f1.2024, f1.2023)
rm(f1.2024, f1.2023)
f2 <- rbind(f2.2024, f2.2023)
rm(f2.2024, f2.2023)
f3 <- rbind(f3.2024, f3.2023)
rm(f3.2024, f3.2023)

f1 <- aggregate(data=f1, `Total revenue` ~ `UNITID`, FUN=mean)
f2 <- aggregate(data=f2, `Total revenue` ~ `UNITID`, FUN=mean)
f3 <- aggregate(data=f3, `Total revenue` ~ `UNITID`, FUN=mean)

fAll <- rbind(
  f1, f2, f3
)
rm(f1, f2, f3)

collegeDF <- left_join(x=collegeDF, y=fAll, by="UNITID")
rm(fAll)

imputedRevenue <- aggregate(
  data=collegeDF, 
  `Total revenue` ~ `CONTROL` + `CARNEGIESIZE`,
  FUN=sum
) %>% rename(
  `Imputed revenue` = `Total revenue`
)
collegeDF <- left_join(x=collegeDF, y=imputedRevenue, by=c("CONTROL", "CARNEGIESIZE"))
rm(imputedRevenue)

collegeDF <- collegeDF %>% mutate(
  `Total revenue` = ifelse(
    is.na(`Total revenue`), 
    `Imputed revenue`, 
    `Total revenue`
  )
) %>% select(
  -(`Imputed revenue`)
)

#### End #### 

################################################
#### Function A: [Plan A]                   ####
#### Fed-state partnership: Reduce tuition  ####
#### and fees to $X                         ####
################################################

functionA <- function(
    
  #### List inputs ####
  
  studentData,
  stateData, 
  collegeData, 
  select1, # To what amount does tuition and fees among eligible students decrease? 
  select2, # For each $1 in the federal block grant, how much does a participating state need to match? 
  select3, # Is student eligibility limited on the basis of enrollment intensity?
  select4, # Is student eligibility limited on a financial basis?
  select5, # Is student eligibility limited to in-state students? 
  select6, # Skipped
  select7, # Is institutional eligibility limited to a certain level?
  select8, # Should it be assumed that states that declined to participate in ACA’s Medicaid expansion will decline to participate in this program? 
  select9  # What level of increase in a state’s annual appropriations for higher education would be too large to participate in the program? 
    
  #### End #### 
    
){
 
  # studentData <- studentDF
  # stateData <- stateDF
  # collegeData <- collegeDF
  # select1 <- "$1,000 at all eligible institutions"
  # select2 <- "$0.50"
  # select3 <- "No restriction based on enrollment intensity"
  # select4 <- "Pell Grant recipients only"
  # select5 <- "Yes"
  # select6 <- "Skipped"
  # select7 <- "Only two-year institutions"
  # select8 <- "No"
  # select9 <- "15% and above"
  
  #### [S] Institutional eligibility ####
  
  collegeData <- collegeData %>% mutate(
    `Eligible` = ifelse(
      `CONTROL`==1, 
      "Yes", 
      "No"
    )
  )
  
  if(select7=="Only two-year institutions"){
    collegeData <- collegeData %>% mutate(
      `Eligible` = ifelse(
        `C21BASIC` %in% (15:32), 
        "No", 
        `Eligible`
      )
    )
  }
  
  #### End #### 
  
  #### [S] Student eligibility ####
  
  studentData <- studentData %>% mutate(
    `Eligible` = rep("Yes")
  )
  
  # Is student eligibility limited on the basis of enrollment intensity?
  if(select3=="Restricted to students enrolled full-time"){
    studentData <- studentData %>% mutate(
      `Eligible` = ifelse(
        `Enrollment intensity`=="Part-time", 
        "No", 
        `Eligible`
      )
    )
  }
  
  # Is student eligibility limited on a financial basis?
  if(select4=="Pell Grant recipients only"){
    studentData <- studentData %>% mutate(
      `Eligible` = ifelse(
        `Receives Pell`=="No", 
        "No", 
        `Eligible`
      )
    )
  }
  
  # Is student eligibility limited to in-state students?
  if(select5=="Yes"){
    studentData <- studentData %>% mutate(
      `Eligible` = ifelse(
        `Tuition jurisdiction`=="Out-of-state tuition", 
        "No", 
        `Eligible`
      )
    )
  }
  
  #### End #### 
  
  #### [C] Combine eligibility ####
  
  eligibleColleges <- collegeData %>% filter(`Eligible`=="Yes")
  studentData <- studentData %>% mutate(
    `Eligible` = ifelse(
      (`UNITID` %in% eligibleColleges$`UNITID`)==FALSE,
      "No", 
      `Eligible`
    )
  )
  rm(eligibleColleges)

  #### End #### 
  
  #### [S] Target ####
  
  if(select1=="$0 at all eligible institutions"){
    studentData <- studentData %>% mutate(
      `Target tuition` = rep(0)
    )
  }
  if(select1=="$1,000 at all eligible institutions"){
    studentData <- studentData %>% mutate(
      `Target tuition` = pmin(1000, `Tuition and fees paid`)
    )
  }
  if(select1=="$1,000 at eligible two-year institutions and $3,000 at eligible four-year institutions"){
    studentData <- studentData %>% mutate(
      `Target tuition` = ifelse(
        `C18BASIC` %in% (15:32),
        pmin(3000, `Tuition and fees paid`), 
        pmin(1000, `Tuition and fees paid`)
      )
    )
  }
  
  #### End #### 
  
  #### [C] Calculate average delta ####
  
  studentData <- studentData %>% mutate(
    `Delta` = ifelse(
      `Eligible`=="Yes", 
      pmax(`Tuition and fees paid` - `Target tuition`, 0), # How much cost has to move 
      NA
    )
  )
  
  mdnDelta <- median(studentData$`Delta`[studentData$`Enrollment intensity`=="Full-time"], na.rm=TRUE)
  
  #### End #### 

  #### [S] Define state match ####
  
  if(select2=="$0.10"){stateMatch <- 0.1}
  if(select2=="$0.25"){stateMatch <- 0.25}
  if(select2=="$0.50"){stateMatch <- 0.5}
  if(select2=="$1.00"){stateMatch <- 1}
  
  #### End #### 
  
  #### [C] Calculate participation costs ####
  
  # Eligible FTEs 
  studentData <- studentData %>% mutate(
    `Eligible FTEs` = ifelse(
      `Eligible`=="No", 
      0, 
      ifelse(
        `Enrollment intensity`=="Full=time", 
        1, 
        0.5
      )
    )
  )
  eligibleFTEs <- aggregate(
    data=studentData, 
    `Eligible FTEs` ~ `STABBR`, 
    FUN=sum
  )
  stateData <- left_join(x=stateData, y=eligibleFTEs, by="STABBR")
  rm(eligibleFTEs)
  
  federalPerFTE <- mdnDelta / (1 + stateMatch)
  statePerFTE <- federalPerFTE * stateMatch
  
  stateData <- stateData %>% mutate(
    `Federal funding per FTE` = rep(federalPerFTE), 
    `State cost of entry per FTE` = rep(statePerFTE)
  ) %>% mutate(
    `Federal block grant` = rep(`Federal funding per FTE` * `Eligible FTEs`), 
    `State cost of entry` = rep(`State cost of entry per FTE` * `Eligible FTEs`)
  )
  rm(federalPerFTE, statePerFTE, mdnDelta, stateMatch)
  
  #### End ####
  
  #### [C] State grant aid among eligible students ####
  
  stateGrants <- aggregate(
    data=studentData, 
    `State grant amount` ~ `STABBR` + `Eligible`, 
    FUN=sum
  ) %>% filter(
    `Eligible`=="Yes"
  ) %>% select(
    -(`Eligible`)
  )  
  stateData <- left_join(x=stateData, y=stateGrants, by="STABBR")
  rm(stateGrants)
  
  #### End #### 
  
  #### [C] Calculate overflow and backfill ####
  
  trueDelta <- aggregate(
    data=studentData, 
    `Delta` ~ `STABBR` + `Eligible`, 
    FUN=sum
  ) %>% filter(
    `Eligible`=="Yes"
  ) %>% select(
    -(`Eligible`)
  ) %>% rename(
    `Cost of enactment` = `Delta`
  )
  stateData <- left_join(x=stateData, y=trueDelta, by="STABBR")
  rm(trueDelta)
  
  stateData <- stateData %>% mutate(
    `State cost of entry minus state grants` = pmax(`State cost of entry` - `State grant amount`, 0)
  ) %>% mutate(
    `Overflow or backfill` = ifelse(
      (`State cost of entry minus state grants` + `Federal block grant`) >= `Cost of enactment`,
      "Overflow", 
      "Backfill"
    )
  ) %>% mutate(
    `Overflow amount` = ifelse(
      `Overflow or backfill`=="Overflow",
      (`State cost of entry minus state grants` + `Federal block grant`) - `Cost of enactment`,
      0
    ), 
    `Backfill amount` = ifelse(
      `Overflow or backfill`=="Backfill",
      `Cost of enactment`- (`State cost of entry minus state grants` + `Federal block grant`),
      0
    )
  ) %>% mutate(
    `Total state contributions` = `State cost of entry minus state grants` + `Backfill amount`
  )
   
  #### End ####
  
  #### [C] Calculate increase in state funding ####
  
  stateData <- stateData %>% mutate(
    `Total state contributions as a share of education appropriations` = `Total state contributions` / `Education Appropriations`
  )
  
  #### End ####
  
  #### [S] State participation: Financial ####
  
  if(select9=="15% and above"){maxIncrease <- 0.15}
  if(select9=="25% and above"){maxIncrease <- 0.25}
  if(select9=="35% and above"){maxIncrease <- 0.35}
  
  stateData <- stateData %>% mutate(
    `Participation status` = rep("Yes")
  ) %>% mutate(
    `Participation status` = ifelse(
      `Total state contributions as a share of education appropriations` > maxIncrease, 
      "No",
      `Participation status`
    )
  )
  
  rm(maxIncrease)
  
  #### End #### 
  
  #### [S] State participation: Political ####
  
  if(select8=="Yes"){
    stateData <- stateData %>% mutate(
      `Participation status` = ifelse(
        `State` %in% c("Florida", "Georgia", "Kansas", "Mississippi", "South Carolina", "Wisconsin", "Wyoming"),
        "No",
        `Participation status`
      )
    )
  }
  
  #### End ####
  
  #### [C] Combine participation ####
  
  participantStates <- stateData %>% filter(
    `Participation status`=="Yes"
  )
  
  studentData <- studentData %>% mutate(
    `Participant` = ifelse(
      (`STABBR` %in% participantStates$`STABBR`) | (`STABBR` %in% c(
        "AS", "FM", "GU", "MH", "MP", "PR", "PW", "VI"
      )), 
      "Yes", 
      "No"
    )
  ) %>% mutate(
    `Participant` = ifelse(
      `Eligible`=="No", 
      "No", 
      `Participant`
    )
  )
  
  rm(participantStates)
  
  #### End #### 
  
  
}

################################################
#### Function B: [Plan B]                   ####
#### Fed-state partnership: Reduce tuition  ####
#### and fees by X%                         ####
################################################

functionB <- function(

  #### List inputs ####
  
  studentData,
  stateData, 
  collegeData, 
  select1, # By what percentage does tuition and fees among eligible students decrease?
  select2, # For each $1 in the federal block grant, how much does a participating state need to match? 
  select3, # Is student eligibility limited on the basis of enrollment intensity?
  select4, # Is student eligibility limited on a financial basis?
  select5, # Is student eligibility limited to in-state students? 
  select6, # Skipped
  select7, # Is institutional eligibility limited to a certain level?
  select8, # Should it be assumed that states that declined to participate in ACA’s Medicaid expansion will decline to participate in this program? 
  select9  # What level of increase in a state’s annual appropriations for higher education would be too large to participate in the program? 
  
  #### End #### 
  
){
  
}

################################################
#### Function C: [Plan C]                   ####
#### Fed-state partnership: Increase grants ####
#### to reduce student debt by X%           ####
################################################

functionC <- function(

  #### List inputs ####
  
  studentData,
  stateData, 
  collegeData,  
  select1, # By what percentage does student debt among eligible students decrease?
  select2, # For each $1 in the federal block grant, how much does a participating state need to match? 
  select3, # Is student eligibility limited on the basis of enrollment intensity?
  select4, # Is student eligibility limited on a financial basis?
  select5, # Is student eligibility limited to in-state students? 
  select6, # Skipped
  select7, # Is institutional eligibility limited to a certain level?
  select8, # Should it be assumed that states that declined to participate in ACA’s Medicaid expansion will decline to participate in this program? 
  select9  # What level of increase in a state’s annual appropriations for higher education would be too large to participate in the program? 
  
  #### End #### 
  
){
  
}

################################################
#### Function D: [Plan D]                   ####
#### Fed-state partnership: Increase grants ####
#### to reduce net price to X% of family    #### 
#### income                                 ####
################################################

functionD <- function(

  #### List inputs ####
  
  studentData,
  stateData, 
  collegeData, 
  select1, # To what maximum percentage of family income is the net price among eligible students reduced?
  select2, # For each $1 in the federal block grant, how much does a participating state need to match? 
  select3, # Is student eligibility limited on the basis of enrollment intensity?
  select4, # Is student eligibility limited on a financial basis?
  select5, # Is student eligibility limited to in-state students? 
  select6, # Skipped
  select7, # Is institutional eligibility limited to a certain level?
  select8, # Should it be assumed that states that declined to participate in ACA’s Medicaid expansion will decline to participate in this program? 
  select9  # What level of increase in a state’s annual appropriations for higher education would be too large to participate in the program? 
  
  #### End #### 
  
){
  
}

################################################
#### Function E: [Plan E]                   ####
#### Fed-state partnership: Increase        ####
#### federal and state investment to equal  ####
#### X% of revenue                          ####
################################################

functionE <- function(

  #### List inputs ####
  
  studentData,
  stateData, 
  collegeData, 
  select1, # To what minimum percentage of total revenue are federal and state funds increased? 
  select2, # For each $1 in state funding for public institutions, how much does the federal government match? 
  select3, # Skipped
  select4, # Skipped
  select5, # Skipped
  select6, # Skipped
  select7, # Is institutional eligibility limited to a certain level?
  select8, # Should it be assumed that states that declined to participate in ACA’s Medicaid expansion will decline to participate in this program? 
  select9  # What level of increase in a state’s annual appropriations for higher education would be too large to participate in the program? 
  
  #### End #### 
  
){
  
}

################################################
#### Function F: [Plan F]                   ####
#### Fed-college partnership: Government    ####
#### sends colleges subsidy in exchange for ####
#### X pricing policy                       ####
################################################

functionF <- function(

  #### List inputs ####
  
  studentData,
  stateData, 
  collegeData, 
  select1, # What pricing policy is required among participating colleges?
  select2, # Skipped
  select3, # Does the policy only apply to students enrolled full-time?
  select4, # Is student eligibility limited on a financial basis?
  select5, # Does the policy only apply to in-state students?
  select6, # Is institutional eligibility limited to certain controls?
  select7, # Is institutional eligibility limited to a certain level?
  select8, # Skipped
  select9  # What level of decrease in a college’s total revenue would be too large to participate in the program?
  
  #### End #### 
  
){
  
}

################################################
#### Function G: [Plan G]                   ####
#### Increase federal grants to students    #### 
#### by X%                                  ####
################################################

functionG <- function(

  #### List inputs ####
  
  studentData,
  stateData, 
  collegeData, 
  select1, # By what percentage do total federal grants among eligible students increase? 
  select2, # Skipped 
  select3, # Skipped
  select4, # Skipped
  select5, # Skipped
  select6, # Is institutional eligibility limited to certain controls?
  select7, # Is institutional eligibility limited to a certain level?
  select8, # Skipped 
  select9  # Skipped
  
  #### End #### 
  
){
  
}

################################################
#### Function 1: Institutional              ####
#### participation map                      ####
################################################

function1 <- function(){
  
}

################################################
#### Function 2: State                      ####
#### participation map                      ####
################################################

function2 <- function(){
  
}

################################################
#### Function 3: Net price percentiles      ####
################################################

function3 <- function(){
  
}

################################################
#### Function 4: Educational attainment     ####
################################################

function4 <- function(){
  
}

################################################
#### Function 5: Economic impact            ####
################################################

function5 <- function(){
  
}

################################################
#### Function 6: State funding              ####
################################################

function6 <- function(){
  
}

################################################
#### Function 7: Student participation      ####
################################################

function7 <- function(){
  
}

################################################
#### Function 8: Student debt               ####
################################################

function8 <- function(){
  
}

################################################
#### Function 9: Degrees and certificates   ####
################################################

function9 <- function(){
  
}

################################################
#### Function 10: Government cost           ####
################################################

function10 <- function(){
  
}

################################################
#### Set choice lists                       ####
################################################

#### Choice list 1 ####

choices1a <- c(
  "$0 at all eligible institutions", 
  "$1,000 at all eligible institutions",
  "$1,000 at eligible two-year institutions and $3,000 at eligible four-year institutions"
)

choices1b <- c(
  "100%", 
  "50%", 
  "25%"
)

choices1c <- choices1b

choices1d <- c(
  "10%", 
  "20%", 
  "30%"
)

choices1e <- c(
  "65%", 
  "75%", 
  "85%"
)

choices1f <- c(
  "No tuition charged", 
  "No undergraduate loans"
)

choices1g <- choices1b

#### End #### 

#### Choice list 2 ####

choices2a <- c(
  "$0.10", 
  "$0.25", 
  "$0.50", 
  "$1.00"
)

choices2b <- choices2a

choices2c <- choices2a

choices2d <- choices2a

choices2e <- c(
  "$0.50", 
  "$1.00", 
  "$1.50"
)

choices2f <- c(
  "Skipped"
)

choices2g <- c(
  "Skipped"
)

#### End #### 

#### Choice list 3 ####

choices3a <- c(
  "No restriction based on enrollment intensity", 
  "Restricted to students enrolled full-time"
)

choices3b <- choices3a

choices3c <- choices3a

choices3d <- choices3a

choices3e <- c(
  "Skipped"
)

choices3f <- choices3a

choices3g <- c(
  "Skipped"
)

#### End #### 

#### Choice list 4 ####

choices4a <- c(
  "No means testing", 
  "Pell Grant recipients only"
)

choices4b <- choices4a

choices4c <- choices4a

choices4d <- choices4a

choices4e <- c(
  "Skipped"
)

choices4f <- choices4a

choices4g <- c(
  "Skipped"
)

#### End #### 

#### Choice list 5 ####

choices5a <- c(
  "Yes", 
  "No"
)

choices5b <- choices5a

choices5c <- choices5a

choices5d <- choices5a

choices5e <- c(
  "Skipped"
)

choices5f <- choices5a

choices5g <- c(
  "Skipped"
)

#### End #### 

#### Choice list 6 ####

choices6a <- c(
  "Skipped"
)

choices6b <- c(
  "Skipped"
)

choices6c <- c(
  "Skipped"
)

choices6d <- c(
  "Skipped"
)

choices6e <- c(
  "Skipped"
)

choices6f <- c(
  "Public only", 
  "Public and nonprofit only", 
  "All controls"
)

choices6g <- choices6f

#### End #### 

#### Choice list 7 ####

choices7a <- c(
  "Only two-year institutions", 
  "Both two- and four-year institutions"
)

choices7b <- choices7a

choices7c <- choices7a

choices7d <- choices7a

choices7e <- choices7a

choices7f <- choices7a

choices7g <- choices7a

#### End #### 

#### Choice list 8 ####

choices8a <- c(
  "No", 
  "Yes"
)

choices8b <- choices8a

choices8c <- choices8a

choices8d <- choices8a

choices8e <- choices8a

choices8f <- c(
  "Skipped"
)

choices8g <- c(
  "Skipped"
)

#### End #### 

#### Choice list 9 ####

choices9a <- c(
  "15% and above", 
  "25% and above", 
  "35% and above"
)

choices9b <- choices9a

choices9c <- choices9a

choices9d <- choices9a

choices9e <- choices9a

choices9f <- c(
  "5% and above", 
  "10% and above", 
  "15% and above"
)

choices9g <- c(
  "Skipped"
)

#### End #### 

#### Count total number of simulations for ReadMe ####

(
  length(choices1a) * length(choices2a) * length(choices3a) * length(choices4a) * length(choices5a) * length(choices6a) * length(choices7a) * length(choices8a) * length(choices9a)
) + (
  length(choices1b) * length(choices2b) * length(choices3b) * length(choices4b) * length(choices5b) * length(choices6b) * length(choices7b) * length(choices8b) * length(choices9b)
) + (
  length(choices1c) * length(choices2c) * length(choices3c) * length(choices4c) * length(choices5c) * length(choices6c) * length(choices7c) * length(choices8c) * length(choices9c)
) + (
  length(choices1d) * length(choices2d) * length(choices3d) * length(choices4d) * length(choices5d) * length(choices6d) * length(choices7d) * length(choices8d) * length(choices9d)
) + (
  length(choices1e) * length(choices2e) * length(choices3e) * length(choices4e) * length(choices5e) * length(choices6e) * length(choices7e) * length(choices8e) * length(choices9e)
) + (
  length(choices1f) * length(choices2f) * length(choices3f) * length(choices4f) * length(choices5f) * length(choices6f) * length(choices7f) * length(choices8f) * length(choices9f)
) + (
  length(choices1g) * length(choices2g) * length(choices3g) * length(choices4g) * length(choices5g) * length(choices6g) * length(choices7g) * length(choices8g) * length(choices9g)
) 

#### End #### 

################################################
#### Write File A                           ####
################################################

counter <- 0
for(r in choices1a){
  for(s in choices2a){
    for(t in choices3a){
      for(u in choices4a){
        for(v in choices5a){
          for(w in choices6a){
            for(x in choices7a){
              for(y in choices8a){
                for(z in choices9a){
                  
                  counter <- counter + 1
                  
                  #### Run simulations #### 
                  #### End #### 
                  
                  #### Bind output ####
                  
                  if(counter==1){
                    output1 <- temp1
                    output2 <- temp2
                    output3 <- temp3
                    output4 <- temp4
                    output5 <- temp5
                    output6 <- temp6
                    output7 <- temp7
                    output8 <- temp8
                    output9 <- temp9
                    output10 <- temp10
                  }else{
                    output1 <- rbind(output1, temp1)
                    output2 <- rbind(output2, temp2)
                    output3 <- rbind(output3, temp3)
                    output4 <- rbind(output4, temp4)
                    output5 <- rbind(output5, temp5)
                    output6 <- rbind(output6, temp6)
                    output7 <- rbind(output7, temp7)
                    output8 <- rbind(output8, temp8)
                    output9 <- rbind(output9, temp9)
                    output10 <- rbind(output10, temp10)
                  }
                  
                  #### End #### 
                  
}}}}}}}}}

#### Write File A #### 

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Model-V2/Simulation results")

sheetList <- list(
  Sheet1 = output1,
  Sheet2 = output2,
  Sheet3 = output3,
  Sheet4 = output4,
  Sheet5 = output5,
  Sheet6 = output6,
  Sheet7 = output7,
  Sheet8 = output8,
  Sheet9 = output9,
  Sheet10 = output10
)

write_xlsx(sheetList, "Plan A.xlsx")
rm(sheetList, output1, output2, output3, output4, output5, output6, output7, output8, output9, output10, counter)
rm(choices1a, choices2a, choices3a, choices4a, choices5a, choices6a, choices7a, choices8a, choices9a)

#### End #### 

################################################
#### Write File B                           ####
################################################

counter <- 0
for(r in choices1b){
  for(s in choices2b){
    for(t in choices3b){
      for(u in choices4b){
        for(v in choices5b){
          for(w in choices6b){
            for(x in choices7b){
              for(y in choices8b){
                for(z in choices9b){
                  
                  counter <- counter + 1
                  
                  #### Run simulations #### 
                  #### End #### 
                  
                  #### Bind output ####
                  
                  if(counter==1){
                    output1 <- temp1
                    output2 <- temp2
                    output3 <- temp3
                    output4 <- temp4
                    output5 <- temp5
                    output6 <- temp6
                    output7 <- temp7
                    output8 <- temp8
                    output9 <- temp9
                    output10 <- temp10
                  }else{
                    output1 <- rbind(output1, temp1)
                    output2 <- rbind(output2, temp2)
                    output3 <- rbind(output3, temp3)
                    output4 <- rbind(output4, temp4)
                    output5 <- rbind(output5, temp5)
                    output6 <- rbind(output6, temp6)
                    output7 <- rbind(output7, temp7)
                    output8 <- rbind(output8, temp8)
                    output9 <- rbind(output9, temp9)
                    output10 <- rbind(output10, temp10)
                  }
                  
                  #### End #### 
                  
}}}}}}}}}

#### Write File B #### 

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Model-V2/Simulation results")

sheetList <- list(
  Sheet1 = output1,
  Sheet2 = output2,
  Sheet3 = output3,
  Sheet4 = output4,
  Sheet5 = output5,
  Sheet6 = output6,
  Sheet7 = output7,
  Sheet8 = output8,
  Sheet9 = output9,
  Sheet10 = output10
)

write_xlsx(sheetList, "Plan B.xlsx")
rm(sheetList, output1, output2, output3, output4, output5, output6, output7, output8, output9, output10, counter)
rm(choices1b, choices2b, choices3b, choices4b, choices5b, choices6b, choices7b, choices8b, choices9b)

#### End #### 

################################################
#### Write File C                           ####
################################################

counter <- 0
for(r in choices1c){
  for(s in choices2c){
    for(t in choices3c){
      for(u in choices4c){
        for(v in choices5c){
          for(w in choices6c){
            for(x in choices7c){
              for(y in choices8c){
                for(z in choices9c){
                  
                  counter <- counter + 1
                  
                  #### Run simulations #### 
                  #### End #### 
                  
                  #### Bind output ####
                  
                  if(counter==1){
                    output1 <- temp1
                    output2 <- temp2
                    output3 <- temp3
                    output4 <- temp4
                    output5 <- temp5
                    output6 <- temp6
                    output7 <- temp7
                    output8 <- temp8
                    output9 <- temp9
                    output10 <- temp10
                  }else{
                    output1 <- rbind(output1, temp1)
                    output2 <- rbind(output2, temp2)
                    output3 <- rbind(output3, temp3)
                    output4 <- rbind(output4, temp4)
                    output5 <- rbind(output5, temp5)
                    output6 <- rbind(output6, temp6)
                    output7 <- rbind(output7, temp7)
                    output8 <- rbind(output8, temp8)
                    output9 <- rbind(output9, temp9)
                    output10 <- rbind(output10, temp10)
                  }
                  
                  #### End #### 
                  
}}}}}}}}}

#### Write File C #### 

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Model-V2/Simulation results")

sheetList <- list(
  Sheet1 = output1,
  Sheet2 = output2,
  Sheet3 = output3,
  Sheet4 = output4,
  Sheet5 = output5,
  Sheet6 = output6,
  Sheet7 = output7,
  Sheet8 = output8,
  Sheet9 = output9,
  Sheet10 = output10
)

write_xlsx(sheetList, "Plan C.xlsx")
rm(sheetList, output1, output2, output3, output4, output5, output6, output7, output8, output9, output10, counter)
rm(choices1c, choices2c, choices3c, choices4c, choices5c, choices6c, choices7c, choices8c, choices9c)

#### End #### 

################################################
#### Write File D                           ####
################################################

counter <- 0
for(r in choices1d){
  for(s in choices2d){
    for(t in choices3d){
      for(u in choices4d){
        for(v in choices5d){
          for(w in choices6d){
            for(x in choices7d){
              for(y in choices8d){
                for(z in choices9d){
                  
                  counter <- counter + 1
                  
                  #### Run simulations #### 
                  #### End #### 
                  
                  #### Bind output ####
                  
                  if(counter==1){
                    output1 <- temp1
                    output2 <- temp2
                    output3 <- temp3
                    output4 <- temp4
                    output5 <- temp5
                    output6 <- temp6
                    output7 <- temp7
                    output8 <- temp8
                    output9 <- temp9
                    output10 <- temp10
                  }else{
                    output1 <- rbind(output1, temp1)
                    output2 <- rbind(output2, temp2)
                    output3 <- rbind(output3, temp3)
                    output4 <- rbind(output4, temp4)
                    output5 <- rbind(output5, temp5)
                    output6 <- rbind(output6, temp6)
                    output7 <- rbind(output7, temp7)
                    output8 <- rbind(output8, temp8)
                    output9 <- rbind(output9, temp9)
                    output10 <- rbind(output10, temp10)
                  }
                  
                  #### End #### 
                  
}}}}}}}}}

#### Write File D #### 

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Model-V2/Simulation results")

sheetList <- list(
  Sheet1 = output1,
  Sheet2 = output2,
  Sheet3 = output3,
  Sheet4 = output4,
  Sheet5 = output5,
  Sheet6 = output6,
  Sheet7 = output7,
  Sheet8 = output8,
  Sheet9 = output9,
  Sheet10 = output10
)

write_xlsx(sheetList, "Plan D.xlsx")
rm(sheetList, output1, output2, output3, output4, output5, output6, output7, output8, output9, output10, counter)
rm(choices1d, choices2d, choices3d, choices4d, choices5d, choices6d, choices7d, choices8d, choices9d)

#### End #### 

################################################
#### Write File E                           ####
################################################

counter <- 0
for(r in choices1e){
  for(s in choices2e){
    for(t in choices3e){
      for(u in choices4e){
        for(v in choices5e){
          for(w in choices6e){
            for(x in choices7e){
              for(y in choices8e){
                for(z in choices9e){
                  
                  counter <- counter + 1
                  
                  #### Run simulations #### 
                  #### End #### 
                  
                  #### Bind output ####
                  
                  if(counter==1){
                    output1 <- temp1
                    output2 <- temp2
                    output3 <- temp3
                    output4 <- temp4
                    output5 <- temp5
                    output6 <- temp6
                    output7 <- temp7
                    output8 <- temp8
                    output9 <- temp9
                    output10 <- temp10
                  }else{
                    output1 <- rbind(output1, temp1)
                    output2 <- rbind(output2, temp2)
                    output3 <- rbind(output3, temp3)
                    output4 <- rbind(output4, temp4)
                    output5 <- rbind(output5, temp5)
                    output6 <- rbind(output6, temp6)
                    output7 <- rbind(output7, temp7)
                    output8 <- rbind(output8, temp8)
                    output9 <- rbind(output9, temp9)
                    output10 <- rbind(output10, temp10)
                  }
                  
                  #### End #### 
                  
}}}}}}}}}

#### Write File E #### 

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Model-V2/Simulation results")

sheetList <- list(
  Sheet1 = output1,
  Sheet2 = output2,
  Sheet3 = output3,
  Sheet4 = output4,
  Sheet5 = output5,
  Sheet6 = output6,
  Sheet7 = output7,
  Sheet8 = output8,
  Sheet9 = output9,
  Sheet10 = output10
)

write_xlsx(sheetList, "Plan E.xlsx")
rm(sheetList, output1, output2, output3, output4, output5, output6, output7, output8, output9, output10, counter)
rm(choices1e, choices2e, choices3e, choices4e, choices5e, choices6e, choices7e, choices8e, choices9e)

#### End #### 

################################################
#### Write File F                           ####
################################################

counter <- 0
for(r in choices1f){
  for(s in choices2f){
    for(t in choices3f){
      for(u in choices4f){
        for(v in choices5f){
          for(w in choices6f){
            for(x in choices7f){
              for(y in choices8f){
                for(z in choices9f){
                  
                  counter <- counter + 1
                  
                  #### Run simulations #### 
                  #### End #### 
                  
                  #### Bind output ####
                  
                  if(counter==1){
                    output1 <- temp1
                    # output2 <- temp2
                    output3 <- temp3
                    output4 <- temp4
                    output5 <- temp5
                    # output6 <- temp6
                    output7 <- temp7
                    output8 <- temp8
                    output9 <- temp9
                    output10 <- temp10
                  }else{
                    output1 <- rbind(output1, temp1)
                    # output2 <- rbind(output2, temp2)
                    output3 <- rbind(output3, temp3)
                    output4 <- rbind(output4, temp4)
                    output5 <- rbind(output5, temp5)
                    # output6 <- rbind(output6, temp6)
                    output7 <- rbind(output7, temp7)
                    output8 <- rbind(output8, temp8)
                    output9 <- rbind(output9, temp9)
                    output10 <- rbind(output10, temp10)
                  }
                  
                  #### End #### 
                  
}}}}}}}}}

#### Write File F #### 

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Model-V2/Simulation results")

sheetList <- list(
  Sheet1 = output1,
  # Sheet2 = output2,
  Sheet3 = output3,
  Sheet4 = output4,
  Sheet5 = output5,
  # Sheet6 = output6,
  Sheet7 = output7,
  Sheet8 = output8,
  Sheet9 = output9,
  Sheet10 = output10
)

write_xlsx(sheetList, "Plan F.xlsx")
rm(
  sheetList, output1, 
  # output2, 
  output3, output4, output5, 
  # output6, 
  output7, output8, output9, output10, counter
)
rm(choices1f, choices2f, choices3f, choices4f, choices5f, choices6f, choices7f, choices8f, choices9f)

#### End #### 

################################################
#### Write File G                           ####
################################################

counter <- 0
for(r in choices1g){
  for(s in choices2g){
    for(t in choices3g){
      for(u in choices4g){
        for(v in choices5g){
          for(w in choices6g){
            for(x in choices7g){
              for(y in choices8g){
                for(z in choices9g){
                  
                  counter <- counter + 1
                  
                  #### Run simulations #### 
                  #### End #### 
                  
                  #### Bind output ####
                  
                  if(counter==1){
                    output1 <- temp1
                    # output2 <- temp2
                    output3 <- temp3
                    output4 <- temp4
                    output5 <- temp5
                    # output6 <- temp6
                    output7 <- temp7
                    output8 <- temp8
                    output9 <- temp9
                    output10 <- temp10
                  }else{
                    output1 <- rbind(output1, temp1)
                    # output2 <- rbind(output2, temp2)
                    output3 <- rbind(output3, temp3)
                    output4 <- rbind(output4, temp4)
                    output5 <- rbind(output5, temp5)
                    # output6 <- rbind(output6, temp6)
                    output7 <- rbind(output7, temp7)
                    output8 <- rbind(output8, temp8)
                    output9 <- rbind(output9, temp9)
                    output10 <- rbind(output10, temp10)
                  }
                  
                  #### End #### 
                  
}}}}}}}}}

#### Write File G #### 

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Model-V2/Simulation results")

sheetList <- list(
  Sheet1 = output1,
  # Sheet2 = output2,
  Sheet3 = output3,
  Sheet4 = output4,
  Sheet5 = output5,
  # Sheet6 = output6,
  Sheet7 = output7,
  Sheet8 = output8,
  Sheet9 = output9,
  Sheet10 = output10
)

write_xlsx(sheetList, "Plan G.xlsx")
rm(
  sheetList, output1, 
  # output2, 
  output3, output4, output5, 
  # output6, 
  output7, output8, output9, output10, counter
)
rm(choices1g, choices2g, choices3g, choices4g, choices5g, choices6g, choices7g, choices8g, choices9g)

#### End #### 




