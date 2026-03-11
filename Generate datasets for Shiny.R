
#### Setup ####

library(readxl)
library(scales)
library(writexl)
library(tidyverse)
library(data.table)

quiet_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

mem.maxVSize(32000)

#### End ####

#### Write function to add model specification columns #### 

specs <- function(
  data1
){
  data1 <- data1 %>% mutate(
    `select1` = rep(r), 
    `select2` = rep(s), 
    `select3` = rep(t), 
    `select4` = rep(u), 
    `select5` = rep(v), 
    `select6` = rep(w), 
    `select7` = rep(x), 
    `select8` = rep(y), 
    `select9` = rep(z)
  )
  return(data1)
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
) %>% filter(
  `State` != "U.S."
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
stateDF$STABBR[51] <- "DC"

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

#### Save directory info for app ####

saveCollege <- collegeDF %>% select(
  `UNITID`,
  `INSTNM`,
  `CITY`,
  `STABBR`,
  `CONTROL`,
  `C21BASIC`,
  `LONGITUD`, 
  `LATITUDE`
)

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Model-V2")
write.csv(saveCollege, "College info.csv", row.names=FALSE)
rm(saveCollege)

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
    `Eligible` = ifelse(
      `Citizenship`=="Citizen or eligible non-citizen", 
      "Yes", 
      "No"
    )
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
  ) %>% mutate(
    `Turn down due to finances` = ifelse(
      `Total state contributions as a share of education appropriations` > maxIncrease, 
      "Yes",  
      "No"
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
  
  #### [C] Store pricing and aid changes ####
  
  studentData <- studentData %>% mutate(
    `New tuition subsidy` = ifelse(
      `Participant`=="Yes", 
      `Delta`, 
      0
    ), 
    `New grants` = rep(0)
  )
  
  #### End #### 
  
  #### [C] Store institutional funding changes ####
  
  stateData <- stateData %>% mutate(
    `Overflow per FTE` = `Overflow amount` / `Eligible FTEs`
  ) 
  
  importOverflow <- stateData %>% select(
    `STABBR`, 
    `Overflow per FTE`
  )
  studentData <- left_join(x=studentData, y=importOverflow, by="STABBR")
  rm(importOverflow)
  
  studentData <- studentData %>% mutate(
    `Overflow per FTE` = ifelse(
      is.na(`Overflow per FTE`), 
      0, 
      `Overflow per FTE`
    )
  ) %>% mutate(
    `Overflow per FTE` = ifelse(
      `Participant`=="Yes", 
      `Overflow per FTE`, 
      0
    )
  ) %>% mutate(
    `Overflow` = ifelse(
      `Enrollment intensity` == "Part-time", 
      `Overflow per FTE` * 0.5, 
      `Overflow per FTE`
    )
  ) %>% select(
    -(`Overflow per FTE`)
  )
    
  #### End #### 
  
  #### [C] Return list ####
  
  return(list(studentData, stateData, collegeData))
  
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
    `Eligible` = ifelse(
      `Citizenship`=="Citizen or eligible non-citizen", 
      "Yes", 
      "No"
    )
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
  
  if(select1=="100%"){
    studentData <- studentData %>% mutate(
      `Target tuition` = rep(0)
    )
  }
  if(select1=="50%"){
    studentData <- studentData %>% mutate(
      `Target tuition` = `Tuition and fees paid` * 0.5
    )
  }
  if(select1=="25%"){
    studentData <- studentData %>% mutate(
      `Target tuition` = `Tuition and fees paid` * 0.25
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
  ) %>% mutate(
    `Turn down due to finances` = ifelse(
      `Total state contributions as a share of education appropriations` > maxIncrease, 
      "Yes",  
      "No"
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
  
  #### [C] Store pricing and aid changes ####
  
  studentData <- studentData %>% mutate(
    `New tuition subsidy` = ifelse(
      `Participant`=="Yes", 
      `Delta`, 
      0
    ), 
    `New grants` = rep(0)
  )
  
  #### End #### 
  
  #### [C] Store institutional funding changes ####
  
  stateData <- stateData %>% mutate(
    `Overflow per FTE` = `Overflow amount` / `Eligible FTEs`
  ) 
  
  importOverflow <- stateData %>% select(
    `STABBR`, 
    `Overflow per FTE`
  )
  studentData <- left_join(x=studentData, y=importOverflow, by="STABBR")
  rm(importOverflow)
  
  studentData <- studentData %>% mutate(
    `Overflow per FTE` = ifelse(
      is.na(`Overflow per FTE`), 
      0, 
      `Overflow per FTE`
    )
  ) %>% mutate(
    `Overflow per FTE` = ifelse(
      `Participant`=="Yes", 
      `Overflow per FTE`, 
      0
    )
  ) %>% mutate(
    `Overflow` = ifelse(
      `Enrollment intensity` == "Part-time", 
      `Overflow per FTE` * 0.5, 
      `Overflow per FTE`
    )
  ) %>% select(
    -(`Overflow per FTE`)
  )
  
  #### End #### 
  
  #### [C] Return list ####
  
  return(list(studentData, stateData, collegeData))
  
  #### End #### 
  
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
    `Eligible` = ifelse(
      `Citizenship`=="Citizen or eligible non-citizen", 
      "Yes", 
      "No"
    )
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
  
  studentData <- studentData %>% mutate(
    `Total loans` = `Federal loan amount` + `Parent loan amount`
  )
  
  if(select1=="100%"){
    studentData <- studentData %>% mutate(
      `Target loans` = rep(0)
    )
  }
  if(select1=="50%"){
    studentData <- studentData %>% mutate(
      `Target loans` = `Total loans` * 0.5
    )
  }
  if(select1=="25%"){
    studentData <- studentData %>% mutate(
      `Target loans` = `Total loans` * 0.25
    )
  }
  
  #### End #### 
  
  #### [C] Calculate average delta ####
  
  studentData <- studentData %>% mutate(
    `Delta` = ifelse(
      `Eligible`=="Yes", 
      pmax(`Total loans` - `Target loans`, 0), # How much cost has to move 
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
  ) %>% mutate(
    `Turn down due to finances` = ifelse(
      `Total state contributions as a share of education appropriations` > maxIncrease, 
      "Yes",  
      "No"
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
  
  #### [C] Store pricing and aid changes ####
  
  studentData <- studentData %>% mutate(
    `New tuition subsidy` = rep(0), 
    `New grants` = ifelse(
      `Participant`=="Yes",
      `Delta`,
      0
    )
  )
  
  #### End #### 
  
  #### [C] Store institutional funding changes ####
  
  stateData <- stateData %>% mutate(
    `Overflow per FTE` = `Overflow amount` / `Eligible FTEs`
  ) 
  
  importOverflow <- stateData %>% select(
    `STABBR`, 
    `Overflow per FTE`
  )
  studentData <- left_join(x=studentData, y=importOverflow, by="STABBR")
  rm(importOverflow)
  
  studentData <- studentData %>% mutate(
    `Overflow per FTE` = ifelse(
      is.na(`Overflow per FTE`), 
      0, 
      `Overflow per FTE`
    )
  ) %>% mutate(
    `Overflow per FTE` = ifelse(
      `Participant`=="Yes", 
      `Overflow per FTE`, 
      0
    )
  ) %>% mutate(
    `Overflow` = ifelse(
      `Enrollment intensity` == "Part-time", 
      `Overflow per FTE` * 0.5, 
      `Overflow per FTE`
    )
  ) %>% select(
    -(`Overflow per FTE`)
  )
  
  #### End #### 
  
  #### [C] Return list ####
  
  return(list(studentData, stateData, collegeData))
  
  #### End #### 
  
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
    `Eligible` = ifelse(
      `Citizenship`=="Citizen or eligible non-citizen", 
      "Yes", 
      "No"
    )
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
  
  if(select1=="10%"){
    studentData <- studentData %>% mutate(
      `Target net price as a share of family income` = rep(0.1)
    )
  }
  if(select1=="20%"){
    studentData <- studentData %>% mutate(
      `Target tuition as a share of family income` = rep(0.2)
    )
  }
  if(select1=="30%"){
    studentData <- studentData %>% mutate(
      `Target tuition as a share of family income` = rep(0.3)
    )
  }
  
  #### End #### 
  
  #### [C] Calculate average delta ####
  
  studentData <- studentData %>% mutate(
    `Net price` = pmax(0, (`Tuition and fees paid` + `Non-tuition expense budget`) - (`Federal grant amount` + `VA/DOD grant amount` + `State grant amount` + `Institutional grant amount` + `Private grant amount`))
  ) %>% mutate(
    `Net price as a share of family income` = `Net price` / `Family income`
  )
  
  studentData <- studentData %>% mutate(
    `Delta` = ifelse(
      `Eligible`=="Yes", 
      pmax((`Net price as a share of family income` - `Target net price as a share of family income`) * `Family income`, 0), # How much cost has to move 
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
  ) %>% mutate(
    `Turn down due to finances` = ifelse(
      `Total state contributions as a share of education appropriations` > maxIncrease, 
      "Yes",  
      "No"
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
  
  #### [C] Store pricing and aid changes ####
  
  studentData <- studentData %>% mutate(
    `New tuition subsidy` = rep(0), 
    `New grants` = ifelse(
      `Participant`=="Yes", 
      `Delta`, 
      0
    )
  )
  
  #### End #### 
  
  #### [C] Store institutional funding changes ####
  
  stateData <- stateData %>% mutate(
    `Overflow per FTE` = `Overflow amount` / `Eligible FTEs`
  ) 
  
  importOverflow <- stateData %>% select(
    `STABBR`, 
    `Overflow per FTE`
  )
  studentData <- left_join(x=studentData, y=importOverflow, by="STABBR")
  rm(importOverflow)
  
  studentData <- studentData %>% mutate(
    `Overflow per FTE` = ifelse(
      is.na(`Overflow per FTE`), 
      0, 
      `Overflow per FTE`
    )
  ) %>% mutate(
    `Overflow per FTE` = ifelse(
      `Participant`=="Yes", 
      `Overflow per FTE`, 
      0
    )
  ) %>% mutate(
    `Overflow` = ifelse(
      `Enrollment intensity` == "Part-time", 
      `Overflow per FTE` * 0.5, 
      `Overflow per FTE`
    )
  ) %>% select(
    -(`Overflow per FTE`)
  )
  
  #### End #### 
  
  #### [C] Return list ####
  
  return(list(studentData, stateData, collegeData))
  
  #### End #### 
  
}

################################################
#### Function E: [Plan E]                   ####
#### Fed-state partnership: Increase        ####
#### federal and state investment to equal  ####
#### X% of revenue                          ####
################################################

################################################
#### Function F: [Plan F]                   ####
#### Fed-college partnership: Government    ####
#### sends colleges subsidy in exchange for ####
#### X pricing policy                       ####
################################################

################################################
#### Function G: [Plan G]                   ####
#### Increase federal grants to students    #### 
#### by X%                                  ####
################################################

################################################
#### Function X: Long-term impacts          ####
################################################

functionX <- function(students1){
  
  #### Increase in degrees from net price ####
  
  students1 <- students1 %>% mutate(
    `New expected certificates` = pmin(1, `Expected certificates` * (1 + ((`New grants` + `New tuition subsidy`) / 5000))),
    `New expected associate's degrees` = pmin(1, `Expected associate's degrees` * (1 + ((`New grants` + `New tuition subsidy`) / 15000))), 
    `New expected bachelor's degrees` = pmin(1, `Expected bachelor's degrees` * (1 + ((`New grants` + `New tuition subsidy`) / 25000))) 
  )
  
  #### End #### 
  
  #### Increase in degrees from overflow ####
  
  students1 <- students1 %>% mutate(
    `New expected certificates` = pmin(1, `New expected certificates` * (1 + (`Overflow` / 5000))),
    `New expected associate's degrees` = pmin(1, `New expected associate's degrees` * (1 + (`Overflow` / 15000))), 
    `New expected bachelor's degrees` = pmin(1, `New expected bachelor's degrees` * (1 + (`Overflow` / 25000))) 
  )
  
  #### End #### 
  
  #### Increase in earnings from attainment ####
  
  students1 <- students1 %>% mutate(
    `Gain: Certificates` = `New expected certificates` - `Expected certificates`, 
    `Gain: Associate's degrees` = `New expected associate's degrees` - `Expected associate's degrees`, 
    `Gain: Bachelor's degrees` = `New expected bachelor's degrees` - `Expected bachelor's degrees`
  ) %>% mutate(
    `Gain: Earnings` = (`Gain: Certificates` * 3000) + (`Gain: Associate's degrees` * 8000) + (`Gain: Bachelor's degrees` * 20000)
  )
  
  #### End #### 
  
  #### Increase in taxes from earnings ####
  
  students1 <- students1 %>% mutate(
    `Increase: Taxes` = `Gain: Earnings` * 0.2
  )
  
  #### End #### 
  
  return(students1)
  
}

################################################
#### Functions 1 to 10: Outputs for app     ####
################################################

#### Function 1: Institutional participation map #### 

function1 <- function(students1, states1, colleges1, plan1){
  
  participantStates <- states1 %>% filter(
    `Participation status`=="Yes"
  )
  
  colleges1 <- colleges1 %>% mutate(
    `Participant` = ifelse(
      (`Eligible`=="Yes") & (`STABBR` %in% participantStates$`STABBR`), 
      "Yes", 
      "No"
    )
  )
  rm(participantStates)
  
  students1 <- students1 %>% mutate(`Count` = rep(1))
  
  participantStudents <- students1 %>% filter(
    `Participant`=="Yes"
  ) %>% group_by(
    `UNITID`
  ) %>% summarize(
    `Total participating students` = sum(`Count`),
    `Total funds received by students` = sum(`New tuition subsidy`) + sum(`New grants`), 
    .groups = "drop"
  )
  colleges1 <- left_join(x=colleges1, y=participantStudents, by="UNITID") %>% mutate(
    `Total participating students` = ifelse(
      is.na(`Total participating students`),
      0, 
      `Total participating students`
    ), 
    `Total funds received by students` = ifelse(
      is.na(`Total funds received by students`),
      0, 
      `Total funds received by students`
    )
  )
  rm(participantStudents)
  
  shareParticipating <- aggregate(
    data=students1, 
    `Count` ~ `UNITID` + `Participant`, 
    FUN=sum
  ) %>% pivot_wider(
    id_cols=c(`UNITID`),
    names_from=`Participant`, 
    values_from=`Count`
  ) %>% mutate(
    `Yes` = ifelse(is.na(`Yes`), 0, `Yes`), 
    `No` = ifelse(is.na(`No`), 0, `No`)
  ) %>% mutate(
    `Share participating` = `Yes` / (`Yes` + `No`)
  ) %>% select(
    `UNITID`,
    `Share participating`
  )
  colleges1 <- left_join(x=colleges1, y=shareParticipating, by="UNITID") %>% mutate(
    `Share participating` = ifelse(
      is.na(`Share participating`), 
      0, 
      `Share participating`
    )
  )
  rm(shareParticipating)
  
  degreesPerYear <- aggregate(
    data=students1,
    cbind(`Gain: Certificates`, `Gain: Associate's degrees`, `Gain: Bachelor's degrees`) ~ `UNITID`,
    FUN=sum
  ) %>% mutate(
    `Increased expected degrees and certificates` = `Gain: Certificates` + `Gain: Associate's degrees` + `Gain: Bachelor's degrees`
  ) %>% select(
    `UNITID`, 
    `Increased expected degrees and certificates`
  )
  colleges1 <- left_join(x=colleges1, y=degreesPerYear, by="UNITID") %>% mutate(
    `Increased expected degrees and certificates` = ifelse(
      is.na(`Increased expected degrees and certificates`), 
      0, 
      `Increased expected degrees and certificates`
    )
  )
  rm(degreesPerYear)
  
  results1 <- colleges1 %>% select(
    `UNITID`,
    `Participant`,
    `Total participating students`, 
    `Share participating`, 
    `Total funds received by students`, 
    `Increased expected degrees and certificates`
  )
  
  return(results1)
}

#### End #### 

#### Function 2: State participation map ####

function2 <- function(students1, states1, colleges1, plan1){
  # Data will not come from Plans F or G
  
  # students1 <- students
  # states1 <- states
  # colleges1 <- colleges
  # plan1 <- "Plan A"
  
  states1 <- states1 %>% select(
    `State`,
    `STABBR`,
    `Participation status`, 
    `Federal block grant`,
    `Total state contributions`, 
    `Total state contributions as a share of education appropriations`
  )
  
  if(plan1 != "Plan E"){
    students1 <- students1 %>% mutate(`Count` = rep(1))
    
    publicStudents <- aggregate(
      data=students1, 
      `Count` ~ `Participant` + `STABBR` + `Control`, 
      FUN=sum
    ) %>% filter(
      `Control` == "Public"
    ) %>% select(
      -(`Control`)
    ) %>% pivot_wider(
      id_cols=c(`STABBR`), 
      names_from=`Participant`,
      values_from=`Count`
    ) %>% mutate(
      `Yes` = ifelse(is.na(`Yes`), 0, `Yes`), 
      `No` = ifelse(is.na(`No`), 0, `No`)
    ) %>% mutate(
      `Share of students at public institutions participating` = `Yes` / (`Yes` + `No`)
    ) %>% select(
      `STABBR`,
      `Share of students at public institutions participating`
    )
    states1 <- left_join(x=states1, y=publicStudents, by="STABBR") %>% mutate(
      `Share of students at public institutions participating` = ifelse(
        is.na(`Share of students at public institutions participating`),
        0, 
        `Share of students at public institutions participating`
      )
    )
    rm(publicStudents)
    
    participantStudents <- aggregate(
      data=students1, 
      `Count` ~ `Participant` + `STABBR`,
      FUN=sum
    ) %>% filter(
      `Participant`=="Yes"
    ) %>% rename(
      `Number of participating students` = `Count`
    )
    results1 <- left_join(x=states1, y=participantStudents, by="STABBR") %>% mutate(
      `Number of participating students` = ifelse(
        is.na(`Number of participating students`),
        0, 
        `Number of participating students`
      )
    )
    rm(participantStudents)
  }
  
  return(results1)
}

#### End ####

#### Function 3: Net price percentiles #### 

function3 <- function(students1, states1, colleges1, plan1){
  
  # students1 <- students
  # states1 <- states
  # colleges1 <- colleges
  # plan1 <- "Plan A"
  
  students1 <- students1 %>% mutate(
    `Pre-policy net price` = pmax(0, (`Tuition and fees paid` + `Non-tuition expense budget`) - (`Federal grant amount` + `VA/DOD grant amount` + `State grant amount` + `Institutional grant amount` + `Private grant amount`))
  ) %>% mutate(
    `Post-policy net price` = pmax(0, `Pre-policy net price` - (`New tuition subsidy` + `New grants`))
  )
  
  suppressWarnings({
    prePolicyNP <- students1 %>% group_by(
      `Participant`
    ) %>% summarize(
      quantile(`Pre-policy net price`, probs = seq(0.1, 0.9, 0.1)), 
      .groups = "drop"
    ) %>% mutate(
      `Percentile` = rep(
        c(10, 20, 30, 40, 50, 60, 70, 80, 90), 
        2
      )
    )
    names(prePolicyNP)[2] <- "Pre-policy net price"
  })
  
  suppressWarnings({
    postPolicyNP <- students1 %>% group_by(
      `Participant`
    ) %>% summarize(
      quantile(`Post-policy net price`, probs = seq(0.1, 0.9, 0.1)), 
      .groups = "drop"
    ) %>% mutate(
      `Percentile` = rep(
        c(10, 20, 30, 40, 50, 60, 70, 80, 90), 
        2
      )
    )
    names(postPolicyNP)[2] <- "Post-policy net price"
  })
  results1 <- full_join(x=prePolicyNP, y=postPolicyNP, by=c("Participant", "Percentile"))
  
  return(results1)
}

#### End #### 

#### Function 4: Educational attainment #### 

function4 <- function(students1, states1, colleges1, plan1){
  
  # students1 <- students
  # states1 <- states
  # colleges1 <- colleges
  # plan1 <- "Plan A"
  
  results1 <- states1 %>% select(
    `State`,
    `STABBR`, 
    `Population 25 years and over`,
    `Population with an associate's degree or higher`, 
    `Population with a bachelor's degree or higher`
  )
  
  newDegrees <- aggregate(
    data=students1, 
    cbind(`Gain: Associate's degrees`, `Gain: Bachelor's degrees`) ~ `STABBR`,
    FUN=sum
  ) 
  results1 <- left_join(x=results1, y=newDegrees, by="STABBR")
  rm(newDegrees)
  
  # Assume 80% of bachelor's degree holders don't already have an associate's. See ReadMe.  
  results1 <- results1 %>% mutate(
    `Associate's or higher: Pre-policy percentage` = `Population with an associate's degree or higher` / `Population 25 years and over`, 
    `Associate's or higher: Post-policy percentage` = (`Population with an associate's degree or higher` + `Gain: Associate's degrees` + (`Gain: Bachelor's degrees` * 0.8)) / `Population 25 years and over`, 
    `Bachelor's or higher: Pre-policy percentage` = `Population with a bachelor's degree or higher` / `Population 25 years and over`, 
    `Bachelor's or higher: Post-policy percentage` = (`Population with a bachelor's degree or higher` + `Gain: Bachelor's degrees`) / `Population 25 years and over`
  ) %>% mutate(
    `Percentage point change, associate's or higher` = `Associate's or higher: Post-policy percentage` - `Associate's or higher: Pre-policy percentage`, 
    `Percentage point change, bachelor's or higher` = `Bachelor's or higher: Post-policy percentage` - `Bachelor's or higher: Pre-policy percentage`
  ) %>% select(
    `State`, 
    `STABBR`,
    `Percentage point change, associate's or higher`, 
    `Percentage point change, bachelor's or higher`
  )
  
  return(results1)
}

#### End #### 

#### Function 5: Economic impact #### 

function5 <- function(students1, states1, colleges1, plan1){
  
  # students1 <- students
  # states1 <- states
  # colleges1 <- colleges
  # plan1 <- "Plan A"
  
  annualCost <- sum(states1$`Total state contributions`, na.rm=TRUE) + sum(states1$`Federal block grant`, na.rm=TRUE)
  annualTax <- sum(students1$`Increase: Taxes`)
  
  results1 <- data.frame(
    `Annual cost` = c(annualCost), 
    `Increase in annual taxes` = c(annualTax), 
    check.names=FALSE
  )
  
  return(results1)
}

#### End #### 

#### Function 6: State funding #### 

function6 <- function(students1, states1, colleges1, plan1){
  # Data will not come from Plans F or G
  
  # students1 <- students
  # states1 <- states
  # colleges1 <- colleges
  # plan1 <- "Plan A"
  
  results1 <- states1 %>% select(
    `State`,
    `STABBR`, 
    `Federal block grant`, 
    `Total state contributions`, 
    `Overflow amount`
  )
  return(results1)
}

#### End #### 

#### Function 7: Student participation #### 

function7 <- function(students1, states1, colleges1, plan1){
  # Data won't come from Plan E
  
  # students1 <- students
  # states1 <- states
  # colleges1 <- colleges
  # plan1 <- "Plan A"
  
  students1 <- students1 %>% mutate(
    `Count` = rep(1)
  ) %>% mutate(
    `Student of color` = ifelse(
      `Race` %in% c(
        "American Indian or Alaska Native",
        "Asian", 
        "Black or African American",
        "Hispanic or Latino", 
        "More than one race", 
        "Native Hawaiian/other Pacific Islander"
      ), 
      "Yes",
      "No"
    )
  )
  
  results1 <- data.frame(
    `Measure` = character(), 
    `Value` = character()
  )
  
  results1 <- results1 %>% add_row(
    `Measure` = "Number of students participating", 
    `Value` = comma(round(sum(students1$`Count`[students1$`Participant`=="Yes"]), -3))
  )
  
  agg1 <- aggregate(
    data=students1, 
    `Count` ~ `Participant`, 
    FUN=sum
  ) %>% pivot_wider(
    names_from=`Participant`, 
    values_from=`Count`
  ) %>% mutate(
    `Yes` = ifelse(is.na(`Yes`), 0, `Yes`), 
    `No` = ifelse(is.na(`No`), 0, `No`)
  ) %>% mutate(
    `Share` = `Yes` / (`Yes` + `No`)
  )
  results1 <- results1 %>% add_row(
    `Measure` = "Share of students participating", 
    `Value` = percent(agg1$`Share`[1], accuracy=0.1)
  )
  rm(agg1)
  
  results1 <- results1 %>% add_row(
    `Measure` = "Number of Pell Grant recipients participating", 
    `Value` = comma(round(sum(students1$`Count`[(students1$`Participant`=="Yes") & (students1$`Receives Pell`=="Yes")]), -3))
  )
  
  agg2 <- aggregate(
    data=students1, 
    `Count` ~ `Participant` + `Receives Pell`, 
    FUN=sum
  ) %>% filter(
    `Receives Pell` == "Yes"
  ) %>% select(
    -(`Receives Pell`)
  ) %>% pivot_wider(
    names_from=`Participant`, 
    values_from=`Count`
  ) %>% mutate(
    `Yes` = ifelse(is.na(`Yes`), 0, `Yes`), 
    `No` = ifelse(is.na(`No`), 0, `No`)
  ) %>% mutate(
    `Share` = `Yes` / (`Yes` + `No`)
  )
  results1 <- results1 %>% add_row(
    `Measure` = "Share of Pell Grant recipients participating", 
    `Value` = percent(agg2$`Share`[1], accuracy=0.1)
  )
  rm(agg2)
  
  results1 <- results1 %>% add_row(
    `Measure` = "Number of first-generation college students participating", 
    `Value` = comma(round(sum(students1$`Count`[(students1$`Participant`=="Yes") & (students1$`First-gen status`=="First-gen")]), -3))
  )
  
  agg3 <- aggregate(
    data=students1, 
    `Count` ~ `Participant` + `First-gen status`, 
    FUN=sum
  ) %>% filter(
    `First-gen status` == "First-gen"
  ) %>% select(
    -(`First-gen status`)
  ) %>% pivot_wider(
    names_from=`Participant`, 
    values_from=`Count`
  ) %>% mutate(
    `Yes` = ifelse(is.na(`Yes`), 0, `Yes`), 
    `No` = ifelse(is.na(`No`), 0, `No`)
  ) %>% mutate(
    `Share` = `Yes` / (`Yes` + `No`)
  )
  results1 <- results1 %>% add_row(
    `Measure` = "Share of first-generation college students participating", 
    `Value` = percent(agg3$`Share`[1], accuracy=0.1)
  )
  rm(agg3)
  
  results1 <- results1 %>% add_row(
    `Measure` = "Number of students of color participating", 
    `Value` = comma(round(sum(students1$`Count`[(students1$`Participant`=="Yes") & (students1$`Student of color`=="Yes")]), -3))
  )
  
  agg4 <- aggregate(
    data=students1, 
    `Count` ~ `Participant` + `Student of color`, 
    FUN=sum
  ) %>% filter(
    `Student of color` == "Yes"
  ) %>% select(
    -(`Student of color`)
  ) %>% pivot_wider(
    names_from=`Participant`, 
    values_from=`Count`
  ) %>% mutate(
    `Yes` = ifelse(is.na(`Yes`), 0, `Yes`), 
    `No` = ifelse(is.na(`No`), 0, `No`)
  ) %>% mutate(
    `Share` = `Yes` / (`Yes` + `No`)
  )
  results1 <- results1 %>% add_row(
    `Measure` = "Share of students of color participating", 
    `Value` = percent(agg4$`Share`[1], accuracy=0.1)
  )
  rm(agg4)
  
  results1 <- results1 %>% add_row(
    `Measure` = "Number of student-parents participating", 
    `Value` = comma(round(sum(students1$`Count`[(students1$`Participant`=="Yes") & (students1$`Parent status`=="Has dependents")]), -3))
  )
  
  agg5 <- aggregate(
    data=students1, 
    `Count` ~ `Participant` + `Parent status`, 
    FUN=sum
  ) %>% filter(
    `Parent status` == "Has dependents"
  ) %>% select(
    -(`Parent status`)
  ) %>% pivot_wider(
    names_from=`Participant`, 
    values_from=`Count`
  ) %>% mutate(
    `Yes` = ifelse(is.na(`Yes`), 0, `Yes`), 
    `No` = ifelse(is.na(`No`), 0, `No`)
  ) %>% mutate(
    `Share` = `Yes` / (`Yes` + `No`)
  )
  results1 <- results1 %>% add_row(
    `Measure` = "Share of student-parents participating", 
    `Value` = percent(agg5$`Share`[1], accuracy=0.1)
  )
  rm(agg5)
  
  results1 <- results1 %>% add_row(
    `Measure` = "Number of student-veterans participating", 
    `Value` = comma(round(sum(students1$`Count`[(students1$`Participant`=="Yes") & (students1$`Veteran status`=="Veteran")]), -3))
  )
  
  agg6 <- aggregate(
    data=students1, 
    `Count` ~ `Participant` + `Veteran status`, 
    FUN=sum
  ) %>% filter(
    `Veteran status` == "Veteran"
  ) %>% select(
    -(`Veteran status`)
  ) %>% pivot_wider(
    names_from=`Participant`, 
    values_from=`Count`
  ) %>% mutate(
    `Yes` = ifelse(is.na(`Yes`), 0, `Yes`), 
    `No` = ifelse(is.na(`No`), 0, `No`)
  ) %>% mutate(
    `Share` = `Yes` / (`Yes` + `No`)
  )
  results1 <- results1 %>% add_row(
    `Measure` = "Share of student-veterans participating", 
    `Value` = percent(agg6$`Share`[1], accuracy=0.1)
  )
  rm(agg6)
  
  return(results1)
}

#### End #### 

#### Function 8: Student debt #### 

function8 <- function(students1, states1, colleges1, plan1){
  # Data won't come from Plan E
  
  # students1 <- students
  # states1 <- states
  # colleges1 <- colleges
  # plan1 <- "Plan A"
  
  students1 <- students1 %>% mutate(
    `Pre-policy student loans` = `Federal loan amount` + `Parent loan amount`
  ) %>% mutate(
    `Post-policy student loans` = pmax(`Pre-policy student loans` - (`New grants` + `New tuition subsidy`), 0) 
  ) %>% mutate(
    `Pre-policy borrower status` = ifelse(`Pre-policy student loans` > 0, "Borrower", "Not a borrower"),
    `Post-policy borrower status` = ifelse(`Post-policy student loans` > 0, "Borrower", "Not a borrower") 
  )
  deltaDebt <- sum(students1$`Pre-policy student loans`) - sum(students1$`Post-policy student loans`)
  shareDebt <- deltaDebt / sum(students1$`Pre-policy student loans`)
  deltaBorrowers <- sum(students1$`Pre-policy borrower status`=="Borrower") - sum(students1$`Post-policy borrower status`=="Borrower")
  shareBorrowers <- deltaBorrowers / sum(students1$`Pre-policy borrower status`=="Borrower")
  
  results1 <- data.frame(
    `Measure` = character(), 
    `Value` = character()
  )
  
  results1 <- results1 %>% add_row(
    `Measure` = "Reduction in annual student loans", 
    `Value` = dollar(round(deltaDebt, -6))
  ) %>% add_row(
    `Measure` = "Percentage reduction in annual student loans", 
    `Value` = percent(shareDebt, accuracy=0.1)
  ) %>% add_row(
    `Measure` = "Reduction in number of students borrowing any loans", 
    `Value` = comma(round(deltaBorrowers, -3))
  ) %>% add_row(
    `Measure` = "Percentage reduction in number of students borrowing any loans", 
    `Value` = percent(shareBorrowers, accuracy=0.1)
  )
  rm(deltaDebt, shareDebt, deltaBorrowers, shareBorrowers)
  
  return(results1)
}

#### End #### 

#### Function 9: Degrees and certificates #### 

function9 <- function(students1, states1, colleges1, plan1){
  
  # students1 <- students
  # states1 <- states
  # colleges1 <- colleges
  # plan1 <- "Plan A"
  
  results1 <- data.frame(
    `Measure` = character(), 
    `Value` = character()
  ) %>% add_row(
    `Measure` = "Estimated new certificates created", 
    `Value` = comma(round(sum(students1$`Gain: Certificates`), -2))
  ) %>% add_row(
    `Measure` = "Estimated new associate's degrees created", 
    `Value` = comma(round(sum(students1$`Gain: Associate's degrees`), -2))
  ) %>% add_row(
    `Measure` = "Estimated new bachelor's degrees created", 
    `Value` = comma(round(sum(students1$`Gain: Bachelor's degrees`), -2))
  )
  
  return(results1)
}

#### End #### 

#### Function 10: Government cost #### 

function10 <- function(students1, states1, colleges1, plan1){
  
  # students1 <- students
  # states1 <- states
  # colleges1 <- colleges
  # plan1 <- "Plan A"
  
  # Pell and TEACH: $37.9 billion, per FSA Data Center
  # Campus programs: $2.1 billion, per FSA Data Center
  # MSI funding: $1.3 billion, per https://www.congress.gov/crs-product/R43237
  totalFederalSpending <- (37.9 + 2.1 + 1.3) * 1000000000
  
  federalMoney <- sum(states1$`Federal block grant`[states1$`Participation status`=="Yes"], na.rm=TRUE)
  federalIncrease <- federalMoney / totalFederalSpending
  
  stateMoney <- sum(states1$`Total state contributions`[states1$`Participation status`=="Yes"], na.rm=TRUE)
  stateIncrease <- stateMoney / sum(states1$`Education Appropriations`, na.rm=TRUE)
  
  stateTurnDown <- sum(states1$`Turn down due to finances`=="Yes", na.rm=TRUE)
  
  results1 <- data.frame(
    `Measure` = character(), 
    `Value` = character()
  ) %>% add_row(
    `Measure` = "Total annual cost to federal government", 
    `Value` = dollar(round(federalMoney, -6))
  ) %>% add_row(
    `Measure` = "Percentage increase in annual federal spending on higher education (excl. research grants and student loans)", 
    `Value` = percent(federalIncrease, accuracy=0.1)
  ) %>% add_row(
    `Measure` = "Total annual cost to state governments", 
    `Value` = dollar(round(stateMoney, -6))
  ) %>% add_row(
    `Measure` = "Percentage increase in annual state education appropriations", 
    `Value` = percent(stateIncrease, accuracy=0.1)
  ) %>% add_row(
    `Measure` = "Number of states that decline to participate due to costs", 
    `Value` = comma(stateTurnDown)
  ) 
  
  return(results1)
}

#### End #### 

################################################
#### Set choice lists                       ####
################################################

#### Choice list 1 ####

choices1a <- c(
  "$0 at all eligible institutions"
  # , 
  # "$1,000 at all eligible institutions", DEMO
  # "$1,000 at eligible two-year institutions and $3,000 at eligible four-year institutions" DEMO
)

choices1b <- c(
  "100%"
  # , 
  # "50%", DEMO
  # "25%" DEMO
)

choices1c <- choices1b

choices1d <- c(
  "10%"
  # , 
  # "20%", DEMO
  # "30%" DEMO
)

choices1e <- c(
  "65%"
  , 
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
  # "$0.10", SLIM
  "$0.25"
  # , DEMO
  # "$0.50", SLIM
  # "$1.00" DEMO
)

choices2b <- choices2a

choices2c <- choices2a

choices2d <- choices2a

choices2e <- c(
  "$0.50"
  , 
  # "$1.00", SLIM
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
  "No restriction based on enrollment intensity"
  # , 
  # "Restricted to students enrolled full-time" DEMO
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
  "No means testing"
  # , 
  # "Pell Grant recipients only" DEMO
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
  "Yes"
  # , 
  # "No" DEMO
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
  "Only two-year institutions"
  # , 
  # "Both two- and four-year institutions" SLIM
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
  "No"
  # , 
  # "Yes" SLIM
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
  # "15% and above", SLIM 
  # "25% and above", SLIM
  "35% and above"
)

choices9b <- choices9a

choices9c <- choices9a

choices9d <- choices9a

choices9e <- choices9a

choices9f <- c(
  # "5% and above", SLIM
  "10% and above"
  # , 
  # "15% and above" SLIM
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

                  print(Sys.time())
                  counter <- counter + 1
                  
                  #### Run simulations #### 
                  
                  tempAll <- functionA(
                    studentDF, 
                    stateDF, 
                    collegeDF, 
                    r, s, t, u, v, w, x, y, z
                  )
                  
                  students0 <- functionX(tempAll[[1]])
                  states0 <- tempAll[[2]]
                  colleges0 <- tempAll[[3]]
                  rm(tempAll)
                  
                  temp1 <- function1(students0, states0, colleges0, "Model A")
                  temp2 <- function2(students0, states0, colleges0, "Model A")
                  temp3 <- function3(students0, states0, colleges0, "Model A")
                  temp4 <- function4(students0, states0, colleges0, "Model A")
                  temp5 <- function5(students0, states0, colleges0, "Model A")
                  temp6 <- function6(students0, states0, colleges0, "Model A")
                  temp7 <- function7(students0, states0, colleges0, "Model A")
                  temp8 <- function8(students0, states0, colleges0, "Model A")
                  temp9 <- function9(students0, states0, colleges0, "Model A")
                  temp10 <- function10(students0, states0, colleges0, "Model A")
                  rm(students0, states0, colleges0)
                  
                  #### End #### 
                  
                  #### Bind output ####
                  
                  if(counter==1){
                    output1 <- specs(temp1)
                    output2 <- specs(temp2)
                    output3 <- specs(temp3)
                    output4 <- specs(temp4)
                    output5 <- specs(temp5)
                    output6 <- specs(temp6)
                    output7 <- specs(temp7)
                    output8 <- specs(temp8)
                    output9 <- specs(temp9)
                    output10 <- specs(temp10)
                  }else{
                    output1 <- rbind(output1, specs(temp1))
                    output2 <- rbind(output2, specs(temp2))
                    output3 <- rbind(output3, specs(temp3))
                    output4 <- rbind(output4, specs(temp4))
                    output5 <- rbind(output5, specs(temp5))
                    output6 <- rbind(output6, specs(temp6))
                    output7 <- rbind(output7, specs(temp7))
                    output8 <- rbind(output8, specs(temp8))
                    output9 <- rbind(output9, specs(temp9))
                    output10 <- rbind(output10, specs(temp10))
                  }
                  rm(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10)
                  
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
                  
                  print(Sys.time())
                  counter <- counter + 1
                  
                  #### Run simulations #### 
                  
                  tempAll <- functionB(
                    studentDF, 
                    stateDF, 
                    collegeDF, 
                    r, s, t, u, v, w, x, y, z
                  )
                  
                  students0 <- functionX(tempAll[[1]])
                  states0 <- tempAll[[2]]
                  colleges0 <- tempAll[[3]]
                  rm(tempAll)
                  
                  temp1 <- function1(students0, states0, colleges0, "Model B")
                  temp2 <- function2(students0, states0, colleges0, "Model B")
                  temp3 <- function3(students0, states0, colleges0, "Model B")
                  temp4 <- function4(students0, states0, colleges0, "Model B")
                  temp5 <- function5(students0, states0, colleges0, "Model B")
                  temp6 <- function6(students0, states0, colleges0, "Model B")
                  temp7 <- function7(students0, states0, colleges0, "Model B")
                  temp8 <- function8(students0, states0, colleges0, "Model B")
                  temp9 <- function9(students0, states0, colleges0, "Model B")
                  temp10 <- function10(students0, states0, colleges0, "Model B")
                  rm(students0, states0, colleges0)
                  
                  #### End #### 
                  
                  #### Bind output ####
                  
                  if(counter==1){
                    output1 <- specs(temp1)
                    output2 <- specs(temp2)
                    output3 <- specs(temp3)
                    output4 <- specs(temp4)
                    output5 <- specs(temp5)
                    output6 <- specs(temp6)
                    output7 <- specs(temp7)
                    output8 <- specs(temp8)
                    output9 <- specs(temp9)
                    output10 <- specs(temp10)
                  }else{
                    output1 <- rbind(output1, specs(temp1))
                    output2 <- rbind(output2, specs(temp2))
                    output3 <- rbind(output3, specs(temp3))
                    output4 <- rbind(output4, specs(temp4))
                    output5 <- rbind(output5, specs(temp5))
                    output6 <- rbind(output6, specs(temp6))
                    output7 <- rbind(output7, specs(temp7))
                    output8 <- rbind(output8, specs(temp8))
                    output9 <- rbind(output9, specs(temp9))
                    output10 <- rbind(output10, specs(temp10))
                  }
                  rm(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10)
                  
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
                  
                  print(Sys.time())
                  counter <- counter + 1
                  
                  #### Run simulations #### 
                  
                  tempAll <- functionC(
                    studentDF, 
                    stateDF, 
                    collegeDF, 
                    r, s, t, u, v, w, x, y, z
                  )
                  
                  students0 <- functionX(tempAll[[1]])
                  states0 <- tempAll[[2]]
                  colleges0 <- tempAll[[3]]
                  rm(tempAll)
                  
                  temp1 <- function1(students0, states0, colleges0, "Model C")
                  temp2 <- function2(students0, states0, colleges0, "Model C")
                  temp3 <- function3(students0, states0, colleges0, "Model C")
                  temp4 <- function4(students0, states0, colleges0, "Model C")
                  temp5 <- function5(students0, states0, colleges0, "Model C")
                  temp6 <- function6(students0, states0, colleges0, "Model C")
                  temp7 <- function7(students0, states0, colleges0, "Model C")
                  temp8 <- function8(students0, states0, colleges0, "Model C")
                  temp9 <- function9(students0, states0, colleges0, "Model C")
                  temp10 <- function10(students0, states0, colleges0, "Model C")
                  rm(students0, states0, colleges0)
                  
                  #### End #### 
                  
                  #### Bind output ####
                  
                  if(counter==1){
                    output1 <- specs(temp1)
                    output2 <- specs(temp2)
                    output3 <- specs(temp3)
                    output4 <- specs(temp4)
                    output5 <- specs(temp5)
                    output6 <- specs(temp6)
                    output7 <- specs(temp7)
                    output8 <- specs(temp8)
                    output9 <- specs(temp9)
                    output10 <- specs(temp10)
                  }else{
                    output1 <- rbind(output1, specs(temp1))
                    output2 <- rbind(output2, specs(temp2))
                    output3 <- rbind(output3, specs(temp3))
                    output4 <- rbind(output4, specs(temp4))
                    output5 <- rbind(output5, specs(temp5))
                    output6 <- rbind(output6, specs(temp6))
                    output7 <- rbind(output7, specs(temp7))
                    output8 <- rbind(output8, specs(temp8))
                    output9 <- rbind(output9, specs(temp9))
                    output10 <- rbind(output10, specs(temp10))
                  }
                  rm(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10)
                  
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
                  
                  print(Sys.time())
                  counter <- counter + 1
                  
                  #### Run simulations #### 
                  
                  tempAll <- functionD(
                    studentDF, 
                    stateDF, 
                    collegeDF, 
                    r, s, t, u, v, w, x, y, z
                  )
                  
                  students0 <- functionX(tempAll[[1]])
                  states0 <- tempAll[[2]]
                  colleges0 <- tempAll[[3]]
                  rm(tempAll)
                  
                  temp1 <- function1(students0, states0, colleges0, "Model D")
                  temp2 <- function2(students0, states0, colleges0, "Model D")
                  temp3 <- function3(students0, states0, colleges0, "Model D")
                  temp4 <- function4(students0, states0, colleges0, "Model D")
                  temp5 <- function5(students0, states0, colleges0, "Model D")
                  temp6 <- function6(students0, states0, colleges0, "Model D")
                  temp7 <- function7(students0, states0, colleges0, "Model D")
                  temp8 <- function8(students0, states0, colleges0, "Model D")
                  temp9 <- function9(students0, states0, colleges0, "Model D")
                  temp10 <- function10(students0, states0, colleges0, "Model D")
                  rm(students0, states0, colleges0)
                  
                  #### End #### 
                  
                  #### Bind output ####
                  
                  if(counter==1){
                    output1 <- specs(temp1)
                    output2 <- specs(temp2)
                    output3 <- specs(temp3)
                    output4 <- specs(temp4)
                    output5 <- specs(temp5)
                    output6 <- specs(temp6)
                    output7 <- specs(temp7)
                    output8 <- specs(temp8)
                    output9 <- specs(temp9)
                    output10 <- specs(temp10)
                  }else{
                    output1 <- rbind(output1, specs(temp1))
                    output2 <- rbind(output2, specs(temp2))
                    output3 <- rbind(output3, specs(temp3))
                    output4 <- rbind(output4, specs(temp4))
                    output5 <- rbind(output5, specs(temp5))
                    output6 <- rbind(output6, specs(temp6))
                    output7 <- rbind(output7, specs(temp7))
                    output8 <- rbind(output8, specs(temp8))
                    output9 <- rbind(output9, specs(temp9))
                    output10 <- rbind(output10, specs(temp10))
                  }
                  rm(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10)
                  
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

# counter <- 0
# for(r in choices1e){
#   for(s in choices2e){
#     for(t in choices3e){
#       for(u in choices4e){
#         for(v in choices5e){
#           for(w in choices6e){
#             for(x in choices7e){
#               for(y in choices8e){
#                 for(z in choices9e){
#                   
#                   counter <- counter + 1
#                   
#                   #### Run simulations #### 
#                   #### End #### 
#                   
#                   #### Bind output ####
#                   
#                   if(counter==1){
#                     output1 <- specs(temp1)
#                     output2 <- specs(temp2)
#                     # output3 <- specs(temp3)
#                     output4 <- specs(temp4)
#                     output5 <- specs(temp5)
#                     output6 <- specs(temp6)
#                     # output7 <- specs(temp7)
#                     # output8 <- specs(temp8)
#                     output9 <- specs(temp9)
#                     output10 <- specs(temp10)
#                   }else{
#                     output1 <- rbind(output1, specs(temp1))
#                     output2 <- rbind(output2, specs(temp2))
#                     # output3 <- rbind(output3, specs(temp3))
#                     output4 <- rbind(output4, specs(temp4))
#                     output5 <- rbind(output5, specs(temp5))
#                     output6 <- rbind(output6, specs(temp6))
#                     # output7 <- rbind(output7, specs(temp7))
#                     # output8 <- rbind(output8, specs(temp8))
#                     output9 <- rbind(output9, specs(temp9))
#                     output10 <- rbind(output10, specs(temp10))
#                   }
#                   rm(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10)
#                   
#                   #### End #### 
#                   
# }}}}}}}}}

#### Write File E #### 

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Model-V2/Simulation results")

sheetList <- list(
  Sheet1 = output1,
  Sheet2 = output2,
  # Sheet3 = output3,
  Sheet4 = output4,
  Sheet5 = output5,
  Sheet6 = output6,
  # Sheet7 = output7,
  # Sheet8 = output8,
  Sheet9 = output9,
  Sheet10 = output10
)

write_xlsx(sheetList, "Plan E.xlsx")
rm(sheetList, output1, output2, 
   # output3, 
   output4, output5, output6, 
   # output7, 
   # output8, 
   output9, output10, counter)
rm(choices1e, choices2e, choices3e, choices4e, choices5e, choices6e, choices7e, choices8e, choices9e)

#### End #### 

################################################
#### Write File F                           ####
################################################

# counter <- 0
# for(r in choices1f){
#   for(s in choices2f){
#     for(t in choices3f){
#       for(u in choices4f){
#         for(v in choices5f){
#           for(w in choices6f){
#             for(x in choices7f){
#               for(y in choices8f){
#                 for(z in choices9f){
#                   
#                   counter <- counter + 1
#                   
#                   #### Run simulations #### 
#                   #### End #### 
#                   
#                   #### Bind output ####
#                   
#                   if(counter==1){
#                     output1 <- specs(temp1)
#                     # output2 <- specs(temp2)
#                     output3 <- specs(temp3)
#                     output4 <- specs(temp4)
#                     output5 <- specs(temp5)
#                     # output6 <- specs(temp6)
#                     output7 <- specs(temp7)
#                     output8 <- specs(temp8)
#                     output9 <- specs(temp9)
#                     output10 <- specs(temp10)
#                   }else{
#                     output1 <- rbind(output1, specs(temp1))
#                     # output2 <- rbind(output2, specs(temp2))
#                     output3 <- rbind(output3, specs(temp3))
#                     output4 <- rbind(output4, specs(temp4))
#                     output5 <- rbind(output5, specs(temp5))
#                     # output6 <- rbind(output6, specs(temp6))
#                     output7 <- rbind(output7, specs(temp7))
#                     output8 <- rbind(output8, specs(temp8))
#                     output9 <- rbind(output9, specs(temp9))
#                     output10 <- rbind(output10, specs(temp10))
#                   }
#                   rm(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10)
#                   
#                   #### End #### 
#                   
# }}}}}}}}}

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

# counter <- 0
# for(r in choices1g){
#   for(s in choices2g){
#     for(t in choices3g){
#       for(u in choices4g){
#         for(v in choices5g){
#           for(w in choices6g){
#             for(x in choices7g){
#               for(y in choices8g){
#                 for(z in choices9g){
#                   
#                   counter <- counter + 1
#                   
#                   #### Run simulations #### 
#                   
#                   temp1 <- data.frame(`Hello` = c("Hello"))
#                   temp2 <- data.frame(`Hello` = c("Hello"))
#                   temp3 <- data.frame(`Hello` = c("Hello"))
#                   temp4 <- data.frame(`Hello` = c("Hello"))
#                   temp5 <- data.frame(`Hello` = c("Hello"))
#                   temp6 <- data.frame(`Hello` = c("Hello"))
#                   temp7 <- data.frame(`Hello` = c("Hello"))
#                   temp8 <- data.frame(`Hello` = c("Hello"))
#                   temp9 <- data.frame(`Hello` = c("Hello"))
#                   temp10 <- data.frame(`Hello` = c("Hello"))
#                   
#                   #### End #### 
#                   
#                   #### Bind output ####
#                   
#                   if(counter==1){
#                     output1 <- specs(temp1)
#                     # output2 <- specs(temp2)
#                     output3 <- specs(temp3)
#                     output4 <- specs(temp4)
#                     output5 <- specs(temp5)
#                     # output6 <- specs(temp6)
#                     output7 <- specs(temp7)
#                     output8 <- specs(temp8)
#                     output9 <- specs(temp9)
#                     output10 <- specs(temp10)
#                   }else{
#                     output1 <- rbind(output1, specs(temp1))
#                     # output2 <- rbind(output2, specs(temp2))
#                     output3 <- rbind(output3, specs(temp3))
#                     output4 <- rbind(output4, specs(temp4))
#                     output5 <- rbind(output5, specs(temp5))
#                     # output6 <- rbind(output6, specs(temp6))
#                     output7 <- rbind(output7, specs(temp7))
#                     output8 <- rbind(output8, specs(temp8))
#                     output9 <- rbind(output9, specs(temp9))
#                     output10 <- rbind(output10, specs(temp10))
#                   }
#                   rm(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10)
#                   
#                   #### End #### 
#                   
# }}}}}}}}}

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




