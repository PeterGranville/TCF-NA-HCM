
#### Setup ####

library(scales)
library(tidyverse)

#### End #### 

#############################################
#### Load IPEDS student enrollment data  ####
#############################################

#### Load IPEDS data ####

setwd("/Users/peter_granville/Fed State Modeling/IPEDS data")

hd <- read.csv("hd2023.csv", header=TRUE) %>% select(
  `UNITID`, 
  `INSTNM`,
  `CONTROL`,
  `STABBR`, 
  `OBEREG`,
  `C18BASIC`
)

ic <- read.csv("ic2023.csv", header=TRUE) %>% select(
  `UNITID`, 
  `TUITVARY`
) 

# Question for later: do we care about degree-seeking status?
effy <- read.csv("effy2023.csv", header=TRUE) %>% select(
  `UNITID`,
  `EFFYALEV`,
  `EFYTOTLT`,
  `EFYTOTLM`,
  `EFYTOTLW`,
  `EFYAIANT`,
  `EFYAIANM`,
  `EFYAIANW`,
  `EFYASIAT`,
  `EFYASIAM`,
  `EFYASIAW`,
  `EFYBKAAT`,
  `EFYBKAAM`,
  `EFYBKAAW`,
  `EFYHISPT`,
  `EFYHISPM`,
  `EFYHISPW`,
  `EFYNHPIT`,
  `EFYNHPIM`,
  `EFYNHPIW`,
  `EFYWHITT`,
  `EFYWHITM`,
  `EFYWHITW`,
  `EFY2MORT`,
  `EFY2MORM`,
  `EFY2MORW`,
  `EFYUNKNT`,
  `EFYUNKNM`,
  `EFYUNKNW`,
  `EFYNRALT`,
  `EFYNRALM`,
  `EFYNRALW`,
  `EFYGUUN`,
  `EFYGUAN`,
  `EFYGUTOT`,
  `EFYGUKN`
) %>% filter(
  `EFFYALEV` %in% c(
    22, # Full-time students, Undergraduate total
    42  # Part-time students, Undergraduate total
  )
) %>% pivot_longer(
  cols=c(`EFYTOTLT`,
         `EFYTOTLM`,
         `EFYTOTLW`,
         `EFYAIANT`,
         `EFYAIANM`,
         `EFYAIANW`,
         `EFYASIAT`,
         `EFYASIAM`,
         `EFYASIAW`,
         `EFYBKAAT`,
         `EFYBKAAM`,
         `EFYBKAAW`,
         `EFYHISPT`,
         `EFYHISPM`,
         `EFYHISPW`,
         `EFYNHPIT`,
         `EFYNHPIM`,
         `EFYNHPIW`,
         `EFYWHITT`,
         `EFYWHITM`,
         `EFYWHITW`,
         `EFY2MORT`,
         `EFY2MORM`,
         `EFY2MORW`,
         `EFYUNKNT`,
         `EFYUNKNM`,
         `EFYUNKNW`,
         `EFYNRALT`,
         `EFYNRALM`,
         `EFYNRALW`,
         `EFYGUUN`,
         `EFYGUAN`,
         `EFYGUTOT`,
         `EFYGUKN`), 
  names_to="Total name", 
  values_to="Student count"
)

effyLabs <- data.frame(
  `Total name` = character(), 
  `Race` = character(), 
  `Gender` = character(), 
  check.names=FALSE
) %>% add_row(
  `Total name`="EFYTOTLT", `Race`="Total", `Gender`="Total"
) %>% add_row(
  `Total name`="EFYTOTLM", `Race`="Total", `Gender`="Male"
) %>% add_row(
  `Total name`="EFYTOTLW", `Race`="Total", `Gender`="Female"
) %>% add_row(
  `Total name`="EFYAIANT", `Race`="American Indian or Alaska Native", `Gender`="Total"
) %>% add_row(
  `Total name`="EFYAIANM", `Race`="American Indian or Alaska Native", `Gender`="Male"
) %>% add_row(
  `Total name`="EFYAIANW", `Race`="American Indian or Alaska Native", `Gender`="Female"
) %>% add_row(
  `Total name`="EFYASIAT", `Race`="Asian", `Gender`="Total"
) %>% add_row(
  `Total name`="EFYASIAM", `Race`="Asian", `Gender`="Male"
) %>% add_row(
  `Total name`="EFYASIAW", `Race`="Asian", `Gender`="Female"
) %>% add_row(
  `Total name`="EFYBKAAT", `Race`="Black or African American", `Gender`="Total"
) %>% add_row(
  `Total name`="EFYBKAAM", `Race`="Black or African American", `Gender`="Male"
) %>% add_row(
  `Total name`="EFYBKAAW", `Race`="Black or African American", `Gender`="Female"
) %>% add_row(
  `Total name`="EFYHISPT", `Race`="Hispanic or Latino", `Gender`="Total"
) %>% add_row(
  `Total name`="EFYHISPM", `Race`="Hispanic or Latino", `Gender`="Male"
) %>% add_row(
  `Total name`="EFYHISPW", `Race`="Hispanic or Latino", `Gender`="Female"
) %>% add_row(
  `Total name`="EFYNHPIT", `Race`="Native Hawaiian/other Pacific Islander", `Gender`="Total"
) %>% add_row(
  `Total name`="EFYNHPIM", `Race`="Native Hawaiian/other Pacific Islander", `Gender`="Male"
) %>% add_row(
  `Total name`="EFYNHPIW", `Race`="Native Hawaiian/other Pacific Islander", `Gender`="Female"
) %>% add_row(
  `Total name`="EFYWHITT", `Race`="White", `Gender`="Total"
) %>% add_row(
  `Total name`="EFYWHITM", `Race`="White", `Gender`="Male"
) %>% add_row(
  `Total name`="EFYWHITW", `Race`="White", `Gender`="Female"
) %>% add_row(
  `Total name`="EFY2MORT", `Race`="Two or more races", `Gender`="Total"
) %>% add_row(
  `Total name`="EFY2MORM", `Race`="Two or more races", `Gender`="Male"
) %>% add_row(
  `Total name`="EFY2MORW", `Race`="Two or more races", `Gender`="Female"
) %>% add_row(
  `Total name`="EFYUNKNT", `Race`="Race/ethnicity unknown", `Gender`="Total"
) %>% add_row(
  `Total name`="EFYUNKNM", `Race`="Race/ethnicity unknown", `Gender`="Male"
) %>% add_row(
  `Total name`="EFYUNKNW", `Race`="Race/ethnicity unknown", `Gender`="Female"
) %>% add_row(
  `Total name`="EFYNRALT", `Race`="U.S. Nonresident", `Gender`="Total"
) %>% add_row(
  `Total name`="EFYNRALM", `Race`="U.S. Nonresident", `Gender`="Male"
) %>% add_row(
  `Total name`="EFYNRALW", `Race`="U.S. Nonresident", `Gender`="Female"
) %>% add_row(
  `Total name`="EFYGUUN", `Race`="Total", `Gender`="Gender unknown"
) %>% add_row(
  `Total name`="EFYGUAN", `Race`="Total", `Gender`="Another gender"
) %>% add_row(
  `Total name`="EFYGUTOT", `Race`="Total", `Gender`="Total of gender unknown and another gender"
) %>% add_row(
  `Total name`="EFYGUKN", `Race`="Total", `Gender`="Total gender reported as one of the mutually exclusive binary categories (Men/Women)"
)

effy <- left_join(x=effy, y=effyLabs, by="Total name")
rm(effyLabs)
effy <- effy %>% mutate(
  `Enrollment intensity` = ifelse(
    `EFFYALEV`==22, "Full-time", "Part-time"
  )
)

# I've confirmed that there is no additional information gained from these categories: 
effy <- effy %>% filter(
  (`Total name` %in% c(
    "EFYGUAN", "EFYGUKN", "EFYGUTOT", "EFYGUUN", "EFYTOTLT", "EFYTOTLM", "EFYTOTLW"
  ))==FALSE
)

# We also do not want Male + Female totals, since they asre duplicative. 
effy <- effy %>% filter(
  (`Total name` %in% c(
    "EFY2MORT", "EFYAIANT", "EFYASIAT", "EFYBKAAT", "EFYHISPT", "EFYNHPIT", "EFYNRALT", "EFYUNKNT", "EFYWHITT"
  ))==FALSE
)

#### End #### 

#### Create student level dataset ####

# Index effy
effy <- effy %>% mutate(
  `Index` = (1:nrow(effy))
)

# Create student level dataset: 
for(i in (1:2000)){ # Starting small: 
# for(i in (1:nrow(effy))){
  
  if(i %% 100 == 0){
    print(paste("Trying number ", comma(i), " out of ", comma(nrow(effy)), ".", sep=""))
  }
  
  tempEFFY <- effy %>% filter(`Index`==i)

  if(tempEFFY$`Student count`[1] > 0){
    tempSTU <- data.frame(
      `Effy index` = rep(tempEFFY$`Index`[1], tempEFFY$`Student count`[1]), 
      `Student index` = (1:tempEFFY$`Student count`[1]),
      `UNITID` = rep(tempEFFY$`UNITID`[1], tempEFFY$`Student count`[1]), 
      `Race` = rep(tempEFFY$`Race`[1], tempEFFY$`Student count`[1]), 
      `Gender` = rep(tempEFFY$`Gender`[1], tempEFFY$`Student count`[1]), 
      `Enrollment intensity` = rep(tempEFFY$`Enrollment intensity`[1], tempEFFY$`Student count`[1]), 
      check.names=FALSE
    ) %>% mutate(
      `Effy-student index` = paste(`Effy index`, ":", `Student index`, sep="")
    )
  }else{
    tempSTU <- data.frame(
      `Effy index` = numeric(), 
      `Student index` = numeric(),
      `UNITID` = numeric(), 
      `Race` = character(), 
      `Gender` = character(), 
      `Enrollment intensity` = character(), 
      `Effy-student index` = character(),
      check.names=FALSE
    ) 
  }
  
  if(i==1){
    studentList <- tempSTU
  }else{
    studentList <- rbind(studentList, tempSTU)
  }
  
  rm(tempSTU, tempEFFY)
  
}
rm(i, effy)

#### End #### 

#### Import institutional information #### 

hdControl <- data.frame(
  `CONTROL` = numeric(), 
  `Control` = character()
) %>% add_row(
  `CONTROL` = 1, `Control` = "Public"
) %>% add_row(
  `CONTROL` = 2, `Control` = "Private nonprofit"
) %>% add_row(
  `CONTROL` = 3, `Control` = "Private for-profit"
)
hd <- left_join(x=hd, y=hdControl, by="CONTROL") %>% select(-(`CONTROL`))
rm(hdControl)

hdRegion <- data.frame(
  `OBEREG` = numeric(), 
  `Region` = character()
) %>% add_row(
  `OBEREG` = 1, `Region` = "New England (CT ME MA NH RI VT)"
) %>% add_row(
  `OBEREG` = 2, `Region` = "Mideast (DE DC MD NJ NY PA)"
) %>% add_row(
  `OBEREG` = 3, `Region` = "Great Lakes (IL IN MI OH WI)"
) %>% add_row(
  `OBEREG` = 4, `Region` = "Plains (IA KS MN MO NE ND SD)"
) %>% add_row(
  `OBEREG` = 5, `Region` = "South East (AL AR FL GA KY LA MS NC SC TN VA WV)"
) %>% add_row(
  `OBEREG` = 6, `Region` = "Southwest (AZ NM OK TX)"
) %>% add_row(
  `OBEREG` = 7, `Region` = "Rocky Mountains (CO ID MT UT WY)"
) %>% add_row(
  `OBEREG` = 8, `Region` = "Far West (AK CA HI NV OR WA)"
) %>% add_row(
  `OBEREG` = 9, `Region` = "Other U.S. jurisdictions"
) 
hd <- left_join(x=hd, y=hdRegion, by="OBEREG") %>% select(-(`OBEREG`))
rm(hdRegion)

studentList <- left_join(x=studentList, y=hd, by="UNITID")
rm(hd)

icTuition <- data.frame(
  `TUITVARY` = numeric(),
  `Tuition policy` = character(), 
  check.names=FALSE
) %>% add_row(
  `TUITVARY` = 1, 
  `Tuition policy` = "Varies tuition by in-state status"
) %>% add_row(
  `TUITVARY` = 2, 
  `Tuition policy` = "Does not vary tuition by in-state status"
) %>% add_row(
  `TUITVARY` = -1, 
  `Tuition policy` = "Does not vary tuition by in-state status" # Imputing for "not reported"
) %>% add_row(
  `TUITVARY` = -2, 
  `Tuition policy` = "Does not vary tuition by in-state status" # Imputing for "not applicable"
)
ic <- left_join(x=ic, y=icTuition, by="TUITVARY") %>% select(-(`TUITVARY`))
rm(icTuition)

studentList <- left_join(x=studentList, y=ic, by="UNITID")
rm(ic)

#### End #### 

#### Prep dataset for regressions ####

studentList <- studentList %>% mutate(
  `Enrollment intensity NPSAS` = ifelse(`Enrollment intensity`=="Full-time", "Exclusively full-time", "Exclusively part-time")
) %>% mutate(
  `Region NPSAS` = ifelse(`Region`=="Other U.S. jurisdictions", "Puerto Rico", `Region`)
) 

carnegieNPSAS <- data.frame(
  `C18BASIC` = c(-2, (1:33)), 
  `Carnegie NPSAS` = rep(NA, 34), 
  check.names=FALSE
) %>% mutate(
  `Carnegie NPSAS` = ifelse(`C18BASIC` %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 14), "Associate's", `Carnegie NPSAS`)
) %>% mutate(
  `Carnegie NPSAS` = ifelse(`C18BASIC` %in% c(15, 16, 17), "Research & Doctoral", `Carnegie NPSAS`)
) %>% mutate(
  `Carnegie NPSAS` = ifelse(`C18BASIC` %in% c(18, 19, 20), "Master's", `Carnegie NPSAS`)
) %>% mutate(
  `Carnegie NPSAS` = ifelse(`C18BASIC` %in% c(21, 22, 23), "Baccalaureate", `Carnegie NPSAS`)
) %>% mutate(
  `Carnegie NPSAS` = ifelse(`C18BASIC` %in% c(10, 11, 12, 13, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33), "Special Focus & other", `Carnegie NPSAS`)
) %>% mutate(
  `Carnegie NPSAS` = ifelse(`C18BASIC` %in% c(-2), "Not degree-granting", `Carnegie NPSAS`)
) 

studentList <- left_join(x=studentList, y=carnegieNPSAS, by="C18BASIC")
rm(carnegieNPSAS)

#### End #### 

#############################################
#### Make predictions from regressions   ####
#############################################

processRegression <- function(
  
  #### List of inputs #### 
  
  studentListDF,   
  newVariableName, 
  retrievalCode,
  interceptRow, 
  regressionType, 
  positiveClass, 
  negativeClass, 

  includeVar1, 
  startLine1, 
  endLine1, 
  linkingVar1, 
  varType1, 
  
  includeVar2, 
  startLine2, 
  endLine2, 
  linkingVar2,
  varType2, 
  
  includeVar3, 
  startLine3, 
  endLine3, 
  linkingVar3,
  varType3, 
  
  includeVar4, 
  startLine4, 
  endLine4, 
  linkingVar4,
  varType4, 
  
  includeVar5, 
  startLine5, 
  endLine5, 
  linkingVar5,
  varType5, 
  
  includeVar6, 
  startLine6, 
  endLine6, 
  linkingVar6,
  varType6, 
  
  includeVar7,
  startLine7, 
  endLine7, 
  linkingVar7,
  varType7, 
  
  includeVar8,
  startLine8, 
  endLine8, 
  linkingVar8,
  varType8,
  
  includeVar9,
  startLine9, 
  endLine9, 
  linkingVar9,
  varType9
  
  #### End #### 
  
){
  
  #### Presets for testing: Linear regression #### 

#   studentListDF <-  studentList
#   newVariableName <-  "EFC"
#   retrievalCode <- "gkcfkv"
#   interceptRow <-  17
#   regressionType <- "Linear"
#   positiveClass <- ""
#   negativeClass <- ""
# 
#   includeVar1 <- TRUE
#   startLine1 <-  20
#   endLine1 <-  21
#   linkingVar1 <-  "Control"
#   varType1 <-  "Categorical"
# 
#   includeVar2 <-  TRUE
#   startLine2 <-  24
#   endLine2 <-  31
#   linkingVar2 <- "Region NPSAS"
#   varType2 <-  "Categorical"
# 
#   includeVar3 <-  TRUE
#   startLine3 <-  34
#   endLine3 <-  39
#   linkingVar3 <- "Race"
#   varType3 <-  "Categorical"
# 
#   includeVar4 <-  TRUE
#   startLine4 <-  42
#   endLine4 <-  46
#   linkingVar4 <- "Carnegie NPSAS"
#   varType4 <-  "Categorical"
# 
#   includeVar5 <-  TRUE
#   startLine5 <-  53
#   endLine5 <-  53
#   linkingVar5 <- "Gender"
#   varType5 <-  "Categorical"
# 
#   includeVar6 <-  FALSE
#   startLine6 <-  ""
#   endLine6 <-  ""
#   linkingVar6 <- ""
#   varType6 <-  ""
# 
#   includeVar7 <- FALSE
#   startLine7 <-  ""
#   endLine7 <-  ""
#   linkingVar7 <- ""
#   varType7 <-  ""
# 
#   includeVar8 <- FALSE
#   startLine8 <-  ""
#   endLine8 <-  ""
#   linkingVar8 <- ""
#   varType8 <- ""
# 
#   includeVar9 <- FALSE
#   startLine9 <-  ""
#   endLine9 <-  ""
#   linkingVar9 <- ""
#   varType9 <- ""

  #### End #### 
  
  #### Obtain intercept #### 
  
  setwd("/Users/peter_granville/Fed State Modeling/Datalab outputs")
  
  interceptDF <- read.csv(
    paste("PowerStats_", retrievalCode, ".csv", sep=""), 
    header=FALSE, 
    skip=interceptRow - 1, 
    nrows=1
  ) %>% select(
    `V1`, `V2`, `V3`
  ) %>% rename(
    `Group` = `V1`, 
    `Coefficient` = `V2`, 
    `Standard Error` = `V3`
  )
  studentListDF <- studentListDF %>% mutate(
    `Intercept` = rep(interceptDF$`Coefficient`[1]), 
    `Intercept SE` = rep(interceptDF$`Standard Error`[1])
  )
  rm(interceptDF)
  
  #### End #### 
  
  for(i in (1:9)){
    
    #### Assign includeVar, startLine, endLine, linkingVar, varType #### 
    
    if(i==1){
      includeVar <- includeVar1 
      startLine <- startLine1 
      endLine <- endLine1 
      linkingVar <- linkingVar1
      varType <- varType1
    }
    if(i==2){
      includeVar <- includeVar2
      startLine <- startLine2
      endLine <- endLine2
      linkingVar <- linkingVar2
      varType <- varType2
    }
    if(i==3){
      includeVar <- includeVar3
      startLine <- startLine3
      endLine <- endLine3
      linkingVar <- linkingVar3
      varType <- varType3
    }
    if(i==4){
      includeVar <- includeVar4
      startLine <- startLine4
      endLine <- endLine4
      linkingVar <- linkingVar4
      varType <- varType4
    }
    if(i==5){
      includeVar <- includeVar5
      startLine <- startLine5
      endLine <- endLine5
      linkingVar <- linkingVar5
      varType <- varType5
    }
    if(i==6){
      includeVar <- includeVar6
      startLine <- startLine6
      endLine <- endLine6
      linkingVar <- linkingVar6
      varType <- varType6
    }
    if(i==7){
      includeVar <- includeVar7
      startLine <- startLine7
      endLine <- endLine7
      linkingVar <- linkingVar7
      varType <- varType7
    }
    if(i==8){
      includeVar <- includeVar8
      startLine <- startLine8
      endLine <- endLine8
      linkingVar <- linkingVar8
      varType <- varType8
    }
    if(i==9){
      includeVar <- includeVar9
      startLine <- startLine9
      endLine <- endLine9
      linkingVar <- linkingVar9
      varType <- varType9
    }
    
    #### End #### 
    
    if(includeVar==TRUE){
      
      #### Load tempDF ####
      
      if(varType=="Categorical"){
        tempDF <- read.csv(
          paste("PowerStats_", retrievalCode, ".csv", sep=""), 
          header=FALSE, 
          skip=startLine - 1, 
          nrows=endLine - startLine + 1
        ) %>% select(
          `V1`, `V2`, `V3`
        ) %>% mutate(
          `V1` = gsub("  ", "", `V1`)
        ) %>% rename(
          `Group` = `V1`, 
          `Coefficient` = `V2`, 
          `Standard Error` = `V3`
        )
      }else{
        tempDF <- read.csv(
          paste("PowerStats_", retrievalCode, ".csv", sep=""), 
          header=FALSE, 
          skip=startLine - 1, 
          nrows=1
        ) %>% select(
          `V1`, `V2`, `V3`
        ) %>% mutate(
          `V1` = gsub("  ", "", `V1`)
        ) %>% rename(
          `Group` = `V1`, 
          `Coefficient` = `V2`, 
          `Standard Error` = `V3`
        )
      }
      
      #### End #### 
      
      #### Obtain reference var ####
      
      if(varType=="Categorical"){
        
        # Control
        if(grepl("nonprofit", tempDF$`Group`[1])){
          tempDF <- tempDF %>% add_row(
            `Group` = "Public", 
            `Coefficient` = 0, 
            `Standard Error` = median(tempDF$`Standard Error`, na.rm=TRUE)
          )
        }
        
        # Region
        if(grepl("Mideast", tempDF$`Group`[1])){
          tempDF <- tempDF %>% add_row(
            `Group` = "New England (CT ME MA NH RI VT)", 
            `Coefficient` = 0, 
            `Standard Error` = median(tempDF$`Standard Error`, na.rm=TRUE)
          )
        }
        
        # Race
        if(grepl("African American", tempDF$`Group`[1])){
          tempDF <- tempDF %>% add_row(
            `Group` = "White", 
            `Coefficient` = 0, 
            `Standard Error` = median(tempDF$`Standard Error`, na.rm=TRUE)
          )
        }
        
        # Carnegie Classification
        if(grepl("Research & Doctoral", tempDF$`Group`[1])){
          tempDF <- tempDF %>% add_row(
            `Group` = "Associate's", 
            `Coefficient` = 0, 
            `Standard Error` = median(tempDF$`Standard Error`, na.rm=TRUE)
          )
        }
        
        # Enrollment intensity
        if(grepl("part-time", tempDF$`Group`[1])){
          tempDF <- tempDF %>% add_row(
            `Group` = "Exclusively full-time", 
            `Coefficient` = 0, 
            `Standard Error` = median(tempDF$`Standard Error`, na.rm=TRUE)
          )
        }
        
        # Gender
        if(grepl("Female", tempDF$`Group`[1])){
          tempDF <- tempDF %>% add_row(
            `Group` = "Male", 
            `Coefficient` = 0, 
            `Standard Error` = median(tempDF$`Standard Error`, na.rm=TRUE)
          )
        }
        
        # In-state status
        if(grepl("Out-of-state", tempDF$`Group`[1])){
          tempDF <- tempDF %>% add_row(
            `Group` = "In-state tuition", 
            `Coefficient` = 0, 
            `Standard Error` = median(tempDF$`Standard Error`, na.rm=TRUE)
          )
        }
        
      }
      
      #### End #### 
      
      #### Import into dataset ####
      
      names(tempDF)[1] <- linkingVar
      names(tempDF)[2] <- paste("Variable ", i, " Coefficient", sep="")
      names(tempDF)[3] <- paste("Variable ", i, " Standard Error", sep="")
      
      studentListDF <- left_join(x=studentListDF, y=tempDF, by=linkingVar)
      rm(tempDF)
      
      #### End #### 
      
    }
  }
  
  #### Fill out remaining info ####
  
  coefficientCount <- sum(includeVar1, includeVar2, includeVar3, includeVar4, includeVar5, includeVar6, includeVar7, includeVar8, includeVar9)
  
  if(coefficientCount < 6){
    studentListDF <- studentListDF %>% mutate(
      `Variable 6 Coefficient` = rep(0), 
      `Variable 6 Standard Error` = rep(NA)
    )
  }
  if(coefficientCount < 7){
    studentListDF <- studentListDF %>% mutate(
      `Variable 7 Coefficient` = rep(0), 
      `Variable 7 Standard Error` = rep(NA)
    )
  }
  if(coefficientCount < 8){
    studentListDF <- studentListDF %>% mutate(
      `Variable 8 Coefficient` = rep(0), 
      `Variable 8 Standard Error` = rep(NA)
    )
  }
  if(coefficientCount < 9){
    studentListDF <- studentListDF %>% mutate(
      `Variable 9 Coefficient` = rep(0), 
      `Variable 9 Standard Error` = rep(NA)
    )
  }
  
  #### End #### 
  
  #### Create components #### 
  
  if(varType1=="Numeric"){
    tempDF <- studentListDF %>% select(
      `Effy-student index`, 
      `Variable 1 Coefficient`, 
      all_of(linkingVar1)
    )
    names(tempDF)[3] <- "Variable 1 Multiplier"
    tempDF <- tempDF %>% mutate(
      `Variable 1 Component` = `Variable 1 Coefficient` * `Variable 1 Multiplier`
    ) %>% select(
      `Effy-student index`, 
      `Variable 1 Component`
    )
    studentListDF <- left_join(x=studentListDF, y=tempDF, by="Effy-student index")
    rm(tempDF)
  }else{
    studentListDF <- studentListDF %>% mutate(
      `Variable 1 Component` = `Variable 1 Coefficient`
    )
  }
  
  if(varType2=="Numeric"){
    tempDF <- studentListDF %>% select(
      `Effy-student index`, 
      `Variable 2 Coefficient`, 
      all_of(linkingVar2)
    )
    names(tempDF)[3] <- "Variable 2 Multiplier"
    tempDF <- tempDF %>% mutate(
      `Variable 2 Component` = `Variable 2 Coefficient` * `Variable 2 Multiplier`
    ) %>% select(
      `Effy-student index`, 
      `Variable 2 Component`
    )
    studentListDF <- left_join(x=studentListDF, y=tempDF, by="Effy-student index")
    rm(tempDF)
  }else{
    studentListDF <- studentListDF %>% mutate(
      `Variable 2 Component` = `Variable 2 Coefficient`
    )
  }
  
  if(varType3=="Numeric"){
    tempDF <- studentListDF %>% select(
      `Effy-student index`, 
      `Variable 3 Coefficient`, 
      all_of(linkingVar3)
    )
    names(tempDF)[3] <- "Variable 3 Multiplier"
    tempDF <- tempDF %>% mutate(
      `Variable 3 Component` = `Variable 3 Coefficient` * `Variable 3 Multiplier`
    ) %>% select(
      `Effy-student index`, 
      `Variable 3 Component`
    )
    studentListDF <- left_join(x=studentListDF, y=tempDF, by="Effy-student index")
    rm(tempDF)
  }else{
    studentListDF <- studentListDF %>% mutate(
      `Variable 3 Component` = `Variable 3 Coefficient`
    )
  }
  
  if(varType4=="Numeric"){
    tempDF <- studentListDF %>% select(
      `Effy-student index`, 
      `Variable 4 Coefficient`, 
      all_of(linkingVar4)
    )
    names(tempDF)[3] <- "Variable 4 Multiplier"
    tempDF <- tempDF %>% mutate(
      `Variable 4 Component` = `Variable 4 Coefficient` * `Variable 4 Multiplier`
    ) %>% select(
      `Effy-student index`, 
      `Variable 4 Component`
    )
    studentListDF <- left_join(x=studentListDF, y=tempDF, by="Effy-student index")
    rm(tempDF)
  }else{
    studentListDF <- studentListDF %>% mutate(
      `Variable 4 Component` = `Variable 4 Coefficient`
    )
  }
  
  if(varType5=="Numeric"){
    tempDF <- studentListDF %>% select(
      `Effy-student index`, 
      `Variable 5 Coefficient`, 
      all_of(linkingVar5)
    )
    names(tempDF)[3] <- "Variable 5 Multiplier"
    tempDF <- tempDF %>% mutate(
      `Variable 5 Component` = `Variable 5 Coefficient` * `Variable 5 Multiplier`
    ) %>% select(
      `Effy-student index`, 
      `Variable 5 Component`
    )
    studentListDF <- left_join(x=studentListDF, y=tempDF, by="Effy-student index")
    rm(tempDF)
  }else{
    studentListDF <- studentListDF %>% mutate(
      `Variable 5 Component` = `Variable 5 Coefficient`
    )
  }
  
  if(varType6=="Numeric"){
    tempDF <- studentListDF %>% select(
      `Effy-student index`, 
      `Variable 6 Coefficient`, 
      all_of(linkingVar6)
    )
    names(tempDF)[3] <- "Variable 6 Multiplier"
    tempDF <- tempDF %>% mutate(
      `Variable 6 Component` = `Variable 6 Coefficient` * `Variable 6 Multiplier`
    ) %>% select(
      `Effy-student index`, 
      `Variable 6 Component`
    )
    studentListDF <- left_join(x=studentListDF, y=tempDF, by="Effy-student index")
    rm(tempDF)
  }else{
    studentListDF <- studentListDF %>% mutate(
      `Variable 6 Component` = `Variable 6 Coefficient`
    )
  }
  
  if(varType7=="Numeric"){
    tempDF <- studentListDF %>% select(
      `Effy-student index`, 
      `Variable 7 Coefficient`, 
      all_of(linkingVar7)
    )
    names(tempDF)[3] <- "Variable 7 Multiplier"
    tempDF <- tempDF %>% mutate(
      `Variable 7 Component` = `Variable 7 Coefficient` * `Variable 7 Multiplier`
    ) %>% select(
      `Effy-student index`, 
      `Variable 7 Component`
    )
    studentListDF <- left_join(x=studentListDF, y=tempDF, by="Effy-student index")
    rm(tempDF)
  }else{
    studentListDF <- studentListDF %>% mutate(
      `Variable 7 Component` = `Variable 7 Coefficient`
    )
  }
  
  if(varType8=="Numeric"){
    tempDF <- studentListDF %>% select(
      `Effy-student index`, 
      `Variable 8 Coefficient`, 
      all_of(linkingVar8)
    )
    names(tempDF)[3] <- "Variable 8 Multiplier"
    tempDF <- tempDF %>% mutate(
      `Variable 8 Component` = `Variable 8 Coefficient` * `Variable 8 Multiplier`
    ) %>% select(
      `Effy-student index`, 
      `Variable 8 Component`
    )
    studentListDF <- left_join(x=studentListDF, y=tempDF, by="Effy-student index")
    rm(tempDF)
  }else{
    studentListDF <- studentListDF %>% mutate(
      `Variable 8 Component` = `Variable 8 Coefficient`
    )
  }
  
  if(varType9=="Numeric"){
    tempDF <- studentListDF %>% select(
      `Effy-student index`, 
      `Variable 9 Coefficient`, 
      all_of(linkingVar9)
    )
    names(tempDF)[3] <- "Variable 9 Multiplier"
    tempDF <- tempDF %>% mutate(
      `Variable 9 Component` = `Variable 9 Coefficient` * `Variable 9 Multiplier`
    ) %>% select(
      `Effy-student index`, 
      `Variable 9 Component`
    )
    studentListDF <- left_join(x=studentListDF, y=tempDF, by="Effy-student index")
    rm(tempDF)
  }else{
    studentListDF <- studentListDF %>% mutate(
      `Variable 9 Component` = `Variable 9 Coefficient`
    )
  }
  
  #### End #### 

  #### Run predictions #### 
  
  studentListDF <- studentListDF %>% mutate(
    `New variable` = `Intercept` + `Variable 1 Component` + `Variable 2 Component` + `Variable 3 Component` + `Variable 4 Component` + `Variable 5 Component` + `Variable 6 Component` + `Variable 7 Component` + `Variable 8 Component` + `Variable 9 Component`
  )
  if(regressionType=="Logistic"){
    studentListDF <- studentListDF %>% mutate(
      `New variable` = 2.718282^(`New variable`) / (1 + (2.718282^(`New variable`)))
    )
    studentListDF <- studentListDF %>% mutate(
      `New variable` = ifelse(`New variable` >= 0.5, posiiveClass, negativeClass)
    )
  }
  names(studentListDF)[ncol(studentListDF)] <- newVariableName
  
  studentListDF <- studentListDF %>% select(
    -(`Intercept`), 
    -(`Variable 1 Coefficient`), -(`Variable 1 Standard Error`), -(`Variable 1 Component`),
    -(`Variable 2 Coefficient`), -(`Variable 2 Standard Error`), -(`Variable 2 Component`),
    -(`Variable 3 Coefficient`), -(`Variable 3 Standard Error`), -(`Variable 3 Component`),
    -(`Variable 4 Coefficient`), -(`Variable 4 Standard Error`), -(`Variable 4 Component`),
    -(`Variable 5 Coefficient`), -(`Variable 5 Standard Error`), -(`Variable 5 Component`),
    -(`Variable 6 Coefficient`), -(`Variable 6 Standard Error`), -(`Variable 6 Component`),
    -(`Variable 7 Coefficient`), -(`Variable 7 Standard Error`), -(`Variable 7 Component`),
    -(`Variable 8 Coefficient`), -(`Variable 8 Standard Error`), -(`Variable 8 Component`),
    -(`Variable 9 Coefficient`), -(`Variable 9 Standard Error`), -(`Variable 9 Component`)
  )
  
  #### End #### 

  return(studentListDF)
  
}

#### End #### 

#### Regression 1: Predict EFC ####
#### End #### 









##############################################
#### Student list using single-var pulls  ####
##############################################

#### Establish empty dataframe ####

distInfoWide <- data.frame(
  `Table Source Code` = character(), 
  `Target Name` = character(), 
  `Target Type` = character(), 
  `Group Name` = character(), 
  `Group Value` = character(), 
  `Class 1 Name` = character(), 
  `Class 1 Share` = character(), 
  `Class 2 Name` = character(), 
  `Class 2 Share` = character(), 
  `Class 3 Name` = character(), 
  `Class 3 Share` = character(), 
  `Class 4 Name` = character(), 
  `Class 4 Share` = character(), 
  `Class 5 Name` = character(), 
  `Class 5 Share` = character(), 
  `Class 6 Name` = character(), 
  `Class 6 Share` = character(), 
  `Class 7 Name` = character(), 
  `Class 7 Share` = character(), 
  `Class 8 Name` = character(), 
  `Class 8 Share` = character(), 
  `Class 9 Name` = character(), 
  `Class 9 Share` = character(), 
  `Class 10 Name` = character(), 
  `Class 10 Share` = character(), 
  `Class 11 Name` = character(), 
  `Class 11 Share` = character(), 
  `Class 12 Name` = character(), 
  `Class 12 Share` = character(), 
  `Class 13 Name` = character(), 
  `Class 13 Share` = character(), 
  `Class 14 Name` = character(), 
  `Class 14 Share` = character(), 
  check.names=FALSE
)

#### End #### 

#### Write function to read data per data block (inner) ####

readCombo1block <- function(data0, filename, targetName, targetType, groupName, startDataRow, endDataRow, classRow){
  
  sourceCode <- filename
  sourceCode <- gsub("PowerStats_", "", sourceCode)
  sourceCode <- gsub(".csv", "", sourceCode)
  
  nSkip <- startDataRow - 1
  nRows <- endDataRow - startDataRow + 1
  data1 <- read.csv(filename, header=FALSE, nrows=nRows, skip=nSkip, check.names=FALSE)
  names(data1)[ncol(data1)] <- "Total column"
  data1 <- data1 %>% select(-(`Total column`))
  names(data1)[1] <- "Group Value"
  rm(nSkip, nRows)
  
  header1 <- read.csv(filename, header=FALSE, nrows=1, skip=classRow-1)
  header1 <- as.character(as.vector(header1[1, ]))
  header1 <- header1[(1:length(header1)-1)] # Why does 2 not work, but 3 does?
  
  if("V2" %in% names(data1)){
    data1 <- data1 %>% mutate(
      `Class 1 Name` = rep(header1[2])
    ) %>% rename(
      `Class 1 Share` = `V2`
    )
  }else{data1 <- data1 %>% mutate(`Class 1 Name` = rep(NA), `Class 1 Share` = rep(NA))}
  
  if("V3" %in% names(data1)){
    data1 <- data1 %>% mutate(
      `Class 2 Name` = rep(header1[3])
    ) %>% rename(
      `Class 2 Share` = `V3`
    )
  }else{data1 <- data1 %>% mutate(`Class 2 Name` = rep(NA), `Class 2 Share` = rep(NA))}
  
  if("V4" %in% names(data1)){
    data1 <- data1 %>% mutate(
      `Class 3 Name` = rep(header1[4])
    ) %>% rename(
      `Class 3 Share` = `V4`
    )
  }else{data1 <- data1 %>% mutate(`Class 3 Name` = rep(NA), `Class 3 Share` = rep(NA))}
  
  if("V5" %in% names(data1)){
    data1 <- data1 %>% mutate(
      `Class 4 Name` = rep(header1[5])
    ) %>% rename(
      `Class 4 Share` = `V5`
    )
  }else{data1 <- data1 %>% mutate(`Class 4 Name` = rep(NA), `Class 4 Share` = rep(NA))}
  
  if("V6" %in% names(data1)){
    data1 <- data1 %>% mutate(
      `Class 5 Name` = rep(header1[6])
    ) %>% rename(
      `Class 5 Share` = `V6`
    )
  }else{data1 <- data1 %>% mutate(`Class 5 Name` = rep(NA), `Class 5 Share` = rep(NA))}
  
  if("V7" %in% names(data1)){
    data1 <- data1 %>% mutate(
      `Class 6 Name` = rep(header1[7])
    ) %>% rename(
      `Class 6 Share` = `V7`
    )
  }else{data1 <- data1 %>% mutate(`Class 6 Name` = rep(NA), `Class 6 Share` = rep(NA))}
  
  if("V8" %in% names(data1)){
    data1 <- data1 %>% mutate(
      `Class 7 Name` = rep(header1[8])
    ) %>% rename(
      `Class 7 Share` = `V8`
    )
  }else{data1 <- data1 %>% mutate(`Class 7 Name` = rep(NA), `Class 7 Share` = rep(NA))}
  
  if("V9" %in% names(data1)){
    data1 <- data1 %>% mutate(
      `Class 8 Name` = rep(header1[9])
    ) %>% rename(
      `Class 8 Share` = `V9`
    )
  }else{data1 <- data1 %>% mutate(`Class 8 Name` = rep(NA), `Class 8 Share` = rep(NA))}
  
  if("V10" %in% names(data1)){
    data1 <- data1 %>% mutate(
      `Class 9 Name` = rep(header1[10])
    ) %>% rename(
      `Class 9 Share` = `V10`
    )
  }else{data1 <- data1 %>% mutate(`Class 9 Name` = rep(NA), `Class 9 Share` = rep(NA))}
  
  if("V11" %in% names(data1)){
    data1 <- data1 %>% mutate(
      `Class 10 Name` = rep(header1[11])
    ) %>% rename(
      `Class 10 Share` = `V11`
    )
  }else{data1 <- data1 %>% mutate(`Class 10 Name` = rep(NA), `Class 10 Share` = rep(NA))}
  
  if("V12" %in% names(data1)){
    data1 <- data1 %>% mutate(
      `Class 11 Name` = rep(header1[12])
    ) %>% rename(
      `Class 11 Share` = `V12`
    )
  }else{data1 <- data1 %>% mutate(`Class 11 Name` = rep(NA), `Class 11 Share` = rep(NA))}
  
  if("V13" %in% names(data1)){
    data1 <- data1 %>% mutate(
      `Class 12 Name` = rep(header1[13])
    ) %>% rename(
      `Class 12 Share` = `V13`
    )
  }else{data1 <- data1 %>% mutate(`Class 12 Name` = rep(NA), `Class 12 Share` = rep(NA))}
  
  if("V14" %in% names(data1)){
    data1 <- data1 %>% mutate(
      `Class 13 Name` = rep(header1[14])
    ) %>% rename(
      `Class 13 Share` = `V14`
    )
  }else{data1 <- data1 %>% mutate(`Class 13 Name` = rep(NA), `Class 13 Share` = rep(NA))}
  
  if("V15" %in% names(data1)){
    data1 <- data1 %>% mutate(
      `Class 14 Name` = rep(header1[15])
    ) %>% rename(
      `Class 14 Share` = `V15`
    )
  }else{data1 <- data1 %>% mutate(`Class 14 Name` = rep(NA), `Class 14 Share` = rep(NA))}
  
  rm(header1)
  
  data1 <- data1 %>% mutate(
    `Table Source Code` = rep(sourceCode), 
    `Target Name` = rep(targetName), 
    `Target Type` = rep(targetType), 
    `Group Name` = rep(groupName)
  )
  
  data1 <- data1 %>% select(
    `Table Source Code`, 
    `Target Name`, 
    `Target Type`, 
    `Group Name`, 
    `Group Value`, 
    `Class 1 Name`, 
    `Class 1 Share`, 
    `Class 2 Name`, 
    `Class 2 Share`, 
    `Class 3 Name`, 
    `Class 3 Share`, 
    `Class 4 Name`, 
    `Class 4 Share`, 
    `Class 5 Name`, 
    `Class 5 Share`, 
    `Class 6 Name`, 
    `Class 6 Share`, 
    `Class 7 Name`, 
    `Class 7 Share`, 
    `Class 8 Name`, 
    `Class 8 Share`, 
    `Class 9 Name`, 
    `Class 9 Share`, 
    `Class 10 Name`, 
    `Class 10 Share`, 
    `Class 11 Name`, 
    `Class 11 Share`, 
    `Class 12 Name`, 
    `Class 12 Share`, 
    `Class 13 Name`, 
    `Class 13 Share`, 
    `Class 14 Name`, 
    `Class 14 Share` 
  )
  
  data0 <- rbind(data0, data1)
  return(data0)
  rm(data1, sourceCode)
  
}

#### End #### 

#### Write function to read data per data file (outer) ####

readCombo1file <- function(data0outer, filenameOuter, targetNameOuter, targetTypeOuter){
  
  # Institution sector 
  data0outer <- readCombo1block(data0 = data0outer, 
                                filename = filenameOuter, 
                                targetName = targetNameOuter, 
                                targetType = targetTypeOuter, 
                                groupName = "Institution sector", 
                                startDataRow = 13, 
                                endDataRow = 21, 
                                classRow = 7
  )
  
  # Institution region
  data0outer <- readCombo1block(data0 = data0outer, 
                                filename = filenameOuter, 
                                targetName = targetNameOuter, 
                                targetType = targetTypeOuter, 
                                groupName = "Institution region", 
                                startDataRow = 24, 
                                endDataRow = 32, 
                                classRow = 7
  )
  
  # Student race
  data0outer <- readCombo1block(data0 = data0outer, 
                                filename = filenameOuter, 
                                targetName = targetNameOuter, 
                                targetType = targetTypeOuter, 
                                groupName = "Student race", 
                                startDataRow = 35, 
                                endDataRow = 41, 
                                classRow = 7
  )
  
  # Institution size
  data0outer <- readCombo1block(data0 = data0outer, 
                                filename = filenameOuter, 
                                targetName = targetNameOuter, 
                                targetType = targetTypeOuter, 
                                groupName = "Institution size", 
                                startDataRow = 44, 
                                endDataRow = 49, 
                                classRow = 7
  )
  
  # Student attendance intensity 
  data0outer <- readCombo1block(data0 = data0outer, 
                                filename = filenameOuter, 
                                targetName = targetNameOuter, 
                                targetType = targetTypeOuter, 
                                groupName = "Student attendance intensity", 
                                startDataRow = 52, 
                                endDataRow = 54, 
                                classRow = 7
  )
  
  # Student gender
  data0outer <- readCombo1block(data0 = data0outer, 
                                filename = filenameOuter, 
                                targetName = targetNameOuter, 
                                targetType = targetTypeOuter, 
                                groupName = "Student gender", 
                                startDataRow = 57, 
                                endDataRow = 58, 
                                classRow = 7
  )
  
  # Student EFC
  data0outer <- readCombo1block(data0 = data0outer, 
                                filename = filenameOuter, 
                                targetName = targetNameOuter, 
                                targetType = targetTypeOuter, 
                                groupName = "Student EFC", 
                                startDataRow = 61, 
                                endDataRow = 67, 
                                classRow = 7
  )
  
  return(data0outer)
  
}

#### End #### 

#### Run data #### 

setwd("/Users/peter_granville/Fed State Modeling/Combo1")

# BUDGETAJ
distInfoWide <- readCombo1file(data0outer = distInfoWide, 
                               filenameOuter = "PowerStats_ecygyd.csv", 
                               targetNameOuter = "Student budget", 
                               targetTypeOuter = "Numeric"
)

# TUITION2
distInfoWide <- readCombo1file(data0outer = distInfoWide, 
                               filenameOuter = "PowerStats_wdslqr.csv", 
                               targetNameOuter = "Tuition and fees paid", 
                               targetTypeOuter = "Numeric"
)

# TFEDGRT
distInfoWide <- readCombo1file(data0outer = distInfoWide, 
                               filenameOuter = "PowerStats_xndjqa.csv", 
                               targetNameOuter = "Federal grants", 
                               targetTypeOuter = "Numeric"
)

# VADODAMT
distInfoWide <- readCombo1file(data0outer = distInfoWide, 
                               filenameOuter = "PowerStats_xerval.csv", 
                               targetNameOuter = "Federal veterans' benefits", 
                               targetTypeOuter = "Numeric"
)

# STGTAMT
distInfoWide <- readCombo1file(data0outer = distInfoWide, 
                               filenameOuter = "PowerStats_ouoyum.csv", 
                               targetNameOuter = "State grants", 
                               targetTypeOuter = "Numeric"
)

# INGRTAMT
distInfoWide <- readCombo1file(data0outer = distInfoWide, 
                               filenameOuter = "PowerStats_mgrfas.csv", 
                               targetNameOuter = "Institution grants", 
                               targetTypeOuter = "Numeric"
)

# PRIVAID
distInfoWide <- readCombo1file(data0outer = distInfoWide, 
                               filenameOuter = "PowerStats_ufvsze.csv", 
                               targetNameOuter = "Private grants", 
                               targetTypeOuter = "Numeric"
)

# TFEDLN
distInfoWide <- readCombo1file(data0outer = distInfoWide, 
                               filenameOuter = "PowerStats_jhcovg.csv", 
                               targetNameOuter = "Federal loans", 
                               targetTypeOuter = "Numeric"
)

# PLUSAMT
distInfoWide <- readCombo1file(data0outer = distInfoWide, 
                               filenameOuter = "PowerStats_vgwakh.csv", 
                               targetNameOuter = "Parent PLUS loans", 
                               targetTypeOuter = "Numeric"
)

# AGE
distInfoWide <- readCombo1file(data0outer = distInfoWide, 
                               filenameOuter = "PowerStats_exloou.csv", 
                               targetNameOuter = "Student age", 
                               targetTypeOuter = "Numeric"
)

# DEPANY
distInfoWide <- readCombo1file(data0outer = distInfoWide, 
                               filenameOuter = "PowerStats_iduzqw.csv", 
                               targetNameOuter = "Parent status", 
                               targetTypeOuter = "Categorical"
)

# CITIZEN2
distInfoWide <- readCombo1file(data0outer = distInfoWide, 
                               filenameOuter = "PowerStats_mztdud.csv", 
                               targetNameOuter = "Citizenship", 
                               targetTypeOuter = "Categorical"
)

# MAJORS12
distInfoWide <- readCombo1file(data0outer = distInfoWide, 
                               filenameOuter = "PowerStats_bywzye.csv", 
                               targetNameOuter = "Field of study", 
                               targetTypeOuter = "Categorical"
)

# PAREDUC_AC
distInfoWide <- readCombo1file(data0outer = distInfoWide, 
                               filenameOuter = "PowerStats_vpyhcz.csv", 
                               targetNameOuter = "Parents' highest education level", 
                               targetTypeOuter = "Categorical"
)

# DEPEND
distInfoWide <- readCombo1file(data0outer = distInfoWide, 
                               filenameOuter = "PowerStats_jjsvoo.csv", 
                               targetNameOuter = "Dependency status", 
                               targetTypeOuter = "Categorical"
)

# FEDAPP
distInfoWide <- readCombo1file(data0outer = distInfoWide, 
                               filenameOuter = "PowerStats_svlbcg.csv", 
                               targetNameOuter = "FAFSA status", 
                               targetTypeOuter = "Categorical"
)

# VETERAN
distInfoWide <- readCombo1file(data0outer = distInfoWide, 
                               filenameOuter = "PowerStats_cfmduq.csv", 
                               targetNameOuter = "Veteran status", 
                               targetTypeOuter = "Categorical"
)

# HSGPA2
distInfoWide <- readCombo1file(data0outer = distInfoWide, 
                               filenameOuter = "PowerStats_vlnhld.csv", 
                               targetNameOuter = "High school GPA", 
                               targetTypeOuter = "Categorical"
)

# EFC
distInfoWide <- readCombo1file(data0outer = distInfoWide, 
                               filenameOuter = "PowerStats_zxifrf.csv", 
                               targetNameOuter = "EFC", 
                               targetTypeOuter = "Numeric"
)

#### End #### 

#### Handle problem row ####

distInfoWide <- distInfoWide %>% filter(
  ((`Target Name`=="High school GPA") & (`Group Name`=="Institution sector") & (`Group Value`=="Private nonprofit less-than-2-year"))==FALSE 
)

copy1 <- distInfoWide %>% filter(
  (`Target Name`=="High school GPA") & (`Group Name`=="Institution sector") & (`Group Value`=="Public less-than-2-year") 
) %>% mutate(
  `Group Value` = rep("Private nonprofit less-than-2-year")
)

distInfoWide <- rbind(
  distInfoWide, 
  copy1
)
rm(copy1)

#### End #### 

#### Process distributions ####

for(i in (1:nrow(distInfoWide))){
  
  tempDF <- distInfoWide[i, ]
  
  numClasses1 <- 14
  if(is.na(tempDF$`Class 14 Share`[1])){numClasses1 <- numClasses1 - 1}
  if(is.na(tempDF$`Class 13 Share`[1])){numClasses1 <- numClasses1 - 1}
  if(is.na(tempDF$`Class 12 Share`[1])){numClasses1 <- numClasses1 - 1}
  if(is.na(tempDF$`Class 11 Share`[1])){numClasses1 <- numClasses1 - 1}
  if(is.na(tempDF$`Class 10 Share`[1])){numClasses1 <- numClasses1 - 1}
  if(is.na(tempDF$`Class 9 Share`[1])){numClasses1 <- numClasses1 - 1}
  if(is.na(tempDF$`Class 8 Share`[1])){numClasses1 <- numClasses1 - 1}
  if(is.na(tempDF$`Class 7 Share`[1])){numClasses1 <- numClasses1 - 1}
  if(is.na(tempDF$`Class 6 Share`[1])){numClasses1 <- numClasses1 - 1}
  if(is.na(tempDF$`Class 5 Share`[1])){numClasses1 <- numClasses1 - 1}
  if(is.na(tempDF$`Class 4 Share`[1])){numClasses1 <- numClasses1 - 1}
  if(is.na(tempDF$`Class 3 Share`[1])){numClasses1 <- numClasses1 - 1}
  if(is.na(tempDF$`Class 2 Share`[1])){numClasses1 <- numClasses1 - 1}
  if(is.na(tempDF$`Class 1 Share`[1])){numClasses1 <- numClasses1 - 1}
  
  longDF <- tempDF %>% pivot_longer(
    cols=c(
      `Class 1 Share`, 
      `Class 2 Share`, 
      `Class 3 Share`, 
      `Class 4 Share`, 
      `Class 5 Share`, 
      `Class 6 Share`, 
      `Class 7 Share`, 
      `Class 8 Share`, 
      `Class 9 Share`, 
      `Class 10 Share`, 
      `Class 11 Share`, 
      `Class 12 Share`, 
      `Class 13 Share`, 
      `Class 14 Share`
    ), 
    names_to="Class", 
    values_to="Share"
  ) %>% select(
    `Class`, 
    `Share`
  ) 
  
  suppressWarnings({
    longDF <- longDF %>% filter(
      is.na(`Share`)==FALSE
    ) %>% mutate(
      `Share` = gsub("‡", "0", `Share`)
    ) %>% mutate(
      `Share` = gsub("#", "0", `Share`)
    ) %>% mutate(
      `Share` = gsub(" !!", "", `Share`)
    ) %>% mutate(
      `Share` = gsub(" !", "", `Share`)
    ) %>% mutate(
      `Share` = as.numeric(`Share`)
    ) %>% mutate(
      `Share` = round(`Share`, 3)
    )
  })

  # Confirming that all entries are numbers: 
  if(sum(is.na(longDF$`Share`)) > 0){
    print(paste("Row ", i, " has missing data.", sep=""))
  }

  # Confirming that only eight rows (1% of all) are not within 0.3 percentage points of 100
  # if(between(sum(longDF$Share), 99.7, 100.3)==FALSE){
  #   print(paste("Row ", i, ": ", sum(longDF$Share), sep=""))
  # }
  
  # Add any missing percentage points into the largest group 
  missingVal <- round(100 - sum(longDF$`Share`), 3)
  if(missingVal != 0){
    longDF <- longDF %>% arrange(
      desc(`Share`)
    )
    longDF$`Share`[1] <- longDF$`Share`[1] + missingVal
  }
  rm(missingVal)
  
  longDF <- longDF %>% mutate(
    `Share` = round(`Share`, 1)
  )
  
  # Once again add any missing percentage points into the largest group 
  missingVal <- round(100.0 - sum(longDF$`Share`), 1)
  if(missingVal != 0){
    longDF <- longDF %>% arrange(
      desc(`Share`)
    )
    longDF$`Share`[1] <- longDF$`Share`[1] + missingVal
  }
  rm(missingVal)
  
  # Confirming that all now add up to 100
  if(round(sum(longDF$`Share`), 1) != round(100, 1)){
    print(paste("Row ", i, ": ", sum(longDF$Share), sep=""))
  }
  
  if(nrow(longDF) < 3){longDF <- longDF %>% add_row(`Class` = "Class 3 Share", `Share` = 0)}
  if(nrow(longDF) < 4){longDF <- longDF %>% add_row(`Class` = "Class 4 Share", `Share` = 0)}
  if(nrow(longDF) < 5){longDF <- longDF %>% add_row(`Class` = "Class 5 Share", `Share` = 0)}
  if(nrow(longDF) < 6){longDF <- longDF %>% add_row(`Class` = "Class 6 Share", `Share` = 0)}
  if(nrow(longDF) < 7){longDF <- longDF %>% add_row(`Class` = "Class 7 Share", `Share` = 0)}
  if(nrow(longDF) < 8){longDF <- longDF %>% add_row(`Class` = "Class 8 Share", `Share` = 0)}
  if(nrow(longDF) < 9){longDF <- longDF %>% add_row(`Class` = "Class 9 Share", `Share` = 0)}
  if(nrow(longDF) < 10){longDF <- longDF %>% add_row(`Class` = "Class 10 Share", `Share` = 0)}
  if(nrow(longDF) < 11){longDF <- longDF %>% add_row(`Class` = "Class 11 Share", `Share` = 0)}
  if(nrow(longDF) < 12){longDF <- longDF %>% add_row(`Class` = "Class 12 Share", `Share` = 0)}
  if(nrow(longDF) < 13){longDF <- longDF %>% add_row(`Class` = "Class 13 Share", `Share` = 0)}
  if(nrow(longDF) < 14){longDF <- longDF %>% add_row(`Class` = "Class 14 Share", `Share` = 0)}

  wideDF <- longDF %>% pivot_wider(
    names_from=`Class`, 
    values_from=`Share`
  ) %>% mutate(
    `Table Source Code` = rep(tempDF$`Table Source Code`[1])
  ) %>% select(
    `Table Source Code`, 
    `Class 1 Share`, 
    `Class 2 Share`, 
    `Class 3 Share`, 
    `Class 4 Share`, 
    `Class 5 Share`, 
    `Class 6 Share`, 
    `Class 7 Share`, 
    `Class 8 Share`, 
    `Class 9 Share`, 
    `Class 10 Share`, 
    `Class 11 Share`, 
    `Class 12 Share`, 
    `Class 13 Share`, 
    `Class 14 Share`
  )
  
  tempDF <- tempDF %>% select(
    `Table Source Code`,
    `Target Name`, 
    `Target Type`, 
    `Group Name`, 
    `Class 1 Name`, 
    `Class 2 Name`, 
    `Class 3 Name`, 
    `Class 4 Name`, 
    `Class 5 Name`, 
    `Class 6 Name`, 
    `Class 7 Name`, 
    `Class 8 Name`, 
    `Class 9 Name`, 
    `Class 10 Name`, 
    `Class 11 Name`, 
    `Class 12 Name`, 
    `Class 13 Name`, 
    `Class 14 Name` 
  )
  tempDF <- left_join(x=tempDF, y=wideDF, by="Table Source Code")
  
  if(i==1){
    newInfoWide <- tempDF 
  }else{
    newInfoWide <- rbind(
      newInfoWide, 
      tempDF
    )
  }
  rm(tempDF, longDF, wideDF, numClasses1)
  
}
rm(i)

#### End #### 

#### Turn distributions into percentiles ####

for(i in (1:nrow(newInfoWide))){
  
  tempDF <- newInfoWide[i, ]
  numCols <- ncol(tempDF)
  
  for(j in (1:100)){
    
    if(tempDF$`Class 1 Share`[1] > 0){
      selectedClass <- tempDF$`Class 1 Name`[1]
      tempDF$`Class 1 Share`[1] <- tempDF$`Class 1 Share`[1] - 1
      if(tempDF$`Class 1 Share`[1] < 0){
        tempDF$`Class 2 Share`[1] <- tempDF$`Class 2 Share`[1] + tempDF$`Class 1 Share`[1]
        tempDF$`Class 1 Share`[1] <- 0
        if(tempDF$`Class 2 Share`[1] < 0){
          tempDF$`Class 3 Share`[1] <- tempDF$`Class 3 Share`[1] + tempDF$`Class 2 Share`[1]
          tempDF$`Class 2 Share`[1] <- 0
          if(tempDF$`Class 3 Share`[1] < 0){
            tempDF$`Class 4 Share`[1] <- tempDF$`Class 4 Share`[1] + tempDF$`Class 3 Share`[1]
            tempDF$`Class 3 Share`[1] <- 0
            if(tempDF$`Class 4 Share`[1] < 0){
              tempDF$`Class 5 Share`[1] <- tempDF$`Class 5 Share`[1] + tempDF$`Class 4 Share`[1]
              tempDF$`Class 4 Share`[1] <- 0
              if(tempDF$`Class 5 Share`[1] < 0){
                tempDF$`Class 6 Share`[1] <- tempDF$`Class 6 Share`[1] + tempDF$`Class 5 Share`[1]
                tempDF$`Class 5 Share`[1] <- 0
                if(tempDF$`Class 6 Share`[1] < 0){
                  tempDF$`Class 7 Share`[1] <- tempDF$`Class 7 Share`[1] + tempDF$`Class 6 Share`[1]
                  tempDF$`Class 6 Share`[1] <- 0
                  if(tempDF$`Class 7 Share`[1] < 0){
                    tempDF$`Class 8 Share`[1] <- tempDF$`Class 8 Share`[1] + tempDF$`Class 7 Share`[1]
                    tempDF$`Class 7 Share`[1] <- 0
                    if(tempDF$`Class 8 Share`[1] < 0){
                      tempDF$`Class 9 Share`[1] <- tempDF$`Class 9 Share`[1] + tempDF$`Class 8 Share`[1]
                      tempDF$`Class 8 Share`[1] <- 0
                      if(tempDF$`Class 9 Share`[1] < 0){
                        tempDF$`Class 10 Share`[1] <- tempDF$`Class 10 Share`[1] + tempDF$`Class 9 Share`[1]
                        tempDF$`Class 9 Share`[1] <- 0
                        if(tempDF$`Class 10 Share`[1] < 0){
                          tempDF$`Class 11 Share`[1] <- tempDF$`Class 11 Share`[1] + tempDF$`Class 10 Share`[1]
                          tempDF$`Class 10 Share`[1] <- 0
                          if(tempDF$`Class 11 Share`[1] < 0){
                            tempDF$`Class 12 Share`[1] <- tempDF$`Class 12 Share`[1] + tempDF$`Class 11 Share`[1]
                            tempDF$`Class 11 Share`[1] <- 0
                            if(tempDF$`Class 12 Share`[1] < 0){
                              tempDF$`Class 13 Share`[1] <- tempDF$`Class 13 Share`[1] + tempDF$`Class 12 Share`[1]
                              tempDF$`Class 12 Share`[1] <- 0
                              if(tempDF$`Class 13 Share`[1] < 0){
                                tempDF$`Class 14 Share`[1] <- tempDF$`Class 14 Share`[1] + tempDF$`Class 13 Share`[1]
                                tempDF$`Class 13 Share`[1] <- 0
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }else{
      if(tempDF$`Class 2 Share`[1] > 0){
        selectedClass <- tempDF$`Class 2 Name`[1]
        tempDF$`Class 2 Share`[1] <- tempDF$`Class 2 Share`[1] - 1
        if(tempDF$`Class 2 Share`[1] < 0){
          tempDF$`Class 3 Share`[1] <- tempDF$`Class 3 Share`[1] + tempDF$`Class 2 Share`[1]
          tempDF$`Class 2 Share`[1] <- 0
          if(tempDF$`Class 3 Share`[1] < 0){
            tempDF$`Class 4 Share`[1] <- tempDF$`Class 4 Share`[1] + tempDF$`Class 3 Share`[1]
            tempDF$`Class 3 Share`[1] <- 0
            if(tempDF$`Class 4 Share`[1] < 0){
              tempDF$`Class 5 Share`[1] <- tempDF$`Class 5 Share`[1] + tempDF$`Class 4 Share`[1]
              tempDF$`Class 4 Share`[1] <- 0
              if(tempDF$`Class 5 Share`[1] < 0){
                tempDF$`Class 6 Share`[1] <- tempDF$`Class 6 Share`[1] + tempDF$`Class 5 Share`[1]
                tempDF$`Class 5 Share`[1] <- 0
                if(tempDF$`Class 6 Share`[1] < 0){
                  tempDF$`Class 7 Share`[1] <- tempDF$`Class 7 Share`[1] + tempDF$`Class 6 Share`[1]
                  tempDF$`Class 6 Share`[1] <- 0
                  if(tempDF$`Class 7 Share`[1] < 0){
                    tempDF$`Class 8 Share`[1] <- tempDF$`Class 8 Share`[1] + tempDF$`Class 7 Share`[1]
                    tempDF$`Class 7 Share`[1] <- 0
                    if(tempDF$`Class 8 Share`[1] < 0){
                      tempDF$`Class 9 Share`[1] <- tempDF$`Class 9 Share`[1] + tempDF$`Class 8 Share`[1]
                      tempDF$`Class 8 Share`[1] <- 0
                      if(tempDF$`Class 9 Share`[1] < 0){
                        tempDF$`Class 10 Share`[1] <- tempDF$`Class 10 Share`[1] + tempDF$`Class 9 Share`[1]
                        tempDF$`Class 9 Share`[1] <- 0
                        if(tempDF$`Class 10 Share`[1] < 0){
                          tempDF$`Class 11 Share`[1] <- tempDF$`Class 11 Share`[1] + tempDF$`Class 10 Share`[1]
                          tempDF$`Class 10 Share`[1] <- 0
                          if(tempDF$`Class 11 Share`[1] < 0){
                            tempDF$`Class 12 Share`[1] <- tempDF$`Class 12 Share`[1] + tempDF$`Class 11 Share`[1]
                            tempDF$`Class 11 Share`[1] <- 0
                            if(tempDF$`Class 12 Share`[1] < 0){
                              tempDF$`Class 13 Share`[1] <- tempDF$`Class 13 Share`[1] + tempDF$`Class 12 Share`[1]
                              tempDF$`Class 12 Share`[1] <- 0
                              if(tempDF$`Class 13 Share`[1] < 0){
                                tempDF$`Class 14 Share`[1] <- tempDF$`Class 14 Share`[1] + tempDF$`Class 13 Share`[1]
                                tempDF$`Class 13 Share`[1] <- 0
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }else{
        if(tempDF$`Class 3 Share`[1] > 0){
          selectedClass <- tempDF$`Class 3 Name`[1]
          tempDF$`Class 3 Share`[1] <- tempDF$`Class 3 Share`[1] - 1
          if(tempDF$`Class 3 Share`[1] < 0){
            tempDF$`Class 4 Share`[1] <- tempDF$`Class 4 Share`[1] + tempDF$`Class 3 Share`[1]
            tempDF$`Class 3 Share`[1] <- 0
            if(tempDF$`Class 4 Share`[1] < 0){
              tempDF$`Class 5 Share`[1] <- tempDF$`Class 5 Share`[1] + tempDF$`Class 4 Share`[1]
              tempDF$`Class 4 Share`[1] <- 0
              if(tempDF$`Class 5 Share`[1] < 0){
                tempDF$`Class 6 Share`[1] <- tempDF$`Class 6 Share`[1] + tempDF$`Class 5 Share`[1]
                tempDF$`Class 5 Share`[1] <- 0
                if(tempDF$`Class 6 Share`[1] < 0){
                  tempDF$`Class 7 Share`[1] <- tempDF$`Class 7 Share`[1] + tempDF$`Class 6 Share`[1]
                  tempDF$`Class 6 Share`[1] <- 0
                  if(tempDF$`Class 7 Share`[1] < 0){
                    tempDF$`Class 8 Share`[1] <- tempDF$`Class 8 Share`[1] + tempDF$`Class 7 Share`[1]
                    tempDF$`Class 7 Share`[1] <- 0
                    if(tempDF$`Class 8 Share`[1] < 0){
                      tempDF$`Class 9 Share`[1] <- tempDF$`Class 9 Share`[1] + tempDF$`Class 8 Share`[1]
                      tempDF$`Class 8 Share`[1] <- 0
                      if(tempDF$`Class 9 Share`[1] < 0){
                        tempDF$`Class 10 Share`[1] <- tempDF$`Class 10 Share`[1] + tempDF$`Class 9 Share`[1]
                        tempDF$`Class 9 Share`[1] <- 0
                        if(tempDF$`Class 10 Share`[1] < 0){
                          tempDF$`Class 11 Share`[1] <- tempDF$`Class 11 Share`[1] + tempDF$`Class 10 Share`[1]
                          tempDF$`Class 10 Share`[1] <- 0
                          if(tempDF$`Class 11 Share`[1] < 0){
                            tempDF$`Class 12 Share`[1] <- tempDF$`Class 12 Share`[1] + tempDF$`Class 11 Share`[1]
                            tempDF$`Class 11 Share`[1] <- 0
                            if(tempDF$`Class 12 Share`[1] < 0){
                              tempDF$`Class 13 Share`[1] <- tempDF$`Class 13 Share`[1] + tempDF$`Class 12 Share`[1]
                              tempDF$`Class 12 Share`[1] <- 0
                              if(tempDF$`Class 13 Share`[1] < 0){
                                tempDF$`Class 14 Share`[1] <- tempDF$`Class 14 Share`[1] + tempDF$`Class 13 Share`[1]
                                tempDF$`Class 13 Share`[1] <- 0
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }else{
          if(tempDF$`Class 4 Share`[1] > 0){
            selectedClass <- tempDF$`Class 4 Name`[1]
            tempDF$`Class 4 Share`[1] <- tempDF$`Class 4 Share`[1] - 1
            if(tempDF$`Class 4 Share`[1] < 0){
              tempDF$`Class 5 Share`[1] <- tempDF$`Class 5 Share`[1] + tempDF$`Class 4 Share`[1]
              tempDF$`Class 4 Share`[1] <- 0
              if(tempDF$`Class 5 Share`[1] < 0){
                tempDF$`Class 6 Share`[1] <- tempDF$`Class 6 Share`[1] + tempDF$`Class 5 Share`[1]
                tempDF$`Class 5 Share`[1] <- 0
                if(tempDF$`Class 6 Share`[1] < 0){
                  tempDF$`Class 7 Share`[1] <- tempDF$`Class 7 Share`[1] + tempDF$`Class 6 Share`[1]
                  tempDF$`Class 6 Share`[1] <- 0
                  if(tempDF$`Class 7 Share`[1] < 0){
                    tempDF$`Class 8 Share`[1] <- tempDF$`Class 8 Share`[1] + tempDF$`Class 7 Share`[1]
                    tempDF$`Class 7 Share`[1] <- 0
                    if(tempDF$`Class 8 Share`[1] < 0){
                      tempDF$`Class 9 Share`[1] <- tempDF$`Class 9 Share`[1] + tempDF$`Class 8 Share`[1]
                      tempDF$`Class 8 Share`[1] <- 0
                      if(tempDF$`Class 9 Share`[1] < 0){
                        tempDF$`Class 10 Share`[1] <- tempDF$`Class 10 Share`[1] + tempDF$`Class 9 Share`[1]
                        tempDF$`Class 9 Share`[1] <- 0
                        if(tempDF$`Class 10 Share`[1] < 0){
                          tempDF$`Class 11 Share`[1] <- tempDF$`Class 11 Share`[1] + tempDF$`Class 10 Share`[1]
                          tempDF$`Class 10 Share`[1] <- 0
                          if(tempDF$`Class 11 Share`[1] < 0){
                            tempDF$`Class 12 Share`[1] <- tempDF$`Class 12 Share`[1] + tempDF$`Class 11 Share`[1]
                            tempDF$`Class 11 Share`[1] <- 0
                            if(tempDF$`Class 12 Share`[1] < 0){
                              tempDF$`Class 13 Share`[1] <- tempDF$`Class 13 Share`[1] + tempDF$`Class 12 Share`[1]
                              tempDF$`Class 12 Share`[1] <- 0
                              if(tempDF$`Class 13 Share`[1] < 0){
                                tempDF$`Class 14 Share`[1] <- tempDF$`Class 14 Share`[1] + tempDF$`Class 13 Share`[1]
                                tempDF$`Class 13 Share`[1] <- 0
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }else{
            if(tempDF$`Class 5 Share`[1] > 0){
              selectedClass <- tempDF$`Class 5 Name`[1]
              tempDF$`Class 5 Share`[1] <- tempDF$`Class 5 Share`[1] - 1
              if(tempDF$`Class 5 Share`[1] < 0){
                tempDF$`Class 6 Share`[1] <- tempDF$`Class 6 Share`[1] + tempDF$`Class 5 Share`[1]
                tempDF$`Class 5 Share`[1] <- 0
                if(tempDF$`Class 6 Share`[1] < 0){
                  tempDF$`Class 7 Share`[1] <- tempDF$`Class 7 Share`[1] + tempDF$`Class 6 Share`[1]
                  tempDF$`Class 6 Share`[1] <- 0
                  if(tempDF$`Class 7 Share`[1] < 0){
                    tempDF$`Class 8 Share`[1] <- tempDF$`Class 8 Share`[1] + tempDF$`Class 7 Share`[1]
                    tempDF$`Class 7 Share`[1] <- 0
                    if(tempDF$`Class 8 Share`[1] < 0){
                      tempDF$`Class 9 Share`[1] <- tempDF$`Class 9 Share`[1] + tempDF$`Class 8 Share`[1]
                      tempDF$`Class 8 Share`[1] <- 0
                      if(tempDF$`Class 9 Share`[1] < 0){
                        tempDF$`Class 10 Share`[1] <- tempDF$`Class 10 Share`[1] + tempDF$`Class 9 Share`[1]
                        tempDF$`Class 9 Share`[1] <- 0
                        if(tempDF$`Class 10 Share`[1] < 0){
                          tempDF$`Class 11 Share`[1] <- tempDF$`Class 11 Share`[1] + tempDF$`Class 10 Share`[1]
                          tempDF$`Class 10 Share`[1] <- 0
                          if(tempDF$`Class 11 Share`[1] < 0){
                            tempDF$`Class 12 Share`[1] <- tempDF$`Class 12 Share`[1] + tempDF$`Class 11 Share`[1]
                            tempDF$`Class 11 Share`[1] <- 0
                            if(tempDF$`Class 12 Share`[1] < 0){
                              tempDF$`Class 13 Share`[1] <- tempDF$`Class 13 Share`[1] + tempDF$`Class 12 Share`[1]
                              tempDF$`Class 12 Share`[1] <- 0
                              if(tempDF$`Class 13 Share`[1] < 0){
                                tempDF$`Class 14 Share`[1] <- tempDF$`Class 14 Share`[1] + tempDF$`Class 13 Share`[1]
                                tempDF$`Class 13 Share`[1] <- 0
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }else{
              if(tempDF$`Class 6 Share`[1] > 0){
                selectedClass <- tempDF$`Class 6 Name`[1]
                tempDF$`Class 6 Share`[1] <- tempDF$`Class 6 Share`[1] - 1
                if(tempDF$`Class 6 Share`[1] < 0){
                  tempDF$`Class 7 Share`[1] <- tempDF$`Class 7 Share`[1] + tempDF$`Class 6 Share`[1]
                  tempDF$`Class 6 Share`[1] <- 0
                  if(tempDF$`Class 7 Share`[1] < 0){
                    tempDF$`Class 8 Share`[1] <- tempDF$`Class 8 Share`[1] + tempDF$`Class 7 Share`[1]
                    tempDF$`Class 7 Share`[1] <- 0
                    if(tempDF$`Class 8 Share`[1] < 0){
                      tempDF$`Class 9 Share`[1] <- tempDF$`Class 9 Share`[1] + tempDF$`Class 8 Share`[1]
                      tempDF$`Class 8 Share`[1] <- 0
                      if(tempDF$`Class 9 Share`[1] < 0){
                        tempDF$`Class 10 Share`[1] <- tempDF$`Class 10 Share`[1] + tempDF$`Class 9 Share`[1]
                        tempDF$`Class 9 Share`[1] <- 0
                        if(tempDF$`Class 10 Share`[1] < 0){
                          tempDF$`Class 11 Share`[1] <- tempDF$`Class 11 Share`[1] + tempDF$`Class 10 Share`[1]
                          tempDF$`Class 10 Share`[1] <- 0
                          if(tempDF$`Class 11 Share`[1] < 0){
                            tempDF$`Class 12 Share`[1] <- tempDF$`Class 12 Share`[1] + tempDF$`Class 11 Share`[1]
                            tempDF$`Class 11 Share`[1] <- 0
                            if(tempDF$`Class 12 Share`[1] < 0){
                              tempDF$`Class 13 Share`[1] <- tempDF$`Class 13 Share`[1] + tempDF$`Class 12 Share`[1]
                              tempDF$`Class 12 Share`[1] <- 0
                              if(tempDF$`Class 13 Share`[1] < 0){
                                tempDF$`Class 14 Share`[1] <- tempDF$`Class 14 Share`[1] + tempDF$`Class 13 Share`[1]
                                tempDF$`Class 13 Share`[1] <- 0
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }else{
                if(tempDF$`Class 7 Share`[1] > 0){
                  selectedClass <- tempDF$`Class 7 Name`[1]
                  tempDF$`Class 7 Share`[1] <- tempDF$`Class 7 Share`[1] - 1
                  if(tempDF$`Class 7 Share`[1] < 0){
                    tempDF$`Class 8 Share`[1] <- tempDF$`Class 8 Share`[1] + tempDF$`Class 7 Share`[1]
                    tempDF$`Class 7 Share`[1] <- 0
                    if(tempDF$`Class 8 Share`[1] < 0){
                      tempDF$`Class 9 Share`[1] <- tempDF$`Class 9 Share`[1] + tempDF$`Class 8 Share`[1]
                      tempDF$`Class 8 Share`[1] <- 0
                      if(tempDF$`Class 9 Share`[1] < 0){
                        tempDF$`Class 10 Share`[1] <- tempDF$`Class 10 Share`[1] + tempDF$`Class 9 Share`[1]
                        tempDF$`Class 9 Share`[1] <- 0
                        if(tempDF$`Class 10 Share`[1] < 0){
                          tempDF$`Class 11 Share`[1] <- tempDF$`Class 11 Share`[1] + tempDF$`Class 10 Share`[1]
                          tempDF$`Class 10 Share`[1] <- 0
                          if(tempDF$`Class 11 Share`[1] < 0){
                            tempDF$`Class 12 Share`[1] <- tempDF$`Class 12 Share`[1] + tempDF$`Class 11 Share`[1]
                            tempDF$`Class 11 Share`[1] <- 0
                            if(tempDF$`Class 12 Share`[1] < 0){
                              tempDF$`Class 13 Share`[1] <- tempDF$`Class 13 Share`[1] + tempDF$`Class 12 Share`[1]
                              tempDF$`Class 12 Share`[1] <- 0
                              if(tempDF$`Class 13 Share`[1] < 0){
                                tempDF$`Class 14 Share`[1] <- tempDF$`Class 14 Share`[1] + tempDF$`Class 13 Share`[1]
                                tempDF$`Class 13 Share`[1] <- 0
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }else{
                  if(tempDF$`Class 8 Share`[1] > 0){
                    selectedClass <- tempDF$`Class 8 Name`[1]
                    tempDF$`Class 8 Share`[1] <- tempDF$`Class 8 Share`[1] - 1
                    if(tempDF$`Class 8 Share`[1] < 0){
                      tempDF$`Class 9 Share`[1] <- tempDF$`Class 9 Share`[1] + tempDF$`Class 8 Share`[1]
                      tempDF$`Class 8 Share`[1] <- 0
                      if(tempDF$`Class 9 Share`[1] < 0){
                        tempDF$`Class 10 Share`[1] <- tempDF$`Class 10 Share`[1] + tempDF$`Class 9 Share`[1]
                        tempDF$`Class 9 Share`[1] <- 0
                        if(tempDF$`Class 10 Share`[1] < 0){
                          tempDF$`Class 11 Share`[1] <- tempDF$`Class 11 Share`[1] + tempDF$`Class 10 Share`[1]
                          tempDF$`Class 10 Share`[1] <- 0
                          if(tempDF$`Class 11 Share`[1] < 0){
                            tempDF$`Class 12 Share`[1] <- tempDF$`Class 12 Share`[1] + tempDF$`Class 11 Share`[1]
                            tempDF$`Class 11 Share`[1] <- 0
                            if(tempDF$`Class 12 Share`[1] < 0){
                              tempDF$`Class 13 Share`[1] <- tempDF$`Class 13 Share`[1] + tempDF$`Class 12 Share`[1]
                              tempDF$`Class 12 Share`[1] <- 0
                              if(tempDF$`Class 13 Share`[1] < 0){
                                tempDF$`Class 14 Share`[1] <- tempDF$`Class 14 Share`[1] + tempDF$`Class 13 Share`[1]
                                tempDF$`Class 13 Share`[1] <- 0
                              }
                            }
                          }
                        }
                      }
                    }
                  }else{
                    if(tempDF$`Class 9 Share`[1] > 0){
                      selectedClass <- tempDF$`Class 9 Name`[1]
                      tempDF$`Class 9 Share`[1] <- tempDF$`Class 9 Share`[1] - 1
                      if(tempDF$`Class 9 Share`[1] < 0){
                        tempDF$`Class 10 Share`[1] <- tempDF$`Class 10 Share`[1] + tempDF$`Class 9 Share`[1]
                        tempDF$`Class 9 Share`[1] <- 0
                        if(tempDF$`Class 10 Share`[1] < 0){
                          tempDF$`Class 11 Share`[1] <- tempDF$`Class 11 Share`[1] + tempDF$`Class 10 Share`[1]
                          tempDF$`Class 10 Share`[1] <- 0
                          if(tempDF$`Class 11 Share`[1] < 0){
                            tempDF$`Class 12 Share`[1] <- tempDF$`Class 12 Share`[1] + tempDF$`Class 11 Share`[1]
                            tempDF$`Class 11 Share`[1] <- 0
                            if(tempDF$`Class 12 Share`[1] < 0){
                              tempDF$`Class 13 Share`[1] <- tempDF$`Class 13 Share`[1] + tempDF$`Class 12 Share`[1]
                              tempDF$`Class 12 Share`[1] <- 0
                              if(tempDF$`Class 13 Share`[1] < 0){
                                tempDF$`Class 14 Share`[1] <- tempDF$`Class 14 Share`[1] + tempDF$`Class 13 Share`[1]
                                tempDF$`Class 13 Share`[1] <- 0
                              }
                            }
                          }
                        }
                      }
                    }else{
                      if(tempDF$`Class 10 Share`[1] > 0){
                        selectedClass <- tempDF$`Class 10 Name`[1]
                        tempDF$`Class 10 Share`[1] <- tempDF$`Class 10 Share`[1] - 1
                        if(tempDF$`Class 10 Share`[1] < 0){
                          tempDF$`Class 11 Share`[1] <- tempDF$`Class 11 Share`[1] + tempDF$`Class 10 Share`[1]
                          tempDF$`Class 10 Share`[1] <- 0
                          if(tempDF$`Class 11 Share`[1] < 0){
                            tempDF$`Class 12 Share`[1] <- tempDF$`Class 12 Share`[1] + tempDF$`Class 11 Share`[1]
                            tempDF$`Class 11 Share`[1] <- 0
                            if(tempDF$`Class 12 Share`[1] < 0){
                              tempDF$`Class 13 Share`[1] <- tempDF$`Class 13 Share`[1] + tempDF$`Class 12 Share`[1]
                              tempDF$`Class 12 Share`[1] <- 0
                              if(tempDF$`Class 13 Share`[1] < 0){
                                tempDF$`Class 14 Share`[1] <- tempDF$`Class 14 Share`[1] + tempDF$`Class 13 Share`[1]
                                tempDF$`Class 13 Share`[1] <- 0
                              }
                            }
                          }
                        }
                      }else{
                        if(tempDF$`Class 11 Share`[1] > 0){
                          selectedClass <- tempDF$`Class 11 Name`[1]
                          tempDF$`Class 11 Share`[1] <- tempDF$`Class 11 Share`[1] - 1
                          if(tempDF$`Class 11 Share`[1] < 0){
                            tempDF$`Class 12 Share`[1] <- tempDF$`Class 12 Share`[1] + tempDF$`Class 11 Share`[1]
                            tempDF$`Class 11 Share`[1] <- 0
                            if(tempDF$`Class 12 Share`[1] < 0){
                              tempDF$`Class 13 Share`[1] <- tempDF$`Class 13 Share`[1] + tempDF$`Class 12 Share`[1]
                              tempDF$`Class 12 Share`[1] <- 0
                              if(tempDF$`Class 13 Share`[1] < 0){
                                tempDF$`Class 14 Share`[1] <- tempDF$`Class 14 Share`[1] + tempDF$`Class 13 Share`[1]
                                tempDF$`Class 13 Share`[1] <- 0
                              }
                            }
                          }
                        }else{
                          if(tempDF$`Class 12 Share`[1] > 0){
                            selectedClass <- tempDF$`Class 12 Name`[1]
                            tempDF$`Class 12 Share`[1] <- tempDF$`Class 12 Share`[1] - 1
                            if(tempDF$`Class 12 Share`[1] < 0){
                              tempDF$`Class 13 Share`[1] <- tempDF$`Class 13 Share`[1] + tempDF$`Class 12 Share`[1]
                              tempDF$`Class 12 Share`[1] <- 0
                              if(tempDF$`Class 13 Share`[1] < 0){
                                tempDF$`Class 14 Share`[1] <- tempDF$`Class 14 Share`[1] + tempDF$`Class 13 Share`[1]
                                tempDF$`Class 13 Share`[1] <- 0
                              }
                            }
                          }else{
                            if(tempDF$`Class 13 Share`[1] > 0){
                              selectedClass <- tempDF$`Class 13 Name`[1]
                              tempDF$`Class 13 Share`[1] <- tempDF$`Class 13 Share`[1] - 1
                              if(tempDF$`Class 13 Share`[1] < 0){
                                tempDF$`Class 14 Share`[1] <- tempDF$`Class 14 Share`[1] + tempDF$`Class 13 Share`[1]
                                tempDF$`Class 13 Share`[1] <- 0
                              }
                            }else{
                              if(tempDF$`Class 14 Share`[1] > 0){
                                selectedClass <- tempDF$`Class 14 Name`[1]
                                tempDF$`Class 14 Share`[1] <- tempDF$`Class 14 Share`[1] - 1
                              }else{
                                selectedClass <- "Error"
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    
    tempDF <- tempDF %>% mutate(
      `New Percentile` = rep(selectedClass)
    )
    names(tempDF)[numCols + j] <- paste("Percentile ", j, sep="")
    rm(selectedClass)
  }
  rm(j)
  
  if(i==1){
    percentileDF <- tempDF
  }else{
    percentileDF <- rbind(
      percentileDF, 
      tempDF
    )
  }

  rm(tempDF, numCols)
}
rm(i)

#### End #### 

#### Separate into numeric and categorical ####

numericDF <- percentileDF %>% filter(`Target Type`=="Numeric")
categorDF <- percentileDF %>% filter(`Target Type`=="Categorical")

rm(distInfoWide, newInfoWide)

#### End #### 

#### Assign percentile values from numeric ranges ####



#### End #### 

##############################################
#### Student list using multi-var pulls   ####
##############################################

#### Establish empty dataframe ####

distInfoWide <- data.frame(
  `Table Source Code` = character(), 
  `Target Name` = character(), 
  `Target Label` = character(), 
  `Target Type` = character(), 
  `Group 1 Name` = character(), 
  `Group 1 Label` = character(),
  `Group 1 Value` = character(), 
  `Group 2 Name` = character(), 
  `Group 2 Label` = character(),
  `Group 2 Value` = character(), 
  `Group 3 Name` = character(), 
  `Group 3 Label` = character(),
  `Group 3 Value` = character(), 
  `Group 4 Name` = character(), 
  `Group 4 Label` = character(),
  `Group 4 Value` = character(),
  `Class 1 Name` = character(), 
  `Class 1 Share` = character(), 
  `Class 2 Name` = character(), 
  `Class 2 Share` = character(), 
  `Class 3 Name` = character(), 
  `Class 3 Share` = character(), 
  `Class 4 Name` = character(), 
  `Class 4 Share` = character(), 
  `Class 5 Name` = character(), 
  `Class 5 Share` = character(), 
  `Class 6 Name` = character(), 
  `Class 6 Share` = character(), 
  `Class 7 Name` = character(), 
  `Class 7 Share` = character(), 
  `Class 8 Name` = character(), 
  `Class 8 Share` = character(), 
  `Class 9 Name` = character(), 
  `Class 9 Share` = character(), 
  `Class 10 Name` = character(), 
  `Class 10 Share` = character(), 
  `Class 11 Name` = character(), 
  `Class 11 Share` = character(), 
  `Class 12 Name` = character(), 
  `Class 12 Share` = character(), 
  `Class 13 Name` = character(), 
  `Class 13 Share` = character(), 
  `Class 14 Name` = character(), 
  `Class 14 Share` = character(), 
  check.names=FALSE
)

#### End #### 







