
#### Setup ####

library(scales)
library(tidyverse)

#### End #### 

#############################################
#### Write function for making preds     ####
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
  thresholdVal, 
  absoluteAdjustment, 
  relativeAdjustment,
  showWork, 
  randomI, 
  randomC,
  
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
  
  #### Obtain intercept #### 
  
  setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Datalab outputs")
  
  interceptDF <- read.csv(
    paste("PowerStats_", retrievalCode, ".csv", sep=""), 
    header=FALSE, 
    skip=interceptRow - 1, 
    nrows=1
  ) 
  
  if(regressionType=="Linear"){
    interceptDF <- interceptDF %>% select(
      `V1`, `V2`, `V3`
    ) %>% rename(
      `Group` = `V1`, 
      `Coefficient` = `V2`, 
      `Standard Error` = `V3`
    )
  }
  if(regressionType=="Logistic"){
    interceptDF <- interceptDF %>% select(
      `V1`, `V8`, `V9`
    ) %>% rename(
      `Group` = `V1`, 
      `Coefficient` = `V8`, 
      `Standard Error` = `V9`
    )
  }
  
  studentListDF <- studentListDF %>% mutate(
    `Intercept` = rep(interceptDF$`Coefficient`[1]), 
    `Intercept SE` = rep(interceptDF$`Standard Error`[1])
  )
  rm(interceptDF)
  
  #### End #### 
  
  #### Add randomness to intercept ####
  
  if(randomI==TRUE){
    studentListDF <- studentListDF %>% mutate(
      `Random error` = rnorm(
        nrow(studentListDF), 
        mean = 0, 
        sd = 1
      )
    ) %>% mutate(
      `Intercept` = `Intercept` + (`Random error` * `Intercept SE`)
    ) %>% select(
      -(`Random error`), -(`Intercept SE`)
    )
  }else{
    studentListDF <- studentListDF %>% select(
      -(`Intercept SE`)
    )
  }
  
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
        ) 
      }else{
        tempDF <- read.csv(
          paste("PowerStats_", retrievalCode, ".csv", sep=""), 
          header=FALSE, 
          skip=startLine - 1, 
          nrows=1
        ) 
      }
      
      if(regressionType=="Linear"){
        tempDF <- tempDF %>% select(
          `V1`, `V2`, `V3`
        ) %>% mutate(
          `V1` = gsub("  ", "", `V1`)
        ) %>% rename(
          `Group` = `V1`, 
          `Coefficient` = `V2`, 
          `Standard Error` = `V3`
        )
      }
      if(regressionType=="Logistic"){
        tempDF <- tempDF %>% select(
          `V1`, `V8`, `V9`
        ) %>% mutate(
          `V1` = gsub("  ", "", `V1`)
        ) %>% rename(
          `Group` = `V1`, 
          `Coefficient` = `V8`, 
          `Standard Error` = `V9`
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
        
        # Tuition jurisdiction
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
      
      if(varType=="Categorical"){
        
        names(tempDF)[1] <- linkingVar
        names(tempDF)[2] <- paste("Variable ", i, " Coefficient", sep="")
        names(tempDF)[3] <- paste("Variable ", i, " Standard Error", sep="")
        
        studentListDF <- left_join(x=studentListDF, y=tempDF, by=linkingVar)
        
      }else{
        
        studentListDF <- studentListDF %>% mutate(
          `Variable i Coefficient` = rep(tempDF$`Coefficient`[1]), 
          `Variable i Standard Error` = rep(tempDF$`Standard Error`[1])
        )
        
        names(studentListDF)[ncol(studentListDF)-1] <- paste("Variable ", i, " Coefficient", sep="")
        names(studentListDF)[ncol(studentListDF)] <- paste("Variable ", i, " Standard Error", sep="")
        
      }
      
      rm(tempDF)
      
      #### End #### 
      
    }
    rm(includeVar, startLine, endLine, linkingVar, varType)
  }
  rm(i)
  
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
  
  #### Add randomness to coefficients ####
  
  if(randomC==TRUE){
    if(sum(studentListDF$`Variable 1 Coefficient`, na.rm=TRUE) != 0){
      studentListDF <- studentListDF %>% mutate(
        `Random error` = rnorm(
          nrow(studentListDF), 
          mean = 0, 
          sd = 1
        )
      ) %>% mutate(
        `Variable 1 Coefficient` = `Variable 1 Coefficient` + (`Random error` * `Variable 1 Standard Error`)
      ) %>% select(
        -(`Random error`), -(`Variable 1 Standard Error`)
      )
    }else{
      studentListDF <- studentListDF %>% select(-(`Variable 1 Standard Error`))
    }
    
    if(sum(studentListDF$`Variable 2 Coefficient`, na.rm=TRUE) != 0){
      studentListDF <- studentListDF %>% mutate(
        `Random error` = rnorm(
          nrow(studentListDF), 
          mean = 0, 
          sd = 1
        )
      ) %>% mutate(
        `Variable 2 Coefficient` = `Variable 2 Coefficient` + (`Random error` * `Variable 2 Standard Error`)
      ) %>% select(
        -(`Random error`), -(`Variable 2 Standard Error`)
      )
    }else{
      studentListDF <- studentListDF %>% select(-(`Variable 2 Standard Error`))
    }
    
    if(sum(studentListDF$`Variable 3 Coefficient`, na.rm=TRUE) != 0){
      studentListDF <- studentListDF %>% mutate(
        `Random error` = rnorm(
          nrow(studentListDF), 
          mean = 0, 
          sd = 1
        )
      ) %>% mutate(
        `Variable 3 Coefficient` = `Variable 3 Coefficient` + (`Random error` * `Variable 3 Standard Error`)
      ) %>% select(
        -(`Random error`), -(`Variable 3 Standard Error`)
      )
    }else{
      studentListDF <- studentListDF %>% select(-(`Variable 3 Standard Error`))
    }
    
    if(sum(studentListDF$`Variable 4 Coefficient`, na.rm=TRUE) != 0){
      studentListDF <- studentListDF %>% mutate(
        `Random error` = rnorm(
          nrow(studentListDF), 
          mean = 0, 
          sd = 1
        )
      ) %>% mutate(
        `Variable 4 Coefficient` = `Variable 4 Coefficient` + (`Random error` * `Variable 4 Standard Error`)
      ) %>% select(
        -(`Random error`), -(`Variable 4 Standard Error`)
      )
    }else{
      studentListDF <- studentListDF %>% select(-(`Variable 4 Standard Error`))
    }
    
    if(sum(studentListDF$`Variable 5 Coefficient`, na.rm=TRUE) != 0){
      studentListDF <- studentListDF %>% mutate(
        `Random error` = rnorm(
          nrow(studentListDF), 
          mean = 0, 
          sd = 1
        )
      ) %>% mutate(
        `Variable 5 Coefficient` = `Variable 5 Coefficient` + (`Random error` * `Variable 5 Standard Error`)
      ) %>% select(
        -(`Random error`), -(`Variable 5 Standard Error`)
      )
    }else{
      studentListDF <- studentListDF %>% select(-(`Variable 5 Standard Error`))
    }
    
    if(sum(studentListDF$`Variable 6 Coefficient`, na.rm=TRUE) != 0){
      studentListDF <- studentListDF %>% mutate(
        `Random error` = rnorm(
          nrow(studentListDF), 
          mean = 0, 
          sd = 1
        )
      ) %>% mutate(
        `Variable 6 Coefficient` = `Variable 6 Coefficient` + (`Random error` * `Variable 6 Standard Error`)
      ) %>% select(
        -(`Random error`), -(`Variable 6 Standard Error`)
      )
    }else{
      studentListDF <- studentListDF %>% select(-(`Variable 6 Standard Error`))
    }
    
    if(sum(studentListDF$`Variable 7 Coefficient`, na.rm=TRUE) != 0){
      studentListDF <- studentListDF %>% mutate(
        `Random error` = rnorm(
          nrow(studentListDF), 
          mean = 0, 
          sd = 1
        )
      ) %>% mutate(
        `Variable 7 Coefficient` = `Variable 7 Coefficient` + (`Random error` * `Variable 7 Standard Error`)
      ) %>% select(
        -(`Random error`), -(`Variable 7 Standard Error`)
      )
    }else{
      studentListDF <- studentListDF %>% select(-(`Variable 7 Standard Error`))
    }
    
    if(sum(studentListDF$`Variable 8 Coefficient`, na.rm=TRUE) != 0){
      studentListDF <- studentListDF %>% mutate(
        `Random error` = rnorm(
          nrow(studentListDF), 
          mean = 0, 
          sd = 1
        )
      ) %>% mutate(
        `Variable 8 Coefficient` = `Variable 8 Coefficient` + (`Random error` * `Variable 8 Standard Error`)
      ) %>% select(
        -(`Random error`), -(`Variable 8 Standard Error`)
      )
    }else{
      studentListDF <- studentListDF %>% select(-(`Variable 8 Standard Error`))
    }
    
    if(sum(studentListDF$`Variable 9 Coefficient`, na.rm=TRUE) != 0){
      studentListDF <- studentListDF %>% mutate(
        `Random error` = rnorm(
          nrow(studentListDF), 
          mean = 0, 
          sd = 1
        )
      ) %>% mutate(
        `Variable 9 Coefficient` = `Variable 9 Coefficient` + (`Random error` * `Variable 9 Standard Error`)
      ) %>% select(
        -(`Random error`), -(`Variable 9 Standard Error`)
      )
    }else{
      studentListDF <- studentListDF %>% select(-(`Variable 9 Standard Error`))
    }
  }else{
    studentListDF <- studentListDF %>% select(
      -(`Variable 1 Standard Error`), 
      -(`Variable 2 Standard Error`), 
      -(`Variable 3 Standard Error`), 
      -(`Variable 4 Standard Error`), 
      -(`Variable 5 Standard Error`), 
      -(`Variable 6 Standard Error`), 
      -(`Variable 7 Standard Error`), 
      -(`Variable 8 Standard Error`), 
      -(`Variable 9 Standard Error`)
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
      `New variable` = 1 / (1 + exp(`New variable` * -1))
    )
    studentListDF <- studentListDF %>% mutate(
      `New variable` = ifelse(`New variable` >= thresholdVal, positiveClass, negativeClass)
    )
  }
  
  if(regressionType=="Linear"){
    studentListDF <- studentListDF %>% mutate(
      `New variable` = `New variable` + absoluteAdjustment
    ) %>% mutate(
      `New variable` = `New variable` * relativeAdjustment
    )
  }
  
  names(studentListDF)[ncol(studentListDF)] <- newVariableName
  
  if(showWork==FALSE){
    
    studentListDF <- studentListDF %>% select(
      -(`Intercept`), 
      -(`Variable 1 Coefficient`), -(`Variable 1 Component`),
      -(`Variable 2 Coefficient`), -(`Variable 2 Component`),
      -(`Variable 3 Coefficient`), -(`Variable 3 Component`),
      -(`Variable 4 Coefficient`), -(`Variable 4 Component`),
      -(`Variable 5 Coefficient`), -(`Variable 5 Component`),
      -(`Variable 6 Coefficient`), -(`Variable 6 Component`),
      -(`Variable 7 Coefficient`), -(`Variable 7 Component`),
      -(`Variable 8 Coefficient`), -(`Variable 8 Component`),
      -(`Variable 9 Coefficient`), -(`Variable 9 Component`)
    )
    
  }
  
  #### End #### 
  
  return(studentListDF)
  
}

#############################################
#### Load IPEDS student enrollment data  ####
#############################################

#### Load IPEDS data ####

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/IPEDS data")

hd <- read.csv("hd2024.csv", header=TRUE) %>% select(
  `UNITID`, 
  `INSTNM`,
  `CONTROL`,
  `STABBR`, 
  `OBEREG`,
  `C21BASIC`
)
importC18BASIC <- read.csv("hd2023.csv", header=TRUE) %>% select(
  `UNITID`, 
  `C18BASIC`
)
hd <- left_join(x=hd, y=importC18BASIC, by="UNITID")
rm(importC18BASIC)
hd <- hd %>% mutate(
  `C18BASIC` = ifelse(
    is.na(`C18BASIC`), 
    `C21BASIC`, 
    `C18BASIC`
  )
) %>% select(
  -(`C21BASIC`)
)

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
) %>% add_row(
  `OBEREG` = 0, `Region` = "Other U.S. jurisdictions"
) 
hd <- left_join(x=hd, y=hdRegion, by="OBEREG") %>% select(-(`OBEREG`))
rm(hdRegion)

cost1 <- read.csv("cost1_2024.csv", header=TRUE) %>% select(
  `UNITID`, 
  `TUITVARY`
) 

cost1tuition <- data.frame(
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
cost1 <- left_join(x=cost1, y=cost1tuition, by="TUITVARY") %>% select(-(`TUITVARY`))
rm(cost1tuition)

effy <- read.csv("effy2024.csv", header=TRUE) %>% select(
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
    
    # As of 1-19-2026 I believe it's best not to include degree/certificate-seeking students,
    # so I'm excluding them here. 
    
    # Degree/certificate-seeking 
    24, #	Full-time students, Undergraduate, Degree/certificate-seeking, First-time
    39, #	Full-time students, Undergraduate, Other degree/certificate-seeking, Transfer-ins
    40, #	Full-time students, Undergraduate, Other degree/certificate-seeking, Continuing
    44, #	Part-time students, Undergraduate, Degree/certificate-seeking, First-time
    59, #	Part-time students, Undergraduate, Other degree/certificate-seeking, Transfer-ins
    60  #	Part-time students, Undergraduate, Other degree/certificate-seeking, Continuing
    # ,
    #
    # # Non-degree/certificate-seeking
    # 31, #	Full-time students, Undergraduate, Non-degree/certificate-seeking
    # 51  #	Part-time students, Undergraduate, Non-degree/certificate-seeking
    
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
  `Total name`="EFY2MORT", `Race`="More than one race", `Gender`="Total"
) %>% add_row(
  `Total name`="EFY2MORM", `Race`="More than one race", `Gender`="Male"
) %>% add_row(
  `Total name`="EFY2MORW", `Race`="More than one race", `Gender`="Female"
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
    `EFFYALEV` %in% c(
      24, #	Full-time students, Undergraduate, Degree/certificate-seeking, First-time
      39, #	Full-time students, Undergraduate, Other degree/certificate-seeking, Transfer-ins
      40, #	Full-time students, Undergraduate, Other degree/certificate-seeking, Continuing
      31  #	Full-time students, Undergraduate, Non-degree/certificate-seeking
    ), 
    "Full-time", 
    "Part-time"
  )
)

# I've confirmed that there is no additional information gained from these categories: 
effy <- effy %>% filter(
  (`Total name` %in% c(
    "EFYGUAN", "EFYGUKN", "EFYGUTOT", "EFYGUUN", "EFYTOTLT", "EFYTOTLM", "EFYTOTLW"
  ))==FALSE
)

# We also do not want Male + Female totals, since they are duplicative. 
effy <- effy %>% filter(
  (`Total name` %in% c(
    "EFY2MORT", "EFYAIANT", "EFYASIAT", "EFYBKAAT", "EFYHISPT", "EFYNHPIT", "EFYNRALT", "EFYUNKNT", "EFYWHITT"
  ))==FALSE
)

effy <- effy %>% filter(
  `Student count` > 0
)

# Index effy
effy <- effy %>% mutate(
  `Index` = (1:nrow(effy))
)

#### End #### 

# #### Create student level dataset #### 
# 
# for(i in effy$`Index`){
#   
#   if(i %% 1000 == 0){print(paste("Number ", comma(i), " out of ", comma(nrow(effy)), ".", sep=""))}
#   
#   tempEFFY <- effy %>% filter(`Index`==i)
#   
#   if(tempEFFY$`Student count`[1] > 0){
#     tempSTU <- data.frame(
#       `Effy index` = rep(tempEFFY$`Index`[1], tempEFFY$`Student count`[1]), 
#       `Student index` = (1:tempEFFY$`Student count`[1]),
#       `UNITID` = rep(tempEFFY$`UNITID`[1], tempEFFY$`Student count`[1]), 
#       `EFFYALEV` = rep(tempEFFY$`EFFYALEV`[1], tempEFFY$`Student count`[1]), 
#       `Race` = rep(tempEFFY$`Race`[1], tempEFFY$`Student count`[1]), 
#       `Gender` = rep(tempEFFY$`Gender`[1], tempEFFY$`Student count`[1]), 
#       `Enrollment intensity` = rep(tempEFFY$`Enrollment intensity`[1], tempEFFY$`Student count`[1]), 
#       check.names=FALSE
#     ) %>% mutate(
#       `Effy-student index` = paste(`Effy index`, ":", `Student index`, sep="")
#     )
#   }else{
#     tempSTU <- data.frame(
#       `Effy index` = numeric(), 
#       `Student index` = numeric(),
#       `UNITID` = numeric(), 
#       `EFFYALEV` = numeric(),
#       `Race` = character(), 
#       `Gender` = character(), 
#       `Enrollment intensity` = character(), 
#       `Effy-student index` = character(),
#       check.names=FALSE
#     ) 
#   }
#   
#   if(i == 1){ 
#     studentList <- tempSTU
#   }else{
#     studentList <- rbind(studentList, tempSTU)
#   }
#   
#   rm(tempSTU, tempEFFY)
#   
# }
# rm(i)
# 
# #### End #### 

# Temporary, 01-19-2026 
setwd("/Volumes/TOSHIBA EXT/Fed State Modeling")
studentList <- read.csv(
  "Test file 01-18-2026.csv", 
  header=TRUE, 
  check.names=FALSE
) %>% filter(
  `EFFYALEV` %in% c(
    31, #	Full-time students, Undergraduate, Non-degree/certificate-seeking
    51  #	Part-time students, Undergraduate, Non-degree/certificate-seeking
  )==FALSE
)

#### Import institutional information #### 

studentList <- left_join(x=studentList, y=hd, by="UNITID")
rm(hd)

studentList <- left_join(x=studentList, y=cost1, by="UNITID")
rm(cost1)

studentList <- studentList %>% mutate(
  `Tuition policy` = ifelse(
    is.na(`Tuition policy`), 
    "Does not vary tuition by in-state status", 
    `Tuition policy`
  )
)

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

#### Handle "U.S. Nonresident" and "Race/ethnicity unknown" #### 

assignRandomRace <- rbind(
  
  # White (48 out of 100)
  data.frame(
    `2 Digits` = c(
      "00", "01", "02", "03", "04", "05", "06", "07", "08", "09", 
      "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", 
      "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", 
      "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", 
      "40", "41", "42", "43", "44", "45", "46", "47" 
    ), 
    `Race substitution` = rep("White"), 
    check.names=FALSE
  ),
  
  # Black (13 out of 100)
  data.frame(
    `2 Digits` = c(
      "48", "49", 
      "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", 
      "60"
    ), 
    `Race substitution` = rep("Black or African American"), 
    check.names=FALSE
  ),
  
  # Hispanic (21 out of 100) 
  data.frame(
    `2 Digits` = c(
      "61", "62", "63", "64", "65", "66", "67", "68", "69", 
      "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", 
      "80", "81"
    ), 
    `Race substitution` = rep("Hispanic or Latino"), 
    check.names=FALSE
  ),
  
  # Asian (10 out of 100) 
  data.frame(
    `2 Digits` = c(
      "82", "83", "84", "85", "86", "87", "88", "89", 
      "90", "91" 
    ), 
    `Race substitution` = rep("Asian"), 
    check.names=FALSE
  ),
  
  # Native American (1 out of 100)
  data.frame(
    `2 Digits` = c(
      "92"
    ), 
    `Race substitution` = rep("American Indian or Alaska Native"), 
    check.names=FALSE
  ),
  
  # Native Hawaiian/other Pacific Islander (1 out of 100)
  data.frame(
    `2 Digits` = c(
      "93"
    ), 
    `Race substitution` = rep("Native Hawaiian/other Pacific Islander"), 
    check.names=FALSE
  ),
  
  # More than one race (6 out of 100)
  data.frame(
    `2 Digits` = c(
      "94", "95", "96", "97", "98", "99"
    ), 
    `Race substitution` = rep("More than one race"), 
    check.names=FALSE
  )
) 

studentList <- studentList %>% mutate(
  `2 Digits` = paste(
    sample((0:9), 1), 
    sample((0:9), 1), 
    sep=""
  )
)

studentList <- left_join(x=studentList, y=assignRandomRace, by="2 Digits")
rm(assignRandomRace)

studentList <- studentList %>% mutate(
  `Race NPSAS` = ifelse(
    `Race` %in% c("U.S. Nonresident", "Race/ethnicity unknown"), 
    `Race substitution`, 
    `Race`
  )  
)

studentList <- studentList %>% select(
  -(`2 Digits`),
  -(`Race substitution`)
)
 
#### End #### 

#############################################
#### Predictions from regressions: Set 1 ####
#############################################

#### Save current dataset in case reset is needed ####

studentListSave <- studentList

#### End ####

#### Write function to display distribution as percentages ####

showDistribution <- function(variableName){
  
  tempDF <- studentList %>% select(
    all_of(variableName)
  ) %>% mutate(
    `Count` = rep(1)
  )
  names(tempDF)[1] <- "InterestVar"
  
  totalStudents <- sum(tempDF$`Count`)
  
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

showPercentiles <- function(variableName, removeZeros){
  
  tempDF <- studentList %>% select(
    all_of(variableName)
  )
  names(tempDF)[1] <- "InterestVar"
  
  if(removeZeros){
    tempDF <- tempDF %>% filter(
      `InterestVar` > 0
    )
  }
  
  print(quantile(tempDF$`InterestVar`, probs = seq(.1, .9, by = .1)))
  
  rm(tempDF)
  
}

#### End #### 

#### Regression 1a: Predict Zero-EFC ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "Zero-EFC",
  retrievalCode = "qnnsor",
  interceptRow = 17, 
  regressionType = "Logistic", 
  positiveClass = "Zero-EFC", 
  negativeClass = "Nonzero-EFC", 
  thresholdVal = 0.4178, # EDITED 08-29-2025 
  absoluteAdjustment = 0, 
  relativeAdjustment = 1, 
  showWork = FALSE,
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 31, 
  linkingVar2 = "Region NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 34, 
  endLine3 = 39, 
  linkingVar3 = "Race NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 42, 
  endLine4 = 46, 
  linkingVar4 = "Carnegie NPSAS",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 49, 
  endLine5 = 50, 
  linkingVar5 = "Enrollment intensity NPSAS",
  varType5 = "Categorical", 
  
  includeVar6 = TRUE, 
  startLine6 = 53, 
  endLine6 = 53, 
  linkingVar6 = "Gender",
  varType6 = "Categorical", 
  
  includeVar7 = FALSE,
  startLine7 = NA, 
  endLine7 = NA, 
  linkingVar7 = NA,
  varType7 = "", 
  
  includeVar8 = FALSE,
  startLine8 = NA, 
  endLine8 = NA, 
  linkingVar8 = NA,
  varType8 = "",
  
  includeVar9 = FALSE,
  startLine9 = NA, 
  endLine9 = NA, 
  linkingVar9 = NA,
  varType9 = ""
)

#### End #### 

#### Regression 1b: Predict EFC ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "EFC",
  retrievalCode = "xtmjuz",
  interceptRow = 17, 
  regressionType = "Linear", 
  positiveClass = "", 
  negativeClass = "", 
  thresholdVal = 0.5, 
  absoluteAdjustment = 0, 
  relativeAdjustment = 1, 
  showWork = FALSE,
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 31, 
  linkingVar2 = "Region NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 34, 
  endLine3 = 39, 
  linkingVar3 = "Race NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 42, 
  endLine4 = 46, 
  linkingVar4 = "Carnegie NPSAS",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 49, 
  endLine5 = 50, 
  linkingVar5 = "Enrollment intensity NPSAS",
  varType5 = "Categorical", 
  
  includeVar6 = TRUE, 
  startLine6 = 53, 
  endLine6 = 53, 
  linkingVar6 = "Gender",
  varType6 = "Categorical", 
  
  includeVar7 = FALSE,
  startLine7 = NA, 
  endLine7 = NA, 
  linkingVar7 = NA,
  varType7 = "", 
  
  includeVar8 = FALSE,
  startLine8 = NA, 
  endLine8 = NA, 
  linkingVar8 = NA,
  varType8 = "",
  
  includeVar9 = FALSE,
  startLine9 = NA, 
  endLine9 = NA, 
  linkingVar9 = NA,
  varType9 = ""
)

#### End #### 

#### Combine 1a and 1b ####

studentList <- studentList %>% mutate(
  `EFC` = ifelse(`Zero-EFC` == "Zero-EFC", 0, `EFC`)
) 

#### End #### 

#### Calibrate EFC: Bring down high values  #### 

studentList <- studentList %>% mutate(
  `EFC` = ifelse(
    between(`EFC`, 1, 15000),
    `EFC` * 0.1, 
    `EFC`
  )
)

studentList <- studentList %>% mutate(
  `EFC` = ifelse(
    between(`EFC`, 15001, 18000), 
    `EFC` * 0.2, 
    `EFC`
  )
)

studentList <- studentList %>% mutate(
  `EFC` = ifelse(
    between(`EFC`, 18001, 20000), 
    `EFC` * 0.3, 
    `EFC`
  )
)

studentList <- studentList %>% mutate(
  `EFC` = ifelse(
    between(`EFC`, 20001, 22000), 
    `EFC` * 0.4, 
    `EFC`
  )
)

studentList <- studentList %>% mutate(
  `EFC` = ifelse(
    between(`EFC`, 22001, 24000), 
    `EFC` * 0.6, 
    `EFC`
  )
)

studentList <- studentList %>% mutate(
  `EFC` = ifelse(
    between(`EFC`, 24001, 26000), 
    `EFC` * 0.75, 
    `EFC`
  )
)

studentList <- studentList %>% mutate(
  `EFC` = ifelse(
    between(`EFC`, 26001, 28000), 
    `EFC` * 0.9, 
    `EFC`
  )
)

#### End #### 

#### Calibrate EFC: Bring up low values #### 

studentList <- studentList %>% mutate(
  `EFC` = ifelse(
    between(`EFC`, 34001, 36000), 
    `EFC` * 3.2, 
    `EFC`
  )
)

studentList <- studentList %>% mutate(
  `EFC` = ifelse(
    between(`EFC`, 33001, 34000),
    `EFC` * 2.5, 
    `EFC`
  )
)

studentList <- studentList %>% mutate(
  `EFC` = ifelse(
    between(`EFC`, 32001, 33000),
    `EFC` * 2, 
    `EFC`
  )
)

studentList <- studentList %>% mutate(
  `EFC` = ifelse(
    between(`EFC`, 31001, 32000), 
    `EFC` * 1.75, 
    `EFC`
  )
)

studentList <- studentList %>% mutate(
  `EFC` = ifelse(
    between(`EFC`, 30501, 31000), 
    `EFC` * 1.45, 
    `EFC`
  )
)

studentList <- studentList %>% mutate(
  `EFC` = ifelse(
    between(`EFC`, 30001, 30500), 
    `EFC` * 1.25, 
    `EFC`
  )
)

studentList <- studentList %>% mutate(
  `EFC` = ifelse(
    between(`EFC`, 29001, 30000), 
    `EFC` * 1.1, 
    `EFC`
  )
)

studentList <- studentList %>% mutate(
  `EFC` = ifelse(
    between(`EFC`, 28001, 29000), 
    `EFC` * 1, 
    `EFC`
  )
)

#### End #### 

#### Calibrate EFC: Bring down high values #### 

studentList <- studentList %>% mutate(
  `EFC` = ifelse(
    between(`EFC`, 1, 3200), 
    `EFC` * 0.3, 
    `EFC`
  )
)

studentList <- studentList %>% mutate(
  `EFC` = ifelse(
    between(`EFC`, 3201, 5800), 
    `EFC` * 0.7, 
    `EFC`
  )
)

studentList <- studentList %>% mutate(
  `EFC` = ifelse(
    between(`EFC`, 15001, 20000), 
    `EFC` * 0.75, 
    `EFC`
  )
)

studentList <- studentList %>% mutate(
  `EFC` = ifelse(
    between(`EFC`, 20001, 26000), 
    `EFC` * 0.8, 
    `EFC`
  )
)

#### End #### 

# #### Check EFC distribution ####
# 
# showDistribution("Zero-EFC")
# studentList <- studentList %>% select(-(`Zero-EFC`))
# 
# showPercentiles("EFC", removeZeros=TRUE)
# 
# #### End #### 

#### Regression 2: Predict Tuition Jurisdiction ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "Tuition jurisdiction",
  retrievalCode = "pwappp",
  interceptRow = 17, 
  regressionType = "Logistic", 
  positiveClass = "In-state tuition", 
  negativeClass = "Out-of-state tuition",  
  thresholdVal = 0.79, # EDITED FROM 0.5 
  absoluteAdjustment = 0, 
  relativeAdjustment = 1,
  showWork = FALSE, 
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 31, 
  linkingVar2 = "Region NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 34, 
  endLine3 = 39, 
  linkingVar3 = "Race NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 42, 
  endLine4 = 46, 
  linkingVar4 = "Carnegie NPSAS",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 49, 
  endLine5 = 50, 
  linkingVar5 = "Enrollment intensity NPSAS",
  varType5 = "Categorical", 
  
  includeVar6 = TRUE, 
  startLine6 = 53, 
  endLine6 = 53, 
  linkingVar6 = "Gender",
  varType6 = "Categorical", 
  
  includeVar7 = TRUE,
  startLine7 = 55, 
  endLine7 = 55, 
  linkingVar7 = "EFC",
  varType7 = "Numeric", 
  
  includeVar8 = FALSE,
  startLine8 = NA, 
  endLine8 = NA, 
  linkingVar8 = NA,
  varType8 = "",
  
  includeVar9 = FALSE,
  startLine9 = NA, 
  endLine9 = NA, 
  linkingVar9 = NA,
  varType9 = ""
)

studentList <- studentList %>% mutate(
  `Tuition jurisdiction` = ifelse(
    `Tuition policy` == "Does not vary tuition by in-state status", "No differential tuition charged", `Tuition jurisdiction`
  )
)

#### End #### 

# #### Check tuition jurisdiction distribution ####
# 
# showDistribution("Tuition jurisdiction")
# 
# #### End #### 

#### Integrate IPEDS data on tuition jurisdiction #### 

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/IPEDS Data")

cost2 <- read.csv(
  "cost2_2024.csv", 
  header=TRUE
) %>% select(
  `UNITID`,   #	Unique identification number of the institution
  `SCFA11N`,  #	Number of students in fall cohort who are paying in-district tuition rates
  `SCFA12N`,  #	Number of students in fall cohort who are paying in-state tuititon rates
  `SCFA13N`   #	Number of students in fall cohort who are paying out-of-state tuition rates
) %>% mutate(
  `SCFA11N` = ifelse(
    is.na(`SCFA11N`), 
    0,
    `SCFA11N`
  ), 
  `SCFA12N` = ifelse(
    is.na(`SCFA12N`), 
    0,
    `SCFA12N`
  ), 
  `SCFA13N` = ifelse(
    is.na(`SCFA13N`), 
    0,
    `SCFA13N`
  )
) %>% mutate(
  `In-state students` = `SCFA11N` + `SCFA12N`
) %>% select(
  -(`SCFA11N`), -(`SCFA12N`)
) %>% rename(
  `Out-of-state students` = `SCFA13N`
) %>% mutate(
  `In-state share` = `In-state students` / (`In-state students` + `Out-of-state students`)
)

# This is just for testing (1-19-2026), it will be redundant when running the full dataset 
cost2 <- cost2 %>% filter(
  `UNITID` %in% studentList$UNITID
)

varyTuition <- studentList %>% filter(
  `Tuition policy` == "Varies tuition by in-state status", 
  `UNITID` %in% cost2$`UNITID`
)

for(i in (1:length(unique(varyTuition$UNITID)))){

  if(i %% 100==1){print(paste("Starting number ", comma(i), " out of ", comma(length(unique(varyTuition$UNITID))), " colleges.", sep=""))}
  
  tempDF <- studentList %>% filter(
    `UNITID` == unique(varyTuition$UNITID)[i]
  ) %>% rename(
    `Actual` = `Tuition jurisdiction`
  ) %>% mutate(
    `Random digits` = paste(
      sample((0:9), 1), 
      sample((0:9), 1), 
      sample((0:9), 1), 
      sep=""
    )
  )
  
  tempDF <- tempDF %>% arrange(
    `Actual`, 
    `Random digits`
  ) %>% select(
    -(`Random digits`)
  )
  
  for.actual.share <- aggregate(
    data=tempDF, 
    `UNITID` ~ `Actual`, 
    FUN=length
  ) %>% pivot_wider(
    names_from=`Actual`, 
    values_from=`UNITID`
  )
  if(ncol(for.actual.share)==1 & names(for.actual.share)[1]=="In-state tuition"){
    actual.share <- 1
  }
  if(ncol(for.actual.share)==1 & names(for.actual.share)[1]=="Out-of-state tuition"){
    actual.share <- 0
  }
  if(ncol(for.actual.share)==2){
    for.actual.share <- for.actual.share %>% mutate(
      `Actual share` = `In-state tuition` / (`In-state tuition` + `Out-of-state tuition`)
    )
    actual.share <- for.actual.share$`Actual share`[1]
  }
  rm(for.actual.share)
  
  student.count <- nrow(tempDF)
  correct.share <- cost2 %>% filter(
    `UNITID` == unique(varyTuition$UNITID)[i]
  )
  correct.share <- correct.share$`In-state share`[1]
  
  tempDF <- tempDF %>% mutate(
    `Expected` = c(
      rep("In-state tuition", round(student.count * correct.share)), 
      rep("Out-of-state tuition", round(student.count * (1-correct.share)))
    )
  )
  tempDF <- tempDF %>% filter(
    `Expected` != `Actual`
  )
  
  if(nrow(tempDF) > 0){
    if(actual.share < correct.share){
      imputed.label <- "In-state tuition"
    }else{
      imputed.label <- "Out-of-state tuition"
    }
    studentList <- studentList %>% mutate(
      `Tuition jurisdiction` = ifelse(
        `Effy-student index` %in% tempDF$`Effy-student index`, 
        imputed.label, 
        `Tuition jurisdiction`
      )
    )
    if(exists("imputed.label")){
      rm(imputed.label)
    }
  }
  rm(tempDF, actual.share, correct.share, student.count)
  
}
rm(varyTuition, i, cost2)

#### End #### 

#### Regression 3: Predict Tuition and Fees Paid ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "Tuition and fees paid",
  retrievalCode = "btdqqq",
  interceptRow = 17, 
  regressionType = "Linear", 
  positiveClass = "", 
  negativeClass = "",  
  thresholdVal = 0.5, 
  absoluteAdjustment = 2600,
  relativeAdjustment = 1,
  showWork = FALSE, 
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 31, 
  linkingVar2 = "Region NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 34, 
  endLine3 = 39, 
  linkingVar3 = "Race NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 42, 
  endLine4 = 46, 
  linkingVar4 = "Carnegie NPSAS",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 49, 
  endLine5 = 50, 
  linkingVar5 = "Enrollment intensity NPSAS",
  varType5 = "Categorical", 
  
  includeVar6 = TRUE, 
  startLine6 = 53, 
  endLine6 = 53, 
  linkingVar6 = "Gender",
  varType6 = "Categorical", 
  
  includeVar7 = TRUE,
  startLine7 = 55, 
  endLine7 = 55, 
  linkingVar7 = "EFC",
  varType7 = "Numeric", 
  
  includeVar8 = TRUE,
  startLine8 = 58, 
  endLine8 = 59, 
  linkingVar8 = "Tuition jurisdiction",
  varType8 = "Categorical",
  
  includeVar9 = FALSE,
  startLine9 = NA, 
  endLine9 = NA, 
  linkingVar9 = NA,
  varType9 = ""
)

studentList <- studentList %>% mutate(
  `Tuition and fees paid` = ifelse(`Tuition and fees paid` < 0, 0, `Tuition and fees paid`)
)

#### End #### 

#### Calibrate tuition and fees paid: Lower high values ####

studentList <- studentList %>% mutate(
  `Tuition and fees paid` = ifelse(
    between(`Tuition and fees paid`, 1000, 3500),
    `Tuition and fees paid` * 0.75, 
    `Tuition and fees paid`
  )
)

studentList <- studentList %>% mutate(
  `Tuition and fees paid` = ifelse(
    between(`Tuition and fees paid`, 3501, 8500),
    `Tuition and fees paid` * 0.5, 
    `Tuition and fees paid`
  )
)

studentList <- studentList %>% mutate(
  `Tuition and fees paid` = ifelse(
    between(`Tuition and fees paid`, 8501, 20000),
    `Tuition and fees paid` * 0.6, 
    `Tuition and fees paid`
  )
)

studentList <- studentList %>% mutate(
  `Tuition and fees paid` = ifelse(
    between(`Tuition and fees paid`, 20001, 35000),
    `Tuition and fees paid` * 0.65, 
    `Tuition and fees paid`
  )
)

studentList <- studentList %>% mutate(
  `Tuition and fees paid` = ifelse(
    `Tuition and fees paid` > 35000,
    `Tuition and fees paid` * 0.8, 
    `Tuition and fees paid`
  )
)

#### End #### 

# #### Check tuition and fees distribution ####
# 
# showPercentiles("Tuition and fees paid", removeZeros=FALSE)
# 
# #### End #### 

#### Save current dataset in case reset is needed ####

studentListSave <- studentList

#### End ####

#### Randomly assign in-district status ####

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/IPEDS Data")

studentList <- studentList %>% mutate(
  `Tuition jurisdiction (with in-district)` = `Tuition jurisdiction`
)

cost2 <- read.csv(
  "cost2_2024.csv", 
  header=TRUE
) %>% select(
  `UNITID`,   #	Unique identification number of the institution
  `SCFA11N`,  #	Number of students in fall cohort who are paying in-district tuition rates
  `SCFA12N`   #	Number of students in fall cohort who are paying in-state tuititon rates
) %>% mutate(
  `SCFA11N` = ifelse(
    is.na(`SCFA11N`), 
    0,
    `SCFA11N`
  ), 
  `SCFA12N` = ifelse(
    is.na(`SCFA12N`), 
    0,
    `SCFA12N`
  )
) %>% rename(
  `In-district students` = `SCFA11N`,
  `In-state students` = `SCFA12N`
) %>% filter(
  `In-district students` > 0
) %>% mutate(
  `In-district share` = `In-district students` / (`In-district students` + `In-state students`)
)

# This is just for testing (1-19-2026), it will be redundant when running the full dataset 
cost2 <- cost2 %>% filter(
  `UNITID` %in% studentList$UNITID
)

for(i in (1:length(unique(cost2$UNITID)))){
  
  if(i %% 100==1){print(paste("Starting number ", comma(i), " out of ", comma(length(unique(cost2$UNITID))), " colleges.", sep=""))}
  
  tempDF <- studentList %>% filter(
    `UNITID` == unique(cost2$UNITID)[i], 
    `Tuition jurisdiction`=="In-state tuition"
  ) %>% mutate(
    `Random digits` = paste(
      sample((0:9), 1), 
      sample((0:9), 1), 
      sample((0:9), 1), 
      sep=""
    )
  ) %>% arrange(
    `Random digits`
  ) %>% select(
    -(`Random digits`)
  )
  
  student.count <- nrow(tempDF)
  correct.share <- cost2 %>% filter(
    `UNITID` == unique(cost2$UNITID)[i]
  )
  correct.share <- correct.share$`In-district share`[1]
  
  # Number of rows to change 
  tempDF <- tempDF[(1:round(correct.share * student.count)), ]
  
  if(nrow(tempDF) > 1){
    studentList <- studentList %>% mutate(
      `Tuition jurisdiction (with in-district)` = ifelse(
        `Effy-student index` %in% tempDF$`Effy-student index`,
        "In-district tuition", 
        `Tuition jurisdiction (with in-district)`
      )
    )
  }
  rm(tempDF, correct.share, student.count)
  
}
rm(i, cost2)

#### End #### 

#### Integrate IPEDS data on tuition ####

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/IPEDS Data")

cost1 <- read.csv(
  "cost1_2024.csv", 
  header=TRUE
) %>% select(
  `UNITID`, 
  `TUITION1`, #	 In-district average tuition for full-time undergraduates
  `FEE1`,     #	 In-district required fees for full-time undergraduates
  `TUITION2`, #	In-state average tuition for full-time undergraduates
  `FEE2`,     #	In-state required fees for full-time undergraduates
  `TUITION3`, #	Out-of-state average tuition for full-time undergraduates
  `FEE3`      #	Out-of-state required fees for full-time undergraduates
) %>% filter(
  # Remove if they're all NAs 
  ((is.na(`TUITION1`)) & (is.na(`FEE1`)) & (is.na(`TUITION2`)) & (is.na(`FEE2`)) & (is.na(`TUITION3`)) & (is.na(`FEE3`)))==FALSE 
) %>% mutate(
  `TUITION1` = ifelse(is.na(`TUITION1`), 0, `TUITION1`), 
  `FEE1` = ifelse(is.na(`FEE1`), 0, `FEE1`), 
  `TUITION2` = ifelse(is.na(`TUITION2`), 0, `TUITION2`), 
  `FEE2` = ifelse(is.na(`FEE2`), 0, `FEE2`), 
  `TUITION3` = ifelse(is.na(`TUITION3`), 0, `TUITION3`), 
  `FEE3` = ifelse(is.na(`FEE3`), 0, `FEE3`)
) %>% mutate(
  `In-district tuition and fees` = `TUITION1` + `FEE1`, 
  `In-state tuition and fees` = `TUITION2` + `FEE2`, 
  `Out-of-state tuition and fees` = `TUITION3` + `FEE3`
) %>% select(
  -(`TUITION1`), -(`FEE1`),
  -(`TUITION2`), -(`FEE2`), 
  -(`TUITION3`), -(`FEE3`)
) %>% mutate(
  `No differential tuition charged` = ifelse(
    (`In-district tuition and fees` == `In-state tuition and fees`) & (`In-state tuition and fees` == `Out-of-state tuition and fees`), 
    `In-state tuition and fees`,
    NA
  )
) %>% rename(
  `In-district tuition and fees (IPEDS)` = `In-district tuition and fees`, 
  `In-state tuition and fees (IPEDS)` = `In-state tuition and fees`, 
  `Out-of-state tuition and fees (IPEDS)` = `Out-of-state tuition and fees`, 
  `No differential tuition charged (IPEDS)` = `No differential tuition charged`
)

fullTimers <- studentList %>% filter(
  `Enrollment intensity NPSAS`=="Exclusively full-time"
)
fullTimers <- aggregate(
  data=fullTimers, 
  `Tuition and fees paid` ~ `UNITID` + `Tuition jurisdiction (with in-district)`, 
  FUN=mean
) %>% pivot_wider(
  id_cols=c(`UNITID`), 
  names_from=`Tuition jurisdiction (with in-district)`, 
  values_from=`Tuition and fees paid`
) %>% rename(
  `In-district tuition and fees (SDS)` = `In-district tuition`, 
  `In-state tuition and fees (SDS)` = `In-state tuition`, 
  `Out-of-state tuition and fees (SDS)` = `Out-of-state tuition`, 
  `No differential tuition charged (SDS)` = `No differential tuition charged`
)

ratios <- inner_join(x=cost1, y=fullTimers, by="UNITID")
rm(cost1, fullTimers)

ratios <- ratios %>% mutate(
  `Ratio, IPEDS to SDS: In-district` = ifelse(
    (is.na(`In-district tuition and fees (IPEDS)`)==FALSE) & (is.na(`In-district tuition and fees (SDS)`)==FALSE) & (`In-district tuition and fees (SDS)` > 0), 
    `In-district tuition and fees (IPEDS)` / `In-district tuition and fees (SDS)`, 
    NA
  ), 
  `Ratio, IPEDS to SDS: In-state` = ifelse(
    (is.na(`In-state tuition and fees (IPEDS)`)==FALSE) & (is.na(`In-state tuition and fees (SDS)`)==FALSE)  & (`In-state tuition and fees (SDS)` > 0), 
    `In-state tuition and fees (IPEDS)` / `In-state tuition and fees (SDS)`, 
    NA
  ), 
  `Ratio, IPEDS to SDS: Out-of-state` = ifelse(
    (is.na(`Out-of-state tuition and fees (IPEDS)`)==FALSE) & (is.na(`Out-of-state tuition and fees (SDS)`)==FALSE)  & (`Out-of-state tuition and fees (SDS)` > 0), 
    `Out-of-state tuition and fees (IPEDS)` / `Out-of-state tuition and fees (SDS)`, 
    NA
  ), 
  `Ratio, IPEDS to SDS: No differential` = ifelse(
    (is.na(`No differential tuition charged (IPEDS)`)==FALSE) & (is.na(`No differential tuition charged (SDS)`)==FALSE)  & (`No differential tuition charged (SDS)` > 0), 
    `No differential tuition charged (IPEDS)` / `No differential tuition charged (SDS)`, 
    NA
  )
) %>% select(
  `UNITID`,
  `Ratio, IPEDS to SDS: In-district`,
  `Ratio, IPEDS to SDS: In-state`, 
  `Ratio, IPEDS to SDS: Out-of-state`, 
  `Ratio, IPEDS to SDS: No differential`
)

studentList <- left_join(x=studentList, y=ratios, by="UNITID")
rm(ratios)

studentList <- studentList %>% mutate(
  `Appropriate ratio` = ifelse(
    `Tuition jurisdiction (with in-district)`=="In-district tuition", 
    `Ratio, IPEDS to SDS: In-district`,
    ifelse(
      `Tuition jurisdiction (with in-district)`=="In-state tuition", 
      `Ratio, IPEDS to SDS: In-state`,
      ifelse(
        `Tuition jurisdiction (with in-district)`=="Out-of-state tuition", 
        `Ratio, IPEDS to SDS: Out-of-state`, 
        ifelse(
          `Tuition jurisdiction (with in-district)`=="No differential tuition charged", 
          `Ratio, IPEDS to SDS: No differential`, 
          1
        )
      )
    )
  )
) %>% mutate(
  `Appropriate ratio` = ifelse(
    is.na(`Appropriate ratio`), 
    1,
    `Appropriate ratio`
  )
) %>% mutate(
  `Tuition and fees paid` = `Tuition and fees paid` * `Appropriate ratio`
) %>% select(
  -(`Ratio, IPEDS to SDS: In-district`), 
  -(`Ratio, IPEDS to SDS: In-state`), 
  -(`Ratio, IPEDS to SDS: Out-of-state`),
  -(`Ratio, IPEDS to SDS: No differential`), 
  -(`Appropriate ratio`)
)

#### End ####

#### Regression 4: Predict Age ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "Age",
  retrievalCode = "swdwhv",
  interceptRow = 17, 
  regressionType = "Linear", 
  positiveClass = "", 
  negativeClass = "",  
  thresholdVal = 0.5, 
  absoluteAdjustment = 0, 
  relativeAdjustment = 1,
  showWork = FALSE, 
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 31, 
  linkingVar2 = "Region NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 34, 
  endLine3 = 39, 
  linkingVar3 = "Race NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 42, 
  endLine4 = 46, 
  linkingVar4 = "Carnegie NPSAS",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 49, 
  endLine5 = 50, 
  linkingVar5 = "Enrollment intensity NPSAS",
  varType5 = "Categorical", 
  
  includeVar6 = TRUE, 
  startLine6 = 53, 
  endLine6 = 53, 
  linkingVar6 = "Gender",
  varType6 = "Categorical", 
  
  includeVar7 = TRUE,
  startLine7 = 55, 
  endLine7 = 55, 
  linkingVar7 = "EFC",
  varType7 = "Numeric", 
  
  includeVar8 = TRUE,
  startLine8 = 58, 
  endLine8 = 59, 
  linkingVar8 = "Tuition jurisdiction",
  varType8 = "Categorical",
  
  includeVar9 = TRUE,
  startLine9 = 61, 
  endLine9 = 61, 
  linkingVar9 = "Tuition and fees paid",
  varType9 = "Numeric"
)

# RANDOMLY MAKE HALF OF THEM YOUNGER 
studentList <- studentList %>% mutate(
  `Age` = ifelse(
    `Effy index` %% 2 == 0,
    17 + ((`Age` - 17) * 0.5),  # Reduce the amount over 17 by 50% 
    `Age`
  )
)

studentList <- studentList %>% mutate(
  `Age` = ifelse(`Age` < 17, 17, `Age`)
)

#### End #### 

#### Calibrate age: Lower high values ####

studentList <- studentList %>% mutate(
  `Age` = ifelse(
    between(`Age`, 19, 19.99),
    `Age` * 0.9, 
    `Age`
  )
)

studentList <- studentList %>% mutate(
  `Age` = ifelse(
    between(`Age`, 20, 24.99),
    `Age` * 0.95, 
    `Age`
  )
)

#### End #### 

#### Calibrate age: Raise low values #### 

studentList <- studentList %>% mutate(
  `Age` = ifelse(
    `Age` > 29.41,
    `Age` * 1.25, 
    `Age`
  )
)

studentList <- studentList %>% mutate(
  `Age` = ifelse(
    between(`Age`, 29, 29.4),
    `Age` * 1.2, 
    `Age`
  )
)

studentList <- studentList %>% mutate(
  `Age` = ifelse(
    between(`Age`, 28, 28.99),
    `Age` * 1.15, 
    `Age`
  )
)

studentList <- studentList %>% mutate(
  `Age` = ifelse(
    between(`Age`, 26, 27.99),
    `Age` * 1.1, 
    `Age`
  )
)

#### End #### 

# #### Check age distribution #### 
# 
# showPercentiles("Age", removeZeros=FALSE)
# 
# #### End #### 

#### Regression 5: Predict Citizenship ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "Citizenship",
  retrievalCode = "fbmoox",
  interceptRow = 17, 
  regressionType = "Logistic", 
  positiveClass = "Citizen or eligible non-citizen", 
  negativeClass = "Non-citizen",  
  thresholdVal = 0.3, 
  absoluteAdjustment = 0, 
  relativeAdjustment = 1,
  showWork = FALSE, 
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 31, 
  linkingVar2 = "Region NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 34, 
  endLine3 = 39, 
  linkingVar3 = "Race NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 42, 
  endLine4 = 46, 
  linkingVar4 = "Carnegie NPSAS",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 49, 
  endLine5 = 50, 
  linkingVar5 = "Enrollment intensity NPSAS",
  varType5 = "Categorical", 
  
  includeVar6 = TRUE, 
  startLine6 = 53, 
  endLine6 = 53, 
  linkingVar6 = "Gender",
  varType6 = "Categorical", 
  
  includeVar7 = TRUE,
  startLine7 = 55, 
  endLine7 = 55, 
  linkingVar7 = "EFC",
  varType7 = "Numeric", 
  
  includeVar8 = TRUE,
  startLine8 = 58, 
  endLine8 = 59, 
  linkingVar8 = "Tuition jurisdiction",
  varType8 = "Categorical",
  
  includeVar9 = TRUE,
  startLine9 = 61, 
  endLine9 = 61, 
  linkingVar9 = "Tuition and fees paid",
  varType9 = "Numeric"
)

studentList <- studentList %>% mutate(
  `Citizenship` = ifelse(
    `Race` == "U.S. Nonresident", 
    "Non-citizen", 
    `Citizenship`
  )
)

#### End #### 

# #### Check citizenship distribution #### 
# 
# showDistribution("Citizenship")
# 
# #### End #### 

#### Regression 6: Veteran Status ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "Veteran status",
  retrievalCode = "cxjrap",
  interceptRow = 17, 
  regressionType = "Logistic", 
  positiveClass = "Veteran", 
  negativeClass = "Not a veteran", 
  thresholdVal = 0.11, # EDITED 
  absoluteAdjustment = 0, 
  relativeAdjustment = 1,
  showWork = FALSE, 
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 31, 
  linkingVar2 = "Region NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 34, 
  endLine3 = 39, 
  linkingVar3 = "Race NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 42, 
  endLine4 = 46, 
  linkingVar4 = "Carnegie NPSAS",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 49, 
  endLine5 = 50, 
  linkingVar5 = "Enrollment intensity NPSAS",
  varType5 = "Categorical", 
  
  includeVar6 = TRUE, 
  startLine6 = 53, 
  endLine6 = 53, 
  linkingVar6 = "Gender",
  varType6 = "Categorical", 
  
  includeVar7 = TRUE,
  startLine7 = 55, 
  endLine7 = 55, 
  linkingVar7 = "EFC",
  varType7 = "Numeric", 
  
  includeVar8 = TRUE,
  startLine8 = 58, 
  endLine8 = 59, 
  linkingVar8 = "Tuition jurisdiction",
  varType8 = "Categorical",
  
  includeVar9 = TRUE,
  startLine9 = 61, 
  endLine9 = 61, 
  linkingVar9 = "Tuition and fees paid",
  varType9 = "Numeric"
)

studentList <- studentList %>% mutate(
  `Veteran status` = ifelse(
    `Citizenship`=="Non-citizen", 
    "Not a veteran", 
    `Veteran status`
  )
)

#### End #### 

# #### Check veteran status distribution #### 
# 
# showDistribution("Veteran status")
# 
# #### End #### 

#### Regression 7: Predict Dependency Status ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "Dependency status",
  retrievalCode = "mbryhw",
  interceptRow = 17, 
  regressionType = "Logistic", 
  positiveClass = "Dependent", 
  negativeClass = "Independent",  
  thresholdVal = 0.818, 
  absoluteAdjustment = 0, 
  relativeAdjustment = 1,
  showWork = FALSE, 
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 31, 
  linkingVar2 = "Region NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 34, 
  endLine3 = 39, 
  linkingVar3 = "Race NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 42, 
  endLine4 = 46, 
  linkingVar4 = "Carnegie NPSAS",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 49, 
  endLine5 = 50, 
  linkingVar5 = "Enrollment intensity NPSAS",
  varType5 = "Categorical", 
  
  includeVar6 = TRUE, 
  startLine6 = 53, 
  endLine6 = 53, 
  linkingVar6 = "Gender",
  varType6 = "Categorical", 
  
  includeVar7 = TRUE,
  startLine7 = 55, 
  endLine7 = 55, 
  linkingVar7 = "EFC",
  varType7 = "Numeric", 
  
  includeVar8 = TRUE,
  startLine8 = 58, 
  endLine8 = 59, 
  linkingVar8 = "Tuition jurisdiction",
  varType8 = "Categorical",
  
  includeVar9 = TRUE,
  startLine9 = 61, 
  endLine9 = 61, 
  linkingVar9 = "Tuition and fees paid",
  varType9 = "Numeric"
)

studentList <- studentList %>% mutate(
  `Dependency status` = ifelse(
    `Age` >= 24, "Independent", `Dependency status`
  )
) %>% mutate(
  `Dependency status` = ifelse(
    `Veteran status` == "Veteran", "Independent", `Dependency status`
  )
)

#### End #### 

# #### Check dependency status distribution ####
# 
# showDistribution("Dependency status")
# 
# #### End #### 

#### Regression 8: Applied for Federal Aid ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "Applied for federal aid",
  retrievalCode = "jvlplk",
  interceptRow = 17, 
  regressionType = "Logistic", 
  positiveClass = "Yes", 
  negativeClass = "No",  
  thresholdVal = 0.558, # EDITED 
  absoluteAdjustment = 0, 
  relativeAdjustment = 1,
  showWork = FALSE, 
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 31, 
  linkingVar2 = "Region NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 34, 
  endLine3 = 39, 
  linkingVar3 = "Race NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 42, 
  endLine4 = 46, 
  linkingVar4 = "Carnegie NPSAS",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 49, 
  endLine5 = 50, 
  linkingVar5 = "Enrollment intensity NPSAS",
  varType5 = "Categorical", 
  
  includeVar6 = TRUE, 
  startLine6 = 53, 
  endLine6 = 53, 
  linkingVar6 = "Gender",
  varType6 = "Categorical", 
  
  includeVar7 = TRUE,
  startLine7 = 55, 
  endLine7 = 55, 
  linkingVar7 = "EFC",
  varType7 = "Numeric", 
  
  includeVar8 = TRUE,
  startLine8 = 58, 
  endLine8 = 59, 
  linkingVar8 = "Tuition jurisdiction",
  varType8 = "Categorical",
  
  includeVar9 = TRUE,
  startLine9 = 61, 
  endLine9 = 61, 
  linkingVar9 = "Tuition and fees paid",
  varType9 = "Numeric"
)

studentList <- studentList %>% mutate(
  `Applied for federal aid` = ifelse(
    `Citizenship`=="Non-citizen", "No", `Applied for federal aid`
  )
)

#### End #### 

# #### Check federal aid application distribution #### 
# 
# showDistribution("Applied for federal aid")
# 
# #### End #### 

#### Regression 9: Non-Tuition Expense Budget ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "Non-tuition expense budget",
  retrievalCode = "tekbez",
  interceptRow = 17, 
  regressionType = "Linear", 
  positiveClass = "", 
  negativeClass = "",  
  thresholdVal = 0.5, 
  absoluteAdjustment = 0, 
  relativeAdjustment = 1,
  showWork = FALSE, 
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 31, 
  linkingVar2 = "Region NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 34, 
  endLine3 = 39, 
  linkingVar3 = "Race NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 42, 
  endLine4 = 46, 
  linkingVar4 = "Carnegie NPSAS",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 49, 
  endLine5 = 50, 
  linkingVar5 = "Enrollment intensity NPSAS",
  varType5 = "Categorical", 
  
  includeVar6 = TRUE, 
  startLine6 = 53, 
  endLine6 = 53, 
  linkingVar6 = "Gender",
  varType6 = "Categorical", 
  
  includeVar7 = TRUE,
  startLine7 = 55, 
  endLine7 = 55, 
  linkingVar7 = "EFC",
  varType7 = "Numeric", 
  
  includeVar8 = TRUE,
  startLine8 = 58, 
  endLine8 = 59, 
  linkingVar8 = "Tuition jurisdiction",
  varType8 = "Categorical",
  
  includeVar9 = TRUE,
  startLine9 = 61, 
  endLine9 = 61, 
  linkingVar9 = "Tuition and fees paid",
  varType9 = "Numeric"
)

studentList <- studentList %>% mutate(
  `Non-tuition expense budget` = ifelse(`Non-tuition expense budget` < 0, 0, `Non-tuition expense budget`)
)

#### End #### 

#### Calibrate non-tuition budget: Lower high values #### 

studentList <- studentList %>% mutate(
  `Non-tuition expense budget` = ifelse(
    between(`Non-tuition expense budget`, 1, 4000), 
    `Non-tuition expense budget` * 0.5, 
    `Non-tuition expense budget`
  )
)

studentList <- studentList %>% mutate(
  `Non-tuition expense budget` = ifelse(
    between(`Non-tuition expense budget`, 4001, 7000), 
    `Non-tuition expense budget` * 0.7, 
    `Non-tuition expense budget`
  )
)

studentList <- studentList %>% mutate(
  `Non-tuition expense budget` = ifelse(
    between(`Non-tuition expense budget`, 7001, 8000),
    `Non-tuition expense budget` * 0.8,
    `Non-tuition expense budget`
  )
)

#### End #### 

#### Calibrate non-tuition budget: Raise low values #### 

studentList <- studentList %>% mutate(
  `Non-tuition expense budget` = ifelse(
    between(`Non-tuition expense budget`, 17001, 99999), 
    `Non-tuition expense budget` * 1.45, 
    `Non-tuition expense budget`
  )
)

studentList <- studentList %>% mutate(
  `Non-tuition expense budget` = ifelse(
    between(`Non-tuition expense budget`, 15001, 17000), 
    `Non-tuition expense budget` * 1.35, 
    `Non-tuition expense budget`
  )
)

studentList <- studentList %>% mutate(
  `Non-tuition expense budget` = ifelse(
    between(`Non-tuition expense budget`, 13501, 15000), 
    `Non-tuition expense budget` * 1.25, 
    `Non-tuition expense budget`
  )
)

studentList <- studentList %>% mutate(
  `Non-tuition expense budget` = ifelse(
    between(`Non-tuition expense budget`, 12001, 13500), 
    `Non-tuition expense budget` * 1.15, 
    `Non-tuition expense budget`
  )
)

studentList <- studentList %>% mutate(
  `Non-tuition expense budget` = ifelse(
    between(`Non-tuition expense budget`, 9001, 12000), 
    `Non-tuition expense budget` * 1.05, 
    `Non-tuition expense budget`
  )
)

#### End #### 

# #### Check non-tuition expense budget distribution #### 
# 
# showPercentiles("Non-tuition expense budget", removeZeros=FALSE)
# 
# #### End #### 

#############################################
#### Predictions from regressions: Set 2 ####
#############################################

#### Save current dataset in case reset is needed ####

studentListSave <- studentList

#### End ####

#### Regression 10: Receives Federal Grants ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "Receives federal grants",
  retrievalCode = "lgqfmf",
  interceptRow = 17, 
  regressionType = "Logistic", 
  positiveClass = "Yes", 
  negativeClass = "No",  
  thresholdVal = 0.35, 
  absoluteAdjustment = 0, 
  relativeAdjustment = 1,
  showWork = FALSE, 
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 31, 
  linkingVar2 = "Region NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 34, 
  endLine3 = 39, 
  linkingVar3 = "Race NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 42, 
  endLine4 = 46, 
  linkingVar4 = "Carnegie NPSAS",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 49, 
  endLine5 = 50, 
  linkingVar5 = "Enrollment intensity NPSAS",
  varType5 = "Categorical", 
  
  includeVar6 = TRUE, 
  startLine6 = 53, 
  endLine6 = 53, 
  linkingVar6 = "Gender",
  varType6 = "Categorical", 
  
  includeVar7 = TRUE,
  startLine7 = 55, 
  endLine7 = 55, 
  linkingVar7 = "EFC",
  varType7 = "Numeric", 
  
  includeVar8 = TRUE,
  startLine8 = 58, 
  endLine8 = 59, 
  linkingVar8 = "Tuition jurisdiction",
  varType8 = "Categorical",
  
  includeVar9 = TRUE,
  startLine9 = 61, 
  endLine9 = 61, 
  linkingVar9 = "Tuition and fees paid",
  varType9 = "Numeric"
)

studentList <- studentList %>% mutate(
  `Receives federal grants` = ifelse(
    (`Citizenship`=="Non-citizen") | (`Applied for federal aid`=="No"), "No", `Receives federal grants`
  )
)

#### End #### 

# #### Check receives federal grants distribution #### 
# 
# showDistribution("Receives federal grants")
# 
# #### End #### 

#### Regression 11A: Receives VA/DOD Grants (veterans) ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "Receives VA/DOD grants (veterans)",
  retrievalCode = "hvhxhi",
  interceptRow = 17, 
  regressionType = "Logistic", 
  positiveClass = "Yes", 
  negativeClass = "No",  
  thresholdVal = 0.3, # EDITED 
  absoluteAdjustment = 0, 
  relativeAdjustment = 1,
  showWork = FALSE, 
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 31, 
  linkingVar2 = "Region NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 34, 
  endLine3 = 39, 
  linkingVar3 = "Race NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 42, 
  endLine4 = 46, 
  linkingVar4 = "Carnegie NPSAS",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 49, 
  endLine5 = 50, 
  linkingVar5 = "Enrollment intensity NPSAS",
  varType5 = "Categorical", 
  
  includeVar6 = TRUE, 
  startLine6 = 53, 
  endLine6 = 53, 
  linkingVar6 = "Gender",
  varType6 = "Categorical", 
  
  includeVar7 = TRUE,
  startLine7 = 55, 
  endLine7 = 55, 
  linkingVar7 = "EFC",
  varType7 = "Numeric", 
  
  includeVar8 = TRUE,
  startLine8 = 58, 
  endLine8 = 59, 
  linkingVar8 = "Tuition jurisdiction",
  varType8 = "Categorical",
  
  includeVar9 = TRUE,
  startLine9 = 61, 
  endLine9 = 61, 
  linkingVar9 = "Tuition and fees paid",
  varType9 = "Numeric"
)

#### End #### 

#### Regression 11B: Receives VA/DOD Grants (non-veterans) ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "Receives VA/DOD grants (non-veterans)",
  retrievalCode = "mkfuco",
  interceptRow = 17, 
  regressionType = "Logistic", 
  positiveClass = "Yes", 
  negativeClass = "No",  
  thresholdVal = 0.02, 
  absoluteAdjustment = 0, 
  relativeAdjustment = 1,
  showWork = FALSE, 
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 31, 
  linkingVar2 = "Region NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 34, 
  endLine3 = 39, 
  linkingVar3 = "Race NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 42, 
  endLine4 = 46, 
  linkingVar4 = "Carnegie NPSAS",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 49, 
  endLine5 = 50, 
  linkingVar5 = "Enrollment intensity NPSAS",
  varType5 = "Categorical", 
  
  includeVar6 = TRUE, 
  startLine6 = 53, 
  endLine6 = 53, 
  linkingVar6 = "Gender",
  varType6 = "Categorical", 
  
  includeVar7 = TRUE,
  startLine7 = 55, 
  endLine7 = 55, 
  linkingVar7 = "EFC",
  varType7 = "Numeric", 
  
  includeVar8 = TRUE,
  startLine8 = 58, 
  endLine8 = 59, 
  linkingVar8 = "Tuition jurisdiction",
  varType8 = "Categorical",
  
  includeVar9 = TRUE,
  startLine9 = 61, 
  endLine9 = 61, 
  linkingVar9 = "Tuition and fees paid",
  varType9 = "Numeric"
)

#### End #### 

#### Regression 11: Combine 11A and 11B ####

studentList <- studentList %>% mutate(
  `Receives VA/DOD grants` = ifelse(
    `Veteran status`=="Veteran", 
    `Receives VA/DOD grants (veterans)`, 
    `Receives VA/DOD grants (non-veterans)`
  )
) %>% select(
  -(`Receives VA/DOD grants (veterans)`), 
  -(`Receives VA/DOD grants (non-veterans)`)
)

#### End #### 

# #### Check receives VA/DOD grants distribution #### 
# 
# showDistribution("Receives VA/DOD grants")
# 
# #### End #### 

#### Regression 12: Receives State Grants ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "Receives state grants",
  retrievalCode = "nczufm",
  interceptRow = 17, 
  regressionType = "Logistic", 
  positiveClass = "Yes", 
  negativeClass = "No",  
  thresholdVal = 0.295,  
  absoluteAdjustment = 0, 
  relativeAdjustment = 1,
  showWork = FALSE, 
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 31, 
  linkingVar2 = "Region NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 34, 
  endLine3 = 39, 
  linkingVar3 = "Race NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 42, 
  endLine4 = 46, 
  linkingVar4 = "Carnegie NPSAS",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 49, 
  endLine5 = 50, 
  linkingVar5 = "Enrollment intensity NPSAS",
  varType5 = "Categorical", 
  
  includeVar6 = TRUE, 
  startLine6 = 53, 
  endLine6 = 53, 
  linkingVar6 = "Gender",
  varType6 = "Categorical", 
  
  includeVar7 = TRUE,
  startLine7 = 55, 
  endLine7 = 55, 
  linkingVar7 = "EFC",
  varType7 = "Numeric", 
  
  includeVar8 = TRUE,
  startLine8 = 58, 
  endLine8 = 59, 
  linkingVar8 = "Tuition jurisdiction",
  varType8 = "Categorical",
  
  includeVar9 = TRUE,
  startLine9 = 61, 
  endLine9 = 61, 
  linkingVar9 = "Tuition and fees paid",
  varType9 = "Numeric"
)

#### End #### 

# #### Check receives state grants distribution #### 
# 
# showDistribution("Receives state grants")
# 
# #### End #### 

#### Regression 13: Receives Institutional Grants ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "Receives institutional grants",
  retrievalCode = "xjuweb",
  interceptRow = 17, 
  regressionType = "Logistic", 
  positiveClass = "Yes", 
  negativeClass = "No",  
  thresholdVal = 0.372, 
  absoluteAdjustment = 0, 
  relativeAdjustment = 1,
  showWork = FALSE, 
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 31, 
  linkingVar2 = "Region NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 34, 
  endLine3 = 39, 
  linkingVar3 = "Race NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 42, 
  endLine4 = 46, 
  linkingVar4 = "Carnegie NPSAS",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 49, 
  endLine5 = 50, 
  linkingVar5 = "Enrollment intensity NPSAS",
  varType5 = "Categorical", 
  
  includeVar6 = TRUE, 
  startLine6 = 53, 
  endLine6 = 53, 
  linkingVar6 = "Gender",
  varType6 = "Categorical", 
  
  includeVar7 = TRUE,
  startLine7 = 55, 
  endLine7 = 55, 
  linkingVar7 = "EFC",
  varType7 = "Numeric", 
  
  includeVar8 = TRUE,
  startLine8 = 58, 
  endLine8 = 59, 
  linkingVar8 = "Tuition jurisdiction",
  varType8 = "Categorical",
  
  includeVar9 = TRUE,
  startLine9 = 61, 
  endLine9 = 61, 
  linkingVar9 = "Tuition and fees paid",
  varType9 = "Numeric"
)

#### End #### 

# #### Check receives institutional grants distribution #### 
# 
# showDistribution("Receives institutional grants")
# 
# #### End #### 

#### Regression 14: Receives Private Grants ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "Receives private grants",
  retrievalCode = "vgdnty",
  interceptRow = 17, 
  regressionType = "Logistic", 
  positiveClass = "Yes", 
  negativeClass = "No",  
  thresholdVal = 0.135, # EDITED  
  absoluteAdjustment = 0, 
  relativeAdjustment = 1,
  showWork = FALSE, 
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 31, 
  linkingVar2 = "Region NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 34, 
  endLine3 = 39, 
  linkingVar3 = "Race NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 42, 
  endLine4 = 46, 
  linkingVar4 = "Carnegie NPSAS",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 49, 
  endLine5 = 50, 
  linkingVar5 = "Enrollment intensity NPSAS",
  varType5 = "Categorical", 
  
  includeVar6 = TRUE, 
  startLine6 = 53, 
  endLine6 = 53, 
  linkingVar6 = "Gender",
  varType6 = "Categorical", 
  
  includeVar7 = TRUE,
  startLine7 = 55, 
  endLine7 = 55, 
  linkingVar7 = "EFC",
  varType7 = "Numeric", 
  
  includeVar8 = TRUE,
  startLine8 = 58, 
  endLine8 = 59, 
  linkingVar8 = "Tuition jurisdiction",
  varType8 = "Categorical",
  
  includeVar9 = TRUE,
  startLine9 = 61, 
  endLine9 = 61, 
  linkingVar9 = "Tuition and fees paid",
  varType9 = "Numeric"
)

#### End ####

# #### Check receives private grants distribution #### 
# 
# showDistribution("Receives private grants")
# 
# #### End #### 

#### Regression 15: Receives Federal Loans ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "Receives federal loans",
  retrievalCode = "gynrmk",
  interceptRow = 17, 
  regressionType = "Logistic", 
  positiveClass = "Yes", 
  negativeClass = "No",  
  thresholdVal = 0.55, 
  absoluteAdjustment = 0, 
  relativeAdjustment = 1,
  showWork = FALSE, 
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 31, 
  linkingVar2 = "Region NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 34, 
  endLine3 = 39, 
  linkingVar3 = "Race NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 42, 
  endLine4 = 46, 
  linkingVar4 = "Carnegie NPSAS",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 49, 
  endLine5 = 50, 
  linkingVar5 = "Enrollment intensity NPSAS",
  varType5 = "Categorical", 
  
  includeVar6 = TRUE, 
  startLine6 = 53, 
  endLine6 = 53, 
  linkingVar6 = "Gender",
  varType6 = "Categorical", 
  
  includeVar7 = TRUE,
  startLine7 = 55, 
  endLine7 = 55, 
  linkingVar7 = "EFC",
  varType7 = "Numeric", 
  
  includeVar8 = TRUE,
  startLine8 = 58, 
  endLine8 = 59, 
  linkingVar8 = "Tuition jurisdiction",
  varType8 = "Categorical",
  
  includeVar9 = TRUE,
  startLine9 = 61, 
  endLine9 = 61, 
  linkingVar9 = "Tuition and fees paid",
  varType9 = "Numeric"
)

studentList <- studentList %>% mutate(
  `Receives federal loans` = ifelse(
    (`Citizenship`=="Non-citizen") | (`Applied for federal aid`=="No"), "No", `Receives federal loans`
  )
)

#### End #### 

# #### Check receives federal loans distribution #### 
# 
# showDistribution("Receives federal loans")
# 
# #### End #### 

#### Regression 16: Receives Parent Loans ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "Receives parent loans",
  retrievalCode = "asdbgx",
  interceptRow = 17, 
  regressionType = "Logistic", 
  positiveClass = "Yes", 
  negativeClass = "No",  
  thresholdVal = 0.195, # EDITED 
  absoluteAdjustment = 0, 
  relativeAdjustment = 1,
  showWork = FALSE, 
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE,
  startLine2 = 24,
  endLine2 = 28,
  linkingVar2 = "Carnegie NPSAS",
  varType2 = "Categorical",
  
  includeVar3 = TRUE, 
  startLine3 = 31, 
  endLine3 = 32, 
  linkingVar3 = "Enrollment intensity NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 35, 
  endLine4 = 35, 
  linkingVar4 = "Gender",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 37, 
  endLine5 = 37, 
  linkingVar5 = "EFC",
  varType5 = "Numeric", 
  
  includeVar6 = TRUE, 
  startLine6 = 40, 
  endLine6 = 41, 
  linkingVar6 = "Tuition jurisdiction",
  varType6 = "Categorical", 
  
  includeVar7 = TRUE,
  startLine7 = 43, 
  endLine7 = 43, 
  linkingVar7 = "Tuition and fees paid",
  varType7 = "Numeric", 
  
  includeVar8 = FALSE,
  startLine8 = NA, 
  endLine8 = NA, 
  linkingVar8 = NA,
  varType8 = "",
  
  includeVar9 = FALSE,
  startLine9 = NA, 
  endLine9 = NA, 
  linkingVar9 = NA,
  varType9 = ""
)

studentList <- studentList %>% mutate(
  `Receives parent loans` = ifelse(
    (`Citizenship`=="Non-citizen") | (`Dependency status`=="Independent"), "No", `Receives parent loans`
  )
)

#### End #### 

# #### Check receives parent loans distribution #### 
# 
# showDistribution("Receives parent loans")
# 
# #### End #### 

#############################################
#### Predictions from regressions: Set 3 ####
#############################################

#### Save current dataset in case reset is needed ####

studentListSave <- studentList

#### End ####

#### Regression 17: Federal Grant Amount ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "Federal grant amount",
  retrievalCode = "uakjre",
  interceptRow = 17, 
  regressionType = "Linear", 
  positiveClass = "", 
  negativeClass = "",  
  thresholdVal = 0.5, 
  absoluteAdjustment = 0, 
  relativeAdjustment = 1, 
  showWork = FALSE, 
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 31, 
  linkingVar2 = "Region NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 34, 
  endLine3 = 39, 
  linkingVar3 = "Race NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 42, 
  endLine4 = 46, 
  linkingVar4 = "Carnegie NPSAS",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 49, 
  endLine5 = 50, 
  linkingVar5 = "Enrollment intensity NPSAS",
  varType5 = "Categorical", 
  
  includeVar6 = TRUE, 
  startLine6 = 53, 
  endLine6 = 53, 
  linkingVar6 = "Gender",
  varType6 = "Categorical", 
  
  includeVar7 = TRUE,
  startLine7 = 55, 
  endLine7 = 55, 
  linkingVar7 = "EFC",
  varType7 = "Numeric", 
  
  includeVar8 = TRUE,
  startLine8 = 58, 
  endLine8 = 59, 
  linkingVar8 = "Tuition jurisdiction",
  varType8 = "Categorical",
  
  includeVar9 = TRUE,
  startLine9 = 61, 
  endLine9 = 61, 
  linkingVar9 = "Tuition and fees paid",
  varType9 = "Numeric"
)

studentList <- studentList %>% mutate(
  `Federal grant amount` = ifelse(
    `Receives federal grants`=="No", 0, `Federal grant amount`
  )
)

studentList <- studentList %>% mutate(
  `Federal grant amount` = ifelse(`Federal grant amount` < 0, 0, `Federal grant amount`)
)

#### End #### 

#### Calibrate Federal grant amount: Lower high values ####

studentList <- studentList %>% mutate(
  `Federal grant amount` = ifelse(
    between(`Federal grant amount`, 1, 1000), 
    `Federal grant amount` * 0.1, 
    `Federal grant amount`
  )
)

studentList <- studentList %>% mutate(
  `Federal grant amount` = ifelse(
    between(`Federal grant amount`, 1000, 2000), 
    `Federal grant amount` * 0.25, 
    `Federal grant amount`
  )
)

studentList <- studentList %>% mutate(
  `Federal grant amount` = ifelse(
    between(`Federal grant amount`, 2001, 2500), 
    `Federal grant amount` * 0.35, 
    `Federal grant amount`
  )
)

studentList <- studentList %>% mutate(
  `Federal grant amount` = ifelse(
    between(`Federal grant amount`, 2501, 2650), 
    `Federal grant amount` * 0.4, 
    `Federal grant amount`
  )
)

studentList <- studentList %>% mutate(
  `Federal grant amount` = ifelse(
    between(`Federal grant amount`, 2651, 2800), 
    `Federal grant amount` * 0.65, 
    `Federal grant amount`
  )
)

#### End #### 

#### Calibrate Federal grant amount: Raise low values ####

studentList <- studentList %>% mutate(
  `Federal grant amount` = ifelse(
    between(`Federal grant amount`, 4001, 6000), 
    `Federal grant amount` * 1.3, 
    `Federal grant amount`
  )
)

studentList <- studentList %>% mutate(
  `Federal grant amount` = ifelse(
    between(`Federal grant amount`, 3501, 4000), 
    `Federal grant amount` * 1.2, 
    `Federal grant amount`
  )
)

studentList <- studentList %>% mutate(
  `Federal grant amount` = ifelse(
    between(`Federal grant amount`, 3000, 3500), 
    `Federal grant amount` * 1.1, 
    `Federal grant amount`
  )
)

#### End #### 
 
# #### Check federal grant amount distribution ####
# 
# showPercentiles("Federal grant amount", removeZeros=TRUE)
# 
# #### End #### 

#### Regression 18: VA/DOD Grant Amount ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "VA/DOD grant amount",
  retrievalCode = "nfayff",
  interceptRow = 17, 
  regressionType = "Linear", 
  positiveClass = "", 
  negativeClass = "",  
  thresholdVal = 0.5, 
  absoluteAdjustment = 0, 
  relativeAdjustment = 1, 
  showWork = FALSE, 
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 31, 
  linkingVar2 = "Region NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 34, 
  endLine3 = 39, 
  linkingVar3 = "Race NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 42, 
  endLine4 = 46, 
  linkingVar4 = "Carnegie NPSAS",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 49, 
  endLine5 = 50, 
  linkingVar5 = "Enrollment intensity NPSAS",
  varType5 = "Categorical", 
  
  includeVar6 = TRUE, 
  startLine6 = 53, 
  endLine6 = 53, 
  linkingVar6 = "Gender",
  varType6 = "Categorical", 
  
  includeVar7 = TRUE,
  startLine7 = 55, 
  endLine7 = 55, 
  linkingVar7 = "EFC",
  varType7 = "Numeric", 
  
  includeVar8 = TRUE,
  startLine8 = 58, 
  endLine8 = 59, 
  linkingVar8 = "Tuition jurisdiction",
  varType8 = "Categorical",
  
  includeVar9 = TRUE,
  startLine9 = 61, 
  endLine9 = 61, 
  linkingVar9 = "Tuition and fees paid",
  varType9 = "Numeric"
)

studentList <- studentList %>% mutate(
  `VA/DOD grant amount` = ifelse(
    `Receives VA/DOD grants`=="No", 0, `VA/DOD grant amount`
  )
)

studentList <- studentList %>% mutate(
  `VA/DOD grant amount` = ifelse(`VA/DOD grant amount` < 0, 0, `VA/DOD grant amount`)
)

#### End #### 

#### Calibrate VA/DOD grant amount: Lower high values ####

studentList <- studentList %>% mutate(
  `VA/DOD grant amount` = ifelse(
    `VA/DOD grant amount` < 8000,
    `VA/DOD grant amount` * 0.4, 
    `VA/DOD grant amount`
  )
)

studentList <- studentList %>% mutate(
  `VA/DOD grant amount` = ifelse(
    between(`VA/DOD grant amount`, 8001, 10000),
    `VA/DOD grant amount` * 0.6, 
    `VA/DOD grant amount`
  )
)

studentList <- studentList %>% mutate(
  `VA/DOD grant amount` = ifelse(
    between(`VA/DOD grant amount`, 10001, 11000),
    `VA/DOD grant amount` * 0.8, 
    `VA/DOD grant amount`
  )
)

studentList <- studentList %>% mutate(
  `VA/DOD grant amount` = ifelse(
    between(`VA/DOD grant amount`, 11001, 12000),
    `VA/DOD grant amount` * 0.95, 
    `VA/DOD grant amount`
  )
)

#### End #### 

#### Calibrate VA/DOD grant amount: Raise low values ####

studentList <- studentList %>% mutate(
  `VA/DOD grant amount` = ifelse(
    `VA/DOD grant amount` > 22000,
    `VA/DOD grant amount` * 1.5, 
    `VA/DOD grant amount`
  )
)

studentList <- studentList %>% mutate(
  `VA/DOD grant amount` = ifelse(
    between(`VA/DOD grant amount`, 20001, 22000),
    `VA/DOD grant amount` * 1.2, 
    `VA/DOD grant amount`
  )
)

studentList <- studentList %>% mutate(
  `VA/DOD grant amount` = ifelse(
    between(`VA/DOD grant amount`, 18000, 20000),
    `VA/DOD grant amount` * 1.1, 
    `VA/DOD grant amount`
  )
)

#### End #### 

# #### Check VA/DOD grant amount distribution ####
# 
# showPercentiles("VA/DOD grant amount", removeZeros=TRUE)
# 
# #### End #### 

#### Regression 19: State Grant Amount ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "State grant amount",
  retrievalCode = "gwqopy",
  interceptRow = 17, 
  regressionType = "Linear", 
  positiveClass = "", 
  negativeClass = "",  
  thresholdVal = 0.5, 
  absoluteAdjustment = 0, 
  relativeAdjustment = 1,
  showWork = FALSE, 
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 31, 
  linkingVar2 = "Region NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 34, 
  endLine3 = 39, 
  linkingVar3 = "Race NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 42, 
  endLine4 = 46, 
  linkingVar4 = "Carnegie NPSAS",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 49, 
  endLine5 = 50, 
  linkingVar5 = "Enrollment intensity NPSAS",
  varType5 = "Categorical", 
  
  includeVar6 = TRUE, 
  startLine6 = 53, 
  endLine6 = 53, 
  linkingVar6 = "Gender",
  varType6 = "Categorical", 
  
  includeVar7 = TRUE,
  startLine7 = 55, 
  endLine7 = 55, 
  linkingVar7 = "EFC",
  varType7 = "Numeric", 
  
  includeVar8 = TRUE,
  startLine8 = 58, 
  endLine8 = 59, 
  linkingVar8 = "Tuition jurisdiction",
  varType8 = "Categorical",
  
  includeVar9 = TRUE,
  startLine9 = 61, 
  endLine9 = 61, 
  linkingVar9 = "Tuition and fees paid",
  varType9 = "Numeric"
)

studentList <- studentList %>% mutate(
  `State grant amount` = ifelse(
    `Receives state grants`=="No", 0, `State grant amount`
  )
)

studentList <- studentList %>% mutate(
  `State grant amount` = ifelse(`State grant amount` < 0, 0, `State grant amount`)
)

#### End #### 

#### Calibrate state grant amount: Lower high values #### 

studentList <- studentList %>% mutate(
  `State grant amount` = ifelse(
    between(`State grant amount`, 1, 2000), 
    `State grant amount` * 0.35, 
    `State grant amount`
  )
)

studentList <- studentList %>% mutate(
  `State grant amount` = ifelse(
    between(`State grant amount`, 2001, 3000), 
    `State grant amount` * 0.45, 
    `State grant amount`
  )
)

studentList <- studentList %>% mutate(
  `State grant amount` = ifelse(
    between(`State grant amount`, 3001, 4000), 
    `State grant amount` * 0.5, 
    `State grant amount`
  )
)

studentList <- studentList %>% mutate(
  `State grant amount` = ifelse(
    between(`State grant amount`, 4001, 4200), 
    `State grant amount` * 0.6, 
    `State grant amount`
  )
)

studentList <- studentList %>% mutate(
  `State grant amount` = ifelse(
    between(`State grant amount`, 4201, 4500), 
    `State grant amount` * 0.7, 
    `State grant amount`
  )
)

studentList <- studentList %>% mutate(
  `State grant amount` = ifelse(
    between(`State grant amount`, 4501, 4700), 
    `State grant amount` * 0.85, 
    `State grant amount`
  )
)

#### End #### 

#### Calibrate state grant amount: Raise low values #### 

studentList <- studentList %>% mutate(
  `State grant amount` = ifelse(
    `State grant amount` > 5001, 
    `State grant amount` * 1.4, 
    `State grant amount`
  )
)

studentList <- studentList %>% mutate(
  `State grant amount` = ifelse(
    between(`State grant amount`, 4701, 5000), 
    `State grant amount` * 1.1, 
    `State grant amount`
  )
)

#### End #### 

# #### Check state grant amount distribution ####
# 
# showPercentiles("State grant amount", removeZeros=TRUE)
# 
# #### End #### 

#### Regression 20: Institutional Grant Amount ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "Institutional grant amount",
  retrievalCode = "sclsrj",
  interceptRow = 17, 
  regressionType = "Linear", 
  positiveClass = "", 
  negativeClass = "",  
  thresholdVal = 0.5,
  absoluteAdjustment = 0, 
  relativeAdjustment = 0.7, # EDITED 
  showWork = FALSE, 
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 31, 
  linkingVar2 = "Region NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 34, 
  endLine3 = 39, 
  linkingVar3 = "Race NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 42, 
  endLine4 = 46, 
  linkingVar4 = "Carnegie NPSAS",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 49, 
  endLine5 = 50, 
  linkingVar5 = "Enrollment intensity NPSAS",
  varType5 = "Categorical", 
  
  includeVar6 = TRUE, 
  startLine6 = 53, 
  endLine6 = 53, 
  linkingVar6 = "Gender",
  varType6 = "Categorical", 
  
  includeVar7 = TRUE,
  startLine7 = 55, 
  endLine7 = 55, 
  linkingVar7 = "EFC",
  varType7 = "Numeric", 
  
  includeVar8 = TRUE,
  startLine8 = 58, 
  endLine8 = 59, 
  linkingVar8 = "Tuition jurisdiction",
  varType8 = "Categorical",
  
  includeVar9 = TRUE,
  startLine9 = 61, 
  endLine9 = 61, 
  linkingVar9 = "Tuition and fees paid",
  varType9 = "Numeric"
)

studentList <- studentList %>% mutate(
  `Institutional grant amount` = ifelse(
    `Receives institutional grants`=="No", 0, `Institutional grant amount`
  )
)

studentList <- studentList %>% mutate(
  `Institutional grant amount` = ifelse(`Institutional grant amount` < 0, 0, `Institutional grant amount`)
)

#### End #### 

#### Calibrate institutional grant amount: Lower high values #### 

studentList <- studentList %>% mutate(
  `Institutional grant amount` = ifelse(
    between(`Institutional grant amount`, 1, 2350), 
    `Institutional grant amount` * 0.2, 
    `Institutional grant amount`
  )
)

studentList <- studentList %>% mutate(
  `Institutional grant amount` = ifelse(
    between(`Institutional grant amount`, 2351, 3000), 
    `Institutional grant amount` * 0.4, 
    `Institutional grant amount`
  )
)

studentList <- studentList %>% mutate(
  `Institutional grant amount` = ifelse(
    between(`Institutional grant amount`, 3001, 3300), 
    `Institutional grant amount` * 0.55, 
    `Institutional grant amount`
  )
)

studentList <- studentList %>% mutate(
  `Institutional grant amount` = ifelse(
    between(`Institutional grant amount`, 3301, 3500), 
    `Institutional grant amount` * 0.85, 
    `Institutional grant amount`
  )
)

#### End #### 

#### Calibrate institutional grant amount: Raise low values #### 

studentList <- studentList %>% mutate(
  `Institutional grant amount` = ifelse(
    `Institutional grant amount` > 13501, 
    `Institutional grant amount` * 1.8, 
    `Institutional grant amount`
  )
)

studentList <- studentList %>% mutate(
  `Institutional grant amount` = ifelse(
    between(`Institutional grant amount`, 12501, 13500), 
    `Institutional grant amount` * 1.25, 
    `Institutional grant amount`
  )
)

studentList <- studentList %>% mutate(
  `Institutional grant amount` = ifelse(
    between(`Institutional grant amount`, 3801, 6000), 
    `Institutional grant amount` * 1.2, 
    `Institutional grant amount`
  )
)

#### End ####

#### Calibrate institutional grant amount: Lower high values ####

studentList <- studentList %>% mutate(
  `Institutional grant amount` = ifelse(
    between(`Institutional grant amount`, 7001, 9000), 
    `Institutional grant amount` * 0.85, 
    `Institutional grant amount`
  )
)

#### End #### 

# #### Check institutional grant amount distribution ####
# 
# showPercentiles("Institutional grant amount", removeZeros=TRUE)
# 
# #### End #### 

#### Regression 21: Private Grant Amount ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "Private grant amount",
  retrievalCode = "sjeisl",
  interceptRow = 17, 
  regressionType = "Linear", 
  positiveClass = "", 
  negativeClass = "",  
  thresholdVal = 0.5, 
  absoluteAdjustment = 0, 
  relativeAdjustment = 1,  
  showWork = FALSE, 
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 31, 
  linkingVar2 = "Region NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 34, 
  endLine3 = 39, 
  linkingVar3 = "Race NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 42, 
  endLine4 = 46, 
  linkingVar4 = "Carnegie NPSAS",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 49, 
  endLine5 = 50, 
  linkingVar5 = "Enrollment intensity NPSAS",
  varType5 = "Categorical", 
  
  includeVar6 = TRUE, 
  startLine6 = 53, 
  endLine6 = 53, 
  linkingVar6 = "Gender",
  varType6 = "Categorical", 
  
  includeVar7 = TRUE,
  startLine7 = 55, 
  endLine7 = 55, 
  linkingVar7 = "EFC",
  varType7 = "Numeric", 
  
  includeVar8 = TRUE,
  startLine8 = 58, 
  endLine8 = 59, 
  linkingVar8 = "Tuition jurisdiction",
  varType8 = "Categorical",
  
  includeVar9 = TRUE,
  startLine9 = 61, 
  endLine9 = 61, 
  linkingVar9 = "Tuition and fees paid",
  varType9 = "Numeric"
)

studentList <- studentList %>% mutate(
  `Private grant amount` = ifelse(
    `Receives private grants`=="No", 0, `Private grant amount`
  )
)

studentList <- studentList %>% mutate(
  `Private grant amount` = ifelse(`Private grant amount` < 0, 0, `Private grant amount`)
)

#### End #### 

#### Calibrate private grant amount: Lower high values ####

studentList <- studentList %>% mutate(
  `Private grant amount` = ifelse(
    between(`Private grant amount`, 1, 6300), 
    `Private grant amount` * 0.08, 
    `Private grant amount`
  )
)

studentList <- studentList %>% mutate(
  `Private grant amount` = ifelse(
    between(`Private grant amount`, 6301, 6500), 
    `Private grant amount` * 0.12, 
    `Private grant amount`
  )
)

studentList <- studentList %>% mutate(
  `Private grant amount` = ifelse(
    between(`Private grant amount`, 6501, 6800), 
    `Private grant amount` * 0.15, 
    `Private grant amount`
  )
)


studentList <- studentList %>% mutate(
  `Private grant amount` = ifelse(
    between(`Private grant amount`, 6801, 7200), 
    `Private grant amount` * 0.25, 
    `Private grant amount`
  )
)

studentList <- studentList %>% mutate(
  `Private grant amount` = ifelse(
    between(`Private grant amount`, 7201, 7300), 
    `Private grant amount` * 0.35, 
    `Private grant amount`
  )
)

studentList <- studentList %>% mutate(
  `Private grant amount` = ifelse(
    between(`Private grant amount`, 7301, 7500), 
    `Private grant amount` * 0.5, 
    `Private grant amount`
  )
)

studentList <- studentList %>% mutate(
  `Private grant amount` = ifelse(
    between(`Private grant amount`, 7501, 7700), 
    `Private grant amount` * 0.65, 
    `Private grant amount`
  )
)

#### End ####

#### Calibrate private grant amount: Raise low values ####

studentList <- studentList %>% mutate(
  `Private grant amount` = ifelse(
    `Private grant amount` > 7800, 
    `Private grant amount` * 1.3, 
    `Private grant amount`
  )
)

studentList <- studentList %>% mutate(
  `Private grant amount` = ifelse(
    between(`Private grant amount`, 7701, 7800),
    `Private grant amount` * 1.15, 
    `Private grant amount`
  )
)

#### End ####

# #### Check private grant amount distribution ####
# 
# showPercentiles("Private grant amount", removeZeros=TRUE)
# 
# #### End #### 

#### Regression 22: Federal Loan Amount ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "Federal loan amount",
  retrievalCode = "ojeaer",
  interceptRow = 17, 
  regressionType = "Linear", 
  positiveClass = "", 
  negativeClass = "",  
  thresholdVal = 0.5, 
  absoluteAdjustment = 0, 
  relativeAdjustment = 1,
  showWork = FALSE, 
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 31, 
  linkingVar2 = "Region NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 34, 
  endLine3 = 39, 
  linkingVar3 = "Race NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 42, 
  endLine4 = 46, 
  linkingVar4 = "Carnegie NPSAS",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 49, 
  endLine5 = 50, 
  linkingVar5 = "Enrollment intensity NPSAS",
  varType5 = "Categorical", 
  
  includeVar6 = TRUE, 
  startLine6 = 53, 
  endLine6 = 53, 
  linkingVar6 = "Gender",
  varType6 = "Categorical", 
  
  includeVar7 = TRUE,
  startLine7 = 55, 
  endLine7 = 55, 
  linkingVar7 = "EFC",
  varType7 = "Numeric", 
  
  includeVar8 = TRUE,
  startLine8 = 58, 
  endLine8 = 59, 
  linkingVar8 = "Tuition jurisdiction",
  varType8 = "Categorical",
  
  includeVar9 = TRUE,
  startLine9 = 61, 
  endLine9 = 61, 
  linkingVar9 = "Tuition and fees paid",
  varType9 = "Numeric"
)

studentList <- studentList %>% mutate(
  `Federal loan amount` = ifelse(
    `Receives federal loans`=="No", 0, `Federal loan amount`
  )
)

studentList <- studentList %>% mutate(
  `Federal loan amount` = ifelse(`Federal loan amount` < 0, 0, `Federal loan amount`)
)

#### End #### 

#### Calibrate federal loan amount: Lower high values ####

studentList <- studentList %>% mutate(
  `Federal loan amount` = ifelse(
    between(`Federal loan amount`, 1, 6100), 
    `Federal loan amount` * 0.4, 
    `Federal loan amount`
  )
)

studentList <- studentList %>% mutate(
  `Federal loan amount` = ifelse(
    between(`Federal loan amount`, 6101, 6200), 
    `Federal loan amount` * 0.6, 
    `Federal loan amount`
  )
)

studentList <- studentList %>% mutate(
  `Federal loan amount` = ifelse(
    between(`Federal loan amount`, 6201, 6450), 
    `Federal loan amount` * 0.85, 
    `Federal loan amount`
  )
)

#### End ####

#### Calibrate federal loan amount: Raise low values ####  

studentList <- studentList %>% mutate(
  `Federal loan amount` = ifelse(
    `Federal loan amount` > 6851, 
    `Federal loan amount` * 1.4, 
    `Federal loan amount`
  )
)

studentList <- studentList %>% mutate(
  `Federal loan amount` = ifelse(
    between(`Federal loan amount`, 6651, 6850), 
    `Federal loan amount` * 1.3, 
    `Federal loan amount`
  )
)

studentList <- studentList %>% mutate(
  `Federal loan amount` = ifelse(
    between(`Federal loan amount`, 6551, 6650), 
    `Federal loan amount` * 1.15, 
    `Federal loan amount`
  )
)

#### End ####

# #### Check federal loan amount distribution ####
# 
# showPercentiles("Federal loan amount", removeZeros=TRUE)
# 
# #### End #### 

#### Regression 23: Parent Loan Amount ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "Parent loan amount",
  retrievalCode = "nuwhux",
  interceptRow = 17, 
  regressionType = "Linear", 
  positiveClass = "", 
  negativeClass = "",  
  thresholdVal = 0.5, 
  absoluteAdjustment = 0, 
  relativeAdjustment = 0.75, # EDITED 
  showWork = FALSE, 
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 28, 
  linkingVar2 = "Carnegie NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 31, 
  endLine3 = 32, 
  linkingVar3 = "Enrollment intensity NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 35, 
  endLine4 = 35, 
  linkingVar4 = "Gender",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 37, 
  endLine5 = 37, 
  linkingVar5 = "EFC",
  varType5 = "Numeric", 
  
  includeVar6 = TRUE, 
  startLine6 = 40, 
  endLine6 = 41, 
  linkingVar6 = "Tuition jurisdiction",
  varType6 = "Categorical", 
  
  includeVar7 = TRUE,
  startLine7 = 43, 
  endLine7 = 43, 
  linkingVar7 = "Tuition and fees paid",
  varType7 = "Numeric", 
  
  includeVar8 = FALSE,
  startLine8 = NA, 
  endLine8 = NA, 
  linkingVar8 = NA,
  varType8 = "",
  
  includeVar9 = FALSE,
  startLine9 = NA, 
  endLine9 = NA, 
  linkingVar9 = NA,
  varType9 = ""
)

studentList <- studentList %>% mutate(
  `Parent loan amount` = ifelse(
    `Receives parent loans`=="No", 0, `Parent loan amount`
  )
)

studentList <- studentList %>% mutate(
  `Parent loan amount` = ifelse(`Parent loan amount` < 0, 0, `Parent loan amount`)
)

#### End #### 

#### Calibrate parent loan amount: Lower high values #### 

studentList <- studentList %>% mutate(
  `Parent loan amount` = ifelse(
    between(`Parent loan amount`, 1, 15000), 
    `Parent loan amount` * 0.3, 
    `Parent loan amount`
  )
)

studentList <- studentList %>% mutate(
  `Parent loan amount` = ifelse(
    between(`Parent loan amount`, 15001, 15500), 
    `Parent loan amount` * 0.45, 
    `Parent loan amount`
  )
)

studentList <- studentList %>% mutate(
  `Parent loan amount` = ifelse(
    between(`Parent loan amount`, 15501, 16000), 
    `Parent loan amount` * 0.55, 
    `Parent loan amount`
  )
)

studentList <- studentList %>% mutate(
  `Parent loan amount` = ifelse(
    between(`Parent loan amount`, 16001, 16700), 
    `Parent loan amount` * 0.7, 
    `Parent loan amount`
  )
)

studentList <- studentList %>% mutate(
  `Parent loan amount` = ifelse(
    between(`Parent loan amount`, 16701, 17000), 
    `Parent loan amount` * 0.8, 
    `Parent loan amount`
  )
)

studentList <- studentList %>% mutate(
  `Parent loan amount` = ifelse(
    between(`Parent loan amount`, 17001, 18000), 
    `Parent loan amount` * 0.95, 
    `Parent loan amount`
  )
)

#### End #### 

#### Calibrate parent loan amount: Raise low values #### 

studentList <- studentList %>% mutate(
  `Parent loan amount` = ifelse(
    `Parent loan amount` > 19500, 
    `Parent loan amount` * 1.55, 
    `Parent loan amount`
  )
)

studentList <- studentList %>% mutate(
  `Parent loan amount` = ifelse(
    between(`Parent loan amount`, 18500, 19500), 
    `Parent loan amount` * 1.25, 
    `Parent loan amount`
  )
)

#### End #### 

# #### Check parent loan amount distribution ####
# 
# showPercentiles("Parent loan amount", removeZeros=TRUE)
# 
# #### End #### 

#############################################
#### Predictions from regressions: Set 4 ####
#############################################

#### Save current dataset in case reset is needed ####

studentListSave <- studentList

#### End ####

#### Regression 24: Parent Status ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "Parent status",
  retrievalCode = "vrhoke",
  interceptRow = 17, 
  regressionType = "Logistic", 
  positiveClass = "Has dependents", 
  negativeClass = "Does not have dependents",  
  thresholdVal = 0.41, # EDITED 
  absoluteAdjustment = 0, 
  relativeAdjustment = 1,
  showWork = FALSE, 
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 31, 
  linkingVar2 = "Region NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 34, 
  endLine3 = 39, 
  linkingVar3 = "Race NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 42, 
  endLine4 = 46, 
  linkingVar4 = "Carnegie NPSAS",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 49, 
  endLine5 = 50, 
  linkingVar5 = "Enrollment intensity NPSAS",
  varType5 = "Categorical", 
  
  includeVar6 = TRUE, 
  startLine6 = 53, 
  endLine6 = 53, 
  linkingVar6 = "Gender",
  varType6 = "Categorical", 
  
  includeVar7 = TRUE,
  startLine7 = 55, 
  endLine7 = 55, 
  linkingVar7 = "EFC",
  varType7 = "Numeric", 
  
  includeVar8 = TRUE,
  startLine8 = 58, 
  endLine8 = 59, 
  linkingVar8 = "Tuition jurisdiction",
  varType8 = "Categorical",
  
  includeVar9 = TRUE,
  startLine9 = 61, 
  endLine9 = 61, 
  linkingVar9 = "Tuition and fees paid",
  varType9 = "Numeric"
)

#### End #### 

# #### Check parent status distribution ####
# 
# showDistribution("Parent status")
# 
# #### End #### 

#### Regression 25: Parental Education Attainment ####

studentList <- processRegression(
  
  studentListDF = studentList,
  newVariableName = "Parental education attainment",
  retrievalCode = "pprqfp",
  interceptRow = 17, 
  regressionType = "Logistic", 
  positiveClass = "Parents have a college degree", 
  negativeClass = "Parents do not have a college degree",  
  thresholdVal = 0.52,  
  absoluteAdjustment = 0, 
  relativeAdjustment = 1,
  showWork = FALSE, 
  randomI = FALSE, 
  randomC = FALSE, 
  
  includeVar1 = TRUE, 
  startLine1 = 20, 
  endLine1 = 21, 
  linkingVar1 = "Control", 
  varType1 = "Categorical", 
  
  includeVar2 = TRUE, 
  startLine2 = 24, 
  endLine2 = 31, 
  linkingVar2 = "Region NPSAS",
  varType2 = "Categorical", 
  
  includeVar3 = TRUE, 
  startLine3 = 34, 
  endLine3 = 39, 
  linkingVar3 = "Race NPSAS",
  varType3 = "Categorical", 
  
  includeVar4 = TRUE, 
  startLine4 = 42, 
  endLine4 = 46, 
  linkingVar4 = "Carnegie NPSAS",
  varType4 = "Categorical", 
  
  includeVar5 = TRUE, 
  startLine5 = 49, 
  endLine5 = 50, 
  linkingVar5 = "Enrollment intensity NPSAS",
  varType5 = "Categorical", 
  
  includeVar6 = TRUE, 
  startLine6 = 53, 
  endLine6 = 53, 
  linkingVar6 = "Gender",
  varType6 = "Categorical", 
  
  includeVar7 = TRUE,
  startLine7 = 55, 
  endLine7 = 55, 
  linkingVar7 = "EFC",
  varType7 = "Numeric", 
  
  includeVar8 = TRUE,
  startLine8 = 58, 
  endLine8 = 59, 
  linkingVar8 = "Tuition jurisdiction",
  varType8 = "Categorical",
  
  includeVar9 = TRUE,
  startLine9 = 61, 
  endLine9 = 61, 
  linkingVar9 = "Tuition and fees paid",
  varType9 = "Numeric"
)

#### End #### 

# #### Check parental education attainment distribution ####
# 
# showDistribution("Parental education attainment")
# 
# #### End #### 

#### Save file #### 

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Postprocessing data")

write.csv(
  studentList, 
  "All merged student data.csv", 
  row.names=FALSE
)

#### End #### 



