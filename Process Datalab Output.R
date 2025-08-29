
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
  
  setwd("/Users/peter_granville/Fed State Modeling/Datalab outputs")
  
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

setwd("/Users/peter_granville/Fed State Modeling/IPEDS data")

hd <- read.csv("hd2023.csv", header=TRUE) %>% select(
  `UNITID`, 
  `INSTNM`,
  `CONTROL`,
  `STABBR`, 
  `OBEREG`,
  `C18BASIC`
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

ic <- read.csv("ic2023.csv", header=TRUE) %>% select(
  `UNITID`, 
  `TUITVARY`
) 

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

#### Mainly for testing #### 

set.seed(1001)
unitidLevels <- sample(unique(effy$UNITID))
effy$UNITID <- factor(effy$UNITID, levels=unitidLevels)
# 
# # Shuffle effy by UNITID
# # effy <- effy %>% arrange(`UNITID`)
# 
# # Or shuffle by EFFY row 
# effy <- effy[sample(nrow(effy)),]

# Index effy
effy <- effy %>% mutate(
  `Index` = (1:nrow(effy))
)

hd <- hd %>% mutate(
  `UNITID` = factor(`UNITID`, levels=unitidLevels)
)

ic <- ic %>% mutate(
  `UNITID` = factor(`UNITID`, levels=unitidLevels)
)

#### End #### 

for(j in (1:176)){
  
  #### Filter for students in EFFY block #### 
  
  print(paste("Trying block ", j, ".", sep=""))
  
  newEffy <- effy %>% filter(
    between(
      `Index`,
      1 + ((j-1) * 1000),
      min(j * 1000, nrow(effy))
    )
  )
  
  #### End #### 
  
  newEffy <- effy # PURELY FOR TESTING PURPOSES 08-28-2025
  
  #### Create student level dataset #### 
  
  for(i in newEffy$`Index`){
    
    if(i %% 1000 == 0){print(paste("Number ", comma(i), " out of ", comma(nrow(newEffy)), ".", sep=""))}
    
    tempEFFY <- newEffy %>% filter(`Index`==i)
    
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
    
    if(i == 1){
    # if(i %% 1000 == 1){ # WHY DID I HAVE THIS HERE? 
      studentList <- tempSTU
    }else{
      studentList <- rbind(studentList, tempSTU)
    }
    
    rm(tempSTU, tempEFFY)
    
  }
  rm(i, newEffy)
  
  #### End #### 
  
  #### Import institutional information #### 
  
  studentList <- left_join(x=studentList, y=hd, by="UNITID")
  
  studentList <- left_join(x=studentList, y=ic, by="UNITID")
  
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

  # Using last 2 digits of Student Index as essentially random 
  studentList <- studentList %>% mutate(
    `2 Digits` = substr(`Student index`, nchar(`Student index`)-1, nchar(`Student index`))
  ) %>% mutate(
    `2 Digits` = ifelse(
      nchar(`2 Digits`)==1, 
      paste("0", `2 Digits`, sep=""),
      `2 Digits`
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
  
  # #### Save current dataset in case reset is needed #### 
  # 
  # studentListSave <- studentList
  # 
  # #### End #### 
  
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
  
  showPercentiles <- function(variableName){
    
    tempDF <- studentList %>% select(
      all_of(variableName)
    )
    names(tempDF)[1] <- "InterestVar"
    
    print(quantile(tempDF$`InterestVar`, probs = seq(.05, .95, by = .05)))
    
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
    thresholdVal = 0.414, # EDITED 08-29-2025 
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
  
  # showDistribution("Zero-EFC")
  
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
  
  studentList <- studentList %>% select(
    -(`Zero-EFC`)
  )
  
  #### End #### 
  
  #### Calibrate EFC: Bring down high values  #### 
  
  studentList <- studentList %>% mutate(
    `EFC` = ifelse(between(`EFC`, 1, 15000), `EFC` * 0.1, `EFC`)
  )
  
  studentList <- studentList %>% mutate(
    `EFC` = ifelse(between(`EFC`, 15001, 18000), `EFC` * 0.2, `EFC`)
  )
  
  studentList <- studentList %>% mutate(
    `EFC` = ifelse(between(`EFC`, 18001, 20000), `EFC` * 0.3, `EFC`)
  )
  
  studentList <- studentList %>% mutate(
    `EFC` = ifelse(between(`EFC`, 20001, 22000), `EFC` * 0.4, `EFC`)
  )
  
  studentList <- studentList %>% mutate(
    `EFC` = ifelse(between(`EFC`, 22001, 24000), `EFC` * 0.6, `EFC`)
  )
  
  studentList <- studentList %>% mutate(
    `EFC` = ifelse(between(`EFC`, 24001, 26000), `EFC` * 0.75, `EFC`)
  )
  
  studentList <- studentList %>% mutate(
    `EFC` = ifelse(between(`EFC`, 26001, 28000), `EFC` * 0.9, `EFC`)
  )
  
  #### End #### 
  
  #### Calibrate EFC: Bring up low values #### 
  
  studentList <- studentList %>% mutate(
    `EFC` = ifelse(between(`EFC`, 34001, 36000), `EFC` * 3.2, `EFC`)
  )
  
  studentList <- studentList %>% mutate(
    `EFC` = ifelse(between(`EFC`, 33001, 34000), `EFC` * 2.5, `EFC`)
  )
  
  studentList <- studentList %>% mutate(
    `EFC` = ifelse(between(`EFC`, 32001, 33000), `EFC` * 2, `EFC`)
  )
  
  studentList <- studentList %>% mutate(
    `EFC` = ifelse(between(`EFC`, 31001, 32000), `EFC` * 1.5, `EFC`)
  )
  
  studentList <- studentList %>% mutate(
    `EFC` = ifelse(between(`EFC`, 30501, 31000), `EFC` * 1.35, `EFC`)
  )
  
  studentList <- studentList %>% mutate(
    `EFC` = ifelse(between(`EFC`, 30001, 30500), `EFC` * 1.2, `EFC`)
  )
  
  studentList <- studentList %>% mutate(
    `EFC` = ifelse(between(`EFC`, 29001, 30000), `EFC` * 1.1, `EFC`)
  )
  
  studentList <- studentList %>% mutate(
    `EFC` = ifelse(between(`EFC`, 28001, 29000), `EFC` * 1, `EFC`)
  )
  
  #### End #### 
  
  #### Calibrate EFC: Bring down high values #### 
  
  studentList <- studentList %>% mutate(
    `EFC` = ifelse(between(`EFC`, 1, 1000), `EFC` * 0.2, `EFC`)
  )
  
  studentList <- studentList %>% mutate(
    `EFC` = ifelse(between(`EFC`, 1001, 1500), `EFC` * 0.25, `EFC`)
  )
  
  studentList <- studentList %>% mutate(
    `EFC` = ifelse(between(`EFC`, 1501, 2000), `EFC` * 0.3, `EFC`)
  )
  
  studentList <- studentList %>% mutate(
    `EFC` = ifelse(between(`EFC`, 2001, 2500), `EFC` * 0.4, `EFC`)
  )
  
  studentList <- studentList %>% mutate(
    `EFC` = ifelse(between(`EFC`, 2501, 3000), `EFC` * 0.5, `EFC`)
  )
  
  studentList <- studentList %>% mutate(
    `EFC` = ifelse(between(`EFC`, 3001, 3500), `EFC` * 0.7, `EFC`)
  )
  
  studentList <- studentList %>% mutate(
    `EFC` = ifelse(between(`EFC`, 3501, 4000), `EFC` * 0.9, `EFC`)
  )
  
  #### End #### 
  
  #### Regression 2: Predict Tuition Jurisdiction ####
  
  studentList <- processRegression(
    
    studentListDF = studentList,
    newVariableName = "Tuition jurisdiction",
    retrievalCode = "pwappp",
    interceptRow = 17, 
    regressionType = "Logistic", 
    positiveClass = "In-state tuition", 
    negativeClass = "Out-of-state tuition",  
    thresholdVal = 0.81, # EDITED FROM 0.5 
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
  
  # showDistribution("Tuition jurisdiction")
  
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
    absoluteAdjustment = 2000,  
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
  
  #### Calibrate tuition and fees paid ####
  
  studentList <- studentList %>% mutate(
    `Tuition and fees paid` = ifelse(
      `Tuition and fees paid` <= 2000, 
      `Tuition and fees paid` * 1.5, 
      `Tuition and fees paid` * 0.9
    )
  )
  
  studentList <- studentList %>% mutate(
    `Tuition and fees paid` = ifelse(
      between(`Tuition and fees paid`, 3500, 15000),
      `Tuition and fees paid` * 0.8, 
      `Tuition and fees paid`
    )
  )
  
  studentList <- studentList %>% mutate(
    `Tuition and fees paid` = ifelse(
      between(`Tuition and fees paid`, 15001, 25000),
      `Tuition and fees paid` * 0.9, 
      `Tuition and fees paid`
    )
  )
  
  
  studentList <- studentList %>% mutate(
    `Tuition and fees paid` = ifelse(
      between(`Tuition and fees paid`, 25001, 35000),
      `Tuition and fees paid` * 0.95, 
      `Tuition and fees paid`
    )
  )
  
  # showPercentiles("Tuition and fees paid")
  
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
      `Age` * 0.85, 
      `Age`
    )
  )
  
  studentList <- studentList %>% mutate(
    `Age` = ifelse(
      between(`Age`, 20, 21.99),
      `Age` * 0.95, 
      `Age`
    )
  )
  
  
  studentList <- studentList %>% mutate(
    `Age` = ifelse(
      between(`Age`, 22, 24.99),
      `Age` * 0.95, 
      `Age`
    )
  )
  
  #### End #### 
  
  #### Calibrate age: Raise low values #### 
  
  studentList <- studentList %>% mutate(
    `Age` = ifelse(
      `Age` > 29.71,
      `Age` * 1.8, 
      `Age`
    )
  )
  
  studentList <- studentList %>% mutate(
    `Age` = ifelse(
      between(`Age`, 29.41, 29.7),
      `Age` * 1.6, 
      `Age`
    )
  )
  
  studentList <- studentList %>% mutate(
    `Age` = ifelse(
      between(`Age`, 29, 29.4),
      `Age` * 1.4, 
      `Age`
    )
  )
  
  studentList <- studentList %>% mutate(
    `Age` = ifelse(
      between(`Age`, 28, 28.99),
      `Age` * 1.3, 
      `Age`
    )
  )
  
  studentList <- studentList %>% mutate(
    `Age` = ifelse(
      between(`Age`, 26, 27.99),
      `Age` * 1.2, 
      `Age`
    )
  )
  
  # showPercentiles("Age")
  
  #### End #### 
  
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
  
  # showDistribution("Citizenship")
  
  #### End #### 
  
  #### Regression 6: Veteran Status ####
  
  studentList <- processRegression(
    
    studentListDF = studentList,
    newVariableName = "Veteran status",
    retrievalCode = "cxjrap",
    interceptRow = 17, 
    regressionType = "Logistic", 
    positiveClass = "Veteran", 
    negativeClass = "Not a veteran", 
    thresholdVal = 0.1, # EDITED 
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
  
  # showDistribution("Veteran status")
  
  #### End #### 
  
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
  
  # showDistribution("Dependency status")
  
  #### End #### 
  
  #### Regression 8: Applied for Federal Aid ####
  
  studentList <- processRegression(
    
    studentListDF = studentList,
    newVariableName = "Applied for federal aid",
    retrievalCode = "jvlplk",
    interceptRow = 17, 
    regressionType = "Logistic", 
    positiveClass = "Yes", 
    negativeClass = "No",  
    thresholdVal = 0.539, # EDITED 
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
  
  # showDistribution("Applied for federal aid")
  
  #### End #### 
  
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
      `Non-tuition expense budget` * 0.6, 
      `Non-tuition expense budget`
    )
  )
  
  studentList <- studentList %>% mutate(
    `Non-tuition expense budget` = ifelse(
      between(`Non-tuition expense budget`, 7001, 9000),
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
      between(`Non-tuition expense budget`, 12001, 17000), 
      `Non-tuition expense budget` * 1.2, 
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
  
  # showPercentiles("Non-tuition expense budget")
  
  #### End #### 
  
  #############################################
  #### Predictions from regressions: Set 2 ####
  #############################################
  
  # #### Save current dataset in case reset is needed #### 
  # 
  # studentListSave <- studentList
  # 
  # #### End #### 
  
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
  
  showDistribution("Receives federal grants")
  
  #### End #### 
  
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
    thresholdVal = 0.4, 
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
  
  # showDistribution("Receives VA/DOD grants")
  
  #### End #### 
  
  #### Regression 12: Receives State Grants ####
  
  studentList <- processRegression(
    
    studentListDF = studentList,
    newVariableName = "Receives state grants",
    retrievalCode = "nczufm",
    interceptRow = 17, 
    regressionType = "Logistic", 
    positiveClass = "Yes", 
    negativeClass = "No",  
    thresholdVal = 0.32,  
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
  
  # showDistribution("Receives state grants")
  
  #### End #### 
  
  #### Regression 13: Receives Institutional Grants ####
  
  studentList <- processRegression(
    
    studentListDF = studentList,
    newVariableName = "Receives institutional grants",
    retrievalCode = "xjuweb",
    interceptRow = 17, 
    regressionType = "Logistic", 
    positiveClass = "Yes", 
    negativeClass = "No",  
    thresholdVal = 0.365, 
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
  
  # showDistribution("Receives institutional grants")
  
  #### End #### 
  
  #### Regression 14: Receives Private Grants ####
  
  studentList <- processRegression(
    
    studentListDF = studentList,
    newVariableName = "Receives private grants",
    retrievalCode = "vgdnty",
    interceptRow = 17, 
    regressionType = "Logistic", 
    positiveClass = "Yes", 
    negativeClass = "No",  
    thresholdVal = 0.14, # EDITED  
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
  
  # showDistribution("Receives private grants")
  
  #### End #### 
  
  #### Regression 15: Receives Federal Loans ####
  
  studentList <- processRegression(
    
    studentListDF = studentList,
    newVariableName = "Receives federal loans",
    retrievalCode = "gynrmk",
    interceptRow = 17, 
    regressionType = "Logistic", 
    positiveClass = "Yes", 
    negativeClass = "No",  
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
    `Receives federal loans` = ifelse(
      (`Citizenship`=="Non-citizen") | (`Applied for federal aid`=="No"), "No", `Receives federal loans`
    )
  )
  
  # showDistribution("Receives federal loans")
  
  #### End #### 
  
  #### Regression 16: Receives Parent Loans ####
  
  studentList <- processRegression(
    
    studentListDF = studentList,
    newVariableName = "Receives parent loans",
    retrievalCode = "asdbgx",
    interceptRow = 17, 
    regressionType = "Logistic", 
    positiveClass = "Yes", 
    negativeClass = "No",  
    thresholdVal = 0.2, # EDITED 
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
  
  # showDistribution("Receives parent loans")
  
  #### End #### 
  
  #############################################
  #### Predictions from regressions: Set 3 ####
  #############################################
  
  # #### Save current dataset in case reset is needed #### 
  # 
  # studentListSave <- studentList
  # 
  # #### End #### 
  
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
      `Federal grant amount` * 0.65, 
      `Federal grant amount`
    )
  )
  
  studentList <- studentList %>% mutate(
    `Federal grant amount` = ifelse(
      between(`Federal grant amount`, 2501, 2800), 
      `Federal grant amount` * 0.9, 
      `Federal grant amount`
    )
  )

  #### End #### 
  
  #### Calibrate Federal grant amount: Raise low values ####
  
  studentList <- studentList %>% mutate(
    `Federal grant amount` = ifelse(
      between(`Federal grant amount`, 4001, 6000), 
      `Federal grant amount` * 1.4, 
      `Federal grant amount`
    )
  )
  
  studentList <- studentList %>% mutate(
    `Federal grant amount` = ifelse(
      between(`Federal grant amount`, 3501, 4000), 
      `Federal grant amount` * 1.3, 
      `Federal grant amount`
    )
  )
  
  studentList <- studentList %>% mutate(
    `Federal grant amount` = ifelse(
      between(`Federal grant amount`, 3000, 3500), 
      `Federal grant amount` * 1.2, 
      `Federal grant amount`
    )
  )

  # showPercentiles("Federal grant amount")
  
  # test <- studentList %>% filter(`Federal grant amount` > 0)
  # print(quantile(test$`Federal grant amount`, probs = seq(.1, .9, by = .1)))
  # rm(test)
  
  #### End #### 
  
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
  
  #### Calibrate VA/DOD grant amount: Raise low values ####
  
  studentList <- studentList %>% mutate(
    `VA/DOD grant amount` = ifelse(
      between(`VA/DOD grant amount`, 6000, 9000),
      `VA/DOD grant amount` + ((`VA/DOD grant amount` - 6000) * 1.1), 
      `VA/DOD grant amount`
    )
  )
  
  studentList <- studentList %>% mutate(
    `VA/DOD grant amount` = ifelse(
      between(`VA/DOD grant amount`, 9000, 99999),
      `VA/DOD grant amount` + ((`VA/DOD grant amount` - 6000) * 1.25), 
      `VA/DOD grant amount`
    )
  )
  
  # test <- studentList %>% filter(`VA/DOD grant amount` > 0)
  # print(quantile(test$`VA/DOD grant amount`, probs = seq(.1, .9, by = .1)))
  # rm(test)
  
  #### End #### 
  
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
      `State grant amount` * 0.2, 
      `State grant amount`
    )
  )
  
  studentList <- studentList %>% mutate(
    `State grant amount` = ifelse(
      between(`State grant amount`, 2001, 3000), 
      `State grant amount` * 0.35, 
      `State grant amount`
    )
  )
  
  studentList <- studentList %>% mutate(
    `State grant amount` = ifelse(
      between(`State grant amount`, 3001, 4000), 
      `State grant amount` * 0.45, 
      `State grant amount`
    )
  )
  
  studentList <- studentList %>% mutate(
    `State grant amount` = ifelse(
      between(`State grant amount`, 4001, 4500), 
      `State grant amount` * 0.6, 
      `State grant amount`
    )
  )
  
  studentList <- studentList %>% mutate(
    `State grant amount` = ifelse(
      between(`State grant amount`, 4501, 5000), 
      `State grant amount` * 0.75, 
      `State grant amount`
    )
  )
  
  #### End #### 
  
  #### Calibrate state grant amount: Raise low values #### 
  
  studentList <- studentList %>% mutate(
    `State grant amount` = ifelse(
      between(`State grant amount`, 5501, 99999), 
      `State grant amount` * 1.3, 
      `State grant amount`
    )
  )
  
  # test <- studentList %>% filter(`State grant amount` > 0)
  # print(quantile(test$`State grant amount`, probs = seq(.1, .9, by = .1)))
  # rm(test)
  
  #### End #### 
  
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
      between(`Institutional grant amount`, 0, 3000), 
      `Institutional grant amount` * 0.2, 
      `Institutional grant amount`
    )
  )
  
  studentList <- studentList %>% mutate(
    `Institutional grant amount` = ifelse(
      between(`Institutional grant amount`, 3001, 3500), 
      `Institutional grant amount` * 0.3, 
      `Institutional grant amount`
    )
  )
  
  studentList <- studentList %>% mutate(
    `Institutional grant amount` = ifelse(
      between(`Institutional grant amount`, 3501, 4000), 
      `Institutional grant amount` * 0.7, 
      `Institutional grant amount`
    )
  )
  
  studentList <- studentList %>% mutate(
    `Institutional grant amount` = ifelse(
      between(`Institutional grant amount`, 4001, 4500), 
      `Institutional grant amount` * 0.9, 
      `Institutional grant amount`
    )
  )
  
  #### End #### 
  
  #### Calibrate institutional grant amount: Raise low values #### 
  
  studentList <- studentList %>% mutate(
    `Institutional grant amount` = ifelse(
      between(`Institutional grant amount`, 14501, 99999), 
      `Institutional grant amount` * 1.5, 
      `Institutional grant amount`
    )
  )
  
  studentList <- studentList %>% mutate(
    `Institutional grant amount` = ifelse(
      between(`Institutional grant amount`, 13501, 14500), 
      `Institutional grant amount` * 1.3, 
      `Institutional grant amount`
    )
  )
  
  studentList <- studentList %>% mutate(
    `Institutional grant amount` = ifelse(
      between(`Institutional grant amount`, 12501, 13500), 
      `Institutional grant amount` * 1.1, 
      `Institutional grant amount`
    )
  )
  
  # test <- studentList %>% filter(`Institutional grant amount` > 0)
  # print(quantile(test$`Institutional grant amount`, probs = seq(.1, .9, by = .1)))
  # rm(test)
  
  #### End #### 
  
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
    relativeAdjustment = 0.6, # EDITED 
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
  
  #############################################
  #### Predictions from regressions: Set 4 ####
  #############################################
  
  # #### Save current dataset in case reset is needed #### 
  # 
  # studentListSave <- studentList
  # 
  # #### End #### 
  
  #### Regression 24: Parent Status ####
  
  studentList <- processRegression(
    
    studentListDF = studentList,
    newVariableName = "Parent status",
    retrievalCode = "vrhoke",
    interceptRow = 17, 
    regressionType = "Logistic", 
    positiveClass = "Has dependents", 
    negativeClass = "Does not have dependents",  
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
  
  #### Regression 25: STEM Major ####
  
  studentList <- processRegression(
    
    studentListDF = studentList,
    newVariableName = "STEM major status",
    retrievalCode = "badsvw",
    interceptRow = 17, 
    regressionType = "Logistic", 
    positiveClass = "STEM major", 
    negativeClass = "Not a STEM major",  
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
  
  #### Regression 26: Parental Education Attainment ####
  
  studentList <- processRegression(
    
    studentListDF = studentList,
    newVariableName = "Parental education attainment",
    retrievalCode = "pprqfp",
    interceptRow = 17, 
    regressionType = "Logistic", 
    positiveClass = "Parents have a college degree", 
    negativeClass = "Parents do not have a college degree",  
    thresholdVal = 0.55, # EDITED 
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
  
  #### Regression 27A: High School GPA >= 2.0 ####
  
  studentList <- processRegression(
    
    studentListDF = studentList,
    newVariableName = "High school GPA >= 2.0",
    retrievalCode = "nioduc",
    interceptRow = 17, 
    regressionType = "Logistic", 
    positiveClass = "GPA >= 2.0", 
    negativeClass = "GPA < 2.0",  
    thresholdVal = 0.9, 
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
  
  #### Regression 27B: High School GPA >= 2.5 ####
  
  studentList <- processRegression(
    
    studentListDF = studentList,
    newVariableName = "High school GPA >= 2.5",
    retrievalCode = "ymonfy",
    interceptRow = 17, 
    regressionType = "Logistic", 
    positiveClass = "GPA >= 2.5", 
    negativeClass = "GPA < 2.5",  
    thresholdVal = 0.83, # EDITED  
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
    `High school GPA >= 2.5` = ifelse(
      `High school GPA >= 2.0`=="GPA < 2.0", 
      "GPA < 2.5", 
      `High school GPA >= 2.5`
    )
  )
  
  #### End #### 
  
  #### Regression 27C: High School GPA >= 3.0 ####
  
  studentList <- processRegression(
    
    studentListDF = studentList,
    newVariableName = "High school GPA >= 3.0",
    retrievalCode = "jicznw",
    interceptRow = 17, 
    regressionType = "Logistic", 
    positiveClass = "GPA >= 3.0", 
    negativeClass = "GPA < 3.0",  
    thresholdVal = 0.75, # EDITED 
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
    `High school GPA >= 3.0` = ifelse(
      `High school GPA >= 2.5`=="GPA < 2.5", 
      "GPA < 3.0", 
      `High school GPA >= 3.0`
    )
  )
  
  #### End #### 
  
  #### Regression 27D: High School GPA >= 3.5 ####
  
  studentList <- processRegression(
    
    studentListDF = studentList,
    newVariableName = "High school GPA >= 3.5",
    retrievalCode = "kzclud",
    interceptRow = 17, 
    regressionType = "Logistic", 
    positiveClass = "GPA >= 3.5", 
    negativeClass = "GPA < 3.5",  
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
    `High school GPA >= 3.5` = ifelse(
      `High school GPA >= 3.0`=="GPA < 3.0", 
      "GPA < 3.5", 
      `High school GPA >= 3.5`
    )
  )
  
  #### End #### 
  
  #### Regression 27: Combine 27A, 27B, 27C, 27D ####
  
  labelGPA <- data.frame(
    `High school GPA >= 2.0` = character(),
    `High school GPA >= 2.5` = character(),
    `High school GPA >= 3.0` = character(),
    `High school GPA >= 3.5` = character(),
    `High school GPA` = character(),
    check.names=FALSE
  ) %>% add_row(
    `High school GPA >= 2.0` = "GPA < 2.0",
    `High school GPA >= 2.5` = "GPA < 2.5",
    `High school GPA >= 3.0` = "GPA < 3.0",
    `High school GPA >= 3.5` = "GPA < 3.5",
    `High school GPA` = "Below 2.5"
  ) %>% add_row(
    `High school GPA >= 2.0` = "GPA >= 2.0",
    `High school GPA >= 2.5` = "GPA < 2.5",
    `High school GPA >= 3.0` = "GPA < 3.0",
    `High school GPA >= 3.5` = "GPA < 3.5",
    `High school GPA` = "Between 2.0 and 2.5"
  ) %>% add_row(
    `High school GPA >= 2.0` = "GPA >= 2.0",
    `High school GPA >= 2.5` = "GPA >= 2.5",
    `High school GPA >= 3.0` = "GPA < 3.0",
    `High school GPA >= 3.5` = "GPA < 3.5",
    `High school GPA` = "Between 2.5 and 3.0"
  ) %>% add_row(
    `High school GPA >= 2.0` = "GPA >= 2.0",
    `High school GPA >= 2.5` = "GPA >= 2.5",
    `High school GPA >= 3.0` = "GPA >= 3.0",
    `High school GPA >= 3.5` = "GPA < 3.5",
    `High school GPA` = "Between 3.0 and 3.5"
  ) %>% add_row(
    `High school GPA >= 2.0` = "GPA >= 2.0",
    `High school GPA >= 2.5` = "GPA >= 2.5",
    `High school GPA >= 3.0` = "GPA >= 3.0",
    `High school GPA >= 3.5` = "GPA >= 3.5",
    `High school GPA` = "Above 3.5"
  )
  
  studentList <- left_join(x=studentList, y=labelGPA, by=c("High school GPA >= 2.0", "High school GPA >= 2.5", "High school GPA >= 3.0", "High school GPA >= 3.5"))
  rm(labelGPA)
  
  #### End #### 
  
  #### Save file #### 
  
  setwd("/Users/peter_granville/Fed State Modeling/Postprocessing data")
  
  write.csv(
    studentList, 
    paste("Set-", j, ".csv", sep=""), 
    row.names=FALSE
  )
  
  #### End #### 
  
  rm(studentList)
  
}
rm(j)


