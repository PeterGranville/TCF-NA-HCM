
#### Setup ####

library(readxl)
library(scales)
library(writexl)
library(tidyverse)

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

################################################
#### Institutional administration data      ####
################################################


################################################
#### Function A: [Plan A]                   ####
#### Fed-state partnership: Reduce tuition  ####
#### and fees to $X                         ####
################################################

functionA <- function(
    
  #### List inputs ####
  
    data1, 
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
  
}

################################################
#### Function B: [Plan B]                   ####
#### Fed-state partnership: Reduce tuition  ####
#### and fees by X%                         ####
################################################

functionB <- function(

  #### List inputs ####
  
  data1, 
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
  
  data1, 
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
  
  data1, 
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
  
  data1, 
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
  
  data1, 
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
  
  data1, 
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
#### Simulation code                        ####
################################################

runSimulation <- function(
  
  #### List all inputs ####  
  
  data1
  
  #### End #### 
  
){

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

#### Write File F #### 

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

write_xlsx(sheetList, "Plan F.xlsx")
rm(sheetList, output1, output2, output3, output4, output5, output6, output7, output8, output9, output10, counter)

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

#### Write File G #### 

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

write_xlsx(sheetList, "Plan G.xlsx")
rm(sheetList, output1, output2, output3, output4, output5, output6, output7, output8, output9, output10, counter)

#### End #### 




