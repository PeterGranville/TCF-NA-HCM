
#### Setup ####

library(readxl)
library(tidyverse)

#### End #### 

#### Bind Y and N ####

bindFiles <- function(file1, file2, newFileName){
  
  print(paste("Running ", file1, " and ", file2, " at ", Sys.time(), ".", sep=""))
  
  setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Temp Folder")
  
  oldFile1 <- read.csv(file1, header=TRUE, check.names=FALSE)
  oldFile2 <- read.csv(file2, header=TRUE, check.names=FALSE)
  newFile <- rbind(oldFile1, oldFile2)
  
  setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Model-V3/Simulation results")
  
  write.csv(newFile, newFileName, row.names=FALSE)

}

bindFiles("A-6N-1.csv", "A-6Y-1.csv", "A-1.csv")
bindFiles("A-6N-2.csv", "A-6Y-2.csv", "A-2.csv")
bindFiles("A-6N-3.csv", "A-6Y-3.csv", "A-3.csv")
bindFiles("A-6N-4.csv", "A-6Y-4.csv", "A-4.csv")
bindFiles("A-6N-5.csv", "A-6Y-5.csv", "A-5.csv")
bindFiles("A-6N-6.csv", "A-6Y-6.csv", "A-6.csv")
bindFiles("A-6N-7.csv", "A-6Y-7.csv", "A-7.csv")
bindFiles("A-6N-8.csv", "A-6Y-8.csv", "A-8.csv")
bindFiles("A-6N-9.csv", "A-6Y-9.csv", "A-9.csv")
bindFiles("A-6N-10.csv", "A-6Y-10.csv", "A-10.csv")

bindFiles("B-6N-1.csv", "B-6Y-1.csv", "B-1.csv")
bindFiles("B-6N-2.csv", "B-6Y-2.csv", "B-2.csv")
bindFiles("B-6N-3.csv", "B-6Y-3.csv", "B-3.csv")
bindFiles("B-6N-4.csv", "B-6Y-4.csv", "B-4.csv")
bindFiles("B-6N-5.csv", "B-6Y-5.csv", "B-5.csv")
bindFiles("B-6N-6.csv", "B-6Y-6.csv", "B-6.csv")
bindFiles("B-6N-7.csv", "B-6Y-7.csv", "B-7.csv")
bindFiles("B-6N-8.csv", "B-6Y-8.csv", "B-8.csv")
bindFiles("B-6N-9.csv", "B-6Y-9.csv", "B-9.csv")
bindFiles("B-6N-10.csv", "B-6Y-10.csv", "B-10.csv")

bindFiles("C-6N-1.csv", "C-6Y-1.csv", "C-1.csv")
bindFiles("C-6N-2.csv", "C-6Y-2.csv", "C-2.csv")
bindFiles("C-6N-3.csv", "C-6Y-3.csv", "C-3.csv")
bindFiles("C-6N-4.csv", "C-6Y-4.csv", "C-4.csv")
bindFiles("C-6N-5.csv", "C-6Y-5.csv", "C-5.csv")
bindFiles("C-6N-6.csv", "C-6Y-6.csv", "C-6.csv")
bindFiles("C-6N-7.csv", "C-6Y-7.csv", "C-7.csv")
bindFiles("C-6N-8.csv", "C-6Y-8.csv", "C-8.csv")
bindFiles("C-6N-9.csv", "C-6Y-9.csv", "C-9.csv")
bindFiles("C-6N-10.csv", "C-6Y-10.csv", "C-10.csv")

bindFiles("D-6N-1.csv", "D-6Y-1.csv", "D-1.csv")
bindFiles("D-6N-2.csv", "D-6Y-2.csv", "D-2.csv")
bindFiles("D-6N-3.csv", "D-6Y-3.csv", "D-3.csv")
bindFiles("D-6N-4.csv", "D-6Y-4.csv", "D-4.csv")
bindFiles("D-6N-5.csv", "D-6Y-5.csv", "D-5.csv")
bindFiles("D-6N-6.csv", "D-6Y-6.csv", "D-6.csv")
bindFiles("D-6N-7.csv", "D-6Y-7.csv", "D-7.csv")
bindFiles("D-6N-8.csv", "D-6Y-8.csv", "D-8.csv")
bindFiles("D-6N-9.csv", "D-6Y-9.csv", "D-9.csv")
bindFiles("D-6N-10.csv", "D-6Y-10.csv", "D-10.csv")

bindFiles("E-8N-1.csv", "E-8Y-1.csv", "E-1.csv")
bindFiles("E-8N-2.csv", "E-8Y-2.csv", "E-2.csv")
bindFiles("E-8N-3.csv", "E-8Y-3.csv", "E-3.csv")
bindFiles("E-8N-4.csv", "E-8Y-4.csv", "E-4.csv")
bindFiles("E-8N-5.csv", "E-8Y-5.csv", "E-5.csv")
bindFiles("E-8N-6.csv", "E-8Y-6.csv", "E-6.csv")
bindFiles("E-8N-7.csv", "E-8Y-7.csv", "E-7.csv")
bindFiles("E-8N-8.csv", "E-8Y-8.csv", "E-8.csv")
bindFiles("E-8N-9.csv", "E-8Y-9.csv", "E-9.csv")
bindFiles("E-8N-10.csv", "E-8Y-10.csv", "E-10.csv")

#### End #### 

#### Bind three ####

bind3Files <- function(file1, file2, file3, newFileName){
  
  print(paste("Running ", file1, ", ", file2, ", and ", file3, " at ", Sys.time(), ".", sep=""))
  
  setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Temp Folder")
  
  oldFile1 <- read.csv(file1, header=TRUE, check.names=FALSE)
  oldFile2 <- read.csv(file2, header=TRUE, check.names=FALSE)
  oldFile3 <- read.csv(file3, header=TRUE, check.names=FALSE)
  newFile <- rbind(oldFile1, oldFile2, oldFile3)
  
  setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Model-V3/Simulation results")
  
  write.csv(newFile, newFileName, row.names=FALSE)
  
}

bind3Files("F-905-1.csv", "F-910-1.csv", "F-915-1.csv", "F-1.csv")
bind3Files("F-905-3.csv", "F-910-3.csv", "F-915-3.csv", "F-3.csv")
bind3Files("F-905-4.csv", "F-910-4.csv", "F-915-4.csv", "F-4.csv")
bind3Files("F-905-5.csv", "F-910-5.csv", "F-915-5.csv", "F-5.csv")
bind3Files("F-905-7.csv", "F-910-7.csv", "F-915-7.csv", "F-7.csv")
bind3Files("F-905-8.csv", "F-910-8.csv", "F-915-8.csv", "F-8.csv")
bind3Files("F-905-9.csv", "F-910-9.csv", "F-915-9.csv", "F-9.csv")
bind3Files("F-905-10.csv", "F-910-10.csv", "F-915-10.csv", "F-10.csv")

bind3Files("B-5Y-6Y-1.csv", "B-5N-6Y-1.csv", "B-6N-1.csv", "B-1.csv")

#### End #### 

#### Bind six ####

bind6Files <- function(
    file1, file2, file3, 
    file4, file5, file6, 
    newFileName
){
  
  print(paste("Running ", file1, ", ", file2, ", ", file3, ", ", file4, ", ", file5, ", and ", file6, " at ", Sys.time(), ".", sep=""))
  
  setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Temp Folder/Weekend of September 20")
  
  oldFile1 <- read.csv(file1, header=TRUE, check.names=FALSE)
  oldFile2 <- read.csv(file2, header=TRUE, check.names=FALSE)
  oldFile3 <- read.csv(file3, header=TRUE, check.names=FALSE)
  oldFile4 <- read.csv(file4, header=TRUE, check.names=FALSE)
  oldFile5 <- read.csv(file5, header=TRUE, check.names=FALSE)
  oldFile6 <- read.csv(file6, header=TRUE, check.names=FALSE)
  newFile <- rbind(oldFile1, oldFile2, oldFile3, oldFile4, oldFile5, oldFile6)
  
  setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Model-V3/Simulation results")
  
  write.csv(newFile, newFileName, row.names=FALSE)
  
}

bind6Files(
  file1 = "F/Computer 1/F-1.csv", 
  file2 = "F/Computer 2/F-1.csv", 
  file3 = "F/Computer 3/F-1.csv", 
  file4 = "F/Computer 4/F-1.csv", 
  file5 = "F/Computer 5/F-1.csv", 
  file6 = "F/Computer 6/F-1.csv", 
  newFileName = "F-1.csv"
)

bind6Files(
  file1 = "F/Computer 1/F-3.csv", 
  file2 = "F/Computer 2/F-3.csv", 
  file3 = "F/Computer 3/F-3.csv", 
  file4 = "F/Computer 4/F-3.csv", 
  file5 = "F/Computer 5/F-3.csv", 
  file6 = "F/Computer 6/F-3.csv", 
  newFileName = "F-3.csv"
)

bind6Files(
  file1 = "F/Computer 1/F-4.csv", 
  file2 = "F/Computer 2/F-4.csv", 
  file3 = "F/Computer 3/F-4.csv", 
  file4 = "F/Computer 4/F-4.csv", 
  file5 = "F/Computer 5/F-4.csv", 
  file6 = "F/Computer 6/F-4.csv", 
  newFileName = "F-4.csv"
)

bind6Files(
  file1 = "F/Computer 1/F-5.csv", 
  file2 = "F/Computer 2/F-5.csv", 
  file3 = "F/Computer 3/F-5.csv", 
  file4 = "F/Computer 4/F-5.csv", 
  file5 = "F/Computer 5/F-5.csv", 
  file6 = "F/Computer 6/F-5.csv", 
  newFileName = "F-5.csv"
)

bind6Files(
  file1 = "F/Computer 1/F-7.csv", 
  file2 = "F/Computer 2/F-7.csv", 
  file3 = "F/Computer 3/F-7.csv", 
  file4 = "F/Computer 4/F-7.csv", 
  file5 = "F/Computer 5/F-7.csv", 
  file6 = "F/Computer 6/F-7.csv", 
  newFileName = "F-7.csv"
)

bind6Files(
  file1 = "F/Computer 1/F-8.csv", 
  file2 = "F/Computer 2/F-8.csv", 
  file3 = "F/Computer 3/F-8.csv", 
  file4 = "F/Computer 4/F-8.csv", 
  file5 = "F/Computer 5/F-8.csv", 
  file6 = "F/Computer 6/F-8.csv", 
  newFileName = "F-8.csv"
)

bind6Files(
  file1 = "F/Computer 1/F-9.csv", 
  file2 = "F/Computer 2/F-9.csv", 
  file3 = "F/Computer 3/F-9.csv", 
  file4 = "F/Computer 4/F-9.csv", 
  file5 = "F/Computer 5/F-9.csv", 
  file6 = "F/Computer 6/F-9.csv", 
  newFileName = "F-9.csv"
)

bind6Files(
  file1 = "F/Computer 1/F-10.csv", 
  file2 = "F/Computer 2/F-10.csv", 
  file3 = "F/Computer 3/F-10.csv", 
  file4 = "F/Computer 4/F-10.csv", 
  file5 = "F/Computer 5/F-10.csv", 
  file6 = "F/Computer 6/F-10.csv", 
  newFileName = "F-10.csv"
)

bind6Files(
  file1 = "F/Computer 1/F-11.csv", 
  file2 = "F/Computer 2/F-11.csv", 
  file3 = "F/Computer 3/F-11.csv", 
  file4 = "F/Computer 4/F-11.csv", 
  file5 = "F/Computer 5/F-11.csv", 
  file6 = "F/Computer 6/F-11.csv", 
  newFileName = "F-11.csv"
)

#### End #### 

#### Split the 1 files by final digit of policy index ####

for(i in c(
  "A", "B", "C", "D", "E", "F", "G", "H"
)){
  
  setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Temp Folder/Weekend of September 20/Folder 1")
  
  oldDF <- read.csv(
    paste(i, "-1.csv", sep=""), 
    header=TRUE, 
    check.names=FALSE
  ) %>% mutate(
    `Last digit` = substr(
      `Policy index`, 
      nchar(`Policy index`), 
      nchar(`Policy index`)
    )
  )
  
  setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Model-V3/Simulation results")
  
  for(j in (0:9)){
    
    newDF <- oldDF %>% filter(
      `Last digit` == as.character(j)
    )
    
    write.csv(
      newDF, 
      paste(i, "-1-", j, ".csv", sep=""), 
      row.names=FALSE
    )
    rm(newDF)
    
  }
  rm(j, oldDF)
  
}
rm(i)

#### End #### 

#### Split the 11 files by final digit of policy index ####

for(i in c(
  "A", "B", "C", "D", "E", "F", "G", "H"
)){
  
  setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Temp Folder/Weekend of September 20/Folder 11 Post")
  
  oldDF <- read.csv(
    paste(i, "-1.csv", sep=""), 
    header=TRUE, 
    check.names=FALSE
  ) %>% mutate(
    `Last digit` = substr(
      `Policy index`, 
      nchar(`Policy index`), 
      nchar(`Policy index`)
    )
  )
  
  setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Model-V3/Simulation results")
  
  for(j in (0:9)){
    
    newDF <- oldDF %>% filter(
      `Last digit` == as.character(j)
    )
    
    write.csv(
      newDF, 
      paste(i, "-1-", j, ".csv", sep=""), 
      row.names=FALSE
    )
    rm(newDF)
    
  }
  rm(j, oldDF)
  
}
rm(i)

#### End #### 

#### Run simulations #### 

tempAll <- functionB(
  studentDF, 
  stateDF, 
  collegeDF, 
  choices1b[2], choices2b[3], choices3b[1], choices4b[1], choices5b[1], choices6b[1], choices7b[1], choices8b[1], choices9b[1]
)

students0 <- functionX(tempAll[[1]])
states0 <- tempAll[[2]]
colleges0 <- tempAll[[3]]
rm(tempAll)

temp1 <- function1(students0, states0, colleges0, "Plan B")
temp2 <- function2(students0, states0, colleges0, "Plan B")
temp3 <- function3(students0, states0, colleges0, "Plan B")
temp4 <- function4(students0, states0, colleges0, "Plan B")
temp5 <- function5(students0, states0, colleges0, "Plan B")
temp6 <- function6(students0, states0, colleges0, "Plan B")
temp7 <- function7(students0, states0, colleges0, "Plan B")
temp8 <- function8(students0, states0, colleges0, "Plan B")
temp9 <- function9(students0, states0, colleges0, "Plan B")
temp10 <- function10(students0, states0, colleges0, "Plan B")
rm(students0, states0, colleges0)

#### End #### 

#### Bind output ####

if(counter==1){
  output1 <- specs(temp1, "B")
  output2 <- specs(temp2, "B")
  output3 <- specs(temp3, "B")
  output4 <- specs(temp4, "B")
  output5 <- specs(temp5, "B")
  output6 <- specs(temp6, "B")
  output7 <- specs(temp7, "B")
  output8 <- specs(temp8, "B")
  output9 <- specs(temp9, "B")
  output10 <- specs(temp10, "B")
}else{
  output1 <- rbind(output1, specs(temp1, "B"))
  output2 <- rbind(output2, specs(temp2, "B"))
  output3 <- rbind(output3, specs(temp3, "B"))
  output4 <- rbind(output4, specs(temp4, "B"))
  output5 <- rbind(output5, specs(temp5, "B"))
  output6 <- rbind(output6, specs(temp6, "B"))
  output7 <- rbind(output7, specs(temp7, "B"))
  output8 <- rbind(output8, specs(temp8, "B"))
  output9 <- rbind(output9, specs(temp9, "B"))
  output10 <- rbind(output10, specs(temp10, "B"))
}
rm(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10)

#### End #### 

#### Find all missing index values ####

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Model-V3/Simulation results")

allDF <- data.frame(
  `Policy index` = character(), 
  `I Value` = character(), 
  `J Value` = character(),
  check.names=FALSE
)

for(i in c("A", "B", "C", "D", "E", "F", "G", "H")){
  
  for(j in (1:10)){
    
    print(paste("Running ", i, "-", j, ".", sep=""))
    
    if(((i %in% c("F", "G")) & (j %in% c(2, 6)))==FALSE){
      
      tempDF <- read.csv(
        paste(i, "-", j, ".csv", sep=""), 
        header=TRUE, 
        check.names=FALSE
      )
      
      uniqueIndexVals <- unique(tempDF$`Policy index`)
      rm(tempDF)
      
      newDF <- data.frame(
        `Policy index` = uniqueIndexVals, 
        `I Value` = rep(i, length(uniqueIndexVals)), 
        `J Value` = rep(as.character(j), length(uniqueIndexVals)), 
        check.names=FALSE
      )
      allDF <- rbind(allDF, newDF)
      rm(newDF)
      
    }
     
  }
  
}

allDF <- allDF %>% mutate(
  `Count` = rep(1)
) %>% pivot_wider(
  id_cols=c(`Policy index`, `I Value`), 
  names_from=`J Value`, 
  values_from=`Count`
)

allCombos <- allCombos %>% select(
  `Policy index`
) %>% mutate(
  `Original list` = rep("Present")
)

combinedDF <- full_join(x=allDF, y=allCombos, by="Policy index")

problemDF <- combinedDF %>% filter(
  is.na(`I Value`) | is.na(`1`) | is.na(`2`) | is.na(`3`) | is.na(`4`) | is.na(`5`) | is.na(`6`) | is.na(`7`) | is.na(`8`) | is.na(`9`) | is.na(`10`) | is.na(`Original list`)
)

problemB <- problemDF %>% filter(`I Value`=="B")
problemF <- problemDF %>% filter(`I Value`=="F")
problemG <- problemDF %>% filter(`I Value`=="G")

# The F and G rows are just 2 and 6 (which makes sense)
sum(is.na(problemF$`1`)) + sum(is.na(problemG$`1`))
sum(is.na(problemF$`3`)) + sum(is.na(problemG$`3`))
sum(is.na(problemF$`4`)) + sum(is.na(problemG$`4`))
sum(is.na(problemF$`5`)) + sum(is.na(problemG$`5`))
sum(is.na(problemF$`7`)) + sum(is.na(problemG$`7`))
sum(is.na(problemF$`8`)) + sum(is.na(problemG$`8`))
sum(is.na(problemF$`9`)) + sum(is.na(problemG$`9`))
sum(is.na(problemF$`10`)) + sum(is.na(problemG$`10`))

table(is.na(problemB$`1`)) # It's just 1 
table(is.na(problemB$`2`))
table(is.na(problemB$`3`))
table(is.na(problemB$`4`))
table(is.na(problemB$`5`))
table(is.na(problemB$`6`))
table(is.na(problemB$`7`))
table(is.na(problemB$`8`))
table(is.na(problemB$`9`))
table(is.na(problemB$`10`))

badPolicyIndex <- problemB$`Policy index`

combos <- read.csv("Input combos.csv", header=TRUE, check.names=FALSE)

combos <- combos %>% mutate(
  `Didn't run` = ifelse(
    `Policy index` %in% badPolicyIndex, 
    "Didn't run", 
    "Ran"
  )
) 

combos <- combos %>% mutate(`Count` = rep(1)) %>% filter(
  `Plan`=="B"
)

aggregate(data=combos, `Count` ~ `Choice1` + `Didn't run`, FUN=sum) %>% pivot_wider(id_cols=c(`Choice1`), names_from=`Didn't run`, values_from=`Count`)
aggregate(data=combos, `Count` ~ `Choice2` + `Didn't run`, FUN=sum) %>% pivot_wider(id_cols=c(`Choice2`), names_from=`Didn't run`, values_from=`Count`)
aggregate(data=combos, `Count` ~ `Choice3` + `Didn't run`, FUN=sum) %>% pivot_wider(id_cols=c(`Choice3`), names_from=`Didn't run`, values_from=`Count`)
aggregate(data=combos, `Count` ~ `Choice4` + `Didn't run`, FUN=sum) %>% pivot_wider(id_cols=c(`Choice4`), names_from=`Didn't run`, values_from=`Count`)
aggregate(data=combos, `Count` ~ `Choice5` + `Didn't run`, FUN=sum) %>% pivot_wider(id_cols=c(`Choice5`), names_from=`Didn't run`, values_from=`Count`)
aggregate(data=combos, `Count` ~ `Choice6` + `Didn't run`, FUN=sum) %>% pivot_wider(id_cols=c(`Choice6`), names_from=`Didn't run`, values_from=`Count`)
aggregate(data=combos, `Count` ~ `Choice7` + `Didn't run`, FUN=sum) %>% pivot_wider(id_cols=c(`Choice7`), names_from=`Didn't run`, values_from=`Count`)
aggregate(data=combos, `Count` ~ `Choice8` + `Didn't run`, FUN=sum) %>% pivot_wider(id_cols=c(`Choice8`), names_from=`Didn't run`, values_from=`Count`)
aggregate(data=combos, `Count` ~ `Choice9` + `Didn't run`, FUN=sum) %>% pivot_wider(id_cols=c(`Choice9`), names_from=`Didn't run`, values_from=`Count`)

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Temp Folder")

tempDF <- read.csv("B-6Y-1.csv", header=TRUE, check.names=FALSE)

tempDF <- tempDF %>% mutate(
  `Didn't run` = ifelse(
    `Policy index` %in% badPolicyIndex, 
    "Didn't run", 
    "Ran"
  )
)

#### End #### 

file_list <- list.files(
  path = getwd(),
  pattern = "\\.csv$",
  full.names = TRUE
)
cols_to_keep <- c("Policy index")

combined_data <- file_list |>
  lapply(function(f) {
    read.csv(f, check.names=FALSE) |>
      select(all_of(cols_to_keep))
  }) |>
  bind_rows()

official <- read.csv("Input combos.csv", header=TRUE, check.names=FALSE)
