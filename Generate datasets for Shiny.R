
#### Setup ####

library(scales)
library(tidyverse)

#### End ####

################################################
#### Load daatsets                          ####
################################################

#### Write function to remove columns ####

removeColumns <- function(data1){
  
  data1 <- data1 %>% mutate(
    
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
    `Parent loan amount` = `Parent loan amount` * 1.159704 
    
  ) %>% mutate(
    
    # Calculate total grants, loans, and cost 
    `Total grants` = `Federal grant amount` + `VA/DOD grant amount` + `State grant amount` + `Institutional grant amount` + `Private grant amount`, 
    `Total loans` = `Federal loan amount` + `Parent loan amount`,
    `Total cost` = `Tuition and fees paid` + `Non-tuition expense budget`
  
  ) %>% mutate(
    
    # Calculate net price 
    `Net price` = pmax(0, `Total cost` - `Total grants`)
    
  ) %>% select(
    
    # Remove variables we don't need 
    -(`Effy index`), 
    -(`Student index`), 
    -(`Race NPSAS`), 
    -(`Region NPSAS`), 
    -(`Enrollment intensity NPSAS`), 
    -(`Tuition policy`), 
    -(`Effy-student index`), 
    -(`Receives federal grants`),
    -(`Receives VA/DOD grants`),
    -(`Receives state grants`),
    -(`Receives institutional grants`),
    -(`Receives private grants`),
    -(`Receives federal loans`),
    -(`Receives parent loans`),
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

#### Load procesed output (LONG WAY: 20 MIN) ####

setwd("/Users/peter_granville/Fed State Modeling/Postprocessing data")

for(i in (1:176)){

  print(Sys.time())

  print(paste("Merging dataset ", i, ".", sep=""))

  if(i==1){
    studentDF <- removeColumns(read.csv(
      paste("Set-", i, ".csv", sep=""),
      header=TRUE,
      check.names=FALSE
    )) %>% mutate(
      `Source file number` = rep(i)
    )
  }else{
    tempDF <- removeColumns(read.csv(
      paste("Set-", i, ".csv", sep=""),
      header=TRUE,
      check.names=FALSE
    )) %>% mutate(
      `Source file number` = rep(i)
    )
    studentDF <- rbind(
      studentDF,
      tempDF
    )
    rm(tempDF)
  }

}
rm(i, removeColumns)

setwd("/Users/peter_granville/Fed State Modeling")

write.csv(studentDF, "All merged student data.csv", row.names=FALSE)

setwd("/Users/peter_granville/Fed State Modeling")

#### End ####

# #### Load processed output (SHORT WAY: 2 MIN) ####
# 
# studentDF <- read.csv(
#   "All merged student data.csv",
#   header=TRUE,
#   check.names=FALSE
# )
# 
# #### End ####

#### Load College Scorecard institution data ####


#### End #### 

#### Load state finance data ####


#### End #### 

#### Load IPEDS finance data ####


#### End #### 

################################################
#### Checking validity of studentDF         ####
################################################

#### Aggregate sums #### 

dollar(sum(studentDF$`Federal grant amount`))
dollar(sum(studentDF$`State grant amount`))
dollar(sum(studentDF$`Institutional grant amount`))
dollar(sum(studentDF$`Private grant amount`))
dollar(sum(studentDF$`Federal loan amount`))
dollar(sum(studentDF$`Parent loan amount`))

#### End #### 

#### Turn numeric values into groups ####

studentDF <- studentDF %>% mutate(
  `Receiving federal grants` = ifelse(`Federal grant amount` > 0, "Yes", "No"), 
  `Receiving VA/DOD grants` = ifelse(`VA/DOD grant amount` > 0, "Yes", "No"), 
  `Receiving state grants` = ifelse(`State grant amount` > 0, "Yes", "No"), 
  `Receiving institutional grants` = ifelse(`Institutional grant amount` > 0, "Yes", "No"), 
  `Receiving private grants` = ifelse(`Private grant amount` > 0, "Yes", "No"), 
  `Receiving federal loans` = ifelse(`Federal loan amount` > 0, "Yes", "No"), 
  `Receiving parent loans` = ifelse(`Parent loan amount` > 0, "Yes", "No")
) %>% mutate(
  `Zero EFC status` = ifelse(
    `EFC`==0, "Zero EFC", "Nonzero EFC"
  )
) 

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
  
  print(tempDF)
  
  rm(tempDF, totalStudents)
  
}

#### End #### 

#### Write function to display distribution as percentiles ####

showPercentiles <- function(variableName, removeZeros){
  
  tempDF <- studentDF %>% select(
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

#### Run function to display percentages #### 

showDistribution("Control")
showDistribution("Region")
showDistribution("Race")
showDistribution("Carnegie NPSAS")
showDistribution("Enrollment intensity")
showDistribution("Gender")
showDistribution("Zero-EFC")
showPercentiles("EFC", removeZeros=TRUE)
showDistribution("Tuition jurisdiction")
showPercentiles("Tuition and fees", removeZeros=TRUE)
showPercentiles("Age", removeZeros=FALSE)
showDistribution("Citizenship")
showDistribution("Dependency status")
showDistribution("Applied for federal aid")
showDistribution("Veteran status")
showPercentiles("Non-tuition expense budget", removeZeros=FALSE)
showDistribution("Receiving federal grants")
showDistribution("Receiving VA/DOD grants")
showDistribution("Receiving state grants")
showDistribution("Receiving institutional grants")
showDistribution("Receiving private grants")
showDistribution("Receiving federal loans")
showDistribution("Receiving parent loans")
showPercentiles("Federal grant amount", removeZeros=FALSE)
showPercentiles("VA/DOD grant amount", removeZeros=FALSE)
showPercentiles("State grant amount", removeZeros=FALSE)
showPercentiles("Institutional grant amount", removeZeros=FALSE)
showPercentiles("Private grant amount", removeZeros=FALSE)
showPercentiles("Federal loan amount", removeZeros=FALSE)
showPercentiles("Parent loan amount", removeZeros=FALSE)
showDistribution("Parent status")
showDistribution("Parental education attainment")
showDistribution("High school GPA")
showPercentiles("Total grants", removeZeros=FALSE)
showPercentiles("Total loans", removeZeros=FALSE)
showPercentiles("Total cost", removeZeros=FALSE)
showPercentiles("Net price", removeZeros=FALSE)



#### End #### 

################################################
#### Run baseline numbers                   ####
################################################

#### Create baseline dataframe ####

baselineDF <- studentDF 

#### End #### 

#### Turn numeric values into groups #### 

baselineDF <- baselineDF %>% mutate(
  `Net price group` = ifelse(
    `Net price`==0, "$0", ifelse(
      between(`Net price`, 1, 5000), "$1 to $5,000", ifelse(
        between(`Net price`, 5001, 10000), "$5,001 to $10,000", ifelse(
          between(`Net price`, 10001, 15000), "$10,001 to $15,000", ifelse(
            between(`Net price`, 15001, 20000), "$15,001 to $20,000", "Above $20,000"
          )
        )
      )
    )
  )
)

baselineDF <- baselineDF %>% mutate(
  `Total loans group` = ifelse(
    `Total loans`==0, "$0", ifelse(
      between(`Total loans`, 1, 5000), "$1 to $5,000", ifelse(
        between(`Total loans`, 5001, 10000), "$5,001 to $10,000", ifelse(
          between(`Total loans`, 10001, 15000), "$10,001 to $15,000", ifelse(
            between(`Total loans`, 15001, 20000), "$15,001 to $20,000", "Above $20,000"
          )
        )
      )
    )
  )
)

#### End #### 

#### Table 1: Total students #### 

table1.students <- sum(baselineDF$Count)

#### End ####

#### Table 2: Net price ####

table2 <- aggregate(
  data=baselineDF,
  `Count` ~ `Net price group`, 
  FUN=sum
) %>% pivot_wider(
  names_from=`Net price group`, 
  values_from=`Count`
) %>% mutate(
  `$0` = ifelse(is.na(`$0`), 0, `$0`), 
  `$1 to $5,000` = ifelse(is.na(`$1 to $5,000`), 0, `$1 to $5,000`), 
  `$5,001 to $10,000` = ifelse(is.na(`$5,001 to $10,000`), 0, `$5,001 to $10,000`), 
  `$10,001 to $15,000` = ifelse(is.na(`$10,001 to $15,000`), 0, `$10,001 to $15,000`), 
  `$15,001 to $20,000` = ifelse(is.na(`$15,001 to $20,000`), 0, `$15,001 to $20,000`), 
  `Above $20,000` = ifelse(is.na(`Above $20,000`), 0, `Above $20,000`)
) %>% mutate(
  `Total` = `$0` + `$1 to $5,000` + `$5,001 to $10,000` + `$10,001 to $15,000` + `$15,001 to $20,000` + `Above $20,000`
) %>% mutate(
  `Share $0` = `$0` / `Total`, 
  `Share $1 to $5,000` = `$1 to $5,000` / `Total`, 
  `Share $5,001 to $10,000` = `$5,001 to $10,000` / `Total`,
  `Share $10,001 to $15,000` = `$10,001 to $15,000` / `Total`, 
  `Share $15,001 to $20,000` = `$15,001 to $20,000` / `Total`, 
  `Share Above $20,000` = `Above $20,000` / `Total`
)

table2.netprice0 <- table2$`Share $0`[1]
table2.netprice5000 <- table2$`Share $1 to $5,000`[1]
table2.netprice10000 <- table2$`Share $5,001 to $10,000`[1]
table2.netprice15000 <- table2$`Share $10,001 to $15,000`[1]
table2.netprice20000 <- table2$`Share $15,001 to $20,000`[1]
table2.netpriceceiling <- table2$`Share Above $20,000`[1]

#### End #### 

#### Table 3: Student loans ####

table3 <- aggregate(
  data=baselineDF,
  `Count` ~ `Total loans group`, 
  FUN=sum
) %>% pivot_wider(
  names_from=`Total loans group`, 
  values_from=`Count`
) %>% mutate(
  `$0` = ifelse(is.na(`$0`), 0, `$0`), 
  `$1 to $5,000` = ifelse(is.na(`$1 to $5,000`), 0, `$1 to $5,000`), 
  `$5,001 to $10,000` = ifelse(is.na(`$5,001 to $10,000`), 0, `$5,001 to $10,000`), 
  `$10,001 to $15,000` = ifelse(is.na(`$10,001 to $15,000`), 0, `$10,001 to $15,000`), 
  `$15,001 to $20,000` = ifelse(is.na(`$15,001 to $20,000`), 0, `$15,001 to $20,000`), 
  `Above $20,000` = ifelse(is.na(`Above $20,000`), 0, `Above $20,000`)
) %>% mutate(
  `Total` = `$0` + `$1 to $5,000` + `$5,001 to $10,000` + `$10,001 to $15,000` + `$15,001 to $20,000` + `Above $20,000`
) %>% mutate(
  `Share $0` = `$0` / `Total`, 
  `Share $1 to $5,000` = `$1 to $5,000` / `Total`, 
  `Share $5,001 to $10,000` = `$5,001 to $10,000` / `Total`,
  `Share $10,001 to $15,000` = `$10,001 to $15,000` / `Total`, 
  `Share $15,001 to $20,000` = `$15,001 to $20,000` / `Total`, 
  `Share Above $20,000` = `Above $20,000` / `Total`
)

table3.totalloans0 <- table3$`Share $0`[1]
table3.totalloans5000 <- table3$`Share $1 to $5,000`[1]
table3.totalloans10000 <- table3$`Share $5,001 to $10,000`[1]
table3.totalloans15000 <- table3$`Share $10,001 to $15,000`[1]
table3.totalloans20000 <- table3$`Share $15,001 to $20,000`[1]
table3.totalloansceiling <- table3$`Share Above $20,000`[1]

#### End #### 

#### Table 4a: Beneficiaries by race ####

table4a <- aggregate(
  data=baselineDF, 
  `Count` ~ `Race`, 
  FUN=sum
) %>% pivot_wider(
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
  `White` = ifelse(is.na(`White`), 0, `White`),
) %>% mutate(
  `Total` = `American Indian or Alaska Native` + `Asian` + `Black or African American` + `Hispanic or Latino` + `More than one race` + `Native Hawaiian/other Pacific Islander` + `Race/ethnicity unknown` + `U.S. Nonresident` + `White` 
) %>% mutate(
  `Share American Indian or Alaska Native` = `American Indian or Alaska Native` / `Total`,
  `Share Asian` = `Asian` / `Total`,
  `Share Black or African American` = `Black or African American` / `Total`,
  `Share Hispanic or Latino` = `Hispanic or Latino` / `Total`,
  `Share More than one race` = `More than one race` / `Total`,
  `Share Native Hawaiian/other Pacific Islander` = `Native Hawaiian/other Pacific Islander` / `Total`,
  `Share Race/ethnicity unknown` = `Race/ethnicity unknown` / `Total`,
  `Share U.S. Nonresident` = `U.S. Nonresident` / `Total`,
  `Share White` = `White` / `Total`,
)

table4.aian <- table4a$`Share American Indian or Alaska Native`[1]
table4.asia <- table4a$`Share Asian`[1]
table4.bkaa <- table4a$`Share Black or African American`[1]
table4.hisp <- table4a$`Share Hispanic or Latino`[1]
table4.2mor <- table4a$`Share More than one race`[1]
table4.nhpi <- table4a$`Share Native Hawaiian/other Pacific Islander`[1]
table4.unkn <- table4a$`Share Race/ethnicity unknown`[1]
table4.nonr <- table4a$`Share U.S. Nonresident`[1]
table4.whit <- table4a$`Share White`[1]

#### End #### 

#### Table 4b: Beneficiaries by gender ####

table4b <- aggregate(
  data=baselineDF, 
  `Count` ~ `Gender`, 
  FUN=sum
) %>% pivot_wider(
  names_from=`Gender`, 
  values_from=`Count`
) %>% mutate(
  `Male` = ifelse(is.na(`Male`), 0, `Male`),
  `Female` = ifelse(is.na(`Female`), 0, `Female`),
) %>% mutate(
  `Total` = `Male` + `Female` 
) %>% mutate(
  `Share Male` = `Male` / `Total`,
  `Share Female` = `Female` / `Total`,
)

table4.male <- table4b$`Share Male`[1]
table4.female <- table4b$`Share Female`[1]

#### End #### 

#### Table 4c: Beneficiaries by first-gen status ####

table4c <- aggregate(
  data=baselineDF, 
  `Count` ~ `Parental education attainment`, 
  FUN=sum
) %>% pivot_wider(
  names_from=`Parental education attainment`, 
  values_from=`Count`
) %>% mutate(
  `Parents do not have a college degree` = ifelse(is.na(`Parents do not have a college degree`), 0, `Parents do not have a college degree`),
  `Parents have a college degree` = ifelse(is.na(`Parents have a college degree`), 0, `Parents have a college degree`),
) %>% mutate(
  `Total` = `Parents do not have a college degree` + `Parents have a college degree` 
) %>% mutate(
  `Share first-gen` = `Parents do not have a college degree` / `Total`,
  `Share not first-gen` = `Parents have a college degree` / `Total`,
)

table4.firstgen <- table4c$`Share first-gen`[1]
table4.notfirstgen <- table4c$`Share not first-gen`[1]

#### End #### 

#### Table 4d: Beneficiaries by dependency status ####

table4d <- aggregate(
  data=baselineDF, 
  `Count` ~ `Dependency status`, 
  FUN=sum
) %>% pivot_wider(
  names_from=`Dependency status`, 
  values_from=`Count`
) %>% mutate(
  `Dependent` = ifelse(is.na(`Dependent`), 0, `Dependent`),
  `Independent` = ifelse(is.na(`Independent`), 0, `Independent`),
) %>% mutate(
  `Total` = `Dependent` + `Independent` 
) %>% mutate(
  `Share Dependent` = `Dependent` / `Total`,
  `Share Independent` = `Independent` / `Total`,
)

table4.dependent <- table4d$`Share Dependent`[1]
table4.independent <- table4d$`Share Independent`[1]

#### End #### 

#### Table 4e: Beneficiaries by zero-EFC status ####

table4e <- aggregate(
  data=baselineDF, 
  `Count` ~ `Zero EFC status`, 
  FUN=sum
) %>% pivot_wider(
  names_from=`Zero EFC status`, 
  values_from=`Count`
) %>% mutate(
  `Zero EFC` = ifelse(is.na(`Zero EFC`), 0, `Zero EFC`),
  `Nonzero EFC` = ifelse(is.na(`Nonzero EFC`), 0, `Nonzero EFC`),
) %>% mutate(
  `Total` = `Zero EFC` + `Nonzero EFC` 
) %>% mutate(
  `Share Zero EFC` = `Zero EFC` / `Total`,
  `Share Nonzero EFC` = `Nonzero EFC` / `Total`,
)

table4.zeroEFC <- table4e$`Share Zero EFC`[1]
table4.nonzeroEFC <- table4e$`Share Nonzero EFC`[1]

#### End #### 

#### Table 4f: Beneficiaries by parent status ####

table4f <- aggregate(
  data=baselineDF, 
  `Count` ~ `Parent status`, 
  FUN=sum
) %>% pivot_wider(
  names_from=`Parent status`, 
  values_from=`Count`
) %>% mutate(
  `Has dependents` = ifelse(is.na(`Has dependents`), 0, `Has dependents`),
  `Does not have dependents` = ifelse(is.na(`Does not have dependents`), 0, `Does not have dependents`),
) %>% mutate(
  `Total` = `Has dependents` + `Does not have dependents` 
) %>% mutate(
  `Share parent` = `Has dependents` / `Total`,
  `Share nonparent` = `Does not have dependents` / `Total`,
)

table4.parent <- table4f$`Share parent`[1]
table4.nonparent <- table4f$`Share nonparent`[1]

#### End #### 

#### Table 4g: Beneficiaries by Veteran status ####

table4g <- aggregate(
  data=baselineDF, 
  `Count` ~ `Veteran status`, 
  FUN=sum
) %>% pivot_wider(
  names_from=`Veteran status`, 
  values_from=`Count`
) %>% mutate(
  `Veteran` = ifelse(is.na(`Veteran`), 0, `Veteran`),
  `Not a veteran` = ifelse(is.na(`Not a veteran`), 0, `Not a veteran`),
) %>% mutate(
  `Total` = `Veteran` + `Not a veteran` 
) %>% mutate(
  `Share Veteran` = `Veteran` / `Total`,
  `Share Not a veteran` = `Not a veteran` / `Total`,
)

table4.veteran <- table4g$`Share Veteran`[1]
table4.nonveteran <- table4g$`Share Not a veteran`[1]

#### End #### 

#### Return baseline results ####

baselineResults <- data.frame(
  `table1.students` = c(table1.students),
  `table2.netprice0` = c(table2.netprice0),
  `table2.netprice5000` = c(table2.netprice5000),
  `table2.netprice10000` = c(table2.netprice10000),
  `table2.netprice15000` = c(table2.netprice15000),
  `table2.netprice20000` = c(table2.netprice20000),
  `table2.netpriceceiling` = c(table2.netpriceceiling),
  `table3.totalloans0` = c(table3.totalloans0),
  `table3.totalloans5000` = c(table3.totalloans5000),
  `table3.totalloans10000` = c(table3.totalloans10000),
  `table3.totalloans15000` = c(table3.totalloans15000),
  `table3.totalloans20000` = c(table3.totalloans20000),
  `table3.totalloansceiling` = c(table3.totalloansceiling),
  `table4.aian` = c(table4.aian),
  `table4.asia` = c(table4.asia),
  `table4.bkaa` = c(table4.bkaa),
  `table4.hisp` = c(table4.hisp),
  `table4.2mor` = c(table4.2mor),
  `table4.nhpi` = c(table4.nhpi),
  `table4.unkn` = c(table4.unkn),
  `table4.nonr` = c(table4.nonr),
  `table4.whit` = c(table4.whit),
  `table4.male` = c(table4.male),
  `table4.female` = c(table4.female),
  `table4.firstgen` = c(table4.firstgen),
  `table4.notfirstgen` = c(table4.notfirstgen),
  `table4.dependent` = c(table4.dependent),
  `table4.independent` = c(table4.independent),
  `table4.zeroEFC` = c(table4.zeroEFC),
  `table4.nonzeroEFC` = c(table4.nonzeroEFC),
  `table4.parent` = c(table4.parent),
  `table4.nonparent` = c(table4.nonparent),
  `table4.veteran` = c(table4.veteran),
  `table4.nonveteran` = c(table4.nonveteran)
)

write.csv(baselineResults, "Baseline results.csv", row.names=FALSE)

#### End #### 

#### Delete objects #### 

rm(
  baselineDF,
  table2,
  table3,
  table4a,
  table4b,
  table4c,
  table4d,
  table4e,
  table4f,
  table4g, 
  table1.students,
  table2.netprice0,
  table2.netprice5000,
  table2.netprice10000,
  table2.netprice15000,
  table2.netprice20000,
  table2.netpriceceiling,
  table3.totalloans0,
  table3.totalloans5000,
  table3.totalloans10000,
  table3.totalloans15000,
  table3.totalloans20000,
  table3.totalloansceiling,
  table4.aian,
  table4.asia,
  table4.bkaa,
  table4.hisp,
  table4.2mor,
  table4.nhpi,
  table4.unkn,
  table4.nonr,
  table4.whit,
  table4.male,
  table4.female,
  table4.firstgen,
  table4.notfirstgen,
  table4.dependent,
  table4.independent,
  table4.zeroEFC,
  table4.nonzeroEFC,
  table4.parent,
  table4.nonparent,
  table4.veteran,
  table4.nonveteran
)

#### End #### 

################################################
#### Simulation code                        ####
################################################

runSimulation <- function(
  
  #### List all inputs ####  
  
  data1, 
  elig.carnegie, 
  elig.control, 
  elig.halftime, 
  elig.efc, 
  elig.outofstate, 
  elig.noncitizen, 
  elig.fafsa, 
  elig.gpa,
  program.amount
  
  #### End #### 
  
){
  
  #### Establish eligiblity vectors #### 
  
  # Carnegie Classification
  if(elig.carnegie=="Associate's colleges only"){
    vector.carnegie <- c("Associate's")
  }
  if(elig.carnegie=="Associate's and bachelor's colleges only"){
    vector.carnegie <- c("Associate's", "Baccalaureate")
  }
  if(elig.carnegie=="All"){
    vector.carnegie <- c("Associate's", "Baccalaureate", "Research & Doctoral", "Master's", "Special Focus & other", "Not degree-granting")
  }

  # Control
  if(elig.control=="Public only"){
    vector.control <- c("Public")
  }
  if(elig.control=="Public and nonprofit"){
    vector.control <- c("Public", "Private nonprofit")
  }
  if(elig.control=="All"){
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
  if(elig.efc=="All"){
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
  if(elig.gpa=="All"){
    vector.gpa <- c("Above 3.5", "Between 3.0 and 3.5", "Between 2.5 and 3.0", "Below 2.5")
  }
  
  #### End #### 
  
  #### Classify by eligibility ####
  
  data1 <- data1 %>% mutate(
    `Eligible for program` = ifelse(
      (`Carnegie NPSAS` %in% vector.carnegie) & (`Control` %in% vector.control) & (`Enrollment intensity` %in% vector.intensity) & (`EFC group` %in% vector.efc) & (`Tuition jurisdiction` %in% vector.oos) & (`Citizenship` %in% vector.cit) & (`Applied for federal aid` %in% vector.fafsa) & (`High school GPA` %in% vector.gpa), 
      "Eligible", 
      "Not eligible"
    )
  )
  
  #### End #### 
  
  #### Account for grant amount #### 
  
  if(program.amount=="$2,000"){
    coefficient.amount <- 2000
  }
  if(program.amount=="$5,000"){
    coefficient.amount <- 5000
  }
  if(program.amount=="$10,000"){
    coefficient.amount <- 10000
  }
  data1 <- data1 %>% mutate(
    `Program amount` = ifelse(
      `Eligible for program`=="Eligible", 
      coefficient.amount, 
      0
    )
  )
  
  #### End #### 
  
  #### Recalculate net price ####
  
  data1 <- data1 %>% mutate(
    `Total grants` = `Total grants` + `Program amount`
  ) %>% mutate(
    `Net price` = pmax(0, `Total cost` - `Total grants`), 
    `Total loans` = pmax(0, `Total loans` - `Program amount`)
  )
  
  #### End #### 
  
  #### Turn numeric values into groups #### 
  
  data1 <- data1 %>% mutate(
    `Net price group` = ifelse(
      `Net price`==0, "$0", ifelse(
        between(`Net price`, 1, 5000), "$1 to $5,000", ifelse(
          between(`Net price`, 5001, 10000), "$5,001 to $10,000", ifelse(
            between(`Net price`, 10001, 15000), "$10,001 to $15,000", ifelse(
              between(`Net price`, 15001, 20000), "$15,001 to $20,000", "Above $20,000"
            )
          )
        )
      )
    )
  )
  
  data1 <- data1 %>% mutate(
    `Total loans group` = ifelse(
      `Total loans`==0, "$0", ifelse(
        between(`Total loans`, 1, 5000), "$1 to $5,000", ifelse(
          between(`Total loans`, 5001, 10000), "$5,001 to $10,000", ifelse(
            between(`Total loans`, 10001, 15000), "$10,001 to $15,000", ifelse(
              between(`Total loans`, 15001, 20000), "$15,001 to $20,000", "Above $20,000"
            )
          )
        )
      )
    )
  )
  
  #### End #### 
  
  #### Table 1a: Total recipients, total cost ####
  
  table1a <- aggregate(
    data=data1, 
    cbind(`Count`, `Program amount`) ~ `Eligible for program`, 
    FUN=sum
  ) %>% filter(
    `Eligible for program`=="Eligible"
  )
  
  table1.recipients <- table1a$`Count`[1]
  table1.totalamount <- table1a$`Program amount`[1]
  
  #### End #### 
  
  #### Table 1b: Share eligible #### 
  
  table1b <- aggregate(
    data=data1, 
    `Count` ~ `Eligible for program`, 
    FUN=sum
  ) %>% pivot_wider(
    names_from=`Eligible for program`, 
    values_from=`Count`
  ) %>% mutate(
    `Eligible` = ifelse(is.na(`Eligible`), 0, `Eligible`), 
    `Not eligible` = ifelse(is.na(`Not eligible`), 0, `Not eligible`)
  ) %>% mutate(
    `Share eligible` = `Eligible` / (`Eligible` + `Not eligible`)
  )
  table1.share <- table1b$`Share eligible`[1]
  
  #### End #### 
  
  #### Table 2: Net price ####
  
  table2 <- aggregate(
    data=data1,
    `Count` ~ `Net price group`, 
    FUN=sum
  ) %>% pivot_wider(
    names_from=`Net price group`, 
    values_from=`Count`
  ) %>% mutate(
    `$0` = ifelse(is.na(`$0`), 0, `$0`), 
    `$1 to $5,000` = ifelse(is.na(`$1 to $5,000`), 0, `$1 to $5,000`), 
    `$5,001 to $10,000` = ifelse(is.na(`$5,001 to $10,000`), 0, `$5,001 to $10,000`), 
    `$10,001 to $15,000` = ifelse(is.na(`$10,001 to $15,000`), 0, `$10,001 to $15,000`), 
    `$15,001 to $20,000` = ifelse(is.na(`$15,001 to $20,000`), 0, `$15,001 to $20,000`), 
    `Above $20,000` = ifelse(is.na(`Above $20,000`), 0, `Above $20,000`)
  ) %>% mutate(
    `Total` = `$0` + `$1 to $5,000` + `$5,001 to $10,000` + `$10,001 to $15,000` + `$15,001 to $20,000` + `Above $20,000`
  ) %>% mutate(
    `Share $0` = `$0` / `Total`, 
    `Share $1 to $5,000` = `$1 to $5,000` / `Total`, 
    `Share $5,001 to $10,000` = `$5,001 to $10,000` / `Total`,
    `Share $10,001 to $15,000` = `$10,001 to $15,000` / `Total`, 
    `Share $15,001 to $20,000` = `$15,001 to $20,000` / `Total`, 
    `Share Above $20,000` = `Above $20,000` / `Total`
  )
  
  table2.netprice0 <- table2$`Share $0`[1]
  table2.netprice5000 <- table2$`Share $1 to $5,000`[1]
  table2.netprice10000 <- table2$`Share $5,001 to $10,000`[1]
  table2.netprice15000 <- table2$`Share $10,001 to $15,000`[1]
  table2.netprice20000 <- table2$`Share $15,001 to $20,000`[1]
  table2.netpriceceiling <- table2$`Share Above $20,000`[1]
  
  #### End #### 
  
  #### Table 3: Student loans ####
  
  table3 <- aggregate(
    data=data1,
    `Count` ~ `Total loans group`, 
    FUN=sum
  ) %>% pivot_wider(
    names_from=`Total loans group`, 
    values_from=`Count`
  ) %>% mutate(
    `$0` = ifelse(is.na(`$0`), 0, `$0`), 
    `$1 to $5,000` = ifelse(is.na(`$1 to $5,000`), 0, `$1 to $5,000`), 
    `$5,001 to $10,000` = ifelse(is.na(`$5,001 to $10,000`), 0, `$5,001 to $10,000`), 
    `$10,001 to $15,000` = ifelse(is.na(`$10,001 to $15,000`), 0, `$10,001 to $15,000`), 
    `$15,001 to $20,000` = ifelse(is.na(`$15,001 to $20,000`), 0, `$15,001 to $20,000`), 
    `Above $20,000` = ifelse(is.na(`Above $20,000`), 0, `Above $20,000`)
  ) %>% mutate(
    `Total` = `$0` + `$1 to $5,000` + `$5,001 to $10,000` + `$10,001 to $15,000` + `$15,001 to $20,000` + `Above $20,000`
  ) %>% mutate(
    `Share $0` = `$0` / `Total`, 
    `Share $1 to $5,000` = `$1 to $5,000` / `Total`, 
    `Share $5,001 to $10,000` = `$5,001 to $10,000` / `Total`,
    `Share $10,001 to $15,000` = `$10,001 to $15,000` / `Total`, 
    `Share $15,001 to $20,000` = `$15,001 to $20,000` / `Total`, 
    `Share Above $20,000` = `Above $20,000` / `Total`
  )
  
  table3.totalloans0 <- table3$`Share $0`[1]
  table3.totalloans5000 <- table3$`Share $1 to $5,000`[1]
  table3.totalloans10000 <- table3$`Share $5,001 to $10,000`[1]
  table3.totalloans15000 <- table3$`Share $10,001 to $15,000`[1]
  table3.totalloans20000 <- table3$`Share $15,001 to $20,000`[1]
  table3.totalloansceiling <- table3$`Share Above $20,000`[1]
  
  #### End #### 
  
  #### Table 4a: Beneficiaries by race ####
  
  table4a <- aggregate(
    data=data1, 
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
    `White` = ifelse(is.na(`White`), 0, `White`),
  ) %>% mutate(
    `Total` = `American Indian or Alaska Native` + `Asian` + `Black or African American` + `Hispanic or Latino` + `More than one race` + `Native Hawaiian/other Pacific Islander` + `Race/ethnicity unknown` + `U.S. Nonresident` + `White` 
  ) %>% mutate(
    `Share American Indian or Alaska Native` = `American Indian or Alaska Native` / `Total`,
    `Share Asian` = `Asian` / `Total`,
    `Share Black or African American` = `Black or African American` / `Total`,
    `Share Hispanic or Latino` = `Hispanic or Latino` / `Total`,
    `Share More than one race` = `More than one race` / `Total`,
    `Share Native Hawaiian/other Pacific Islander` = `Native Hawaiian/other Pacific Islander` / `Total`,
    `Share Race/ethnicity unknown` = `Race/ethnicity unknown` / `Total`,
    `Share U.S. Nonresident` = `U.S. Nonresident` / `Total`,
    `Share White` = `White` / `Total`,
  )
  
  table4.aian <- table4a$`Share American Indian or Alaska Native`[1]
  table4.asia <- table4a$`Share Asian`[1]
  table4.bkaa <- table4a$`Share Black or African American`[1]
  table4.hisp <- table4a$`Share Hispanic or Latino`[1]
  table4.2mor <- table4a$`Share More than one race`[1]
  table4.nhpi <- table4a$`Share Native Hawaiian/other Pacific Islander`[1]
  table4.unkn <- table4a$`Share Race/ethnicity unknown`[1]
  table4.nonr <- table4a$`Share U.S. Nonresident`[1]
  table4.whit <- table4a$`Share White`[1]
  
  #### End #### 
  
  #### Table 4b: Beneficiaries by gender ####
  
  table4b <- aggregate(
    data=data1, 
    `Count` ~ `Eligible for program` + `Gender`, 
    FUN=sum
  ) %>% pivot_wider(
    id_cols=c(`Eligible for program`), 
    names_from=`Gender`, 
    values_from=`Count`
  ) %>% mutate(
    `Male` = ifelse(is.na(`Male`), 0, `Male`),
    `Female` = ifelse(is.na(`Female`), 0, `Female`),
  ) %>% mutate(
    `Total` = `Male` + `Female` 
  ) %>% mutate(
    `Share Male` = `Male` / `Total`,
    `Share Female` = `Female` / `Total`,
  )
  
  table4.male <- table4b$`Share Male`[1]
  table4.female <- table4b$`Share Female`[1]
  
  #### End #### 
  
  #### Table 4c: Beneficiaries by first-gen status ####
  
  table4c <- aggregate(
    data=data1, 
    `Count` ~ `Eligible for program` + `Parental education attainment`, 
    FUN=sum
  ) %>% pivot_wider(
    id_cols=c(`Eligible for program`), 
    names_from=`Parental education attainment`, 
    values_from=`Count`
  ) %>% mutate(
    `Parents do not have a college degree` = ifelse(is.na(`Parents do not have a college degree`), 0, `Parents do not have a college degree`),
    `Parents have a college degree` = ifelse(is.na(`Parents have a college degree`), 0, `Parents have a college degree`),
  ) %>% mutate(
    `Total` = `Parents do not have a college degree` + `Parents have a college degree` 
  ) %>% mutate(
    `Share first-gen` = `Parents do not have a college degree` / `Total`,
    `Share not first-gen` = `Parents have a college degree` / `Total`,
  )
  
  table4.firstgen <- table4c$`Share first-gen`[1]
  table4.notfirstgen <- table4c$`Share not first-gen`[1]
  
  #### End #### 
  
  #### Table 4d: Beneficiaries by dependency status ####
  
  table4d <- aggregate(
    data=data1, 
    `Count` ~ `Eligible for program` + `Dependency status`, 
    FUN=sum
  ) %>% pivot_wider(
    id_cols=c(`Eligible for program`), 
    names_from=`Dependency status`, 
    values_from=`Count`
  ) %>% mutate(
    `Dependent` = ifelse(is.na(`Dependent`), 0, `Dependent`),
    `Independent` = ifelse(is.na(`Independent`), 0, `Independent`),
  ) %>% mutate(
    `Total` = `Dependent` + `Independent` 
  ) %>% mutate(
    `Share Dependent` = `Dependent` / `Total`,
    `Share Independent` = `Independent` / `Total`,
  )
  
  table4.dependent <- table4d$`Share Dependent`[1]
  table4.independent <- table4d$`Share Independent`[1]
  
  #### End #### 
  
  #### Table 4e: Beneficiaries by zero-EFC status ####
  
  table4e <- aggregate(
    data=data1, 
    `Count` ~ `Eligible for program` + `Zero EFC status`, 
    FUN=sum
  ) %>% pivot_wider(
    id_cols=c(`Eligible for program`), 
    names_from=`Zero EFC status`, 
    values_from=`Count`
  ) %>% mutate(
    `Zero EFC` = ifelse(is.na(`Zero EFC`), 0, `Zero EFC`),
    `Nonzero EFC` = ifelse(is.na(`Nonzero EFC`), 0, `Nonzero EFC`),
  ) %>% mutate(
    `Total` = `Zero EFC` + `Nonzero EFC` 
  ) %>% mutate(
    `Share Zero EFC` = `Zero EFC` / `Total`,
    `Share Nonzero EFC` = `Nonzero EFC` / `Total`,
  )
  
  table4.zeroEFC <- table4e$`Share Zero EFC`[1]
  table4.nonzeroEFC <- table4e$`Share Nonzero EFC`[1]
  
  #### End #### 
  
  #### Table 4f: Beneficiaries by parent status ####
  
  table4f <- aggregate(
    data=data1, 
    `Count` ~ `Eligible for program` + `Parent status`, 
    FUN=sum
  ) %>% pivot_wider(
    id_cols=c(`Eligible for program`), 
    names_from=`Parent status`, 
    values_from=`Count`
  ) %>% mutate(
    `Has dependents` = ifelse(is.na(`Has dependents`), 0, `Has dependents`),
    `Does not have dependents` = ifelse(is.na(`Does not have dependents`), 0, `Does not have dependents`),
  ) %>% mutate(
    `Total` = `Has dependents` + `Does not have dependents` 
  ) %>% mutate(
    `Share parent` = `Has dependents` / `Total`,
    `Share nonparent` = `Does not have dependents` / `Total`,
  )
  
  table4.parent <- table4f$`Share parent`[1]
  table4.nonparent <- table4f$`Share nonparent`[1]
  
  #### End #### 
  
  #### Table 4g: Beneficiaries by Veteran status ####
  
  table4g <- aggregate(
    data=data1, 
    `Count` ~ `Eligible for program` + `Veteran status`, 
    FUN=sum
  ) %>% pivot_wider(
    id_cols=c(`Eligible for program`), 
    names_from=`Veteran status`, 
    values_from=`Count`
  ) %>% mutate(
    `Veteran` = ifelse(is.na(`Veteran`), 0, `Veteran`),
    `Not a veteran` = ifelse(is.na(`Not a veteran`), 0, `Not a veteran`),
  ) %>% mutate(
    `Total` = `Veteran` + `Not a veteran` 
  ) %>% mutate(
    `Share Veteran` = `Veteran` / `Total`,
    `Share Not a veteran` = `Not a veteran` / `Total`,
  )
  
  table4.veteran <- table4g$`Share Veteran`[1]
  table4.nonveteran <- table4g$`Share Not a veteran`[1]
  
  #### End #### 
  
  #### Table 5: Dollars by state ####
  
  table5 <- aggregate(
    data=data1, 
    `Program amount` ~ `STABBR`, 
    FUN=sum
  ) %>% mutate(
    `Program amount` = ifelse(is.na(`Program amount`), 0, `Program amount`)
  ) %>% pivot_wider(
    names_from=`STABBR`, 
    values_from=`Program amount`
  ) 
  
  table5.AK <- table5$`AK`[1]
  table5.AL <- table5$`AL`[1]
  table5.AR <- table5$`AR`[1]
  table5.AS <- table5$`AS`[1]
  table5.AZ <- table5$`AZ`[1]
  table5.CA <- table5$`CA`[1]
  table5.CO <- table5$`CO`[1]
  table5.CT <- table5$`CT`[1]
  table5.DC <- table5$`DC`[1]
  table5.DE <- table5$`DE`[1]
  table5.FL <- table5$`FL`[1]
  table5.FM <- table5$`FM`[1]
  table5.GA <- table5$`GA`[1]
  table5.GU <- table5$`GU`[1]
  table5.HI <- table5$`HI`[1]
  table5.IA <- table5$`IA`[1]
  table5.ID <- table5$`ID`[1]
  table5.IL <- table5$`IL`[1]
  table5.IN <- table5$`IN`[1]
  table5.KS <- table5$`KS`[1]
  table5.KY <- table5$`KY`[1]
  table5.LA <- table5$`LA`[1]
  table5.MA <- table5$`MA`[1]
  table5.MD <- table5$`MD`[1]
  table5.ME <- table5$`ME`[1]
  table5.MH <- table5$`MH`[1]
  table5.MI <- table5$`MI`[1]
  table5.MN <- table5$`MN`[1]
  table5.MO <- table5$`MO`[1]
  table5.MP <- table5$`MP`[1]
  table5.MS <- table5$`MS`[1]
  table5.MT <- table5$`MT`[1]
  table5.NC <- table5$`NC`[1]
  table5.ND <- table5$`ND`[1]
  table5.NE <- table5$`NE`[1]
  table5.NH <- table5$`NH`[1]
  table5.NJ <- table5$`NJ`[1]
  table5.NM <- table5$`NM`[1]
  table5.NV <- table5$`NV`[1]
  table5.NY <- table5$`NY`[1]
  table5.OH <- table5$`OH`[1]
  table5.OK <- table5$`OK`[1]
  table5.OR <- table5$`OR`[1]
  table5.PA <- table5$`PA`[1]
  table5.PR <- table5$`PR`[1]
  table5.PW <- table5$`PW`[1]
  table5.RI <- table5$`RI`[1]
  table5.SC <- table5$`SC`[1]
  table5.SD <- table5$`SD`[1]
  table5.TN <- table5$`TN`[1]
  table5.TX <- table5$`TX`[1]
  table5.UT <- table5$`UT`[1]
  table5.VA <- table5$`VA`[1]
  table5.VI <- table5$`VI`[1]
  table5.VT <- table5$`VT`[1]
  table5.WA <- table5$`WA`[1]
  table5.WI <- table5$`WI`[1]
  table5.WV <- table5$`WV`[1]
  table5.WY <- table5$`WY`[1]
  
  #### End #### 
  
  #### Return simulation results #### 
  
  simuResults <- data.frame(
    `elig.carnegie` = c(elig.carnegie),
    `elig.control` = c(elig.control),
    `elig.halftime` = c(elig.halftime),
    `elig.efc` = c(elig.efc),
    `elig.outofstate` = c(elig.outofstate),
    `elig.noncitizen` = c(elig.noncitizen),
    `elig.fafsa` = c(elig.fafsa),
    `elig.gpa` = c(elig.gpa),
    `table1.recipients` = c(table1.recipients),
    `table1.totalamount` = c(table1.totalamount),
    `table1.share` = c(table1.share),
    `table2.netprice0` = c(table2.netprice0),
    `table2.netprice5000` = c(table2.netprice5000),
    `table2.netprice10000` = c(table2.netprice10000),
    `table2.netprice15000` = c(table2.netprice15000),
    `table2.netprice20000` = c(table2.netprice20000),
    `table2.netpriceceiling` = c(table2.netpriceceiling),
    `table3.totalloans0` = c(table3.totalloans0),
    `table3.totalloans5000` = c(table3.totalloans5000),
    `table3.totalloans10000` = c(table3.totalloans10000),
    `table3.totalloans15000` = c(table3.totalloans15000),
    `table3.totalloans20000` = c(table3.totalloans20000),
    `table3.totalloansceiling` = c(table3.totalloansceiling),
    `table4.aian` = c(table4.aian),
    `table4.asia` = c(table4.asia),
    `table4.bkaa` = c(table4.bkaa),
    `table4.hisp` = c(table4.hisp),
    `table4.2mor` = c(table4.2mor),
    `table4.nhpi` = c(table4.nhpi),
    `table4.unkn` = c(table4.unkn),
    `table4.nonr` = c(table4.nonr),
    `table4.whit` = c(table4.whit),
    `table4.male` = c(table4.male),
    `table4.female` = c(table4.female),
    `table4.firstgen` = c(table4.firstgen),
    `table4.notfirstgen` = c(table4.notfirstgen),
    `table4.dependent` = c(table4.dependent),
    `table4.independent` = c(table4.independent),
    `table4.zeroEFC` = c(table4.zeroEFC),
    `table4.nonzeroEFC` = c(table4.nonzeroEFC),
    `table4.parent` = c(table4.parent),
    `table4.nonparent` = c(table4.nonparent),
    `table4.veteran` = c(table4.veteran),
    `table4.nonveteran` = c(table4.nonveteran),
    `table5.AK` = c(table5.AK),
    `table5.AL` = c(table5.AL),
    `table5.AR` = c(table5.AR),
    `table5.AS` = c(table5.AS),
    `table5.AZ` = c(table5.AZ),
    `table5.CA` = c(table5.CA),
    `table5.CO` = c(table5.CO),
    `table5.CT` = c(table5.CT),
    `table5.DC` = c(table5.DC),
    `table5.DE` = c(table5.DE),
    `table5.FL` = c(table5.FL),
    `table5.FM` = c(table5.FM),
    `table5.GA` = c(table5.GA),
    `table5.GU` = c(table5.GU),
    `table5.HI` = c(table5.HI),
    `table5.IA` = c(table5.IA),
    `table5.ID` = c(table5.ID),
    `table5.IL` = c(table5.IL),
    `table5.IN` = c(table5.IN),
    `table5.KS` = c(table5.KS),
    `table5.KY` = c(table5.KY),
    `table5.LA` = c(table5.LA),
    `table5.MA` = c(table5.MA),
    `table5.MD` = c(table5.MD),
    `table5.ME` = c(table5.ME),
    `table5.MH` = c(table5.MH),
    `table5.MI` = c(table5.MI),
    `table5.MN` = c(table5.MN),
    `table5.MO` = c(table5.MO),
    `table5.MP` = c(table5.MP),
    `table5.MS` = c(table5.MS),
    `table5.MT` = c(table5.MT),
    `table5.NC` = c(table5.NC),
    `table5.ND` = c(table5.ND),
    `table5.NE` = c(table5.NE),
    `table5.NH` = c(table5.NH),
    `table5.NJ` = c(table5.NJ),
    `table5.NM` = c(table5.NM),
    `table5.NV` = c(table5.NV),
    `table5.NY` = c(table5.NY),
    `table5.OH` = c(table5.OH),
    `table5.OK` = c(table5.OK),
    `table5.OR` = c(table5.OR),
    `table5.PA` = c(table5.PA),
    `table5.PR` = c(table5.PR),
    `table5.PW` = c(table5.PW),
    `table5.RI` = c(table5.RI),
    `table5.SC` = c(table5.SC),
    `table5.SD` = c(table5.SD),
    `table5.TN` = c(table5.TN),
    `table5.TX` = c(table5.TX),
    `table5.UT` = c(table5.UT),
    `table5.VA` = c(table5.VA),
    `table5.VI` = c(table5.VI),
    `table5.VT` = c(table5.VT),
    `table5.WA` = c(table5.WA),
    `table5.WI` = c(table5.WI),
    `table5.WV` = c(table5.WV),
    `table5.WY` = c(table5.WY)
  )
  
  return(simuResults)
  rm(simuResults)
  
  #### End #### 
  
  #### Delete objects #### 
  
  rm(
    coefficient.amount,
    vector.carnegie,
    vector.control,
    vector.intensity,
    vector.efc,
    vector.oos,
    vector.cit,
    vector.fafsa,
    vector.gpa, 
    table1a,
    table1b,
    table2,
    table3,
    table4a,
    table4b,
    table4c,
    table4d,
    table4e,
    table4f,
    table4g, 
    elig.carnegie,
    elig.control,
    elig.halftime,
    elig.efc,
    elig.outofstate,
    elig.noncitizen,
    elig.fafsa,
    elig.gpa,
    program.amount,
    table1.recipients,
    table1.totalamount,
    table1.share,
    table2.netprice0,
    table2.netprice5000,
    table2.netprice10000,
    table2.netprice15000,
    table2.netprice20000,
    table2.netpriceceiling,
    table3.totalloans0,
    table3.totalloans5000,
    table3.totalloans10000,
    table3.totalloans15000,
    table3.totalloans20000,
    table3.totalloansceiling,
    table4.aian,
    table4.asia,
    table4.bkaa,
    table4.hisp,
    table4.2mor,
    table4.nhpi,
    table4.unkn,
    table4.nonr,
    table4.whit,
    table4.male,
    table4.female,
    table4.firstgen,
    table4.notfirstgen,
    table4.dependent,
    table4.independent,
    table4.zeroEFC,
    table4.nonzeroEFC,
    table4.parent,
    table4.nonparent,
    table4.veteran,
    table4.nonveteran,
    table5.AK,
    table5.AL,
    table5.AR,
    table5.AS,
    table5.AZ,
    table5.CA,
    table5.CO,
    table5.CT,
    table5.DC,
    table5.DE,
    table5.FL,
    table5.FM,
    table5.GA,
    table5.GU,
    table5.HI,
    table5.IA,
    table5.ID,
    table5.IL,
    table5.IN,
    table5.KS,
    table5.KY,
    table5.LA,
    table5.MA,
    table5.MD,
    table5.ME,
    table5.MH,
    table5.MI,
    table5.MN,
    table5.MO,
    table5.MP,
    table5.MS,
    table5.MT,
    table5.NC,
    table5.ND,
    table5.NE,
    table5.NH,
    table5.NJ,
    table5.NM,
    table5.NV,
    table5.NY,
    table5.OH,
    table5.OK,
    table5.OR,
    table5.PA,
    table5.PR,
    table5.PW,
    table5.RI,
    table5.SC,
    table5.SD,
    table5.TN,
    table5.TX,
    table5.UT,
    table5.VA,
    table5.VI,
    table5.VT,
    table5.WA,
    table5.WI,
    table5.WV,
    table5.WY
  )
  
  #### End #### 
  
}

################################################
#### Run results                            ####
################################################

#### Test simulation #### 

Sys.time()

test <- runSimulation(
  data1=studentDF, 
  elig.carnegie="Associate's colleges only", 
  elig.control="Public only", 
  elig.halftime="All enrollment intensity", 
  elig.efc="All", 
  elig.outofstate="In-state only", 
  elig.noncitizen="U.S. citizens or eligible nonciizens only", 
  elig.fafsa="FAFSA completers only", 
  elig.gpa="All",
  program.amount="$5,000"
)

Sys.time()

#### End #### 

#### Run simulation for all combinations ####


#### End #### 



