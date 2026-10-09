
#### Setup ####

library(scales)
library(tidyverse)

quiet_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

#### End #### 

###############################################
#### Scorecard data                        ####
###############################################

#### Load Scorecard data #### 

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/IPEDS data")

scorecard <- read.csv("Most-Recent-Cohorts-Institution-Apr2025.csv", header=TRUE) %>% select(
  INSTNM, 
  STABBR,
  UNITID,
  OPEID,
  OPEID6,
  CONTROL,
  PREDDEG,
  MAIN, 
  
  # Default statuses
  
  BBRR1_FED_UGCOMP_N,
  BBRR2_FED_UGCOMP_N,
  BBRR3_FED_UGCOMP_N,
  BBRR4_FED_UGCOMP_N,
  
  BBRR1_FED_UGNOCOMP_N,
  BBRR2_FED_UGNOCOMP_N,
  BBRR3_FED_UGNOCOMP_N,
  BBRR4_FED_UGNOCOMP_N,
  
  BBRR1_FED_UGCOMP_DFLT,
  BBRR2_FED_UGCOMP_DFLT,
  BBRR3_FED_UGCOMP_DFLT,
  BBRR4_FED_UGCOMP_DFLT,
  
  BBRR1_FED_UGNOCOMP_DFLT,
  BBRR2_FED_UGNOCOMP_DFLT,
  BBRR3_FED_UGNOCOMP_DFLT,
  BBRR4_FED_UGNOCOMP_DFLT,
  
  # Years since entry: 
  
  COUNT_WNE_P6,
  COUNT_WNE_P7,
  COUNT_WNE_P8,
  COUNT_WNE_P9,
  COUNT_WNE_P10,
  COUNT_WNE_P11,
  
  PCT25_EARN_WNE_P6,
  PCT25_EARN_WNE_P7,
  PCT25_EARN_WNE_P8,
  PCT25_EARN_WNE_P9,
  PCT25_EARN_WNE_P10,
  PCT25_EARN_WNE_P11,
  
  MD_EARN_WNE_P6,
  MD_EARN_WNE_P7,
  MD_EARN_WNE_P8,
  MD_EARN_WNE_P9,
  MD_EARN_WNE_P10,
  MD_EARN_WNE_P11,
  
  PCT75_EARN_WNE_P6,
  PCT75_EARN_WNE_P7,
  PCT75_EARN_WNE_P8,
  PCT75_EARN_WNE_P9,
  PCT75_EARN_WNE_P10,
  PCT75_EARN_WNE_P11
  
) %>% arrange(
  desc(`MAIN`)
) %>% filter(
  duplicated(`OPEID6`)==FALSE
)

#### End #### 

#### Format PS values ####

scorecard <- scorecard %>% mutate(
  
  `BBRR1_FED_UGCOMP_N` = ifelse(`BBRR1_FED_UGCOMP_N`=="PS", NA, `BBRR1_FED_UGCOMP_N`),
  `BBRR2_FED_UGCOMP_N` = ifelse(`BBRR2_FED_UGCOMP_N`=="PS", NA, `BBRR2_FED_UGCOMP_N`),
  `BBRR3_FED_UGCOMP_N` = ifelse(`BBRR3_FED_UGCOMP_N`=="PS", NA, `BBRR3_FED_UGCOMP_N`),
  `BBRR4_FED_UGCOMP_N` = ifelse(`BBRR4_FED_UGCOMP_N`=="PS", NA, `BBRR4_FED_UGCOMP_N`),
  
  `BBRR1_FED_UGNOCOMP_N` = ifelse(`BBRR1_FED_UGNOCOMP_N`=="PS", NA, `BBRR1_FED_UGNOCOMP_N`),
  `BBRR2_FED_UGNOCOMP_N` = ifelse(`BBRR2_FED_UGNOCOMP_N`=="PS", NA, `BBRR2_FED_UGNOCOMP_N`),
  `BBRR3_FED_UGNOCOMP_N` = ifelse(`BBRR3_FED_UGNOCOMP_N`=="PS", NA, `BBRR3_FED_UGNOCOMP_N`),
  `BBRR4_FED_UGNOCOMP_N` = ifelse(`BBRR4_FED_UGNOCOMP_N`=="PS", NA, `BBRR4_FED_UGNOCOMP_N`),
  
  `BBRR1_FED_UGCOMP_DFLT` = ifelse(`BBRR1_FED_UGCOMP_DFLT`=="PS", NA, `BBRR1_FED_UGCOMP_DFLT`),
  `BBRR2_FED_UGCOMP_DFLT` = ifelse(`BBRR2_FED_UGCOMP_DFLT`=="PS", NA, `BBRR2_FED_UGCOMP_DFLT`),
  `BBRR3_FED_UGCOMP_DFLT` = ifelse(`BBRR3_FED_UGCOMP_DFLT`=="PS", NA, `BBRR3_FED_UGCOMP_DFLT`),
  `BBRR4_FED_UGCOMP_DFLT` = ifelse(`BBRR4_FED_UGCOMP_DFLT`=="PS", NA, `BBRR4_FED_UGCOMP_DFLT`),
  
  `BBRR1_FED_UGNOCOMP_DFLT` = ifelse(`BBRR1_FED_UGNOCOMP_DFLT`=="PS", NA, `BBRR1_FED_UGNOCOMP_DFLT`),
  `BBRR2_FED_UGNOCOMP_DFLT` = ifelse(`BBRR2_FED_UGNOCOMP_DFLT`=="PS", NA, `BBRR2_FED_UGNOCOMP_DFLT`),
  `BBRR3_FED_UGNOCOMP_DFLT` = ifelse(`BBRR3_FED_UGNOCOMP_DFLT`=="PS", NA, `BBRR3_FED_UGNOCOMP_DFLT`),
  `BBRR4_FED_UGNOCOMP_DFLT` = ifelse(`BBRR4_FED_UGNOCOMP_DFLT`=="PS", NA, `BBRR4_FED_UGNOCOMP_DFLT`)
  
) %>% mutate(
  
  `BBRR1_FED_UGCOMP_N` = quiet_numeric(`BBRR1_FED_UGCOMP_N`),
  `BBRR2_FED_UGCOMP_N` = quiet_numeric(`BBRR2_FED_UGCOMP_N`),
  `BBRR3_FED_UGCOMP_N` = quiet_numeric(`BBRR3_FED_UGCOMP_N`),
  `BBRR4_FED_UGCOMP_N` = quiet_numeric(`BBRR4_FED_UGCOMP_N`),
  
  `BBRR1_FED_UGNOCOMP_N` = quiet_numeric(`BBRR1_FED_UGNOCOMP_N`),
  `BBRR2_FED_UGNOCOMP_N` = quiet_numeric(`BBRR2_FED_UGNOCOMP_N`),
  `BBRR3_FED_UGNOCOMP_N` = quiet_numeric(`BBRR3_FED_UGNOCOMP_N`),
  `BBRR4_FED_UGNOCOMP_N` = quiet_numeric(`BBRR4_FED_UGNOCOMP_N`),
  
  `COUNT_WNE_P6` = quiet_numeric(`COUNT_WNE_P6`),
  `COUNT_WNE_P7` = quiet_numeric(`COUNT_WNE_P7`),
  `COUNT_WNE_P8` = quiet_numeric(`COUNT_WNE_P8`),
  `COUNT_WNE_P9` = quiet_numeric(`COUNT_WNE_P9`),
  `COUNT_WNE_P10` = quiet_numeric(`COUNT_WNE_P10`),
  `COUNT_WNE_P11` = quiet_numeric(`COUNT_WNE_P11`)
  
)

#### End #### 

#### Record ranges ####

rangeLookup <- data.frame(
  `Character variable` = character(), 
  `Numeric variable` = numeric(), 
  check.names=FALSE
) 

rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0", `Numeric variable` = 0)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "<=0.01", `Numeric variable` = 0.005)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "<=0.02", `Numeric variable` = 0.01)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.01", `Numeric variable` = 0.01)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.02", `Numeric variable` = 0.02)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "<=0.05", `Numeric variable` = 0.025)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.03", `Numeric variable` = 0.03)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.03-0.04", `Numeric variable` = 0.035)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.04", `Numeric variable` = 0.04)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "<=0.10", `Numeric variable` = 0.05)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.05", `Numeric variable` = 0.05)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.05-0.06", `Numeric variable` = 0.055)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.06", `Numeric variable` = 0.06)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.05-0.09", `Numeric variable` = 0.07)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.07", `Numeric variable` = 0.07)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.07-0.08", `Numeric variable` = 0.075)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.08", `Numeric variable` = 0.08)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.09", `Numeric variable` = 0.09)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.09-0.10", `Numeric variable` = 0.095)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "<=0.20", `Numeric variable` = 0.1)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.1", `Numeric variable` = 0.1)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.11", `Numeric variable` = 0.11)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.11-0.12", `Numeric variable` = 0.115)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.12", `Numeric variable` = 0.12)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.10-0.14", `Numeric variable` = 0.12)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.13", `Numeric variable` = 0.13)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.13-0.14", `Numeric variable` = 0.135)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.14", `Numeric variable` = 0.14)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.10-0.19", `Numeric variable` = 0.145)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.15", `Numeric variable` = 0.15)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.15-0.16", `Numeric variable` = 0.155)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.16", `Numeric variable` = 0.16)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.15-0.19", `Numeric variable` = 0.17)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.17", `Numeric variable` = 0.17)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.17-0.18", `Numeric variable` = 0.175)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.18", `Numeric variable` = 0.18)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.19", `Numeric variable` = 0.19)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.19-0.20", `Numeric variable` = 0.195)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.2", `Numeric variable` = 0.2)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.21", `Numeric variable` = 0.21)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.21-0.22", `Numeric variable` = 0.215)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.20-0.24", `Numeric variable` = 0.22)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.22", `Numeric variable` = 0.22)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.23", `Numeric variable` = 0.23)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.23-0.24", `Numeric variable` = 0.235)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.24", `Numeric variable` = 0.24)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.20-0.29", `Numeric variable` = 0.245)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.25", `Numeric variable` = 0.25)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.25-0.26", `Numeric variable` = 0.255)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.26", `Numeric variable` = 0.26)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.25-0.29", `Numeric variable` = 0.27)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.27", `Numeric variable` = 0.27)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.27-0.28", `Numeric variable` = 0.275)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.28", `Numeric variable` = 0.28)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.29", `Numeric variable` = 0.29)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.20-0.39", `Numeric variable` = 0.295)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.29-0.30", `Numeric variable` = 0.295)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.3", `Numeric variable` = 0.3)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.31", `Numeric variable` = 0.31)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.31-0.32", `Numeric variable` = 0.315)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.30-0.34", `Numeric variable` = 0.32)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.32", `Numeric variable` = 0.32)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.33", `Numeric variable` = 0.33)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.33-0.34", `Numeric variable` = 0.335)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.30-0.39", `Numeric variable` = 0.345)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.35", `Numeric variable` = 0.35)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.35-0.36", `Numeric variable` = 0.355)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.36", `Numeric variable` = 0.36)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.35-0.39", `Numeric variable` = 0.37)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.37-0.38", `Numeric variable` = 0.375)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.39", `Numeric variable` = 0.39)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.39-0.40", `Numeric variable` = 0.395)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.41-0.42", `Numeric variable` = 0.415)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.40-0.44", `Numeric variable` = 0.42)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.43-0.44", `Numeric variable` = 0.435)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.40-0.49", `Numeric variable` = 0.445)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.45-0.46", `Numeric variable` = 0.455)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.45-0.49", `Numeric variable` = 0.47)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.47-0.48", `Numeric variable` = 0.475)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.40-0.59", `Numeric variable` = 0.495)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.49-0.50", `Numeric variable` = 0.495)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.50-0.54", `Numeric variable` = 0.52)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.50-0.59", `Numeric variable` = 0.545)
rangeLookup <- rangeLookup %>% add_row(`Character variable` = "0.60-0.79", `Numeric variable` = 0.695)

#### End #### 

#### Format ranges ####

formatRange <- function(data1, column1){
  
  data2 <- data1 %>% select(
    all_of(c(
      "OPEID6", 
      column1
    ))
  )
  names(data2)[2] <- "Character variable"
  
  data2 <- left_join(
    x=data2, 
    y=rangeLookup, 
    by="Character variable"
  ) %>% select(
    -(`Character variable`)
  )
  
  data1 <- data1 %>% select(
    -(all_of(c(column1))) # Remove the original character variable 
  )
  data1 <- left_join(x=data1, y=data2, by="OPEID6")
  names(data1)[ncol(data1)] <- column1
 
  return(data1)
  
}

scorecard <- formatRange(scorecard, "BBRR1_FED_UGCOMP_DFLT")
scorecard <- formatRange(scorecard, "BBRR2_FED_UGCOMP_DFLT")
scorecard <- formatRange(scorecard, "BBRR3_FED_UGCOMP_DFLT")
scorecard <- formatRange(scorecard, "BBRR4_FED_UGCOMP_DFLT")

scorecard <- formatRange(scorecard, "BBRR1_FED_UGNOCOMP_DFLT")
scorecard <- formatRange(scorecard, "BBRR2_FED_UGNOCOMP_DFLT")
scorecard <- formatRange(scorecard, "BBRR3_FED_UGNOCOMP_DFLT")
scorecard <- formatRange(scorecard, "BBRR4_FED_UGNOCOMP_DFLT")

rm(rangeLookup, formatRange)

#### End #### 

#### Impute sector means (CONTROL and PREDDEG) ####

imputeNAsFirstTry <- function(data1, valueCol, countCol){
  
  data2 <- data1 %>% select(
    all_of(c(
      "OPEID6", 
      "CONTROL", 
      "PREDDEG",
      valueCol, 
      countCol
    ))
  )
  names(data2)[4] <- "Value variable"
  names(data2)[5] <- "Count variable"
  
  data2 <- data2 %>% group_by(
    `CONTROL`, 
    `PREDDEG`
  ) %>% filter(
    is.na(`Value variable`)==FALSE, 
    is.na(`Count variable`)==FALSE
  ) %>% summarize(
    `Weighted mean` = weighted.mean(x=`Value variable`, w=`Count variable`), 
    .groups="drop"
  )
  names(data2)[3] <- "Sector average"
  
  data3 <- data1 %>% select(
    all_of(c(
      "OPEID6", 
      "CONTROL", 
      "PREDDEG",
      valueCol 
    ))
  )
  names(data3)[4] <- "Value variable"
  
  data3 <- left_join(x=data3, y=data2, by=c("CONTROL", "PREDDEG"))
  data3 <- data3 %>% mutate(
    `Combined variable` = ifelse(
      is.na(`Value variable`),  
      `Sector average`, 
      `Value variable`
    )
  ) %>% select(
    `OPEID6`, 
    `Combined variable`
  )
  names(data3)[2] <- valueCol
  
  data1 <- data1 %>% select(
    -(all_of(valueCol))
  ) 
  data1 <- left_join(x=data1, y=data3, by="OPEID6") 
  
  return(data1)
}

scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="BBRR1_FED_UGCOMP_DFLT", countCol="BBRR1_FED_UGCOMP_N")
scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="BBRR2_FED_UGCOMP_DFLT", countCol="BBRR2_FED_UGCOMP_N")
scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="BBRR3_FED_UGCOMP_DFLT", countCol="BBRR3_FED_UGCOMP_N")
scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="BBRR4_FED_UGCOMP_DFLT", countCol="BBRR4_FED_UGCOMP_N")

scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="BBRR1_FED_UGNOCOMP_DFLT", countCol="BBRR1_FED_UGNOCOMP_N")
scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="BBRR2_FED_UGNOCOMP_DFLT", countCol="BBRR2_FED_UGNOCOMP_N")
scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="BBRR3_FED_UGNOCOMP_DFLT", countCol="BBRR3_FED_UGNOCOMP_N")
scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="BBRR4_FED_UGNOCOMP_DFLT", countCol="BBRR4_FED_UGNOCOMP_N")

scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="PCT25_EARN_WNE_P6", countCol="COUNT_WNE_P6")
scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="PCT25_EARN_WNE_P7", countCol="COUNT_WNE_P7")
scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="PCT25_EARN_WNE_P8", countCol="COUNT_WNE_P8")
scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="PCT25_EARN_WNE_P9", countCol="COUNT_WNE_P9")
scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="PCT25_EARN_WNE_P10", countCol="COUNT_WNE_P10")
scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="PCT25_EARN_WNE_P11", countCol="COUNT_WNE_P11")

scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="MD_EARN_WNE_P6", countCol="COUNT_WNE_P6")
scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="MD_EARN_WNE_P7", countCol="COUNT_WNE_P7")
scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="MD_EARN_WNE_P8", countCol="COUNT_WNE_P8")
scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="MD_EARN_WNE_P9", countCol="COUNT_WNE_P9")
scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="MD_EARN_WNE_P10", countCol="COUNT_WNE_P10")
scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="MD_EARN_WNE_P11", countCol="COUNT_WNE_P11")

scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="PCT75_EARN_WNE_P6", countCol="COUNT_WNE_P6")
scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="PCT75_EARN_WNE_P7", countCol="COUNT_WNE_P7")
scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="PCT75_EARN_WNE_P8", countCol="COUNT_WNE_P8")
scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="PCT75_EARN_WNE_P9", countCol="COUNT_WNE_P9")
scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="PCT75_EARN_WNE_P10", countCol="COUNT_WNE_P10")
scorecard <- imputeNAsFirstTry(data1=scorecard, valueCol="PCT75_EARN_WNE_P11", countCol="COUNT_WNE_P11")

#### End #### 

#### Impute sector means (CONTROL only) ####

imputeNAsSecondTry <- function(data1, valueCol, countCol){
  
  data2 <- data1 %>% select(
    all_of(c(
      "OPEID6", 
      "CONTROL", 
      valueCol, 
      countCol
    ))
  )
  names(data2)[3] <- "Value variable"
  names(data2)[4] <- "Count variable"
  
  data2 <- data2 %>% group_by(
    `CONTROL`
  ) %>% filter(
    is.na(`Value variable`)==FALSE, 
    is.na(`Count variable`)==FALSE
  ) %>% summarize(
    `Weighted mean` = weighted.mean(x=`Value variable`, w=`Count variable`), 
    .groups="drop"
  )
  names(data2)[2] <- "Sector average"
  
  data3 <- data1 %>% select(
    all_of(c(
      "OPEID6", 
      "CONTROL", 
      valueCol 
    ))
  )
  names(data3)[3] <- "Value variable"
  
  data3 <- left_join(x=data3, y=data2, by="CONTROL")
  data3 <- data3 %>% mutate(
    `Combined variable` = ifelse(
      is.na(`Value variable`),  
      `Sector average`, 
      `Value variable`
    )
  ) %>% select(
    `OPEID6`, 
    `Combined variable`
  )
  names(data3)[2] <- valueCol
  
  data1 <- data1 %>% select(
    -(all_of(valueCol))
  ) 
  data1 <- left_join(x=data1, y=data3, by="OPEID6") 
  
  return(data1)
}

scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="BBRR1_FED_UGCOMP_DFLT", countCol="BBRR1_FED_UGCOMP_N")
scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="BBRR2_FED_UGCOMP_DFLT", countCol="BBRR2_FED_UGCOMP_N")
scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="BBRR3_FED_UGCOMP_DFLT", countCol="BBRR3_FED_UGCOMP_N")
scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="BBRR4_FED_UGCOMP_DFLT", countCol="BBRR4_FED_UGCOMP_N")

scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="BBRR1_FED_UGNOCOMP_DFLT", countCol="BBRR1_FED_UGNOCOMP_N")
scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="BBRR2_FED_UGNOCOMP_DFLT", countCol="BBRR2_FED_UGNOCOMP_N")
scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="BBRR3_FED_UGNOCOMP_DFLT", countCol="BBRR3_FED_UGNOCOMP_N")
scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="BBRR4_FED_UGNOCOMP_DFLT", countCol="BBRR4_FED_UGNOCOMP_N")

scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="PCT25_EARN_WNE_P6", countCol="COUNT_WNE_P6")
scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="PCT25_EARN_WNE_P7", countCol="COUNT_WNE_P7")
scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="PCT25_EARN_WNE_P8", countCol="COUNT_WNE_P8")
scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="PCT25_EARN_WNE_P9", countCol="COUNT_WNE_P9")
scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="PCT25_EARN_WNE_P10", countCol="COUNT_WNE_P10")
scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="PCT25_EARN_WNE_P11", countCol="COUNT_WNE_P11")

scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="MD_EARN_WNE_P6", countCol="COUNT_WNE_P6")
scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="MD_EARN_WNE_P7", countCol="COUNT_WNE_P7")
scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="MD_EARN_WNE_P8", countCol="COUNT_WNE_P8")
scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="MD_EARN_WNE_P9", countCol="COUNT_WNE_P9")
scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="MD_EARN_WNE_P10", countCol="COUNT_WNE_P10")
scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="MD_EARN_WNE_P11", countCol="COUNT_WNE_P11")

scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="PCT75_EARN_WNE_P6", countCol="COUNT_WNE_P6")
scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="PCT75_EARN_WNE_P7", countCol="COUNT_WNE_P7")
scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="PCT75_EARN_WNE_P8", countCol="COUNT_WNE_P8")
scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="PCT75_EARN_WNE_P9", countCol="COUNT_WNE_P9")
scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="PCT75_EARN_WNE_P10", countCol="COUNT_WNE_P10")
scorecard <- imputeNAsSecondTry(data1=scorecard, valueCol="PCT75_EARN_WNE_P11", countCol="COUNT_WNE_P11")


#### End #### 

#### Select for only the columns now needed ####

scorecard <- scorecard %>% select(

  OPEID6,
  
  # Default statuses
  
  BBRR1_FED_UGCOMP_DFLT,
  BBRR2_FED_UGCOMP_DFLT,
  BBRR3_FED_UGCOMP_DFLT,
  BBRR4_FED_UGCOMP_DFLT,
  
  BBRR1_FED_UGNOCOMP_DFLT,
  BBRR2_FED_UGNOCOMP_DFLT,
  BBRR3_FED_UGNOCOMP_DFLT,
  BBRR4_FED_UGNOCOMP_DFLT,
  
  # Years since entry: 
  
  PCT25_EARN_WNE_P6,
  PCT25_EARN_WNE_P7,
  PCT25_EARN_WNE_P8,
  PCT25_EARN_WNE_P9,
  PCT25_EARN_WNE_P10,
  PCT25_EARN_WNE_P11,
  
  MD_EARN_WNE_P6,
  MD_EARN_WNE_P7,
  MD_EARN_WNE_P8,
  MD_EARN_WNE_P9,
  MD_EARN_WNE_P10,
  MD_EARN_WNE_P11,
  
  PCT75_EARN_WNE_P6,
  PCT75_EARN_WNE_P7,
  PCT75_EARN_WNE_P8,
  PCT75_EARN_WNE_P9,
  PCT75_EARN_WNE_P10,
  PCT75_EARN_WNE_P11
  
)

#### End #### 

###############################################
#### BPS data                              ####
###############################################

#### Load files ####

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/IPEDS data")

loadBPS <- function(fileName, programLevel, selectedPercentile){
  
  overallDF <- read.csv(
    file=fileName,
    skip=10,
    header=FALSE, 
    nrows=1
  ) %>% rename(
    `Degree attained` = `V1`, 
    `10th percentile salary` = `V2`,
    `25th percentile salary` = `V3`, 
    `50th percentile salary` = `V4`,
    `75th percentile salary` = `V5`, 
    `90th percentile salary` = `V6`
  ) %>% mutate(
    `10th percentile salary` = quiet_numeric(`10th percentile salary`),
    `25th percentile salary` = quiet_numeric(`25th percentile salary`), 
    `50th percentile salary` = quiet_numeric(`50th percentile salary`),
    `75th percentile salary` = quiet_numeric(`75th percentile salary`), 
    `90th percentile salary` = quiet_numeric(`90th percentile salary`)
  ) %>% mutate(
    `Starting program level` = rep(programLevel)
  ) %>% mutate(
    `Completion status` = rep("All students")
  ) %>% select(
    -(`Degree attained`)
  )
  
  completionDF <- read.csv(
    file=fileName,
    skip=12,
    header=FALSE, 
    nrows=4
  ) %>% rename(
    `Degree attained` = `V1`, 
    `10th percentile salary` = `V2`,
    `25th percentile salary` = `V3`, 
    `50th percentile salary` = `V4`,
    `75th percentile salary` = `V5`, 
    `90th percentile salary` = `V6`
  ) %>% mutate(
    `10th percentile salary` = quiet_numeric(`10th percentile salary`),
    `25th percentile salary` = quiet_numeric(`25th percentile salary`), 
    `50th percentile salary` = quiet_numeric(`50th percentile salary`),
    `75th percentile salary` = quiet_numeric(`75th percentile salary`), 
    `90th percentile salary` = quiet_numeric(`90th percentile salary`)
  ) %>% mutate(
    `Starting program level` = rep(programLevel)
  ) %>% filter(
    (`Degree attained`==`Starting program level`) | (`Degree attained`=="Not attained a degree")
  ) %>% mutate(
    `Completion status` = ifelse(
      `Degree attained`=="Not attained a degree", 
      "Non-completer", 
      "Completer"
    )
  ) %>% select(
    -(`Degree attained`)
  )
  
  tempDF <- rbind(
    overallDF, 
    completionDF
  ) %>% select(
    all_of(c(
      "Starting program level",
      "Completion status",
      paste(selectedPercentile, " percentile salary", sep="")
    ))
  )
  names(tempDF)[3] <- "Salary"
  
  tempDF <- tempDF %>% pivot_wider(
    id_cols=c(`Starting program level`), 
    names_from=`Completion status`, 
    values_from=`Salary`
  ) %>% mutate(
    `Ratio: Completers to overall` = `Completer` / `All students`, 
    `Ratio: Non-completers to overall` = `Non-completer` / `All students`
  ) %>% select(
    `Starting program level`, 
    `Ratio: Completers to overall`, 
    `Ratio: Non-completers to overall`
  ) %>% mutate(
    `Percentile` = rep(selectedPercentile)
  )
  
  return(tempDF)
  
}

bps <- rbind(
  
  # 25th percentile
  loadBPS(
    fileName="PowerStats_pazjtp.csv", 
    programLevel="Certificate",
    selectedPercentile="25th"
  ), 
  loadBPS(
    fileName="PowerStats_qipszr.csv", 
    programLevel="Associate's degree",
    selectedPercentile="25th"
  ),
  loadBPS(
    fileName="PowerStats_xloamd.csv", 
    programLevel="Bachelor's degree",
    selectedPercentile="25th"
  ),
  
  # 50th percentile
  loadBPS(
    fileName="PowerStats_pazjtp.csv", 
    programLevel="Certificate",
    selectedPercentile="50th"
  ), 
  loadBPS(
    fileName="PowerStats_qipszr.csv", 
    programLevel="Associate's degree",
    selectedPercentile="50th"
  ),
  loadBPS(
    fileName="PowerStats_xloamd.csv", 
    programLevel="Bachelor's degree",
    selectedPercentile="50th"
  ), 
  
  # 75th percentile
  loadBPS(
    fileName="PowerStats_pazjtp.csv", 
    programLevel="Certificate",
    selectedPercentile="75th"
  ), 
  loadBPS(
    fileName="PowerStats_qipszr.csv", 
    programLevel="Associate's degree",
    selectedPercentile="75th"
  ),
  loadBPS(
    fileName="PowerStats_xloamd.csv", 
    programLevel="Bachelor's degree",
    selectedPercentile="75th"
  )
)

#### End #### 

#### Integrate BPS into Scorecard ####

integrateBPS <- function(scorecardData, bpsData, oldScorecardCol, bpsCol, newScorecardCol){
  
  # I'm going to use PREDDEG here but I'm not sure if it's what makes the most sense - worth revisiting.
  # Basically, do we merge data at the level of the student or the institution? 
  # I'm going with institution for now since it's easiest. 
  
  scorecardData <- scorecard
  bpsData <- bps
  oldScorecardCol <- "PCT25_EARN_WNE_P6"
  
  
  data1 <- scorecardData
  
}

#### End #### 




#### Test: File A ####

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Temp Folder/Weekend of September 20/Folder 11 Pre")

set.seed(1234)
tempData <- read.csv("A-11.csv", header=TRUE, check.names=FALSE) %>% select(
  -(`Participant`),      # Cohort is all participants 
  -(`Entering status`),  # Cohort is all entering
  -(`First-time status`) # Cohort is all first-time
) %>% arrange(
  `Policy index`, `UNITID`
)
sampleUnitID <- sample(tempData$UNITID, 25)
tempData <- tempData %>% filter(
  `UNITID` %in% sampleUnitID
)
write.csv(tempData[(1:20), ], "output-09-20-2026.csv", row.names=FALSE)

#### End #### 



#### Write files ####

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Temp Folder/Weekend of September 20/Folder 11 Pre")



#### End #### 
# 



