
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

scorecard <- read.csv("Most-Recent-Cohorts-Institution-Apr2025.csv", header=TRUE) %>% filter(
  is.na(`OPEID6`)==FALSE
) %>% select(
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
  PREDDEG,
  CONTROL,
  
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

#### Inflation adjustments to 2024 ####

# July to July adjustments 

coefficient2021 <- 1.152149
coefficient2022 <- 1.061645

# P6: 2022
# P7: 2021
# P8: 2022
# P9: 2021
# P10: 2022
# P11: 2021

scorecard <- scorecard %>% mutate(
  
  `PCT25_EARN_WNE_P6` = `PCT25_EARN_WNE_P6` * coefficient2022,
  `PCT25_EARN_WNE_P7` = `PCT25_EARN_WNE_P7` * coefficient2021,
  `PCT25_EARN_WNE_P8` = `PCT25_EARN_WNE_P8` * coefficient2022,
  `PCT25_EARN_WNE_P9` = `PCT25_EARN_WNE_P9` * coefficient2021,
  `PCT25_EARN_WNE_P10` = `PCT25_EARN_WNE_P10` * coefficient2022,
  `PCT25_EARN_WNE_P11` = `PCT25_EARN_WNE_P11` * coefficient2021,
  
  `MD_EARN_WNE_P6` = `MD_EARN_WNE_P6` * coefficient2022,
  `MD_EARN_WNE_P7` = `MD_EARN_WNE_P7` * coefficient2021,
  `MD_EARN_WNE_P8` = `MD_EARN_WNE_P8` * coefficient2022,
  `MD_EARN_WNE_P9` = `MD_EARN_WNE_P9` * coefficient2021,
  `MD_EARN_WNE_P10` = `MD_EARN_WNE_P10` * coefficient2022,
  `MD_EARN_WNE_P11` = `MD_EARN_WNE_P11` * coefficient2021,
  
  `PCT75_EARN_WNE_P6` = `PCT75_EARN_WNE_P6` * coefficient2022,
  `PCT75_EARN_WNE_P7` = `PCT75_EARN_WNE_P7` * coefficient2021,
  `PCT75_EARN_WNE_P8` = `PCT75_EARN_WNE_P8` * coefficient2022,
  `PCT75_EARN_WNE_P9` = `PCT75_EARN_WNE_P9` * coefficient2021,
  `PCT75_EARN_WNE_P10` = `PCT75_EARN_WNE_P10` * coefficient2022,
  `PCT75_EARN_WNE_P11` = `PCT75_EARN_WNE_P11` * coefficient2021
  
)

rm(
  coefficient2021,
  coefficient2022
)

#### End #### 

###############################################
#### BPS earnings data                     ####
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

importPreddeg <- data.frame(
  `PREDDEG` = numeric(),
  `Starting program level` = character(), 
  check.names=FALSE
)
importPreddeg <- importPreddeg %>% add_row(`PREDDEG`=0, `Starting program level`="Certificate")
importPreddeg <- importPreddeg %>% add_row(`PREDDEG`=1, `Starting program level`="Certificate")
importPreddeg <- importPreddeg %>% add_row(`PREDDEG`=2, `Starting program level`="Associate's degree")
importPreddeg <- importPreddeg %>% add_row(`PREDDEG`=3, `Starting program level`="Bachelor's degree")
importPreddeg <- importPreddeg %>% add_row(`PREDDEG`=4, `Starting program level`="Bachelor's degree")
# I don't think 0 or 4 will be present in the data, but I've listed them anyway. 

integrateBPS <- function(scorecardData, bpsData, oldScorecardCol, bpsCol, newScorecardCol){
  
  # I'm going to use PREDDEG here but I'm not sure if it's what makes the most sense - worth revisiting.
  # Basically, do we merge data at the level of the student or the institution? 
  # I'm going with institution for now since it's easiest. 
  
  data1 <- scorecardData %>% select(
    all_of(c(
      "OPEID6", 
      "PREDDEG",
      oldScorecardCol
    ))
  )
  names(data1)[3] <- "Earnings"
  data1 <- left_join(x=data1, y=importPreddeg, by="PREDDEG") %>% select(-(`PREDDEG`))
  
  if(grepl("PCT25", oldScorecardCol)){
    bpsData <- bpsData %>% filter(
      `Percentile` == "25th"
    )
  }
  if(grepl("MD", oldScorecardCol)){
    bpsData <- bpsData %>% filter(
      `Percentile` == "50th"
    )
  }
  if(grepl("PCT75", oldScorecardCol)){
    bpsData <- bpsData %>% filter(
      `Percentile` == "75th"
    )
  }
  
  bpsData <- bpsData %>% select(
    all_of(c(
      "Starting program level", 
      bpsCol
    ))
  )
  names(bpsData)[2] <- "Adjustment"
  
  data1 <- left_join(
    x=data1, 
    y=bpsData, 
    by="Starting program level"
  ) %>% mutate(
    `New earnings` = `Earnings` * `Adjustment`
  ) %>% select(
    `OPEID6`, 
    `New earnings`
  )
  names(data1)[2] <- newScorecardCol
  
  data2 <- left_join(x=scorecardData, y=data1, by="OPEID6")
  return(data2)
  
}

scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "PCT25_EARN_WNE_P6", bpsCol = "Ratio: Completers to overall", newScorecardCol = "PCT25_EARN_WNE_COMP_P6")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "PCT25_EARN_WNE_P7", bpsCol = "Ratio: Completers to overall", newScorecardCol = "PCT25_EARN_WNE_COMP_P7")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "PCT25_EARN_WNE_P8", bpsCol = "Ratio: Completers to overall", newScorecardCol = "PCT25_EARN_WNE_COMP_P8")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "PCT25_EARN_WNE_P9", bpsCol = "Ratio: Completers to overall", newScorecardCol = "PCT25_EARN_WNE_COMP_P9")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "PCT25_EARN_WNE_P10", bpsCol = "Ratio: Completers to overall", newScorecardCol = "PCT25_EARN_WNE_COMP_P10")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "PCT25_EARN_WNE_P11", bpsCol = "Ratio: Completers to overall", newScorecardCol = "PCT25_EARN_WNE_COMP_P11")

scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "MD_EARN_WNE_P6", bpsCol = "Ratio: Completers to overall", newScorecardCol = "MD_EARN_WNE_COMP_P6")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "MD_EARN_WNE_P7", bpsCol = "Ratio: Completers to overall", newScorecardCol = "MD_EARN_WNE_COMP_P7")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "MD_EARN_WNE_P8", bpsCol = "Ratio: Completers to overall", newScorecardCol = "MD_EARN_WNE_COMP_P8")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "MD_EARN_WNE_P9", bpsCol = "Ratio: Completers to overall", newScorecardCol = "MD_EARN_WNE_COMP_P9")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "MD_EARN_WNE_P10", bpsCol = "Ratio: Completers to overall", newScorecardCol = "MD_EARN_WNE_COMP_P10")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "MD_EARN_WNE_P11", bpsCol = "Ratio: Completers to overall", newScorecardCol = "MD_EARN_WNE_COMP_P11")

scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "PCT75_EARN_WNE_P6", bpsCol = "Ratio: Completers to overall", newScorecardCol = "PCT75_EARN_WNE_COMP_P6")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "PCT75_EARN_WNE_P7", bpsCol = "Ratio: Completers to overall", newScorecardCol = "PCT75_EARN_WNE_COMP_P7")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "PCT75_EARN_WNE_P8", bpsCol = "Ratio: Completers to overall", newScorecardCol = "PCT75_EARN_WNE_COMP_P8")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "PCT75_EARN_WNE_P9", bpsCol = "Ratio: Completers to overall", newScorecardCol = "PCT75_EARN_WNE_COMP_P9")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "PCT75_EARN_WNE_P10", bpsCol = "Ratio: Completers to overall", newScorecardCol = "PCT75_EARN_WNE_COMP_P10")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "PCT75_EARN_WNE_P11", bpsCol = "Ratio: Completers to overall", newScorecardCol = "PCT75_EARN_WNE_COMP_P11")

scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "PCT25_EARN_WNE_P6", bpsCol = "Ratio: Non-completers to overall", newScorecardCol = "PCT25_EARN_WNE_NOCOMP_P6")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "PCT25_EARN_WNE_P7", bpsCol = "Ratio: Non-completers to overall", newScorecardCol = "PCT25_EARN_WNE_NOCOMP_P7")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "PCT25_EARN_WNE_P8", bpsCol = "Ratio: Non-completers to overall", newScorecardCol = "PCT25_EARN_WNE_NOCOMP_P8")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "PCT25_EARN_WNE_P9", bpsCol = "Ratio: Non-completers to overall", newScorecardCol = "PCT25_EARN_WNE_NOCOMP_P9")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "PCT25_EARN_WNE_P10", bpsCol = "Ratio: Non-completers to overall", newScorecardCol = "PCT25_EARN_WNE_NOCOMP_P10")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "PCT25_EARN_WNE_P11", bpsCol = "Ratio: Non-completers to overall", newScorecardCol = "PCT25_EARN_WNE_NOCOMP_P11")

scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "MD_EARN_WNE_P6", bpsCol = "Ratio: Non-completers to overall", newScorecardCol = "MD_EARN_WNE_NOCOMP_P6")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "MD_EARN_WNE_P7", bpsCol = "Ratio: Non-completers to overall", newScorecardCol = "MD_EARN_WNE_NOCOMP_P7")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "MD_EARN_WNE_P8", bpsCol = "Ratio: Non-completers to overall", newScorecardCol = "MD_EARN_WNE_NOCOMP_P8")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "MD_EARN_WNE_P9", bpsCol = "Ratio: Non-completers to overall", newScorecardCol = "MD_EARN_WNE_NOCOMP_P9")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "MD_EARN_WNE_P10", bpsCol = "Ratio: Non-completers to overall", newScorecardCol = "MD_EARN_WNE_NOCOMP_P10")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "MD_EARN_WNE_P11", bpsCol = "Ratio: Non-completers to overall", newScorecardCol = "MD_EARN_WNE_NOCOMP_P11")

scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "PCT75_EARN_WNE_P6", bpsCol = "Ratio: Non-completers to overall", newScorecardCol = "PCT75_EARN_WNE_NOCOMP_P6")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "PCT75_EARN_WNE_P7", bpsCol = "Ratio: Non-completers to overall", newScorecardCol = "PCT75_EARN_WNE_NOCOMP_P7")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "PCT75_EARN_WNE_P8", bpsCol = "Ratio: Non-completers to overall", newScorecardCol = "PCT75_EARN_WNE_NOCOMP_P8")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "PCT75_EARN_WNE_P9", bpsCol = "Ratio: Non-completers to overall", newScorecardCol = "PCT75_EARN_WNE_NOCOMP_P9")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "PCT75_EARN_WNE_P10", bpsCol = "Ratio: Non-completers to overall", newScorecardCol = "PCT75_EARN_WNE_NOCOMP_P10")
scorecard <- integrateBPS(scorecardData = scorecard, bpsData = bps, oldScorecardCol = "PCT75_EARN_WNE_P11", bpsCol = "Ratio: Non-completers to overall", newScorecardCol = "PCT75_EARN_WNE_NOCOMP_P11")

rm(importPreddeg)

scorecard <- scorecard %>% select(
  
  -(`PCT25_EARN_WNE_P6`),
  -(`PCT25_EARN_WNE_P7`),
  -(`PCT25_EARN_WNE_P8`),
  -(`PCT25_EARN_WNE_P9`),
  -(`PCT25_EARN_WNE_P10`),
  -(`PCT25_EARN_WNE_P11`),
  
  -(`MD_EARN_WNE_P6`),
  -(`MD_EARN_WNE_P7`),
  -(`MD_EARN_WNE_P8`),
  -(`MD_EARN_WNE_P9`),
  -(`MD_EARN_WNE_P10`),
  -(`MD_EARN_WNE_P11`),
  
  -(`PCT75_EARN_WNE_P6`),
  -(`PCT75_EARN_WNE_P7`),
  -(`PCT75_EARN_WNE_P8`),
  -(`PCT75_EARN_WNE_P9`),
  -(`PCT75_EARN_WNE_P10`),
  -(`PCT75_EARN_WNE_P11`)
)

#### End #### 

#### Extrapolate P2, P3, P4, and P5 (median only) - COMP columns ####

tempDF1 <- scorecard %>% select(
  `OPEID6`,
  `MD_EARN_WNE_COMP_P6`,
  `MD_EARN_WNE_COMP_P7`,
  `MD_EARN_WNE_COMP_P8`,
  `MD_EARN_WNE_COMP_P9`,
  `MD_EARN_WNE_COMP_P10`,
  `MD_EARN_WNE_COMP_P11`,
) %>% rename(
  `6` = `MD_EARN_WNE_COMP_P6`,
  `7` = `MD_EARN_WNE_COMP_P7`,
  `8` = `MD_EARN_WNE_COMP_P8`,
  `9` = `MD_EARN_WNE_COMP_P9`,
  `10` = `MD_EARN_WNE_COMP_P10`,
  `11` = `MD_EARN_WNE_COMP_P11`,
) %>% pivot_longer(
  cols=c(
    `6`,
    `7`,
    `8`,
    `9`,
    `10`,
    `11`
  ),
  names_to="Year", 
  values_to="Earnings"
) %>% mutate(
  `Year` = as.integer(`Year`)
)

predictionsDF <- data.frame(
  `OPEID6` = character(),
  `Year` = integer(),
  `Earnings` = numeric()
)

counter <- 1
for(i in unique(tempDF1$`OPEID6`)){
  
  print(paste("Trying number ", counter, " of ", length(unique(tempDF1$`OPEID6`)), ".", sep=""))
  
  tempDF2 <- tempDF1 %>% filter(
    `OPEID6` == i
  )
  
  model1 <- lm(log(`Earnings`) ~ `Year`, data = tempDF2)
  
  tempPreds <- data.frame(
    `OPEID6` = i,
    `Year` = 2:5,
    `Earnings` = exp(predict(
      model1,
      newdata = data.frame(Year = 2:5)
    ))
  )
  
  predictionsDF <- rbind(
    predictionsDF,
    tempPreds
  )
  
  rm(tempPreds, model1, tempDF2)
  counter <- counter+1
}
rm(i, counter, tempDF1)

predictionsDF <- predictionsDF %>% pivot_wider(
  id_cols=c(`OPEID6`), 
  names_from=`Year`, 
  values_from=`Earnings`
) %>% rename(
  `MD_EARN_WNE_COMP_P2` = `2`,
  `MD_EARN_WNE_COMP_P3` = `3`,
  `MD_EARN_WNE_COMP_P4` = `4`,
  `MD_EARN_WNE_COMP_P5` = `5`
)

scorecard <- left_join(x=scorecard, y=predictionsDF, by="OPEID6")
rm(predictionsDF)

#### End #### 

#### Extrapolate P2, P3, P4, and P5 (median only) - NOCOMP columns ####

tempDF1 <- scorecard %>% select(
  `OPEID6`,
  `MD_EARN_WNE_NOCOMP_P6`,
  `MD_EARN_WNE_NOCOMP_P7`,
  `MD_EARN_WNE_NOCOMP_P8`,
  `MD_EARN_WNE_NOCOMP_P9`,
  `MD_EARN_WNE_NOCOMP_P10`,
  `MD_EARN_WNE_NOCOMP_P11`,
) %>% rename(
  `6` = `MD_EARN_WNE_NOCOMP_P6`,
  `7` = `MD_EARN_WNE_NOCOMP_P7`,
  `8` = `MD_EARN_WNE_NOCOMP_P8`,
  `9` = `MD_EARN_WNE_NOCOMP_P9`,
  `10` = `MD_EARN_WNE_NOCOMP_P10`,
  `11` = `MD_EARN_WNE_NOCOMP_P11`,
) %>% pivot_longer(
  cols=c(
    `6`,
    `7`,
    `8`,
    `9`,
    `10`,
    `11`
  ),
  names_to="Year", 
  values_to="Earnings"
) %>% mutate(
  `Year` = as.integer(`Year`)
)

predictionsDF <- data.frame(
  `OPEID6` = character(),
  `Year` = integer(),
  `Earnings` = numeric()
)

counter <- 1
for(i in unique(tempDF1$`OPEID6`)){
  
  print(paste("Trying number ", counter, " of ", length(unique(tempDF1$`OPEID6`)), ".", sep=""))
  
  tempDF2 <- tempDF1 %>% filter(
    `OPEID6` == i
  )
  
  model1 <- lm(log(`Earnings`) ~ `Year`, data = tempDF2)
  
  tempPreds <- data.frame(
    `OPEID6` = i,
    `Year` = 2:5,
    `Earnings` = exp(predict(
      model1,
      newdata = data.frame(Year = 2:5)
    ))
  )
  
  predictionsDF <- rbind(
    predictionsDF,
    tempPreds
  )
  
  rm(tempPreds, model1, tempDF2)
  counter <- counter+1
}
rm(i, counter, tempDF1)

predictionsDF <- predictionsDF %>% pivot_wider(
  id_cols=c(`OPEID6`), 
  names_from=`Year`, 
  values_from=`Earnings`
) %>% rename(
  `MD_EARN_WNE_NOCOMP_P2` = `2`,
  `MD_EARN_WNE_NOCOMP_P3` = `3`,
  `MD_EARN_WNE_NOCOMP_P4` = `4`,
  `MD_EARN_WNE_NOCOMP_P5` = `5`
)

scorecard <- left_join(x=scorecard, y=predictionsDF, by="OPEID6")
rm(predictionsDF)

#### End #### 

#### Calculate completion gain at each P value ####

scorecard <- scorecard %>% mutate(
  `Gain in earnings from completion: P2` = `MD_EARN_WNE_COMP_P2` - `MD_EARN_WNE_NOCOMP_P2`,
  `Gain in earnings from completion: P3` = `MD_EARN_WNE_COMP_P3` - `MD_EARN_WNE_NOCOMP_P3`,
  `Gain in earnings from completion: P4` = `MD_EARN_WNE_COMP_P4` - `MD_EARN_WNE_NOCOMP_P4`,
  `Gain in earnings from completion: P5` = `MD_EARN_WNE_COMP_P5` - `MD_EARN_WNE_NOCOMP_P5`,
  `Gain in earnings from completion: P6` = `MD_EARN_WNE_COMP_P6` - `MD_EARN_WNE_NOCOMP_P6`,
  `Gain in earnings from completion: P7` = `MD_EARN_WNE_COMP_P7` - `MD_EARN_WNE_NOCOMP_P7`,
  `Gain in earnings from completion: P8` = `MD_EARN_WNE_COMP_P8` - `MD_EARN_WNE_NOCOMP_P8`,
  `Gain in earnings from completion: P9` = `MD_EARN_WNE_COMP_P9` - `MD_EARN_WNE_NOCOMP_P9`,
  `Gain in earnings from completion: P10` = `MD_EARN_WNE_COMP_P10` - `MD_EARN_WNE_NOCOMP_P10`,
  `Gain in earnings from completion: P11` = `MD_EARN_WNE_COMP_P11` - `MD_EARN_WNE_NOCOMP_P11`
)

#### End #### 

#### Remove data no longer needed ####

rm(bps)

#### End #### 

###############################################
#### BPS default data                      ####
###############################################

#### Derive ratios from BPS to create BBRR[5][6][7] ####

# If you were in repayment by 2009, you had at least 7 years in repayment by 2016,
# which is when the relevant BPS data was recorded from NSLDS. 
# We can ignore the 8+ years group. 

# This is probably not a "perfect" method but I think it's good enough. 

# Ever entered default by 2016 
bps1 <- read.csv(
  "PowerStats_hazbxr.csv", 
  skip=12, 
  header=FALSE, 
  nrows = 3
) %>% select(
  -(`V4`)
) %>% mutate(
  `V2` = `V2` / 100, 
  `V3` = `V3` / 100
) %>% rename(
  `Control` = `V1`, 
  `Never defaulted` = `V2`, 
  `Defaulted` = `V3`
) 

# Years between entering repayment and first default 
bps2 <- read.csv(
  "PowerStats_projez.csv", 
  skip=12, 
  header=FALSE, 
  nrows = 3
) %>% select(
  -(`V10`)
) %>% mutate(
  `V2` = gsub(" !!", "", `V2`),
  `V3` = gsub(" !!", "", `V3`),
  `V4` = gsub(" !!", "", `V4`),
  `V5` = gsub(" !!", "", `V5`),
  `V6` = gsub(" !!", "", `V6`),
  `V7` = gsub(" !!", "", `V7`),
  `V8` = gsub(" !!", "", `V8`),
  `V9` = gsub(" !!", "", `V9`)
) %>% mutate(
  `V2` = as.numeric(gsub(" !", "", `V2`)) / 100,
  `V3` = as.numeric(gsub(" !", "", `V3`)) / 100,
  `V4` = as.numeric(gsub(" !", "", `V4`)) / 100,
  `V5` = as.numeric(gsub(" !", "", `V5`)) / 100,
  `V6` = as.numeric(gsub(" !", "", `V6`)) / 100,
  `V7` = as.numeric(gsub(" !", "", `V7`)) / 100,
  `V8` = as.numeric(gsub(" !", "", `V8`)) / 100,
  `V9` = as.numeric(gsub(" !", "", `V9`)) / 100
) %>% rename(
  `Control` = `V1`, 
  `Defaulted in year 1` = `V2`, 
  `Defaulted in year 2` = `V3`, 
  `Defaulted in year 3` = `V4`,
  `Defaulted in year 4` = `V5`,
  `Defaulted in year 5` = `V6`, 
  `Defaulted in year 6` = `V7`,
  `Defaulted in year 7` = `V8`, 
  `Defaulted in year 8` = `V9`
)

bps <- left_join(x=bps1, y=bps2, by="Control")
rm(bps1, bps2)

bps <- bps %>% mutate(
  `Defaulted in year 1` = `Defaulted` * `Defaulted in year 1`,
  `Defaulted in year 2` = `Defaulted` * `Defaulted in year 2`,
  `Defaulted in year 3` = `Defaulted` * `Defaulted in year 3`,
  `Defaulted in year 4` = `Defaulted` * `Defaulted in year 4`,
  `Defaulted in year 5` = `Defaulted` * `Defaulted in year 5`,
  `Defaulted in year 6` = `Defaulted` * `Defaulted in year 6`,
  `Defaulted in year 7` = `Defaulted` * `Defaulted in year 7`,
  `Defaulted in year 8` = `Defaulted` * `Defaulted in year 8`
) %>% select(
  -(`Defaulted`),
  -(`Never defaulted`)
) %>% mutate(
  `Cumulative default: year 1` = `Defaulted in year 1`, 
  `Cumulative default: year 2` = rowSums(pick(all_of(
    c(
      "Defaulted in year 1",
      "Defaulted in year 2"
    )))
  ), 
  `Cumulative default: year 3` = rowSums(pick(all_of(
    c(
      "Defaulted in year 1",
      "Defaulted in year 2",
      "Defaulted in year 3"
    )))
  ),
  `Cumulative default: year 4` = rowSums(pick(all_of(
    c(
      "Defaulted in year 1",
      "Defaulted in year 2",
      "Defaulted in year 3", 
      "Defaulted in year 4"
    )))
  ),
  `Cumulative default: year 5` = rowSums(pick(all_of(
    c(
      "Defaulted in year 1",
      "Defaulted in year 2",
      "Defaulted in year 3", 
      "Defaulted in year 4",
      "Defaulted in year 5"
    )))
  ),
  `Cumulative default: year 6` = rowSums(pick(all_of(
    c(
      "Defaulted in year 1",
      "Defaulted in year 2",
      "Defaulted in year 3", 
      "Defaulted in year 4",
      "Defaulted in year 5", 
      "Defaulted in year 6"
    )))
  ),
  `Cumulative default: year 7` = rowSums(pick(all_of(
    c(
      "Defaulted in year 1",
      "Defaulted in year 2",
      "Defaulted in year 3", 
      "Defaulted in year 4",
      "Defaulted in year 5", 
      "Defaulted in year 6", 
      "Defaulted in year 7"
    )))
  ),
  `Cumulative default: year 8` = rowSums(pick(all_of(
    c(
      "Defaulted in year 1",
      "Defaulted in year 2",
      "Defaulted in year 3", 
      "Defaulted in year 4",
      "Defaulted in year 5", 
      "Defaulted in year 6", 
      "Defaulted in year 7", 
      "Defaulted in year 8"
    )))
  ),
)

#### End #### 

#### Retrieve ratios for extrapolation ####

bps <- bps %>% mutate(
  `Ratio: Year 5 to Year 4` = `Cumulative default: year 5` / `Cumulative default: year 4`, 
  `Ratio: Year 6 to Year 4` = `Cumulative default: year 6` / `Cumulative default: year 4`, 
  `Ratio: Year 7 to Year 4` = `Cumulative default: year 7` / `Cumulative default: year 4`, 
  `Ratio: Year 8 to Year 4` = `Cumulative default: year 8` / `Cumulative default: year 4`
) %>% select(
  `Control`, 
  `Ratio: Year 5 to Year 4`, 
  `Ratio: Year 6 to Year 4`, 
  `Ratio: Year 7 to Year 4`, 
  `Ratio: Year 8 to Year 4`
)

importControl <- data.frame(
  `Control` = character(), 
  `CONTROL` = numeric()
)
importControl <- importControl %>% add_row(`Control`="Public", `CONTROL`=1)
importControl <- importControl %>% add_row(`Control`="Private not-for-profit", `CONTROL`=2)
importControl <- importControl %>% add_row(`Control`="Private for-profit", `CONTROL`=3)
bps <- left_join(x=bps, y=importControl, by="Control") %>% select(-(`Control`))

scorecard <- left_join(x=scorecard, y=bps, by="CONTROL")

#### End #### 

#### Extrapolate BBRR[5][6][7][8] ####

scorecard <- scorecard %>% mutate(
  
  `BBRR5_FED_UGCOMP_DFLT` = `BBRR4_FED_UGCOMP_DFLT` * `Ratio: Year 5 to Year 4`,
  `BBRR5_FED_UGNOCOMP_DFLT` = `BBRR4_FED_UGNOCOMP_DFLT` * `Ratio: Year 5 to Year 4`,
  
  `BBRR6_FED_UGCOMP_DFLT` = `BBRR4_FED_UGCOMP_DFLT` * `Ratio: Year 6 to Year 4`,
  `BBRR6_FED_UGNOCOMP_DFLT` = `BBRR4_FED_UGNOCOMP_DFLT` * `Ratio: Year 6 to Year 4`,
  
  `BBRR7_FED_UGCOMP_DFLT` = `BBRR4_FED_UGCOMP_DFLT` * `Ratio: Year 7 to Year 4`,
  `BBRR7_FED_UGNOCOMP_DFLT` = `BBRR4_FED_UGNOCOMP_DFLT` * `Ratio: Year 7 to Year 4`,
  
  `BBRR8_FED_UGCOMP_DFLT` = `BBRR4_FED_UGCOMP_DFLT` * `Ratio: Year 8 to Year 4`,
  `BBRR8_FED_UGNOCOMP_DFLT` = `BBRR4_FED_UGNOCOMP_DFLT` * `Ratio: Year 8 to Year 4`

) %>% select(
  -(`Ratio: Year 5 to Year 4`),
  -(`Ratio: Year 6 to Year 4`),
  -(`Ratio: Year 7 to Year 4`),
  -(`Ratio: Year 8 to Year 4`)
)

#### End #### 

###############################################
#### Run cohort files                      ####
###############################################

#### Load OPEIDs ####

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/IPEDS data")

importOPEIDs <- read.csv("Most-Recent-Cohorts-Institution-Apr2025.csv", header=TRUE) %>% filter(
  is.na(`OPEID6`)==FALSE
) %>% select(
  UNITID,
  OPEID6
)

#### End #### 

#### Test: File A-11 ####

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Temp Folder/Weekend of September 20/Folder 11 Pre")

set.seed(123)
fullData <- read.csv("A-11.csv", header=TRUE, check.names=FALSE) %>% select(
  -(`Participant`),      # Cohort is all participants 
  -(`Entering status`),  # Cohort is all entering
  -(`First-time status`) # Cohort is all first-time
) %>% arrange(
  `Policy index`, `UNITID`
) %>% mutate(
  `Gain: Certificates` = `New expected certificates` - `Expected certificates`, 
  `Gain: Associate's degrees` = `New expected associate's degrees` - `Expected associate's degrees`, 
  `Gain: Bachelor's degrees` = `New expected bachelor's degrees` - `Expected bachelor's degrees`, 
  `Decrease: Student loan borrowers` = `Pre-policy borrower status` - `Post-policy borrower status`
)
fullData <- left_join(x=fullData, y=importOPEIDs, by="UNITID")
fullData <- left_join(x=fullData, y=scorecard, by="OPEID6")

# REMOVE THIS AFTER TESTING 
fullData <- fullData %>% filter(
  `Policy index`==sample(fullData$`Policy index`, 1)
)
# END

FTdata <- fullData %>% filter(
  `Enrollment intensity`=="Full-time"
)
PTdata <- fullData %>% filter(
  `Enrollment intensity`=="Part-time"
)

#### End #### 

# I'm slightly worried about this showing extremely strong outcomes for plans that boost
# lower credentials at predominantly-bachelors-granting institutions. (Would any plans 
# actually do that, though?)

#### Assign average income gain by degree level: full-time ####

FTdata <- FTdata %>% mutate(
  `Average certificate income gain` = rowSums(pick(all_of(
    c(
      "Gain in earnings from completion: P2",
      "Gain in earnings from completion: P3",
      "Gain in earnings from completion: P4",
      "Gain in earnings from completion: P5",
      "Gain in earnings from completion: P6",
      "Gain in earnings from completion: P7",
      "Gain in earnings from completion: P8",
      "Gain in earnings from completion: P9",
      "Gain in earnings from completion: P10"
    )))
  ), 
  `Average associate's income gain` = rowSums(pick(all_of(
    c(
      "Gain in earnings from completion: P3",
      "Gain in earnings from completion: P4",
      "Gain in earnings from completion: P5",
      "Gain in earnings from completion: P6",
      "Gain in earnings from completion: P7",
      "Gain in earnings from completion: P8",
      "Gain in earnings from completion: P9",
      "Gain in earnings from completion: P10"
    )))
  ), 
  `Average bachelor's income gain` = rowSums(pick(all_of(
    c(
      "Gain in earnings from completion: P5",
      "Gain in earnings from completion: P6",
      "Gain in earnings from completion: P7",
      "Gain in earnings from completion: P8",
      "Gain in earnings from completion: P9",
      "Gain in earnings from completion: P10"
    )))
  ), 
) %>% mutate(
  `Average annual certificate income gain` = `Average certificate income gain` / 9, 
  `Average annual associate's income gain` = `Average associate's income gain` / 8, 
  `Average annual bachelor's income gain` = `Average bachelor's income gain` / 6,
)

#### End #### 

#### Calculate decrease in student defaults from completion rates: full-time ####

# Find student loan borrowers' share of cohort 
# Apply that share to each gained credential total 
# That's the number of student loan borrowers who switched from "noncompleter" to "completer" 

# Find the expected number of defaulters if they stayed noncompleters
# Find the expected number of defaulters now that they're completers 
# The difference is the improvement 

# FT Certs: BBRR8
# FT Assoc: BBRR7
# FT Bach:  BBRR5

#### End #### 

#### Assign average income gain by degree level: part-time ####

PTdata <- PTdata %>% mutate(
  `Average certificate income gain` = rowSums(pick(all_of(
    c(
      "Gain in earnings from completion: P3",
      "Gain in earnings from completion: P4",
      "Gain in earnings from completion: P5",
      "Gain in earnings from completion: P6",
      "Gain in earnings from completion: P7",
      "Gain in earnings from completion: P8",
      "Gain in earnings from completion: P9",
      "Gain in earnings from completion: P10"
    )))
  ), 
  `Average associate's income gain` = rowSums(pick(all_of(
    c(
      "Gain in earnings from completion: P5",
      "Gain in earnings from completion: P6",
      "Gain in earnings from completion: P7",
      "Gain in earnings from completion: P8",
      "Gain in earnings from completion: P9",
      "Gain in earnings from completion: P10"
    )))
  ), 
  `Average bachelor's income gain` = rowSums(pick(all_of(
    c(
      "Gain in earnings from completion: P9",
      "Gain in earnings from completion: P10"
    )))
  ), 
) %>% mutate(
  `Average annual certificate income gain` = `Average certificate income gain` / 8, 
  `Average annual associate's income gain` = `Average associate's income gain` / 6, 
  `Average annual bachelor's income gain` = `Average bachelor's income gain` / 2,
)

#### End #### 

#### Calculate decrease in student defaults from completion rates: part-time ####


# PT Certs: BBRR7
# PT Assoc: BBRR5
# PT Bach:  BBRR1

#### End #### 

#### Combine files again ####

fullData <- rbind(
  FTdata, 
  PTdata
) %>% arrange(
  `Policy index`, `UNITID`
) 

#### End #### 



#### Total gains in earnings ####

agg1 <- fullData %>% group_by(
  `OPEID6`, 
  `Policy index`
)

#### End #### 

#### Write files ####

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Temp Folder/Weekend of September 20/Folder 11 Post")



#### End #### 
# 



