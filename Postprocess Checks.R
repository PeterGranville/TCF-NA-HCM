
#### Setup ####

library(readxl)
library(scales)
library(tidyverse)

quiet_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

#### End ####

################################################
#### Load datasets                          ####
################################################

#### Create list of j values ####

jValues <- c(
  "00",	"01",	"02",	"03",	"04",	"05",	"06",	"07",	"08",	"09",
  "10",	"11",	"12",	"13",	"14",	"15",	"16",	"17",	"18",	"19",
  "20",	"21",	"22",	"23",	"24",	"25",	"26",	"27",	"28",	"29",
  "30",	"31",	"32",	"33",	"34",	"35",	"36",	"37",	"38",	"39",
  "40",	"41",	"42",	"43",	"44",	"45",	"46",	"47",	"48",	"49",
  "50",	"51",	"52",	"53",	"54",	"55",	"56",	"57",	"58",	"59",
  "60",	"61",	"62",	"63",	"64",	"65",	"66",	"67",	"68",	"69",
  "70",	"71",	"72",	"73",	"74",	"75",	"76",	"77",	"78",	"79",
  "80",	"81",	"82",	"83",	"84",	"85",	"86",	"87",	"88",	"89",
  "90",	"91",	"92",	"93",	"94",	"95",	"96",	"97",	"98",	"99"
)

#### End #### 

# #### Load procesed output (LONG WAY: 30+ MIN) ####
# 
# setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Postprocessing data")
# 
# for(j in jValues){
#   
#   print(Sys.time())
#   
#   print(paste("Merging dataset ", j, ".", sep=""))
#   
#   if(j=="00"){
#     studentDF <- read.csv(
#       paste("Set-", j, ".csv", sep=""),
#       header=TRUE,
#       check.names=FALSE
#     ) %>% mutate(
#       `Source file number` = rep(j)
#     )
#   }else{
#     tempDF <- read.csv(
#       paste("Set-", j, ".csv", sep=""),
#       header=TRUE,
#       check.names=FALSE
#     ) %>% mutate(
#       `Source file number` = rep(j)
#     )
#     studentDF <- rbind(
#       studentDF,
#       tempDF
#     )
#     rm(tempDF)
#   }
#   
# }
# rm(j)
# rm(jValues)
# 
# setwd("/Volumes/TOSHIBA EXT/Fed State Modeling")
# 
# write.csv(studentDF, "All merged student data.csv", row.names=FALSE)
# 
# #### End ####

#### Load processed output (SHORT WAY: 6 MIN) ####

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling")

Sys.time()
studentDF <- read.csv(
  "All merged student data.csv",
  header=TRUE,
  check.names=FALSE
)
Sys.time()

#### End ####

################################################
#### Make adjustments and additions         ####
#### after processing                       ####
################################################

#### Trim extreme values ####

studentDF <- studentDF %>% mutate(
  `Tuition and fees paid` = pmin(`Tuition and fees paid`, 75000)
)

#### End #### 

#### Assign family income values ####

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/IPEDS data")

depTiles <- read.csv("PowerStats_rgtxrw.csv", skip=82, nrow=8, header=FALSE)
indTiles <- read.csv("PowerStats_rgtxrw.csv", skip=151, nrow=8, header=FALSE)

depTiles <- depTiles %>% mutate(`Dependency status` = rep("Dependent"))
indTiles <- indTiles %>% mutate(`Dependency status` = rep("Independent"))

famTiles <- rbind(
  depTiles, 
  indTiles
)
rm(depTiles, indTiles)

famTiles <- famTiles %>% rename(
  `EFC group` = `V1`, 
  `5th percentile` = `V2`,
  `15th percentile` = `V3`,
  `25th percentile` = `V4`, 
  `35th percentile` = `V5`, 
  `45th percentile` = `V6`, 
  `55th percentile` = `V7`, 
  `65th percentile` = `V8`, 
  `75th percentile` = `V9`, 
  `85th percentile` = `V10`, 
  `95th percentile` = `V11`
) %>% mutate(
  `5th percentile` = quiet_numeric(`5th percentile`), 
  `15th percentile` = quiet_numeric(`15th percentile`)
) %>% mutate(
  `5th percentile` = ifelse(
    is.na(`5th percentile`), 
    0, 
    `5th percentile`
  ), 
  `15th percentile` = ifelse(
    is.na(`15th percentile`), 
    0, 
    `15th percentile`
  )
) %>% mutate(
  `10th percentile` = (`5th percentile` + `15th percentile`) / 2, 
  `20th percentile` = (`15th percentile` + `25th percentile`) / 2, 
  `30th percentile` = (`25th percentile` + `35th percentile`) / 2, 
  `40th percentile` = (`35th percentile` + `45th percentile`) / 2, 
  `50th percentile` = (`45th percentile` + `55th percentile`) / 2, 
  `60th percentile` = (`55th percentile` + `65th percentile`) / 2, 
  `70th percentile` = (`65th percentile` + `75th percentile`) / 2, 
  `80th percentile` = (`75th percentile` + `85th percentile`) / 2, 
  `90th percentile` = (`85th percentile` + `95th percentile`) / 2
) %>% pivot_longer(
  cols=c(
    `5th percentile`,
    `15th percentile`,
    `25th percentile`, 
    `35th percentile`, 
    `45th percentile`, 
    `55th percentile`, 
    `65th percentile`, 
    `75th percentile`, 
    `85th percentile`, 
    `95th percentile`, 
    `10th percentile`, 
    `20th percentile`, 
    `30th percentile`, 
    `40th percentile`, 
    `50th percentile`, 
    `60th percentile`, 
    `70th percentile`, 
    `80th percentile`, 
    `90th percentile`
  ), 
  names_to="Percentile", 
  values_to="Family income"
)

studentDF <- studentDF %>% mutate(
  `EFC group` = ifelse(`EFC`==0, "0 <= X <= 0", 
    ifelse(between(`EFC`, 1, 500), "1 <= X <= 500", 
      ifelse(between(`EFC`, 501, 2000), "501 <= X <= 2000", 
        ifelse(between(`EFC`, 2001, 5000), "2001 <= X <= 5000", 
          ifelse(between(`EFC`, 5001, 10000), "5001 <= X <= 10000", 
            ifelse(between(`EFC`, 10001, 20000), "10001 <= X <= 20000", 
              ifelse(between(`EFC`, 20001, 50000), "20001 <= X <= 50000", 
                ifelse(`EFC` > 50001, "50001 <= X <= 1000000", 
                  "No group"
                )
              )
            )
          )
        )
      )
    )
  )
)

set.seed(10281993)
studentDF <- studentDF %>% mutate(
  `Percentile` = sample(
    c(
      "5th percentile",
      "15th percentile",
      "25th percentile", 
      "35th percentile", 
      "45th percentile", 
      "55th percentile", 
      "65th percentile", 
      "75th percentile", 
      "85th percentile", 
      "95th percentile", 
      "10th percentile", 
      "20th percentile", 
      "30th percentile", 
      "40th percentile", 
      "50th percentile", 
      "60th percentile", 
      "70th percentile", 
      "80th percentile", 
      "90th percentile"
    ), 
    size=nrow(studentDF),
    replace=TRUE
  )
)

studentDF <- left_join(
  x=studentDF,
  y=famTiles, 
  by=c("Dependency status", "EFC group", "Percentile")
)
rm(famTiles)

studentDF <- studentDF %>% select(
  -(`EFC group`), 
  -(`Percentile`)
)

#### End #### 

#### Assign Pell recipient status ####

# 98.8% of federal grant recipients receive Pell (Datalab table retrieval code rxkadc)
# I selected the EFC value this so that 98.8% of federal grant recipients in studentDF are assumed to receive Pell 

studentDF <- studentDF %>% mutate(
  `Receives Pell` = ifelse(
    (`Receives federal grants`=="Yes") & (`EFC` < 82800), 
    "Yes", 
    "No"
  )
)

# studentDF <- studentDF %>% mutate(`Count` = rep(1))
# aggregate(
#   data=studentDF, 
#   `Count` ~ `Receives federal grants` + `Receives Pell`, 
#   FUN=sum
# ) %>% pivot_wider(
#   id_cols=c(`Receives federal grants`), 
#   names_from=`Receives Pell`,
#   values_from=`Count`
# ) %>% mutate(
#   `Share receiving Pell` = `Yes` / (`Yes` + `No`)
# )
# studentDF <- studentDF %>% select(-(`Count`))

#### End #### 

#### Apply inflation adjustments ####

studentDF <- studentDF %>% mutate(

  # Adjust by inflation: January 2020 to January 2024 
  `Non-tuition expense budget` = `Non-tuition expense budget` * 1.1955491, 
  `Private grant amount` = `Private grant amount` * 1.1955491, 
  `Family income` = `Family income` * 1.1955491
  # Only these columns, since the rest were already adjusted to 2023-24 (see ReadMe) 
  
) 

#### End #### 

#### Calibrate total state grants and total private grants ####

studentDF <- studentDF %>% mutate(
  `State grant amount` = ifelse(
    `Enrollment intensity`=="Full-time", 
    `State grant amount` * 0.75, 
    `State grant amount` * 0.1
  )
) # See ReadMe. 

studentDF <- studentDF %>% mutate(
  `Private grant amount` = `Private grant amount` * 0.49304
) # See ReadMe. 

#### End #### 

# #### Assign expected degree/certificate values (ATTEMPT ONE) ####
# 
# setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/IPEDS data")
# 
# c2024 <- read.csv("c2024_a.csv", header=TRUE) %>% filter(
#   `MAJORNUM`==1, 
#   `CIPCODE`==99, 
#   (`AWLEVEL` %in% c(7, 8, 17, 18, 19))==FALSE
# ) %>% select(
#   `UNITID`,
#   `AWLEVEL`, 
#   `CTOTALT`
# ) %>% mutate(
#   `Year` = rep("2024")
# )
# 
# c2023 <- read.csv("c2023_a_RV.csv", header=TRUE) %>% filter(
#   `MAJORNUM`==1, 
#   `CIPCODE`==99, 
#   (`AWLEVEL` %in% c(7, 8, 17, 18, 19))==FALSE
# ) %>% select(
#   `UNITID`,
#   `AWLEVEL`, 
#   `CTOTALT`
# ) %>% mutate(
#   `Year` = rep("2023")
# )
# 
# c2022 <- read.csv("c2022_a_rv.csv", header=TRUE) %>% filter(
#   `MAJORNUM`==1, 
#   `CIPCODE`==99, 
#   (`AWLEVEL` %in% c(7, 8, 17, 18, 19))==FALSE
# ) %>% select(
#   `UNITID`,
#   `AWLEVEL`, 
#   `CTOTALT`
# ) %>% mutate(
#   `Year` = rep("2022")
# )
# 
# cAll <- rbind(
#   c2024, 
#   c2023, 
#   c2022
# )
# rm(c2024, c2023, c2022)
# 
# degreeMap <- data.frame(
#   `AWLEVEL` = numeric(), 
#   `Award` = character()
# )
# degreeMap <- degreeMap %>% add_row(`AWLEVEL` = 3, `Award` = "Associate's degree")
# degreeMap <- degreeMap %>% add_row(`AWLEVEL` = 5, `Award` = "Bachelor's degree")
# degreeMap <- degreeMap %>% add_row(`AWLEVEL` = 20, `Award` = "Certificate")
# degreeMap <- degreeMap %>% add_row(`AWLEVEL` = 21, `Award` = "Certificate")
# degreeMap <- degreeMap %>% add_row(`AWLEVEL` = 2, `Award` = "Certificate")
# degreeMap <- degreeMap %>% add_row(`AWLEVEL` = 4, `Award` = "Certificate")
# degreeMap <- degreeMap %>% add_row(`AWLEVEL` = 6, `Award` = "Certificate")
# 
# cAll <- left_join(x=cAll, y=degreeMap, by="AWLEVEL")
# rm(degreeMap)
# 
# cAll <- aggregate(
#   data=cAll, 
#   `CTOTALT` ~ `UNITID` + `Award` + `Year`, 
#   FUN=sum # Total by year 
# )
# cAll <- aggregate(
#   data=cAll, 
#   `CTOTALT` ~ `UNITID` + `Award`,
#   FUN=mean # Average by year 
# ) %>% pivot_wider(
#   id_cols=c(`UNITID`), 
#   names_from=`Award`, 
#   values_from=`CTOTALT`
# ) %>% mutate(
#   `Associate's degree` = ifelse(
#     is.na(`Associate's degree`), 
#     0,
#     `Associate's degree`
#   ), 
#   `Bachelor's degree` = ifelse(
#     is.na(`Bachelor's degree`), 
#     0,
#     `Bachelor's degree`
#   ), 
#   `Certificate` = ifelse(
#     is.na(`Certificate`), 
#     0,
#     `Certificate`
#   )
# )
# 
# effy2024 <- read.csv("effy2024.csv", header=TRUE) %>% filter(
#   `EFFYALEV`==3  #	All students, Undergraduate, Degree/certificate-seeking total
# ) %>% select(
#   `UNITID`, 
#   `EFYTOTLT`
# ) %>% mutate(
#   `Year` = rep("2024")
# )
# effy2023 <- read.csv("effy2023_RV.csv", header=TRUE) %>% filter(
#   `EFFYALEV`==3  #	All students, Undergraduate, Degree/certificate-seeking total
# ) %>% select(
#   `UNITID`, 
#   `EFYTOTLT`
# ) %>% mutate(
#   `Year` = rep("2023")
# )
# effy2022 <- read.csv("effy2022_rv.csv", header=TRUE) %>% filter(
#   `EFFYALEV`==3  #	All students, Undergraduate, Degree/certificate-seeking total
# ) %>% select(
#   `UNITID`, 
#   `EFYTOTLT`
# ) %>% mutate(
#   `Year` = rep("2022")
# )
# 
# effyAll <- rbind(
#   effy2024, 
#   effy2023, 
#   effy2022
# )
# rm(effy2024, effy2023, effy2022)
# 
# effyAll <- aggregate(
#   data=effyAll, 
#   `EFYTOTLT` ~ `UNITID` + `Year`, 
#   FUN=sum
# )
# effyAll <- aggregate(
#   data=effyAll, 
#   `EFYTOTLT` ~ `UNITID`, 
#   FUN=mean
# ) 
# 
# cAll <- left_join(x=cAll, y=effyAll, by="UNITID")
# rm(effyAll)
# 
# cAll <- cAll %>% filter(
#   `Associate's degree` + `Bachelor's degree` + `Certificate` > 0, 
#   is.na(`EFYTOTLT`)==FALSE,
#   `EFYTOTLT` > 0
# ) %>% mutate(
#   `Expected associate's degrees` = pmin(`Associate's degree` / `EFYTOTLT`, 1), 
#   `Expected bachelor's degrees` = pmin(`Bachelor's degree` / `EFYTOTLT`, 1), 
#   `Expected certificates` = pmin(`Certificate` / `EFYTOTLT`, 1)
# ) %>% select(
#   `UNITID`, 
#   `Expected associate's degrees`,
#   `Expected bachelor's degrees`, 
#   `Expected certificates`
# )
# 
# studentDF <- left_join(x=studentDF, y=cAll, by="UNITID")
# rm(cAll)
#   
# fillNAs <- aggregate(
#   data=studentDF, 
#   cbind(`Expected associate's degrees`, `Expected bachelor's degrees`, `Expected certificates`) ~ `Carnegie NPSAS`, 
#   FUN=mean
# ) %>% rename(
#   `Imputed associate's degrees` = `Expected associate's degrees`, 
#   `Imputed bachelor's degrees` = `Expected bachelor's degrees`, 
#   `Imputed certificates` = `Expected certificates`
# )
# 
# studentDF <- left_join(x=studentDF, y=fillNAs, by="Carnegie NPSAS") %>% mutate(
#   
# ) %>% select(
#   -(`Imputed associate's degrees`), 
#   -(`Imputed bachelor's degrees`),
#   -(`Imputed certificates`)
# )
# rm(fillNAs)
# 
# # What to do about NAs in the resulting file 
# 
# #### End #### 

#### Assign expected degree/certificate values (ATTEMPT TWO) ####

om23 <- read.csv("om2023_RV.csv", header=TRUE) %>% select(
  `UNITID`,  
  `OMCHRT`,  
  `OMACHRT`, 
  `OMCERT8`, 
  `OMASSC8`, 
  `OMBACH8`	
) %>% mutate(
  `Year` = rep(2023)
)
om22 <- read.csv("om2022_rv.csv", header=TRUE) %>% select(
  `UNITID`,  
  `OMCHRT`,  
  `OMACHRT`, 
  `OMCERT8`, 
  `OMASSC8`, 
  `OMBACH8`	
) %>% mutate(
  `Year` = rep(2022)
)
om21 <- read.csv("om2021_rv.csv", header=TRUE) %>% select(
  `UNITID`,  
  `OMCHRT`,  
  `OMACHRT`, 
  `OMCERT8`, 
  `OMASSC8`, 
  `OMBACH8`	
) %>% mutate(
  `Year` = rep(2021)
)

omAll <- rbind(
  om23, 
  om22, 
  om21
)
rm(om23, om22, om21)

omMap <- data.frame(
  `OMCHRT` = numeric(), 
  `OM Category` = character(), 
  check.names=FALSE
) 
omMap <- omMap %>% add_row(`OMCHRT` = 10, `OM Category` = "Full-time overall")
omMap <- omMap %>% add_row(`OMCHRT` = 11, `OM Category` = "Full-time Pell")
omMap <- omMap %>% add_row(`OMCHRT` = 12, `OM Category` = "Full-time non-Pell")
omMap <- omMap %>% add_row(`OMCHRT` = 20, `OM Category` = "Part-time overall")
omMap <- omMap %>% add_row(`OMCHRT` = 21, `OM Category` = "Part-time Pell")
omMap <- omMap %>% add_row(`OMCHRT` = 22, `OM Category` = "Part-time non-Pell")
omMap <- omMap %>% add_row(`OMCHRT` = 30, `OM Category` = "Full-time overall")
omMap <- omMap %>% add_row(`OMCHRT` = 31, `OM Category` = "Full-time Pell")
omMap <- omMap %>% add_row(`OMCHRT` = 32, `OM Category` = "Full-time non-Pell")
omMap <- omMap %>% add_row(`OMCHRT` = 40, `OM Category` = "Part-time overall")
omMap <- omMap %>% add_row(`OMCHRT` = 41, `OM Category` = "Part-time Pell")
omMap <- omMap %>% add_row(`OMCHRT` = 42, `OM Category` = "Part-time non-Pell")
omMap <- omMap %>% add_row(`OMCHRT` = 50, `OM Category` = "All students overall")
omMap <- omMap %>% add_row(`OMCHRT` = 51, `OM Category` = "Pell overall")
omMap <- omMap %>% add_row(`OMCHRT` = 52, `OM Category` = "Non-Pell overall")
omAll <- left_join(x=omAll, y=omMap, by="OMCHRT")
rm(omMap)

omAll <- omAll %>% mutate(
  `OMBACH8` = ifelse(
    is.na(`OMBACH8`), 
    0, 
    `OMBACH8`
  )
)

omAll <- aggregate(
  data=omAll, 
  cbind(`OMACHRT`, `OMCERT8`, `OMASSC8`, `OMBACH8`) ~ `UNITID` + `OM Category`, 
  FUN=sum
)
omAll <- omAll %>% filter(
  `OMACHRT` >= 50
) %>% mutate(
  `Expected certificates` = `OMCERT8` / `OMACHRT`, 
  `Expected associate's degrees` = `OMASSC8` / `OMACHRT`,
  `Expected bachelor's degrees` = `OMBACH8` / `OMACHRT`
) 

omCert <- omAll %>% select(
  `UNITID`, 
  `OM Category`, 
  `Expected certificates`
) %>% pivot_wider(
  id_cols=c(`UNITID`), 
  names_from=`OM Category`, 
  values_from=`Expected certificates`
) %>% mutate( # First we prioritize full-time status
  `Full-time Pell` = ifelse(
    is.na(`Full-time Pell`), 
    `Full-time overall`, 
    `Full-time Pell`
  ), 
  `Part-time Pell` = ifelse(
    is.na(`Part-time Pell`), 
    `Part-time overall`, 
    `Part-time Pell`
  ), 
  `Full-time non-Pell` = ifelse(
    is.na(`Full-time non-Pell`), 
    `Full-time overall`, 
    `Full-time non-Pell`
  ), 
  `Part-time non-Pell` = ifelse(
    is.na(`Part-time non-Pell`), 
    `Part-time overall`, 
    `Part-time non-Pell`
  )
) %>% mutate( # Next we prioritize Pell status
  `Full-time Pell` = ifelse(
    is.na(`Full-time Pell`), 
    `Pell overall`, 
    `Full-time Pell`
  ), 
  `Part-time Pell` = ifelse(
    is.na(`Part-time Pell`), 
    `Pell overall`, 
    `Part-time Pell`
  ), 
  `Full-time non-Pell` = ifelse(
    is.na(`Full-time non-Pell`), 
    `Non-Pell overall`, 
    `Full-time non-Pell`
  ), 
  `Part-time non-Pell` = ifelse(
    is.na(`Part-time non-Pell`), 
    `Non-Pell overall`, 
    `Part-time non-Pell`
  )
) %>% mutate( # Finally we use overall 
  `Full-time Pell` = ifelse(
    is.na(`Full-time Pell`), 
    `All students overall`, 
    `Full-time Pell`
  ), 
  `Part-time Pell` = ifelse(
    is.na(`Part-time Pell`), 
    `All students overall`, 
    `Part-time Pell`
  ), 
  `Full-time non-Pell` = ifelse(
    is.na(`Full-time non-Pell`), 
    `All students overall`, 
    `Full-time non-Pell`
  ), 
  `Part-time non-Pell` = ifelse(
    is.na(`Part-time non-Pell`), 
    `All students overall`, 
    `Part-time non-Pell`
  )
) %>% select(
  `UNITID`, 
  `Full-time Pell`, 
  `Part-time Pell`, 
  `Full-time non-Pell`, 
  `Part-time non-Pell`
) %>% pivot_longer(
  cols=c(
    `Full-time Pell`, 
    `Part-time Pell`, 
    `Full-time non-Pell`, 
    `Part-time non-Pell`
  ), 
  names_to="OM Group",
  values_to="Expected certificates"
)

omAssc <- omAll %>% select(
  `UNITID`, 
  `OM Category`, 
  `Expected associate's degrees`
) %>% pivot_wider(
  id_cols=c(`UNITID`), 
  names_from=`OM Category`, 
  values_from=`Expected associate's degrees`
) %>% mutate( # First we prioritize full-time status
  `Full-time Pell` = ifelse(
    is.na(`Full-time Pell`), 
    `Full-time overall`, 
    `Full-time Pell`
  ), 
  `Part-time Pell` = ifelse(
    is.na(`Part-time Pell`), 
    `Part-time overall`, 
    `Part-time Pell`
  ), 
  `Full-time non-Pell` = ifelse(
    is.na(`Full-time non-Pell`), 
    `Full-time overall`, 
    `Full-time non-Pell`
  ), 
  `Part-time non-Pell` = ifelse(
    is.na(`Part-time non-Pell`), 
    `Part-time overall`, 
    `Part-time non-Pell`
  )
) %>% mutate( # Next we prioritize Pell status
  `Full-time Pell` = ifelse(
    is.na(`Full-time Pell`), 
    `Pell overall`, 
    `Full-time Pell`
  ), 
  `Part-time Pell` = ifelse(
    is.na(`Part-time Pell`), 
    `Pell overall`, 
    `Part-time Pell`
  ), 
  `Full-time non-Pell` = ifelse(
    is.na(`Full-time non-Pell`), 
    `Non-Pell overall`, 
    `Full-time non-Pell`
  ), 
  `Part-time non-Pell` = ifelse(
    is.na(`Part-time non-Pell`), 
    `Non-Pell overall`, 
    `Part-time non-Pell`
  )
) %>% mutate( # Finally we use overall 
  `Full-time Pell` = ifelse(
    is.na(`Full-time Pell`), 
    `All students overall`, 
    `Full-time Pell`
  ), 
  `Part-time Pell` = ifelse(
    is.na(`Part-time Pell`), 
    `All students overall`, 
    `Part-time Pell`
  ), 
  `Full-time non-Pell` = ifelse(
    is.na(`Full-time non-Pell`), 
    `All students overall`, 
    `Full-time non-Pell`
  ), 
  `Part-time non-Pell` = ifelse(
    is.na(`Part-time non-Pell`), 
    `All students overall`, 
    `Part-time non-Pell`
  )
) %>% select(
  `UNITID`, 
  `Full-time Pell`, 
  `Part-time Pell`, 
  `Full-time non-Pell`, 
  `Part-time non-Pell`
) %>% pivot_longer(
  cols=c(
    `Full-time Pell`, 
    `Part-time Pell`, 
    `Full-time non-Pell`, 
    `Part-time non-Pell`
  ), 
  names_to="OM Group",
  values_to="Expected associate's degrees"
)

omBach <- omAll %>% select(
  `UNITID`, 
  `OM Category`, 
  `Expected bachelor's degrees`
) %>% pivot_wider(
  id_cols=c(`UNITID`), 
  names_from=`OM Category`, 
  values_from=`Expected bachelor's degrees`
) %>% mutate( # First we prioritize full-time status
  `Full-time Pell` = ifelse(
    is.na(`Full-time Pell`), 
    `Full-time overall`, 
    `Full-time Pell`
  ), 
  `Part-time Pell` = ifelse(
    is.na(`Part-time Pell`), 
    `Part-time overall`, 
    `Part-time Pell`
  ), 
  `Full-time non-Pell` = ifelse(
    is.na(`Full-time non-Pell`), 
    `Full-time overall`, 
    `Full-time non-Pell`
  ), 
  `Part-time non-Pell` = ifelse(
    is.na(`Part-time non-Pell`), 
    `Part-time overall`, 
    `Part-time non-Pell`
  )
) %>% mutate( # Next we prioritize Pell status
  `Full-time Pell` = ifelse(
    is.na(`Full-time Pell`), 
    `Pell overall`, 
    `Full-time Pell`
  ), 
  `Part-time Pell` = ifelse(
    is.na(`Part-time Pell`), 
    `Pell overall`, 
    `Part-time Pell`
  ), 
  `Full-time non-Pell` = ifelse(
    is.na(`Full-time non-Pell`), 
    `Non-Pell overall`, 
    `Full-time non-Pell`
  ), 
  `Part-time non-Pell` = ifelse(
    is.na(`Part-time non-Pell`), 
    `Non-Pell overall`, 
    `Part-time non-Pell`
  )
) %>% mutate( # Finally we use overall 
  `Full-time Pell` = ifelse(
    is.na(`Full-time Pell`), 
    `All students overall`, 
    `Full-time Pell`
  ), 
  `Part-time Pell` = ifelse(
    is.na(`Part-time Pell`), 
    `All students overall`, 
    `Part-time Pell`
  ), 
  `Full-time non-Pell` = ifelse(
    is.na(`Full-time non-Pell`), 
    `All students overall`, 
    `Full-time non-Pell`
  ), 
  `Part-time non-Pell` = ifelse(
    is.na(`Part-time non-Pell`), 
    `All students overall`, 
    `Part-time non-Pell`
  )
) %>% select(
  `UNITID`, 
  `Full-time Pell`, 
  `Part-time Pell`, 
  `Full-time non-Pell`, 
  `Part-time non-Pell`
) %>% pivot_longer(
  cols=c(
    `Full-time Pell`, 
    `Part-time Pell`, 
    `Full-time non-Pell`, 
    `Part-time non-Pell`
  ), 
  names_to="OM Group",
  values_to="Expected bachelor's degrees"
)

omAll <- full_join(x=omCert, y=omAssc, by=c("UNITID", "OM Group"))
omAll <- full_join(x=omAll, y=omBach, by=c("UNITID", "OM Group"))
rm(omCert, omAssc, omBach)

omMap2 <- data.frame(
  `OM Group` = character(), 
  `Enrollment intensity` = character(), 
  `Receives federal grants` = character(), 
  check.names=FALSE
)
omMap2 <- omMap2 %>% add_row(`OM Group`="Full-time Pell", `Enrollment intensity`="Full-time", `Receives federal grants`="Yes")
omMap2 <- omMap2 %>% add_row(`OM Group`="Part-time Pell", `Enrollment intensity`="Part-time", `Receives federal grants`="Yes")
omMap2 <- omMap2 %>% add_row(`OM Group`="Full-time non-Pell", `Enrollment intensity`="Full-time", `Receives federal grants`="No")
omMap2 <- omMap2 %>% add_row(`OM Group`="Part-time non-Pell", `Enrollment intensity`="Part-time", `Receives federal grants`="No")
omAll <- left_join(x=omAll, y=omMap2, by="OM Group") %>% select(
  -(`OM Group`)
)
rm(omMap2)

studentDF <- left_join(x=studentDF, y=omAll, by=c("UNITID", "Enrollment intensity", "Receives federal grants"))
rm(omAll)

imputedCerts <- aggregate(
  data=studentDF, 
  `Expected certificates` ~ `Enrollment intensity` + `Carnegie NPSAS` + `Receives federal grants`, 
  FUN=mean
) %>% rename(
  `Imputed certificates` = `Expected certificates`
)
studentDF <- left_join(x=studentDF, y=imputedCerts, by=c("Enrollment intensity", "Carnegie NPSAS", "Receives federal grants")) %>% mutate(
  `Expected certificates` = ifelse(
    is.na(`Expected certificates`), 
    `Imputed certificates`,
    `Expected certificates`
  )
) %>% select(
  -(`Imputed certificates`)
)
rm(imputedCerts)

imputedAsscs <- aggregate(
  data=studentDF, 
  `Expected associate's degrees` ~ `Enrollment intensity` + `Carnegie NPSAS` + `Receives federal grants`, 
  FUN=mean
) %>% rename(
  `Imputed associate's degrees` = `Expected associate's degrees`
)
studentDF <- left_join(x=studentDF, y=imputedAsscs, by=c("Enrollment intensity", "Carnegie NPSAS", "Receives federal grants")) %>% mutate(
  `Expected associate's degrees` = ifelse(
    is.na(`Expected associate's degrees`), 
    `Imputed associate's degrees`,
    `Expected associate's degrees`
  )
) %>% select(
  -(`Imputed associate's degrees`)
)
rm(imputedAsscs)

imputedBachs <- aggregate(
  data=studentDF, 
  `Expected bachelor's degrees` ~ `Enrollment intensity` + `Carnegie NPSAS` + `Receives federal grants`, 
  FUN=mean
) %>% rename(
  `Imputed bachelor's degrees` = `Expected bachelor's degrees`
)
studentDF <- left_join(x=studentDF, y=imputedBachs, by=c("Enrollment intensity", "Carnegie NPSAS", "Receives federal grants")) %>% mutate(
  `Expected bachelor's degrees` = ifelse(
    is.na(`Expected bachelor's degrees`), 
    `Imputed bachelor's degrees`,
    `Expected bachelor's degrees`
  )
) %>% select(
  -(`Imputed bachelor's degrees`)
)
rm(imputedBachs)

#### End #### 

#### Save adjusted file ####

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling")

write.csv(studentDF, "All merged adjusted student data.csv", row.names=FALSE)

#### End #### 

################################################
#### Validity Check Set 1: SDS vs EFFY      ####
#### (To make sure nothing got messed up)   ####
################################################

#### Re-load EFFY ####

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

effy <- effy %>% mutate(
  `Index` = (1:nrow(effy))
)

effy <- left_join(x=effy, y=hd, by="UNITID")

effy <- left_join(x=effy, y=cost1, by="UNITID")

effy <- effy %>% mutate(
  `Tuition policy` = ifelse(
    is.na(`Tuition policy`), 
    "Does not vary tuition by in-state status", 
    `Tuition policy`
  )
)

effy <- effy %>% mutate(
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

effy <- left_join(x=effy, y=carnegieNPSAS, by="C18BASIC")
rm(carnegieNPSAS)

# Only doing this because of replicability issues, 1-18-2026 
effy <- effy %>% mutate(
  `Race NPSAS` = ifelse(
    `Race` %in% c("U.S. Nonresident", "Race/ethnicity unknown"), 
    "Native Hawaiian/other Pacific Islander", 
    `Race`
  )  
)

#### End #### 

#### Write function to compare effy and studentDF ####

compareTwo <- function(varName){
  
  effy2 <- effy %>% select(
    all_of(
      c(
        "Student count", 
        varName
      )
    )
  )
  names(effy2)[2] <- "Comparison variable"
  
  agg1 <- aggregate(
    data=effy2, 
    `Student count` ~ `Comparison variable`, 
    FUN=sum
  ) %>% rename(
    `effy count` = `Student count`
  ) %>% mutate(
    `effy count` = comma(`effy count`)
  )
  
  studentDF2 <- studentDF %>% select(
    all_of(
      c(
        "UNITID", 
        varName
      )
    )
  )
  names(studentDF2)[2] <- "Comparison variable"
  
  agg2 <- aggregate(
    data=studentDF2, 
    `UNITID` ~ `Comparison variable`, 
    FUN=length
  ) %>% rename(
    `studentDF count` = `UNITID`
  ) %>% mutate(
    `studentDF count` = comma(`studentDF count`)
  )
  
  agg3 <- full_join(x=agg1, y=agg2, by="Comparison variable")
  return(agg3)
  
  rm(effy2, studentDF2, agg1, agg2, agg3)
  
}

#### End #### 

#### Run comparison function ####

compareTwo("Race")
compareTwo("Race NPSAS")
compareTwo("Gender")
compareTwo("Enrollment intensity NPSAS")
compareTwo("STABBR")
compareTwo("Carnegie NPSAS")
compareTwo("Control")
compareTwo("Region NPSAS")
compareTwo("Tuition policy")

#### End #### 

#### Remove files no longer needed ####

rm(hd, cost1, effy)

#### End #### 

################################################
#### Validity Check Set 2: By STABBR and    ####
#### UNITID (to see if the distributions    ####
#### are weird for certain places)          ####
################################################

#### Write function to track studentDF ####

compareMost <- function(varName, stateOrCollege, ABTrigger, ABValue){
  
  tempDF <- studentDF %>% select(
    all_of(
      c(
        "UNITID", 
        "INSTNM", 
        "STABBR", 
        varName
      )
    )
  )
  names(tempDF)[4] <- "Comparison variable"
  
  if(ABTrigger==TRUE){
    tempDF <- tempDF %>% mutate(
      `Comparison variable` = ifelse(
        `Comparison variable` >= ABValue,
        paste("Over ", ABValue, sep=""),
        paste("Under ", ABValue, sep="")
      )
    )
  }
  
  if(stateOrCollege=="State"){
    numerator <- aggregate(
      data=tempDF, 
      `UNITID` ~ `STABBR` + `Comparison variable`, 
      FUN=length
    ) %>% pivot_wider(
      id_cols=c(`STABBR`), 
      names_from=`Comparison variable`, 
      values_from=`UNITID`
    )
    denominator <- aggregate(
      data=tempDF, 
      `UNITID` ~ `STABBR`, 
      FUN=length
    ) 
    percentages <- full_join(x=numerator, y=denominator, by="STABBR")
  }else{
    numerator <- aggregate(
      data=tempDF, 
      `UNITID` ~ `INSTNM` + `STABBR` + `Comparison variable`, 
      FUN=length
    ) %>% pivot_wider(
      id_cols=c(`INSTNM`, `STABBR`), 
      names_from=`Comparison variable`, 
      values_from=`UNITID`
    )
    denominator <- aggregate(
      data=tempDF, 
      `UNITID` ~ `INSTNM` + `STABBR`, 
      FUN=length
    ) 
    percentages <- full_join(x=numerator, y=denominator, by=c("INSTNM", "STABBR"))
  }
  
  percentages[is.na(percentages)] <- 0 
  
  return(percentages)
  rm(percentages, numerator, denominator, tempDF)
  
}

#### End #### 

#### Compare Zero-EFC by state and college #### 

compare1 <- compareMost(
  varName = "Zero-EFC", 
  stateOrCollege = "State",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Zero-EFC` = `Zero-EFC` / `UNITID`,
  `Nonzero-EFC` = `Nonzero-EFC` / `UNITID`
) %>% arrange(
  desc(`Zero-EFC`)
) %>% mutate(
  `Zero-EFC` = percent(`Zero-EFC`, accuracy=0.1), 
  `Nonzero-EFC` = percent(`Nonzero-EFC`, accuracy=0.1)
)

compare2 <- compareMost(
  varName = "Zero-EFC", 
  stateOrCollege = "College",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Zero-EFC` = `Zero-EFC` / `UNITID`,
  `Nonzero-EFC` = `Nonzero-EFC` / `UNITID`
) %>% arrange(
  desc(`Zero-EFC`)
) %>% mutate(
  `Zero-EFC` = percent(`Zero-EFC`, accuracy=0.1), 
  `Nonzero-EFC` = percent(`Nonzero-EFC`, accuracy=0.1)
)

#### End #### 

#### Compare tuition jurisdiction by state and college #### 

compare1 <- compareMost(
  varName = "Tuition jurisdiction", 
  stateOrCollege = "State",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `In-state tuition` = `In-state tuition` / `UNITID`,
  `Out-of-state tuition` = `Out-of-state tuition` / `UNITID`, 
  `No differential tuition charged` = `No differential tuition charged` / `UNITID`
) %>% arrange(
  desc(`In-state tuition`), desc(`Out-of-state tuition`)
) %>% mutate(
  `In-state tuition` = percent(`In-state tuition`, accuracy=0.1), 
  `Out-of-state tuition` = percent(`Out-of-state tuition`, accuracy=0.1), 
  `No differential tuition charged` = percent(`No differential tuition charged`, accuracy=0.1)
)

compare2 <- compareMost(
  varName = "Tuition jurisdiction", 
  stateOrCollege = "College",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `In-state tuition` = `In-state tuition` / `UNITID`,
  `Out-of-state tuition` = `Out-of-state tuition` / `UNITID`, 
  `No differential tuition charged` = `No differential tuition charged` / `UNITID`
) %>% arrange(
  desc(`In-state tuition`), desc(`Out-of-state tuition`)
) %>% mutate(
  `In-state tuition` = percent(`In-state tuition`, accuracy=0.1), 
  `Out-of-state tuition` = percent(`Out-of-state tuition`, accuracy=0.1), 
  `No differential tuition charged` = percent(`No differential tuition charged`, accuracy=0.1)
)

#### End #### 

#### Compare age by state and college #### 

compare1 <- compareMost(
  varName = "Age", 
  stateOrCollege = "State",
  ABTrigger = TRUE,
  ABValue = 25
) %>% mutate(
  `Over 25` = `Over 25` / `UNITID`,
  `Under 25` = `Under 25` / `UNITID`
) %>% arrange(
  desc(`Over 25`), desc(`Under 25`)
) %>% mutate(
  `Over 25` = percent(`Over 25`, accuracy=0.1), 
  `Under 25` = percent(`Under 25`, accuracy=0.1)
)

compare2 <- compareMost(
  varName = "Age", 
  stateOrCollege = "College",
  ABTrigger = TRUE,
  ABValue = 25
) %>% mutate(
  `Over 25` = `Over 25` / `UNITID`,
  `Under 25` = `Under 25` / `UNITID`
) %>% arrange(
  desc(`Over 25`), desc(`Under 25`)
) %>% mutate(
  `Over 25` = percent(`Over 25`, accuracy=0.1), 
  `Under 25` = percent(`Under 25`, accuracy=0.1)
)

#### End #### 

#### Compare citizenship by state and college #### 

compare1 <- compareMost(
  varName = "Citizenship", 
  stateOrCollege = "State",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Citizen or eligible non-citizen` = `Citizen or eligible non-citizen` / `UNITID`,
  `Non-citizen` = `Non-citizen` / `UNITID`
) %>% arrange(
  desc(`Citizen or eligible non-citizen`)
) %>% mutate(
  `Citizen or eligible non-citizen` = percent(`Citizen or eligible non-citizen`, accuracy=0.1), 
  `Non-citizen` = percent(`Non-citizen`, accuracy=0.1)
)

compare2 <- compareMost(
  varName = "Citizenship", 
  stateOrCollege = "College",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Citizen or eligible non-citizen` = `Citizen or eligible non-citizen` / `UNITID`,
  `Non-citizen` = `Non-citizen` / `UNITID`
) %>% arrange(
  desc(`Citizen or eligible non-citizen`)
) %>% mutate(
  `Citizen or eligible non-citizen` = percent(`Citizen or eligible non-citizen`, accuracy=0.1), 
  `Non-citizen` = percent(`Non-citizen`, accuracy=0.1)
)

#### End #### 

#### Compare veteran status by state and college #### 

compare1 <- compareMost(
  varName = "Veteran status", 
  stateOrCollege = "State",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Veteran` = `Veteran` / `UNITID`,
  `Not a veteran` = `Not a veteran` / `UNITID`
) %>% arrange(
  desc(`Veteran`)
) %>% mutate(
  `Veteran` = percent(`Veteran`, accuracy=0.1), 
  `Not a veteran` = percent(`Not a veteran`, accuracy=0.1)
)

compare2 <- compareMost(
  varName = "Veteran status", 
  stateOrCollege = "College",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Veteran` = `Veteran` / `UNITID`,
  `Not a veteran` = `Not a veteran` / `UNITID`
) %>% arrange(
  desc(`Veteran`)
) %>% mutate(
  `Veteran` = percent(`Veteran`, accuracy=0.1), 
  `Not a veteran` = percent(`Not a veteran`, accuracy=0.1)
)

#### End #### 

#### Compare dependency status by state and college #### 

compare1 <- compareMost(
  varName = "Dependency status", 
  stateOrCollege = "State",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Dependent` = `Dependent` / `UNITID`,
  `Independent` = `Independent` / `UNITID`
) %>% arrange(
  desc(`Dependent`)
) %>% mutate(
  `Dependent` = percent(`Dependent`, accuracy=0.1), 
  `Independent` = percent(`Independent`, accuracy=0.1)
)

compare2 <- compareMost(
  varName = "Dependency status", 
  stateOrCollege = "College",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Dependent` = `Dependent` / `UNITID`,
  `Independent` = `Independent` / `UNITID`
) %>% arrange(
  desc(`Dependent`)
) %>% mutate(
  `Dependent` = percent(`Dependent`, accuracy=0.1), 
  `Independent` = percent(`Independent`, accuracy=0.1)
)

#### End #### 

#### Compare FAFSA completion status by state and college #### 

compare1 <- compareMost(
  varName = "Applied for federal aid", 
  stateOrCollege = "State",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Yes` = `Yes` / `UNITID`,
  `No` = `No` / `UNITID`
) %>% arrange(
  desc(`Yes`)
) %>% mutate(
  `Yes` = percent(`Yes`, accuracy=0.1), 
  `No` = percent(`No`, accuracy=0.1)
)

compare2 <- compareMost(
  varName = "Applied for federal aid", 
  stateOrCollege = "College",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Yes` = `Yes` / `UNITID`,
  `No` = `No` / `UNITID`
) %>% arrange(
  desc(`Yes`)
) %>% mutate(
  `Yes` = percent(`Yes`, accuracy=0.1), 
  `No` = percent(`No`, accuracy=0.1)
)

#### End #### 

#### Compare federal grant recipient status by state and college #### 

compare1 <- compareMost(
  varName = "Receives federal grants", 
  stateOrCollege = "State",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Yes` = `Yes` / `UNITID`,
  `No` = `No` / `UNITID`
) %>% arrange(
  desc(`Yes`)
) %>% mutate(
  `Yes` = percent(`Yes`, accuracy=0.1), 
  `No` = percent(`No`, accuracy=0.1)
)

compare2 <- compareMost(
  varName = "Receives federal grants", 
  stateOrCollege = "College",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Yes` = `Yes` / `UNITID`,
  `No` = `No` / `UNITID`
) %>% arrange(
  desc(`Yes`)
) %>% mutate(
  `Yes` = percent(`Yes`, accuracy=0.1), 
  `No` = percent(`No`, accuracy=0.1)
)

#### End #### 

#### Compare VA/DOD grant recipient status by state and college #### 

compare1 <- compareMost(
  varName = "Receives VA/DOD grants", 
  stateOrCollege = "State",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Yes` = `Yes` / `UNITID`,
  `No` = `No` / `UNITID`
) %>% arrange(
  desc(`Yes`)
) %>% mutate(
  `Yes` = percent(`Yes`, accuracy=0.1), 
  `No` = percent(`No`, accuracy=0.1)
)

compare2 <- compareMost(
  varName = "Receives VA/DOD grants", 
  stateOrCollege = "College",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Yes` = `Yes` / `UNITID`,
  `No` = `No` / `UNITID`
) %>% arrange(
  desc(`Yes`)
) %>% mutate(
  `Yes` = percent(`Yes`, accuracy=0.1), 
  `No` = percent(`No`, accuracy=0.1)
)

#### End #### 

#### Compare state grant recipient status by state and college #### 

compare1 <- compareMost(
  varName = "Receives state grants", 
  stateOrCollege = "State",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Yes` = `Yes` / `UNITID`,
  `No` = `No` / `UNITID`
) %>% arrange(
  desc(`Yes`)
) %>% mutate(
  `Yes` = percent(`Yes`, accuracy=0.1), 
  `No` = percent(`No`, accuracy=0.1)
)

compare2 <- compareMost(
  varName = "Receives state grants", 
  stateOrCollege = "College",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Yes` = `Yes` / `UNITID`,
  `No` = `No` / `UNITID`
) %>% arrange(
  desc(`Yes`)
) %>% mutate(
  `Yes` = percent(`Yes`, accuracy=0.1), 
  `No` = percent(`No`, accuracy=0.1)
)

#### End #### 

#### Compare institutional grant recipient status by state and college #### 

compare1 <- compareMost(
  varName = "Receives institutional grants", 
  stateOrCollege = "State",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Yes` = `Yes` / `UNITID`,
  `No` = `No` / `UNITID`
) %>% arrange(
  desc(`Yes`)
) %>% mutate(
  `Yes` = percent(`Yes`, accuracy=0.1), 
  `No` = percent(`No`, accuracy=0.1)
)

compare2 <- compareMost(
  varName = "Receives institutional grants", 
  stateOrCollege = "College",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Yes` = `Yes` / `UNITID`,
  `No` = `No` / `UNITID`
) %>% arrange(
  desc(`Yes`)
) %>% mutate(
  `Yes` = percent(`Yes`, accuracy=0.1), 
  `No` = percent(`No`, accuracy=0.1)
)

#### End #### 

#### Compare private grant recipient status by state and college #### 

compare1 <- compareMost(
  varName = "Receives private grants", 
  stateOrCollege = "State",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Yes` = `Yes` / `UNITID`,
  `No` = `No` / `UNITID`
) %>% arrange(
  desc(`Yes`)
) %>% mutate(
  `Yes` = percent(`Yes`, accuracy=0.1), 
  `No` = percent(`No`, accuracy=0.1)
)

compare2 <- compareMost(
  varName = "Receives private grants", 
  stateOrCollege = "College",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Yes` = `Yes` / `UNITID`,
  `No` = `No` / `UNITID`
) %>% arrange(
  desc(`Yes`)
) %>% mutate(
  `Yes` = percent(`Yes`, accuracy=0.1), 
  `No` = percent(`No`, accuracy=0.1)
)

#### End #### 

#### Compare federal loan recipient status by state and college #### 

compare1 <- compareMost(
  varName = "Receives federal loans", 
  stateOrCollege = "State",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Yes` = `Yes` / `UNITID`,
  `No` = `No` / `UNITID`
) %>% arrange(
  desc(`Yes`)
) %>% mutate(
  `Yes` = percent(`Yes`, accuracy=0.1), 
  `No` = percent(`No`, accuracy=0.1)
)

compare2 <- compareMost(
  varName = "Receives federal loans", 
  stateOrCollege = "College",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Yes` = `Yes` / `UNITID`,
  `No` = `No` / `UNITID`
) %>% arrange(
  desc(`Yes`)
) %>% mutate(
  `Yes` = percent(`Yes`, accuracy=0.1), 
  `No` = percent(`No`, accuracy=0.1)
)

#### End #### 

#### Compare parent loan recipient status by state and college #### 

compare1 <- compareMost(
  varName = "Receives parent loans", 
  stateOrCollege = "State",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Yes` = `Yes` / `UNITID`,
  `No` = `No` / `UNITID`
) %>% arrange(
  desc(`Yes`)
) %>% mutate(
  `Yes` = percent(`Yes`, accuracy=0.1), 
  `No` = percent(`No`, accuracy=0.1)
)

compare2 <- compareMost(
  varName = "Receives parent loans", 
  stateOrCollege = "College",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Yes` = `Yes` / `UNITID`,
  `No` = `No` / `UNITID`
) %>% arrange(
  desc(`Yes`)
) %>% mutate(
  `Yes` = percent(`Yes`, accuracy=0.1), 
  `No` = percent(`No`, accuracy=0.1)
)

#### End #### 

#### Compare parent status by state and college #### 

compare1 <- compareMost(
  varName = "Parent status", 
  stateOrCollege = "State",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Has dependents` = `Has dependents` / `UNITID`,
  `Does not have dependents` = `Does not have dependents` / `UNITID`
) %>% arrange(
  desc(`Has dependents`)
) %>% mutate(
  `Has dependents` = percent(`Has dependents`, accuracy=0.1), 
  `Does not have dependents` = percent(`Does not have dependents`, accuracy=0.1)
)

compare2 <- compareMost(
  varName = "Parent status", 
  stateOrCollege = "College",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Has dependents` = `Has dependents` / `UNITID`,
  `Does not have dependents` = `Does not have dependents` / `UNITID`
) %>% arrange(
  desc(`Has dependents`)
) %>% mutate(
  `Has dependents` = percent(`Has dependents`, accuracy=0.1), 
  `Does not have dependents` = percent(`Does not have dependents`, accuracy=0.1)
)

#### End ####

#### Compare parental education status by state and college #### 

compare1 <- compareMost(
  varName = "First-gen status", 
  stateOrCollege = "State",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Not first-gen` = `Not first-gen` / `UNITID`,
  `First-gen` = `First-gen` / `UNITID`
) %>% arrange(
  desc(`Not first-gen`)
) %>% mutate(
  `Not first-gen` = percent(`Not first-gen`, accuracy=0.1), 
  `First-gen` = percent(`First-gen`, accuracy=0.1)
)

compare2 <- compareMost(
  varName = "First-gen status", 
  stateOrCollege = "College",
  ABTrigger = FALSE,
  ABValue = 0
) %>% mutate(
  `Not first-gen` = `Not first-gen` / `UNITID`,
  `First-gen` = `First-gen` / `UNITID`
) %>% arrange(
  desc(`Not first-gen`)
) %>% mutate(
  `Not first-gen` = percent(`Not first-gen`, accuracy=0.1), 
  `First-gen` = percent(`First-gen`, accuracy=0.1)
)

#### End #### 

################################################
#### Validity Check Set 3:                  ####
#### Aggregate sums and net price summary   ####
################################################

#### Aggregate sums #### 

dollar(sum(studentDF$`Federal grant amount`))
dollar(sum(studentDF$`VA/DOD grant amount`))
dollar(sum(studentDF$`State grant amount`))
dollar(sum(studentDF$`Institutional grant amount`))
dollar(sum(studentDF$`Private grant amount`))
dollar(sum(studentDF$`Federal loan amount`))
dollar(sum(studentDF$`Parent loan amount`))

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
  
  print(as.data.frame(tempDF))
  
  rm(tempDF, totalStudents)
  
}

#### End #### 

#### Write function to display distribution as percentiles ####

showPercentiles <- function(variableName, removeZeros, USD){
  
  tempDF <- studentDF %>% select(
    all_of(variableName)
  )
  names(tempDF)[1] <- "InterestVar"
  
  if(removeZeros){
    tempDF <- tempDF %>% filter(
      `InterestVar` > 0
    )
  }
  
  if(USD){
    print(as.data.frame(dollar(quantile(tempDF$`InterestVar`, probs = seq(.1, .9, by = .1)), accuracy=1)))
  }else{
    print(as.data.frame(quantile(tempDF$`InterestVar`, probs = seq(.1, .9, by = .1))))
  }
  
  rm(tempDF)
  
}

#### End #### 

#### Run function to display distributions ####

showDistribution("Control")
showDistribution("Region NPSAS")
showDistribution("Race NPSAS")
showDistribution("Carnegie NPSAS")
showDistribution("Enrollment intensity NPSAS")
showDistribution("Gender")
showDistribution("Zero-EFC")
showPercentiles("EFC", removeZeros=TRUE, USD=TRUE)
showDistribution("Tuition jurisdiction")
showPercentiles("Tuition and fees paid", removeZeros=TRUE, USD=TRUE)
showPercentiles("Age", removeZeros=FALSE, USD=FALSE)
showDistribution("Citizenship")
showDistribution("Veteran status")
showDistribution("Dependency status")
showDistribution("Applied for federal aid")
showPercentiles("Non-tuition expense budget", removeZeros=FALSE, USD=TRUE)
showDistribution("Receives federal grants")
showDistribution("Receives VA/DOD grants")
showDistribution("Receives state grants")
showDistribution("Receives institutional grants")
showDistribution("Receives private grants")
showDistribution("Receives federal loans")
showDistribution("Receives parent loans")
showPercentiles("Federal grant amount", removeZeros=TRUE, USD=TRUE)
showPercentiles("VA/DOD grant amount", removeZeros=TRUE, USD=TRUE)
showPercentiles("State grant amount", removeZeros=TRUE, USD=TRUE)
showPercentiles("Institutional grant amount", removeZeros=TRUE, USD=TRUE)
showPercentiles("Private grant amount", removeZeros=TRUE, USD=TRUE)
showPercentiles("Federal loan amount", removeZeros=TRUE, USD=TRUE)
showPercentiles("Parent loan amount", removeZeros=TRUE, USD=TRUE)
showDistribution("Parent status")
showDistribution("Parental education attainment")
showPercentiles("Total cost", removeZeros=FALSE, USD=TRUE)
showDistribution("Receives any grants")
showPercentiles("Total grants", removeZeros=TRUE, USD=TRUE)
showPercentiles("Net price", removeZeros=FALSE, USD=TRUE)
showDistribution("Receives any loans")
showPercentiles("Total loans", removeZeros=TRUE, USD=TRUE)

#### End ####

rm(studentDF)
