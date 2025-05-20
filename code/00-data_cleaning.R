library(dataverse)
library(tidyverse)
library(haven)
library(here)

files <- c("cces_2006_common.dta", "CCES07_OUTPUT.sav", "cces_2008_common.dta", "CCES_2010_CommonContent.sav", 
            "commoncontent2012.dta", 
            "CCES14_Common_Content_Validated.dta",
           "CCES16_Common_OUTPUT_Feb2018_VV.dta",
           "Common Content Data.dta", "cces18_common_vv.dta",
           "CCES19_Common_OUTPUT.tab", "CES20_Common_OUTPUT_vv.csv",
           "CCES22_Common_OUTPUT_vv_topost.csv")

doi <- c("10.7910/DVN/Q8HC9N/OK6YQS", "10.7910/DVN/OOXTJ5/C9WH7I", "10.7910/DVN/YUYIVB/P3MCIW",
         "10.7910/DVN/VKKRWA/G859FG",
         "10.7910/DVN/HQEVPK/ONOLPW", "10.7910/DVN/XFXJVY/GQMQLG",
         "10.7910/DVN/GDF6Z0/JPMOZZ", "10.7910/DVN/3STEZY/VS6TBP",
         "10.7910/DVN/ZSBZ7K/H5IDTA","10.7910/DVN/WOT7O8", 
         "10.7910/DVN/E9N6PH", "10.7910/DVN/PR4L8P")

years <- c(seq(2006, 2008,1),2010, seq(2012, 2016, 2), seq(2017,2020,1), 2022) 
server <- "dataverse.harvard.edu"
all <- list()  

for(i in 1:length(files)) {  
  if (grepl(".sav", files[i])) {
    data <- get_dataframe_by_doi( 
      server = server,
      filedoi = doi[i],
      original= TRUE,
      .f = haven::read_sav) 
  }
  else if (grepl(".csv", files[i])) {
    data <- get_dataframe_by_name( 
      filename = files[i],
      dataset = doi[i],
      server = server,
    original = TRUE, 
    .f = readr::read_csv)
  }
  else if (grepl(".dta", files[i])) {
    data <- get_dataframe_by_doi( 
      filedoi = doi[i],
      server = server,
      original = TRUE, 
      .f = haven::read_dta)
  }
  else {
    data <- get_dataframe_by_name( 
      filename = files[i],
      server = server,
      dataset = doi[i])
  }
  all[[i]] <- data  
}
names(all) <- years

remove(data)

gc()
  
mapping <- list(
  "2006" = list(state = "v1002",
                votereg = "v3004",
                vote_intent = "vote06turn",
                union = "v2082",
                unionhh = "v2084",
                group_1 = "v4050",
                group_2 = "v4051",
                group_3 = "v4052",
                group_4 = "v4053",
                group_5 = "v4054",
                group_6 = "v4055",
                group_7 = "v4056",
                group_8 = "v4057",
                group_9 = "v4058",
                group_10 = "v4058",
                group_11 = "v4059",
                group_12 = "V4060",
                donate = "v4062",
                contact = "v4065",
                pid7 = "v3007",
                gender = "v2004",
                race = "v2005",
                educ = "v2018",
                marstat = "v2019",
                employ = "v2030",
                immstat = "v3086",
                inc = "v2032",
                investor = "v2086",
                ideo5 = "v2021",
                relig = "v2022",
                religimp = "v2029",
                home = "v2033",
                int = "v2042",
                tv = "v2043",
                birthyr = "v2020",
                mil_1 = "v3081",
                mil_2 = "v3082",
                mil_3 = "v3083",
                mil_4 = "v3084",
                mil_5 = "v3085",
                know_h1 = "v3036",
                know_h2 = "v3039",
                child = "v2129",
                news_watch="v2046"),
  
  "2007" = list(state = "CC06_V1002",
                votereg = "CC06_V3004",
                vote_prior = "CC06_V4004",
                union = "CC06_V2082",
                unionhh = "CC06_V2084",
                group_1 = "CC06_V4050",
                group_2 = "CC06_V4051",
                group_3 = "CC06_V4052",
                group_4 = "CC06_V4053",
                group_5 = "CC06_V4054",
                group_6 = "CC06_V4055",
                group_7 = "CC06_V4056",
                group_8 = "CC06_V4057",
                group_9 = "CC06_V4058",
                group_10 = "CC06_V4058",
                group_11 = "CC06_v4059",
                group_12 = "CC06_V4060",
                donate = "CC06_V4062",
                contact = "CC06_V4065",
                pid7 = "CC06_V3007",
                gender = "CC06_V2004",
                race = "CC06_V2005",
                educ = "CC06_V2018",
                marstat = "CC06_V2019",
                employ = "CC06_V2030",
                immstat = "CC06_V3086",
                inc = "CC06_V2032",
                investor = "CC06_V2086",
                ideo5 = "CC06_V2021",
                relig = "CC06_V2022",
                religimp = "CC06_V2029",
                home = "CC06_V2033",
                int = "CC06_V2042",
                tv = "CC06_V2043",
                birthyr = "CC06_V2020",
                mil_1 = "CC06_V3081",
                mil_2 = "CC06_V3082",
                mil_3 = "CC06_V3083",
                mil_4 = "CC06_V3084",
                mil_5 = "CC06_V3085",
                know_h1 = "CC06_V3036",
                know_h2 = "CC06_V3039",
                child = "CC06_V2129",
                internet = "CC_06_V2106",
                news_watch ="Cc06_v2046"),
  
  "2008" = list(votereg = "V203",
                state = "v206",
                birthyr = "v207",
                gender = "v208",
                race = "v211",
                marstat = "v214",
                child = "V242",
                educ = "v213",
                employ = "v209",
                blog = "cc301_1",
                news = "cc301_2",
                paper = "cc301_3",
                radio =  "cc301_4",
                pid7 = "CC307a",
                gov_name = "CC309a",
                sen1_name = "CC309b",
                sen2_name = "CC309c",
                house_name = "CC309d",
                religimp = "v216",
                relig = "v219",
                inc = "v246",
                home = "cc333",
                ideo5 = "v243",
                newsint = "v245",
                mil_1 = "CC328_1",
                mil_2 = "CC328_2",
                mil_3 = "CC328_3",
                mil_4 = "CC328_4",
                mil_5 = "CC328_5",
                unionhh = "cc329",
                immstat = "CC332",
                phone = "V255",
                meeting = "CC415_1",
                sign = "CC415_3",
                work_pol = "CC415_4",
                donate = "CC415_6",
                vote_intent = "CC326"),
  
                
  
  "2010" = list(votereg = "V203",
                state = "v206",
                birthyr = "v207",
                gender = "v208",
                race = "v211",
                marstat = "v214",
                child = "V242",
                educ = "v213",
                employ = "v209",
                blog = "cc301_1",
                news = "cc301_2",
                paper = "cc301_3",
                radio =  "cc301_4",
                pid7 = "V212d",
                gov_name = "CC310a",
                sen1_name = "CC310b",
                sen2_name = "CC310c",
                house_name = "CC310d",
                religimp = "v216",
                relig = "v219",
                inc = "v246",
                home = "V250",
                ideo5 = "v243",
                newsint = "v244",
                mil_1 = "V252",
                mil_2 = "V253",
                mil_3 = "V254",
                mil_4 = "V255",
                mil_5 = "V256",
                union = "V264",
                unionhh = "v265",
                investor = "V266",
                immstat = "V263",
                phone = "V273",
                internet = "v274",
                meeting = "CC417a_1",
                sign = "CC417a_2",
                work_pol = "CC417a_3",
                donate = "CC417a_4",
                contact = "CC425a",
                run = "CC418a",
                vote_intent = "CC354",
                vote_prior = "CC316"),
  
  
  "2012" = list(state = "inputstate",
                birthyr = "birthyr",
                gender = "gender",
                educ = "educ",
                race = "race",
                votereg = "votereg",
                employ = "employ",
                phone = "phone",
                internet = "internethome",
                marstat = "marstat",
                pid7 = "pid7",
                ideo5 = "ideo5",
                religimp = "pew_religimp",
                relig = "religpew",
                int = "newsint",
                inc = "faminc",
                home = "ownhome",
                mil_1 = "milstat_1",
                mil_2 = "milstat_2",
                mil_3 = "milstat_3",
                mil_4 = "milstat_4",
                mil_5 = "milstat_5",
                immstat = "immstat",
                union = "union",
                unionhh = "unionhh",
                investor = "investor",
                gov_name = "CC310a",
                sen1_name = "CC310b",
                sen2_name = "CC310c",
                house_name = "CC310d",
                vote_prior = "CC316",
                vote_intent = "CC354",
                meeting = "CC417a_1",
                sign = "CC417a_2",
                work_pol = "CC417a_3",
                donate = "CC417a_4",
                contact = "CC425a",
                run  = "CC418a",
                child = "child18"),
  
  "2014" = list(state = "inputstate",
                birthyr = "birthyr",
                gender = "gender",
                educ = "educ",
                race = "race",
                votereg = "votereg",
                employ = "employ",
                phone = "phone",
                internet = "internethome",
                marstat = "marstat",
                pid7 = "pid7",
                ideo5 = "ideo5",
                religimp = "pew_religimp",
                relig = "religpew",
                int = "newsint",
                inc = "faminc",
                home = "ownhome",
                mil_1 = "milstat_1",
                mil_2 = "milstat_2",
                mil_3 = "milstat_3",
                mil_4 = "milstat_4",
                mil_5 = "milstat_5",
                immstat = "immstat",
                union = "union",
                unionhh = "unionhh",
                investor = "investor",
                gov_name = "CC14_310a",
                sen1_name = "CC14_310b",
                sen2_name = "CC14_310c",
                house_name = "CC14_310d",
                vote_prior = "CC14_316",
                vote_intent = "CC354",
                meeting = "CC417a_1",
                sign = "CC417a_2",
                work_pol = "CC417a_3",
                donate = "CC417a_4",
                contact = "CC425a",
                run  = "CC418a",
                child = "child18",
                blog = "CC14_301_1",
                news = "CC14_301_2",
                paper = "CC14_301_3",
                radio = "CC14_301_4"),
  
  "2016" = list(state = "inputstate",
                birthyr = "birthyr",
                gender = "gender",
                educ = "educ",
                race = "race",
                votereg = "votereg",
                employ = "employ",
                phone = "phone",
                internet = "internethome",
                marstat = "marstat",
                pid7 = "pid7",
                ideo5 = "ideo5",
                religimp = "pew_religimp",
                relig = "religpew",
                int = "newsint",
                inc = "faminc",
                home = "ownhome",
                mil_1 = "milstat_1",
                mil_2 = "milstat_2",
                mil_3 = "milstat_3",
                mil_4 = "milstat_4",
                mil_5 = "milstat_5",
                immstat = "immstat",
                union = "union",
                unionhh = "unionhh",
                investor = "investor",
                gov_name = "CC16_322a",
                sen1_name = "CC16_322b",
                sen2_name = "CC16_322c",
                house_name = "CC16_322d",
                vote_prior = "CC16_316",
                vote_intent = "CC16_364",
                meeting = "CC16_417a_1",
                sign = "CC16_417a_2",
                work_pol = "CC16_417a_3",
                donate = "CC16_417a_4",
                contact = "CC16_425a",
                run  = "CC16_418a",
                child = "child18",
                blog = "CC16_300_1",
                news = "CC16_300_2",
                paper = "CC16_300_3",
                radio = "CC16_300_4",
                social_media = "CC16_300_5"),
                
  
  "2017" = list(state = "inputstate",
                birthyr = "birthyr",
                gender = "gender",
                educ = "educ",
                race = "race",
                votereg = "votereg",
                employ = "employ",
                phone = "phone",
                internet = "internethome",
                marstat = "marstat",
                pid7 = "pid7",
                ideo5 = "ideo5",
                religimp = "pew_religimp",
                relig = "religpew",
                int = "newsint",
                inc = "faminc_new",
                home = "ownhome",
                mil_1 = "milstat_1",
                mil_2 = "milstat_2",
                mil_3 = "milstat_3",
                mil_4 = "milstat_4",
                mil_5 = "milstat_5",
                immstat = "immstat",
                union = "union",
                unionhh = "unionhh",
                investor = "investor",
                gov_name = "CC17_321a",
                sen1_name = "CC17_321b",
                sen2_name = "CC17_321c",
                house_name = "CC17_321d",
                vote_prior = "CC17_326",
                vote_intent = "CC17_329",
                donate = "CC17_304_15",
                protest = "CC17_304_12",
                work_pol = "CC17_304_14",
                contact = "CC17_304_13",
                child = "child18"),
                
  "2018" = list(state = "inputstate",
                birthyr = "birthyr",
                gender = "gender",
                educ = "educ",
                race = "race",
                votereg = "votereg",
                employ = "employ",
                phone = "phone",
                internet = "internethome",
                marstat = "marstat",
                pid7 = "pid7",
                ideo5 = "ideo5",
                religimp = "pew_religimp",
                relig = "religpew",
                int = "newsint",
                inc = "faminc_new",
                home = "ownhome",
                mil_1 = "milstat_1",
                mil_2 = "milstat_2",
                mil_3 = "milstat_3",
                mil_4 = "milstat_4",
                mil_5 = "milstat_5",
                immstat = "immstat",
                union = "union",
                unionhh = "unionhh",
                investor = "investor",
                gov_name = "CC18_310a",
                sen1_name = "CC18_310b",
                sen2_name = "CC18_310c",
                house_name = "CC18_310d",
                vote_prior = "CC18_316",
                vote_intent = "CC18_350",
                meeting = "CC18_417a_1",
                sign = "CC18_417a_2",
                work_pol = "CC18_417a_3",
                donate = "CC18_417a_6",
                contact = "CC18_417a_5",
                protest = "CC18_417a_4",
                run  = "CC18_418a",
                child = "child18",
                blog = "CC18_300_1",
                news = "CC18_300_2",
                paper = "CC18_300_3",
                radio = "CC18_300_4",
                social_media = "CC18_300_5"),
  
  "2019" = list(state = "inputstate",
                birthyr = "birthyr",
                gender = "gender",
                educ = "educ",
                race = "race",
                votereg = "votereg",
                employ = "employ",
                phone = "phone",
                internet = "internethome",
                marstat = "marstat",
                pid7 = "pid7",
                ideo5 = "ideo5",
                religimp = "pew_religimp",
                relig = "religpew",
                int = "newsint",
                inc = "faminc_new",
                home = "ownhome",
                mil_1 = "milstat_1",
                mil_2 = "milstat_2",
                mil_3 = "milstat_3",
                mil_4 = "milstat_4",
                mil_5 = "milstat_5",
                immstat = "immstat",
                union = "union",
                unionhh = "unionhh",
                investor = "investor",
                gov_name = "CC19_310a",
                sen1_name = "CC19_310b",
                sen2_name = "CC19_310c",
                house_name = "CC19_310d",
                vote_prior = "CC19_313",
                vote_intent = "CC19_314x",
                donate = "CC19_303_15",
                protest = "CC19_303_12",
                work_pol = "CC19_303_14",
                contact = "CC19_303_13",
                child = "child18",
                blog = "CC19_300_1",
                news = "CC19_300_2",
                paper = "CC19_300_3",
                radio = "CC19_300_4",
                social_media = "CC19_300_5"),

     
  "2020" = list(state = "inputstate",
                birthyr = "birthyr",
                gender = "gender",
                educ = "educ",
                race = "race",
                votereg = "votereg",
                employ = "employ",
                phone = "phone",
                internet = "internethome",
                marstat = "marstat",
                pid7 = "pid7",
                ideo5 = "ideo5",
                religimp = "pew_religimp",
                relig = "religpew",
                int = "newsint",
                inc = "faminc_new",
                home = "ownhome",
                mil_1 = "milstat_1",
                mil_2 = "milstat_2",
                mil_3 = "milstat_3",
                mil_4 = "milstat_4",
                mil_5 = "milstat_5",
                immstat = "immstat",
                union = "union",
                unionhh = "unionhh",
                investor = "investor",
                gov_name = "CC20_311a",
                sen1_name = "CC20_311b",
                sen2_name = "CC20_311c",
                house_name = "CC20_311d",
                vote_prior = "presvote16post",
                vote_intent = "CC20_363",
                meeting = "CC20_430a_1",
                sign = "CC20_430a_2",
                work_pol = "CC20_430a_3",
                protest = "CC20_430a_4",
                contact = "CC20_431a",
                donate = "CC20_430a_6",
                run  = "CC20_432a",
                child = "child18",
                blog = "CC20_300_1",
                news = "CC20_300_2",
                paper = "CC20_300_3",
                radio = "CC20_300_4",
                social_media = "CC20_300_5"),
  
  "2022" = list(state = "inputstate",
                birthyr = "birthyr",
                gender = "gender4",
                educ = "educ",
                race = "race",
                votereg = "votereg",
                employ = "employ",
                phone = "phone",
                internet = "internethome",
                marstat = "marstat",
                pid7 = "pid7",
                ideo5 = "ideo5",
                religimp = "pew_religimp",
                relig = "religpew",
                int = "newsint",
                inc = "faminc_new",
                home = "ownhome",
                mil_1 = "milstat_1",
                mil_2 = "milstat_2",
                mil_3 = "milstat_3",
                mil_4 = "milstat_4",
                mil_5 = "milstat_5",
                immstat = "immstat",
                union = "union",
                unionhh = "unionhh",
                investor = "investor",
                gov_name = "CC22_311a",
                sen1_name = "CC22_311b",
                sen2_name = "CC22_311c",
                house_name = "CC22_311d",
                vote_prior = "presvote20post",
                vote_intent = "CC22_363",
                meeting = "CC22_430a_1",
                sign = "CC22_430a_2",
                work_pol = "CC22_430a_3",
                protest = "CC22_430a_4",
                contact = "CC22_431a",
                donate = "CC22_430a_6",
                run  = "CC22_432a",
                child = "child18",
                blog = "CC22_300_1",
                news = "CC22_300_2",
                paper = "CC22_300_3",
                radio = "CC22_300_4",
                social_media = "CC22_300_5") 
  )

columns <- unique(c(
  names(mapping[[1]]),
  names(mapping[[2]]),
  names(mapping[[3]]),
  names(mapping[[4]]),
  names(mapping[[5]]),
  names(mapping[[6]]),
  names(mapping[[7]]),
  names(mapping[[8]]),
  names(mapping[[9]]),
  names(mapping[[10]]),
  names(mapping[[11]]),
  names(mapping[[12]])
))





process <- function(data, year, mapping) {
  
  names(data) <- tolower(names(data))
  mapping[[as.character(year)]] <- lapply(mapping[[as.character(year)]], tolower)
  
  
  missing <- setdiff(columns, names(mapping[[as.character(year)]]))
  
  for(col in missing) {
    mapping[[as.character(year)]][col] <- mapping[[as.character(year)]]$union
  }
  
  data <- data %>%
    mutate(
      year = year,
      state = case_when(
        is.character(!!sym(mapping[[as.character(year)]]$state)) ~
          factor(!!sym(mapping[[as.character(year)]]$state)),
        TRUE ~ factor(recode(
          !!sym(mapping[[as.character(year)]]$state),
          `1` = "AL", `2` = "AK", `4` = "AZ", `5` = "AR", `6` = "CA",
          `8` = "CO", `9` = "CT", `10` = "DE", `11` = "DC", `12` = "FL",
          `13` = "GA", `15` = "HI", `16` = "ID", `17` = "IL", `18` = "ID",
          `19` = "IA", `20` = "KS", `21` = "KY", `22` = "LA", `23` = "MA",
          `24` = "MD", `25` = "MA", `26` = "MI", `27` = "MN", `28` = "MS",
          `29` = "MO", `30` = "MT", `31` = "NE", `32` = "NV", `33` = "NH",
          `34` = "NJ", `35` = "NM", `36` = "NY", `37` = "NC", `38` = "ND",
          `39` = "OH", `40` = "OK", `41` = "OR", `42` = "PA", `44` = "RI",
          `45` = "SC", `46` = "SD", `47` = "TN", `48` = "TX", `49` = "UT",
          `50` = "VT", `51` = "VA", `53` = "WA", `54` = "WV", `55` = "WI",
          `56` = "WY"))),
  male = if_else(!!sym(mapping[[as.character(year)]]$gender)==1, 1, 0),
  
  race = factor(recode(!!sym(mapping[[as.character(year)]]$race),
                   `1`= "White", `2`="Black",
                   `3`="Hispanic", `4`="Asian", 
                   `5`="Native American",
                   .default = "Other"), 
                   levels = c("White", "Black", "Hispanic",
                                                   "Asian", "Native American", "Other")),
  educ = factor(recode(!!sym(mapping[[as.character(year)]]$educ), 
                          `1`= "No HS",
                          `2` = "HS",
                          `3` = "Some College",
                          `4` = "2 Year",
                          `5` = "4 Year",
                          `6` = "Postgrad"
                          ), levels = c("No HS", "HS", "Some College",
                                                        "2 Year", "4 Year", "Postgrad")),
  marriage = factor(recode(!!sym(mapping[[as.character(year)]]$marstat),
                       `1` = "Married",
                       `2` = "Separated",
                       `3` = "Divorced",
                       `4`= "Widowed",
                       `5` = "Single",
                       `6` = "Domestic Partner"
                       ), levels = c("Single", "Married", "Separated",
                                                  "Divorced", "Widowed", "Domestic Partner")),
  
  
  
 ideology = factor(recode(!!sym(mapping[[as.character(year)]]$ideo5),
                             `1` = "Very Liberal",
                             `2`= "Liberal", 
                             `3` = "Moderate", 
                             `4` = "Conservative",
                            `5` = "Very Conservative", 
                             .default = "Unsure"), 
                      levels = c("Unsure", "Moderate", "Liberal",
                                                        "Conservative", "Very Liberal",
                                                        "Very Conservative")),
  votereg = if_else(!!sym(mapping[[as.character(year)]]$votereg)==1, 1,0),
 
  union_self = if_else(!!sym(mapping[[as.character(year)]]$union) %in% c(1,2), 1,0),
  union_hh = if_else(!!sym(mapping[[as.character(year)]]$unionhh) %in% c(1,2), 1,0),
  union = if_else(union_self==1|union_hh==1, 1,0),
 
  employ = factor(recode(!!sym(mapping[[as.character(year)]]$employ),
                            `1` = "Full",
                            `2` = "Part",
                            `3` = "Furlough",
                            `4` = "Unemployed",
                            `5` = "Retired",
                            `6` = "Disabled",
                            `7` = "Homemaker",
                            `8` = "Student",
                            .default = "Not in Labor Force"), levels = c(
                              "Not in Labor Force", "Full", "Part", "Furlough",
                              "Unemployed", "Retired", "Disabled", "Homemaker",
                              "Student")),
 
  party = factor(recode(!!sym(mapping[[as.character(year)]]$pid7),
                            `1` = "Strong D",
                            `2` = "D",
                            `3`= "Lean D",
                            `4` = "Indep",
                            `5` = "Lean R",
                            `6` = "R",
                            `7`= "Strong R",
                            .default = "Unsure"), levels = c("Unsure",
                     "Indep", "Lean R", "Lean D", "R", "D", "Strong R", 
                     "Strong D")),
  donate = if_else(!!sym(mapping[[as.character(year)]]$donate)==1 , 1 , 0),
  
  parent = case_when(
    length(unique((!!sym(mapping[[as.character(year)]]$child)) )) > 4 ~ 
      if_else(!!sym(mapping[[as.character(year)]]$child)>0, 1, 0),
    TRUE ~ if_else(!!sym(mapping[[as.character(year)]]$child)==1, 1, 0)),
  
 recognition = case_when(
   year %in% c(2006, 2007) ~if_else(!!sym(mapping[[as.character(year)]]$know_h1) == 1, 1, 0)+
                                if_else(!!sym(mapping[[as.character(year)]]$know_h2) == 1, 1,0),
                                
   TRUE~
     if_else(!!sym(mapping[[as.character(year)]]$gov_name) > 1 & 
             !!sym(mapping[[as.character(year)]]$gov_name) < 5, 1, 0)+
         if_else(!!sym(mapping[[as.character(year)]]$sen1_name) > 1 & 
                   !!sym(mapping[[as.character(year)]]$sen1_name) < 5, 1, 0)+
         if_else(!!sym(mapping[[as.character(year)]]$sen2_name) > 1 & 
               !!sym(mapping[[as.character(year)]]$sen2_name) < 5, 1, 0)+
     if_else(!!sym(mapping[[as.character(year)]]$house_name) > 1 & 
              !!sym(mapping[[as.character(year)]]$house_name) < 5, 1, 0)),
 
  pol_activites = case_when(
    year %in% c(2006,2007)~ 
      if_else(any(c(!!sym(mapping[[as.character(year)]]$group_1),
                    !!sym(mapping[[as.character(year)]]$group_2), !!sym(mapping[[as.character(year)]]$group_3),
                   !!sym(mapping[[as.character(year)]]$group_4), !!sym(mapping[[as.character(year)]]$group_5),
                   !!sym(mapping[[as.character(year)]]$group_6), !!sym(mapping[[as.character(year)]]$group_7),
                   !!sym(mapping[[as.character(year)]]$group_8), !!sym(mapping[[as.character(year)]]$group_9),
                   !!sym(mapping[[as.character(year)]]$group_10), !!sym(mapping[[as.character(year)]]$group_11),
                   !!sym(mapping[[as.character(year)]]$group_12))) == 1, 1, 0),
    year %in% c(2019,2017) ~
      if_else(!!sym(mapping[[as.character(year)]]$protest) ==1, 1 , 0)+
          if_else(!!sym(mapping[[as.character(year)]]$work_pol) ==1, 1 , 0),
    year %in% c(2020,2022, 2018) ~
      if_else(!!sym(mapping[[as.character(year)]]$meeting) ==1, 1 , 0)+
      if_else(!!sym(mapping[[as.character(year)]]$sign) ==1, 1 , 0)+
      if_else(!!sym(mapping[[as.character(year)]]$protest) ==1, 1 , 0)+
      if_else(!!sym(mapping[[as.character(year)]]$work_pol) ==1, 1 , 0),
    TRUE~ 
      if_else(!!sym(mapping[[as.character(year)]]$meeting) ==1, 1 , 0)+
      if_else(!!sym(mapping[[as.character(year)]]$sign) ==1, 1 , 0)+
      if_else(!!sym(mapping[[as.character(year)]]$work_pol) ==1, 1 , 0)),
    
  contacted = case_when(
    year==2008 ~ 0,
  TRUE~ if_else(!!sym(mapping[[as.character(year)]]$contact) ==1, 1,0)),
 
  ran_for_office = case_when(
    year %in% c(2010,2012,2014,2016,2018,2020,2022) ~ 
      if_else(!!sym(mapping[[as.character(year)]]$run)==1, 1,0),
    TRUE ~ 0),
  
  age = year - !!sym(mapping[[as.character(year)]]$birthyr),
  
  immigrant = factor(recode(!!sym(mapping[[as.character(year)]]$immstat),
                               .default = "Born Citizen",
                               `1` = "Immigrant Citizen",
                               `2` = "Immigrant Noncitizen"),
                        levels = c("Born Citizen", "Immigrant Citizen",
                                   "Immigrant Noncitizen")),
 mil = if_else(!!sym(mapping[[as.character(year)]]$mil_5)==2, 1,0),
 

  vote_prior = case_when(
  !(year %in% c(2014,2022)) ~
    if_else(!!sym(mapping[[as.character(year)]]$vote_prior) == 1, 1,0),
  year==2014~ if_else(!!sym(mapping[[as.character(year)]]$vote_prior) == 4, 1,0),
  year==2022~ if_else(!!sym(mapping[[as.character(year)]]$vote_prior) == 6, 0, 1),
  TRUE~ NA),
 
  vote_intent = case_when(
    year %in% c(2017,2019,2006,2007)~ 
      if_else(!!sym(mapping[[as.character(year)]]$vote_intent) == 1 , 1,0),
    year==2016 ~ if_else(!!sym(mapping[[as.character(year)]]$vote_intent) == 1|
                           !!sym(mapping[[as.character(year)]]$vote_intent) == 2|
                           !!sym(mapping[[as.character(year)]]$vote_intent) == 3, 1, 0),
    TRUE ~ if_else(!!sym(mapping[[as.character(year)]]$vote_intent) == 1|
            !!sym(mapping[[as.character(year)]]$vote_intent) == 2|
            !!sym(mapping[[as.character(year)]]$vote_intent) == 3|
            !!sym(mapping[[as.character(year)]]$vote_intent) == 4, 1, 0)),

  
  investor = case_when(
    year==2008 ~ NA,
    TRUE ~ if_else(!!sym(mapping[[as.character(year)]]$investor) == 1, 1,0)),

  pol_interest = factor(recode(!!sym(mapping[[as.character(year)]]$int),
                           `1` = "Most",
                           `2` = "Some",
                           `3` = "Little",
                           .default="None"), levels = c("None", "Little",
                                                      "Some", "Most")),
  
    
  religion = case_when(
    year %in% c(2006, 2007) ~
      factor(recode(!!sym(mapping[[as.character(year)]]$relig),
                     `1` = "Protestant",
                     `2` = "Catholic",
                     `3` = "Jewish",
                     `4` = "Muslim",
                     `5` = "None",
                     .default = "Other"),
              levels = c("Nothing", "Protestant", "Catholic", "Mormon", "Orthodox",
                         "Jewish", "Muslim", "Buddhist", "Hindu", "Athiest", "Agnostic",
                         "Other")),
    TRUE~
      factor(recode(!!sym(mapping[[as.character(year)]]$relig),
                       `1` = "Protestant",
                       `2` = "Catholic",
                       `3` = "Mormon",
                       `4` = "Orthodox",
                       `5` = "Jewish",
                       `6` = "Muslim",
                       `7` = "Buddhist",
                       `8` = "Hindu",
                       `9` = "Athiest",
                       `10` = "Agnostic",
                       `11` = "Nothing",
                       .default = "Other"),
                levels = c("Nothing", "Protestant", "Catholic", "Mormon", "Orthodox",
                         "Jewish", "Muslim", "Buddhist", "Hindu", "Athiest", "Agnostic",
                         "Other"))),
  
  relig_imp = case_when(
    year %in% c(2006,2007)~
      factor(recode(!!sym(mapping[[as.character(year)]]$religimp), 
                     '1' = "Very",
                     `2` = "None",
                    .default = "None"),
              levels = c("None", "Little", "Somewhat", "Very")),
    
    TRUE~ 
      factor(recode(!!sym(mapping[[as.character(year)]]$religimp),
                               `1` = "Very",
                               `2`= "Somewhat",
                               `3` = "Little",
                               .default = "None"),
                        levels = c("None", "Little", "Somewhat", "Very"))),
  
    
  own_home = if_else(!!sym(mapping[[as.character(year)]]$home) == 1, 1, 0),
  
  news_activites = case_when(
    year %in% c(2006,2007)~
      if_else(!!sym(mapping[[as.character(year)]]$news_watch)==4, 1,0),
    year %in% c(2012,2017) ~ NA,
    year %in% c(2010,2014,2008)~if_else(!!sym(mapping[[as.character(year)]]$blog)==1, 1,0)+
                                    if_else(!!sym(mapping[[as.character(year)]]$news)==1, 1,0)+
                                    if_else(!!sym(mapping[[as.character(year)]]$paper)==1, 1,0)+
                                    if_else(!!sym(mapping[[as.character(year)]]$radio)==1, 1,0),
    TRUE ~ if_else(!!sym(mapping[[as.character(year)]]$blog)==1, 1,0)+
               if_else(!!sym(mapping[[as.character(year)]]$news)==1, 1,0)+
               if_else(!!sym(mapping[[as.character(year)]]$paper)==1, 1,0)+
               if_else(!!sym(mapping[[as.character(year)]]$radio)==1, 1,0)+
               if_else(!!sym(mapping[[as.character(year)]]$social_media)==1, 1,0)),
 
  income = case_when(
    year %in% c(2018,2022,2020,2016,2014,2012,2017,2019)~
      factor(recode(!!sym(mapping[[as.character(year)]]$inc),
                     `1` = "<10k",
                     `2` = "10k-20k",
                     `3` = "20k-30k",
                     `4` = "30k-40k",
                     `5` = "40k-50k",
                     `6` = "50k-60k",
                     `7` = "60k-70k",
                     `8` = "70k-80k",
                     `9` = "80k-90k",
                     `10` = "100k-120k",
                     `11` = "120k-150k",
                     `12` = "150k-200k",
                     `13` = "200k-250k",
                     `14` = "250k-350k",
                     `15` = "350k-500k",
                     `16` = ">500k"),
              levels = c("<10k","10k-20k","20k-30k","30k-40k", "40k-50k", "50k-60k", "60k-70k", 
                         "70k-80k", "80k-90k", "100k-120k", "120k-150k", ">150k", "150k-200k", 
                         "200k-250k", "250k-350k", "350k-500k",">500k")),
    TRUE~
      factor(recode(!!sym(mapping[[as.character(year)]]$inc),
                     `1` = "<10k",
                     `2` = "10k-20k",
                     `3` = "10k-20k",
                     `4` = "20k-30k",
                     `5` = "20k-30k",
                     `6` = "30k-40k",
                     `7` = "40k-50k",
                     `8` = "50k-60k",
                     `9` = "60k-70k",
                     `10` = "70k-80k",
                     `11` = "80k-100k",
                     `12` = "100k-120k",
                     `13` = "120k-150k",
                     `14` = ">150k"),
              levels = c("<10k","10k-20k","20k-30k","30k-40k", "40k-50k", "50k-60k", "60k-70k", 
                         "70k-80k", "80k-90k", "100k-120k", "120k-150k", ">150k", "150k-200k", 
                         "200k-250k", "250k-350k", "350k-500k",">500k")))) %>% 
   select(year, state, male, race, educ, marriage, ideology, votereg, union,
          employ, party, parent, recognition, pol_activites,
           contacted, ran_for_office, age, immigrant, mil, vote_prior,
           vote_intent, investor, pol_interest, religion, relig_imp, own_home, news_activites,
           income, donate)
}



for(year in years) {
  all[[as.character(year)]] <- all[[as.character(year)]] %>%
      mutate(across(everything(), ~ zap_labels(.)))
  all[[as.character(year)]] <- process(all[[as.character(year)]], year, mapping)
}



cleaned_data <- tibble(bind_rows(all))
levels(cleaned_data$state) <- droplevels(cleaned_data$state)
cleaned_data <- cleaned_data %>% rename(pol_activities = pol_activites,
                                news_activities = news_activites)


     




write_rds(cleaned_data, file ="data/cleaned_data.rds")
write_csv(cleaned_data, file ="data/cleaned_data.csv")
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
         
         
         
      
