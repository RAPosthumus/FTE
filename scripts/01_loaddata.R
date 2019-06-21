## Step 0: include libs
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

## Step 1: declare functions
#look for the differences
datadif <- function(d1,d2,...) {
  d1p <- do.call("paste",d1)
  d2p <- do.call("paste",d2)
  d1[! d1p %in% d2p,]
}

# a function to bring a quarterdate in the form a timestamp
convert_quarter_dates <- function(myinput="20191",monthday=15) {
  year <- substr(myinput,1,4)
  q <- substr(myinput,5,5)
  m <- 3*as.numeric(q)-1
  date <- as.Date(paste0(year,sprintf("%02d",m),monthday),'%Y%m%d')
  return(date)
}
#Step 2: define constants
debug <- FALSE

#Step 3: load data
#use fte2 dataset for more detailed info
datafilename="fte2.csv"
orig_dataset <- read_delim(paste0("input/rawdata/",datafilename), delim= ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = ".", asciify = TRUE), trim_ws = TRUE)
#save a copy to work with
fte_data <- read_delim(paste0("input/rawdata/",datafilename),delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",asciify = TRUE), trim_ws = TRUE)

#Step 4: clean data

#Step 4a: general cleaning
if (!debug) { fte_data <- fte_data %>% select(-c(X1)) }
#convert fgrfunctie to general unicode
fte_data <- fte_data %>% mutate(fgrfunctie = stringi::stri_trans_general(fgrfunctie,"latin-ascii"))
#reshape to long format with year/quarter colums
fte_data <- fte_data %>% gather(key=yearquarter,value = fte_count,  starts_with('2'))
#split from the colums the quarter part
fte_data <- fte_data %>% mutate(year=str_sub(yearquarter,1,4), quarter=str_sub(yearquarter,5,6) )
#drop the yarquarter column
fte_data <- fte_data %>% select(-yearquarter)

#Step 4b: split of the salary scale
#for checking get the number_of_rows of the original data set
nr_fte_data <- nrow(fte_data)
#4b. case 1: fgrfunctie == geen FGR -> salary sdale = 0
fte_data_no_fgr <- fte_data %>% filter(fgrfunctie=="geen FGR") %>% mutate(salaryscale=0)
nr_treated <- nrow(fte_data_no_fgr)
#4b. case 2: fgr functie contains "schaal"
fte_data_schaal <- fte_data %>% filter(str_detect(str_to_upper(fgrfunctie),'SCHAAL'))   
nr_treated = nr_treated + nrow(fte_data_schaal)
fte_data_schaal <- fte_data_schaal %>% mutate(salaryscale=str_sub(fgrfunctie,-2) ) %>% 
                                       mutate(fgrfunctie=str_sub(fgrfunctie,1,str_length(fgrfunctie)-10))
#4b. case 3 fgrfunctie ends with SXX
#regex for filtering: 
regstr <- ".*S[0-9].$"
fte_data_ss <- fte_data %>% filter(str_detect(fgrfunctie,regstr))
nr_treated <-nr_treated + nrow(fte_data_ss)
fte_data_ss <- fte_data_ss %>% mutate(salaryscale=str_sub(fgrfunctie,-2)) %>%
                               mutate(fgrfunctie=str_sub(fgrfunctie,1,str_length(fgrfunctie)-3))
#4b. case 4 fgrfunctie ends with s. XX
regstr <- "s. [0-9][0-9]"
fte_data_s <- fte_data %>% filter(str_detect(fgrfunctie,regstr))
nr_treated <-nr_treated + nrow(fte_data_s)
fte_data_s <- fte_data_s %>% mutate(salaryscale=str_sub(fgrfunctie,-2)) %>%
                             mutate(fgrfunctie=str_sub(fgrfunctie,1,str_length(fgrfunctie)-5))
#4b. case 5 topmanagers end with (NXX)
regstr <- ".(N[0-9][1-9]).$"
fte_data_top1 <- fte_data %>% filter(str_detect(fgrfunctie,regstr))
nr_treated <- nr_treated + nrow(fte_data_top1)
fte_data_top1 <- fte_data_top1 %>% mutate(salaryscale=str_sub(fgrfunctie,-3,-2)) %>%
                                   mutate(fgrfunctie=str_sub(fgrfunctie,1,str_length(fgrfunctie)-5))
#4b. case 6 topmanagers end with NXX
regstr <- "N[0-9][1-9]$"
fte_data_top2 <- fte_data %>% filter(str_detect(fgrfunctie,regstr))
nr_treated <- nr_treated + nrow(fte_data_top2)
fte_data_top2 <- fte_data_top2 %>% mutate(salaryscale=str_sub(fgrfunctie,-2,-1)) %>%
                                   mutate(fgrfunctie=str_sub(fgrfunctie,1,str_length(fgrfunctie)-3))
#4c convert salary_scale to numeric (done separately in case you need to debug it)
fte_data_schaal <- fte_data_schaal %>% mutate(salaryscale=as.numeric(salaryscale))
fte_data_no_fgr <- fte_data_no_fgr %>% mutate(salaryscale=as.numeric(salaryscale))
fte_data_s <- fte_data_s %>% mutate(salaryscale=as.numeric(salaryscale))
fte_data_schaal <- fte_data_schaal %>% mutate(salaryscale=as.numeric(salaryscale))
fte_data_ss <- fte_data_ss %>% mutate(salaryscale=as.numeric(salaryscale))
fte_data_top1 <- fte_data_top1 %>% mutate(salaryscale=as.numeric(salaryscale))
fte_data_top2 <- fte_data_top2 %>% mutate(salaryscale=as.numeric(salaryscale))

#4d. test if we have all cases treated
ifelse(nr_fte_data==nr_treated,"All well","Not a correct number of rows, check script")
#4e. code in case you need debug if not all cases are treated
if (debug) {
  nofgr_id  <- fte_data_no_fgr %>% select(X1)
  schaal_id <- fte_data_schaal %>% select(X1)
  ss_id     <- fte_data_ss     %>% select(X1)
  s_id      <- fte_data_s      %>% select(X1)
  top1_id   <- fte_data_top1   %>% select(X1)
  top2_id   <- fte_data_top2   %>% select(X1)
  ids <- bind_rows(nofgr_id,schaal_id,ss_id,s_id,top1_id,top2_id)
  ids_orig <- fte_data %>% select(X1)
  aa <- datadif(ids_orig,ids)
  un_treated <- fte_data %>% filter(X1 %in% aa$X1)
}
#4e. join back to one dataframe
fte_data <- bind_rows(fte_data_no_fgr,fte_data_schaal,fte_data_ss,fte_data_s,fte_data_top1,fte_data_top2)
#4f. if fte_count = NA, set fte_count to 0
fte_data <- fte_data %>% mutate(fte_count = if_else(is.na(fte_count),0,fte_count))
#4g. make the year, quater to numeric
fte_data <- fte_data %>% mutate_at(vars(year,quarter),as.numeric)
#4i add a column year*10+quarter (used in quarter_dates)
fte_data <- fte_data %>% mutate(tijd=quarter+10*year)
#4i add colum that converts quarter to date
fte_data <- fte_data %>% mutate(times=convert_quarter_dates(tijd)) 
#4j dd some acronyms used for binding old years to current year (not exact, 100% fte's is assumed to go over)
fte_data <- fte_data %>% mutate(Afk=case_when(
  .$Instelling == "Algemene Zaken" ~ "AZ",
  .$Instelling == "Binnenlandse Zaken en Koninkrijksrelaties" ~ "BZK",
  .$Instelling == "Buitenlandse Zaken" ~ "BZ",
  .$Instelling == "Economische Zaken en Klimaat" ~ "EZK",
  .$Instelling == "Economische Zaken" ~ "EZK",
  .$Instelling == "Financien" ~ "FIN",
  .$Instelling == "Hoge Colleges van Staat" ~ "HCS",
  .$Instelling == "Infrastructuur en Milieu" ~ "IenW",
  .$Instelling == "Infrastructuur en Waterstaat" ~ "IenW",
  .$Instelling == "Justitie en Veiligheid" ~ "JenV",
  .$Instelling == "Landbouw, Natuur en Voedselkwaliteit" ~ "EZK",
  .$Instelling == "Onderwijs, Cultuur en Wetenschap" ~ "OCW",
  .$Instelling == "Sociale Zaken en Werkgelegenheid" ~ "SZW",
  .$Instelling == "Veiligheid en Justitie" ~ "JenV",
  .$Instelling == "Volksgezondheid, Welzijn en Sport" ~ "VWS"
))
#4h. give an overview of the converted data
summary(fte_data)

#Step 5: save cleaned data to disk
# write detailed info set
write_delim(fte_data,path=paste0("input/cleaneddata/",datafilename), delim=";", col_names = TRUE)

#Step 6: cleanup variables
rm(aa,fte_data_no_fgr,fte_data_s,fte_data_schaal,fte_data_ss,fte_data_top1,fte_data_top2,un_treated,nr_treated,regstr)
rm(ids,ids_orig,nofgr_id,s_id,schaal_id,ss_id,top1_id,top2_id)

