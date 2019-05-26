#Step 0 loading librarys
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(stringr)
library(gganimate)
library(hrbrthemes)

maxrank <- 10

#Step 1 load the data
fte_data <- read_delim("input/cleaneddata/fte_cleaned.csv", ";",locale = locale(decimal_mark = ",", grouping_mark = ".", asciify = TRUE), trim_ws = TRUE)
fte_data <- fte_data %>% mutate(fte_count=as.numeric(fte_count))
fte_data <- fte_data %>% mutate(times=year+(quarter/5))
summary(fte_data)

#Add some acronyms used for binding old years to current year (not exact, 100% fte's is assumed to go over)
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

#make a graph for each organisation depicting the change of fte_count by salaryscale over the years
inst_list <- unique(fte_data$Afk)
for (inst in inst_list) {
  yearcounts <- fte_data %>% 
  filter(quarter==4) %>% filter(Afk==inst) %>%
  group_by(year,salaryscale) %>% summarise(ftes=sum(fte_count))
  p<- ggplot(yearcounts,aes(x=year,y=salaryscale, size=ftes))+geom_point()+ ggtitle(label=paste0(inst," salary scale over the years")) + theme_ipsum()
  ggsave(paste0("output/afk/",inst,".jpg"),plot=last_plot(),device="jpeg")
}

#make a graph for each function depicting change in fte_count by function over the years
fun_list <- unique(fte_data$fgrfunctie)
n<-length(fun_list)
for (f in fun_list) {
  #get number in list
  x<-match(f,fun_list)
  graphics.off()
  yearcounts <- fte_data %>% 
    filter(quarter==4) %>% filter(fgrfunctie==f) %>%
    group_by(year,salaryscale,Afk) %>% summarise(ftes=sum(fte_count))
  p<- ggplot(yearcounts,aes(x=year,y=salaryscale, size=ftes))+geom_point()+ ggtitle(label=paste0(f," #fte")) + theme_ipsum() +
    ylim(0,19)
  ggsave(paste0("output/funct/",x,".jpg"),plot=last_plot(),device="jpeg")
}
