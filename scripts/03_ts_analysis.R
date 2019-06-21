#anomaly detection
#Install the devtools package then github packages
#Plotting data
library(ggplot2)
library(tidyverse)
#Installing anomalize
library(anomalize)

#Apply anomaly detection and plot the results
d <- fte_data
d <- d %>% group_by(Afk,times) %>% summarise(ftec=sum(fte_count))
d <- d %>% select(times,Afk,ftec)
p <- ggplot(d,aes(x=times,y=ftec, color=Afk))+geom_line()
ggsave(p,file=paste0("output/afk/ano/","fte_by_year",".jpg"))

d <- fte_data
d <- d %>% group_by(times) %>% summarise(ftec=sum(fte_count))
d <- d %>% select(times,ftec)
d_ts = d %>% as.tibble()
d_ts %>%
  time_decompose(ftec, method = "stl", frequency = "auto", trend = "auto") %>% 
  anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.1) %>%
  plot_anomaly_decomposition()
ggsave(file=paste0("output/afk/ano/","anomaly_overall",".jpg"))

inst <- unique(fte_data$Afk)
for (i in inst) {
  d <- fte_data %>% filter(Afk==i) %>% mutate(times=convert_quarter_dates(tijd))
  d <- d %>% group_by(Afk,times) %>% summarise(ftec=sum(fte_count))
  p<- ggplot(d, aes(x=times, y=ftec, color=Afk)) + geom_line()
  Sys.sleep(0)
  ggsave(p,file=paste0("output/afk/ano/",i,".jpg"))
  d <- d %>% group_by(times) %>% summarise(ftec=sum(ftec))
  d <- d %>% select(times,ftec)
  
  d_ts <- d %>% as.tibble()
  
  d_ts <- d_ts %>%
    time_decompose(ftec, method = "stl", frequency = "auto", trend = "auto") %>% 
    anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.1)
  p <- d_ts %>% plot_anomaly_decomposition()
  ggsave(p,file=paste0("output/afk/ano/",i,"_ts.jpg"))
  Sys.sleep(0)
  print(p)
}

###analysis on functions
#Apply anomaly detection and plot the results
d <- fte_data
d <- d %>% group_by(fgrfunctie,times) %>% summarise(ftec=sum(fte_count))
d <- d %>% select(times,fgrfunctie,ftec)
p <- ggplot(d,aes(x=times,y=ftec, color=fgrfunctie))+geom_line()
ggsave(p,file=paste0("output/fun/ano/","fte_by_year",".jpg"))

d <- fte_data
d <- d %>% group_by(times) %>% summarise(ftec=sum(fte_count))
d <- d %>% select(times,ftec)
d_ts = d %>% as.tibble()
d_ts %>%
  time_decompose(ftec, method = "stl", frequency = "auto", trend = "auto") %>% 
  anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.1) %>%
  plot_anomaly_decomposition()
ggsave(file=paste0("output/fun/ano/","anomaly_overall",".jpg"))

fun <- unique(fte_data$fgrfunctie)
for (f in fun) {
  d <- fte_data %>% filter(fgrfunctie==f) %>% group_by(times) %>% summarise(ftec=sum(fte_count))
  p<- ggplot(d, aes(x=times, y=ftec)) + geom_line()
  Sys.sleep(0)
  ggsave(p,file=paste0("output/fun/ano/",f,".jpg"))
  
  d <- d %>% group_by(times) %>% summarise(ftec=sum(ftec))
  d <- d %>% select(times,ftec)
  
  d_ts <- d %>% as.tibble()
  
  d_ts <- d_ts %>%
    time_decompose(ftec, method = "stl", frequency = "auto", trend = "auto") %>% 
    anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.1)
  p <- d_ts %>% plot_anomaly_decomposition()
  ggsave(p,file=paste0("output/fun/ano/",f,"_ts.jpg"))
  Sys.sleep(0)
  print(p)
}
