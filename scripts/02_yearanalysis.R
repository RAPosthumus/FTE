# Step 0 loading librarys
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(stringr)
library(gganimate)
library(hrbrthemes)

maxrank <- 10

# Step 1 load the data
fte_data <- read_delim(paste0("input/cleaneddata/", datafilename), delim = ";", locale = locale(decimal_mark = ",", grouping_mark = ".", asciify = TRUE), trim_ws = TRUE)
fte_data <- fte_data %>% mutate(fte_count = as.numeric(fte_count))
summary(fte_data)

# make a graph for each organisation depicting the change of fte_count by salaryscale over the years
inst_list <- unique(fte_data$Afk)
for (inst in inst_list) {
  yearcounts <- fte_data %>%
    filter(Afk == inst) %>%
    group_by(times, salaryscale) %>%
    summarise(ftec = sum(fte_count))
  p <- ggplot(yearcounts, aes(x = times, y = salaryscale, size = ftec)) + geom_point() + ggtitle(label = paste0(inst, " salary scale over the years by instelling")) + theme_ipsum()
  ggsave(paste0("output/afk/", inst, ".jpg"), plot = last_plot(), device = "jpeg")
}

# make a graph for each function depicting change in fte_count by function over the years
fun_list <- unique(fte_data$fgrfunctie)
n <- length(fun_list)
for (f in fun_list) {
  # get number in list
  x <- match(f, fun_list)
  graphics.off()
  yearcounts <- fte_data %>%
    filter(fgrfunctie == f) %>%
    group_by(times, Afk, salaryscale) %>%
    summarise(ftec = sum(fte_count))
  p <- ggplot(yearcounts, aes(x = times, y = ftec, color = Afk)) + geom_point() + ggtitle(label = paste0(f, " #fte by instelling")) + theme_ipsum() +
    ggsave(paste0("output/fun/", x, ".jpg"), plot = last_plot(), device = "jpeg")
}

# make a graph for each function depicting change in fte_count by function over the years
sal_list <- unique(fte_data$salaryscale)
for (s in sal_list) {
  # get number in list
  yearcounts <- fte_data %>%
    filter(salaryscale == s) %>%
    group_by(times, salaryscale, Afk) %>%
    summarise(ftec = sum(fte_count))
  p <- ggplot(yearcounts, aes(x = times, y = ftec, color = Afk)) + geom_point() + ggtitle(label = paste0(" #fte per scale ", s)) + theme_ipsum()
  ggsave(paste0("output/sal/", s, ".jpg"), plot = last_plot(), device = "jpg")
}

yearcounts <- fte_data %>% filter(quarter == 4, salaryscale != 0)

p <- ggplot(yearcounts, aes(x = salaryscale, y = fte_count)) +
  geom_histogram(bins = 20, stat='sum') +
  facet_grid(Afk~year, scales = "free") +
  ggtitle(label = "#fte per scale") + theme_ipsum()+theme(legend.position="none")
p
         
