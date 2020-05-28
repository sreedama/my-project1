library(tidyverse)
library(tidyr)
library(dslabs)
library(dplyr)
library(ggplot2)
# ---- Read csv file and format it
wine <- read.csv('wine.csv',stringsAsFactors = FALSE,encoding = 'UTF-8')
View(wine)
# remove not necessary columns
wine <- wine[,-c(1,3)]
View(wine)
#count group by columns
wine %>% group_by(country) %>% summarize(count=n()) %>% arrange(desc(count))
#listout top 10 countries
selected_countries <- wine %>% group_by(country) %>% summarize(count=n()) %>% arrange(desc(count)) %>% top_n(10) %>% select(country)
class(selected_countries)
#change it to character set
selected_countries <- as.character(selected_countries$country)
#listout points for top 10 counties
select_point <- wine %>% filter(country %in% selected_countries) %>% select(country,points) %>% arrange(country)
##ggplot points verses price
wine %>% ggplot(aes(points,price))+geom_point()+geom_smooth()
select_point %>%ggplot(aes(x=reorder(country,points,median),y=points))+geom_boxplot(aes(fill=country))+xlab("Country")+ylab("Points")+
  ggtitle("Distribution of top Ten wine producing countries")+theme(plot.title = element_text(hjust=.5))


wine %>% filter(!country%in%selected_countries) %>% group_by(country) %>% summarize(median=median(points)) %>% arrange(desc(median))
top <- wine %>% group_by(country) %>% summarize(median=median(points)) %>% arrange(desc(median))
top <- as.character(top$country)
#top <- intersect(top,selected_countries)
top <- top[1:10]
both <- intersect(top,selected_countries)
not <- setdiff(top,selected_countries)

topwine <- wine %>% group_by(variety) %>% summarize(number=n()) %>%arrange(desc(number)) %>% top_n(10)
topwine <- as.character(topwine$variety)
wine %>% filter(variety%in%topwine) %>% group_by(variety) %>% summarize(median=median(points)) %>% 
  ggplot(aes(reorder(variety,median),median))+geom_col(aes(fill=variety))+xlab("Variety")+ylab("Median point")+scale_x_discrete(labels=abbreviate)
top15p <- wine %>% arrange(desc(points)) %>% filter(points>quantile(points,prob=0.85))
cheapest15p <- wine %>% arrange(price) %>% head(nrow(top15p))
#find the good quality at cheap price 
goodvalue <- intersect(top15p,cheapest15p)

####Wine dataset
wine <- wine %>% mutate(PPratio=points/price)
wine1 <- wine %>% transmute(PPratio=points/price)
aaa <- wine %>% group_by(country) %>% summarize(total=n())
wine[wine$country=='',]
wine$country=ifelse(wine$designation == "Askitikos","Greece",wine$country)
wine$country=ifelse(wine$designation == "Piedra Feliz","Chile",wine$country)
wine$country=ifelse(wine$designation=="Shah","Turkey",wine$country)
wine %>% group_by(country) %>% summarize(total=n())
wine[wine$country=='',]

newwine <- wine %>% group_by(country) %>% summarize(total=n()) %>% arrange(desc(total))

subset1 <- head(wine)
subset2 <- head(newwine)

full <- full_join(subset1,subset2)
View(full)
innner <- inner_join(subset1,subset2)

left <- left_join(subset1,subset2)
right <- right_join(subset1,subset2)
