library(tidyverse)
library(ggplot2)
library(dplyr)
# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
ncarceration_trends<-read_csv("../source/incarceration-trends/incarceration_trends.csv")
variables<-c(ncarceration_trends, "black_jail_pop","female_adult_jail_pop","male_adult_jail_pop")
new_data<-group_by(ncarceration_trends,county_name)%>%summarise(black=black_jail_pop, female=female_adult_jail_pop,male=male_adult_jail_pop,year=year,county=county_name,state=state)
as.numeric(new_data$black)
avg_black_2018<-filter(ncarceration_trends,year==2018)%>%summarise(mean(black_jail_pop,na.rm=T))
print(avg_black_2018)
avg_fem_2018<-filter(ncarceration_trends,year==2018)%>%summarise(mean(female_adult_jail_pop,na.rm=T))
avg_male_2018<-filter(ncarceration_trends,year==2018)%>%summarise(mean(male_adult_jail_pop,na.rm=T))
print(avg_male_2018)
print(avg_fem_2018)
max_regi_black<-filter(ncarceration_trends,black_jail_pop==max(black_jail_pop,na.rm = T))%>%pull(county_name)
print(max_regi_black)
max_regi_female<-filter(ncarceration_trends,female_adult_jail_pop==max(female_adult_jail_pop,na.rm = T))%>%pull(county_name)
print(max_regi_female)
max_regi_male<-filter(ncarceration_trends,male_adult_jail_pop==max(male_adult_jail_pop,na.rm = T))%>%pull(county_name)
print(max_regi_male)
range_black<-group_by(ncarceration_trends,year)%>%summarise(black_jail_pop=sum(black_jail_pop,na.rm = T))
range(range_black$black_jail_pop)
range_female<-group_by(ncarceration_trends,year)%>%summarise(female_adult_jail_pop=sum(female_adult_jail_pop,na.rm = T))
range(range_female$female_adult_jail_pop)
range_male<-group_by(ncarceration_trends,year)%>%summarise(male_adult_jail_pop=sum(male_adult_jail_pop,na.rm = T))
range(range_male$male_adult_jail_pop)
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population

# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  df<-group_by(ncarceration_trends,year)%>%summarise(total=sum(total_jail_pop,na.rm = T)) %>% select(year,total)
return(df)   
}
get_year_jail_pop()
# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  chart<-ggplot(data=get_year_jail_pop(), aes(x=year, y=total)) +
    geom_bar(stat="identity")+
    ggtitle("Increase of Jail Population in the U.S.")
  return(chart)   
} 
plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
get_jail_pop_by_states<-function(stateA){
  df2<- filter(ncarceration_trends,state == c(stateA)) %>% 
    group_by(year,state)%>%
    summarise(total=total_jail_pop,na.rm = T)
  return(df2)
}

get_jail_pop_by_states(c("WA", "OR", "CA"))
plot_jail_pop_by_states<-function(stateA) {
  chart2<-ggplot(data=get_jail_pop_by_states(stateA), aes(x=year,y=total,color=state))+
  geom_line()+
  ggtitle("Growth of Prison Population by State")
  return(chart2)   
} 
plot_jail_pop_by_states(c("WA", "OR", "CA"))

# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
female_jail_ratio<-function(){
  df3<-mutate(ncarceration_trends, ratio=female_jail_pop/total_jail_pop,na.rm = TRUE)%>% filter(state=="WA")
  return(df3)
}

plot_female_jail_ratio<-function(){
  chart3<- ggplot(data= female_jail_ratio(), mapping = aes(county_name,ratio))+
  geom_point() +
  coord_cartesian()+
  ggtitle("Ratio of Female Jail Population to All Jail Population in Washington")
  return(chart3)
  }
plot_female_jail_ratio()
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
require(maps)
require(viridis)
female_jail_ratio_state<-function(){
  df4<- ncarceration_trends%>%
    select(female_jail_pop,total_jail_pop,state) %>%
    group_by(state)%>%
    summarise(ratio=mean(female_jail_pop,na.rm = TRUE) /mean(total_jail_pop,na.rm = TRUE))
  return(df4)
}

library(stringr)
states_map <- map_data("state")
colnames(states_map)[5]<-"state"
states_map$state<-str_to_title(states_map$state)
states_map$state<-state.abb[match(states_map$state,state.name)]
#state.name[match(ncarceration_trends$state,state.abb)]

ratio_map<-left_join(states_map, female_jail_ratio_state(), by = "state",)
plot_female_jail_ratio_state<-function(){
  chart4<-ggplot(ratio_map,aes(long, lat, group = group))+
    geom_polygon(aes(fill = ratio), color = "white")+
    scale_fill_viridis_c(option = "C")+
    ggtitle("Ratio of Female Jail Population to All Jail Population")
  return(chart4)
}
plot_female_jail_ratio_state()
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


