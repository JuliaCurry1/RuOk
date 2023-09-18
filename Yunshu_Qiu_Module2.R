#load tidyverse and features packages
library(tidyverse)
library(features)

#load data as tibbles
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

#left join of speed to pseed
pseed2 <- pseed %>% 
  left_join(speeds, by = c("speed" = "vol")) %>% 
  print()

#left join of fish in pssed.bl to pseed2
pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")%>%
  print()

#Adding specific speed to pseed2
pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)%>%
  print()

#The custome function for finding peaks
find.peaks <- function(x,y,mult=100){ #define the functions parameter/inputs:x,y, and how much we won't to multiple y by (remember the rounding issue)
  f <- fget(features(x = x,y=y*mult))[2:3]%>% #store results in `f` and compute the features for the x-y relationship, wrap in in fget to retrieve the important features, subset the results to take the 2nd and 3rd and  items, the critical points and curvature, then pass it to a tibble
    as_tibble()%>% # pass in through a filter that returns curvatures <0
    filter(curvature<0)%>% #add a column that rounds the critical point to an integer that represents the frame
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) # return the peaks from tibble
}

#Finding peaks for all data in pseed2
pseed.max <- pseed2%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  filter(peak==T) #new filter

#Plotting amplitude vs. speed 
pseed.max%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point()+geom_smooth(method="lm")

#Using anova to test if the relationship is statistically significant
amp.aov <-  aov(amp.bl~bl.s,pseed.max)
summary(amp.aov)

#Plot the mean max
pseed.max %>%
  group_by(fish, bl.s) %>%
  summarize(mean.max=mean(amp.bl)) %>%
  ggplot(aes(x=bl.s,y=mean.max,col=fish))+geom_point()+geom_smooth(method="lm")

#pivot wider to create pseed.wide tibble
pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 

#2 Creating the cutom function to 
find.se <- function(data)
  return (sd(data) / sqrt(length(data)))

#Question 3
#Creating the pseed.sum.max tibble displaying the max amp.sum
pseed.filter <- pseed.wide %>% 
  group_by(fish,bl.s) %>% 
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T) #new filter

pseed.filter%>%
  ggplot(aes(x=bl.s,y=amp.sum))+geom_point()+geom_smooth(method="lm")

pseed.filter %>%
  group_by(fish, bl.s) %>%
  summarize(mean.max=mean(amp.sum)) %>%
  ggplot(aes(x=bl.s,y=mean.max,col=fish))+geom_point()+geom_smooth(method="lm")

#Question 4
pseed.filter <- pseed.filter %>% 
  group_by(bl.s) %>% 
  mutate(n = length(unique(amp.sum))) %>% 
  mutate(SD = sd(unique(amp.sum))) %>% 
  mutate(amp.sum.se = find.se(amp.sum))

pseed.sum.max <- pseed.filter %>% 
  as_tibble() %>% 
  group_by(fish, bl.s, amp.sum.se) %>% 
  summarize(amp.sum.mean = mean(amp.sum))

#Question 5
#Generating graph with error bar
pseed.sum.max %>% 
  group_by(fish, bl.s) %>% 
  ggplot(aes(x=bl.s,y=amp.sum.mean,col=fish))+geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), width=.1)+geom_point()+geom_smooth(method="lm")

#Question 6
#Merging the two tibble and generating a plot of met vs. amp.sum
pseed.met.rate <- read_csv("pseed.met.rate.csv")
pseed.sum.max.met <- pseed.filter %>% 
  as_tibble() %>% 
  group_by(fish, bl.s, date, amp.sum.se) %>% 
  summarize(amp.sum.mean = mean(amp.sum)) %>% 
  left_join(pseed.met.rate, by = c("date" = "date", "fish" = "fish"))

pseed.sum.max.met %>% 
  ggplot(aes(x = met.rate, y = amp.sum.mean, col = fish)) + geom_point()








  
  
  
  
  








