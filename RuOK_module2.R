####### PROJECT REPORT
## set working directory and load tidyverse and features
setwd("/Users/elizabethbreitmeyer/Desktop/Experimental Methods in Organismal Bio/RuOK/RuOK")
library(tidyverse)
library(features)

## 1
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

pseed2 <- pseed %>%
  left_join(speeds,by=c("speed"="vol")) %>%
  left_join(pseed.bl,by="fish")%>%
  mutate(bl.s=cm.s/bl)

find.peaks <- function(x,y,mult=100){
  f <- fget(features(x = x,y=y*mult))[2:3]%>%
    as_tibble()%>%
    filter(curvature<0)%>%
    mutate(peaks=round(crit.pts,0))
  return(f$peaks)
}

pseed.wide <- pseed2%>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl)%>%
  mutate(amp.sum=L+R)%>%
  print()

## 2-4: creating a custom function
amp.sum.mean <- pseed.wide%>%
  group_by(fish,bl.s)%>%
  summarize(amp.sum.mean=mean(amp.sum,na.rm=TRUE))%>%
  print()

amp.sum.se <- pseed.wide%>%
  group_by(fish,bl.s)%>%
  summarize(amp.sum.se=sd(amp.sum, na.rm=TRUE)/sqrt(n()))%>%
  print()

pseed.sum.max <- left_join(amp.sum.mean, amp.sum.se, by=c("fish", "bl.s"))
# here is the tibble pseed.sum.max with columns amp.sum.mean and amp.sum.se

## 5: plotting
library(ggplot2)
ggplot(pseed.sum.max, aes(x=bl.s, y=amp.sum.mean, color=fish)) +
  geom_point() +
  geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se, group=fish), width=0.1, position=position_dodge(0.1)) +
  geom_line(position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1)) +
  labs(x = "Swimming Speed", y = "Mean Amp.Sum") +
  theme_minimal()

## 6 and 7
pseed.met.rate <- read_csv("pseed.met.rate.csv")

pseed.sum.max2 <- left_join(pseed.sum.max, pseed.met.rate, by=c("fish", "bl.s"))

ggplot(pseed.sum.max2, aes(x=amp.sum.mean, y=met.rate, color=fish)) +
  geom_point() +
  geom_line(position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1)) +
  theme_minimal()


