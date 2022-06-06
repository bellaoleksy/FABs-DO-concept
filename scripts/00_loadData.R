if(!require(here)){install.packages("here")};library(here)
if(!require(tidyverse)){install.packages("tidyverse")};library(tidyverse)
if(!require(lubridate)){install.packages("lubridate")};library(lubridate)
if(!require(ggthemes)){install.packages("ggthemes")};library(ggthemes)
if(!require(viridis)){install.packages("viridis")};library(viridis)
install.packages("remotes")
remotes::install_github("MilesMcBain/breakerofchains")

# Sky littoral sites
SB1<-read_csv(here("data/raw/SB1_2016.csv")) %>%
  mutate(dateTime=mdy_hm(dateTime))
SB2<-read_csv(here("data/raw/SB2_2016.csv")) %>%
  mutate(dateTime=mdy_hm(dateTime))
SB3<-read_csv(here("data/raw/SB3_2016.csv")) %>%
  mutate(dateTime=mdy_hm(dateTime))
SB4<-read_csv(here("data/raw/SB4_2016.csv")) %>%
  mutate(dateTime=mdy_hm(dateTime))
SB5<-read_csv(here("data/raw/SB5_2016.csv")) %>%
  mutate(dateTime=mdy_hm(dateTime))

# Loch littoral sites
LB3<-read_csv(here("data/raw/LB3_2016.csv")) %>%
  mutate(dateTime=mdy_hm(dateTime))
LB4<-read_csv(here("data/raw/LB4_2016.csv")) %>%
  mutate(dateTime=mdy_hm(dateTime))
LB5<-read_csv(here("data/raw/LB5_2016.csv")) %>%
  mutate(dateTime=mdy_hm(dateTime))
LB6<-read_csv(here("data/raw/LB6_2016.csv")) %>%
  mutate(dateTime=mdy_hm(dateTime))
LI<-read_csv(here("data/raw/LochInlet_2016.csv")) %>%
  mutate(dateTime=mdy_hm(dateTime))
str(SB1)
str(SB2)
str(SB3)
str(SB4)
str(SB5)


# Data vis ----------------------------------------------------------------



#preview raw data
bind_rows(SB1,SB2,SB3,SB4,SB5,
          LB3,LB4,LB5,LB6,LI)%>%
  ggplot(aes(x=dateTime,y=temp_C,color=siteID,shape=lakeID,linetype=lakeID))+
  geom_point(alpha=0.5,size=0.2)+
  geom_line(alpha=0.5)+
  theme_few()+
  facet_wrap(lakeID~., ncol=2)+
  scale_color_viridis(discrete=TRUE)

# how large are the diurnal swings?
bind_rows(SB1,SB2,SB3,SB4,SB5,
          LB3,LB4,LB5,LB6,LI)%>%
  mutate(date=date(dateTime))%>%
  group_by(lakeID,siteID,date)%>%
  summarize(min_T=min(temp_C, na.rm=TRUE),
            max_T=max(temp_C, na.rm=TRUE),
            diff_T=max_T-min_T) %>%
  ggplot(aes(x=date,y=diff_T,color=siteID,shape=lakeID,linetype=lakeID))+
  geom_point(alpha=0.5)+
  geom_line(alpha=0.5)+
  theme_few()+
  facet_wrap(lakeID~., ncol=2)+
  scale_color_viridis(discrete=TRUE)

bind_rows(SB1,SB2,SB3,SB4,SB5,
          LB3,LB4,LB5,LB6,LI)%>%
  mutate(date=date(dateTime))%>%
  group_by(lakeID,siteID,date)%>%
  summarize(min_T=min(temp_C, na.rm=TRUE),
            max_T=max(temp_C, na.rm=TRUE),
            diff_T=max_T-min_T) %>%
  ggplot(aes(x=diff_T, fill=siteID)) +
  geom_histogram(alpha=0.5, position="identity", bins=10)+
  scale_fill_viridis(discrete=TRUE)+
  facet_wrap(siteID~lakeID, nrow=2, ncol=5, scales="free_x")

