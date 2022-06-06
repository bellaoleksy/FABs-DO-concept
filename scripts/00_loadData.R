if(!require(here)){install.packages("here")};library(here)
if(!require(tidyverse)){install.packages("tidyverse")};library(tidyverse)
if(!require(lubridate)){install.packages("lubridate")};library(lubridate)
if(!require(ggthemes)){install.packages("ggthemes")};library(ggthemes)
if(!require(viridis)){install.packages("viridis")};library(viridis)
if(!require(rLakeAnalyzer)){install.packages("rLakeAnalyzer")};library(rLakeAnalyzer)
# install.packages("remotes")
remotes::install_github("MilesMcBain/breakerofchains")

# Sky littoral sites
SB1<-read_csv(here("data/SB1_2016.csv")) %>%
  mutate(dateTime=mdy_hm(dateTime))
SB2<-read_csv(here("data/SB2_2016.csv")) %>%
  mutate(dateTime=mdy_hm(dateTime))
SB3<-read_csv(here("data/SB3_2016.csv")) %>%
  mutate(dateTime=mdy_hm(dateTime))
SB4<-read_csv(here("data/SB4_2016.csv")) %>%
  mutate(dateTime=mdy_hm(dateTime))
SB5<-read_csv(here("data/SB5_2016.csv")) %>%
  mutate(dateTime=mdy_hm(dateTime))

# Sky buoy temperature
sky_buoy <- read.table(here("data/sky_2016_tempProfile.txt"), sep=",", header=TRUE) %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  filter(dateTime > "2016-05-31" & dateTime <= "2016-10-30") %>% #ice off and on dates
  rename(wtr_6.5=wtr_7.0)
#Note that 0.5 and 6.5 are MiniDOT observations which are more frequent than the others
#All other depths were recorded with iButtons, same as the littoral zone sites
#sky_2016_sensorTemp_6.5m.txt is the temperature only at 6.5m; MiniDOT
#sky_2016_sensorTemp.txt is the temperature only at 0.5m; MiniDOT

# Loch littoral sites
LB3<-read_csv(here("data/LB3_2016.csv")) %>%
  mutate(dateTime=mdy_hm(dateTime))
LB4<-read_csv(here("data/LB4_2016.csv")) %>%
  mutate(dateTime=mdy_hm(dateTime))
LB5<-read_csv(here("data/LB5_2016.csv")) %>%
  mutate(dateTime=mdy_hm(dateTime))
LB6<-read_csv(here("data/LB6_2016.csv")) %>%
  mutate(dateTime=mdy_hm(dateTime))
LI<-read_csv(here("data/LochInlet_2016.csv")) %>%
  mutate(dateTime=mdy_hm(dateTime))


# Water column heat map

# We only have > 2 depths for temperature through 2016-08-04
sky_buoy_trim <- sky_buoy %>%
  filter(dateTime <= "2016-08-04")
wtr.heat.map(sky_buoy_trim)

#All temps,line graph
sky_buoy %>%
  pivot_longer(-dateTime, names_to="depth") %>%
  mutate(
    season = case_when(
      dateTime > "2016-09-01" ~ "fall",
      TRUE                      ~ "summer"
    )
  ) %>%
  ggplot(aes(x=dateTime,y=value,color=depth))+
  geom_point(alpha=0.5,size=0.2)+
  geom_line(alpha=0.5)+
  theme_few()+
  facet_wrap(season~., scales="free")

# Just summer when all sensors were present
sky_buoy %>%
  pivot_longer(-dateTime, names_to="depth") %>%
  filter(dateTime <= "2016-08-04") %>%
  ggplot(aes(x=dateTime,y=value,color=depth))+
  geom_point(alpha=0.5,size=0.2)+
  geom_line(alpha=0.5)+
  theme_few()+
  facet_wrap(depth~., ncol=5)+
  scale_color_viridis(discrete=TRUE)+
  theme(panel.background = element_rect(fill="black"))

# Temperature fluctuations
sky_buoy %>%
  pivot_longer(-dateTime, names_to="depth") %>%
  filter(dateTime <= "2016-08-04") %>%
  mutate(date=date(dateTime))%>%
  group_by(depth,date)%>%
  summarize(min_T=min(value, na.rm=TRUE),
            max_T=max(value, na.rm=TRUE),
            diff_T=max_T-min_T) %>%
  ggplot(aes(x=date,y=diff_T,color=depth))+
  geom_point(alpha=0.5,size=1)+
  geom_line()+
  theme_few()+
  # facet_wrap(depth~., ncol=5)+
  scale_color_viridis(discrete=TRUE)+
  theme(panel.background = element_rect(fill="black"))

# Data vis littoral ----------------------------------------------------------------



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

