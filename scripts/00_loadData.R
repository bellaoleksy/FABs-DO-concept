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
  filter(dateTime >= "2016-06-13" & dateTime <= "2016-10-30") %>% #ice off and on dates
  rename(wtr_6.5=wtr_7.0)
#Note that 0.5 and 6.5 are MiniDOT observations which are more frequent than the others
#All other depths were recorded with iButtons, same as the littoral zone sites
#sky_2016_sensorTemp_6.5m.txt is the temperature only at 6.5m; MiniDOT
#sky_2016_sensorTemp.txt is the temperature only at 0.5m; MiniDOT

sky_buoy_long <- sky_buoy %>%
  pivot_longer(-dateTime, names_to="depth") %>%
  mutate(habitat="pelagic") %>%
  separate(col = depth, into = c("parameter", "depth"), sep = "_")%>%
  mutate(parameter="temperature",
         lakeID="SkyPond")

# Loch buoy temperature
loch_buoy <- read.table(here("data/loch_2016_tempProfile.txt"), sep=",", header=TRUE) %>%
  mutate(dateTime = ymd_hms(as.factor(dateTime)),
         dateTime = force_tz(dateTime, tz="America/Denver"),
         dateTime = with_tz(dateTime, "GMT")) %>%
  filter(dateTime > "2016-05-31" & dateTime <= "2016-10-30") #ice off and on dates
#Note that 0.5 and 4.5 are MiniDOT observations which are more frequent than the others
#All other depths were recorded with iButtons, same as the littoral zone sites
#loch_2016_sensorTemp_4.5m.txt is the temperature only at 4.5; MiniDOT
#loch_2016_sensorTemp_0.5m.txt is the temperature only at 0.5m; MiniDOT

loch_buoy_long <- loch_buoy %>%
  pivot_longer(-dateTime, names_to="depth") %>%
  mutate(habitat="pelagic") %>%
  separate(col = depth, into = c("parameter", "depth"), sep = "_") %>%
  mutate(parameter="temperature",
         lakeID="TheLoch")



# Sky buoy DO
sky_DO_0.5 <- read.table(here("data/sky_2016_DO_0.5m.txt"), sep=",", header=TRUE) %>%
  mutate(dateTime = ymd_hms(dateTime),
         depth = 0.5,
         lakeID = "SkyPond",
         habitat = "pelagic") %>%
  filter(dateTime < "2016-11-02")
sky_DO_6.5 <- read.table(here("data/sky_2016_DO_6.5m.txt"), sep=",", header=TRUE) %>%
  mutate(dateTime = ymd_hms(dateTime),
         depth = 6.5,
         lakeID = "SkyPond",
         habitat = "pelagic") %>%
  filter(dateTime < "2016-11-02")


# Loch buoy DO
loch_DO_0.5 <- read.table(here("data/loch_2016_DO_0.5m.txt"), sep=",", header=TRUE) %>%
  mutate(dateTime = ymd_hms(dateTime),
         depth = 0.5,
         lakeID = "TheLoch",
         habitat = "pelagic") 
loch_DO_4.5 <- read.table(here("data/loch_2016_DO_4.5m.txt"), sep=",", header=TRUE) %>%
  mutate(dateTime = ymd_hms(dateTime),
         depth = 4.5,
         lakeID = "TheLoch",
         habitat = "pelagic") 

#Combine all DO data
DO_master <- bind_rows(sky_DO_0.5,
                       sky_DO_6.5,
                       loch_DO_0.5,
                       loch_DO_4.5) %>%
  rename(value=DO) %>%
  mutate(parameter="DO",
         depth=as.character(as.numeric(depth)))


# Loch littoral sites
LB1<-read_csv(here("data/LochInlet_2016.csv")) %>%
  mutate(dateTime=mdy_hm(dateTime))
LB3<-read_csv(here("data/LB3_2016.csv")) %>%
  mutate(dateTime=mdy_hm(dateTime))
LB4<-read_csv(here("data/LB4_2016.csv")) %>%
  mutate(dateTime=mdy_hm(dateTime))
LB5<-read_csv(here("data/LB5_2016.csv")) %>%
  mutate(dateTime=mdy_hm(dateTime))
LB6<-read_csv(here("data/LB6_2016.csv")) %>%
  mutate(dateTime=mdy_hm(dateTime))



#Master file for all data
sky_littoral <-bind_rows(SB1,SB2,SB3,SB4,SB5) %>%
  mutate(habitat="littoral")
loch_littoral <- bind_rows(LB3,LB4,LB5,LB6,LB1) %>%
  mutate(habitat="littoral")
littoral_master <- bind_rows(sky_littoral,
                             loch_littoral) %>%
  mutate(dateTime=round_date(dateTime, "hour"))
head(littoral_master)

buoy_master <- bind_rows(DO_master, loch_buoy_long, sky_buoy_long) %>%
  mutate(
    depth_category = case_when(
      depth == 0.5 ~ "surface",
      TRUE ~ "bottom"
    ),
    depth_category = factor(depth_category,
                            levels =c("surface","bottom")),
    season = case_when(
      dateTime > "2016-09-01" ~ "fall",
      TRUE                      ~ "summer"
    ),
    season = factor(season,
                    levels = c("summer","fall"))
  ) 


# Data vis buoys - D.O.----------------------------------------------------------------


#All DO,line graph
buoy_master %>%
  filter(parameter=="DO")%>%
  ggplot(aes(x=dateTime,y=value,color=depth_category))+
  geom_point(alpha=0.5,size=0.2)+
  geom_line(alpha=0.5)+
  theme_few()+
  facet_wrap(lakeID~season, scales="free_x") +
  labs(title="Dissolved oxygen",
       y="D.O. (mg/L)")

# Just summer 
buoy_master %>%
  filter(parameter=="DO")%>%
  filter(dateTime <= "2016-08-04") %>%
  ggplot(aes(x=dateTime,y=value,color=depth_category))+
  geom_point(alpha=0.5,size=0.2)+
  geom_line(alpha=0.5)+
  theme_few()+
  facet_wrap(depth~., ncol=5)+
  facet_wrap(.~lakeID, scales="free_x") +
  labs(title="Summer dissolved oxygen",
       y="D.O. (mg/L)")

# DO fluctuations
buoy_master %>%
  filter(parameter=="DO")%>%
  # filter(dateTime <= "2016-08-04") %>%
  mutate(date=date(dateTime))%>%
  group_by(lakeID,depth_category,date,parameter)%>%
  summarize(min_O=min(value, na.rm=TRUE),
            max_O=max(value, na.rm=TRUE),
            diff_O=max_O-min_O) %>%
  ggplot(aes(x=date,y=diff_O,color=depth_category))+
  geom_point(alpha=0.5,size=1)+
  # geom_line()+
  theme_few()+
  facet_grid(depth_category~lakeID, scales="free_y")+
  labs(title="Summer dissolved oxygen",
       y="Daily diel oxygen fluctuation (mg/L)")


# Data vis buoys - temperature ----------------------------------------------------------------



#All temps,line graph
buoy_master %>%
  filter(parameter=="temperature")%>%
  ggplot(aes(x=dateTime,y=value,color=depth))+
  geom_point(alpha=0.5,size=0.2)+
  geom_line(alpha=0.5)+
  theme_few()+
  facet_wrap(lakeID~season, scales="free_x") +
  labs(title="Temperature",
       y="Temp (deg C)")

# Just summer when all sensors were present
buoy_master %>%
  filter(parameter=="DO")%>%
  filter(dateTime <= "2016-08-04") %>%
  ggplot(aes(x=dateTime,y=value,color=depth_category))+
  geom_point(alpha=0.5,size=0.2)+
  geom_line(alpha=0.5)+
  theme_few()+
  facet_wrap(depth~., ncol=5)+
  facet_wrap(.~lakeID, scales="free_x") +
  labs(title="Summer dissolved oxygen",
       y="D.O. (mg/L)")

# DO fluctuations
buoy_master %>%
  filter(parameter=="DO")%>%
  # filter(dateTime <= "2016-08-04") %>%
  mutate(date=date(dateTime))%>%
  group_by(lakeID,depth_category,date,parameter)%>%
  summarize(min_O=min(value, na.rm=TRUE),
            max_O=max(value, na.rm=TRUE),
            diff_O=max_O-min_O) %>%
  ggplot(aes(x=date,y=diff_O,color=depth_category))+
  geom_point(alpha=0.5,size=1)+
  # geom_line()+
  theme_few()+
  facet_grid(depth_category~lakeID, scales="free_y")+
  labs(title="Summer dissolved oxygen",
       y="Daily diel oxygen fluctuation (mg/L)")



# Data vis Loch and Sky littoral ----------------------------------------------------------------



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


