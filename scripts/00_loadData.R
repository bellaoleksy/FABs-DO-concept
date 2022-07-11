
#+ warning = FALSE, message = FALSE, echo=FALSE
if(!require(here)){install.packages("here")};library(here)
if(!require(tidyverse)){install.packages("tidyverse")};library(tidyverse)
if(!require(lubridate)){install.packages("lubridate")};library(lubridate)
if(!require(ggthemes)){install.packages("ggthemes")};library(ggthemes)
if(!require(viridis)){install.packages("viridis")};library(viridis)
if(!require(rLakeAnalyzer)){install.packages("rLakeAnalyzer")};library(rLakeAnalyzer)
source(here("scripts/00_helperFunctions.R"))

#' #  Read in data from Sky Pond (alpine) and The Loch (subalpine)
#' Sky littoral sites
#+ warning = FALSE, message = FALSE
SB1 <- read_csv(here("data/sky/littoral/SB1_2016.csv")) %>%
  mutate(dateTime = mdy_hm(dateTime))
SB2 <- read_csv(here("data/sky/littoral/SB2_2016.csv")) %>%
  mutate(dateTime = mdy_hm(dateTime))
SB3 <- read_csv(here("data/sky/littoral/SB3_2016.csv")) %>%
  mutate(dateTime = mdy_hm(dateTime))
SB4 <- read_csv(here("data/sky/littoral/SB4_2016.csv")) %>%
  mutate(dateTime = mdy_hm(dateTime))
SB5 <- read_csv(here("data/sky/littoral/SB5_2016.csv")) %>%
  mutate(dateTime = mdy_hm(dateTime))

#' Loch littoral sites
#+ warning = FALSE, message = FALSE
LB1 <- read_csv(here("data/loch/littoral/LochInlet_2016.csv")) %>%
  mutate(dateTime = mdy_hm(dateTime))
LB3 <- read_csv(here("data/loch/littoral/LB3_2016.csv")) %>%
  mutate(dateTime = mdy_hm(dateTime))
LB4 <- read_csv(here("data/loch/littoral/LB4_2016.csv")) %>%
  mutate(dateTime = mdy_hm(dateTime))
LB5 <- read_csv(here("data/loch/littoral/LB5_2016.csv")) %>%
  mutate(dateTime = mdy_hm(dateTime))
LB6 <- read_csv(here("data/loch/littoral/LB6_2016.csv")) %>%
  mutate(dateTime = mdy_hm(dateTime))

#' Master littoral
#+ warning = FALSE, message = FALSE
sky_littoral <- bind_rows(SB1, SB2, SB3, SB4, SB5) %>%
  mutate(habitat = "littoral")
loch_littoral <- bind_rows(LB3, LB4, LB5, LB6, LB1) %>%
  mutate(habitat = "littoral")

littoral_master <- bind_rows(sky_littoral,
                             loch_littoral) %>%
  mutate(dateTime = round_date(dateTime, "hour")) #For making joining to pelagic data easier

#' # Sky buoy temperature 2016
#+ warning = FALSE, message = FALSE, eval=FALSE
#' sky_buoy_long_original <-
#'   read.table(here("data/sky/pelagic/sky_2016_tempProfile.txt"),
#'              sep = ",",
#'              header = TRUE) %>%
#'   mutate(
#'     dateTime = ymd_hms(dateTime),
#'     dateTime = with_tz(dateTime, tz = "America/Denver"),
#'     dateTime = force_tz(dateTime, "America/Denver")
#'   ) %>%
#'   # filter(dateTime >= "2016-06-13" &
#'   #          dateTime <= "2016-10-30") %>% #ice off and on dates
#'   rename(wtr_6.5 = wtr_7.0) %>%
#'   pivot_longer(-dateTime, names_to = "depth") %>%
#'   mutate(habitat = "pelagic") %>%
#'   separate(col = depth,
#'            into = c("parameter", "depth"),
#'            sep = "_") %>%
#'   mutate(parameter = "temperature",
#'          lakeID = "SkyPond")
#' #' 
#' #' #' Updated Sky Pond 0.5m and 6.5m miniDot data from 2016 through 2018
#' surface <- read_csv(here("data/sky/pelagic/new/sky_0.5m_temp_DO.csv"))%>%
#'   mutate(dateTime= mdy_hm(dateTime)) %>%
#'   pivot_longer(temp_0.5:DO_0.5,
#'                names_to = c("parameter","depth"),
#'                names_sep = "_",
#'                values_to = "value")
#' hypo <- read_csv(here("data/sky/pelagic/new/sky_6.5m_temp_DO.csv"))%>%
#'   mutate(dateTime= mdy_hm(dateTime)) %>%
#'   pivot_longer(temp_6.5:DO_6.5,
#'                names_to = c("parameter","depth"),
#'                names_sep = "_",
#'                values_to = "value")
#' new_sky <- bind_rows(surface, hypo) %>%
#'   mutate(dateTime = with_tz(dateTime, tz = "America/Denver"),
#'          dateTime = force_tz(dateTime, "America/Denver"),
#'          dateTime=round_date(dateTime, "30 min"))
#' #' 
#' sky_buoy_long <- new_sky %>%
#'   bind_rows(., sky_buoy_long_original %>%
#'               select(-habitat) %>%
#'               mutate(parameter=recode(parameter, temperature="temp"),
#'                      dateTime=round_date(dateTime, "30 min"))%>%
#'               filter(!dateTime %in% new_sky$dateTime))
#' min(sky_buoy_long$dateTime)
#' max(sky_buoy_long$dateTime)
#' write_csv(sky_buoy_long, here("data/sky_buoy_long_temp_DO_2016-07-14_to_2018-08-25.csv"))

#' Read in Sky Pond buoy data 2016-07-14 20:30:00 UTC through 2018-08-25 09:00:00 UTC
#' I commented out the wrangling I did above to combine a few .csv files
sky_buoy_long<-read_csv(here("data/sky_buoy_long_temp_DO_2016-07-14_to_2018-08-25.csv")) %>%
  mutate(dateTime = with_tz(dateTime, tz = "America/Denver"),
         dateTime = force_tz(dateTime, "America/Denver"),
         depth = as.character(as.numeric(depth)))

#' + Note that 0.5 and 6.5 are MiniDOT observations which are more frequent than the others
#' + All other depths were recorded with iButtons, same as the littoral zone sites


#' # Loch buoy temperature
#+ warning = FALSE, message = FALSE

#' loch_buoy_long_original <- read.table(here("data/loch/pelagic/loch_2016_tempProfile.txt"), sep=",", header=TRUE) %>%
#'   mutate(dateTime = ymd_hms(as.factor(dateTime)),
#'          dateTime = force_tz(dateTime, tz="America/Denver"),
#'          dateTime = with_tz(dateTime, "America/Denver")) %>%
#'   filter(dateTime > "2016-05-31" & dateTime <= "2016-10-30") %>% #ice off and on dates
#'   pivot_longer(-dateTime, names_to="depth") %>%
#'   mutate(habitat="pelagic") %>%
#'   separate(col = depth, into = c("parameter", "depth"), sep = "_") %>%
#'   mutate(parameter="temperature",
#'          lakeID="TheLoch")
#' 
#' #' #' Updated The Loch 0.5m and 4.5m miniDot data from 2016 through 2018
#' surface <- read_csv(here("data/loch/pelagic/loch_0.5m_temp_DO.csv"))%>%
#'   mutate(dateTime= mdy_hm(dateTime)) %>%
#'   pivot_longer(temp_0.5:DO_0.5,
#'                names_to = c("parameter","depth"),
#'                names_sep = "_",
#'                values_to = "value")
#' hypo <- read_csv(here("data/loch/pelagic/loch_4.5m_temp_DO.csv"))%>%
#'   mutate(dateTime= mdy_hm(dateTime)) %>%
#'   pivot_longer(temp_4.5:DO_4.5,
#'                names_to = c("parameter","depth"),
#'                names_sep = "_",
#'                values_to = "value")
#' new_loch <- bind_rows(surface, hypo) %>%
#'   mutate(dateTime = with_tz(dateTime, tz = "America/Denver"),
#'          dateTime = force_tz(dateTime, "America/Denver"),
#'          dateTime=round_date(dateTime, "30 min"))
#' #' 
#' loch_buoy_long <- new_loch %>%
#'   bind_rows(., loch_buoy_long_original %>%
#'               select(-habitat) %>%
#'               mutate(parameter=recode(parameter, temperature="temp"),
#'                      dateTime=round_date(dateTime, "30 min"))%>%
#'               filter(!dateTime %in% new_loch$dateTime))
#' min(loch_buoy_long$dateTime)
#' max(loch_buoy_long$dateTime)
#' write_csv(loch_buoy_long, here("data/loch_buoy_long_temp_DO_2016-07-19_to_2018-09-11.csv"))

#' Read in Sky Pond buoy data 2016-07-14 20:30:00 UTC through 2018-08-25 09:00:00 UTC
#' I commented out the wrangling I did above to combine a few .csv files
loch_buoy_long<-read_csv(here("data/loch_buoy_long_temp_DO_2016-07-19_to_2018-09-11.csv")) %>%
  mutate(dateTime = with_tz(dateTime, tz = "America/Denver"),
         dateTime = force_tz(dateTime, "America/Denver"),
         depth = as.character(as.numeric(depth)))

#' Master buoy df
#+ warning = FALSE, message = FALSE
buoy_master <-
  bind_rows(loch_buoy_long, sky_buoy_long) %>%
  mutate(
    depth_category = case_when(depth == 0.5 ~ "surface",
                               depth == 4.5 | depth == 6.5 ~ "bottom",
                               depth %in% c("2","4","4") ~ depth,
                               TRUE ~ "other")
    # depth_category = factor(depth_category,
    #                         levels = c("surface", "bottom"))
  ) 

#' Meterological data
wx <- read_csv(here("data/WY2016to2022_LochVale_WX.csv")) %>%
  mutate(dateTime = mdy_hm(dateTime))

#' Chlorophyll data
chl <- read_csv(here("data/AllPelagicCHLA_2016-2021.csv")) %>%
  mutate(date= mdy(date))




#' Does the timeseries look ok? 
sky_buoy_long %>%
  filter(depth %in% c("0.5","6.5")) %>%
  ggplot(aes(x=dateTime, y=value, color=factor(depth))) +
  geom_point(alpha=0.2)+
  facet_wrap(parameter~., nrow=2, scales="free_y")+
  scale_x_datetime(date_breaks = "2 months",
               date_minor_breaks = "4 weeks")
#' Some kind of crazy dynamics going on around ice-off.
#' Interesting that winter 2016-2017 the lake pretty distinctly reverse stratifes, but not in 2017-2018
#' in 2017, The Loch (subalpine lakes ~200m lower in elevation) had ice-off by 5/16. 
#' Usually Sky Pond is ~2 weeks or a little more behind.

sky_buoy_long %>%
  filter(depth %in% c("0.5","6.5")) %>%
  filter(dateTime >= "2017-05-01" & dateTime <= "2017-06-01") %>%
  ggplot(aes(x=dateTime, y=value, color=factor(depth))) +
  geom_point(alpha=0.2)+
  facet_wrap(parameter~., nrow=2, scales="free_y")+
  scale_x_datetime(date_breaks = "3 days",
                   date_minor_breaks = "1 day")+
  labs(title="Around ice-off 2017")

sky_buoy_long %>%
  filter(depth %in% c("0.5","6.5")) %>%
  filter(dateTime >= "2018-05-01" & dateTime <= "2018-06-01") %>%
  ggplot(aes(x=dateTime, y=value, color=factor(depth))) +
  geom_point(alpha=0.2)+
  facet_wrap(parameter~., nrow=2, scales="free_y")+
  scale_x_datetime(date_breaks = "3 days",
                   date_minor_breaks = "1 day")+
  labs(title="Around ice-off 2018")




#' Does the timeseries look ok? 
loch_buoy_long %>%
  filter(depth %in% c("0.5","4.5")) %>%
  ggplot(aes(x=dateTime, y=value, color=factor(depth))) +
  geom_point(alpha=0.2)+
  facet_wrap(parameter~., nrow=2, scales="free_y")+
  scale_x_datetime(date_breaks = "2 months",
                   date_minor_breaks = "4 weeks")
#' Some kind of crazy dynamics going on around ice-off.
#' Interesting that winter 2016-2017 the lake pretty distinctly inverse stratifes, but not in 2017-2018
#' in 2017, The Loch  had ice-off by 5/16. 

loch_buoy_long %>%
  filter(depth %in% c("0.5","4.5")) %>%
  filter(dateTime >= "2017-05-01" & dateTime <= "2017-06-01") %>%
  ggplot(aes(x=dateTime, y=value, color=factor(depth))) +
  geom_point(alpha=0.2)+
  facet_wrap(parameter~., nrow=2, scales="free_y")+
  scale_x_datetime(date_breaks = "3 days",
                   date_minor_breaks = "1 day")+
  labs(title="Around ice-off 2017")

#' Some wild DO dynamics in late april early may ahead of ice-out. 
loch_buoy_long %>%
  filter(depth %in% c("0.5","4.5")) %>%
  filter(dateTime >= "2018-04-15" & dateTime <= "2018-05-15") %>%
  ggplot(aes(x=dateTime, y=value, color=factor(depth))) +
  geom_point(alpha=0.2)+
  facet_wrap(parameter~., nrow=2, scales="free_y")+
  scale_x_datetime(date_breaks = "3 days",
                   date_minor_breaks = "1 day")+
  labs(title="Around ice-off 2018")


#' # Data vis: D.O. from buoys ----------------------------------------------------------------


#' All DO, line graph
#+ fig.height=10, fig.width=15, echo=FALSE, warning=FALSE, message=FALSE
buoy_master %>%
  filter(parameter=="DO")%>%
  ggplot(aes(x=dateTime,y=value,color=depth_category,shape=season))+
  geom_point(alpha=0.5,size=0.2)+
  geom_line(alpha=0.5)+
  facet_wrap(lakeID~year, scales="free_x") +
  labs(title="Dissolved oxygen",
       y="D.O. (mg/L)")+
  theme(legend.position="bottom")

#' Plotting just summer below, where we have overlap with littoral zone measurements
#+ fig.height=10, fig.width=15, echo=FALSE, warning=FALSE, message=FALSE
buoy_master %>%
  filter(parameter=="DO")%>%
  filter(dateTime <= "2016-08-04") %>%
  ggplot(aes(x=dateTime,y=value,color=depth_category))+
  geom_point(alpha=0.5,size=0.2)+
  geom_line(alpha=0.5)+
  facet_wrap(depth~., ncol=5)+
  facet_wrap(.~lakeID, scales="free_x") +
  labs(title="Summer dissolved oxygen",
       y="D.O. (mg/L)")+
  theme(legend.position="bottom")

#' How much does DO fluctuate daily at each depth? 
#+ fig.height=6, fig.width=15, echo=FALSE, warning=FALSE, message=FALSE
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
  facet_grid(depth_category~lakeID, scales="free_y")+
  labs(title="Summer dissolved oxygen",
       y="Daily diel oxygen fluctuation (mg/L)")+
  theme(legend.position="bottom")



#' # Data vis: temperature from buoys ----------------------------------------------------------------

#' All temps,line graph
#+ fig.height=10, fig.width=15, echo=FALSE, warning=FALSE, message=FALSE
buoy_master %>%
  filter(parameter=="temperature")%>%
  ggplot(aes(x=dateTime,y=value,color=depth))+
  geom_point(alpha=0.5,size=0.2)+
  geom_line(alpha=0.5)+
  facet_wrap(lakeID~season, scales="free_x") +
  labs(title="Temperature",
       y="Temp (deg C)")+
  theme(legend.position="bottom")

#' Just summer when all sensors were present
#+ fig.height=10, fig.width=15, echo=FALSE, warning=FALSE, message=FALSE
buoy_master %>%
  filter(parameter=="temperature")%>%
  filter(dateTime <= "2016-08-04") %>%
  ggplot(aes(x=dateTime,y=value,color=depth_category))+
  geom_point(alpha=0.5,size=0.2)+
  geom_line(alpha=0.5)+
  facet_wrap(depth~., ncol=5)+
  facet_wrap(.~lakeID, scales="free_x") +
  labs(title="Summer temperature",
       y="Temp (deg C)")+
  theme(legend.position="bottom")

#' How much does temperature fluctuate daily at each depth? 
#+ fig.height=6, fig.width=15, echo=FALSE, warning=FALSE, message=FALSE
buoy_master %>%
  filter(parameter=="temperature")%>%
  # filter(dateTime <= "2016-08-04") %>%
  mutate(date=date(dateTime))%>%
  group_by(lakeID,depth_category,date,parameter)%>%
  summarize(min_O=min(value, na.rm=TRUE),
            max_O=max(value, na.rm=TRUE),
            diff_O=max_O-min_O) %>%
  ggplot(aes(x=date,y=diff_O,color=depth_category))+
  geom_point(alpha=0.5,size=1)+
  facet_grid(depth_category~lakeID, scales="free_y")+
  labs(title="Summer temperature",
       y="Daily diel tempearture fluctuation (deg C)")+
  theme(legend.position="bottom")



#' # Data vis: temperature from littoral zone ----------------------------------------------------------------

#' Littoral zones temperature in both lakes, 2016
#+ fig.height=10, fig.width=15, echo=FALSE, warning=FALSE, message=FALSE
littoral_master %>%
  ggplot(aes(x=dateTime,y=temp_C,color=siteID,shape=lakeID,linetype=lakeID))+
  geom_point(alpha=0.5,size=0.2)+
  geom_line(alpha=0.5)+
  facet_wrap(lakeID~siteID, ncol=5, nrow=2)+
  scale_color_viridis(discrete=TRUE)+
  theme(legend.position="none")

#' How large are the diurnal swings?
#+ fig.height=10, fig.width=15, echo=FALSE, warning=FALSE, message=FALSE
littoral_master %>%
  mutate(date=date(dateTime))%>%
  group_by(lakeID,siteID,date)%>%
  summarize(min_T=min(temp_C, na.rm=TRUE),
            max_T=max(temp_C, na.rm=TRUE),
            diff_T=max_T-min_T) %>%
  ggplot(aes(x=date,y=diff_T,color=siteID,shape=lakeID,linetype=lakeID))+
  geom_point(alpha=0.5)+
  geom_line(alpha=0.5)+
  facet_wrap(lakeID~siteID, ncol=5, nrow=2)+
  scale_color_viridis(discrete=TRUE)+
  theme(legend.position="none")

#' Histograms of diel temperature swings
#+ fig.height=10, fig.width=15, echo=FALSE, warning=FALSE, message=FALSE
bind_rows(SB1,SB2,SB3,SB4,SB5,
          LB3,LB4,LB5,LB6,LB1)%>%
  mutate(date=date(dateTime))%>%
  group_by(lakeID,siteID,date)%>%
  summarize(min_T=min(temp_C, na.rm=TRUE),
            max_T=max(temp_C, na.rm=TRUE),
            diff_T=max_T-min_T) %>%
  ggplot(aes(x=diff_T, fill=siteID)) +
  geom_histogram(alpha=0.5, position="identity", bins=10)+
  scale_fill_viridis(discrete=TRUE)+
  facet_wrap(siteID~lakeID, nrow=2, ncol=5, scales="free_x")


#' How do the littoral zone temperatures compared to 0.5m temperatures?
#' Separate panel for each site
#+ fig.height=10, fig.width=15, echo=FALSE, warning=FALSE, message=FALSE
bind_rows(SB1,SB2,SB3,SB4,SB5,
          LB3,LB4,LB5,LB6,LB1)%>%
  select(lakeID, siteID, dateTime, temp_C) %>%
  bind_rows(.,loch_buoy_long %>%
              filter(parameter=="temperature" & depth=="0.5") %>%
              rename(temp_C=value,
                     siteID=depth) %>%
              select(lakeID, siteID, dateTime, temp_C)) %>%
  bind_rows(.,sky_buoy_long %>%
              filter(parameter=="temperature" & depth=="0.5") %>%
              rename(temp_C=value,
                     siteID=depth) %>%
              select(lakeID, siteID, dateTime, temp_C)) %>%
  mutate(date=date(dateTime))%>%
  ggplot(aes(x=date,y=temp_C,color=siteID,shape=lakeID,linetype=lakeID))+
  geom_point(alpha=0.5)+
  geom_line(alpha=0.5)+
  facet_wrap(lakeID~siteID, ncol=6, nrow=2)+
  scale_color_viridis(discrete=TRUE)+
  theme(legend.position="none")

#' How do the littoral zone temperatures compared to 0.5m temperatures?
#' Include 0.5m depth in the background of each panel
#+ fig.height=10, fig.width=15, echo=FALSE, warning=FALSE, message=FALSE
surface_temps<- loch_buoy_long %>%
            filter(parameter=="temperature" & depth=="0.5") %>%
            rename(surface_temp_C=value,
                   siteID=depth) %>%
            select(lakeID, dateTime, surface_temp_C) %>%
  bind_rows(.,sky_buoy_long %>%
              filter(parameter=="temperature" & depth=="0.5") %>%
              rename(surface_temp_C=value,
                     siteID=depth) %>%
              select(lakeID, dateTime, surface_temp_C)) %>%
  mutate(date=date(dateTime))


bind_rows(SB1,SB2,SB3,SB4,SB5,
          LB3,LB4,LB5,LB6,LB1)%>%
  mutate(siteID=recode(siteID, SB3 = "SB3 (inlet, rock glacier)", LB1 = "LB1 (inlet, stream)")) %>%
  mutate(date=date(dateTime),
         dateTime=round_date(dateTime, "hour"))%>% #so they play nice 
  left_join(., surface_temps, by=c("lakeID","date","dateTime")) %>%
  select(lakeID, siteID, date, dateTime, temp_C, surface_temp_C) %>%
  ggplot(aes(linetype=lakeID))+
  geom_point(aes(x=dateTime,y=surface_temp_C, group=lakeID), color="grey80", alpha=0.7)+
  geom_line(aes(x=dateTime, y=surface_temp_C, group=lakeID), color="grey50", alpha=0.7)+
  geom_point(aes(x=dateTime,y=temp_C,color=siteID,shape=lakeID), alpha=0.2)+
  geom_line(aes(x=dateTime,y=temp_C,color=siteID,shape=lakeID), alpha=0.2)+
  facet_wrap(lakeID~siteID, ncol=5, nrow=2)+
  scale_color_viridis(discrete=TRUE)+
  theme(legend.position="none")+
  labs(title="Loch and Sky Pond 2016 - raw data",
       subtitle="Panels are individual littoral zone sites while grey points on each panel are pelagic temperature measurements at 0.5m",
       y="Temperature (deg C)",
       x="Date")

ggsave("figures/Littoral_and_pelagic_temperature_2016_raw.png", width=15, height=9,units="in", dpi=300)



#' How do the littoral zone diel fluctuations compare to 0.5m temperatures?
#' Include 0.5m depth in the background of each panel
#+ fig.height=10, fig.width=15, echo=FALSE, warning=FALSE, message=FALSE
  bind_rows(SB1,SB2,SB3,SB4,SB5,
            LB3,LB4,LB5,LB6,LB1)%>%
  mutate(siteID=recode(siteID, SB3 = "SB3 (inlet, rock glacier)", LB1 = "LB1 (inlet, stream)")) %>%
  mutate(date=date(dateTime),
         dateTime=round_date(dateTime, "hour"))%>% #so they play nice 
  group_by(lakeID, siteID, date) %>%
  summarize(min_T=min(temp_C, na.rm=TRUE),
              max_T=max(temp_C, na.rm=TRUE),
              diff_T=max_T-min_T) %>%
  select(-c(min_T, max_T)) %>%
  left_join(., surface_temps %>%
              group_by(lakeID,date)%>%
              summarize(min_T=min(surface_temp_C, na.rm=TRUE),
                        max_T=max(surface_temp_C, na.rm=TRUE),
                        diff_T_surface=max_T-min_T) %>%
              select(-c(min_T, max_T)) %>%
              mutate(diff_T_surface = na_if(diff_T_surface, -Inf)), by=c("lakeID","date")) %>%
  select(lakeID, siteID, date, diff_T, diff_T_surface) %>%
  ggplot(aes(linetype=lakeID))+
  geom_point(aes(x=date,y=diff_T_surface, group=lakeID), color="grey80", alpha=0.7)+
  geom_line(aes(x=date, y=diff_T_surface, group=lakeID), color="grey50", alpha=0.7)+
  geom_point(aes(x=date,y=diff_T,color=siteID,shape=lakeID), alpha=0.2)+
  geom_line(aes(x=date,y=diff_T,color=siteID,shape=lakeID), alpha=0.2)+
  facet_wrap(lakeID~siteID, ncol=5, nrow=2)+
  scale_color_viridis(discrete=TRUE)+
  theme(legend.position="none")+
  labs(title="Loch and Sky Pond 2016 - diel fluctuations",
       subtitle="Panels are individual littoral zone sites while grey points on each panel are pelagic temperature measurements at 0.5m",
       y="MaximumT - MinimumT (deg C)",
       x="Date")

ggsave("figures/Littoral_and_pelagic_temperature_2016_diel.png", width=15, height=9,units="in", dpi=300)

#' # Chlorophyll data
#' Not entirely sure why there is a gap in 2017 since I know I was collecting data during this time (IAO)
#+ fig.height=10, fig.width=15, echo=FALSE, warning=FALSE, message=FALSE
chl %>%
  mutate(year=year(date)) %>%
  filter(sampleType %in% c("Pelagic","Epilimnion","Hypolimnion")) %>%
  ggplot(aes(x=date, y=chla_ugL, color=lakeID, shape=sampleType))+
  geom_point()+
  facet_wrap(lakeID~year, scales="free", ncol=6)


#' # Met data
#' How do the various meterological variables vary by year? 
#' ## Year-round ata
#+ fig.height=10, fig.width=15, echo=FALSE, warning=FALSE, message=FALSE
wx %>%
  pivot_longer(T_air:LWout) %>%
  mutate(year=year(dateTime),
         doy=yday(dateTime)) %>%
  filter(year>2015 & year <2022) %>% #look at complete years only 
  filter(!name %in% c("LWin","LWout","SWin","SWout")) %>% #only show variables with data across all years
  # filter(doy > 121 & doy < 273) %>% #let's just look at May-Sept inclusive
  group_by(name) %>%
  mutate(scaled_value=scale(value)) %>%
  ggplot(aes(y=scaled_value, x=factor(year), color=factor(year))) +
  geom_violin() +
  geom_boxplot(width=0.2) +
  facet_wrap(name~., scales="free_y") +
  scale_color_viridis(discrete=TRUE) +
  labs(x="Year",
       y="Scaled value (z-score)") +
  theme(legend.position="none")
ggsave("figures/LochVale_wx_summary_2016-2021.png", width=15, height=9,units="in", dpi=300)


#' ## Generally open-water season (June-Aug inclusive)
wx %>%
  pivot_longer(T_air:LWout) %>%
  mutate(year=year(dateTime),
         doy=yday(dateTime)) %>%
  filter(year>2015 & year <2022) %>% #look at complete years only 
  filter(!name %in% c("LWin","LWout","SWin","SWout")) %>% #only show variables with data across all years
  filter(doy > 152 & doy < 243) %>% #let's just look at June-Aug inclusive
  group_by(name) %>%
  mutate(scaled_value=scale(value)) %>%
  ggplot(aes(y=scaled_value, x=factor(year), color=factor(year))) +
  geom_violin() +
  geom_boxplot(width=0.2) +
  facet_wrap(name~., scales="free_y") +
  scale_color_viridis(discrete=TRUE) +
  labs(x="Year",
       y="Scaled value (z-score)") +
  theme(legend.position="none")
