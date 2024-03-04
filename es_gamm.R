####Tidy data####
# Alot of this is going to be done through excel shuffling given sp_gamm_tac_na
library(tidyverse)
library(mgcv)
library(gratia)
library(readr)
#Here's all the loggers from SP in short format 

library(readr)
es_lit_temp_lex <- read_csv("es_lit_temp_lex.csv")
View(es_lit_temp_lex)

#sp_to_tidy$date <- as.Date(sp_to_tidy$date)
#Change date and time to characters for easy consolidation
es_lit_temp_lex$time <- as.character(es_lit_temp_lex$time)
es_lit_temp_lex$date <- as.character(es_lit_temp_lex$date)

# Convert the date and time to a datestamp column to POSIXct format if it's not already
es_lit_temp_lex$timestamp <- as.POSIXct(paste(es_lit_temp_lex$date, es_lit_temp_lex$time), format = "%Y-%m-%d %H:%M:%OS")

datetime_strings_es <- paste(es_lit_temp_lex$date,es_lit_temp_lex$time)

es_lit_temp_lex$timestamp <- as.POSIXct(datetime_strings_es, format = "%Y-%m-%d %H:%M:%S")

# Create new columns for the date and hour
es_lit_temp_lex$hour <- format(es_lit_temp_lex$timestamp, format="%H:00:00")

es_day_hour <- es_lit_temp_lex %>%
  group_by(date, hour) %>%
  summarize(across(starts_with("ES_"), mean, na.rm = TRUE))

#Don't have to tidy buoy goodies now, already done! 
#Tidy buoy data from 2023 SP 
#spbuoy_to_tidy <- read_csv("spbuoy_to_tidy.csv")
#View(spbuoy_to_tidy)

#spbuoy_to_tidy$time <- as.character(spbuoy_to_tidy$time)
#spbuoy_to_tidy$date <- as.character(spbuoy_to_tidy$date)

#spbuoy_to_tidy$datetime <- as.POSIXct(paste(spbuoy_to_tidy$date, spbuoy_to_tidy$time), format="%Y-%m-%d %H:%M:%S")

#spbuoy_to_tidy$hour <- format(spbuoy_to_tidy$datetime, format="%H:00:00")

#buoy_hour 
# Create new columns for the date and hour
#sp_to_tidy$hour <- format(sp_to_tidy$timestamp, format="%H:00:00")

#sp_buoy_hour <- spbuoy_to_tidy %>%
#  group_by(date, hour) %>%
#  summarize(across(starts_with("air_"), mean, na.rm = TRUE))


#write.csv(sp_buoy_hour, "sp_buoy_hr.csv", row.names=FALSE)
write.csv(es_day_hour, "es_day_hr.csv", row.names=FALSE)

#Take out lux values, shove in the air temp reading from sp buoy 

#Make a new thing - es_gamm_stac

#Add doy 
library(readr)
library(tidyverse)
es_gamm_stac <- read_csv("es_gamm_stac.csv")
View(mcd_gamm_stac)

es_gamm_stac$date <- as.Date(es_gamm_stac$date)

#mcd_gamm_stac$doy <- yday(mcd_gamm_stac$date)

#write.csv(sp_air_water_hour, "sp_gamm_short.csv", row.names=FALSE)

#Make it long version 

#sp_gamm_long <- read_csv("sp_gamm_long.csv")
#View(sp_gamm_long)

#cosine transform
es_gamm_stac$hour <- substr(es_gamm_stac$time, 1, 2)  
es_gamm_stac$hour <- as.numeric(es_gamm_stac$hour)
es_gamm_stac$cohour <- cos(2 * pi * es_gamm_stac$hour / 24)

#Make things numeric 

es_gamm_stac <- transform(es_gamm_stac,
                           doy    = as.numeric(format(date, '%j')),
                           macro = as.factor(macro),
                           depth = as.numeric(depth), 
                           site=as.factor(site))

#Omit NAs 
es_gamm_stac_na <- na.omit(es_gamm_stac)
#### Import data and model ####


str(es_gamm_stac_na)
lm2 <- lm(temp~doy, data=es_gamm_stac_na)
plot(temp~doy, data=es_gamm_stac_na)
abline(lm2, col="red")
#Depth has fewer unique covariate combinations than specified maximum degrees of freedom

#Finalize temporal autocorrelation 
#mcd_gamm_stac_na$hour <- substr(sp_gamm_tac$time, 1, 2)  
#sp_gamm_tac$hour <- as.numeric(sp_gamm_tac$hour)
#sp_gamm_tac$cohour <- cos(2 * pi * sp_gamm_tac$hour / 24)

es_gamm_stac_na$codoy <- cos(2 * pi * es_gamm_stac_na$doy / 365)

#Sparkling model that had lowest AIC
#Goofing with intercation - originally hour depth 

mod <- gamm(temp ~ s(codoy, bs = "cr") 
            + s(air_temp, bs = "cr") 
            + s(cohour, bs= "cr") 
            + s(site, bs = "re")
            #+ s(depth, bs="cr", k=3) 
            + macro,
            #+ ti(hour, doy, by=macro, bs = c("cc", "cr"), k=3),
            #+ te(macro, depth, k=3), 
            data = es_gamm_stac_na, 
            random = list(site = ~1, doy = pdMat(~doy - 1)))

#Depth here has been cut out as there's only 2 levels - 0.5m and 1m

draw(mod)
appraise(mod$gam)
summary(mod$lme)
summary(mod$gam)


es_1m_viz <- read_csv("es_1m_viz.csv")
View(es_1m_viz)

es1m_comp<- ggplot(data = es_1m_viz,aes(x = doy, y = sub_t), color="black") + 
  geom_point(alpha=0.2) +
  geom_smooth(color="blue", se=FALSE)+
  ylab("Temperature(°C)")+
  xlab("Day of year") + 
  ggtitle("Escanaba Lake - 1m daily Comp") +
  geom_vline(xintercept=172,linetype = "dashed", color = "goldenrod") +
  geom_hline( yintercept=0,linetype = "dashed", color = "red") +
  geom_hline( yintercept=1.17,linetype = "dashed", color = "darkgreen") +
  theme_bw()+
  #xlim(144,226) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) 

es1m_comp

#Calculate daily average 
es_1m_avg <- es_1m_viz %>%
  group_by(doy) %>%
  summarise(ES_1 = mean(ES_1, na.rm = TRUE), 
            ES_10 = mean(ES_10, na.rm = TRUE), 
            ES_11 = mean(ES_11, na.rm = TRUE), 
            ES_12 = mean(ES_12, na.rm = TRUE), 
            ES_3 = mean(ES_3, na.rm = TRUE), 
            ES_4 = mean(ES_4, na.rm = TRUE), 
            ES_5 = mean(ES_5, na.rm = TRUE),
            ES_6 = mean(ES_6, na.rm = TRUE),
            ES_8 = mean(ES_8, na.rm = TRUE),
            avg_1t = mean(avg_1t, na.rm = TRUE),
            avg_1tm = mean(avg_1tm, na.rm = TRUE),
            sub_t = mean(sub_t, na.rm = TRUE),
            air_temp = mean(air_temp, na.rm = TRUE),
            .groups = "drop") 

#Daily average 
es1m_daily <- ggplot(data =es_1m_avg) + 
  geom_line(alpha=0.75,mapping = aes(x = doy, y =ES_1), color="#33a02c") +
  geom_line(alpha=0.75,mapping = aes(x = doy, y =ES_10), color="#1f78b4") +
  geom_line(alpha=0.75,mapping = aes(x = doy, y =ES_11), color="#e31a1c") +
  geom_line(alpha=0.75,mapping = aes(x = doy, y =ES_12), color="#ff7f00") +
  geom_line(alpha=0.75,mapping = aes(x = doy, y =ES_3), color="#6a3d9a") +
  geom_line(alpha=0.75,mapping = aes(x = doy, y =ES_4), color="darkblue") +
  geom_line(alpha=0.75,mapping = aes(x = doy, y =ES_5), color="darkgreen") +
  geom_line(alpha=0.75,mapping = aes(x = doy, y =ES_6), color="pink") +
  geom_line(alpha=0.75,mapping = aes(x = doy, y =ES_8), color="cyan") +
  #geom_line(alpha=0.75,mapping = aes(x = doy, y = air_temp), color="black") +
  #geom_line(alpha=0.75,mapping = aes(x = sample, y =sp_pm_temp), color="orange") + 
  ylab("Temperature(°C)")+
  xlab("DoY") + 
  #geom_vline(xintercept=769,lwd=2,colour="darkgrey") + 
  #geom_vline(xintercept=3745,lwd=2,colour="darkgrey") +
  #geom_vline(xintercept=6721,lwd=2,colour="darkgrey") +
  #geom_vline(xintercept=9601,lwd=2,colour="darkgrey") +
  #geom_vline(xintercept=12577,lwd=2,colour="darkgrey") +
  #guides(color=guide_legend(title="Littoral Logger")) +
  #guides(color=guide_legend(title="Littoral Logger")) +
  #scale_fill_discrete(name = "New Legend Title") +
  ggtitle("Escanaba Lake - 1m daily") +
  #xlim(175,200)+
  #ylim(20,25) +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16))    

es1m_daily

es1m_daily_m <- ggplot(data = es_1m_avg) + 
  geom_line(alpha=0.75,mapping = aes(x = doy, y =avg_1tm), color="darkgreen") +
  geom_line(alpha=0.75,mapping = aes(x = doy, y =avg_1t), color="blue") +
  #geom_line(alpha=0.75,mapping = aes(x = doy, y =sp5), color="#e31a1c") +
  #geom_line(alpha=0.75,mapping = aes(x = doy, y =sp6), color="#ff7f00") +
  #geom_line(alpha=0.75,mapping = aes(x = doy, y =sp7), color="#6a3d9a") +
  #geom_line(alpha=0.75,mapping = aes(x = doy, y = air_temp), color="black") +
  #geom_line(alpha=0.75,mapping = aes(x = sample, y =sp_pm_temp), color="orange") + 
  ylab("Temperature(°C)")+
  xlab("DoY") + 
  #geom_vline(xintercept=769,lwd=2,colour="darkgrey") + 
  #geom_vline(xintercept=3745,lwd=2,colour="darkgrey") +
  #geom_vline(xintercept=6721,lwd=2,colour="darkgrey") +
  #geom_vline(xintercept=9601,lwd=2,colour="darkgrey") +
  #geom_vline(xintercept=12577,lwd=2,colour="darkgrey") +
  #guides(color=guide_legend(title="Littoral Logger")) +
  #guides(color=guide_legend(title="Littoral Logger")) +
  #scale_fill_discrete(name = "New Legend Title") +
  ggtitle("Escanaba Lake - 1m daily average macros & no macros") +
  #xlim(200,240)+
  #ylim(20,25) +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16))    

es1m_daily_m

