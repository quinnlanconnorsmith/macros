####Tidy data####

library(tidyverse)
library(mgcv)
library(gratia)
#Here's all the loggers from SP in short format 

library(readr)
sp_to_tidy <- read_csv("sp_to_tidy.csv")
View(sp_to_tidy)

#sp_to_tidy$date <- as.Date(sp_to_tidy$date)
sp_to_tidy$time <- as.character(sp_to_tidy$time)
sp_to_tidy$date <- as.character(sp_to_tidy$date)

# Convert the date and time to a datestamp column to POSIXct format if it's not already
sp_to_tidy$timestamp <- as.POSIXct(paste(sp_to_tidy$date, sp_to_tidy&time), format = "%Y-%m-%d %H:%M:%OS")

datetime_strings <- paste(sp_to_tidy$date, sp_to_tidy$time)

sp_to_tidy$timestamp <- as.POSIXct(datetime_strings, format = "%Y-%m-%d %H:%M:%S")

# Create new columns for the date and hour
sp_to_tidy$hour <- format(sp_to_tidy$timestamp, format="%H:00:00")

sp_day_hour <- sp_to_tidy %>%
  group_by(date, hour) %>%
  summarize(across(starts_with("SP_"), mean, na.rm = TRUE))


#Tidy buoy data from 2023 SP 
spbuoy_to_tidy <- read_csv("spbuoy_to_tidy.csv")
View(spbuoy_to_tidy)

spbuoy_to_tidy$time <- as.character(spbuoy_to_tidy$time)
spbuoy_to_tidy$date <- as.character(spbuoy_to_tidy$date)

spbuoy_to_tidy$datetime <- as.POSIXct(paste(spbuoy_to_tidy$date, spbuoy_to_tidy$time), format="%Y-%m-%d %H:%M:%S")

spbuoy_to_tidy$hour <- format(spbuoy_to_tidy$datetime, format="%H:00:00")

#buoy_hour 
# Create new columns for the date and hour
sp_to_tidy$hour <- format(sp_to_tidy$timestamp, format="%H:00:00")

sp_buoy_hour <- spbuoy_to_tidy %>%
  group_by(date, hour) %>%
  summarize(across(starts_with("air_"), mean, na.rm = TRUE))


write.csv(sp_buoy_hour, "sp_buoy_hr.csv", row.names=FALSE)
write.csv(sp_day_hour, "sp_day_hr.csv", row.names=FALSE)

#Make a new thing 

#Add doy 
library(readr)
library(tidyverse)
sp_air_water_hour <- read_csv("sp_air_water_hour.csv")
View(sp_air_water_hour)

sp_air_water_hour$date <- as.Date(sp_air_water_hour$date)

sp_air_water_hour$doy <- yday(sp_air_water_hour$date)

write.csv(sp_air_water_hour, "sp_gamm_short.csv", row.names=FALSE)

#Make it long version 

sp_gamm_long <- read_csv("sp_gamm_long.csv")
View(sp_gamm_long)

#cosine transform
sp_gamm_long$hour <- substr(sp_gamm_long$time, 1, 2)  
sp_gamm_long$hour <- as.numeric(sp_gamm_long$hour)
sp_gamm_long$cohour <- cos(2 * pi * sp_gamm_long$hour / 24)

#Make things numeric 

sp_gamm_long <- transform(sp_gamm_long,
                          doy    = as.numeric(format(date, '%j')),
                          macro = as.factor(macro),
                          depth = as.factor(depth), 
                          site=as.factor(site))

#Omit NAs 
sp_gamm_na <- na.omit(sp_gamm_long)
#### Import data and model ####
library(mgcv)
library(gratia)

str(sp_gamm_long)
lm1 <- lm(temp~doy, data=sp_gamm_long)
plot(temp~doy, data=sp_gamm_long)
abline(lm1, col="red")
#Depth has fewer unique covariate combinations than specified maximum degrees of freedom

model <- gamm(temp ~ s(doy) + s(air_temp, bs = "cr") + depth + macro + s(cohour) + 
                s(site, bs = "re"), data = sp_gamm_na, random = list(site = ~1))
#plot(model$lme)
#plot(model$gam)

draw(model)
draw(model, residuals=TRUE)
appraise(model$gam)
summary(model$lme)
summary(model$gam)


#Gam approach here
mod_gam <- gam(temp ~ s(doy) + s(air_temp, bs = "cr")  + depth + macro + s(cohour) + 
                 s(site, bs = "re"), data = sp_gamm_na, random = list(site = ~1))
draw(mod_gam)
appraise(mod_gam)
# Generate data for plotting
pred_data <- data.frame(air_temp = seq(min(sp_gamm_na$air_temp), max(sp_gamm_na$air_temp), length.out = 100))
# Predict using the smooth term for air_temp
pred_data$pred_values <- predict(model$lme, newdata = pred_data, type = "response")

#Goofin 

mod2 <- gamm(temp ~ s(doy, bs = "cc") 
             + s(air_temp, bs = "cr") 
             + s(cohour) 
             + s(site, bs = "re")
             + depth 
             + macro, 
             data = sp_gamm_na, 
             random = list(site = ~1))

draw(mod2)
appraise(mod2$gam)
summary(mod2$lme)
summary(mod2$gam)


#Combine sites 

str(sp_gamm_long_site)
sp_gamm_site <- sp_gamm_long_site

sp_gamm_site$timestamp <- as.POSIXct(paste(sp_gamm_site$date, sp_gamm_site$time), format="%Y-%m-%d %H:%M:%S")

#cosine transform
sp_gamm_site$hour <- substr(sp_gamm_site$time, 1, 2)  
sp_gamm_site$hour <- as.numeric(sp_gamm_site$hour)
sp_gamm_site$cohour <- cos(2 * pi * sp_gamm_site$hour / 24)


#Make things numeric 

sp_gamm_site <- transform(sp_gamm_site,
                          doy    = as.numeric(doy),
                          macro = as.factor(macro),
                          #depth = as.numeric(depth), 
                          site=as.factor(site))

#Omit NAs 
sp_gamm_site_na <- na.omit(sp_gamm_site)
str(sp_gamm_site_na)

#cosine transform doy 
sp_gamm_site_na$codoy <- cos(2 * pi * sp_gamm_na$doy / 365)

#Recode macro 0/1
library(dplyr)
sp_gamm_site_na <- sp_gamm_site_na %>%
  mutate(macro_cat = ifelse(macro == 0, "absent", "present"))


mod3 <- gamm(temp ~ s(doy, bs = "cc") 
             + s(air_temp, bs = "cr") 
             + s(cohour) 
             + s(site, bs = "re")
             + depth 
             + macro, 
             data = sp_gamm_site_na, 
             random = list(site = ~1))

draw(mod3)
appraise(mod3$gam)
summary(mod3$lme)
summary(mod3$gam)

#Play around with ToD and depth interaction 

mod4 <- gamm(temp ~ s(doy, bs = "cc") 
             + s(air_temp, bs = "cr") 
             + s(cohour) 
             + s(site, bs = "re")
             + depth 
             + macro
             + ti(cohour, depth, bs = c("fs", "fs")),
             data = sp_gamm_site_na, 
             random = list(site = ~1, time = pdMat(~time - 1)))

draw(mod4)
appraise(mod4$gam)
summary(mod4$lme)
summary(mod4$gam)


mod5 <- gamm(temp ~ s(doy, bs = "cc") 
             + s(air_temp, bs = "cr") 
             + s(cohour) 
             + s(site, bs = "re")
             + depth 
             + macro
             + ti(cohour, depth, bs = c("fs", "fs")),
             data = sp_gamm_site_na, 
             random = list(site = ~1, doy = pdMat(~doy - 1)))

draw(mod5)
appraise(mod5$gam)
summary(mod5$lme)
summary(mod5$gam)
plot(mod5$lme)
plot(mod5$gam)

#Will probably take eons- proceed at your own risk 
#mod6 <- gamm(temp ~ s(doy, bs = "cc") 
#             + s(air_temp, bs = "cr") 
#             + s(cohour) 
#             + s(site, bs = "re")
#             + depth 
#             + macro
#             + ti(cohour, depth, bs = c("fs", "fs")),
#             data = sp_gamm_site_na, 
#             random = list(site = ~1, timestamp = pdMat(~timestamp - 1)))

#POST OJ CHAT
#Cr on hour (switch back to mod5 cc?) 
#cosine transform on doy 
#Add interaction of depth and macros (Can't support this because binary?) 


mod6 <- gamm(temp ~ s(doy, bs = "cc") 
             + s(air_temp, bs = "cr") 
             + s(hour, bs= "cc") 
             + s(site, bs = "re")
             + depth 
             + macro_cat
             + ti(cohour, depth, bs = c("fs", "fs")),
             #+ te(macro, depth, k=1),
             data = sp_gamm_site_na, 
             random = list(site = ~1, doy = pdMat(~doy - 1)))

draw(mod6)
appraise(mod6$gam)
summary(mod6$lme)
summary(mod6$gam)
plot(mod6$lme)
plot(mod6$gam)

#Simple with depth as a number 

mod7 <- gamm(temp ~ s(doy, bs = "cc") 
             + s(air_temp, bs = "cr") 
             + s(cohour) 
             + s(site, bs = "re")
             + s(depth, bs="cr", k=3) 
             + macro, 
             data = sp_gamm_site_na, 
             random = list(site = ~1))

draw(mod7)
appraise(mod7$gam)
summary(mod7$lme)
summary(mod7$gam)



mod7 <- gamm(temp ~ s(codoy, bs = "cr") 
             + s(air_temp, bs = "cr") 
             + s(hour, bs= "cc") 
             + s(site, bs = "re")
             + s(depth, bs="cr", k=3) 
             + macro
             + ti(hour, depth, bs = c("cc", "cr"), k=3),
             #+ te(macro, depth, k=3), 
             data = sp_gamm_site_na, 
             random = list(site = ~1, doy = pdMat(~doy - 1)))

draw(mod7)
appraise(mod7$gam)
summary(mod7$lme)
summary(mod7$gam)



mod8 <- gamm(temp ~ s(codoy, bs = "cr") 
             + s(air_temp, bs = "cr") 
             + s(hour, bs= "cc") 
             + s(site, bs = "re")
             + s(depth, bs="cr", k=3) 
             + macro
             + ti(hour, depth, by=macro, bs = c("cc", "cr"), k=3),
             #+ te(macro, depth, k=3), 
             data = sp_gamm_site_na, 
             random = list(site = ~1, doy = pdMat(~doy - 1)))

draw(mod8)
appraise(mod8$gam)
summary(mod8$lme)
summary(mod8$gam)


#Dealing with temporal autocorrelation 
library(readr)
sp_gamm_tac <- read_csv("sp_gamm_tac.csv")
View(sp_gamm_tac)


sp_gamm_tac$timestamp <- as.POSIXct(paste(sp_gamm_tac$date, sp_gamm_tac$time), format="%Y-%m-%d %H:%M:%S")

#cosine transform
sp_gamm_tac$hour <- substr(sp_gamm_tac$time, 1, 2)  
sp_gamm_tac$hour <- as.numeric(sp_gamm_tac$hour)
sp_gamm_tac$cohour <- cos(2 * pi * sp_gamm_tac$hour / 24)

sp_gamm_tac$codoy <- cos(2 * pi * sp_gamm_tac$doy / 365)

#Make things numeric 

sp_gamm_tac <- transform(sp_gamm_tac,
                         doy    = as.numeric(doy),
                         macro = as.factor(macro),
                         macrodepth = as.factor(macrodepth),
                         #depth = as.numeric(depth), 
                         site=as.factor(site))

str(sp_gamm_tac)

#Omit NAs 
sp_gamm_tac_na <- na.omit(sp_gamm_tac)
str(sp_gamm_site_na)

#cosine transform doy 
sp_gamm_site_na$codoy <- cos(2 * pi * sp_gamm_na$doy / 365)

#Recode macro 0/1
sp_gamm_tac_na <- sp_gamm_tac_na %>%
  mutate(macro_cat = ifelse(macro == 0, "absent", "present"))


mod9 <- gamm(temp ~ s(codoy, bs = "cr") 
             + s(air_temp, bs = "cr") 
             + s(hour, bs= "cc") 
             + s(site, bs = "re")
             + s(depth, bs="cr", k=3) 
             + macro
             + ti(hour, depth, by=macro, bs = c("cc", "cr"), k=3),
             #+ te(macro, depth, k=3), 
             data = sp_gamm_tac_na, 
             random = list(site = ~1, doy = pdMat(~doy - 1)))

draw(mod9)
appraise(mod9$gam)
summary(mod9$lme)
summary(mod9$gam)

hist(sp_gamm_tac_na$temp)


#Trying the macro depth interaction 
#Make new column 

sp_gamm_tac_na$macrodepth <- as.factor(sp_gamm_tac_na$macrodepth)

sp_gamm_tac_na <- sp_gamm_tac_na %>%
  mutate(macrodepthbin = case_when(
    macrodepth == "abs_half" ~ "1",
    macrodepth == "abs_one" ~ "2",
    macrodepth == "pres_one" ~ "3",
    macrodepth == "abs_three" ~ "4",
    macrodepth == "pres_three" ~ "5",
    TRUE ~ NA_character_
  ))
sp_gamm_tac_na$macrodepthbin <- as.factor(sp_gamm_tac_na$macrodepthbin)


mod10 <- gamm(temp ~ s(codoy, bs = "cr") 
              + s(air_temp, bs = "cr") 
              + s(hour, bs= "cc") 
              + s(site, bs = "re")
              #+ s(depth, bs="cr", k=3) 
              #+ macro
              + macrodepthbin #shoe horn in an interaction 
              + ti(hour, depth, bs = c("cc", "cr"), k=3)
              #+ te(macro, depth, k=3) 
              ,data = sp_gamm_tac_na, 
              random = list(site = ~1, doy = pdMat(~doy - 1)))

draw(mod10)
appraise(mod10$gam)
summary(mod10$lme)
summary(mod10$gam)

####Some big visuals####

sp_3m_viz <- read_csv("sp_3m_viz.csv")
View(sp_3m_viz)

sp3m_viz <- ggplot(data = sp_3m_viz) + 
  geom_line(alpha=0.75,mapping = aes(x = obs, y =d3), color="darkgreen") +
  geom_line(alpha=0.75,mapping = aes(x = obs, y =d2), color="blue") +
  #geom_line(alpha=0.75,mapping = aes(x = obs, y = avg_t), color="black", linetype = "solid") +
  #geom_line(alpha=0.75,mapping = aes(x = sample, y =sp_pm_temp), color="orange") + 
  ylab("Temperature(°C)")+
  xlab("Obs") + 
  #geom_vline(xintercept=769,lwd=2,colour="darkgrey") + 
  #geom_vline(xintercept=3745,lwd=2,colour="darkgrey") +
  #geom_vline(xintercept=6721,lwd=2,colour="darkgrey") +
  #geom_vline(xintercept=9601,lwd=2,colour="darkgrey") +
  #geom_vline(xintercept=12577,lwd=2,colour="darkgrey") +
  #guides(color=guide_legend(title="Littoral Logger")) +
  #guides(color=guide_legend(title="Littoral Logger")) +
  #scale_fill_discrete(name = "New Legend Title") +
  ggtitle("Sparkling Lake - 3m hourly") +
  #xlim(7000,13000)+
  #ylim(16,25) +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) 

sp3m_viz


sp3m_comp<- ggplot(data = sp_3m_viz, aes(x = doy, y = sub_t), color="black") + 
  geom_smooth(color="blue", se=FALSE)+
  geom_point(alpha=0.25) +
  #geom_line(alpha=0.75,mapping = aes(x = sample, y =sp_pm_temp), color="orange") + 
  ylab("Temperature difference(°C)")+
  xlab("DoY") + 
  #geom_vline(xintercept=769,lwd=2,colour="darkgrey") + 
  #geom_vline(xintercept=3745,lwd=2,colour="darkgrey") +
  #geom_vline(xintercept=6721,lwd=2,colour="darkgrey") +
  #geom_vline(xintercept=9601,lwd=2,colour="darkgrey") +
  #geom_vline(xintercept=12577,lwd=2,colour="darkgrey") +
  #guides(color=guide_legend(title="Littoral Logger")) +
  #guides(color=guide_legend(title="Littoral Logger")) +
  #scale_fill_discrete(name = "New Legend Title") +
  ggtitle("Sparkling Lake - 3m hourly Comp") +
  geom_hline( yintercept=0,linetype = "dashed", color = "red") +
  geom_hline( yintercept=-1.3,linetype = "dashed", color = "darkgreen") +
  #xlim(7000,13000)+
  #ylim(16,25) +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) 

sp3m_comp


sp3m_point <- ggplot(data = sp_3m_viz) + 
  geom_point(alpha=0.5,mapping = aes(x = doy, y =d3), color="darkgreen") +
  geom_point(alpha=0.5,mapping = aes(x = doy, y =d2), color="blue") +
  #geom_point(alpha=0.25,mapping = aes(x = doy, y = avg_t), color="black", linetype = "solid") +
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
  ggtitle("Sparkling Lake - 3m hourly") +
  #xlim(165,245)+
  #ylim(16,25) +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) 

sp3m_point


#Calculate daily average 
sp_3m_avg <- sp_3m_viz %>%
  group_by(doy) %>%
  summarise(d2 = mean(d2, na.rm = TRUE), 
            d3 = mean(d3, na.rm = TRUE), 
            avg_t = mean(avg_t, na.rm = TRUE), 
            sub_t = mean(sub_t, na.rm = TRUE), 
            .groups = "drop") 

#Daily average 
sp3m_daily <- ggplot(data = sp_3m_avg) + 
  geom_line(alpha=0.75,mapping = aes(x = doy, y =d3), color="darkgreen") +
  geom_line(alpha=0.75,mapping = aes(x = doy, y =d2), color="blue") +
  #geom_line(alpha=0.75,mapping = aes(x = doy, y = avg_t), color="black") +
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
  ggtitle("Sparkling Lake - 3m daily") +
  #xlim(7000,13000)+
  #ylim(16,25) +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16))    

sp3m_daily

#Daily comp 

sp3m_daily_comp<- ggplot(data = sp_3m_avg,aes(x = doy, y = sub_t), color="black") + 
  geom_line(alpha=0.75) +
  geom_smooth(color="blue", se=FALSE)+
  ylab("Temperature(°C)")+
  xlab("Day of year") + 
  ggtitle("Sparkling Lake - 3m daily Comp") +
  geom_hline( yintercept=0,linetype = "dashed", color = "red") +
  geom_hline( yintercept=-1.3,linetype = "dashed", color = "darkgreen") +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) 

sp3m_daily_comp


#1m 

sp_1m_viz <- read_csv("sp_1m_viz.csv")
View(sp_1m_viz)
sp_1m_viz$sub_t <- as.numeric(sp_1m_viz$sub_t)
str(sp_1m_viz)

sp1m_viz <- ggplot(data = sp_1m_viz) + 
  geom_point(alpha=0.25,mapping = aes(x = doy, y =avg_1tm), color="darkgreen") +
  geom_point(alpha=0.25,mapping = aes(x = doy, y =avg_1t), color="blue") +
  #geom_point(alpha=0.25,mapping = aes(x = doy, y =sp4), color="#33a02c") +
  #geom_point(alpha=0.25,mapping = aes(x = doy, y =sp3), color="#1f78b4") +
  #geom_point(alpha=0.25,mapping = aes(x = doy, y =sp5), color="#e31a1c") +
  #geom_point(alpha=0.25,mapping = aes(x = doy, y =sp6), color="#ff7f00") +
  #geom_point(alpha=0.25,mapping = aes(x = doy, y =sp7), color="#6a3d9a") +
  #geom_line(alpha=0.75,mapping = aes(x = obs, y =sp5), color="blue") +
  #geom_point(alpha=0.5,mapping = aes(x = doy, y = air_temp), color="black", linetype = "solid") +
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
  ggtitle("Sparkling Lake - 1m average macros & no macros") +
  #xlim(180,244)+
  #ylim(0,1992) +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) 

sp1m_viz



sp1m_comp<- ggplot(data = sp_1m_viz, aes(x = doy, y = sub_t), color="black") + 
  geom_smooth(color="blue", se=FALSE)+
  geom_point(alpha=0.15) +
  #geom_line(alpha=0.75,mapping = aes(x = sample, y =sp_pm_temp), color="orange") + 
  ylab("Temperature difference(°C)")+
  xlab("DoY") + 
  #geom_vline(xintercept=769,lwd=2,colour="darkgrey") + 
  #geom_vline(xintercept=3745,lwd=2,colour="darkgrey") +
  #geom_vline(xintercept=6721,lwd=2,colour="darkgrey") +
  #geom_vline(xintercept=9601,lwd=2,colour="darkgrey") +
  #geom_vline(xintercept=12577,lwd=2,colour="darkgrey") +
  #guides(color=guide_legend(title="Littoral Logger")) +
  #guides(color=guide_legend(title="Littoral Logger")) +
  #scale_fill_discrete(name = "New Legend Title") +
  ggtitle("Sparkling Lake - 1m hourly Comp") +
  geom_vline(xintercept=194,linetype = "dashed", color = "goldenrod") + #doy 194 obs 1200
  geom_hline( yintercept=0,linetype = "dashed", color = "red") +
  geom_hline( yintercept=-1.3,linetype = "dashed", color = "darkgreen") +
  xlim(144,228)+
  #ylim() +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) 

sp1m_comp



#Calculate daily average 
sp_1m_avg <- sp_1m_viz %>%
  group_by(doy) %>%
  summarise(sp3 = mean(sp3, na.rm = TRUE), 
            sp4 = mean(sp4, na.rm = TRUE), 
            sp5 = mean(sp5, na.rm = TRUE), 
            avg_1t = mean(avg_1t, na.rm = TRUE), 
            avg_1tm = mean(avg_1tm, na.rm = TRUE), 
            sub_t = mean(sub_t, na.rm = TRUE), 
            sp2 = mean(sp2, na.rm = TRUE),
            sp6 = mean(sp6, na.rm = TRUE),
            sp7 = mean(sp7, na.rm = TRUE),
            sp8 = mean(sp8, na.rm = TRUE),
            air_temp = mean(air_temp, na.rm = TRUE),
            .groups = "drop") 

#Daily average 
sp1m_daily <- ggplot(data = sp_1m_avg) + 
  geom_line(alpha=0.75,mapping = aes(x = doy, y =sp4), color="#33a02c") +
  geom_line(alpha=0.75,mapping = aes(x = doy, y =sp3), color="#1f78b4") +
  geom_line(alpha=0.75,mapping = aes(x = doy, y =sp5), color="#e31a1c") +
  geom_line(alpha=0.75,mapping = aes(x = doy, y =sp6), color="#ff7f00") +
  geom_line(alpha=0.75,mapping = aes(x = doy, y =sp7), color="#6a3d9a") +
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
  ggtitle("Sparkling Lake - 1m daily") +
  xlim(175,200)+
  #ylim(20,25) +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16))    

sp1m_daily

sp1m_daily <- ggplot(data = sp_1m_avg) + 
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
  ggtitle("Sparkling Lake - 1m daily average macros & no macros") +
  #xlim(200,240)+
  #ylim(20,25) +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16))    

sp1m_daily


#Daily comp 

sp1m_daily_comp<- ggplot(data = sp_1m_avg,aes(x = doy, y = sub_t), color="black") + 
  geom_line(alpha=0.75) +
  geom_smooth(color="blue", se=FALSE)+
  ylab("Temperature(°C)")+
  xlab("Day of year") + 
  ggtitle("Sparkling Lake - 1m daily Comp") +
  geom_vline(xintercept=194,linetype = "dashed", color = "goldenrod") +
  geom_hline( yintercept=0,linetype = "dashed", color = "red") +
  geom_hline( yintercept=-1.3,linetype = "dashed", color = "darkgreen") +
  theme_bw()+
  xlim(144,226) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) 

sp1m_daily_comp

#1m and 3m raw on the same figure? 
sp1_3m_daily <- ggplot() + 
  geom_line(alpha=1,mapping = aes(x = doy, y =d3),linewidth=1.5, color="darkgreen",data = sp_3m_avg) +
  geom_line(alpha=1,mapping = aes(x = doy, y =d2),linewidth=1.5, color="blue4",data = sp_3m_avg) +
  geom_line(alpha=1,mapping = aes(x = doy, y =avg_1tm),linetype="dashed", linewidth=1.5, color="darkgreen", data = sp_1m_avg) +
  geom_line(alpha=1,mapping = aes(x = doy, y =avg_1t),linetype="dashed", linewidth=1.5, color="blue4", data = sp_1m_avg) +
  #geom_line(alpha=0.75,mapping = aes(x = doy, y = avg_t), color="black") +
  #geom_line(alpha=0.75,mapping = aes(x = sample, y =sp_pm_temp), color="orange") + 
  ylab("Temperature(°C)")+
  xlab("Day of year") + 
  ylim(16,25) +
  theme_bw()+
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16))    

sp1_3m_daily

tiff(filename = "sp_1_3m.tiff", 
     width = 12, 
     height = 8, 
     units = "in", 
     res = 600)
sp1_3m_daily
dev.off()

####Poster images - comp####
sp3m_comp<- ggplot(data = sp_3m_viz, aes(x = doy, y = sub_t), color="black") + 
  geom_smooth(color="blue4", se=FALSE)+
  geom_point(alpha=0.25) +
  #geom_line(alpha=0.75,mapping = aes(x = sample, y =sp_pm_temp), color="orange") + 
  ylab("Temperature difference(°C)")+
  xlab("Day of year") + 
  ggtitle("Sparkling Lake - 3m Comparison") +
  geom_hline( alpha=0.5,yintercept=0,linetype = "solid", linewidth=1, color = "grey10") +
  geom_hline( yintercept=-1.3,linetype = "dashed",linewidth=1.5, color = "darkgreen") +
  #xlim(7000,13000)+
  #ylim(16,25) +
  theme_bw()+
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) 

sp3m_comp
tiff(filename = "sp_3m_comp.tiff", 
     width = 12, 
     height = 8, 
     units = "in", 
     res = 600)
sp3m_comp
dev.off()

#Now for 1m 
sp1m_comp<- ggplot(data = sp_1m_viz, aes(x = doy, y = sub_t), color="black") + 
  geom_smooth(color="blue", se=FALSE)+
  geom_point(alpha=0.15) +
  #geom_line(alpha=0.75,mapping = aes(x = sample, y =sp_pm_temp), color="orange") + 
  ylab("Temperature difference(°C)")+
  xlab("Day of year") + 
  ggtitle("Sparkling Lake - 1m Comparison") +
  geom_vline(xintercept=194,linetype = "dashed",linewidth=2,color = "goldenrod") + #doy 194 obs 1200
  geom_hline( yintercept=0,linetype = "solid",linewidth=1.5,  color = "grey10") +
  geom_hline( yintercept=-1.3,linetype = "dashed", linewidth=2, color = "darkgreen") +
  xlim(144,228)+
  #ylim() +
  theme_bw()+
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) 

sp1m_comp

tiff(filename = "sp_1m_comp.tiff", 
     width = 12, 
     height = 8, 
     units = "in", 
     res = 600)
sp1m_comp
dev.off()

#3m all season comp 
sp3m_long_viz <- sp_3m_hobo

sp3m_long_viz$doy <- yday(sp3m_long_viz$date)

sp3m_long_comp<- ggplot(data = sp3m_long_viz, aes(x = doy, y = sp_m_comp), color="black") + 
  geom_smooth(color="blue", se=FALSE)+
  geom_point(alpha=0.02) +
  #geom_line(alpha=0.75,mapping = aes(x = sample, y =sp_pm_temp), color="orange") + 
  ylab("Temperature difference(°C)")+
  xlab("DoY") + 
  #geom_vline(xintercept=769,lwd=2,colour="darkgrey") + 
  #geom_vline(xintercept=3745,lwd=2,colour="darkgrey") +
  #geom_vline(xintercept=6721,lwd=2,colour="darkgrey") +
  #geom_vline(xintercept=9601,lwd=2,colour="darkgrey") +
  #geom_vline(xintercept=12577,lwd=2,colour="darkgrey") +
  #guides(color=guide_legend(title="Littoral Logger")) +
  #guides(color=guide_legend(title="Littoral Logger")) +
  #scale_fill_discrete(name = "New Legend Title") +
  ggtitle("Sparkling Lake - 3m daily Comp") +
  geom_hline( yintercept=0,linetype = "dashed", color = "red") +
  geom_hline( yintercept=-1.3,linetype = "dashed", color = "darkgreen") +
  #xlim(7000,13000)+
  #ylim(16,25) +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) 

sp3m_long_comp

sp3m_long_point <- ggplot(data = sp3m_long_viz) + 
  geom_point(alpha=0.25,mapping = aes(x = doy, y =sp_m_temp), color="darkgreen") +
  geom_point(alpha=0.25,mapping = aes(x = doy, y =sp_nom_temp), color="blue") +
  #geom_point(alpha=0.25,mapping = aes(x = doy, y = avg_t), color="black", linetype = "solid") +
  #geom_line(alpha=0.75,mapping = aes(x = sample, y =sp_pm_temp), color="orange") + 
  ylab("Temperature(°C)")+
  xlab("DOY") + 
  #geom_vline(xintercept=769,lwd=2,colour="darkgrey") + 
  #geom_vline(xintercept=3745,lwd=2,colour="darkgrey") +
  #geom_vline(xintercept=6721,lwd=2,colour="darkgrey") +
  #geom_vline(xintercept=9601,lwd=2,colour="darkgrey") +
  #geom_vline(xintercept=12577,lwd=2,colour="darkgrey") +
  #guides(color=guide_legend(title="Littoral Logger")) +
  #guides(color=guide_legend(title="Littoral Logger")) +
  #scale_fill_discrete(name = "New Legend Title") +
  ggtitle("Sparkling Lake - 3m daily") +
  #xlim(165,245)+
  #ylim(16,25) +
  theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=20), legend.title = element_text(size=16)) 

sp3m_long_point

####Experimenting with AIC#### 


mod_full <- gamm(temp ~ s(codoy, bs = "cr") 
                 + s(air_temp, bs = "cr") 
                 + s(hour, bs= "cc") 
                 + s(site, bs = "re")
                 + s(depth, bs="cr", k=3) 
                 + macro
                 + ti(hour, depth, by=macro, bs = c("cc", "cr"), k=3),
                 data = sp_gamm_tac_na, 
                 random = list(site = ~1, doy = pdMat(~doy - 1)))
anova(mod_full$gam)
gam.check(mod_full$gam)
AIC(mod_full$lme)

mod_simple <- gamm(temp ~ s(codoy, bs = "cr"), 
                   #+ s(air_temp, bs = "cr") 
                   #+ s(hour, bs= "cc") 
                   #+ s(site, bs = "re")
                   #+ s(depth, bs="cr", k=3) 
                   #+ macro
                   #+ ti(hour, depth, by=macro, bs = c("cc", "cr"), k=3),
                   data = sp_gamm_tac_na) 
#random = list(site = ~1, doy = pdMat(~doy - 1)))

mod_rand <- gamm(temp ~ s(codoy, bs = "cr"), 
                 #+ s(air_temp, bs = "cr") 
                 #+ s(hour, bs= "cc") 
                 #+ s(site, bs = "re")
                 #+ s(depth, bs="cr", k=3) 
                 #+ macro
                 #+ ti(hour, depth, by=macro, bs = c("cc", "cr"), k=3),
                 data = sp_gamm_tac_na, 
                 random = list(site = ~1))

mod_rand_temp <- gamm(temp ~ s(codoy, bs = "cr"), 
                      #+ s(air_temp, bs = "cr") 
                      #+ s(hour, bs= "cc") 
                      #+ s(site, bs = "re")
                      #+ s(depth, bs="cr", k=3) 
                      #+ macro
                      #+ ti(hour, depth, by=macro, bs = c("cc", "cr"), k=3),
                      data = sp_gamm_tac_na, 
                      random = list(site = ~1, doy = pdMat(~doy - 1)))

AIC(mod_full$lme, mod_simple$lme, mod_rand$lme, mod_rand_temp$lme)

mod_air <- gamm(temp ~ s(codoy, bs = "cr") 
                + s(air_temp, bs = "cr") 
                #+ s(hour, bs= "cc") 
                #+ s(site, bs = "re")
                #+ s(depth, bs="cr", k=3) 
                #+ macro
                #+ ti(hour, depth, by=macro, bs = c("cc", "cr"), k=3),
                ,data = sp_gamm_tac_na, 
                random = list(site = ~1, doy = pdMat(~doy - 1)))

mod_airhr <- gamm(temp ~ s(codoy, bs = "cr") 
                  + s(air_temp, bs = "cr") 
                  + s(hour, bs= "cc") 
                  #+ s(site, bs = "re")
                  #+ s(depth, bs="cr", k=3) 
                  #+ macro
                  #+ ti(hour, depth, by=macro, bs = c("cc", "cr"), k=3),
                  ,data = sp_gamm_tac_na, 
                  random = list(site = ~1, doy = pdMat(~doy - 1)))

mod_airhrsite <- gamm(temp ~ s(codoy, bs = "cr") 
                      + s(air_temp, bs = "cr") 
                      + s(hour, bs= "cc") 
                      + s(site, bs = "re")
                      #+ s(depth, bs="cr", k=3) 
                      #+ macro
                      #+ ti(hour, depth, by=macro, bs = c("cc", "cr"), k=3),
                      ,data = sp_gamm_tac_na, 
                      random = list(site = ~1, doy = pdMat(~doy - 1)))

mod_airhrsitedep <- gamm(temp ~ s(codoy, bs = "cr") 
                         + s(air_temp, bs = "cr") 
                         + s(hour, bs= "cc") 
                         + s(site, bs = "re")
                         + s(depth, bs="cr", k=3) 
                         #+ macro
                         #+ ti(hour, depth, by=macro, bs = c("cc", "cr"), k=3),
                         ,data = sp_gamm_tac_na, 
                         random = list(site = ~1, doy = pdMat(~doy - 1)))

mod_noint <- gamm(temp ~ s(codoy, bs = "cr") 
                  + s(air_temp, bs = "cr") 
                  + s(hour, bs= "cc") 
                  + s(site, bs = "re")
                  + s(depth, bs="cr", k=3) 
                  + macro
                  #+ ti(hour, depth, by=macro, bs = c("cc", "cr"), k=3),
                  ,data = sp_gamm_tac_na, 
                  random = list(site = ~1, doy = pdMat(~doy - 1)))

mod_simpleint <- gamm(temp ~ s(codoy, bs = "cr") 
                      + s(air_temp, bs = "cr") 
                      + s(hour, bs= "cc") 
                      + s(site, bs = "re")
                      + s(depth, bs="cr", k=3) 
                      + macro
                      + ti(hour, depth, bs = c("cc", "cr"), k=3)
                      ,data = sp_gamm_tac_na, 
                      random = list(site = ~1, doy = pdMat(~doy - 1)))

mod_minus1 <- gamm(temp ~ s(codoy, bs = "cr") 
                   + s(air_temp, bs = "cr") 
                   + s(hour, bs= "cc") 
                   + s(site, bs = "re")
                   #+ s(depth, bs="cr", k=3) 
                   + macro
                   + ti(hour, depth, by=macro, bs = c("cc", "cr"), k=3),
                   data = sp_gamm_tac_na, 
                   random = list(site = ~1, doy = pdMat(~doy - 1)))

AIC(mod_full$lme, mod_simple$lme, mod_rand$lme, mod_rand_temp$lme, mod_air$lme, mod_airhr$lme, mod_airhrsite$lme, mod_airhrsitedep$lme, mod_noint$lme, mod_simpleint$lme, mod_minus1$lme, mod10$lme)

library(lme4)
lmer1 <- lmer(temp~codoy + air_temp + hour + site + depth + macro + hour*depth + macro*depth + (1|site), sp_gamm_site_na, na.action="na.omit")

summary(lmer1)
vcov(lmer1)

library(VGAM)
library(MuMIn)
drop1(lmer1)
summary(mod9$lme)

#Post lab chat, random effect of day and spatial autocorrelation attempt 

write.csv(sp_gamm_tac_na, "sp_gamm_tac_na.csv")

sp_gamm_stac <- transform(sp_gamm_stac,
                          doy    = as.numeric(doy),
                          macro = as.factor(macro),
                          #depth = as.numeric(depth), 
                          site=as.factor(site))

library(mgcv)
mod11 <- gamm(temp ~ s(codoy, bs = "cr") 
              + s(air_temp, bs = "cr") 
              + s(hour, bs= "cc") 
              + s(site, bs = "re")
              + s(depth, bs="cr", k=3) 
              + t2(long,lat, bs = c("cr", "cr")) 
              + macro
              + ti(hour, depth, by=macro, bs = c("cc", "cr"), k=3), 
              data = sp_gamm_stac, 
              random = list(site = ~1), doy = pdMat(~doy - 1))

draw(mod11)
appraise(mod11$gam)
summary(mod11$lme)
summary(mod11$gam)

summary(mod9$lme)

AIC(mod9$lme,mod11$lme)



library(leaflet)

sp_hobo <- leaflet(data = SP_HOBO_locs) %>% 
  addTiles(group="Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addCircleMarkers(~longitude, ~latitude, popup = ~as.character(site), radius=1, fillOpacity=0.9, color="white")
sp_hobo


#Predicting values 

#Import data from BMWS R becasue my computer is a mess 

sp_gamm_tac_na <- transform(sp_gamm_tac_na,
                          doy    = as.numeric(doy),
                          macro = as.factor(macro),
                          #depth = as.numeric(depth), 
                          site=as.factor(site))

#Changed from codoy to doy to alleviate some issues? 

mod_full <- gamm(temp ~ s(doy, bs = "cc") 
                 + s(air_temp, bs = "cr") 
                 + s(hour, bs= "cc") 
                 + s(site, bs = "re")
                 + s(depth, bs="cr", k=3) 
                 + macro,
                 #+ ti(hour, depth, by=macro, bs = c("cc", "cr"), k=3),
                 data = sp_gamm_tac_na, 
                 random = list(site = ~1, doy = pdMat(~doy - 1)))

####We're making predict work baybee####

summary(sp_gamm_stac)

codays <- seq(-1,-0.49,0.01)
airtemps <- 10:25
hours <- 12
sites <- "SP_2"
depth <- 1
macro <- "0"

new_data <- expand.grid(
  codoy = codays,
  air_temp = airtemps,
  hour = hours,  
  site = sites,  
  depth = depth,
  macro = macro
)



predictions <- predict.gam(mod_full$gam, newdata = new_data, type = "response")

new_data$wt_pred <- predictions

par(mfrow = c(2,3))
par(mfrow = c(1,1))

plot(mod_full$gam, select=1, se = TRUE, col = "blue")  # Plot smooth terms with standard errors
lines(new_data$codoy, predictions, col = "red", lwd = 2)  # Add predicted values to the plot

plot(new_data$codoy, predictions, col = "red", lwd = 2)


# Add legend
legend("topright", legend = c("Smooth terms", "Predicted values"), col = c("blue", "red"), lty = c(1, 1), lwd = c(1, 2))


plot(predictions) #This doesn't do much good 


#Now for the macro sites 

codays <- seq(-1,-0.49,0.01)
airtemps <- 10:25
hours <- 1:23
sites <- "SP_d23"
depth <- 3
macro <- "0"

new_data_3 <- expand.grid(
  codoy = codays,
  air_temp = airtemps,
  hour = hours,  
  site = sites,  
  depth = depth,
  macro = macro
)

predictions_3 <- predict.gam(mod_full$gam, newdata = new_data_3, type = "response")

new_data_3$wt_pred <- predictions_3

plot(new_data_3$codoy, predictions_3, col = "red", lwd = 2)

#Try macro site? 

codays <- seq(-1,-0.49,0.01)
airtemps <- 10
hours <- 1:23
sites <- "SP_d23"
depth <- 3
macro <- "1"

new_data_3m <- expand.grid(
  codoy = codays,
  air_temp = airtemps,
  hour = hours,  
  site = sites,  
  depth = depth,
  macro = macro
)

predictions_3m <- predict.gam(mod_full$gam, newdata = new_data_3m, type = "response")

new_data_3m$wt_pred <- predictions_3m

plot(new_data_3m$codoy, predictions_3m, col = "red", lwd = 2)

#Try 2 at just 3m predictions 
#3m predicitons 

new_data_all3 <- expand.grid(
  codoy = seq(-0.98,-0.49,0.01), # range of cosign transformed day of year
  air_temp = seq(min(sp_gamm_tac_na$air_temp), max(sp_gamm_tac_na$air_temp), length.out = 100), # range of air temperatures
  hour = seq(min(sp_gamm_tac_na$hour), max(sp_gamm_tac_na$hour), length.out = 24), # range of hours
  site = "SP_d23", # unique sites
  depth = c(3), # depths
  macro = c(0, 1) # presence/absence of macrophytes
)

predictions_all3 <- predict.gam(mod_full$gam, newdata = new_data_all3, type = "response")

predicted_data_all3 <- cbind(new_data_all3, predictions_all3)

ggplot(predicted_data_all3, aes(x = codoy, y = predictions_all3, color = factor(macro))) +
  geom_point() +
  labs(x = "Cosign Transformed Day of Year", y = "Predicted Water Temperature", color = "Macrophyte") +
  theme_minimal()




#Try again? 
#This is working better, but is probably highly influenced by 1m water temps 
#Code for all sites 

new_data_all <- expand.grid(
  codoy = seq(min(sp_gamm_tac_na$codoy), max(sp_gamm_tac_na$codoy), length.out = 100), # range of cosign transformed day of year
  air_temp = seq(min(sp_gamm_tac_na$air_temp), max(sp_gamm_tac_na$air_temp), length.out = 100), # range of air temperatures
  hour = seq(min(sp_gamm_tac_na$hour), max(sp_gamm_tac_na$hour), length.out = 24), # range of hours
  site = unique(sp_gamm_tac_na$site), # unique sites
  depth = c(1,3), # depths
  macro = c(0, 1) # presence/absence of macrophytes
)

predictions_all <- predict.gam(mod_full$gam, newdata = new_data_all, type = "response")

predicted_data_all <- cbind(new_data_all, predictions_all)

library(ggplot2)
ggplot(predicted_data_all, aes(x = codoy, y = predictions_all, color = factor(macro))) +
  geom_line() +
  labs(x = "Cosign Transformed Day of Year", y = "Predicted Water Temperature", color = "Macrophyte") +
  theme_minimal()

#Let's make a new dataset to draw off of 

site_data <- subset(sp_gamm_tac_na, site == "SP_d23")

new_data_site <- expand.grid(
  doy = seq(min(site_data$doy), max(site_data$doy)),
  air_temp = seq(min(site_data$air_temp), max(site_data$air_temp), length.out = 100), # range of air temperatures
  hour = seq(min(site_data$hour), max(site_data$hour), length.out = 24), # range of hours
  site = "SP_d23", # specific site
  depth = c(3), # depths
  macro = c(0, 1) # presence/absence of macrophytes
)

predictions_site <- predict.gam(mod_full$gam, newdata = new_data_site, type = "response")

predicted_data_site <- cbind(new_data_site, predictions_site)


ggplot(predicted_data_site, aes(x = doy, y = predictions_site, color=factor(macro))) +
  geom_point() +
  labs(x = "Cosign Transformed Day of Year", y = "Predicted Water Temperature", color = "Macrophyte") +
  theme_minimal()

#Grabbing differences from predicitons 
ag_data_site <- aggregate(predictions_site ~ doy + macro, data = predicted_data_site, FUN = mean)

macro_diff <- ag_data_site$predictions_site[ag_data_site$macro == 1] - 
  ag_data_site$predictions_site[ag_data_site$macro == 0]

difference_data <- data.frame(doy = unique(ag_data_site$doy),
                              macro_difference = macro_diff)

library(ggplot2)
ggplot(difference_data, aes(x = doy, y = macro_difference)) +
  geom_line() +
  labs(x = "Cosign Transformed Day of Year", y = "Difference in Mean Water Temperature") +
  ggtitle("Difference in Mean Water Temperature Between Macrophyte and Non-Macrophyte Sites") +
  theme_minimal()

# Old bogus below 
days <- 200:210
hours <- 1:23


pred_values <- expand.grid(
  codoy = rep(days, each = length(hours)),
  air_temp = 20, 
  hour= rep(hours, times=length(days)), 
  site= "SP_5" ,
  depth= "1", 
  macro = rep(c(0, 1), each = length(days) * length(hours) / 2)
)

pred_values$int = interaction(pred_values$hour, pred_values$depth,by=pred_values$macro, bs = c("cc", "cr"), k=3)

pred_values$temp <- predict(mod_full$lme, newdata=pred_values, re.form=NA)


#Example 

temp_d <- expand.grid(
  mean_air_temp = seq(10,25,0.1),
  treatment = gc_mod_df$treatment, 
  replicate = levels(gc_mod_df$replicate)[[1]]
)
temp_d
