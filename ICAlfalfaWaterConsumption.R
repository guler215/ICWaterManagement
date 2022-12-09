#Download Libraries 

install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("tibble")

#Import Libraries

library("ggplot2")
library("dplyr")
library("tidyverse")
library("tibble")

#Import data from .csv file

ICET <- read.csv("/cloud/project/ICET.csv", header = TRUE)

#Select Desired Columns

ETo_Data <- ICET %>%
  select(AvgRelHum_., MaxRelHum_., Eto_in, SolRad_Ly.day, AvgAirTemp_.F, Month)

#Print Clean Table

head(ETo_Data)

#Checking Distribution

hist(ETo_Data$AvgRelHum_.) 
hist(ETo_Data$SolRad_Ly.day) 
hist(ETo_Data$AvgAirTemp_.F) 
hist(ETo_Data$Eto_in) 

#Graphs (1) and Linear Regression Models (1)

#Graph 1: ETo v.s Avg Relative Humidity

ETo_Data %>%
  ggplot(mapping = aes(x = Eto_in, y = AvgRelHum_.)) +
  geom_point(color = 'black') +
  labs(x = "Evapotranspiration (in)", y = "Relative Humidity (%)") +
  stat_smooth(method = "lm", col = "red")

#Linear Regression for ETo v.s. Avg Relative Humidity

ETo.Hum.lm <- lm(Eto_in ~ AvgRelHum_., data = ETo_Data)
lm_summary1 <- summary(ETo.Hum.lm)
lm_summary1

#Graph 2: ETo v.s. Solar Radiation

ETo_Data %>%
  ggplot(mapping = aes(x = Eto_in, y = SolRad_Ly.day)) +
  geom_point(color = 'black') +
  labs(x = "Evapotranspiration (in)", y = "Solar Radiation (Ly)") +
  stat_smooth(method = "lm", col = "red")

#Linear Regression for ETo v.s. Solar Radiation

ETo.SolRad.lm <- lm(Eto_in ~ SolRad_Ly.day, data = ETo_Data)
lm_summary1 <- summary(ETo.SolRad.lm)
lm_summary1

#Graph 3: ETo v.s. Avg Air Temp

ETo_Data %>%
  ggplot(mapping = aes(x = Eto_in, y = AvgAirTemp_.F)) +
  geom_point(color = 'black') +
  labs(x = "Evapotranspiration (in)", y = "Air Temperature (F)") +
  stat_smooth(method = "lm", col = "red")

#Linear Regression for ETo v.s. Avg Air Temp

ETo.AirTemp.lm <- lm(Eto_in ~ AvgAirTemp_.F, data = ETo_Data)
lm_summary1 <- summary(ETo.AirTemp.lm)
lm_summary1

#Derive Data with Imperial County Research Statistics
#27154 gal.water/ac.in
#1200000 ac.ft.water/yr for alfalfa
#135000 ac.alfalfa/yr grown
#325851 gal.water/ac.ft
#30.42 days/month average

EToConsumption_Data <- ETo_Data %>%
  mutate(RequiredWater = Eto_in * 27154, AvgWaterUsed = ((1200000/135000)/12*325851)/30.42, ExcessWater = AvgWaterUsed - RequiredWater)
  
#Graphs (2) Including Excess Water Variable Aesthetic

#Graph 1: ETo v.s. Avg Rel Humidity

ggplot(data=EToConsumption_Data, aes(x=Eto_in, y=AvgRelHum_., color=ExcessWater)) +
  scale_color_gradient(low = "green", high = "red")+
  geom_smooth(method = 'lm') + geom_point(size=2) +
  labs( x = 'Evapotranspiration (in)', y = 'Relative Humidity (%)', color = "Excess Water (gal/ac.)")

#Graph 2: ETo v.s. Solar Radiation

ggplot(data=EToConsumption_Data, aes(x=Eto_in, y=SolRad_Ly.day, color=ExcessWater)) +
  scale_color_gradient(low = "green", high = "red")+
  geom_smooth(method = 'lm') + geom_point(size=2) +
  labs( x = 'Evapotranspiration (in)', y = 'Solar Radiation (Ly)', color = "Excess Water (gal/ac.)")

#Graph 3: ETo v.s. Avg Air Temp

ggplot(data=EToConsumption_Data, aes(x=Eto_in, y=AvgAirTemp_.F, color=ExcessWater)) +
  scale_color_gradient(low = "green", high = "red")+
  geom_smooth(method = 'lm') + geom_point(size=2) +
  labs( x = 'Evapotranspiration (in)', y = 'Air Temperature (F)', color = "Excess Water (gal/ac.)")

#Isolate Data for 6 Month Time Frame

IsoConsumption6MO_Data <- EToConsumption_Data %>%
  filter(ExcessWater > 0, Month < 5 | Month > 10)

#Isolate Data for 3 Month Time Frame

IsoConsumption3MO_Data <- EToConsumption_Data %>%
  filter(ExcessWater > 0, Month < 2 | Month > 10)

#Graphs (3) and Linear Regression Models (2) of 6 Month Isolated Data

#Graph 1: ETo v.s. Avg Rel Humidity

ggplot(data=IsoConsumption6MO_Data, aes(x=Eto_in, y=AvgRelHum_., color=ExcessWater)) +
  scale_color_gradient(low = "green", high = "red")+
  geom_smooth(method = 'lm') + geom_point(size=2) +
  labs( x = 'Evapotranspiration (in)', y = 'Relative Humidity (%)', color = "Excess Water (gal/ac.)")

#Linear Regression for ETo v.s. Avg Relative Humidity

ETo.Hum.lm <- lm(Eto_in ~ AvgRelHum_., data = IsoConsumption6MO_Data)
lm_summary1 <- summary(ETo.Hum.lm)
lm_summary1

#Graph 2: ETo v.s. Solar Radiation

ggplot(data=IsoConsumption6MO_Data, aes(x=Eto_in, y=SolRad_Ly.day, color=ExcessWater)) +
  scale_color_gradient(low = "green", high = "red")+
  geom_smooth(method = 'lm') + geom_point(size=2) +
  labs( x = 'Evapotranspiration (in)', y = 'Solar Radiation (Ly)', color = "Excess Water (gal/ac.)")

#Linear Regression for ETo v.s. Solar Radiation 

ETo.SolRad.lm <- lm(Eto_in ~ SolRad_Ly.day, data = IsoConsumption6MO_Data)
lm_summary1 <- summary(ETo.SolRad.lm)
lm_summary1

#Graph 3: ETo v.s. Avg Air Temp

ggplot(data=IsoConsumption6MO_Data, aes(x=Eto_in, y=AvgAirTemp_.F, color=ExcessWater)) +
  scale_color_gradient(low = "green", high = "red")+
  geom_smooth(method = 'lm') + geom_point(size=2) +
  labs( x = 'Evapotranspiration (in)', y = 'Air Temperature (F)', color = "Excess Water (gal/ac.)")

#Linear Regression for ETo v.s. Avg Air Temp

ETo.AirTemp.lm <- lm(Eto_in ~ AvgAirTemp_.F, data = IsoConsumption6MO_Data)
lm_summary1 <- summary(ETo.AirTemp.lm)
lm_summary1

#Graphs (4) and Linear Regression Models (3) of 3 Month Isolated Data

#Graph 1: ETo v.s. Avg Rel Humidity

ggplot(data=IsoConsumption3MO_Data, aes(x=Eto_in, y=AvgRelHum_., color=ExcessWater)) +
  scale_color_gradient(low = "green", high = "red")+
  geom_smooth(method = 'lm') + geom_point(size=2) +
  labs( x = 'Evapotranspiration (in)', y = 'Relative Humidity (%)', color = "Excess Water (gal/ac.)")

#Linear Regression for ETo v.s. Avg Relative Humidity

ETo.Hum.lm <- lm(Eto_in ~ AvgRelHum_., data = IsoConsumption3MO_Data)
lm_summary1 <- summary(ETo.Hum.lm)
lm_summary1

#Graph 2: ETo v.s. Solar Radiation

ggplot(data=IsoConsumption3MO_Data, aes(x=Eto_in, y=SolRad_Ly.day, color=ExcessWater)) +
  scale_color_gradient(low = "green", high = "red")+
  geom_smooth(method = 'lm') + geom_point(size=2) +
  labs( x = 'Evapotranspiration (in)', y = 'Solar Radiation (Ly)', color = "Excess Water (gal/ac.)")

#Linear Regression for ETo v.s. Solar Radiation 

ETo.SolRad.lm <- lm(Eto_in ~ SolRad_Ly.day, data = IsoConsumption3MO_Data)
lm_summary1 <- summary(ETo.SolRad.lm)
lm_summary1

#Graph 3: ETo v.s. Avg Air Temp

ggplot(data=IsoConsumption3MO_Data, aes(x=Eto_in, y=AvgAirTemp_.F, color=ExcessWater)) +
  scale_color_gradient(low = "green", high = "red")+
  geom_smooth(method = 'lm') + geom_point(size=2) +
  labs( x = 'Evapotranspiration (in)', y = 'Air Temperature (F)', color = "Excess Water (gal/ac.)")

#Linear Regression for ETo v.s. Avg Air Temp

ETo.AirTemp.lm <- lm(Eto_in ~ AvgAirTemp_.F, data = IsoConsumption3MO_Data)
lm_summary1 <- summary(ETo.AirTemp.lm)
lm_summary1

#Excess Water Calculations for 12 Month time frame

sum(EToConsumption_Data[, 'ExcessWater'])
mean(EToConsumption_Data[, 'RequiredWater'])

#Excess Water Calculations for 6 Month time frame

sum(IsoConsumption6MO_Data[, 'ExcessWater'])
mean(IsoConsumption6MO_Data[, 'RequiredWater'])

#Excess Water Calculation for 3 Month time frame

sum(IsoConsumption3MO_Data[, 'ExcessWater'])
mean(IsoConsumption3MO_Data[, 'RequiredWater'])

#Average Humidity and Average Max Humidity in 12 Month time frame 

mean(EToConsumption_Data[,"AvgRelHum_."])
mean(EToConsumption_Data[,"MaxRelHum_."])

#Average Humidity and Average Max Humidity in 6 Month time frame 

mean(IsoConsumption6MO_Data[,"AvgRelHum_."])
mean(IsoConsumption6MO_Data[,"MaxRelHum_."])

#Average Humidity and Average Max Humidity in 3 Month time frame 

mean(IsoConsumption3MO_Data[,"AvgRelHum_."])
mean(IsoConsumption3MO_Data[,"MaxRelHum_."])

#Excess Water Histogram - 12 Month Averages

hist(EToConsumption_Data$ExcessWater)
hist(EToConsumption_Data$Month)

#Excess Water Histogram - 6 Month Averages

hist(IsoConsumption6MO_Data$ExcessWater)
hist(IsoConsumption6MO_Data$Month)

#Excess Water Histogram - 3 Month Averages

hist(IsoConsumption3MO_Data$ExcessWater)
hist(IsoConsumption3MO_Data$Month)