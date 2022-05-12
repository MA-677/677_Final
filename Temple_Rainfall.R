#Lauren Temple 
#MA 677 Final Project


pacman::p_load(readxl, tidyverse, dplyr, fitdistrplus, boot)
rain_data <- read_excel("Illinois_rain_1960-1964(1).xlsx")
#average rainfall in inches for each storm

#tidy the data
tidy_data <- cbind(stack(rain_data[1:5])) %>% rename('Year' = 'ind') %>% drop_na(values)
storm_n <- c(1:227)
tidy_data <- cbind(tidy_data, storm_n)

#EDA visualization
point <- ggplot(tidy_data) + geom_point(aes(storm_n, values, color= Year)) ; point

gghisto <- ggplot(tidy_data) + geom_histogram(aes(x = values), bins = 50) + ggtitle('Histogram of Rainfall Data') ; gghisto

ggdens <- ggplot(tidy_data) + geom_density(aes(x= values)) ; ggdens

ggbox <- ggplot(tidy_data) + geom_boxplot(aes(x = Year, y= values, color = Year)) + theme(legend.position = "none") ; ggbox

ggvio <- ggplot(tidy_data) + geom_violin(aes(x = Year, y= values, color = Year)) + 
  geom_jitter(aes(x= Year, y= values), shape=16, position=position_jitter(0.1)) + 
  theme(legend.position = "none") ; ggvio


plotdist(tidy_data$values, histo = TRUE, demp = TRUE)
#there are a lot values close to 0, find a distribution that deals well with this phenomenon

descdist(tidy_data$values, boot = 1000)

#Trying out different distributions

#exponential
eModel <- fitdist(tidy_data$values, "exp")
gofstat(eModel) #the smaller the better for these values for KS stat and AD stat
eModel$estimate

qqcomp(eModel)
cdfcomp(eModel)
ppcomp(eModel)
denscomp(eModel) #best fitting density of the three choices, 
  #captures the large number of values near 0

#gamma
gModel <- fitdist(tidy_data$values, "gamma")
gofstat(gModel) ##lower KS and AD stat than exp
gModel$estimate

qqcomp(gModel)
cdfcomp(gModel)
ppcomp(gModel)
denscomp(gModel)
#better fitting pp and qq plot than exp

#weibull
wModel <- fitdist(tidy_data$values, 'weibull')
gofstat(wModel) #smaller KS but larger AD than gamma
wModel$estimate

qqcomp(wModel)
cdfcomp(wModel)
ppcomp(wModel)
denscomp(wModel)

#decision to move forward with gamma

#estimate parameters
mledist(tidy_data$values, 'gamma')
#estimate: parameter estimates
#Shape: 0.4408386
#Rate: 1.9648409

#simulate data based on those parameters

sim_values <- rgamma(227, 0.4408386, 1.9648409) %>% round(3)
sim_year <- rep(c(1960, 1961, 1962, 1963, 1964), times=c(48, 48, 56, 37, 38))
sim_data <- data.frame(sim_values, sim_year, storm_n)

#compare the simulated data with actual data

comp_data <- cbind(tidy_data, sim_values)

point_comp <- ggplot(comp_data) + geom_point(aes(x = storm_n, y = values, color = "blue")) + 
  geom_point(aes(x = storm_n, y = sim_values, color = "red")) + labs(color = "Data") + 
  scale_color_manual(labels = c("Actual", "Simulated"), values = c("cornflowerblue", "tomato")); point_comp

dense_comp <- ggplot(comp_data) + geom_density(aes(x = values, color = "blue")) + 
  geom_density(aes(x = sim_values, color = "red")) + labs(color = "Data") + 
  scale_color_manual(labels = c("Actual", "Simulated"), values = c("cornflowerblue", "tomato")); dense_comp

box_comp <- ggplot(comp_data) + geom_boxplot(aes(x = Year, y = values, color = "blue")) + 
  geom_boxplot(aes(x = Year, y = sim_values, color = "red")) + labs(color = "Data") + 
  scale_color_manual(labels = c("Actual", "Simulated"), values = c("cornflowerblue", "tomato")); box_comp

#Wet vs Dry

summary(tidy_data)

summary(rain_data)

#create summary tibble

sum_values <- c(sum(rain_data$`1960`, na.rm = T),
                sum(rain_data$`1961`, na.rm = T),
                sum(rain_data$`1962`, na.rm = T),
                sum(rain_data$`1963`, na.rm = T),
                sum(rain_data$`1964`, na.rm = T))

m_values <- c(mean(rain_data$`1960`, na.rm = T),
              mean(rain_data$`1961`, na.rm = T),
              mean(rain_data$`1962`, na.rm = T),
              mean(rain_data$`1963`, na.rm = T),
              mean(rain_data$`1964`, na.rm = T))

rain_summarydf <- data.frame(Years = c(1960, 1961, 1962, 1963, 1964), sum = sum_values, mean = m_values, 
                             storm_n = c(48, 48, 56, 37, 38))


