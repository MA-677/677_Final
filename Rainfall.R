#due when final exam is scheduled to be over
pacman::p_load(readxl, tidyverse, dplyr, fitdistrplus)
rain_data <- read_excel("Illinois_rain_1960-1964(1).xlsx")
#average rainfall in inches for each storm

#tidy the data
tidy_data <- cbind(stack(rain_data[1:5])) %>% rename('Year' = 'ind') %>% drop_na(values)
storm_n <- c(1:227)
tidy_data <- cbind(tidy_data, storm_n)

#EDA visualization
point <- ggplot(tidy_data) + geom_point(aes(storm_n, values, color= Year)) + labels()

hist(tidy_data$values)

plotdist(tidy_data$values, histo = TRUE, demp = TRUE)
#there are a lot values close to 0, find a distribution that deals well with this

#consider removing them?
set.seed(42)
descdist(tidy_data$values, boot = 1000)

#trying out different distributions

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

#wet vs dry years
avg_1960 <- rain_data[1:48, 1]
avg_1960 <- mean(avg_1960$'1960')
avg_1961 <- rain_data[1:48, 2]
avg_1961 <- mean(avg_1961$'1961')
avg_1962 <- rain_data[1:56, 3]
avg_1962 <- mean(avg_1962$'1962')
avg_1963 <- rain_data[1:37, 4]
avg_1963 <- mean(avg_1963$'1963')
avg_1964 <- rain_data[1:38, 5]
avg_1964 <- mean(avg_1964$'1964')

avg_df <- data.frame(avg_1960, avg_1961, avg_1962, avg_1963, avg_1964)
