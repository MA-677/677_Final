#due when final exam is scheduled to be over
pacman::p_load(readxl, tidyverse, dplyr, fitdistrplus, boot)
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

mledist(tidy_data$values, 'gamma')
#estimate: parameter estimates
#Shape: 0.4408386
#Rate: 1.9648409

sim_values <- rgamma(227, 0.4408386, 1.9648409) %>% round(3)
sim_year <- rep(c(1960, 1961, 1962, 1963, 1964), times=c(48, 48, 56, 37, 38))
sim_data <- data.frame(sim_values, sim_year, storm_n)

hist(sim_data$sim_values)

comp_data <- cbind(tidy_data, sim_values)
ggplot(comp_data) + geom_point(aes(x = storm_n, y = values, color = "blue")) + 
  geom_point(aes(x = storm_n, y = sim_values, color = "red"))

#random gamma generated data appears to fit well with the rain data
