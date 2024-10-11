
Ricker <- function(S, B ){136*S*exp(-S/B)}

plot(x, Ricker(x, 2400), type="l", ylim = c(0,500000))
lines(x, Ricker(x, 5000), col="blue")
lines(x, Ricker(x, 10000), col="red")

# Changing B only makes difference once pass about 1000

Ricker(1000, 2400)
# 89656.73
Ricker(1000, 5000)
# 111347.4
Ricker(1000, 10000)
# 123057.9

# not huge difference in number of smolts produced at 1000 across B's

Data <- read.csv("DataIn/OK Chinook AUC Abundance.csv", fileEncoding="UTF-8-BOM")

cor(Data$Natural, Data$Total_Summer, use="complete.obs")
# 85% correlated -- update to 2022, only 67%

plot(Data$Year, Data$Natural, ylim = c(0,200), type="l", xlab="Year", ylab = "Cdn. .Ok Chinook Spawners")
lines(Data$Year, Data$Total_Summer/1000, col="red")
mtext(side = 4, "Upper Columbia Summer Chinook (1000's)", col="red")

plot(Data$Year, Data$Total_Summer/Data$Natural, type="l", xlab="Year",
     ylab = "Ratio")

mean(Data$Total_Summer/Data$Natural, na.rm=T)
# 1/3187
min(Data$Total_Summer/Data$Natural, na.rm=T) #713
max(Data$Total_Summer/Data$Natural, na.rm=T) #9444

min(Data$Natural/Data$Total_Summer, na.rm=T)*12143
max(Data$Natural/Data$Total_Summer, na.rm=T)*12143
# assuming highest and lowest ratios -- between 1 and 17 fish

# This means at escapement goal of 12,143  would only expect:
12143*mean(Data$Natural/Data$Total_Summer, na.rm=T)
# 6 Cdn Ok Chinook


# might make more sense to do total spawners?
Data$Total <- Data$Natural+Data$Hatchery
cor(Data$Total, Data$Total_Summer, use="complete.obs")
# 69% correlated
mean(Data$Total_Summer/Data$Total, na.rm=T)
# 1/3000
min(Data$Total_Summer/Data$Total, na.rm=T) #713
max(Data$Total_Summer/Data$Total, na.rm=T) #9444


# what is 75th percentile?

quantile(Data$Cdn_Nat_Spwn, 0.25)
# 40 fish

# Make figure for paper

plot(Data$Year, Data$Cdn_Nat_Spwn, ylim = c(0,200), xlab="Year", 
     ylab = "Ok Chinook Spawners Estimate", type="o", pch=19)
points(Data$Year, Data$MRC_Est, col="red", pch=19, cex=1.2 )
#mtext(side = 4, "Pit-tag Mark-Recap. Estimate", col="red")
legend("topleft", legend = c("AUC Estimate", "Pit-tag Mark-recap. Estimate"), 
       col = c("black", "red"), pch=19, bty="n")

# Figure without M-R Estimate
plot(Data$Year, Data$Cdn_Nat_Spwn, xlab="Year", ylim = c(0,90),
     ylab = "Ok Chinook Spawners Estimate", type="o", pch=19)

# Plot CYER over time
CYER_Dat <- read.csv("OK Chinook CYER.csv")

plot(CYER_Dat$Year, CYER_Dat$CYER, xlab="Year", ylim = c(0,1),
     ylab = "Ok Chinook CYER Estimates", type="o", pch=19)

# Look at percent change metric to try and understand how calculated

# Need to "smooth" time series? -- not sure if did this

# look at slope over last 3 gens

library(dplyr)
# get 2017:2021 value
years <- 2017:2022
Pchange.raw <- NULL
Pchange.ln <- NULL
for(yy in 1:length(years)){
  
   Dat3Gen <- Data %>% filter(Year %in% c((years[yy]-11):years[yy]))
   n <- length(Dat3Gen$Year)
   lm.coeff <- .lm.fit(cbind(1,1:n),Dat3Gen$Cdn_Nat_Spwn)$coefficients # uses model matrix that is usually created inside lm()


    Pchange.raw[yy] <- ( (lm.coeff[1]+lm.coeff[2]*n) -  (lm.coeff[1]+lm.coeff[2])) / (lm.coeff[1]+lm.coeff[2]) *100
    
    # what if log transform
    lm.coeff.ln <- .lm.fit(cbind(1,1:n),log(Dat3Gen$Cdn_Nat_Spwn))$coefficients
    Pchange.ln[yy] <- (exp(lm.coeff.ln[1]+lm.coeff.ln[2]*n) -  
                  exp(lm.coeff.ln[1]+lm.coeff.ln[2])) / exp(lm.coeff.ln[1]+lm.coeff.ln[2]) *100
}

data.frame(Year = years, Pchange.raw, Pchange.ln)

#Pchange.ln looks right

# but does this really capture percent change?

y <- seq(1000, 1150, length.out = 12)

n<-12
lm.coeff.ln <- .lm.fit(cbind(1,1:12),log(y))$coefficients
(exp(lm.coeff.ln[1]+lm.coeff.ln[2]*n) -  
                     exp(lm.coeff.ln[1]+lm.coeff.ln[2])) / exp(lm.coeff.ln[1]+lm.coeff.ln[2]) *100

# Ok I guess it seems to give the right number back

# is it smoothed?
library(zoo)

gm_mean = function(a){prod(a)^(1/length(a))}

Data$Geo.Avg <- c(NA, NA, NA, round(rollapply(Data$Cdn_Nat_Spwn, width=4, FUN = gm_mean) ))

years <- 2020:2022
Pchange.raw.geo <- NULL
Pchange.ln.geo <- NULL
for(yy in 1:length(years)){
  
  Dat3Gen <- Data %>% filter(Year %in% c((years[yy]-11):years[yy]))
  n <- length(Dat3Gen$Year)
  lm.coeff <- .lm.fit(cbind(1,1:n),Dat3Gen$Geo.Avg)$coefficients # uses model matrix that is usually created inside lm()
  
  
  Pchange.raw.geo[yy] <- ( (lm.coeff[1]+lm.coeff[2]*n) -  (lm.coeff[1]+lm.coeff[2])) / (lm.coeff[1]+lm.coeff[2]) *100
  
  # what if log transform
  lm.coeff.ln <- .lm.fit(cbind(1,1:n),log(Dat3Gen$Geo.Avg))$coefficients
  Pchange.ln.geo[yy] <- (exp(lm.coeff.ln[1]+lm.coeff.ln[2]*n) -  
                       exp(lm.coeff.ln[1]+lm.coeff.ln[2])) / exp(lm.coeff.ln[1]+lm.coeff.ln[2]) *100
}

data.frame(Year = years, Pchange.raw.geo, Pchange.ln.geo)

# must not be smoothed