
#   Part 1) Preparing the data set

library(fpp3)
library(forecast)

dat <- read.csv("bike_share_day.csv")
cnts <- ts(dat[,14],frequency = 7)
cntts <- as_tsibble(cnts)
names(cntts)[2] <- "count"
str(cntts)
fit_bike_mean <- model(cntts, Mean = MEAN(count),
                     Naive = NAIVE(count),
                     Drift = NAIVE(count))
accuracy(fit_bike_mean)

fit_bike_SES_0.25 <- model(cntts, ETS(count ~ error("A") + 
                                   trend("N", alpha = 0.25) + 
                                   season("N")))
report(fit_bike_SES_0.25)
accuracy(fit_bike_SES_0.25)


fit_bike_SES_0.75 <- model(cntts, ETS(count ~ error("A") + 
                                        trend("N", alpha = 0.75) + 
                                        season("N")))
report(fit_bike_SES_0.75)
accuracy(fit_bike_SES_0.75)

# Model with a = 0.25 is better fit because it has the 
#   lowest RMSE

#   Part 2) Holt's model

fit_cntts_Holt_addi <- model(cntts, 
                          ETS(count ~ error("A") 
                              + trend("A") 
                              + season("N")))
accuracy(fit_cntts_Holt_addi)

fit_cntts_Holt_multi <- model(cntts, 
                               ETS(count ~ error("M") 
                                   + trend("M") 
                                   + season("N")))
accuracy(fit_cntts_Holt_multi)

# Out of 2 models, addictive model has smaller RMSE
#   and this RMSE is equal to RMSE in model with a = 0.25
# Therefore, to compare 2 models we use MAPE which is
#   mean absolute percentage error and additive has smaller
#   MAPE so it's better than a = 0.25.

#   Part 3) Preparing the data set

cntts_damped <- model(cntts,
  hw = ETS(count ~ error("M") + trend("Ad") + season("M")))
accuracy(cntts_damped)

cntts_Holt_season_add <- model(cntts, 
                                 ETS(count ~ error("A") 
                                     + trend("A") 
                                     + season("A")))
accuracy(cntts_Holt_season_add)

cntts_Holt_season_multi <- model(cntts, 
                              ETS(count ~ error("M") 
                                  + trend("M") 
                                  + season("M")))
accuracy(cntts_Holt_season_multi)

# Damped multiplicative is a better model because
#   of lower RMSE. The result is not surprising becuase
#   it lowers the uncertainty of trend.

#   Part 4) Forecasting

forc_cntts_0.25 <- forecast(fit_bike_SES_0.25, h =28)
autoplot(forc_cntts_0.25, cntts)
forc_cntts_0.25

forc_cntts_addi <- forecast(fit_cntts_Holt_addi, h =28)
autoplot(forc_cntts_addi, cntts)
forc_cntts_0.25

forc_cntts_damp <- forecast(cntts_damped, h =28)
autoplot(forc_cntts_damp, cntts) 
forc_cntts_damp

#   Part 5) AAA ETS
data()
Johnson <- JohnsonJohnson
Johnsonts<- ts(JohnsonJohnson)
JJ <- as_tsibble(Johnsonts)
str(JJ)
summary(JJ)

# This dataset has 84 observations (rows), range is frome
#   0.44 to 16.2. The periodicity is quarterly

ETS_JJ <- model(JJ, ETS(value ~ error("A") + 
                                        trend("A") + 
                                        season("A")))
# Since model above has an error with season,
#   we will run it with N in season

ETS_JJ <- model(JJ, ETS(value ~ error("A") + 
                            trend("A") + 
                            season("N")))
report(ETS_JJ)
# coefficient: 0.7 and bo = -0.0077

aug_ETS_JJ <- augment(ETS_JJ)
autoplot(aug_ETS_JJ, value) +
  autolayer(aug_ETS_JJ,.fitted, colour = "Red") +
  autolayer(aug_ETS_JJ,.resid, colour = "Green") +
  labs(y = "USD", title = "Quarterly earnings of Johnson Jonhson",
       x = "Quarter") 

#   Part 6) Different ETS

ETS_JJ_MMN <- model(JJ,ETS(value ~ error("M") 
                             + trend("M") 
                             + season("N")))
accuracy(ETS_JJ_MMN)

ETS_JJ_AMN <- model(JJ,ETS(value ~ error("A") 
                        + trend("M") 
                        + season("N")))
accuracy(ETS_JJ_AMN)

ETS_JJ_MAN <- model(JJ,ETS(value ~ error("M") 
                           + trend("A") 
                           + season("N")))

accuracy(ETS_JJ_MAN)

# Out of 3 models, the best model is EE_JJ_MAN which 
#   we use multiplicative for error and addictive for 
#   both season.

aug_ETS_JJ_MAN <- augment(ETS_JJ_MAN)
autoplot(aug_ETS_JJ_MAN, value) +
  autolayer(aug_ETS_JJ_MAN,.fitted, colour = "Red") +
  autolayer(aug_ETS_JJ_MAN,.resid, colour = "Green") +
  labs(y = "USD", title = "Quarterly earnings of Johnson Jonhson",
       x = "Quarter") 

forc_JJ <- forecast(ETS_JJ_MAN, h = 36)
forc_JJ

#  Plot the chosen model forcasting
#
autoplot(forc_JJ, JJ, 
         level = NULL, colour = "Blue") +
  labs(y = "USD", title = "Earnings of Johnson Jonhson",
       x = "Days")

#   Part 7) How is the preferred model selected by the ETS command 

# ets()  AICC and to determine which of the ETS model 
#   is appropriate for given time series

#   Part 8) ETS model on the US employment data 

usemp <- us_employment
usemp <- filter(usemp, Title == "Total Private")
usemp <- usemp[,c(1,4)]
autoplot(usemp, Employed) 
summary(usemp)

#Manual selected ETS model

ETS_USE_AAA <- model(usemp, ETS(Employed ~ error("A") + 
                          trend("A") + 
                          season("A")))
accuracy(ETS_USE_AAA)

ETS_USE_MMM <- model(usemp,ETS(Employed ~ error("M") 
                           + trend("M") 
                           + season("M")))
accuracy(ETS_USE_MMM)

ETS_USE_AMA <- model(usemp,ETS(Employed ~ error("A") 
                           + trend("M") 
                           + season("A")))
accuracy(ETS_USE_AMA)

ETS_USE_MAM <- model(usemp,ETS(Employed ~ error("M") 
                           + trend("A") 
                           + season("M")))

accuracy(ETS_USE_MAM)

ETS_USE_MMA <- model(usemp,ETS(Employed ~ error("M") 
                               + trend("M") 
                               + season("A")))

accuracy(ETS_USE_MMA)

ETS_USE_MAdM <- model(usemp,ETS(Employed ~ error("M") 
                               + trend("Ad") 
                               + season("M")))

accuracy(ETS_USE_MAdM)

ETS_USE_MAdN <- model(usemp,ETS(Employed ~ error("M") 
                                + trend("Ad") 
                                + season("N")))

accuracy(ETS_USE_MAdN)

# Use ETS function to select model
USE_autoETS <- ets(usemp$Employed)
summary(USE_autoETS)

# The best model is the ETS_USE_MAdN with the smallest
#   RMSE at 265 (Associate parameters: Multiplicative
#   error, Additive trend and Multiplicative season)

# Model components

USE_autoETS_aug <- augment(ETS_USE_MAdN)
autoplot(USE_autoETS_aug, Employed) +
  autolayer(USE_autoETS_aug,.fitted, colour = "Red") +
  autolayer(USE_autoETS_aug,.resid, colour = "Green") +
  labs(y = "Count", title = "Monthly Employment",
       x = "Month")

# Graph Forecast

forc_usemp <- forecast(ETS_USE_MAdN, h='5 years', level=.80)
autoplot(forc_usemp, usemp,  colour = "Blue")  +
  labs(y = "Employed", title = "US Monthly Employment",
       x = "Month") 


#   Part 9) ETS model on the Google closing price data from 2015 only

goog2015  <- filter(gafa_stock, Symbol == "GOOG", 
                    year(Date) == 2015)
goog2015 <- mutate(goog2015, day = row_number())
goog2015 <- update_tsibble(goog2015, index = day, 
                           regular = TRUE)  
goog_ets <- ets(goog2015$Close, model = "ZZZ")
summary(goog_ets)

# Best model is ETS(M,N,N): error multiplicative, 
#   trend Null and season Null
goog_ets_MNN <- model(goog2015,ETS(Close ~ error("M") 
                                + trend("N") 
                                + season("N")))

accuracy(goog_ets_MNN)
report(goog_ets_MNN)
# Graph forcast

forc_goog <- forecast(goog_ets_MNN, h=30)
autoplot(forc_goog, goog2015,  colour = "Blue")  +
  labs(y = "USD", title = "Daily Closing Price",
       x = "Days") 

#   Part 10) Arima model on the Google closing price

goog_arima <- model(goog2015, ARIMA(Close))
goog_arima
goog_arima1 <- model(goog2015,ASa1 = ARIMA(Close ~ pdq(0,1,1)))
report(goog_arima1)
glance(goog_arima1)
accuracy(goog_arima1)

# Both techniques work well but Arima give the better model
#   with lower AIC and error (RMSE)
# The chosen model is 0 AR term, 1 difference and 1 MA term
# The Arima auto chose this model because it has lowest AIC.
#   The RMSE of this model is 11.1 

### 

