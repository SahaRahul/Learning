# An Example - Time Series

d <- read.table("E:/Time Series with R/D4/EPS.txt", header = TRUE)

# df = read.table("C:/Users/rahul/Google Drive/IISWBM Study Materials/Time series/From Subhojit Sir/Final Mail from 2018_9_22/EPS.txt", header = T)


View(d)

head(d)
tail(d)
dim(d)
class(d)


# Description of dataset: It captures information of Earning per share. 
# The information is from first quarter of 1983 to last quarter of 2009.
# Therefore, information of 107 quarters is present in the dataset
# still the object is a "dataframe" , but not a time series

plot(d$value, type = "l")

# from the plot we can see an upward trend and consistent fluctation
# there is a repititive fluctuation - which is indicative of seasonality
# the series is not a stationary one

# CREATION OF A TIME SERIES OBJECT 

v <- ts(d$value, start=c(1983,1),end=c(2009,3), frequency = 4)

class(v)
plot(v)


# DECOMPOSING TIME SERIES

# there 3 elements to flcutuation - trend, seasonality, and residual (noise)
# in this case we can observe an upward trend and presence of seasonality

decom.v = decompose(v, type = "additive")

class(decom.v)

names(decom.v)

# names of decom.v indicates that it contains "seasonal","trend" and "random"
# further the aforesaid can be plotted - let us plot 

windows(10,10)

par(mfrow=c(4,1))

plot(d$value, main="Original Data", type = "l")

plot(decom.v$seasonal, xaxp=c(1983,2009,17), cex.axis=0.2, main="Seasonality")

plot(decom.v$trend, main="Trend")

plot(decom.v$random, main="Residuals - Noise")


# Putting the pieces together - seasonal & Trend

t_s = decom.v$trend+decom.v$seasonal

head(t_s)


t_s = replace(t_s, list=c(is.na(t_s)==TRUE),0) 

# there are NA values which needs to be removed for plotting purpose


par(mfrow=c(1,1))
plot(v, main="Original Data", type = "l", xlim = c(1983,2009))
lines(t_s, main =" Trend and Seasonla Components", col="red", type="l")



# Seasonality , ACF with lagged variables and Plotting of ACF

# diff() function  estimates difference from the previous value.
# for time series object one can define the lag - default value of lag is 1 i.e. x(t) - x(t-1)
# similarly diff(x,4) -> x(t) - x(t-4) 
# by taking difference we can remove the trend part and seasonality is extracted. 
# ACF indicates towards order of MA and PACF towards order of AR 
# In ARMA(p,d,q) , "p" -> order of AR, d -> difference , q-> order of MA 

windows(10,10)
par(mfrow=c(4,1))


plot(diff(log(v)), main="lag1")
plot(diff(log(v),2), main="lag 2")
plot(diff(log(v),3), main="lag 3")
plot(diff(log(v),4), main="lag 4") 

# seasonal component - 4 datapoints completes a season as these are quaterly "EPS".
# There can be following issues 
## a. Both trend & seasonality present - then take a seasonal difference. plot the difference term , if still trend remains , take another difference.
## b. Only Trend & no seasonlaity - take difference of lag 1
## c. Only seasonlaity & no trend - taken seasonla difference


## Examining the ACF and PACF of Difference terms
## In this case after taling seasonal difference (lag 4) no trend  is visible

windows(12,10)
par(mfrow=c(3,2))


plot(acf(log(d$value)),main="ACF of Original Variable")

plot(acf(diff(log(d$value))), main= "ACF with Difference of lag 1")

# such significant (beyond CI) ACF both positive or negative indicates about seasonlaity 

plot(pacf(diff(log(d$value),4)),main="examing PACF of difference terms") # PACF is marginally significant at lag 1

plot(acf(diff(log(d$value),4)), main= "ACF with Difference of lag 4") # ACF close to 0 at 4

plot(pacf(diff(log(v),4))) 


# Regression and Moving average can be used to detect the trend 
# A glimpse of the same - overlaying a moving average of 4 period
# But moving average does not allows to estimate the residuals and its propoerties

ma4= filter(d$value, filter = rep(1/4,4), method = "convolution")
plot(d$value, main="Original Data", type = "l")
lines(ma4, col="red")

# A glimpse of regression with time 


plot.new()
plot(d$value, type="l", main = "Original and Fitted Values")
t_r=lm(d$value~time(d$value))
lines(t_r$fitted.values, col="red")

# to access all the plots relevant for regression use plot(t_r)
# the properties of residuals can be explored as well



# Examining the correlation structure - ACF and PACF
# the maximum lag that shoould be looked at is 10*logN i.e.10*log(107)=4.67*10 = 47

acf(d$value, type = "correlation") 

# One can perform the Porform Box-pierce test OR Portmanteu test 
# by default the lad value is 1 , it can be changed to the desired level

Box.test(d$value, type = "Box-Pierce")
Box.test(d$value, type = "Ljung-Box")

# Given the 'p-value' the Null Hypothesis of Independence is rejected , which also points towards trend.

# say the lag is set at 4 

Box.test(d$value, lag = 4, type = "Ljung-Box")



# It can be noted R by default is estimating ACF with a lag of 20 period. 
# Examining non-seasonal behavior of ACF for the maximum possible lag without looking at seasonality 

plot.new()
acf(d$value, type="correlation",lag.max = 40, main="ACF at Various Lags",xaxp=c(0,40,10))


# from the plot we can note that around 40th period ACF becomes 0
# From the waves of the ACF we can observe some seasonality
# Further we may also note that ACF remains significant till 28

# let us store the ACF object

o_acf = acf(d$value, type = "correlation", lag.max = 40)
o_acf$acf[20:30]




# from the above values it can be noted that the minimum value of ACF is at lag 40

# Examining Partial Auto-correlation

partial_acf= pacf(d$value, main="Partial Auto-correlations at Different Lags")

plot(partial_acf$acf, type = "l",main="Partial Auto-correlations at Different Lags", xaxp=c(0,20,20))

abline(h=0, col="blue", lwd=2)

partial_acf$acf[0:10]

# It can be noted that around partial auto-correlation becomes 0 between lag 0 to 5.
# At lag 2 only it closest to 0

# to understand the relation structure with past values we are looking at auto-regression models

auto_reg = ar(d$value, method = "mle", aic = TRUE)

auto_reg$order

auto_reg$ar # Auto-regression coefficient 

plot(auto_reg$aic, type="l")

abline(h=0, col="green")

# Tests for stationarity - ADF test
# Stationarity is checked with Unit Root test - call library(FunitRoots)

library("fUnitRoots")

# Please note already obtained that order of AR is 10
# In the test the NUll Hypothesis is series is "NON-Stationary"

adfTest(d$value, lags = 10)

# from the p-value, it can be noted that the series not stationary



# Given that Auto-regression indicates that the at lag 10 AIC is 0.

# ARIMA Model: 
# We have noted that order of AR is captured by PACF and that of MA is captured by ACF
# Now refer back to the ACF and PACF of the difference terms (diff()) 
# Exaploring some of the models

plot(pacf(diff(log(d$value),4)),main="examing PACF of difference terms") # PACF is marginally significant at lag 1

plot(acf(diff(log(d$value),4)), main= "ACF with Difference of lag 4") # ACF close to 0 at 4



model_arma1 = arima(log(d$value), order = c(1,1,0), seasonal = list(order=c(1,1,0),period=4))
model_arma1$coef
model_arma1$aic

arma1_resi <- model_arma1$residuals

hist(arma1_resi) # visualising normality of residuals

qqplot(log(d$value),arma1_resi)

ks.test(arma1_resi, "pnorm") # test of normality of residuals

Box.test(arma1_resi, lag=20) # examining null hypothesis of indepedence


model_arma2 = arima(log(v),order=c(0,1,1), seasonal = list(order=c(0,1,1), period=4))

# Accessing the coefficients

model_arma2$coef
model_arma2$aic # AIC values have reduced further

qqplot(log(d$value),model_arma2$residuals) # visualising normality

ks.test(model_arma2$residuals, "pnorm") # satisfies normality condition

Box.test(model_arma2$residuals) # satisfies independence condition

# Now we would like to apply this model and 

values_predicted <- predict(model_arma2,107)

plot(values_predicted$pred) 
plot(log(d$value),type="l", col="red")



