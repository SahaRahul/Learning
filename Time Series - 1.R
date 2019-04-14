## Introduction to Time Series

## Basics - Visualisation of Time Series

setwd("E:/Time Series with R/Datasets - TSA")
dir()
bakery <- readxl::read_excel("E:/Outline/WD/D1/Bakery Data.xlsx") 
head(bakery)
colnames(bakery)
windows(10,10)
par(mfrow=c(3,1))
plot.ts(bakery$Pies, main= "Sales of Pies")
plot.ts(bakery$Cookies, main= "Sales of Cookies")
plot.ts(bakery$Smoothies, main= "Sales of Smoothies")
plot.ts(bakery$Coffee, main= "Sales of cofee")



### Another variant of time series plotting - pies and cookies

windows(8,8)
par(mfrow=c(1,1))
bakery1 <- as.ts(bakery[,-c(1,4:6)])
colnames(bakery1)
class(bakery1)

ts.plot(bakery1, xlab = "Time" , ylab="Sales", lty=c(1:2), col=c("red","blue"))
legend("topleft", c("Pies","Cookies"), pch = 20, col = c("red", "blue"))

## Key woords for legend location - "bottomright", "bottom", "bottomleft", "left", 
## "topleft", "top", "topright", "right" and "center".




## Taking three products - Pies, Cookies, Coffee

bakery2 <- bakery[,-c(1,4,6)]
ts.plot(bakery2, xlab = "Time" , ylab="Sales", lty=1, lwd=c(1,2,3),col=c("red","blue","orange"))
legend(locator("leftabove"),c("Pies","Cookies","Coffee"), pch = 20, col = c("red", "blue", "orange"))

## ====================================================================================================##

## Setting Frequencies in Time Series 

# Creating Time Series Objects 

# Monthly objects

bakery_p_m <- ts(bakery$Pies, start= c(2013,1), end = c(2015,12), frequency = 12)

bakery_p_m

plot(bakery_p_m)

# Weekly Series

bakery_p_q <- ts(bakery$Pies, start = c(2013,1),end= c(2015,52),  frequency = 52)
bakery_p_q




## ===================================================================================================##
##  Dependence Structure: 
##  1. Autocorrelation , 
##  2. Cross-correlation, 
##  3. Partial - autocorrelation
##  4. Autocovariance
##  5. Cross-covariance

# 1. Autocorrelation - Pies, Smoothies

bakery <- readxl::read_excel("E:/Outline/WD/D1/Bakery Data.xlsx")
windows(8,8)
par(mfrow=c(1,1))

pies <- bakery$Pies
acf_pies <- acf(pies,type= "correlation", plot = TRUE, demean = TRUE, main="Auto-correlation - Pies")

smoothies <- bakery$Smoothies
acf(smoothies, type = "correlation", plot = TRUE, demean = TRUE, main = "Auto-correlation - Smoothies")

coffee <- bakery$Coffee
acf(coffee, type = "correlation", plot = TRUE, demean = TRUE, main = "Auto-correlation - Coffee")

# 2. Auto-covariance

windows(8,8)
par(mfrow=c(3,1))
pies <- bakery$Pies
acf(pies,type= "covariance", plot = TRUE, demean = TRUE, main="Auto-covariance - Pies")
smoothies <- bakery$Smoothies
acf(smoothies, type = "covariance", plot = TRUE, demean = TRUE, main = "Auto-covariance - Smoothies")
coffee <- bakery$Coffee
acf(coffee, type = "covariance", plot = TRUE, demean = TRUE, main = "Auto-covariance - Coffee")


# 4. Cross-correlation
windows(8,8)
par(mfrow=c(3,1))

ccf(pies, smoothies, type="correlation", main="Cross-correlation - Pies & Smoothies", plot = TRUE)

ccf(pies, coffee, type="correlation", main="Cross-correlation - Pies & Coffee", plot = TRUE)

ccf(coffee, smoothies, type="correlation", main="Cross-correlation - Coffee & Smoothies", plot = TRUE)

dr <- read.csv("Daily Return DJI.csv")
head(dr)
dr$Return <- round(((dr$Close-dr$Open)/dr$Open),4)

head(dr)
plot.ts(dr$Return, main= "Daily Return Dow Jones Index")
acf(dr$Return, type = "correlation", main = " Autoccorelation Return", demean = TRUE,plot=TRUE)

ch <- read.csv("chicken.csv")
plot.ts(ch$x, xlab="Time", ylab="Price")
acf(ch$x,type = "correlation", plot = TRUE, demean = TRUE)

## Signal & Noise

cs = 2*cos(2*pi*1:500/50 + .6*pi)
w = rnorm(500,0,1) ## white noise 

windows (10,10)
par(mfrow=c(3,1))

plot.ts(cs, main = "without White Noise")
plot.ts(cs+w,main = "White noise with Std.Dev=1")
plot.ts (cs+5*w, main = "White noise with Std.Dev =5")

grid(nx=26, ny=20)
acf(cs, type = "correlation", plot = TRUE, demean = TRUE)
mtext("Confidence Interval Lines", side=4)
legen("left","Confidence")

acf(cs+w, type = "correlation", plot = TRUE, demean = TRUE)
acf(cs+5*w, type = "correlation", plot = TRUE, demean = TRUE)


## ==========================================================================================##



## Smoothening - Techniques
## 1. Moving Average
## 2. Lowess function
## 3. Loess function
## 4. Kernel Smoothening 
## 5. Spline

## Ref:
# https://www.itl.nist.gov/div898/handbook/pmd/section1/pmd144.htm 
# Moving Average , Rob J Hyndman

## Understanding  Moving Average
## Among the arguments of filter two are most important, "filter" and "method" 
## "filter" which decides coeefficients to be used for moving average (MA)
## "method" which is "convolution" for MA and "recursive" for Autoregression
## For "sides" you can select 1 and 2.
## For sides=1, indicates past values will be considerd. This suitable for prediction
## For sides=2, future values are selected. this is suitable for smoothening.
## Usual Types of MA - Simple, Centered and Weighted.
## For the time series having trend we may do the following -
# 1. Double Moving Average
# 2. Double Exponential
# 3. Holt-Winters Additive & Multiplicative Models
# 4. Regreession - Linear Quadratic Trend models


## Computing a simple two period moving average 
par(mfrow=c(1,1))

x<- 1:10

two_ma <- filter(x, filter = c(1/2,1/2), sides = 1, method = "convolution", circular = TRUE) 
two_ma
## Three period moving average 

three_ma <- filter(x, filter = c(1/3,1/3,1/3), sides = 1, method = "convolution", circular = TRUE)

together <- cbind(x, two_ma, three_ma)
together

## if circular provision is "FALSE"

two_ma1 <- filter(x, filter = c(1/2,1/2), sides = 1, method = "convolution", circular = FALSE)
three_ma1 <- filter(x,filter = c(1/3,1/3,1/3), sides = 1, method = "convolution", circular = FALSE)

together1 <- cbind(x, two_ma1, three_ma1)
together1

## if sides=2

two_ma2 <- filter(x, filter = c(1/2,1/2), sides = 2, method = "convolution", circular = FALSE)
three_ma2 <- filter(x, filter = c(1/3,1/3,1/3), sides = 2, method = "convolution", circular = FALSE)

together2 <- cbind(x, three_ma1, three_ma2)
together2

## Basic Analysis - with Stationary Data


ma <- readxl::read_xlsx("E:/Time Series with R/D1/SMA.xlsx")

plot.ts(ma$`Sales of DVD`, ylab="Sales of DVD")
dim(ma) # Note the length is even number which calls for centering 

ma_2 <- filter(ma$`Sales of DVD`, filter = rep(1/5,5), sides = 1, method = "convolution", circular = FALSE)
head(bd_2)

ma_2.1 <- filter(ma$`Sales of DVD`, filter = rep(1/5,5), sides = 2, method = "convolution", circular = FALSE)
lines(ma_2, col="red", lwd=2)
lines(ma_2.1, col="blue", lwd=2)
grid(nx=24,ny=10)

head(bd_2.1)
length(bd_2.1)

cbind(ma$`Sales of DVD`,ma_2,ma_2.1)



ma_4 <- filter(ma$`Sales of DVD`, filter = c(1/4,1/4,1/4,1/4), sides = 1, method = "convolution", circular = FALSE)
head(bd_3)

ma_4.1 <- filter(ma$`Sales of DVD`, filter = c(1/4,1/4,1/4,1/4), sides = 2, method = "convolution", circular = FALSE)
ma_4.1.2 <- filter(ma_4.1, filter = c(1/2,1/2), sides = 2, method = "convolution", circular = FALSE)

cbind(ma$`Sales of DVD`,ma_4.1, ma_4.1.2)

plot(ma$Month,ma$`Sales of DVD`, type = "l")

lines(ma_4, col="blue")
lines(ma_4.1, col="red")

ma_5 <- filter(ma$`Sales of DVD`, filter = rep(1/5,5), sides = 1, method = "convolution", circular = FALSE)
ma_5.1 <- filter(ma$`Sales of DVD`, filter = rep(1/5,5), sides = 2, method = "convolution", circular = FALSE)

lines(ma_5, col="green", lwd=2)
lines(ma_5.1, col="cyan", lwd=2)

grid(24,10)

# Exponential average

par(mfrow=c(1,1))

ma <- readxl::read_xlsx("E:/Time Series with R/D1/SMA.xlsx")
ma_2 <- filter(ma$`Sales of DVD`, filter = rep(1/5,5), sides = 1, method = "convolution", circular = FALSE)
plot.ts(ma_2)
 

lamda <- .9
  e<-0
 
for(i in 1:Inf){
  e[i] <- lamda^i
}
  
e
par(mfrow=c(2,1))

ma_exp = filter(ma$`Sales of DVD`, filter = e, sides = 1, method = "convolution")

plot.ts(ma_exp)
plot.ts(ma_2)



# Accuracy - Errors
# Mean Absolute Deviation (MAD)
# Mean Absolute Percent Error (MAPE)
# Mean Squre Error (MSE)

length(bd_2)
mad_2 <- ma$`Sales of DVD`[c(2:24)] - ma_2
ma_2_na <- na.omit(ma_2)



bd1[2]
bd_2.1[1]
mad_3 <- err1$bd1 - err1$bd_3)

mape_2 <- (err1$bd1 - err1$bd_2)/ err1$bd1*100
mape_2

## Two period & three period MA - Pies , Bakery dataset - Practice

windows(10,10)

plot.ts(bakery$Pies,main="Sales of Pies")
pies_2ma <- filter(bakery$Pies, filter = c(1/2,1/2), sides = 1, method = "convolution", circular = TRUE)
lines(pies_2ma, col="red")

windows(10,10)
pies_3ma <- filter (bakery$Pies, filter = c(1/3,1/3,1/3), sides = 1, method ="convolution", circular = TRUE)
lines(pies_3ma, col="blue")

windows(10,10)
plot.ts(bakery$Pies,main="Sales of Pies")
pies_10ma <- filter (bakery$Pies, filter = rep(1/25,25), sides = 1, method ="convolution", circular = TRUE)
lines(pies_10ma, col="yellow", lwd=3)

## keep on increasing the MA periods and see the smoothening effect in MAlines

## Smootheing - smoothies 

windows(10,10)
par(mfrow=c(3,1))
plot.ts(bakery$Smoothies, main ="Sales of Smoothies")

 acf(bakery$Smoothies, type = "correlation", plot = TRUE, demean = TRUE)


grid(nx=30, ny=20)
text(4,.5, "correlaton is min at lag-4", pos = 3)

## pos -> position , 1 stands for "below", 2 for "above", 3 for "above" and 4 for "right" of the coordinates

smoothies_90ma <- filter(bakery$Smoothies, filter = rep(1/90,90), sides = 1, method = "convolution",circular = TRUE) 
smoothies_120ma <- filter(bakery$Smoothies, filter = rep(1/120,120), sides = 1, method = "convolution",circular = TRUE) 
lines(smoothies_120ma, col="blue", lwd=3)
lines(smoothies_90ma, col="red", lwd=2)


## Adding A LoWESS smoother
## its a non-parametric smoothening function very suitable for regressing non-linear functions


windows(6,6)
plot.ts(bakery$Smoothies)
lines(lowess(bakery$Smoothies, f=.5, iter = 3), col="red", lwd=1.5) ## default values

windows(6,6)
plot.ts(bakery$Smoothies)
lines(lowess (bakery$Smoothies, f=.2, iter = 3),col="blue", lwd=2) ## f - value has been reduced

windows(6,6)
plot.ts(bakery$Smoothies)
lines(lowess (bakery$Smoothies, f=.8, iter = 3),col="yellow", lwd=2.5) ## f-value has beenincreased

windows(6,6)
plot.ts(bakery$Smoothies)
lines(lowess (bakery$Smoothies, f=.8, iter = 7),col="cyan", lwd=4)

## its apparent that higher f value the curve is becoming a straight line
## "f" basically indicates the proportion of points used for regression 
## lesser its value better it tracks the fluctuation, but too less a value is alos not recommended


## Using Loess - it helps to add smoothening function for multivariate dataset
## https://en.wikipedia.org/wiki/Loess 

windows(8,8)

chk <- read.csv("E:/Time Series with R/Datasets - TSA/chicken.csv")
windows(10,10)
plot.ts(chk$x)
chk$

mod_lowess1 <- loess(chk$x~chk$X, span = .2, degree = 2)
mod_lowess1

predict_lowess <- predict(mod_lowess1)

class(predict_lowess1)

lines(chk$X, predict_lowess, col="red")

## another model - reducing the span to .4

mod2_lowess <- loess(chk$x~chk$X, span = .4, degree = 2)

predict_lowess2 <- predict(mod2_lowess)

lines(chk$X, predict_lowess2, col="blue")

## model-3 - reducing the span to 0.2

mod3_lowess <- loess(chk$x~chk$X, span = .2, degree = 2)

predict_lowess3 <- predict(mod3_lowess)

lines(chk$X, predict_lowess3, col="yellow")


## Kernel Smoothening - another non-parametric smoothening method

## Like span in lowess, f value is lowess , kernel has "bandwidth parameter" 
## Higher the bandwidth, smoother is the curve
## https://en.wikipedia.org/wiki/Kernel_smoother 
## The wider the bandwidth, b, the smoother the result. From the R ksmooth
## The kernels are scaled so that their quartiles (viewed as probability densities) are at ± 0.25*bandwidth.

windows(8,8)
plot.ts(bakery$Smoothies)
quantile(time(bakery$Smoothies))
quantile(bakery$Smoothies)
lines(ksmooth(time(bakery$Smoothies),bakery$Smoothies, kernel = "normal", bandwidth = 50), lwd=2,col="orange")
lines(ksmooth(time(bakery$Smoothies),bakery$Smoothies, kernel = "normal", bandwidth = 165), lwd=2.5,col="cyan")
lines(ksmooth(time(bakery$Smoothies),bakery$Smoothies, kernel = "normal", bandwidth = 660), lwd=3,col="red")




## Smoothing - Splines
## Another way to smooth data would be to fit a polynomial regression in terms of time
## Cubic Polynomial - m(t)= b0 + b1*t + b2*t^2 + b3*t^3 and xt = w(t) + m(t)
## m(t) is the smoothening curve and w(t) is the noise

# In this case entire is time line is splitted in several sections 
# and for each section polynormial regression is done where there is control on departure from m(t).
## That is (x(t)-m(t)) and smoothness (curvature of m(t))
## The parameter controlling the smoothness of spline is known as spar.
## The value of spar is between 01 to 1. When it is 0 , there is no smoohening effect

windows(8,8)

plot.ts(bakery$Smoothies)
lines(smooth.spline(bakery$Smoothies, spar = .5), lwd=2, col="red")
lines(smooth.spline(bakery$Smoothies, spar = 0), lwd=2, col="yellow")

lines(smooth.spline(bakery$Smoothies, spar = .8), lwd=2, col="blue")


## Compararison - Smoothening methods with default values

windows(8.8)
quantile(chk$x)

plot.ts(chk$x)
lines(smooth.spline(chk$X,chk$x, spar = .5), lwd=2, col="red")
lines(smooth.spline(chk$X,chk$x, spar =.2), lwd=2, col="blue")


windows(8.8)
plot.ts(chk$x)

lines(ksmooth(time(chk$x), chk$x, kernel = "normal", bandwidth = 100),lwd=2, col="yellow") 
lines(ksmooth(time(chk$x), chk$x, kernel = "normal", bandwidth = 50),lwd=2, col="green")

windows(8.8)
plot.ts(chk$x)
chk_model1<- loess(chk$x~chk$X,span = 0.3, degree = 2)
chk_model2<- loess(chk$x~chk$X,span =0.5, degree = 1)

chk_predict1 <- predict(chk_model1)
chk_predict2 <- predict(chk_model2)


lines(chk$X,chk_predict1, col="cyan", lwd=2)
lines(chk$X,chk_predict2, col="pink", lwd=2)

##=========================================================================================##

## lm model - Autoregression Regression - accessing different elements

# summary about the regresssion model 
# it describes the coefficient of dertermination (model fit)-
# statistical significance of each independent variables




## Auto-regression - pies 

t_pies <- as.ts(bakery$Pies)
lag1_p <- as.ts(diff(bakery$Pies, lag=1))
lag2_p <- as.ts(diff(bakery$Pies, lag=2))
lag3_p <- as.ts(diff(bakery$Pies, lag=3))


## the length of R-objects with various lags will be of different length
## with objects / bariables of different lengeths regression can not be done
## to make a dataframe of "common length" ts.union() is used

bake <- ts.union(t_pies, lag1_p,lag2_p,lag3_p, dframe = TRUE)
a_reg1 <- lm(t_pies~lag1_p, data = bake)
summary(a_reg1)


a_reg12 <- lm(t_pies~lag1_p+lag2_p, data = bake)
summary(a_reg12)

a_reg123 <- lm(t_pies~lag1_p+lag2_p+lag3_p, data = bake)

summary(a_reg123)


## regression with lagged variables

sm_1 <- lag(bakery$Smoothies, - 1)
sm_2 <- lag(bakery$Smoothies, - 2)
sm_3 <- lag(bakery$Smoothies, 3)

head(sm_3)
head(bakery$Smoothies)

sm123 <- cbind(bakery$Smoothies, sm_1, sm_2, sm_3)
head(sm_1)

head(sm123)



