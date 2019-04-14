## Moving Averages

DJI <- read.csv("E:/Time Series with R/Datasets - TSA/Daily Return DJI.csv") # Stationary ? 
## TIME PERIOD - APRIL 2006 TO APRIL 2016 - DAILY MOVEMENT


bakery <- readxl::read_excel("E:/Outline/WD/D1/Bakery Data.xlsx")
sm <- bakery$Smoothies # Stationary ?

dim(DJI)
head(DJI)

## EMA - SMA

lamda  = .9
lamda = .5

d <-0

# decay factor - n = 50

for(t in 1:200){
  d[t]=lamda^t
}

head(d)

windows(8,8)
par(mfrow=c(3,1))

plot.ts(sm)

ema200 = filter(sm, filter = d,method = "convolution", sides=1)

ema1 <-EMA(sm,200)

lines(ema1, col="blue")
plot.ts(ema200)

ma200 = filter(sm, filter = rep(1/200,200),method = "convolution", sides=1)
ma1 <- SMA(sm,200)

lines(ma200, col="green")
lines(ma1, col="red")
plot.ts(ma200)



ma_11 <- filter(sm, filter = rep(1/11,11), sides = 2, method = "convolution")
tma_11 <- filter(ma_11, filter = rep(1/11,11), sides=2, method="convolution")
lines(ma_11, col="red", lwd=1.5)
lines(tma_11, col="blue", lwd=2)



plot.ts(sm)
tma_11_11 <- filter(tma_11, filter = rep(1/11,11), sides=2, method="convolution")
tma_11_11_11 <- filter(tma_11_11, filter = rep(1/11,11), sides=2, method="convolution")


lines(tma_11_11, col="green", lwd=3)
lines(tma_11_11, col="yellow", lwd=3)

head(cbind(ma_11,tma_11,tma_11_11,tma_11_11_11),120)

# Example-1

dm <- readxl::excel_sheets("E:/Time series with R/SMA Stationary.xlsx")


dm <- readxl::excel_sheets("E:/Time series with R/D1/SMA Stationary.xlsx")
head(dm)


dm1 <- readxl::read_xlsx("E:/Time series with R/D1/SMA Stationary.xlsx", sheet = "Sheet2")
head(dm1)
dim(dm1)
## 24 month sales - which is stationary is nature

windows(8,8)
plot.ts(dm1$Sales)

 de_dm1 <- DEMA(dm1$Sales)
 plot.ts(de_dm1)
 plot.ts(dm1$Sales)
 lines(de_dm1, col="red")
 
 de_dm1 <- DEMA(dm1$Sales, n=4)
 lines(de_dm1, col="red")
 
 sm_dm1 <- SMA(dm1$Sales, n=4)
 lines(sm_dm1, col="blue")
 em_dm1 <- EMA(dm1$Sales, n=4)
 lines(em_dm1, col="green")

  zlma_dm1 <- ZLEMA(dm1$Sales,n=4)
 lines(zlma_dm1, col="pink", lwd=3)



# Example-2

DJI <- read.csv("E:/Time Series with R/Datasets - TSA/Daily Return DJI.csv")
plot.ts(DJI$High)
dim(DJI)


library("TTR")
sma_ma <- SMA(DJI$Close, n=150)
exp_ma <- EMA(DJI$Close,n=150)

lines(exp_ma, col="red")
lines(sma_ma, col="blue")




z_ma <- ZLEMA(DJI$Close, n=150)
lines(z_ma, col="green", lwd=2)

hull_ma <- HMA(DJI$Close, n=150)
lines(hull_ma, col="red")

legend("topleft", c("HMA","ZMA","SMA"), pch=10,col =c("red", "green","blue"))


## Stationart Series - Prediction

dm <- readxl::excel_sheets("E:/Time series with R/SMA Stationary.xlsx")

dm1 <- readxl::read_xlsx("E:/Time series with R/D1/SMA Stationary.xlsx", sheet = "Sheet2")
head(dm1)
dim(dm1)

## 24 month sales - which is stationary is nature

mean_sales <- mean(dm1$Sales)
abline(h=mean_sales,v=0)

# Extracting quaterly data

qtr<-0

for(i in 1:nrow(dm1)){
  qtr[i]= if(dm1$`Time Period (t)`%/%3<=1 & dm1$`Time Period (t)`<=12| dm1$`Time Period (t)`%/%3 == 3 & dm1$`Time Period (t)`>12) 1 else
    if(dm1$`Time Period (t)`%/%3 <= 2 & dm1$`Time Period (t)`<=12 | dm1$`Time Period (t)`%/%3 == 4 & dm1$`Time Period (t)`>12)  2 else
      if(dm1$`Time Period (t)`%/%3 == 3 & dm1$`Time Period (t)`<=12 | dm1$`Time Period (t)`%/%3 == 4 & dm1$`Time Period (t)`>12)  3 else
        
      
}

qtr