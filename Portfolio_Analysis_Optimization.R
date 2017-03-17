#OR 538 Project Divya Aigal and Raghuraj Sawant

#Loading all packages
library(corrplot)
library(MASS)
library(quadprog)
library(fBasics)
library(quantmod)

# read the data
# Ford data
ford_data <- read.csv("F_HistoricalQuotes.csv")
# Google data
google_data <- read.csv("GOOGL_HistoricalQuotes.csv")
# Pfizer data
pfizer_data <- read.csv("PFE_HistoricalQuotes.csv")
# Walmart data
walmart_data <- read.csv("WMT_HistoricalQuotes.csv")
# Exxon data
exxon_data <- read.csv("XOM_HistoricalQuotes.csv")
# Apple data
apple_data <- read.csv("AAPL_HistoricalQuotes.csv")

#calculate the returns

n = length(ford_data$close[-1])
# converting to percentages
ford.rtn =  100 * (ford_data$close[-1][2:n ] / ford_data$close[-1][1:(n-1) ] - 1) 
google.rtn = 100 * (google_data$close[-1][2:n ] / google_data$close[-1][1:(n-1) ] - 1)
pfizer.rtn = 100 * (pfizer_data$close[-1][2:n ] / pfizer_data$close[-1][1:(n-1) ] - 1)
walmart.rtn = 100 * (walmart_data$close[-1][2:n ] / walmart_data$close[-1][1:(n-1) ] - 1)
exxon.rtn = 100 * (exxon_data$close[-1][2:n ] / exxon_data$close[-1][1:(n-1) ] - 1)
apple.rtn = 100 * (apple_data$close[-1][2:n ] / apple_data$close[-1][1:(n-1) ] - 1)

#plot the returns
plot(ford.rtn)
plot(google.rtn)
plot(pfizer.rtn)
plot(walmart.rtn)
plot(exxon.rtn)
plot(apple.rtn)

#Time Series plots
year = seq(2005+231/253,2015+184/253,1/253)
length(year) #2484

# Plots for all the 5 stocks
plot(year,ford.rtn,ylab="ford returns",type="l",xlab="year",cex.lab=1.5,
     cex.axis=1.5,cex.main=1.3, main= "Ford time series")
plot(year,google.rtn,ylab="google returns",type="l",xlab="year",cex.lab=1.5,
     cex.axis=1.5,cex.main=1.3, main= "Google time series")
plot(year,pfizer.rtn,ylab="pfizer returns",type="l",xlab="year",cex.lab=1.5,
     cex.axis=1.5,cex.main=1.3, main= "Pfizer time series")
plot(year,walmart.rtn,ylab="walmart returns",type="l",xlab="year",cex.lab=1.5,
     cex.axis=1.5,cex.main=1.3, main= "Walmart time series")
plot(year,exxon.rtn,ylab="exxon returns",type="l",xlab="year",cex.lab=1.5,
     cex.axis=1.5,cex.main=1.3, main= "Exxon time series")
plot(year,apple.rtn,ylab="apple returns",type="l",xlab="year",cex.lab=1.5,
     cex.axis=1.5,cex.main=1.3, main= "Apple time series")

#Binding all 5 stock return data
stocksData = cbind(ford.rtn, pfizer.rtn, walmart.rtn, exxon.rtn, google.rtn)

#Binding stock return data also considering Apple
stocksData_6 = cbind(ford.rtn, pfizer.rtn, walmart.rtn, exxon.rtn,  google.rtn, apple.rtn)

cor(stocksData)
cov(stocksData)
corrplot(cor(stocksData))
pairs(stocksData)

#Plotting the efficient frontier for the risky assets only i.e.the stocks

mean_vect = apply(stocksData,2,mean)
cov_mat = cov(stocksData)
sd_vect = sqrt(diag(cov_mat))
M = length(mean_vect)
Amat = cbind(rep(1,M),mean_vect)  # set the constraints matrix

muP = seq(min(mean_vect)-0.05,max(mean_vect)+0.05,length=300)  # set of 300 possible target values 
# for the expect portfolio return1
sdP = muP # set up storage for standard deviations of portfolio returns
weights = matrix(0,nrow=300,ncol=M) # storage for portfolio weights

# for each expected return (constraint), find the minimum risk/variance portfolio using
# quadratic programming
for (i in 1:length(muP))  # find the optimal portfolios for each target expected return
{
  bvec = c(1,muP[i])  # constraint vector
  result = 
    solve.QP(Dmat=2*cov_mat, dvec=rep(0,M), Amat=Amat, bvec=bvec, meq=2)
  sdP[i] = sqrt(result$value)
  weights[i,] = result$solution
}

plot(sdP,muP,type="l",xlim=c(min(sdP)-1.1,max(sdP)+1.1),ylim=c(min(muP)-0.01,max(muP)+0.01), 
     xlab = "Standard Deviation in percent", ylab = "Mean value", lwd= 2,lty=1)  #  plot 
# the efficient frontier (and inefficient frontier)
sharpe = muP/sdP # compute Sharpe ratios
ind = (sharpe == max(sharpe)) # Find maximum Sharpe ratio
options(digits=3)
weights[ind,] # Find tangency portfolio# show line of optimal portfolios
# show line of optimal portfolios
ind2 = (sdP == min(sdP)) # find the minimum variance portfolio
points(sdP[ind2],muP[ind2],cex=2,pch="+") # show minimum variance portfolio
text(sdP[ind2], muP[ind2]+0.010,'MVP', cex=1)  # plot the minimum variance portfolio
ind3 = (muP > muP[ind2])
lines(sdP[ind3],muP[ind3],type="l",xlim=c(min(sdP)-1.1,max(sdP)+1.1),
      ylim=c(min(muP)-0.01,max(muP)+0.01),col= "red",lwd=2)  #  plot the efficient frontier
text(sd_vect[1],mean_vect[1]-0.003,"Ford",cex=1)
points(sd_vect[1],mean_vect[1],cex=2,col = "purple",pch="*")
text(sd_vect[2],mean_vect[2]-0.003,"Pfizer",cex=1)
points(sd_vect[2],mean_vect[2],cex=2,col = "purple",pch="*")
text(sd_vect[3],mean_vect[3]-0.003,"Walmart",cex=1)
points(sd_vect[3],mean_vect[3],cex=2,col = "purple",pch="*")
text(sd_vect[4],mean_vect[4]+0.003,"Exxon",cex=1)
points(sd_vect[4],mean_vect[4],cex=2,col = "purple",pch="*")
text(sd_vect[5],mean_vect[5]-0.003,"Google",cex=1)
points(sd_vect[5],mean_vect[5],cex=2,col = "purple",pch="*")

###########################################################################

#Plotting the efficient frontier for short selling 
#Short selling is allowed
mean_vect = apply(stocksData,2,mean)
cov_mat = cov(stocksData)
sd_vect = sqrt(diag(cov_mat))
M = length(mean_vect)
Amat = cbind(rep(1,M),mean_vect)  # set the constraints matrix

muP = seq(min(mean_vect)-0.05,max(mean_vect)+0.05,length=300)  # set of 300 possible target values 
# for the expect portfolio return
sdP = muP # set up storage for standard deviations of portfolio returns
weights = matrix(0,nrow=300,ncol=M) # storage for portfolio weights

# for each expected return (constraint), find the minimum risk/variance portfolio using
# quadratic programming
for (i in 1:length(muP))  # find the optimal portfolios for each target expected return
{
  bvec = c(1,muP[i])  # constraint vector
  result = 
    solve.QP(Dmat=2*cov_mat, dvec=rep(0,M), Amat=Amat, bvec=bvec, meq=2)
  sdP[i] = sqrt(result$value)
  weights[i,] = result$solution
}

plot(sdP,muP,type="l",xlim=c(min(sdP)-1.1,max(sdP)+1.1),ylim=c(min(muP)-0.01,max(muP)+0.01),
     xlab = "Standard Deviation in percent", ylab = "Mean value", lwd= 2,lty=1)  #  plot 
# the efficient frontier (and inefficient frontier)
mufree = 0.0033 # input value of risk-free interest rate = 3 Month treasury bill
points(0,mufree,cex=3,pch="*")  # show risk-free asset
text(0.25, mufree-0.005,'Risk Free Asset', cex=1.2)  # plot the Risk Free Asset
sharpe =( muP-mufree)/sdP # compute Sharpe ratios
ind = (sharpe == max(sharpe)) # Find maximum Sharpe ratio
options(digits=3)
weights[ind,] # Find tangency portfolio# show line of optimal portfolios
lines(c(0,max(sdP)+2),mufree+c(0,max(sdP)+2)*(muP[ind]-mufree)/sdP[ind],col="blue",lwd=3,lty=3)
# show line of optimal portfolios
points(sdP[ind],muP[ind],cex=3,pch="*") # show tangency portfolio
text(sdP[ind],muP[ind]-0.005,'Tangency Portfolio', cex=1)  # plot the tangency portfolio
ind2 = (sdP == min(sdP)) # find the minimum variance portfolio
points(sdP[ind2],muP[ind2],cex=2,pch="+") # show minimum variance portfolio
text(sdP[ind2], muP[ind2]+0.010,'MVP', cex=1)  # plot the minimum variance portfolio
ind3 = (muP > muP[ind2])
lines(sdP[ind3],muP[ind3],type="l",xlim=c(min(sdP)-1.1,max(sdP)+1.1),
      ylim=c(min(muP)-0.01,max(muP)+0.01),col= "red",lwd=2)  #  plot the efficient frontier
text(sd_vect[1],mean_vect[1]-0.003,"Ford",cex=1)
points(sd_vect[1],mean_vect[1],cex=2,col = "purple",pch="*")
text(sd_vect[2],mean_vect[2]-0.003,"Pfizer",cex=1)
points(sd_vect[2],mean_vect[2],cex=2,col = "purple",pch="*")
text(sd_vect[3],mean_vect[3]-0.003,"Walmart",cex=1)
points(sd_vect[3],mean_vect[3],cex=2,col = "purple",pch="*")
text(sd_vect[4],mean_vect[4]+0.003,"Exxon",cex=1)
points(sd_vect[4],mean_vect[4],cex=2,col = "purple",pch="*")
text(sd_vect[5],mean_vect[5]-0.003,"Google",cex=1)
points(sd_vect[5],mean_vect[5],cex=2,col = "purple",pch="*")

#####################################################################################
#Plotting the efficient frontier for NO short selling, w>=0
mean_vect_no = apply(stocksData,2,mean)
cov_mat_no = cov(stocksData)
sd_vect_no = sqrt(diag(cov_mat_no))
M = length(mean_vect_no)
Amat_no = cbind(rep(1,M),mean_vect_no, diag(1,nrow=M))  # set the constraints matrix
muP_no = seq(min(mean_vect_no)+0.01,max(mean_vect_no)-0.01 ,length=300)  # set of 300 possible target values 
# for the expect portfolio return1
sdP_no = muP_no # set up storage for standard deviations of portfolio returns
weights = matrix(0,nrow=300,ncol=M) # storage for portfolio weights

# for each expected return (constraint), find the minimum risk/variance portfolio using
# quadratic programming
for (i in 1:length(muP_no))  # find the optimal portfolios for each target expected return
{
  bvec_no = c(1, muP_no[i], rep(0,M)) # constraint thresholds: no short selling, min weights are zero
  result = 
    solve.QP(Dmat=2*cov_mat_no, dvec=rep(0,M), Amat=Amat_no, bvec=bvec_no, meq=3)
  sdP_no[i] = sqrt(result$value)
  weights[i,] = result$solution
}

plot(sdP_no,muP_no,type="l",xlim=c(min(sdP_no)-1.8,max(sdP_no)+1.8),ylim=c(min(muP_no)-0.08,max(muP_no)+0.08), 
     xlab = "Standard Deviation in percent", ylab = "Mean value", lwd= 2,lty=1)  #  plot 
# the efficient frontier (and inefficient frontier)
mufree = 0.0033  # input value of risk-free interest rate = 3 Month treasury bill
points(0,mufree,cex=3,pch="*")  # show risk-free asset
text(0.25, mufree-0.005,'Risk Free Asset', cex=1.2)  # plot the Risk Free Asset
sharpe = (muP_no-mufree)/sdP_no # compute Sharpe ratios
ind = (sharpe == max(sharpe)) # Find maximum Sharpe ratio
options(digits=3)
weights[ind,] # Find tangency portfolio# show line of optimal portfolios
lines(c(0,sdP_no[ind]),c(mufree,muP_no[ind]),col="blue",lwd=3,lty=3)
# show line of optimal portfolios
points(sdP_no[ind],muP_no[ind],cex=3,pch="*") # show tangency portfolio
text(sdP_no[ind],muP_no[ind]-0.005,'Tangency Portfolio', cex=1)  # plot the tangency portfolio
ind2 = (sdP_no == min(sdP_no)) # find the minimum variance portfolio
points(sdP_no[ind2],muP_no[ind2],cex=2,pch="+") # show minimum variance portfolio
text(sdP_no[ind2], muP_no[ind2]+0.010,'MVP', cex=1)  # plot the minimum variance portfolio
ind3 = (muP_no > muP_no[ind2])
lines(sdP_no[ind3],muP_no[ind3],type="l",xlim=c(min(sdP_no)-1.8,max(sdP_no)+1.8),
      ylim=c(min(muP_no)-0.02,max(muP_no)+0.02),col= "red",lwd=2)  #  plot the efficient frontier
text(sd_vect_no[1],mean_vect_no[1]-0.007,"Ford",cex=1)
points(sd_vect_no[1],mean_vect_no[1],cex=2,col = "purple",pch="*")
text(sd_vect_no[2],mean_vect_no[2]-0.007,"Pfizer",cex=1)
points(sd_vect_no[2],mean_vect_no[2],cex=2,col = "purple",pch="*")
text(sd_vect_no[3],mean_vect_no[3]-0.007,"Walmart",cex=1)
points(sd_vect_no[3],mean_vect_no[3],cex=2,col = "purple",pch="*")
text(sd_vect_no[4],mean_vect_no[4]+0.007,"Exxon",cex=1)
points(sd_vect_no[4],mean_vect_no[4],cex=2,col = "purple",pch="*")
text(sd_vect_no[5],mean_vect_no[5]-0.007,"Google",cex=1)
points(sd_vect_no[5],mean_vect_no[5],cex=2,col = "purple",pch="*")

###############################################################################
#When we have correlated stocks: 
#Exxon and Pfizer and Google and Apple

cor(stocksData_6)
cov(stocksData_6)
corrplot(cor(stocksData_6))
pairs(stocksData_6)

#Short selling is allowed
mean_vect = apply(stocksData_6,2,mean)
cov_mat = cov(stocksData_6)
sd_vect = sqrt(diag(cov_mat))
M = length(mean_vect)
Amat = cbind(rep(1,M),mean_vect)  # set the constraints matrix

muP = seq(min(mean_vect)-0.05,max(mean_vect)+0.05,length=300)  # set of 300 possible target values 
# for the expect portfolio return1
sdP = muP # set up storage for standard deviations of portfolio returns
weights = matrix(0,nrow=300,ncol=M) # storage for portfolio weights

# for each expected return (constraint), find the minimum risk/variance portfolio using
# quadratic programming
for (i in 1:length(muP))  # find the optimal portfolios for each target expected return
{
  bvec = c(1,muP[i])  # constraint vector
  result = 
    solve.QP(Dmat=2*cov_mat, dvec=rep(0,M), Amat=Amat, bvec=bvec, meq=2)
  sdP[i] = sqrt(result$value)
  weights[i,] = result$solution
}

plot(sdP,muP,type="l",xlim=c(min(sdP)-1.1,max(sdP)+1.1),ylim=c(min(muP)-0.01,max(muP)+0.01), 
     xlab = "Standard Deviation in percent", ylab = "Mean value", lwd= 2,lty=1)  #  plot 
# the efficient frontier (and inefficient frontier)
mufree = 0.0033  # input value of risk-free interest rate = 3 Month treasury bill
points(0,mufree,cex=3,pch="*")  # show risk-free asset
text(0.25, mufree-0.005,'Risk Free Asset', cex=1.2)  # plot the Risk Free Asset
sharpe =( muP-mufree)/sdP # compute Sharpe ratios
ind = (sharpe == max(sharpe)) # Find maximum Sharpe ratio
options(digits=3)
weights[ind,] # Find tangency portfolio# show line of optimal portfolios
lines(c(0,max(sdP)+2),mufree+c(0,max(sdP)+2)*(muP[ind]-mufree)/sdP[ind],col="blue",lwd=3,lty=3)
# show line of optimal portfolios
points(sdP[ind],muP[ind],cex=3,pch="*") # show tangency portfolio
text(sdP[ind],muP[ind]-0.005,'Tangency Portfolio', cex=1)  # plot the tangency portfolio
ind2 = (sdP == min(sdP)) # find the minimum variance portfolio
points(sdP[ind2],muP[ind2],cex=2,pch="+") # show minimum variance portfolio
text(sdP[ind2], muP[ind2]+0.010,'MVP', cex=1)  # plot the minimum variance portfolio
ind3 = (muP > muP[ind2])
lines(sdP[ind3],muP[ind3],type="l",xlim=c(min(sdP)-1.1,max(sdP)+1.1),
      ylim=c(min(muP)-0.01,max(muP)+0.01),col= "red",lwd=2)  #  plot the efficient frontier
text(sd_vect[1],mean_vect[1]-0.003,"Ford",cex=1)
points(sd_vect[1],mean_vect[1],cex=2,col = "purple",pch="*")
text(sd_vect[2],mean_vect[2]-0.003,"Pfizer",cex=1)
points(sd_vect[2],mean_vect[2],cex=2,col = "purple",pch="*")
text(sd_vect[3],mean_vect[3]-0.003,"Walmart",cex=1)
points(sd_vect[3],mean_vect[3],cex=2,col = "purple",pch="*")
text(sd_vect[4],mean_vect[4]+0.003,"Exxon",cex=1)
points(sd_vect[4],mean_vect[4],cex=2,col = "purple",pch="*")
text(sd_vect[5],mean_vect[5]-0.003,"Google",cex=1)
points(sd_vect[5],mean_vect[5],cex=2,col = "purple",pch="*")
text(sd_vect[5],mean_vect[6]-0.003,"Apple",cex=1)
points(sd_vect[5],mean_vect[6],cex=2,col = "purple",pch="*")

######################################################################################

# Check if the data is normal by using the shapiro wilk test
shapiro.test(ford.rtn) #W = 0.83605, p-value < 2.2e-16
shapiro.test(google.rtn) #W = 0.87978, p-value < 2.2e-16
shapiro.test(pfizer.rtn) #W = 0.93319, p-value < 2.2e-16
shapiro.test(walmart.rtn) #W = 0.91056, p-value < 2.2e-16
shapiro.test(exxon.rtn) #W = 0.87977, p-value < 2.2e-16
shapiro.test(apple.rtn) #W = 0.94347, p-value < 2.2e-16
# Data is not normal

# Now we proceed to check for t-distribution 
t.test(ford.rtn) # t = 0.98771, df = 2483, p-value = 0.3234
t.test(google.rtn) # t = 1.7282, df = 2483, p-value = 0.08407
t.test(pfizer.rtn) # t = 0.60516, df = 2483, p-value = 0.5451
t.test(walmart.rtn) # t = 0.94105, df = 2483, p-value = 0.3468
t.test(exxon.rtn) # t = 0.54847, df = 2483, p-value = 0.5834
t.test(apple.rtn) # t = 2.6256, df = 2483, p-value = 0.008702

######################################################################################
#Calculating Value at Risk and Expected Shortfall

fitt = fitdistr(stocksData/100,"t") # Dividing by 100 because earlier we had multiplied the returns by 100
param = as.numeric(fitt$estimate)
alpha = 0.05
mean = param[1]
df = param[3]
sd = param[2] * sqrt((df)/(df-2))
lambda = param[2]  #  scale parameter (STD)
qalphat = qt(alpha,df=df)
VaR_part = -10000*(mean + lambda*qalphat)
IEVaR = (stocksData < qalphat)
# Expected shortfall 
es1 = dt(qalphat,df=df)/(alpha)
es2 = (df + qalphat^2) / (df - 1)
es3 = -mean+lambda*es1*es2
ES_part = 10000*es3
VaR_part # 254.9284
ES_part # 453.2902

fitt = fitdistr(stocksData_6/100,"t") # Dividing by 100 because earlier we had multiplied the returns by 100
param = as.numeric(fitt$estimate)
alpha = 0.05
mean = param[1]
df = param[3]
sd = param[2] * sqrt((df)/(df-2))
lambda = param[2]  #  scale parameter (STD)
qalphat = qt(alpha,df=df)
VaR_part = -10000*(mean + lambda*qalphat)
IEVaR = (stocksData_6 < qalphat)
# Expected shortfall 
es1 = dt(qalphat,df=df)/(alpha)
es2 = (df + qalphat^2) / (df - 1)
es3 = -mean+lambda*es1*es2
ES_part = 10000*es3
VaR_part # 263
ES_part # 462