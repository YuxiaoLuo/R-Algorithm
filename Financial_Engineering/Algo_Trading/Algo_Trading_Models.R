########################################################################
#  Final Project for Algorithm Trading 
#  Author: Yuxiao Luo   E-mail: yluo52@fordham.edu
#  https://github.com/YuxiaoLuo
#  Updated: 07/10/2016
#  Data is attached in the data file 
########################################################################
setwd("C:/Users/Yuxiao Luo/Documents/R/data/Algo Trading")
library(readxl) # package for function read_excel to input xlsx file

############################## Problem 1
data.prices <- read.csv("Prices.csv", header = T, stringsAsFactors = FALSE, sep = ",")
#x <- read_excel("Algorithmic Trading_Final Group Project.xlsx", sheet = "FF Factors", col_names = T, skip =1)
data.fama <- read.csv("Fama-French Factors.csv", header = T, stringsAsFactors =  FALSE, sep = ",")
data.stock <- read_excel("Algorithmic Trading_Final Group Project.xlsx", sheet = "Stock Data", col_names =  T, skip =1)

tail(data.fama)
tail(data.prices)
tail(data.stock)
data.stock <- data.stock[-nrow(data.stock),]

#rearrange prices data
data.extract.stock <- data.stock$Symbol[data.stock$Group == 3] # stock that need to extract for project group

data.construction <- function(data.1, data.2){ # data.1: specific stock symbols, data.2: data of different stocks
  
  data.output <- data.frame("Date" = data.2$Date[1:504]) 
  n <- length(data.1)
  k <- 0
  for(i in 1:n){
    prices <- data.2[,4][data.2[,1] == data.1[i]]
    data.output <- cbind(data.output, prices)
    k <- k+1
  }
  colnames(data.output) <- c("Date", data.1)
  print(k)
  return(data.output)
}
data.25stocks <- data.construction(data.extract.stock, data.prices)
rm(data.extract.stock)

# reverse the rows of data.25stocks to accord time series
data.25stocks <- data.25stocks[nrow(data.25stocks):1,]
rownames(data.25stocks) <- 1:nrow(data.25stocks)

# function for computing log returns 
log.return <- function(x){
  
  lr <- x[,2:ncol(x)] # remove the date colum in data frame
  lr[1,] <-NA
  
  for(i in 2:ncol(x)){ # first for loop for different columns in data frame x
    
    vector <- x[,i]
    
    index <- 0
    l.r.vector <- rep(NA, times = nrow(x)) # vector for storing the value of log returns of each column
    
    for(j in 2:nrow(x)){ # second for loop for computing the log returns of each column 
      
      l.r.vector[j] <- log(vector[j]) - log(vector[j-1])
      index <- index +1
    } # end for loop
    
    lr[,i-1] <- l.r.vector # input the each column of data frame x with the computed log return
  } # end second for loop
  print(index)
  return(lr)
}

data.logreturn <- log.return(data.25stocks)
data.logreturn <- data.logreturn[2:nrow(data.logreturn),]
rownames(data.logreturn) <- 1:nrow(data.logreturn) 

# reverse data.fama to accord time series 
data.fama <- data.fama[nrow(data.fama):1,]
rownames(data.fama) <- 1:nrow(data.fama)
data.fama <- data.fama[2:nrow(data.fama),]
rownames(data.fama) <- 1:nrow(data.fama)

lm.1 <- lm(data.logreturn$AZO ~ data.fama$Mkt.RF + data.fama$SMB + data.fama$HML)
summary(lm.1)
# regression models for 25 stocks, put the regression in a data frame
Function.regression <- function(data.1, data.2){ # data.1: dependent variables, data.logreturn. data.2: independent variables, data.fama.
  
  data.stocksymbol <- colnames(data.logreturn)
  n <- length(data.stocksymbol)
  regression.result <- data.frame(matrix(NA, n, 10)) # the data.frame for regression result
  colnames(regression.result) <- c("b0", "b1", "b2", "b3", "t0", "t1", "t2", "t3", "R^2", "MSE")
  k <- 0
  for(i in 1:n){
    eval(parse(text = paste('lm.',i, ' <- lm(data.1$', data.stocksymbol[i],' ~ data.2$Mkt.RF + data.2$SMB + data.2$HML)'
                            , sep = ""))) # linear regression
    regression.result[i,1] <- eval(parse(text = paste( 'as.vector(lm.', i, '$coefficients)[1]', sep = "")))# b0
    regression.result[i,2] <- eval(parse(text = paste( 'as.vector(lm.', i, '$coefficients)[2]', sep ="")))# b1
    regression.result[i,3] <- eval(parse(text = paste( 'as.vector(lm.', i, '$coefficients)[3]', sep ="")))# b2
    regression.result[i,4] <- eval(parse(text = paste( 'as.vector(lm.', i, '$coefficients)[4]', sep = "")))# b3
    regression.result[i,5] <- eval(parse(text = paste( 'as.data.frame(summary(lm.', i, ')[4])$coefficients.t.value[1]', sep = "")))# t0
    regression.result[i,6] <- eval(parse(text = paste( 'as.data.frame(summary(lm.', i, ')[4])$coefficients.t.value[2]', sep = "")))# t1
    regression.result[i,7] <- eval(parse(text = paste( 'as.data.frame(summary(lm.', i, ')[4])$coefficients.t.value[3]', sep = "")))# t2
    regression.result[i,8] <- eval(parse(text = paste( 'as.data.frame(summary(lm.', i, ')[4])$coefficients.t.value[4]', sep = "")))# t3
    regression.result[i,9] <- eval(parse(text = paste( 'summary(lm.', i, ')$r.squared', sep = ""))) # r.squared
    regression.result[i,10] <- eval(parse(text = paste(' summary(lm.', i, ')$sigma^2', sep= ""))) # MSE
    k <- k + 1
  }
  print(k)
  rownames(regression.result) <- data.stocksymbol
  return(regression.result)
}
regression.result <- Function.regression(data.logreturn, data.fama)

# output logreturn and regression result
rownames(data.logreturn) <- data.fama$Date # Put dates into log.return

write.csv(data.logreturn, file = "Problem_1_log.return.csv")
write.csv(regression.result, file = "Problem_1_regression.result.csv")

########################################
# calculate C matrix
B <- as.matrix(regression.result[,c(2,3,4)])
covF <- cov(data.fama[,c(3,4,5)])
eta <- diag(regression.result$MSE, nrow = 25, ncol = 25) # eta
c <- (B%*%covF%*%t(B) + eta)*250
write.csv(c, file = "Problem_1_C.matrix.csv")

# calculate C_sample matrix using log return data
c_sample <- cov(data.logreturn)*250
write.csv(c_sample, file = "Problem_1_C.sample.matrix.csv")

## comparison between c and c_sample
# check the signs of two corresponding covariance
Function.opposite.sign <- function(data.1, data.2){
  # data.1: matrix 1, c; data.2: matrix 2, c_sample; Note: matrices should be square matrix
  n <- nrow(data.1)
  matrix.result <- matrix(NA, nrow = n, ncol =n)
  for(i in 1:n){
    for(j in 1:n){
      if( data.1[i,j] * data.2[i,j] > 0){
        matrix.result[i,j] <- "Same" 
      }else{ 
        matrix.result[i,j] <- "Diff" 
      }
    } 
  }
  return(matrix.result)
} 
check.opposite.sign <- Function.opposite.sign(c, c_sample)
rownames(check.opposite.sign) <- rownames(regression.result)
colnames(check.opposite.sign) <- rownames(regression.result)
write.csv(check.opposite.sign, "Problem_1_Opposite.sign.csv")

# compare difference between matrices
sum(abs(c - c_sample) > 0.01)
difference.matrix <- c - c_sample
colnames(difference.matrix) <- colnames(data.1)
rownames(difference.matrix) <- rownames(data.2)
write.csv(difference.matrix, "Problem_1_Difference.csv")

# plot of the differences between c and c_sample matrices
col.vector <- c("black",rainbow(24)) # color vector
plot(seq(1,1.000000001, length = 25), difference.matrix[,1], xlim = c(0,26), ylim = c(-0.03, 0.03), 
     main = "Differences between C and C_Sample matrices on 25 stocks", xlab = "25 Stocks",
     ylab = "Differences", col = col.vector[1], pch =19, las = T)
for( i in 2:ncol(difference.matrix)){
  eval(parse(text = 
               paste('points(seq(',i, ',', i+0.000000001, ', length = 25), 
                      difference.matrix[,',i,'], col = col.vector[', i, '], pch = 19)', sep = "")))
}
abline( h = c(0.01,-0.01), col = "brown", lty = 2)
legend("topleft", legend = rownames(regression.result), 
       pch = 19, bty = "n", col = col.vector, cex = 0.9, ncol = 5)

# clean temporary variables
rm(B, eta, covF, c, c_sample, check.opposite.sign, difference.matrix, col.vector, i) 
dev.off()

#########Problem 2 Estimate MI parameters and construct MI model

# input and clean data 
data.MI <- read_excel("Algorithmic Trading_Final Group Project.xlsx", sheet = "Market Impact", col_names = T, skip =1)
data.MI <- as.data.frame(data.MI)
data.MI <- data.MI[-nrow(data.MI), -1]

## non-linear regression models

# selfStart
selfStart(Cost_bp ~ a1 * Size^a2 * Volatility^a3 * LnMktCap^a5 * (b1*POV^a4 + 1 - b1), parameters = c(a1,a2,a3,a5,b1,a4))

# full models with all 6 parameters
# sample without replacement 
data.nls.full <- data.frame("a1" = rep(NA,25), "a2" = rep(NA,25), 
                            "a3" = rep(NA,25), "a5" = rep(NA,25), 
                            "b1" = rep(NA,25), "a4" = rep(NA,25))
for(i in 1:25){
  index <- sample(1:nrow(data.MI), size = 250)
  data.sample <- data.frame("Obs" = index,"Size" = data.MI$Size[index], "Volatility" = data.MI$Volatility[index], 
                            "POV" = data.MI$POV[index], "LnMktCap" = data.MI$LnMktCap[index], 
                            "Cost_bp" = data.MI$Cost_bp[index])
  nls.result <- nls(Cost_bp ~ a1 * Size^a2 * Volatility^a3 * LnMktCap^a5 * (b1*POV^a4 + 1 - b1), 
               data = data.sample, start = list(a1=400, a2=0.1, a3=0.1, a5=0.1, b1=0.1, a4=0.1), 
               control = list(warnOnly = T, maxiter = 200), algorithm = "port")
  data.nls.full[i,] <- as.vector(summary(nls.result)$coefficients[,1])
}
rm(i,index, nls.result,data.sample)
data.nls.full <- rbind(data.nls.full, "SE" = c(sd(data.nls.full$a1), sd(data.nls.full$a2), 
                                               sd(data.nls.full$a3), sd(data.nls.full$a5),
                                               sd(data.nls.full$b1),sd(data.nls.full$a4)))
data.nls.full <- rbind(data.nls.full, "T-stat" = c(mean(data.nls.full[,1])/data.nls.full["SE",][1],
                                                   mean(data.nls.full[,2])/data.nls.full["SE",][2],
                                                   mean(data.nls.full[,3])/data.nls.full["SE",][3],
                                                   mean(data.nls.full[,4])/data.nls.full["SE",][4],
                                                   mean(data.nls.full[,5])/data.nls.full["SE",][5],
                                                   mean(data.nls.full[,6])/data.nls.full["SE",][6]))

### Note: the model may not work because of convergence issue if the start value too far from optimum
### we find the value in Excel Solver that the a1 would always be from 300 to 800, thus, we set the 
### start value to 400

# preferred model with 5 parameters, 
data.nls.part <- data.frame("a1" = rep(NA,25), "a2" = rep(NA,25), 
                            "a3" = rep(NA,25), "b1" = rep(NA,25), "a4" = rep(NA,25))
for(i in 1:25){
  index <- sample(1:nrow(data.MI), size = 250)
  data.sample <- data.frame("Obs" = index,"Size" = data.MI$Size[index], "Volatility" = data.MI$Volatility[index], 
                            "POV" = data.MI$POV[index], "Cost_bp" = data.MI$Cost_bp[index])
  nls.result <- nls(Cost_bp ~ a1 * Size^a2 * Volatility^a3 * (b1*(POV^a4-1) + 1), data = data.sample,
                    start = list(a1=400, a2=0.1, a3=0.1, b1=0.1, a4=0.1), 
                    control = list(warnOnly = T, maxiter = 200), algorithm = "port")
  data.nls.part[i,] <- as.vector(summary(nls.result)$coefficients[,1])
}
rm(i,index, nls.result, data.sample)
data.nls.part <- rbind(data.nls.part, "SE" = c(sd(data.nls.part$a1), sd(data.nls.part$a2), 
                                               sd(data.nls.part$a3), sd(data.nls.part$b1), sd(data.nls.part$a4)))
data.nls.part <- rbind(data.nls.part, "T-stat" = c(mean(data.nls.part[,1])/data.nls.part["SE",][1],
                                                   mean(data.nls.part[,2])/data.nls.part["SE",][2],
                                                   mean(data.nls.part[,3])/data.nls.part["SE",][3],
                                                   mean(data.nls.part[,4])/data.nls.part["SE",][4],
                                                   mean(data.nls.part[,5])/data.nls.part["SE",][5]))

####################Problem 3

## data needed
# Covariance matrix: c
# MI model:
model.MI <- function(size,sigma,pov){685.992*size^0.335*sigma^0.569*(0.982*pov^0.725 + 0.018)}
# Returns:
data.25 <- data.stock[data.stock$Group == 3,]
# Price: data.25
# Volatility:
data.volatility <- data.frame("Symbol" = colnames(data.logreturn), "Volatility" = rep(NA,25))
for(i in 1:25){
  data.volatility$Volatility[i] <- sd(data.logreturn[,i])*sqrt(250)
}
data.25$Volatility <- data.volatility$Volatility # replace calculated volatility with the one in dataset
rm(i,data.volatility)

# construct linear function

# construct a vector of 25 weights

QP <- function(w, c, data){
  # w: is a vector of 25 weights; data: data.25, indcluding price, volatility, est returns of stock
  # c: covariance matrix of fama-french
  total.return <- t(as.matrix(data[,7])) %*% as.matrix(w)
  risk <- t(as.matrix(w)) %*% c %*% as.matrix(w)
  return(total.return)
}

## fuction to sample weights with specific interval
Function.sample <- function(N = 25, Sum = 1, lowerBound = 0.005, upperBound = 0.25){
  X <- rep(NA,N)
  remainingSum <- Sum
  for (i in 1:(N-1))
  {
    a <- max( lowerBound, remainingSum-(N-i)*upperBound )
    b <- min( upperBound, remainingSum-(N-i)*lowerBound )
    
    A <- ceiling(1e+8*a)
    B <- floor(1e+8*b)
    
    X[i] <- ifelse( A==B, A, sample(A:B,1)) / 1e+8
    
    remainingSum <- remainingSum - X[i]
  }
  X[N] <- remainingSum
  X <- sample(X,N)
  return(X)
}
num1 <- Function.sample()
num2 <- Function.sample()


###################################### Problem 5

# Macro Decisions
PoV <- seq(0.01, 1, length =100) # initiate 100 PoV rates
MI <- function(PoV){ # Pov is the vector of different PoV rates
  n <- length(PoV)
  result <- rep(NA, n)
  for(i in 1:n){
    result[i] <- 685.992*0.1^0.335*0.25^0.569*(0.982*PoV[i]^0.725+0.018) #using the parameters from MI model
  }
  return(result)
}
Mi <- MI(PoV) # market impact
TR <- function(PoV){
  n <- length(PoV)
  result <- rep(NA,n)
  for( i in 1:n){
    result[i] <- 0.25*sqrt(1/250*1/3*100000/1000000*(1-PoV[i])/PoV[i])*10^4 #using given trading scenario 
  }
  return(result)
}
Tr <- TR(PoV) # timing risk

Minimize <- function(Mi,Tr,lambda){
  n <- length(Mi)
  result <- rep(NA,n)
  for(i in 1:n){
    result[i] <- Mi[i] + lambda*Tr[i]
  }
  return(min(result)) # return the minimum value
}

Minimize.1 <- function(Mi,Tr,lambda){ #lamb da is 1 here 
  n <- length(Mi)
  result <- rep(NA,n)
  for(i in 1:n){
    result[i] <- Mi[i] + lambda*Tr[i]
  }
  return(result) # return the minimum value
}
result <- Minimize.1(Mi, Tr, 1)

# Macro Decision: maximize the probability of out-performing a cost of 80bp
prob <- rep(NA, 100)
for(i in 1:100){
  prob[i] <- pnorm(80, mean = Mi[i], sd = Tr[i], lower.tail = T)
}
which.max(prob)

lambda <- seq(0, 3, length = 100)
output <- rep(NA, 100)
for( i in 1:100){
  output[i] <- Minimize(Mi, Tr, lambda[i])
}
# efficient trading frontier 
plot(Tr, output, main = "Efficient Trading Frontier", xlab= "Risk(bp)", 
     ylab = "Cost(bp)", xlim = c(0,300), ylim = c(0,140),type = "l", lwd =3)

# clean temporary variables and functions
rm(i, lambda, Mi,output, PoV, prob, result, Tr, MI, TR, Minimize.1, Minimize)

### Micro Decisions 
# AIM Strategy
Mi.et <- 0.5 * Mi # Et Mi = 0.5 * Mi.star
Cost.et <- 0.5*0.05 + 0.5*0.1 + Mi.et # Estimated cost = Et Mi + theta*realized cost + (1-theta) * price improvement
TR.et <- function(PoV){
  n <- length(PoV)
  result <- rep(NA,n)
  for(i in 1:n){
    result[i] <- 0.25*sqrt(0.5*1/250*1/3*0.1*(1-PoV[i])/PoV[i])*10^4
  }
  return(result)
}
Tr.et <- TR.et(PoV)
AIM <- (Mi[which.min(result)] - Cost.et)/Tr.et
which.min(AIM[-length(AIM)])
plot(x=PoV, y = AIM, main = "PoV Rate in AIM Strategy", xlab = "PoV Rate (bp)", 
     ylab = "AIM value", type = "l", ylim = c(-12, 2),lwd = 3)
rm(Mi.et, Cost.et, Tr.et, AIM)

# PIM Strategy
Mi.et <- 0.5 * Mi # Et Mi = 0.5 * Mi.star
Cost.et <- 0.5*0.15 + 0.5*0.05 + Mi.et # Estimated cost = Et Mi + theta*realized cost + (1-theta) * price improvement
TR.et <- function(PoV){
  n <- length(PoV)
  result <- rep(NA,n)
  for(i in 1:n){
    result[i] <- 0.15*sqrt(0.5*1/250*1/3*0.1*(1-PoV[i])/PoV[i])*10^4
  }
  return(result)
}
Tr.et <- TR.et(PoV)
PIM <- (Cost.et - Mi[which.min(result)])/Tr.et
plot(x = PoV, y = PIM, main = "PoV Rate in PIM Strategy", xlab = "PoV Rate (bp)", 
     ylab = "PIM value", type = "l", lwd =3, ylim = c(-5,20))
which.min(PIM) # 21, -1.0772411
PIM[21]
