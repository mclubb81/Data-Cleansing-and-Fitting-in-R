#adding packages for use in program
library(deSolve)
library(minpack.lm)
library(data.table)
library(gtools)
library(gmodels)
library(graphics)
library(svDialogs)

#Set working directory
setwd("C:/Users/clubb/OneDrive/Desktop/Yahara Lakes New Data 2020")

#Ask user to enter lake name
#lake <- dlgInput("Enter name of lake you wish to find parameters for.", Sys.info()["user"])$res # Example:lake kegonsa.csv
lake <- "mendota"

#Creating dataframe to read data from
myLakeFile <- paste("prepared ", lake, ".csv", sep = "")
lakeDF <- read.csv(myLakeFile)

#defining timestep
t <- seq(from = 0, to = 31, by = 0.02) #Previous value of to is 30

#starting parameters and lake
aveLoadToLake <- 33400
q <- 7.88
parStart <- list(s=0.511, r=732742, m=175258)

#theoretical result
getPred <- function(parS,tt){
  #defining initial conditions
  pini <- c(y = 85000)
  
  #defining lake ode
  lakeEqn <- function(tt, p, parS)
    list(aveLoadToLake - (parS$s)*p  + (parS$r)*(p^(q))/((parS$m)^(q) + p^(q)))
  
  #computing numerical ode
  Numode <- as.data.frame(ode(y = pini, times = tt, func = lakeEqn, parms = parS, "ode45"))
  
  Numode[seq(1,nrow(Numode),47),]
}

#printing data vs solution to estimate initial parameters
plot(getPred(parStart,t)$y,main = paste("lake", lake),xlab = "time in years", ylab = "amount phosphorus in kg",type = "o", col = "red", ylim = c(0,100000))
lines(lakeDF$Phosphorus_kg, type = "o", col = "blue")
plot(getPred(parStart,t)$y,lakeDF$Phosphorus_kg, main = paste(lake, "Predicted vs Actual"), xlab = "Predicted Value in kg", ylab = "Actual Value in kg", xlim = c(0, 100000), ylim = c(0,100000))

#defining residue function
residFun <- function(pp,observed, tt) observed - getPred(pp,tt)$y

############################applying bootstrap#################################

#defining number of iterations to perform
numIterations <- 10000

#allocating space to hold bootstrap values
sBoot <- rep(NA, numIterations)
rBoot <- rep(NA, numIterations)
mBoot <- rep(NA, numIterations)
#qBoot <- rep(NA, numIterations)

#allocating space to hold randomized parameter values
ran_q <- rep(NA, numIterations)
ran_s <- rep(NA, numIterations)
ran_r <- rep(NA, numIterations)
ran_m <- rep(NA, numIterations)

#loop to get bootstrap estimates via nls.lm and store parameters in vectors
#for later analysis
for(i in 1:numIterations){
  #Displays current bootstrap iteration in multiples of 10
  if(i %% 100 == 0){
    print(paste("Current bootstrap iteration: ", i))
  }
  
  #employing resample to recorded lake values
  lakeBoot <- sample(lakeDF$Phosphorus_kg,size = 32, replace = TRUE)
  
  #randomize the starting parameters
  # q <- runif(1, min = 2, max = 20)
  
  #new code for q randomized parameter
  if(lake == "mendota"){
    q <- runif(1, min = 5, max = 10)
  } else if(lake == "monona"){
    q <- runif(1, min = 7, max = 12)
  } else if(lake == "waubesa"){
    q <- runif(1, min = 12, max = 17)
  } else{
    q <- runif(1, min = 12, max = 17)
  }
  
  #other randomized parameters
  parStart[1] <- runif(1, min = 0.001, max = 5) #s value
  parStart[2] <- runif(1, min = 500000, max = 800000) #r value
  parStart[3] <- runif(1, min = 100000, max = 300000) #m value
  
  #storing randomized starting parameters in vectors to get histogram later
  ran_q[i] <- q
  ran_s[i] <- parStart[1]
  ran_r[i] <- parStart[2]
  ran_m[i] <- parStart[3]

  #employing nls to the lakeBoot sample
  lake.nls <- nls.lm(par = parStart, fn = residFun, observed = lakeBoot, tt = t, lower = c(0.001, 500000, 100000), upper = c(5.000, 800000, 300000), control = nls.lm.control(maxiter = 100))	

  #storing found parameters in vectors
  sBoot[i] <- last(lake.nls$par$s)
  rBoot[i] <- last(lake.nls$par$r)
  mBoot[i] <- last(lake.nls$par$m)
  #qBoot[i] <- last(lake.nls$par$q)
}

#Creating data frame of bootstrapped parameters and converting to csv
parameterBootDF <- data.frame(sBoot, rBoot, mBoot)
names(parameterBootDF) <- c("sBoot", "rBoot", "mBoot")
parameterBootFileName <- paste(lake,"NarrowBootParameters",".csv", sep = "")
#write.csv(parameterBootDF, parameterBootFileName, row.names = FALSE)

#Appending to old data frame and converting to csv
oldParameterBootDF <- read.csv(paste(lake,"NarrowBootParameters",".csv", sep = ""))
newParameterBootDF <- rbind(oldParameterBootDF, parameterBootDF)
write.csv(newParameterBootDF, parameterBootFileName, row.names = FALSE) 

#create histograms of randomized starting parameters
hist(as.numeric(ran_q), main = "q randomized distribution")
hist(as.numeric(ran_s), main = "s randomized distribution")
hist(as.numeric(ran_r), main = "r randomized distribution")
hist(as.numeric(ran_m), main = "m randomized distribution")

#create histograms to look at relative frequencies
hist(sBoot,main = "sBoot Histogram",xlab = "s parameter value", ylab = "frequency")
hist(rBoot,main = "rBoot Histogram",xlab = "r parameter value", ylab = "frequency")
hist(mBoot,main = "mBoot Histogram",xlab = "m parameter value", ylab = "frequency")
#hist(qBoot,main = "qBoot Histogram",xlab = "q parameter value", ylab = "frequency")

#show boxplots of estimated parameter values
boxplot(sBoot, main = "sBoot Box Plot", range = 1)
boxplot(rBoot, main = "rBoot Box Plot", range = 1)
boxplot(mBoot, main = "mBoot Box Plot", range = 1)
#boxplot(qBoot, main = "qBoot Box Plot", range = 1)

#evaluate bootstrap estimate confidence intervals
ci(sBoot,confidence = 0.95)
ci(rBoot,confidence = 0.95)
ci(mBoot,confidence = 0.95)
#ci(qBoot)

