## maternal mortality curve fitting

# create data frame
matmort <- data.frame(age=seq(15,90,by=1), low=rep(NA,76), med=rep(NA,76), high=rep(NA,76))

# data from Blanc et al. 2013 (http://www.plosone.org/article/info%3Adoi%2F10.1371%2Fjournal.pone.0059864)
# all values are MMRatio / 100,000 to give Pr(death per birth in each age group)
# assume constant probability of death within an age group (i.e. each value repeated five times)
#matmort$low <- c( rep(0.00288, 5),  # 15-19
#                  rep(0.00218, 5),  # 20-24
#                  rep(0.00316, 5),  # 25-29
#                  rep(0.00366, 5),  # 30-34
#                  rep(0.00654, 5),  # 35-39
#                  rep(0.01249, 10), # 40-49
#                  rep(NA, 41) )     # make the rest blank at older ages

#matmort$med <- c( rep(0.00408, 5),  # 15-19
#                  rep(0.00319, 5),  # 20-24
#                  rep(0.00411, 5),  # 25-29
#                  rep(0.00525, 5),  # 30-34
#                  rep(0.0074, 5),   # 35-39
#                  rep(0.01351, 10), # 40-49
#                  rep(NA, 41) )     # make the rest blank at older ages

#matmort$high <- c( rep(0.00561, 5),  # 15-19
#                   rep(0.00466, 5),  # 20-24
#                   rep(0.0055, 5),  # 25-29
#                   rep(0.00741, 5),  # 30-34
#                   rep(0.00847, 5),   # 35-39
#                   rep(0.01445, 10), # 40-49
#                   rep(NA, 41) )     # make the rest blank at older ages

# try a version with values at mid-point in age groups
matmort[matmort$age == 17, ]$low <- 0.00288
matmort[matmort$age == 22, ]$low <- 0.00218
matmort[matmort$age == 27, ]$low <- 0.00316
matmort[matmort$age == 32, ]$low <- 0.00366
matmort[matmort$age == 37, ]$low <- 0.00654
matmort[matmort$age == 45, ]$low <- 0.01249

matmort[matmort$age == 17, ]$med <- 0.00408
matmort[matmort$age == 22, ]$med <- 0.00319
matmort[matmort$age == 27, ]$med <- 0.00411
matmort[matmort$age == 32, ]$med <- 0.00525
matmort[matmort$age == 37, ]$med <- 0.0074
matmort[matmort$age == 45, ]$med <- 0.01351

matmort[matmort$age == 17, ]$high <- 0.00561
matmort[matmort$age == 22, ]$high <- 0.00466
matmort[matmort$age == 27, ]$high <- 0.0055
matmort[matmort$age == 32, ]$high <- 0.00741
matmort[matmort$age == 37, ]$high <- 0.00847
matmort[matmort$age == 45, ]$high <- 0.01445

############################################################################
## Function to do the fitting
## Outputs a list of predicted values (insert them into the matmort data frame)
## also prints COD and the predicted parameters
##
fitmort <- function(age, probs, a.init=0.0002, b.init=0.01, c.init=0.25) {
  # try to fit function of the form (a * age^2) - (b * age) + c
  fit <- nls(probs ~ a*(age^2) - b*age + c, start=c(a=a.init, b=b.init, c=c.init))
  
  # try an exponential function
  #fit <- nls(probs, a*exp(b*age))
  
  #fit
  #summary(fit)
  #confint(fit)
  
  muLow <- mean(probs, na.rm=T)
  SSD <- sum((probs - muLow)^2, na.rm=T)
  SSD.res <- sum(resid(fit)^2) # residual sum of squares
  
  # coefficient of determination
  COD <- (SSD - SSD.res) / SSD
  print(COD)
  
  # predict values for model
  #matmort$predictLow <- predict(fit.low, list(age=matmort$age))
  
  # plot data and fitted model
  #plot(matmort$low ~ matmort$age, ylim=c(0, 0.1))
  #lines(matmort$age, predictLow, lwd=2)
  
  # what are the parameter values for the model?
  print(fit$m$getPars())
  
  return (predict(fit, list(age=age)))
}


############################################################################
## Do the fitting
##
matmort$predictLow  <- fitmort(matmort$age, matmort$low)
matmort$predictMed  <- fitmort(matmort$age, matmort$med)
matmort$predictHigh <- fitmort(matmort$age, matmort$high, b=0.03)

#plot(matmort$low ~ matmort$age, ylim=c(0, 0.1), col="blue")
#lines(matmort$age, matmort$predictLow, lwd=2, col="blue")

#points(matmort$med ~ matmort$age, col="green")
#lines(matmort$age, matmort$predictMed, lwd=2, col="green")

#points(matmort$high ~ matmort$age, col="red")
#lines(matmort$age, matmort$predictHigh, lwd=2, col="red")


############################################################################
## Maternal mortality curves from Blanc et al.
##
matmort.blanc <- melt(subset(matmort, select=c(age, predictLow, predictMed, predictHigh)), id="age", variable_name="level")


############################################################################
## Maternal mortality curves from Grimes 1994
##
grimes <- function(level, age)
{
    a <- 0.0
    b <- 0.0
    
    if (level=="L")
    {
      a = 0.002928;
      b = 0.1;
    }
    else if (level=="M")
    {
      a = 0.000485;
      b = 0.181221;
    }
    else if (level=="H")
    {
      a = 1e-6;
      b = 0.5;
    }
    
    return (a * exp(b * (age - 15))) + (0.0 - a);
}

# set up data frames
matmort.grimes.raw <- data.frame(age=seq(15, max.age, by=1))
matmort.grimes.raw$None   <- grimes("N", matmort.grimes.raw$age)
matmort.grimes.raw$Low    <- grimes("L", matmort.grimes.raw$age)
matmort.grimes.raw$Medium <- grimes("M", matmort.grimes.raw$age)
matmort.grimes.raw$High   <- grimes("H", matmort.grimes.raw$age)

require(reshape)
matmort.grimes <- melt(matmort.grimes.raw, id=c("age"), variable_name="level")

rm(matmort.grimes.raw)
