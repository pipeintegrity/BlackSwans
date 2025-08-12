
#Here is the creation code for normal origins. We generate N = 1000 normally
#distributed random variables with a zero mean and unit standard deviation,
#select the maximum value out of these 1000 values, and repeat the process 1000
#times to get 1000 maximum values. These maximum values converge to the Type I
#extreme value distribution - Gumbel (e^{-e^{-y}}). The code runs like an
#animation. You can control the speed by changing the number in Sys.sleep().
#There is a plot that shows the convergence of the average values to a Normal
#distribution. Do you know why?

library(locfit)
library(logspline)

# Normal Origins #
par(mfrow=c(3,1))
x = rnorm(10000,0,1)
plot(
  logspline(x),
  xlim = c(-5, 5),
  ylim = c(0, 0.5),
  font = 2,
  cex.lab = 2,
  font.lab = 2,
  xlab = "Normal Distribution"
)
hist(
  x,
  prob = T,
  ylab = "",
  main = "",
  font = 2,
  cex.lab = 2,
  font.lab = 2,
  col = 'white',
  add=T
)

N = 1000
ave = matrix(NA,nrow=N,ncol=1)
max1 = matrix(NA,nrow=N,ncol=1)

for (i in 1:N)
{
  x = rnorm(N,0,1)

  lines(locfit(~x),col="grey")
  points(mean(x),0,col="blue",pch=17)
  points(max(x),0,col="red",pch=17)
  ave[i] = mean(x)
  max1[i] = max(x)
  # Sys.sleep(0.01)
}

# # Sys.sleep(1)
plot(
  locfit( ~ ave),
  xlim = c(-5, 5),
  ylim = c(0, 9),
  ylab = "",
  cex.lab = 2,
  font = 2,
  font.lab = 2,
  xlab = "Normal Distribution",
  col = "white"
)
lines(locfit( ~ ave), lwd = 2, col = "blue")

# Sys.sleep(1)

plot(
  locfit( ~ max1),
  xlim = c(-5, 5),
  font = 2,
  font.lab = 2,
  ylab = "",
  xlab = "Extreme Value Distribution (Gumbel)",
  cex.lab = 2,
  col = "white"
)
lines(locfit(~max1),lwd=2,col="red")

par(mfrow=c(1,1))

plot(
  locfit( ~ ave),
  xlim = c(-0.15, 0.15),
  font = 2,
  font.lab = 2,
  ylab = "",
  xlab = "Normal Distribution of Averages",
  cex.lab = 2,
  col = "white"
)

lines(locfit(~ave),lwd=2,col="blue")


## EVD from Exponential----------------------------------------------
#The creation code for exponential origins has the same procedure. We generate
#N = 1000 exponentially distributed random variables with lambda = 0.5 as the
#parent. The maximum values of an exponential distribution again converge to the
#Gumbel distribution.

par(mfrow=c(3,1))
x = rexp(10000,0.5)
hist(
  x,
  prob = T,
  xlim = c(0, 25),
  ylim = c(0,0.5),
  ylab = "",
  main = "",
  font = 2,
  cex.lab = 2,
  font.lab = 2,
  col = 'white',
  xlab = "Exponential Distribution"
)
plot(logspline(x),add=T)

N = 1000

ave = matrix(NA,nrow=N,ncol=1)
max1 = matrix(NA,nrow=N,ncol=1)

for (i in 1:N)
{
  x = rexp(N,0.5)

  lines(locfit(~x),col="grey")
  points(mean(x),0,col="blue",pch=17)
  points(max(x),0,col="red",pch=17)
  ave[i] = mean(x)
  max1[i] = max(x)
  # Sys.sleep(0)
}

# Sys.sleep(1)
plot(
  locfit( ~ ave),
  xlim = c(0, 25),
  ylim = c(0, 4),
  ylab = "",
  cex.lab = 2,
  font = 2,
  font.lab = 2,
  xlab = "Normal Distribution",
  col = "white"
)
lines(locfit(~ave),lwd=2,col="blue")

# Sys.sleep(1)
plot(
  locfit( ~ max1),
  xlim = c(0, 25),
  font = 2,
  font.lab = 2,
  ylab = "",
  xlab = "Extreme Value Distribution (Gumbel)",
  cex.lab = 2,
  col = "white"
)
lines(locfit(~max1),lwd=2,col="red")




# Apply to PHMSA incidents ------------------------------------------------


