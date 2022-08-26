##
install.packages("GeneCycle")
library(zoo)
library(GeneCycle)
library(TSA)
library(stats)
library(ggplot2)
ph_<-read.csv('C:/Users/Ryan_/Documents/R/time series/files/ph_across_sites.csv')

plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
  
  
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}


lol <- ph_[ph_$site=="Lompoc Landing",c(3,7)]
y=strptime("2021-06-14 11:30:00","%Y-%m-%d %H:%M:%S")+900*1:8826 
total=ts(lol[,2],y,start=1, end=8826,frequency=1)
lol$index <- 1:8826

trend <- lm(p_h ~ index(index), data = lol)
plot(total)
abline(trend, col="red")
detrended.trajectory <- trend$residuals # detrend the data
plot(detrended.trajectory, type="l", main="detrended time series")

f.data <- GeneCycle::periodogram(detrended.trajectory)
harmonics <- 1:200 
plot(f.data$freq[harmonics]*length(detrended.trajectory), 
     f.data$spec[harmonics]/sum(f.data$spec), 
     xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h") # Harmonic at 8Hz.

X.k <- fft(detrended.trajectory)
plot.frequency.spectrum(X.k,xlimits=c(0,20))  # Harmonic at 8Hz.

# Using the base-r algorithm
?TSA::periodogram
TSA::periodogram(detrended.trajectory)

del<-1/(15*60) # sampling frequency
x.spec <- spectrum(detrended.trajectory,log="no",span=10,plot=FALSE)
spx <- x.spec$freq/del
spy <- 2*x.spec$spec
sp.df<-cbind(spx, spy)
sp.df<-as.data.frame(sp.df)

plot(spy~spx,xlab="frequency",ylab="spectral density",type="l")

ggplot(data=sp.df, aes(x=spx, y=spy)) +
  geom_line() +
  xlim(0, 40) +
  ylim(0, 4) # Harmonic at 12Hz.


# a subset
lol <- ph_[ph_$site=="Lompoc Landing",c(3,7)]
lol_sub <- lol[1:1000,] 
y=strptime("2021-06-14 11:30:00","%Y-%m-%d %H:%M:%S")+900*1:1000
ph1=ts(lol[1:1000,2],y,start=1, end=1000,frequency=1)

lol_sub$index <- 1:1000
trend_sub <- lm(p_h ~ index(index), data = lol_sub)
plot(ph1)
abline(trend_sub, col="red")


detrended.trajectory_sub <- trend_sub$residuals
plot(detrended.trajectory_sub, type="l", main="detrended time series")

f.data_sub <- GeneCycle::periodogram(detrended.trajectory_sub)
harmonics <- 1:40 
plot(f.data_sub$freq[harmonics]*length(detrended.trajectory_sub), 
     f.data_sub$spec[harmonics]/sum(f.data_sub$spec), 
     xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h") # Harmonic at 14Hz.

X.k_sub <- fft(detrended.trajectory_sub)

plot.frequency.spectrum(X.k_sub,xlimits=c(0,20)) # Harmonic at 14Hz.

TSA::periodogram(detrended.trajectory_sub)

del<-1/(15*60) # sampling frequency
x.spec_sub <- spectrum(detrended.trajectory_sub,log="no",span=10,plot=FALSE)
spx_sub <- x.spec_sub$freq/del
spy_sub <- 2*x.spec_sub$spec
sp.df_sub<-cbind(spx_sub, spy_sub)
sp.df_sub<-as.data.frame(sp.df_sub)

plot(spy_sub~spx_sub,xlab="frequency",ylab="spectral density",type="l")

ggplot(data=sp.df_sub, aes(x=spx_sub, y=spy_sub)) +
  geom_line() +
  xlim(0, 50) +
  ylim(0, 0.5) # Harmonic between 9~16 Hz.






##Complex Wave
xs <- ph
wave.1 <- sin(3*xs)
wave.2 <- sin(10*xs)
par(mfrow = c(1, 2))
plot(xs,wave.1,type="l",ylim=c(-1,1)); abline(h=0,lty=3)
plot(xs,wave.2,type="l",ylim=c(-1,1)); abline(h=0,lty=3)

wave.3 <- 0.5 * wave.1 + 0.25 * wave.2
plot(xs,wave.3,type="l"); title("Eg complex wave"); abline(h=0,lty=3)

##Fourier Series
wave.4 <- wave.3
wave.4[wave.3>0.5] <- 0.5
plot(xs,wave.4,type="l",ylim=c(-1.25,1.25)); title("overflowed, non-linear complex wave"); abline(h=0,lty=3)

repeat.xs     <- ph
wave.3.repeat <- 0.5*sin(3*repeat.xs) + 0.25*sin(10*repeat.xs)
plot(xs,wave.3,type="l"); title("Repeating pattern")
points(repeat.xs,wave.3.repeat,type="l",col="red"); abline(h=0,v=c(-2*pi,0),lty=3)

plot.fourier <- function(fourier.series, f.0, ts) {
  w <- 2*pi*f.0
  trajectory <- sapply(ts, function(t) fourier.series(t,w))
  plot(ts, trajectory, type="l", xlab="time", ylab="f(t)"); abline(h=0,lty=3)
}

plot.fourier(function(t,w) {sin(w*t)}, 1/(15*60*8825), ts=ph) #####f.0 value

##Fourier Transform
library(stats)
fph=fft(ph)
fph
fph_n=fft(ph)/length(ph)  # to normalize
fph_n

##The Equations
get.trajectory <- function(X.k,ts,acq.freq) {
  
  N   <- length(ts)
  i   <- complex(real = 0, imaginary = 1)
  x.n <- rep(0,N)           # create vector to keep the trajectory
  ks  <- 0:(length(X.k)-1)
  
  for(n in 0:(N-1)) {       # compute each time point x_n based on freqs X.k
    x.n[n+1] <- sum(X.k * exp(i*2*pi*ks*n/N)) / N
  }
  
  x.n * acq.freq 
}

plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
  
  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

plot.harmonic <- function(Xk, i, ts, acq.freq, color="red") {
  Xk.h <- rep(0,length(Xk))
  Xk.h[i+1] <- Xk[i+1] # i-th harmonic
  harmonic.trajectory <- get.trajectory(Xk.h, ts, acq.freq=acq.freq)
  points(ts, harmonic.trajectory, type="l", col=color)
}
ph1<-ph-7
X.k <- fft(ph1)                   # get amount of each frequency k
time     <- 92                         # measuring time interval (seconds)  15*60*8825 
acq.freq <- 1/(15*60)                       # data acquisition frequency (Hz)  1/(15*60)
ts  <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s) 

plot.frequency.spectrum(X.k, xlimits=c(0,100))
x.n <- get.trajectory(X.k,ts,acq.freq)   # create time wave

plot(ts,x.n,type="l",ylim=c(-4,4),lwd=2)
abline(v=0:time,h=-2:4,lty=3); abline(h=0)

plot.harmonic(X.k,1,ts,acq.freq,"red")
plot.harmonic(X.k,2,ts,acq.freq,"green")
plot.harmonic(X.k,3,ts,acq.freq,"blue")


                        