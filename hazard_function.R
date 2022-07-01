#For question 1:
# DEFINE TIME IN SEQUENCE

time <- seq(from=0,to=5.8,by=0.005)
theta <- c(1,5,10,25, 50,100,200,400,600)

hazard_function <- function(t,theta){  
  
  #A <- 2*t*theta*exp(-1*t^2)*(1-exp(-1*t^2))^(theta-1)
  #B <- (1-exp(-1*t^2))^theta
  #hazard <- A/B
  hazard <- (2*t*theta*exp(-1*t^2)*(1-exp(-1*t^2))^(theta-1))/(1-(1-exp(-1*t^2))^theta)
  return(hazard)
}


par(mfrow=c(3,3))

for(i in 1:length(theta)){
  plot(x=time,y=hazard_function(t=time,theta=theta[i]))
}

