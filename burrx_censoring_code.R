source("C:\\Users\\gmostafa\\Downloads\\exponential censoring functions.txt")


mletype1 = vector(mode="numeric", length=1000)
mletype2 = vector(mode="numeric", length=1000)
for (i in 1:1000)
{
  x = rburrxtype1(n=100, theta=10, C=50) # Censoring probability(T<c=1.645)=0.5
  y = rburrxtype2(n=100, theta=10, r=50)
  mletype1[i] = burrxmle(t=x$t, delta=x$delta)
  mletype2[i] = burrxmle(t=y$t, delta=y$delta)
}
par(mfrow=c(2,1))

hist(mletype1,  sub=paste("Mean =", round(mean(mletype1),4), " SD =", round(sd(mletype1),4)), pch = 19, col = 4)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray", # Grid line color
     lwd = 2)   

qqnorm(mletype1)
qqline(mletype1, col = "steelblue", lwd = 2)
shapiro.test(mletype1)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray", # Grid line color
     lwd = 2)   
hist(mletype2,  sub=paste("Mean =", round(mean(mletype2),4), " SD =", round(sd(mletype2),4)), pch = 19, col = 4)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray", # Grid line color
     lwd = 2)   


qqnorm(mletype2)
qqline(mletype1, col = "steelblue", lwd = 2)
shapiro.test(mletype2)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray", # Grid line color
     lwd = 2)   
shapiro.test(mletype2)
