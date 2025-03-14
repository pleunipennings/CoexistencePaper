

set.seed(3)
n = 10
x = (1:n)+rnorm(n = n, mean = 0, sd = 0.5)
y = (1:n)+rnorm(n = n, mean = 0, sd = 0.5)

par(bty="l")
plot(x, y, xlim = c(-1,12), ylim = c(-1,12), pch = 16, cex = 2, 
     xlab = "", 
     ylab = "", xaxt='n', yaxt = "n", 
#     frame.plot=F)
)
abline(a=0, b=1)

x2 = 1:200
y_old = 0
ylist = c(); mu = 0.01; s = 0.1

for (i in 1:200){
  y_new = y_old + (1-y_old)*mu - y_old*(1-y_old)*s
  y_old = y_new
  ylist = c(ylist, y_new)
}

par(bty="l")
plot(x2, ylist, xlim = c(0,100),
     #xlim = c(-1,12), ylim = c(-1,12),
     type = "l", lwd = 2,
     xlab = "", 
     ylab = "", xaxt='n', yaxt = "n", 
     #     frame.plot=F)
)

