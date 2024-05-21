# Exhaustive Selection
library(leaps)

regfit <- regsubsets(y~., method = "exhaustive", data = xdata)
reg.sum <- summary(regfit)
reg.sum

plot(reg.sum$rss, type = "l")
plot(regfit,scale="adjr2")

# Forward selection
regfit <- regsubsets(y~., method = "forward", data = xdata)

