# ----- BAN404 -----
# Cheat sheet for descriptive statistics

# Here I will present some common techniques for creating descriptive statistics
# in this course.

# Re-coding as factors: -----
# First of all, we will determine if the variables has to be re-coded as factors.

unique_count <- function(df){apply(df, 2, function(x) length(unique(x)))}
ucnt <- unique_count(xdata)
ucnt

#The variables which has less than 5 unique observations could be re-coded as factors. This is done like this:
for(i in 1:length(ucnt)) if(ucnt[i]<=5) xdata[,i]=as.factor(xdata[,i])



# Making plots for all variables compared to response variable
{
  par(mfrow=c(2,4))
  plot(claimcst0 ~ ., data = xdata)
}