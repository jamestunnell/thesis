x <- c(1,2,2,3,4,4,5,6,6,7,7,7,8,9)

cat(paste("mean:", mean(x)))
cat(paste("median:", median(x)))
cat(paste("std dev:", sd(x)))
cat(paste("variance:", var(x)))

# midmean is the 25% trimmed mean, which should be 25% trimmed from each side
# of the sorted data. In R, using the 'mean' function with the 'trim' param
# will only trim that % total. So to achieve midmean, set trim to 50%.
midmean <- mean(x, trim = 0.5)
cat(paste("midmean:",midmean))

quantile(x) # print out quantiles!
boxplot(x) # make a boxplot!
hist(x) # plot a histogram!

# plot an empirical cumulative distribution function
n <- length(x)
plot(sort(x), (1:n)/n, type="s",ylim=c(0,1))

y <- c(2,3,3,4,4,4,5,6,6,7,8,8)
par(mfrow=c(1,2)) # set up for 1 row, 2 columns
boxplot(x)
boxplot(y)
par(mfrow=c(1,1)) # reset layout for 1x1, affecting subsequent plots
