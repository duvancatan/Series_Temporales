# This demonstration will illustrate how to use the newsimpact function
# load dataset

library(rgarch)
data(sp500ret)
plot.ts(sp500ret)
par(mfrow=c(2,1))
acf(sp500ret, lag.max=100)
pacf(sp500ret, lag.max=100)

# We'll fit from various models and compare

# start with the "ALLGARCH" in the "fGARCH" family which is very richly parametrized

spec = ugarchspec(variance.model = list(model = "fGARCH", submodel = "ALLGARCH"))
fit.all = ugarchfit(sp500ret, spec, fit.control = list(scale = 1))

spec = ugarchspec(variance.model = list(model = "eGARCH"))
fit.e = ugarchfit(sp500ret, spec)

spec = ugarchspec(variance.model = list(model = "gjrGARCH"))
fit.gjr = ugarchfit(sp500ret, spec,  fit.control = list(scale = 1))

spec = ugarchspec(variance.model = list(model = "apARCH"))
fit.a = ugarchfit(sp500ret, spec,  fit.control = list(scale = 1))

spec = ugarchspec(variance.model = list(model = "sGARCH"))
fit.s = ugarchfit(sp500ret, spec,  fit.control = list(scale = 1))

# generate the first newsimpact
ni = newsimpact(z = NULL, fit.all)
# note that when passing z with NULL, the function finds the extremes of the 
# residuals and uses them for the range
x.all = ni$zx
y.all = ni$zy
# we'll now use a common residual range (x.all)
plot(x.all, y.all, ylab = ni$yexpr, xlab = ni$xexpr, type ="l")
# note that the function also returns the x/y expression labels
lines(x.all, newsimpact(z = x.all, fit.e)$zy, col = 2)
lines(x.all, newsimpact(z = x.all, fit.gjr)$zy, col = 3)
lines(x.all, newsimpact(z = x.all, fit.a)$zy, col = 4)
lines(x.all, newsimpact(z = x.all, fit.s)$zy, col = 5)


