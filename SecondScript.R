library(tseries);library(PerformanceAnalytics);library(zoo)

PLI_prices = get.hist.quote("PLI.TO",start="2005-01-01",end="2014-01-01", compression="m",quote="AdjClose",retclass="zoo")
PLI_full_prices = get.hist.quote("PLI.TO",start="2005-01-01", compression="m",quote="AdjClose",retclass="zoo")

# Change the class of the time index to yearmon, which is appropriate for monthly data.
# index() and as.yearmon() are functions in the zoo package 
#index(PLI_prices) = as.yearmon(index(PLI_prices))
#index(PLI_full_prices) = as.yearmon(index(PLI_full_prices))

# Calculate cc returns as difference in log prices
PLI_returns = diff(log(coredata(PLI_prices)))
PLI_full_returns = diff(log(coredata(PLI_full_prices)))

Fut_returns = mean(PLI_returns,na.rm=T) + sd(PLI_returns,na.rm=T)*rnorm(12);

for(i in 1:100)
{
	Fut_returns = Fut_returns + sd(PLI_returns,na.rm=T)*rnorm(12);
}
Fut_returns = Fut_returns/100;

Comb_returns = c(PLI_returns, Fut_returns)

plot(PLI_full_returns,type="l")
lines(Comb_returns,col="red")