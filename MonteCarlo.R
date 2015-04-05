library(tseries);library(PerformanceAnalytics)


Mean = 0.05;
StDev = 0.2;
NoOfTrials = 4;
NoOfPoints = 1000;
SimulatedReturns = rnorm(NoOfPoints,mean=Mean,sd=StDev);
StartPrice = 1;
Prices = rep(StartPrice,NoOfTrials)

ComputePrices <- function(RatesOfReturn,StartingPrice)
{
	PriceArray = rep(StartingPrice,length(RatesOfReturn));
	for(i in 2:length(RatesOfReturn))
		PriceArray[i] = PriceArray[i-1]*(1+RatesOfReturn[i-1]);
	return(PriceArray);
}

for(i in 1:NoOfTrials)
{
	SimulatedReturns = rnorm(NoOfPoints,mean=Mean,sd=StDev);
	Prices = cbind(Prices,ComputePrices(SimulatedReturns, StartPrice));
}

