library(tseries);library(PerformanceAnalytics);library(zoo)

XUS_prices = get.hist.quote("XUS.TO",start="2005-01-01",compression="m",quote="AdjClose",retclass="zoo")
PLI_prices = get.hist.quote("PLI.TO",start="2005-01-01",compression="m",quote="AdjClose",retclass="zoo")
VCE_prices = get.hist.quote("VCE.TO",start="2005-01-01",compression="m",quote="AdjClose",retclass="zoo")

# Change the class of the time index to yearmon, which is appropriate for monthly data.
# index() and as.yearmon() are functions in the zoo package 
index(XUS_prices) = as.yearmon(index(XUS_prices))
index(PLI_prices) = as.yearmon(index(PLI_prices))
index(VCE_prices) = as.yearmon(index(VCE_prices))

# Create merged price data
all_prices = merge(XUS_prices , PLI_prices, VCE_prices)
# Rename columns
colnames(all_prices) = c("XUS", "PLI", "VCE")

# Calculate cc returns as difference in log prices
all_returns = diff(log(all_prices))

# Create matrix with returns
return_matrix = coredata(all_returns)

pdf("test.pdf");
chart.TimeSeries(all_returns, legend.loc="bottom", main=" ")
chart.Bar(all_returns, legend.loc="bottom", main=" ")
Mean_vector = apply(return_matrix,2,na.rm=TRUE,mean)
SD_vector = apply(return_matrix,2,na.rm=TRUE,sd)

simple_returns = diff(all_prices)/lag(all_prices, k=-1);
chart.CumReturns(simple_returns,wealth.index = TRUE,legend.loc="topleft", main="Future Value of $1 invested")
#pdf("SampleGraph.pdf",width=7,height=5) - to output to PDF. Close device with dev.off()

for(i in 1:3)
{
	par(mfrow=c(2,2))
	returns = return_matrix[!is.na(return_matrix[,i]),i];
	hist(returns,main=paste(colnames(all_returns)[i],"monthly returns",sep=" "),
	     xlab=colnames(all_returns)[i], probability=T, col="slateblue1")
	boxplot(returns,outchar=T, main="Boxplot", col="slateblue1")
	plot(density(returns),type="l", main="Smoothed density",
	     xlab="monthly return", ylab="density estimate", col="slateblue1")
	qqnorm(returns, col="slateblue1")
	qqline(returns)
	par(mfrow=c(1,1))
}

which(index(all_returns) == as.yearmon("Jan 2014"))
dev.off()
