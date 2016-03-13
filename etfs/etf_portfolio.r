library(tseries);library(PerformanceAnalytics);library(zoo)

rm(list = ls());

init_investment = 10000;

xus_prices = get.hist.quote("SPY", quote = c("AdjClose"), compression="d",retclass="zoo");
vcn_prices = get.hist.quote("XIU.TO", quote = c("AdjClose"), compression="d",retclass="zoo");
xid_prices = get.hist.quote("INDY", quote = c("AdjClose"), compression="d",retclass="zoo");
vab_prices = get.hist.quote("BND", quote = c("AdjClose"), compression="d",retclass="zoo");

xus_prices_coredata = coredata(xus_prices);
xid_prices_coredata = coredata(xid_prices);
vcn_prices_coredata = coredata(vcn_prices);
vab_prices_coredata = coredata(vab_prices);

max_date = index(xus_prices)[1];

if (index(xid_prices)[1] > max_date)
	max_date = index(xid_prices)[1];
if (index(vcn_prices)[1] > max_date)
	max_date = index(vcn_prices)[1];
if (index(vab_prices)[1] > max_date)
	max_date = index(vab_prices)[1];

xus_index = match(max_date, index(xus_prices));
xid_index = match(max_date, index(xid_prices));
vcn_index = match(max_date, index(vcn_prices));
vab_index = match(max_date, index(vab_prices));

min_length = length(xus_prices[xus_index:length(xus_prices)]);
if (length(xus_prices[xid_index:length(xid_prices)]) < min_length)
	min_length = length(xid_prices) - xid_index;
if (length(vcn_prices[vcn_index:length(vcn_prices)]) < min_length)
	min_length = length(vcn_prices) - vcn_index;
if (length(vab_prices[vab_index:length(vab_prices)]) < min_length)
	min_length = length(vab_prices) - vab_index;

xus_portfolio_growth = xus_prices_coredata[xus_index:(xus_index + min_length - 1)]/xus_prices_coredata[xus_index] * init_investment * 0.34;
xid_portfolio_growth = xid_prices_coredata[xid_index:(xid_index + min_length - 1)]/xid_prices_coredata[xid_index] * init_investment * 0.33;
vcn_portfolio_growth = vcn_prices_coredata[vcn_index:(vcn_index + min_length - 1)]/vcn_prices_coredata[vcn_index] * init_investment * 0;
vab_portfolio_growth = vab_prices_coredata[vab_index:(vab_index + min_length - 1)]/vab_prices_coredata[vab_index] * init_investment * 0.33;
total_portfolio_growth = xus_portfolio_growth + xid_portfolio_growth + vcn_portfolio_growth + vab_portfolio_growth;

#plot(index(xus_prices)[1:643], total_portfolio_growth, type = 'l', main = "XUS", xlab = "time", ylab = "Growth of 10K");

xus_returns = diff(log(xus_prices[,]));
xid_returns = diff(log(xid_prices[,]));
vcn_returns = diff(log(vcn_prices[,]));
vab_returns = diff(log(vab_prices[,]));

#par(mfrow = c(2,2));
#plot(xus_returns,main="XUS",ylab="returns", col="red");
#plot(xid_returns,main="XID",ylab="returns", col="green");
#plot(vcn_returns,main="VCN",ylab="returns", col="blue");
#plot(vab_returns,main="VAB",ylab="returns", col="black");

#par(mfrow = c(2,2));
#hist(xus_returns,breaks=30,main="XUS",xlab="returns", probability=T, col="slateblue1");
#hist(xid_returns,breaks=30,main="XID",xlab="returns", probability=T, col="slateblue1");
#hist(vcn_returns,breaks=30,main="VCN",xlab="returns", probability=T, col="slateblue1");
#hist(vab_returns,breaks=30,main="VAB",xlab="returns", probability=T, col="slateblue1");

#chart.CumReturns(xus_returns,wealth.index = TRUE,legend.loc="topleft", main="Future Value of $1 invested");
#chart.CumReturns(xid_returns,col = "green", wealth.index = TRUE,legend.loc="topleft", main="Future Value of $1 invested");

#Invest entire amount in one of each funds.

xus_pure_growth = coredata(xus_prices)/coredata(xus_prices[1]) * init_investment;
xid_pure_growth = coredata(xid_prices)/coredata(xid_prices[1]) * init_investment;
vcn_pure_growth = coredata(vcn_prices)/coredata(vcn_prices[1]) * init_investment;
vab_pure_growth = coredata(vab_prices)/coredata(vab_prices[1]) * init_investment;


#par(mfrow = c(2,2));
plot(index(xus_prices[xus_index:(xus_index + min_length - 1)]), xus_pure_growth[xus_index:(xus_index + min_length - 1)], type = 'l', main = "XUS", xlab = "time", ylab = "Growth of 10K", col="red");
lines(index(xid_prices[xid_index:(xid_index + min_length - 1)]), xid_pure_growth[xid_index:(xid_index + min_length - 1)], main = "XID", xlab = "time", ylab = "Growth of 10K", col="green");
lines(index(vcn_prices[vcn_index:(vcn_index + min_length - 1)]), vcn_pure_growth[vcn_index:(vcn_index + min_length - 1)], main = "VCN", xlab = "time", ylab = "Growth of 10K", col="blue");
lines(index(vab_prices[vab_index:(vab_index + min_length - 1)]), vab_pure_growth[vab_index:(vab_index + min_length - 1)], main = "VAB", xlab = "time", ylab = "Growth of 10K", col="black");
lines(index(xus_prices[xus_index:(xus_index + min_length - 1)]), total_portfolio_growth, main = "Portfolio", xlab = "time", ylab = "Growth of 10K", col="orange");