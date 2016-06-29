library(tseries);library(PerformanceAnalytics);library(zoo)

rm(list=ls());

means = c();
vars = c();

plot_data <- function(data) {
	returns = diff(log(data));
	max_returns = max(returns);
	min_returns = min(returns);
	x = seq(min_returns, max_returns, length.out=100);
	y = dnorm(mean = mean(returns), sd = sd(returns), x)
	hist(returns);
	par(new=TRUE);
	plot(x,y);
}

returns_mean = function(data){
	returns= diff(log(data));
	return(mean(returns));
}

returns_var = function(data) {
	returns= diff(log(data));
	return (var(returns));
}

calc_stats = function(data, idx) {
	assign("means[idx]", returns_mean(data), envir = .GlobalEnv);
	assign("vars[idx]", returns_var(data), envir = .GlobalEnv);
}

xus_data = read.zoo("./StockData/XUS.csv",sep=',', format="%Y-%m-%d");
means[1] = returns_mean(xus_data);
vars[1] = returns_var(xus_data);

xic_data = read.zoo("./StockData/XIC.csv",sep=',', format="%Y-%m-%d");
means[2] = returns_mean(xic_data);
vars[2] = returns_var(xic_data);

xch_data = read.zoo("./StockData/XCH.csv",sep=',', format="%Y-%m-%d");
means[3] = returns_mean(xch_data);
vars[3] = returns_var(xch_data);

xid_data = read.zoo("./StockData/XID.csv",sep=',', format="%Y-%m-%d");
means[4] = returns_mean(xid_data);
vars[4] = returns_var(xid_data);

xbz_data = read.zoo("./StockData/XBZ.csv",sep=',', format="%Y-%m-%d");
means[5] = returns_mean(xbz_data);
vars[5] = returns_var(xbz_data);

xem_data = read.zoo("./StockData/XEM.csv",sep=',', format="%Y-%m-%d");
means[6] = returns_mean(xem_data);
vars[6] = returns_var(xem_data);

#plot_data(xus_data);
#plot_data(xic_data);
#plot_data(xch_data);
#plot_data(xid_data);
#plot_data(xbz_data);
#plot_data(xem_data);

rets = cbind(means, vars);