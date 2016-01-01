library(tseries);library(PerformanceAnalytics);library(zoo)

rm(list=ls());

init_investment = 100;
today = Sys.Date();
start = today - 5;
end = today - 1;

all_data = read.table("./TSX_Stock_list.txt",sep='\t');
symbol_list = all_data[,1];
rm(all_data);

simple_returns = c();
compound_returns = c();

for (i in 1:20)
{
	symbol_prices = get.hist.quote(as.character(symbol_list[i]), start=as.character(start), end=as.character(end),
								  quote = c("High","Low"), compression="d",retclass="zoo")
	symbol_prices = coredata(symbol_prices);
	returns = (symbol_prices[,"High"] - symbol_prices[,"Low"])/symbol_prices[,"Low"];
	simple_returns[i] = init_investment * (1 + sum(returns));
	compound_returns[i] = init_investment * prod(returns + 1);
}

init_investment * length(symbol_list)
sum(simple_returns)
sum(compound_returns)

AVO_prices = get.hist.quote("AVO.TO",compression="d",quote="AdjClose",retclass="zoo")