library(tseries);library(PerformanceAnalytics);library(zoo)

rm(list=ls());

init_investment = 100;
today = Sys.Date();
start = today - 5;
end = today - 1;

all_data = read.table("./TSX_Stock_list.txt",sep='\t');
symbol_list = all_data[,1];
rm(all_data);

snp_prices = get.hist.quote("^GSPC",compression="w",quote="AdjClose",retclass="zoo");

for (i in 1:20)
	symbol_prices = get.hist.quote(symbol_list[i],compression="w",quote="AdjClose",retclass="zoo")
