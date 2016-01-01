library(tseries);library(PerformanceAnalytics);library(zoo)

rm(list=ls());

init_investment = 100;
today = Sys.Date();
start = today - 5;
end = today - 1;

all_data = read.table("./TSX_Stock_list.txt",sep='\t');
symbol_list = all_data[,1];
rm(all_data);

snp_prices = get.hist.quote("^GSPC", quote = c("AdjClose"), compression="m",retclass="zoo");
index(snp_prices) = as.yearmon(index(snp_prices));
all_prices = snp_prices;
col_names = c("S&P500");
non_na_index = c(1);

for (i in 1:10)
{
	symbol_prices = get.hist.quote(as.character(symbol_list[i]), quote = c("AdjClose"), compression="m",retclass="zoo");
	index(symbol_prices) = as.yearmon(index(symbol_prices));
	all_prices = merge(all_prices, symbol_prices);
	col_names = c(col_names, as.character(symbol_list[i]));
	tmp = which(is.na(all_prices[,i+1]));
	if(length(tmp) > 0)
		non_na_index = c(non_na_index, max(tmp)+1);
}

colnames(all_prices) = col_names;
