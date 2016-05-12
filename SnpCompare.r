library(tseries);library(PerformanceAnalytics);library(zoo)

source("etfs/etf_portfolio_functions.r");
plot_single_window = function(values)
{
	num_cols = ncol(values);
	yrange = c(0, max(values));
	colours = c("red", "green", "blue", "orange");

	plot(index(values), values[,1], type="l", ylim=yrange);

	for (i in 2:num_cols)
		lines(index(values), values[,i], col=colours[(i-1) %% 4]);
}

init_investment = 100;
today = Sys.Date();
start = today - 5;
end = today - 1;

#all_data = read.table("./TSX_Stock_list.txt",sep='\t');
#symbol_list = t(all_data[,1]);
#rm(all_data);
symbol_list = c("XUS.TO", "VCN.TO");
snp_prices = get.hist.quote("^GSPC",compression="w",quote="AdjClose",retclass="zoo");

total_data = download_clip_data(symbol_list);
total_returns = calc_single_investment_value(10000, total_data);
plot_single_window(total_returns);
