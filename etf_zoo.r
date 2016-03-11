library(tseries);
library(PerformanceAnalytics);
library(zoo);

rm(list=ls());
download_clip_data = function(sym_list)
{
	for (i in 1:length(sym_list)) {
		if (i ==1) {
			stock_price = get.hist.quote(sym_list[i], compression="d",quote="AdjClose",retclass="zoo");
			max_date = start(stock_price);
			min_date = end(stock_price);
		} else {
			stock_price = merge(stock_price, get.hist.quote(sym_list[i], compression="d",quote="AdjClose",retclass="zoo"));
			if (max_date < start(stock_price))
				max_date = start(stock_price);
			if (min_date > end(stock_price))
				min_date = end(stock_price);
		}
	}
	stock_price = window(stock_price, start = max_date, end = min_date);
	stock_price = na.locf(stock_price);
	colnames(stock_price) = sym_list;
	return(stock_price);
}

list = c("XUS.TO", "XID.TO");
full_data = download_clip_data(list);

		
