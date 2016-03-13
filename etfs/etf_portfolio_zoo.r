library(tseries);
library(PerformanceAnalytics);
library(zoo);

rm(list=ls());
download_clip_data = function(sym_list)
{
	for (i in 1:length(sym_list)) {
		if (i == 1) {
			stock_price = get.hist.quote(sym_list[i], end = "2014-07-01", compression="d",quote="AdjClose",retclass="zoo");
			max_date = start(stock_price);
			min_date = end(stock_price);
		} else {
			new_data = get.hist.quote(sym_list[i], end = "2014-07-01", compression="d",quote="AdjClose",retclass="zoo")
			if (max_date < start(new_data))
				max_date = start(new_data);
			if (min_date > end(new_data))
				min_date = end(new_data);
			stock_price = merge(stock_price, new_data);
		}
	}
	stock_price = window(stock_price, start = max_date, end = min_date);
	stock_price = na.locf(stock_price);
	colnames(stock_price) = sym_list;
	return(stock_price);
}

calc_returns = function(sym_data)
{
	return(diff(log(sym_data)));
}

calc_single_investment_value = function(inv_amount, sym_data)
{
	inv_growth = sweep(sym_data, 2, sym_data[1,], "/")
	return(inv_growth * inv_amount);
}

list = c("XUS.TO", "XCH.TO", "XID.TO");
full_data = download_clip_data(list);
full_returns_data = calc_returns(full_data);
inv_growth_data = calc_single_investment_value(10000, full_data);
plot(inv_growth_data, plot.type = "single", col = c("red", "blue"), lwd = 2);

#par(mfrow = c(2,2));
