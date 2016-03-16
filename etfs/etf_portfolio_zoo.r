library(tseries);
library(PerformanceAnalytics);
library(zoo);

rm(list=ls());
download_clip_data = function(sym_list)
{
	for (i in 1:length(sym_list)) {
		if (i == 1) {
			stock_price = get.hist.quote(sym_list[i], compression="d",quote="AdjClose",retclass="zoo");
			max_date = start(stock_price);
			min_date = end(stock_price);
		} else {
			new_data = get.hist.quote(sym_list[i], compression="d",quote="AdjClose",retclass="zoo")
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
	inv_growth = sweep(sym_data, 2, sym_data[1,], "/");
	return(inv_growth * inv_amount);
}

calc_portfolio_value = function(inv_amount, sym_data, allocation)
{
	ret_val = calc_single_investment_value(inv_amount, sym_data);
	return(rowSums(sweep(ret_val, MARGIN=2,allocation, '*')));
}



#list = c("XUS.TO", "VAB.TO", "VE.TO");
list = c("SPY", "XIU.TO", "XBB.TO");
fractions = c(0.33, 0.33, 0.34);

colours = c(1:length(list));
line_width = c(2,2);
full_data = download_clip_data(list);
full_data[as.Date("2000-11-23"), "SPY"] = full_data[as.Date("2000-11-24"), "SPY"];
full_returns_data = calc_returns(full_data);
inv_growth_data = calc_single_investment_value(10000, full_data);
port_growth_data = calc_portfolio_value(10000, full_data, fractions);
inv_growth_data = merge(inv_growth_data, port_growth_data);
#col = c("red", "blue", "green", "black", "orange")
plot(inv_growth_data, plot.type = "single", col = colours, lwd = line_width);
lines(port_growth_data, col = "slateblue", lwd = line_width);
#legend(x = as.Date("2013-09-01"), y = 19000, legend = colnames(full_returns_data), col = colours, lwd = line_width);

#par(mfrow = c(2,2));
