library(tseries);
library(PerformanceAnalytics);
library(zoo);

rm(list=ls());

download_clip_data_single = function(sym_list, freq="m")
{
	stock_price = get.hist.quote(sym_list, compression=freq, quote="AdjClose",retclass="zoo");
	stock_price = na.locf(stock_price);
	stock_price = na.trim(stock_price);
	colnames(stock_price) = sym_list;
	return(stock_price);
}

download_clip_data_multiple = function(sym_list, freq="m")
{
	for (i in 1:length(sym_list)) {
		if (i == 1) {
			stock_price = get.hist.quote(sym_list[i], compression=freq,quote="AdjClose",retclass="zoo");
			max_date = start(stock_price);
			min_date = end(stock_price);
		} else {
			new_data = get.hist.quote(sym_list[i], compression=freq,quote="AdjClose",retclass="zoo")
			if (max_date < start(new_data))
				max_date = start(new_data);
			if (min_date > end(new_data))
				min_date = end(new_data);
			stock_price = merge(stock_price, new_data);
		}
	}
	stock_price = window(stock_price, start = max_date, end = min_date);
	stock_price = na.locf(stock_price);
	stock_price = na.trim(stock_price);
	colnames(stock_price) = sym_list;
	return(stock_price);
}

download_clip_data = function(sym_list, freq="m")
{
	if (length(sym_list) == 1) {
		return(download_clip_data_single(sym_list, freq));
	} else {
		return(download_clip_data_multiple(sym_list, freq));
	}
}

clip_data = function(stock_data, start_date, end_date)
{
	return(window(stock_data, start=start_date, end=end_date));
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

calc_portfolio_indiv_values = function(inv_amount, sym_data, allocation)
{
	ret_val = calc_single_investment_value(inv_amount, sym_data);
	return(sweep(ret_val, MARGIN=2,allocation, '*'));
}

calc_portfolio_value = function(inv_amount, sym_data, allocation)
{
	ret_val = calc_single_investment_value(inv_amount, sym_data);
	return(rowSums(sweep(ret_val, MARGIN=2,allocation, '*')));
}
calc_shares = function(prices, investments)
{
	return(investments/prices);
}

rebalance = function(no_of_shares, curr_prices, allocation)
{
	total_wealth = sum(no_of_shares*curr_prices);
	return((total_wealth*allocation)/curr_prices);
}

calc_rebalanced_portfolio_value = function(inv_amount, sym_data, rebalance_alloc)
{
	sym_coredata = coredata(sym_data);
	no_of_shares = matrix(nrow = dim(sym_data)[1], ncol = dim(sym_data)[2]);
	inv_capital = inv_amount * rebalance_alloc;
	no_of_shares[1,] = calc_shares(sym_data[1,], inv_capital);
	next_rebalance_day = index(sym_data)[2];
	for (i in 2:dim(sym_data)[1]) {
		if (index(sym_data)[i] == next_rebalance_day) {
			no_of_shares[i,] = rebalance(no_of_shares[i-1,], sym_data[i,], rebalance_alloc);
			if (i <= (dim(sym_data)[1] - 4))
				next_rebalance_day = index(sym_data)[i+4];
		} else {
			no_of_shares[i,] = no_of_shares[i-1,];
		}
	}
	no_of_shares_zoo = as.zoo(no_of_shares);
	index(no_of_shares_zoo) = index(sym_data);
	colnames(no_of_shares_zoo) = colnames(sym_data);
	return(no_of_shares_zoo);
}
