library(tseries);
library(PerformanceAnalytics);
library(zoo);

rm(list=ls());

monte_carlo = function(m, s, num, init_cap) {
	cap = c();
	returns = rnorm(num, mean=m, sd = s);
	cap[1] = init_cap;
	for(i in 2:length(returns)) {
		cap[i] = cap[i-1] * (1 + returns[i]);
	};
	
	return(cap);
}

plot_histograms = function(data, breaks = 40)
{
	ncolumns = ncol(data);
	disp_ncols = floor(sqrt(ncolumns));
	disp_nrows = ceiling(sqrt(ncolumns));

	if (disp_ncols == 1)
		disp_ncols = 2;
	par(mfrow = c(disp_nrows, disp_ncols));
	for (i in 1:ncolumns)
		hist(data[,i], breaks, main=colnames(data)[i]);
	par(mfrow = c(1,1));
}

plot_boxplots = function(data)
{
	ncolumns = ncol(data);
	disp_ncols = floor(sqrt(ncolumns));
	disp_nrows = ceiling(sqrt(ncolumns));

	if (disp_ncols == 1)
		disp_ncols = 2;
	par(mfrow = c(disp_nrows, disp_ncols));
	for (i in 1:ncolumns)
		boxplot(as.vector(data[,i]), main=colnames(data)[i]);
	par(mfrow = c(1,1));
}

plot_qqplots = function(data)
{
	ncolumns = ncol(data);
	disp_ncols = floor(sqrt(ncolumns));
	disp_nrows = ceiling(sqrt(ncolumns));

	if (disp_ncols == 1)
		disp_ncols = 2;
	par(mfrow = c(disp_nrows, disp_ncols));
	for (i in 1:ncolumns) {
		qqnorm(data[,i], main=colnames(data)[i]);
		qqline(data[,i]);
	}
	par(mfrow = c(1,1));
}

# Download and clip data for a single stock symbols. We don't need to find a
# common time frame with defined data as we had to while downloading data for
# multiple stock symbols.
download_clip_data_single = function(sym_list, freq="m")
{
	stock_price = get.hist.quote(sym_list, compression=freq,
				     quote="AdjClose",retclass="zoo");
	stock_price = na.locf(stock_price);
	stock_price = na.trim(stock_price);
	colnames(stock_price) = sym_list;
	return(stock_price);
}

# Download and clip data for a list of stock symbols. The function downloads all
# data, finds the time span for which each stock has defined data and clips the
# data to the shortest time frame so that all stock symbols have data in the
# defined time frame.
download_clip_data_multiple = function(sym_list, freq="m")
{
	for (i in 1:length(sym_list)) {
		if (i == 1) {
			stock_price = get.hist.quote(sym_list[i],
						     compression=freq,
						     quote="AdjClose",
						     retclass="zoo");
			max_date = start(stock_price);
			min_date = end(stock_price);
		} else {
			new_data = get.hist.quote(sym_list[i], compression=freq,
						  quote="AdjClose",
						  retclass="zoo")
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
	max_date;
	min_date;
	return(stock_price);
}

# Common function that will check if we have a single symbol or multiple and call
# the appropriate function above to download data. Use this for downloading any
# data.
download_clip_data = function(sym_list, freq="m")
{
	if (length(sym_list) == 1) {
		return(download_clip_data_single(sym_list, freq));
	} else {
		return(download_clip_data_multiple(sym_list, freq));
	}
}

#clip data to the time frame defined by start and end date.
clip_data = function(stock_data, start_date, end_date)
{
	return(window(stock_data, start=start_date, end=end_date));
}

# Calculate returns for the data in sym_data. Calculates continuous returns, not
# simple returns Return.calculate() calculates simple returns.
calc_returns_continuous = function(sym_data)
{
	return(diff(log(sym_data)));
}

# Calculate returns for the data in sym_data. Calculates continuous returns, not
# simple returns Return.calculate() calculates simple returns.
calc_returns_simple = function(sym_data)
{
	return(Return.calculate(sym_data));
}

# Calculate the value of an investment of "inv_amount" in a stock with returns
# data in sym_data
calc_single_stock_value = function(inv_amount, sym_data)
{
	inv_growth = sym_data/(coredata(sym_data)[1]);
	return(inv_growth * inv_amount);
}

calc_single_investment_value = function(inv_amount, sym_data)
{
	inv_growth = sweep(sym_data, 2, sym_data[1,], "/");
	return(inv_growth * inv_amount);
}

# Calculate the growth of a total amount "inv_amount" invested in the stocks
# whose data is in "sym_data" with an allocation listed in "allocation". This
# function returns the growth of the individual allocations.
calc_portfolio_indiv_values = function(inv_amount, sym_data, allocation)
{
	ret_val = calc_single_investment_value(inv_amount, sym_data);
	return(sweep(ret_val, MARGIN=2,allocation, '*'));
}

# Calculate the growth of a total amount "inv_amount" invested in the stocks
# whose data is in "sym_data" with an allocation listed in "allocation". Returns
# the value of the full portfolio and not the individual stocks.
calc_portfolio_value = function(inv_amount, sym_data, allocation)
{
	ret_val = calc_single_investment_value(inv_amount, sym_data);
	port_val = rowSums(sweep(ret_val, MARGIN=2,allocation, '*'));
	port_val = as.zoo(port_val);
	index(port_val) = index(sym_data);
	return(port_val);
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
			no_of_shares[i,] = rebalance(no_of_shares[i-1,],
						     sym_data[i,],
						     rebalance_alloc);
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

# Calculate mean of returns
calc_returns_mean = function(stock_data, returns = FALSE)
{
	if (returns) {
		stock_returns = calc_returns_continuous(stock_data);
		return(apply(stock_returns, 2, mean));
	}
	
	return(apply(stock_data, 2, mean));
}

# Calculate std. dev of returns
calc_returns_sd = function(stock_data, returns = FALSE)
{
	if (returns) {
		stock_returns = calc_returns_continuous(stock_data);
		return(apply(stock_returns, 2, sd));
	}
	
	return(apply(stock_data, 2, sd));
}

calc_portfolio_return_sd = function(stock_data, allocation)
{
	stock_returns = calc_returns_continuous(stock_data);
	return (sqrt((allocation^2) * calc_returns_sd(stock_returns)));
}
