library(tseries);library(PerformanceAnalytics);library(zoo)

rm(list = ls());

init_investment = 10000;

download_clip_data = function(sym_list)
{
	start_index = c();
	sym_data = c();
	sym_coredata = c();

	min_len = 1000000000;

	for (i in 1:length(sym_list)) {
		sym_data[[i]] = get.hist.quote(sym_list[i], quote = c("AdjClose"), compression="d",retclass="zoo");
		if (i == 1) {
			max_date = index(sym_data[[i]])[1];
		} else {
			if (max_date < index(sym_data[[i]])[1])
				max_date = index(sym_data[[i]])[1];
		}
	}

	for (i in 1:length(sym_list)) {
		start_index[i] = match(max_date, index(sym_data[[i]]));
		print(start_index);
		if (min_len > (length(coredata(sym_data[[i]])) - start_index[i]))
			min_len = length(coredata(sym_data[[i]])) - start_index[i];
	}
	print(min_len);
	for (i in 1:length(sym_list)) {
		sym_coredata[[i]] = coredata(sym_data[[i]])[start_index[i]:(start_index[i] + min_len)];
	}
	
	full_data = data.frame(index(sym_data[[1]])[start_index[1]:(start_index[1] + min_len)], sym_coredata);
	colnames(full_data) = c("Date", sym_list);
	return(full_data);
}

symbols = c("XUS.TO", "XID.TO");
stock_data = download_clip_data(symbols);
