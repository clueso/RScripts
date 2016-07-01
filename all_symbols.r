source("etfs/etf_portfolio_functions.r");

chosen_list = c("RY.TO", "PLI.TO", "AVO.TO");

test_list = c("TD.TO", "ABT.TO")
full_chosen_data = calc_returns(download_clip_data(chosen_list));
full_cor_matrix = c();
for (stock in test_list) {
	full_test_data = calc_returns(download_clip_data(stock));
	if (nrow(full_test_data) > nrow(full_chosen_data)) {
		start = index(full_chosen_data)[1];
		stop = index(full_chosen_data)[nrow(full_chosen_data)];
		test_data = window(full_test_data, start = start, end = stop);
		chosen_data = full_chosen_data;
	} else {
		start = index(full_test_data)[1];
		stop = index(full_test_data)[nrow(full_test_data)];
		chosen_data = window(full_chosen_data, start = start, end = stop);
		test_data = full_test_data;
	}
	cor_matrix = cor(chosen_data, test_data);
	full_cor_matrix = cbind(full_cor_matrix, cor_matrix);
}

rownames(full_cor_matrix) = chosen_list;
colnames(full_cor_matrix) = test_list;
