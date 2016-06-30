source("etfs/etf_portfolio_functions.r");

chosen_list = c("RY.TO", "PLI.TO", "AVO.TO");

#all_data = read.table("./TSX_Stock_list.txt",sep='\t');
#test_list = all_data[,1];
test_list = c("TD.TO", "ABT.TO")
#rm(all_data);

# Method 1 - same effect as method 2
#test_list = setdiff(test_list, chosen_list);
#combined_list = c(test_list, chosen_list);
#full_combined_data = download_clip_data(combined_list);
#full_test_data = as.zoo(full_combined_data[,1:length(test_list)]);
#full_chosen_data = as.zoo(full_combined_data[,(length(test_list) + 1):(length(test_list) + length(chosen_list))]);

# Method 2
#full_chosen_data = calc_returns(download_clip_data(chosen_list));
full_cor_matrix = c();
for (stock in test_list) {
	tmp_stock_list = c(chosen_list, stock);
	full_data = calc_returns(download_clip_data(tmp_stock_list));
	chosen_data_end = length(chosen_list);
	full_chosen_data = full_data[,1:chosen_data_end];
	full_test_data = full_data[,(chosen_data_end+1)];
	cor_matrix = cor(full_chosen_data, full_test_data);
	full_cor_matrix = cbind(full_cor_matrix, cor_matrix);
}

#full_test_data = calc_returns(download_clip_data(test_list));
#if (length(full_test_data) > length(full_chosen_data)) {
#	start = index(full_chosen_data)[1];
#	stop = index(full_chosen_data)[nrow(full_chosen_data)];
#	full_test_data = window(full_test_data, start = start, end = stop);
#} else {
#	start = index(full_test_data)[1];
#	stop = index(full_test_data)[nrow(full_test_data)];
#	full_chosen_data = window(full_chosen_data, start = start, end = stop);
#}

#full_cor_matrix = cor(full_chosen_data, full_test_data);
rownames(full_cor_matrix) = chosen_list;
colnames(full_cor_matrix) = test_list;
