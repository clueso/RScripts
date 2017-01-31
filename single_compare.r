source("etfs/etf_portfolio_functions.r");


list = c("SPY", "AVGO");

colours = c(1:length(list));
line_width = c(2,2,2,2,2);

full_data = download_clip_data(list);

inv_growth_data = calc_single_investment_value(10000, full_data);
col = c("red", "blue", "green", "black", "orange")
plot(inv_growth_data, plot.type = "single", col = colours, lwd = line_width);
legend(x = as.Date("2008-01-01"), y = 20000, legend = colnames(inv_growth_data), col = colours, lwd = line_width);