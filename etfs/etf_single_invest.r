source("etfs/etf_portfolio_functions.r");


list = c("XUS.TO", "VAB.TO", "XID.TO", "VCN.TO");
#list = c("SPY", "XIU.TO", "XBB.TO");

colours = c(1:length(list));
line_width = c(2,2,2,2,2);

full_data = download_clip_data(list);
#full_data[as.Date("2000-11-23"), "SPY"] = full_data[as.Date("2000-11-24"), "SPY"];

inv_growth_data = calc_single_investment_value(10000, full_data);
col = c("red", "blue", "green", "black", "orange")
plot(inv_growth_data, plot.type = "single", col = colours, lwd = line_width);
legend(x = as.Date("2013-09-01"), y = 19000, legend = colnames(inv_growth_data), col = colours, lwd = line_width);