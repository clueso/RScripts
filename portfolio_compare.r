source("etfs/etf_portfolio_functions.r");
colours = c("red", "blue", "green", "black", "darkmagenta");
line_width = c(2,2,2,2,2);

port1 = c("RY.TO", "TD.TO");
port2 = c("RY.TO", "AVO.TO");

full_port1_data = download_clip_data(port1);
full_port2_data = download_clip_data(port2);
data_start = index(full_port2_data)[1];
data_end = index(full_port2_data)[length(index(full_port2_data))];
full_port1_data = clip_data(full_port1_data, start = data_start, end = data_end);

port1_growth_data = calc_portfolio_value(10000, full_port1_data, c(0.5, 0.5));
port2_growth_data = calc_portfolio_value(10000, full_port2_data, c(0.75, 0.25));

full_growth_data = merge(port1_growth_data, port2_growth_data);
full_growth_data = na.locf(full_growth_data);
legend_entries = c("high", "low");

#png("test.png", width=800);
plot(full_growth_data, type="l", plot.type = "single", xlab = "Time",
     ylab = "Value of $10000 investment", col = colours, lwd = line_width);
#lines(inv_growth_data, col="darkmagenta", lwd=2);
legend(x = min(index(full_growth_data[,2])), y = floor(max(full_growth_data[,2])*0.75),
       legend_entries, col = colours, lwd = line_width);
grid(nx = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
#dev.off();
