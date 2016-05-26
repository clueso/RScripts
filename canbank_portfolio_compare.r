source("etfs/etf_portfolio_functions.r");


canbank = c("RY.TO", "TD.TO", "BMO.TO", "CM.TO");
sp = c("SPY");

colours = c("red", "blue", "green", "black", "darkmagenta");
line_width = c(2,2,2,2,2);
fractions = c(0.25, 0.25, 0.25, 0.25);

full_canbank_data = download_clip_data(canbank);
full_sp_data = download_clip_data(sp);
full_sp_data = clip_data(full_sp_data, start(full_canbank_data), end(full_canbank_data));

port_growth_data = calc_portfolio_value(10000, full_canbank_data, fractions);
inv_growth_data = calc_single_stock_value(10000, full_sp_data);


full_growth_data = merge(inv_growth_data, port_growth_data);
full_growth_data = na.locf(full_growth_data);
legend_entries = c("SP500", "Bank portfolio");
#png("test.png", width=800);
plot(full_growth_data, type="l", plot.type = "single", xlab = "Time", ylab = "Value of $10000 investment", col = colours, lwd = line_width);
#lines(inv_growth_data, col="darkmagenta", lwd=2);
legend(x = min(index(full_growth_data[,2])), y = floor(max(full_growth_data[,2])*0.75), legend_entries, col = colours, lwd = line_width);
#grid(nx = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
#dev.off();

#full_data = download_clip_data(canbank);
#port_growth_data = calc_portfolio_value(10000, full_data[,1:4], fractions);
#full_growth_data = merge(port_growth_data, full_data[,5]);
#plot(full_growth_data, type="l", plot.type = "single", xlab = "Time", ylab = "Value of $10000 investment", col = colours, lwd = line_width);
