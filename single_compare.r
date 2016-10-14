source("etfs/etf_portfolio_functions.r");
colours = c("red", "blue", "green", "black", "darkmagenta");
line_width = c(2,2,2,2,2);

port1 = c("TSM", "SPY");

full_port1_data = download_clip_data(port1);

port1_growth_data = calc_portfolio_indiv_values(20000, full_port1_data, c(0.5, 0.5));

full_growth_data = na.locf(port1_growth_data);
legend_entries = c("TSM", "SPY");

#png("test.png", width=800);
plot(full_growth_data, type="l", plot.type = "single", xlab = "Time",
     ylab = "Value of $10000 investment", col = colours, lwd = line_width);
#lines(inv_growth_data, col="darkmagenta", lwd=2);
legend(x = min(index(full_growth_data[,2])), y = floor(max(full_growth_data[,2])*0.75),
       legend_entries, col = colours, lwd = line_width);
grid(nx = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
#dev.off();
