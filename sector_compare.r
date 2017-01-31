source("etfs/etf_portfolio_functions.r");


sectors = c("XLK", "SPY");

colours = c("red", "blue", "green", "black", "darkmagenta");
line_width = c(2,2,2,2,2);

full_sector_data = download_clip_data(sectors);
inv_growth_data = calc_single_investment_value(10000, full_sector_data);
inv_growth_data = na.locf(inv_growth_data);

legend_entries = c("Tech", "SP500");
#png("test.png", width=800);
plot(inv_growth_data, type="l", plot.type = "single", xlab = "Time",
     ylab = "Value of $10000 investment", col = colours, lwd = line_width);
#lines(inv_growth_data, col="darkmagenta", lwd=2);
legend(x = min(index(inv_growth_data[,2])), y = floor(max(inv_growth_data[,2])*0.90),
       legend_entries, col = colours, lwd = line_width);
grid(nx = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
#dev.off();
