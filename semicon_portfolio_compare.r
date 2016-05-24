source("etfs/etf_portfolio_functions.r");


semicon = c("INTC", "QCOM", "TSM", "ARMH");
sp = c("SPY");

colours = c("red", "blue", "green", "black", "darkmagenta");
line_width = c(2,2,2,2,2);
fractions = c(0.25, 0.25, 0.25, 0.25);

full_semicon_data = download_clip_data(semicon);
full_sp_data = download_clip_data(sp);
full_sp_data = clip_data(full_sp_data, start(full_semicon_data), end(full_semicon_data));

port_growth_data = calc_portfolio_value(10000, full_semicon_data, fractions);
inv_growth_data = calc_single_stock_value(10000, full_sp_data);


full_growth_data = merge(inv_growth_data, port_growth_data);
legend_entries = c(sp, semicon);
#png("test.png", width=800);
plot(full_growth_data, type="l", plot.type = "single", xlab = "Time", ylab = "Value of $10000 investment", col = colours, lwd = line_width);
#lines(inv_growth_data, col="darkmagenta", lwd=2);
legend(x = as.Date("2002-09-01"), y = 250000, legend_entries, col = colours, lwd = line_width);
grid(nx = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
#dev.off();
