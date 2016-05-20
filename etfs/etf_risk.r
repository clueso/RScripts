source("etfs/etf_portfolio_functions.r");


list = c("XUS.TO", "VAB.TO", "XID.TO", "VCN.TO");
#list = c("SPY", "XIU.TO", "XBB.TO", "INDY");

colours = c("red", "blue", "green", "black", "darkmagenta");
fractions = c(0.4, 0.25, 0.1, 0.25);

full_data = download_clip_data(list, freq="d");
full_returns_data = calc_returns(full_data);
full_data_means = apply(full_returns_data, MARGIN=2, FUN=mean);
full_data_sds = apply(full_returns_data, MARGIN=2, FUN=sd);
full_data_cov = cov(full_returns_data);
full_data_cor = cor(full_returns_data);

#plot_histograms(full_returns_data);
plot_boxplots(full_returns_data);
#plot_qqplots(full_returns_data);
