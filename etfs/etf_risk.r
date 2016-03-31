source("etfs/etf_portfolio_functions.r");


#list = c("XUS.TO", "VAB.TO", "XID.TO", "VCN.TO");
list = c("SPY", "XIU.TO", "XBB.TO", "INDY");

colours = c("red", "blue", "green", "black", "orange");
fractions = c(0.5, 0.25, 0.25);

full_data = download_clip_data(list);
#full_data[as.Date("2000-11-23"), "SPY"] = full_data[as.Date("2000-11-24"), "SPY"];
full_returns_data = calc_returns(full_data);
full_data_means = apply(full_returns_data, MARGIN=2, FUN=mean);
full_data_sds = apply(full_returns_data, MARGIN=2, FUN=sd);
full_data_cov = cov(full_returns_data);
full_data_cor = cor(full_returns_data);

par(mfrow = c(2,2));
hist(full_returns_data[,1], breaks=40, main=colnames(full_returns_data)[1]);
hist(full_returns_data[,1], breaks=40, main=colnames(full_returns_data)[2]);
hist(full_returns_data[,1], breaks=40, main=colnames(full_returns_data)[3]);
hist(full_returns_data[,1], breaks=40, main=colnames(full_returns_data)[4]);
