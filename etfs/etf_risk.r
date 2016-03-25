source("etfs/etf_portfolio_functions.r");


#list = c("XUS.TO", "VAB.TO", "XID.TO", "VCN.TO");
list = c("SPY", "XIU.TO", "XBB.TO");

colours = c("red", "blue", "green", "black", "orange");
fractions = c(0.5, 0.25, 0.25);

full_data = download_clip_data(list);
full_data[as.Date("2000-11-23"), "SPY"] = full_data[as.Date("2000-11-24"), "SPY"];
full_returns_data = calc_returns(full_data);

par(mfrow = c(2,2));
hist(full_returns_data[,1], breaks=40, main=colnames(full_returns_data)[1]);
hist(full_returns_data[,1], breaks=40, main=colnames(full_returns_data)[2]);
hist(full_returns_data[,1], breaks=40, main=colnames(full_returns_data)[3]);