source("etfs/etf_portfolio_functions.r");
library(neuralnet);

list = c("AVO.TO");

data = download_clip_data(list, freq="d");
simple_returns = coredata(diff(data, 1)/lag(data, 1));

train_length = length(data)*2/3;
train_in = data.frame(simple_returns[1:(train_length-5)],simple_returns[2:(train_length-4)],simple_returns[3:(train_length-3)],
			    simple_returns[4:(train_length-2)],simple_returns[5:(train_length-1)],simple_returns[6:train_length]);
colnames(train_in) = c("In1","In2","In3","In4","In5", "Out")

net = neuralnet(Out~In1+In2+In3+In4+In5,train_in,hidden=15,threshold=0.01); 
