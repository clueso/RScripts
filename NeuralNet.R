library('neuralnet');

training_input1 = as.data.frame(runif(100,max=100,min=0));
training_input2 = as.data.frame(runif(100,max=100,min=0));
training_output = sqrt(training_input1+training_input2);

training_set = cbind(training_input1,training_input2,training_output);
colnames(training_set) = c("Input1","Input2","Output");
net = neuralnet(as.formula('Output~Input1+Input2'),training_set,hidden=3,threshold=0.01);

