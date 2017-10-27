leave1out = function(equation, dat) {
	n = nrow(dat) 	# n is the number of rows of this dataset
	pred = rep(0, n) # pred is a vector of n zeros
	for (k in 1:n) {
		traindata = dat[-k,]  # traindata contains every row except row k
		model = lm(equation, data = traindata) # train the model without row k
		pred[k] = predict(model, dat[k,]) # make prediction for row k		
	}
	pred
}

cross.validation = function(equation, dat, K) {
	n = nrow(dat) 	# n is the number of rows of this dataset
	m = n/K # number of testing datapoints for each iteration
	pred = rep(0, n) # pred is a vector of n zeros
	for (k in 1:K) {
		a = (k-1) * m + 1
		if (k == K)
			b = n
		else	
			b = k * m
		test = a:b
		model = lm(equation, data = dat[-test,]) # train the model without rows from a to b
		pred[test] = predict(model, dat[test,]) # make prediction for rows from a to b		
	}
	pred
}

mre = function(actual, predicted) {# mean relative error
	mean(abs(actual - predicted)/actual)
}


