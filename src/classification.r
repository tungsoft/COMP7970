count.error = function(bug, prob, threshold = 0.5) {
	n = length(bug)
	x1 = x2 = x3 = x4 = 0
	for (i in 1:n)
		if (prob[i] < threshold)
			if (bug[i] == 0)
				x4 = x4 + 1
			else x3 = x3 + 1		
		else
		if (bug[i] > 0)  
				x1 = x1 + 1
			else 
				x2 = x2 + 1
	precision = x1/(x1 + x2)
	recall = x1/(x1 + x3)
	list(precision = precision, recall = recall, accuracy = (x1 + x4)/n, fscore = 2/(1/precision + 1/recall))
}