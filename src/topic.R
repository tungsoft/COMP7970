number.topic = function(number.words, threshold = 0.1) {
	total.words = sum(number.words)
	percent = number.words/total.words
	sum(percent >= threshold)
}