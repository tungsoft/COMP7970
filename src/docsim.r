sim = function(docx, doc) {
	n = length(docx)
	sump = 0
	sumq = 0
	sumd = 0
	for (i in 1:n) {
		sump = sump + docx[i] * doc[i]
		sumq = sumq + docx[i]^2
		sumd = sumd + docx[i]^2
	} 
	sump/(sqrt(sumq) * sqrt(sumd))
}

sim.doc = function(query, idf, tf.idf) {
	words = names(idf)
	sim = rep(0, ncol(tf.idf))
	for (d in 1:ncol(tf.idf)) 	# go to each document
	{
		sump = 0
		sumq = 0
	    sumd = sum(tf.idf[,d]^2)
		for (i in 1:length(query)) {
			j = which(query[i] == words)		# search for this term
			sump = sump + idf[j] * tf.idf[j, d]
		    sumq = sumq + idf[j]^2
		} 
		sim[d] = sump/(sqrt(sumq) * sqrt(sumd))
	}
	sim
}