defect.project = function(bugcount, npoint, scale = 4, shape = 2) {
	n = length(bugcount)
	volume = sum(bugcount)/sum(dweibull(1:n, shape, scale))
	volume * dweibull(1:npoint, shape, scale)
}

defect.project.nls = function(bugcount, npoint, scale = 4, shape = 2) {
	n = length(bugcount)
	if (n == 1)
		volume = bugcount[1]/dweibull(1, shape, scale)
	else {	
		volume = sum(bugcount)/sum(dweibull(1:n, shape, scale)) # initial guess!
		model = nls(bug ~ vol * dweibull(time, shape, scale), data = data.frame(time = 1:n, bug = bugcount), start = list(vol = volume))
		
		volume = summary(model)$coefficients[1,1] # this first element
	}
	volume * dweibull(1:npoint, shape, scale)
}

