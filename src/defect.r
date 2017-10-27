load.data = function(path) {
	old.path = getwd()
	setwd(path)
	code.metrics = read.csv('single-version-ck-oo.csv', header = T, sep = ';', row.names = 1)
	bug.metrics = read.csv('bug-metrics.csv', header = T, sep = ';', row.names = 1)
	change.metrics = read.csv('change-metrics.csv', header = T, sep = ';',row.names = 1)
	change.complex.metrics = read.csv('complexity-code-change.csv', header = T, sep = ';', row.names = 1)	
	setwd(old.path)
	result = data.frame(code.metrics[,1:17], change.metrics[,1:15], change.complex.metrics[,1:5], bug.metrics[,c(1,2,6)])
	
	result
}

jdt = load.data('C:\\Teaching\\COMP 7970\\Data\\jdt')
pde = load.data('C:\\Teaching\\COMP 7970\\Data\\pde')[-7,] # remove 7th row: on PDEUIMessages
equ = load.data('C:\\Teaching\\COMP 7970\\Data\\equinox')
myl = load.data('C:\\Teaching\\COMP 7970\\Data\\mylyn')
luc = load.data('C:\\Teaching\\COMP 7970\\Data\\lucene')

calculate.correlation = function(dat, met = 'pearson') {
	n = ncol(dat) - 1
	cor.val = rep(0, n)
	for (i in 1:n)
		cor.val[i] = cor.test(dat[,i], dat[,n+1], method = met)$estimate
	names(cor.val) = names(dat)[1:n]
	return(cor.val)
} 

jdt.cor = calculate.correlation(jdt)
pde.cor = calculate.correlation(pde)
equ.cor = calculate.correlation(equ)
myl.cor = calculate.correlation(myl)
luc.cor = calculate.correlation(luc)

all.cor = data.frame(jdt.cor, pde.cor, equ.cor, myl.cor, luc.cor)
new.cor = all.cor[1:39,]

new.cor$min = rep(0, 39)
for (i in 1:39) new.cor$min[i] = min(new.cor[i, 1:5])

new.cor$max = rep(0, 39)
for (i in 1:39) new.cor$max[i] = max(new.cor[i, 1:5])

new.cor$avg = rep(0, 39)
for (i in 1:39) new.cor$avg[i] = sum(new.cor[i, 1:5])/5

cv.all = function(form) {
	mean(cross.val.divide(form, jdt),cross.val.divide(form, pde), cross.val.divide(form, equ), cross.val.divide(form, myl), cross.val.divide(form, luc), na.rm = T)
}
