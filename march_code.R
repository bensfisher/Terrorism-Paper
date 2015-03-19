rm(list=ls())

pkgs = list('randomForestSRC','foreign','caTools','ROCR','pROC','caret','dplyr','verification','edarf')
invisible(lapply(pkgs, library, character.only=TRUE, quietly=TRUE))

#setwd()
set.seed(1989)
data = read.dta('dyad_month_jun6_onlypr.dta')
data = data[c(1,2,4,6,16,17,20,49,37,10,11,14,15,43)]
data = na.omit(data)

fmla = formula(onset_lead ~ trade_dep + relcap + contig_06 + peacemonths + GTDevent1 + GTDevent2 + civil_chal + civil_targ)

y = data$onset_lead
data$k = createFolds(y, k = 5, list=FALSE)

## Function for cross validation ##

predit <- function(x){
	train = subset(data, k !=x)
	test = subset(data, k==x)
	mod = rfsrc(fmla, data=train, ntree = 1000)
	pred = predict(mod, test)
	test$prob = pred$predicted
	out = subset(test, select = c(k, year, ccode2, ccode1, month, onset_lead, prob))
}
 
 ## 5-fold CV ##
 
test1 = predit(1)
test2 = predit(2)
test3 = predit(3)
test4 = predit(4)
test5 = predit(5)

out = rbind(test1, test2, test3, test4, test5)

## Precision-Recall and ROC ##

pred = prediction(out$prob, out$onset_lead)
roc = performance(pred, 'tpr', 'fpr')
roc.auc = performance(pred, measure = 'auc')
pr = performance(pred, 'prec', 'rec')
prec = pr@y.values[[1]]
recall = pr@x.values[[1]]
prec[is.na(prec)] = 0
pr.auc = trapz(recall, prec)

## Plotting Precision-Recall and ROC ##

plot(roc, main='ROC Curve')
legend(.5,.4, paste('AUC =', round(roc.auc@y.values[[1]], digits=2))

plot(pr, main='Precision-Recall Curve', ylim=c(0,1))
legend(.5,.4, paste('AUC =', round(pr.auc, digits=2)))

## partial dependence and variable importance ##

mod = rfsrc(fmla, data=data, ntree = 1000, importance='permute')

varimp = vimp(mod)
fit = varimp$importance[,1]
plot_imp(names(fit), fit, xlab = 'Mean Accuracy Decrease')

plot.variable(mod, xvar.names = c('trade_dep','contig_06','relcap','peacemonths','GTDevent1','GTDevent2'), which.outcome='1', partial=TRUE, ylab='Probability of Onset')