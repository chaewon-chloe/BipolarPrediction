library(glmnet)

# ds1
(x = as.matrix(dat_new[,-1]))
(y = dat_new$SuiHx)

set.seed(111)
cv.glmmod <- cv.glmnet(x, y=as.factor(y), alpha = 1, family="binomial", nfolds = 56, type.measure = "class")
names(cv.glmmod)
plot(cv.glmmod)
(best.lambda <- cv.glmmod$lambda.min)
cv.glmmod$lambda
coef(cv.glmmod, s = 0.1168796)
