myData <- CommViolPredUnnormalizedData
myData = as.data.frame(unclass(myData))
myData[myData =="?"]=NA
myData = myData[, -130:-145]
myData = myData[, -131]

cols.num <- c("X104", "X105", "X106", "X107", "X108", "X109", "X110","X111"
              ,"X112", "X113", "X114", "X115", "X116", "X117", "X118", "X119",
              "X120","X121","X122", "X123","X124" ,"X125", "X126", "X127", "X128" ,"X129", "X146")
myData[cols.num] <- sapply(myData[cols.num],as.numeric)
sapply(myData, class)

cols.num <- c("X1","X2","X3","X4","X104", "X105", "X106", "X107", "X108", "X109", "X110","X111"
              ,"X112", "X113", "X114", "X115", "X116", "X117", "X118", "X119",
              "X120","X124" ,"X125", "X126", "X127" 
              ,"X129")
myData[,c(cols.num)]=NULL
#myData$X146 = as.numeric(as.character(myData$X146))

myData= na.omit(myData)
summary(myData)
# building a simple linear regression model
glm.fit = glm(X146~.,data = myData)
cv.out = cv.glm(myData, glm.fit, K=10)
cv.out$delta[1]
summary(myData$X146)

# MODEL for Ridge and Lasso
x=model.matrix(X146~.,myData)[,-1]
y=myData$X146
cv.out=cv.glmnet(x,y,alpha=0)
names(cv.out)

# Ridge out put 
cv.out$lambda.min
#cv.out$cvm
min(cv.out$cvm)

# Lasso out put
cv.out=cv.glmnet(x,y,alpha=1)
min(cv.out$cvm)
bestlam =cv.out$lambda.min

#testing lasso with grid
lasso.coef=predict(cv.out,type ="coefficients",s=bestlam )[1:103,]
lasso.coef[lasso.coef!=0]

#PCR
pcr.fit = pcr(X146~., data=myData, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
summary(pcr.fit)

# selecting set of training rows for CV 
train = sample(1:1993,1000)
train.x = scale(x[train,])
test.x = scale(x[-train,])
train.y = y[train]
test.y = y[-train]
knn.fit = knn.reg(train.x, test.x, train.y, k=103)
mean((test.y - knn.fit$pred)^2)

errs = rep(0,15)
for(i in 1:15){
knn.fit = knn.reg(train.x, test.x, train.y, k=i)
errs[i] = mean((test.y - knn.fit$pred)^2)
}
errs

#k-fold CV
bins = sample(1:10,1993, replace = TRUE)
binErrs = rep(0,10)
for(k in 1:10){
train.x = scale(x[bins != k,])
test.x = scale(x[bins == k,])
train.y = y[bins != k]
test.y = y[bins == k]
knn.fit = knn.reg(train.x, test.x, train.y, k=103)
binErrs[k] = mean((test.y - knn.fit$pred)^2)
}
mean(binErrs)

#combine analysis
errs = rep(0,15)
for(i in 1:15){
for(k in 1:10){
train.x = scale(x[bins != k,])
test.x = scale(x[bins == k,])
train.y = y[bins != k]
test.y = y[bins == k]
knn.fit = knn.reg(train.x, test.x, train.y, k=i)
binErrs[k] = mean((test.y - knn.fit$pred)^2)
}
errs[i] = mean(binErrs)
}
errs








