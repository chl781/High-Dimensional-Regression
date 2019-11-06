####################### First Data Generation ########################
#In fact, in the test error and cv error parts we will generate it again.
#So, this is for convience.

# First dataset
set.seed(1)
X1 = matrix(0,nrow = 5000,ncol = 100)
for(i in 1:5000){
  for(j in 1:100){
    if(i<=50 & j <= 50){
      X1[i,j] = 3+rnorm(1)
    }else if(i <=50 & j >50){
      X1[i,j] = 4+rnorm(1)
    }else{
      X1[i,j] = 3.5 + rnorm(1)
    }
  }
}

y = apply(X1[1:50,],2,sum)
y = y/25+rnorm(100,sd=1.5)

train = sample(1:nrow(t(X1)),floor(nrow(t(X1))*0.7))
X1 = t(X1)


####################### First Data Generation ########################

set.seed(1)
X2 = matrix(0,nrow = 5000,ncol = 100)
u1 = runif(100)
u2 = runif(100)
u3 = runif(100)
for(i in 1:5000){
  for(j in 1:100){
    if(i<=50 & j <= 50){
      X2[i,j] = 3+rnorm(1)
    }else if(i <=50 & j >50){
      X2[i,j] = 4+rnorm(1)
    }else if(i <= 100){
      X2[i,j] = 3.5+1.5*(u1[j]<0.4)+rnorm(1)
      
    }else if(i <=200){
      X2[i,j] = 3.5+0.5*(u2[j]<0.7)+rnorm(1)
      
    }else if(i <=300){
      X2[i,j] = 3.5-1.5*(u3[j]<0.3)+rnorm(1)
      
    }else{
      X2[i,j] = 3.5 + rnorm(1)
    }
  }
}
y = apply(X2[1:50,],2,sum)
y = y/25+rnorm(100,sd=1.5)
X2 = t(X2)


####################### PCA: First Dataset ########################
set.seed(1)
X1 = matrix(0,nrow = 5000,ncol = 100)
for(i in 1:5000){
  for(j in 1:100){
    if(i<=50 & j <= 50){
      X1[i,j] = 3+rnorm(1)
    }else if(i <=50 & j >50){
      X1[i,j] = 4+rnorm(1)
    }else{
      X1[i,j] = 3.5 + rnorm(1)
    }
  }
}
y = apply(X1[1:50,], 2, sum)
y = y/25+rnorm(100,sd = 1.5)
X1 = t(X1)

pcr.model = pcr(y ~ X1, scale = T,validation = "CV")
validationplot(pcr.model,val.type = "MSEP")


X1 = matrix(0,nrow = 5000,ncol = 100)
for(i in 1:5000){
  for(j in 1:100){
    if(i<=50 & j <= 50){
      X1[i,j] = 3+rnorm(1)
    }else if(i <=50 & j >50){
      X1[i,j] = 4+rnorm(1)
    }else{
      X1[i,j] = 3.5 + rnorm(1)
    }
  }
}
y = apply(X1[1:50,], 2, sum)
y = y/25+rnorm(100,sd = 1.5)
X1 = t(X1)


test_error = 100000
k = 100
for(i in 1:89){
  pred.y = predict(pcr.model,X1,ncomp = i)
  error = sum((y-pred.y)^2)
  if(i == 1){
    cat("The test error for one principle component is ",error,"\n")
  }
  if(error<test_error){
    k = i
    test_error = error
  }
}

cat("The optimal number is ",k,"\n")
cat("The minimum test error is ",error,"\n")


############################# PCA: Second Dataset ###################################
X2 = matrix(0,nrow = 5000,ncol = 100)
u1 = runif(100)
u2 = runif(100)
u3 = runif(100)
for(i in 1:5000){
  for(j in 1:100){
    if(i<=50 & j <= 50){
      X2[i,j] = 3+rnorm(1)
    }else if(i <=50 & j >50){
      X2[i,j] = 4+rnorm(1)
    }else if(i <= 100){
      X2[i,j] = 3.5+1.5*(u1[j]<0.4)+rnorm(1)
      
    }else if(i <=200){
      X2[i,j] = 3.5+0.5*(u2[j]<0.7)+rnorm(1)
      
    }else if(i <=300){
      X2[i,j] = 3.5-1.5*(u3[j]<0.3)+rnorm(1)
      
    }else{
      X2[i,j] = 3.5 + rnorm(1)
    }
  }
}
y2 = apply(X2[1:50,],2,sum)
y2 = y2/25+rnorm(100, sd = 1.5)
X2 = t(X2)

pcr.model2 = pcr(y2 ~ X2, scale = T,validation = "CV")
validationplot(pcr.model2,val.type = "MSEP")

X2 = matrix(0,nrow = 5000,ncol = 100)
u1 = runif(100)
u2 = runif(100)
u3 = runif(100)
for(i in 1:5000){
  for(j in 1:100){
    if(i<=50 & j <= 50){
      X2[i,j] = 3+rnorm(1)
    }else if(i <=50 & j >50){
      X2[i,j] = 4+rnorm(1)
    }else if(i <= 100){
      X2[i,j] = 3.5+1.5*(u1[j]<0.4)+rnorm(1)
      
    }else if(i <=200){
      X2[i,j] = 3.5+0.5*(u2[j]<0.7)+rnorm(1)
      
    }else if(i <=300){
      X2[i,j] = 3.5-1.5*(u3[j]<0.3)+rnorm(1)
      
    }else{
      X2[i,j] = 3.5 + rnorm(1)
    }
  }
}
y2 = apply(X2[1:50,],2,sum)
y2 = y2/25+rnorm(100, sd = 1.5)
X2 = t(X2)


test_error = 100000
k = 1000
for(i in 1:89){
  pred.y = predict(pcr.model2,X2,ncomp = i)
  error = sum((y2-pred.y)^2)
  if(i == 1){
    cat("The test error for one principle component is ",error,"\n")
  }
  if(error<test_error){
    k = i
    test_error = error
  }
}
cat("The optimal number is ",k,"\n")
cat("The minimum test error is ",error,"\n")




############################# PLS ###################################
library(pls)

#################### PLS: First Dataset #############################
set.seed(1)
u=c()
for(times in 1:10){
  X1 = matrix(0,nrow = 5000,ncol = 100)
  for(i in 1:5000){
    for(j in 1:100){
      if(i<=50 & j <= 50){
        X1[i,j] = 3+rnorm(1)
      }else if(i <=50 & j >50){
        X1[i,j] = 4+rnorm(1)
      }else{
        X1[i,j] = 3.5 + rnorm(1)
      }
    }
  }
  
  y = apply(X1[1:50,], 2, sum)
  y = y/25 + rnorm(100, sd = 1.5)
  
  X1 = t(X1)
  
  # use {y,X1} train model
  pls.model = plsr(y ~ X1, scale=TRUE, validation = "CV",segments = 10)
  validationplot(pls.model, val.type="MSEP")
  
  #new data generation
  X2 = matrix(0,nrow = 5000,ncol = 100)
  for(i in 1:5000){
    for(j in 1:100){
      if(i<=50 & j <= 50){
        X2[i,j] = 3+rnorm(1)
      }else if(i <=50 & j >50){
        X2[i,j] = 4+rnorm(1)
      }else{
        X2[i,j] = 3.5 + rnorm(1)
      }
    }
  }
  
  y1 = apply(X2[1:50,],2,sum)
  y1 = y1/25 + rnorm(100, sd = 1.5)
  
  X2 = t(X2)
  #use {y1,X2} to predict model and gain the prediction error
  
  pls.pred = predict( pls.model , X2, ncomp = 8)
  u[times] = sum((pls.pred - y1)^2)
  
  #assign error
}
mean(u)

####################### PLS: Second Dataset ########################


test.error = c()

set.seed(1)
for(times in 1:10){
  
  X1 = matrix(0,nrow = 5000,ncol = 100)
  u1 = runif(100)
  u2 = runif(100)
  u3 = runif(100)
  for(i in 1:5000){
    for(j in 1:100){
      if(i<=50 & j <= 50){
        X1[i,j] = 3+rnorm(1)
      }else if(i <=50 & j >50){
        X1[i,j] = 4+rnorm(1)
      }else if(i <= 100){
        X1[i,j] = 3.5+1.5*(u1[j]<0.4)+rnorm(1)
        
      }else if(i <=200){
        X1[i,j] = 3.5+0.5*(u2[j]<0.7)+rnorm(1)
        
      }else if(i <=300){
        X1[i,j] = 3.5-1.5*(u3[j]<0.3)+rnorm(1)
        
      }else{
        X1[i,j] = 3.5 + rnorm(1)
      }
    }
  }
  y = apply(X1[1:50,],2,sum)
  y = y/25+rnorm(100,sd=1.5)
  X1 = t(X1)
  
  # use {y,X1} train model
  pls.model2 = plsr(y ~ X1, scale=TRUE, validation = "CV",segments = 10)
  # validationplot(pls.model2, val.type="MSEP")
  
  #new data generation
  
  X2 = matrix(0,nrow = 5000,ncol = 100)
  u1 = runif(100)
  u2 = runif(100)
  u3 = runif(100)
  for(i in 1:5000){
    for(j in 1:100){
      if(i<=50 & j <= 50){
        X2[i,j] = 3+rnorm(1)
      }else if(i <=50 & j >50){
        X2[i,j] = 4+rnorm(1)
      }else if(i <= 100){
        X2[i,j] = 3.5+1.5*(u1[j]<0.4)+rnorm(1)
        
      }else if(i <=200){
        X2[i,j] = 3.5+0.5*(u2[j]<0.7)+rnorm(1)
        
      }else if(i <=300){
        X2[i,j] = 3.5-1.5*(u3[j]<0.3)+rnorm(1)
        
      }else{
        X2[i,j] = 3.5 + rnorm(1)
      }
    }
  }
  y = apply(X2[1:50,],2,sum)
  y = y/25+rnorm(100,sd=1.5)
  X2 = t(X2)
  #use {y1,X2} to predict model and gain the prediction error
  
  pls.pred = predict( pls.model2 , X2, ncomp = 8)
  test.error[times] = sum((pls.pred - y1)^2)
  
  #assign error
}

mean(test.error)

############################# LASSO ###################################
#lasso
library(glmnet)

######################### First Dataset ###############################
# test error
set.seed(1)
cv.error = c()
u=c()
for(times in 1:10){
  X1 = matrix(0,nrow = 5000,ncol = 100)
  for(i in 1:5000){
    for(j in 1:100){
      if(i<=50 & j <= 50){
        X1[i,j] = 3+rnorm(1)
      }else if(i <=50 & j >50){
        X1[i,j] = 4+rnorm(1)
      }else{
        X1[i,j] = 3.5 + rnorm(1)
      }
    }
  }
  
  y = apply(X1[1:50,],2,sum)
  y = y/25+rnorm(100,sd=1.5)
  
  X1 = t(X1)
  
  #use {y,X1} train model
  
  cv.model = cv.glmnet(X1, y, type.measure = "mse", nfolds = 10)
  model = glmnet(X1, y)
  cv.error[times] = 100 * cv.model$lambda.1se
  
  #new data generation
  X2 = matrix(0,nrow = 5000,ncol = 100)
  for(i in 1:5000){
    for(j in 1:100){
      if(i<=50 & j <= 50){
        X2[i,j] = 3+rnorm(1)
      }else if(i <=50 & j >50){
        X2[i,j] = 4+rnorm(1)
      }else{
        X2[i,j] = 3.5 + rnorm(1)
      }
    }
  }
  
  y1 = apply(X2[1:50,],2,sum)
  y1 = y1/25+rnorm(100, sd = 1.5)
  
  X2 = t(X2)
  #use {y1,X2} to predict model and gain the prediction error
  u[times] = sum((predict(model, newx = X2, s = cv.model$lambda.min) - y1)^2)
}

mean(u)
mean(cv.error)

######################### Second Dataset ###############################
set.seed(1)
cv.error = c()
u=c()
for(times in 1:10){
  X1 = matrix(0,nrow = 5000,ncol = 100)
  u1 = runif(100)
  u2 = runif(100)
  u3 = runif(100)
  for(i in 1:5000){
    for(j in 1:100){
      if(i<=50 & j <= 50){
        X1[i,j] = 3+rnorm(1)
      }else if(i <=50 & j >50){
        X1[i,j] = 4+rnorm(1)
      }else if(i <= 100){
        X1[i,j] = 3.5+1.5*(u1[j]<0.4)+rnorm(1)
      }else if(i <=200){
        X1[i,j] = 3.5+0.5*(u2[j]<0.7)+rnorm(1)
      }else if(i <=300){
        X1[i,j] = 3.5-1.5*(u3[j]<0.3)+rnorm(1)
      }else{
        X1[i,j] = 3.5 + rnorm(1)
      }
    }
  }
  y = apply(X1[1:50,],2,sum)
  y = y/25+rnorm(100,sd=1.5)
  X1 = t(X1)
  
  #use {y,X1} train model
  
  cv.model = cv.glmnet(X1, y, type.measure = "mse", nfolds = 10)
  model = glmnet(X1, y)
  cv.error[times] = 100 * cv.model$lambda.1se
  
  #new data generation
  X2 = matrix(0,nrow = 5000,ncol = 100)
  u1 = runif(100)
  u2 = runif(100)
  u3 = runif(100)
  for(i in 1:5000){
    for(j in 1:100){
      if(i<=50 & j <= 50){
        X2[i,j] = 3+rnorm(1)
      }else if(i <=50 & j >50){
        X2[i,j] = 4+rnorm(1)
      }else if(i <= 100){
        X2[i,j] = 3.5+1.5*(u1[j]<0.4)+rnorm(1)
      }else if(i <=200){
        X2[i,j] = 3.5+0.5*(u2[j]<0.7)+rnorm(1)
      }else if(i <=300){
        X2[i,j] = 3.5-1.5*(u3[j]<0.3)+rnorm(1)
      }else{
        X2[i,j] = 3.5 + rnorm(1)
      }
    }
  }
  y = apply(X2[1:50,],2,sum)
  y = y/25+rnorm(100,sd=1.5)
  X2 = t(X2)
  #use {y1,X2} to predict model and gain the prediction error
  u[times] = sum((predict(model, newx = X2, s = cv.model$lambda.min) - y1)^2)
}


mean(u)
mean(cv.error)

############################# SPCA ###################################
library(superpc)
featurenames <- paste("feature",as.character(1:100),sep="")

####################### SPCA: First Dataset ##########################

### SPCA part components number part

#one example test
#####data generation
set.seed(1)
X1 = matrix(0,nrow = 5000,ncol = 100)
for(i in 1:5000){
  for(j in 1:100){
    if(i<=50 & j <= 50){
      X1[i,j] = 3+rnorm(1)
    }else if(i <=50 & j >50){
      X1[i,j] = 4+rnorm(1)
    }else{
      X1[i,j] = 3.5 + rnorm(1)
    }
  }
}

y = apply(X1[1:50,],2,sum)
y = y/25+rnorm(100,sd=1.5)

train = sample(1:nrow(t(X1)),floor(nrow(t(X1))*0.7))
X1 = t(X1)


train = sample(1:nrow(t(X1)),floor(nrow(t(X1))*0.7))

traindata=list(x=t(X1[train,]),y=y[train], featurenames=featurenames)
testdata=list(x=t(X1[-train,]),y=y[-train], featurenames=featurenames)
superpc.train.2<-superpc.train(traindata,type = "regression")
superpc.cv.result<-superpc.cv(superpc.train.2,traindata)

# best shreshold is 12th 
#superpc.cv.result$thresholds[12]=1.348038


fit.cts<- superpc.predict(superpc.train.2, traindata,testdata,
                          threshold=1.348038, n.components=1,    prediction.type="continuous")

fit.model=superpc.fit.to.outcome(superpc.train.2,
                                 testdata,score = fit.cts$v.pred,print=F)


sum((testdata$y-fit.model$results$fitted.values)^2)
sum(fit.cts$which.features)

#48 parameters 

#IN FACT, the following plot is the same as the next two one.
spca.residuals=fit.model$results$fitted.values-testdata$y
pdf("residualsplot_1.pdf")
plot(fit.model$results$fitted.values,spca.residuals,main="ncomponent=1")
dev.off()

pdf("predictionplot_1.pdf")
superpc.predictionplot(superpc.train.2,traindata,testdata,1.348038)
dev.off()

fit.red<- superpc.predict.red(superpc.train.2,
                              traindata,testdata, threshold= 1.348038)
fit.redcv<-superpc.predict.red.cv( fit.red,superpc.cv.result, 
                                   traindata, threshold=1.348038)
superpc.plotred.lrtest(fit.redcv)

pdf("cvplot.pdf")
superpc.plotcv(superpc.cv.result,cv.type=c("full","preval"))
dev.off()



### SPCA test error

# test error
set.seed(1)
u=c()
for(times in 1:10){
  X1 = matrix(0,nrow = 5000,ncol = 100)
  for(i in 1:5000){
    for(j in 1:100){
      if(i<=50 & j <= 50){
        X1[i,j] = 3+rnorm(1)
      }else if(i <=50 & j >50){
        X1[i,j] = 4+rnorm(1)
      }else{
        X1[i,j] = 3.5 + rnorm(1)
      }
    }
  }
  
  y = apply(X1[1:50,],2,sum)
  y = y/25+rnorm(100,sd=1.5)
  
  X1 = t(X1)
  
  #use {y,X1} train model
  traindata=list(x=t(X1),y=y, featurenames=featurenames)
  superpc.train.2<-superpc.train(traindata,type = "regression")
  superpc.cv.result<-superpc.cv(superpc.train.2,traindata)
  
  
  thresholds=superpc.cv.result$thresholds[superpc.cv.result$scor[1,]==
                                            max(superpc.cv.result$scor[1,])]
  
  
  
  #new data generation
  X2 = matrix(0,nrow = 5000,ncol = 100)
  for(i in 1:5000){
    for(j in 1:100){
      if(i<=50 & j <= 50){
        X2[i,j] = 3+rnorm(1)
      }else if(i <=50 & j >50){
        X2[i,j] = 4+rnorm(1)
      }else{
        X2[i,j] = 3.5 + rnorm(1)
      }
    }
  }
  
  y1 = apply(X2[1:50,],2,sum)
  y1 = y1/25+rnorm(100,sd=1.5)
  
  X2 = t(X2)
  #use {y1,X2} to predict model and gain the prediction error
  testdata=list(x=t(X2),y=y1, featurenames=featurenames)
  fit.cts<- superpc.predict(superpc.train.2, traindata,testdata,
                            threshold=thresholds, n.components=1,    prediction.type="continuous")
  
  fit.model=superpc.fit.to.outcome(superpc.train.2,testdata,score =
                                     fit.cts$v.pred,print=F)
  
  
  u[times]=sum((y1-fit.model$results$fitted.values)^2)# write down the error part
}

### SPCA cross validation

#10-folds
#cross validation

#first we should split the total datas as 10 folds and then we can simulate

u=c()
set.seed(1)
for(counttime in 1:10){
  X1 = matrix(0,nrow = 5000,ncol = 100)
  for(i in 1:5000){
    for(j in 1:100){
      if(i<=50 & j <= 50){
        X1[i,j] = 3+rnorm(1)
      }else if(i <=50 & j >50){
        X1[i,j] = 4+rnorm(1)
      }else{
        X1[i,j] = 3.5 + rnorm(1)
      }
    }
  }
  
  y = apply(X1[1:50,],2,sum)
  y = y/25+rnorm(100,sd=1.5)
  
  train = sample(1:nrow(t(X1)),floor(nrow(t(X1))*0.7))
  X1 = t(X1)
  
  featurenames <- paste("feature",as.character(1:5000),sep="")
  n=length(y)
  
  #first we should split the total datas as 10 folds and then we can simulate
  require(caret)
  flds <- createFolds(1:100, k = 10, list = TRUE, 
                      returnTrain = FALSE)#we name it as split procedure
  s=c()
  for(i in 1:10){
    testdata=list(x=t(X1[flds[[i]],]),y=y[flds[[i]]], featurenames=featurenames)
    traindata<-list(x=t(X1[-flds[[i]],]),y=y[-flds[[i]]], featurenames=featurenames)
    superpc.train.1<-superpc.train(traindata,type = "regression")
    superpc.cv.result<-superpc.cv(superpc.train.1,traindata)
    
    fit.cts<- superpc.predict(superpc.train.1, traindata,testdata,
                              threshold=superpc.cv.result$thresholds[superpc.cv.result$scor[1,]==
                                                                       max(superpc.cv.result$scor[1,])], n.components=3,    prediction.type="continuous")
    #a<-superpc.fit.to.outcome(superpc.train.1, newdata1, 
    fit.cts$v.pred)
fit.model=superpc.fit.to.outcome(superpc.train.1,data.test = 
                                   testdata,score = fit.cts$v.pred,print=F)
s[i]=sum((testdata$y-fit.model$results$fitted.values)^2)
  }
  u[counttime]=sum(s)
  
  # we can simulate the 10-folds experiments multiple times by using loop
  # the code in each loop is cv for 100observations.
}
#144.2776

####################### SPCA: Second Dataset #########################

# test error

set.seed(1)
u=c()
for(times in 1:10){
  X1 = matrix(0,nrow = 5000,ncol = 100)
  X2 = matrix(0,nrow = 5000,ncol = 100)
  u1 = runif(100)
  u2 = runif(100)
  u3 = runif(100)
  for(i in 1:5000){
    for(j in 1:100){
      if(i<=50 & j <= 50){
        X2[i,j] = 3+rnorm(1)
      }else if(i <=50 & j >50){
        X2[i,j] = 4+rnorm(1)
      }else if(i <= 100){
        X2[i,j] = 3.5+1.5*(u1[j]<0.4)+rnorm(1)
        
      }else if(i <=200){
        X2[i,j] = 3.5+0.5*(u2[j]<0.7)+rnorm(1)
        
      }else if(i <=300){
        X2[i,j] = 3.5-1.5*(u3[j]<0.3)+rnorm(1)
        
      }else{
        X2[i,j] = 3.5 + rnorm(1)
      }
    }
  }
  y = apply(X2[1:50,],2,sum)
  y.train = y/25+rnorm(100,sd=1.5)
  X.train = t(X2)
  
  #use {y,X1} train model
  traindata=list(x=t(X.train),y=y.train, featurenames=featurenames)
  superpc.train.2<-superpc.train(traindata,type = "regression")
  superpc.cv.result<-superpc.cv(superpc.train.2,traindata)
  
  
  thresholds=superpc.cv.result$thresholds[superpc.cv.result$scor[1,]==
                                            max(superpc.cv.result$scor[1,])]
  
  
  
  #new data generation
  X2 = matrix(0,nrow = 5000,ncol = 100)
  u1 = runif(100)
  u2 = runif(100)
  u3 = runif(100)
  for(i in 1:5000){
    for(j in 1:100){
      if(i<=50 & j <= 50){
        X2[i,j] = 3+rnorm(1)
      }else if(i <=50 & j >50){
        X2[i,j] = 4+rnorm(1)
      }else if(i <= 100){
        X2[i,j] = 3.5+1.5*(u1[j]<0.4)+rnorm(1)
        
      }else if(i <=200){
        X2[i,j] = 3.5+0.5*(u2[j]<0.7)+rnorm(1)
        
      }else if(i <=300){
        X2[i,j] = 3.5-1.5*(u3[j]<0.3)+rnorm(1)
        
      }else{
        X2[i,j] = 3.5 + rnorm(1)
      }
    }
  }
  y = apply(X2[1:50,],2,sum)
  y.test = y/25+rnorm(100,sd=1.5)
  X.test = t(X2)
  
  #use new {y1,X2} to predict model and gain the prediction error
  testdata=list(x=t(X.test),y=y.test, featurenames=featurenames)
  fit.cts<- superpc.predict(superpc.train.2, traindata,testdata,
                            threshold=thresholds, n.components=1,    prediction.type="continuous")
  
  fit.model=superpc.fit.to.outcome(superpc.train.2,testdata,score = fit.cts$v.pred,print=F)
  
  
  u[times]=sum((y.test-fit.model$results$fitted.values)^2)# write down the error part
}


#test error 223.5655

#cv 10folds
#10-folds
#cross validation

#first we should split the total datas as 10 folds and then we can simulate
set.seed(1)
u=c()
for(counttime in 1:10){
  X1 = matrix(0,nrow = 5000,ncol = 100)
  X2 = matrix(0,nrow = 5000,ncol = 100)
  u1 = runif(100)
  u2 = runif(100)
  u3 = runif(100)
  for(i in 1:5000){
    for(j in 1:100){
      if(i<=50 & j <= 50){
        X2[i,j] = 3+rnorm(1)
      }else if(i <=50 & j >50){
        X2[i,j] = 4+rnorm(1)
      }else if(i <= 100){
        X2[i,j] = 3.5+1.5*(u1[j]<0.4)+rnorm(1)
        
      }else if(i <=200){
        X2[i,j] = 3.5+0.5*(u2[j]<0.7)+rnorm(1)
        
      }else if(i <=300){
        X2[i,j] = 3.5-1.5*(u3[j]<0.3)+rnorm(1)
        
      }else{
        X2[i,j] = 3.5 + rnorm(1)
      }
    }
  }
  y = apply(X2[1:50,],2,sum)
  y = y/25+rnorm(100,sd=1.5)
  X2  = t(X2)
  
  featurenames <- paste("feature",as.character(1:5000),sep="")
  n=length(y)
  
  #first we should split the total datas as 10 folds and then we can simulate
  require(caret)
  flds <- createFolds(1:100, k = 10, list = TRUE,
                      returnTrain = FALSE)#we name it as split procedure
  s=c()
  for(i in 1:10){
    testdata=list(x=t(X2[flds[[i]],]),y=y[flds[[i]]], featurenames=featurenames)
    traindata<-list(x=t(X2[-flds[[i]],]),y=y[-flds[[i]]], featurenames=featurenames)
    superpc.train.1<-superpc.train(traindata,type = "regression")
    superpc.cv.result<-superpc.cv(superpc.train.1,traindata)
    
    fit.cts<- superpc.predict(superpc.train.1, traindata,testdata, threshold=
                                superpc.cv.result$thresholds[superpc.cv.result$scor[1,]==
                                                               max(superpc.cv.result$scor[1,])], n.components=3,    prediction.type="continuous")
    #a<-superpc.fit.to.outcome(superpc.train.1, newdata1, fit.cts$v.pred)
    fit.model=superpc.fit.to.outcome(superpc.train.1,data.test =
                                       testdata,score = fit.cts$v.pred,print=F)
    s[i]=sum((testdata$y-fit.model$results$fitted.values)^2)
  }
  u[counttime]=sum(s)
  # we can simulate the 10-folds experiments multiple times by using loop
  #164.8654 for one time cross validation
}
#160.5622 for loop