# https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
#elastic net uses alpha which bridges the gap betwen lasso (alpha = 1, its default) and ridge(alpha = 0)

# It is known that the ridge penalty shrinks the coefficients of correlated predictors towards each other 
# while the lasso tends to pick one of them and discard the others. The elastic-net penalty mixes these two; 
# if predictors are correlated in groups, an alpha=0.5 tends to select the groups in or out together. 
# This is a higher level parameter, and users might pick a value upfront, else experiment with a few different values. 
# One use of alpha is for numerical stability

library(glmnet)
library(AppliedPredictiveModeling)

##Quick Start
    
    data(FuelEconomy)
    y <- matrix(cars2010$FE)
    x <- model.matrix(FE ~ Transmission + EngDispl + AirAspirationMethod + NumCyl, data = cars2010)
    x <- x[,2:NCOL(x)]
    
    #fit the model. need to have X in design matrix, y is also matrix. standardize defaults to true. alpha set to 1
    fit = glmnet(x, y)
    #view plot of coefficients for different lambda
    plot(fit, label = TRUE)
    print(fit)
    #coefficients at specific lambda
    coef(fit,s=0.1)
    
    #predict at specific lamda value. these values dont mean anything
    nx = matrix(rnorm(1107*19),1107,19)
    predict <- predict(fit,newx=nx,s=c(0.1,0.05))
    predictDF <- cbind.data.frame(y, predict)
    
    #use cross validation
    cvfit = cv.glmnet(x, y)
    plot(cvfit)
    #We can view the selected ????'s and the corresponding coefficients. For example,
    cvfit$lambda.min
    
    # lambda.min is the value of lambda that gives minimum mean cross-validated error. 
    # The other lambda saved is lambda.1se, which gives the most regularized model such that 
    # error is within one standard error of the minimum. To use that, we only need to replace lambda.min with lambda.1se above.
    
    #get coefficients with 
    coef(cvfit, s = "lambda.min")
    #for non sparse output
    as.matrix(coef(cvfit, s = "lambda.min"))
    # Predictions can be made based on the fitted cv.glmnet object. Let's see a toy example.
    predict(cvfit, newx = x[1:5,], s = "lambda.min")
    
    
##Linear Regression
    rm(list = ls())
    data(FuelEconomy)
    y <- matrix(cars2010$FE)
    x <- model.matrix(FE ~ Transmission + EngDispl + AirAspirationMethod + NumCyl, data = cars2010)
    x <- x[,2:NCOL(x)]
    
    #fitting by setting additional params
    fit = glmnet(x, y, alpha = 0.2, weights = c(rep(1,500),rep(2,607)), nlambda = 120)
    #nlambda controls the number of computations.
    #df is number of non zero coefficients, %dev pct deviance explained, lambda value
    print(fit)
    
    # Users can decide what is on the X-axis. xvar allows three measures: 
    #   "norm" for the l1-norm of the coefficients (default), "lambda" for the log-lambda value and "dev" for %deviance explained.
    plot(fit, xvar = "norm", label = TRUE); plot(fit, xvar = "lambda", label = TRUE); plot(fit, xvar = "dev", label = TRUE)
    
    
    # We can extract the coefficients and make predictions at certain values of lambda Two commonly used options are: 
    # 1. "s" specifies the value(s) of lambda at which extraction is made.
    # 2. "exact" indicates whether the exact values of coefficients are desired or not. That is, 
    # if exact = TRUE, and predictions are to be made at values of s not included in the original fit, 
    # these values of s are merged with object$lambda, and the model is refit before predictions are made. 
    # If exact=FALSE (default), then the predict function uses linear interpolation to make predictions for values of 
    # s that do not coincide with lambdas used in the fitting algorithm.
    any(fit$lambda == 0.5)
    coef.exact = coef(fit, s = 0.5, exact = TRUE)
    coef.apprx = coef(fit, s = 0.5, exact = FALSE)
    cbind2(coef.exact, coef.apprx)
    
    # Users can make predictions from the fitted object. In addition to the options in coef, the primary argument is newx, a matrix of new values for x. 
    # The type option allows users to choose the type of prediction: * "link" gives the fitted values
    # 1. "response" the sames as "link" for gaussian faimly
    # 2. "coefficients" computes the coefficients at values of s
    # 3. "nonzero" returns a list of the indicies of the nonzero coefficients for each value of 's'
    
    predict(fit, newx = x[1:5,], type = "response", s = 0.05)
    
    
    # Users can customize K-fold cross-validation. In addition to all the glmnet parameters, cv.glmnet has its special parameters 
    # including nfolds (the number of folds), foldid (user-supplied folds), 
    # type.measure(the loss used for cross-validation): * "deviance" or "mse" uses squared loss
    
    cvfit = cv.glmnet(x, y, type.measure = "mse", nfolds = 20)
    plot(cvfit)
    cvfit$lambda.min
    coef(cvfit, s = "lambda.min")
    predict(cvfit, newx = x[1:5,], s = "lambda.min")
    
    #users can control folds
    foldid=sample(1:10,size=length(y),replace=TRUE)
    cv1=cv.glmnet(x,y,foldid=foldid,alpha=1)
    cv.5=cv.glmnet(x,y,foldid=foldid,alpha=.5)
    cv0=cv.glmnet(x,y,foldid=foldid,alpha=0)
    
    #make the plots in base R
    par(mfrow=c(2,2))
    plot(cv1);plot(cv.5);plot(cv0)
    plot(log(cv1$lambda),cv1$cvm,pch=19,col="red",xlab="log(Lambda)",ylab=cv1$name)
    points(log(cv.5$lambda),cv.5$cvm,pch=19,col="grey")
    points(log(cv0$lambda),cv0$cvm,pch=19,col="blue")
    legend("topleft",legend=c("alpha= 1","alpha= .5","alpha 0"),pch=19,col=c("red","grey","blue"))
    par(mfrow=c(1,1))
    
    #coefficient upper and lower bounds
    # hese are recently added features that enhance the scope of the models. Suppose we want to fit our model, 
    # but limit the coefficients to be bigger than -0.7 and less than 0.5. This is easily achieved via the upper.limits and lower.limits arguments:
    tfit=glmnet(x,y,lower=0,upper=3)
    plot(tfit)
    
    # These are rather arbitrary limits; often we want the coefficients to be positive, so we can set only lower.limit to be 0. 
    # (Note, the lower limit must be no bigger than zero, and the upper limit no smaller than zero.) These bounds can be a vector, 
    # with different values for each coefficient. If given as a scalar, the same number gets recycled for all.
    
    
    #penalty factors
    # This argument allows users to apply separate penalty factors to each coefficient. 
    # Its default is 1 for each parameter, but other values can be specified. In particular, 
    # any variable with penalty.factor equal to zero is not penalized at all! Let vjvj denote the penalty factor for jj th variable.
    # Note the penalty factors are internally rescaled to sum to nvars.
    # This is very useful when people have prior knowledge or preference over the variables. 
    # In many cases, some variables may be so important that one wants to keep them all the time, 
    # which can be achieved by setting corresponding penalty factors to 0:
    p.fac = rep(1, 19)
    p.fac[c(5, 10, 15)] = 0
    pfit = glmnet(x, y, penalty.factor = p.fac)
    plot(pfit, label = TRUE)
    
    #parallel can do parrelel, need to research how to do parallel
    require('doParallel')
    X = matrix(rnorm(1e4 * 200), 1e4, 200)
    Y = rnorm(1e4)
    cl <- makeCluster(2)
    registerDoParallel(cl)
    system.time(cv.glmnet(X, Y))
    system.time(cv.glmnet(X, Y, parallel = TRUE))
    
    
#Multi reponse Y
    rm(list = ls())
    load(url("https://github.com/cran/glmnet/blob/master/data/MultiGaussianExample.RData?raw=true"))
    
    # We fit the data, with an object "mfit" returned.
    mfit = glmnet(x, y, family = "mgaussian")
    
    # For multiresponse Gaussian, the options in glmnet are almost the same as the single-response case, 
    # such as alpha, weights, nlambda, standardize. A exception to be noticed is that standardize.response is only for mgaussian family.
    # The default value is FALSE. If standardize.response = TRUE, it standardizes the response variables.
    
    
    plot(mfit, xvar = "lambda", label = TRUE, type.coef = "2norm")
    #creates one for each coefficient figure
    plot(mfit, xvar = "lambda", label = TRUE, type.coef = "coef")
    #creates 3 dim array
    predict(mfit, newx = x[1:5,], s = c(0.1, 0.01))
    
    cvmfit = cv.glmnet(x, y, family = "mgaussian")
    plot(cvmfit)
    cvmfit$lambda.min; cvmfit$lambda.1se
    
    
#logistic regression
    rm(list = ls())
    a <- 'https://github.com/cran/glmnet/blob/master/data/BinomialExample.RData?raw=true'
    load(url(a))
    
    # The input matrix x is the same as other families. For binomial logistic regression, 
    # the response variable y should be either a factor with two levels, or a two-column matrix of counts or proportions.
    fit = glmnet(x, y, family = "binomial")
    plot(fit, xvar = "dev", label = TRUE)
    
    #Prediction is a little different for logistic from Gaussian, mainly in the option type. 
    # "link" and "response" are never equivalent and "class" is only available for logistic regression. 
    # In summary, * "link" gives the linear predictors
    #reponse gives the fitted probabilities
    #class produces the class label with max probability
    #coefficients computers coefficients at values of s
    #non zero returns a list of the indices of the nonzero coefficients for each value of s
    predict(fit, newx = x[1:5,], type = "class", s = c(0.05, 0.01))
    predict(fit, newx = x[1:5,], type = "response", s = c(0.05, 0.01))
    
    #cv measure
    cvfit = cv.glmnet(x, y, family = "binomial", type.measure = "class"); plot(cvfit)
    #type.measure can be deviance, mae, class, or auc for ROC
    cvfit$lambda.min; cvfit$lambda.1se
    coef(cvfit, s = "lambda.min")
    predict(cvfit, newx = x[1:10,], s = "lambda.min", type = "class")
    
#multinomial models
    rm(list = ls())
    a <- 'https://github.com/cran/glmnet/blob/master/data/MultinomialExample.RData?raw=true'
    load(url(a))
    # The response variable can be a nc >= 2 level factor, or a nc-column matrix of counts or proportions. 
    # Internally glmnet will make the rows of this matrix sum to 1, and absorb the total mass into the weight for that observation.
    # A special option for multinomial regression is type.multinomial, which allows the usage of a grouped lasso penalty 
    # if type.multinomial = "grouped". This will ensure that the multinomial coefficients for a variable are all in or out together, 
    # just like for the multi-response Gaussian.
    fit = glmnet(x, y, family = "multinomial", type.multinomial = "grouped")
    plot(fit, xvar = "lambda", label = TRUE, type.coef = "2norm")
    cvfit=cv.glmnet(x, y, family="multinomial", type.multinomial = "grouped", parallel = TRUE)
    plot(cvfit)
    predict(cvfit, newx = x[1:10,], s = "lambda.min", type = "class")
    
#poisson models
    rm(list = ls())
    a <- 'https://github.com/cran/glmnet/blob/master/data/PoissonExample.RData?raw=true'
    load(url(a))
    fit = glmnet(x, y, family = "poisson")
    #offset is a useful argument particularly in Poisson models.
    
    # 
    # When dealing with rate data in Poisson models, the counts collected are often based on different exposures, 
    # such as length of time observed, area and years. A poisson rate mu(x) is relative to a unit exposure time, 
    # so if an observation y(i) was exposed for E(i) units of time, then the expected count would be E(i)mu(x), and the log mean would be 
    # log(E(i))+log(mu(x)). In a case like this, we would supply an offset log(E(i)) for each observation. 
    # Hence offset is a vector of length nobs that is included in the linear predictor. 
    # Other families can also use options, typically for different reasons.
    # 
    # (Warning: if offset is supplied in glmnet, offsets must also also be supplied to predict to make reasonable predictions.)
    plot(fit)
    coef(fit, s = 1)
    predict(fit, newx = x[1:5,], type = "response", s = c(0.1,1))
    cvfit = cv.glmnet(x, y, family = "poisson")
    plot(cvfit)
    opt.lam = c(cvfit$lambda.min, cvfit$lambda.1se)
    coef(cvfit, s = opt.lam)
    
#COX models
    rm(list = ls())
    a <- 'https://github.com/cran/glmnet/blob/master/data/CoxExample.RData?raw=true'
    load(url(a))
    
    # The Surv function in the package survival can create such a matrix. Note, however, that the coxph and related linear models can handle interval 
    # and other fors of censoring, while glmnet can only handle right censoring in its present form.
    
    fit = glmnet(x, y, family = "cox")
    plot(fit)
    as.matrix(coef(fit, s = 0.05))
    cvfit = cv.glmnet(x, y, family = "cox")
    plot(cvfit)
    
    coef.min = coef(cvfit, s = "lambda.min")
    active.min = which(coef.min != 0)
    index.min = coef.min[active.min]
    
    # Our package supports sparse input matrices, which allow efficient storage and operations of large matrices but with only a few nonzero entries. 
    # It is available for all families except for the cox family. The usage of sparse matrices (inherit from class "sparseMatrix" as in package Matrix) 
    # in glmnet is the same as if a regular matrix is provided.
