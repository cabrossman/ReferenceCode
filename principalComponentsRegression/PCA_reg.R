# 
# http://stats.stackexchange.com/questions/72839/how-to-use-r-prcomp-results-for-prediction

# While I'm unsure as to the nature of your problem, I can tell you that I have used PCA as a means of extracting dominant patterns 
# in a group of predictor variables in the later building of a model. In your example, these would be found in the principle components (PCs), 
# PCAAnalysis$x, and they would be based on the weighting of variables found in PCAAnalysis$rotation. One advantage of this process is 
# that PCs are orthogonal, and so you remove issues of multicollinearity between the model predictors. The second, is that you might be able to 
# identify a smaller subset of PCs that capture the majority of variance in your predictors. This information can be found in summary(PCAAnalysis) or 
# in PCAAnalysis$sdev. Finally, if you are interested in using a subset of the PCs for prediction, then you can set the tol parameter in prcomp to a 
# higher level to remove trailing PCs.
# 
# Now, you can "project" new data onto the PCA coordinate basis using the predict.prcomp() function. Since you are calling your data set a "training" data set, 
# this might make sense to then project a validation data set onto your PCA basis for the calculation of their respective PC coordinates. 
# Below is an example of fitting a PCA to 4 biometric measurements of different iris species (which are correlated to some degree). 
# Following this, I project biometric values of a new data set of flowers that have similar combinations of these measurements for 
# each of the three species of iris. You will see from the final graph that their projected PCs lie in a similar area of the plot 
# as the original data set.





###pca - calculated for the first 4 columns of the data set that correspond to biometric measurements 
#("Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width")
data(iris)
dat <- as.matrix(iris[,-5])
pca <- prcomp(dat, retx=TRUE, center=TRUE, scale=TRUE)

###Create new data sets for each of the three species. 
#Biometric values are based on the distributions of the original data means
#and the covariances between these parameters. 
setosa.mean <- apply(iris[iris$Species=="setosa",-5], 2, mean)
setosa.cov <- cov(iris[iris$Species=="setosa",-5])

versicolor.mean <- apply(iris[iris$Species=="versicolor",-5], 2, mean)
versicolor.cov <- cov(iris[iris$Species=="versicolor",-5])

virginica.mean <- apply(iris[iris$Species=="virginica",-5], 2, mean)
virginica.cov <- cov(iris[iris$Species=="virginica",-5])

#Make new random data based on the calculated biometry info. each species
#The MASS package allows for the calculation of correlated/covarying random 
#numbers using this information.
require(MASS)
set.seed(1)
n <- 30
new.setosa <- mvrnorm(n, setosa.mean, setosa.cov)
new.versicolor <- mvrnorm(n, versicolor.mean, versicolor.cov)
new.virginica <- mvrnorm(n, virginica.mean, virginica.cov)

###Predict PCs by projecting the new data using the predict.prcomp function
pred.setosa <- predict(pca, new.setosa)
pred.versicolor <- predict(pca, new.versicolor)
pred.virginica <- predict(pca, new.virginica)

###Plot result
SPP <- iris$Species
COLOR <- c(2:4)
PCH <- c(1,16)

pc <- c(1,2)
plot(pca$x[,pc[1]], pca$x[,pc[2]], col=COLOR[SPP], cex=PCH[1], xlab=paste0("PC ", pc[1], " (", round(pca$sdev[pc[1]]/sum(pca$sdev)*100,0), "%)"), ylab=paste0("PC ", pc[2], " (", round(pca$sdev[pc[2]]/sum(pca$sdev)*100,0), "%)"))
points(pred.setosa[,pc[1]], pred.setosa[,pc[2]], col=COLOR[levels(SPP)=="setosa"], pch=PCH[2])
points(pred.versicolor[,pc[1]], pred.versicolor[,pc[2]], col=COLOR[levels(SPP)=="versicolor"], pch=PCH[2])
points(pred.virginica[,pc[1]], pred.virginica[,pc[2]], col=COLOR[levels(SPP)=="virginica"], pch=PCH[2])
legend("topright", legend=levels(iris$Species), col=COLOR, pch=17)
legend("topleft", legend=c("Original data", "New data"), col=1, pch=PCH)