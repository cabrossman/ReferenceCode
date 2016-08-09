# http://strata.uga.edu/software/pdf/pcaTutorial.pdf

data("USArrests")
mydata <- USArrests

#pca
  mydata.pca <- prcomp(mydata, retx=TRUE, center=TRUE,
                         scale.=TRUE)
  # variable means set to zero, and variances set to one
  # sample scores stored in mydata.pca$x
  # loadings stored in mydata.pca$rotation
  # singular values (square roots of eigenvalues) stored
  # in mydata.pca$sdev
  # variable means stored in mydata.pca$center
  # variable standard deviations stored in mydata.pca$scale
  sd <- mydata.pca$sdev
  loadings <- mydata.pca$rotation
  rownames(loadings) <- colnames(mydata)
  scores <- mydata.pca$x


#DO pca by hand
  R <- cor(mydata)
  # calculate a correlation matrix
  myEig <- eigen(R)
  # find the eigenvalues and eigenvectors of correlation matrix
  # eigenvalues stored in myEig$values
  # eigenvectors (loadings) stored in myEig$vectors
  sdLONG <- sqrt(myEig$values)
  # calculating singular values from eigenvalues
  loadingsLONG <- myEig$vectors
  
  rownames(loadingsLONG) <- colnames(mydata)
  # saving as loadings, and setting rownames
  standardize <- function(x) {(x - mean(x))/sd(x)}
  X <- apply(mydata, MARGIN=2, FUN=standardize)
  # transforming data to zero mean and unit variance
  scoresLONG <- X %*% loadingsLONG
  # calculating scores from eigenanalysis results


#compare results by hand
  range(sd - sdLONG)
  range(loadings - loadingsLONG)
  range(scores - scoresLONG)
  
#distance biplot
  quartz(height=7, width=7)
  plot(scores[,1], scores[,2], xlab="PCA 1", ylab="PCA 2",
         type="n", xlim=c(min(scores[,1:2]), max(scores[,1:2])),
         ylim=c(min(scores[,1:2]), max(scores[,1:2])))
  arrows(0,0,loadings[,1]*10,loadings[,2]*10, length=0.1,
           angle=20, col="red")
  # note that this scaling factor of 10 may need to be changed,
  # depending on the data set
  text(loadings[,1]*10*1.2,loadings[,2]*10*1.2,
         rownames(loadings), col="red", cex=0.7)
  # 1.2 scaling insures that labels are plotted just beyond
  # the arrows
  text(scores[,1],scores[,2], rownames(scores), col="blue",
         cex=0.7)
  
  quartz(height=7, width=7)
  biplot(scores[,1:2], loadings[,1:2], xlab=rownames(scores),
           ylab=rownames(loadings), cex=0.7)
  # using built-in biplot function
  
# Do a correlation biplot (see Legendre & Legendre, 1998, p. 404)
  quartz(height=7, width=7)
  plot(scores[,1]/sd[1], scores[,2]/sd[2], xlab="PCA 1",
         ylab="PCA 2", type="n")
  arrows(0,0,loadings[,1]*sd[1],loadings[,2]*sd[2], length=0.1, angle=20, col="red")
  text(loadings[,1]*sd[1]*1.2,loadings[,2]*sd[2]*1.2,
       rownames(loadings), col="red", cex=0.7)
  text(scores[,1]/sd[1],scores[,2]/sd[2], rownames(scores),
         col="blue", cex=0.7)
  # 1.2 scaling insures that labels are plotted just beyond
  # the arrows
  quartz(height=7, width=7)
  biplot(mydata.pca)
  # using built-in biplot function of prcomp(); note that only
  # the top and right axes match the coordinates of the points;
  # also note that still doesn't quite replicate the correlation
  # biplot. It???s unclear what this function really does.
  
# Calculate the correlation coefficients between variables and principal components
  correlations <- t(loadings)*sd
  # find the correlation of all the variables with all PC's
  correlations <- cor(scores,mydata)
  # another way to find these correlations
  
  quartz(height=7, width=7)
  plot(mydata.pca)
  # using built-in function for prcomp; may not show all PCs
  quartz(height=7, width=7)
  plot(log(sd^2), xlab="principle component",
       ylab="log(variance)", type="b", pch=16)
  # using a general plot, with variance on a log scale
  
# Find variance along each principal component and the eigenvalues
  newsd <- sd(scores)
  max (sd - newsd)
  # finds maximum difference between the standard deviation form 
  # prcomp and the standard deviation calculated longhand;
  # should be close to zero
  eigenvalues <- sd^2
  sum(eigenvalues)
  # should equal number of variables
  length(mydata)
  # number of variables