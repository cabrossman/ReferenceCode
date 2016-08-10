# https://cran.r-project.org/web/packages/vars/vignettes/vars.pdf

library("vars")
data("Canada")
summary(Canada)

# The authors investigated the Canadian labor market. They utilized the following series:
#   labor productivity defined as the log difference between GDP and employment, the log of
# employment, the unemployment rate and real wages, defined as the log of the real wage
# index. These series are signified by ???prod???, ???e???, ???U??? and ???rw???, respectively. The data is taken
# from the OECD data base and spans from the first quarter 1980 until the fourth quarter 2004.



plot(Canada, nc = 2, xlab = "")


# A preliminary data analysis is conducted by displaying the summary statistics of the series
# involved as well as the corresponding time series plots (see Figure 1). In a next step, the
# authors conducted unit root tests by applying the Augmented Dickey-Fuller test regressions
# to the series (henceforth: ADF test). The ADF test has been implemented in the package urca
# as function ur.df(), for instance. The result of the ADF tests are summarized in Table 2.

prod <- summary(ur.df(Canada[, "prod"], type = "trend", lags = 2)); prod
chgProd <- summary(ur.df(diff(Canada[, "prod"]), type = "drift",lags = 1)); chgProd
e <- summary(ur.df(Canada[, "e"], type = "trend", lags = 2)); e
chgE <- summary(ur.df(diff(Canada[, "e"]), type = "drift",lags = 1)); chgE
u <- summary(ur.df(Canada[, "U"], type = "trend", lags = 1)); u
chgU <- summary(ur.df(diff(Canada[, "U"]), type = "drift",lags = 0)); chgU
rw <- summary(ur.df(Canada[, "rw"], type = "trend", lags = 4)); rw
chgRw <- summary(ur.df(diff(Canada[, "rw"]), type = "drift",lags = 3)); chgRw
chgRw2 <- summary(ur.df(diff(Canada[, "rw"]), type = "drift",lags = 0)); chgRw2

# It can be concluded that all time series are integrated of order one.
# is z.lag.1 sig?

# In an ensuing step, the authors determined an optimal lag length for an unrestricted VAR for
# a maximal lag length of eight.
VARselect(Canada, lag.max = 8, type = "both")
# According to the AIC and FPE the optimal lag number is p = 3, whereas the HQ criterion
# indicates p = 2 and the SC criterion indicates an optimal lag length of p = 1.

# They estimated for all three lag orders a VAR including a constant and a trend as deterministic regressors
# and conducted diagnostic tests with respect to the residuals.

#########################
# In the R code example below,
# the relevant commands are exhibited for the VAR(1) model. First, the variables have to be
# reordered in the same sequence as in Breitung et al. (2004). This step is necessary, because
# otherwise the results of the multivariate Jarque-Bera test, in which a Choleski decomposition
# is employed, would differ slightly from the reported ones in Breitung et al. (2004). In the
# R code lines below the estimation of the VAR(1) as well as the summary output and the
# diagram of fit for equation ???e??? is shown.

#re order
Canada <- Canada[, c("prod", "e", "U", "rw")]
p1ct <- VAR(Canada, p = 1, type = "both"); p1ct
summary(p1ct, equation = 'e')
plot(p1ct, names = "e")

#####################
####################
##diagnostic test##
###################
###################
ser11 <- serial.test(p1ct, lags.pt = 16, type = "PT.asymptotic"); ser11
norm1 <- normality.test(p1ct); norm1


############
###should this not be a VAR
###Maybe 
###########
arch1 <- arch.test(p1ct, lags.multi = 5)
plot(arch1, names = "e")
plot(stability(p1ct), nc = 2)


# Given the diagnostic test results the authors concluded that a VAR(1)-specification might be
# too restrictive. They argued further, that although some of the stability tests do indicate
# deviations from parameter constancy, the time-invariant specification of the VAR(2) and
# VAR(3) model will be maintained as tentative candidates for the following cointegration
# analysis he estimation of these models as well as the statistical inference with
# respect to the cointegration rank can be swiftly accomplished with the function ca.jo().
# Although the following R code examples are using functions contained in the package urca,
# it is however beneficial to reproduce these results for two reasons: the interplay between the
# functions contained in package urca and vars is exhibited and it provides an understanding
# of the then following SVEC specification.
summary(ca.jo(Canada, type = "trace", ecdet = "trend", K = 3, spec = "transitory"))
summary(ca.jo(Canada, type = "trace", ecdet = "trend", K = 2, spec = "transitory"))

# These results do indicate one cointegration relationship



# the R code snippet below the VECM is re-estimated with this restriction and a
# normalization of the long-run relationship with respect to real wages. The results are shown
# in Table 5.
vecm <- ca.jo(Canada[, c("rw", "prod", "e", "U")], type = "trace", ecdet = "trend", K = 3, spec = "transitory")
vecm.r1 <- cajorls(vecm, r = 1)



# Because B,1,4 has already been set to zero, only two additional restrictions have
# been added. The last restriction is imposed on the element B,4,2. Here, it is assumed that
# labor demand shocks do not exert an immediate effect on real wages.
# 
# In the R code example below the matrix objects LR and SR are set up accordingly and the
# just-identified SVEC is estimated with function SVEC(). In the call to the function SVEC() the
# argument boot = TRUE has been employed such that bootstrapped standard errors and hence
# t statistics can be computed for the structural long-run and contemporaneous coefficients.
vecm <- ca.jo(Canada[, c("prod", "e", "U", "rw")], type = "trace",ecdet = "trend", K = 3, spec = "transitory")
SR <- matrix(NA, nrow = 4, ncol = 4)
SR[4, 2] <- 0
LR <- matrix(NA, nrow = 4, ncol = 4)
LR[1, 2:4] <- 0
LR[2:4, 4] <- 0
svec <- SVEC(vecm, LR = LR, SR = SR, r = 1, lrtest = FALSE, boot = TRUE,runs = 100)
summary(svec)

# The authors investigated further if labor supply shocks do have no long-run impact on unemployment.
# This hypothesis is mirrored by setting ??B3,3 = 0. Because one more zero restriction
# has been added to the long-run impact matrix, the SVEC model is now over-identified. The
# validity of this over-identification restriction can be tested with a LR test. In the R code
# example below first the additional restriction has been set and then the SVEC is re-estimated
# by using the upate method. The result of the LR test is contained in the returned list as
# named element LRover.
svec.oi <- update(svec, LR = LR, lrtest = TRUE, boot = FALSE)
# The value of the test statistic is 6.07 and the p value of this ??
# 2
# (1)-distributed variable is 0.014.
# Therefore, the null hypothesis that shocks to the labor supply do not exert a long-run effect
# on unemployment has to be rejected for a significance level of 5%.



##############
# Impulse Response
###############
svec.irf <- irf(svec, response = "U", n.ahead = 48, boot = TRUE)
plot(svec.irf)


# In a final step, a forecast error variance decomposition is conducted with respect to unemployment.
# This is achieved by applying the fevd method to the object with class attribute
# svecest.
fevd.U <- fevd(svec, n.ahead = 48)$U