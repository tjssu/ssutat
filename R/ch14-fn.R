# [Ch-14 Functions] ----------------------------------------------------------------------------------
# [Ch-14 Function Manual] -----------------------------------------

#' @title Manual for Ch14. Functions
#' @description Ch14. Correlation and Regression Analysis
#' @param fn Function number, Default: 0
#' @return None.
#'
#' @examples
#' ch14.man()
#' ch14.man(1)
#' ch14.man(1:3)
#' ch14.man(c(3,6,9))
#' @rdname ch14.man
#' @export
ch14.man = function(fn=0) {
    if (0 %in% fn) {
	cat("[1] mcorr\t\tDraw Scatter Plots & Calculate Correlation Coefficients\n")
	cat("[2] mcorr.test\t\tTest for Correlations Using Lists of Bivariate Data\n")
	cat("[3] reg1.pre\t\tPreliminary Analysis for a Simple Linear Regression\n")
	cat("[4] reg1.cor\t\tTest for Correlation in a Simple Linear Regression\n")
	cat("[5] reg1.plot\t\tDraw a Scatter Plot with a Simple Linear Regression Line\n")
	cat("[6] reg1.lse\t\tCalculate the Least Square Estimates for a Simple Regression\n")
	cat("[7] reg1.aov\t\tTest for the Significance of a Simple Regression Model\n")
	cat("[8] reg1.test\t\tTest on the Simple Regression Coefficients\n")
	cat("[9] reg1.pred\t\tPrediction and Confidence Interval in a Simple Regression\n")
	cat("[10] reg1.diag\t\tDraw Diagnosis Plots for a Simple Regression Model\n")
	cat("[11] mreg.lse\t\tCalculate Least Square Estimates for a Multiple Regression\n")
	cat("[12] mreg.aov\t\tTest for the Significance of a Multiple Regression Model\n")
	cat("[13] mreg.test\t\tTest on the Multiple Regression Coefficients\n")
	cat("[14] mreg.pred\t\tPrediction and Confidence Interval in a Multiple Regression\n")
	cat("[15] mreg.two\t\tCompare Two Multiple Regression Models\n")
    }
    if (1 %in% fn) {
	cat("[1] Draw Multiple Scatter Plots & Calculate Correlation Coefficients\n")
	cat("mcorr(X, item, xl, yl, mt, plot=TRUE, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("X\t List vector of bivariate data with 2 columns\n")
	cat("[Optional Input]--------------------------\n")
	cat("item\t String vector of list names\n")
	cat("xl\t Label of x-axis (default=\"group1\")\n")
	cat("yl\t Label of y-axis (default=\"group2\")\n")
	cat("mt\t Title of the scatter plot\n")
	cat("plot\t Logical value for drawing scatter plots (default=TRUE)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (2 %in% fn) {
	cat("[2] Test for Correlations Using Lists of Bivariate Data\n")
	cat("mcorr.test(X, item, alp=0.05, plot=TRUE, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("X\t List vector of bivariate data with 2 columns\n")
	cat("[Optional Input]--------------------------\n")
	cat("item\t String vector of list names\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("plot\t Logical value for drawing scatter plots (default=TRUE)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (3 %in% fn) {
	cat("[3] Preliminary Analysis for a Simple Linear Regression\n")
	cat("reg1.pre(x, y, xl, yl, mt, plot=TRUE, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x\t Vector of independent variable (explanatory variable) data\n")
	cat("y\t Vector of dependent variable (response variable) data\n")
	cat("[Optional Input]--------------------------\n")
	cat("xl\t Label of x-axis (default=x variable name)\n")
	cat("yl\t Label of y-axis (default=y variable name)\n")
	cat("mt\t Title of the scatter plot\n")

	cat("plot\t Logical value for drawing scatter plots (default=TRUE)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (4 %in% fn) {
	cat("[4] Test for Correlation in a Simple Linear Regression\n")
	cat("reg1.cor(x, y, r0=0, alp=0.05, plot=TRUE, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x\t Vector of independent variable (explanatory variable) data\n")
	cat("y\t Vector of dependent variable (response variable) data\n")
	cat("[Optional Input]--------------------------\n")
	cat("r0\t Correlation coefficient value under the null hypothesis (default=0)\n")
	cat("\t (if r0=0, perform t-test, otherwise, perform z-test)\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("plot\t Logical value for drawing scatter plots (default=TRUE)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (5 %in% fn) {
	cat("[5] Draw a Scatter Plot with a Simple Linear Regression Line\n")
	cat("reg1.plot(x, y, xl, yl, mt, pres=TRUE, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x\t Vector of independent variable (explanatory variable) data\n")
	cat("y\t Vector of dependent variable (response variable) data\n")
	cat("[Optional Input]--------------------------\n")
	cat("xl\t Label of x-axis (default=x variable name)\n")
	cat("yl\t Label of y-axis (default=y variable name)\n")
	cat("mt\t Title of the scatter plot\n")
	cat("pres\t Logical value for plotting residual symbols (default=TRUE)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (6 %in% fn) {
	cat("[6] Calculate the Least Square Estimates for a Simple Regression\n")
	cat("reg1.lse(x, y, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x\t Vector of independent variable (explanatory variable) data\n")
	cat("y\t Vector of dependent variable (response variable) data\n")
	cat("[Optional Input]--------------------------\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (7 %in% fn) {
	cat("[7] Test for the Significance of a Simple Regression Model\n")
	cat("reg1.aov(x, y, alp=0.05, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x\t Vector of independent variable (explanatory variable) data\n")
	cat("y\t Vector of dependent variable (response variable) data\n")
	cat("[Optional Input]--------------------------\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (8 %in% fn) {
	cat("[8] Test for the Significance of Simple Regression Coefficients\n")
	cat("reg1.test(x, y, alp=0.05, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x\t Vector of independent variable (explanatory variable) data\n")
	cat("y\t Vector of dependent variable (response variable) data\n")
	cat("[Optional Input]--------------------------\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (9 %in% fn) {
	cat("[9] Prediction and Confidence Interval in a Simple Regression\n")
	cat("reg1.pred(x, y, xl, yl, mt, plot=FALSE, x0, xrng, by, alp=0.05, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x\t Vector of independent variable (explanatory variable) data\n")
	cat("y\t Vector of dependent variable (response variable) data\n")
	cat("[Optional Input]--------------------------\n")
	cat("xl\t Label of x-axis (default=x variable name)\n")
	cat("yl\t Label of y-axis (default=y variable name)\n")
	cat("mt\t Plot title\n")
	cat("plot\t Logical value for drawing confidence & predition bands (default=FALSE)\n")
	cat("x0\t Conditioning value of the independent variable\n")
	cat("xrng\t Plot range of the independent variable\n")
	cat("by\t Plot interval of the independent variable\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (10 %in% fn) {
	cat("[10] Draw Diagnosis Plots for a Simple Regression Model\n")
	cat("reg1.diag(x, y, wh=c(1:3,5))\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x\t Vector of independent variable (explanatory variable) data\n")
	cat("y\t Vector of dependent variable (response variable) data\n")
	cat("[Optional Input]--------------------------\n")
	cat("wh\t Index vector for the diagnosis plots(default=c(1:3,5))\n")
    }
    if (11 %in% fn) {
	cat("[11] Calculate Least Square Estimates for a Multiple Regression\n")
	cat("mreg.lse(form, plot=TRUE, detail=T, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("form\t Formula for the regression model (eg: y ~ x1 + x2)\n")
	cat("[Optional Input]--------------------------\n")
	cat("plot\t Logical value for drawing a scatter plot matrix (default=TRUE)\n")
	cat("detail\t Logical value for printing detailed calculations (default=TRUE)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (12 %in% fn) {
	cat("[12] Test for the Significance of a Multiple Regression Model\n")
	cat("mreg.aov(form, alp=0.05, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("form\t Formula for the regression model (eg: y ~ x1 + x2)\n")
	cat("[Optional Input]--------------------------\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (13 %in% fn) {
	cat("[13] Test for the Significance of Multiple Regression Coefficients\n")
	cat("mreg.test(form, alp=0.05, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("form\t Formula for the regression model (eg: y ~ x1 + x2)\n")
	cat("[Optional Input]--------------------------\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (14 %in% fn) {
	cat("[14] Prediction and Confidence Interval in a Multiple Regression\n")
	cat("mreg.pred(form, newd, pvx=1, xrng, nx=50, plot=F, alp=0.05, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("form\t Formula for the regression model (ex: y ~ x1 + x2)\n")
	cat("[Optional Input]--------------------------\n")
	cat("newd\t A new data frame of independent variables\n")
	cat("pvx\t Designated number of independent variables (default=1)\n")
	cat("xrng\t Plot range of the independent variable\n")
	cat("nx\t Number of plotting points (default=50)\n")
	cat("plot\t Logical value for drawing conf. & pred. bands (default=FALSE)\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (15 %in% fn) {
	cat("[15] Compare Two Multiple Regression Models\n")
	cat("mreg.two(form1, form2, plot=FALSE, detail=FALSE, wh=c(1:3,5), alp=0.05, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("form\t Formula for a regression model (eg: y ~ x1 + x2)\n")
	cat("form2\t Formula for another regression model (eg: y ~ x1 * x2)\n")
	cat("[Optional Input]--------------------------\n")
	cat("plot\t Logical value for drawing diagnosis plots (default=FALSE)\n")
	cat("detail\t Logical value for printing detailed calculations (default=FALSE)\n")
	cat("wh\t Index vector for the diagnosis plots(default=c(1:3,5))\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (21 %in% fn) {
	cat("[21] Multiple Scatter Plots from Lists of Bivariate Data\n")
	cat("corr.mplot(X, item, xl, yl, mt, step=1:4, alp=0.05, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("X\t List vector of bivariate data with 2 columns\n")
	cat("item\t String vector of list names\n")
	cat("[Optional Input]--------------------------\n")
	cat("xl\t Label of x-axis (default=\"group1\")\n")
	cat("yl\t Label of y-axis (default=\"group2\")\n")
	cat("step\t Steps for correlation analysis (default=1:4)\n")
	cat("\t 1\t Scatter plot for prior investigation of data\n")
	cat("\t 2\t Estimate correlation coefficients\n")
	cat("\t 3\t Correlation tests\n")
	cat("\t 4\t Confidence intervals for correlation coefficients\n")
	cat("\t 5\t T-test plot\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (22 %in% fn) {
	cat("[22] Correlation Analysis & Simple Linear Regression Analysis\n")
	cat("corr.reg1(x, y, r0=0, xl, yl, mt, step=1:9, x0, xrng, by, alp=0.05, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("x\t Vector of independent variable (explanatory variable) data\n")
	cat("y\t Vector of dependent variable (response variable) data\n")
	cat("[Optional Input]--------------------------\n")
	cat("r0\t Correlation coefficient value under the null hypothesis (default=0)\n")
	cat("\t (if r0=0, perform t-test, otherwise, perform z-test)\n")
	cat("xl\t Label of x-axis (default=x variable name)\n")
	cat("yl\t Label of y-axis (default=y variable name)\n")
	cat("mt\t Plot title\n")
	cat("step\t Steps of simple regression analysis (default=1:9)\n")
	cat("\t 1\t x-y scatter plot for prior investigation of data\n")
	cat("\t 2\t Estimate correlation coefficient\n")
	cat("\t 3\t Correlation tests\n")
	cat("\t 4\t Confidence intervals for correlation coefficients\n")
	cat("\t 5\t T-test(or z-test) plot\n")
	cat("\t 6\t Display the simple regression line\n")
	cat("\t 7\t Estimate simple regression coefficients\n")
	cat("\t 8\t ANOVA table by calculating sum of squares\n")
	cat("\t 9\t Confidence intervals & significance tests for regression coefficients\n")
	cat("\t 10\t  Confidence intervals and prediction intervals at x0\n")
	cat("\t 11\t Plot confidence bands and prediction bands\n")
	cat("\t 12\t Diagnosis of the regression model\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
    if (23 %in% fn) {
	cat("[23] Multiple Regression Analysis\n")
	cat("corr.mreg(xd, y, form, xd2, form2, step=0:7, newd, pvx, xrng, nx, alp=0.05, dig=4)\n")
	cat("[Mandatory Input]--------------------------\n")
	cat("xd\t Data frame of independent variables (explanatory variables)\n")
	cat("y\t Vector of dependent variable (response variable) data\n")
	cat("form\t Formula of regression model (ex: y ~ x1 + x2)\n")
	cat("[Optional Input]--------------------------\n")
	cat("xd2\t Data frame of  independent variables in model2 (step=4, 5)\n")
	cat("form2\t Formula of regression model2 (ex: y ~ x1 * x2) (step=4, 5)\n")
	cat("step\t Steps of multiple regression analysis (default=0:4)\n")
	cat("\t 0\t Scatter matrix plot for prior investigation of data\n")
	cat("\t 1\t Estimate multiple regression coefficients\n")
	cat("\t 2\t ANOVA table by calculating sum of squares\n")
	cat("\t 3\t Confidence intervals & significance tests for regression coefficients\n")
	cat("\t 4\t Analysis of model2 (redo step 0~3)\n")
	cat("\t 5\t Compare and diagnose regression models\n")
	cat("\t 6\t Confidence intervals and prediction intervals at x=newd\n")
	cat("\t 7\t Plot confidence bands and prediction bands\n")
	cat("newd\t Data frame of independent variables for step 6\n")
	cat("pvx\t Designated number of independent variables for step 6(step=7)\n")
	cat("xrng\t Range of independent variables for step 7\n")
	cat("nx\t Designated number of independent variables for step 7\n")
	cat("alp\t Level of significance (default=0.05)\n")
	cat("dig\t Number of digits below the decimal point (default=4)\n")
    }
}
# ---------------------------------------------------------------------------------------------------------
# [14-1] Draw Multiple Scatter Plots & Calculate Correlation Coefficients
#' @title Multiple Scatter Plots
#' @description Multiple Scatter Plots & Calculate Correlation Coefficients from Lists of Bivariate Data.
#' @param X List vector of bivariate data with 2 columns.
#' @param item String vector of list names.
#' @param xl Label of x-axis, Default: "group1".
#' @param yl Label of y-axis, Default: "group2".
#' @param mt Title of the scatter plot.
#' @param plot Logical value for drawing scatter plots, Default: TRUE.
#' @param dig Number of digits below the decimal point, Default: 4.
#' @return None.
#'
#' @examples
#' dd = list(cbind(iris[[1]], iris[[2]]), cbind(iris[[3]], iris[[4]]))
#' sp = c("Sepal", "Petal")
#' mcorr(X=dd, item=sp, xl="Length", yl="Width")
#'
#' data(exa14_1); dat=exa14_1
#' Subject = c("Logic", "English", "Math", "Society", "Science", "Art")
#' xd = list()
#' for (k in 1:6) xd[[k]] = cbind(dat[[2*k-1]], dat[[2*k]])
#' mcorr(X=xd, item=Subject, xl="1st Semester", yl="2nd Semester")
#' @rdname mcorr
#' @export

mcorr = function(X, item, xl, yl, mt, plot=TRUE, dig=4) {
    # Number of data pairs
	nv = length(X)

    # Multiple scatter plots -----------------------------------------
      if (plot) {
    	# Set the graphic window
	nc = ifelse(nv<=5, 2, 3)
	nr = ceiling(nv/nc)
	h = ifelse(nr>2, 9, ifelse(nr==1, 4, 6))
	w = ifelse(nc>2, 9, 7)

	cat("[P] Scatter plot for prior investigation of data -------------\n")
	if (missing(item)) item = paste0("Variable", 1:nv)
	if (missing(mt)) mt = paste0("Scatter Plot (", letters[1:nv] ,") ", item)
	if (missing(xl)) xl = "Group1"
	if (missing(yl)) yl = "Group2"
	win.graph(w, h)
	par(mfrow=c(nr, nc))
	for (k in 1:nv) {
  		plot(X[[k]][,1], X[[k]][,2], pch=19, main=mt[k], xlab=xl, ylab=yl)
  		abline(lm(X[[k]][,2]~X[[k]][,1]), lty=2, col=2)  }
      }
    # Estimate correlation coefficients by utilizing cor( ) function
	cat("[1] Estimate correlation coefficients -----------------\n")
	var1 = var2 = cov12 = cor12 = rep(NA, nv)
	for (k in 1:nv) {
		var1[k] = var(X[[k]][,1])
		var2[k] = var(X[[k]][,2])
		cov12[k] = cov(X[[k]][,1], X[[k]][,2])
		cor12[k] = cor(X[[k]][,1], X[[k]][,2])
		cat(paste0(item[k], "\t Corr(X1,X2) = ", round(cov12[k], dig), " / \U221A(",
		round(var1[k], dig), "\U00D7", round(var2[k], dig), ") = ", round(cor12[k], dig)), "\n")
	}
}

# mcorr.test(X=xd, item=spec, xl="Length", yl="Width")

# [14-2] Test for Correlations Using Lists of Bivariate Data
#' @title Test for Correlations
#' @description Test for Correlations Using Lists of Bivariate Data.
#' @param X List vector of bivariate data with 2 columns.
#' @param item String vector of list names.
#' @param alp Level of significance, Default: 0.05.
#' @param plot Logical value for drawing scatter plots, Default: TRUE.
#' @param dig Number of digits below the decimal point, Default: 4.
#' @return Table of statistics.
#'
#' @examples
#' dd = list(cbind(iris[[1]], iris[[2]]), cbind(iris[[3]], iris[[4]]))
#' sp = c("Sepal", "Petal")
#' mcorr.test(X=dd, item=sp)
#'
#' data(exa14_1); dat=exa14_1
#' Subject = c("Logic", "English", "Math", "Society", "Science", "Art")
#' xd = list()
#' for (k in 1:6) xd[[k]] = cbind(dat[[2*k-1]], dat[[2*k]])
#' mcorr.test(X=xd, item=Subject)
#' @rdname mcorr.test
#' @export

mcorr.test = function(X, item, alp=0.05, plot=TRUE, dig=4) {
    # Number of data pairs
	nv = length(X)
    # Set the graphic window
	nc = ifelse(nv<=5, 2, 3)
	nr = ceiling(nv/nc)
	h = ifelse(nr>2, 9, ifelse(nr==1, 4, 6))
	w = ifelse(nc>2, 9, 7)
	if (missing(item)) item = paste0("Var.", 1:nv)

    # Correlation tests by cor.test( ) function
	ct = list()
	for (k in 1:nv) ct[[k]] = cor.test(X[[k]][,1], X[[k]][,2], conf.level =1-alp)

	cat("[1]", paste0(100*(1-alp), "% CI"), "and Tests for Correlation -----------------\n")
	coef = lcl = ucl = stat = pval = NULL
	for (k in 1:nv) {
		coef = c(coef, ct[[k]]$est)
		lcl = c(lcl, ct[[k]]$conf[1])
		ucl = c(ucl, ct[[k]]$conf[2])
		stat = c(stat, ct[[k]]$stat)
		pval = c(pval, ct[[k]]$p.val)
	}
	tab = cbind(coef, lcl, ucl, stat, pval)
	colnames(tab) = c("Corr", "LCL", "UCL", "T0", "P-value")
	rownames(tab) = item

	dum = cbind(round(coef,dig), round(lcl,dig), round(ucl,dig),
		round(abs(stat),dig), format(pval,F,dig))
	colnames(dum) = c("Corr", "LCL", "UCL", "|T0|", "P-value")
	rownames(dum) = item
	print(as.data.frame(dum))

    # T-test plot
      if (plot) {
	cat("[P] T-test plots ------------------------\n")
	win.graph(w, h)
	par(mfrow=c(nr, nc))
	# Utilize ttest.plot( ) function in chapter 11
	for (k in 1:nv) {
  		mr = max(c(3, abs(ct[[k]]$stat*1.2)))
		# Chapter 11 --> ttest.plot()
  		ttest.plot(ct[[k]]$stat, ct[[k]]$para, prng=c(-mr, mr), dig=2, mt=item[k], pvout=F)
	}
      }
      invisible(tab)
}

# [14-3] Preliminary Analysis for a Simple Linear Regression
#' @title Preliminary Analysis for a Simple Linear Regression
#' @description Preliminary Analysis via a Scatter Plot for a Simple Linear Regression.
#' @param x Vector of independent variable (explanatory variable) data.
#' @param y Vector of dependent variable (response variable) data.
#' @param xl Label of x-axis (default=x variable name).
#' @param yl Label of y-axis (default=y variable name).
#' @param mt Title of the scatter plot.
#' @param plot Logical value for drawing scatter plots, Default: TRUE.
#' @param dig Number of digits below the decimal point, Default: 4.
#' @return None.
#'
#' @examples
#' reg1.pre(iris[[1]], iris[[2]])
#' reg1.pre(iris[[3]], iris[[4]])
#'
#' x = c(1095, 1110, 1086, 1074, 1098, 1105, 1163, 1124, 1088, 1064)
#' y = c(49, 52, 48, 49, 50, 51, 50, 51, 49, 48)
#' reg1.pre(x, y)
#' (y2 = y*x/1000)
#' reg1.pre(x, y2)
#' @rdname reg1.pre
#' @export

reg1.pre = function(x, y, xl, yl, mt, plot=TRUE, dig=4) {
    # Check input
	if (missing(y)) stop("A vector of dependent variable is required.")
	if (missing(x)) stop("A vector of independent variable is required.")
	if (is.list(y)) {
		if (missing(yl)) yl = names(y)
		y = as.vector(unlist(y)) }
	if (is.list(x)) {
		if (missing(xl)) xl = names(x)
		x = as.vector(unlist(x)) }
	nn = length(y)

    # Estimate correlation coefficients  by cor( ) function
	Sxx = sum(x^2)-sum(x)^2/nn
	Syy = sum(y^2)-sum(y)^2/nn
	Sxy = sum(x*y)-sum(x)*sum(y)/nn
	rxy = Sxy/sqrt(Sxx*Syy)

	cat("[1] Estimate correlation coefficients ------------------------\n")
	cat(paste0("Sxx = ", round(sum(x^2), dig), " - (", round(sum(x), dig), ")\U00B2",
		"/ ", nn, " = ", round(Sxx, dig)), "\n")
	cat(paste0("Syy = ", round(sum(y^2), dig), " - (", round(sum(y), dig), ")\U00B2",
		"/ ", nn,	" = ", round(Syy, dig)), "\n")
	cat(paste0("Sxy = ", round(sum(x*y), dig), " - (", round(sum(x), dig), "\U00D7",
		round(sum(y), dig), ") / ", nn, " = ", round(Sxy, dig)), "\n")
	cat(paste0("Cor(xy) = ", round(Sxy, dig), " / \U221A(", round(Sxx, dig),
		"\U00D7", round(Syy, dig), ") = ", round(rxy, dig)), "\n")

     # Scatter plot for prior investigation
      if (plot) {
	cat("[P] Scatter plot for prior investigation ------------------------\n")
    	# Set labels & simple regression
	if (missing(xl)) xl = deparse(substitute(x))
	if (missing(yl)) yl = deparse(substitute(y))
	if (missing(mt)) mt = paste0("Scatter Plot of ", xl, " vs. ", yl)
	lm1 = lm(y~x)

	win.graph(7, 5)
	y1 = floor(min(y, lm1$fit))
	y2 = ceiling(max(y, lm1$fit))
	plot(x, y, pch=19, main=mt, xlab=xl, ylab=yl, ylim=c(y1, y2))
	grid(col=3)
	abline(lm1, col=2)
      }
}

# [14-4] Test for Correlation in a Simple Linear Regression
#' @title Test for Correlation
#' @description Test for Correlation in a Simple Linear Regression.
#' @param x Vector of independent variable (explanatory variable) data.
#' @param y Vector of dependent variable (response variable) data.
#' @param r0 Correlation coefficient value under the null hypothesis, Default: 0.
#' @param alp Level of significance, Default: 0.05.
#' @param plot Logical value for drawing scatter plots, Default: TRUE.
#' @param dig Number of digits below the decimal point, Default: 4.
#' @return None.
#'
#' @examples
#' reg1.cor(iris[[1]], iris[[2]])
#' reg1.cor(iris[[3]], iris[[4]])
#'
#' x = c(1095, 1110, 1086, 1074, 1098, 1105, 1163, 1124, 1088, 1064)
#' y = c(49, 52, 48, 49, 50, 51, 50, 51, 49, 48)
#' reg1.cor(x, y)
#' (y2 = y*x/1000)
#' reg1.cor(x, y2)
#' reg1.cor(x, y2, r0=0.9, plot=F)
#' @rdname reg1.cor
#' @export

reg1.cor = function(x, y, r0=0, alp=0.05, plot=TRUE, dig=4) {
    # Check input
	if (missing(y)) stop("A vector of dependent variable is required.")
	if (missing(x)) stop("A vector of independent variable is required.")
	if (is.list(y)) {
		y = as.vector(unlist(y)) }
	if (is.list(x)) {
		x = as.vector(unlist(x)) }
	nn = length(x)

    # Estimate correlation coefficients  by cor( ) function
	Sxx = sum(x^2)-sum(x)^2/nn
	Syy = sum(y^2)-sum(y)^2/nn
	Sxy = sum(x*y)-sum(x)*sum(y)/nn
	rxy = Sxy/sqrt(Sxx*Syy)

    # Correlation test by cor.test( ) function
	ct = cor.test(x, y, conf.level =1-alp)
	T0 = rxy*sqrt((nn-2)/(1-rxy^2))
	pv0 = 2*pt(-abs(T0), nn-2)

	cat("[1] Test for the correlation coefficient ------------------------\n")
	cat("Sample Correlation Coefficient =", format(ct$est, F, dig), "\n")
          if (r0==0) {
	cat(paste0("t-statistic = ", format(rxy, F, dig), " \U00D7 \U221A(", nn-2,
		"/(1-", format(rxy, F, dig), "\U00B2)) = ", round(ct$stat, dig), "\n"))

	cat(paste0("p-value = P(|T", nn-2, "| > ", round(abs(T0), dig), ") = ",
		format(pv0, F, dig), "\n"))
          } else {
	Z0 = sqrt(nn-3)*0.5*(log((1+rxy)/(1-rxy))-log((1+r0)/(1-r0)))
	pv0 = 2*pnorm(-abs(Z0))
	cat(paste0("Z-statistic = \U221A(", nn-3, ") \U00D7 0.5 \U00D7 (log((1+",
		format(rxy, F, dig), ")/(1-", format(rxy, F, dig),
		"))-log((1+", r0, ")/(1-", r0, "))) = ", format(Z0, F, dig), "\n"))

	cat(paste0("p-value = P(|Z| > ", format(abs(Z0),F,dig), ") = ", format(pv0,F,dig), "\n"))
          }

    # Confidence intervals for correlation coefficients
	cat("[2] Confidence interval for the correlation coefficient -------------------\n")
	cat(paste0(100*(1-alp), "% Confidence Interval = [",
		format(ct$conf, F, dig)[1], ", ", format(ct$conf, F, dig)[2], "]\n"))

    # T-test(or z-test) plot
      if (plot) {
          if (r0==0) {
	cat("[P] T-test plot ------------------------\n")
	win.graph(7, 5)
	# Utilize ttest.plot( ) function in chapter 11
	mr = max(c(4, abs(ct$stat*1.2)))
  	ttest.plot(ct$stat, ct$para, prng=c(-mr, mr), pvout=F)
          } else {
	cat("[P] Z-test plot ------------------------\n")
	win.graph(7, 5)
	# Utilize normtest.plot( ) function in chapter 11
	mr = max(c(4, abs(Z0*1.2)))
  	normtest.plot(Z0, prng=c(-mr, mr), xlab="Z-statistic", pvout=F)
          }
      }
}

# [14-5] Draw a Scatter Plot with a Simple Linear Regression Line
#' @title Scatter Plot with a Simple Linear Regression Line
#' @description Draw a Scatter Plot with a Simple Linear Regression Line.
#' @param x Vector of independent variable (explanatory variable) data.
#' @param y Vector of dependent variable (response variable) data.
#' @param xl Label of x-axis (default=x variable name).
#' @param yl Label of y-axis (default=y variable name).
#' @param mt Title of the scatter plot.
#' @param pres Logical value for plotting residual symbols, Default: TRUE.
#' @param dig Number of digits below the decimal point, Default: 4.
#' @return None.
#'
#' @examples
#' reg1.plot(iris[[3]], iris[[4]])
#' reg1.plot(iris[[3]], iris[[4]], pres=F)
#'
#' x = c(1095, 1110, 1086, 1074, 1098, 1105, 1163, 1124, 1088, 1064)
#' y = c(49, 52, 48, 49, 50, 51, 50, 51, 49, 48)
#' (y2 = y*x/1000)
#' reg1.plot(x, y2, xl="Exchange Rate", yl="Exports")
#' @rdname reg1.plot
#' @export

reg1.plot = function(x, y, xl, yl, mt, pres=TRUE, dig=4) {
    # Check input
	if (missing(y)) stop("A vector of dependent variable is required.")
	if (missing(x)) stop("A vector of independent variable is required.")
	if (is.list(y)) {
		if (missing(yl)) yl = names(y)
		y = as.vector(unlist(y))
	} else if (missing(xl)) xl = deparse(substitute(x))
	if (is.list(x)) {
		if (missing(xl)) xl = names(x)
		x = as.vector(unlist(x))
	} else if (missing(yl)) yl = deparse(substitute(y))
	if (missing(mt)) mt = paste0("Simple Linear Regression of ", yl, " w.r.t. ", xl)

	nn = length(x)
	lm1 = lm(y~x)

    # Display the simple regression line
	y1 = floor(min(y, lm1$fit))
	y2 = ceiling(max(y, lm1$fit))

	cat("Display the simple regression line ------------------------\n")
	win.graph(7, 5)
	plot(x, y, pch=19, main=mt, xlab=xl, ylab=yl, ylim=c(y1, y2))
	grid(col=3)
	abline(lm1, lty=2, col=2)

    	# Plot regression equation
	pos = ifelse(lm1$coef[[2]]>0, "bottomright", "upright")
	sign =  ifelse(lm1$coef[[2]]>0, "+", "")
	legend(pos, c("Regression Equation", paste0("Y = ", round(lm1$coef[[1]],dig), sign,
		round(lm1$coef[[2]],dig), " * X")), text.col=c(1,4), bg="white")
    	# Plot residuals
	segments(x, y, x, lm1$fit, lty=2, col=4)
	if (pres) text(x, (y+lm1$fit)/2, labels="e", col=4, pos=4)
}


# [14-6] Calculate the Least Square Estimates for a Simple Regression
#' @title Calculate the Least Square Estimates
#' @description Calculate the Least Square Estimates for a Simple Regression.
#' @param x Vector of independent variable (explanatory variable) data.
#' @param y Vector of dependent variable (response variable) data.
#' @param dig Number of digits below the decimal point, Default: 4.
#' @return None.
#'
#' @examples
#' reg1.lse(iris[1], iris[3])
#' reg1.lse(iris[3], iris[4])
#'
#' x = c(1095, 1110, 1086, 1074, 1098, 1105, 1163, 1124, 1088, 1064)
#' y = c(49, 52, 48, 49, 50, 51, 50, 51, 49, 48)
#' (y2 = y*x/1000)
#' reg1.lse(x, y2)
#' @rdname reg1.lse
#' @export

reg1.lse = function(x, y, dig=4) {
    # Check input
	if (missing(y)) stop("A vector of dependent variable is required.")
	if (missing(x)) stop("A vector of independent variable is required.")
	if (is.list(y)) {
		y = as.vector(unlist(y)) }
	if (is.list(x)) {
		x = as.vector(unlist(x)) }
	nn = length(x)

    # Sum of squares
	Sxx = sum(x^2)-sum(x)^2/nn
	Syy = sum(y^2)-sum(y)^2/nn
	Sxy = sum(x*y)-sum(x)*sum(y)/nn

    # Detailed output of regression equation
	b1 = Sxy/Sxx
	xb = mean(x)
	yb = mean(y)
	b0 = yb - b1*xb
	sign =  ifelse(b1>0, "+", "")

    # Estimate simple regression coefficients
	cat("LSE of simple regression coefficients ------------------\n")
	cat(paste0("Sxx = ", round(sum(x^2),dig), " - (", round(sum(x),dig),
		")\U00B2", "/ ", nn, " = ", round(Sxx,dig)), "\n")
	cat(paste0("Sxy = ", round(sum(x*y),dig), " - (", round(sum(x),dig),
		"\U00D7", round(sum(y),dig), ") / ", nn, " = ", round(Sxy,dig)), "\n")
	cat(paste0("Slope = ", round(Sxy,dig), " / ", round(Sxx,dig), " = ",
		format(b1,F,dig)), "\n")
	cat(paste0("Intersection = ", round(yb,dig), " - ", round(b1,dig),
		"\U00D7", round(xb,dig), " = ", round(b0,dig)), "\n")
	cat(paste0("Regression Equation : y = ", round(b0,dig), " ", sign, " ",
		format(abs(b1),F,dig), " x"), "\n")
}

# [14-7] Test for the Significance of a Simple Regression Model
#' @title Test for the Significance of a Simple Regression Model
#' @description Test for the Significance of a Simple Regression Model by ANOVA.
#' @param x Vector of independent variable (explanatory variable) data.
#' @param y Vector of dependent variable (response variable) data.
#' @param alp Level of significance, Default: 0.05.
#' @param dig Number of digits below the decimal point, Default: 4.
#' @return ANOVA table.
#'
#' @examples
#' reg1.aov(iris[[1]], iris[[2]])
#' reg1.aov(iris[[3]], iris[[4]])
#' @rdname reg1.aov
#' @export

reg1.aov = function(x, y, alp=0.05, dig=4) {
    # Check input
	if (missing(y)) stop("A vector of dependent variable is required.")
	if (missing(x)) stop("A vector of independent variable is required.")
	if (is.list(y)) {
		y = as.vector(unlist(y)) }
	if (is.list(x)) {
		x = as.vector(unlist(x)) }
	nn = length(x)
	lm1 = lm(y~x)
	an1 = anova(lm1)

    # Analysis of variance
	Sxx = sum(x^2)-sum(x)^2/nn
	Syy = sum(y^2)-sum(y)^2/nn
	Sxy = sum(x*y)-sum(x)*sum(y)/nn
	SST = Syy
	SSR = Sxy^2 / Sxx
	SSE = SST-SSR
	Rsq = SSR/SST

    # ANOVA table by calculating sum of squares
	cat("Calculating sum of squares -------------------\n")
	cat(paste0("SST = ", round(sum(y^2),dig), " - (", round(sum(y),dig), ")\U00B2",
		"/ ", nn,	" = ", round(SST,dig)), "\n")
	cat(paste0("SSR = (", round(Sxy,dig), ")\U00B2", "/ ", round(Sxx,dig),
		" = ", round(SSR,dig)), "\n")
	cat(paste0("SSE = ", round(SST,dig), " - ", round(SSR,dig),
		" = ", round(SSE,dig)), "\n")

	# p-value
	pv1 = an1$Pr[1]
	if (pv1<0.001) {star ="***"
	} else if (pv1<0.01) {star ="**"
	} else if (pv1<0.05) star ="*"

	cat("--------------- ANOVA table  ---------------------\n")
	cr.v = c(qf(1-alp, an1$Df[1], an1$Df[2]), NA)
	antab = matrix(NA, 3, 6)
	colnames(antab) = c("Sum Sq.", "df", "Mean Sq.", "F0", "Crit.-v", "P-value")
	rownames(antab)=c("Regress", "Residual", "Total")
	antab[1:2,]=cbind(an1$Sum, an1$Df, an1$Mean, an1$F, cr.v, an1$Pr)
	antab[3,1:2]=c(sum(an1$Sum), sum(an1$Df))

	dum = as.data.frame(antab)
	dum=format(dum, F, dig)
	for (k in 3:6) dum[, k]=sub("NA", "  ", dum[, k])
	print(dum)

	cat(paste0("F-test critical value = ", round(qf(1-alp, 1, nn-2),dig)), "\n")
	cat(paste0("R-square = ", round(SSR,dig), " / ", round(SST,dig),
		" = ", round(Rsq,dig)), "\n")
        # [Correction]
	invisible(lm1)
}


# [14-8] Test for the Significance of Simple Regression Coefficients
#' @title Test on the Simple Regression Coefficients
#' @description Test for the Significance of Simple Regression Coefficients.
#' @param x Vector of independent variable (explanatory variable) data.
#' @param y Vector of dependent variable (response variable) data.
#' @param alp Level of significance, Default: 0.05.
#' @param dig Number of digits below the decimal point, Default: 4.
#' @return None.
#'
#' @examples
#' reg1.test(iris[1], iris[2])
#' reg1.test(iris[3], iris[4])
#'
#' x = c(1095, 1110, 1086, 1074, 1098, 1105, 1163, 1124, 1088, 1064)
#' y = c(49, 52, 48, 49, 50, 51, 50, 51, 49, 48)
#' (y2 = y*x/1000)
#' reg1.test(x, y2)
#'
#' rr = reg1.aov(x, y2)
#' reg1.test(rr)
#'
#' reg1.test(lm(y2 ~ x))
#' @rdname reg1.test
#' @export

reg1.test = function(x, y, alp=0.05, dig=4) {
    # [Correction]
      if ("lm" %in% class(x)) {
	nn = length(x$fitted)
	MSE = anova(x)$Mean[2]
	slm = summary(x)
	b0 = slm$coef[1,1]
	b1 = slm$coef[2,1]
	se0 = slm$coef[1,2]
	se1 = slm$coef[2,2]
      } else {
        # Check input (Do not use lm() or anova())
	if (missing(y)) stop("A vector of dependent variable is required.")
	if (missing(x)) stop("A vector of independent variable is required.")
	if (is.list(y)) {
		y = as.vector(unlist(y)) }
	if (is.list(x)) {
		x = as.vector(unlist(x)) }
	nn = length(x)

        # Analysis of variance
	Sxx = sum(x^2)-sum(x)^2/nn
	Syy = sum(y^2)-sum(y)^2/nn
	Sxy = sum(x*y)-sum(x)*sum(y)/nn
	SST = Syy
	SSR = Sxy^2 / Sxx
	SSE = SST-SSR
	MSE = SSE/(nn-2)

	b1 = Sxy/Sxx
	xb = mean(x)
	yb = mean(y)
	b0 = yb - b1*xb
	se1 = sqrt(MSE/Sxx)
	se0 = sqrt(MSE*(1/nn+xb^2/Sxx))
      }

        # Confidence intervals & significance tests for regression coefficients
	tval = qt(1-alp/2, nn-2)
	conf = 100*(1-alp)
	tol1 = tval*se1
	tol0 = tval*se0
	tstat1 = b1/se1
	tstat0 = b0/se0
	pv1 = 2*pt(-abs(tstat1), nn-2)
	pv0 = 2*pt(-abs(tstat0), nn-2)

	cat("[1] Confidence intervals & significance tests for regression coefficients --------\n")
	cat(paste0(conf, "%CI(b1) = [", round(b1,dig), " \U00B1 ", round(tval,dig),
		"\U00D7", round(se1,dig),
		"] = [", round(b1,dig), " \U00B1 ", round(tol1,dig), "] = [",
		round(b1-tol1,dig), ", ", round(b1+tol1,dig), "]\n"))
	cat(paste0(conf, "%CI(b0) = [", round(b0,dig), " \U00B1 ", round(tval,dig),
		"\U00D7", round(se0,dig),
		"] = [", round(b0,dig), " \U00B1 ", round(tol0,dig), "] = [",
		round(b0-tol0,dig), ", ", round(b0+tol0,dig), "]\n"))
	cat("[2] Significance tests for regression coefficient ------------------------\n")
	cat(paste0("T1 = ", round(b1,dig), " / ", round(se1,dig), " = ",
		round(tstat1,dig), "\t P-val = P(|T_", nn-2, "| > ",
		round(abs(tstat1),dig), ") = ", format(pv1,F,dig)), "\n")
	cat(paste0("T0 = ", round(b0,dig), " / ", round(se0,dig), " = ",
		round(tstat0,dig), "\t P-val = P(|T_", nn-2, "| > ",
		round(abs(tstat0),dig), ") = ", format(pv0,F,dig)), "\n")
}

# [14-9] Prediction and Confidence Interval in a Simple Regression
#' @title Prediction and Confidence Interval
#' @description Prediction and Confidence Interval in a Simple Regression.
#' @param x Vector of independent variable (explanatory variable) data.
#' @param y Vector of dependent variable (response variable) data.
#' @param xl Label of x-axis (default=x variable name).
#' @param yl Label of y-axis (default=y variable name).
#' @param mt Plot title.
#' @param plot Logical value for confidence & predition bands, Default: FALSE.
#' @param x0 Conditioning value of the independent variable.
#' @param xrng Plot range of the independent variable.
#' @param by Plot interval of the independent variable.
#' @param alp Level of significance, Default: 0.05.
#' @param dig Number of digits below the decimal point, Default: 4.
#' @return None.
#'
#' @examples
#' reg1.pred(iris[3], iris[4], x0=2)
#' reg1.pred(iris[3], iris[4], x0=2, plot=T)
#'
#' x = c(1095, 1110, 1086, 1074, 1098, 1105, 1163, 1124, 1088, 1064)
#' y = c(49, 52, 48, 49, 50, 51, 50, 51, 49, 48)
#' (y2 = y*x/1000)
#' reg1.pred(x, y2, x0=1200, plot=T)
#'
#' rr = reg1.aov(x, y2)
#' reg1.pred(rr, x0=1200, plot=T)
#'
#' reg1.pred(lm(y2 ~ x), x0=1200, plot=T)
#' @rdname reg1.pred
#' @export

reg1.pred = function(x, y, xl, yl, mt, plot=FALSE, x0, xrng, by, alp=0.05, dig=4) {
    # [Correction]
      if ("lm" %in% class(x)) {
	lm1 = x
	x = lm1$model[[2]]
	y = lm1$model[[1]]
	if (missing(yl)) yl = names(lm1$model)[[1]]
	if (missing(xl)) xl = names(lm1$model)[[2]]
      } else {
        # Check input
	if (missing(y)) stop("A vector of dependent variable is required.")
	if (missing(x)) stop("A vector of independent variable is required.")
	if (is.list(y)) {
		if (missing(yl)) yl = names(y)
		y = as.vector(unlist(y))
	} else if (missing(xl)) xl = deparse(substitute(x))
	if (is.list(x)) {
		if (missing(xl)) xl = names(x)
		x = as.vector(unlist(x))
	} else if (missing(yl)) yl = deparse(substitute(y))

        # Regression
	lm1 = lm(y~x)
      }
    # Common Procedure
	if (missing(x0)) x0 = max(x)

	nn = length(x)
	xb = mean(x)
	yb = mean(y)
	Sxx = sum(x^2)-sum(x)^2/nn
	b0 = lm1$coef[[1]]
	b1 = lm1$coef[[2]]
	MSE = anova(lm1)$Mean[2]
	tval = qt(1-alp/2, nn-2)
	conf = 100*(1-alp)

    # Confidence intervals and prediction intervals at x0
	Ex0 = b0+b1*x0
	vcx0 = MSE*(1/nn+(x0-xb)^2/Sxx)
	vpx0 = MSE*(1+1/nn+(x0-xb)^2/Sxx)
	cse = sqrt(vcx0)
	pse = sqrt(vpx0)
	ctol = tval*cse
	ptol = tval*pse
      if (!plot) {
	cat("[1] Confidence interval for E[Y|x0] -------------\n")
	cat(paste0(conf, "%CI of E(Y|", x0, ") = [", round(Ex0, dig), " \U00B1 ",
		round(tval, dig), "\U00D7", round(cse, dig),
		"] = [", round(Ex0, dig), " \U00B1 ", round(ctol, dig), "] = [",
		round(Ex0-ctol,dig), ", ", round(Ex0+ctol,dig), "]\n"))
	cat("[2] Prediction interval for Y|x0 -------------------\n")
	cat(paste0(conf, "%PI pf (Y|", x0, ") = [", round(Ex0, dig), " \U00B1 ",
		round(tval, dig), "\U00D7", round(pse, dig),
		"] = [", round(Ex0, dig), " \U00B1 ", round(ptol, dig), "] = [",
		round(Ex0-ptol,dig), ", ", round(Ex0+ptol,dig), "]\n"))
      }
    # Plot confidence bands and prediction bands
      if (plot) {
	cat("[P] Plot confidence bands and prediction bands ------------------------\n")
	x1 = min(x0, x)
	x2 = max(x0, x)
	if (missing(xrng)) xrng = c(x1-0.1*(x2-x1), x2+0.1*(x2-x1))
	if (missing(by)) by = (x2-x1)/50
    	# Set data frame
	nd = data.frame(x=seq(xrng[1], xrng[2], by=by))
    	# Confidence intervals and prediction intervals
	conf2 = predict(lm1, interval="confidence", newdata=nd)
	pred2 = predict(lm1, interval="prediction", newdata=nd)
	y1 = min(pred2[, "lwr"])
	y2 = max(pred2[, "upr"])
	ymin = y1 - (y2-y1)*0.1
	ymax = y2 + (y2-y1)*0.1
   	# Plot confidence bands and prediction bands
	win.graph(7, 6)
	plot(x, y, pch=19, main=paste("Confidence and Prediction Bands of", yl, "given", xl),
		xlab=xl, ylab=yl, ylim=c(ymin, ymax), xlim=xrng)
	# Regression line, confidence band, and prediction band
	abline(lm1, col=4)
	abline(v=xb, lty=2, col=3)
	text(xb, ymin, labels=expression(bar(x)), pos=4)
	matlines(nd$x, conf2[,c("lwr","upr")], col=2, type="p", pch="+")
	matlines(nd$x, pred2[,c("lwr","upr")], col=4, type="p", pch=1)
	abline(v=x0, lty=2, col="orange")
	text(x0, ymin, labels=expression(x[0]), cex=0.9, col="green4", pos=4)
	text(x0, Ex0+c(-ptol, 0, ptol), labels=round(Ex0+c(-ptol, 0, ptol), dig),
		cex=0.8, col="green4", pos=c(1,1,3))
      }
}


# [14-10] Draw Diagnosis Plots of a Simple Regression Model
#' @title Draw Diagnosis Plots
#' @description Draw Diagnosis Plots of a Simple Regression Mode.
#' @param x Vector of independent variable (explanatory variable) data.
#' @param y Vector of dependent variable (response variable) data.
#' @param wh Index vector for the diagnosis plots, Default: c(1:3,5).
#' @return None.
#'
#' @examples
#' reg1.diag(iris[3], iris[4])
#' reg1.diag(iris[3], iris[4], wh=1:2)
#'
#' x = c(1095, 1110, 1086, 1074, 1098, 1105, 1163, 1124, 1088, 1064)
#' y = c(49, 52, 48, 49, 50, 51, 50, 51, 49, 48)
#' (y2 = y*x/1000)
#' reg1.diag(x, y2)
#'
#' rr = reg1.aov(x, y2)
#' reg1.diag(rr)
#'
#' reg1.diag(lm(y2 ~ x))
#' @rdname reg1.diag
#' @export

reg1.diag = function(x, y, wh=c(1:3,5)) {
    # [Correction]
      if ("lm" %in% class(x)) {
	lm1 = x
      } else {
      # Check input
	if (missing(y)) stop("A vector of dependent variable is required.")
	if (missing(x)) stop("A vector of independent variable is required.")
	if (is.list(y)) {
		y = as.vector(unlist(y)) }
	if (is.list(x)) {
		x = as.vector(unlist(x)) }
	lm1 = lm(y~x)
      }

	nw = length(wh)
    # Diagnosis of the regression model
	cat("Diagnosis of the regression model ------------------------\n")
    # [Correction]
	nn=switch(nw, c(4,4,1,1), c(6,3,1,2), c(9,3,1,3), c(6,6,2,2), c(9,6,2,3), c(9,6,2,3))
	win.graph(nn[1], nn[2])
	par(mfrow=c(nn[3], nn[4]))
	plot(lm1, which=wh)
}

# Multiple Regression Analysis --------------------------------------------------------
# Scatter plot matrix with correlation coefficients
panel.cor = function(x, y, alp=0.05, digits = 4, prefix = "", cex.cor, ...)
{
    usr = par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    cxy = cor(x, y)
    r = abs(cxy)
    txt = format(c(r, 0.123456789), digits = digits)[1]
    txt = paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor = 0.5/strwidth(txt)
    cc2 = cex.cor*0.8
    text(0.5, 0.6, format(cxy, F, digits), col=ifelse(cxy>0, 2, 4), cex = cex.cor)
    pv = cor.test(x, y, conf.level =1-alp)$p.val
    text(0.5, 0.3, paste0("P-v=", format(pv, F, digits)), cex = cc2, col=1)
}

# [14-11] Calculate Least Square Estimates for a Multiple Regression
#' @title Calculate Least Square Estimates: Multiple Regression
#' @description Calculate Least Square Estimates for a Multiple Regression.
#' @param form Formula of regression model (ex: y ~ x1 + x2)
#' @param plot Logical value for drawing a scatter plot matrix, Default: TRUE.
#' @param detail Logical value for printing detailed calculations, Default: TRUE.
#' @param dig Number of digits below the decimal point, Default: 4
#' @return Matrices for calculating the LSE.
#'
#' @examples
#' attach(mtcars)
#' form = mpg ~ hp + drat + wt
#' mreg.lse(form)
#'
#' data(exa14_10)
#' Score=exa14_10[[1]]; Study=exa14_10[[2]]; Reading=exa14_10[[3]]
#' form2 = Score ~ Study + Reading
#' mreg.lse(form2)
#' @rdname mreg.lse
#' @export

mreg.lse = function(form, plot=TRUE, detail=T, dig=4) {
    # Check input
	if (missing(form)) stop("A formula for the regression model is required.")
	dum = model.frame(form)
    # Set inputs
	kk = length(dum)
	ke = kk-1
	y = dum[1]
	xd = dum[2:kk]
	xl = names(xd)
	yl = names(y)
	y = dum[[1]]
	nn = length(y)
    # Check Model
	dd2 = attr(terms(form), "factors")[-1, ]
	kk2 = ncol(dd2)
	# Extended Case
	if (kk2 > ke) {
		idx = list()
		for (k in kk:kk2) idx[[k]]=as.numeric(which(dd2[,k]==1))
		xd2 = cbind(xd, matrix(1, nn, kk2-ke))
		for (k in kk:kk2) for (j in 1:length(idx[[k]])) xd2[,k]=xd2[,k]*xd[, idx[[k]][j]]
		names(xd2)=colnames(dd2)
		xd=xd2
		xl = names(xd)
		ke = length(xd)
		kk = ke+1
	}

     # Prior investigation of data -----------------------------------------
      if (plot) {
	cat("[P] Scatter plot matrix for prior investigation of data --------------------\n")
	win.graph(7, 5)
	xyd = as.data.frame(cbind(xd, y))
	names(xyd) = c(xl, yl)
	pairs( xyd, lower.panel=panel.cor,
		upper.panel=function(x, y){
		points(x, y)
		abline(lm(y~x), col='red') })
      }

    # LSE of multiple regression coefficients
	X = as.matrix(xd)
	const = rep(1, nn)
	X = cbind(const, X)
	XX = t(X) %*% X
	Xy = t(X) %*% y
	colnames(Xy)=yl
	XXI = solve(XX)
	bh = XXI %*% Xy

	cat("LSE of multiple regression coefficients ------------------------\n")
	fbh = format(c(bh,0.123456789),F,dig)
	fbh = fbh[-length(fbh)]
      if (detail) {
	cat("X'X matrix ----------\n")
	print(XX)
	cat("LSE : b = inv(X'X) (X'y) ----------\n")
	fXXI = format(XXI,F,dig)
	fXy = format(Xy,F,dig)

	cat("b0:  ", fXXI[1, ], "  ", fXy[1], "  ", fbh[1], "\n")
	plab = slab = blab = clab = rep("", ke)
	for (k in 1:ke) clab[k] = paste0(as.numeric(which(dd2[,k]==1)), collapse="")
	for (k in 1:ke) blab[k] = paste0("b", clab[k])
	for (k in 1:ke) slab[k] = paste0(rep(" ", 3-nchar(clab[k])), collapse="")
	for (k in 1:ke) plab[k] = paste0(blab[k], ":", slab[k])
	for (k in 1:ke) cat(plab[k], fXXI[k+1, ], "  ", fXy[k+1], "  ", fbh[k+1], "\n")
      } else  {
	names(fbh) = colnames(X)
	print(as.data.frame(t(fbh)))
      }
      out = list(kk=kk, xl=xl, xd=xd, X=X, XX=XX, Xy=Xy, XXI=XXI)
      invisible(out)
}

# [14-12] Test for the Significance of a Multiple Regression Model
#' @title Test for the Significance of a Multiple Regression Model
#' @description Test for the Significance of a Multiple Regression Model via ANOVA.
#' @param form Formula of regression model (ex: y ~ x1 + x2)
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#' @return ANOVA table.
#'
#' @examples
#' attach(mtcars)
#' form = mpg ~ hp + drat + wt
#' mreg.aov(form)
#'
#' data(exa14_10)
#' Score=exa14_10[[1]]; Study=exa14_10[[2]]; Reading=exa14_10[[3]]
#' form2 = Score ~ Study + Reading
#' mreg.aov(form2)
#' @rdname mreg.aov
#' @export

mreg.aov = function(form, alp=0.05, dig=4) {
    # Check input
	if (missing(form)) stop("A formula for the regression model is required.")
	dum = model.frame(form)
    # Set inputs
	lse = mreg.lse(form, detail=F, plot=F, dig=dig)
	kk = lse$kk
	ke = kk-1
	y = dum[1]
	xl = lse$xl
	yl = names(y)
	y = dum[[1]]
	nn = length(y)
    # Regression
	lm1 = lm(form)
	an1 = anova(lm1)

    # Analysis of variance
	SST = sum(an1$Sum)
	SSR = sum(an1$Sum[1:ke])
	SSE = SST-SSR
	dfe = nn-kk
	MSR = SSR/ke
	MSE = SSE/dfe
	F0 = MSR/MSE
	pv0 = 1-pf(F0, ke, dfe)
	crv0 = qf(1-alp, ke, dfe)
	crv1 = rep(qf(1-alp, 1, dfe), ke)
	Rsq = SSR/SST
	aRsq = 1-SSE/SST*(nn-1)/(nn-kk)
	tval = qt(1-alp/2, dfe)
	bh = lm1$coef
	Xy = lse$Xy
    # ANOVA table by calculating sum of squares
	cat("Calculating sum of squares ----------------------\n")
	cat(paste0("SST = ", round(sum(y^2), dig), " - (", round(sum(y), dig), ")\U00B2",
		"/ ", nn,	" = ", round(SST, dig)), "\n")
	cat(paste0("SSR = (", paste(round(bh, dig), collapse=" "), ").(",
		paste(round(Xy, dig), collapse=" "), ") = ", round(SSR, dig)), "\n")
	cat(paste0("SSE = ", round(SST, dig), " - ", round(SSR, dig), " = ",
		round(SSE, dig)), "\n")

	cat("Analysis of Variance Table------------------------\n")
	antab=cbind(an1$Sum[1:ke], an1$Df[1:ke], an1$Mean[1:ke], an1$F[1:ke], crv1, an1$Pr[1:ke])
	antab=rbind(antab, c(SSR, ke, MSR, F0, crv0, pv0))
	antab=rbind(antab, c(an1$Sum[kk], an1$Df[kk], an1$Mean[kk], NA, NA, NA))
	antab=rbind(antab, c(sum(an1$Sum), sum(an1$Df), NA, NA, NA, NA))
	colnames(antab) = c("Sum Sq.", "df", "Mean Sq.", "F0", "Crit.-v", "P-value")
	rownames(antab)=c(xl, "Regression", "Residual", "Total")

	# dum=round(antab, dig)
	dum = as.data.frame(antab)
	dum=format(dum, F, dig)
	# dum[is.na(dum)]=""
	for (k in 3:6) dum[, k]=sub("NA", "  ", dum[, k])
	print(dum)

	cat(paste0("R-square = ", round(SSR, dig), " / ", round(SST, dig), " = ",
		round(Rsq, dig)), "\n")
	cat(paste0("Adj R-sq = 1 - (", round(SSE, dig), "/", nn-kk, ") / (",
		round(SST, dig), "/", nn-1, ") = ", round(aRsq, dig)), "\n")
    # [Correction]
	invisible(lm1)
}

# [14-13] Test for the Significance of Multiple Regression Coefficients
#' @title Test on the Multiple Regression Coefficients
#' @description Test for the Significance of Multiple Regression Coefficients.
#' @param form Formula of regression model (ex: y ~ x1 + x2)
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#' @return Table of statistics.
#'
#' @examples
#' attach(mtcars)
#' form = mpg ~ hp + drat + wt
#' mreg.test(form)
#'
#' aa = mreg.aov(form)
#' mreg.test(aa)
#'
#' mreg.test(lm(form))
#'
#' data(exa14_10)
#' Score=exa14_10[[1]]; Study=exa14_10[[2]]; Reading=exa14_10[[3]]
#' form2 = Score ~ Study + Reading
#' mreg.test(form2)
#'
#' bb = mreg.aov(form2)
#' mreg.test(bb)
#'
#' mreg.test(lm(form2))
#' @rdname mreg.test
#' @export

mreg.test = function(form, alp=0.05, dig=4) {
    # [Correction]
      if ("lm" %in% class(form)) {
	lm1 = form
	form = formula(lm1)
      } else {
        # Check input
	if (missing(form)) stop("A formula for the regression model is required.")
        # Regression
	lm1 = lm(form)
      }
        # Common procedure
	dum = model.frame(form)
        # Set inputs
	lse = mreg.lse(form, detail=F, plot=F, dig=dig)
	kk = lse$kk
	ke = kk-1
	y = dum[1]
	xl = lse$xl
	yl = names(y)
	y = dum[[1]]
	nn = length(y)
        # ANOVA
	an1 = anova(lm1)

    # LSE of multiple regression coefficients
	XXI = lse$XXI
	bh = lm1$coef
	MSE = an1$Mean[kk]
	dfe =  an1$Df[kk]
	tval = qt(1-alp/2, dfe)
    # Confidence intervals & significance tests for regression coefficients
	dXXI = diag(XXI)
	se = sqrt(MSE * dXXI)
	tstat = bh/se
	tol = tval*se
	lcl = bh - tol
	ucl = bh + tol
	pv = 2*pt(-abs(tstat), dfe)
	# Print output
	cat(paste0(100*(1-alp), "% CI & Tests for regression coefficients"), "-------------\n")
	citab = cbind(bh, se, tol, lcl, ucl, tstat, pv)
	colnames(citab)=c("Estimate", "Std Err", "Tolerance", "LCL", "UCL", "T-stat", "P-value")
	dum = format(as.data.frame(rbind(citab,rep(0.123456789,7))), F, dig)
	dum = dum[-nrow(dum), ]
	print(dum)
	invisible(citab)
}


# [14-14] Prediction and Confidence Interval in a Multiple Regression Model
#' @title Prediction and Confidence Interval: Multiple Regression
#' @description Prediction and Confidence Interval in a Multiple Regression Model.
#' @param form Formula of regression model (ex: y ~ x1 + x2)
#' @param newd A new data frame of independent variables.
#' @param pvx Designated number of independent variables, Default: 1.
#' @param xrng Plot range of the independent variable.
#' @param nx Number of plotting points, Default: 50.
#' @param plot Logical value for drawing conf. & pred. bands, Default: FALSE.
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#'
#' @examples
#' attach(mtcars)
#' form = mpg ~ hp + drat + wt
#' newd = data.frame(hp=300, drat=4, wt=4)
#' mreg.pred(form, newd)
#' mreg.pred(form, plot=T, pvx=1, xrng=c(30,400))
#' mreg.pred(form, plot=T, pvx=2, xrng=c(2,6))
#' mreg.pred(form, plot=T, pvx=3, xrng=c(1,6))
#'
#' aa = mreg.aov(form)
#' mreg.pred(aa, newd)
#' mreg.pred(aa, plot=T, pvx=1, xrng=c(30,400))
#'
#' mreg.pred(lm(mpg ~ hp + drat + wt), newd)
#' mreg.pred(lm(mpg ~ hp + drat + wt), plot=T, pvx=1, xrng=c(30,400))
#'
#' data(exa14_10)
#' Score=exa14_10[[1]]; Study=exa14_10[[2]]; Reading=exa14_10[[3]]
#' form2 = Score  Study + Reading
#' nd= data.frame(Study=5, Reading=5)
#' mreg.pred(form2, newd=nd)
#' mreg.pred(form2, xrng=c(0, 15), pvx=1, plot=T)
#' mreg.pred(form2, xrng=c(0, 15), pvx=2, plot=T)
#'
#' bb = mreg.aov(form2)
#' mreg.pred(bb, nd)
#' mreg.pred(bb, xrng=c(0, 15), pvx=1, plot=T)
#'
#' mreg.pred(lm(form2), nd)
#' mreg.pred(lm(form2), xrng=c(0, 15), pvx=1, plot=T)
#' @rdname mreg.pred
#' @export

mreg.pred = function(form, newd, pvx=1, xrng, nx=50, plot=F, alp=0.05, dig=4) {
    # [Correction]
      if ("lm" %in% class(form)) {
	lm1 = form
	form = formula(lm1)
      } else {
        # Check input
	if (missing(form)) stop("A formula for the regression model is required.")
	lm1 = lm(form)
      }
    # Common Procedure
	if (!plot & missing(newd)) stop("New data for the prediction is required.")
	dum = model.frame(form)
    # Set inputs
	lse = mreg.lse(form, detail=F, plot=F, dig=dig)
	xd = lse$xd
	kk = lse$kk
	ke = kk-1
	y = dum[1]
	xl = lse$xl
	yl = names(y)
	y = dum[[1]]
	nn = length(y)
	conf = 100*(1-alp)

    # Confidence intervals and prediction intervals at x=newd
      if (!plot) {
	cat("Confidence intervals and Prediction intervals at x=newd --------------\n")
	print(newd)
	cat(paste0(conf, "% Confidence intervals --------------\n"))
	print(round(predict(lm1, newd, interval="confidence"), dig))
	cat(paste0(conf, "% Prediction intervals --------------\n"))
	print(round(predict(lm1, newd, interval="prediction"), dig))

    # Plot confidence bands and prediction bands
      } else {
	cat("Plot Confidence bands and Prediction bands --------------\n")
	xb = as.numeric(apply(xd, 2, mean))
    	# Set plot range
	if (missing(xrng)) {
		xmin = min(xd[[pvx]])
		xmax = max(xd[[pvx]])
		xspan = xmax-xmin
		xrng = c(xmin, xmax)+xspan*c(-0.1, 0.1)
	}
	xrng2 = range(xrng)+(max(xrng)-min(xrng))*c(-0.02, 0.02)
	avx = setdiff(1:ke, pvx)
	ndat = as.data.frame(matrix(NA, nx, ke))
	ndat[[pvx]] = seq(xrng[1], xrng[2], length=nx)
	for (k in avx) ndat[[k]] = rep(xb[k], nx)
	names(ndat) = xl

	# Confidence intervals and prediction intervals
	conf1 = predict(lm1, interval="confidence", newdata=ndat)
	pred1 = predict(lm1, interval="prediction", newdata=ndat)
	# Plot confidence band and prediction band
	y1 = min(pred1[, "lwr"])
	y2 = max(pred1[, "upr"])
	ymin = y1 - (y2-y1)*0.1
	ymax = y2 + (y2-y1)*0.1
	win.graph(8, 6)
	plot(xd[[pvx]], y, pch=19, cex=1.2, main=paste("Confidence and Prediction Bands of ",
		yl, "given", xl[pvx]), xlab=xl[pvx], ylab=yl, xlim=xrng2, ylim=c(ymin, ymax))
	lines(ndat[[pvx]], conf1[,1], lty=2, col="purple")
	abline(v=c(xrng[1], xb[pvx], xrng[2]), lty=2, col=3)
	text(xb[pvx], ymin, labels=expression(bar(x)), pos=4)
	text(xrng,  ymin, round(xrng, dig), cex=0.8)
	matlines(ndat[[pvx]], conf1[,c("lwr","upr")],col=2, lty=1,type="b",pch="+")
	matlines(ndat[[pvx]], pred1[,c("lwr","upr")],col=4, lty=2,type="b",pch=1)
	text(xrng[1], conf1[1,], labels=format(conf1[1,], digit=4), cex=0.8, col=2, pos=c(1,1,3))
	text(xrng[2], conf1[nx,], labels=format(conf1[nx,], digit=4), cex=0.8, col=2, pos=c(1,1,3))
      }
}

# [14-15] Compare Two Multiple Regression Models
#' @title Compare Two Multiple Regression Models
#' @description Compare Two Multiple Regression Models.
#' @param form Formula of regression model (ex: y ~ x1 + x2)
#' @param form2 Formula for another regression model (eg: y ~ x1 * x2)
#' @param plot Logical value for drawing diagnosis plots, Default: FALSE.
#' @param detail Logical value for printing detailed calculations, Default: FALSE.
#' @param wh Index vector for the diagnosis plots, Default: c(1:3,5).
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#'
#' @examples
#' attach(mtcars)
#' form.1 = mpg ~ hp + drat + wt
#' form.2 = mpg ~ hp * drat * wt
#' mreg.two(form.1, form.2)
#' mreg.two(lm(form.1), lm(form.2))
#'
#' data(exa14_10)
#' Score=exa14_10[[1]]; Study=exa14_10[[2]]; Reading=exa14_10[[3]]
#' form2.1 = Score ~ Study + Reading
#' form2.2 = Score ~ Study * Reading
#' mreg.two(form2.1, form2.2, plot=T)
#' mreg.two(lm(form2.1), lm(form2.2), plot=T)
#' @rdname mreg.two
#' @export

mreg.two = function(form1, form2, plot=FALSE, detail=FALSE, wh=c(1:3,5), alp=0.05, dig=4) {
    # [Correction]
      if ("lm" %in% class(form1)) {
	lm1 = form1
	form1 = formula(lm1)
	lm2 = form2
	form2 = formula(lm2)
      } else {
        # Check input
	if (missing(form1)) stop("A formula for the regression model 1 is required.")
	if (missing(form2)) stop("A formula for the regression model 2 is required.")
        # Regression
	lm1 = lm(form1)
	lm2 = lm(form2)
      }

      if (detail) {
	cat("[P1] ANOVA for Model 1 ------------------------\n")
	mreg.aov(form1, alp=alp, dig=dig)
	cat("[P2] ANOVA for Model 2 ------------------------\n")
	mreg.aov(form2, alp=alp, dig=dig)
      }
    # Compare and diagnose regression models
	cat("[1] Compare regression models (analysis of variance) -------------\n")
	an12 = anova(lm1, lm2)
	print(an12)
      if (plot) {
	cat("[2] Diagnose regression Model 1 & Model 2 -------------\n")
	nwh = length(wh)
	win.graph(2.5*nwh, 5)
	par(mfrow=c(2,nwh))
	plot(lm1, which=wh)
	title(main="Model 1", col.main=4)
	plot(lm2, which = wh)
	title(main="Model 2", col.main=4)
      }
}

# ---------------------------------------------------------------------------------------------------------
# [14-21] Multiple Scatter Plots from Lists of Bivariate Data
#' @title Multiple Scatter Plots
#' @description Multiple Scatter Plots from Lists of Bivariate Data.
#' @param X List vector of bivariate data with 2 columns.
#' @param item String vector of list names.
#' @param xl Label of x-axis (default="group1").
#' @param yl Label of y-axis (default="group2").
#' @param mt Plot title.
#' @param step Steps for correlation analysis, Default: 1:4.
#' @param alp Level of significance, Default: 0.05.
#' @param dig Number of digits below the decimal point, Default: 4.
#' @return None.
#'
#' @examples
#' xd = list(cbind(iris[[1]], iris[[2]]), cbind(iris[[3]], iris[[4]]))
#' spec = c("Sepal", "Petal")
#' corr.mplot(X=xd, item=spec, xl="Length", yl="Width")
#' @rdname corr.mplot
#' @export
corr.mplot = function(X, item, xl, yl, mt, step=1:4, alp=0.05, dig=4) {
    # Number of data pairs
	nv = length(X)
    # Set the graphic window
	nc = ifelse(nv<=5, 2, 3)
	nr = ceiling(nv/nc)
	h = ifelse(nr>2, 9, ifelse(nr==1, 4, 6))
	w = ifelse(nc>2, 9, 7)
    # Multiple scatter plots -----------------------------------------
      if (1 %in% step) {
	cat("[Step 1] Scatter plot for prior investigation of data -------------\n")
	if (missing(mt)) mt = paste0("Scatter Plot (", letters[1:nv] ,") ", item)
	if (missing(xl)) xl = "group1"
	if (missing(yl)) yl = "group2"
	win.graph(w, h)
	par(mfrow=c(nr, nc))
	for (k in 1:nv) {
  		plot(X[[k]][,1], X[[k]][,2], pch=19, main=mt[k], xlab=xl, ylab=yl)
  		abline(lm(X[[k]][,2]~X[[k]][,1]), lty=2, col=2)  }
      }
    # Estimate correlation coefficients by utilizing cor( ) function
      if (2 %in% step) {
	cat("[Step 2] Estimate correlation coefficients -----------------\n")
	var1 = var2 = cov12 = cor12 = rep(NA, nv)
	for (k in 1:nv) {
		var1[k] = var(X[[k]][,1])
		var2[k] = var(X[[k]][,2])
		cov12[k] = cov(X[[k]][,1], X[[k]][,2])
		cor12[k] = cor(X[[k]][,1], X[[k]][,2])
		cat(paste0(item[k], "\t Corr(X1,X2) = ", round(cov12[k], dig), " / \U221A(",
		round(var1[k], dig), " \U00D7 ", round(var2[k], dig), ") = ", round(cor12[k], dig)), "\n")
	}
      }
    # Correlation tests by cor.test( ) function
	ct = list()
	for (k in 1:nv) ct[[k]] = cor.test(X[[k]][,1], X[[k]][,2], conf.level =1-alp)
      if (3 %in% step) {
	cat("[Step 3] Correlation tests ------------------------\n")
	for (k in 1:nv) {
		cat(item[k], "\t Corr =", round(ct[[k]]$est, dig),
		" \t t-Stat =", round(ct[[k]]$stat, dig), "\t P-v =", round(ct[[k]]$p.val, dig), "\n")
	}
      }
    # Confidence intervals for correlation coefficients
      if (4 %in% step) {
	cat("[Step 4] Confidence intervals for correlation coefficients ------------------------\n")
	for (k in 1:nv) {
		cat(item[k], "\t ", paste0(100*(1-alp), "% CI = [",
		round(ct[[k]]$conf[1], 4), ", ", round(ct[[k]]$conf[2], 4), "]\n") )
	}
      }
    # T-test plot
      if (5 %in% step) {
	cat("[Step 5] T-test plot ------------------------\n")
	win.graph(w, h)
	par(mfrow=c(nr, nc))
	# Utilize ttest.plot( ) function in chapter 11
	for (k in 1:nv) {
  		mr = max(c(3, abs(ct[[k]]$stat*1.2)))
		# Chapter 11 --> ttest.plot()
  		ttest.plot(ct[[k]]$stat, ct[[k]]$para, prng=c(-mr, mr), dig=2, mt=item[k], pvout=F)
	}
      }
}

# [14-22] Correlation Analysis & Simple Linear Regression Analysis
#' @title Correlation & Simple Regression Analysis
#' @description Correlation Analysis & Simple Linear Regression Analysis
#' @param x Vector of independent variable (explanatory variable) data
#' @param y Vector of dependent variable (response variable) data
#' @param r0 , Default: 0
#' @param xl Label of x-axis (default=x variable name)
#' @param yl Label of y-axis (default=y variable name)
#' @param mt Plot title
#' @param step Steps of simple regression analysis, Default: 1:9
#' @param x0 Specipic poit value for confidence and prediction intervals
#' @param xrng Range of x-axis
#' @param by Plotting interavl of confidence and prediction bands
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#'
#' @examples
#' corr.reg1(iris[[3]], iris[[4]])
#' corr.reg1(iris[[3]], iris[[4]], step=6)
#' corr.reg1(iris[[3]], iris[[4]], x0=3, step=11)
#' @rdname corr.reg1
#' @export
corr.reg1 = function(x, y, r0=0, xl, yl, mt, step=1:4, x0, xrng, by, alp=0.05, dig=4) {
     # Set labels & simple regression
	if (missing(xl)) xl = deparse(substitute(x))
	if (missing(yl)) yl = deparse(substitute(y))
	if (missing(x0)) x0 = max(x)
	nn = length(x)
	lm1 = lm(y~x)
     # Scatter plot for prior investigation
      if (1 %in% step | 6 %in% step) {
	cat("[Step 1] Scatter plot for prior investigation ------------------------\n")
	if (missing(mt)) mt = paste0("Scatter Plot of ", xl, " vs. ", yl)
	win.graph(7, 5)
	y1 = floor(min(y, lm1$fit))
	y2 = ceiling(max(y, lm1$fit))
	plot(x, y, pch=19, main=mt, xlab=xl, ylab=yl, ylim=c(y1, y2))
	grid(col=3)
      }
    # Estimate correlation coefficients  by cor( ) function
	Sxx = sum(x^2)-sum(x)^2/nn
	Syy = sum(y^2)-sum(y)^2/nn
	Sxy = sum(x*y)-sum(x)*sum(y)/nn
	rxy = Sxy/sqrt(Sxx*Syy)
      if (2 %in% step) {
	cat("[Step 2] Estimate correlation coefficients ------------------------\n")
	cat(paste0("Sxx = ", round(sum(x^2), dig), " - (", round(sum(x), dig), ")\U00B2", "/ ", nn,
		" = ", round(Sxx, dig)), "\n")
	cat(paste0("Syy = ", round(sum(y^2), dig), " - (", round(sum(y), dig), ")\U00B2", "/ ", nn,
		" = ", round(Syy, dig)), "\n")
	cat(paste0("Sxy = ", round(sum(x*y), dig), " - (", round(sum(x), dig), " \U00D7 ", round(sum(y), dig),
		") / ", nn, " = ", round(Sxy, dig)), "\n")
	cat(paste0("Cor(xy) = ", round(Sxy, dig), " / \U221A(",
		round(Sxx, dig), " \U00D7 ", round(Syy, dig), ") = ", round(rxy, dig)), "\n")
      }
    # Correlation test by cor.test( ) function
	ct = cor.test(x, y, conf.level =1-alp)
	T0 = rxy*sqrt((nn-2)/(1-rxy^2))
	pv0 = 2*pt(-abs(T0), nn-2)
      if (3 %in% step) {
	cat("[Step 3] Correlation tests ------------------------\n")
	cat("Sample Correlation Coefficient =", round(ct$est, dig), "\n")
          if (r0==0) {
	cat(paste0("t-statistic = ", round(rxy, dig), " \U00D7 \U221A(", nn-2, "/(1-", round(rxy, dig), "\U00B2)) = ",
		round(ct$stat, dig), "\n"))
	cat(paste0("p-value = P(|T", nn-2, "| > ", round(abs(T0), dig), ") = ", round(pv0, dig), "\n"))
          } else {
	Z0 = sqrt(nn-3)*0.5*(log((1+rxy)/(1-rxy))-log((1+r0)/(1-r0)))
	pv0 = 2*pnorm(-abs(Z0))
	cat(paste0("Z-statistic = \U221A(", nn-3, ") \U00D7 0.5 \U00D7 (log((1+", round(rxy, dig), ")/(1-", round(rxy, dig),
		"))-log((1+", r0, ")/(1-", r0, "))) = ", round(Z0, dig), "\n"))
	cat(paste0("p-value = P(|Z| > ", round(abs(Z0), dig), ") = ", round(pv0, dig), "\n"))
          }
      }

    # Confidence intervals for correlation coefficients
      if (4 %in% step) {
	cat("[Step 4] Confidence intervals for correlation coefficients -------------------\n")
	cat(paste0(100*(1-alp), "% Confidence Interval = [",
		round(ct$conf[1], dig), ", ", round(ct$conf[2], dig), "]\n"))
      }
    # T-test(or z-test) plot
      if (5 %in% step) {
          if (r0==0) {
	cat("[Step 5] T-test plot ------------------------\n")
	win.graph(7, 5)
	# Utilize ttest.plot( ) function in chapter 11
	mr = max(c(4, abs(ct$stat*1.2)))
  	ttest.plot(ct$stat, ct$para, prng=c(-mr, mr), pvout=F)
          } else {
	cat("[Step 5] Z-test plot ------------------------\n")
	win.graph(7, 5)
	# Utilize normtest.plot( ) function in chapter 11
	mr = max(c(4, abs(Z0*1.2)))
  	normtest.plot(Z0, prng=c(-mr, mr), xlab="Z-statistic", pvout=F)
          }
      }
    # Display the simple regression line
      if (6 %in% step) {
	cat("[Step 6] Display the simple regression line ------------------------\n")
	win.graph(7, 5)
	plot(x, y, pch=19, main=mt, xlab=xl, ylab=yl, ylim=c(y1, y2))
	grid(col=3)
	abline(lm1, lty=2, col=2)
    	# Plot regression equation
	pos = ifelse(lm1$coef[[2]]>0, "bottomright", "upright")
	sign =  ifelse(lm1$coef[[2]]>0, "+", "")
	legend(pos, c("Regression Equation", paste0("Y = ", round(lm1$coef[[1]], dig), sign,
		round(lm1$coef[[2]], dig), " * X")), text.col=c(1,4), bg="white")
    	# Plot residuals
	segments(x, y, x, lm1$fit, lty=2, col=4)
	text(x, (y+lm1$fit)/2, labels="e", col=4, pos=4)
      }
    # Detailed output of regression equation
      if (any(7:11 %in% step)) {
	b1 = Sxy/Sxx
	xb = mean(x)
	yb = mean(y)
	b0 = yb - b1*xb
	sign =  ifelse(b1>0, "+", "")
      }
    # Estimate simple regression coefficients
      if (7 %in% step) {
	cat("[Step 7] Estimate simple regression coefficients ------------------\n")
	cat(paste0("Sxx = ", round(sum(x^2), dig), " - (", round(sum(x), dig), ")\U00B2", "/ ", nn,
		" = ", round(Sxx, dig)), "\n")
	cat(paste0("Sxy = ", round(sum(x*y), dig), " - (", round(sum(x), dig), " \U00D7 ", round(sum(y), dig),
		") / ", nn, " = ", round(Sxy, dig)), "\n")
	cat(paste0("Slope = ", round(Sxy, dig), " / ", round(Sxx, dig), " = ", round(b1, dig)), "\n")
	cat(paste0("Intersection = ", round(yb, dig), " - ", round(b1, dig), " \U00D7 ", round(xb, dig),
		" = ", round(b0, dig)), "\n")
	cat(paste0("Regression Equation : y = ", round(b0, dig), " ", sign, " ", round(abs(b1), dig), " x"), "\n")
      }
    # Analysis of variance
      if (any(8:11 %in% step)) {
	SST = Syy
	SSR = Sxy^2 / Sxx
	SSE = SST-SSR
	MSE = SSE/(nn-2)
	Rsq = SSR/SST
	tval = qt(1-alp/2, nn-2)
	an1 = anova(lm1)
	sm1 = summary(lm1)
	conf = 100*(1-alp)
      }
    # ANOVA table by calculating sum of squares
      if (8 %in% step) {
	cat("[Step 8] ANOVA table by calculating sum of squares -------------------\n")
	cat(paste0("SST = ", round(sum(y^2), dig), " - (", round(sum(y), dig), ")\U00B2", "/ ", nn,
		" = ", round(SST, dig)), "\n")
	cat(paste0("SSR = (", round(Sxy, dig), ")\U00B2", "/ ", round(Sxx, dig), " = ", round(SSR, dig)), "\n")
	cat(paste0("SSE = ", round(SST, dig), " - ", round(SSR, dig), " = ", round(SSE, dig)), "\n")
	# p-value
	pv1 = an1$Pr[1]
	if (pv1<0.001) {star ="***"
	} else if (pv1<0.01) {star ="**"
	} else if (pv1<0.05) star ="*"
	cat("--------------- ANOVA table  ---------------------\n")
	antab=cbind(an1$Sum, an1$Df, an1$Mean, an1$F, an1$Pr)
	antab=rbind(antab, c(sum(an1$Sum), sum(an1$Df), NA,NA,NA))
	rownames(antab)=c("Regress", "Residual", "Total")
	dum=cbind(round(antab[, 1:4], dig), round(antab[, 5], dig*2))
	colnames(dum)=c("Sum of Sq.","df","Mean Sq.","F-value","p-value")
	dum[is.na(dum)]=""
	print(as.data.frame(dum))

	cat(paste0("F-test critical value = ", round(qf(1-alp, 1, nn-2), dig)), "\n")
	cat(paste0("R-square = ", round(SSR, dig), " / ", round(SST, dig), " = ", round(Rsq, dig)), "\n")
      }
    # Confidence intervals & significance tests for regression coefficients
      if (9 %in% step) {
	se1 = sqrt(MSE/Sxx)
	se0 = sqrt(MSE*(1/nn+xb^2/Sxx))
	tol1 = tval*se1
	tol0 = tval*se0
	tstat1 = b1/se1
	tstat0 = b0/se0
	pv1 = 2*pt(-abs(tstat1), nn-2)
	pv0 = 2*pt(-abs(tstat0), nn-2)
	cat("[Step 9] Confidence intervals & significance tests for regression coefficients --------\n")
	cat(paste0(conf, "%CI(b1) = [", round(b1, dig), " \U00B1 ", round(tval, dig), " \U00D7 ", round(se1, dig),
		"] = [", round(b1, dig), " \U00B1 ", round(tol1, dig), "] = [",
		round(b1-tol1,dig), ", ", round(b1+tol1,dig), "]\n"))
	cat(paste0(conf, "%CI(b0) = [", round(b0, dig), " \U00B1 ", round(tval, dig), " \U00D7 ", round(se0, dig),
		"] = [", round(b0, dig), " \U00B1 ", round(tol0, dig), "] = [",
		round(b0-tol0,dig), ", ", round(b0+tol0,dig), "]\n"))
	cat("    Significance tests for regression coefficient ------------------------\n")
	cat(paste0("T1 = ", round(b1, dig), " / ", round(se1, dig), " = ", round(tstat1, dig),
		"  \t P-val = P(|T", nn-2, "| > ", round(abs(tstat1), dig), ") = ", round(pv1, dig)), "\n")
	cat(paste0("T0 = ", round(b0, dig), " / ", round(se0, dig), " = ", round(tstat0, dig),
		"\t P-val = P(|T", nn-2, "| > ", round(abs(tstat0), dig), ") = ", round(pv0, dig)), "\n")
      }
    # Confidence intervals and prediction intervals at x0
      if (any(10:11 %in% step)) {
	Ex0 = b0+b1*x0
	vcx0 = MSE*(1/nn+(x0-xb)^2/Sxx)
	vpx0 = MSE*(1+1/nn+(x0-xb)^2/Sxx)
	cse = sqrt(vcx0)
	pse = sqrt(vpx0)
	ctol = tval*cse
	ptol = tval*pse
      }
      if (10 %in% step) {
	cat("[Step 10-1] Confidence interval for E[Y|x0] -------------\n")
	cat(paste0(conf, "%CI E(Y|", x0, ") = [", round(Ex0, dig), " \U00B1 ", round(tval, dig), " \U00D7 ", round(cse, dig),
		"] = [", round(Ex0, dig), " \U00B1 ", round(ctol, dig), "] = [",
		round(Ex0-ctol,dig), ", ", round(Ex0+ctol,dig), "]\n"))
	cat("[Step 10-2] Prediction interval for Y|x0 -------------------\n")
	cat(paste0(conf, "%PI (Y|", x0, ") = [", round(Ex0, dig), " \U00B1 ", round(tval, dig), " \U00D7 ", round(pse, dig),
		"] = [", round(Ex0, dig), " \U00B1 ", round(ptol, dig), "] = [",
		round(Ex0-ptol,dig), ", ", round(Ex0+ptol,dig), "]\n"))
      }
    # Plot confidence bands and prediction bands
      if (11 %in% step) {
	cat("[Step 11] Plot confidence bands and prediction bands ------------------------\n")
	x1 = min(x0, x)
	x2 = max(x0, x)
	if (missing(xrng)) xrng = c(x1-0.1*(x2-x1), x2+0.1*(x2-x1))
	if (missing(by)) by = (x2-x1)/50
    	# Set data frame
	nd = data.frame(x=seq(xrng[1], xrng[2], by=by))
    	# Confidence intervals and prediction intervals
	conf2 = predict(lm1, interval="confidence", newdata=nd)
	pred2 = predict(lm1, interval="prediction", newdata=nd)
	y1 = min(pred2[, "lwr"])
	y2 = max(pred2[, "upr"])
	ymin = y1 - (y2-y1)*0.1
	ymax = y2 + (y2-y1)*0.1
   	# Plot confidence bands and prediction bands
	win.graph(7, 6)
	plot(x, y, pch=19, main=paste("Confidence and Prediction Bands of", yl, "given", xl),
		xlab=xl, ylab=yl, ylim=c(ymin, ymax), xlim=xrng)
	# Regression line, confidence band, and prediction band
	abline(lm1, col=4)
	abline(v=xb, lty=2, col=3)
	text(xb, ymin, labels=expression(bar(x)), pos=4)
	matlines(nd$x, conf2[,c("lwr","upr")], col=2, type="p", pch="+")
	matlines(nd$x, pred2[,c("lwr","upr")], col=4, type="p", pch=1)
	abline(v=x0, lty=2, col="orange")
	text(x0, ymin, labels=expression(x[0]), cex=0.9, col="green4", pos=4)
	text(x0, Ex0+c(-ptol, 0, ptol), labels=format(Ex0+c(-ptol, 0, ptol), digit=dig),
		cex=0.8, col="green4", pos=c(1,1,3))
      }
    # Diagnosis of the regression model
      if (12 %in% step) {
	cat("[Step 12] Diagnosis of the regression model ------------------------\n")
	win.graph(7, 5)
	par(mfrow=c(2,2))
	plot(lm1)
      }

}

# [14-23] Multiple Regression Analysis
# Scatter plot matrix with correlation coefficients
panel.cor = function(x, y, alp=0.05, digits = 4, prefix = "", cex.cor, ...)
{
    usr = par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    cxy = cor(x, y)
    r = abs(cxy)
    txt = format(c(r, 0.123456789), digits = digits)[1]
    txt = paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor = 0.5/strwidth(txt)
    text(0.5, 0.6, txt, col=ifelse(cxy>0, 2, 4), cex = cex.cor)
    pv = cor.test(x, y, conf.level =1-alp)$p.val
    text(0.5, 0.4, paste("P-v =", round(pv, digits)), cex = 1.7, col=4)
}

# Multiple Regression Analysis
#' @title Multiple Regression Analysis
#' @description Multiple Regression Analysis
#' @param xd Data frame of independent variables (explanatory variables)
#' @param y Vector of dependent variable (response variable) data
#' @param form Formula of regression model (ex: y ~ x1 + x2)
#' @param xd2 Data frame of  independent variables in model2 (step=4, 5)
#' @param form2 Formula of regression model2 (ex: y ~ x1 * x2) (step=4, 5)
#' @param step Steps of multiple regression analysis, Default: 0:4
#' @param newd Data frame of independent variables for step 6
#' @param pvx Designated number of independent variables for step 6(step=7)
#' @param xrng Range of independent variables for step 7
#' @param nx  Designated number of independent variables for step 7
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#'
#' @examples
#' mpg = mtcars$mpg
#' xd = mtcars[4:6]
#' attach(xd)
#' form = mpg ~ hp + drat + wt
#' corr.mreg(xd, mpg, form, step=0:3)
#'
#' form2 = mpg ~ hp * drat + wt
#' xd2 = data.frame(hp, drat, wt, hpd= hp * drat)
#' corr.mreg(xd, mpg, form, xd2, form2, step=4:5)
#' @rdname corr.mreg
#' @export
corr.mreg = function(xd, y, form, xd2, form2, step=0:4, newd, pvx, xrng, nx, alp=0.05, dig=4) {
    # Set inputs
	ke = length(xd)
	kk = ke+1
	xl = names(xd)
	yl = deparse(substitute(y))
	model1 = deparse(substitute(form))
	if (!missing(form2)) model2 = deparse(substitute(form2))
	nn = length(y)
     # Prior investigation of data -----------------------------------------
      if (0 %in% step) {
	cat("[Step 0] Scatter matrix plot for prior investigation of data --------------------\n")
	win.graph(7, 5)
	xyd = as.data.frame(cbind(xd, y))
	names(xyd) = c(xl, yl)
	pairs( xyd, lower.panel=panel.cor,
		upper.panel=function(x, y){
		points(x, y)
		abline(lm(y~x), col='red') })
      }
    # Regression analysis
	conf = 100*(1-alp)
	lm1 = lm(form)
	an1 = anova(lm1)
	sm1 = summary(lm1)
    # Estimate multiple regression coefficients
	X = as.matrix(xd)
	const = rep(1, nn)
	X = cbind(const, X)
	XX = t(X) %*% X
	Xy = t(X) %*% y
	colnames(Xy)=yl
	XXI = solve(XX)
	bh = XXI %*% Xy
      if (1 %in% step) {
	cat("[Step 1] Estimate multiple regression coefficients ------------------------\n")
	cat("X'X matrix ----------\n")
	print(XX)
	cat("Equation : b = inv(X'X) (X'y) ----------\n")
	for (k in 1:kk) cat(paste("b",k-1), "\t", round(XXI[k, ],6), "\t", Xy[k], "\t", round(bh[k], dig), "\n")
      }
    # Analysis of variance
	CT = sum(y)^2 / nn
	SST = sum(y^2) - CT
	SSR = t(bh) %*% Xy -CT
	SSE = SST-SSR
	dfe = nn-kk
	MSR = SSR/ke
	MSE = SSE/dfe
	F0 = MSR/MSE
	pv0 = 1-pf(F0, ke, dfe)
	Rsq = SSR/SST
	aRsq = 1-SSE/SST*(nn-1)/(nn-kk)
	tval = qt(1-alp/2, dfe)
    # ANOVA table by calculating sum of squares
      if (2 %in% step) {
	cat("[Step 2] ANOVA table by calculating sum of squares ----------------------\n")
	cat(paste0("SST = ", round(sum(y^2), dig), " - (", round(sum(y), dig), ")\U00B2", "/ ", nn,
		" = ", round(SST, dig)), "\n")
	cat(paste0("SSR = (", paste(round(bh, dig), collapse=" "), ").(", paste(round(Xy, dig), collapse=" "),
		") = ", round(SSR, dig)), "\n")
	cat(paste0("SSE = ", round(SST, dig), " - ", round(SSR, dig), " = ", round(SSE, dig)), "\n")

	cat("------ Analysis of Variance Table------------------------\n")
	antab=cbind(an1$Sum[1:ke], an1$Df[1:ke], an1$Mean[1:ke], an1$F[1:ke], an1$Pr[1:ke])
	antab=rbind(antab, c(SSR, ke, MSR, F0, pv0))
	antab=rbind(antab, c(an1$Sum[kk], an1$Df[kk], an1$Mean[kk], an1$F[kk], an1$Pr[kk]))
	antab=rbind(antab, c(sum(an1$Sum), sum(an1$Df), NA, NA, NA))
	rownames(antab)=c(xl, "Regression", "Residual", "Total")
	dum=cbind(round(antab[, 1:4], dig), round(antab[, 5], dig*2))
	colnames(dum)=c("Sum of Sq.","df","Mean Sq.","F-value","p-value")
	dum[is.na(dum)]=""
	print(as.data.frame(dum))

	cat(paste0("F-test critical value = ", round(qf(1-alp, ke, dfe), dig)), "\n")
	cat(paste0("R-square = ", round(SSR, dig), " / ", round(SST, dig), " = ", round(Rsq, dig)), "\n")
	cat(paste0("Adj R-sq = 1 - (", round(SSE, dig), " / ", nn-kk, ") / (",
		round(SST, dig), " / ", nn-1, ") = ", round(aRsq, dig)), "\n")
      }
    # Confidence intervals & significance tests for regression coefficients
      if (3 %in% step) {
	dXXI = diag(XXI)
	se = sqrt(MSE[1,1] * dXXI)
	tstat = bh/se
	tol = tval*se
	lcl = bh - tol
	ucl = bh + tol
	pv = 2*pt(-abs(tstat), dfe)
	# Print output
	cat("[Step 3-1] Confidence intervals for regression coefficients --------------------\n")
	citab = cbind(bh, tol, lcl, ucl)
	colnames(citab)=c("Estimate", "Tolerance", "Lower limit", "Upper limit")
	print(round(citab, dig))
	cat("[Step 3-2] Significance tests for regression coefficients ------------------------\n")
	tstab = cbind(bh, se, tstat, pv)
	dum = cbind(round(tstab[, 1:3], dig), round(tstab[, 4], 2*dig))
	colnames(dum) = c("Estimate", "Std Error", "T-stat", "P-value")
	print(dum)
      }
    # Analysis of model 2
      if (any(4:5 %in% step)) {
	ke2 = length(xd2)
	kk2 = ke2+1
	xl2 = names(xd2)
	lm2 = lm(form2)
	an2 = anova(lm2)
	sm2 = summary(lm2)
      }
      if (4 %in% step) {
	cat("[Step 4] Analysis of model 2 (redo step 0~3) ------------------------\n")
	cat("[Step 4-0] Scatter plot matrix for model 2 -------------------\n")
	xl2 = names(xd2)
	xd2y = as.data.frame(cbind(xd2, y))
	names(xd2y) = c(xl2, yl)
	win.graph(7, 5)
	pairs(xd2y, lower.panel=panel.cor,
		upper.panel=function(x, y){
		points(x, y)
		abline(lm(y~x), col='red') })
	cat("[Step 4-1] Regression equation for model 2 ------------------------\n")
	# Estimate regression coefficients
	X2 = as.matrix(xd2)
	const = rep(1, nn)
	X2 = cbind(const, X2)
	XX2 = t(X2) %*% X2
	cat("X2'X2 matrix ----------\n")
	print(XX2)
	X2y = t(X2) %*% y
	colnames(X2y)=yl
	XX2I = solve(XX2)
	bh2 = XX2I %*% X2y
	cat("Equation : b = inv(X2'X2) (X2'y) ------------\n")
	for (k in 1:kk2) cat(paste0("b", k-1), "\t ", round(XX2I[k, ], 6), "\t ", X2y[k], "\t ",
			round(bh2[k], dig), "\n")
        # Analysis of variance
	SSR2 = t(bh2) %*% X2y -CT
	SSE2 = SST-SSR2
	dfe2 = nn-kk2
	MSR2 = SSR2/ke2
	MSE2 = SSE2/dfe2
	F0 = MSR2/MSE2
	pv0 = 1-pf(F0, ke2, dfe2)
	Rsq2 = SSR2/SST
	aRsq2 = 1-SSE2/SST*(nn-1)/(nn-kk2)
	tval2 = qt(1-alp/2, dfe2)
	cat("[Step 4-2] ANOVA table of model 2 ------------------------\n")
	cat(paste0("SST = ", round(sum(y^2), dig), "-(", round(sum(y), dig), ")\U00B2 /", nn,
		" = ", round(SST, dig)), "\n")
	cat("SSR = (", round(bh2, dig), ").(", round(X2y, dig), ") = ", round(SSR2, dig), "\n")
	cat(paste0("SSE = ", round(SST, dig), " - ", round(SSR2, dig), " = ", round(SSE2, dig)), "\n")

	cat("------- ANOVA table ------------------------\n")
	antab2=cbind(an2$Sum[1:ke2], an2$Df[1:ke2], an2$Mean[1:ke2], an2$F[1:ke2], an2$Pr[1:ke2])
	antab2=rbind(antab2, c(SSR2, ke2, MSR2, F0, pv0))
	antab2=rbind(antab2, c(an2$Sum[kk2], an2$Df[kk2], an2$Mean[kk2], an2$F[kk2], an2$Pr[kk2]))
	antab2=rbind(antab2, c(sum(an2$Sum), sum(an2$Df), NA, NA, NA))
	colnames(antab2)=c("Sum of Sq.","df","Mean Sq.","F-value","p-value")
	rownames(antab2)=c(xl2, "Regression", "Residual", "Total")
	dum=round(antab2, dig)
	dum[is.na(dum)]=""
	print(as.data.frame(dum))

	cat(paste0("F-test critical value = ", round(qf(1-alp, ke2, dfe2), dig)), "\n")
	cat(paste0("R-square = ", round(SSR2, dig), "/", round(SST, dig), " = ", round(Rsq2, dig)), "\n")
	cat(paste0("Adj R-sq = 1-(", round(SSE2, dig), "/", nn-kk2, ")/(",
		round(SST, dig), "/", nn-1, ") = ", round(aRsq2, dig)), "\n")
       # Confidence intervals & significance tests for regression coefficients
	dXX2I = diag(XX2I)
	se2 = sqrt(MSE2[1,1] * dXX2I)
	tstat2 = bh2/se2
	tol2 = tval2*se2
	lcl2 = bh2 - tol2
	ucl2 = bh2 + tol2
	pv2 = 2*pt(-abs(tstat2), dfe2)
	cat("[Step 4-3-1] Confidence intervals for regression coefficients --------\n")
	citab2 = cbind(bh2, tol2, lcl2, ucl2)
	colnames(citab2)=c("Estimate", "Tolerance", "Lower limit", "Upper limit")
	print(round(citab2, dig))
	cat("[Step 4-3-2] Significance tests for regression coefficients-----------------\n")
	tstab2 = cbind(bh2, se2, tstat2, pv2)
	colnames(tstab2)=c("Estimate", "Std Error", "T-stat", "P-value")
	print(round(tstab2, dig))
      }
    # Compare and diagnose regression models
      if (5 %in% step) {
	cat("[Step 5-1] Compare regression models (analysis of variance) -------------\n")
	an12 = anova(lm1, lm2)
	print(an12)
	cat("[Step 5-2] Diagnose regression model 1 -------------\n")
	win.graph(7, 6)
	par(mfrow=c(2,2))
	plot(lm1)
	cat("[Step 5-3] Diagnose regression model 2  -------------\n")
	win.graph(7, 6)
	par(mfrow=c(2,2))
	plot(lm2)
      }
    # Confidence intervals and prediction intervals at x=newd
      if (6 %in% step) {
	cat("[Step 6] Confidence intervals and prediction intervals at x=newd --------------\n")
	print(newd)
	cat(paste0(conf, "% Confidence intervals --------------\n"))
	print(round(predict(lm1, newd, interval="confidence"), dig))
	cat(paste0(conf, "% Prediction intervals --------------\n"))
	print(round(predict(lm1, newd, interval="prediction"), dig))
      }
    # Plot confidence bands and prediction bands
      if (7 %in% step) {
	cat("[Step 7] Plot confidence bands and prediction bands --------------\n")
	xb = as.numeric(apply(xd, 2, mean))
    	# Set plot range
	if (missing(xrng)) {
		xmin = min(xd[[pvx]])
		xmax = max(xd[[pvx]])
		xspan = xmax-xmin
		xrng = c(xmin, xmax)+xspan*c(-0.1, 0.1)
	}
	avx = setdiff(1:ke, pvx)
	ndat = as.data.frame(matrix(NA, nx, ke))
	ndat[[pvx]] = seq(xrng[1], xrng[2], length=nx)
	for (k in avx) ndat[[k]] = rep(xb[k], nx)
	names(ndat) = xl
	# Confidence intervals and prediction intervals
	conf1 = predict(lm1, interval="confidence", newdata=ndat)
	pred1 = predict(lm1, interval="prediction", newdata=ndat)
	# Plot confidence band and prediction band
	y1 = min(pred1[, "lwr"])
	y2 = max(pred1[, "upr"])
	ymin = y1 - (y2-y1)*0.1
	ymax = y2 + (y2-y1)*0.1
	win.graph(7, 6)
	plot(xd[[pvx]], y, pch=19, cex=1.2, main=paste("Confidence and Prediction Bands of ",
		yl, "given", xl[pvx]), xlab=xl[pvx], ylab=yl, xlim=xrng, ylim=c(ymin, ymax))
	lines(ndat[[pvx]], conf1[,1], lty=2, col="purple")
	abline(v=xb[pvx], lty=2, col=3)
	text(xb[pvx], ymin, labels=expression(bar(x)), pos=4)
	matlines(ndat[[pvx]], conf1[,c("lwr","upr")],col=2, lty=1,type="b",pch="+")
	matlines(ndat[[pvx]], pred1[,c("lwr","upr")],col=4, lty=2,type="b",pch=1)
	text(xrng[2], conf1[nx,], labels=format(conf1[nx,], digit=4), pos=c(1,1,3))
      }
}
