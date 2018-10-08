########################################################################################################################
#
# Bivariate lognormal tools
# 
# Author:	Mori
# Date:		18-03-2014
#
########################################################################################################################


########## INCLUDES
library(mvtnorm)

# source("R/Math_toolbox.R")


########## FUNCTIONS

# FUNCTION: rbvlnorm
#
# DESCRIPTION: generates random values from a bivariate lognormal distribution.
#
# PARAMETERS:
#	* n (numeric): number of values to generate.
#	* location (vector): 2-length numeric vector with the location parameter values.
#	* scale (matrix): 2x2 non-negative definite numeric matrix with the scale parameter values.
#
# RETURNS: numeric matrix with n rows and 2 columns, where each row is a random bivariate sample from the bivariate
#	lognormal distribution with "location" and "scale" parameters.
rbvlnorm <- function(n, location, scale) {
	
	# Parameter parsing
	if(!is.numeric(n) | !is.vector(n))
		stop("'n' must be a numeric vector.")
	if(!is.numeric(location) | !is.vector(location) | length(location) != 2)
		stop("'location' must be a length-2 numeric vector.")
	if(!is.numeric(scale) | !is.matrix(scale) | !all.equal(dim(scale), c(2, 2)))
		stop("'scale' must be a 2x2 numeric matrix.")
	
	return(exp(rmvnorm(n, location, scale)))
}

# FUNCTION: bvlnorm.scale
#
# DESCRIPTION: obtains the off diagonal value of the scale parameter from the diagonal scale parameters and a desired
#	correlation for the distribution.
#
# PARAMETERS:
#	* diagonal (vector): 2-length numeric vector with the diagonal scale parameter values.
#	* corr (numeric): numeric value with the desired correlation of the distribution.
#
# RETURNS: numeric 2x2 matrix with the scale parameter of the distribution.
bvlnorm.scale <- function(diagonal, corr){
	
	# Parameter parsing
	if(!is.numeric(diagonal) | !is.vector(diagonal) | length(diagonal) != 2)
		stop("'diagonal' must be a length-2 numeric vector.")
	if(!is.numeric(corr) | !is.vector(corr))
		stop("'corr' must be a numeric vector.")
	
	return(
		matrix(
			c(
				diagonal[1],
				rep(log(sqrt(exp(sum(diagonal)) - exp(diagonal[1]) - exp(diagonal[2]) + 1) * corr + 1), 2),
				diagonal[2]
			),
			nrow = 2, ncol = 2
		)
	)
}


# FUNCTION: bvlnorm.params
#
# DESCRIPTION: obtains the mean, variance and correlation of a bivariate lognormal distribution.
#
# PARAMETERS:
#	* location (vector): 2-length numeric vector with the location parameter values.
#	* scale (matrix): 2x2 non-negative definite numeric matrix with the scale parameter values.
#
# RETURNS: list with 3 elements:
#	*mean: 2-length numeric vector with the mean values of the distribution.
#	*var.cov: 2x2 numeric matrix with the variance-covariance matrix of the distribution.
#	*corr: 2x2 numeric matrix with the correlation matrix of the distribution.
bvlnorm.params <- function(location, scale) {
	
	mean <- exp(location + .5 * diag(scale))
	
	var.cov <- matrix(
		c(
			exp(location[1] * 2 + scale[1, 1]) * (exp(scale [1, 1]) - 1),
			rep(exp(sum(location) + sum(diag(scale)) / 2) * (exp(scale[1, 2]) - 1), 2),
			exp(location[2] * 2 + scale[2, 2]) * (exp(scale [2, 2]) - 1)
		),
		2
	)
	
	corr <- cov2cor(var.cov)
	
	return(list(mean = mean, var.cov = var.cov, corr = corr))
}

