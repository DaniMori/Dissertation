######################################################################
###################################################################### 
# Example of code analyzing the Quality of life dataset (Chen, West & Souza, 2006) 
# by using Schmid-Leiman, target Schmid-Leiman and iterative Schmid-Leiman
# Supporting functions for computing the analysis in "SLi.R".
# 
# Abad, F.J., Garcia-Garzon, E., Garrido, L.E. & Barrada, J.R. (in press). 
# Iteration of Partially Specified Target Matrices: Application to the bi-factor case.
# Multivariate Behavioral Research.
#
# Functions based on psych and GPArotation packages:
#
# Revelle, W. (2016) psych: Procedures for Personality and Psychological Research, 
# Northwestern University,Evanston, Illinois, USA, 
# https://CRAN.R-project.org/package=psych Version = 1.6.9.
#
#
# Bernaards, Coen A. and Jennrich, Robert I. (2005) Gradient Projection Algorithms 
# and Software forArbitraryRotation Criteria in Factor Analysis, 
# Educational and Psychological Measurement: 65, 676-696.
# <http://www.stat.ucla.edu/research/gpa>
######################################################################
######################################################################

######################################################################
# Functions for iterative target factor rotation
######################################################################

######################################################################
perform_rotation <- function(L, method = NULL, targ = NULL, reps = 10) {
# Compute factor rotation for geomin and target rotations
#
# Args:
#
#   L: Unrotated factor solution
#   method: Factor rotation method, as in GPArotation:
#           geominQ: geomin orthogonal
#           targetT: target
#           Default is NULL
#   Targ: Target matrix required for partially specified target rotation. 
#         Default is NULL.
#   reps: Number of random starts. Default is 10.
#
# Returns:
#
#   Loadings: Rotated factor loading matrix
#   Phi     : Factor correlation matrix (only when geominQ is specified)
#   Rotating matrix: Rotation matrix
#  
# Error handling  
  if (is.null(method)) {
      stop("A rotation method must be specified")
  }
  
# Compute the factor rotation using GPArotation package  
  results   <- rep( list(list()), reps) 
  criterion <- rep(NA,reps)
  
  for (i in 1:reps) {
       if (method == "geominQ") {
       x <- geominQ(L,Tmat = Random.Start(ncol(L)), maxit=5000)
    }  else if (method == "targetT") {
       x <- targetT (L, Tmat = Random.Start(ncol(L)), maxit=5000, Target = targ)
    } 

# Selecting the best random start
    if (x$convergence == TRUE) {
        criterion[i] <- min(x$Table[,2])
        results[[i]] <- x
    } else { 
        criterion[i] <- NA
        results[[i]] <- NA
        cat ("Convergence problem in factor rotation for random start ",i, "\n")
    }
  }

#return the best random start solution  

  j <- order(criterion)[1]
  return(results[[j]])
}
######################################################################

######################################################################
get_target_from <- function (L, cutpoint = NULL) {
# Computes the target matrix 
#
# Args:
#
#   L: Factor loading matrix
#   cutpoint: Cut-off for factor loading substantivity.
#
# Returns:
#
#   Partially specified target matrix
#   
  targ<-L
  if (is.null(cutpoint)){
      stop("A cut-off point must be defined")
  }   else {
      targ[abs(targ) >  cutpoint] <- NA
      targ[abs(targ) <= cutpoint] <- 0
  }
# The first factor is the general factor:
  targ[,1] <- NA
  return(targ)
}
######################################################################

target_convergence_check <- function (Targetprev, Targetnew) {
  
# Convergence check for the SLi rotation
#
# Args:
#
#   Targetprev: Target matrix from previous iteration
#   Targetnew : Target matrix from actual iteration
#
# Returns:
#
#   converg: If TRUE, convergence is achieved
#            If FALSE, convergence is not achieved
    
    Targetprev[is.na(Targetprev)] <- 1
    Targetnew [is.na(Targetnew) ] <- 1
  
    if (sum((Targetnew - Targetprev)^2) == 0) {
         converg    <- TRUE
    }    else {
         converg    <- FALSE
    }
    return(converg)
}
  
######################################################################
SLi <- function (matrix = NULL,
                 specific_factors = NULL,
                 fm = "minres",
                 cutpoint=NULL,
                 iterations = 20) {
# Compute the iterative target factor rotation
#
# Args:
#
#   matrix: Correlation or Covariance matrix to be analyzed. Default is NULL.
#   specific_factors: number of specific factors to be extracted. Default is NULL.
#   fm: factor estimation method. Default is MINRES. Other alternatives can 
#       be found in the fa() function documentation (psych) package.
#   cutpoint: Value for cut-off point criterion (e.g., .20). Default is .20
#   iterations: iterations number. Default is 20. If 0, Schmid-Leiman with target rotation (without iterations) is performed.
#  
# Returns:
#
#   Loadings: Rotated factor loading matrix
#  
# Error handling:  
  if (is.null(matrix)){
    stop("A correlation or covariance matrix must be specified")
  }
  
  if (is.null(specific_factors)){
    stop("A number of factors must be specified")
  }

  if (is.null(cutpoint)){
      stop("A cut-off point must be specified")
  }
  
## Step 1: First order factor analysis with Geomin oblique rotation

  lp  <- fa (r = matrix, 
             nfactors = specific_factors, 
             rotate = "none", 
             fm = fm)$loadings

  lp_rotated     <- perform_rotation (lp, method = "geominQ")
  convergence_lp <- lp_rotated$convergence
  
# Convergence check:    
  if (convergence_lp == FALSE) {
    stop("Convergence problems when estimating first order solution for SL solution")
  }

## Step 2: Second order factor analysis
  lp1            <- fa (lp_rotated$Phi, nfactors=1, fm = "minres")
  
## Step 3: Schmid-Leiman transformation
  lpSL1          <- lp_rotated$loadings %*% lp1$loadings
  psl1           <- matrix (0, dim(lp1$loadings), dim(lp1$loadings))
  diag(psl1)     <- sqrt(1- lp1$loadings^2)
  lpsl2          <- lp_rotated$loadings %*% psl1
  SL_loadings    <- cbind (lpSL1,lpsl2)
  
  #round(SL_loadings,2)
## Step 4: Calculate an unrotated solution with specific + 1 factors  

  unrotated_l     <- fa (r = matrix, 
                         nfactors = (specific_factors+1), 
                         rotate ="none", 
                         fm = fm)$loadings
  
# Step 4: Schmid-Leiman iterated target rotation (SLi)

  targF           <- get_target_from (SL_loadings, cutpoint = cutpoint)
  SLt_result      <- perform_rotation (L = unrotated_l, 
                                       method = "targetT",
                                       targ = targF)
  
  # Convergence check
  if ( SLt_result$convergence == FALSE) {
    stop("Convergence problems when estimating SL target rotation")
  }
  # SLt rotation factor loadings
  loadings_targ_prev <- SLt_result$loadings
  prev_target        <- targF

# Step 5: Schmid-Leiman iterated target rotation (SLi)

  if (iterations == 0) {
    loadings_targ_new  <- SLt_result$loadings
  } else {
    for (it in 1:iterations) {
    # Target matrix is updated iteratively until convergence
    
    new_target          <- get_target_from  (loadings_targ_prev, cutpoint = cutpoint)
    new_SLi_result      <- perform_rotation (loadings_targ_prev, 
                                             method = "targetT", 
                                             targ = new_target)
    
    if ( new_SLi_result$convergence == FALSE) {
      stop("Convergence problems when estimating SLi target rotation")
    }
    
    loadings_targ_new   <- new_SLi_result$loadings

    # Check criteria for ending the iterative procedure
    converg <- target_convergence_check(prev_target, new_target)
    if (converg == TRUE) {
        cat("Convergence achieved in", it, "iterations \n")
        break()
    }   else {
        loadings_targ_prev   <- loadings_targ_new
        prev_target          <- new_target
    }
}
# Convergence check
    if (converg == FALSE) {
        cat("Convergence has not obtained for the Target matrix. Please increase the number of iterations.")
    }
  }
# Return rotated factor matrix
  return(list(model = unrotated_l, loadings = loadings_targ_new))
}  
######################################################################


SLi.rotation <- function (loadings, target, specific_factors = 6, cutpoint = .2, iterations = 20) {
				 
# Step 4: Schmid-Leiman iterated target rotation (SLi)

  SLt_result      <- perform_rotation (L = loadings, method = "targetT", targ = target)
  
  # Convergence check
  if ( SLt_result$convergence == FALSE) {
    stop("Convergence problems when estimating SL target rotation")
  }
  # SLt rotation factor loadings
  loadings_targ_prev <- SLt_result$loadings
  prev_target        <- target

# Step 5: Schmid-Leiman iterated target rotation (SLi)

  if (iterations == 0) {
    loadings_targ_new  <- SLt_result$loadings
  } else {
    for (it in 1:iterations) {
    # Target matrix is updated iteratively until convergence
    
    new_target          <- get_target_from  (loadings_targ_prev, cutpoint = cutpoint)
    new_SLi_result      <- perform_rotation (loadings_targ_prev, 
                                             method = "targetT", 
                                             targ = new_target)
    
    if ( new_SLi_result$convergence == FALSE) {
      stop("Convergence problems when estimating SLi target rotation")
    }
    
    loadings_targ_new   <- new_SLi_result$loadings

    # Check criteria for ending the iterative procedure
    converg <- target_convergence_check(prev_target, new_target)
    if (converg == TRUE) {
        cat("Convergence achieved in", it, "iterations \n")
        break()
    }   else {
        loadings_targ_prev   <- loadings_targ_new
        prev_target          <- new_target
    }
}
# Convergence check
    if (converg == FALSE) {
        cat("Convergence has not obtained for the Target matrix. Please increase the number of iterations.")
    }
  }
# Return rotated factor matrix
  return(loadings = loadings_targ_new)
}  
