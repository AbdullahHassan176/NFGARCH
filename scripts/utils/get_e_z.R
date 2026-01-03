# E|z| Calculation Utilities
# Theoretical expectation of |z| for different distributions

#' Calculate theoretical E|z| for Normal distribution
#' 
#' For a standard normal random variable z ~ N(0,1),
#' E|z| = sqrt(2/pi) ≈ 0.798
#' 
#' @return Theoretical E|z| for N(0,1)
#' @export
get_e_z_normal <- function() {
  sqrt(2/pi)
}

#' Calculate theoretical E|z| for Student-t distribution
#' 
#' For a standardized Student-t random variable z ~ t(ν),
#' E|z| = sqrt(ν/π) · Γ((ν-1)/2) / Γ(ν/2)
#' 
#' @param nu Degrees of freedom (must be > 2)
#' @return Theoretical E|z| for t(ν)
#' @export
get_e_z_student_t <- function(nu) {
  if (nu <= 2) {
    stop("Degrees of freedom must be greater than 2 for finite variance")
  }
  sqrt(nu/pi) * gamma((nu-1)/2) / gamma(nu/2)
}

#' Get E|z| based on distribution type
#' 
#' @param distribution Distribution name ("norm", "std", "sstd")
#' @param nu Degrees of freedom (required for Student-t distributions)
#' @return Theoretical E|z| value
#' @export
get_e_z <- function(distribution, nu = NULL) {
  if (distribution == "norm" || distribution == "normal") {
    return(get_e_z_normal())
  } else if (distribution == "std" || distribution == "sstd" || distribution == "student") {
    if (is.null(nu)) {
      stop("Degrees of freedom (nu) required for Student-t distribution")
    }
    return(get_e_z_student_t(nu))
  } else {
    stop("Unsupported distribution: ", distribution)
  }
}




