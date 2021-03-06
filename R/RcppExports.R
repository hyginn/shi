# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#'
#' Split a string into codons
#' @param cDNA - a string
#' @return a vector of strings of length 3
#' @export
#'
cpp_codonSplit1 <- function(cDNA) {
    .Call('_shi_cpp_codonSplit1', PACKAGE = 'shi', cDNA)
}

#'
#' Split a string into codons
#' @param cDNA - a string
#' @return a vector of strings of length 3
#' @export
#'
cpp_codonSplit2 <- function(cDNA) {
    .Call('_shi_cpp_codonSplit2', PACKAGE = 'shi', cDNA)
}

#'
#' Split a string into codons
#' @param cDNA - a string
#' @return a vector of strings of length 3
#' @export
#'
cpp_codonSplit3 <- function(cDNA) {
    .Call('_shi_cpp_codonSplit3', PACKAGE = 'shi', cDNA)
}

hello <- function() {
    invisible(.Call('_shi_hello', PACKAGE = 'shi'))
}

