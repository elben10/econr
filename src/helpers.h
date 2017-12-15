#ifndef dplyr_helpers_H
#define dplyr_helpers_H

#include <RcppArmadillo.h>

arma::mat inv(arma::mat X) {
  return arma::solve(X, arma::eye(arma::size(X)));
}

Rcpp::NumericVector colvec_to_NumVec(const arma::colvec& x, bool names_lgl = false, Rcpp::CharacterVector names = "") {
  Rcpp::NumericVector res = Rcpp::NumericVector(x.begin(), x.end());
  if (names_lgl) {
    res.names() = names;
  }

  return res;
}

#endif
