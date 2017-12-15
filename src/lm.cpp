// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "helpers.h"
using namespace Rcpp;
using namespace arma;



// [[Rcpp::export]]
List lm_rcpp(arma::mat X, arma::vec y, CharacterVector X_names) {
  int n = X.n_rows, k = X.n_cols;

  arma::colvec coef = arma::solve(X.t() * X, X.t() * y);
  arma::colvec resid = y - X * coef;
  arma::colvec fitted = X * coef;

  double resid_var =  arma::as_scalar(resid.t() * resid) / (n - k);
  arma::mat coef_var = inv(X.t() * X) * resid_var;


  // RETURN VARIABLES

  NumericVector res_coef = colvec_to_NumVec(coef, true, X_names);
  NumericVector res_resid = colvec_to_NumVec(resid);
  NumericVector res_fitted = colvec_to_NumVec(fitted);
  double res_resid_err = sqrt(resid_var);
  NumericVector res_SE = colvec_to_NumVec(sqrt(coef_var.diag()), true, X_names);
  NumericVector res_t_values = colvec_to_NumVec(res_coef / res_SE, true, X_names);
  NumericVector res_p_values = 2 * pt(- abs(res_t_values), n - k);
  double res_r_squared = 1 - arma::as_scalar(resid.t() * resid) / arma::as_scalar((y.t() - mean(y)) * (y - mean(y)));
  double res_adj_r_squared = 1 - (1 - res_r_squared) * ((n - 1) /(double) (n - k));
  double res_f_statistics = (res_r_squared * (n - k)) / ((1 - res_r_squared) * (k - 1));


  return List::create(Named("coefficients") = res_coef,
                      Named("residuals") = res_resid,
                      Named("fitted") = res_fitted,
                      Named("resid_err") = res_resid_err,
                      Named("SE") = res_SE,
                      Named("t_values") = res_t_values,
                      Named("p_values") = res_p_values,
                      Named("r_squared") = res_r_squared,
                      Named("adj_r_squared") = res_adj_r_squared,
                      Named("f_statistics") = res_f_statistics);
}
