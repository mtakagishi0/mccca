#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat calc_3mat(arma::mat A, arma::mat B,arma::mat C){

	arma::mat ABC=A*B*C;
    return ABC;

}
