#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat calc_MnZD(arma::mat Mn, arma::mat dummy_mat,arma::mat Ddiag_sq){

	arma::mat MZD=Mn*dummy_mat*Ddiag_sq;
    return MZD;

}
