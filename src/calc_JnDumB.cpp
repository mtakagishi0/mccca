#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat calc_JnDumB(arma::mat Jn, arma::mat dummy_mat,arma::mat B){

	arma::mat JZB=Jn*dummy_mat*B;
    return JZB;

}
