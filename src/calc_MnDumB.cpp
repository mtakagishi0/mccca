#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat calc_MnDumB(arma::mat Mn, arma::mat dummy_mat,arma::mat B){

	arma::mat MZB=Mn*dummy_mat*B;
    return MZB;

}
