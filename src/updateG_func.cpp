#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat calc_updateG(arma::mat U, arma::mat X,int J){

	arma::mat G=arma::inv(U.t()*U)*U.t()*X;
	//G=G/J;
    return G;

}
