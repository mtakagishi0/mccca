#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

arma::mat create_Mn(int n_all){

	arma::mat E=arma::eye(n_all,n_all);
	arma::mat A(n_all,n_all,arma::fill::ones);
	A=A/n_all;
	//arma::vec one_vec=arma::ones(n_all);

	arma::mat Mn=E-A;//(1/n_all)*((one_vec)*one_vec.t());//(1/n_all)%
	//A.print();
	//((one_vec)*one_vec.t()).print();
	//((1/n_all)*((one_vec)*one_vec.t())).print();
	//E.print();
	//Jn <- diag(n.all)-(1/n.all)*(rep(1,n.all)%*%t(rep(1,n.all)))

  return(Mn);
}
