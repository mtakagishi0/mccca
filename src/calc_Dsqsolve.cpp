#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat calc_Dsqsolve(arma::mat D){

	//arma::mat DD=dummy_diag.t()*dummy_diag;

	////eigen
	arma::vec eigval;
    arma::mat eigvec;

    //printf("1\n");

    arma::eig_sym(eigval, eigvec, D);
    //printf("2\n");

    arma::vec eigval_sq=arma::sqrt(abs(eigval));
    arma::vec eigval_sqinv=1/eigval_sq;

    //printf("3\n");

    arma::mat Lam=arma::diagmat(eigval_sqinv);

    //printf("4\n");

    arma::mat Ddiag_sq=eigvec*Lam*eigvec.t();

    //printf("5\n");

    return Ddiag_sq;


}
