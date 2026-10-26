#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat calc_Ddiag_sq(arma::mat dummy_diag){

	arma::mat DD=dummy_diag.t()*dummy_diag;

	////eigen
	arma::vec eigval;
    arma::mat eigvec;

    arma::eig_sym(eigval, eigvec, DD);

    arma::vec eigval_sq=arma::sqrt(abs(eigval));
    arma::vec eigval_sqinv=1/eigval_sq;

    arma::mat Lam=arma::diagmat(eigval_sqinv);

    arma::mat Ddiag_sq=eigvec*Lam*eigvec.t();

    return Ddiag_sq;


}
