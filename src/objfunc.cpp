#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
double obj(arma::mat XB,arma::mat U,int J){

    //int N=U.n_rows;
    //int m=J//dummy_diag.n_rows/N;

    //arma::mat PP=B.t()*dummy_mat.t()*Jn*U*arma::inv(U.t()*U)*U.t()*Jn*dummy_mat*B;
    //double obval=sum(arma::diagvec(PP))/(N*(pow(m,2)));;
    arma::mat PP=XB.t()*U*arma::inv(U.t()*U)*U.t()*XB;//22/12/19
    double obval=sum(arma::diagvec(PP))/J;

    obval=obval*(-1);
    return obval;

}
