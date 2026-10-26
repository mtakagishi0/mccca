#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat updateB(arma::mat Xtil,arma::mat U,
                   int J, int lowdim){

    //int n_all=U.n_rows;

    //arma::mat Yn=U.t()*Jn*dummy_mat;
    //arma::mat DZZD=Ddiag_sq*Yn.t()*arma::inv(U.t()*U)*(Yn*Ddiag_sq);
    arma::mat DZZD=Xtil.t()*U*arma::inv(U.t()*U)*(U.t()*Xtil);
    DZZD=DZZD/J;

    ///eigen
	arma::vec eigval;
    arma::mat eigvec;

    arma::eig_sym(eigval, eigvec, DZZD);
    int d=DZZD.n_cols;
    //eigvec.print();
    //d.print();

    //pick up first lowdim's eigen vectors
    arma::mat Bstar=arma::zeros(d,lowdim);
    int dd2=d-1;

    for(int dd = 0; dd < lowdim; ++dd){
        //printf("dd=%d,dd2=%d\n",dd,dd2);
        Bstar.col(dd)=eigvec.col(dd2);
        dd2--;
    }

    //arma::mat Bstar=eigvec2;

    //arma::mat Bmat=Ddiag_sq*Bstar;
    //Bmat=sqrt(n_all*m)*Bmat;
    //return Bmat;

    return Bstar;


}
