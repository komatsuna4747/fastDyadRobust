# include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]


// [[Rcpp::export()]]
arma::mat sum_by_dyad_cluster(arma::mat &sw, arma::uvec &cluster_bool) {
  return arma::join_cols(
    arma::sum(sw.rows(arma::find(cluster_bool == true)), 0),
    sw.rows(arma::find(cluster_bool == false))
  );
}


// [[Rcpp::export()]]
arma::mat create_meat(arma::mat &sw, arma::mat &pair, arma::vec &id) {
  arma::mat meat(sw.n_cols, sw.n_cols);

  for (int i = 0; i < id.n_rows; i++) {
    arma::uvec cluster_bool = (pair.col(0) == id[i] || pair.col(1) == id[i]);
    arma::mat uj = sum_by_dyad_cluster(sw, cluster_bool);
    meat += uj.t() * uj;
  }

  return(meat / sw.n_rows);
}
