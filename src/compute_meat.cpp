#include <RcppArmadillo.h>
#include <RcppParallel.h>
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(RcppArmadillo)]]


struct MeatCreator : public RcppParallel::Worker {

  // inputs to read from
  const arma::mat &sw;
  const arma::mat &pair;
  const arma::vec &id;

  // output matrix to write to
  arma::mat meat;

  // Constructor 1: The main constructor
  MeatCreator (
      const arma::mat &sw,
      const arma::mat &pair,
      const arma::vec &id
  ) :
    sw(sw),
    pair(pair),
    id(id),
    meat()
  {
    meat.resize(sw.n_cols, sw.n_cols);
  }

  // Constructor 2: Called for each split job
  MeatCreator (
      const MeatCreator &meatCreator,
      RcppParallel::Split
  ) :
    sw(meatCreator.sw),
    pair(meatCreator.pair),
    id(meatCreator.id),
    meat()
  {
    meat.resize(sw.n_cols, sw.n_cols);
  }

  arma::mat create_meat_for_i(
      int i,
      const arma::mat &sw,
      const arma::mat &pair,
      const arma::vec &id
  ) {
    arma::uvec cluster_bool = (pair.col(0) == id[i] || pair.col(1) == id[i]);

    // This part comes from sum_by_dyad_cluster()
    arma::mat uj = arma::join_cols(
      arma::sum(sw.rows(arma::find(cluster_bool == true)), 0),
      sw.rows(arma::find(cluster_bool == false))
    );
    return uj.t() * uj;
  }

  // Parallel function operator
  void operator() (std::size_t begin, std::size_t end)
  {
    for (size_t i = begin; i < end; i++) {
      meat += create_meat_for_i(i, sw, pair, id);
    }
  }

  void join(const MeatCreator &meatCreator)
  {
    meat += meatCreator.meat;
  }
};


// [[Rcpp::export]]
arma::mat create_meat(
    const arma::mat &sw,
    const arma::mat &pair,
    const arma::vec &id
) {

  // declare the MeatCreator instance
  MeatCreator meatCreator(sw, pair, id);

  // call parallelReduce to start the work
  RcppParallel::parallelReduce(0, id.n_rows, meatCreator);

  // return the computed sum
  return meatCreator.meat / sw.n_rows;
}
