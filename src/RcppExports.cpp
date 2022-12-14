// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// create_meat
arma::mat create_meat(const arma::mat& sw, const arma::mat& pair, const arma::vec& id);
RcppExport SEXP _fastDyadRobust_create_meat(SEXP swSEXP, SEXP pairSEXP, SEXP idSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type sw(swSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type pair(pairSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type id(idSEXP);
    rcpp_result_gen = Rcpp::wrap(create_meat(sw, pair, id));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_fastDyadRobust_create_meat", (DL_FUNC) &_fastDyadRobust_create_meat, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_fastDyadRobust(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
