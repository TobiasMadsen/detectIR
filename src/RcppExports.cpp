// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// detectImperfectIR_cpp
List detectImperfectIR_cpp(IntegerVector enc1, IntegerVector enc2, int minLen, int maxMismcNum);
RcppExport SEXP detectIR_detectImperfectIR_cpp(SEXP enc1SEXP, SEXP enc2SEXP, SEXP minLenSEXP, SEXP maxMismcNumSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type enc1(enc1SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type enc2(enc2SEXP);
    Rcpp::traits::input_parameter< int >::type minLen(minLenSEXP);
    Rcpp::traits::input_parameter< int >::type maxMismcNum(maxMismcNumSEXP);
    rcpp_result_gen = Rcpp::wrap(detectImperfectIR_cpp(enc1, enc2, minLen, maxMismcNum));
    return rcpp_result_gen;
END_RCPP
}
// detectImperfectIRWithLoop_cpp
List detectImperfectIRWithLoop_cpp(IntegerVector enc1, IntegerVector enc2, int minLen, int maxMismcNum, int loopLength);
RcppExport SEXP detectIR_detectImperfectIRWithLoop_cpp(SEXP enc1SEXP, SEXP enc2SEXP, SEXP minLenSEXP, SEXP maxMismcNumSEXP, SEXP loopLengthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type enc1(enc1SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type enc2(enc2SEXP);
    Rcpp::traits::input_parameter< int >::type minLen(minLenSEXP);
    Rcpp::traits::input_parameter< int >::type maxMismcNum(maxMismcNumSEXP);
    Rcpp::traits::input_parameter< int >::type loopLength(loopLengthSEXP);
    rcpp_result_gen = Rcpp::wrap(detectImperfectIRWithLoop_cpp(enc1, enc2, minLen, maxMismcNum, loopLength));
    return rcpp_result_gen;
END_RCPP
}
// findAllowedPatches
List findAllowedPatches(LogicalVector seq, int minLen);
RcppExport SEXP detectIR_findAllowedPatches(SEXP seqSEXP, SEXP minLenSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< LogicalVector >::type seq(seqSEXP);
    Rcpp::traits::input_parameter< int >::type minLen(minLenSEXP);
    rcpp_result_gen = Rcpp::wrap(findAllowedPatches(seq, minLen));
    return rcpp_result_gen;
END_RCPP
}
