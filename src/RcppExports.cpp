// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// annotateMotif_cpp
IntegerVector annotateMotif_cpp(IntegerVector enc1, IntegerVector enc2, IntegerVector start, IntegerVector end);
RcppExport SEXP detectIR_annotateMotif_cpp(SEXP enc1SEXP, SEXP enc2SEXP, SEXP startSEXP, SEXP endSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type enc1(enc1SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type enc2(enc2SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type start(startSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type end(endSEXP);
    rcpp_result_gen = Rcpp::wrap(annotateMotif_cpp(enc1, enc2, start, end));
    return rcpp_result_gen;
END_RCPP
}
// detectImperfectIR_cpp
List detectImperfectIR_cpp(IntegerVector enc1, IntegerVector enc2, int minStemLen, int maxMismcNum, bool verbose);
RcppExport SEXP detectIR_detectImperfectIR_cpp(SEXP enc1SEXP, SEXP enc2SEXP, SEXP minStemLenSEXP, SEXP maxMismcNumSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type enc1(enc1SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type enc2(enc2SEXP);
    Rcpp::traits::input_parameter< int >::type minStemLen(minStemLenSEXP);
    Rcpp::traits::input_parameter< int >::type maxMismcNum(maxMismcNumSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(detectImperfectIR_cpp(enc1, enc2, minStemLen, maxMismcNum, verbose));
    return rcpp_result_gen;
END_RCPP
}
// detectImperfectIRWithLoop_cpp
List detectImperfectIRWithLoop_cpp(IntegerVector enc1, IntegerVector enc2, int minStemLen, int maxMismcNum, int loopLength, bool verbose);
RcppExport SEXP detectIR_detectImperfectIRWithLoop_cpp(SEXP enc1SEXP, SEXP enc2SEXP, SEXP minStemLenSEXP, SEXP maxMismcNumSEXP, SEXP loopLengthSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type enc1(enc1SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type enc2(enc2SEXP);
    Rcpp::traits::input_parameter< int >::type minStemLen(minStemLenSEXP);
    Rcpp::traits::input_parameter< int >::type maxMismcNum(maxMismcNumSEXP);
    Rcpp::traits::input_parameter< int >::type loopLength(loopLengthSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(detectImperfectIRWithLoop_cpp(enc1, enc2, minStemLen, maxMismcNum, loopLength, verbose));
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
