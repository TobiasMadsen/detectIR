#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector annotateMotif_cpp(IntegerVector enc1, IntegerVector enc2, IntegerVector start, IntegerVector end){
  int K = start.length(); // Number of instances
  int N = enc1.length(); // Length of sequence
  IntegerVector ret(K);

  for(int i = 0; i < K; ++i){
    int l = 1;
    while(start[i]-1-l >= 0 && end[i]-1+l < N && 
	  enc1[start[i]-1-l] + enc1[end[i]-1+l] == 0 &&
	  enc2[start[i]-1-l] + enc2[end[i]-1+l] == 0  ){
      ++l;
    }
    --l;
    ret[i] = l;
  }

  return ret;
}
