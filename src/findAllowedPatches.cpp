#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

// [[Rcpp::export]]
List findAllowedPatches(LogicalVector seq, int minLen) {
  // seq: vector indicating of symbol is allowed
  
  int allowedCount = 0; // Internal count add to list 
  
  std::vector<int> startPos;
  std::vector<int> endPos;
  
  for(int i = 0; i <= seq.size(); ++i){
    if(i < seq.size() && seq[i]){
      allowedCount++;
    }
    else {
     if(allowedCount >= minLen){
       startPos.push_back(i - allowedCount);
       endPos.push_back(i - 1);
     }
     allowedCount = 0;
    }
  }
  
  List ret;
  ret["startPos"] = startPos;
  ret["endPos"]   = endPos;
  return ret;
}