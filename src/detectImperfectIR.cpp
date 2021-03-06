#include <numeric>
#include <vector>
#include <Rcpp.h>

using namespace Rcpp;

struct Enc {
  // Encode A(1,0), T(-1,0), C(0,1), G(0,-1)
  int m1,m2;

  Enc() : m1(0), m2(0) {} ;
  Enc(int m1_, int m2_) : m1(m1_), m2(m2_) {};

  Enc operator+(const Enc& o){
    return Enc(m1 + o.m1, m2 + o.m2);
  }

  Enc operator-(const Enc& o){
    return Enc(m1 - o.m1, m2 - o.m2);
  }

  bool operator==(const Enc & o){
    return m1 == o.m1 && m2 == o.m2;
  }

  int norm(){
    return (m1>0?m1:-m1) + (m2>0?m2:-m2);
  }

  friend bool isZero(const Enc & enc);
  friend std::ostream& operator<<(std::ostream& os, const Enc & enc);
};

std::ostream& operator<<(std::ostream& os, const Enc & enc){
  os << "(" << enc.m1 << "," << enc.m2 << ")";
  return os;
}

bool isZero(const Enc & enc){
  return (enc.m1 == 0 && enc.m2 == 0);
}


// Structure to hold candidates
struct Candidate {
  int start;
  int end;
  int mismcNum; // Maintain as mismcNum between first and last match
  int firstMatch;
  int lastMatch;

  Candidate(int start_, int end_, int mismcNum_, int firstMatch_, int lastMatch_) :
    start(start_), end(end_), mismcNum(mismcNum_), firstMatch(firstMatch_), lastMatch(lastMatch_) {}
};

void addCandidates(int stemLen, int maxMismcNum, int loopLen, std::vector<Enc> & cumScore, std::vector<Enc> & nseq, std::vector<Candidate> & candidates, bool verbose = false){
  int N = nseq.size();

  // Add candidate IR with distance len
  for(int i = 0 ; i < N-2*stemLen-loopLen+1; ++i){
    // Loop over cummulative score
    if( (cumScore[stemLen+i] -  cumScore[i] + cumScore[2*stemLen+loopLen+i] - cumScore[stemLen+loopLen+i] ).norm()  <= maxMismcNum ){
      // TODO: Calculate actual mismatch number (possibly not counting a central loop)
      int misMatchNum = 0;
      int firstMatch = -1;
      int lastMatch = -1;
      for(int j = stemLen-1; j >= 0; --j){
	bool match = isZero(nseq[ i + j ] + nseq[ i + 2*stemLen+loopLen - j - 1]);
	misMatchNum += match?0:2;
	if(match){
	  firstMatch = i + j;
	  lastMatch  = i + 2*stemLen+loopLen - j - 1;
	}
      }
      if(misMatchNum <= maxMismcNum){
	if (verbose)
	  printf("Adding candidate with loop: From: %i To: %i FirstMatch: %i LastMatch: %i\n", i, i+2*stemLen+loopLen-1, firstMatch, lastMatch);
	candidates.push_back(Candidate(i, i+2*stemLen+loopLen-1, misMatchNum, firstMatch, lastMatch));
      }
    }
  }
}


// [[Rcpp::export]]
List detectImperfectIR_cpp(IntegerVector enc1, IntegerVector enc2, int minStemLen, int maxMismcNum, bool verbose = false) {
  // Perform checks on input

  // Useful variables
  int N = enc1.size();

  // Return structures
  std::vector<int> startPos;
  std::vector<int> endPos;

  // 1. Mapping
  std::vector<Enc> nseq;
  for(int i = 0; i < N; ++i){
    nseq.push_back( Enc( enc1(i), enc2(i) ) );
  }

  // 2. Calculate cumulative score
  std::vector<Enc> cumScore(nseq.size()+1);
  cumScore[0] = Enc(0,0);
  std::partial_sum(nseq.begin(), nseq.end(), cumScore.begin()+1);
  
  //FIXME copy(cumScore.begin(), cumScore.end(), std::ostream_iterator<Enc>(Rcout, " "));
  // 3. Maintain list of candidates
  std::vector<Candidate> candidates;
  
  // 3.1 Possible IRs' cumulative score
  // Candidates with even length 
  addCandidates(minStemLen, maxMismcNum, 0, cumScore, nseq, candidates, verbose);
  // Candidates with odd length
  addCandidates(minStemLen, maxMismcNum, 1, cumScore, nseq, candidates, verbose);

  // 4. Grow candidates
  for(std::vector<Candidate>::iterator it = candidates.begin(); it != candidates.end(); ++it){
    while(true){
      // Increment start and end
      if( 0 > --(it->start) || N <= ++(it->end) )
	break;

      // Check if match
      if ( isZero( nseq.at(it->start) + nseq.at(it->end) ) ){
	it->mismcNum += (it->firstMatch - it->start - 1) * 2;
	it->firstMatch = it->start;
	it->lastMatch  = it->end;
      }
      else if (it->mismcNum + (it->firstMatch - it->start) * 2 > maxMismcNum){
	break;
      }
    }
  }
  
  // 5. Report full-grown candidates
  for(std::vector<Candidate>::iterator it = candidates.begin(); it != candidates.end(); ++it){
    if(it->lastMatch - it->firstMatch + 1 >= 2*minStemLen){
      startPos.push_back( it->firstMatch + 1); // Add one for 1-indexing in R
      endPos.push_back( it->lastMatch + 1);
    }
  }

  // Return
  List ret;
  ret["startPos"] = wrap( startPos);
  ret["endPos"]   = wrap( endPos);
  return ret;
}

// [[Rcpp::export]]
List detectImperfectIRWithLoop_cpp(IntegerVector enc1, IntegerVector enc2, int minStemLen, int maxMismcNum, int loopLength, bool verbose = false) {
  // Perform checks on input

  // Useful variables
  int N = enc1.size();

  // Return structures
  std::vector<int> startPos;
  std::vector<int> endPos;

  // 1. Mapping
  std::vector<Enc> nseq;
  for(int i = 0; i < N; ++i){
    nseq.push_back( Enc( enc1(i), enc2(i) ) );
  }

  // 2. Calculate cumulative score
  std::vector<Enc> cumScore(nseq.size()+1);
  cumScore[0] = Enc(0,0);
  std::partial_sum(nseq.begin(), nseq.end(), cumScore.begin()+1);
  
  //FIXME copy(cumScore.begin(), cumScore.end(), std::ostream_iterator<Enc>(Rcout, " "));
  // 3. Maintain list of candidates
  std::vector<Candidate> candidates;
  
  // 3.1 Possible IRs' cumulative score
  // Candidates with distance minLen
  addCandidates(minStemLen, maxMismcNum, loopLength, cumScore, nseq, candidates, verbose);

  // 4. Grow candidates
  for(std::vector<Candidate>::iterator it = candidates.begin(); it != candidates.end(); ++it){
    while(true){
      // Increment start and end
      if( 0 > --(it->start) || N <= ++(it->end) )
	break;

      // Check if match
      if ( isZero( nseq.at(it->start) + nseq.at(it->end) ) ){
	it->mismcNum += (it->firstMatch - it->start - 1) * 2;
	it->firstMatch = it->start;
	it->lastMatch  = it->end;
      }
      else if (it->mismcNum + (it->firstMatch - it->start) * 2 > maxMismcNum){
	break;
      }
    }
  }
  
  // 5. Report full-grown candidates
  for(std::vector<Candidate>::iterator it = candidates.begin(); it != candidates.end(); ++it){
    if(it->lastMatch - it->firstMatch + 1 >= 2*minStemLen+loopLength){
      startPos.push_back( it->firstMatch + 1); // Add one for 1-indexing in R
      endPos.push_back( it->lastMatch + 1);
    }
  }

  // Return
  List ret;
  ret["startPos"] = wrap( startPos);
  ret["endPos"]   = wrap( endPos);
  return ret;
}
