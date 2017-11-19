#include <Rcpp.h>
//'
//' Split a string into codons
//' @param cDNA - a string
//' @return a vector of strings of length 3
//' @export
//'
// [[Rcpp::export]]
Rcpp::StringVector cpp_codonSplit2( std::string cDNA ) {

  std::vector < std::string > codons;
  for( int i=0; i < cDNA.length(); i+=3 ) {

    codons.push_back( cDNA.substr( i, 3 ) );

  }
  Rcpp::StringVector x;
  x = codons;
  return x;
}
