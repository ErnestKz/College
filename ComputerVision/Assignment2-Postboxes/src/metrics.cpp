// input file 1
// input file 2

// evaluate
// - precision
// - recall
// - F1 metrics

// for
// - which frames postboxes are not obscured
// - whether individual boxes contain post

// TP FP_OBSCURED FN_OBSCURED 

// recall tp / (tp + fn_obscured)
// precision tp / (tp + fp_obscured)
// f1 = 2 * (precision * recall) / (precision + recall)

float recall(float tp, float fn){
  return (tp / (tp + fn));
}
float precision(float tp, float fp){
  return (tp / (tp + fp));
}
float f1(float precision, float recall){
  return (2 * (precision * recall) / (float)(precision + recall));
}

#include <fstream>
#include <iostream>
#include <vector>
#include <string>
#include <sstream>

using namespace std;


vector<string> tokenizer( const string& string_with_tokens, char d )  {
  vector<string> tokens;
  string token;
  stringstream string_stream(string_with_tokens);
  while( getline( string_stream, token, d ) ) {
    tokens.push_back( token );
  }

  return tokens;
} 

// arg 1: file path of ground truth
// arg 2: file path of vision system results
int main(int args, char* argv[]){
  const char* ground_truth_filepath = argv[1];
  const char* predictions_filepath = argv[2];
  cout << "hello" << endl;
  ifstream ground_truth_file(ground_truth_filepath);
  ifstream predictions_file(predictions_filepath);

  cout << ground_truth_filepath << endl;
  cout << predictions_filepath << endl;
  
  string ground_truth;
  string prediction;

  int tp_obscured = 0;
  int fp_obscured = 0;
  int fn_obscured = 0;
  int tn_obscured = 0;
  
  while (getline(ground_truth_file, ground_truth) &&  getline(predictions_file, prediction)) {
    vector<string> prediction_tokens = tokenizer(prediction, ' ');
    vector<string> truth_tokens = tokenizer(ground_truth, ' ');

      // View obscured
      if (truth_tokens[1] == "View")
	if (prediction_tokens[1] == "View")
	  ++tn_obscured;
	else
	  ++fp_obscured;

      if (truth_tokens[1] == "No" || truth_tokens[1] == "Post")
	if (prediction_tokens[1] == "Post" || prediction_tokens[1] == "No"){
	  ++tp_obscured;
	  

	  
	} else
	  ++fn_obscured;

  }
  cout << "fp_obscured:" << endl;
  cout << fp_obscured << endl;

  cout << "tp_obscured:" << endl;
  cout << tp_obscured << endl;

  cout << "fn_obscured:" << endl;
  cout << fn_obscured << endl;

  cout << "recall:" << endl;
  cout << recall(tp_obscured, fn_obscured) << endl;

  cout << "precision:" << endl;
  cout << precision(tp_obscured, fp_obscured) << endl;

  cout << "f1:" << endl;
  cout << f1(precision(tp_obscured, fp_obscured), recall(tp_obscured, fn_obscured)) << endl;
  
}

// STRING:
// <frame>, REST

// REST
// No post
// or
// View obscured
// or
// Post in FRAMES

// FRAMES
// 1
// 1 2
//...
