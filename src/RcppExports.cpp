// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// calculate_ACMI_contribution
arma::vec calculate_ACMI_contribution(double dist_sum, arma::vec colsums, arma::vec rowsums, int num_cols, arma::vec column_contributions, arma::vec row_index_counts, arma::mat joint, int total_non_zeros, double full_MI);
RcppExport SEXP SpeedReader_calculate_ACMI_contribution(SEXP dist_sumSEXP, SEXP colsumsSEXP, SEXP rowsumsSEXP, SEXP num_colsSEXP, SEXP column_contributionsSEXP, SEXP row_index_countsSEXP, SEXP jointSEXP, SEXP total_non_zerosSEXP, SEXP full_MISEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type dist_sum(dist_sumSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type colsums(colsumsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type rowsums(rowsumsSEXP);
    Rcpp::traits::input_parameter< int >::type num_cols(num_colsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type column_contributions(column_contributionsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type row_index_counts(row_index_countsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type joint(jointSEXP);
    Rcpp::traits::input_parameter< int >::type total_non_zeros(total_non_zerosSEXP);
    Rcpp::traits::input_parameter< double >::type full_MI(full_MISEXP);
    rcpp_result_gen = Rcpp::wrap(calculate_ACMI_contribution(dist_sum, colsums, rowsums, num_cols, column_contributions, row_index_counts, joint, total_non_zeros, full_MI));
    return rcpp_result_gen;
END_RCPP
}
// calculate_unique_MI_contribution
arma::vec calculate_unique_MI_contribution(arma::vec colsums, arma::vec rowsums, int num_cols, int num_rows, arma::mat joint, arma::vec column_type_counts, double dist_sum);
RcppExport SEXP SpeedReader_calculate_unique_MI_contribution(SEXP colsumsSEXP, SEXP rowsumsSEXP, SEXP num_colsSEXP, SEXP num_rowsSEXP, SEXP jointSEXP, SEXP column_type_countsSEXP, SEXP dist_sumSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type colsums(colsumsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type rowsums(rowsumsSEXP);
    Rcpp::traits::input_parameter< int >::type num_cols(num_colsSEXP);
    Rcpp::traits::input_parameter< int >::type num_rows(num_rowsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type joint(jointSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type column_type_counts(column_type_countsSEXP);
    Rcpp::traits::input_parameter< double >::type dist_sum(dist_sumSEXP);
    rcpp_result_gen = Rcpp::wrap(calculate_unique_MI_contribution(colsums, rowsums, num_cols, num_rows, joint, column_type_counts, dist_sum));
    return rcpp_result_gen;
END_RCPP
}
// calculate_document_frequency
arma::vec calculate_document_frequency(arma::mat document_word_matrix);
RcppExport SEXP SpeedReader_calculate_document_frequency(SEXP document_word_matrixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type document_word_matrix(document_word_matrixSEXP);
    rcpp_result_gen = Rcpp::wrap(calculate_document_frequency(document_word_matrix));
    return rcpp_result_gen;
END_RCPP
}
// Calculate_TFIDF
List Calculate_TFIDF(arma::mat document_word_matrix);
RcppExport SEXP SpeedReader_Calculate_TFIDF(SEXP document_word_matrixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type document_word_matrix(document_word_matrixSEXP);
    rcpp_result_gen = Rcpp::wrap(Calculate_TFIDF(document_word_matrix));
    return rcpp_result_gen;
END_RCPP
}
// Col_and_Row_Sums
List Col_and_Row_Sums(arma::mat joint_dist);
RcppExport SEXP SpeedReader_Col_and_Row_Sums(SEXP joint_distSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type joint_dist(joint_distSEXP);
    rcpp_result_gen = Rcpp::wrap(Col_and_Row_Sums(joint_dist));
    return rcpp_result_gen;
END_RCPP
}
// Combine_Document_Term_Matrices
List Combine_Document_Term_Matrices(List document_word_matrix_list, List vocabularies, std::vector<std::string> unique_words, int number_of_corpora);
RcppExport SEXP SpeedReader_Combine_Document_Term_Matrices(SEXP document_word_matrix_listSEXP, SEXP vocabulariesSEXP, SEXP unique_wordsSEXP, SEXP number_of_corporaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type document_word_matrix_list(document_word_matrix_listSEXP);
    Rcpp::traits::input_parameter< List >::type vocabularies(vocabulariesSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type unique_words(unique_wordsSEXP);
    Rcpp::traits::input_parameter< int >::type number_of_corpora(number_of_corporaSEXP);
    rcpp_result_gen = Rcpp::wrap(Combine_Document_Term_Matrices(document_word_matrix_list, vocabularies, unique_words, number_of_corpora));
    return rcpp_result_gen;
END_RCPP
}
// Count_Words
List Count_Words(int number_of_documents, List Document_Words, arma::vec Document_Lengths, int max_vocab_size, int add_to_vocabulary, arma::vec existing_word_counts, std::vector<std::string> existing_vocabulary, int existing_vocabulary_size, int using_wordcounts, List Document_Word_Counts, int print_counter);
RcppExport SEXP SpeedReader_Count_Words(SEXP number_of_documentsSEXP, SEXP Document_WordsSEXP, SEXP Document_LengthsSEXP, SEXP max_vocab_sizeSEXP, SEXP add_to_vocabularySEXP, SEXP existing_word_countsSEXP, SEXP existing_vocabularySEXP, SEXP existing_vocabulary_sizeSEXP, SEXP using_wordcountsSEXP, SEXP Document_Word_CountsSEXP, SEXP print_counterSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type number_of_documents(number_of_documentsSEXP);
    Rcpp::traits::input_parameter< List >::type Document_Words(Document_WordsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type Document_Lengths(Document_LengthsSEXP);
    Rcpp::traits::input_parameter< int >::type max_vocab_size(max_vocab_sizeSEXP);
    Rcpp::traits::input_parameter< int >::type add_to_vocabulary(add_to_vocabularySEXP);
    Rcpp::traits::input_parameter< arma::vec >::type existing_word_counts(existing_word_countsSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type existing_vocabulary(existing_vocabularySEXP);
    Rcpp::traits::input_parameter< int >::type existing_vocabulary_size(existing_vocabulary_sizeSEXP);
    Rcpp::traits::input_parameter< int >::type using_wordcounts(using_wordcountsSEXP);
    Rcpp::traits::input_parameter< List >::type Document_Word_Counts(Document_Word_CountsSEXP);
    Rcpp::traits::input_parameter< int >::type print_counter(print_counterSEXP);
    rcpp_result_gen = Rcpp::wrap(Count_Words(number_of_documents, Document_Words, Document_Lengths, max_vocab_size, add_to_vocabulary, existing_word_counts, existing_vocabulary, existing_vocabulary_size, using_wordcounts, Document_Word_Counts, print_counter));
    return rcpp_result_gen;
END_RCPP
}
// Fast_Mutual_Information
double Fast_Mutual_Information(arma::mat joint_dist, arma::vec non_zero_cols);
RcppExport SEXP SpeedReader_Fast_Mutual_Information(SEXP joint_distSEXP, SEXP non_zero_colsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type joint_dist(joint_distSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type non_zero_cols(non_zero_colsSEXP);
    rcpp_result_gen = Rcpp::wrap(Fast_Mutual_Information(joint_dist, non_zero_cols));
    return rcpp_result_gen;
END_RCPP
}
// Fast_Sparse_Mutual_Information
double Fast_Sparse_Mutual_Information(arma::vec rows, arma::vec cols, arma::vec vals, arma::vec colsums, arma::vec rowsums, int num_entries);
RcppExport SEXP SpeedReader_Fast_Sparse_Mutual_Information(SEXP rowsSEXP, SEXP colsSEXP, SEXP valsSEXP, SEXP colsumsSEXP, SEXP rowsumsSEXP, SEXP num_entriesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type rows(rowsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type cols(colsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type vals(valsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type colsums(colsumsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type rowsums(rowsumsSEXP);
    Rcpp::traits::input_parameter< int >::type num_entries(num_entriesSEXP);
    rcpp_result_gen = Rcpp::wrap(Fast_Sparse_Mutual_Information(rows, cols, vals, colsums, rowsums, num_entries));
    return rcpp_result_gen;
END_RCPP
}
// Fast_Sparse_Mutual_Information_Full
List Fast_Sparse_Mutual_Information_Full(arma::vec rows, arma::vec cols, arma::vec vals, arma::vec colsums, arma::vec rowsums, int num_entries);
RcppExport SEXP SpeedReader_Fast_Sparse_Mutual_Information_Full(SEXP rowsSEXP, SEXP colsSEXP, SEXP valsSEXP, SEXP colsumsSEXP, SEXP rowsumsSEXP, SEXP num_entriesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type rows(rowsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type cols(colsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type vals(valsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type colsums(colsumsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type rowsums(rowsumsSEXP);
    Rcpp::traits::input_parameter< int >::type num_entries(num_entriesSEXP);
    rcpp_result_gen = Rcpp::wrap(Fast_Sparse_Mutual_Information_Full(rows, cols, vals, colsums, rowsums, num_entries));
    return rcpp_result_gen;
END_RCPP
}
// Generate_Document_Term_Matrix
arma::mat Generate_Document_Term_Matrix(int number_of_documents, int number_of_unique_words, std::vector<std::string> unique_words, List Document_Words, arma::vec Document_Lengths, int using_wordcounts, List Document_Word_Counts);
RcppExport SEXP SpeedReader_Generate_Document_Term_Matrix(SEXP number_of_documentsSEXP, SEXP number_of_unique_wordsSEXP, SEXP unique_wordsSEXP, SEXP Document_WordsSEXP, SEXP Document_LengthsSEXP, SEXP using_wordcountsSEXP, SEXP Document_Word_CountsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type number_of_documents(number_of_documentsSEXP);
    Rcpp::traits::input_parameter< int >::type number_of_unique_words(number_of_unique_wordsSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type unique_words(unique_wordsSEXP);
    Rcpp::traits::input_parameter< List >::type Document_Words(Document_WordsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type Document_Lengths(Document_LengthsSEXP);
    Rcpp::traits::input_parameter< int >::type using_wordcounts(using_wordcountsSEXP);
    Rcpp::traits::input_parameter< List >::type Document_Word_Counts(Document_Word_CountsSEXP);
    rcpp_result_gen = Rcpp::wrap(Generate_Document_Term_Matrix(number_of_documents, number_of_unique_words, unique_words, Document_Words, Document_Lengths, using_wordcounts, Document_Word_Counts));
    return rcpp_result_gen;
END_RCPP
}
// Generate_Sparse_Document_Term_Matrix_Stem_Vocabulary
List Generate_Sparse_Document_Term_Matrix_Stem_Vocabulary(int number_of_documents, int number_of_unique_words, std::vector<std::string> unique_words, List Document_Words, arma::vec Document_Lengths, List Document_Word_Counts, int total_terms, std::vector<std::string> stem_lookup, arma::vec starts, arma::vec ends, int lookup_size);
RcppExport SEXP SpeedReader_Generate_Sparse_Document_Term_Matrix_Stem_Vocabulary(SEXP number_of_documentsSEXP, SEXP number_of_unique_wordsSEXP, SEXP unique_wordsSEXP, SEXP Document_WordsSEXP, SEXP Document_LengthsSEXP, SEXP Document_Word_CountsSEXP, SEXP total_termsSEXP, SEXP stem_lookupSEXP, SEXP startsSEXP, SEXP endsSEXP, SEXP lookup_sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type number_of_documents(number_of_documentsSEXP);
    Rcpp::traits::input_parameter< int >::type number_of_unique_words(number_of_unique_wordsSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type unique_words(unique_wordsSEXP);
    Rcpp::traits::input_parameter< List >::type Document_Words(Document_WordsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type Document_Lengths(Document_LengthsSEXP);
    Rcpp::traits::input_parameter< List >::type Document_Word_Counts(Document_Word_CountsSEXP);
    Rcpp::traits::input_parameter< int >::type total_terms(total_termsSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type stem_lookup(stem_lookupSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type starts(startsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type ends(endsSEXP);
    Rcpp::traits::input_parameter< int >::type lookup_size(lookup_sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(Generate_Sparse_Document_Term_Matrix_Stem_Vocabulary(number_of_documents, number_of_unique_words, unique_words, Document_Words, Document_Lengths, Document_Word_Counts, total_terms, stem_lookup, starts, ends, lookup_size));
    return rcpp_result_gen;
END_RCPP
}
// Generate_Sparse_Document_Term_Matrix
List Generate_Sparse_Document_Term_Matrix(int number_of_documents, int number_of_unique_words, std::vector<std::string> unique_words, List Document_Words, arma::vec Document_Lengths, List Document_Word_Counts, int total_terms);
RcppExport SEXP SpeedReader_Generate_Sparse_Document_Term_Matrix(SEXP number_of_documentsSEXP, SEXP number_of_unique_wordsSEXP, SEXP unique_wordsSEXP, SEXP Document_WordsSEXP, SEXP Document_LengthsSEXP, SEXP Document_Word_CountsSEXP, SEXP total_termsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type number_of_documents(number_of_documentsSEXP);
    Rcpp::traits::input_parameter< int >::type number_of_unique_words(number_of_unique_wordsSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type unique_words(unique_wordsSEXP);
    Rcpp::traits::input_parameter< List >::type Document_Words(Document_WordsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type Document_Lengths(Document_LengthsSEXP);
    Rcpp::traits::input_parameter< List >::type Document_Word_Counts(Document_Word_CountsSEXP);
    Rcpp::traits::input_parameter< int >::type total_terms(total_termsSEXP);
    rcpp_result_gen = Rcpp::wrap(Generate_Sparse_Document_Term_Matrix(number_of_documents, number_of_unique_words, unique_words, Document_Words, Document_Lengths, Document_Word_Counts, total_terms));
    return rcpp_result_gen;
END_RCPP
}
// LineWise_Dice_Coefficients
arma::mat LineWise_Dice_Coefficients(int number_of_lines, List Lines, int number_of_lines2, List Lines2);
RcppExport SEXP SpeedReader_LineWise_Dice_Coefficients(SEXP number_of_linesSEXP, SEXP LinesSEXP, SEXP number_of_lines2SEXP, SEXP Lines2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type number_of_lines(number_of_linesSEXP);
    Rcpp::traits::input_parameter< List >::type Lines(LinesSEXP);
    Rcpp::traits::input_parameter< int >::type number_of_lines2(number_of_lines2SEXP);
    Rcpp::traits::input_parameter< List >::type Lines2(Lines2SEXP);
    rcpp_result_gen = Rcpp::wrap(LineWise_Dice_Coefficients(number_of_lines, Lines, number_of_lines2, Lines2));
    return rcpp_result_gen;
END_RCPP
}
// Mutual_Information
double Mutual_Information(arma::mat joint_dist);
RcppExport SEXP SpeedReader_Mutual_Information(SEXP joint_distSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type joint_dist(joint_distSEXP);
    rcpp_result_gen = Rcpp::wrap(Mutual_Information(joint_dist));
    return rcpp_result_gen;
END_RCPP
}
// Sparse_Document_Frequencies
arma::vec Sparse_Document_Frequencies(int length_sparse_counts, arma::vec sparse_counts, arma::vec document_frequencies, arma::vec print_sequence, int print_sequence_length);
RcppExport SEXP SpeedReader_Sparse_Document_Frequencies(SEXP length_sparse_countsSEXP, SEXP sparse_countsSEXP, SEXP document_frequenciesSEXP, SEXP print_sequenceSEXP, SEXP print_sequence_lengthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type length_sparse_counts(length_sparse_countsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type sparse_counts(sparse_countsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type document_frequencies(document_frequenciesSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type print_sequence(print_sequenceSEXP);
    Rcpp::traits::input_parameter< int >::type print_sequence_length(print_sequence_lengthSEXP);
    rcpp_result_gen = Rcpp::wrap(Sparse_Document_Frequencies(length_sparse_counts, sparse_counts, document_frequencies, print_sequence, print_sequence_length));
    return rcpp_result_gen;
END_RCPP
}
// Sparse_PMI_Statistics
List Sparse_PMI_Statistics(int length_sparse_counts, int table_sum, arma::vec colsums, arma::vec rowsums, arma::vec sparse_col_indexes, arma::vec sparse_row_indexes, arma::vec sparse_counts, arma::vec print_sequence, int print_sequence_length);
RcppExport SEXP SpeedReader_Sparse_PMI_Statistics(SEXP length_sparse_countsSEXP, SEXP table_sumSEXP, SEXP colsumsSEXP, SEXP rowsumsSEXP, SEXP sparse_col_indexesSEXP, SEXP sparse_row_indexesSEXP, SEXP sparse_countsSEXP, SEXP print_sequenceSEXP, SEXP print_sequence_lengthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type length_sparse_counts(length_sparse_countsSEXP);
    Rcpp::traits::input_parameter< int >::type table_sum(table_sumSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type colsums(colsumsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type rowsums(rowsumsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type sparse_col_indexes(sparse_col_indexesSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type sparse_row_indexes(sparse_row_indexesSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type sparse_counts(sparse_countsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type print_sequence(print_sequenceSEXP);
    Rcpp::traits::input_parameter< int >::type print_sequence_length(print_sequence_lengthSEXP);
    rcpp_result_gen = Rcpp::wrap(Sparse_PMI_Statistics(length_sparse_counts, table_sum, colsums, rowsums, sparse_col_indexes, sparse_row_indexes, sparse_counts, print_sequence, print_sequence_length));
    return rcpp_result_gen;
END_RCPP
}
// Sequential_Raw_Term_Dice_Matches
List Sequential_Raw_Term_Dice_Matches(List Lines, List Lines2, int Dice_Terms);
RcppExport SEXP SpeedReader_Sequential_Raw_Term_Dice_Matches(SEXP LinesSEXP, SEXP Lines2SEXP, SEXP Dice_TermsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type Lines(LinesSEXP);
    Rcpp::traits::input_parameter< List >::type Lines2(Lines2SEXP);
    Rcpp::traits::input_parameter< int >::type Dice_Terms(Dice_TermsSEXP);
    rcpp_result_gen = Rcpp::wrap(Sequential_Raw_Term_Dice_Matches(Lines, Lines2, Dice_Terms));
    return rcpp_result_gen;
END_RCPP
}
// Sequential_string_Set_Hash_Comparison
List Sequential_string_Set_Hash_Comparison(std::vector<std::string> doc1, std::vector<std::string> doc2, int Dice_Terms);
RcppExport SEXP SpeedReader_Sequential_string_Set_Hash_Comparison(SEXP doc1SEXP, SEXP doc2SEXP, SEXP Dice_TermsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::string> >::type doc1(doc1SEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type doc2(doc2SEXP);
    Rcpp::traits::input_parameter< int >::type Dice_Terms(Dice_TermsSEXP);
    rcpp_result_gen = Rcpp::wrap(Sequential_string_Set_Hash_Comparison(doc1, doc2, Dice_Terms));
    return rcpp_result_gen;
END_RCPP
}
// Sequential_Token_Set_Hash_Comparison
List Sequential_Token_Set_Hash_Comparison(std::vector<std::string> doc1, std::vector<std::string> doc2);
RcppExport SEXP SpeedReader_Sequential_Token_Set_Hash_Comparison(SEXP doc1SEXP, SEXP doc2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::string> >::type doc1(doc1SEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type doc2(doc2SEXP);
    rcpp_result_gen = Rcpp::wrap(Sequential_Token_Set_Hash_Comparison(doc1, doc2));
    return rcpp_result_gen;
END_RCPP
}
// Variable_Dice_Coefficients
List Variable_Dice_Coefficients(int number_of_lines, List Lines, int number_of_lines2, List Lines2, int Dice_Terms, bool rem_duplicates);
RcppExport SEXP SpeedReader_Variable_Dice_Coefficients(SEXP number_of_linesSEXP, SEXP LinesSEXP, SEXP number_of_lines2SEXP, SEXP Lines2SEXP, SEXP Dice_TermsSEXP, SEXP rem_duplicatesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type number_of_lines(number_of_linesSEXP);
    Rcpp::traits::input_parameter< List >::type Lines(LinesSEXP);
    Rcpp::traits::input_parameter< int >::type number_of_lines2(number_of_lines2SEXP);
    Rcpp::traits::input_parameter< List >::type Lines2(Lines2SEXP);
    Rcpp::traits::input_parameter< int >::type Dice_Terms(Dice_TermsSEXP);
    Rcpp::traits::input_parameter< bool >::type rem_duplicates(rem_duplicatesSEXP);
    rcpp_result_gen = Rcpp::wrap(Variable_Dice_Coefficients(number_of_lines, Lines, number_of_lines2, Lines2, Dice_Terms, rem_duplicates));
    return rcpp_result_gen;
END_RCPP
}
