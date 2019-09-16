// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// Rffrtnm
int Rffrtnm(Rcpp::String url, Rcpp::String rootname);
RcppExport SEXP _Rfits_Rffrtnm(SEXP urlSEXP, SEXP rootnameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::String >::type url(urlSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type rootname(rootnameSEXP);
    rcpp_result_gen = Rcpp::wrap(Rffrtnm(url, rootname));
    return rcpp_result_gen;
END_RCPP
}
// Rfits_read_img
Rcpp::NumericMatrix Rfits_read_img(Rcpp::String filename, int xpix, int ypix);
RcppExport SEXP _Rfits_Rfits_read_img(SEXP filenameSEXP, SEXP xpixSEXP, SEXP ypixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::String >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< int >::type xpix(xpixSEXP);
    Rcpp::traits::input_parameter< int >::type ypix(ypixSEXP);
    rcpp_result_gen = Rcpp::wrap(Rfits_read_img(filename, xpix, ypix));
    return rcpp_result_gen;
END_RCPP
}
// Rfits_read_col
RcppExport SEXP Rfits_read_col(Rcpp::String filename, int colref, int ext);
RcppExport SEXP _Rfits_Rfits_read_col(SEXP filenameSEXP, SEXP colrefSEXP, SEXP extSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::String >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< int >::type colref(colrefSEXP);
    Rcpp::traits::input_parameter< int >::type ext(extSEXP);
    rcpp_result_gen = Rcpp::wrap(Rfits_read_col(filename, colref, ext));
    return rcpp_result_gen;
END_RCPP
}
// Rfits_read_nrow
int Rfits_read_nrow(Rcpp::String filename, int ext);
RcppExport SEXP _Rfits_Rfits_read_nrow(SEXP filenameSEXP, SEXP extSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::String >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< int >::type ext(extSEXP);
    rcpp_result_gen = Rcpp::wrap(Rfits_read_nrow(filename, ext));
    return rcpp_result_gen;
END_RCPP
}
// Rfits_read_ncol
int Rfits_read_ncol(Rcpp::String filename, int ext);
RcppExport SEXP _Rfits_Rfits_read_ncol(SEXP filenameSEXP, SEXP extSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::String >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< int >::type ext(extSEXP);
    rcpp_result_gen = Rcpp::wrap(Rfits_read_ncol(filename, ext));
    return rcpp_result_gen;
END_RCPP
}
// Rfits_read_colname
SEXP Rfits_read_colname(Rcpp::String filename, int colref, int ext);
RcppExport SEXP _Rfits_Rfits_read_colname(SEXP filenameSEXP, SEXP colrefSEXP, SEXP extSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::String >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< int >::type colref(colrefSEXP);
    Rcpp::traits::input_parameter< int >::type ext(extSEXP);
    rcpp_result_gen = Rcpp::wrap(Rfits_read_colname(filename, colref, ext));
    return rcpp_result_gen;
END_RCPP
}
// Rfits_create_bintable
int Rfits_create_bintable(Rcpp::String filename, int tfields, Rcpp::CharacterVector ttype, Rcpp::CharacterVector tform, Rcpp::CharacterVector tunit, Rcpp::String extname, int ext);
RcppExport SEXP _Rfits_Rfits_create_bintable(SEXP filenameSEXP, SEXP tfieldsSEXP, SEXP ttypeSEXP, SEXP tformSEXP, SEXP tunitSEXP, SEXP extnameSEXP, SEXP extSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::String >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< int >::type tfields(tfieldsSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type ttype(ttypeSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type tform(tformSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type tunit(tunitSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type extname(extnameSEXP);
    Rcpp::traits::input_parameter< int >::type ext(extSEXP);
    rcpp_result_gen = Rcpp::wrap(Rfits_create_bintable(filename, tfields, ttype, tform, tunit, extname, ext));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_Rfits_Rffrtnm", (DL_FUNC) &_Rfits_Rffrtnm, 2},
    {"_Rfits_Rfits_read_img", (DL_FUNC) &_Rfits_Rfits_read_img, 3},
    {"_Rfits_Rfits_read_col", (DL_FUNC) &_Rfits_Rfits_read_col, 3},
    {"_Rfits_Rfits_read_nrow", (DL_FUNC) &_Rfits_Rfits_read_nrow, 2},
    {"_Rfits_Rfits_read_ncol", (DL_FUNC) &_Rfits_Rfits_read_ncol, 2},
    {"_Rfits_Rfits_read_colname", (DL_FUNC) &_Rfits_Rfits_read_colname, 3},
    {"_Rfits_Rfits_create_bintable", (DL_FUNC) &_Rfits_Rfits_create_bintable, 7},
    {NULL, NULL, 0}
};

RcppExport void R_init_Rfits(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
