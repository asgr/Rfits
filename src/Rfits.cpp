#include <algorithm>
#include <utility>
#include <vector>
#include <Rcpp.h>

#include "cfitsio/fitsio.h"

using namespace Rcpp;

std::runtime_error fits_status_to_exception(const char *func_name, int status)
{
    char err_msg[30];
    fits_get_errstatus(status, err_msg);
    std::ostringstream os;
    os << "Error when invoking fits_" << func_name << ": " << err_msg;
    throw std::runtime_error(os.str());
}

/**
 * Utility function to convert FITS API call errors into exceptions.
 *
 * Notably, this doesn't work with fits_open_file as this name is actually
 * defined as a macro function, so it cannot be just passed around.
 */
template <typename F, typename ... Args>
void _fits_invoke(const char *func_name, F&& func, Args&& ... args)
{
  int status = 0;
  func(std::forward<Args>(args)..., &status);
  if (status) {
    throw fits_status_to_exception(func_name, status);
  }
}

fitsfile *fits_safe_open_file(const char *filename, int mode)
{
  int status = 0;
  fitsfile *file;
  fits_open_file(&file, const_cast<char *>(filename), mode, &status);
  if (status) {
    throw fits_status_to_exception("open_file", status);
  }
  return file;
}

#define fits_invoke(F, ...) _fits_invoke(#F, fits_ ## F, __VA_ARGS__)

std::vector<char *> to_string_vector(const Rcpp::CharacterVector &strings)
{
  std::vector<char *> c_strings(strings.size());
  std::transform(strings.begin(), strings.end(), c_strings.begin(),
                 [](const Rcpp::String &string) {
                   return const_cast<char *>(string.get_cstring());
                 }
  );
  return c_strings;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix Rfits_read_img(Rcpp::String filename, int xpix=100, int ypix=100, int ext=1)
{
  int anynull, nullvals = 0;

  fitsfile *fptr;
  fits_invoke(open_image, &fptr, filename.get_cstring(), READONLY);
  fits_invoke(movabs_hdu, fptr, ext, &hdutype);
  
  int npixels = xpix * ypix;
  std::vector<float> pixels(npixels);
  fits_invoke(read_img, fptr, TFLOAT, 1, npixels, &nullvals, pixels.data(), &anynull);
  fits_invoke(close_file, fptr);

  NumericMatrix pixel_matrix(xpix, ypix);
  std::copy(pixels.begin(), pixels.end(), pixel_matrix.begin());
  return pixel_matrix;
}

// [[Rcpp::export]]
RcppExport SEXP Rfits_read_col(Rcpp::String filename, int colref=1, int ext=2){

  int hdutype,anynull,typecode,ii;
  long nrow,repeat,width;

  auto *fptr = fits_safe_open_file(filename.get_cstring(), READONLY);
  fits_invoke(movabs_hdu, fptr, ext, &hdutype);
  fits_invoke(get_num_rows, fptr, &nrow);
  fits_invoke(get_coltype, fptr, colref, &typecode, &repeat, &width);

  if ( typecode == TSTRING ) {
    int cwidth;
    fits_invoke(get_col_display_width, fptr, colref, &cwidth);

    char **data = (char **)malloc(sizeof(char *) * nrow);
    for (ii = 0 ; ii < nrow ; ii++ ) {
      data[ii] = (char*)malloc(cwidth);
    }
    fits_invoke(read_col, fptr, TSTRING, colref, 1, 1, nrow, nullptr, data, &anynull);
    Rcpp::StringVector out(nrow);
    std::copy(data, data + nrow, out.begin());
    for (int i = 0; i != nrow; i++) {
      free(data[i]);
    }
    free(data);
    fits_invoke(close_file, fptr);
    return out;
  }
  else if ( typecode == TBYTE ) {
    int nullval = 0;
    std::vector<Rbyte> col(nrow);
    fits_invoke(read_col, fptr, TBYTE, colref, 1, 1, nrow, &nullval, col.data(), &anynull);
    Rcpp::IntegerVector out(nrow);
    std::copy(col.begin(), col.end(), out.begin());
    fits_invoke(close_file, fptr);
    return out;
  }
  else if ( typecode == TINT ) {
    int nullval = -999;
    std::vector<int> col(nrow);
    fits_invoke(read_col, fptr, TINT, colref, 1, 1, nrow, &nullval, col.data(), &anynull);
    Rcpp::IntegerVector out(nrow);
    std::copy(col.begin(), col.end(), out.begin());
    fits_invoke(close_file, fptr);
    return out;
  }
  else if ( typecode == TUINT ) {
    unsigned int nullval = 0;
    std::vector<unsigned int> col(nrow);
    fits_invoke(read_col, fptr, TUINT, colref, 1, 1, nrow, &nullval, col.data(), &anynull);
    Rcpp::IntegerVector out(nrow);
    std::copy(col.begin(), col.end(), out.begin());
    fits_invoke(close_file, fptr);
    return out;
  }
  else if ( typecode == TINT32BIT ) {
    long nullval = 0;
    std::vector<long> col(nrow);
    fits_invoke(read_col, fptr, TINT32BIT, colref, 1, 1, nrow, &nullval, col.data(), &anynull);
    Rcpp::IntegerVector out(nrow);
    std::copy(col.begin(), col.end(), out.begin());
    fits_invoke(close_file, fptr);
    return out;
  }
  else if ( typecode == TSHORT ) {
    short nullval = -128;
    std::vector<short> col(nrow);
    fits_invoke(read_col, fptr, TSHORT, colref, 1, 1, nrow, &nullval, col.data(), &anynull);
    Rcpp::NumericVector out(nrow);
    std::copy(col.begin(), col.end(), out.begin());
    fits_invoke(close_file, fptr);
    return out;
  }
  else if ( typecode == TUSHORT ) {
    unsigned short nullval = 255;
    std::vector<unsigned short> col(nrow);
    fits_invoke(read_col, fptr, TUSHORT, colref, 1, 1, nrow, &nullval, col.data(), &anynull);
    Rcpp::NumericVector out(nrow);
    std::copy(col.begin(), col.end(), out.begin());
    fits_invoke(close_file, fptr);
    return out;
  }
  else if ( typecode == TFLOAT ) {
    float nullval = -999;
    std::vector<float> col(nrow);
    fits_invoke(read_col, fptr, TFLOAT, colref, 1, 1, nrow, &nullval, col.data(), &anynull);
    Rcpp::NumericVector out(nrow);
    std::copy(col.begin(), col.end(), out.begin());
    fits_invoke(close_file, fptr);
    return out;
  }
  else if ( typecode == TLONG ) {
    long nullval = -999;
    std::vector<long> col(nrow);
    fits_invoke(read_col, fptr, TLONG, colref, 1, 1, nrow, &nullval, col.data(), &anynull);
    Rcpp::NumericVector out(nrow);
    std::copy(col.begin(), col.end(), out.begin());
    fits_invoke(close_file, fptr);
    return out;
  }
  else if ( typecode == TLONGLONG ) {
    long nullval = -999;
    std::vector<int64_t> col(nrow);
    fits_invoke(read_col, fptr, TLONGLONG, colref, 1, 1, nrow, &nullval, col.data(), &anynull);
    Rcpp::NumericVector out(nrow);
    std::memcpy(&(out[0]), &(col[0]), nrow * sizeof(double));
    fits_invoke(close_file, fptr);
    out.attr("class") = "integer64";
    return out;
  }
  else if ( typecode == TDOUBLE ) {
    double nullval = -999;
    std::vector<double> col(nrow);
    fits_invoke(read_col, fptr, TDOUBLE, colref, 1, 1, nrow, &nullval, col.data(), &anynull);
    Rcpp::NumericVector out(nrow);
    std::copy(col.begin(), col.end(), out.begin());
    fits_invoke(close_file, fptr);
    return out;
  }

  throw std::runtime_error("unsupported type");
}

// [[Rcpp::export]]
int Rfits_read_nrow(Rcpp::String filename, int ext=2){
  int hdutype;
  long nrow;

  auto *fptr = fits_safe_open_file(filename.get_cstring(), READONLY);
  fits_invoke(movabs_hdu, fptr, ext, &hdutype);
  fits_invoke(get_num_rows, fptr, &nrow);
  fits_invoke(close_file, fptr);
  return nrow;
}

// [[Rcpp::export]]
int Rfits_read_ncol(Rcpp::String filename, int ext=2){
  int hdutype,ncol;

  auto *fptr = fits_safe_open_file(filename.get_cstring(), READONLY);
  fits_invoke(movabs_hdu, fptr, ext, &hdutype);
  fits_invoke(get_num_cols, fptr,&ncol);
  fits_invoke(close_file, fptr);
  return ncol;
}

// [[Rcpp::export]]
SEXP Rfits_read_colname(Rcpp::String filename, int colref=2, int ext=2){
  int hdutype, ncol;

  auto *fptr = fits_safe_open_file(filename.get_cstring(), READONLY);
  fits_invoke(movabs_hdu, fptr, ext, &hdutype);
  fits_invoke(get_num_cols, fptr, &ncol);

  Rcpp::StringVector out(ncol);
  char colname[9] = "";
  //std::string colname(9, '\0');

  int status = 0;
  int ii = 0;
  while ( status != COL_NOT_FOUND ) {
    fits_get_colname(fptr, CASEINSEN, "*", (char *)colname, &colref, &status);
    if (status != COL_NOT_FOUND) {
      out[ii] = colname;
    }
    ii++;
  }

  fits_invoke(close_file, fptr);
  return out;
}

// [[Rcpp::export]]
void Rfits_create_bintable(Rcpp::String filename, int tfields,
                         Rcpp::CharacterVector ttypes, Rcpp::CharacterVector tforms,
                         Rcpp::CharacterVector tunits, Rcpp::String extname)
{
  int hdutype;
  fitsfile *fptr;

  fits_invoke(create_file, &fptr, filename.get_cstring());
  fits_invoke(create_hdu, fptr);

  auto c_ttypes = to_string_vector(ttypes);
  auto c_tforms = to_string_vector(tforms);
  auto c_tunits = to_string_vector(tunits);
  fits_invoke(create_tbl, fptr, BINARY_TBL, 0, tfields,
              c_ttypes.data(), c_tforms.data(), c_tunits.data(),
              (char *)extname.get_cstring());
  fits_invoke(close_file, fptr);
}

// [[Rcpp::export]]
RcppExport SEXP Rfits_write_col(Rcpp::String filename, SEXP data, int nrow, int colref=1, int ext=2, int typecode = TDOUBLE){
  
  int hdutype,anynull,ii;
  long repeat,width;
  
  auto *fptr = fits_safe_open_file(filename.get_cstring(), READWRITE);
  fits_invoke(movabs_hdu, fptr, ext, &hdutype);

  if ( typecode == TSTRING ) {
    int cwidth;
    std::vector<char *> s_data(nrow);
    for (ii = 0 ; ii < nrow ; ii++ ) {
      s_data[ii] = (char*)CHAR(STRING_ELT(data, ii));
    }
    fits_invoke(write_col, fptr, typecode, colref, 1, 1, nrow, s_data.data());
  }else if (typecode == TINT){
    fits_invoke(write_col, fptr, typecode, colref, 1, 1, nrow, INTEGER(data));
  }else if(typecode == TLONGLONG){
    fits_invoke(write_col, fptr, typecode, colref, 1, 1, nrow, REAL(data));
  }else if(typecode == TDOUBLE){
    fits_invoke(write_col, fptr, typecode, colref, 1, 1, nrow, REAL(data));
  }
  
  fits_invoke(close_file, fptr);
}
