#include "cfitsio/fitsio.h"
#include <algorithm>
#include <utility>
#include <vector>
#include <Rcpp.h>

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
  int status;
  fitsfile *file;
  fits_open_file(&file, const_cast<char *>(filename), mode, &status);
  if (status) {
    throw fits_status_to_exception("open_file", status);
  }
  return file;
}

#define fits_invoke(F, ...) _fits_invoke(#F, fits_ ## F, __VA_ARGS__)

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
int Rffrtnm(Rcpp::String url, Rcpp::String rootname){
  int status;
  int temp = ffrtnm((char *)url.get_cstring(), (char *)rootname.get_cstring(), &status);
  return temp;
};

// [[Rcpp::export]]
Rcpp::NumericMatrix Rfits_read_img(Rcpp::String filename, int xpix=100, int ypix=100)
{
  int anynull, nullvals = 0;

  fitsfile *fptr;
  fits_invoke(open_image, &fptr, filename.get_cstring(), READONLY);

  int npixels = xpix * ypix;
  std::vector<float> pixels(npixels);
  fits_invoke(read_img, fptr, TFLOAT, 1, npixels, &nullvals, pixels.data(), &anynull);
  fits_invoke(close_file, fptr);

  NumericMatrix pixel_matrix(xpix, ypix);
  std::copy(pixels.begin(), pixels.end(), pixel_matrix.begin());
  return pixel_matrix;
}

// [[Rcpp::export]]
RcppExport SEXP Rfits_read_col(Rcpp::String filename, int colref=2, int ext=2){

  int hdutype,anynull,typecode,ii;
  long nrows,repeat,width;

  auto *fptr = fits_safe_open_file(filename.get_cstring(), READONLY);
  fits_invoke(movabs_hdu, fptr, ext, &hdutype);
  fits_invoke(get_num_rows, fptr, &nrows);
  fits_invoke(get_coltype, fptr, colref, &typecode, &repeat, &width);

  if ( typecode == TSTRING ) {
    int cwidth;
    fits_invoke(get_col_display_width, fptr, colref, &cwidth);

    char **data = (char **)malloc(sizeof(char *) * nrows);
    for (ii = 0 ; ii < nrows ; ii++ ) {
      data[ii] = (char*)malloc(cwidth);
    }
    fits_invoke(read_col, fptr, TSTRING, colref, 1, 1, nrows, nullptr, data, &anynull);
    Rcpp::StringVector out(nrows);
    std::copy(data, data + nrows, out.begin());
    for (int i = 0; i != nrows; i++) {
      delete data[i];
    }
    delete [] data;
    fits_invoke(close_file, fptr);
    return out;
  }
  else if ( typecode == TBYTE ) {
    int nullval = 0;
    std::vector<Rbyte> col(nrows);
    fits_invoke(read_col, fptr, TBYTE, colref, 1, 1, nrows, &nullval, col.data(), &anynull);
    Rcpp::IntegerVector out(nrows);
    std::copy(col.begin(), col.end(), out.begin());
    fits_invoke(close_file, fptr);
    return out;
  }
  else if ( typecode == TINT ) {
    int nullval = -999;
    std::vector<int> col(nrows);
    fits_invoke(read_col, fptr, TINT, colref, 1, 1, nrows, &nullval, col.data(), &anynull);
    Rcpp::IntegerVector out(nrows);
    std::copy(col.begin(), col.end(), out.begin());
    fits_invoke(close_file, fptr);
    return out;
  }
  else if ( typecode == TUINT ) {
    unsigned int nullval = 0;
    std::vector<unsigned int> col(nrows);
    fits_invoke(read_col, fptr, TUINT, colref, 1, 1, nrows, &nullval, col.data(), &anynull);
    Rcpp::IntegerVector out(nrows);
    std::copy(col.begin(), col.end(), out.begin());
    fits_invoke(close_file, fptr);
    return out;
  }
  else if ( typecode == TSHORT ) {
    short nullval = -128;
    std::vector<short> col(nrows);
    fits_invoke(read_col, fptr, TSHORT, colref, 1, 1, nrows, &nullval, col.data(), &anynull);
    Rcpp::NumericVector out(nrows);
    std::copy(col.begin(), col.end(), out.begin());
    fits_invoke(close_file, fptr);
    return out;
  }
  else if ( typecode == TUSHORT ) {
    unsigned short nullval = 255;
    std::vector<unsigned short> col(nrows);
    fits_invoke(read_col, fptr, TUSHORT, colref, 1, 1, nrows, &nullval, col.data(), &anynull);
    Rcpp::NumericVector out(nrows);
    std::copy(col.begin(), col.end(), out.begin());
    fits_invoke(close_file, fptr);
    return out;
  }
  else if ( typecode == TFLOAT ) {
    float nullval = -999;
    std::vector<float> col(nrows);
    fits_invoke(read_col, fptr, TFLOAT, colref, 1, 1, nrows, &nullval, col.data(), &anynull);
    Rcpp::NumericVector out(nrows);
    std::copy(col.begin(), col.end(), out.begin());
    fits_invoke(close_file, fptr);
    return out;
  }
  else if ( typecode == TLONG ) {
    long nullval = -999;
    std::vector<long> col(nrows);
    fits_invoke(read_col, fptr, TLONG, colref, 1, 1, nrows, &nullval, col.data(), &anynull);
    Rcpp::NumericVector out(nrows);
    std::copy(col.begin(), col.end(), out.begin());
    fits_invoke(close_file, fptr);
    return out;
  }
  else if ( typecode == TLONGLONG ) {
    long nullval = -999;
    std::vector<int64_t> col(nrows);
    fits_invoke(read_col, fptr, TLONGLONG, colref, 1, 1, nrows, &nullval, col.data(), &anynull);
    Rcpp::NumericVector out(nrows);
    std::memcpy(&(out[0]), &(col[0]), nrows * sizeof(double));
    fits_invoke(close_file, fptr);
    out.attr("class") = "integer64";
    return out;
  }
  else if ( typecode == TDOUBLE ) {
    double nullval = -999;
    std::vector<double> col(nrows);
    fits_invoke(read_col, fptr, TDOUBLE, colref, 1, 1, nrows, &nullval, col.data(), &anynull);
    Rcpp::NumericVector out(nrows);
    std::copy(col.begin(), col.end(), out.begin());
    fits_invoke(close_file, fptr);
    return out;
  }

  throw std::runtime_error("unsupported type");
}

// [[Rcpp::export]]
int Rfits_read_nrow(Rcpp::String filename, int ext=2){
  int hdutype;
  long nrows;

  auto *fptr = fits_safe_open_file(filename.get_cstring(), READONLY);
  fits_invoke(movabs_hdu, fptr, ext, &hdutype);
  fits_invoke(get_num_rows, fptr, &nrows);
  fits_invoke(close_file, fptr);
  return nrows;
}

// [[Rcpp::export]]
int Rfits_read_ncol(Rcpp::String filename, int ext=2){
  int hdutype,ncols;

  auto *fptr = fits_safe_open_file(filename.get_cstring(), READONLY);
  fits_invoke(movabs_hdu, fptr, ext, &hdutype);
  fits_invoke(get_num_cols, fptr,&ncols);
  fits_invoke(close_file, fptr);
  return ncols;
}

// [[Rcpp::export]]
SEXP Rfits_read_colname(Rcpp::String filename, int colref=2, int ext=2){
  int hdutype, ncols;

  auto *fptr = fits_safe_open_file(filename.get_cstring(), READONLY);
  fits_invoke(movabs_hdu, fptr, ext, &hdutype);
  fits_invoke(get_num_cols, fptr, &ncols);

  Rcpp::StringVector out(ncols);
  std::string colname(9, '\0');

  int status = 0;
  int ii = 0;
  while ( status != COL_NOT_FOUND ) {
    fits_get_colname(fptr, CASEINSEN, "*", (char *)colname.data(), &colref, &status);
    if (status != COL_NOT_FOUND) {
      out[ii] = colname;
    }
    ii++;
  }

  fits_invoke(close_file, fptr);
  return out;
}

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
void Rfits_create_bintable(Rcpp::String filename, int tfields,
                         Rcpp::CharacterVector ttypes, Rcpp::CharacterVector tforms,
                         Rcpp::CharacterVector tunits, Rcpp::String extname, int ext)
{
  int hdutype;
  fitsfile *fptr;

  fits_invoke(create_file, &fptr, filename.get_cstring());
  fits_invoke(create_hdu, fptr);
  fits_invoke(movabs_hdu, fptr, ext, &hdutype);

  auto c_ttypes = to_string_vector(ttypes);
  auto c_tforms = to_string_vector(tforms);
  auto c_tunits = to_string_vector(tunits);
  fits_invoke(create_tbl, fptr, BINARY_TBL, 0, 1,
              c_ttypes.data(), c_tforms.data(), c_tunits.data(),
              (char *)extname.get_cstring());
  fits_invoke(close_file, fptr);
}
