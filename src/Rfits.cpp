#include "cfitsio/fitsio.h"
#include <algorithm>
#include <vector>
#include <Rcpp.h>

using namespace Rcpp;

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
  fitsfile *fptr;
  int status = 0, anynull, nullvals = 0;

  fits_open_image(&fptr, filename.get_cstring(), READONLY, &status);
  if (status) {
    fits_report_error(stderr, status);
    throw std::runtime_error("cannot open file");
  }

  int npixels = xpix * ypix;
  std::vector<float> pixels(npixels);
  fits_read_img(fptr, TFLOAT, 1, npixels, &nullvals, pixels.data(), &anynull, &status);
  if (status) {
    fits_report_error(stderr, status);
    throw std::runtime_error("cant read image");
  }

  fits_close_file(fptr, &status);
  if (status) {
    fits_report_error(stderr, status);
    throw std::runtime_error("cant close file");
  }

  NumericMatrix pixel_matrix(xpix, ypix);
  std::copy(pixels.begin(), pixels.end(), pixel_matrix.begin());
  return pixel_matrix;
}

// [[Rcpp::export]]
Rcpp::NumericVector rfits_read_col(Rcpp::String filename, Rcpp::String column,Rcpp::IntegerVector cwidth,Rcpp::IntegerVector hdu)
{
  short prot=0;
  int err,status=0;
  int hdutype,anynull,typecode;
  long ii,nrows,width,repeat;
  int* colint=NULL;
  int colwidth=1;
  int* hduint;
  int* ischarint;
  double* coldatadbl;
  char* fname;
  char* colchar=NULL;

  fitsfile *fptr;

  fits_open_file(&fptr, filename.get_cstring(), READONLY, &status);
  if (status) {
    fits_report_error(stderr, status);
    throw std::runtime_error("cannot open file");
  }

  float nullval = -99;
  std::vector<float> col(100);
  fits_read_col(fptr, TFLOAT, 1, 1, 1, 100, &nullval, col.data(), &anynull, &status);
  if (status) {
    fits_report_error(stderr, status);
    throw std::runtime_error("cant read image");
  }

  fits_close_file(fptr, &status);
  if (status) {
    fits_report_error(stderr, status);
    throw std::runtime_error("cant close file");
  }

  Rcpp::NumericVector out(100);
  std::copy(col.begin(), col.end(), out.begin());

  return(out);
}

