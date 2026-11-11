#Check the Zarr image back-end, which mirrors the HDF5 one in Rfits_image_hdf5.R
#
#This is a trimmed critical subset of the Zarr checks: one representative assertion
#per behaviour that would lose or corrupt data if it broke. The exhaustive edge-case
#iterations (parallel/cluster equivalence, quantise mode matrices, live-bucket S3)
#live in the git history rather than here, since this package is mostly about FITS
#data and the Zarr suite should not dominate the run.
#
#Optional back ends are guarded with requireNamespace() rather than a top-level
#skip_if_not_installed(): this file has no test_that() blocks, so a skip at top level
#would abandon every expectation after it rather than one test.

skip_if_not_installed("zarr")

#Unique per run, so repeat runs cannot collide with existing stores
subdir = file.path(tempdir(), paste0("zarr_test_", sample(1e8, 1)))
dir.create(subdir)

library(Rfits)
library(testthat)
library(bit64)

context("Check Rfits Zarr image read/write")

keyvalues_1d = list(SIMPLE = TRUE, BITPIX = -32L, NAXIS = 1L, NAXIS1 = 10L,
                    CRPIX1 = 5.5, CRVAL1 = 100)
keyvalues_2d = list(SIMPLE = TRUE, BITPIX = -32L, NAXIS = 2L, NAXIS1 = 4L, NAXIS2 = 6L,
                    CRPIX1 = 2.5, CRPIX2 = 3.5, EXTNAME = 'data1')
keyvalues_3d = list(SIMPLE = TRUE, BITPIX = -32L, NAXIS = 3L, NAXIS1 = 4L, NAXIS2 = 5L,
                    NAXIS3 = 6L, CRPIX1 = 2.5, CRPIX2 = 3.5, CRPIX3 = 1.5)
keyvalues_4d = list(SIMPLE = TRUE, BITPIX = -64L, NAXIS = 4L, NAXIS1 = 4L, NAXIS2 = 5L,
                    NAXIS3 = 3L, NAXIS4 = 4L, CRPIX1 = 2.5, CRPIX2 = 3.5, CRPIX3 = 1.5,
                    CRPIX4 = 2.5)

make_comments = function(kv){
  setNames(lapply(names(kv), function(name) paste('comment for', name)), names(kv))
}

root_of = function(path){
  zarr::open_zarr(path, protocol = 'local', read_only = TRUE)$root$attributes
}

#ex 1 the shipped example image should survive a Zarr round trip
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)
file_image_zarr = file.path(subdir, 'example_image.zarr')
Rfits_write_image_zarr(temp_image, file_image_zarr, extname = 'image')
temp_image_zarr = Rfits_read_image_zarr(file_image_zarr, extname = 'image')
expect_identical(dim(temp_image$imDat), dim(temp_image_zarr$imDat))
expect_equal(temp_image$imDat, temp_image_zarr$imDat)
expect_equal(temp_image$header, temp_image_zarr$header)

#ex 2 1D/2D/3D/4D data, with the class of the output matching the dimensionality
data_1d = as.numeric(1:10)
data_2d = matrix(as.numeric(1:24), 4, 6)
data_3d = array(as.numeric(1:120), c(4, 5, 6))
data_4d = array(as.numeric(1:240), c(4, 5, 3, 4))

file_dims = file.path(subdir, 'dims.zarr')
Rfits_write_image_zarr(data_1d, file_dims, extname = 'data1', keyvalues = keyvalues_1d,
                       keycomments = make_comments(keyvalues_1d))
Rfits_write_image_zarr(data_2d, file_dims, extname = 'data2', keyvalues = keyvalues_2d,
                       keycomments = make_comments(keyvalues_2d))
Rfits_write_image_zarr(data_3d, file_dims, extname = 'data3', keyvalues = keyvalues_3d,
                       keycomments = make_comments(keyvalues_3d))
Rfits_write_image_zarr(data_4d, file_dims, extname = 'data4', keyvalues = keyvalues_4d,
                       keycomments = make_comments(keyvalues_4d))

read_1d = Rfits_read_image_zarr(file_dims, extname = 'data1')
read_2d = Rfits_read_image_zarr(file_dims, extname = 'data2')
read_3d = Rfits_read_image_zarr(file_dims, extname = 'data3')
read_4d = Rfits_read_image_zarr(file_dims, extname = 'data4')

expect_identical(vapply(list(read_1d, read_2d, read_3d, read_4d),
                        function(x) class(x)[1], character(1)),
                 c('Rfits_vector', 'Rfits_image', 'Rfits_cube', 'Rfits_array'))
expect_equal(read_1d$imDat, data_1d)
expect_equal(read_2d$imDat, data_2d)
expect_equal(read_3d$imDat, data_3d)
expect_equal(read_4d$imDat, data_4d)

#ex 3 header components are all present and consistent
expect_true(all(c('imDat', 'header', 'hdr', 'keyvalues', 'keycomments', 'keynames',
                  'comment', 'history', 'filename', 'ext', 'extname') %in% names(read_2d)))
expect_equal(read_2d$keycomments$NAXIS1, 'comment for NAXIS1')
#card images are always a whole number of 80 character records
expect_equal(nchar(read_2d$raw) %% 80, 0)

#ex 4 header = FALSE gives the bare array
expect_identical(Rfits_read_image_zarr(file_dims, extname = 'data2', header = FALSE), data_2d)

#ex 5 subsetting, checked against the equivalent R subset, with the reference pixels
#shifting to follow the cut
sub_2d = Rfits_read_image_zarr(file_dims, extname = 'data2', xlo = 2, xhi = 3, ylo = 2, yhi = 4)
expect_equal(sub_2d$imDat, data_2d[2:3, 2:4])
expect_equal(sub_2d$keyvalues$CRPIX1, keyvalues_2d$CRPIX1 - 2L + 1L)
sub_4d = Rfits_read_image_zarr(file_dims, extname = 'data4', xlo = 2, xhi = 3, ylo = 1, yhi = 4,
                               zlo = 2, zhi = 3, tlo = 1, thi = 2)
expect_equal(sub_4d$imDat, data_4d[2:3, 1:4, 2:3, 1:2])

#ex 6 partially and wholly out of bounds subsets pad with NA
sub_oob = Rfits_read_image_zarr(file_dims, extname = 'data2', xlo = -2, xhi = 6)
expect_equal(sub_oob$imDat[4:6, 1:3], data_2d[1:3, 1:3])
expect_true(all(is.na(sub_oob$imDat[1:3, ])))

#ex 7 extensions can be addressed by index as well as name
expect_equal(Rfits_read_image_zarr(file_dims, ext = 4)$extname, 'data4')

#ex 8 writing the same extension again replaces rather than duplicates it
file_replace = file.path(subdir, 'replace.zarr')
Rfits_write_image_zarr(data_2d, file_replace, extname = 'data1', keyvalues = keyvalues_2d)
Rfits_write_image_zarr(data_2d[, 1:3], file_replace, extname = 'data1', keyvalues = keyvalues_2d)
expect_equal(dim(Rfits_read_image_zarr(file_replace, extname = 'data1', header = FALSE)), c(4, 3))
expect_error(Rfits_write_image_zarr(data_2d, file_replace, extname = 'data1',
                                   create_ext = FALSE), 'already exists')

#ex 9 the data type round trips, including NA preservation
file_types = file.path(subdir, 'types.zarr')
data_dbl = matrix(c(1.5, NA, NaN, -1e300, 0, 1e-300, 9.97e36, 1e30, -5), 3, 3)
Rfits_write_image_zarr(data_dbl, file_types, extname = 'f64', data_type = 'float64')
expect_equal(Rfits_read_image_zarr(file_types, extname = 'f64', header = FALSE), data_dbl)

data_int = matrix(c(1L, NA, 3L, 4L, -2147483646L, 6L, 7L, 8L, 9L), 3, 3)
Rfits_write_image_zarr(data_int, file_types, extname = 'i32', data_type = 'int32')
read_int = Rfits_read_image_zarr(file_types, extname = 'i32', header = FALSE)
expect_identical(typeof(read_int), 'integer')
expect_equal(read_int, data_int)

#logical data defaults to int8 so that NA survives
data_lgl = matrix(c(TRUE, NA, FALSE, TRUE, FALSE, TRUE), 3, 2)
Rfits_write_image_zarr(data_lgl, file_types, extname = 'lgl')
expect_equal(Rfits_read_image_zarr(file_types, extname = 'lgl', header = FALSE),
             matrix(c(1L, NA, 0L, 1L, 0L, 1L), 3, 2))

#ex 10 integer64 is coerced to float64 with a message, since Zarr cannot write it
expect_message(Rfits_write_image_zarr(bit64::as.integer64(1:5), file_types, extname = 'i64'),
               'integer64')

#ex 11 chunking is honoured, and a subset read of a chunked array is still correct
file_chunk = file.path(subdir, 'chunked.zarr')
data_big = array(as.numeric(1:4000), c(10, 10, 40))
Rfits_write_image_zarr(data_big, file_chunk, extname = 'cube', chunk_shape = c(10, 10, 1))
expect_equal(Rfits_read_image_zarr(file_chunk, extname = 'cube', header = FALSE), data_big)
expect_equal(Rfits_read_image_zarr(file_chunk, extname = 'cube', zlo = 20, zhi = 25,
                                  header = FALSE), data_big[, , 20:25])

#ex 12 pointers read only what is asked of them
point = Rfits_point_zarr(file_dims, extname = 'data4')
expect_identical(class(point), 'Rfits_pointer_zarr')
expect_equal(dim(point), c(4, 5, 3, 4))
expect_equal(point[2:3, 1:4, 2:3, 1:2]$imDat, data_4d[2:3, 1:4, 2:3, 1:2])

#ex 13 ZIMAGE style (compressed image) headers update the ZNAXIS keys
file_zim = file.path(subdir, 'zimage.zarr')
keyvalues_zim = list(XTENSION = 'IMAGE ', BITPIX = 8L, NAXIS = 0L, ZIMAGE = TRUE,
                     ZBITPIX = -32L, ZNAXIS = 2L, ZNAXIS1 = 4L, ZNAXIS2 = 6L,
                     ZCRPIX1 = 1.5, ZCRPIX2 = 3.5)
Rfits_write_image_zarr(data_2d, file_zim, extname = 'zdata', keyvalues = keyvalues_zim)
read_zim = Rfits_read_image_zarr(file_zim, extname = 'zdata', xlo = 2, xhi = 3)
expect_equal(read_zim$keyvalues$ZNAXIS1, 2L)

#ex 14 bad input is rejected
expect_error(Rfits_read_image_zarr(file.path(subdir, 'definitely_not_a_store.zarr')))
expect_error(Rfits_write_image_zarr(array(as.numeric(1:720), c(2, 3, 4, 5, 6)), file_dims,
                                   extname = 'five'), '4 dimension')

#ex 15 the back end agrees with the HDF5 back end where both can be used
if(requireNamespace('hdf5r', quietly = TRUE)){
  file_hdf5 = file.path(subdir, 'compare.h5')
  file_zarr_cmp = file.path(subdir, 'compare.zarr')
  obj_2d = Rfits_create_image(data_2d, keyvalues = keyvalues_2d,
                             keycomments = make_comments(keyvalues_2d))
  Rfits_write_image_hdf5(obj_2d, file_hdf5, extname = 'data1')
  Rfits_write_image_zarr(obj_2d, file_zarr_cmp, extname = 'data1')
  from_hdf5 = Rfits_read_image_hdf5(file_hdf5, extname = 'data1')
  from_zarr = Rfits_read_image_zarr(file_zarr_cmp, extname = 'data1')
  expect_equal(from_hdf5$imDat, from_zarr$imDat)
  expect_equal(unclass(from_hdf5$keyvalues), unclass(from_zarr$keyvalues))
}

#ex 16 store objects, which is how remote stores are reached. A local store stands in
#for a remote one, so this needs no network or credentials
store_local = zarr::zarr_localstore$new(file_dims, read_only = TRUE)
via_store = Rfits_read_image_zarr(store_local, extname = 'data2')
expect_equal(via_store$imDat, data_2d)

#ex 17 writing to a store object with no root group yet bootstraps it (the shape a new
#remote store arrives in)
file_writable = file.path(subdir, 'store_writable.zarr')
fresh = zarr::zarr_localstore$new(file_writable, read_only = FALSE)
expect_error(Rfits_read_image_zarr(fresh), 'no root group')
Rfits_write_image_zarr(data_2d, fresh, extname = 'data2', keyvalues = keyvalues_2d)
expect_equal(Rfits_read_image_zarr(fresh, extname = 'data2', header = FALSE), data_2d)

#ex 18 an un-bootstrapped memory store, the shape a new remote store arrives in
mem = zarr::zarr_memorystore$new()
Rfits_write_image_zarr(data_3d, mem, extname = 'data3', keyvalues = keyvalues_3d)
expect_equal(Rfits_read_image_zarr(mem, extname = 'data3')$imDat, data_3d)

#ex 19 pointers over a store object re-read through the store rather than the filename.
#A memory store makes this checkable, since its filename label is not a path
point_mem = zarr::zarr_memorystore$new()
Rfits_write_image_zarr(data_4d, point_mem, extname = 'data4', keyvalues = keyvalues_4d)
ptr = Rfits_point_zarr(point_mem, extname = 'data4')
expect_equal(ptr[2:3, 1:4, 2:3, 1:2]$imDat, data_4d[2:3, 1:4, 2:3, 1:2])

#ex 20 pointer subsetting matches [.Rfits_image, which is what the FITS pointer mirrors
#too. Uses the shipped example image, since it has a real WCS
point_img = Rfits_point_zarr(file_image_zarr, extname = 'image')
point_fits = Rfits_point(file_image)
expect_equal(point_img[50:150, 50:150]$imDat, temp_image$imDat[50:150, 50:150])
#two values for i alone are a location to centre a box on, not a range
expect_identical(dim(point_img[c(50, 150)]$imDat), c(201L, 201L))
expect_equal(point_img[101, 101, box = 100]$imDat, temp_image$imDat[52:151, 52:151])
#a:end reads to the end of that dimension
expect_equal(point_img[50:end]$imDat, temp_image$imDat[50:356, ])

#ex 21 the FITS pointer gives the same answers for the same calls
expect_equal(point_img[50:150, 50:150]$imDat, point_fits[50:150, 50:150]$imDat)

#an array with no FITS metadata has no WCS to resolve against (used by ex 22 and ex 26)
file_nowcs = file.path(subdir, 'nowcs.zarr')
Rfits_write_image_zarr(data_2d, file_nowcs, extname = 'bare')

#ex 22 type = 'coord' resolves RA/Dec to pixels before cutting out
if(requireNamespace('Rwcs', quietly = TRUE)){
  coords = Rwcs::Rwcs_p2s(101, 101, temp_image$keyvalues)
  expect_equal(point_img[coords[1], coords[2], box = 100, type = 'coord']$imDat,
               point_img[101, 101, box = 100]$imDat)
}

#ex 23 collapse drops a trailing dimension that was sliced, and only one the caller
#actually asked for. Matches the documented [.Rfits_cube behaviour
file_cube_zarr = file.path(subdir, 'cube.zarr')
file_cube_fits = system.file('extdata', 'cube.fits', package = "Rfits")
temp_cube = Rfits_read_cube(file_cube_fits)
Rfits_write_image_zarr(temp_cube, file_cube_zarr, extname = 'cube')
point_cube = Rfits_point_zarr(file_cube_zarr, extname = 'cube')
expect_identical(class(point_cube[10:20, 10:20, 1])[1], 'Rfits_image')
expect_equal(dim(point_cube[10:20, 10:20, 1]$imDat), c(11L, 11L))
expect_equal(dim(point_cube[10:20, 10:20, 1, collapse = FALSE]$imDat), c(11L, 11L, 1L))
#and the FITS pointer collapses the same slice to an identical header
point_cube_fits = Rfits_point(file_cube_fits)
expect_equal(point_cube[10:20, 10:20, 1]$header, point_cube_fits[10:20, 10:20, 1]$header)

#ex 24 a 1D pointer keeps range meaning for c(a,b), since there is no box to centre
point_vec = Rfits_point_zarr(file_dims, extname = 'data1')
expect_equal(point_vec[c(3, 8)]$imDat, data_1d[3:8])

#ex 25 the WCS class methods work on a pointer, and agree with the same calls on the
#FITS pointer. The pointer carries the keywords but no raw header, so the fixed width
#form has to be rebuilt from them without loss
if(requireNamespace('Rwcs', quietly = TRUE)){
  expect_equal(centre(point_img), centre(point_fits))
  expect_identical(corners(point_img), corners(point_fits))
  expect_equal(pixscale(point_img), pixscale(point_fits))
  expect_equal(rotation(point_img), rotation(point_fits))
}

#ex 26 an array with no FITS metadata has no WCS to use, which is an error rather than
#an NA, since there is no way to tell the caller anything useful otherwise
point_nowcs = Rfits_point_zarr(file_nowcs, extname = 'bare')
expect_error(centre(point_nowcs), 'No FITS style metadata')

#ex 27 the store describes itself. Root attributes exist after a write, list that
#array, and agree with what the array actually is
file_root = file.path(subdir, 'root.zarr')
Rfits_write_image_zarr(data_2d, file_root, extname = 'alpha', keyvalues = keyvalues_2d,
                       compressor = 'lz4', clevel = 5L)
attrs = root_of(file_root)
expect_equal(attrs$convention, 'Rfits.zarr')
#recorded values are read off the array, so they must match the array itself
node_alpha = zarr::open_zarr(file_root, protocol = 'local', read_only = TRUE)$get_node('/alpha')
expect_equal(as.integer(node_alpha$shape), attrs$images_shape$alpha)
#a second array is added to the lists rather than replacing the first
Rfits_write_image_zarr(data_1d, file_root, extname = 'beta', keyvalues = keyvalues_1d)
expect_setequal(root_of(file_root)$images_array, c('alpha', 'beta'))

#ex 28 append grows dimension 1, the slowest varying axis in FITS, R and Zarr alike, so
#the existing elements keep their indices
file_app = file.path(subdir, 'append.zarr')
Rfits_write_image_zarr(data_2d, file_app, extname = 'data1', keyvalues = keyvalues_2d)
before = Rfits_read_image_zarr(file_app, extname = 'data1')
inc = matrix(as.numeric(101:118), 3, 6)
ap = Rfits_write_image_zarr(inc, file_app, extname = 'data1', append = TRUE)
expect_identical(c(ap$start_index, ap$end_index), c(5L, 7L))
after = Rfits_read_image_zarr(file_app, extname = 'data1')
expect_equal(after$imDat[1:4, ], before$imDat)
expect_equal(after$imDat[5:7, ], inc)
expect_equal(after$keyvalues$NAXIS1, 7L)

#ex 29 the append guards
expect_error(Rfits_write_image_zarr(matrix(as.numeric(1:15), 3, 5), file_app,
                                    extname = 'data1', append = TRUE), 'dimensions')
expect_error(Rfits_write_image_zarr(inc, file_app, extname = 'nope', append = TRUE),
             'non-existent')

#ex 30 compressor mapping, and the recorded level is the applied level
file_comp = file.path(subdir, 'codecs.zarr')
Rfits_write_image_zarr(data_2d, file_comp, extname = 'c_lz4', compressor = 'lz4', clevel = 9L)
expect_equal(root_of(file_comp)$compressor$c_lz4, 'lz4')
expect_equal(Rfits_read_image_zarr(file_comp, extname = 'c_lz4', header = FALSE), data_2d)

#ex 31 Rfits_inspect_zarr reports what it can state truthfully, and a store with no
#self-description is reported as such rather than guessed at
info = Rfits_inspect_zarr(file_root, print = FALSE)
expect_identical(info$n_arrays, 2L)
file_older = file.path(subdir, 'older.zarr')
Rfits_write_image_zarr(data_2d, file_older, extname = 'data1', keyvalues = keyvalues_2d,
                       update_root = FALSE)
expect_false(Rfits_inspect_zarr(file_older, print = FALSE)$self_describing)

#ex 32 the store name follows the file name, with every accepted extension removed
expect_identical(Rfits:::.zarr_store_stub('y.fits.gz'), 'y.zarr')

# --- Batch conversion of a directory of FITS files (Rfits_dir_to_zarr) -----------

#ex 33 a directory batch, one store per file, all using the same extension name
batch_dir = file.path(subdir, 'batch_in')
dir.create(batch_dir, recursive = TRUE)
batch_out = file.path(subdir, 'batch_out')
ex_image = system.file('extdata', 'image.fits', package = "Rfits")
ex_cube = system.file('extdata', 'cube.fits', package = "Rfits")
file.copy(ex_image, file.path(batch_dir, 'image_one.fits'))
file.copy(ex_cube, file.path(batch_dir, 'cube_one.fits'))
#not FITS, so it must be filtered out rather than attempted
writeLines('nothing here', file.path(batch_dir, 'notes.txt'))
batch = Rfits_dir_to_zarr(dir = batch_dir, target = batch_out, verbose = FALSE)
expect_true(all(batch$status == 'ok'))
#the pixels and header survive the round trip
src_image = Rfits_read_image(ex_image)
back_image = Rfits_read_image_zarr(file.path(batch_out, 'image_one.zarr'), extname = 'data1')
expect_equal(back_image$imDat, src_image$imDat)
expect_equal(back_image$header, src_image$header)

#ex 34 a file that cannot be read is recorded and skipped, not fatal
mixed = Rfits_dir_to_zarr(filelist = c(file.path(batch_dir, 'image_one.fits'),
                                       file.path(batch_dir, 'missing.fits')),
                          target = batch_out, verbose = FALSE)
expect_identical(mixed$status, c('ok', 'error'))

#ex 35 the destination must be given, and only one way
expect_error(Rfits_dir_to_zarr(dir = batch_dir), 'target or bucket')

# --- Searching a directory of stores by position (Rfits_cutout_zarr_dir) -----------

#ex 36 a 3x3 grid of 200x200 tiles at 1 arcsec/pixel, built straight into Zarr with a
#known WCS so the expected cutout can be computed rather than eyeballed. The tiles
#touch exactly, which puts centres and corners in reach. Writing needs no WCS package.
cut_dir = file.path(subdir, 'cutout_grid')
dir.create(cut_dir, recursive = TRUE)
cut_N = 200
cut_scale = 1/3600
cut_ra0 = 53.1
cut_dec0 = -27.8
cut_tile = function(it, jt, N = cut_N, scale = cut_scale){
  kv = list(SIMPLE = TRUE, BITPIX = -32L, NAXIS = 2L, NAXIS1 = N, NAXIS2 = N,
            CTYPE1 = 'RA---TAN', CTYPE2 = 'DEC--TAN',
            CRVAL1 = cut_ra0 + (it - 1) * N * scale / cos(cut_dec0 * pi/180),
            CRVAL2 = cut_dec0 + (jt - 1) * N * scale,
            CRPIX1 = (N + 1)/2, CRPIX2 = (N + 1)/2,
            CD1_1 = -scale, CD1_2 = 0, CD2_1 = 0, CD2_2 = scale,
            RADESYS = 'ICRS', EQUINOX = 2000)
  #Values encode the tile index, so a wrong store cannot pass for a right one
  dat = matrix(it * 1000 + jt, N, N)
  list(kv = kv, dat = dat)
}
for(it in 1:3){
  for(jt in 1:3){
    tt = cut_tile(it, jt)
    Rfits_write_image_zarr(tt$dat, file.path(cut_dir, sprintf('tile_%i_%i.zarr', it, jt)),
                           extname = 'data1', keyvalues = tt$kv)
  }
}
cen_22 = c(cut_ra0 + cut_N * cut_scale / cos(cut_dec0 * pi/180),
           cut_dec0 + cut_N * cut_scale)
corner = c(cut_ra0 + 0.5 * cut_N * cut_scale / cos(cut_dec0 * pi/180),
           cut_dec0 + 0.5 * cut_N * cut_scale)

if(requireNamespace('Rwcs', quietly = TRUE)){
  cut_silent = function(...){
    Rfits_cutout_zarr_dir(..., verbose = FALSE)
  }

  #the centre of one tile matches that tile alone, and the output is the same object
  #the pointer cutout method gives for the same request
  one = cut_silent(dir = cut_dir, RA = cen_22[1], Dec = cen_22[2], box = 101)
  expect_identical(names(one), 'tile_2_2')
  ptr_22 = Rfits_point_zarr(file.path(cut_dir, 'tile_2_2.zarr'), extname = 'data1')
  direct_22 = ptr_22[cen_22[1], cen_22[2], box = 101, type = 'coord']
  expect_equal(one[[1]]$imDat, direct_22$imDat)
  #a position on the corner shared by four tiles returns all four
  four = cut_silent(dir = cut_dir, RA = corner[1], Dec = corner[2], box = 101)
  expect_identical(sort(names(four)), sort(c('tile_1_1', 'tile_1_2', 'tile_2_1', 'tile_2_2')))
  #the scan table records every store it looked at
  expect_identical(nrow(attributes(four)$scan), 9L)

  #ex 37 extract = FALSE reports the matches as pointers, without reading pixels
  pts = cut_silent(dir = cut_dir, RA = corner[1], Dec = corner[2], box = 101, extract = FALSE)
  expect_true(all(vapply(pts, function(x) inherits(x, 'Rfits_pointer_zarr'), logical(1))))

  #ex 38 cubes keep their planes: a box is a two dimensional feature
  cube_dir = file.path(subdir, 'cutout_nd')
  dir.create(cube_dir)
  tt = cut_tile(1, 1)
  cube_dat = array(0, c(cut_N, cut_N, 4))
  for(k in 1:4){
    cube_dat[,,k] = tt$dat + k
  }
  Rfits_write_image_zarr(cube_dat, file.path(cube_dir, 'cube_a.zarr'), extname = 'data1',
                         keyvalues = tt$kv)
  nd = cut_silent(dir = cube_dir, RA = tt$kv$CRVAL1, Dec = tt$kv$CRVAL2, box = 51)
  expect_identical(dim(nd[['cube_a']]), c(51L, 51L, 4L))

  #ex 39 argument checking happens before anything is read from disk
  expect_error(Rfits_cutout_zarr_dir(RA = 1, Dec = 1), 'One of dir, filelist, or bucket')

  #ex 40 the metadata cache gives exactly the same answer as not having it
  if(requireNamespace('arrow', quietly = TRUE)){
    cache_file = file.path(subdir, 'cutout_cache.rds')
    cold = cut_silent(dir = cut_dir, RA = corner[1], Dec = corner[2], box = 101,
                      cache = cache_file)
    warm = cut_silent(dir = cut_dir, RA = corner[1], Dec = corner[2], box = 101,
                      cache = cache_file)
    expect_identical(names(cold), names(warm))
    expect_identical(sort(names(warm)), sort(names(four)))
  }
}

# --- Lossy (scaled integer) Zarr writes ----------------------------------------

#ex 41 a float image stored as scaled integers reads back as physical values, with the
#error bounded by half the quantisation step and the scale surviving the card round trip
file_lossy = file.path(subdir, 'lossy.zarr')
set.seed(4917)
data_flt = matrix(rnorm(400), 20, 20)
w_lossy = suppressMessages(Rfits_write_image_zarr(data_flt, file_lossy, extname = 'q',
                                                  lossy = TRUE))
expect_equal(w_lossy$data_type, 'int16')
read_lossy = Rfits_read_image_zarr(file_lossy, extname = 'q')
expect_lt(max(abs(read_lossy$imDat - data_flt)), w_lossy$bscale/2 * 1.01)
expect_equal(read_lossy$keyvalues$BSCALE, w_lossy$bscale, tolerance = 1e-13)
#physical = FALSE is the stored integers, and they reconstruct the physical values
raw_lossy = Rfits_read_image_zarr(file_lossy, extname = 'q', header = FALSE, physical = FALSE)
expect_equal(as.numeric(w_lossy$bzero + w_lossy$bscale * raw_lossy),
             as.numeric(read_lossy$imDat))

#ex 42 whole numbers need no scaling at all: the smallest exact type is taken
data_whl = matrix(as.numeric(sample(-50:50, 400, replace = TRUE)), 20, 20)
w_whl = suppressMessages(Rfits_write_image_zarr(data_whl, file.path(subdir, 'lossy_whole.zarr'),
                                                 extname = 'q', lossy = TRUE))
expect_false(w_whl$quantised)

#ex 43 missing pixels survive a lossy round trip exactly, since the usable integer span
#excludes the fill value by construction
data_na = data_flt
data_na[1:50] = NA
suppressMessages(Rfits_write_image_zarr(data_na, file.path(subdir, 'lossy_na.zarr'),
                                         extname = 'q', lossy = TRUE, data_type = 'int16'))
read_na = Rfits_read_image_zarr(file.path(subdir, 'lossy_na.zarr'), extname = 'q')
expect_identical(is.na(read_na$imDat), is.na(data_na))

#ex 44 a pointer cutout of a quantised array reads scaled values, not stored integers
point_q = Rfits_point_zarr(file_lossy, extname = 'q')
expect_equal(point_q[3:8, 4:9]$imDat, read_lossy$imDat[3:8, 4:9])

#ex 45 a float array carrying a scale in its header is physical data already, so it is
#never scaled a second time on read (a FITS reader applies BSCALE itself)
kv_pin = keyvalues_2d
kv_pin$BSCALE = 2
kv_pin$BZERO = 10
file_copy = file.path(subdir, 'lossy_copyheader.zarr')
suppressMessages(Rfits_write_image_zarr(data_flt, file_copy, extname = 'phys',
                                         keyvalues = kv_pin))
expect_equal(as.numeric(Rfits_read_image_zarr(file_copy, extname = 'phys')$imDat),
             as.numeric(data_flt))

#ex 46 qlevel sets the step from the noise rather than the range, mad()/qlevel
set.seed(30407)
noise = matrix(rnorm(40000, mean = 7, sd = 0.02), 200, 200)
w_qlevel = suppressMessages(Rfits_write_image_zarr(noise, file.path(subdir, 'lossy_qlevel.zarr'),
                                                   extname = 'q', qlevel = 4))
expect_equal(w_qlevel$bscale, mad(noise)/4, tolerance = 1e-12)

# --- The remote half, offline. A fake client backed by local bytes lets the S3 logic
# --- be checked without a network or credentials -------------------------------

#ex 47 a directory of stores is found in one delimited listing, with no GET at all, and
#the header of one array is one small GET
make_dir_client = function(root){
  files = list.files(root, recursive = TRUE, include.dirs = FALSE, all.files = TRUE)
  keys = setNames(as.list(file.path(root, files)), files)
  state = new.env(parent = emptyenv())
  state$get = 0
  state$list = 0
  client = list(
    head_object = function(Bucket, Key, ...){
      if(Key %in% names(keys)){
        return(list(ContentLength = file.info(keys[[Key]])$size))
      }
      stop('The specified key does not exist. (Service: NoSuchKey; Status Code: 404)',
           call. = FALSE)
    },
    get_object = function(Bucket, Key, ...){
      state$get = state$get + 1
      if(Key %in% names(keys)){
        size = file.info(keys[[Key]])$size
        return(list(Body = readBin(keys[[Key]], what = 'raw', n = as.integer(size))))
      }
      stop('The specified key does not exist. (Service: NoSuchKey; Status Code: 404)',
           call. = FALSE)
    },
    list_objects_v2 = function(Bucket, Prefix, Delimiter = NULL, MaxKeys = 1000, ...){
      state$list = state$list + 1
      found = names(keys)[startsWith(names(keys), Prefix)]
      out = list(Contents = lapply(found, function(k) list(Key = k)), IsTruncated = FALSE)
      if(!is.null(Delimiter)){
        rest = sub(paste0('^', Prefix), '', found)
        sub_dir = sub('/.*', '', rest)
        sub_dir = unique(sub_dir[nzchar(sub_dir) & sub_dir != rest])
        out$CommonPrefixes = lapply(sub_dir, function(p) list(Prefix = paste0(Prefix, p, '/')))
      }
      return(out)
    },
    counts = function() list(get = state$get, list = state$list))
  return(client)
}
disc = make_dir_client(cut_dir)
stores = Rfits:::.zarr_s3_find_stores(disc, 'bucket', '')
expect_identical(stores, sort(sprintf('tile_%i_%i.zarr', rep(1:3, each = 3), rep(1:3, 3))))
expect_identical(disc$counts(), list(get = 0, list = 1))
meta = Rfits:::.zarr_meta_from_s3(make_dir_client(cut_dir), 'bucket', 'tile_2_2.zarr', 'data1')
expect_equal(meta$keyvalues$CRVAL1, cut_tile(2, 2)$kv$CRVAL1, tolerance = 1e-6)

#ex 48 the root group bytes are valid v3 with attributes as an empty object, not the []
#that jsonlite produces for an empty list
expect_identical(Rfits:::.zarr_root_group_bytes(),
                 charToRaw('{"zarr_format":3,"node_type":"group","attributes":{}}'))

#ex 49 missing and unreadable must not be confused: a store behind credentials that are
#not allowed to see it must not be taken for a free prefix that is safe to write over
not_found = Rfits:::.zarr_s3_not_found
expect_true(not_found('The specified key does not exist. (Service: NoSuchKey; Status Code: 404)'))
expect_false(not_found('Access Denied (HTTP 403)'))

#ex 50 the probe distinguishes absent from unreadable, against a stand in client
make_fake_s3 = function(objects = list(), put_error = NULL){
  state = new.env(parent = emptyenv())
  state$objects = objects
  state$puts = list()
  client = list(
    head_object = function(Bucket, Key, ...){
      hit = state$objects[[Key]]
      if(isTRUE(hit)){
        return(list(ContentLength = 53))
      }
      if(is.character(hit)){
        stop(hit, call. = FALSE)
      }
      stop('The specified key does not exist. (Service: NoSuchKey; Status Code: 404)',
           call. = FALSE)
    },
    put_object = function(Bucket, Key, Body, ...){
      if(!is.null(put_error)){
        stop(put_error, call. = FALSE)
      }
      state$puts[[length(state$puts) + 1]] = list(Bucket = Bucket, Key = Key,
                                                  body = rawToChar(Body))
      invisible(list(ETag = '"fake"'))
    })
  attr(client, 'state') = state
  return(client)
}
probe = Rfits:::.zarr_s3_probe
expect_identical(probe(make_fake_s3(), 'b', 'x.zarr/zarr.json'),
                 list(exists = FALSE, message = NULL))
expect_identical(probe(make_fake_s3(objects = list('x.zarr/zarr.json' = 'Access Denied (HTTP 403)')),
                       'b', 'x.zarr/zarr.json'),
                 list(exists = NA, message = 'Access Denied (HTTP 403)'))

#ex 51 creating the root group writes the normalised key, and refuses to rebuild a
#prefix that is already a store
create_root = Rfits:::.zarr_s3_root_create
client = make_fake_s3()
create_root(client, 'image-tests', 'cutout_test.zarr')
expect_identical(attr(client, 'state')$puts[[1]]$Key, 'cutout_test.zarr/zarr.json')
exists_client = make_fake_s3(objects = list('x.zarr/zarr.json' = TRUE))
expect_error(create_root(exists_client, 'b', 'x.zarr'), 'already exists')

#ex 52 the public function rejects a call that cannot possibly work before it touches
#the network: without keys zarr_s3store silently goes anonymous
expect_error(Rfits_s3_store_new('image-tests', 'x.zarr'),
             'access_key and secret_key are required')