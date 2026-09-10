#Check the Zarr image back-end, which mirrors the HDF5 one in Rfits_image_hdf5.R

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

#ex 1 the shipped example image should survive a Zarr round trip
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)
file_image_zarr = file.path(subdir, 'example_image.zarr')
Rfits_write_image_zarr(temp_image, file_image_zarr, extname = 'image')
temp_image_zarr = Rfits_read_image_zarr(file_image_zarr, extname = 'image')
expect_identical(dim(temp_image$imDat), dim(temp_image_zarr$imDat))
expect_equal(temp_image$imDat, temp_image_zarr$imDat)
expect_equal(temp_image$keyvalues$NAXIS1, temp_image_zarr$keyvalues$NAXIS1)
expect_equal(temp_image$keyvalues$NAXIS2, temp_image_zarr$keyvalues$NAXIS2)
expect_equal(temp_image$keyvalues$CRPIX1, temp_image_zarr$keyvalues$CRPIX1)
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

expect_identical(class(read_1d)[1], 'Rfits_vector')
expect_identical(class(read_2d)[1], 'Rfits_image')
expect_identical(class(read_3d)[1], 'Rfits_cube')
expect_identical(class(read_4d)[1], 'Rfits_array')

expect_equal(read_1d$imDat, data_1d)
expect_equal(read_2d$imDat, data_2d)
expect_equal(read_3d$imDat, data_3d)
expect_equal(read_4d$imDat, data_4d)

#ex 3 header components are all present and consistent
expect_true(all(c('imDat', 'header', 'hdr', 'keyvalues', 'keycomments', 'keynames',
                  'comment', 'history', 'filename', 'ext', 'extname') %in% names(read_2d)))
expect_equal(read_2d$keyvalues$NAXIS1, 4L)
expect_equal(read_2d$keyvalues$NAXIS2, 6L)
expect_equal(read_2d$keycomments$NAXIS1, 'comment for NAXIS1')
expect_equal(read_2d$extname, 'data2')
expect_identical(read_2d$keynames, names(read_2d$keyvalues))
#card images are always a whole number of 80 character records
expect_equal(nchar(read_2d$raw) %% 80, 0)

#ex 4 header = FALSE gives the bare array
expect_identical(Rfits_read_image_zarr(file_dims, extname = 'data2', header = FALSE), data_2d)
expect_equal(dim(Rfits_read_image_zarr(file_dims, extname = 'data3', header = FALSE)),
             c(4, 5, 6))

#ex 5 subsetting, checked against the equivalent R subset
sub_2d = Rfits_read_image_zarr(file_dims, extname = 'data2', xlo = 2, xhi = 3, ylo = 2, yhi = 4)
expect_equal(dim(sub_2d$imDat), c(2, 3))
expect_equal(sub_2d$imDat, data_2d[2:3, 2:4])
#reference pixels shift with the cut, and the new sizes are recorded
expect_equal(sub_2d$keyvalues$NAXIS1, 2L)
expect_equal(sub_2d$keyvalues$NAXIS2, 3L)
expect_equal(sub_2d$keyvalues$CRPIX1, keyvalues_2d$CRPIX1 - 2L + 1L)
expect_equal(sub_2d$keyvalues$CRPIX2, keyvalues_2d$CRPIX2 - 2L + 1L)
expect_true(any(grepl('SUBMOD', sub_2d$header)))

sub_4d = Rfits_read_image_zarr(file_dims, extname = 'data4', xlo = 2, xhi = 3, ylo = 1, yhi = 4,
                               zlo = 2, zhi = 3, tlo = 1, thi = 2)
expect_equal(dim(sub_4d$imDat), c(2, 4, 2, 2))
expect_equal(sub_4d$imDat, data_4d[2:3, 1:4, 2:3, 1:2])
expect_equal(sub_4d$keyvalues$CRPIX3, keyvalues_4d$CRPIX3 - 2L + 1L)
expect_equal(sub_4d$keyvalues$CRPIX4, keyvalues_4d$CRPIX4 - 1L + 1L)

#ex 6 partially and wholly out of bounds subsets pad with NA
sub_oob = Rfits_read_image_zarr(file_dims, extname = 'data2', xlo = -2, xhi = 6)
expect_equal(dim(sub_oob$imDat), c(9, 6))
expect_equal(sub_oob$imDat[4:6, 1:3], data_2d[1:3, 1:3])
expect_true(all(is.na(sub_oob$imDat[1:3, ])))

sub_far = Rfits_read_image_zarr(file_dims, extname = 'data2', xlo = 100, xhi = 105)
expect_equal(dim(sub_far$imDat), c(6, 6))
expect_true(all(is.na(sub_far$imDat)))

#ex 7 extensions can be addressed by index as well as name
expect_equal(Rfits_read_image_zarr(file_dims, ext = 1)$extname, 'data1')
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

#logical data defaults to int8 so that NA survives; bool is offered but NA is rejected
data_lgl = matrix(c(TRUE, NA, FALSE, TRUE, FALSE, TRUE), 3, 2)
Rfits_write_image_zarr(data_lgl, file_types, extname = 'lgl')
read_lgl = Rfits_read_image_zarr(file_types, extname = 'lgl', header = FALSE)
expect_equal(read_lgl, matrix(c(1L, NA, 0L, 1L, 0L, 1L), 3, 2))
expect_error(Rfits_write_image_zarr(data_lgl, file_types, extname = 'bad', data_type = 'bool'),
             'Cannot store NA')

#force_logical converts an integer image back to logical on read, keeping dims
forced = Rfits_read_image_zarr(file_types, extname = 'lgl', header = FALSE, force_logical = TRUE)
expect_identical(typeof(forced), 'logical')
expect_equal(dim(forced), c(3, 2))
expect_equal(forced, data_lgl)

#ex 10 integer64 is coerced to float64 with a message, since Zarr cannot write it
expect_message(Rfits_write_image_zarr(bit64::as.integer64(1:5), file_types, extname = 'i64'),
               'integer64')

#ex 11 chunking is honoured, and a subset read of a chunked array is still correct
file_chunk = file.path(subdir, 'chunked.zarr')
data_big = array(as.numeric(1:4000), c(10, 10, 40))
Rfits_write_image_zarr(data_big, file_chunk, extname = 'cube', chunk_shape = c(10, 10, 1))
expect_equal(Rfits_read_image_zarr(file_chunk, extname = 'cube', header = FALSE), data_big)
chunk_sub = Rfits_read_image_zarr(file_chunk, extname = 'cube', zlo = 20, zhi = 25, header = FALSE)
expect_equal(chunk_sub, data_big[, , 20:25])
expect_error(Rfits_write_image_zarr(data_2d, file_chunk, extname = 'badchunk',
                                   chunk_shape = c(2, 3, 4)), 'chunk_shape')
expect_error(Rfits_write_image_zarr(data_2d, file_chunk, extname = 'badchunk',
                                   chunk_shape = c(2, 9)), 'chunk_shape')

#ex 12 pointers read only what is asked of them
point = Rfits_point_zarr(file_dims, extname = 'data4')
expect_identical(class(point), 'Rfits_pointer_zarr')
expect_equal(dim(point), c(4, 5, 3, 4))
expect_equal(point$dim, c(4, 5, 3, 4))
expect_equal(point$type, 'array')
expect_equal(length(point), prod(c(4, 5, 3, 4)))
point_sub = point[2:3, 1:4, 2:3, 1:2]
expect_equal(dim(point_sub$imDat), c(2, 4, 2, 2))
expect_equal(point_sub$imDat, data_4d[2:3, 1:4, 2:3, 1:2])
expect_equal(Rfits_point_zarr(file_dims, ext = 1)$extname, 'data1')

#ex 13 comment and history strings round trip
file_hist = file.path(subdir, 'history.zarr')
Rfits_write_image_zarr(data_2d, file_hist, extname = 'data1', keyvalues = keyvalues_2d,
                       comment = c('first comment', 'second comment'),
                       history = c('made this up', 'then that'))
read_hist = Rfits_read_image_zarr(file_hist, extname = 'data1')
expect_equal(read_hist$comment, c('first comment', 'second comment'))
expect_equal(read_hist$history, c('made this up', 'then that'))

#ex 14 ZIMAGE style (compressed image) headers update the ZNAXIS keys
file_zim = file.path(subdir, 'zimage.zarr')
keyvalues_zim = list(XTENSION = 'IMAGE ', BITPIX = 8L, NAXIS = 0L, ZIMAGE = TRUE,
                     ZBITPIX = -32L, ZNAXIS = 2L, ZNAXIS1 = 4L, ZNAXIS2 = 6L,
                     ZCRPIX1 = 1.5, ZCRPIX2 = 3.5)
Rfits_write_image_zarr(data_2d, file_zim, extname = 'zdata', keyvalues = keyvalues_zim)
read_zim = Rfits_read_image_zarr(file_zim, extname = 'zdata', xlo = 2, xhi = 3)
expect_true(isTRUE(read_zim$keyvalues$ZIMAGE))
expect_equal(read_zim$keyvalues$ZNAXIS1, 2L)
expect_equal(read_zim$keyvalues$ZNAXIS2, 6L)

#ex 15 collapse trims trailing singleton dimensions
file_single = file.path(subdir, 'singleton.zarr')
data_single = array(as.numeric(1:24), c(4, 6, 1, 1))
Rfits_write_image_zarr(data_single, file_single, extname = 'sdata',
                       keyvalues = keyvalues_4d)
expect_identical(class(Rfits_read_image_zarr(file_single, extname = 'sdata'))[1], 'Rfits_array')
expect_identical(class(Rfits_read_image_zarr(file_single, extname = 'sdata', collapse = TRUE))[1],
                 'Rfits_image')

#ex 16 bad input is rejected
expect_error(Rfits_read_image_zarr(file.path(subdir, 'definitely_not_a_store.zarr')))
expect_error(Rfits_write_image_zarr(data_2d, file_dims, extname = 'has space'))
expect_error(Rfits_write_image_zarr('not an image', file_dims, extname = 'strings'))
expect_error(Rfits_write_image_zarr(array(as.numeric(1:120), c(2, 3, 4, 5)), file_dims,
                                   extname = 'five'), NA)
expect_error(Rfits_write_image_zarr(array(as.numeric(1:720), c(2, 3, 4, 5, 6)), file_dims,
                                   extname = 'five'), '4 dimension')

#ex 17 the back end agrees with the HDF5 back end where both can be used
skip_if_not_installed("hdf5r")
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
expect_equal(from_hdf5$keycomments, from_zarr$keycomments)
expect_equal(from_hdf5$header, from_zarr$header)

sub_hdf5 = Rfits_read_image_hdf5(file_hdf5, extname = 'data1', xlo = 2, xhi = 3, ylo = 2, yhi = 4)
sub_zarr = Rfits_read_image_zarr(file_zarr_cmp, extname = 'data1', xlo = 2, xhi = 3, ylo = 2, yhi = 4)
expect_equal(sub_hdf5$imDat, sub_zarr$imDat)
expect_equal(unclass(sub_hdf5$keyvalues), unclass(sub_zarr$keyvalues))

#ex 18 Rfits_check_image style re-read of a written file is idempotent
again = Rfits_check_image(Rfits_read_image_zarr(file_dims, extname = 'data2'))
expect_equal(again$imDat, data_2d)
expect_equal(again$keyvalues$NAXIS1, 4L)
expect_equal(again$keyvalues$NAXIS2, 6L)

#ex 19 store objects, which is how remote stores are reached. A local store and a
#memory store stand in for a remote one, so this needs no network or credentials
store_local = zarr::zarr_localstore$new(file_dims, read_only = TRUE)
via_store = Rfits_read_image_zarr(store_local, extname = 'data2')
expect_equal(via_store$imDat, data_2d)
expect_equal(via_store$header, read_2d$header)
expect_equal(via_store$keyvalues$NAXIS1, 4L)
#the filename field records the store rather than a local path
expect_equal(via_store$filename, store_local$uri)
#ext and subset arguments work the same way over a store object
expect_equal(Rfits_read_image_zarr(store_local, ext = 1)$extname, 'data1')
sub_store = Rfits_read_image_zarr(store_local, extname = 'data2', xlo = 2, xhi = 3, ylo = 2, yhi = 4)
expect_equal(sub_store$imDat, data_2d[2:3, 2:4])
expect_equal(sub_store$keyvalues$CRPIX1, keyvalues_2d$CRPIX1 - 2L + 1L)
expect_identical(Rfits_read_image_zarr(store_local, extname = 'data2', header = FALSE), data_2d)
#an already open zarr object is accepted in place of its store
expect_equal(Rfits_read_image_zarr(zarr::zarr$new(store_local), extname = 'data2')$imDat, data_2d)

#ex 20 writing to a store object, including one that has no root group yet. The
#constructor makes the directory but writes no root, so the store is unusable
#until something bootstraps it (this is the same shape a new remote store has)
file_writable = file.path(subdir, 'store_writable.zarr')
fresh = zarr::zarr_localstore$new(file_writable, read_only = FALSE)
expect_true(dir.exists(file_writable))
expect_setequal(list.files(file_writable, all.files = TRUE), c('.', '..'))
expect_false(fresh$exists('zarr.json'))
expect_error(Rfits_read_image_zarr(fresh), 'no root group')
written = Rfits_write_image_zarr(data_2d, fresh, extname = 'data2', keyvalues = keyvalues_2d)
expect_true(fresh$exists('zarr.json'))
expect_equal(written$filename, fresh$uri)
expect_equal(Rfits_read_image_zarr(fresh, extname = 'data2', header = FALSE), data_2d)
#appending a second extension to the same store object keeps the first
Rfits_write_image_zarr(data_1d, fresh, extname = 'data1', keyvalues = keyvalues_1d)
expect_equal(Rfits_read_image_zarr(fresh, extname = 'data2', header = FALSE), data_2d)
expect_equal(Rfits_read_image_zarr(fresh, extname = 'data1', header = FALSE), data_1d)
#rewriting an existing extension replaces it rather than erroring or duplicating
Rfits_write_image_zarr(data_2d[, 1:3], fresh, extname = 'data2', keyvalues = keyvalues_2d)
expect_equal(dim(Rfits_read_image_zarr(fresh, extname = 'data2', header = FALSE)), c(4, 3))
expect_error(Rfits_write_image_zarr(data_2d, fresh, extname = 'data2', create_ext = FALSE),
             'already exists')

#ex 21 overwrite_file empties a store object, since there is no directory to remove
Rfits_write_image_zarr(data_2d, fresh, extname = 'data2', keyvalues = keyvalues_2d,
                       create_ext = FALSE, overwrite_file = TRUE)
read_over = Rfits_read_image_zarr(fresh, extname = 'data2')
expect_equal(read_over$imDat, data_2d)
#the cleared store lost the other extension. A missing extension is reported by
#the try() wrapper inside the reader, so it returns NULL rather than raising. Will print an error message to screen and stop.
#Identical to what a path does for the same case
expect_null(Rfits_read_image_zarr(fresh, extname = 'data1'))

#ex 22 a read only store object is refused by the writer
expect_error(Rfits_write_image_zarr(data_2d, store_local, extname = 'nope'), 'read only')

#ex 23 an un-bootstrapped memory store, the shape a new remote store arrives in
mem = zarr::zarr_memorystore$new()
expect_error(Rfits_read_image_zarr(mem), 'no root group')
expect_error(Rfits_point_zarr(mem), 'no root group')
Rfits_write_image_zarr(data_3d, mem, extname = 'data3', keyvalues = keyvalues_3d)
read_mem = Rfits_read_image_zarr(mem, extname = 'data3')
expect_equal(read_mem$imDat, data_3d)
expect_identical(class(read_mem)[1], 'Rfits_cube')
#memory stores have no URI, so the label falls back to the store type
expect_equal(read_mem$filename, 'memory store')

#ex 24 pointers over a store object re-read through the store rather than the
#filename. A memory store makes this checkable, since its filename label is not a
#path and so any attempt to open it by path would fail
point_mem = zarr::zarr_memorystore$new()
Rfits_write_image_zarr(data_4d, point_mem, extname = 'data4', keyvalues = keyvalues_4d)
ptr = Rfits_point_zarr(point_mem, extname = 'data4')
expect_identical(class(ptr), 'Rfits_pointer_zarr')
expect_equal(ptr$filename, 'memory store')
expect_equal(dim(ptr), c(4, 5, 3, 4))
expect_equal(ptr$type, 'array')
sub_ptr = ptr[2:3, 1:4, 2:3, 1:2]
expect_equal(sub_ptr$imDat, data_4d[2:3, 1:4, 2:3, 1:2])
#and the open store is carried through to the pointer, so it stays re-readable
expect_true(inherits(ptr$store, 'zarr'))
expect_equal(Rfits_read_image_zarr(ptr$store, extname = 'data4', header = FALSE), data_4d)
#pointers made from a path keep working unchanged
ptr_path = Rfits_point_zarr(file_dims, extname = 'data4')
expect_equal(dim(ptr_path), c(4, 5, 3, 4))
expect_equal(ptr_path[2:3, 1:4, 2:3, 1:2]$imDat, data_4d[2:3, 1:4, 2:3, 1:2])

#ex 25 pointer subsetting matches [.Rfits_image, which is what the FITS pointer
#mirrors too. Uses the shipped example image, since it has a real WCS
point_img = Rfits_point_zarr(file_image_zarr, extname = 'image')
point_fits = Rfits_point(file_image)
full_dim = dim(point_img)
expect_identical(full_dim, c(356L, 356L))

#ranges, and a pair of bounds given as a vector, are the same cut
expect_equal(point_img[50:150, 50:150]$imDat, temp_image$imDat[50:150, 50:150])
expect_equal(point_img[c(50, 150), c(60, 160)]$imDat, temp_image$imDat[50:150, 60:160])
expect_equal(point_img[50:150, 50:150]$imDat, point_img[c(50, 150), c(50, 150)]$imDat)

#two values for i alone are a location to centre a box on, not a range, so this
#defaults to box=201 rather than cutting out rows 50:150
expect_equal(dim(point_img[c(50, 150)]$imDat), c(201L, 201L))
#adjacent values really are a range though
expect_equal(dim(point_img[c(50, 51)]$imDat), c(2L, 356L))

#box sets the cutout size around a location, scalar or per axis
expect_equal(point_img[101, 101, box = 100]$imDat, temp_image$imDat[52:151, 52:151])
expect_equal(dim(point_img[101, 101, box = c(30, 50)]$imDat), c(30L, 50L))
#a box with no location given is centred on the image
expect_equal(point_img[box = 50]$imDat, point_img[ceiling(356/2), ceiling(356/2), box = 50]$imDat)

#a:end reads to the end of that dimension, for i and for j
expect_equal(point_img[50:end]$imDat, temp_image$imDat[50:356, ])
expect_equal(point_img[50:150, 60:end]$imDat, temp_image$imDat[50:150, 60:356])

#header keywords are fixed up for the cutout, as for any Rfits subset
sub_box = point_img[101, 101, box = 100]
expect_equal(sub_box$keyvalues$NAXIS1, 100L)
expect_equal(sub_box$keyvalues$NAXIS2, 100L)
expect_equal(sub_box$keyvalues$CRPIX1, temp_image$keyvalues$CRPIX1 - 52 + 1L)
#header = FALSE gives back just the array
expect_equal(point_img[101, 101, box = 20, header = FALSE],
             temp_image$imDat[92:111, 92:111])

#ex 26 the FITS pointer gives the same answers for the same calls
expect_equal(point_img[50:150, 50:150]$imDat, point_fits[50:150, 50:150]$imDat)
expect_equal(point_img[101, 101, box = 100]$imDat, point_fits[101, 101, box = 100]$imDat)
expect_equal(point_img[c(50, 150)]$imDat, point_fits[c(50, 150)]$imDat)
expect_equal(point_img[]$imDat, point_fits[]$imDat)
#and an identical header, so a cutout written back out would carry the same WCS
expect_equal(point_img[10:20, 10:20, box = 5]$header, point_fits[10:20, 10:20, box = 5]$header)

#ex 27 type = 'coord' resolves RA/Dec to pixels before cutting out
skip_if_not_installed("Rwcs")
coords = Rwcs::Rwcs_p2s(101, 101, temp_image$keyvalues)
expect_equal(point_img[coords[1], coords[2], box = 100, type = 'coord']$imDat,
             point_img[101, 101, box = 100]$imDat)
expect_equal(point_img[coords[1], coords[2], box = 100, type = 'coord']$imDat,
             point_fits[coords[1], coords[2], box = 100, type = 'coord']$imDat)
expect_error(point_img[coords[1], coords[2], type = 'notatype'],
             "Must be element of set \\{'pix','coord'\\}")
#an array with no FITS metadata has no WCS to resolve against
file_nowcs = file.path(subdir, 'nowcs.zarr')
Rfits_write_image_zarr(data_2d, file_nowcs, extname = 'bare')
expect_error(Rfits_point_zarr(file_nowcs, extname = 'bare')[10, 10, type = 'coord'],
             'No FITS style metadata')

#ex 28 collapse drops a trailing dimension that was sliced, and only one the
#caller actually asked for. Matches the documented [.Rfits_cube behaviour
file_cube_zarr = file.path(subdir, 'cube.zarr')
file_cube_fits = system.file('extdata', 'cube.fits', package = "Rfits")
temp_cube = Rfits_read_cube(file_cube_fits)
Rfits_write_image_zarr(temp_cube, file_cube_zarr, extname = 'cube')
point_cube = Rfits_point_zarr(file_cube_zarr, extname = 'cube')

expect_equal(dim(point_cube[10:20, 10:20, 1]$imDat), c(11L, 11L))
expect_identical(class(point_cube[10:20, 10:20, 1])[1], 'Rfits_image')
expect_equal(point_cube[10:20, 10:20, 1]$imDat, temp_cube$imDat[10:20, 10:20, 1])
#a non singleton slice stays a cube
expect_equal(dim(point_cube[10:20, 10:20, 1:3]$imDat), c(11L, 11L, 3L))
expect_identical(class(point_cube[10:20, 10:20, 1:3])[1], 'Rfits_cube')
#collapse = FALSE keeps the trailing dimension
expect_equal(dim(point_cube[10:20, 10:20, 1, collapse = FALSE]$imDat), c(11L, 11L, 1L))
expect_identical(class(point_cube[10:20, 10:20, 1, collapse = FALSE])[1], 'Rfits_cube')
#leaving k out is not a request to collapse, so nothing is dropped
expect_equal(dim(point_cube[10:20, 10:20, ]$imDat), c(11L, 11L, 4L))
#the collapsed header no longer describes a third axis
expect_null(point_cube[10:20, 10:20, 1]$keyvalues$NAXIS3)
expect_equal(point_cube[10:20, 10:20, 1]$keyvalues$NAXIS, 2L)
#and the FITS pointer collapses the same slice identically
point_cube_fits = Rfits_point(file_cube_fits)
expect_equal(point_cube[10:20, 10:20, 1]$imDat, point_cube_fits[10:20, 10:20, 1]$imDat)
expect_equal(point_cube[10:20, 10:20, 1]$header, point_cube_fits[10:20, 10:20, 1]$header)
#same for a 4D array, collapsing to a cube and all the way to an image
expect_equal(dim(ptr[2:3, 1:4, 1:2, 1]$imDat), c(2L, 4L, 2L))
expect_identical(class(ptr[2:3, 1:4, 1:2, 1])[1], 'Rfits_cube')
expect_equal(dim(ptr[2:3, 1:4, 1, 1]$imDat), c(2L, 4L))
expect_identical(class(ptr[2:3, 1:4, 1, 1])[1], 'Rfits_image')
#slicing k alone is not a request to drop m, so the array stays 4D
expect_equal(dim(ptr[2:3, 1:4, 1, ]$imDat), c(2L, 4L, 1L, 4L))

#ex 29 too many dimensions and unsupported indexing are rejected
expect_error(point_img[1:2, 1:2, 1], 'third dimension')
expect_error(point_img[1:2, 1:2, 1, 1], 'third dimension')
expect_error(point_cube[1:2, 1:2, 1:2, 1], 'fourth dimension')
expect_error(point_img[cbind(c(1, 2), c(1, 2))], 'Matrix indexing')

#ex 30 a 1D pointer keeps range meaning for c(a,b), since there is no box to
#centre, as for [.Rfits_vector
point_vec = Rfits_point_zarr(file_dims, extname = 'data1')
expect_identical(point_vec$type, 'vector')
expect_equal(point_vec[c(3, 8)]$imDat, data_1d[3:8])
expect_equal(point_vec[3:8]$imDat, data_1d[3:8])
expect_equal(point_vec[3:end]$imDat, data_1d[3:10])
expect_equal(point_vec[]$imDat, data_1d)
expect_equal(point_vec[3:8, header = FALSE], data_1d[3:8])
#box is ignored rather than misread as a location, since it needs two dimensions
expect_equal(point_vec[3:8, box = 4]$imDat, data_1d[3:8])
expect_error(point_vec[3:8, 1:2], '1 dimensional')

#ex 31 collapse also works with header = FALSE, where there is no [ method to
#rebuild keywords, so the array is just re-dimensioned
point_cube_bare = Rfits_point_zarr(file_cube_zarr, extname = 'cube', header = FALSE)
expect_equal(dim(point_cube_bare[10:20, 10:20, 1]), c(11L, 11L))
expect_equal(point_cube_bare[10:20, 10:20, 1], temp_cube$imDat[10:20, 10:20, 1])
expect_equal(dim(point_cube_bare[10:20, 10:20, 1, collapse = FALSE]), c(11L, 11L, 1L))
expect_equal(dim(point_cube_bare[10:20, 10:20, 1:3]), c(11L, 11L, 3L))
#header = FALSE can be given per call, overriding the pointer
expect_equal(dim(point_cube[10:20, 10:20, 1, header = FALSE]), c(11L, 11L))

#ex 32 the WCS class methods work on a pointer, and agree with the same calls on
#the FITS pointer and on the image in RAM. The pointer carries the keywords but
#no raw header, so the fixed width form has to be rebuilt from them
expect_equal(centre(point_img), centre(point_fits))
expect_equal(centre(point_img), centre(temp_image))
#center is the alias
expect_equal(center(point_img), centre(point_img))

expect_equal(corners(point_img), corners(point_fits))
expect_equal(corners(point_img), corners(temp_image))
expect_identical(row.names(corners(point_img)), c('BL', 'TL', 'TR', 'BR'))
#RAneg is passed through to the shared method
expect_equal(corners(point_img, RAneg = TRUE), corners(point_fits, RAneg = TRUE))

expect_equal(extremes(point_img), extremes(point_fits))
expect_equal(extremes(point_img), extremes(temp_image))
expect_identical(row.names(extremes(point_img)), c('min', 'max', 'range'))
#unit converts the range row, and the amin default matches the generic
expect_equal(extremes(point_img, unit = 'deg')['range'] * 60, extremes(point_img)['range'],
             tolerance = 1e-8)

expect_equal(pixscale(point_img), pixscale(point_fits))
expect_equal(pixscale(point_img), pixscale(temp_image))
#loc selects where the scale is measured, and works for both spellings
expect_equal(pixscale(point_img, loc = 'tl'), pixscale(point_fits, loc = 'tl'))
expect_equal(pixscale(point_img, loc = c(100, 200)), pixscale(point_fits, loc = c(100, 200)))
expect_equal(pixscale(point_img, unit = 'deg') * 3600, pixscale(point_img), tolerance = 1e-8)

expect_equal(pixarea(point_img), pixarea(point_fits))
expect_equal(pixarea(point_img), pixarea(temp_image))
#on square pixels the area is the square of the scale
expect_equal(pixarea(point_img), pixscale(point_img)^2, tolerance = 1e-6)

expect_equal(rotation(point_img), rotation(point_fits))
expect_equal(rotation(point_img), rotation(temp_image))
#keypass = FALSE skips the keyword conversion, as for the other classes
expect_equal(rotation(point_img, keypass = FALSE), rotation(point_fits, keypass = FALSE))

#useraw = FALSE drops the fixed width header, which is the one case the pointer
#and the FITS file genuinely differ. The simple TAN WCS here gives the same
#answer either way
expect_equal(centre(point_img, useraw = FALSE), centre(point_img))
expect_equal(corners(point_img, useraw = FALSE), corners(point_img))

#ex 33 a cube pointer works off the first two dimensions, as any cube does. The
#scale and area are only defined for the celestial axes, so the extra dimension
#must not disturb them, which is checked against CDELT as well as the FITS one
expect_equal(centre(point_cube), centre(Rfits_point(file_cube_fits)))
expect_equal(corners(point_cube), corners(Rfits_point(file_cube_fits)))
expect_equal(pixscale(point_cube), pixscale(Rfits_point(file_cube_fits)))
expect_equal(pixarea(point_cube), pixarea(Rfits_point(file_cube_fits)))
expect_equal(pixscale(point_cube), abs(temp_cube$keyvalues$CDELT1) * 3600, tolerance = 1e-6)

#ex 34 the guards. A 1D pointer is not an image, and gives NA with a message, as
#the header and keylist methods do for an array with too few dimensions
expect_message(expect_true(is.na(centre(point_vec))), 'Probably not an image')
expect_message(expect_true(is.na(corners(point_vec))), 'Probably not an image')

#an array with no FITS metadata has no WCS to use, which is an error rather than
#an NA, since there is no way to tell the caller anything useful otherwise
point_nowcs = Rfits_point_zarr(file_nowcs, extname = 'bare')
expect_error(centre(point_nowcs), 'No FITS style metadata')
expect_error(corners(point_nowcs), 'No FITS style metadata')
expect_error(rotation(point_nowcs), 'No FITS style metadata')

#ex 35 the store describes itself. Root attributes exist after a write, list that
#array, and agree with what the array actually is
file_root = file.path(subdir, 'root.zarr')
Rfits_write_image_zarr(data_2d, file_root, extname = 'alpha', keyvalues = keyvalues_2d,
                       compressor = 'lz4', clevel = 5L)
root_of = function(path){
  zarr::open_zarr(path, protocol = 'local', read_only = TRUE)$root$attributes
}
attrs = root_of(file_root)
expect_equal(attrs$convention, 'Rfits.zarr')
expect_identical(attrs$schema_version, 1L)
expect_true(is.character(attrs$Rfits_version))
expect_match(attrs$created, '^[0-9]{4}-[0-9]{2}-[0-9]{2}T')
expect_identical(attrs$images_array, 'alpha')
#total_images is the total number of elements across all arrays, not a count of them
expect_identical(attrs$total_images, 24L)
expect_equal(attrs$images_shape$alpha, as.integer(dim(data_2d)))
expect_equal(attrs$data_type$alpha, 'float64')
expect_equal(attrs$codec$alpha, 'blosc')
expect_equal(attrs$compressor$alpha, 'lz4')
expect_equal(attrs$compression_level$alpha, 5L)
expect_equal(attrs$fill_value$alpha, 'NaN')
expect_identical(attrs$fits_header$alpha, TRUE)
expect_identical(attrs$key_count$alpha, length(keyvalues_2d))
expect_equal(attrs$creation_info$batched_nchw, FALSE)
#recorded values are read off the array, so they must match the array itself
node_alpha = zarr::open_zarr(file_root, protocol = 'local', read_only = TRUE)$get_node('/alpha')
expect_equal(as.integer(node_alpha$shape), attrs$images_shape$alpha)
expect_equal(node_alpha$metadata$data_type, attrs$data_type$alpha)
expect_equal(node_alpha$metadata$chunk_grid$configuration$chunk_shape, attrs$chunk_shape$alpha)

#a second array is added to the lists rather than replacing the first
Rfits_write_image_zarr(data_1d, file_root, extname = 'beta', keyvalues = keyvalues_1d)
attrs = root_of(file_root)
expect_setequal(attrs$images_array, c('alpha', 'beta'))
expect_identical(attrs$total_images, 34L)
#rewriting an array refreshes its entry without duplicating it
Rfits_write_image_zarr(data_2d[, 1:3], file_root, extname = 'beta', keyvalues = keyvalues_2d)
attrs = root_of(file_root)
expect_setequal(attrs$images_array, c('alpha', 'beta'))
expect_equal(attrs$images_shape$beta, c(4L, 3L))
expect_identical(attrs$total_images, 36L)
#the per array mirror agrees with the root
b_node = zarr::open_zarr(file_root, protocol = 'local', read_only = TRUE)$get_node('/beta')
expect_equal(b_node$attributes$compressor, attrs$compressor$beta)
expect_equal(as.integer(b_node$attributes$chunk_shape), attrs$chunk_shape$beta)
expect_equal(b_node$attributes$data_type, attrs$data_type$beta)

#ex 36 append grows dimension 1, which is the slowest varying axis in FITS, R and
#Zarr alike, so the existing elements keep their indices
file_app = file.path(subdir, 'append.zarr')
kv_app = keyvalues_2d
kv_app$NAXIS1 = 4L
Rfits_write_image_zarr(data_2d, file_app, extname = 'data1', keyvalues = kv_app)
before = Rfits_read_image_zarr(file_app, extname = 'data1')
inc = matrix(as.numeric(101:118), 3, 6)
ap = Rfits_write_image_zarr(inc, file_app, extname = 'data1', append = TRUE)
expect_identical(ap$start_index, 5L)
expect_identical(ap$end_index, 7L)
expect_identical(ap$appended, 3L)
expect_equal(ap$dim, c(7L, 6L))
after = Rfits_read_image_zarr(file_app, extname = 'data1')
expect_equal(dim(after$imDat), c(7, 6))
expect_equal(after$imDat[1:4, ], before$imDat)
expect_equal(after$imDat[5:7, ], inc)
expect_equal(after$keyvalues$NAXIS1, 7L)
expect_equal(after$keyvalues$NAXIS2, 6L)
expect_true(any(grepl('Rfits appended 3 elements', after$history)))
hist1 = root_of(file_app)$append_history
expect_length(hist1, 1)
expect_identical(hist1[[1]]$start_index, 5L)
expect_identical(hist1[[1]]$end_index, 7L)
expect_equal(hist1[[1]]$extname, 'data1')
#a second append adds a second entry rather than replacing the first
Rfits_write_image_zarr(matrix(as.numeric(201:212), 2, 6), file_app, extname = 'data1',
                       append = TRUE)
hist2 = root_of(file_app)$append_history
expect_length(hist2, 2)
expect_identical(hist2[[2]]$start_index, 8L)
expect_identical(hist2[[2]]$end_index, 9L)
expect_equal(root_of(file_app)$total_images, 54L)
#matrix(201:212, 2, 6) fills by column, so the second new row is the even values
expect_equal(Rfits_read_image_zarr(file_app, extname = 'data1', header = FALSE)[9, ],
             as.numeric(c(202, 204, 206, 208, 210, 212)))
#the named wrapper does the same thing
expect_equal(Rfits_append_image_zarr(matrix(as.numeric(1:12), 2, 6), file_app,
                                     extname = 'data1')$dim, c(11L, 6L))
#dimension 1 of a 1D array is all there is, so it grows the same way
file_app1 = file.path(subdir, 'append1.zarr')
Rfits_write_image_zarr(data_1d, file_app1, extname = 'v', keyvalues = keyvalues_1d)
Rfits_write_image_zarr(as.numeric(11:15), file_app1, extname = 'v', append = TRUE)
expect_equal(Rfits_read_image_zarr(file_app1, extname = 'v', header = FALSE),
             as.numeric(c(1:10, 11:15)))

#ex 37 the append guards
expect_error(Rfits_write_image_zarr(inc, file_app, extname = 'data1', append = TRUE,
                                    overwrite_file = TRUE), 'both append and overwrite_file')
expect_error(Rfits_write_image_zarr(matrix(as.numeric(1:15), 3, 5), file_app,
                                    extname = 'data1', append = TRUE), 'dimensions')
expect_error(Rfits_write_image_zarr(data_3d[1:2, , ], file_app, extname = 'data1',
                                    append = TRUE), 'dimensions')
expect_error(Rfits_write_image_zarr(inc, file_app, extname = 'nope', append = TRUE),
             'non-existent')
#an append never widens or narrows the stored type. Unrequested, it coerces with
#a message; requested explicitly, it is an error rather than a silent rewrite
expect_message(Rfits_write_image_zarr(matrix(1L:18, 3, 6), file_app, extname = 'data1',
                                      append = TRUE), 'float64')
expect_equal(Rfits_read_image_zarr(file_app, extname = 'data1')$keyvalues$NAXIS1, 14L)
expect_error(Rfits_write_image_zarr(matrix(1L:18, 3, 6), file_app, extname = 'data1',
                                    append = TRUE, data_type = 'int32'), 'data_type')
#a compressor cannot be changed on an array that already exists, and says so
expect_message(Rfits_write_image_zarr(inc, file_app, extname = 'data1', append = TRUE,
                                      compressor = 'blosclz'), 'were not applied')

#ex 38 compressor mapping, and the recorded level is the applied level
file_comp = file.path(subdir, 'codecs.zarr')
codec_of = function(path, extname){
  zarr::open_zarr(path, protocol = 'local', read_only = TRUE)$get_node(.zarr_name_to_path(extname))
}
for(name in c('blosc', 'blosclz', 'lz4', 'lz4hc', 'zstd')){
  ext = paste0('c_', name)
  res = Rfits_write_image_zarr(data_2d, file_comp, extname = ext, compressor = name, clevel = 9L)
  #the default name is not itself a blosc cname, it means zarr's own default
  want = if(name == 'blosc') 'zstd' else name
  expect_equal(root_of(file_comp)$compressor[[ext]], want)
  expect_identical(root_of(file_comp)$compression_level[[ext]], 9L)
  expect_equal(res$compressor, want)
  expect_equal(res$compression_level, 9L)
}
#gzip and zlib are the same thing, carried by blosc since the standalone codec
#needs a package that is not a dependency of zarr or Rfits
Rfits_write_image_zarr(data_2d, file_comp, extname = 'gz', compressor = 'gzip', clevel = 3L)
expect_equal(root_of(file_comp)$compressor$gz, 'zlib')
#names blosc cannot produce fall back with a warning rather than being recorded
expect_warning(Rfits_write_image_zarr(data_2d, file_comp, extname = 'bz',
                                      compressor = 'bz2'), 'cannot be produced')
expect_equal(root_of(file_comp)$compressor$bz, 'zstd')
expect_warning(Rfits_write_image_zarr(data_2d, file_comp, extname = 'nope',
                                      compressor = 'not_a_thing'), 'not recognised')
expect_equal(root_of(file_comp)$compressor$nope, 'zstd')
#shuffle is recorded as a blosc shuffle name, never as a logical
Rfits_write_image_zarr(data_2d, file_comp, extname = 'shuf_on', shuffle = TRUE)
Rfits_write_image_zarr(data_2d, file_comp, extname = 'shuf_off', shuffle = FALSE)
expect_equal(root_of(file_comp)$shuffle$shuf_on, 'shuffle')
expect_equal(root_of(file_comp)$shuffle$shuf_off, 'noshuffle')
expect_error(Rfits_write_image_zarr(data_2d, file_comp, extname = 'shuf_bad',
                                    shuffle = 'sometimes'), 'shuffle')
#what is recorded matches what the array actually carries
n = codec_of(file_comp, 'shuf_on')
expect_equal(n$metadata$codecs[[3]]$configuration$shuffle, 'shuffle')
expect_equal(n$metadata$codecs[[3]]$configuration$cname, root_of(file_comp)$compressor$shuf_on)
#the logical path really does work end to end, which is the point of the mapping
expect_equal(Rfits_read_image_zarr(file_comp, extname = 'shuf_on', header = FALSE), data_2d)
expect_equal(Rfits_read_image_zarr(file_comp, extname = 'gz', header = FALSE), data_2d)

#ex 39 Rfits_inspect_zarr reports what it can state truthfully
info = Rfits_inspect_zarr(file_root, print = FALSE)
expect_equal(info$filename, file_root)
expect_equal(info$zarr_format, 3L)
expect_true(info$self_describing)
expect_equal(info$convention, 'Rfits.zarr')
expect_identical(info$n_arrays, 2L)
expect_setequal(info$array_names, c('alpha', 'beta'))
expect_identical(info$total_elements, 36L)
expect_identical(info$arrays$alpha$shape, c(4L, 6L))
expect_equal(info$arrays$alpha$compressor, 'lz4')
expect_identical(info$arrays$alpha$fill_value, 'NaN')
expect_true(info$arrays$alpha$fits_header)
expect_identical(info$store_bytes, unlist(file.info(
  list.files(file_root, recursive = TRUE, full.names = TRUE))$size) |> sum())
expect_silent(Rfits_inspect_zarr(file_root, print = FALSE))
printed = capture.output(Rfits_inspect_zarr(file_root))
expect_true(any(grepl('SUMMARY STATISTICS', printed)))
expect_true(any(grepl('alpha', printed)))
#no pixel statistics are computed, so inspect stays cheap over a remote store
expect_false(any(grepl('min_value', names(unlist(info)))))

#a store with no self-description is reported as such rather than guessed at
file_older = file.path(subdir, 'older.zarr')
Rfits_write_image_zarr(data_2d, file_older, extname = 'data1', keyvalues = keyvalues_2d,
                       update_root = FALSE)
info_old = Rfits_inspect_zarr(file_older, print = FALSE)
expect_false(info_old$self_describing)
expect_true(is.na(info_old$convention))
expect_identical(info_old$n_arrays, 1L)
expect_equal(info_old$arrays$data1$compressor, 'zstd')
printed_old = capture.output(Rfits_inspect_zarr(file_older))
expect_true(any(grepl('absent', printed_old)))

#ex 40 update_root = FALSE leaves the root stale, and a later refresh fixes it
attrs_stale = root_of(file_older)
expect_true(is.null(attrs_stale$images_array))
Rfits_write_image_zarr(data_1d, file_older, extname = 'later', keyvalues = keyvalues_1d,
                       update_root = FALSE)
expect_true(is.null(root_of(file_older)$images_array))
Rfits_write_image_zarr(data_1d, file_older, extname = 'later2', keyvalues = keyvalues_1d)
attrs_fixed = root_of(file_older)
expect_setequal(attrs_fixed$images_array, c('data1', 'later', 'later2'))
expect_identical(attrs_fixed$total_images, 44L)
#refreshing rebuilds the description but the store is still the one created first,
#so the original timestamp has to survive it
created_1 = attrs_fixed$created
expect_match(created_1, '^[0-9]{4}-')
Sys.sleep(1.1)
Rfits_write_image_zarr(data_1d, file_older, extname = 'later3', keyvalues = keyvalues_1d)
expect_equal(root_of(file_older)$created, created_1)
expect_setequal(root_of(file_older)$images_array,
                c('data1', 'later', 'later2', 'later3'))

#ex 41 a foreign store says so, rather than reporting missing FITS metadata
file_foreign = file.path(subdir, 'foreign.zarr')
Rfits_write_image_zarr(data_2d, file_foreign, extname = 'images', update_root = FALSE)
#overwrite the data without any FITS metadata, then hand the root the key names a
#plain image to zarr converter writes, with no convention marker
fr = zarr::open_zarr(file_foreign, protocol = 'local', read_only = FALSE)
meta = fr$root$metadata
meta$attributes = list(total_images = 24L, images_array = list('images'))
fr$store$set_metadata('/', meta)
expect_null(root_of(file_foreign)$convention)
read_foreign = NULL
expect_message(read_foreign <- Rfits_read_image_zarr(file_foreign, extname = 'images'),
               'not written by Rfits')
expect_equal(read_foreign, data_2d)
#and an ordinary Rfits array with no metadata still gets the old warning
expect_warning(Rfits_read_image_zarr(file_nowcs, extname = 'bare'),
               'No FITS style metadata')

#ex 42 the new attributes must not disturb the FITS metadata in any way, so the
#round trip is still exact and the technical names never leak into the keywords
expect_equal(Rfits_read_image_zarr(file_image_zarr, extname = 'image')$header,
             temp_image$header)
leaked = intersect(c('data_type', 'chunk_shape', 'codec', 'compressor',
                     'compression_level', 'shuffle', 'fill_value'),
                   Rfits_read_image_zarr(file_root, extname = 'alpha')$keynames)
expect_length(leaked, 0)
skip_if_not_installed("hdf5r")
file_zarr_cmp2 = file.path(subdir, 'compare2.zarr')
Rfits_write_image_zarr(obj_2d, file_zarr_cmp2, extname = 'data1')
from_zarr2 = Rfits_read_image_zarr(file_zarr_cmp2, extname = 'data1')
expect_equal(unclass(from_hdf5$keyvalues), unclass(from_zarr2$keyvalues))
expect_equal(from_hdf5$header, from_zarr2$header)

#and the whole self describing plus append cycle works on a store object too. A
#memory store matters here because the root group prefix and the store key for it
#are not the same thing, so writing the root through the node API leaves a second
#phantom key behind that makes the store impossible to reopen
mem_app = zarr::zarr_memorystore$new()
Rfits_write_image_zarr(data_2d, mem_app, extname = 'data1', keyvalues = keyvalues_2d)
expect_equal(zarr::zarr$new(mem_app)$root$attributes$total_images, 24L)
mem_ap = Rfits_write_image_zarr(matrix(as.numeric(101:112), 2, 6), mem_app,
                                extname = 'data1', append = TRUE)
expect_equal(mem_ap$dim, c(6, 6))
#the store must still be openable by anyone after the root was rewritten
expect_silent(zarr::zarr$new(mem_app))
mem_read = Rfits_read_image_zarr(mem_app, extname = 'data1')
expect_equal(mem_read$imDat[1:4, ], data_2d)
expect_equal(mem_read$imDat[5:6, ], matrix(as.numeric(101:112), 2, 6))
expect_equal(mem_read$keyvalues$NAXIS1, 6L)
mem_info = Rfits_inspect_zarr(mem_app, print = FALSE)
expect_equal(mem_info$total_elements, 36L)
expect_length(mem_info$append_history, 1)
#no local directory, so no size to report
expect_true(is.na(mem_info$store_bytes))
