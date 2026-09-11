#load packages
library(Rfits)
library(testthat)
library(FITSio)
#library(tdigest)  - ignoring now tdigest is not on CRAN :-(
library(R.utils)
library(bit64)

context("Check Rfits table/image read/write")

#ex 1 check that we read in images like readFITS
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)
temp_image_FITSio = readFITS(file_image)
file_image_temp = tempfile()
Rfits_write_image(temp_image, file_image_temp)
temp_image2 = Rfits_read_image(file_image_temp)
expect_identical(temp_image$imDat, temp_image_FITSio$imDat) 

#ex 2 check read and write works correctly
expect_identical(temp_image$imDat, temp_image2$imDat)

#ex 3 check HDU extensions work
Rfits_write_image(temp_image, file_image_temp, overwrite_file=F, create_file=F,
                  create_ext=T)
temp_image3 = Rfits_read_image(file_image_temp, ext=2)
expect_identical(temp_image2$imDat, temp_image3$imDat) 

#ex 4 write another extension to file
Rfits_write_image(temp_image, file_image_temp, overwrite_file=F, create_file=F, create_ext=T)
#illegally read ext 3 and get error that we ignore
try(Rfits_read_image(file_image_temp, ext=3), silent=TRUE)
#carry on writing
temp=try(Rfits_write_image(temp_image, file_image_temp, overwrite_file=F, create_file=F, create_ext=T))
expect(temp$ext==4, "Did not write to extension 2!")

#ex 5 check keyvalues are identical
expect_identical(temp_image$keyvalues, temp_image2$keyvalues) 

#ex 6 check comments are identical
expect_identical(temp_image$comments, temp_image2$comments) 

#ex 7 check that 32 and 64 bit versions are the same
Rfits_write_image(temp_image, file_image_temp, numeric=64)
temp_image2 = Rfits_read_image(file_image_temp)
expect_identical(temp_image$imDat, temp_image2$imDat) 

#ex 8 check integer read write
temp_image_int = matrix(as.integer(temp_image$imDat), 356, 356)
Rfits_write_image(temp_image_int, file_image_temp)
temp_image_int2 = Rfits_read_image(file_image_temp)
expect_identical(temp_image_int, temp_image_int2$imDat)

#ex 9 check 16 bit integer read the same as readFITS
temp_image_int[temp_image_int> 2^15] = 0L
Rfits_write_image(temp_image_int, file_image_temp, integer=16)
temp_image_int2 = Rfits_read_image(file_image_temp)
temp_image_int_FITSio = readFITS(file_image_temp)
expect_identical(temp_image_int, temp_image_int_FITSio$imDat)

#ex 10 check 16 bit read write
expect_identical(temp_image_int, temp_image_int2$imDat)

#ex 11 check table read write
file_table = system.file('extdata', 'table.fits', package = "Rfits")
temp_table = Rfits_read_table(file_table)
file_table_temp = tempfile()
Rfits_write_table(temp_table, file_table_temp)
temp_table2 = Rfits_read_table(file_table_temp)
expect_identical(temp_table, temp_table2)

#ex 12 check table writing to HDU extension
Rfits_write_table(temp_table, file_table_temp, overwrite_file=F, create_file=F, create_ext=T)
temp_table3 = Rfits_read_table(file_table_temp, ext=3)
expect_identical(temp_table, temp_table3)

#ex 13 check we can have a file with a mix of images and tables
file_mix_temp = tempfile()
Rfits_write_image(temp_image, file_mix_temp)
Rfits_write_table(temp_table, file_mix_temp, overwrite_file=F, create_file=F, create_ext=T)
temp_image3 = Rfits_read_image(file_mix_temp)
expect_identical(temp_image$imDat, temp_image3$imDat) 

#ex 14 check we can have a file with a mix of images and tables
temp_table4 = Rfits_read_table(file_mix_temp, ext=2)
expect_identical(temp_table, temp_table4)

#ex 15 check we have two headers
file_mix_summary = Rfits_info(file_mix_temp)$summary
expect_length(file_mix_summary, 2)

#ex 16 check we can read and write image subsets to a mixed file
Rfits_write_image(temp_image$imDat[1:100,1:100], file_mix_temp, overwrite_file=F, create_file=F, create_ext=T)
Rfits_write_table(temp_table[1:50,], file_mix_temp, overwrite_file=F, create_file=F, create_ext=T)
temp_image4 = Rfits_read_image(file_mix_temp, ext=3)
expect_identical(temp_image4$imDat, temp_image$imDat[1:100,1:100])

#ex 17 check we can read and write table subsets to a mixed file
temp_table5 = Rfits_read_table(file_mix_temp, ext=4)
expect_identical(temp_table5, temp_table[1:50,])

#ex 18 overwrite and extension 3 with a table subset
Rfits_write_table(temp_table[1:60,], file_mix_temp, overwrite_file=F, create_file=F, create_ext=F, ext=3) #delete ext 3 and append to end
temp_table6 = Rfits_read_table(file_mix_temp, ext=4)
expect_identical(temp_table6, temp_table[1:60,])

#ex 19 read, write and read all and check the same:
Rfits_write_image(temp_image$imDat, file_mix_temp, overwrite_file=F, create_file=F, create_ext=T)
temp_mix = Rfits_read_all(file_mix_temp)
file_mix_temp2 = tempfile()
Rfits_write_all(temp_mix, file_mix_temp2, overwrite_Main=FALSE)
temp_mix2 = Rfits_read_all(file_mix_temp2)
attributes(temp_mix)$filename = attributes(temp_mix2)$filename #should be only changes
temp_mix2[[1]]$filename = temp_mix[[1]]$filename #should be only changes
attributes(temp_mix2[[2]])$filename = attributes(temp_mix[[2]])$filename #should be only changes
attributes(temp_mix2[[3]])$filename = attributes(temp_mix[[3]])$filename #should be only changes
attributes(temp_mix2[[4]])$filename = attributes(temp_mix[[4]])$filename #should be only changes
temp_mix2[[5]]$filename = temp_mix[[5]]$filename #should be only changes
expect_identical(temp_mix, temp_mix2)

#ex 20 check we can read and write ascii tables
Rfits_write_table(temp_table, file_table_temp, table_type = 'ascii')
temp_table7=Rfits_read_table(file_table_temp)
expect_equal(temp_table7[,c(1,3:35)], temp_table[,c(1,3:35)]) #int64 is truncated to int by cfitsio ascii reader

#ex 21  check binary and ascii tables are the same
temp_profound = read.table(system.file('extdata', 'profound.tab', package = "Rfits"))
file_profound_bin = tempfile()
file_profound_ascii = tempfile()
Rfits_write_table(temp_profound, filename = file_profound_bin)
Rfits_write_table(temp_profound, filename = file_profound_ascii, table_type = 'ascii')
temp_profound2 = Rfits_read_table(file_profound_bin)
temp_profound3 = Rfits_read_table(file_profound_ascii)
expect_equal(temp_profound2, temp_profound3)

#ex 22 check compression works within tolerance
file_image_temp = tempfile()
Rfits_write_image(temp_image$imDat, filename = paste(file_image_temp,'[compress]',sep=''))
temp_compress = Rfits_read_image(file_image_temp,ext=2)
expect(abs(log10(sum(temp_image$imDat)/sum(temp_compress$imDat))) < 1e-4, failure_message = 'Images differ too much!')

#ex 23 subset a pointer
temp_point = Rfits_point(file_image, header=FALSE)
expect_equal(temp_image$imDat[1:5,1:5], temp_point[1:5,1:5])

#ex 24 read and write cubes
temp_cube = Rfits_read_cube(system.file('extdata', 'cube.fits', package = "Rfits"))
file_cube_temp = tempfile()
Rfits_write_cube(temp_cube, file_cube_temp)
temp_cube2 = Rfits_read_cube(file_cube_temp)
expect_identical(temp_cube$imDat, temp_cube2$imDat)

#ex 25 check we treat HIERARCH keywords correctly
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)
temp_image$keyvalues$`HIERARCH  TEST` = 100L
temp_image$keynames=c(temp_image$keynames, 'HIERARCH  TEST')
temp_image$keycomments$`HIEARCH  TEST` = ''
file_image_temp = tempfile()
Rfits_write_image(temp_image, file_image_temp)
temp_image_hier = Rfits_read_image(file_image_temp, remove_HIERARCH = FALSE)
expect_identical(temp_image$keyvalues, temp_image_hier$keyvalues)

#ex 26 check DATASUM
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)
file_image_temp = tempfile()
Rfits_write_image(temp_image, file_image_temp)
Rfits_write_chksum(file_image_temp)
temp_check = Rfits_verify_chksum(file_image_temp)
expect_identical(as.character(temp_check['DATASUM']), "correct")

#ex 27 check CHECKSUM
expect_identical(as.character(temp_check['CHECKSUM']), "correct")

#ex 28 check [] methods work for images
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)
expect_identical(temp_image$imDat[1:5,1:5], temp_image[1:5,1:5,header=FALSE])

#ex 29 check [] methods work for cubes
temp_cube = Rfits_read_cube(system.file('extdata', 'cube.fits', package = "Rfits"))
expect_identical(temp_cube$imDat[26:30,26:30,1:2], temp_cube[26:30,26:30,1:2,header=FALSE])

#ex 29b check cube slices collapse, as documented for the collapse argument. A
#singleton third dimension used to short circuit the subset and so never collapsed
expect_identical(class(temp_cube[26:30,26:30,1])[1], 'Rfits_image')
expect_identical(dim(temp_cube[26:30,26:30,1]), c(5L, 5L))
expect_identical(temp_cube[26:30,26:30,1]$imDat, temp_cube$imDat[26:30,26:30,1])
#collapse = FALSE keeps the trailing dimension
expect_identical(class(temp_cube[26:30,26:30,1,collapse=FALSE])[1], 'Rfits_cube')
expect_identical(dim(temp_cube[26:30,26:30,1,collapse=FALSE]), c(5L, 5L, 1L))
#the collapsed header no longer describes a third axis
expect_identical(temp_cube[26:30,26:30,1]$keyvalues$NAXIS, 2L)
expect_null(temp_cube[26:30,26:30,1]$keyvalues$NAXIS3)
#leaving k out is not a request to collapse, so a full extent read is unchanged
expect_identical(class(temp_cube[])[1], 'Rfits_cube')
expect_identical(dim(temp_cube[]), c(50L, 50L, 4L))
expect_identical(class(temp_cube[26:30,26:30,])[1], 'Rfits_cube')
#and the pointer collapses the same slice identically
temp_point_cube = Rfits_point(system.file('extdata', 'cube.fits', package = "Rfits"))
expect_identical(class(temp_point_cube[26:30,26:30,1])[1], 'Rfits_image')
expect_identical(temp_point_cube[26:30,26:30,1]$imDat, temp_cube[26:30,26:30,1]$imDat)
expect_identical(temp_point_cube[26:30,26:30,1]$keyvalues$NAXIS, 2L)
expect_identical(temp_point_cube[26:30,26:30,1]$keyvalues$CRPIX1,
                 temp_cube[26:30,26:30,1]$keyvalues$CRPIX1)
#Note the pointer re-subsets to collapse, so XCUTLO/YCUTLO are relative to the
#already cut out array rather than to the original file, as for every
#Rfits_pointer collapse. The full header therefore differs from the in RAM one

#ex 30 check consistent BZERO and BSCALE reading and writing
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)
temp_image$keyvalues$BZERO = 100
temp_image$keyvalues$BSCALE = 10
temp_image$keycomments$BZERO = ""
temp_image$keycomments$BSCALE = ""
temp_image$keynames = c(temp_image$keynames, "BZERO", "BSCALE")
file_image_temp = tempfile()
Rfits_write_image(temp_image, file_image_temp)
temp_image = Rfits_read_image(file_image_temp)
Rfits_write_image(temp_image, file_image_temp)
temp_image2 = Rfits_read_image(file_image_temp)
expect_equal(temp_image$imDat, temp_image2$imDat) 

#ex 31 check consistent TZEROn and TSCALn reading and writing
file_table = system.file('extdata', 'table.fits', package = "Rfits")
temp_table = Rfits_read_table(file_table)
file_table_temp = tempfile()
Rfits_write_table(temp_table, file_table_temp, tadd=list(TSCAL6=2, TZERO6=10, TSCAL13=10))
temp_table2 = Rfits_read_table(file_table_temp)
expect_identical(temp_table, temp_table2)

#ex 32 tdigest checks - ignoring now tdigest is not on CRAN :-(
#file_image=system.file('extdata', 'image.fits', package = "Rfits")
#temp_image=Rfits_read_image(file_image)
#td=tdigest(temp_image$imDat, compression=1e3) 
#expect_equal(median(temp_image$imDat), td[0.5], tolerance=2e-3)

#ex 33 pure header
temp_head = list(
  SIMPLE = TRUE,
  BITPIX = 16L,
  NAXIS = 0L,
  EXTEND = TRUE,
  RANDOM = 'Hello'
)
class(temp_head) = 'Rfits_keylist'
file_head_temp = tempfile()
Rfits_write_header(file_head_temp, keyvalues=temp_head, create_file=T, create_ext=T)
temp_head2 = Rfits_read_header(file_head_temp)
expect_identical(temp_head, temp_head2$keyvalues)

#ex 34 int64 image
image_int64 = as.integer64(1:1e4)
attributes(image_int64)$dim=c(100,100)
file_image_int64 = tempfile()
Rfits_write_image(image_int64, file=file_image_int64)
image_int642 = Rfits_read_image(file_image_int64)
expect_identical(image_int64, image_int64)

#ex 35 check cube subsets work
temp_cube = Rfits_read_cube(system.file('extdata', 'cube.fits', package = "Rfits"))
temp_cube_subset = Rfits_read_cube(system.file('extdata', 'cube.fits', package = "Rfits"), 
                    xlo=26, xhi=30, ylo=26, yhi=30, zlo=2, zhi=3)
expect_identical(temp_cube$imDat[26:30,26:30,2:3], temp_cube_subset$imDat)

#ex 36 4D array
temp_array = array(runif(1e4), dim=c(10,10,10,10))
file_array = tempfile()
Rfits_write_array(temp_array, file=file_array)
temp_array2 = Rfits_read_array(file_array)
expect_equal(temp_array, temp_array2$imDat, tolerance=3e-8)

#ex 37 1D vector
temp_vector = Rfits_read_vector(system.file('extdata', 'vector.fits', package = "Rfits"), ext=2)
file_vector = tempfile()
Rfits_write_vector(temp_vector, file_vector)
temp_vector2 = Rfits_read_vector(file_vector)
expect_identical(temp_vector$imDat, temp_vector2$imDat)

#ex 38 multi-ext with compressed images
file_mix_temp3 = tempfile()
Rfits_write_image(temp_image$imDat, paste0(file_mix_temp3,'[compress]'), create_ext=T, create_file=T)
Rfits_write_image(temp_image$imDat, file_mix_temp3, create_ext=T, create_file=F, compress=T)
Rfits_write_image(temp_image$imDat, paste0(file_mix_temp3,'[compress GZIP]'), create_ext=T, create_file=F)
Rfits_write_image(temp_image$imDat, file_mix_temp3, create_ext=T, create_file=F, compress='GZIP')
Rfits_write_table(temp_table, file_mix_temp3, create_ext=T, create_file=F)
temp_mix3 = Rfits_read_all(file_mix_temp3)
expect_length(temp_mix3, 6L)

#ex39/40 check ext headers
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)
file_list_temp = tempfile()
Rfits_write(list(temp_image, temp_image), filename=file_list_temp)
temp_list = Rfits_read(file_list_temp)
expect_identical(unlist(temp_list[[1]]$keyvalues[temp_image$keynames]), unlist(temp_image$keyvalues))
expect_identical(unlist(temp_list[[2]]$keyvalues[temp_image$keynames]), unlist(temp_image$keyvalues))

#ex41/42 check gz
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)
file_gz_temp = tempfile(fileext='.fits.gz')
R.utils::gzip(system.file('extdata', 'image.fits', package = "Rfits"), destname=file_gz_temp, remove=FALSE, overwrite=TRUE)
temp_image_gz = Rfits_read_image(file_gz_temp)
expect_identical(temp_image$imDat, temp_image_gz$imDat)
expect_identical(file_gz_temp, options()$Rfits_gunzip[1,1])

#ex43/44/45/46 check some methods

expect_identical(dim(temp_vector), 3722L)
expect_identical(dim(temp_image), c(356L, 356L))
expect_identical(dim(temp_cube), c(50L, 50L, 4L))
expect_identical(dim(temp_array2), c(10L, 10L, 10L, 10L))

#ex47 write a subset to a current FITS file
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)
file_image_temp = tempfile()
Rfits_write_image(temp_image, file_image_temp)

temp_mat = matrix(1:9,3,3)

Rfits_write_pix(temp_mat, file_image_temp, xlo=10, ylo=20)

temp_image2 = Rfits_read_image(file_image_temp)
expect_equal(temp_mat, temp_image2$imDat[10:12,20:22])

#ex48 create blank image and write a subset to it
file_image_temp = tempfile()
Rfits_blank_image(file_image_temp, bitpix=32)

temp_mat = matrix(1:9,3,3)

Rfits_write_pix(temp_mat, file_image_temp, xlo=50, ylo=60)
temp_image2 = Rfits_read_image(file_image_temp)
expect_identical(temp_mat, temp_image2$imDat[50:52,60:62])

#ex 49 write and read various table types
file_table_types = tempfile()
tb_types = data.frame(
  vals_dbl = c(1.1, 2.2, 3.3),
  vals_int = 1:3,
  vals_lgc = c(TRUE, FALSE, TRUE),
  vals_i64 = bit64::as.integer64(c(1L, 2L, 3L))
)
Rfits_write_table(tb_types, file_table_types)
tb_types_read = Rfits_read_table(file_table_types)
expect_identical(tb_types$vals_dbl, tb_types_read$vals_dbl)
expect_identical(tb_types$vals_int, tb_types_read$vals_int)
expect_identical(tb_types$vals_lgc, tb_types_read$vals_lgc)
expect_identical(tb_types$vals_i64, tb_types_read$vals_i64)

#ex 50 write and read vector (list) columns
file_vec_table = tempfile()
tb_vec = data.frame(
  id = 1:3,
  vals_dbl = I(list(c(1.1, 2.2, 3.3), c(4.4, 5.5, 6.6), c(7.7, 8.8, 9.9))),
  vals_int = I(list(1:4, 5:8, 9:12)),
  vals_lgc = I(list(c(TRUE,TRUE), c(FALSE,TRUE), c(FALSE,FALSE))),
  vals_i64 = I(list(bit64::as.integer64(c(1L, 2L, 3L)),
                    bit64::as.integer64(c(4L, 5L, 6L)),
                    bit64::as.integer64(c(7L, 8L, 9L))))
)
Rfits_write_table(tb_vec, file_vec_table)
tb_vec_read = Rfits_read_table(file_vec_table)
expect_identical(tb_vec$id, tb_vec_read$id)
expect_identical(tb_vec$vals_dbl, tb_vec_read$vals_dbl)
expect_identical(tb_vec$vals_int, tb_vec_read$vals_int)
expect_identical(tb_vec$vals_lgc, tb_vec_read$vals_lgc)
expect_identical(tb_vec$vals_i64, tb_vec_read$vals_i64)

#ex 51 inconsistent vector lengths should error
tb_bad = data.frame(
  id = 1:3,
  vals = I(list(1:3, 1:4, 1:3))
)
expect_error(Rfits_write_table(tb_bad, tempfile()), "inconsistent vector lengths")

#ex 52 a:end works on Rfits_pointer subsetting, matching the Zarr back-end. The
#index arguments are promises and `end` is stats::end, so the range must be
#resolved from the unevaluated expression: forcing it first used to die with an
#NA/NaN argument
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)
temp_point_image = Rfits_point(file_image)
#i alone, and j alone
expect_equal(temp_point_image[50:end]$imDat, temp_image$imDat[50:356, ])
expect_equal(temp_point_image[50:150, 60:end]$imDat, temp_image$imDat[50:150, 60:356])
expect_equal(temp_point_image[50:end]$imDat, temp_point_image[50:356, ]$imDat)
#header=FALSE gives back just the array
expect_equal(temp_point_image[50:end, header=FALSE], temp_image$imDat[50:356, ])
#the start may be a variable or an expression, not just a literal
nn = 50
expect_equal(temp_point_image[nn:end]$imDat, temp_image$imDat[50:356, ])
expect_equal(temp_point_image[ceiling(356/2):end]$imDat, temp_image$imDat[178:356, ])
#`a:end` is a range, so it must not be read as a centre to put a box around
expect_equal(dim(temp_point_image[50:end]$imDat), c(307L, 356L))
#but a pair of bounds still is, so the two must not be confused
expect_equal(dim(temp_point_image[c(50, 150)]$imDat), c(201L, 201L))
expect_equal(dim(temp_point_image[c(50, 51)]$imDat), c(2L, 356L))

#ex 53 a:end works on every dimension of a pointer, and still collapses a
#singleton slice the caller asked for
temp_cube = Rfits_read_cube(system.file('extdata', 'cube.fits', package = "Rfits"))
temp_point_cube = Rfits_point(system.file('extdata', 'cube.fits', package = "Rfits"))
expect_equal(temp_point_cube[45:end, , ]$imDat, temp_cube$imDat[45:50, , ])
expect_equal(temp_point_cube[26:30, 26:30, 2:end]$imDat, temp_cube$imDat[26:30, 26:30, 2:4])
#4:end is a single slice, so it collapses to an image as for an explicit k
expect_identical(class(temp_point_cube[26:30, 26:30, 4:end])[1], 'Rfits_image')
expect_equal(temp_point_cube[26:30, 26:30, 4:end]$imDat, temp_cube[26:30, 26:30, 4]$imDat)
#and a 4D array, for all four dimensions
data_4d = array(as.numeric(1:240), c(4, 5, 3, 4))
file_4d = tempfile()
Rfits_write_image(data_4d, file_4d)
temp_point_4d = Rfits_point(file_4d)
expect_equal(temp_point_4d[2:end, , 2:end, 3:end]$imDat, data_4d[2:4, , 2:3, 3:4])

#ex 54 a 1D pointer keeps range meaning for a:end, as [.Rfits_vector does
data_1d = as.numeric(1:10)
file_1d = tempfile()
Rfits_write_image(data_1d, file_1d)
temp_point_vec = Rfits_point(file_1d)
expect_identical(temp_point_vec$type, 'vector')
expect_equal(temp_point_vec[3:end]$imDat, data_1d[3:10])
expect_equal(temp_point_vec[3:end]$imDat, temp_point_vec[3:10]$imDat)

#ex 55 matrix indexing (random single pixel access) still works, since the fix
#stopped forcing i to work out whether it was a range
expect_equal(temp_point_image[cbind(c(1,2,3), c(1,2,3))],
             diag(temp_image$imDat[1:3, 1:3]))

#ex 56 a:end on the in RAM [.Rfits_image. This failed for a different reason to
#the pointer: the start was deparsed to text and glued into c(start, end) as the
#string "50", so i was character and every later min/max/arithmetic died
expect_equal(temp_image[50:end]$imDat, temp_image$imDat[50:356, ])
expect_equal(temp_image[50:150, 60:end]$imDat, temp_image$imDat[50:150, 60:356])
expect_equal(temp_image[50:end, 60:end]$imDat, temp_image$imDat[50:356, 60:356])
expect_equal(temp_image[50:end, header=FALSE], temp_image$imDat[50:356, ])
#the start may be a variable or an expression, not just a literal
expect_equal(temp_image[nn:end]$imDat, temp_image$imDat[50:356, ])
expect_equal(dim(temp_image[ceiling(356/2):end]$imDat), c(179L, 356L))
#keywords are fixed up for the cutout
expect_equal(temp_image[50:end]$keyvalues$NAXIS1, 307L)
expect_equal(temp_image[50:end]$keyvalues$CRPIX1, temp_image$keyvalues$CRPIX1 - 50 + 1L)
#and the pointer agrees with the in RAM image for the same call
expect_equal(temp_image[50:end]$imDat, temp_point_image[50:end]$imDat)

#ex 57 a resolved a:end must not be mistaken for a location to centre a box on,
#while a genuine pair of bounds still is
expect_equal(dim(temp_image[50:end]$imDat), c(307L, 356L))
expect_equal(dim(temp_image[c(50, 150)]$imDat), c(201L, 201L))
#adjacent values really are a range
expect_equal(dim(temp_image[c(50, 51)]$imDat), c(2L, 356L))

#ex 58 a:end on [.Rfits_vector, which had no handling for it at all
data_1d = as.numeric(1:10)
file_1d_vec = tempfile()
Rfits_write_vector(data_1d, file_1d_vec)
temp_vec = Rfits_read_vector(file_1d_vec)
expect_identical(class(temp_vec)[1], 'Rfits_vector')
#as for every [.Rfits_vector subset the result is a 1D array rather than a bare
#numeric, so compare the values
expect_equal(as.vector(temp_vec[3:end]$imDat), data_1d[3:10])
expect_equal(dim(temp_vec[3:end]$imDat), 8L)
expect_equal(temp_vec[3:end]$imDat, temp_vec[3:10]$imDat)
expect_equal(as.vector(temp_vec[3:end, header=FALSE]), data_1d[3:10])
#variable and expression starts
nn2 = 4
expect_equal(as.vector(temp_vec[nn2:end]$imDat), data_1d[4:10])
#NAXIS1 describes the subset that came back
expect_equal(temp_vec[3:end]$keyvalues$NAXIS1, 8L)
expect_equal(temp_vec[3:end]$keyvalues$XCUTLO, 3L)
expect_equal(temp_vec[3:end]$keyvalues$XCUTHI, 10L)
#a 1D vector keeps range meaning for c(a,b), since there is no box to centre
expect_equal(as.vector(temp_vec[c(3, 8)]$imDat), data_1d[3:8])

#ex 59 a:end on [.Rfits_cube and [.Rfits_array, now resolved through the same
#helper as the pointer and the image. These already worked via eval(parse()), so
#the point of these tests is that the shared path keeps their semantics, notably
#a start that is a variable or an expression rather than a literal
temp_cube_ram = Rfits_read_cube(system.file('extdata', 'cube.fits', package = "Rfits"))
expect_equal(temp_cube_ram[46:end, 46:end, 3:end]$imDat,
             temp_cube_ram$imDat[46:50, 46:50, 3:4])
expect_equal(temp_cube_ram[46:end, , ]$imDat, temp_cube_ram$imDat[46:50, , ])
expect_equal(temp_cube_ram[, 1:end, ]$imDat, temp_cube_ram$imDat)
expect_equal(dim(temp_cube_ram[, , 3:end]$imDat), c(50L, 50L, 2L))
#variable and expression starts
n45 = 45
expect_equal(dim(temp_cube_ram[n45:end, , ]$imDat), c(6L, 50L, 4L))
expect_equal(dim(temp_cube_ram[ceiling(50/2):end, , ]$imDat), c(26L, 50L, 4L))
#a start above the end still runs backwards to 1, as min/max is all it is used for
expect_equal(dim(temp_cube_ram[40:10, , ]$imDat), c(31L, 50L, 4L))
#a slice of 1:end covers the whole dimension, so the object is returned untouched
expect_identical(temp_cube_ram[1:end, 1:end, 1:end], temp_cube_ram)
#keywords fixed up, and a singleton k:end still collapses to an image
expect_equal(temp_cube_ram[46:end, , ]$keyvalues$NAXIS1, 5L)
expect_equal(temp_cube_ram[46:end, , ]$keyvalues$CRPIX1,
             temp_cube_ram$keyvalues$CRPIX1 - 46 + 1L)
expect_identical(class(temp_cube_ram[26:30, 26:30, 4:end])[1], 'Rfits_image')
expect_equal(dim(temp_cube_ram[26:30, 26:30, 4:end, collapse = FALSE]$imDat),
             c(5L, 5L, 1L))
expect_equal(dim(temp_cube_ram[46:end, , , header = FALSE]), c(5L, 50L, 4L))

#ex 60 the same for a 4D array, across all four dimensions
data_4d_ram = array(as.numeric(1:240), c(4, 5, 3, 4))
file_4d_ram = tempfile()
Rfits_write_image(data_4d_ram, file_4d_ram)
temp_array_ram = Rfits_read_image(file_4d_ram)
expect_identical(class(temp_array_ram)[1], 'Rfits_array')
expect_equal(temp_array_ram[2:end, 3:end, 2:end, 2:end]$imDat,
             data_4d_ram[2:4, 3:5, 2:3, 2:4])
expect_equal(dim(temp_array_ram[2:end, , , ]$imDat), c(3L, 5L, 3L, 4L))
expect_equal(dim(temp_array_ram[, , , 2:end]$imDat), c(4L, 5L, 3L, 3L))
expect_equal(dim(temp_array_ram[2:3, 1:end, , 3:end]$imDat), c(2L, 5L, 3L, 2L))
n2 = 2
expect_equal(dim(temp_array_ram[n2:end, , , ]$imDat), c(3L, 5L, 3L, 4L))
#collapsing still keys off the slice being a singleton, however it was written
expect_identical(class(temp_array_ram[1:2, 1:2, 1:2, 4:end])[1], 'Rfits_cube')
expect_identical(class(temp_array_ram[1:2, 1:2, 3:end, 4:end])[1], 'Rfits_image')
expect_equal(dim(temp_array_ram[2:end, , , , header = FALSE]), c(3L, 5L, 3L, 4L))
expect_equal(temp_array_ram[2:end, , , ]$keyvalues$NAXIS1, 3L)

#ex 61 pixscale / pixarea on a cube pointer. wcslib takes the number of
#coordinate axes from the header, so a cube header while only RA and Dec are
#being projected leaves ncoord and nelem inconsistent with the wcsprm. Rwcs
#reported that on stderr and returned zeros, which is what both methods gave
#back, and repeated calls corrupted memory in Cwcs_head_p2s. The keywords are
#now trimmed to the two celestial axes before projecting
skip_if_not_installed("Rwcs")
file_cube_fits = system.file('extdata', 'cube.fits', package = "Rfits")
point_cube_fits = Rfits_point(file_cube_fits)
cube_scale = pixscale(point_cube_fits)
#the cube is built from CDELT, so that is the answer to check against
expect_equal(cube_scale, abs(temp_cube$keyvalues$CDELT1) * 3600, tolerance = 1e-6)
expect_equal(pixarea(point_cube_fits), cube_scale^2, tolerance = 1e-6)
#loc selects where the scale is measured, and all positions agree on this cube
expect_equal(pixscale(point_cube_fits, loc = 'bl'), cube_scale, tolerance = 1e-6)
expect_equal(pixscale(point_cube_fits, loc = c(20, 30)), cube_scale, tolerance = 1e-6)
#units are still honoured
expect_equal(pixscale(point_cube_fits, unit = 'deg') * 3600, cube_scale, tolerance = 1e-6)
#centre and corners only ever project one position at a time, so they were
#already right, and must stay right now the header is trimmed. There is no
#centre / corners method for an in RAM Rfits_cube, so the header is the
#independent path to check the pointer against
cube_header = Rfits_read_header(file_cube_fits)
expect_equal(centre(point_cube_fits), centre(cube_header))
expect_equal(corners(point_cube_fits), corners(cube_header))
#the corruption needed repeated calls to show, so loop. One assertion at the end
#keeps the expectation count down while still running the calls
for(i in 1:200){
  scale_loop = pixscale(point_cube_fits)
  area_loop = pixarea(point_cube_fits)
}
expect_equal(c(scale_loop, area_loop), c(cube_scale, cube_scale^2), tolerance = 1e-6)

#ex 62 a 2D image is unaffected, since a header that claims no more than two
#axes is passed through untouched. Comparing a pointer with the same image in RAM
#would be circular here, as both go through the one shared method, so the values
#image.fits has always produced are pinned instead
pixscale_ref = 0.339000044241
pixarea_ref = 0.114921029995
point_image_fits = Rfits_point(file_image)
expect_equal(pixscale(point_image_fits), pixscale_ref, tolerance = 1e-10)
expect_equal(pixarea(point_image_fits), pixarea_ref, tolerance = 1e-10)
expect_equal(pixscale(temp_image), pixscale_ref, tolerance = 1e-10)
expect_equal(pixarea(temp_image), pixarea_ref, tolerance = 1e-10)
#and every documented loc option still returns a sensible scale
for(loc in c('cen', 'bl', 'tl', 'tr', 'br')){
  expect_equal(pixscale(point_image_fits, loc = loc), pixscale(temp_image, loc = loc))
  expect_equal(pixarea(point_image_fits, loc = loc), pixarea(temp_image, loc = loc))
}
expect_equal(pixscale(point_image_fits, loc = c(100, 200)), pixscale(temp_image, loc = c(100, 200)))
expect_equal(pixscale(point_image_fits, useraw = FALSE), pixscale(point_image_fits))
#centre and corners on the 2D image, likewise pinned
expect_equal(corners(point_image_fits)[1, 'RA'], 352.311115817, tolerance = 1e-8)
expect_equal(corners(point_image_fits)[1, 'Dec'], -31.839058568, tolerance = 1e-8)

#ex 63 the fixed width form has to survive a round trip unchanged. The Zarr
#pointers hold the keywords but no raw header, so the cards are rebuilt from
#them before being handed to wcslib, and anything the rebuild loses is lost to
#the projection as well
keyvalues_image = Rfits_point(file_image)$keyvalues
expect_identical(keyvalues_image, Rfits_raw_to_keyvalues(Rfits_keyvalues_to_raw(keyvalues_image)))
#rebuilding from the keywords just read back has to give the same text, so a
#pointer and a Zarr array written from one image project identically
expect_identical(Rfits_keyvalues_to_raw(keyvalues_image),
                 Rfits_keyvalues_to_raw(Rfits_raw_to_keyvalues(Rfits_keyvalues_to_raw(keyvalues_image))))

#split the raw form back into cards, and pull the value field out of one of them
raw_cards = function(keyvalues){
  raw = Rfits_keyvalues_to_raw(keyvalues)
  n = nchar(raw)/80
  substring(raw, 1+(80*((1:n)-1)), 80*(1:n))
}
card_value = function(keyvalues, keyname){
  cards = raw_cards(keyvalues)
  card = grep(paste0('^', keyname, '\\s*='), cards, value = TRUE)
  #strip the keyword and equals, then the comment, leaving just the value field
  trimws(sub('\\s*/.*$', '', sub('^\\S+\\s*=\\s*', '', card)))
}

#A projection keyword is only worth eleven significant figures if the rebuild is
#allowed to round it. CD1_1 is negative, small and long, so it exercised every
#part of the formatting at once
expect_identical(card_value(keyvalues_image, 'CD1_1'), '-9.4166662957930E-05')
expect_identical(card_value(keyvalues_image, 'CD2_2'), '9.4166662957930E-05')
expect_identical(card_value(keyvalues_image, 'CRVAL1'), '352.2914408')
#the scale has to be readable as the double it came from, not merely close
expect_identical(as.numeric(card_value(keyvalues_image, 'CD1_1')), keyvalues_image$CD1_1)

#An integer keyword may carry neither a decimal point nor an exponent. The
#magnitude test was made on the value rather than its absolute value, so every
#negative one was written exponentially, which no compliant reader accepts
expect_identical(card_value(keyvalues_image, 'BITPIX'), '-32')
expect_identical(card_value(keyvalues_image, 'NAXIS1'), '356')
expect_identical(card_value(keyvalues_image, 'EQUINOX'), '2000')
expect_identical(card_value(keyvalues_image, 'CD1_2'), '0')
#and the same holds for the keywords a compressed image carries, which are both
#negative and large
expect_identical(card_value(list(PCOUNT = 122021786L, NEGINT = -42L, ZBITPIX = -64L), 'PCOUNT'),
                 '122021786')
expect_identical(card_value(list(PCOUNT = 122021786L, NEGINT = -42L, ZBITPIX = -64L), 'NEGINT'),
                 '-42')
#whole numbers are integers whichever way they arrive, since the keywords read
#off disk are stored that way
expect_identical(card_value(list(NAXIS2 = 14000), 'NAXIS2'), '14000')
expect_identical(card_value(list(NAXIS2 = 14000), 'NAXIS2'),
                 card_value(list(NAXIS2 = 14000L), 'NAXIS2'))
#a value that is whole but too large for an integer keeps the exponential form
expect_identical(card_value(list(ZRANGE = 1e15), 'ZRANGE'), '1.0000000000000E+15')
#negative numbers in the middle of the range are plain, as the positive ones were
expect_identical(card_value(list(NEGDEC = -0.5), 'NEGDEC'), '-0.5')
expect_identical(card_value(keyvalues_image, 'CTYPE1'), "'RA---TAN'")

#Thirteen digits is the most a twenty character value field can hold, so nothing
#may spill out of it and shove the comment off its column. A negative with a
#three digit exponent is the worst case
wide = list(TINY = -1e-300, HUGE = -1.2345678901234e100, MAXD = -.Machine$double.xmax)
expect_identical(nchar(raw_cards(wide)), rep(80L, 3))
#the value field runs from column eleven to column thirty, so the comment always
#opens on the thirty second column
expect_identical(substring(raw_cards(wide), 32, 32), rep('/', 3))
for(keyname in names(wide)){
  expect_lte(nchar(card_value(wide, keyname)), 20)
}
#small negatives keep the exponential form, which is what the abs() test selects
#them for
expect_identical(card_value(list(TINY = -1e-09), 'TINY'), '-1.0000000000000E-09')

#the keywords as the reader stores them are the input a pointer actually has, so
#this is the case the corners of a Zarr array were being compared against
keyvalues_check = list(SIMPLE = TRUE, BITPIX = -32L, NAXIS = 2L, NAXIS1 = 14000L, NAXIS2 = 14000L,
                       EXTEND = TRUE, EQUINOX = 2000L, RADESYS = 'ICRS',
                       CTYPE1 = 'RA---TAN', CTYPE2 = 'DEC--TAN', CUNIT1 = 'deg', CUNIT2 = 'deg',
                       CRVAL1 = 212, CRVAL2 = 1.5, CRPIX1 = 7000.5, CRPIX2 = 7000.5,
                       CD1_1 = -8.333333333333e-05, CD1_2 = 0L, CD2_1 = 0L,
                       CD2_2 = 8.333333333333e-05, GAIN = 3.955390716043,
                       SATURATE = 3.368085966101e-08, EXPTIME = 0L, OBJECT = 'KIDS_212.0_0.5')
keyvalues_check = Rfits_raw_to_keyvalues(Rfits_keyvalues_to_raw(keyvalues_check))
expect_identical(keyvalues_check, Rfits_raw_to_keyvalues(Rfits_keyvalues_to_raw(keyvalues_check)))
expect_identical(card_value(keyvalues_check, 'CD1_1'), '-8.3333333333330E-05')
expect_identical(card_value(keyvalues_check, 'SATURATE'), '3.3680859661010E-08')

#a header rebuilt from its own keywords has to project the same sky, which is the
#property the Zarr corners depended on. A bare keylist has no raw form to fall
#back on, so corners() is forced to rebuild one, exactly as a Zarr pointer does.
#The error was 2.3e-12 degrees, so expect_equal on the default tolerance would
#not have noticed it
check_keylist = keyvalues_check
class(check_keylist) = 'Rfits_keylist'
expect_identical(corners(check_keylist),
                 corners(structure(list(keyvalues = keyvalues_check),
                                   class = c('Rfits_header', 'list'))))
#the shipped image too, at the full width the header declares
image_header = Rfits_read_header(file_image)
expect_identical(corners(image_header), corners(point_image_fits))
expect_identical(centre(image_header), centre(point_image_fits))
expect_identical(extremes(image_header), extremes(point_image_fits))
expect_identical(pixscale(image_header), pixscale(point_image_fits))
expect_identical(pixarea(image_header), pixarea(point_image_fits))
expect_identical(rotation(image_header), rotation(point_image_fits))
#a keylist is rebuilt rather than read, so it is the one that can drift. It has
#to agree with the pointer that has the real header behind it
expect_identical(corners(keyvalues_image), corners(point_image_fits))
expect_identical(centre(keyvalues_image), centre(point_image_fits))
expect_identical(pixscale(keyvalues_image), pixscale(point_image_fits))

#ex 64 a keyword that is not a whole number must not be turned into one. The
#whole number test was made with %% 1 == 0, and R works that out in long double,
#so for a negative smaller than about 2.7e-20 the correction 1 - |x| rounds to
#the divisor and the remainder comes back as 0. The test then called the number
#whole, and as.integer() replaced it with zero. Positive numbers were never
#affected, because their remainder is the number itself
#the cards are written out by hand here, so that the reader is being tested on
#text a file could genuinely hold rather than on what our own writer produces
hand_cards = function(pairs){
  cards = vapply(names(pairs), function(keyname){
    formatC(paste0(formatC(keyname, width=8, flag='-'), '= ',
                   formatC(pairs[[keyname]], width=20), ' /'), width=80, flag='-')
  }, character(1))
  return(paste(cards, collapse=''))
}

keyvalues_tiny = list(TINY = -1e-300, V20 = -1e-20, V25 = -1e-25, POS = 1e-300, SUB = 4.9e-320)
keyvalues_back = Rfits_raw_to_keyvalues(Rfits_keyvalues_to_raw(keyvalues_tiny))
expect_identical(keyvalues_back$TINY, -1e-300)
expect_identical(keyvalues_back$V20, -1e-20)
expect_identical(keyvalues_back$V25, -1e-25)
expect_identical(keyvalues_back$POS, 1e-300)
expect_identical(keyvalues_back$SUB, 4.9e-320)
#none of these may be reported as an integer
expect_identical(unname(vapply(keyvalues_back, storage.mode, character(1))),
                 rep('double', 5))

#through the reader alone, off cards that are already in fixed width form
read_back = Rfits_raw_to_keyvalues(hand_cards(list(
  TINY = '-1.000000000000E-300', V20 = '-1.0000000000000E-20',
  N = '              14000', H = '            7000.5')))
expect_identical(read_back$TINY, -1e-300)
expect_identical(read_back$V20, -1e-20)
#while a genuine integer keyword still comes back as one
expect_identical(read_back$N, 14000L)
expect_identical(read_back$H, 7000.5)

#the same test decides whether Rfits_read_key with keytype = 'auto' returns an
#integer or a double, so it has to agree with the reader above. A whole number
#still comes back as an integer, and a tiny negative as the double it is
expect_identical(read_back$N, 14000L)

#the writer had the same flaw, where a whole number test picked between an
#integer and a double card. A tiny negative became an integer card holding zero,
#which loses the value outright
file_tiny = tempfile(fileext='.fits')
Rfits_write_image(matrix(as.numeric(1:4), 2, 2), file_tiny)
write_then_read = function(keyname, keyvalue){
  Rfits_write_key(file_tiny, keyname, keyvalue, ext = 1)
  return(Rfits_read_key(file_tiny, keyname, keytype = 'auto', ext = 1))
}
expect_identical(write_then_read('NEG20', -1e-20), -1e-20)
expect_identical(write_then_read('NEG300', -1e-300), -1e-300)
#42 is whole, so it is still stored as an integer rather than a double
expect_identical(write_then_read('WHOLE', 42), 42L)
#a half and a modest double are untouched by any of this
expect_identical(write_then_read('HALF', -0.5), -0.5)
expect_identical(write_then_read('REF', 7000.5), 7000.5)
#and the card that comes off disk is a real one, not an integer holding zero
expect_identical(write_then_read('V25', -1e-25), -1e-25)

#Inf is its own rounding, so a test built on equality alone would have called it
#whole and passed it to as.integer(), which is undefined. NA is the other case,
#where the remainder is NA and if() in the writer failed outright on 'missing
#value where TRUE/FALSE needed'. Both are checked through the public functions
expect_identical(card_value(list(INF = Inf), 'INF'), 'Inf')
expect_identical(card_value(list(NINF = -Inf), 'NINF'), '-Inf')
expect_identical(Rfits_raw_to_keyvalues(hand_cards(list(INFC = '                 Inf')))$INFC, Inf)
#the writer used to error here rather than write anything
expect_identical(write_then_read('NAK', NA_real_), NA)
