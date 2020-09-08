context("Check Rfits table/image read/write")
#load packages
library(Rfits)
library(testthat)
library(FITSio)
library(tdigest)

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
expect(class(temp)=='NULL', "Could not write new extension!")

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
Rfits_write_all(temp_mix, file_mix_temp2)
temp_mix2 = Rfits_read_all(file_mix_temp2)
attributes(temp_mix)$filename = attributes(temp_mix2)$filename
temp_mix2[[1]]$filename = temp_mix[[1]]$filename #should be only changes
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
temp_compress=Rfits_read_image(file_image_temp,ext=2)
expect(abs(log10(sum(temp_image$imDat)/sum(temp_compress$imDat))) < 1e-4, failure_message = 'Images differ too much!')

#ex 23 subset a pointer
temp_point = Rfits_point(file_image)
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
expect_identical(temp_image$imDat[1:5,1:5], temp_image[1:5,1:5])

#ex 29 check [] methods work for cubes
temp_cube = Rfits_read_cube(system.file('extdata', 'cube.fits', package = "Rfits"))
expect_identical(temp_cube$imDat[26:30,26:30,1:2], temp_cube[26:30,26:30,1:2])

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

#ex 32 tdigest checks
file_image=system.file('extdata', 'image.fits', package = "Rfits")
temp_image=Rfits_read_image(file_image)
td=tdigest(temp_image$imDat, compression=1e3) 
expect_equal(median(temp_image$imDat), td[0.5], tolerance=2e-3)

#ex 33 pure header
temp_head=list(
  SIMPLE=TRUE,
  BITPIX=16L,
  NAXIS=0L,
  EXTEND=TRUE,
  RANDOM='Hello'
)
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
