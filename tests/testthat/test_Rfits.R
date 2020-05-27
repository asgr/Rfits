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

#ex 19 check we can read and write ascii tables
Rfits_write_table(temp_table, file_table_temp, table_type = 'ascii')
temp_table7=Rfits_read_table(file_table_temp)
expect_equal(temp_table7[,c(1,3:35)], temp_table[,c(1,3:35)]) #int64 is truncated to int by cfitsio ascii reader

#ex 20  check binary and ascii tables are the same
temp_profound = read.table(system.file('extdata', 'profound.tab', package = "Rfits"))
file_profound_bin = tempfile()
file_profound_ascii = tempfile()
Rfits_write_table(temp_profound, filename = file_profound_bin)
Rfits_write_table(temp_profound, filename = file_profound_ascii, table_type = 'ascii')
temp_profound2 = Rfits_read_table(file_profound_bin)
temp_profound3 = Rfits_read_table(file_profound_ascii)
expect_equal(temp_profound2, temp_profound3)

#ex 21 check compressions works within tolerance
file_image_temp = tempfile()
Rfits_write_image(temp_image$imDat, filename = paste(file_image_temp,'[compress]',sep=''))
temp_compress=Rfits_read_image(file_image_temp,ext=2)
expect(abs(log10(sum(temp_image$imDat)/sum(temp_compress$imDat))) < 1e-4, failure_message = 'Images differ too much!')

#ex 24 subset a pointer
temp_point = Rfits_point(file_image)
expect_equal(temp_image$imDat[1:5,1:5], temp_point[1:5,1:5])

#ex 25 read and write cubes
temp_cube = Rfits_read_image(system.file('extdata', 'cube.fits', package = "Rfits"))
file_cube_temp = tempfile()
Rfits_write_image(temp_cube, file_cube_temp)
temp_cube2 = Rfits_read_image(file_cube_temp)
expect_identical(temp_cube$imDat, temp_cube2$imDat)

#ex 26 check we treat HIERARCH keywords correctly
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)
temp_image$keyvalues$`HIERARCH  TEST` = 100L
temp_image$keynames=c(temp_image$keynames, 'HIERARCH  TEST')
temp_image$keycomments$`HIEARCH  TEST` = ''
file_image_temp = tempfile()
Rfits_write_image(temp_image, file_image_temp)
temp_image_hier = Rfits_read_image(file_image_temp, remove_HIERARCH = FALSE)
expect_identical(temp_image$keyvalues, temp_image_hier$keyvalues)

#ex 27 check DATASUM
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)
file_image_temp = tempfile()
Rfits_write_image(temp_image, file_image_temp)
Rfits_write_chksum(file_image_temp)
temp_check = Rfits_verify_chksum(file_image_temp)
expect_identical(as.character(temp_check['DATASUM']), "correct")

#ex 28 check CHECKSUM
expect_identical(as.character(temp_check['CHECKSUM']), "correct")

#ex 29 check [] methods work for images
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)
expect_identical(temp_image$imDat[1:5,1:5], temp_image[1:5,1:5])

#ex 30 check [] methods work for arrays
temp_cube = Rfits_read_cube(system.file('extdata', 'cube.fits', package = "Rfits"))
expect_identical(temp_cube$imDat[26:30,26:30,1:2], temp_cube[26:30,26:30,1:2])

#ex 31 check consistent BZERO and BSCALE reading and writing
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

#ex 32 check consistent TZEROn and TSCALn reading and writing
file_table = system.file('extdata', 'table.fits', package = "Rfits")
temp_table = Rfits_read_table(file_table)
file_table_temp = tempfile()
Rfits_write_table(temp_table, file_table_temp, tadd=list(TSCAL6=2, TZERO6=10, TSCAL13=10))
temp_table2 = Rfits_read_table(file_table_temp)
expect_identical(temp_table, temp_table2)

#ex 33 tdigest checks
file_image=system.file('extdata', 'image.fits', package = "Rfits")
temp_image=Rfits_read_image(file_image)
td=tdigest(temp_image$imDat, compression=1e3) 
expect_equal(median(temp_image$imDat), td[0.5], tolerance=2e-3)
