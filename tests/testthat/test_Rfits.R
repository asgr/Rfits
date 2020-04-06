context("Check Rfits table/image read/write")
library(Rfits)
library(testthat)
library(FITSio)

file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)
temp_image_FITSio = readFITS(file_image)
file_image_temp = tempfile()
Rfits_write_image(temp_image, file_image_temp)
temp_image2 = Rfits_read_image(file_image_temp)

#ex 1
expect_identical(temp_image$imDat, temp_image_FITSio$imDat) 

#ex 2
expect_identical(temp_image$imDat, temp_image2$imDat) 

#Write multi-extension:
Rfits_write_image(temp_image, file_image_temp, overwrite_file=F, create_file=F,
                  create_ext=T)
temp_image3 = Rfits_read_image(file_image_temp, ext=2)

#ex 3
expect_identical(temp_image2$imDat, temp_image3$imDat) 

Rfits_write_image(temp_image, file_image_temp, overwrite_file=F, create_file=F, create_ext=T)
#illegally read ext 3 and get error that we ignore
try(Rfits_read_image(file_image_temp, ext=3), silent=TRUE)
#carry on writing
temp=try(Rfits_write_image(temp_image, file_image_temp, overwrite_file=F, create_file=F, create_ext=T))

#ex 4
expect(class(temp)=='NULL', "Could not write new extension!")

#ex 5
expect_identical(temp_image$keyvalues, temp_image2$keyvalues) 

#ex 6
expect_identical(temp_image$comments, temp_image2$comments) 

Rfits_write_image(temp_image, file_image_temp, numeric=64)
temp_image2 = Rfits_read_image(file_image_temp)

#ex 7
expect_identical(temp_image$imDat, temp_image2$imDat) 

temp_image_int = matrix(as.integer(temp_image$imDat), 356, 356)
Rfits_write_image(temp_image_int, file_image_temp)
temp_image_int2 = Rfits_read_image(file_image_temp)

#ex 8
expect_identical(temp_image_int, temp_image_int2$imDat)

temp_image_int[temp_image_int> 2^15] = 0L
Rfits_write_image(temp_image_int, file_image_temp, integer=16)
temp_image_int2 = Rfits_read_image(file_image_temp)
temp_image_int_FITSio = readFITS(file_image_temp)

#ex 9
expect_identical(temp_image_int, temp_image_int_FITSio$imDat)

#ex 10
expect_identical(temp_image_int, temp_image_int2$imDat)

file_table = system.file('extdata', 'table.fits', package = "Rfits")
temp_table = Rfits_read_table(file_table)
file_table_temp = tempfile()
Rfits_write_table(temp_table, file_table_temp)
temp_table2 = Rfits_read_table(file_table_temp)

#ex 11
expect_identical(temp_table, temp_table2)

Rfits_write_table(temp_table, file_table_temp, overwrite_file=F, create_file=F, create_ext=T)
temp_table3 = Rfits_read_table(file_table_temp, ext=3)

#ex 12
expect_identical(temp_table, temp_table3)

file_mix_temp = tempfile()
Rfits_write_image(temp_image, file_mix_temp)
Rfits_write_table(temp_table, file_mix_temp, overwrite_file=F, create_file=F, create_ext=T)

temp_image3 = Rfits_read_image(file_mix_temp)

#ex 13
expect_identical(temp_image$imDat, temp_image3$imDat) 

temp_table4 = Rfits_read_table(file_mix_temp, ext=2)

#ex 14
expect_identical(temp_table, temp_table4)

file_mix_summary = Rfits_info(file_mix_temp)$summary

#ex 15
expect_length(file_mix_summary, 2)

Rfits_write_image(temp_image$imDat[1:100,1:100], file_mix_temp, overwrite_file=F, create_file=F, create_ext=T)
Rfits_write_table(temp_table[1:50,], file_mix_temp, overwrite_file=F, create_file=F, create_ext=T)

temp_image4 = Rfits_read_image(file_mix_temp, ext=3)

#ex 16
expect_identical(temp_image4$imDat, temp_image$imDat[1:100,1:100])

temp_table5 = Rfits_read_table(file_mix_temp, ext=4)

#ex 17
expect_identical(temp_table5, temp_table[1:50,])

Rfits_write_table(temp_table[1:60,], file_mix_temp, overwrite_file=F, create_file=F, create_ext=F, ext=3) #delete ext 3 and append to end
temp_table6 = Rfits_read_table(file_mix_temp, ext=4)

#ex 18
expect_identical(temp_table6, temp_table[1:60,])

Rfits_write_table(temp_table, file_table_temp, table_type = 'ascii')
temp_table7=Rfits_read_table(file_table_temp)

#ex 19
expect_equal(temp_table7[,c(1,3:35)], temp_table[,c(1,3:35)]) #int64 is truncated to int by cfitsio ascii reader

temp_profound = read.table(system.file('extdata', 'profound.tab', package = "Rfits"))
file_profound_bin = tempfile()
file_profound_ascii = tempfile()
Rfits_write_table(temp_profound, filename = file_profound_bin)
Rfits_write_table(temp_profound, filename = file_profound_ascii, table_type = 'ascii')

temp_profound2 = Rfits_read_table(file_profound_bin)
temp_profound3 = Rfits_read_table(file_profound_ascii)

#ex 20
expect_equal(temp_profound2, temp_profound3)

file_image_temp = tempfile()
Rfits_write_image(temp_image$imDat, filename = paste(file_image_temp,'[compress]',sep=''))
temp_compress=Rfits_read_image(file_image_temp,ext=2)

#ex 21
expect(abs(log10(sum(temp_image$imDat)/sum(temp_compress$imDat))) < 1e-4, failure_message = 'Images differ too much!')

#ex 24
temp_point = Rfits_point(file_image)
expect_equal(temp_image$imDat[1:5,1:5], temp_point[1:5,1:5])

#ex 25
temp_cube = Rfits_read_image(system.file('extdata', 'cube.fits', package = "Rfits"))
file_cube_temp = tempfile()
Rfits_write_image(temp_cube, file_cube_temp)
temp_cube2 = Rfits_read_image(file_cube_temp)
expect_identical(temp_cube$imDat, temp_cube2$imDat)

#ex 26
file_image = system.file('extdata', 'image.fits', package = "Rfits")
temp_image = Rfits_read_image(file_image)
temp_image$keyvalues$`HIERARCH  TEST` = 100L
temp_image$keynames=c(temp_image$keynames, 'HIERARCH  TEST')
temp_image$keycomments$`HIEARCH  TEST` = ''
file_image_temp = tempfile()
Rfits_write_image(temp_image, file_image_temp)
temp_image_hier = Rfits_read_image(file_image_temp, remove_HIERARCH = FALSE)
expect_identical(temp_image$keyvalues, temp_image_hier$keyvalues)
