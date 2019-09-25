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
#illegally read ext 5 and get error
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
