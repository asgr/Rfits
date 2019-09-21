context("Check Rfits table/image read/write")
library(Rfits)
library(testthat)

file_image=system.file('extdata', 'image.fits', package = "Rfits")
temp_image=Rfits_read_image(file_image)
file_image_temp = tempfile()
Rfits_write_image(temp_image, file_image_temp)
temp_image2 = Rfits_read_image(file_image_temp)

#ex 1
expect_identical(temp_image$imDat, temp_image2$imDat) 

#ex 2
expect_identical(temp_image$keyvalues, temp_image2$keyvalues) 

#ex 3
expect_identical(temp_image$comments, temp_image2$comments) 

Rfits_write_image(temp_image, file_image_temp, numeric=64)
temp_image2 = Rfits_read_image(file_image_temp)

#ex 4
expect_identical(temp_image$imDat, temp_image2$imDat) 

temp_image_int=matrix(as.integer(temp_image$imDat), 356, 356)
Rfits_write_image(temp_image_int, file_image_temp)
temp_image_int2 = Rfits_read_image(file_image_temp)

#ex 5
expect_identical(temp_image_int, temp_image_int2$imDat) 

file_table = system.file('extdata', 'table.fits', package = "Rfits")
temp_table = Rfits_read_table(file_table)
file_table_temp = tempfile()
Rfits_write_table(temp_table, file_table_temp)
temp_table2 = Rfits_read_table(file_table_temp)

#ex 6
expect_identical(temp_table, temp_table2)
