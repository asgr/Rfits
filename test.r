file=system.file('extdata','table.fits',package="Rfits")
temp=Rfits::Rfits_read_table(system.file('extdata','table.fits',package="Rfits"))
Rfits::Rfits_write_table(temp, file='~/Documents/test-table.fits')
temp2=Rfits::Rfits_read_table('~/Documents/test-table.fits')

library(Rfits)
temp=Rfits_read_image('~/Downloads/cut_test.fits')
temp2=temp$imDat
for(i in 1:3){temp2=rbind(temp2,temp2);temp2=cbind(temp2,temp2)}
Rfits_write_image('~/Downloads/big_image.fits', temp2)