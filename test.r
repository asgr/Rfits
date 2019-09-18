file=system.file('extdata','table.fits',package="Rfits")
temp=Rfits::Rfits_read_table(system.file('extdata','table.fits',package="Rfits"))
Rfits::Rfits_write_table(temp, file='~/Documents/test-table.fits')
temp2=Rfits::Rfits_read_table('~/Documents/test-table.fits')
