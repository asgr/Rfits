file=system.file('extdata','table.fits',package="Rfits")
for(i in 1:35){
  print(i)
  temp=Rfits::Rfits_read_col(file,colref=i)
  print(temp[1:20])
}

Rfits::Rfits_read_colname(system.file('extdata','table.fits',package="Rfits"))

file=system.file('extdata','table.fits',package="Rfits")
temp=Rfits::Rfits_read_table(system.file('extdata','table.fits',package="Rfits"))
Rfits::Rfits_write_table(temp, file='~/Documents/test-table.fits')
temp2=Rfits::Rfits_read_table('~/Documents/test-table.fits')

Rfits::Rfits_write_bintable('/Users/aaron/Documents/test_table5.fits')

Rfits::Rfits_create_bintable('/Users/aaron/Documents/test_table.fits',tfields=1, ttypes='Col1', tforms='1E', tunits='0', extname='Main', ext=2)
