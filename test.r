file=system.file('extdata','table.fits',package="Rfits")
for(i in 1:35){
  print(i)
  temp=Rfits::Rfits_read_col(file,colref=i)
  print(temp[1:20])
}

Rfits::Rfits_read_colname(system.file('extdata','table.fits',package="Rfits"))

file=system.file('extdata','table.fits',package="Rfits")
temp=Rfits::Rfits_read_table(file)
Rfits::Rfits_write_bintable('/Users/aaron/Documents/test_table5.fits')