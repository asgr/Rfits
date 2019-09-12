file=system.file('extdata','table.fits',package="Rfits")
for(i in 1:35){
  print(i)
  temp=Rfits::Rfits_read_col(file,colref=i)
  print(temp[1:20])
}
