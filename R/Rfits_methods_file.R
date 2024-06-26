Rfits_centre = function(filename, ext=1, useraw=TRUE, ...){
  temp_header = Rfits_read_header(filename=filename, ext=ext)
  return(centre(temp_header, useraw=useraw, ...))
}

Rfits_center = Rfits_centre

Rfits_corners = function(filename, ext=1, useraw=TRUE, RAneg=FALSE, ...){
  temp_header = Rfits_read_header(filename=filename, ext=ext)
  return(corners(temp_header, useraw=useraw, RAneg=RAneg, ...))
}

Rfits_extremes = function(filename, ext=1, useraw=TRUE, unit='amin', RAneg=FALSE, ...){
  temp_header = Rfits_read_header(filename=filename, ext=ext)
  return(extremes(temp_header, useraw=useraw, unit=unit, RAneg=RAneg, ...))
}

Rfits_pixscale = function(filename, ext=1, useraw=TRUE, unit='asec', loc='cen', ...){
  temp_header = Rfits_read_header(filename=filename, ext=ext)
  return(pixscale(temp_header, useraw=useraw, unit=unit, loc='cen', ...))
}


Rfits_pixarea = function(filename, ext=1, useraw=TRUE, unit='asec2', loc='cen', ...){
  temp_header = Rfits_read_header(filename=filename, ext=ext)
  return(pixarea(temp_header, useraw=useraw, unit=unit, loc='cen', ...))
}

Rfits_length = function(filename, ext=1){
  temp_header = Rfits_read_header(filename=filename, ext=ext)
  return(length(temp_header))
}

Rfits_dim = function(filename, ext=1){
  temp_header = Rfits_read_header(filename=filename, ext=ext)
  return(dim(temp_header))
}

Rfits_rotation = function(filename, ext=1, keypass = TRUE, ...){
  temp_header = Rfits_read_header(filename=filename, ext=ext)
  return(rotation(temp_header, keypass=keypass, ...))
}
