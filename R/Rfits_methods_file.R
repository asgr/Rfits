Rfits_centre = function(filename, ext=1, useraw=FALSE, ...){
  temp_header = Rfits_read_header(filename=filename, ext=ext)
  return(centre(temp_header, useraw=useraw, ...))
}

Rfits_center = Rfits_centre

Rfits_corners = function(filename, ext=1, useraw=FALSE, ...){
  temp_header = Rfits_read_header(filename=filename, ext=ext)
  return(corners(temp_header, useraw=useraw, ...))
}

Rfits_pixscale = function(filename, ext=1, useraw=FALSE, unit='asec', ...){
  temp_header = Rfits_read_header(filename=filename, ext=ext)
  return(pixscale(temp_header, useraw=useraw, unit=unit, ...))
}


Rfits_pixarea = function(filename, ext=1, useraw=FALSE, unit='asec2', ...){
  temp_header = Rfits_read_header(filename=filename, ext=ext)
  return(pixarea(temp_header, useraw=useraw, unit=unit, ...))
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
