#centre

centre = function(x, useraw=FALSE, ...){
  UseMethod("centre", x)
}

centre.Rfits_image = function(x, useraw=FALSE, ...){
  if(!inherits(x, c('Rfits_image', 'Rfits_pointer', 'Rfits_header', 'Rfits_keylist'))){
    stop('Object class is not of type Rfits_image / Rfits_pointer / Rfits_header / Rfits_keylist')
  }
  
  if(inherits(x, 'Rfits_keylist')){
    keyvalues = x
  }else{
    keyvalues = x$keyvalues
  }
  
  if(inherits(x, c('Rfits_header', 'Rfits_keylist'))){
    if(is.null(keyvalues$NAXIS) & is.null(keyvalues$ZNAXIS)){
      message('No NAXIS! Probably not an image, returning NA.')
      return(NA)
    }else{
      if(!is.null(keyvalues$ZNAXIS)){
        if(keyvalues$ZNAXIS < 2){
          message('ZNAXIS: ', keyvalues$ZNAXIS,'.  Probably not an image, returning NA.')
          return(NA)
        }
      }else if(keyvalues$NAXIS < 2){
        message('NAXIS: ', keyvalues$NAXIS,'.  Probably not an image, returning NA.')
        return(NA)
      }
    }
  }
  
  im_dim = dim(x)
  if(requireNamespace("Rwcs", quietly=TRUE)){
    if(useraw){header = x$raw}else{header = NULL}
    output = Rwcs::Rwcs_p2s(im_dim[1]/2, im_dim[2]/2, keyvalues = keyvalues, header=header, pixcen='R', ...)
    return(output)
  }else{
    message('The Rwcs package is needed to find the centre of a Rfits_image object.')
  }
}

center = function(x, useraw=FALSE, ...){
  UseMethod("center", x)
}

#other useful methods:
center.Rfits_image = centre.Rfits_image
centre.Rfits_pointer = centre.Rfits_image
center.Rfits_pointer = centre.Rfits_image
centre.Rfits_header = centre.Rfits_image
center.Rfits_header = centre.Rfits_image
centre.Rfits_keylist = centre.Rfits_image
center.Rfits_keylist = centre.Rfits_image

#corners

corners = function(x, useraw=FALSE, ...){
  UseMethod("corners", x)
}

corners.Rfits_image = function(x, useraw=FALSE, ...){
  if(!inherits(x, c('Rfits_image', 'Rfits_pointer', 'Rfits_header', 'Rfits_keylist'))){
    stop('Object class is not of type Rfits_image / Rfits_pointer / Rfits_header / Rfits_keylist')
  }
  
  if(inherits(x, 'Rfits_keylist')){
    keyvalues = x
  }else{
    keyvalues = x$keyvalues
  }
  
  if(inherits(x, c('Rfits_header', 'Rfits_keylist'))){
    if(is.null(keyvalues$NAXIS) & is.null(keyvalues$ZNAXIS)){
      message('No NAXIS! Probably not an image, returning NA.')
      return(NA)
    }else{
      if(!is.null(keyvalues$ZNAXIS)){
        if(keyvalues$ZNAXIS < 2){
          message('ZNAXIS: ', keyvalues$ZNAXIS,'.  Probably not an image, returning NA.')
          return(NA)
        }
      }else if(keyvalues$NAXIS < 2){
        message('NAXIS: ', keyvalues$NAXIS,'.  Probably not an image, returning NA.')
        return(NA)
      }
    }
  }
  
  im_dim = dim(x)
  if(requireNamespace("Rwcs", quietly=TRUE)){
    if(useraw){header = x$raw}else{header = NULL}
    BL = Rwcs::Rwcs_p2s(0, 0, keyvalues = keyvalues, header=header, pixcen='R', ...)
    TL = Rwcs::Rwcs_p2s(0, im_dim[2], keyvalues = keyvalues, header=header, pixcen='R', ...)
    TR = Rwcs::Rwcs_p2s(im_dim[1], im_dim[2], keyvalues = keyvalues, header=header, pixcen='R', ...)
    BR = Rwcs::Rwcs_p2s(im_dim[1], 0, keyvalues = keyvalues, header=header, pixcen='R', ...)
    output = rbind(BL, TL, TR, BR)
    row.names(output) = c('BL', 'TL', 'TR', 'BR')
    return(output)
  }else{
    message('The Rwcs package is needed to find the corners of a Rfits_image object.')
  }
}

corners.Rfits_pointer = corners.Rfits_image
corners.Rfits_header = corners.Rfits_image
corners.Rfits_keylist = corners.Rfits_image

#pixscale

pixscale = function(x, useraw=FALSE, unit='asec', ...){
  UseMethod("pixscale", x)
}

pixscale.Rfits_image = function(x, useraw=FALSE, unit='asec', ...){
  if(!inherits(x, c('Rfits_image', 'Rfits_pointer', 'Rfits_header', 'Rfits_keylist'))){
    stop('Object class is not of type Rfits_image / Rfits_pointer / Rfits_header / Rfits_keylist')
  }
  
  if(inherits(x, 'Rfits_keylist')){
    keyvalues = x
  }else{
    keyvalues = x$keyvalues
  }
  
  if(inherits(x, c('Rfits_header', 'Rfits_keylist'))){
    if(is.null(keyvalues$NAXIS) & is.null(keyvalues$ZNAXIS)){
      message('No NAXIS! Probably not an image, returning NA.')
      return(NA)
    }else{
      if(!is.null(keyvalues$ZNAXIS)){
        if(keyvalues$ZNAXIS < 2){
          message('ZNAXIS: ', keyvalues$ZNAXIS,'.  Probably not an image, returning NA.')
          return(NA)
        }
      }else if(keyvalues$NAXIS < 2){
        message('NAXIS: ', keyvalues$NAXIS,'.  Probably not an image, returning NA.')
        return(NA)
      }
    }
  }
  
  im_dim = dim(x) #this works on all classes
  if(requireNamespace("Rwcs", quietly=TRUE)){
    if(useraw){header = x$raw}else{header = NULL}
    output = Rwcs::Rwcs_p2s(im_dim[1]/2 + c(-0.5,0.5,-0.5), im_dim[2]/2 + c(-0.5,-0.5,0.5), keyvalues = keyvalues, header=header, pixcen='R', ...)
    if(max(abs(diff(output[,1]))) > 359){
      output[output[,1] > 359,1] = output[output[,1] > 359,1] - 360
    }
    output[,1] = output[,1] * cos(mean(output[,2])*pi/180)
    scale_deg = 0.7071068*sqrt(diff(output[1:2,1])^2 + diff(output[1:2,2])^2 + diff(output[c(1,3),1])^2 + diff(output[c(1,3),2])^2) # 0.7071068 = 1/sqrt(2)
    
    if(unit=='deg'){
      return(scale_deg)
    }else if(unit == 'asec'){
      return(scale_deg*3600)
    }else if(unit == 'amin'){
      return(scale_deg*60)
    }else if(unit=='rad'){
      return(scale_deg * (pi/180))
    }else{
      message('Not a valid unit, must be one of asec / amin / deg / rad')
    }
  }else{
    message('The Rwcs package is needed to find the centre of a Rfits_image object.')
  }
}

pixscale.Rfits_pointer = pixscale.Rfits_image
pixscale.Rfits_header = pixscale.Rfits_image
pixscale.Rfits_keylist = pixscale.Rfits_image

#pixarea

pixarea = function(x, useraw=FALSE, unit='asec2', ...){
  UseMethod("pixarea", x)
}

pixarea.Rfits_image = function(x, useraw=FALSE, unit='asec2', ...){
  if(!inherits(x, c('Rfits_image', 'Rfits_pointer', 'Rfits_header', 'Rfits_keylist'))){
    stop('Object class is not of type Rfits_image / Rfits_pointer / Rfits_header / Rfits_keylist')
  }
  
  if(inherits(x, 'Rfits_keylist')){
    keyvalues = x
  }else{
    keyvalues = x$keyvalues
  }
  
  if(inherits(x, 'Rfits_header')){
    if(is.null(keyvalues$NAXIS) & is.null(keyvalues$ZNAXIS)){
      message('No NAXIS! Probably not an image, returning NA.')
      return(NA)
    }else{
      if(!is.null(keyvalues$ZNAXIS)){
        if(keyvalues$ZNAXIS < 2){
          message('ZNAXIS: ', keyvalues$ZNAXIS,'.  Probably not an image, returning NA.')
          return(NA)
        }
      }else if(keyvalues$NAXIS < 2){
        message('NAXIS: ', keyvalues$NAXIS,'.  Probably not an image, returning NA.')
        return(NA)
      }
    }
  }
  
  im_dim = dim(x) #this works on all classes
  if(requireNamespace("Rwcs", quietly=TRUE)){
    if(useraw){header = x$raw}else{header = NULL}
    output = Rwcs::Rwcs_p2s(im_dim[1]/2 + c(-0.5,0.5,-0.5), im_dim[2]/2 + c(-0.5,-0.5,0.5), keyvalues = keyvalues, header=header, pixcen='R', ...)
    if(max(abs(diff(output[,1]))) > 359){
      output[output[,1] > 359,1] = output[output[,1] > 359,1] - 360
    }
    output[,1] = output[,1] * cos(mean(output[,2])*pi/180)
    area_deg = sqrt(diff(output[1:2,1])^2 + diff(output[1:2,2])^2)*sqrt(diff(output[c(1,3),1])^2 + diff(output[c(1,3),2])^2)
    
    if(unit=='deg2'){
      return(area_deg)
    }else if(unit == 'asec2'){
      return(area_deg*3600^2)
    }else if(unit == 'amin2'){
      return(area_deg*60^2)
    }else if(unit=='rad2' | unit=='str'){
      return(area_deg * (pi/180)^2)
    }else{
      message('Not a valid unit, must be one of asec2 / amin2 / deg2 / rad2 / str')
    }
  }else{
    message('The Rwcs package is needed to find the centre of a Rfits_image object.')
  }
}

pixarea.Rfits_pointer = pixarea.Rfits_image
pixarea.Rfits_header = pixarea.Rfits_image
pixarea.Rfits_keylist = pixarea.Rfits_image

#rotation

rotation = function(x, keypass=TRUE, ...){
  UseMethod("rotation", x)
}

rotation.Rfits_image = function(x, keypass=TRUE, ...){
  if(!inherits(x, c('Rfits_image', 'Rfits_pointer', 'Rfits_header', 'Rfits_keylist'))){
    stop('Object class is not of type Rfits_image / Rfits_pointer / Rfits_header / Rfits_keylist')
  }
  
  if(inherits(x, 'Rfits_keylist')){
    keyvalues = x
  }else{
    keyvalues = x$keyvalues
  }
  
  if(inherits(x, c('Rfits_header', 'Rfits_keylist'))){
    if(is.null(keyvalues$NAXIS) & is.null(keyvalues$ZNAXIS)){
      message('No NAXIS! Probably not an image, returning NA.')
      return(NA)
    }else{
      if(!is.null(keyvalues$ZNAXIS)){
        if(keyvalues$ZNAXIS < 2){
          message('ZNAXIS: ', keyvalues$ZNAXIS,'.  Probably not an image, returning NA.')
          return(NA)
        }
      }else if(keyvalues$NAXIS < 2){
        message('NAXIS: ', keyvalues$NAXIS,'.  Probably not an image, returning NA.')
        return(NA)
      }
    }
  }
  
  if(keypass){
    if(requireNamespace("Rwcs", quietly=TRUE)){
      keyvalues = Rwcs::Rwcs_keypass(keyvalues, ...)
    }else{
      message('The Rwcs package is needed to use keypass.')
    }
  }
  
  North_ang = (atan2(keyvalues$CD1_2, keyvalues$CD2_2)*180/pi) %% 360
  East_ang = (360 - atan2(keyvalues$CD2_1, keyvalues$CD1_1)*180/pi) %% 360
  
  return(c(North = North_ang, East = East_ang))
}

rotation.Rfits_pointer = rotation.Rfits_image
rotation.Rfits_header = rotation.Rfits_image
rotation.Rfits_keylist = rotation.Rfits_image
