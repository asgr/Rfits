# The WCS methods below are shared by every object that carries keywords, which
# is what a Zarr pointer does. A pointer has no raw header though, and may point
# at an array whose metadata says nothing about the WCS, in which case Rwcs has
# nothing to work from and quietly returns nonsense. So a pointer is handed to
# the shared implementation as a header shaped delegate, with the fixed width
# form rebuilt from the keywords, exactly as [.Rfits_pointer_zarr does for
# type = "coord". Going via Rfits_header also keeps the NAXIS guards, which
# return NA with a message rather than passing a nonsense image size to Rwcs.

.zarr_wcs_delegate = function(x){
  keyvalues = x$keyvalues
  if(is.null(keyvalues)){
    #Without keywords there is no WCS to ask about, so this is an error rather
    #than the NA that a short or absent NAXIS gives
    stop('No FITS style metadata is stored for this Zarr array, so the WCS cannot be used!',
         call. = FALSE)
  }
  
  #The array shape is what the store says, so use it wherever the keywords are
  #silent. Written by Rfits they will agree with it anyway. As in
  #[.Rfits_pointer_zarr, the shape recorded at the time the pointer was made is
  #used, rather than reopening the store
  dim_x = x$dim
  if(is.null(keyvalues$NAXIS)){
    keyvalues$NAXIS = length(dim_x)
  }
  for(i in seq_along(dim_x)){
    if(is.null(keyvalues[[paste0('NAXIS', i)]])){
      keyvalues[[paste0('NAXIS', i)]] = dim_x[i]
    }
  }
  
  delegate = list(keyvalues = keyvalues,
                  raw = Rfits_keyvalues_to_raw(keyvalues))
  class(delegate) = c('Rfits_header', 'list')
  return(delegate)
}

#centre

centre = function(x, useraw=TRUE, ...){
  UseMethod("centre", x)
}

centre.Rfits_image = function(x, useraw=TRUE, ...){
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
    if(useraw){
      if(inherits(x, 'Rfits_keylist')){
        header = Rfits_keyvalues_to_raw(x)
      }else{
        header = x$raw
      }
    }else{
      header = NULL
    }
    output = Rwcs::Rwcs_p2s(im_dim[1]/2, im_dim[2]/2, keyvalues = keyvalues, header=header, pixcen='R', ...)
    return(output)
  }else{
    message('The Rwcs package is needed to find the centre of a Rfits_image object.')
  }
}

center = function(x, useraw=TRUE, ...){
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

#A Zarr pointer is a pointer to a whole array rather than a cutout of one, so it
#can never end up in the too few dimensions case that the Rfits_image and
#Rfits_pointer methods have to guard against
centre.Rfits_pointer_zarr = function(x, useraw=TRUE, ...){
  return(centre(.zarr_wcs_delegate(x), useraw=useraw, ...))
}

center.Rfits_pointer_zarr = centre.Rfits_pointer_zarr

#corners

corners = function(x, useraw=TRUE, RAneg=FALSE, ...){
  UseMethod("corners", x)
}

corners.Rfits_image = function(x, useraw=TRUE, RAneg=FALSE, ...){
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
    if(useraw){
      if(inherits(x, 'Rfits_keylist')){
        header = Rfits_keyvalues_to_raw(x)
      }else{
        header = x$raw
      }
    }else{
      header = NULL
    }
    BL = Rwcs::Rwcs_p2s(0, 0, keyvalues = keyvalues, header=header, pixcen='R', ...)
    TL = Rwcs::Rwcs_p2s(0, im_dim[2], keyvalues = keyvalues, header=header, pixcen='R', ...)
    TR = Rwcs::Rwcs_p2s(im_dim[1], im_dim[2], keyvalues = keyvalues, header=header, pixcen='R', ...)
    BR = Rwcs::Rwcs_p2s(im_dim[1], 0, keyvalues = keyvalues, header=header, pixcen='R', ...)
    output = rbind(BL, TL, TR, BR)
    row.names(output) = c('BL', 'TL', 'TR', 'BR')
    
    if(max(output[,'RA'], na.rm=TRUE) - min(output[,'RA'], na.rm=TRUE) > 180 & RAneg){
      output[output[,'RA'] > 180,'RA'] = output[output[,'RA'] > 180,'RA'] - 360
    }
    
    return(output)
  }else{
    message('The Rwcs package is needed to find the corners of a Rfits_image object.')
  }
}

corners.Rfits_pointer = corners.Rfits_image
corners.Rfits_header = corners.Rfits_image
corners.Rfits_keylist = corners.Rfits_image

corners.Rfits_pointer_zarr = function(x, useraw=TRUE, RAneg=FALSE, ...){
  return(corners(.zarr_wcs_delegate(x), useraw=useraw, RAneg=RAneg, ...))
}

#extremes

extremes = function(x, useraw=TRUE, unit='asec', RAneg=FALSE, ...){
  UseMethod("extremes", x)
}

extremes.Rfits_image = function(x, useraw=TRUE, unit='amin', RAneg=FALSE, ...){
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
  
  temp_corners = corners(x=x, useraw=useraw, ...)
  
  if(max(temp_corners[,'RA'], na.rm=TRUE) - min(temp_corners[,'RA'], na.rm=TRUE) > 180){
    wrap_0 = TRUE
    temp_corners[temp_corners[,'RA'] > 180,'RA'] = temp_corners[temp_corners[,'RA'] > 180,'RA'] - 360
  }else{
    wrap_0 = FALSE
  }
  
  temp_min = c(RA = min(temp_corners[,'RA'], na.rm=TRUE), Dec = min(temp_corners[,'Dec'], na.rm=TRUE))
  temp_max = c(RA = max(temp_corners[,'RA'], na.rm=TRUE), Dec = max(temp_corners[,'Dec'], na.rm=TRUE))
  
  output = rbind(temp_min, temp_max)
  
  Dec_worst = max(abs(output[,'Dec']), na.rm=TRUE)
  RA_range = abs(diff(range(output[,'RA'])))*cos(Dec_worst*pi/180)
  Dec_range = abs(diff(range(output[,'Dec'])))
  
  if(unit=='deg'){
    #do nothing
  }else if(unit == 'asec'){
    RA_range = RA_range*3600
    Dec_range = Dec_range*3600
  }else if(unit == 'amin'){
    RA_range = RA_range*60
    Dec_range = Dec_range*60
  }else if(unit=='rad'){
    RA_range = RA_range*pi/180
    Dec_range = Dec_range*pi/180
  }else{
    message('Not a valid unit, must be one of asec / amin / deg / rad')
  }

  output = rbind(output, c(RA = RA_range, Dec = Dec_range))
    
  row.names(output) = c('min', 'max', 'range')
  
  if(RAneg==FALSE & wrap_0){
    output[1,1] = temp_min[1] + 360
  }
  
  return(output)
}

extremes.Rfits_pointer = extremes.Rfits_image
extremes.Rfits_header = extremes.Rfits_image
extremes.Rfits_keylist = extremes.Rfits_image

#Defaults match the generic where the existing pointer method does not (see the
#note on unit there); 'amin' is what extremes.Rfits_image itself defaults to
extremes.Rfits_pointer_zarr = function(x, useraw=TRUE, unit='amin', RAneg=FALSE, ...){
  return(extremes(.zarr_wcs_delegate(x), useraw=useraw, unit=unit, RAneg=RAneg, ...))
}

#pixscale

pixscale = function(x, useraw=TRUE, unit='asec', loc='cen', ...){
  UseMethod("pixscale", x)
}
pixscale.Rfits_image = function(x, useraw=TRUE, unit='asec', loc='cen', ...){
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
  
  if(is.character(loc)){
    im_dim = dim(x) #this works on all classes
    loc = tolower(loc)
    
    if(loc == 'cen'){
      loc_x = im_dim[1]/2
      loc_y = im_dim[2]/2
    }else if(loc == 'bl'){
      loc_x = 0.5
      loc_y = 0.5
    }else if(loc == 'tl'){
      loc_x = 0.5
      loc_y = im_dim[2] - 0.5
    }else if(loc == 'tr'){
      loc_x = im_dim[1] - 0.5
      loc_y = im_dim[2] - 0.5
    }else if(loc == 'br'){
      loc_x = im_dim[1] - 0.5
      loc_y = 0.5
    }
  }else if(is.numeric(loc)){
    if(length(loc) == 1L){
      loc_x = loc
      loc_y = loc
    }else{
      loc_x = loc[1]
      loc_y = loc[2]
    }
  }
  
  if(requireNamespace("Rwcs", quietly=TRUE)){
    if(useraw){
      if(inherits(keyvalues, 'Rfits_keylist')){
        header = Rfits_keyvalues_to_raw(keyvalues)
      }else{
        header = x$raw
      }
    }else{
      header = NULL
    }
    output = .Rwcs_p2s_rows(loc_x + c(-0.5,0.5,-0.5), loc_y + c(-0.5,-0.5,0.5), keyvalues = keyvalues, header=header, ...)
    if(max(abs(diff(output[,1]))) > 359){
      output[output[,1] > 359,1] = output[output[,1] > 359,1] - 360
    }
    output[,1] = output[,1] * cos(mean(output[,2])*pi/180) #this should be the mean! Don't touch it (already made that mistake once)
    #old code, probably ignore. Work the same on square pixels:
    #scale_deg = 0.7071068*sqrt(diff(output[1:2,1])^2 + diff(output[1:2,2])^2 + diff(output[c(1,3),1])^2 + diff(output[c(1,3),2])^2) # 0.7071068 = 1/sqrt(2)
    
    #new code to work with distorted pixels better
    scale_deg = (sqrt(diff(output[1:2,1])^2 + diff(output[1:2,2])^2) + sqrt(diff(output[c(1,3),1])^2 + diff(output[c(1,3),2])^2))/2
    
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
    message('The Rwcs package is needed to find the pixel scale of a Rfits_image object.')
  }
}

pixscale.Rfits_pointer = pixscale.Rfits_image
pixscale.Rfits_header = pixscale.Rfits_image
pixscale.Rfits_keylist = pixscale.Rfits_image

pixscale.Rfits_pointer_zarr = function(x, useraw=TRUE, unit='asec', loc='cen', ...){
  return(pixscale(.zarr_wcs_delegate(x), useraw=useraw, unit=unit, loc=loc, ...))
}

#pixarea

pixarea = function(x, useraw=TRUE, unit='asec2', loc='cen', ...){
  UseMethod("pixarea", x)
}

pixarea.Rfits_image = function(x, useraw=TRUE, unit='asec2', loc='cen', ...){
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
  
  if(is.character(loc)){
    im_dim = dim(x) #this works on all classes
    loc = tolower(loc)
    
    if(loc == 'cen'){
      loc_x = im_dim[1]/2
      loc_y = im_dim[2]/2
    }else if(loc == 'bl'){
      loc_x = 0.5
      loc_y = 0.5
    }else if(loc == 'tl'){
      loc_x = 0.5
      loc_y = im_dim[2] - 0.5
    }else if(loc == 'tr'){
      loc_x = im_dim[1] - 0.5
      loc_y = im_dim[2] - 0.5
    }else if(loc == 'br'){
      loc_x = im_dim[1] - 0.5
      loc_y = 0.5
    }
  }else if(is.numeric(loc)){
    if(length(loc) == 1L){
      loc_x = loc
      loc_y = loc
    }else{
      loc_x = loc[1]
      loc_y = loc[2]
    }
  }
  
  if(requireNamespace("Rwcs", quietly=TRUE)){
    if(useraw){
      if(inherits(x, 'Rfits_keylist')){
        header = Rfits_keyvalues_to_raw(x)
      }else{
        header = x$raw
      }
    }else{
      header = NULL
    }
    output = .Rwcs_p2s_rows(loc_x + c(-0.5,0.5,-0.5), loc_y + c(-0.5,-0.5,0.5), keyvalues = keyvalues, header=header, ...)
    if(max(abs(diff(output[,1]))) > 359){
      output[output[,1] > 359,1] = output[output[,1] > 359,1] - 360
    }
    output[,1] = output[,1] * cos(mean(output[,2])*pi/180)
    #old code, probably ignore. Work the same on square pixels:
    #area_deg = sqrt(diff(output[1:2,1])^2 + diff(output[1:2,2])^2)*sqrt(diff(output[c(1,3),1])^2 + diff(output[c(1,3),2])^2)
    
    #new code to work with distorted pixels better, use the parallelogram cross product area:
    area_deg = abs(sum(diff(output[1:2,1])*diff(output[c(1,3),2]) - diff(output[1:2,2])*diff(output[c(1,3),1])))
    
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
    message('The Rwcs package is needed to find the pixel area of a Rfits_image object.')
  }
}

pixarea.Rfits_pointer = pixarea.Rfits_image
pixarea.Rfits_header = pixarea.Rfits_image
pixarea.Rfits_keylist = pixarea.Rfits_image

pixarea.Rfits_pointer_zarr = function(x, useraw=TRUE, unit='asec2', loc='cen', ...){
  return(pixarea(.zarr_wcs_delegate(x), useraw=useraw, unit=unit, loc=loc, ...))
}

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
  
  return(cbind(North = North_ang, East = East_ang))
}

rotation.Rfits_pointer = rotation.Rfits_image
rotation.Rfits_header = rotation.Rfits_image
rotation.Rfits_keylist = rotation.Rfits_image

rotation.Rfits_pointer_zarr = function(x, keypass=TRUE, ...){
  return(rotation(.zarr_wcs_delegate(x), keypass=keypass, ...))
}
