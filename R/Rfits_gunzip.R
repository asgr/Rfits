Rfits_gunzip = function(filename, tempdir=NULL){
  assertCharacter(filename, max.len=1)
  filename = path.expand(filename)
  assertAccess(filename, access='r')
  if(grepl('fits.gz',filename,fixed=TRUE) | grepl('fit.gz',filename,fixed=TRUE)){
    if(filename %in% options()$Rfits_gunzip[,1]){
      filename = options()$Rfits_gunzip[options()$Rfits_gunzip[,1] == filename,2]
    }else{
      if(!requireNamespace("R.utils", quietly = TRUE)){
        stop('The R.utils package is needed to decompress the target fits.gz. Please install it from CRAN', call. = FALSE)
      }
      if(is.null(tempdir)){
        tempdir = tempdir()
      }
      file_temp = paste(tempdir,strsplit(basename(filename),'.gz')[[1]], sep='/')
      R.utils::gunzip(filename, destname=file_temp, remove=FALSE, overwrite=TRUE)
      options(Rfits_gunzip = rbind(options()$Rfits_gunzip, c(filename, file_temp)))
      return(file_temp)
    }
  }else{
    return(filename)
  }
}

Rfits_gunzip_clear = function(filenames='all'){
  if(!is.null(options()$Rfits_gunzip)){
    if(length(filenames) == 1){
      if(filenames == 'all'){
        if(!is.null(options()$Rfits_gunzip)){
          files_remove = options()$Rfits_gunzip[,2]
          files_remove = files_remove[file.exists(files_remove)]
          if(length(files_remove) >= 1){
            file.remove(files_remove)
          }
          options(Rfits_gunzip = NULL)
        }
      }
    }else{
      files_remove = options()$Rfits_gunzip[options()$Rfits_gunzip[,1] %in% filenames,2]
      if(length(files_remove) >= 1){
        files_remove = files_remove[file.exists(files_remove)]
        if(length(files_remove) >= 1){
          file.remove(files_remove)
        }
      }
      options(Rfits_gunzip = options()$Rfits_gunzip[!options()$Rfits_gunzip[,1] %in% filenames,])
    }
  }
}

Rfits_create_RAMdisk = function(diskname="RAMdisk", sizeGB=1){
  command = paste0("diskutil erasevolume HFS+ \'",diskname,"\' \`hdiutil attach -nomount ram://",2097152*sizeGB,"\`")
  system(command)
  return(paste0('/Volumes/',diskname))
}

Rfits_remove_RAMdisk = function(diskname="RAMdisk"){
  command = paste0("diskutil unmountDisk /Volumes/",diskname)
  system(command)
}
