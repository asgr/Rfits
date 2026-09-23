#Launch the bundled Shiny app for browsing and cutting out Zarr stores.
#
#The app itself is a single file under inst/shiny, so it can also be run directly with
#shiny::runApp(system.file('shiny', 'Rfits_cutout_app', package = 'Rfits')). This wrapper
#exists so that the dependency checks happen once, in R, where they can name the missing
#package, rather than as a failed requireNamespace deep inside the app's UI.

Rfits_cutout_app = function(launch.browser = getOption('shiny.launch.browser', interactive()),
                            display.mode = c('normal', 'static'),
                            port = NULL, ...){
  need = c('shiny', 'bslib', 'ggplot2', 'Rwcs')
  missing = need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
  if(length(missing) > 0){
    stop('Rfits_cutout_app needs: ', paste(missing, collapse = ', '),
         '. Please install them from CRAN.', call. = FALSE)
  }
  app = system.file('shiny', 'Rfits_cutout_app', package = 'Rfits')
  if(!nzchar(app) || !file.exists(file.path(app, 'app.R'))){
    #An empty path is what a namespace installed without its inst directory looks like,
    #which is worth saying plainly rather than letting runApp report a missing app_dir
    stop('The bundled app was not found inside the Rfits installation. Reinstall the ',
         'package from source.', call. = FALSE)
  }
  #Optional in the same sense the app treats them: a table without pagination and a
  #download that cannot be zipped are both worse than useful, so they are advice rather
  #than a refusal
  for(opt in c('DT', 'zip')){
    if(!requireNamespace(opt, quietly = TRUE)){
      message('Install the ', opt, ' package for ',
              if(opt == 'DT') 'paged tables in the app.' else 'bulk downloads from the app.')
    }
  }
  display.mode = match.arg(display.mode)
  args = list(appDir = app, launch.browser = launch.browser,
              display.mode = display.mode)
  if(!is.null(port)){
    args$port = port
  }
  return(do.call(shiny::runApp, c(args, list(...))))
}
