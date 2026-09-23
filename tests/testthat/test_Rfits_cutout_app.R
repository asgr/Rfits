#The bundled Shiny app. What is checked here is the half that can be checked without a
#browser: that the file is installed and parses, that the helpers the server is built from
#behave, and that the observers wire inputs to results when driven against a mock session.
#
#The app is sourced into its own environment rather than started, because the file ends in
#shinyApp(), which builds an app object without serving it. That does attach shiny, bslib
#and DT to the search path for the rest of the run, which is the price of testing a file
#that loads its own dependencies.

app_dir = system.file('shiny', 'Rfits_cutout_app', package = 'Rfits')

#ex 1 the app is installed where the launcher says it is
expect_true(nzchar(app_dir))
expect_true(file.exists(file.path(app_dir, 'app.R')))
expect_true(is.function(Rfits_cutout_app))

app_text = paste(readLines(file.path(app_dir, 'app.R'), warn = FALSE), collapse = '\n')

#ex 2 the file must parse on its own. R CMD check looks at no file under inst/, so a
#syntax error here would only be found by someone trying to start the app
expect_silent(parse(text = app_text))

#ex 3 every icon the UI asks for must be one fontawesome knows. An unknown name is only a
#warning and a missing glyph at run time, which is exactly the kind of thing that ships by
#accident; two of them did, first time
icon_calls = unique(gsub("^icon\\('|'\\)$", '',
                        unlist(regmatches(app_text, gregexpr("icon\\('[a-z0-9-]+'\\)",
                                                             app_text)))))
expect_gt(length(icon_calls), 0)
if(requireNamespace('fontawesome', quietly = TRUE)){
  known = unique(c(fontawesome::fa_metadata()$icon_names, fontawesome:::alias_tbl$alias))
  expect_equal(icon_calls[!icon_calls %in% known], character(0))
}

#ex 4 the internal helpers the server reaches for must still exist. They are not public
#API, so nothing else in the suite would notice one being renamed
internals = unique(gsub('^Rfits:::', '', unlist(regmatches(app_text, gregexpr(
  'Rfits:::[.A-Za-z0-9_]+', app_text)))))
expect_gt(length(internals), 0)
for(name in internals){
  expect_true(exists(name, envir = asNamespace('Rfits'), inherits = FALSE),
              info = paste(name, 'is used by the app but is not in the namespace'))
}

app_env = new.env(parent = globalenv())
sourced = tryCatch({
  suppressMessages(sys.source(file.path(app_dir, 'app.R'), envir = app_env,
                             keep.source = FALSE))
  TRUE
}, error = function(e) e)
#A machine without the suggested packages cannot source the app at all, and that is a skip
#rather than a failure
if(!isTRUE(sourced)){
  skip(paste('the app could not be sourced:', conditionMessage(sourced)))
}
expect_true(is.function(app_env$server))
expect_false(is.null(app_env$ui))

#ex 5 position parsing. Free text is what gets pasted, so a line that is not two finite
#numbers has to be reported rather than dropped, or a truncated paste silently searches
#fewer targets than the user thinks it did
pos = app_env$parse_positions('53.1 -27.8\n10.5, -45.2')
expect_identical(dim(pos), c(2L, 2L))
expect_identical(colnames(pos), c('RA', 'Dec'))
expect_length(attr(pos, 'bad'), 0)
#separators a spreadsheet or a finder list may produce
expect_identical(nrow(app_env$parse_positions('53.1\t-27.8')), 1L)
expect_identical(nrow(app_env$parse_positions('53.1;-27.8')), 1L)
expect_identical(nrow(app_env$parse_positions('53.1    -27.8')), 1L)
expect_identical(nrow(app_env$parse_positions('53.1 -27.8\r\n10 -40')), 2L)
#comments and blanks are not errors
expect_identical(nrow(app_env$parse_positions('# a comment\n\n53.1 -27.8')), 1L)
#but anything that is not a pair of finite numbers is
expect_length(attr(app_env$parse_positions('nonsense'), 'bad'), 1L)
#one number is half a position, not a position
expect_length(attr(app_env$parse_positions('53.1'), 'bad'), 1L)
expect_length(attr(app_env$parse_positions('53.1 abc'), 'bad'), 1L)
#NA and Inf would each fail somewhere far less legible than here
expect_length(attr(app_env$parse_positions('NA 1'), 'bad'), 1L)
expect_length(attr(app_env$parse_positions('Inf 1'), 'bad'), 1L)
expect_identical(nrow(app_env$parse_positions('')), 0L)
#rows come back in the order they were typed, which is what the per position loop assumes
pos = app_env$parse_positions('1 2\n3 4\n5 6')
expect_identical(pos[, 1], c(1, 3, 5))
expect_identical(pos[, 2], c(2, 4, 6))

#ex 6 file names built from store names have to survive being written and zipped
expect_identical(app_env$safe_filename('tile1'), 'tile1')
expect_identical(app_env$safe_filename('a b/c'), 'a_b_c')
#an empty name would otherwise produce a file called '.fits'
expect_identical(app_env$safe_filename(''), 'cutout')
#unsafe runs collapse to a single underscore, so the name stays short and is never empty
expect_identical(app_env$safe_filename('***'), '_')
expect_identical(app_env$safe_filename('a//b'), 'a_b')
#only truncated, never lengthened, so a long store name still gets an extension
expect_lte(nchar(app_env$safe_filename(strrep('x', 300))), 90)
#dots and dashes are kept, since they carry the tile identity
expect_identical(app_env$safe_filename('tile-01_a.fits'), 'tile-01_a.fits')

#ex 7 run messages must be captured rather than printed. A Shiny user has no console, and
#the store loop has to report one failure per store without abandoning the request
cap = app_env$with_capture({
  warning('a warning')
  message('a message')
  42
})
expect_identical(cap$value, 42)
expect_identical(sum(grepl('^WARNING: a warning$', cap$msgs)), 1L)
expect_identical(sum(grepl('^INFO: a message$', cap$msgs)), 1L)
#an error becomes text and a NULL value, which is how the caller knows to carry on
cap = app_env$with_capture(stop('boom'))
expect_null(cap$value)
expect_identical(cap$msgs, 'ERROR: boom')
#and a quiet block stays quiet
expect_length(app_env$with_capture(1 + 1)$msgs, 0)

#ex 8 footprints, drawn from index numbers alone. The box has to be the tile in degrees,
#widened in RA by cos(Dec) the way an angular size on the sky has to be
rows = data.frame(store = c('/a/tile1.zarr', '/b/tile2.zarr'), ra = c(10, 10),
                  dec = c(0, 60), naxis1 = c(1000, 1000), naxis2 = c(1000, 1000),
                  pixscale_x = c(1, 1), pixscale_y = c(1, 1), status = 'ok',
                  extname = 'data1', type = 'image',
                  label = c('/a/tile1.zarr', '/b/tile2.zarr'), stringsAsFactors = FALSE)
fr = app_env$index_footprints(rows)
#1000 pixels of 1 arcsec is 1000/3600 degrees across
expect_equal(fr$xmax[1] - fr$xmin[1], 1000/3600, tolerance = 1e-9)
expect_equal(fr$ymax[1] - fr$ymin[1], 1000/3600, tolerance = 1e-9)
#at 60 degrees a degree of RA covers half the sky of a degree of Dec, so the same tile is
#drawn twice as wide in RA
expect_equal((fr$xmax[2] - fr$xmin[2])/(fr$xmax[1] - fr$xmin[1]), 2, tolerance = 1e-6)
#centred on the reference position
expect_equal((fr$xmin + fr$xmax)/2, rows$ra)
expect_equal((fr$ymin + fr$ymax)/2, rows$dec)
#named for the store rather than for the path it sits on
expect_identical(fr$name, c('tile1', 'tile2'))
#an empty index gives an empty answer rather than an error
expect_identical(nrow(app_env$index_footprints(rows[0, , drop = FALSE])), 0L)
#and the cos(Dec) widening must not blow up at the pole, where it is unbounded
polar = app_env$index_footprints(transform(rows, dec = c(89.99, 89.99)))
expect_true(all(is.finite(unlist(polar[, c('xmin', 'xmax', 'ymin', 'ymax')]))))

#ex 9 the table view drops the columns that are bulk and mean nothing to a reader
view = app_env$index_view_columns(cbind(
  rows,
  data.frame(cache_key = c('k1', 'k2'), kind = 'store', error = NA_character_,
             stamp = 's', created = as.numeric(Sys.time()), has_keyvalues = TRUE,
             crpix1 = 1, crpix2 = 1, text_value = NA_character_,
             stringsAsFactors = FALSE),
  I(data.frame(array_dim = I(list(1:2, 1:2)), stores = I(list('a', 'b')),
               keyvalues = I(list(raw(0), raw(0))), stringsAsFactors = FALSE))))
expect_false(any(c('keyvalues', 'stores', 'array_dim', 'has_keyvalues', 'stamp') %in%
                   names(view)))
expect_true(all(c('label', 'ra', 'dec', 'status') %in% names(view)))
expect_s3_class(view$created, 'POSIXct')

#ex 10 the header filter, which is checked directly because it is the one piece of the app
#that has to reach into the index for the keyword blobs. Values live in the index rather
#than in the pixels, so this is the filter that can narrow a release without reading it
skip_if_not_installed('zarr')
skip_if_not_installed('arrow')
skip_if_not_installed('Rwcs')
skip_if_not_installed('shiny')

src_image = Rfits_read_image(system.file('extdata', 'image.fits', package = 'Rfits'))
img_kv = src_image$keyvalues
app_tiles = file.path(tempdir(), 'app_tiles')
dir.create(app_tiles, showWarnings = FALSE)
for(i in 1:2){
  #two tiles offset in RA, so that a search has something to reject as well as something
  #to match, which is the only way to tell a search apart from a read of everything
  kv = img_kv
  kv$CRVAL1 = kv$CRVAL1 + (i - 1) * 0.05
  suppressWarnings(Rfits_write_image_zarr(src_image$imDat,
                                          filename = file.path(app_tiles,
                                                               paste0('tile', i, '.zarr')),
                                          extname = 'data1', keyvalues = kv,
                                          overwrite_file = TRUE))
}
app_index = file.path(app_tiles, 'index.parquet')
unlink(app_index)
suppressWarnings(suppressMessages(Rfits_zarr_index(dir = app_tiles, index = app_index,
                                                  verbose = FALSE)))
expect_true(file.exists(app_index))
index_rows = as.data.frame(Rfits:::.zarr_index_light_rows(app_index),
                          stringsAsFactors = FALSE)
expect_identical(nrow(index_rows), 2L)

notes = character(0)
add_note = function(...) notes <<- c(notes, paste0(...))
#an index column, matched case insensitively. EXTNAME is a column of the index and not
#necessarily a keyword in the header, and a filter that looked only at keywords would
#return nothing for it and look like an empty release
keep = app_env$filter_by_keyword(index_rows, app_index, 'EXTNAME', 'data1', note = add_note)
expect_identical(nrow(keep), 2L)
expect_match(paste(notes, collapse = ' '), 'index column')
#and the same word as a header keyword that no candidate carries must be reported as
#absent rather than as a filtering decision
notes = character(0)
keep = app_env$filter_by_keyword(index_rows, app_index, 'NOSUCHKEYWORD', '', note = add_note)
expect_identical(nrow(keep), 0L)
expect_match(paste(notes, collapse = ' '), 'No candidate store carries a NOSUCHKEYWORD')
#a keyword that is present but whose values do not match says what the values were, which
#is how a user tells a wrong regular expression apart from a release with no such data
notes = character(0)
keep = app_env$filter_by_keyword(index_rows, app_index, 'CRVAL1', '^999', note = add_note)
expect_identical(nrow(keep), 0L)
expect_match(paste(notes, collapse = ' '), 'Values seen')
#present and matching keeps the store it belongs to, and only that one
notes = character(0)
keep = app_env$filter_by_keyword(index_rows, app_index, 'CRVAL1',
                                 paste0('^', round(img_kv$CRVAL1, 4)), note = add_note)
expect_identical(nrow(keep), 1L)
expect_match(keep$label[1], 'tile1')
#a blank key is no filter at all
expect_identical(nrow(app_env$filter_by_keyword(index_rows, app_index, '', 'x')), 2L)

run_inputs = list(dir = app_tiles, bucket = '', prefix = '', pattern = '',
                  extname = 'data1', index_name = 'index.parquet', region = '',
                  endpoint = '', access_key = '', secret_key = '', session_token = '',
                  max_dirs = 1000, rebuild = FALSE, box = 51, box_unit = 'pix',
                  buffer = 0, kw_key = '', kw_value = '', max_stores = 25, qdiff = TRUE,
                  type = 'quan', stretch = 'asinh', locut = NA, hicut = NA,
                  useraster = TRUE, slice = 1, cut_width = 300, max_show = 24,
                  idx_filter = '', idx_status = 'all', positions = '')

#setInputs takes named arguments, so they have to be splatted; handing it a list
#directly is rejected with an error about an empty key. An unnamed list argument is
#spliced here so a block of defaults can be passed as one object, and load and run are
#always flushed separately because an observer that needs the index must not fire in the
#same flush as the one that builds it
put_inputs = function(session, ...){
  args = list(...)
  nms = names(args)
  if(is.null(nms)){
    nms = rep('', length(args))
  }
  flat = list()
  for(i in seq_along(args)){
    if(!nzchar(nms[i]) && is.list(args[[i]])){
      flat = c(flat, args[[i]])
    }else{
      flat[[nms[i]]] = args[[i]]
    }
  }
  do.call(session$setInputs, flat)
  session$flushReact()
  invisible(NULL)
}

#ex 11 a bare index name is resolved inside the store directory locally, so a session does
#not have to know the absolute path of the file it is about to read. Over S3 the same bare
#name goes to the bucket root instead, which is the published location
testServer(app_env$server, expr = {
  put_inputs(session, run_inputs)
  put_inputs(session, index_name = 'index.parquet', load = 1)
  session$flushReact()
  con = reactiveValuesToList(state)$con
  expect_identical(con$index_name, file.path(app_tiles, 'index.parquet'))
  #but a name that already carries a separator is left alone
  put_inputs(session, index_name = 'sub/index.parquet', load = 2)
  session$flushReact()
  expect_identical(reactiveValuesToList(state)$con$index_name,
                   file.path(app_tiles, 'sub/index.parquet'))
}, session = shiny::MockShinySession$new())

#ex 12 the whole request path: load, query the index, cut, and put the result somewhere a
#user can see it. This is the wiring between the tabs rather than the arithmetic, which is
#checked above, and it is where a renamed input id or a dropped argument would show up
testServer(app_env$server, expr = {
  put_inputs(session, run_inputs)
  put_inputs(session, positions = paste(img_kv$CRVAL1 + 0.02, img_kv$CRVAL2), load = 1)
  session$flushReact()
  s = reactiveValuesToList(state)
  expect_false(is.null(s$idx))
  expect_equal(nrow(s$rows), 2L)
  #the copy of the index has to survive for as long as the session queries it, which is
  #the one thing a remote search would break on
  expect_true(file.exists(s$idx$path))

  put_inputs(session, run = 1)
  session$flushReact()
  s = reactiveValuesToList(state)
  expect_length(s$res, 1L)
  expect_named(s$res, 'tile1')
  expect_false(is.null(s$res[[1]]$imDat))
  expect_identical(dim(s$res[[1]]$imDat), c(51L, 51L))
  #the WCS of the tile is carried through to the cutout, so the box is somewhere knowable
  expect_equal(s$res[[1]]$keyvalues$CRVAL1, img_kv$CRVAL1, tolerance = 1e-6)
  expect_false(is.null(s$matches))
  expect_true(all(c('name', 'position', 'store', 'RA', 'Dec', 'dim') %in%
                    names(s$matches)))
  #and the index file is still there after a search has used it
  expect_true(file.exists(s$idx$path))

  #a position nowhere near any tile must give no results rather than an error
  put_inputs(session, positions = '0 0', run = 2)
  session$flushReact()
  expect_length(reactiveValuesToList(state)$res, 0L)

  #the outputs the tabs render, including with nothing in them
  for(id in c('store_status', 'store_summary', 'log', 'gallery', 'idx_info')){
    expect_no_error(session$getOutput(id))
  }
}, session = shiny::MockShinySession$new())

#ex 13 several positions in one request. Names have to tell the positions apart, and the
#match table has to stay aligned with the results it labels
testServer(app_env$server, expr = {
  put_inputs(session, run_inputs)
  put_inputs(session, positions = paste0(img_kv$CRVAL1 + 0.02, ' ', img_kv$CRVAL2, '\n',
                                        img_kv$CRVAL1, ' ', img_kv$CRVAL2),
             load = 1, run = 1)
  session$flushReact()
  s = reactiveValuesToList(state)
  expect_length(s$res, 2L)
  expect_equal(anyDuplicated(names(s$res)), 0L)
  expect_equal(sort(unique(s$matches$position)), c(1L, 2L))
  #one row of the table per result, and every row names a result that exists
  expect_equal(nrow(s$matches), length(s$res))
  expect_true(all(s$matches$name %in% names(s$res)))
  #the table rows are labelled with the names of the results they came from, which is the
  #alignment that a sort after naming would silently break
  expect_identical(s$matches$name[order(s$matches$position)], names(s$res))
}, session = shiny::MockShinySession$new())

#ex 14 a request that fails part way through must not leave the previous answer on screen.
#Results are cleared before anything is asked for, so what is plotted always belongs to
#the positions in the box
testServer(app_env$server, expr = {
  put_inputs(session, run_inputs)
  put_inputs(session, positions = paste(img_kv$CRVAL1 + 0.02, img_kv$CRVAL2),
             load = 1, run = 1)
  session$flushReact()
  expect_length(reactiveValuesToList(state)$res, 1L)
  #now a run whose keyword filter removes every candidate
  put_inputs(session, kw_key = 'NOSUCHKEYWORD', run = 2)
  session$flushReact()
  expect_length(reactiveValuesToList(state)$res, 0L)
  expect_null(reactiveValuesToList(state)$matches)
}, session = shiny::MockShinySession$new())

#ex 15 the name pattern, applied on top of an index that may be broader than this session
#asked for
testServer(app_env$server, expr = {
  put_inputs(session, run_inputs)
  put_inputs(session, positions = paste(img_kv$CRVAL1 + 0.02, img_kv$CRVAL2),
             pattern = 'tile2', load = 1, run = 1)
  session$flushReact()
  s = reactiveValuesToList(state)
  #tile2 is all the pattern leaves, and the requested position is not inside it
  expect_length(s$res, 0L)
  expect_match(paste(s$log, collapse = '\n'), 'Name pattern kept 1')
}, session = shiny::MockShinySession$new())

#ex 16 the frames tab. The plot is an interactive plotly panel, so what has to hold is the
#contract between the widget and the server: the footprints are drawn from index numbers
#alone, every vertex carries the id of the frame it belongs to, and a click or a box drag
#comes back through that id as a position. The panel sits in a tab Shiny keeps suspended,
#so it is read here as the JSON the renderer hands to the browser
testServer(app_env$server, expr = {
  put_inputs(session, run_inputs)
  put_inputs(session, load = 1)
  session$flushReact()
  spec_of = function() jsonlite::fromJSON(
    paste(as.character(session$getOutput('frames')), collapse = ''),
    simplifyVector = FALSE)
  spec = spec_of()
  #with no cutouts in memory at all the tab still renders, which is the point of drawing
  #the footprints from the index rather than from the tiles
  expect_false(is.null(spec))
  #one polyline trace carrying every footprint, and one marker trace carrying the centres.
  #A trace per tile would mean a trace per thousand tiles, so the count is the thing
  expect_length(spec$x$data, 2)
  expect_identical(spec$x$data[[1]]$mode, 'lines')
  expect_identical(spec$x$data[[2]]$mode, 'markers')
  fr = app_env$index_footprints(as.data.frame(reactiveValuesToList(state)$rows)[
    reactiveValuesToList(state)$rows$status == 'ok', , drop = FALSE])
  n_ok = nrow(fr)
  expect_gt(n_ok, 1)
  expect_equal(length(spec$x$data[[2]]$x), n_ok)
  #five vertices per ring (four corners and the closing point) with a NaN separator between
  #rings, which is what stops plotly drawing a line from one tile to the next. The trailing
  #separator is dropped on the way out, so the total is bounded rather than pinned to a
  #plotly internal: what has to hold is that the payload stays the same length as the
  #coordinates, and that every frame contributes exactly five vertices with an id
  expect_equal(length(spec$x$data[[1]]$customdata), length(spec$x$data[[1]]$x))
  expect_gte(length(spec$x$data[[1]]$x), n_ok * 5)
  expect_lte(length(spec$x$data[[1]]$x), n_ok * 6)
  #the ids are row numbers of the unfiltered set, which is what keeps a pick pointing at
  #the same tile when a filter takes other rows away. Numbers rather than store names
  #because every one of those vertices carries one
  cd = function(sp, trace) vapply(sp$x$data[[trace]]$customdata,
                                  function(z) if(is.null(z)) NA_real_ else as.numeric(z),
                                  numeric(1))
  expect_identical(cd(spec, 2), as.numeric(seq_len(n_ok)))
  #and every vertex of a ring carries its own ring's id, so a click on an edge resolves to
  #the same frame as a click on its centre. The separator carries nothing
  ring = cd(spec, 1)
  expect_identical(ring[1:5], rep(1, 5))
  expect_true(is.na(ring[6]))
  expect_identical(ring[7:11], rep(2, 5))
  #the RA axis runs backwards, the way a sky map is read, and it is the range that does it
  xr = unlist(spec$x$layout$xaxis$range)
  expect_gt(xr[1], xr[2])
  #equal degrees per pixel, so a square tile is drawn square rather than stretched to the
  #panel by whatever shape the release happens to have
  expect_identical(spec$x$layout$xaxis$scaleanchor, 'y')
  #a box drag must be able to catch a tile in any direction, since a selection that only
  #worked left to right would miss tiles on a reversed axis
  expect_identical(spec$x$layout$selectdirection, 'any')
  expect_identical(spec$x$layout$dragmode, 'zoom')
  #the hover text names the tile, which is the one thing needed to identify what is under
  #the cursor on a release of thousands of stores
  hv = vapply(spec$x$data[[2]]$text, function(z) as.character(z), character(1))
  expect_true(all(grepl('tile', hv)))
  expect_true(all(grepl('RA, Dec', hv)))
  expect_true(all(grepl('extname', hv)))

  #a click on a frame centre picks that frame, and the panel says so
  put_inputs(session, 'plotly_click-frames' = as.character(jsonlite::toJSON(
    list(list(curveNumber = 1L, pointNumber = 0L, x = fr$ra[1], y = fr$dec[1],
              customdata = 1)), auto_unbox = TRUE)))
  session$flushReact()
  expect_match(session$getOutput('frames_info'), 'Picked by click: 1')
  #the picked frames are drawn as a third trace, so what was chosen is visible on the panel
  spec = spec_of()
  expect_length(spec$x$data, 3)
  expect_identical(spec$x$data[[3]]$name, 'picked')
  expect_equal(length(spec$x$data[[3]]$x), 1)

  #clicking a second frame adds to the list rather than replacing it, and one click can
  #report several points at once when the traces overlap under the cursor
  put_inputs(session, 'plotly_click-frames' = as.character(jsonlite::toJSON(
    list(list(curveNumber = 1L, pointNumber = 1L, x = fr$ra[2], y = fr$dec[2],
              customdata = 2),
         list(curveNumber = 0L, pointNumber = 4L, x = fr$ra[2], y = fr$dec[2],
              customdata = 2)), auto_unbox = TRUE)))
  session$flushReact()
  expect_match(session$getOutput('frames_info'), 'Picked by click: 2')

  #a click that lands on a separator carries no id and must pick nothing. Left unchecked,
  #the absent customdata arriving as NULL would read as a selection of something
  put_inputs(session, 'plotly_click-frames' = as.character(jsonlite::toJSON(
    list(list(curveNumber = 0L, pointNumber = 5L, x = 0, y = 0)), auto_unbox = TRUE)))
  session$flushReact()
  info = session$getOutput('frames_info')
  expect_match(info, 'Picked by click: 2')
  expect_match(info, 'Selected by box: 0')

  #the table beside the plot lists the picked frames by name
  html = paste(as.character(session$getOutput('frames_table')), collapse = '\n')
  expect_match(html, 'tile1')
  expect_match(html, 'tile2')

  #what goes to the Cutouts tab is the reference position of each picked tile, which is the
  #reason the frames are clickable at all: the middle of a box drawn around two tiles is a
  #position that belongs to neither. The log is checked because MockShinySession swallows
  #an updateTextAreaInput rather than applying it back to the inputs, so this is the only
  #place the coordinates that were sent can be seen
  put_inputs(session, send_pos = 1)
  session$flushReact()
  log = paste(reactiveValuesToList(state)$log, collapse = '\n')
  expect_match(log, 'Sent 2 picked frame centre')
  expect_match(log, 'tile1, tile2')
  #the coordinates in the log are the centres of the two tiles, to the rounding used
  for(i in 1:2){
    expect_match(log, paste(signif(fr$ra[i], 9), signif(fr$dec[i], 9)), fixed = TRUE)
  }
  #and not the middle of the box that would have been drawn around them, which is the
  #regression the old brush had
  expect_false(grepl(paste(signif(mean(fr$ra[1:2]), 9), signif(mean(fr$dec[1:2]), 9)),
                     log, fixed = TRUE))

  #a box selection resolves through the ids of the points it caught. The two traces each
  #carry a point per frame, so the same frame arrives twice and must still be one row
  put_inputs(session, 'plotly_selected-frames' = as.character(jsonlite::toJSON(
    list(list(curveNumber = 0L, pointNumber = 3L, x = fr$ra[2], y = fr$dec[2],
              customdata = 2),
         list(curveNumber = 1L, pointNumber = 0L, x = fr$ra[1], y = fr$dec[1],
              customdata = 1),
         list(curveNumber = 1L, pointNumber = 1L, x = fr$ra[2], y = fr$dec[2],
              customdata = 2)), auto_unbox = TRUE)))
  session$flushReact()
  info = session$getOutput('frames_info')
  expect_match(info, 'Selected by box: 2')
  expect_match(info, 'Selected: tile1, tile2')
  #the box reported is the extent of what was caught, with the smaller bound first
  #whatever way the drag went, since the RA axis is reversed
  lo = as.numeric(sub('.*Selection box: ([-0-9.]+) to .*', '\\1', info))
  hi = as.numeric(sub('.*Selection box: [-0-9.]+ to ([-0-9.]+) RA.*', '\\1', info))
  expect_lt(lo, hi)
  expect_equal(c(lo, hi), range(c(fr$xmin[1:2], fr$xmax[1:2])), tolerance = 1e-3)
  #sending a selection sends the frames in it, not the centre of the box
  put_inputs(session, send_sel = 1)
  session$flushReact()
  log = paste(reactiveValuesToList(state)$log, collapse = '\n')
  expect_match(log, 'Sent 2 selected frame centre')

  #picking is cleared deliberately, and clearing picks leaves the box selection alone: a
  #stray click on empty space must not throw away a list the user built
  put_inputs(session, clear_picks = 1)
  session$flushReact()
  info = session$getOutput('frames_info')
  expect_match(info, 'Picked by click: 0')
  expect_match(info, 'Selected by box: 2')

  #the Index tab's filters apply here too, so a session narrowed by name shows the same
  #tiles in both places. The surviving frame keeps the id it was given in the unfiltered
  #set, which is what stops a pick re-pointing at a different tile
  put_inputs(session, idx_filter = 'tile2')
  session$flushReact()
  spec = spec_of()
  expect_equal(length(spec$x$data[[2]]$x), 1)
  expect_equal(as.numeric(spec$x$data[[2]]$customdata[[1]]), 2)
  expect_match(session$getOutput('frames_info'), 'Frames in view: 1')
  #the box now holds only the frame that is still in the view
  expect_match(session$getOutput('frames_info'), 'Selected by box: 1')
  #with nothing picked, sending picks adds no position at all rather than a stale one
  put_inputs(session, send_pos = 2)
  session$flushReact()
  log = paste(reactiveValuesToList(state)$log, collapse = '\n')
  expect_match(log, 'Sent 2 picked frame centre',
               info = 'the earlier send is still in the log')
  expect_equal(sum(grepl('Sent [0-9]+ picked', strsplit(log, '\n')[[1]])), 1L)
}, session = shiny::MockShinySession$new())

#ex 16b the Frames tab before anything is loaded. A panel with no index behind it has to
#say so in words rather than leaving a blank box, which reads as a broken app rather than
#as an empty one, and no button may invent a position out of nothing
testServer(app_env$server, expr = {
  put_inputs(session, run_inputs)
  session$flushReact()
  expect_match(session$getOutput('frames_info'), 'Load an index on the Store tab first')
  #the plot and the table have nothing to draw, and say so by interrupting quietly rather
  #than by erroring loudly or showing a stale panel
  cond = tryCatch(session$getOutput('frames'), error = function(e) e)
  expect_true(inherits(cond, 'shiny.silent.error'))
  #a click arriving with no index behind it changes nothing about what the tab says, and
  #neither Send button may invent a position out of nothing. The id is recorded in picks,
  #which is harmless because loading an index clears them before it can be resolved
  put_inputs(session, 'plotly_click-frames' = as.character(jsonlite::toJSON(
    list(list(curveNumber = 1L, pointNumber = 0L, x = 1, y = 1, customdata = 1)),
    auto_unbox = TRUE)))
  session$flushReact()
  expect_match(session$getOutput('frames_info'), 'Load an index on the Store tab first')
  put_inputs(session, send_pos = 1, send_sel = 1)
  session$flushReact()
  expect_match(session$getOutput('frames_info'), 'Load an index on the Store tab first')
  log = paste(reactiveValuesToList(state)$log, collapse = '\n')
  expect_false(grepl('frame centre\\(s\\) to the Cutouts tab', log))
}, session = shiny::MockShinySession$new())

#ex 17 credentials. A blank field falls back to the environment, which is how a scripted
#run and this app come by the same keys without them being typed into a form, and a typed
#field wins over the environment. Restored explicitly rather than with on.exit, which does
#not reliably belong to a frame at the top level of a test file
want = c('RFITS_S3_REGION', 'RFITS_S3_ACCESS_KEY')
was = Sys.getenv(want, unset = NA)
Sys.setenv(RFITS_S3_REGION = 'ap-southeast-2', RFITS_S3_ACCESS_KEY = 'ENVKEY')
testServer(app_env$server, expr = {
  put_inputs(session, run_inputs)
  put_inputs(session, load = 1)
  session$flushReact()
  creds = reactiveValuesToList(state)$con$creds
  expect_identical(creds$region, 'ap-southeast-2')
  expect_identical(creds$access_key, 'ENVKEY')
  put_inputs(session, region = 'eu-west-1', access_key = 'FIELDKEY', load = 2)
  session$flushReact()
  creds = reactiveValuesToList(state)$con$creds
  expect_identical(creds$region, 'eu-west-1')
  expect_identical(creds$access_key, 'FIELDKEY')
}, session = shiny::MockShinySession$new())
#A variable that was unset goes away again rather than being left as an empty string,
#which the app would read as absent anyway but which would confuse a later block
for(name in want){
  if(is.na(was[[name]])){
    Sys.unsetenv(name)
  }else{
    do.call(Sys.setenv, stats::setNames(list(was[[name]]), name))
  }
}
#and with nothing in the environment, the fields are the only source, so a blank form
#yields blank credentials rather than a stale value. Only checked when the variable really
#is unset on this machine, since a run that has the S3 credentials in its environment
#would legitimately pick them up here
if(is.na(was[['RFITS_S3_REGION']])){
  testServer(app_env$server, expr = {
    put_inputs(session, run_inputs)
    put_inputs(session, load = 1)
    session$flushReact()
    creds = reactiveValuesToList(state)$con$creds
    expect_null(creds$region)
    expect_null(creds$endpoint)
  }, session = shiny::MockShinySession$new())
}

#ex 18 a bucket and a directory together is refused before anything is contacted, because
#the two back ends disagree about what a store name means
testServer(app_env$server, expr = {
  put_inputs(session, run_inputs)
  put_inputs(session, bucket = 'somewhere', load = 1)
  session$flushReact()
  expect_true(is.null(reactiveValuesToList(state)$idx))
  expect_match(paste(reactiveValuesToList(state)$log, collapse = '\n'), 'not both')
  #and neither of them is also refused, with a message that says which
  put_inputs(session, bucket = '', dir = '', load = 2)
  session$flushReact()
  expect_match(paste(reactiveValuesToList(state)$log, collapse = '\n'),
               'Give a bucket \\(with credentials\\) or a local directory')
}, session = shiny::MockShinySession$new())

#ex 19 rebuild ignores an index that is already there. The checkbox is the only way to
#notice a store whose tiles changed underneath a published index, and the rebuilt copy is
#the session's own file rather than the published one
testServer(app_env$server, expr = {
  put_inputs(session, run_inputs)
  put_inputs(session, load = 1)
  session$flushReact()
  published = reactiveValuesToList(state)$idx$path
  expect_false(is.null(published))
  put_inputs(session, rebuild = TRUE, load = 2)
  session$flushReact()
  s = reactiveValuesToList(state)
  expect_true(s$idx$built)
  expect_false(identical(s$idx$path, published))
  #the published index is untouched by a rebuild, which is the point
  expect_true(file.exists(published))
  expect_match(paste(s$log, collapse = '\n'), 'Rebuilding the index')
}, session = shiny::MockShinySession$new())

#ex 20 the downloads. Both bundle with a write then archive, two steps, and the temporary
#directory has to outlive the first to be there for the second. This reaches into
#MockShinySession's private registry, so it is skipped if that moves rather than failing a
#run over a detail of the test harness
testServer(app_env$server, expr = {
  put_inputs(session, run_inputs)
  put_inputs(session, positions = paste0(img_kv$CRVAL1 + 0.02, ' ', img_kv$CRVAL2, '\n',
                                        img_kv$CRVAL1, ' ', img_kv$CRVAL2),
             load = 1, run = 1)
  session$flushReact()
  expect_length(reactiveValuesToList(state)$res, 2L)
  priv = session$.__enclos_env__$private
  if(is.null(priv) || is.null(priv$file_generators)){
    skip('this shiny version does not expose registered downloads')
  }
  if(!requireNamespace('zip', quietly = TRUE)){
    skip('zip is needed to check the bundles')
  }
  keys = priv$file_generators$keys()
  expect_true(any(grepl('dl_fits', keys)))
  expect_true(any(grepl('dl_jpeg', keys)))
  for(key in keys[grepl('dl_fits|dl_jpeg', keys)]){
    gen = priv$file_generators$get(key)
    dest = tempfile(fileext = '.zip')
    expect_no_error(gen$content(dest))
    #an archive that was written but left empty is the failure mode to catch here
    expect_gt(file.size(dest), 0)
    listed = zip::zip_list(dest)
    expect_equal(nrow(listed), 2L)
    expect_true(all(listed$type == 'file'))
    #names come from the results, so the two bundles describe the same cutouts
    expect_equal(sub('[.](fits|jpg)$', '', listed$filename), names(reactiveValuesToList(state)$res))
    ext = if(grepl('fits', key)) '[.]fits$' else '[.]jpg$'
    expect_equal(sum(grepl(ext, listed$filename)), 2L)
  }
}, session = shiny::MockShinySession$new())

#ex 21 the FITS download specifically, read back from the archive. This is the one place
#the app hands someone a file they will use elsewhere, so the pixels and the WCS both have
#to come out the far end intact
if(requireNamespace('zip', quietly = TRUE)){
  testServer(app_env$server, expr = {
    put_inputs(session, run_inputs)
    put_inputs(session, positions = paste(img_kv$CRVAL1 + 0.02, img_kv$CRVAL2),
               load = 1, run = 1)
    session$flushReact()
    priv = session$.__enclos_env__$private
    if(is.null(priv) || is.null(priv$file_generators)){
      skip('this shiny version does not expose registered downloads')
    }
    keys = priv$file_generators$keys()
    gen = priv$file_generators$get(keys[grepl('dl_fits', keys)])
    dest = tempfile(fileext = '.zip')
    gen$content(dest)
    out = file.path(tempdir(), 'app_unzip')
    dir.create(out, showWarnings = FALSE)
    zip::unzip(dest, exdir = out)
    files = list.files(out, pattern = '[.]fits$', full.names = TRUE)
    expect_length(files, 1L)
    back = Rfits_read_image(files[1])
    #the cutout, not the parent tile
    expect_identical(dim(back$imDat), c(51L, 51L))
    expect_equal(back$keyvalues$CRVAL1, img_kv$CRVAL1, tolerance = 1e-6)
    expect_equal(pixscale(back), pixscale(src_image), tolerance = 1e-4)
    #the box was centred where it was asked for, which only holds if the WCS survived
    cen = centre(back)
    expect_equal(unname(cen[1, 'RA']), unname(img_kv$CRVAL1 + 0.02), tolerance = 0.01)
    expect_equal(unname(cen[1, 'Dec']), unname(img_kv$CRVAL2), tolerance = 0.01)
  }, session = shiny::MockShinySession$new())
}

#ex 22 a session index file must not be left behind. A remote or rebuilt index is the
#app's own temporary file, and one per page reload would accumulate for the life of the R
#process on a server hosting many users
testServer(app_env$server, expr = {
  put_inputs(session, run_inputs)
  put_inputs(session, rebuild = TRUE, load = 1)
  session$flushReact()
  path = reactiveValuesToList(state)$idx$path
  expect_true(file.exists(path))
  session$close()
  expect_false(file.exists(path))
}, session = shiny::MockShinySession$new())

#ex 23 the brush fields, checked against the app text as well as behaviourally. Shiny's
#brush object has xmin/xmax/ymin/ymax and never a range vector, so reading one off the
#brush is the bug that made the Frames tab answer 'Inf to -Inf' and select nothing. The
#grep looks for the field access rather than the words, because the comment on the helper
#that replaced it has to be allowed to name what it fixed
expect_false(any(grepl('br$xrange|br$yrange|\\$xrange|\\$yrange', app_text)))

#ex 24 the gallery draws each cutout as a square cell in a grid rather than as a full
#width panel, which is what stopped every cutout being a wide letter box
testServer(app_env$server, expr = {
  put_inputs(session, run_inputs)
  put_inputs(session, positions = paste(img_kv$CRVAL1, img_kv$CRVAL2),
             cut_width = 280, load = 1, run = 1)
  session$flushReact()
  expect_length(reactiveValuesToList(state)$res, 1L)
  html = paste(as.character(session$getOutput('gallery')), collapse = '\n')
  expect_match(html, 'rf-cut-grid', fixed = TRUE)
  expect_match(html, 'rf-cut-cell', fixed = TRUE)
  expect_match(html, 'rf-cut', fixed = TRUE)
  #the requested side reaches both the grid template and the cell cap
  expect_match(html, '280px', fixed = TRUE)
  expect_match(html, 'id="cutplot_1"', fixed = TRUE)
  #height is given as a percentage so the plot fills the square wrapper, rather than as a
  #pixel height that would make the drawing area as wide as the column
  expect_match(html, 'height:100%', fixed = TRUE)
}, session = shiny::MockShinySession$new())

#ex 25 the display controls are live and capped. Only max_show panels are registered, but
#the message has to say so rather than leave the user counting
testServer(app_env$server, expr = {
  put_inputs(session, run_inputs)
  put_inputs(session, positions = paste(img_kv$CRVAL1, img_kv$CRVAL2),
             load = 1, run = 1)
  session$flushReact()
  put_inputs(session, max_show = 1)
  session$flushReact()
  html = paste(as.character(session$getOutput('gallery')), collapse = '\n')
  expect_match(html, 'id="cutplot_1"', fixed = TRUE)
  put_inputs(session, max_show = 0)
  session$flushReact()
  expect_no_error(session$getOutput('gallery'))
  #a silly width falls back rather than producing a zero sized grid
  put_inputs(session, cut_width = 1)
  session$flushReact()
  html = paste(as.character(session$getOutput('gallery')), collapse = '\n')
  expect_match(html, 'minmax\\(min\\(100%, [0-9]+px\\), 1fr\\)')
}, session = shiny::MockShinySession$new())

#ex 26 the store each cutout came from is reported in the form a user recognises. For a
#local directory that is the path, and it is what the caption under each panel shows
testServer(app_env$server, expr = {
  put_inputs(session, run_inputs)
  put_inputs(session, positions = paste(img_kv$CRVAL1, img_kv$CRVAL2),
             load = 1, run = 1)
  session$flushReact()
  res = reactiveValuesToList(state)$res
  expect_identical(length(attr(res, 'filename')), length(res))
  expect_match(attr(res, 'filename')[1], 'tile1[.]zarr')
  #and the caption in the gallery carries it
  html = paste(as.character(session$getOutput('gallery')), collapse = '\n')
  expect_match(html, 'tile1[.]zarr')
}, session = shiny::MockShinySession$new())

#ex 27 a candidate store that opens cleanly but holds no overlap must be named. This is
#the difference between 'the release does not cover that position' and a result that looks
#quietly lost, which is what two tiles and one picture used to read as. At box = 400 the
#index offers both tiles because a 400 pixel box centred on tile1 reaches into tile2's
#neighbourhood, but tile2's own footprint does not cover the position, so it is scanned,
#answers nothing, and has to be reported as scanned rather than as an error
testServer(app_env$server, expr = {
  put_inputs(session, run_inputs)
  put_inputs(session, positions = paste(img_kv$CRVAL1, img_kv$CRVAL2),
             box = 400, max_stores = 25, load = 1)
  session$flushReact()
  put_inputs(session, run = 1)
  session$flushReact()
  s = reactiveValuesToList(state)
  log = paste(s$log, collapse = '\n')
  #one cutout, from the tile that actually covers the position
  expect_length(s$res, 1L)
  expect_match(names(s$res)[1], 'tile1')
  #the store that was scanned but did not overlap is named in that same sentence, which is
  #the assertion that matters: a bare mention of tile2 anywhere in the log would pass even
  #if the reporting were removed, because the index build names every store it visits
  line = grep('no overlap at the requested position', strsplit(log, '\n')[[1]], value = TRUE)
  expect_length(line, 1L)
  expect_match(line[1], 'tile2')
}, session = shiny::MockShinySession$new())

