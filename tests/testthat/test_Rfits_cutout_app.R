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
#is how a user tells a wrong pattern apart from a release with no such data. The value is
#a glob, so '999*' is the way to ask for a value beginning 999
notes = character(0)
keep = app_env$filter_by_keyword(index_rows, app_index, 'CRVAL1', '999*', note = add_note)
expect_identical(nrow(keep), 0L)
expect_match(paste(notes, collapse = ' '), 'Values seen')
#the note quotes back what was typed rather than the regular expression it became, since a
#user who typed a glob and was shown '^999' would be reading the conversion, not their own
expect_match(paste(notes, collapse = ' '), '"999\\*"')
#present and matching keeps the store it belongs to, and only that one. The glob is
#anchored at the front with the coordinate exactly as it appears in the index, and the
#trailing * is what lets the stored value's full precision through
notes = character(0)
keep = app_env$filter_by_keyword(index_rows, app_index, 'CRVAL1',
                                 paste0(round(img_kv$CRVAL1, 4), '*'), note = add_note)
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

#ex 15a the same pattern has to narrow what the tabs list, not only what a request cuts.
#A published index is the whole release, so a session filtered to one tile must not be shown
#the other one on the Index, Frames or Store tabs while the search refuses to cut it
testServer(app_env$server, expr = {
  put_inputs(session, run_inputs)
  put_inputs(session, load = 1)
  session$flushReact()
  #with no pattern the whole index is in view
  expect_equal(nrow(reactiveValuesToList(state)$rows), 2L)
  expect_match(session$getOutput('frames_info'), 'Frames in view: 2')

  #a pattern narrowing the session to one tile narrows every view at once
  put_inputs(session, pattern = 'tile1', load = 2)
  session$flushReact()
  s = reactiveValuesToList(state)
  expect_equal(nrow(s$rows), 1L)
  expect_true(all(grepl('tile1', s$rows$label)))
  #the index object the queries are made against carries the narrowed rows too, so the
  #status panel and the search cannot be reading two different sets
  expect_equal(nrow(s$idx$rows), 1L)
  #and the tabs, which is what the pattern was not reaching before
  html = paste(as.character(session$getOutput('index_table')), collapse = '')
  expect_true(grepl('tile1', html))
  expect_false(grepl('tile2', html))
  expect_match(session$getOutput('frames_info'), 'Frames in view: 1')
  expect_match(session$getOutput('idx_info'), 'index rows:   1')
  expect_match(paste(s$log, collapse = '\n'), 'Name pattern kept 1 of 2 index row')

  #a pattern that matches nothing is an ordinary empty view rather than an error, and the
  #range() a summary would take over no rows must not warn
  put_inputs(session, pattern = 'nope*', load = 3)
  session$flushReact()
  s = reactiveValuesToList(state)
  expect_equal(nrow(s$rows), 0L)
  expect_no_error(session$getOutput('store_summary'))
  #renderUI hands back a tag list rather than a string, so it is pasted to characters first
  expect_match(paste(as.character(session$getOutput('store_summary')), collapse = ''),
               'No store in the index matches')
  expect_match(session$getOutput('frames_info'), 'No frames to show')
  expect_match(session$getOutput('idx_info'), 'excluded every row')

  #back to the whole index, so the narrowing is not sticky
  put_inputs(session, pattern = '', load = 4)
  session$flushReact()
  expect_equal(nrow(reactiveValuesToList(state)$rows), 2L)
}, session = shiny::MockShinySession$new())

#ex 15a2 the helper both the tabs and the search go through. All patterns must match, so a
#comma separated field is an AND rather than a union, and a blank or absent pattern is no
#filter at all rather than a filter that matches nothing
pr_rows = data.frame(label = c('a/tile1.zarr', 'a/tile2.zarr'), status = 'ok',
                     stringsAsFactors = FALSE)
expect_identical(nrow(app_env$pattern_rows(pr_rows, app_env$glob_regex('tile1'))), 1L)
expect_identical(nrow(app_env$pattern_rows(pr_rows, app_env$glob_regex('tile*'))), 2L)
expect_identical(nrow(app_env$pattern_rows(pr_rows, app_env$glob_regex('*.zarr'))), 2L)
#both have to match, and no single label can be both tile1 and tile2
expect_identical(nrow(app_env$pattern_rows(pr_rows, app_env$glob_regex(c('tile1', 'tile2')))), 0L)
#blank, NULL and an empty set all leave the rows alone
expect_identical(nrow(app_env$pattern_rows(pr_rows, character(0))), 2L)
expect_identical(nrow(app_env$pattern_rows(pr_rows, NULL)), 2L)
expect_identical(nrow(app_env$pattern_rows(pr_rows[0, , drop = FALSE],
                                           app_env$glob_regex('tile1'))), 0L)

#ex 15b the store name pattern is a glob rather than a regular expression, converted with
#glob2rx. What has to hold is that a name finds a store anywhere in its path rather than
#only as the whole string, that * and ? are the wildcards, and that the characters glob2rx
#does not escape are still literals rather than being read as regex
expect_identical(app_env$glob_regex('tile2'), 'tile2')
expect_identical(app_env$glob_regex('tile*'), 'tile.*')
expect_identical(app_env$glob_regex('*tile*'), '.*tile.*')
expect_identical(app_env$glob_regex('*.zarr'), '.*\\.zarr')
#the anchors glob2rx adds are dropped, so 'tile2' is a substring test on a full store path
expect_true(grepl(app_env$glob_regex('tile2'), '/data/releases/dr1/tile2.zarr'))
expect_true(grepl(app_env$glob_regex('*.zarr'), '/data/releases/dr1/tile2.zarr'))
#the characters glob2rx leaves alone must not become regex operators: '+' is a literal
#here, not 'one or more of the previous', and '|' is a literal rather than alternation
expect_true(grepl(app_env$glob_regex('a+b'), 'xa+bx'))
expect_false(grepl(app_env$glob_regex('a+b'), 'xab'))
expect_true(grepl(app_env$glob_regex('grz|riz'), 'xgrz|rizx'))
expect_false(grepl(app_env$glob_regex('grz|riz'), 'xgrzx'))
#a pattern containing a backslash must still compile, which it would not if glob2rx were
#handed a bare trailing one
expect_no_warning(grepl(app_env$glob_regex('a\\'), 'x'))
#vectorised over the comma separated field, and a blank or NA pattern filters nothing
expect_identical(app_env$glob_regex(c('tile*', '*.zarr')), c('tile.*', '.*\\.zarr'))
expect_length(app_env$glob_regex(''), 0L)
expect_length(app_env$glob_regex(NA_character_), 0L)
expect_length(app_env$glob_regex(character(0)), 0L)

#and the globs work through the search path rather than only in the helper
testServer(app_env$server, expr = {
  for(pat in c('tile*', '*tile*', '*.zarr')){
    put_inputs(session, run_inputs)
    put_inputs(session, positions = paste(img_kv$CRVAL1 + 0.02, img_kv$CRVAL2),
               pattern = pat, load = 1, run = 1)
    session$flushReact()
    s = reactiveValuesToList(state)
    #both tiles survive the pattern, and the position is inside tile1
    expect_match(paste(s$log, collapse = '\n'), 'Name pattern kept 2',
                 info = paste('pattern', pat))
    expect_length(s$res, 1L)
  }
  #a glob that matches nothing is reported as such rather than quietly searching everything
  put_inputs(session, pattern = 'nope*', load = 2, run = 2)
  session$flushReact()
  expect_match(paste(reactiveValuesToList(state)$log, collapse = '\n'),
               'Name pattern kept 0')
}, session = shiny::MockShinySession$new())

#ex 15c the Index tab's store name filter is the same glob, and the Frames tab reads it
#through the same helper, so the two views cannot disagree about what is on screen
testServer(app_env$server, expr = {
  put_inputs(session, run_inputs)
  put_inputs(session, load = 1)
  session$flushReact()
  index_rows_html = function(){
    paste(as.character(session$getOutput('index_table')), collapse = '')
  }
  #a bare name is a substring test, so it finds the store it names and not the other
  put_inputs(session, idx_filter = 'tile2')
  html = index_rows_html()
  expect_true(grepl('tile2', html))
  expect_false(grepl('tile1', html))
  expect_match(session$getOutput('frames_info'), 'Frames in view: 1')
  #a wildcard reaches both
  put_inputs(session, idx_filter = 'tile*')
  expect_true(grepl('tile1', index_rows_html()))
  expect_true(grepl('tile2', index_rows_html()))
  expect_match(session$getOutput('frames_info'), 'Frames in view: 2')
  #and an asterisk is a wildcard rather than the regex quantifier it would be if the text
  #reached grepl unchanged, which is what makes 'tile*' work at all
  put_inputs(session, idx_filter = '*.zarr')
  expect_match(session$getOutput('frames_info'), 'Frames in view: 2')
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
  #one polyline trace carrying every footprint, one marker trace carrying the centres, and
  #one trace held open for the clicked positions. A trace per tile would mean a trace per
  #thousand tiles, so the count is the thing; that it stays at three whatever has been
  #picked is what lets a click leave the zoom alone
  expect_length(spec$x$data, 3)
  expect_identical(spec$x$data[[1]]$mode, 'lines')
  expect_identical(spec$x$data[[2]]$mode, 'markers')
  expect_identical(spec$x$data[[3]]$mode, 'markers')
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
  #and every vertex of a ring carries its own ring's id, so a box or lasso drag that catches
  #an edge resolves to the right frame. The separator carries nothing
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

  #a click anywhere is a position, given to the server by the browser as degrees rather
  #than as the point plotly thinks was hit. This is the whole reason the coordinate is
  #worked out client side: plotly's click event reports the coordinates of the nearest
  #point, which would snap every pick to a tile centre or marker
  click = function(ra, dec){
    put_inputs(session, frames_click = list(ra = ra, dec = dec, t = 1))
  }
  before = paste(as.character(session$getOutput('frames')), collapse = '')
  click(fr$ra[1] + 0.001, fr$dec[1] - 0.001)
  expect_match(session$getOutput('frames_info'), 'Picked by click: 1')
  #the panel is not redrawn by a click, which is the whole point of drawing the pick
  #through the proxy: a re-render reconciles a new specification against the panel on
  #screen, and that is what used to snap the view back out to the full extent
  expect_identical(paste(as.character(session$getOutput('frames')), collapse = ''), before)
  #a second click adds to the list rather than replacing it
  click(fr$ra[2] + 0.002, fr$dec[2] + 0.002)
  expect_match(session$getOutput('frames_info'), 'Picked by click: 2')
  #clicking the same position twice must not duplicate it
  click(fr$ra[2] + 0.002, fr$dec[2] + 0.002)
  expect_match(session$getOutput('frames_info'), 'Picked by click: 2')
  #a click whose coordinates never arrived is ignored rather than picked as NA
  click(NA, NA)
  expect_match(session$getOutput('frames_info'), 'Picked by click: 2')

  #what goes to the Cutouts tab is the position that was clicked, to the rounding used
  put_inputs(session, send_pos = 1)
  session$flushReact()
  log = paste(reactiveValuesToList(state)$log, collapse = '\n')
  expect_match(log, 'Sent 2 clicked position')
  expect_match(log, paste(signif(fr$ra[1] + 0.001, 9), signif(fr$dec[1] - 0.001, 9)),
               fixed = TRUE)
  #and it is not the centre of the nearest tile, which is what it used to be
  expect_false(grepl(paste(signif(fr$ra[1], 9), signif(fr$dec[1], 9)), log, fixed = TRUE))

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

  #clearing the clicked list is deliberate, and leaves the box selection alone: a legend
  #toggle or a stray click must not throw away a list the user built
  put_inputs(session, clear_picks = 1)
  session$flushReact()
  info = session$getOutput('frames_info')
  expect_match(info, 'Picked by click: 0')
  expect_match(info, 'Selected by box: 2')

  #the Index tab's filters apply here too, so a session narrowed by name shows the same
  #tiles in both places. The surviving frame keeps the id it was given in the unfiltered
  #set, which is what keeps a box selection pointing at the right tile
  put_inputs(session, idx_filter = 'tile2')
  session$flushReact()
  spec = spec_of()
  expect_equal(length(spec$x$data[[2]]$x), 1)
  expect_equal(as.numeric(spec$x$data[[2]]$customdata[[1]]), 2)
  expect_match(session$getOutput('frames_info'), 'Frames in view: 1')
  #the box now holds only the frame that is still in the view
  expect_match(session$getOutput('frames_info'), 'Selected by box: 1')
  #with nothing clicked left, sending adds no position at all rather than a stale one
  put_inputs(session, send_pos = 2)
  session$flushReact()
  log = paste(reactiveValuesToList(state)$log, collapse = '\n')
  expect_equal(sum(grepl('clicked position', strsplit(log, '\n')[[1]])), 1L,
               info = 'only the earlier send should be in the log')
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


#ex 28 which cutouts go into a bundle. The selection is one logical vector with three
#ways of writing it -- the buttons, the tick beside each plotted cutout, and the row
#checkboxes of the results table -- so what has to hold is that they agree, that a new run
#starts from everything included, and that the count on the page tracks all of it
testServer(app_env$server, expr = {
  put_inputs(session, run_inputs)
  put_inputs(session, positions = paste0(img_kv$CRVAL1 + 0.05, ' ', img_kv$CRVAL2, '\n',
                                         img_kv$CRVAL1, ' ', img_kv$CRVAL2),
             load = 1, run = 1)
  session$flushReact()
  res = reactiveValuesToList(state)$res
  expect_length(res, 2L)
  #a fresh run includes everything, whatever the last run's ticks said
  expect_identical(reactiveValuesToList(state)$sel, rep(TRUE, 2L))
  expect_equal(n_selected(), 2L)
  #ids carry the run number, so a box left on the page from an earlier, longer request
  #cannot be read as a tick against this one
  expect_equal(cell_id(1), 'sel_1_1')

  #untick one cell through the box beside it
  put_inputs(session, sel_1_1 = TRUE, sel_1_2 = FALSE)
  expect_identical(reactiveValuesToList(state)$sel, c(TRUE, FALSE))
  expect_equal(n_selected(), 1L)
  expect_identical(names(selected_res()), names(res)[1])
  #and the count is shown
  #helpText interleaves the values and the literals across lines, so the match is loose
  expect_match(paste(as.character(session$getOutput('sel_count')), collapse = ' '),
               '1\\s+of\\s+2')

  #Include none, then invert. Both have to survive the boxes on the page still reporting
  #their old values, which is what a round trip through the pushed update looks like
  put_inputs(session, sel_none = 1)
  expect_identical(reactiveValuesToList(state)$sel, c(FALSE, FALSE))
  expect_equal(n_selected(), 0L)
  put_inputs(session, sel_1_1 = FALSE, sel_1_2 = FALSE)
  expect_identical(reactiveValuesToList(state)$sel, c(FALSE, FALSE))
  put_inputs(session, sel_invert = 1)
  expect_identical(reactiveValuesToList(state)$sel, c(TRUE, TRUE))
  put_inputs(session, sel_1_1 = TRUE, sel_1_2 = TRUE)
  expect_identical(reactiveValuesToList(state)$sel, c(TRUE, TRUE))
  put_inputs(session, sel_none = 1)
  put_inputs(session, sel_all = 1)
  expect_identical(reactiveValuesToList(state)$sel, c(TRUE, TRUE))

  #a tick on a cell that has not been drawn yet must not be read at all, since a partial
  #set would otherwise clear the results that are included but off screen
  put_inputs(session, max_show = 1)
  put_inputs(session, sel_1_1 = TRUE)
  expect_identical(reactiveValuesToList(state)$sel, c(TRUE, TRUE))
  put_inputs(session, sel_1_1 = FALSE)
  expect_identical(reactiveValuesToList(state)$sel, c(FALSE, TRUE))
  put_inputs(session, max_show = 24)

  #one tick in the results table, reported as the index of the cutout in the result list
  #rather than as a row number, since the rows get sorted and filtered in the browser
  put_inputs(session, sel_none = 1)
  put_inputs(session, sel_1_1 = FALSE, sel_1_2 = FALSE)
  put_inputs(session, match_tick = list(i = 2, on = TRUE))
  expect_identical(reactiveValuesToList(state)$sel, c(FALSE, TRUE))
  #a change outside the current result list is ignored rather than recycled
  put_inputs(session, match_tick = list(i = 99, on = TRUE))
  expect_identical(reactiveValuesToList(state)$sel, c(FALSE, TRUE))
  put_inputs(session, match_tick = list(i = 2, on = FALSE))
  expect_identical(reactiveValuesToList(state)$sel, c(FALSE, FALSE))

  #a run that returns a different number of cutouts resets to all in
  put_inputs(session, sel_none = 1)
  put_inputs(session, positions = paste(img_kv$CRVAL1, img_kv$CRVAL2), run = 2)
  session$flushReact()
  expect_equal(cell_id(1), 'sel_2_1')
  expect_identical(reactiveValuesToList(state)$sel, rep(TRUE, 1L))
  #the previous run's unticked box is stale and must not be read against this one
  put_inputs(session, sel_1_1 = FALSE)
  expect_identical(reactiveValuesToList(state)$sel, TRUE)
}, session = shiny::MockShinySession$new())

#ex 28b the selection is pushed back to the page, including to every box on it. The state
#on its own is not enough: 'Include none' has to clear the boxes as well, or the boxes
#report their old ticks back on the next flush and undo the button. That is the bug the
#count beside the buttons was hiding, because the count reads the state and the pictures
#read the page.
#
#updateCheckboxInput is replaced with something that records what was asked for. It has to
#be put in the environment the app was sourced into rather than in the testServer body,
#because an observer looks the name up in the environment it was created in, which is that
#one and not the one this expression runs in
tick_pushes = list()
app_env$updateCheckboxInput = function(session, inputId, ..., value){
  tick_pushes[[length(tick_pushes) + 1]] <<- list(id = inputId, value = value)
  invisible(NULL)
}
withr::defer(rm(updateCheckboxInput, envir = app_env), teardown_env())

testServer(app_env$server, expr = {
  put_inputs(session, run_inputs)
  put_inputs(session, positions = paste0(img_kv$CRVAL1 + 0.05, ' ', img_kv$CRVAL2, '\n',
                                         img_kv$CRVAL1, ' ', img_kv$CRVAL2),
             load = 1, run = 1)
  session$flushReact()
  tick_pushes <<- list()

  #both boxes are rewritten whenever the selection moves, and only the unticked one differs
  put_inputs(session, sel_1_1 = TRUE, sel_1_2 = FALSE)
  #asserted before anything else, because every check below reads the same list and all()
  #over an empty one is true, so a shadow that never fired would otherwise pass quietly
  expect_gte(length(tick_pushes), 2L)
  ids = vapply(tick_pushes, function(x) x$id, character(1))
  expect_true(all(c('sel_1_1', 'sel_1_2') %in% ids))
  vals = setNames(lapply(tick_pushes, function(x) x$value), ids)
  expect_false(isTRUE(vals$sel_1_2))
  expect_true(isTRUE(vals$sel_1_1))

  #Include none is the case that used to be lost. An all FALSE vector is falsy, so a req()
  #on the selection aborted the push and the page never heard about it
  tick_pushes <<- list()
  put_inputs(session, sel_none = 1)
  expect_identical(reactiveValuesToList(state)$sel, c(FALSE, FALSE))
  ids = vapply(tick_pushes, function(x) x$id, character(1))
  expect_true(all(c('sel_1_1', 'sel_1_2') %in% ids))
  expect_true(all(!vapply(tick_pushes, function(x) isTRUE(x$value), logical(1))))

  #the boxes follow the state rather than the other way round: once they have been told,
  #reporting them settles the selection instead of oscillating
  put_inputs(session, sel_1_1 = FALSE, sel_1_2 = FALSE)
  expect_identical(reactiveValuesToList(state)$sel, c(FALSE, FALSE))
  tick_pushes <<- list()
  put_inputs(session, sel_all = 1)
  expect_true(all(vapply(tick_pushes, function(x) isTRUE(x$value), logical(1))))
  put_inputs(session, sel_1_1 = TRUE, sel_1_2 = TRUE)
  expect_identical(reactiveValuesToList(state)$sel, c(TRUE, TRUE))
}, session = shiny::MockShinySession$new())

#ex 28c the results table carries a real column of checkboxes. DT's own row selection is
#only a highlight, so the boxes a user is meant to click are drawn by the app
testServer(app_env$server, expr = {
  put_inputs(session, run_inputs)
  put_inputs(session, positions = paste0(img_kv$CRVAL1 + 0.05, ' ', img_kv$CRVAL2, '\n',
                                         img_kv$CRVAL1, ' ', img_kv$CRVAL2),
             load = 1, run = 1)
  session$flushReact()
  skip_if_not(app_env$has_dt, 'DT is not installed')
  html = paste(as.character(session$getOutput('match_table')), collapse = '\n')
  #the widget payload is JSON, so the quotes inside a cell arrive escaped; they are not
  #what is being asserted and unescaping them keeps the patterns readable
  html = gsub('\\\\', '', html)
  expect_match(html, 'rf-tick', fixed = TRUE)
  expect_equal(length(gregexpr('type="checkbox"', html, fixed = TRUE)[[1]]), 2L)
  #each box carries the index of its cutout, and starts ticked because the run did
  expect_match(html, 'data-rf-i="1"', fixed = TRUE)
  expect_match(html, 'data-rf-i="2"', fixed = TRUE)
  expect_match(html, 'checked', fixed = TRUE)
  #and the callback that wires them up is in the widget, not lost between renderers
  expect_match(html, 'match_tick', fixed = TRUE)
  expect_match(html, 'rf_set_ticks', fixed = TRUE)
}, session = shiny::MockShinySession$new())

#ex 29 the download options. The compression string is what cfitsio parses out of the file
#name, so its shape is worth pinning: the algorithm, then the tile sizes, then a semicolon
#and the quantization level
testServer(app_env$server, expr = {
  expect_equal(dl_opts()$fits_format, 'raw')
  expect_equal(comp_spec(list(alg = 'RICE', tile = '', quant = NA_real_)), 'RICE')
  #the box is typed by a person, so spaces and a stray comma are tidied rather than
  #passed through into the name cfitsio will parse
  expect_equal(comp_spec(list(alg = 'RICE', tile = ' 64, 64 ', quant = NA_real_)),
               'RICE 64,64')
  expect_equal(comp_spec(list(alg = 'RICE', tile = '64 64,', quant = NA_real_)),
               'RICE 64,64')
  expect_equal(comp_spec(list(alg = 'RICE', tile = '64,64', quant = 8)),
               'RICE 64,64; q 8')
  expect_equal(comp_spec(list(alg = 'HCOMPRESS', tile = '', quant = -0.0002)),
               'HCOMPRESS; q -0.0002')
  #0 has to survive as text, since it is the difference between lossy and lossless
  expect_equal(comp_spec(list(alg = 'GZIP', tile = '', quant = 0)), 'GZIP; q 0')
  #a quality outside the device's range is pulled back rather than passed on
  put_inputs(session, jpeg_quality = 500)
  expect_equal(dl_opts()$jpeg_quality, 100)
  put_inputs(session, jpeg_quality = NA)
  expect_equal(dl_opts()$jpeg_quality, 75)
}, session = shiny::MockShinySession$new())

#ex 30 a compressed bundle, read back from the archive. The point of the option is that
#the file is smaller and still opens, so both halves are checked; the pixels may not be
#identical, because cfitsio quantizes unless told otherwise
if(requireNamespace('zip', quietly = TRUE)){
  testServer(app_env$server, expr = {
    put_inputs(session, run_inputs)
    put_inputs(session, positions = paste(img_kv$CRVAL1, img_kv$CRVAL2),
               load = 1, run = 1)
    session$flushReact()
    priv = session$.__enclos_env__$private
    if(is.null(priv) || is.null(priv$file_generators)){
      skip('this shiny version does not expose registered downloads')
    }
    fits_gen = function(){
      keys = priv$file_generators$keys()
      priv$file_generators$get(keys[grepl('dl_fits', keys)][1])
    }
    raw = tempfile(fileext = '.zip')
    fits_gen()$content(raw)
    raw_dir = file.path(tempdir(), 'app_unzip_raw')
    dir.create(raw_dir, showWarnings = FALSE)
    zip::unzip(raw, exdir = raw_dir)
    raw_file = list.files(raw_dir, pattern = '[.]fits$', full.names = TRUE)
    expect_length(raw_file, 1L)

    put_inputs(session, fits_format = 'compress', fits_alg = 'GZIP', fits_tile = '',
               fits_quant = 0)
    #the name of the archive says what is in it, since raw and compressed are not
    #interchangeable and a downloads folder will hold both
    keys = priv$file_generators$keys()
    gen = fits_gen()
    expect_match(gen$filename(), 'compressed')
    comp = tempfile(fileext = '.zip')
    gen$content(comp)
    comp_dir = file.path(tempdir(), 'app_unzip_comp')
    dir.create(comp_dir, showWarnings = FALSE)
    zip::unzip(comp, exdir = comp_dir)
    comp_file = list.files(comp_dir, pattern = '[.]fits$', full.names = TRUE)
    expect_length(comp_file, 1L)
    #a tile compressed image lives in a binary table, so it is read from extension 2
    back = Rfits_read_image(comp_file[1], ext = 2)
    expect_identical(dim(back$imDat), c(51L, 51L))
    expect_equal(max(abs(back$imDat - Rfits_read_image(raw_file[1])$imDat)), 0,
                 tolerance = 1e-6)
    expect_equal(back$keyvalues$CRVAL1, img_kv$CRVAL1, tolerance = 1e-6)
    expect_true(isTRUE(back$keyvalues$ZIMAGE))
    #and the raw download of the same cutout stays a plain image
    plain = Rfits_read_image(raw_file[1])
    expect_null(plain$keyvalues$ZIMAGE)
    expect_identical(dim(plain$imDat), c(51L, 51L))
  }, session = shiny::MockShinySession$new())
}

#ex 31 a cutout whose header came from a tile compressed store must still download as a
#readable raw file. The shape a compressed store records is ZNAXIS, and Rfits_write_image
#honours ZIMAGE when it is in the keywords, so writing one out raw without stripping them
#gives a plain image that tells a reader to expect a binary table it does not hold
testServer(app_env$server, expr = {
  put_inputs(session, run_inputs)
  put_inputs(session, positions = paste(img_kv$CRVAL1, img_kv$CRVAL2),
             load = 1, run = 1)
  session$flushReact()
  item = reactiveValuesToList(state)$res[[1]]
  item$keyvalues$ZIMAGE = TRUE
  item$keyvalues$ZBITPIX = -32
  item$keyvalues$ZNAXIS = 2L
  item$keyvalues$ZNAXIS1 = 999
  item$keyvalues$ZNAXIS2 = 999
  item$keycomments$ZIMAGE = ''
  item$keycomments$ZNAXIS1 = ''
  item$keycomments$ZNAXIS2 = ''
  item$keynames = names(item$keyvalues)

  stripped = prepare_fits_item(item, list(fits_format = 'raw'))
  expect_null(stripped$keyvalues$ZIMAGE)
  expect_null(stripped$keyvalues$ZNAXIS1)
  #the shape is taken from the pixels, which is the only thing that can be authoritative
  expect_identical(stripped$keyvalues$NAXIS1, nrow(item$imDat))
  expect_identical(stripped$keyvalues$NAXIS2, ncol(item$imDat))
  #a compressed download keeps the keys, since cfitsio rewrites them for real tiles
  kept = prepare_fits_item(item, list(fits_format = 'compress'))
  expect_true(isTRUE(kept$keyvalues$ZIMAGE))

  #and the file it writes has to open, which is the assertion that catches the original
  #failure rather than merely the bookkeeping around it
  out = tempfile(fileext = '.fits')
  Rfits_write_image(stripped, filename = out)
  back = Rfits_read_image(out)
  expect_identical(dim(back$imDat), dim(item$imDat))
  expect_equal(max(abs(back$imDat - item$imDat)), 0, tolerance = 1e-6)
}, session = shiny::MockShinySession$new())

#ex 32 an empty selection downloads nothing rather than everything, and says so. A user
#who has clicked Include none and then the download button needs to be told the two
#actions are the reason for the empty archive, not that the app lost the results
testServer(app_env$server, expr = {
  put_inputs(session, run_inputs)
  put_inputs(session, positions = paste(img_kv$CRVAL1, img_kv$CRVAL2),
             load = 1, run = 1)
  session$flushReact()
  put_inputs(session, sel_none = 1)
  expect_length(selected_idx(), 0L)
  expect_null(selected_res())
  #write_all reports rather than writing an archive of nothing
  wrote = write_all('fits', function(item, path){
    Rfits_write_image(item, filename = path)
  })
  expect_null(wrote)
  log = paste(reactiveValuesToList(state)$log, collapse = '\n')
  expect_match(log, 'No cutouts are selected', fixed = TRUE)
}, session = shiny::MockShinySession$new())

#ex 33 every include box that the gallery draws is an input the server reads back, so a
#renamed id would silently leave the app unable to unselect anything. Checked against the
#rendered cell as well as the observers, because the id is built by a helper
testServer(app_env$server, expr = {
  put_inputs(session, run_inputs)
  put_inputs(session, positions = paste(img_kv$CRVAL1, img_kv$CRVAL2),
             load = 1, run = 1)
  session$flushReact()
  html = paste(as.character(session$getOutput('gallery')), collapse = '\n')
  expect_match(html, 'id="sel_1_1"', fixed = TRUE)
  #the results card carries the three buttons and the count
  bar = paste(as.character(session$getOutput('select_bar')), collapse = '\n')
  for(id in c('sel_all', 'sel_none', 'sel_invert')){
    expect_match(bar, paste0('id="', id, '"'), fixed = FALSE)
  }
}, session = shiny::MockShinySession$new())

#ex 34 the tables that have no callback of their own must still render. DT checks the
#callback argument against its own default and stops for anything else, NULL included, so
#naming it unconditionally errored the Index and Frames tables with "The 'callback'
#argument only accept a value returned from JS()". The default is left unset for those and
#supplied only by the results table, which needs one to report its tick boxes
testServer(app_env$server, expr = {
  put_inputs(session, run_inputs)
  put_inputs(session, positions = paste0(img_kv$CRVAL1 + 0.02, ' ', img_kv$CRVAL2),
             load = 1, run = 1)
  session$flushReact()
  if(!app_env$has_dt){
    skip('DT is not installed')
  }
  #each of the three tables renders rather than erroring, which is the whole assertion:
  #two of them have no callback and one does
  for(id in c('index_table', 'frames_table', 'match_table')){
    expect_no_error(session$getOutput(id))
  }
  #and the one that does carry a callback still carries it, since dropping it would leave
  #the results table's boxes unable to report a change
  html = paste(as.character(session$getOutput('match_table')), collapse = '')
  expect_match(gsub('\\\\', '', html), 'match_tick', fixed = TRUE)
}, session = shiny::MockShinySession$new())

#ex 35 the payload that draws a clicked position on the Frames plot. plotly's restyle wants
#one value per trace, so the arrays have to be a list holding one array, {x: [[...]]}. A
#bare list() lets jsonlite unbox a length one vector, and a single pick then goes over the
#wire as a scalar that restyle cannot draw: the first click appeared to do nothing and the
#second drew two marks at once, because only then was the vector long enough to survive
#unboxing. Checked as the JSON Shiny would send rather than as the R object, since the
#unboxing is the bug and it happens on the way out
shape = function(ra, dec){
  as.character(shiny:::toJSON(list(x = app_env$pick_restyle_args(ra, dec)$x,
                                   y = app_env$pick_restyle_args(ra, dec)$y)))
}
expect_identical(shape(numeric(0), numeric(0)), '{"x":[[]],"y":[[]]}')
#the decisive case: one pick must still arrive as an array of one
expect_identical(shape(53.1234, -27.8123), '{"x":[[53.1234]],"y":[[-27.8123]]}')
expect_identical(shape(c(1, 2, 3), c(4, 5, 6)), '{"x":[[1,2,3]],"y":[[4,5,6]]}')
#and never as the unboxed scalar, which is what the bare list() produced
expect_false(grepl('"x":\\[53', shape(53.1234, -27.8123)))
