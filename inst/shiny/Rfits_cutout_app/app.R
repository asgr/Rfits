#Cutout explorer for a directory (or S3 prefix) of Zarr stores written by Rfits.
#
#The point of this app is that a request is answered from the index rather than from the
#tiles. Rfits_cutout_zarr_dir can search a store set by reading one small metadata
#document per store, but over S3 that is still one request per store, so a release of a
#few thousand tiles spends a few thousand requests before the first pixel. index.parquet,
#built by Rfits_zarr_index, holds the same information once, and
#Rfits_zarr_index_query narrows a request to the handful of stores that can possibly
#overlap it. The app then cuts only those.
#
#Everything here is read-only against the store being browsed. When an index is missing
#the app builds one and keeps it in its own temporary file rather than uploading it
#beside the data, because a shared release is not the place to write a private cache:
#whoever publishes the tiles should decide whether an index ships with them. That is also
#why the search is issued per store instead of passing cache = to
#Rfits_cutout_zarr_dir, which would write what it learns back to the bucket.
#
#Run it with:
#  Rfits_cutout_app()
#or:
#  shiny::runApp(system.file('shiny', 'Rfits_cutout_app', package = 'Rfits'))

library(shiny)
library(bslib)

need = c('Rfits', 'plotly')
missing = need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if(length(missing) > 0){
  stop('The Rfits cutout app needs: ', paste(missing, collapse = ', '),
       '. Please install them from CRAN.', call. = FALSE)
}
if(!requireNamespace('Rwcs', quietly = TRUE)){
  #Without Rwcs there is no WCS aware plot method, and no overlap or position test
  stop('The Rfits cutout app needs the Rwcs package to plot and to search by RA/Dec. ',
       'Please install it from CRAN.', call. = FALSE)
}
has_dt = requireNamespace('DT', quietly = TRUE)
if(has_dt){
  library(DT)
}

#bslib renamed nav_panel to tab_panel in 1.0, and the two take the same arguments. The
#app ships inside the package, so it has to start on whatever version the user has rather
#than the one it was written against
tab_panel = if(exists('tab_panel', envir = asNamespace('bslib'), inherits = FALSE)){
  bslib::tab_panel
}else{
  bslib::nav_panel
}

#The index readers below are internal to Rfits. This app ships inside the package, so
#reaching for them is closer to the intent than rebuilding their logic here. They are the
#read-only half of the index layer; nothing in this file calls .zarr_index_put, which is
#the one function in that file that would write to the bucket.
zarr_open_index = function(index, bucket, prefix, creds){
  src = tryCatch(Rfits:::.zarr_index_open(index = index, bucket = bucket, prefix = prefix,
                                          region = creds$region,
                                          endpoint = creds$endpoint,
                                          access_key = creds$access_key,
                                          secret_key = creds$secret_key,
                                          session_token = creds$session_token),
                 error = function(e) e)
  if(inherits(src, 'error')){
    return(list(src = NULL, error = conditionMessage(src)))
  }
  return(list(src = src, error = NULL))
}

#Three states an index probe can come back with. Absent is ordinary (a release nobody has
#indexed yet) and is the only thing that licenses building one: an AccessDenied or a
#malformed file is reported rather than quietly worked around by rebuilding, because a
#rebuild of the same prefix meets the same permission problem and costs a request per
#store on the way to it.
load_index = function(con, rebuild = FALSE, note = function(...){}){
  creds = con$creds
  if(!rebuild){
    got = zarr_open_index(con$index_name, bucket = con$bucket, prefix = con$index_prefix,
                          creds = creds)
    if(!is.null(got$error)){
      return(list(error = got$error))
    }
    src = got$src
    if(!is.null(src$path)){
      #Closing an index source deletes the file when it was downloaded, so the read
      #below happens first and the close is only taken on the paths that do not keep
      #the file. Releasing it here would leave the caller holding a path to a parquet
      #file that no longer exists, which for a remote index means every later query
      #fails while the local one still works, the worst possible way to be wrong
      rows = tryCatch(Rfits:::.zarr_index_light_rows(src$path), error = function(e) e)
      if(inherits(rows, 'error')){
        Rfits:::.zarr_index_close(src)
        return(list(error = conditionMessage(rows)))
      }
      if(nrow(rows) == 0){
        Rfits:::.zarr_index_close(src)
        return(list(error = paste('The index at', src$shown, 'holds no store rows.')))
      }
      note('Read ', nrow(rows), ' store row(s) from ', src$shown)
      return(list(path = src$path, shown = src$shown, rows = rows,
                  owned = isTRUE(src$tempfile), error = NULL))
    }
    #Nothing to keep, so any downloaded copy goes now rather than with the session
    Rfits:::.zarr_index_close(src)
    #A remote object that is not there comes back with missing = TRUE, but a local path
    #that does not exist comes back with no such field at all, and both mean the same
    #thing here: there is no index to read, so one may be built. Only a remote failure
    #that is not 'absent' (notably AccessDenied) is a real error, and must not be worked
    #around by a rebuild that meets the same problem at a cost of a request per store
    if(isTRUE(src$remote) && !isTRUE(src$missing)){
      return(list(error = paste('No readable index at', src$shown)))
    }
    note('No index at ', src$shown, '; building one for this session.')
  }else{
    note('Rebuilding the index from the tiles (one metadata read per store).')
  }

  #Built in memory and written to this session's own temporary file. Rfits_zarr_index is
  #deliberately called without index = so that it returns rows rather than publishing
  #them, and max_dirs is forwarded so a walk that would stop early still warns.
  built = tryCatch({
    if(is.null(con$bucket)){
      Rfits::Rfits_zarr_index(dir = con$dir, pattern = con$pattern,
                              extname = con$extname, max_dirs = con$max_dirs,
                              verbose = FALSE)
    }else{
      Rfits::Rfits_zarr_index(bucket = con$bucket, prefix = con$prefix,
                              pattern = con$pattern, extname = con$extname,
                              region = creds$region, endpoint = creds$endpoint,
                              access_key = creds$access_key,
                              secret_key = creds$secret_key,
                              session_token = creds$session_token,
                              max_dirs = con$max_dirs, verbose = FALSE)
    }
  }, error = function(e) e)
  if(inherits(built, 'error')){
    return(list(error = conditionMessage(built)))
  }
  if(is.null(built) || nrow(built) == 0){
    return(list(error = 'The index build found no stores to record.'))
  }
  path = tempfile(fileext = '.parquet')
  wrote = tryCatch({
    Rfits:::.zarr_index_write(built, path)
    NULL
  }, error = function(e) conditionMessage(e))
  if(!is.null(wrote)){
    return(list(error = paste('Could not write the session index:', wrote)))
  }
  note('Indexed ', nrow(built), ' store row(s) into ', path)
  return(list(path = path, shown = path, rows = Rfits:::.zarr_index_light_rows(path),
              owned = TRUE, error = NULL, built = TRUE))
}

#Positions are typed as free text because that is how they arrive from a finder list or a
#clipboard, one target per line. A line that is not two finite numbers is reported back
#rather than dropped in silence, so a truncated paste cannot quietly search fewer targets
#than the user believes it did.
parse_positions = function(text){
  lines = strsplit(gsub('\\t', ' ', as.character(text)), '[\r\n]+')[[1]]
  lines = trimws(lines)
  lines = lines[nzchar(lines) & !grepl('^#', lines)]
  bad = character(0)
  out = matrix(numeric(0), ncol = 2, dimnames = list(NULL, c('RA', 'Dec')))
  for(i in seq_along(lines)){
    tok = trimws(strsplit(lines[i], '[,; ]+')[[1]])
    tok = tok[nzchar(tok)]
    val = suppressWarnings(as.numeric(tok))
    if(length(val) == 2 && all(is.finite(val))){
      out = rbind(out, unname(val))
    }else{
      bad = c(bad, paste0('line ', i, ' ("', substr(lines[i], 1, 40), '")'))
    }
  }
  attr(out, 'bad') = bad
  return(out)
}

#Run messages go to the log rather than to the console. Every Rfits call here can emit
#both, and a Shiny user has no console; a warning that is only printed is a warning nobody
#saw. Returning the captured text also lets the store loop report one failure per store
#instead of aborting the whole request on the first.
with_capture = function(expr){
  msgs = character(0)
  add = function(kind, txt){
    msgs <<- c(msgs, paste0(kind, ': ', gsub('[\r\n]+$', '', txt)))
  }
  val = withCallingHandlers(
    tryCatch(expr, error = function(e){
      add('ERROR', conditionMessage(e))
      NULL
    }),
    warning = function(w){
      add('WARNING', conditionMessage(w))
      invokeRestart('muffleWarning')
    },
    message = function(m){
      add('INFO', conditionMessage(m))
      invokeRestart('muffleMessage')
    })
  return(list(value = val, msgs = msgs))
}

#A header filter, applied to the candidate stores only. Band and target are usually
#encoded in a header keyword (FILTER, BAND, TARGET) as often as in the store name, and the
#keywords are already in the index, so filtering them costs a decode rather than a read of
#the tiles. Stores whose keywords are not in the index are dropped rather than kept,
#because a filter that could not be evaluated is a filter that was not honoured.
#
#Some things a user reaches for are index columns rather than header keywords. EXTNAME is
#the clearest: a store written by Rfits_dir_to_zarr does not necessarily carry it in its
#header, but the index always records which extension was read, so a filter that looked
#only at keywords would return nothing and look like a release with no such data.
filter_by_keyword = function(rows, index_path, key, value, note = function(...){}){
  if(!nzchar(key) || nrow(rows) == 0){
    return(rows)
  }
  #The value is a glob, like the store name fields, so *grz* is the way to ask for a band
  #that contains grz. Converted once here rather than at each of the two matches below, so
  #the index column path and the keyword path cannot read the same text differently.
  #
  #The text as typed is kept for the notes, which have to quote back what the user asked
  #for rather than the regular expression it became, and the conversion is skipped when the
  #value is blank: glob_regex drops a blank pattern, and a zero length 'yes match anything'
  #flag would break every nzchar() below
  typed = value
  if(nzchar(value)){
    value = glob_regex(value)
  }
  #FITs keywords are upper case by convention and the index columns are lower case, so the
  #comparison is made case insensitively; otherwise EXTNAME would silently fall through to
  #the keyword path and find nothing. Only the scalar columns a user might mean are
  #offered: the payload columns (keyvalues, stores, array_dim) are lists, and 'kind' and
  #the timestamps mean nothing to someone choosing a band
  fallback = c('extname', 'status', 'type', 'label', 'store')
  col = fallback[tolower(fallback) == tolower(key)]
  if(length(col) == 1){
    note('Filtering on the index column ', col, ' rather than a header keyword.')
    val = as.character(rows[[col]])
    keep = if(nzchar(value)){
      grepl(value, val, ignore.case = TRUE)
    }else{
      !is.na(val) & nzchar(val)
    }
    note('Kept ', sum(keep), ' of ', nrow(rows), ' candidate store row(s)')
    return(rows[keep, , drop = FALSE])
  }

  pay = Rfits:::.zarr_index_payload_rows(index_path, rows$cache_key)
  blobs = stats::setNames(as.list(pay$keyvalues), pay$cache_key)
  found = character(0)
  keep = vapply(seq_len(nrow(rows)), function(i){
    blob = blobs[[rows$cache_key[i]]]
    if(is.null(blob) || length(blob) == 0){
      return(FALSE)
    }
    kv = tryCatch(unserialize(blob), error = function(e) NULL)
    if(is.null(kv)){
      return(FALSE)
    }
    val = kv[[key]]
    if(is.null(val)){
      return(FALSE)
    }
    found <<- c(found, paste(as.character(val), collapse = '/'))
    if(!nzchar(value)){
      return(TRUE)
    }
    return(any(grepl(value, as.character(val), ignore.case = TRUE)))
  }, logical(1))
  note('Header keyword ', key, if(nzchar(typed)) paste0(' matching "', typed, '"') else '',
       ': kept ', sum(keep), ' of ', nrow(rows), ' candidate store row(s)')
  if(sum(keep) == 0){
    #An empty result here is otherwise indistinguishable from a release that does not
    #hold what was asked for, so say which of the two happened
    if(length(found) == 0){
      note('No candidate store carries a ', key, ' keyword. Check the spelling against a ',
           'header shown on the Index tab, or narrow by store name pattern instead.')
    }else{
      vals = sort(unique(found))
      note('The ', key, ' keyword is present, but no value matched "', typed, '". ',
           'Values seen: ', paste(utils::head(vals, 10), collapse = ', '),
           if(length(vals) > 10) paste0(' (+', length(vals) - 10, ' more)') else '')
    }
  }
  rows[keep, , drop = FALSE]
}

#The footprint of a tile, from the index alone. corners() would be exact (it follows the
#projection and the rotation) but costs a pair of wcslib calls per store, and this plot is
#meant to show thousands of tiles at once. An axis aligned box built from the reference
#position, the shape and the pixel scale is the same quantity the search's own cone filter
#works with, and differs from the truth only by rotation and projection curvature.
index_footprints = function(rows){
  rows = as.data.frame(rows, stringsAsFactors = FALSE)
  half_w = rows$naxis1 * rows$pixscale_x / 7200
  half_h = rows$naxis2 * rows$pixscale_y / 7200
  #A degree of RA is shorter than a degree of Dec by cos(Dec), so the box widens towards
  #the poles; the floor keeps a tile at the pole from becoming infinitely wide
  cosd = pmax(abs(cos(rows$dec * pi/180)), 1e-6)
  rows$xmin = rows$ra - half_w/cosd
  rows$xmax = rows$ra + half_w/cosd
  rows$ymin = rows$dec - half_h
  rows$ymax = rows$dec + half_h
  rows$name = sub('\\.zarr$', '', basename(rows$store))
  return(rows)
}

#Columns an index row should be shown with. The keyword blob and the cached store lists
#are the bulk of the file and mean nothing in a table, and created is better as a time.
index_view_columns = function(rows){
  rows = as.data.frame(rows, stringsAsFactors = FALSE)
  rows$keyvalues = NULL
  rows$stores = NULL
  rows$array_dim = NULL
  rows$has_keyvalues = NULL
  rows$stamp = NULL
  if('created' %in% names(rows)){
    rows$created = as.POSIXct(rows$created, origin = '1970-01-01')
  }
  return(rows)
}

#Store name patterns are globs, not regular expressions. A path is what a user is looking
#at, so what they will reach for is *.zarr or tile*, and a plain release name like tile2
#behaves the same either way. glob2rx does the conversion, with two adjustments.
#
#The anchors it adds come straight back off: a pattern is matched against the whole store
#path, and the app wants a name that appears anywhere in it, not a whole-string match, so
#'tile2' has to keep finding releases/dr1/tile2.zarr. glob2rx writes them as ^...$ in that
#order, whatever the pattern ends with, and they are removed by position rather than by
#sub(), which would turn a literal $ in the pattern into a real end-of-string anchor.
#
#glob2rx is built on the opposite assumption: it leaves + ^ $ | and the backslash alone
#because in *its* output those are the metacharacters it needs. Every character it does not
#escape is therefore escaped here first, so that a pattern means only what a glob means.
#Without that, 'a+b' would be handed to grepl as 'one or more a', and a backslash would
#reach it as a bare escape.
glob_regex = function(pattern){
  pattern = as.character(pattern)
  pattern = pattern[!is.na(pattern) & nzchar(pattern)]
  if(length(pattern) == 0){
    return(character(0))
  }
  out = gsub('\\', '\\\\', pattern, fixed = TRUE)
  out = gsub('([+^$|)])', '\\\\\\1', out)
  out = utils::glob2rx(out, trim.tail = FALSE)
  out = substring(out, 2)
  return(substr(out, 1, nchar(out) - 1))
}

#Narrow index rows to those whose label matches every pattern given. The patterns arrive
#already converted by glob_regex, which is why this only grepl()s, and all of them have to
#match, so a comma separated field is an AND.
#
#The same narrowing is applied in two places: where the rows are read, so the Index, Frames
#and Store tabs show the stores this session is working with, and where a request picks its
#candidates. One function rather than a loop written out twice, because a published index
#describes a whole release and a session filtered to one band must not be told two different
#things about what is in it.
pattern_rows = function(rows, patterns){
  if(is.null(rows) || length(patterns) == 0 || nrow(rows) == 0){
    return(rows)
  }
  keep = rep(TRUE, nrow(rows))
  for(p in patterns){
    keep = keep & grepl(p, rows$label)
  }
  return(rows[keep, , drop = FALSE])
}

#The arguments of the restyle call that redraws the picked positions on the Frames plot.
#
#The vectors are wrapped in I() so that Shiny's JSON keeps them as arrays. plotly's
#restyle takes a list of per-trace values, so the payload wants to be a list holding one
#array, {x: [[...]]}. Built from a bare list() jsonlite unboxes the inner vector when it
#has length one, and a single pick goes over the wire as {x: [53.1234]}, which restyle
#reads as a scalar: the marker is silently not drawn. The next click makes the vector
#length two, no unboxing happens, and both picks appear at once -- the first click
#appearing to do nothing and the second drawing two marks. I() marks the vector as
#already a JSON array, so the shape holds for any number of picks, one included
pick_restyle_args = function(ra, dec){
  return(list(x = list(I(ra)), y = list(I(dec))))
}

safe_filename = function(x){
  x = gsub('[^A-Za-z0-9._-]+', '_', as.character(x))
  x = substr(x, 1, 90)
  if(!nzchar(x)){
    x = 'cutout'
  }
  return(x)
}

#magimage's locut/hicut mean different things depending on type, which is worth surfacing
#because the default is a quantile and a user who types 3 for a sigma clip gets
#"'probs' outside [0,1]" from stats::quantile instead of a plot.
magmap_units = c('quan (quantile 0-1)' = 'quan', 'sig (sigma)' = 'sig',
                 'num (data value)' = 'num', 'rank (0-1)' = 'rank')

#The results table is drawn by DT with server = FALSE, so one column of plain checkboxes
#can stand in for DT's own row selection, which is only a highlight and reports under a name
#of its own. Shiny cannot see a click inside a widget, so a change is sent back as an event,
#and the state is pushed back as a custom message rather than by redrawing the table, which
#would cost the user their search, their sort and their page.
#
#Two details are worth the trouble. The box carries its index in the result list as a data
#attribute because the rows get sorted, paged and filtered while the index does not move.
#And ticks are mirrored in ticks rather than read off the DOM, because DataTables takes rows
#off the page entirely and puts them back from the original HTML: without a mirror, the
#boxes on a page the user has walked away from and come back to would revert to whatever
#they were drawn as. ticks stays null until the server says otherwise, so the state the
#table was first rendered with is left alone rather than being cleared
table_tick_callback = "
  var ticks = null;
  function sync(){
    if(ticks === null){ return; }
    table.column(0).nodes().each(function(cell){
      var box = cell.querySelector('input.rf-tick');
      if(box){ box.checked = ticks[box.getAttribute('data-rf-i')] === true; }
    });
  }
  //a delegated binding, so it survives the rows DataTables takes off the page and puts
  //back. The box is read off the event target rather than this, which is the safe form
  //whether or not the delegation sets it
  table.on('change', 'input.rf-tick', function(e){
    if(!window.Shiny){ return; }
    var box = e.target;
    Shiny.onInputChange('match_tick', {i: Number(box.getAttribute('data-rf-i')),
                                       on: box.checked});
  });
  table.on('draw.dt', sync);
  if(window.Shiny){
    Shiny.addCustomMessageHandler('rf_set_ticks', function(msg){
      var on = [].concat(msg.i || []), next = {}, k;
      for(k = 0; k < on.length; k++){ next[on[k]] = true; }
      ticks = next;
      sync();
    });
  }
"

ui = page_navbar(
  title = 'Rfits Cutout Explorer',
  id = 'nav',
  #window_title is renamed windowTitle in bslib 1.0, and either way the browser tab gets
  #the title above, so it is left to the default rather than branching on version.
  #fillable = FALSE matters more than it looks. With the default TRUE every tab is
  #stretched to exactly the viewport height and each card's body scrolls inside its own
  #slice of it, which for a column of tall square cutouts becomes a short letter box with
  #a scrollbar of its own. Flowing the page instead lets the cutout column run as long as
  #it needs to and the window scroll do the work
  fillable = FALSE,
  header = tags$style(HTML("
    .rf-name { font-weight: 600; margin-bottom: 0.15rem; margin-top: 0.8rem; }
    .rf-meta { color: var(--bs-secondary-color, #6c757d); font-size: 0.8rem; }
    pre.rf-log { max-height: 18rem; overflow-y: auto; }
    .rf-head-row { display: flex; justify-content: space-between; align-items: center; }
    /* One square drawing area per cutout. The wrapper carries the aspect ratio and the
       plot inside it is told to fill that wrapper, so a column that is wider than the
       cell can only ever add space beside it, never a band above and below it. Setting a
       pixel height on the plot instead is what produced the letter box: the drawing area
       was as wide as the column, so a square image had to be padded to fit */
    .rf-cut { aspect-ratio: 1 / 1; width: 100%; max-width: 100%; flex: 0 0 auto;
              margin-inline: auto; }
    .rf-cut .shiny-plot-output { width: 100% !important; height: 100% !important;
                                 flex: 1 1 auto; min-height: 0; }
    .rf-cut img { width: 100% !important; height: 100% !important;
                  /* contain, not fill: if the browser and the plot ever disagree about
                     the size of the cell, shrinking the image is the graceful answer and
                     stretching it is not */
                  object-fit: contain; }
    .rf-cut-cell { min-width: 0; }
    /* The include tick shares the name row of a cell, so it must not carry the bottom
       margin Bootstrap gives a form check, and must not be wrapped away by the label */
    .rf-cut-cell .form-check { margin-bottom: 0; flex: 0 0 auto; padding-left: 1.4em; }
    .rf-cut-cell .form-check-label { font-size: 0.8rem;
                                     color: var(--bs-secondary-color, #6c757d); }
    .rf-sel-row { display: flex; flex-wrap: wrap; gap: 0.4rem; align-items: center; }
    .rf-sel-row .rf-name { flex: 1 1 auto; min-width: 0; }
    /* A CSS grid item defaults to min-width: auto, which means it refuses to shrink below
       the intrinsic width of its content. A wide index table is such content: it pushes
       the column open until the whole grid scrolls, so the scrollbar that appears belongs
       to the page rather than to the table and the filter card is shoved off screen. This
       is the reason the Index tab could not be scrolled to the right. Letting the item
       shrink hands the overflow back to the table's own scrollX */
    .bslib-grid-item { min-width: 0; }
    .rf-cut-grid { display: grid; gap: 0.8rem; align-items: start; justify-content: start; }
    /* A DataTable asks to be as wide as its content, so it must be kept inside the width
       its column actually has: see the note on .bslib-grid-item above, which is the fix.
       The wrapper is then allowed to scroll, so the scrollbar belongs to the table rather
       than to the page */
    .dataTables_wrapper { overflow-x: auto; width: 100%; }
    table.dataTable { margin-bottom: 0.6rem !important; }
  ")),

  tab_panel(
    title = 'Store', icon = icon('hdd'), value = 'store',
    layout_column_wrap(width = 1/3,
      card(
        card_header(icon('key'), ' S3 credentials'),
        card_body(
          textInput('bucket', 'Bucket', placeholder = 'my-survey-data'),
          textInput('prefix', 'Prefix of the store directory', value = '',
                    placeholder = 'releases/dr1'),
          textInput('index_name', 'Index', value = 'index.parquet'),
          helpText('Read from the bucket root, so this defaults to ',
                   's3://<bucket>/index.parquet. Type a path (e.g. ',
                   'releases/dr1/index.parquet) to look beside the tiles instead. With no ',
                   'bucket it is a local file.'),
          textInput('region', 'Region', placeholder = 'us-west-2'),
          textInput('endpoint', 'Custom endpoint URL',
                    placeholder = 'https://s3.us-east-1.wasabisys.com'),
          textInput('access_key', 'Access key',
                    placeholder = 'blank uses RFITS_S3_ACCESS_KEY'),
          passwordInput('secret_key', 'Secret key',
                        placeholder = 'blank uses RFITS_S3_SECRET_KEY'),
          passwordInput('session_token', 'Session token',
                        placeholder = 'blank uses RFITS_S3_SESSION_TOKEN'),
          helpText('Secrets stay in this session. They are never rendered back to the ',
                   'page and never written to the log.')
        )
      ),
      card(
        card_header(icon('folder'), ' Or a local directory'),
        card_body(
          textInput('dir', 'Directory of .zarr stores',
                    placeholder = '/data/releases/dr1  (blank to use S3)'),
          helpText('Give a bucket above, or a directory here, not both. A directory whose ',
                   'own name ends in .zarr is treated as a single store. With a local ',
                   'directory a bare Index name is taken as a file inside it.'),
          hr(),
          textInput('extname', 'Extension name(s)', value = 'data1'),
          helpText('Comma separated candidates, tried in order per store.'),
          textInput('pattern', 'Store name pattern(s)', value = '',
                    placeholder = 'e.g. *grdz*, *tile?, or one per line'),
          helpText('Glob(s) matched against the store path, so * and ? are wildcards and ',
                   'a name on its own matches anywhere in the path. Comma or newline ',
                   'separated, and all of them must match. They narrow what the Index, ',
                   'Frames and Store summary tabs list, as well as what a request will ',
                   'cut, so leave them blank to work with the whole index.'),
          numericInput('max_dirs', 'Max directories to walk', value = 1000, min = 1,
                       step = 100),
          checkboxInput('rebuild', 'Rebuild the index from the tiles', FALSE),
          actionButton('load', 'Load index', class = 'btn-primary w-100',
                       icon = icon('play'))
        )
      ),
      card(
        card_header(icon('circle-info'), ' Session'),
        card_body(
          uiOutput('store_status'),
          accordion(
            accordion_panel('Run log', icon = icon('list-check'),
                            verbatimTextOutput('log', placeholder = TRUE))
          )
        )
      )
    ),
    card(
      card_header(icon('database'), ' What is in the store'),
      card_body(uiOutput('store_summary'))
    )
  ),

  tab_panel(
    title = 'Cutouts', icon = icon('crop'), value = 'cutouts',
    #The controls are stacked down a narrow left column and the answers get the wide right
    #one, because a cutout is square and a row of full width panels wastes most of the
    #screen on either side of it
    layout_columns(col_widths = c(4, 8), fill = FALSE,
      div(
        card(
          #The button belongs with the header rather than at the bottom of a tall column of
          #options, so that the action is visible while the positions are being typed
          card_header(div(class = 'rf-head-row',
                          span(icon('crosshairs'), ' Request'),
                          actionButton('run', 'Get cutouts', icon = icon('search'),
                                       class = 'btn-primary btn-sm'))),
          card_body(
            textAreaInput('positions', 'RA, Dec (degrees)', rows = 8,
                          placeholder = 'One target per line:\n53.1234 -27.8123\n10.5 -45.2'),
            helpText('Separate the two numbers with a space or a comma. Lines starting ',
                     'with # are ignored, and anything that is not two finite numbers is ',
                     'reported rather than quietly dropped.'),
            actionButton('clear_pos', 'Clear list', icon = icon('eraser'),
                         class = 'btn-outline-secondary btn-sm'),
            hr(),
            fluidRow(
              column(6, numericInput('box', 'Box size', value = 101, min = 1, step = 10)),
              column(6, selectInput('box_unit', 'Box unit', choices = c('pix', 'arcsec'),
                                    selected = 'pix'))
            ),
            numericInput('buffer', 'Overlap buffer (arcsec)', value = 0, min = 0, step = 5),
            textInput('kw_key', 'Header keyword filter', placeholder = 'e.g. FILTER'),
            helpText('Only cut from stores whose recorded header carries this keyword. ',
                     'Costs an index decode, not a pixel read.'),
            textInput('kw_value', 'Keyword value (glob)', placeholder = 'e.g. *grz*'),
            helpText('Matched case insensitively against the keyword, * and ? being ',
                     'wildcards. Blank accepts any value the keyword takes.'),
            numericInput('max_stores', 'Max stores to cut per position', value = 25,
                         min = 1, step = 1),
            helpText('Caps how many stores are opened, and so how many pixel reads a ',
                     'request makes. How many could overlap is reported before the cap.')
          )
        ),
        card(
          card_header(icon('sliders'), ' Display'),
          card_body(
            helpText('Covers both the on screen plots and the downloaded JPEGs. The raw ',
                     'FITS download always carries the extracted pixels unchanged.'),
            fluidRow(
              column(4, checkboxInput('qdiff', 'qdiff', TRUE)),
              column(8, selectInput('type', 'Clipping type', choices = magmap_units,
                                    selected = 'quan'),
                     helpText('Ignored when qdiff is on, which clips at the symmetric ',
                              'maximum instead.'))
            ),
            fluidRow(
              column(4, numericInput('locut', 'locut', value = NA, step = 0.1)),
              column(4, numericInput('hicut', 'hicut', value = NA, step = 0.1)),
              column(4, selectInput('stretch', 'stretch',
                                    choices = c('lin', 'asinh', 'atan', 'log', 'sqrt', 'cdf'),
                                    selected = 'asinh'))
            ),
            fluidRow(
              column(6, numericInput('slice', 'Slice', value = 1, min = 1, step = 1),
                     div(class = 'rf-meta', 'Plane of a cube or array cutout.')),
              column(6, numericInput('cut_width', 'Cutout width (px)', value = 720,
                                     min = 160, step = 20),
                     div(class = 'rf-meta', 'Side of each square panel, and the pixel ',
                         'size of a downloaded JPEG.'))
            ),
            fluidRow(
              column(6, checkboxInput('useraster', 'useRaster', TRUE)),
              column(6, numericInput('max_show', 'Max cutouts to plot', value = 24,
                                     min = 1, step = 1),
                     div(class = 'rf-meta', 'Results beyond this are still listed and ',
                         'downloadable, just not drawn.'))
            )
          )
        ),
        card(
          card_header(icon('file-zipper'), ' Download'),
          card_body(
            helpText('Only the included cutouts are bundled, and nothing is re-cut: the ',
                     'pixels are those already in memory, however they were chosen. Raw ',
                     'FITS is the extracted data as it stands; tile compressed may drop ',
                     'precision unless the quantization level is 0.'),
            radioButtons('fits_format', 'FITS files',
                         choices = c('Raw (uncompressed)' = 'raw',
                                     'Tile compressed' = 'compress'),
                         selected = 'raw'),
            #Rendered rather than merely shown or hidden. cfitsio only reads the options
            #after [compress when it is asked to compress at all, so controls that can be
            #silently ignored are worse than controls that are not there
            uiOutput('comp_opts_ui'),
            hr(),
            numericInput('jpeg_quality', 'JPEG quality', value = 75, min = 1, max = 100,
                         step = 5),
            div(class = 'rf-meta', 'Given to grDevices::jpeg, so it affects the downloaded ',
                'JPEGs only, not the plots on screen. The R default is 75; 90 and above is ',
                'close to lossless for these images.')
          )
        )
      ),
      div(
        card(
          card_header(icon('table'), ' Results', uiOutput('result_counts')),
          card_body(
            uiOutput('index_note'),
            #One selection for the whole tab: the buttons below, the tick beside each
            #plotted cutout, and the results table's row checkboxes all read and write the
            #same logical vector
            uiOutput('select_bar'),
            div(class = 'd-flex gap-2 mb-2',
                downloadButton('dl_fits', 'Download selected as FITS (zip)',
                               icon = icon('file-zipper'), class = 'btn-outline-primary'),
                downloadButton('dl_jpeg', 'Download selected as JPEG (zip)',
                               icon = icon('file-zipper'), class = 'btn-outline-success')),
            helpText('FITS files are written from the extracted pixels and header with ',
                     'Rfits_write_image, so each keeps the WCS of the tile it came from. ',
                     'JPEGs are re-rendered with the options above.'),
            if(has_dt){
              DT::dataTableOutput('match_table')
            }else{
              tableOutput('match_table')
            }
          )
        ),
        card(
          card_header(icon('images'), ' Cutouts'),
          card_body(uiOutput('gallery'))
        )
      )
    )
  ),

  tab_panel(
    title = 'Index', icon = icon('table-list'), value = 'index',
    layout_columns(col_widths = c(3, 9), fill = FALSE,
      card(
        card_header(icon('magnifying-glass'), ' Filter'),
        card_body(
          textInput('idx_filter', 'Store name glob', value = ''),
          selectInput('idx_status', 'Status', choices = 'all'),
          actionButton('idx_refresh', 'Reload rows', class = 'btn-outline-primary',
                       icon = icon('arrows-rotate')),
          hr(),
          verbatimTextOutput('idx_info', placeholder = TRUE)
        )
      ),
      card(
        card_header(' index rows'),
        card_body(
          helpText('What the search reads: one row per store per extension, holding the ',
                   'reference position, shape and pixel scale the overlap test works from. ',
                   'Keyword blobs are hidden here but are what the header filter uses.'),
          #Height is given in pixels rather than left to 'auto'. A DataTable inside a
          #bslib card has no intrinsic height of its own, and with auto it collapses to
          #the size of the page, which is what makes the horizontal scrollbar unreachable
          if(has_dt){
            DT::dataTableOutput('index_table', height = '620px')
          }else{
            tableOutput('index_table')
          }
        )
      )
    )
  ),

  tab_panel(
    title = 'Frames', icon = icon('map'), value = 'frames',
    layout_columns(col_widths = c(9, 3), fill = FALSE,
      card(
        card_header(icon('vector-square'), ' Available frames'),
        card_body(
          #Interactive rather than a static plot because the whole point of this panel is
          #finding a position inside a crowded release: you zoom on the tiles that matter
          #and click the one you want, which is not something a dragged box can do.
          #Drawn from the index, so no pixel is read to place a box
          helpText('Click anywhere to pick that exact RA/Dec, as often as you like and at ',
                   'any zoom; the view does not move while you do it. To take whole tiles, ',
                   'use the Select box or lasso tool in the mode bar, which highlights the ',
                   'frames caught and sends their centres. Scroll to zoom or use the zoom ',
                   'tool; double click resets. Hovering a frame gives its store, extension ',
                   'and shape. Boxes are footprints worked out from the index, so they are ',
                   'axis aligned and ignore rotation. Filters on the Index tab apply here ',
                   'too.'),
          plotly::plotlyOutput('frames', height = '620px'),
          verbatimTextOutput('frames_info', placeholder = TRUE)
        )
      ),
      card(
        card_header(' Selection'),
        card_body(
          actionButton('send_pos', 'Send clicked positions', class = 'btn-primary w-100',
                       icon = icon('crop')),
          div(style = 'height: 0.4rem;'),
          actionButton('send_sel', 'Send selected frames', class = 'btn-outline-primary w-100',
                       icon = icon('object-group')),
          div(style = 'height: 0.4rem;'),
          actionButton('clear_picks', 'Clear clicked', class = 'btn-outline-secondary w-100',
                       icon = icon('eraser')),
          div(style = 'height: 0.8rem;'),
          if(has_dt){
            DT::dataTableOutput('frames_table', height = '420px')
          }else{
            tableOutput('frames_table')
          }
        )
      )
    )
  )
)

server = function(input, output, session){

  state = reactiveValues(con = NULL, idx = NULL, rows = NULL, log = character(0),
                         res = NULL, matches = NULL, sel = NULL, run = 0L)

  note = function(...){
    txt = paste0(..., collapse = '')
    if(!nzchar(txt)){
      return(invisible(NULL))
    }
    state$log = c(state$log, paste0(format(Sys.time(), '%H:%M:%S'), '  ', txt))
    #Keep the tail only. A rebuild of a large release can emit a line per store, and the
    #oldest of those is the least useful thing in the box
    if(length(state$log) > 500){
      state$log = tail(state$log, 500)
    }
    invisible(NULL)
  }

  blank = function(v) if(is.null(v) || !nzchar(trimws(v))) NULL else trimws(v)

  #An input that has not arrived yet is NULL rather than '', and NULL then breaks the
  #checks it is used in: nzchar(NULL) and is.na(NULL) are logical(0), which makes if() and
  #|| fail with 'argument is of length zero', and an empty string reaching checkmate as
  #character(0) fails assertString. This happens in practice as well as in tests, since a
  #handler can run before every control has reported. Reading a control through in_()
  #gives the empty value its type would have, so a missing value means the same as a
  #blank one and no handler has to remember the difference
  in_ = function(id, default = ''){
    v = input[[id]]
    if(is.null(v)){
      return(default)
    }
    return(v)
  }

  #Numeric controls, whose empty value is NA rather than ''. A control the user cleared
  #arrives as NA on its own, so the two cases are the same to the caller
  num_ = function(id, default = NA_real_){
    v = suppressWarnings(as.numeric(in_(id, default)))[1]
    if(is.null(v)){
      return(default)
    }
    return(v)
  }

  #A checkbox, whose absent value is FALSE
  flag_ = function(id){
    return(isTRUE(in_(id, FALSE)))
  }

  #Secrets are read from the environment when the field is left blank, which is how a
  #scripted run and this app come by the same credentials without them being typed into a
  #form. They are kept in a plain list in a reactiveValues: never rendered, never logged,
  #and a reactiveValue invalidates nothing that does not read it, so typing a key does not
  #re-run the observers that walk the inputs
  collect_creds = function(){
    one = function(field, name){
      v = blank(in_(field))
      if(!is.null(v)){
        return(v)
      }
      v = Sys.getenv(name, '')
      if(nzchar(v)) v else NULL
    }
    return(list(region = one('region', 'RFITS_S3_REGION'),
                endpoint = one('endpoint', 'RFITS_S3_ENDPOINT'),
                access_key = one('access_key', 'RFITS_S3_ACCESS_KEY'),
                secret_key = one('secret_key', 'RFITS_S3_SECRET_KEY'),
                session_token = one('session_token', 'RFITS_S3_SESSION_TOKEN')))
  }

  collect_con = function(){
    #Store name patterns are globs rather than regular expressions: the field holds a name
    #or a path, and *.zarr or tile* is what someone looking at a release will type. They
    #are converted once, here, into the regular expressions that the index builder and the
    #re-filter in do_run both match with, so the two cannot disagree about what a pattern
    #means. It also means the walk key of a built index is the conversion, not the typing
    patterns = trimws(unlist(strsplit(gsub('\\t', ' ', in_('pattern')), '[\r\n,]+')))
    patterns = patterns[nzchar(patterns)]
    patterns = glob_regex(patterns)
    ext = trimws(unlist(strsplit(in_('extname', 'data1'), '[,\r\n ]+')))
    ext = ext[nzchar(ext)]
    dir = blank(in_('dir'))
    bucket = blank(in_('bucket'))
    if(!is.null(bucket) && !is.null(dir)){
      stop('Give a bucket or a local directory, not both.', call. = FALSE)
    }
    if(is.null(bucket) && is.null(dir)){
      stop('Give a bucket (with credentials) or a local directory of stores.',
           call. = FALSE)
    }
    index_name = in_('index_name', 'index.parquet')
    if(!nzchar(index_name)){
      index_name = 'index.parquet'
    }
    #The remote and local defaults differ for a reason. Over S3 a bare name goes to the
    #bucket root, which is the published location this app assumes, and a name carrying a
    #separator is the key itself. Locally, any relative name is resolved inside the store
    #directory: a relative path would otherwise be read against whatever working directory
    #the app happened to be started from, which is neither of the two places a user could
    #mean by it
    index_prefix = NULL
    if(is.null(bucket)){
      if(!grepl('^(/|~|[A-Za-z]:)', index_name)){
        index_name = file.path(dir, index_name)
      }
    }
    max_dirs = num_('max_dirs', 1000)
    if(!is.finite(max_dirs) || max_dirs < 1){
      max_dirs = 1000
    }
    return(list(bucket = bucket, dir = dir, prefix = trimws(in_('prefix')),
                index_name = index_name, index_prefix = index_prefix,
                pattern = if(length(patterns) > 0) patterns else NULL,
                extname = if(length(ext) > 0) ext else 'data1',
                max_dirs = as.integer(max_dirs),
                creds = collect_creds()))
  }

  output$log = renderText(paste(state$log, collapse = '\n'))

  output$store_status = renderUI({
    idx = state$idx
    if(is.null(idx)){
      return(em('No index loaded yet. Describe a store and press Load index.'))
    }
    con = state$con
    tags$ul(
      tags$li(strong('Stores: '), if(is.null(con$bucket)){
        paste0('local ', con$dir)
      }else{
        paste0('s3://', con$bucket, '/', con$prefix)
      }),
      tags$li(strong('Index: '), idx$shown,
              if(isTRUE(idx$built)) ' (built for this session)' else ''),
      tags$li(strong('Rows: '), nrow(idx$rows),
              if(length(con$pattern) > 0) ' (store name pattern applied)')
    )
  })

  output$store_summary = renderUI({
    rows = state$rows
    req(rows)
    #An index whose every row the name pattern excluded is an ordinary state, and range() on
    #no rows warns rather than answering, so it is said in words instead
    if(nrow(rows) == 0){
      return(em('No store in the index matches the store name pattern.'))
    }
    ok = rows$status == 'ok'
    exts = sort(unique(rows$extname[!is.na(rows$extname)]))
    typ = table(rows$type[ok])
    st = table(rows$status)
    tagList(
      fluidRow(
        column(3, value_box(title = 'Stores', value = length(unique(rows$label)),
                            subtitle = 'unique paths')),
        column(3, value_box(title = 'Searchable rows', value = sum(ok),
                            subtitle = paste0('of ', nrow(rows)))),
        column(3, value_box(title = 'RA span (deg)',
                            value = paste(round(range(rows$ra[ok], na.rm = TRUE), 3),
                                          collapse = '\n to '))),
        column(3, value_box(title = 'Dec span (deg)',
                            value = paste(round(range(rows$dec[ok], na.rm = TRUE), 3),
                                          collapse = '\n to ')))
      ),
      strong('Extensions: '), paste(exts, collapse = ', '), br(),
      strong('Array types: '), if(length(typ) > 0)
        paste(names(typ), typ, sep = ' = ', collapse = ', ') else 'none', br(),
      strong('Statuses: '), paste(names(st), st, sep = ' = ', collapse = ', '),
      if(any(!ok)){
        tagList(br(), tags$small('Rows that are not searchable are kept in the index so ',
                                 'the next run does not re-read them. The error column on ',
                                 'the Index tab says why.'))
      }else{
        NULL
      }
    )
  })

  #An index the app downloaded or built lives in the temporary directory and goes with the
  #session, rather than accumulating one parquet file per page reload
  session$onSessionEnded(function(){
    idx = isolate(state$idx)
    if(!is.null(idx) && isTRUE(idx$owned) && !is.null(idx$path)){
      unlink(idx$path)
    }
  })

  adopt_index = function(got){
    if(!is.null(state$idx) && isTRUE(state$idx$owned) && !is.null(state$idx$path)){
      unlink(state$idx$path)
    }
    #Every one of these rows is shown somewhere: the Index and Frames tabs and the Store
    #summary all read state$rows, and the whole point of the name pattern is to say which
    #stores a session is working with. A published index is the whole release, so a pattern
    #that is not applied here would leave every tab listing stores a cutout would refuse to
    #touch -- the search narrowing correctly while the tabs contradicted it. The narrowing is
    #display only: the index file is queried whole, and do_run applies the same helper to the
    #candidates it finds there
    full = as.data.frame(got$rows, stringsAsFactors = FALSE)
    rows = pattern_rows(full, state$con$pattern)
    got$rows = rows
    state$idx = got
    state$rows = rows
    updateSelectInput(session, 'idx_status', choices = c('all', sort(unique(rows$status))))
    note('Index ready: ', nrow(rows), ' row(s), ', length(unique(rows$label)),
         ' store(s), extname(s) ',
         paste(sort(unique(rows$extname)), collapse = ', '))
    if(length(state$con$pattern) > 0){
      note('Name pattern kept ', nrow(rows), ' of ', nrow(full), ' index row(s)')
    }
    invisible(NULL)
  }

  observeEvent(input$load, {
    con = tryCatch(collect_con(), error = function(e) e)
    if(inherits(con, 'error')){
      note('ERROR: ', conditionMessage(con))
      return(NULL)
    }
    if(!is.null(con$bucket) && is.null(con$creds$access_key)){
      #Without a key paws falls back to anonymous credentials, which can read a public
      #bucket but reports AccessDenied for most, and the failure then looks as though the
      #bucket name is wrong rather than the credentials being absent
      note('WARNING: no access key given and RFITS_S3_ACCESS_KEY is unset, so the ',
           'request will be made anonymously.')
    }
    state$con = con
    #Progress takes no message in its constructor; the text is carried by every set() call
    prog = Progress$new(session, min = 0, max = 1)
    on.exit(prog$close(), add = TRUE)
    prog$set(0.1, message = if(flag_('rebuild')) 'Building index' else 'Reading index',
             detail = 'contacting store')
    got = load_index(con, rebuild = flag_('rebuild'), note = note)
    if(!is.null(got$error)){
      note('ERROR: ', got$error)
      showNotification(got$error, type = 'error')
      return(NULL)
    }
    prog$set(1)
    adopt_index(got)
    bslib::nav_select('nav', 'cutouts')
  })

  observeEvent(input$clear_pos, {
    updateTextAreaInput(session, 'positions', value = '')
  })

  #The display controls as one snapshot. Reactive rather than captured with each request
  #so that changing a stretch redraws the plots from the pixels already in memory, which
  #costs nothing, rather than implying the cutouts have to be fetched again
  display_opts = reactive({
    list(qdiff = flag_('qdiff'), type = in_('type', 'quan'),
         stretch = in_('stretch', 'asinh'), locut = num_('locut'), hicut = num_('hicut'),
         useraster = flag_('useraster'), slice = num_('slice', 1),
         width = cut_width(), max_show = max_show())
  })

  #Side of a square cutout panel, in pixels. Used for both the on screen cell and the
  #downloaded JPEG so what a user sees is what they get
  cut_width = reactive({
    w = num_('cut_width', 720)
    if(is.na(w) || w < 160) 720 else min(w, 1200)
  })

  #Only this many of the results are drawn. A request that overlaps a thousand tiles would
  #otherwise bind a thousand plot outputs, each of which the browser has to fetch and
  #decode, and the page becomes unusable rather than merely slow. Everything is still
  #listed in the match table and still goes into the downloads
  max_show = reactive({
    n = num_('max_show', 24)
    if(is.na(n) || n < 1) 24 else as.integer(min(n, 500))
  })

  draw_cutout = function(item, opts){
    if(is.null(item) || is.null(item$imDat)){
      plot.new()
      text(0.5, 0.5, 'No pixel data was extracted for this cutout.', xpd = NA)
      return(invisible(NULL))
    }
    extra = list(qdiff = opts$qdiff, useRaster = opts$useraster, main = '')
    #With qdiff on, magimage sets type = 'num' itself and derives locut from hicut, so
    #passing a quantile through would either error or fight the diverging scale
    if(!opts$qdiff){
      extra$type = opts$type
      extra$stretch = opts$stretch
    }
    if(is.finite(opts$locut)){
      extra$locut = opts$locut
    }
    if(is.finite(opts$hicut)){
      extra$hicut = opts$hicut
    }
    ndim = length(dim(item$imDat))
    if(ndim >= 3){
      nmax = dim(item$imDat)[3]
      sl = max(1L, min(as.integer(if(is.na(opts$slice)) 1 else opts$slice), nmax))
      extra$slice = if(ndim == 3) sl else c(sl, sl)
    }
    do.call(plot, c(list(x = item), extra))
    invisible(NULL)
  }

  #Compression keys a tile-compressed store carries in its header. A cutout inherits them
  #from the tile it came from, but only the compressed download is allowed to keep them:
  #written raw, a plain image that says ZIMAGE = TRUE in its primary header is a file that
  #tells a reader to expect a binary table it does not contain, so they are removed.
  #ZNAXIS is built out rather than listed because it comes with one key per axis, and a
  #header from a cube store carries ZNAXIS1 through ZNAXIS3
  cfitsio_comp_keys = c('ZIMAGE', 'ZBITPIX', 'ZQUANTIZ', 'ZNBDTP', 'ZDITHER0', 'ZTILELEN',
                        'ZNAXIS', paste0('ZNAXIS', 1:6), 'ZEXPSC', 'ZEXPZERO', 'ZNOFFSC',
                        'ZNOFFZERO', 'ZBLANKFIX', 'ZVAL1', 'ZVAL2', 'ZSCALE', 'ZZERO')

  #The pixels of a cutout are the pixels of its header, and the header may not have been
  #the source of either. .zarr_cutout_box_keyvalues rewrites NAXIS to the box size, which
  #for a cube or array is the size of one plane rather than of the data in hand, and a raw
  #write of a tile-compressed cutout has had its ZNAXIS keys dropped above. Both are
  #restored here from the data, which is the only thing that can be authoritative.
  fix_shape_keys = function(item){
    dm = dim(item$imDat)
    if(is.null(dm) || length(dm) == 0){
      return(item)
    }
    if(is.null(item$keyvalues)){
      return(item)
    }
    kv = item$keyvalues
    kv$NAXIS = length(dm)
    for(i in seq_along(dm)){
      kv[[paste0('NAXIS', i)]] = dm[i]
    }
    item$keyvalues = kv
    return(item)
  }

  #Download options, read outside a reactive context by the handlers below. Kept as one
  #function so the FITS and JPEG paths agree on what 'selected' and 'compressed' mean, and
  #so a control that has not reported yet falls back to the value the UI shipped with
  dl_opts = function(){
    q = suppressWarnings(as.numeric(in_('jpeg_quality', 75)))[1]
    if(is.na(q)){
      q = 75
    }
    q = max(1, min(100, round(q)))
    return(list(fits_format = in_('fits_format', 'raw'), alg = in_('fits_alg', 'RICE'),
                tile = in_('fits_tile', ''), quant = num_('fits_quant'),
                jpeg_quality = q))
  }

  #The cfitsio compression spec, as the text between '[compress ' and ']'. The parser in
  #cfileio.c reads the algorithm from its first letter, then the tile dimensions, then
  #parameters after a semicolon, so the pieces are joined in that order.
  #
  #The tile sizes are tidied here rather than where they are typed because this is the
  #only place that knows they are going into a string cfitsio parses: the box happily
  #takes '64, 64' or '64 64' from a user, and either would have to arrive as '64,64'
  comp_spec = function(o){
    tile = gsub('[ ,]+', ',', gsub('[^0-9, ]', '', trimws(o$tile)))
    tile = gsub('^,+|,+$', '', tile)
    spec = o$alg
    if(nzchar(tile)){
      spec = paste0(spec, ' ', tile)
    }
    if(is.finite(o$quant)){
      spec = paste0(spec, '; q ', format(o$quant, scientific = FALSE, trim = TRUE))
    }
    return(spec)
  }

  #Which results go in a bundle. Everything by default, since the common case is 'I want
  #this whole request', and the point of the controls below is that taking a subset is
  #easier than naming one. A selection of the wrong length is treated as no selection at
  #all rather than truncated, because a run that changed the number of results has to
  #re-start from 'everything' for the count on screen to mean anything
  sel_of = function(res, sel){
    if(is.null(res) || length(res) == 0){
      return(logical(0))
    }
    if(is.null(sel) || length(sel) != length(res)){
      return(rep(TRUE, length(res)))
    }
    as.logical(sel)
  }

  #The count beside the buttons, as a reactive of its own. selected_idx() isolates because
  #the download handlers call it from a non-reactive context, and an isolating reader is
  #exactly the wrong thing to depend on for a number that has to move when the user ticks
  #a box
  n_selected = reactive({
    sum(sel_of(state$res, state$sel))
  })

  selected_idx = function(){
    which(sel_of(isolate(state$res), isolate(state$sel)))
  }

  #The cells that have a tick box beside them, which is the plotted prefix of the results
  #rather than all of them. Everything that reads or writes the per cutout inputs goes
  #through here so that the gallery and the observers cannot disagree about how many boxes
  #are on the page
  sel_ids = function(){
    res = isolate(state$res)
    if(is.null(res)){
      return(integer(0))
    }
    seq_len(min(length(res), max_show()))
  }

  #Input ids carry the request that made them. A box is only ever read once every box of
  #the current run has reported, and numbering them per run is what makes that wait mean
  #the current run rather than a leftover from the last one: results shrink between
  #requests as often as they grow, and a stale sel_17 reporting TRUE after a request that
  #returned five cutouts would otherwise be read as a tick on the wrong cutout
  cell_id = function(i){
    paste0('sel_', isolate(state$run), '_', i)
  }

  #The same list, cut to the selection. Everything downstream writes in terms of a result
  #list, so subsetting once here keeps the gallery, the count and the two bundles from each
  #having to work out the selection their own way
  selected_res = function(){
    res = state$res
    idx = selected_idx()
    if(is.null(res) || length(idx) == 0){
      return(NULL)
    }
    res[idx]
  }

  #A new result list always arrives with everything included, which is what the previous
  #request's tick marks would otherwise silently contradict
  observeEvent(state$res, {
    res = state$res
    state$sel = if(is.null(res) || length(res) == 0){
      NULL
    }else{
      rep(TRUE, length(res))
    }
  })

  #Both buttons go through the same three lines, so 'none' and 'invert' cannot disagree
  #about what a selection of the wrong length means
  set_sel = function(fn){
    sel = sel_of(state$res, state$sel)
    if(length(sel) == 0){
      return(invisible(NULL))
    }
    state$sel = fn(sel)
  }

  observeEvent(input$sel_all, {
    set_sel(function(sel) rep(TRUE, length(sel)))
  })

  observeEvent(input$sel_none, {
    set_sel(function(sel) rep(FALSE, length(sel)))
  })

  observeEvent(input$sel_invert, {
    set_sel(function(sel) !sel)
  })

  #One change of one tick in the results table, which is all the page can report: with
  #paging, the rows on the other pages are not in the document, so a tick cannot arrive as
  #the full set of what is included. The index is the position in the result list rather
  #than in the table, so the sort and the search cannot move it
  observeEvent(input$match_tick, {
    ev = input$match_tick
    res = state$res
    if(is.null(res) || is.null(ev) || is.null(ev$i)){
      return(invisible(NULL))
    }
    i = as.integer(ev$i)
    if(length(i) != 1 || is.na(i) || i < 1 || i > length(res)){
      return(invisible(NULL))
    }
    sel = sel_of(res, state$sel)
    on = isTRUE(ev$on)
    if(isTRUE(sel[i] == on)){
      return(invisible(NULL))
    }
    sel[i] = on
    state$sel = sel
  })

  #The boxes as the page reports them, or NULL while any of them is missing. A cell that
  #has not been drawn yet arrives as NULL, and reading a partial set would clear the
  #results that are included but not currently on screen; the cells render together, so
  #the wait is a frame rather than anything the user can see.
  #
  #Deliberately a reactive of its own rather than an observer that reads the selection as
  #well as the inputs: such an observer is re-run by its own write, and would put the
  #stale ticks back over it, so clicking Include none would immediately undo itself
  cell_ticks = reactive({
    res = state$res
    req(res)
    n = length(sel_ids())
    if(n == 0){
      return(NULL)
    }
    got = lapply(seq_len(n), function(i) input[[cell_id(i)]])
    if(any(vapply(got, is.null, logical(1)))){
      return(NULL)
    }
    vapply(got, isTRUE, logical(1))
  })

  observeEvent(cell_ticks(), {
    res = state$res
    req(res)
    ticked = cell_ticks()
    n = length(ticked)
    sel = sel_of(res, state$sel)
    if(identical(sel[seq_len(n)], ticked)){
      return(invisible(NULL))
    }
    sel[seq_len(n)] = ticked
    state$sel = sel
  })

  #Pushed back to the page so that the buttons, the table and the ticks cannot disagree
  #about what is included: the state is the only truth, and the boxes follow it. Only the
  #plotted cells have a box to update; a cutout that is included but not drawn stays
  #included, which is what the count beside the buttons says.
  #
  #req() takes the length check rather than the vector because it treats an all FALSE
  #selection as falsy, which is precisely the state 'Include none' produces: the push was
  #aborted, the boxes on the page kept their old ticks, and they then reported those ticks
  #back as the user's choice and undid it
  observeEvent(state$sel, {
    res = state$res
    req(res)
    sel = state$sel
    if(is.null(sel) || length(sel) != length(res)){
      return(invisible(NULL))
    }
    for(i in sel_ids()){
      updateCheckboxInput(session, cell_id(i), value = isTRUE(sel[i]))
    }
    if(has_dt){
      #The table's boxes are drawn from data and cannot be updated by Shiny, so they are
      #ticked in place. Redrawing the table instead would throw away the search, the sort
      #and the page the user is on, which is the only reason the table has a search box
      session$sendCustomMessage('rf_set_ticks', list(i = which(sel)))
    }
  })

  output$select_bar = renderUI({
    res = state$res
    if(is.null(res) || length(res) == 0){
      return(helpText('Run a request to choose which cutouts to download.'))
    }
    div(class = 'rf-sel-row mb-2',
        actionButton('sel_all', 'Include all', icon = icon('check'),
                     class = 'btn-outline-primary btn-sm'),
        actionButton('sel_none', 'Include none', icon = icon('xmark'),
                     class = 'btn-outline-secondary btn-sm'),
        actionButton('sel_invert', 'Invert', icon = icon('arrows-rotate'),
                     class = 'btn-outline-secondary btn-sm'),
        uiOutput('sel_count'))
  })

  output$sel_count = renderUI({
    res = state$res
    req(res)
    helpText(class = 'ms-auto', n_selected(), ' of ', length(res),
             ' included in the download.')
  })

  #The compression controls only exist while something is being compressed. They are drawn
  #in place rather than greyed out because cfitsio reads the options after [compress only
  #when it is asked to compress, so visible-but-ignored controls would promise a quantizer
  #that a raw download quietly drops
  output$comp_opts_ui = renderUI({
    if(!identical(in_('fits_format', 'raw'), 'compress')){
      return(NULL)
    }
    tagList(
      fluidRow(
        column(6, selectInput('fits_alg', 'Algorithm',
                              choices = c('RICE' = 'RICE', 'GZIP' = 'GZIP',
                                          'HCOMPRESS' = 'HCOMPRESS', 'PLIO' = 'PLIO'),
                              selected = 'RICE')),
        column(6, textInput('fits_tile', 'Tile size (px)', value = '',
                            placeholder = 'e.g. 64,64'))
      ),
      div(class = 'rf-meta', 'Blank tile size leaves cfitsio on its default, which is one ',
          'row per tile. PLIO is integer only and will fail on a floating point cutout.'),
      numericInput('fits_quant', 'Quantization level', value = NA, step = 1),
      #Blank is not the same as zero, and the difference is whether the pixels come back
      #unchanged: cfitsio quantizes by default, so a plain RICE download is a lossy one
      helpText('How much precision the compression is allowed to drop. A positive value ',
               'gives the number of quantization bins per pixel of noise, and cfitsio ',
               'defaults to 4; a negative value is an absolute bin size in data units ',
               '(e.g. -0.0002); 0 is lossless, and GZIP at 0 gives the pixels back to ',
               'single precision. Blank leaves cfitsio on its own lossy default.'),
      helpText('A compressed file is a tile-compressed image held in a binary table, so ',
               'read it back with ext = 2.')
    )
  })

  #Returns the list of written files, or NULL. The directory is removed by the caller
  #after zipping, not here: bundling and archiving are two steps and the files have to
  #survive the gap between them
  write_all = function(ext, fun){
    #The selection is read through a reactive so that this stays the only place the two
    #download paths decide what is in a bundle
    res = selected_res()
    if(is.null(res) || length(res) == 0){
      #Logged as well as flashed, because the notification is gone in a few seconds and a
      #user who has clicked Include none and then the download button needs to be able to
      #see that those are two separate facts rather than a lost result
      showNotification('No cutouts are selected for download.', type = 'warning')
      note('No cutouts are selected for download.')
      return(NULL)
    }
    if(!requireNamespace('zip', quietly = TRUE)){
      showNotification('The zip package is needed to bundle downloads.', type = 'error')
      note('ERROR: the zip package is needed to bundle downloads.')
      return(NULL)
    }
    dir = tempfile('rfits_dl_')
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    wrote = character(0)
    prog = Progress$new(session, min = 0, max = length(res))
    on.exit(prog$close(), add = TRUE)
    for(i in seq_along(res)){
      prog$set(i, message = 'Bundling', detail = paste0(names(res)[i], '.', ext))
      out = file.path(dir, paste0(safe_filename(names(res)[i]), '.', ext))
      one = with_capture(tryCatch({
        fun(res[[i]], out)
        TRUE
      }, error = function(e) e))
      for(m in one$msgs) note(m)
      if(isTRUE(one$value) && file.exists(out)){
        wrote = c(wrote, out)
      }else{
        note('WARNING: could not write ', basename(out))
      }
    }
    if(length(wrote) == 0){
      showNotification('Nothing could be written.', type = 'error')
      unlink(dir, recursive = TRUE)
      return(NULL)
    }
    note('Wrote ', length(wrote), ' ', ext, ' file(s).')
    return(list(dir = dir, files = basename(wrote)))
  }

  #A downloaded FITS must say what it is. The header a cutout carries is the header of the
  #store it came from, and a tile-compressed store records its shape in ZNAXIS rather than
  #NAXIS and sets ZIMAGE; Rfits_write_image honours that flag and writes the keys rather
  #than the pixels they describe, which gives a raw file no one can open. Stripping them is
  #what makes 'download as raw' mean the same thing for either kind of source. Compressed
  #downloads keep them, since cfitsio rewrites the Z* keys for the tiles it actually makes
  #and a stale ZNAXIS1 in a file it is being asked to compress is not a description of the
  #file it will end up with either
  prepare_fits_item = function(item, o){
    item = fix_shape_keys(item)
    if(identical(o$fits_format, 'compress')){
      return(item)
    }
    drop = intersect(cfitsio_comp_keys, names(item$keyvalues))
    if(length(drop) > 0){
      item$keyvalues[drop] = NULL
      item$keycomments[drop] = NULL
      item$keynames = names(item$keyvalues)
      #The card image is rebuilt from the keywords when present, and a stale copy would
      #carry the removed keys straight back into the file
      item$header = NULL
      item$hdr = NULL
      item$raw = NULL
    }
    return(item)
  }

  output$dl_fits = downloadHandler(
    #The name says what is in the zip, since a compressed download is not interchangeable
    #with a raw one and a user with two of these in their downloads folder needs to know
    filename = function(){
      o = isolate(dl_opts())
      if(identical(o$fits_format, 'compress')){
        return('rfits-cutouts-fits-compressed.zip')
      }
      return('rfits-cutouts-fits.zip')
    },
    content = function(file){
      #Rfits_write_image takes the object, so keyvalues and history come along with the
      #pixels and the written file is re-openable with the WCS intact
      o = isolate(dl_opts())
      compress = if(identical(o$fits_format, 'compress')) comp_spec(o) else FALSE
      got = write_all('fits', function(item, out){
        Rfits::Rfits_write_image(prepare_fits_item(item, o), filename = out,
                                 compress = compress)
      })
      if(is.null(got)){
        return(invisible(NULL))
      }
      on.exit(unlink(got$dir, recursive = TRUE), add = TRUE)
      zip::zip(zipfile = file, files = got$files, root = normalizePath(got$dir))
    },
    contentType = 'application/zip'
  )

  #The arguments that open a JPEG device. cairo is the implementation that obeys a
  #quality setting: the macOS quartz device, which is this platform's default, takes the
  #argument and ignores it, so a quality of 20 would quietly produce the same file as 95.
  #With no cairo at all the argument is left off rather than passed as NULL, which
  #grDevices::jpeg hands to match.arg, and the log says so instead of the page pretending
  #the control did something
  jpeg_args = function(quality){
    if(!isTRUE(capabilities('cairo'))){
      note('WARNING: no cairo device, so the JPEG quality setting is ignored.')
      return(list())
    }
    return(list(quality = quality, type = 'cairo'))
  }

  output$dl_jpeg = downloadHandler(
    filename = function() 'rfits-cutouts-jpeg.zip',
    content = function(file){
      #The on screen plots are drawn into the browser's device, so they are re-rendered
      #here with the same arguments rather than captured from the page. A reactive cannot
      #be read outside a reactive context, and downloadHandler$content is not one, so the
      #options are isolated once here rather than inside the per file loop
      opts = isolate(display_opts())
      o = isolate(dl_opts())
      jpeg_args = isolate(jpeg_args(o$jpeg_quality))
      got = write_all('jpg', function(item, out){
        #Square, like the panel on screen. magimage draws into whatever device it is
        #given, so a wide device would buy a wider image rather than a bigger source
        #region, and the two would no longer look like each other
        w = as.integer(opts$width)
        #Quality is what the Download card asks for, and grDevices::jpeg takes it on the
        #open call rather than on close.
        #
        #res is derived from the side rather than fixed at 120 because jpeg() works out
        #its figure size in inches from the two, and magimage needs room for its axes: at
        #120 dpi a 200 px cutout is 1.7 inches square and the plot dies with 'figure
        #margins too large'. Six inches keeps the physical size the old constant gave at
        #the default width, and makes every requested width drawable.
        #
        #cairo is asked for by name because it is the implementation that honours the
        #quality argument. The macOS device takes it and ignores it, which would make the
        #control look like it works while every download came out the same size
        do.call(jpeg, c(list(filename = out, width = w, height = w, res = w/6),
                        jpeg_args))
        on.exit(tryCatch(dev.off(), error = function(e) NULL), add = TRUE)
        draw_cutout(item, opts)
      })
      if(is.null(got)){
        return(invisible(NULL))
      }
      on.exit(unlink(got$dir, recursive = TRUE), add = TRUE)
      zip::zip(zipfile = file, files = got$files, root = normalizePath(got$dir))
    },
    contentType = 'application/zip'
  )

  do_run = function(){
    req(state$idx)
    #Bumped first, so that the tick boxes of the run that is about to be drawn cannot be
    #confused with the ones still on the page
    state$run = state$run + 1L
    #Cleared before anything is asked for, so that what is on screen is always the answer
    #to the request that is in the boxes. A run that fails part way through used to leave
    #the previous results plotted under the new positions, which reads as an answer about
    #targets that were never searched
    state$res = NULL
    state$matches = NULL
    pos = parse_positions(in_('positions'))
    bad = attr(pos, 'bad')
    if(length(bad) > 0){
      note('WARNING: ', length(bad), ' line(s) ignored as positions: ',
           paste(bad, collapse = '; '))
    }
    if(nrow(pos) == 0){
      note('ERROR: no valid RA, Dec positions given.')
      showNotification('No valid RA, Dec positions to search.', type = 'warning')
      return(NULL)
    }
    con = state$con
    box_unit = in_('box_unit', 'pix')
    box = num_('box')
    if(!is.finite(box) || box < 1){
      note('ERROR: box size must be a number of at least 1.')
      return(NULL)
    }
    box = as.numeric(box)
    buffer = num_('buffer', 0)
    buffer = if(is.finite(buffer)) max(0, buffer) else 0
    max_stores = num_('max_stores', 25)
    if(!is.finite(max_stores) || max_stores < 1){
      max_stores = 25
    }
    max_stores = as.integer(max_stores)
    kw_key = trimws(in_('kw_key'))
    note('Searching ', nrow(pos), ' position(s), box ', box, ' ', box_unit,
         if(buffer > 0) paste0(', buffer ', buffer, ' arcsec') else '')

    #One arrow query per position, then the candidate sets are unioned. The query is the
    #cheap stage, a scan of a few hundred kB, and it has to be per position because the
    #radius within which a store can match depends on where the request is.
    cand = NULL
    for(k in seq_len(nrow(pos))){
      #keywords = FALSE always. The query returns the light rows, and a header filter that
      #is asked for later fetches the keyword blobs of the candidates only, which is a
      #much smaller set than the union of every position's candidates
      q = with_capture(Rfits::Rfits_zarr_index_query(
        index = state$idx$path, RA = pos[k, 1], Dec = pos[k, 2], box = box,
        box.unit = box_unit, buffer = buffer, keywords = FALSE,
        verbose = TRUE, data.table = FALSE))
      for(m in q$msgs) note(m)
      hit = q$value
      if(is.null(hit)){
        note('ERROR: the index query failed for position ', k, '.')
        return(NULL)
      }
      if(nrow(hit) > 0){
        cand = rbind(cand, as.data.frame(hit, stringsAsFactors = FALSE))
      }
    }
    if(is.null(cand)){
      note('No store in the index could overlap the requested position(s).')
      state$res = NULL
      state$matches = NULL
      return(NULL)
    }
    cand = cand[order(cand$label), , drop = FALSE]
    cand = cand[!duplicated(cand$cache_key), , drop = FALSE]
    note('Candidate store rows: ', nrow(cand))
    if(nzchar(kw_key)){
      cand = filter_by_keyword(cand, state$idx$path, kw_key, trimws(in_('kw_value')),
                               note = note)
      if(nrow(cand) == 0){
        note('ERROR: the keyword filter removed every candidate.')
        return(NULL)
      }
    }
    #The name pattern already narrowed the index rows that were read, but the file that was
    #queried is the whole published index, so it has to be applied to the candidates too.
    #Through the same helper as adopt_index, so what is cut and what the tabs show cannot
    #drift apart
    if(length(con$pattern) > 0){
      n_cand = nrow(cand)
      cand = pattern_rows(cand, con$pattern)
      note('Name pattern kept ', nrow(cand), ' of ', n_cand, ' candidate row(s)')
    }
    if(nrow(cand) == 0){
      note('ERROR: no candidate store survived the filters.')
      return(NULL)
    }
    cand$name = sub('\\.zarr$', '', basename(cand$store))
    if(nrow(cand) > max_stores){
      note('WARNING: ', nrow(cand), ' stores could overlap, but only the first ',
           max_stores, ' were cut; raise Max stores to cut the rest.')
      cand = cand[seq_len(max_stores), , drop = FALSE]
    }

    #Per store, rather than one call for the whole prefix. Over S3 a single call that
    #searched the prefix would re-read every tile's metadata, which is the cost the index
    #exists to remove, and giving it cache = would write what it learned back to the
    #bucket. The candidate list already is the answer, so each store is opened directly.
    out = list()
    names_out = character(0)
    shown = character(0)
    scans = NULL
    matches = NULL
    #Stores that were opened cleanly but hold nothing at the requested position. Without
    #this a release that scanned two tiles and cut one looks like a lost result, because
    #the second tile is a genuine non-overlap rather than a failure, and the only evidence
    #of it is a verbose line buried in the log
    no_overlap = character(0)
    errored = character(0)
    prog = Progress$new(session, min = 0, max = nrow(cand))
    on.exit(prog$close(), add = TRUE)
    for(i in seq_len(nrow(cand))){
      prog$set(i, message = 'Getting cutouts',
               detail = paste0(i, '/', nrow(cand), '  ', cand$name[i]))
      one = with_capture({
        if(is.null(con$bucket)){
          Rfits::Rfits_cutout_zarr_dir(dir = cand$store[i], RA = pos[, 1], Dec = pos[, 2],
                                       box = box, box.unit = box_unit,
                                       buffer = buffer, extname = con$extname,
                                       header = TRUE, extract = TRUE, verbose = TRUE)
        }else{
          Rfits::Rfits_cutout_zarr_dir(bucket = con$bucket, prefix = cand$store[i],
                                       RA = pos[, 1], Dec = pos[, 2], box = box,
                                       box.unit = box_unit, buffer = buffer,
                                       extname = con$extname, header = TRUE,
                                       extract = TRUE, verbose = TRUE,
                                       region = con$creds$region,
                                       endpoint = con$creds$endpoint,
                                       access_key = con$creds$access_key,
                                       secret_key = con$creds$secret_key,
                                       session_token = con$creds$session_token)
        }
      })
      for(m in one$msgs) note(m)
      r = one$value
      if(is.null(r)){
        errored = c(errored, cand$name[i])
        next
      }
      if(length(r) == 0){
        no_overlap = c(no_overlap, cand$name[i])
        next
      }

      nm = names(r)
      if(is.null(nm)){
        nm = paste0(cand$name[i], '_', seq_along(r))
      }
      #Rfits_cutout_zarr_dir already names each result after the store stub, suffixed with
      #the position when several were asked for, so the names are used as they arrive
      #rather than being prefixed a second time. Two stores in different sub directories
      #can still share a stub, and make.unique at the end separates those
      out[length(out) + seq_along(r)] = unname(r)
      names_out = c(names_out, nm)
      #carried alongside the bare store path because it is the form a user recognises:
      #s3://bucket/prefix remotely, the directory locally. Built in the same loop so it
      #stays aligned with the results by construction rather than by reconstruction
      shown = c(shown, rep(if(is.null(cand$label)) cand$store[i] else cand$label[i],
                           length(r)))
      s = attr(r, 'scan')
      if(!is.null(s)) scans = rbind(scans, s)
      mm = attr(r, 'matches')
      if(!is.null(mm)){
        mm = as.data.frame(mm, stringsAsFactors = FALSE)
        mm$store = cand$store[i]
        #Names are stamped here, while these rows are still in the same order as the
        #results they describe. Sorting the assembled table afterwards and then naming it
        #positionally would attach the wrong label to each row
        mm$name = nm
        matches = rbind(matches, mm)
      }
    }
    if(length(out) == 0){
      note('No cutout could be extracted from the candidate stores.')
      if(length(no_overlap) > 0){
        note('Scanned without finding an overlap at the requested position(s): ',
             paste(utils::head(no_overlap, 10), collapse = ', '),
             if(length(no_overlap) > 10) paste0(' (+', length(no_overlap) - 10, ' more)')
             else '')
        note('An overlap is judged on the tile footprint recorded in the index, so a store ',
             'whose centre is more than about half a tile away from a position will not ',
             'appear even when the two look close on the sky. Increase the box or the ',
             'overlap buffer to reach the next tile in.')
      }
      state$res = NULL
      state$matches = NULL
      return(NULL)
    }
    names(out) = make.unique(names_out, sep = '_')
    class(out) = 'Rfits_list'
    #The label is what identifies a store to a user (s3://bucket/prefix remotely, the
    #directory locally), while cand$store is the bare prefix the API wants back
    attr(out, 'filename') = shown
    attr(out, 'scan') = scans
    if(!is.null(matches)){
      #make.unique can rename a result when two stores share a stub, so the final names
      #are copied across while the rows are still in the order they were appended (one
      #match row per result, by construction of Rfits_cutout_zarr_dir), and only then is
      #the table sorted for display
      if(nrow(matches) == length(out)){
        matches$name = names(out)
      }
      matches = matches[order(matches$position, matches$store),
                        c('name', 'position', 'store', 'extname', 'RA', 'Dec', 'box_x',
                          'box_y', 'dim'), drop = FALSE]
    }
    state$res = out
    state$matches = matches
    note('Got ', length(out), ' cutout(s) from ',
         if(is.null(matches)) 0 else length(unique(matches$store)), ' store(s).')
    if(length(no_overlap) > 0){
      #Named explicitly, because "found two, showed one" is otherwise indistinguishable
      #from a bug in the app rather than a property of where the position fell
      note('Scanned ', length(no_overlap), ' candidate store(s) with no overlap at the ',
           'requested position(s): ',
           paste(utils::head(no_overlap, 10), collapse = ', '),
           if(length(no_overlap) > 10) paste0(' (+', length(no_overlap) - 10, ' more)')
           else '')
    }
    if(length(errored) > 0){
      note('Could not scan ', length(errored), ' candidate store(s): ',
           paste(utils::head(errored, 10), collapse = ', '))
    }
    invisible(NULL)
  }

  observeEvent(input$run, {
    do_run()
  })

  #One plot output per result, ids made from the names of the Rfits_list. Registering
  #them inside an observer means they appear and disappear with the request that made
  #them, rather than leaving outputs pointed at cutouts that no longer exist
  observe({
    res = state$res
    req(res)
    n = min(length(res), max_show())
    lapply(seq_len(n), function(i){
      local({
        ii = i
        output[[paste0('cutplot_', ii)]] = renderPlot({
          draw_cutout(state$res[[ii]], display_opts())
        })
      })
    })
  })

  output$gallery = renderUI({
    res = state$res
    if(is.null(res) || length(res) == 0){
      return(em('Run a request to see the cutouts here.'))
    }
    n_tot = length(res)
    n = min(n_tot, max_show())
    w = cut_width()
    #Heights are read from the inputs rather than through display_opts(), which would
    #rebuild every plotOutput whenever any display option changed and throw away the
    #plots that were just drawn
    cells = lapply(seq_len(n), function(i){
      item = res[[i]]
      kv = item$keyvalues
      #Only the keywords a browse actually wants to see, and only where the tile carries
      #them; a release written without FILTER simply does not show it. Character and
      #numeric keywords are formatted differently, so both are checked for usability
      bits = c(names(res)[i], paste(dim(item$imDat), collapse = ' x '))
      for(key in c('FILTER', 'TARGET', 'EXPTIME', 'PIXSCALE')){
        val = kv[[key]]
        if(is.null(val) || length(val) < 1 || all(is.na(val))){
          next
        }
        if(key %in% c('EXPTIME', 'PIXSCALE')){
          num = suppressWarnings(as.numeric(val)[1])
          if(!is.finite(num)){
            next
          }
          val = if(key == 'EXPTIME') paste0(round(num, 1), 's')
          else paste0(round(num, 4), '"/pix')
        }else{
          val = as.character(val)[1]
        }
        bits = c(bits, val)
      }
      div(
        class = 'rf-cut-cell',
        div(class = 'rf-sel-row',
            #The initial value is read from the selection rather than hard wired to TRUE,
            #so a gallery redrawn because the stretch changed cannot quietly reset which
            #cutouts are included. Isolated because the selection must not invalidate the
            #gallery that reports it. A selection of the wrong length means nothing has
            #been chosen yet, which is the same as everything being chosen
            {
              selected = isolate({
                s = state$sel
                if(is.null(s) || length(s) != n_tot) TRUE else isTRUE(s[i])
              })
              #No width argument: that appeared in shiny 1.6 and the app has to start
              #on whatever version the user has
              checkboxInput(cell_id(i), 'include', value = selected)
            },
            div(class = 'rf-name', paste(bits, collapse = '  |  '))),
        div(class = 'rf-meta', paste0(attr(res, 'filename')[i],
                                      if(!is.null(kv$EXTNAME))
                                        paste0('  ::  ', kv$EXTNAME) else '')),
        #The square is fixed by the wrapper's aspect-ratio and the plot is asked to fill
        #it, rather than by giving the plot a pixel height. A pixel height would make
        #the drawing area as wide as the column, so a square image inside it is padded
        #top and bottom, which is the letter boxed look this is meant to remove
        div(class = 'rf-cut', style = paste0('max-width: ', w, 'px;'),
            plotOutput(paste0('cutplot_', i), height = '100%'))
      )
    })
    tagList(
      #repeat(auto-fill, minmax(min(100%, w), 1fr)) so the column count follows the width
      #of the right hand panel: on a wide screen several cutouts sit side by side, and on a
      #narrow one the same page falls back to a single column with no per-breakpoint rules.
      #The min(100%, ...) stops a large requested side from overflowing a small panel
      div(class = 'rf-cut-grid',
          style = paste0('grid-template-columns: repeat(auto-fill, minmax(min(100%, ',
                         w, 'px), 1fr));'),
          !!!cells),
      if(n < n_tot) helpText('Showing ', n, ' of ', n_tot, ' cutout(s). Raise "Max ',
                             'cutouts to plot" to draw more and give them a tick box; the ',
                             n_tot - n, ' not drawn here are listed in the table above ',
                             'and stay included unless you clear them with "Include ',
                             'none" first.')
    )
  })

  output$result_counts = renderUI({
    pos = parse_positions(in_('positions'))
    bad = attr(pos, 'bad')
    n = if(is.null(state$res)) 0 else length(state$res)
    tagList(
      if(length(bad) > 0) helpText('Unread as positions: ', paste(bad, collapse = ' | ')),
      if(n > 0) helpText(n, ' cutout(s) in this result, named <store>_<position> when ',
                         'more than one position was asked for.')
    )
  })

  output$index_note = renderUI({
    if(is.null(state$idx)){
      return(helpText('Load an index on the Store tab before searching.'))
    }
    NULL
  })

  #A bare data.frame reaching renderDataTable would have the ... options applied by the
  #renderer, but building the datatable explicitly here means those options are the ones
  #that ship with the widget, which is also where scrollX has to live for the wide index
  #table to be scrollable at all. pageLength is kept small because the outputs carry a
  #fixed pixel height and twenty five rows do not fit in it, which would push the last
  #rows and the pager out of the box rather than giving the table more room
  #escape and callback are arguments of datatable rather than of the JS options, so they
  #are named separately from the ... that go to options
  #selection stays a named argument because the default turns rows into something a click
  #highlights, which reads like a control when the tick column is the one that counts
  dt_table = function(df, pageLength = 10, escape = TRUE, callback = NULL,
                      selection = 'none', ...){
    if(!has_dt){
      return(head(df, 200))
    }
    #callback is dropped from the call rather than passed as NULL. DT checks the argument
    #against its own default and stops with "The 'callback' argument only accept a value
    #returned from JS()" for anything else, NULL included, so naming it unconditionally
    #errored every table that has no callback of its own. Omitting it leaves DT's default
    #in place, which is what those tables want
    args = list(data = df, options = list(pageLength = pageLength, scrollX = TRUE, ...),
                rownames = FALSE, fillContainer = FALSE, escape = escape,
                selection = selection)
    if(!is.null(callback)){
      args$callback = callback
    }
    do.call(DT::datatable, args)
  }

  #A column of real checkboxes, because this is the one view that lists every result,
  #including the cutouts the gallery is not drawing, and because DT's own row selection is
  #only a highlight: it shows no mark to read the current state off, and reports it under
  #an input name of its own. The tick is written as HTML, which is why the column is left
  #out of the escape set, and a change is sent back as an event by the table's callback
  output$match_table = if(has_dt){
    DT::renderDataTable({
      mm = state$matches
      req(mm)
      mm = mm[mm$name %in% names(state$res), , drop = FALSE]
      #Position in the result list rather than row number, because the rows get sorted,
      #paged and filtered and the position does not move
      idx = match(mm$name, names(state$res))
      #Isolated: the boxes follow the selection through the message handler, and reading
      #it reactively here would redraw the table on every tick and throw away whatever
      #the user had searched for. It is only the initial state that matters, and that is
      #whatever the selection says at the moment the table is drawn
      sel = isolate(sel_of(state$res, state$sel))
      mm = cbind(data.frame(tick = sprintf(
        '<input type="checkbox" class="rf-tick" data-rf-i="%d"%s>',
        idx, ifelse(sel[idx], ' checked', '')),
        stringsAsFactors = FALSE), mm)
      escape = setdiff(seq_len(ncol(mm)), 1L)
      dt_table(mm, pageLength = 10, escape = escape,
               columnDefs = list(list(targets = 0, orderable = FALSE,
                                      searchable = FALSE, width = '2.6rem')),
               callback = htmlwidgets::JS(table_tick_callback))
    }, server = FALSE)
  }else{
    renderTable({
      mm = state$matches
      req(mm)
      dt_table(mm)
    })
  }

  index_shown = reactive({
    rows = state$rows
    req(rows)
    out = index_view_columns(rows)
    #The filter is a glob, like the store name pattern on the Store tab (see glob_regex),
    #so *.zarr and tile* mean what they look like rather than being handed to grepl as a
    #regular expression
    if(nzchar(in_('idx_filter'))){
      out = out[grepl(glob_regex(in_('idx_filter')), out$label), , drop = FALSE]
    }
    status = in_('idx_status', 'all')
    if(status != 'all'){
      out = out[out$status == status, , drop = FALSE]
    }
    out
  })
#The full column set is wide enough that nothing works without a horizontal scroll, and
  #the reason it did not appear was that scrollX alone leaves DataTables sizing the
  #wrapper to the table; the CSS on .dataTables_wrapper in the header forces the scroll
  #onto the container the card actually gives it
  output$index_table = if(has_dt){
    DT::renderDataTable(dt_table(index_shown()), server = FALSE)
  }else{
    renderTable(head(index_shown(), 200))
  }

  output$idx_info = renderText({
    rows = state$rows
    req(rows)
    if(nrow(rows) == 0){
      return('index rows:   0 -- the store name pattern excluded every row.')
    }
    paste0('index rows:   ', nrow(rows), '\n',
           'stores:       ', length(unique(rows$label)), '\n',
           'extnames:     ', paste(sort(unique(rows$extname)), collapse = ', '), '\n',
           'statuses:     ', paste(names(table(rows$status)), table(rows$status),
                                   sep = ' = ', collapse = ', '), '\n',
           'RA span:      ', paste(round(range(rows$ra, na.rm = TRUE), 4),
                                   collapse = ' to '), '\n',
           'Dec span:     ', paste(round(range(rows$dec, na.rm = TRUE), 4),
                                   collapse = ' to '), '\n',
           'pixscale ("): ', paste(round(range(rows$pixscale_x, na.rm = TRUE), 4),
                                   collapse = ' to '))
  })

  #Re-reading the file rather than reusing the rows in memory: the point of the button is
  #to pick up an index that changed underneath us, which the cached copy cannot show
  observeEvent(input$idx_refresh, {
    req(state$con)
    got = with_capture(load_index(state$con, rebuild = flag_('rebuild'), note = note))
    for(m in got$msgs) note(m)
    val = got$value
    if(is.null(val) || !is.null(val$error)){
      note('ERROR: reload failed: ', if(is.null(val)) 'no result' else val$error)
      return(NULL)
    }
    adopt_index(val)
  })

  #The id plotly hands back on a click or a box drag. Numbers rather than the store name
  #or the index cache key, because every vertex of every footprint carries one and a ring
  #is six vertices: on a release of three thousand tiles a 120 character path would put
  #more than two megabytes of label into the page before any of it was read, and the
  #browser would pay that on every redraw. Numbering the rows of the unfiltered set is
  #what makes a number stable while the index is loaded, because filtering only takes rows
  #away and never renumbers the rest. Only the box and lasso tools resolve to a number;
  #clicked picks are held as coordinates, so they mean the same thing across a reload
  frame_rows = reactive({
    rows = state$rows
    #No index yet is an ordinary state of this tab rather than an error to interrupt on, so
    #that the panel can say so in words. The callers that genuinely need frames req() on
    #the NULL
    if(is.null(rows)){
      return(NULL)
    }
    fr = index_footprints(rows[rows$status == 'ok', , drop = FALSE])
    fr$frame_id = seq_len(nrow(fr))
    return(fr)
  })

  #The frames this tab is showing: every searchable footprint the Index tab's filters
  #leave. One function rather than the filtering in four places, so the plot, the table,
  #the count and the two Send buttons can never disagree about what is on screen. Empty is
  #returned as a zero row data frame rather than as a req() failure, because a renderer that
  #can see 'nothing' says so and one that only gets an interrupting condition leaves a
  #blank box where the answer should be
  frames_subset = function(){
    fr = frame_rows()
    if(is.null(fr)){
      return(NULL)
    }
    #The same glob as the Index tab's table, so the two views cannot disagree about
    #what is on screen
    if(nzchar(in_('idx_filter'))){
      fr = fr[grepl(glob_regex(in_('idx_filter')), fr$label), , drop = FALSE]
    }
    status = in_('idx_status', 'all')
    if(status != 'all'){
      fr = fr[fr$status == status, , drop = FALSE]
    }
    return(fr)
  }

  frames_view = reactive({
    fr = frames_subset()
    req(!is.null(fr), nrow(fr) > 0)
    return(fr)
  })

  #What a hover shows. The store the tile came from is the thing a user wants at this
  #moment -- it is what a result is named after on the Cutouts tab -- and the shape and
  #scale say whether this is the tile they meant. Kept to the columns the index always
  #has, since a release written without a field would otherwise print NA beside every tile
  frame_hover = function(fr){
    bits = paste0('<b>', fr$name, '</b>',
                  if('extname' %in% names(fr)) paste0('<br>extname: ', fr$extname) else '',
                  if('type' %in% names(fr)) paste0('<br>type: ', fr$type) else '',
                  '<br>RA, Dec: ', signif(fr$ra, 8), ', ', signif(fr$dec, 8))
    if(all(c('naxis1', 'naxis2') %in% names(fr))){
      bits = paste0(bits, '<br>shape: ', fr$naxis1, ' x ', fr$naxis2)
    }
    if('pixscale_x' %in% names(fr)){
      bits = paste0(bits, '<br>pixscale: ', signif(fr$pixscale_x, 4), '"/pix')
    }
    if('label' %in% names(fr)){
      bits = paste0(bits, '<br>', fr$label)
    }
    return(bits)
  }

  #The payload of one of the Frames plot's plotly events, parsed the way
  #plotly::event_data() parses it. Reading the widget's own input id rather than calling
  #event_data() is deliberate: event_data() registers an onFlushed callback that warns
  #whenever the event has not been registered by a plot that has already rendered, and this
  #panel sits in a tab that Shiny keeps suspended until the user opens it. At startup, and
  #under testServer, which never renders an output unless asked, nothing has rendered, so
  #the observer below would emit a warning no user can either see or act on. The widget
  #posts 'plotly_<event>-<source>', with the source set on the plot itself, and the value
  #is the JSON text of an array of points. jsonlite is not in the checks at the top of the
  #file because plotly imports it, so a session that can draw the plot can parse this
  plotly_event = function(event){
    txt = input[[paste0('plotly_', event, '-frames')]]
    if(is.null(txt) || !is.character(txt) || !nzchar(txt)){
      return(NULL)
    }
    val = tryCatch(jsonlite::parse_json(txt, simplifyVector = TRUE), error = function(e) NULL)
    return(val)
  }

  #plotly sends each event as JSON text, which arrives as a data frame with one row per
  #point and the columns the widget attaches (curveNumber, pointNumber, x, y, customdata).
  #Everything here resolves back through customdata rather than pointNumber: that would
  #work for the centre markers, but the footprints are drawn as one NaN separated polyline,
  #so their point numbers index into a list of vertices, not into frames. A vertex with no
  #id (the separators) is dropped rather than guessed at
  event_ids = function(ev){
    if(is.null(ev) || NROW(ev) == 0 || is.null(ev$customdata)){
      return(character(0))
    }
    id = suppressWarnings(as.character(unlist(ev$customdata, use.names = FALSE)))
    return(id[!is.na(id) & nzchar(id)])
  }

  #Positions the user clicked, as free RA/Dec rather than as frame ids. A click means "I
  #want a cutout here", and here is wherever the cursor was, not the centre of the nearest
  #tile; the rectangle and lasso tools are the way to ask for whole tiles. Held as a small
  #data frame so the list survives redraws and index reloads, since a coordinate is a
  #coordinate whatever the index says
  picks = reactiveVal(data.frame(ra = numeric(0), dec = numeric(0)))

  #The frames a box or lasso drag caught, in the order they appear in the view rather than
  #the order plotly reported them. plotly selects points, and both traces carry one point
  #per frame, so the ids arrive twice over; unique() through the %in% below already folds
  #that back to one row per frame
  selected_frames = function(fr = frames_subset()){
    id = event_ids(plotly_event('selected'))
    if(length(id) == 0 || is.null(fr)){
      return(NULL)
    }
    out = fr[fr$frame_id %in% id, , drop = FALSE]
    if(nrow(out) == 0){
      return(NULL)
    }
    return(out)
  }

  #The rectangle a selection was made inside, which is the one thing plotly's selected
  #event does not report as a range: plotly_selected carries points, and only the lasso
  #tool's brushing event carries a point cloud of its own. So the extremes of the selected
  #points are used, which is the box the user drew for every purpose this serves -- a
  #single frame selected reports the bounds of that frame
  selected_box = function(fr = frames_subset()){
    sel = selected_frames(fr)
    if(is.null(sel)){
      return(NULL)
    }
    list(x = range(c(sel$xmin, sel$xmax), na.rm = TRUE),
         y = range(c(sel$ymin, sel$ymax), na.rm = TRUE))
  }

  #The centres of a set of frames as the Cutouts tab reads them. The tile's reference
  #position is the coordinate, not the middle of the box that drew it, which is the whole
  #reason the frames are clickable rather than just brushable
  pos_lines = function(fr){
    return(paste(signif(fr$ra, 9), signif(fr$dec, 9)))
  }

  #What a send put in the log: which frames went in, and the coordinates themselves. A
  #pick is otherwise invisible once it leaves the tab, and the log is the only record of
  #what reached the position list. Only the frames whose lines were really added are
  #named, so a send that found them already in the list cannot claim credit for them
  log_send = function(kind, fr, new){
    fr = fr[pos_lines(fr) %in% new, , drop = FALSE]
    if(nrow(fr) == 0){
      return(invisible(NULL))
    }
    note('Sent ', nrow(fr), ' ', kind, ' frame centre(s) to the Cutouts tab: ',
         paste(utils::head(fr$name, 8), collapse = ', '),
         if(nrow(fr) > 8) paste0(' (+', nrow(fr) - 8, ' more)') else '', '. Positions: ',
         paste(utils::head(new, 4), collapse = '; '),
         if(length(new) > 4) paste0(' (+', length(new) - 4, ' more)') else '')
    invisible(NULL)
  }

  #Append to the position list rather than replacing it, and skip lines that are already
  #there, so picking a tile twice cannot fill the request with duplicates. Returns the
  #lines actually added, or NULL when there was nothing new to add
  add_positions = function(lines){
    cur = trimws(unlist(strsplit(as.character(isolate(input$positions)), '[\r\n]+')))
    cur = cur[nzchar(cur)]
    new = unique(lines)
    new = new[!(new %in% cur)]
    if(length(new) == 0){
      return(NULL)
    }
    updateTextAreaInput(session, 'positions', value = paste(c(cur, new), collapse = '\n'))
    return(new)
  }

  #A click anywhere in the panel is a position, in any number and at any zoom. The
  #coordinate is worked out in the browser (see the onRender handler below) because plotly's
  #own click event reports the coordinates of the point that was hit, which would snap every
  #pick to the nearest tile centre or marker. Picks are held as coordinates rather than
  #resolved to frames for the same reason: where you clicked is the answer, not which tile
  #happens to own that spot
  observeEvent(input$frames_click, {
    pt = input$frames_click
    ra = suppressWarnings(as.numeric(pt$ra)[1])
    dec = suppressWarnings(as.numeric(pt$dec)[1])
    if(!is.finite(ra) || !is.finite(dec)){
      return(NULL)
    }
    old = picks()
    both = rbind(old, data.frame(ra = ra, dec = dec))
    #The same pixel clicked twice must not fill the list with duplicates
    picks(both[!duplicated(paste(both$ra, both$dec)), , drop = FALSE])
    return(NULL)
  })

  #The cursor's position in degrees, which is what a pick must be. This is done in the
  #browser off a plain listener on the plot surface rather than from plotly's click event
  #for two reasons. plotly only emits plotly_click when the pointer lands on a mark, so a
  #click on empty sky between tiles - the common case, and exactly the case where you want
  #to choose a position - would report nothing. And that event carries the coordinates of
  #the mark that was hit rather than of the cursor, which would snap every pick to the
  #nearest tile centre. So the mouse position is read against the drag layer, which is
  #positioned at the origin of the plot area and therefore already in the pixel space the
  #axes measure from, and turned into degrees with the axis' own p2d. d3 is taken from the
  #plotly bundle rather than assumed to be global. p2d is an internal, so the same linear
  #map is worked out from the range and length when it is not there. If neither can be read
  #the click is dropped rather than answered with a mark's coordinates, because the click
  #may well have been on empty sky where there is no mark to report
  frames_click_js = paste(
    'function(el, x){',
    '  var gd = el;',
    '  var d3 = (window.Plotly && window.Plotly.d3) || window.d3;',
    '  function to_deg(ev){',
    '    var dl = gd.querySelector(".nsewdrag");',
    '    var fl = gd._fullLayout;',
    '    if(!dl || !fl || !fl.xaxis || !fl.yaxis) return null;',
    '    var pts = null;',
    '    if(d3 && d3.mouse){ try{ pts = d3.mouse(dl); }catch(e){ pts = null; } }',
    '    if(!pts){',
    '      var bb = dl.getBoundingClientRect();',
    '      pts = [ev.clientX - bb.left, ev.clientY - bb.top];',
    '    }',
    '    function inv(ax, p){',
    '      if(ax.p2d) return ax.p2d(p);',
    '      if(!ax._length || !ax.range) return null;',
    '      return ax.range[0] + (p / ax._length) * (ax.range[1] - ax.range[0]);',
    '    }',
    '    var ra = inv(fl.xaxis, pts[0]), dec = inv(fl.yaxis, pts[1]);',
    '    if(ra === null || dec === null) return null;',
    '    if(!isFinite(ra) || !isFinite(dec)) return null;',
    '    return {ra: ra, dec: dec};',
    '  }',
    #Three things have to be filtered out before a press and release counts as a pick. A
    #drag that ends on the surface fires a click too, so a box select or a rubber band zoom
    #would add a position nobody meant to pick. The mode bar and the legend live inside the
    #graph div, so their buttons arrive here as well and would be converted from a pixel
    #somewhere over the toolbar. And double click is bound to reset the view, which would
    #otherwise register two picks on the way to doing that
    '  var down = null, timer = null;',
    '  function in_chrome(node){',
    '    while(node && node !== gd){',
    '      if(node.classList && (node.classList.contains("modebar") ||',
    '         node.classList.contains("legend") ||',
    '         node.classList.contains("select-outline"))) return true;',
    '      node = node.parentNode;',
    '    }',
    '    return false;',
    '  }',
    '  gd.addEventListener("mousedown", function(ev){',
    '    down = {x: ev.clientX, y: ev.clientY, t: Date.now()};',
    '  });',
    '  gd.addEventListener("click", function(ev){',
    '    if(!down || in_chrome(ev.target)){ down = null; return; }',
    '    var moved = Math.abs(ev.clientX - down.x) + Math.abs(ev.clientY - down.y);',
    #down.t is when the press happened, so this is how long the button was held. ev.timeStamp
    #would answer a different question entirely, which is when the click fired
    '    var quick = (Date.now() - down.t) < 700;',
    '    down = null;',
    '    if(moved > 4 || !quick){ return; }',
    '    var at = to_deg(ev);',
    '    if(!at){ return; }',
    #Held briefly so a second click, which is a reset rather than a pick, can cancel it
    '    if(timer) clearTimeout(timer);',
    '    timer = setTimeout(function(){',
    '      timer = null;',
    '      Shiny.setInputValue("frames_click", {ra: at.ra, dec: at.dec,',
    '                          t: Date.now()}, {priority: "event"});',
    '    }, 260);',
    '  });',
    '  gd.addEventListener("dblclick", function(){',
    '    if(timer){ clearTimeout(timer); timer = null; }',
    '    down = null;',
    '  });',
    '}')

  #Clearing the clicked list is a deliberate action. A legend toggle, or a stray click on
  #empty space, is not, and neither should throw away a list the user built
  observeEvent(input$clear_picks, {
    picks(data.frame(ra = numeric(0), dec = numeric(0)))
    showNotification('Picked positions cleared.', type = 'message')
  })

  output$frames = plotly::renderPlotly({
    fr = frames_view()
    #Read with isolate, so that the trace carries the picks made so far without a pick
    #invalidating this renderer. A re-render reconciles a whole new specification against
    #the panel on screen, and that is how the zoom gets thrown away even with uirevision
    #set; a pick reaches the browser through the proxy below instead
    pk = isolate(picks())
    #One NaN separated polyline carrying every footprint, rather than one trace per tile.
    #A release of a few thousand tiles would otherwise build a few thousand plotly traces,
    #and the browser feels that as a stall long before it feels it as a map
    idv = fr$frame_id
    hv = frame_hover(fr)
    #Each ring is five vertices plus a separator, and every vertex of a ring carries that
    #ring's id so a box or lasso drag that catches an edge resolves to the right frame.
    #The separator has to be NA_character_ rather than NA_real_: cbind() with a character
    #column coerces the whole row to character, so a numeric NA would arrive as the string
    #'NA' and read back as the id of a frame called NA
    ring = function(v) as.vector(t(cbind(v, v, v, v, v, NA_character_)))
    xs = as.vector(t(cbind(fr$xmin, fr$xmax, fr$xmax, fr$xmin, fr$xmin, NA_real_)))
    ys = as.vector(t(cbind(fr$ymin, fr$ymin, fr$ymax, fr$ymax, fr$ymin, NA_real_)))
    p = plotly::plot_ly(source = 'frames') |>
      plotly::add_trace(x = xs, y = ys, customdata = ring(idv),
                        text = ring(hv), type = 'scatter', mode = 'lines',
                        name = 'frames', showlegend = FALSE,
                        hovertemplate = '%{text}<extra></extra>',
                        line = list(color = '#4c78a8', width = 0.8), opacity = 0.65) |>
      plotly::add_trace(x = fr$ra, y = fr$dec, customdata = as.character(idv),
                        text = hv, type = 'scatter', mode = 'markers',
                        name = 'frame centres', showlegend = TRUE,
                        hovertemplate = '%{text}<extra></extra>',
                        marker = list(color = '#4c78a8', size = 3.5, opacity = 0.85)) |>
      #The picks live in a third trace that is always there, drawn from the list read with
      #isolate() above so that a pick does not invalidate this renderer. Both halves matter:
      #a re-render reconciles a whole new specification against the panel on screen, which is
      #how the zoom gets thrown away even with uirevision set, and a trace appearing or
      #disappearing changes how many axes have to be worked out again. So the trace count is
      #fixed and a pick reaches the browser through the proxy below. With nothing picked the
      #arrays are empty, which plotly renders as a trace with no data at all
      plotly::add_trace(x = pk$ra, y = pk$dec, type = 'scatter', mode = 'markers',
                        name = 'picked', showlegend = TRUE,
                        hovertemplate = 'clicked<br>RA, Dec: %{x}, %{y}<extra></extra>',
                        marker = list(color = '#e45756', size = 9, symbol = 'cross',
                                      line = list(width = 2)))
    xr = range(c(fr$xmin, fr$xmax), na.rm = TRUE)
    yr = range(c(fr$ymin, fr$ymax), na.rm = TRUE)
    #A little air at the edges so a tile on the boundary is not drawn under an axis
    ax = diff(xr) * 0.03 + 1e-6
    ay = diff(yr) * 0.03 + 1e-6
    p = plotly::layout(p,
                       #The RA axis is reversed the way a sky map is read, and passing the
                       #range with the larger bound first is what does it here. That is
                       #also the order plotly reports a range back in, so nothing
                       #downstream has to care which way the drag went
                       xaxis = list(title = 'RA (deg)', range = c(xr[2] + ax, xr[1] - ax),
                                    zeroline = FALSE, ticks = 'outside',
                                    #Equal degrees per pixel on both axes, so a square tile
                                    #draws square. Without the anchor the two axes scale
                                    #independently to the panel, and a long thin release
                                    #comes out as a row of tall rectangles
                                    scaleanchor = 'y', scaleratio = 1,
                                    constrain = 'domain'),
                       yaxis = list(title = 'Dec (deg)',
                                    range = c(yr[1] - ay, yr[2] + ay),
                                    zeroline = FALSE, ticks = 'outside',
                                    constrain = 'domain'),
                       dragmode = 'zoom', hovermode = 'closest',
                       selectdirection = 'any',
                       #A click fires an event but does not become a one-point selection.
                       #The drag tools are the way to catch whole tiles, and a click that
                       #quietly replaced the box selection would have the two routes fight
                       #over the same state
                       clickmode = 'event',
                       title = list(text = paste(nrow(fr), 'searchable frame(s)'),
                                    font = list(size = 13)),
                       legend = list(orientation = 'h', x = 0, y = 1.04,
                                     font = list(size = 10)),
                       margin = list(l = 58, r = 14, t = 46, b = 42),
                       #A zoom is a state the user set, so a redraw must not throw it
                       #away. The revision is keyed to the filters rather than fixed:
                       #narrowing the view is the one moment an old zoom genuinely is the
                       #wrong window, and holding it would leave an empty panel
                       uirevision = paste(in_('idx_filter'), '|',
                                          in_('idx_status', 'all')))
    p = plotly::config(p, displaylogo = FALSE, scrollZoom = TRUE, doubleClick = 'reset',
                       modeBarButtonsToRemove = c('toImage', 'sendDataToCloud',
                                                  'toggleSpikelines',
                                                  'hoverCompareCartesian',
                                                  'hoverClosestCartesian'))
    p = htmlwidgets::onRender(p, frames_click_js)
    return(p)
  })

  #The picked positions, drawn in the browser by restyling that third trace. Re-rendering
  #the panel to add a marker is what used to throw the zoom away, since a fresh
  #specification is reconciled against the panel on screen even with uirevision set, while
  #restyling one trace touches nothing the user is looking at. ignoreInit matters: a proxy
  #call is a message to the page, and the browser throws when the graph div it names is not
  #there yet, so nothing is sent until a pick has actually been made. Every pick is made by
  #clicking the rendered panel and every clear is a button in the card beside it, so when
  #this runs the panel exists
  observeEvent(picks(), {
    pk = picks()
    plotly::plotlyProxyInvoke(plotly::plotlyProxy('frames'), 'restyle',
                              pick_restyle_args(pk$ra, pk$dec), 2)
  }, ignoreInit = TRUE)

  output$frames_info = renderText({
    #Read through frames_subset rather than frames_view, so that a session with no index
    #yet is told so in words rather than leaving a blank box where the answer should be
    fr = frames_subset()
    if(is.null(fr) || nrow(fr) == 0){
      return(paste0('No frames to show. Load an index on the Store tab first, or widen ',
                    'the store name pattern and the Index tab filters.'))
    }
    sel = selected_frames()
    box = selected_box()
    pk = picks()
    npick = nrow(pk)
    bits = paste0('Picked by click: ', npick,
                  if(npick > 0)
                    paste0(' (', paste(utils::head(signif(pk$ra, 6), 3), collapse = ', '),
                           if(npick > 3) ' ...' else '', ')') else '',
                  '\nSelected by box: ', if(is.null(sel)) 0 else nrow(sel))
    if(!is.null(box)){
      bits = paste0(bits, '\nSelection box: ', paste(round(box$x, 4), collapse = ' to '),
                    ' RA, ', paste(round(box$y, 4), collapse = ' to '), ' Dec')
    }
    if(!is.null(sel) && nrow(sel) > 0){
      bits = paste0(bits, '\nSelected: ', paste(utils::head(sel$name, 8),
                                               collapse = ', '),
                    if(nrow(sel) > 8) paste0(' (+', nrow(sel) - 8, ' more)') else '')
    }
    bits = paste0(bits, '\nFrames in view: ', nrow(fr))
    return(bits)
  })

  #What the table shows. A box selection is listed when there is one, since those rows are
  #the tiles whose centres are about to be sent. Clicked picks are not in it, because a pick
  #is a coordinate rather than a tile; with neither, the frames in view are listed so the
  #tab has something to read before anything is chosen
  frames_shown = reactive({
    fr = frames_subset()
    req(!is.null(fr), nrow(fr) > 0)
    sel = selected_frames(fr)
    return(if(is.null(sel)) fr else sel)
  })

  output$frames_table = if(has_dt){
    DT::renderDataTable({
      sel = frames_shown()
      req(nrow(sel) > 0)
      cols = c('name', 'extname', 'type', 'ra', 'dec', 'naxis1', 'naxis2', 'pixscale_x')
      dt_table(as.data.frame(sel)[, cols, drop = FALSE], pageLength = 10)
    }, server = FALSE)
  }else{
    renderTable({
      sel = frames_shown()
      req(sel)
      head(as.data.frame(sel)[, c('name', 'extname', 'type', 'ra', 'dec')], 50)
    })
  }

  #The positions the user clicked, sent as typed. This is the main route from this tab to a
  #request, and the coordinate is exactly where the cursor was: to cut at tile centres,
  #select the tiles with the box or lasso tool and use Send selected frames
  observeEvent(input$send_pos, {
    pk = picks()
    if(nrow(pk) == 0){
      showNotification('Click a position on the map first, at any zoom.', type = 'warning')
      return(NULL)
    }
    line = paste(signif(pk$ra, 9), signif(pk$dec, 9))
    new = add_positions(line)
    if(is.null(new)){
      showNotification('Those positions are already in the list.', type = 'message')
      return(NULL)
    }
    note('Sent ', length(new), ' clicked position(s) to the Cutouts tab: ',
         paste(utils::head(new, 4), collapse = '; '),
         if(length(new) > 4) paste0(' (+', length(new) - 4, ' more)') else '')
    bslib::nav_select('nav', 'cutouts')
  })

  #The box selection route, kept because a box is the fast way to take in a neighbourhood.
  #What is sent is still the centres of the frames the box caught, not the centre of the
  #box, which is the answer the old brush gave and the reason it was not very useful
  observeEvent(input$send_sel, {
    sel = selected_frames()
    if(is.null(sel)){
      showNotification('Drag a selection box over the frames first.', type = 'warning')
      return(NULL)
    }
    new = add_positions(pos_lines(sel))
    if(is.null(new)){
      showNotification('Those positions are already in the list.', type = 'message')
      return(NULL)
    }
    box = selected_box()
    note('Selection box: ', paste(round(box$x, 4), collapse = ' to '), ' RA, ',
         paste(round(box$y, 4), collapse = ' to '), ' Dec.')
    log_send('selected', sel, new)
    bslib::nav_select('nav', 'cutouts')
  })
}

shinyApp(ui = ui, server = server)
