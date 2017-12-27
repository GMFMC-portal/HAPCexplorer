

gsubC <- function(pattern, replacement, x) {
  .Call('mapview_gsubC', PACKAGE = 'mapview', pattern, replacement, x)
}

brewPopupRowC <- function(colname, value) {
  .Call('mapview_brewPopupRowC', PACKAGE = 'mapview', colname, value)
}

brewPopupRowAltC <- function(colname, value) {
  .Call('mapview_brewPopupRowAltC', PACKAGE = 'mapview', colname, value)
}

brewPopupCoords <- function(colname, value) {
  .Call('mapview_brewPopupCoords', PACKAGE = 'mapview', colname, value)
}

mergePopupRows <- function(names, values) {
  .Call('mapview_mergePopupRows', PACKAGE = 'mapview', names, values)
}

createTemplate <- function(tmpPath) {
  .Call('mapview_createTemplate', PACKAGE = 'mapview', tmpPath)
}

listPopupTemplates <- function(x, names, tmpPath) {
  .Call('mapview_listPopupTemplates', PACKAGE = 'mapview', x, names, tmpPath)
}

df2String <- function(x) {
  .Call('mapview_df2String', PACKAGE = 'mapview', x)
}

one2JSON <- function(x) {
  .Call('mapview_one2JSON', PACKAGE = 'mapview', x)
}

all2JSONlist <- function(x) {
  .Call('mapview_all2JSONlist', PACKAGE = 'mapview', x)
}

anyNA <- function(x) {
  .Call('mapview_anyNA', PACKAGE = 'mapview', x)
}

rowNA <- function(x) {
  .Call('mapview_rowNA', PACKAGE = 'mapview', x)
}