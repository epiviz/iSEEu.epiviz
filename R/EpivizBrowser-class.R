#' Base class for Epiviz genome browser panel
#' @importClassesFrom iSEE Panel
setClass(
  "EpivizBrowser",
  contains = "Panel",
  slots = c(chr = "character",
            start = "numeric",
            end = "numeric",
            genome="ANY",
            tracks="ANY",
            epivizNav = "ANY")
)

setMethod("initialize", "EpivizBrowser",
  function(.Object, chr, start, end, genome, tracks, ...)
  {
    callNextMethod(.Object,
      chr = chr,
      start = start,
      end = end,
      genome = genome,
      tracks = tracks,
      epivizNav=epivizNav(chr=chr, start=start,
        end=end, interactive=TRUE, shiny=TRUE),
      ...)
  })

#' Initialization function
#' @param chr chromosome region to intitialize
#' @param start start region
#' @param end end region
#'
#' @export
EpivizBrowser <- function(chr="chr1", start=1, end=100000, genome, tracks=list(), ...) {
  new("EpivizBrowser", chr=chr, start=start, end=end, genome=genome,
        tracks=tracks, ...)
}


#' TODO: Separate class and methods

#' @importMethodsFrom iSEE .fullName
setMethod(".fullName",
          "EpivizBrowser",
          function(x)
            "Epiviz Genome Browser")

#' @importMethodsFrom iSEE .fullName
setMethod(".panelColor", "EpivizBrowser", function(x)
  "#4285f4")

#' @importMethodsFrom iSEE .defineOutput
setMethod(".defineOutput", "EpivizBrowser", function(x, ...) {
    x[["epivizNav"]]$render_component(shiny = TRUE)
})


#' @importMethodsFrom iSEE .createObservers
setMethod(".createObservers", "EpivizBrowser",
  function(x, se, input, session, pObjects, rObjects)
  {
    callNextMethod()
    x[["epivizNav"]]$register_shiny_handler(session)
})

#' @importMethodsFrom iSEE .renderOutput
setMethod(".renderOutput", "EpivizBrowser",
  function(x, se, ..., output, pObjects, rObjects) {
    x[["epivizNav"]]$add_genome(x[["genome"]])
    for (track in names(x[["tracks"]])) {
      x[["epivizNav"]]$plot(x[["tracks"]][[track]], datasource_name=track)
    }
  })
