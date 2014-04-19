##' Tufte Maximal Data, Minimal Ink Theme
##'
##' Theme based on Chapter 6 "Data-Ink Maximization and Graphical
##' Design" of Edward Tufte *The Visual Display of Quantitative
##' Information*. No border, no axis lines, no grids. This theme works
##' best in combination with \code{\link{geom_rug}} or
##' \code{\link{geom_rangeframe}} and \code{\link{scale_x_tufte}}.
##'
##' @note
##' The default font family is set to "serif" as he uses serif fonts
##' for labels in "The Visual Display of Quantitative Information".
##' The serif font used by Tufte in his books is a variant of Bembo,
##' while the sans serif font is Gill Sans. If these fonts are
##' installed on your system, then you can use them with the package
##' \bold{extrafont}.
##'
##' @param ticks \code{logical} Show axis ticks?
##' @param base_size Base font size
##' @param base_family Base font family
##'
##' @references Tufte, Edward R. (2001) The Visual Display of
##' Quantitative Information, Chapter 6.
##'
##' @family themes tufte
##' @examples
##' # with ticks and range frames
##' (ggplot(mtcars, aes(wt, mpg))
##'  + geom_point() + geom_rangeframe()
##'  + theme_tufte())
##' # with geom_rug
##' (ggplot(mtcars, aes(wt, mpg))
##'  + geom_point() + geom_rug()
##'  + theme_tufte(ticks=FALSE))
##' \dontrun{
##' ## Using the Bembo serif family
##' library(extrafont)
##' (ggplot(mtcars, aes(wt, mpg))
##'  + geom_point() + geom_rangeframe()
##'  + theme_tufte(base_family="BemboStd"))
##' ## Using the Gill Sans sans serif family
##' (ggplot(mtcars, aes(wt, mpg))
##'  + geom_point() + geom_rangeframe()
##'  + theme_tufte(base_family="GillSans"))
##' }
##' @export
theme_tufte <- function(ticks=TRUE, base_family="serif", base_size=11) {
    ## TODO: start with theme_minimal
    ret <- theme_bw(base_family=base_family, base_size=base_size) +
        theme(
            legend.background = element_blank(),
            legend.key        = element_blank(),
            panel.background  = element_blank(),
            panel.border      = element_blank(),
            strip.background  = element_blank(),
            plot.background   = element_blank(),
            axis.line         = element_blank(),
            panel.grid = element_blank())
    if (!ticks) {
        ret <- ret + theme(axis.ticks = element_blank())
    }
    ret
}

##' Axis breaks inclusive of extreme values
##'
##' This function returns pretty axis breaks that always include the extreme 
##' values of the data. This internal function is called by \code{\link{scale_x_tufte}}
##'  and \code{\link{scale_y_tufte}}.
##'
##' @param expand vector of length 2, see \code{\link{scale_x_continuous}}
##' @param ... additional parameters passed to FUN
##' @param threshold numeric, setting to avoid overlaps between labels
##' @param digits numeric, precision of the extreme labels. If NULL, defaults to the precision of other breaks.
##' @param FUN function to compute intermediate breaks
##' @family tufte
range_breaks <- function(expand=c(0.04, 0), ..., threshold=0.5, digits = NULL,
                         FUN = scales::extended_breaks){
  function(x) {
    spread <- range(x)
    
    ## reverse affine transformation
    m <- matrix(c(1+expand[1], expand[1], expand[1], 1+expand[1]), 
                ncol=2, byrow=TRUE)
    limits <- m %*% (spread + c(1,-1)*expand[2]) / (1+2*expand[1])

    pretty_breaks <-  FUN(...)(x)
    spacing <- min(diff(pretty_breaks))
    
    ## work out the accuracy from the range
    ## Josh O'Brien http://stackoverflow.com/a/23171858/471093
    if(is.null(digits))
      digits <- ceiling(log10(abs(limits))) - floor(log10(abs(spacing)))
    limits <- signif(limits, digits)
    
    ## don't include pretty breaks if outside range
    keep <- pretty_breaks > limits[1] & pretty_breaks < limits[2]
    pretty_breaks <- pretty_breaks[keep]
    
    ## prevent potential label overlap at the edges
    n <- length(pretty_breaks)
    clash <- c(abs(limits[1] - pretty_breaks[1]) < threshold * spacing,
               abs(limits[2] - pretty_breaks[n]) < threshold * spacing)
    remove <- -c(1,n)[clash]
    
    all_breaks <- if(any(clash)) 
      c(limits, pretty_breaks[remove]) else
        c(limits, pretty_breaks)
    
    sort(all_breaks)
  }
}

##' Axis breaks inclusive of extreme values
##'
##' These scales draw pretty axis breaks that always include the extreme 
##' values of the data.
##' @aliases scale_x_tufte scale_y_tufte
##' @param breaks see \code{\link{scale_x_continuous}}
##' @param labels see \code{\link{scale_x_continuous}}
##' @param expand see \code{\link{scale_x_continuous}}
##' @param ... additional parameters passed to \code{\link{continuous_scale}}
##' @family tufte
##' @examples
##' ggplot(mtcars, aes(wt, mpg)) + 
##' geom_point() + 
##' geom_rangeframe() + theme_tufte() +  
##' scale_x_tufte(breaks=range_breaks(n=20, digits = 3))+  
##' scale_y_tufte()
##' @export
scale_x_tufte <-  function(breaks = range_breaks(expand), ..., 
                           labels = as.character,
                           expand=c(0.04, 0))
  continuous_scale(c("x", "xmin", "xmax", "xend", "xintercept"), 
                   "position_c", identity,
                   breaks = breaks, ..., labels=labels,
                   expand = expand, guide = "none")

##' @export
scale_y_tufte <-  function(breaks = range_breaks(expand), ..., 
                           labels = as.character,
                           expand=c(0.04, 0))
  continuous_scale(c("y", "ymin", "ymax", "yend", "yintercept"), 
                   "position_c", identity,
                   breaks = breaks, ..., labels=labels,
                   expand = expand, guide = "none")

