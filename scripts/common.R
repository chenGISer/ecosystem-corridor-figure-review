# R-only drawing/export helpers. Legend layout changes preserve all source data.
library(grid)
.destination <- Sys.getenv('AG_LEGEND_OUTPUT')
stopifnot(nzchar(.destination))
dir.create(.destination,recursive=TRUE,showWarnings=FALSE)
.out <- function(p) file.path(.destination,basename(p))
cairo_pdf <- function(filename,...) grDevices::cairo_pdf(.out(filename),...)
svg <- function(filename,...) grDevices::svg(.out(filename),...)
png <- function(filename,...) grDevices::png(.out(filename),...)
tiff <- function(filename,...) grDevices::tiff(.out(filename),...)
write.csv <- function(x,file,...) utils::write.csv(x,.out(file),...)
writeLines <- function(text,con=stdout(),...) base::writeLines(text,if(is.character(con)).out(con)else con,...)
capture.output <- function(...,file=NULL) utils::capture.output(...,file=if(is.character(file)).out(file)else file)
file.copy <- function(from,to,...) base::file.copy(from,.out(to),...)
.legend_log <- list()
legend_text <- function(label,x,y,size=8.5,width=40,family='Times New Roman',face='plain') {
  # Wrap at measured glyph widths, without abbreviating or changing labels.
  gp <- gpar(fontfamily=family,fontsize=size,fontface=face,col='#222222',lineheight=1.02)
  rows <- character()
  for(part in strsplit(label,'\n',fixed=TRUE)[[1]]) {
    words<-strsplit(part,' ',fixed=TRUE)[[1]]; current<-''
    for(word in words) {
      candidate<-if(nzchar(current))paste(current,word)else word
      gw<-convertWidth(grobWidth(textGrob(candidate,gp=gp)),'mm',valueOnly=TRUE)
      if(gw>width && nzchar(current)){rows<-c(rows,current);current<-word}else current<-candidate
    }
    rows<-c(rows,current)
  }
  label<-paste(rows,collapse='\n')
  g<-textGrob(label,x=unit(x,'native'),y=unit(y,'native'),just=c('left','centre'),gp=gp)
  ww<-convertWidth(grobWidth(g),'mm',valueOnly=TRUE);hh<-convertHeight(grobHeight(g),'mm',valueOnly=TRUE)
  .legend_log[[length(.legend_log)+1L]] <<- data.frame(Label=gsub('\n',' ',label),X=x,Y=y,Width=ww,Height=hh,Size=size)
  grid.draw(g)
}
base_legend_large <- function(...,cex=1,pt.cex=NULL,x.intersp=.8,y.intersp=1.15,seg.len=2.4) {
  # Use physical point size independently of base-graphics panel scaling.
  z<-8.5/(par('ps')*par('cex'))
  graphics::legend(...,cex=z,pt.cex=z*1.08,x.intersp=x.intersp,y.intersp=y.intersp,seg.len=seg.len)
}
legend <- base_legend_large
