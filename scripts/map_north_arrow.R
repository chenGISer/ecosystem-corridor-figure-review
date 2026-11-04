# Shared black/white north-arrow glyph for metre-based maps drawn on mm canvases.
draw_north_arrow_grid <- function(x,y,height=6,width=2.8,label_size=6.8,family='Times New Roman') {
  uu<-function(v)grid::unit(v,'native')
  tip<-y+height; notch<-y+.30*height
  grid::grid.polygon(uu(c(x,x-width/2,x)),uu(c(tip,y,notch)),
                    gp=grid::gpar(fill='black',col='black',lwd=.45,linejoin='mitre'))
  grid::grid.polygon(uu(c(x,x,x+width/2)),uu(c(tip,notch,y)),
                    gp=grid::gpar(fill='white',col='black',lwd=.45,linejoin='mitre'))
  grid::grid.text('N',uu(x),uu(tip+2.2),just='centre',
                  gp=grid::gpar(col='black',fontsize=label_size,fontfamily=family))
}
