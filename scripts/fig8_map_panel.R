map_panel <- function() {
  xr <- range(boundary$X); yr <- range(boundary$Y)
  sc <- min(87/diff(xr),86/diff(yr))
  x0 <- 2+(87-diff(xr)*sc)/2; y0 <- 39+(86-diff(yr)*sc)/2
  X <- function(v) x0+(v-xr[1])*sc
  Y <- function(v) y0+(v-yr[1])*sc
  paths <- function(df,col,lwd,lty=1) {
    groups <- split(seq_len(nrow(df)),interaction(df$ID,df$Part,drop=TRUE))
    for(idx in groups) {
      d <- df[idx,]; d <- d[order(d$Sequence),]
      grid.lines(u(X(d$X)),u(Y(d$Y)),gp=gpar(col=col,lwd=lwd,lty=lty,linejoin='round',lineend='round'))
    }
  }
  paths(base,MAP['Existing'],.44); paths(boundary,'#91999E',.6)
  paths(changes[changes$Change=='Retained',],MAP['Retained'],1.25)
  paths(changes[changes$Change=='Removed',],MAP['Removed'],2.0,'33')
  paths(changes[changes$Change=='Added',],MAP['Added'],2.2)
  for(type in c('Supply','Demand')) {
    n <- nodes[nodes$NodeType==type,]
    grid.points(u(X(n$X)),u(Y(n$Y)),pch=if(type=='Supply')21 else 24,
      size=unit(if(type=='Supply')1.0 else 1.55,'mm'),gp=gpar(col='white',fill=MAP[type],lwd=.3))
  }
  a <- endpoints[endpoints$Change!='Retained',]
  ids <- unique(c(a$FromNode,a$ToNode)); n <- nodes[nodes$NetworkID %in% ids,]
  grid.points(u(X(n$X)),u(Y(n$Y)),pch=1,size=unit(2,'mm'),gp=gpar(col='#656565',lwd=.55))
  draw_north_arrow_grid(9,115,height=6,width=2.8,label_size=7.1,family=FONT)
  sx <- 9; sy <- 34; sw <- 300000*sc
  seg(sx,sy,sx+sw,sy,lwd=.8); seg(sx,sy,sx,sy+1.2,lwd=.8); seg(sx+sw,sy,sx+sw,sy+1.2,lwd=.8)
  txt('0',sx,sy+3.1,6.6,just='centre'); txt('300 km',sx+sw,sy+3.1,6.6,just='centre')
}
