if (.Platform$OS.type == "windows") invisible(try(Sys.setlocale("LC_CTYPE", "English_United States.utf8"), silent=TRUE))
arg <- grep('^--file=', commandArgs(), value=TRUE)[1]
root <- dirname(dirname(sub('^--file=', '', arg)))
output <- file.path(root,'output','fig10')
dir.create(output, recursive=TRUE, showWarnings=FALSE)
Sys.setenv(AG_LEGEND_OUTPUT=output)
source(file.path(root,'scripts','common.R'))
if (!capabilities('cairo')) stop('A Cairo-enabled R installation is required.')
library(grid)
source(file.path(root,'scripts','map_north_arrow.R'))
rd <- function(n) read.csv(file.path(root,'data','maps',n),fileEncoding='UTF-8-BOM',stringsAsFactors=FALSE)
boundary<-rd('Map_Boundary.csv');pa<-rd('Map_ProtectedAreas.csv')
sources<-rd('Map_SupplySources.csv');critical<-rd('Map_CurrentCriticalSourceProtection.csv')
rankdata<-rd('Node_Criticality_Change.csv')
sources$Critical<-as.integer(sources$ID %in% rankdata$NetworkID[rankdata$AfterRank<=30])
selected<-rd('Map_GuidedCorridorProtection.csv');baseline<-rd('Map_BaselineCorridorProtection.csv')

stopifnot(length(unique(critical$ID))==30,length(unique(selected$ID))==15,
          length(unique(baseline$ID))==278)
W<-183;H<-215;FONT<-'Times New Roman';INK<-'#222222'
COL<-c(Gap='#C56B53',Covered='#357C63',Other='#DCE1E2',PA='#E6EFE8',
       Selected='#B02C83',Baseline='#507F9A',Boundary='#788084')
u<-function(x)unit(x,'native')
txt<-function(s,x,y,size=7.0,just='left',face='plain',col=INK)
 grid.text(s,u(x),u(y),just=just,gp=gpar(fontfamily=FONT,fontsize=size,fontface=face,col=col))
seg<-function(x1,y1,x2,y2,col=INK,lwd=.6,lty=1)
 grid.segments(u(x1),u(y1),u(x2),u(y2),gp=gpar(col=col,lwd=lwd,lty=lty,lineend='butt'))
box<-function(x,y,w,h,fill,col=NA,lwd=.5,lty=1)
 grid.rect(u(x),u(y),width=u(w),height=u(h),gp=gpar(fill=fill,col=col,lwd=lwd,lty=lty))
legend<-function(s,x,y,fill=NULL,line=NULL,lwd=.8,lty=1,border=NA,size=6.5){
 if(!is.null(fill))box(x+2.75,y,5.5,2.9,fill,border,lty=lty)
 if(!is.null(line))seg(x,y,x+5.5,y,line,lwd*1.15,lty)
 legend_text(s,x+7,y,8.3,width=49)
}

# Fixed illustration windows: chosen to reveal corridor geometry in the central
# and southeastern network. They are not inferred priority zones or buffer areas.
windows<-data.frame(Label=c('b1','b2'),XMin=c(-40000,200000),XMax=c(130000,370000),
                   YMin=c(2790000,2530000),YMax=c(2885000,2625000),
                   Selection='Representative illustration window; no statistical hotspot claim')
write.csv(windows,file.path(output,'Fig10_inset_windows.csv'),row.names=FALSE)

make_map<-function(x,y,w,h,xr,yr,mode='source',insets=FALSE){
 stopifnot(mode %in% c('source','corridor'))
 sc<-min(w/diff(xr),h/diff(yr));mw<-diff(xr)*sc;mh<-diff(yr)*sc
 x0<-x+(w-mw)/2;y0<-y+(h-mh)/2
 pushViewport(viewport(x=u(x0),y=u(y0),width=u(mw),height=u(mh),
                       just=c('left','bottom'),xscale=xr,yscale=yr,clip='on'))
 polygon_layer<-function(d,fill,col=NA,lwd=.3){
   for(k in unique(d$ID)){
     z<-d[d$ID==k,];z<-z[order(z$Part,z$Sequence),]
     if(max(z$X)<xr[1]||min(z$X)>xr[2]||max(z$Y)<yr[1]||min(z$Y)>yr[2])next
     grid.path(u(z$X),u(z$Y),id=match(z$Part,unique(z$Part)),rule='evenodd',
               gp=gpar(fill=fill,col=col,lwd=lwd,linejoin='round'))
   }
 }
 line_layer<-function(d,col,lwd,lty=1){
   # Part numbering restarts within each protection status: never join those
   # separate geometries when drawing a full-layer casing or portfolio overlay.
   status<-if('Status'%in%names(d))d$Status else rep('all',nrow(d))
   for(idx in split(seq_len(nrow(d)),interaction(d$ID,status,d$Part,drop=TRUE))){
     z<-d[idx,];z<-z[order(z$Sequence),]
     if(max(z$X)<xr[1]||min(z$X)>xr[2]||max(z$Y)<yr[1]||min(z$Y)>yr[2])next
     grid.lines(u(z$X),u(z$Y),gp=gpar(col=col,lwd=lwd,lty=lty,lineend='round',linejoin='round'))
   }
 }
 polygon_layer(boundary,'#FBFAF7')
 if(mode=='source'){
   polygon_layer(sources[sources$Critical==0,],COL['Other'])
   polygon_layer(critical[critical$Status=='Unprotected',],COL['Gap'])
 }
 polygon_layer(pa,COL['PA'],COL['Covered'],.45)
 if(mode=='source')
   polygon_layer(critical[critical$Status=='Protected',],COL['Covered'])
 if(mode=='corridor'){
   line_layer(baseline[baseline$Status=='Unprotected',],COL['Baseline'],1.0,1)
   line_layer(baseline[baseline$Status=='Protected',],COL['Covered'],1.0,1)
   # White casing separates selected corridors from baseline lines and PA edges.
   line_layer(selected,'white',3.1)
   line_layer(selected[selected$Status=='Unprotected',],COL['Selected'],1.9,1)
   line_layer(selected[selected$Status=='Protected',],COL['Covered'],1.9,1)
 }
 polygon_layer(boundary,NA,COL['Boundary'],.6)
 popViewport()
 X<-function(z)x0+(z-xr[1])*sc;Y<-function(z)y0+(z-yr[1])*sc
 if(insets)for(i in 1:nrow(windows)){
   r<-windows[i,];xx<-X(r$XMin);yy<-Y(r$YMax)
   grid.rect(u((X(r$XMin)+X(r$XMax))/2),u((Y(r$YMin)+Y(r$YMax))/2),
             width=u((r$XMax-r$XMin)*sc),height=u((r$YMax-r$YMin)*sc),
             gp=gpar(fill=NA,col='#3C4245',lwd=.65,lty=2))
   box(xx+2.8,yy-2.2,5.1,3.9,'white');txt(r$Label,xx+.5,yy-2.2,7,face='bold')
 }
 list(sc=sc,x=x0,y=y0,w=mw,h=mh,mode=mode)
}
scale_bar<-function(x,y,sc,km){
 sw<-km*1000*sc
 # All scale bars sit outside map viewports; no masking rectangle is needed.
 seg(x,y,x+sw,y,lwd=.8);seg(x,y,x,y+1.2);seg(x+sw,y,x+sw,y+1.2)
 txt('0',x,y+3.2,6.3,just='centre');txt(paste(km,'km'),x+sw,y+3.2,6.3,just='centre')
}
draw<-function(){
 grid.newpage();pushViewport(viewport(xscale=c(0,W),yscale=c(0,H)))
 grid.rect(gp=gpar(fill='white',col=NA))
 xr<-range(boundary$X);yr<-range(boundary$Y)
 a<-make_map(3,111,85,95,xr,yr,'source')
 b<-make_map(95,111,85,95,xr,yr,'corridor',TRUE)
 txt('(a)',3,211,8.5,face='bold');txt('(b)',95,211,8.5,face='bold')
 for(xx in c(8,100))draw_north_arrow_grid(xx,194,height=6,width=2.8,label_size=7,family=FONT)
 scale_bar(20,106,a$sc,300);scale_bar(112,106,b$sc,300)
 for(i in 1:2){
   r<-windows[i,];x<-c(4,96)[i]
   # The exact same renderer mode, data layers, order and colours as b.
   m<-make_map(x,53,83,46.38235,c(r$XMin,r$XMax),c(r$YMin,r$YMax),'corridor')
   stopifnot(identical(m$mode,b$mode),identical(a$mode,'source'))
   box(x+41.5,53+46.38235/2,83,46.38235,NA,'#3C4245',.65)
   box(x+3.4,95.9,5.5,4.5,'white');txt(r$Label,x+1,95.9,7.5,face='bold')
   scale_bar(x+5,47.5,m$sc,50)
   stopifnot(47.5+4.5<m$y)
 }
 legend('Critical source outside PA',4,34,fill=COL['Gap'])
 legend('Critical source inside PA',65,34,fill=COL['Covered'])
 legend('Other source',126,34,fill=COL['Other'])
 legend('Selected corridor outside PA',4,23,line=COL['Selected'],lwd=1.9)
 legend('Baseline corridor outside PA',65,23,line=COL['Baseline'],lwd=1.0)
 legend('Corridor inside PA',126,23,line=COL['Covered'],lwd=1.5)
 legend('Protected area (PA)',4,12,fill=COL['PA'],border=COL['Covered'])
 legend('Study-area boundary',65,12,line=COL['Boundary'],lwd=.6)
 legend('Enlargement window',126,12,fill='white',border='#3C4245',lty=2)
 popViewport()
}
cairo_pdf(file.path(output,'Figure10.pdf'),width=W/25.4,height=H/25.4,family=FONT,bg='white'); draw(); dev.off()
