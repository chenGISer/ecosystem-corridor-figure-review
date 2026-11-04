if (.Platform$OS.type == "windows") invisible(try(Sys.setlocale("LC_CTYPE", "English_United States.utf8"), silent=TRUE))
arg <- grep('^--file=', commandArgs(), value=TRUE)[1]
root <- dirname(dirname(sub('^--file=', '', arg)))
output <- file.path(root,'output','fig9')
dir.create(output, recursive=TRUE, showWarnings=FALSE)
Sys.setenv(AG_LEGEND_OUTPUT=output)
source(file.path(root,'scripts','common.R'))
if (!capabilities('cairo')) stop('A Cairo-enabled R installation is required.')
library(grid)
source(file.path(root,'scripts','map_north_arrow.R'))
rd <- function(n) read.csv(file.path(root,'data','maps',n),fileEncoding='UTF-8-BOM',stringsAsFactors=FALSE)
B <- rd('Map_Boundary.csv'); N <- rd('Map_Nodes.csv'); E <- rd('Map_ExistingCorridors.csv')
S <- rd('Map_GuidedCorridorProtection.csv'); D <- rd('Node_Criticality_Change.csv')
stopifnot(length(unique(S$ID))==15,sum(D$AfterRank<=30)==30)
N<-merge(N,D,by='NetworkID',all.x=TRUE,sort=FALSE)
FONT<-'Times New Roman';INK<-'#222222';W<-183;H<-130
C<-c(Supply='#377BB5',Demand='#9C55AA',Critical='#C56B53',Other='#B7C8D3',Selected='#B02C83',
      Baseline='#94ABB7',Persistent='#434E55',Entered='#C56B53',Exited='#808080')
u<-function(x)unit(x,'native')
txt<-function(s,x,y,size=7,just='left',face='plain')grid.text(s,u(x),u(y),just=just,gp=gpar(fontfamily=FONT,fontsize=size,fontface=face,col=INK))
line<-function(x,y,xx,yy,col,lwd=1,lty=1)grid.segments(u(x),u(y),u(xx),u(yy),gp=gpar(col=col,lwd=lwd,lty=lty))
pt<-function(x,y,pch=21,fill='white',col=INK,sz=2.4,lwd=.7)grid.points(u(x),u(y),pch=pch,size=unit(sz,'mm'),gp=gpar(fill=fill,col=col,lwd=lwd))
legend<-function(label,x,y,kind,col,fill=col,pch=21,sz=2.9){
 if(kind=='line')line(x,y,x+5.5,y,col,if(col==C['Selected'])1.9 else 1.0)else pt(x+2.75,y,pch,fill,col,sz)
 legend_text(label,x+7,y,8.5,width=37)
}
drawmap<-function(x,mode){
 xr<-range(B$X);yr<-range(B$Y);sc<-min(85/diff(xr),91/diff(yr))
 X<-function(v)x+(85-diff(xr)*sc)/2+(v-xr[1])*sc
 Y<-function(v)36+(91-diff(yr)*sc)/2+(v-yr[1])*sc
 for(k in unique(B$ID)){
  z<-B[B$ID==k,];grid.path(u(X(z$X)),u(Y(z$Y)),id=match(z$Part,unique(z$Part)),rule='evenodd',gp=gpar(fill='#FBFAF7',col='#838C91',lwd=.55))
 }
 lines<-function(d,col,lwd){
  status<-if('Status'%in%names(d))d$Status else rep('all',nrow(d))
  for(ix in split(seq_len(nrow(d)),interaction(d$ID,status,d$Part,drop=TRUE))){z<-d[ix,];z<-z[order(z$Sequence),];grid.lines(u(X(z$X)),u(Y(z$Y)),gp=gpar(col=col,lwd=lwd))}
 }
 lines(E,C['Baseline'],.65)
 lines(S,'white',2.5);lines(S,C['Selected'],1.5)
 q<-N[N$NodeType=='Demand',];pt(X(q$X),Y(q$Y),24,C['Demand'],NA,1.9)
 q<-N[N$NodeType=='Supply',]
 if(mode=='after'){
  a<-q[q$AfterRank>30,];pt(X(a$X),Y(a$Y),21,C['Other'],'white',1.8,.3)
  a<-q[q$AfterRank<=30,];a<-a[order(a$After),]
  pt(X(a$X),Y(a$Y),21,C['Critical'],'#713D32',3.2,.65)
 }else{
  a<-q[q$Status=='Other',];pt(X(a$X),Y(a$Y),21,'#DCE5E8',NA,1.6)
  a<-q[q$Status=='Persistent',];pt(X(a$X),Y(a$Y),21,NA,C['Persistent'],3.0,1)
  a<-q[q$Status=='Entered',];pt(X(a$X),Y(a$Y),21,C['Entered'],'#713D32',3.2,.7)
  a<-q[q$Status=='Exited',];pt(X(a$X),Y(a$Y),4,NA,C['Exited'],3.2,1.5)
 }
 draw_north_arrow_grid(x+5,113,6,2.8,7,FONT)
 sw<-300000*sc;xx<-x+17;yy<-31.5
 line(xx,yy,xx+sw,yy,INK,.8);line(xx,yy,xx,yy+1,INK,.6);line(xx+sw,yy,xx+sw,yy+1,INK,.6)
 txt('0',xx,yy+3,6.5,'centre');txt('300 km',xx+sw,yy+3,6.5,'centre')
}
draw<-function(){
 grid.newpage();pushViewport(viewport(xscale=c(0,W),yscale=c(0,H)));grid.rect(gp=gpar(fill='white',col=NA))
 drawmap(3,'after');drawmap(95,'change');txt('(a)',3,127,8.5,face='bold');txt('(b)',95,127,8.5,face='bold')
 legend('Top-30 access-critical source',4,23,'point',C['Critical'])
 legend('Other supply source',49,23,'point',C['Other'])
 legend('Persistent top-30 source',96,23,'point',C['Persistent'],fill=NA)
 legend('Entered top 30',141,23,'point',C['Entered'])
 legend('Exited top 30',96,16.5,'point',C['Exited'],fill=NA,pch=4)
 legend('Other supply source',141,16.5,'point','#DCE5E8')
 legend('Demand node',19,5.5,'point',C['Demand'],pch=24)
 legend('Selected corridor',76,5.5,'line',C['Selected'])
 legend('Baseline corridor',133,5.5,'line',C['Baseline'])
 popViewport()
}
cairo_pdf(file.path(output,'Figure9.pdf'),width=W/25.4,height=H/25.4,family=FONT,bg='white'); draw(); dev.off()
