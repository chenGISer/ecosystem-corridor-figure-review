if (.Platform$OS.type == "windows") invisible(try(Sys.setlocale("LC_CTYPE", "English_United States.utf8"), silent=TRUE))
arg <- grep('^--file=', commandArgs(), value=TRUE)[1]
root <- dirname(dirname(sub('^--file=', '', arg)))
output <- file.path(root,'output','fig8')
dir.create(output, recursive=TRUE, showWarnings=FALSE)
Sys.setenv(AG_LEGEND_OUTPUT=output)
source(file.path(root,'scripts','common.R'))
if (!capabilities('cairo')) stop('A Cairo-enabled R installation is required.')
library(grid)
src <- file.path(root,'data','fig8')
out <- output
rd <- function(n) read.csv(file.path(src,n),stringsAsFactors=FALSE)
source(file.path(root,'scripts','fig8_north_arrow.R'))
nodes<-rd('Map_Nodes.csv');base<-rd('Map_ExistingCorridors.csv')
boundary<-rd('Map_Boundary.csv');changes<-rd('Map_RevisedSelection.csv')
endpoints<-rd('Spatial_SelectionChanges.csv')
curves<-rd('Revised_Portfolio_Curves.csv')
auc<-rd('Revised_Portfolio_AUC_Summary.csv')
scenarios<-c('Fixed','BaselineAdaptive','PortfolioAdaptive','Random')
keys<-c('Scenario','Repeat','Step','RemovalFractionRequested','RemovalCount','RemovalFractionActual')
keep<-c(keys,'Pairwise_CommonBaseline')
ex<-curves[curves$Portfolio=='Exact',keep]
gu<-curves[curves$Portfolio=='Guided',keep]
stopifnot(!anyDuplicated(ex[keys]),!anyDuplicated(gu[keys]))
d<-merge(ex,gu,by=keys,suffixes=c('_Exact','_Guided'),all=TRUE)
stopifnot(nrow(d)==2163,!anyNA(d))
d$Difference<-d$Pairwise_CommonBaseline_Guided-d$Pairwise_CommonBaseline_Exact
d<-d[order(match(d$Scenario,scenarios),d$Repeat,d$Step),]
hm<-do.call(rbind,lapply(scenarios,function(s){
 do.call(rbind,lapply(0:20,function(k){
  z<-d[d$Scenario==s & d$Step==k,]
  stopifnot(nrow(z)==if(s=='Random')100 else 1,
            length(unique(z$RemovalFractionRequested))==1)
  data.frame(Scenario=s,Step=k,RemovalFractionRequested=z$RemovalFractionRequested[1],
    RemovalCount=z$RemovalCount[1],RemovalFractionActual=z$RemovalFractionActual[1],
    N_sequences=nrow(z),Mean_Exact=mean(z$Pairwise_CommonBaseline_Exact),
    Mean_Guided=mean(z$Pairwise_CommonBaseline_Guided),Difference=mean(z$Difference),
    Difference_Q025=unname(quantile(z$Difference,.025)),
    Difference_Q975=unname(quantile(z$Difference,.975)))
 }))
}))
stopifnot(nrow(hm)==84,!anyNA(hm),max(abs(hm$Difference))<.04,nrow(nodes)==139)
auc_check<-do.call(rbind,lapply(scenarios,function(s){
 z<-hm[hm$Scenario==s,];z<-z[order(z$Step),]
 integrate<-function(y)sum(diff(z$RemovalFractionRequested)*(head(y,-1)+tail(y,-1))/2)
 a<-auc[auc$Scenario==s & auc$Metric=='Pairwise_CommonBaseline',]
 ea<-a$Mean[a$Portfolio=='Exact'];ga<-a$Mean[a$Portfolio=='Guided']
 id<-integrate(z$Difference)
 stopifnot(abs(integrate(z$Mean_Exact)-ea)<1e-12,
           abs(integrate(z$Mean_Guided)-ga)<1e-12,abs(id-(ga-ea))<1e-12)
 data.frame(Scenario=s,AUC_Exact=ea,AUC_Guided=ga,
   AUC_Difference=ga-ea,Integral_of_stage_differences=id,
   Minimum_stage_difference=min(z$Difference),Maximum_stage_difference=max(z$Difference))
}))
write.csv(hm,file.path(out,'Figure8b_stage_difference.csv'),row.names=FALSE)
write.csv(d,file.path(out,'Figure8b_all_sequence_differences.csv'),row.names=FALSE)
write.csv(auc_check,file.path(out,'Figure8b_AUC_crosscheck.csv'),row.names=FALSE)

MAP<-c(Existing='#C1CDD1',Retained='#658F87',Removed='#3C76A7',Added='#C17D39',Supply='#668998',Demand='#A56CA6')
FONT<-'Times New Roman';INK<-'#222222'
u<-function(x)unit(x,'native')
txt<-function(s,x,y,size=7.2,face='plain',just='left',rot=0,col=INK)
 grid.text(s,u(x),u(y),just=just,rot=rot,
   gp=gpar(fontfamily=FONT,fontsize=size,fontface=face,col=col,lineheight=1.08))
seg<-function(x1,y1,x2,y2,col=INK,lwd=.55,lty=1)
 grid.segments(u(x1),u(y1),u(x2),u(y2),gp=gpar(col=col,lwd=lwd,lty=lty,lineend='round'))
pt<-function(x,y,pch,col,size=1.4)
 grid.points(u(x),u(y),pch=pch,size=unit(size,'mm'),gp=gpar(col=col,fill=col,lwd=.4))
source(file.path(root,'scripts','fig8_map_panel.R'))

map_legend<-function(){
 xs<-c(8,51,8,51);ys<-c(24,24,18,18)
 for(i in 1:4){type<-c('Existing','Retained','Removed','Added')[i]
  seg(xs[i]-2,ys[i],xs[i]+4,ys[i],MAP[type],if(i==1).9 else 1.9,if(type=='Removed')'33'else 1)
  txt(c('Existing corridor','Retained corridor','Replaced corridor','Added corridor')[i],xs[i]+6,ys[i],8.5)
 }
 pt(8.5,12,16,MAP['Supply'],1.9);txt('Supply node',14,12,8.5)
 pt(51.5,12,17,MAP['Demand'],2.2);txt('Demand node',57,12,8.5)
 grid.points(u(8.5),u(6),pch=1,size=unit(2.5,'mm'),gp=gpar(col='#656565',lwd=.7))
 txt('Changed endpoint',14,6,8.5)
}

# Conventional response plot: the original 21 computed stages, joined by lines.
SCOL<-c(Fixed='#3C76A7',BaselineAdaptive='#C17D39',PortfolioAdaptive='#41484E',Random='#6D9188')
SLTY<-c(Fixed='solid',BaselineAdaptive='44',PortfolioAdaptive='1343',Random='22')
response_panel<-function(){
 ax<-111;ay<-46;aw<-66;ah<-72;lo<--.04;hi<-.025
 X<-function(v)ax+aw*v
 Y<-function(v)ay+ah*(v-lo)/(hi-lo)
 for(v in c(-.04,-.02,0,.02)){
  if(v==0)seg(ax,Y(v),ax+aw,Y(v),'#AAAAAA',.6)
  else seg(ax,Y(v),ax+aw,Y(v),'#EBEBEB',.4)
  seg(ax-1.2,Y(v),ax,Y(v),lwd=.5)
  txt(if(v==0)'0'else sprintf('%.2f',v),ax-2.5,Y(v),8.2,just='right')
 }
 for(i in 1:4){
  s<-scenarios[i];z<-hm[hm$Scenario==s,];z<-z[order(z$Step),]
  stopifnot(min(z$Difference)>=lo,max(z$Difference)<=hi)
  grid.lines(u(X(z$RemovalFractionRequested)),u(Y(z$Difference)),
    gp=gpar(col=SCOL[s],lwd=1.45,lty=SLTY[s],linejoin='round',lineend='round'))
 }
 seg(ax,ay,ax+aw,ay,lwd=.65);seg(ax,ay,ax,ay+ah,lwd=.65)
 for(v in seq(0,1,.25)){
  seg(X(v),ay,X(v),ay-1.3,lwd=.55)
  txt(as.character(v*100),X(v),ay-4.7,8.4,just='centre')
 }
 txt('Supply nodes removed (%)',ax+aw/2,ay-12,8.9,just='centre')
 txt('Pairwise accessibility difference',97.5,ay+ah/2,9,just='centre',rot=90)
 xs<-c(109,150,109,150);ys<-c(22,22,11,11)
 labs<-c('Fixed\norder','Shared\nadaptive','Own\nadaptive','Random\n(mean)')
 for(i in 1:4){
  seg(xs[i]-3,ys[i],xs[i]+3,ys[i],SCOL[scenarios[i]],1.6,SLTY[scenarios[i]])
  txt(labs[i],xs[i]+5.5,ys[i],8.6)
 }
}

draw_main<-function(){
 grid.newpage();pushViewport(viewport(xscale=c(0,183),yscale=c(0,132)))
 grid.rect(gp=gpar(col=NA,fill='white'))
 txt('(a)',2,128,8.5,'bold');txt('(b)',96,128,8.5,'bold')
 map_panel();map_legend();response_panel();popViewport()
}
draw_b<-function(){
 grid.newpage();pushViewport(viewport(xscale=c(92,184),yscale=c(6,131)))
 grid.rect(gp=gpar(fill='white',col=NA));txt('(b)',96,128,8.5,'bold')
 response_panel();popViewport()
}
cairo_pdf(file.path(output,'Figure8.pdf'),width=183/25.4,height=132/25.4,family=FONT,bg='white'); draw_main(); dev.off()
