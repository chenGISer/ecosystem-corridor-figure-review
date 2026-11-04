if (.Platform$OS.type == "windows") invisible(try(Sys.setlocale("LC_CTYPE", "English_United States.utf8"), silent=TRUE))
arg <- grep('^--file=', commandArgs(), value=TRUE)[1]
root <- dirname(dirname(sub('^--file=', '', arg)))
output <- file.path(root,'output','fig6')
dir.create(output, recursive=TRUE, showWarnings=FALSE)
Sys.setenv(AG_LEGEND_OUTPUT=output)
source(file.path(root,'scripts','common.R'))
if (!capabilities('cairo')) stop('A Cairo-enabled R installation is required.')
library(grid)
out <- output
data_out <- output
inputs <- file.path(root,'data','fig6')
curve_path <- file.path(inputs,'NestedRandom_Raw.csv')
loss_path <- file.path(inputs,'NestedRandom_HalfPerformance_ByRepeat.csv')
summary_path <- file.path(inputs,'NestedRandom_HalfPerformance_Summary.csv')
raw <- read.csv(curve_path,stringsAsFactors=FALSE,check.names=FALSE)
raw <- raw[raw$Year==2025 & raw$Strategy=='Random',]
loss <- read.csv(loss_path,stringsAsFactors=FALSE,check.names=FALSE)
audited <- read.csv(summary_path,stringsAsFactors=FALSE)
stopifnot(length(unique(raw$Repeat))==100L,!anyDuplicated(raw[c('Repeat','RemovalFractionActual')]))
stopifnot(all(table(loss$Year)==100L),!anyDuplicated(loss[c('Year','Repeat')]))
loss_cols <- c('DirectSupplyLoss','SurvivingPairDisconnectionLoss','ConnectedPairDetourLoss')
stopifnot(max(abs(rowSums(loss[loss_cols])-.5))<1e-10)
stopifnot(max(abs(raw$PairwiseAccessNormalized+raw$DirectSupplyLoss+raw$AdditionalPathLoss-1))<1e-10)
stopifnot(max(abs(raw$SupplyOnlyReference+raw$DirectSupplyLoss-1))<1e-10)

metrics <- c('DemandSideConnectivity','PairwiseAccessNormalized','NearestSupplyAccessNormalized','SupplyOnlyReference')
curve_summary <- do.call(rbind,lapply(metrics,function(m) {
  do.call(rbind,lapply(sort(unique(raw$RemovalFractionActual)),function(f) {
    v <- raw[raw$RemovalFractionActual==f,m]
    # Corrected simulations store endpoints for every nested trajectory.
    stopifnot(length(v)==100L,
              all(is.finite(v)),all(v>=-1e-12 & v<=1+1e-12))
    data.frame(Year=2025,Metric=m,RemovalFraction=f,n=length(v),Mean=mean(v),
               Q025=unname(quantile(v,.025)),Q975=unname(quantile(v,.975)))
  }))
}))
loss_summary <- aggregate(loss[loss_cols],list(Year=loss$Year),mean)
loss_summary <- loss_summary[order(loss_summary$Year),]
loss_intervals <- do.call(rbind,lapply(loss_summary$Year,function(y) {
  do.call(rbind,lapply(loss_cols,function(m) {
    v <- loss[loss$Year==y,m]*100
    data.frame(Year=y,Component=m,n=length(v),Mean=mean(v),
               Q025=unname(quantile(v,.025)),Q975=unname(quantile(v,.975)))
  }))
}))
for(i in seq_len(nrow(loss_intervals))) {
  z <- loss_intervals[i,]
  a <- audited[audited$Year==z$Year & audited$Metric==z$Component,]
  stopifnot(nrow(a)==1L,max(abs(unlist(z[c('Mean','Q025','Q975')])-100*unlist(a[c('Mean','Q025','Q975')])))<1e-10)
}
box_summary <- do.call(rbind,lapply(loss_summary$Year,function(y) {
  do.call(rbind,lapply(loss_cols,function(m) {
    v <- loss[loss$Year==y,m]*100
    q <- unname(quantile(v,c(.25,.5,.75),type=7))
    iq <- q[3]-q[1]
    inner <- v[v>=q[1]-1.5*iq & v<=q[3]+1.5*iq]
    data.frame(Year=y,Component=m,n=length(v),Mean=mean(v),Q1=q[1],Median=q[2],Q3=q[3],
               LowerWhisker=min(inner),UpperWhisker=max(inner),
               OutlierCount=sum(v<min(inner) | v>max(inner)))
  }))
}))
stopifnot(all(box_summary$LowerWhisker<=box_summary$Q1),
          all(box_summary$Q1<=box_summary$Median),
          all(box_summary$Median<=box_summary$Q3),
          all(box_summary$Q3<=box_summary$UpperWhisker))
write.csv(box_summary,file.path(data_out,'Figure6b_LossComponents_BoxplotStatistics_pp.csv'),row.names=FALSE)
for (i in seq_len(nrow(loss_summary))) for(m in loss_cols) {
  check <- audited$Mean[audited$Year==loss_summary$Year[i] & audited$Metric==m]
  stopifnot(length(check)==1L,abs(check-loss_summary[i,m])<1e-12)
}
write.csv(curve_summary,file.path(data_out,'Figure6a_ResponseCurves_2025.csv'),row.names=FALSE)
write.csv(loss_summary,file.path(data_out,'Figure6b_LossComponents_Fraction.csv'),row.names=FALSE)
file.copy(loss_path,file.path(data_out,'Figure6b_TrajectorySpecificHalfPerformance.csv'),overwrite=TRUE)
write.csv(raw[c('Year','Repeat','RemovalFractionActual',metrics)],
          file.path(data_out,'Figure6a_RandomTrajectories_2025.csv'),row.names=FALSE)

W <- 183; H <- 88
FONT <- 'Times New Roman'
INK <- '#222222'
MC <- c(DemandSideConnectivity='#348362',PairwiseAccessNormalized='#3579AD',
        NearestSupplyAccessNormalized='#8764A6',SupplyOnlyReference='#777777')
LC <- c(DirectSupplyLoss='#7F878E',SurvivingPairDisconnectionLoss='#B96C63',ConnectedPairDetourLoss='#C59A4C')
LFILL <- c('#C2C8CE','#D9988F','#E2BE7B')
u <- function(x) unit(x,'native')
txt <- function(s,x,y,size=8,face='plain',just='left',col=INK,rot=0) {
  grid.text(s,u(x),u(y),just=just,rot=rot,
            gp=gpar(fontfamily=FONT,fontsize=size,fontface=face,col=col))
}
seg <- function(x1,y1,x2,y2,col=INK,lwd=.55,lty=1) {
  grid.segments(u(x1),u(y1),u(x2),u(y2),gp=gpar(col=col,lwd=lwd,lty=lty))
}
legend_line <- function(s,x,y,m,pch=NA,lty=1) {
  seg(x,y,x+6,y,MC[m],1.55,lty)
  if(!is.na(pch)) grid.points(u(x+3),u(y),pch=pch,size=unit(1.6,'mm'),gp=gpar(col=MC[m],fill=MC[m]))
  legend_text(s,x+7.5,y,8.3,width=32)
}
draw_plot <- function() {
  grid.newpage(); pushViewport(viewport(xscale=c(0,W),yscale=c(0,H)))
  grid.rect(gp=gpar(fill='white',col=NA))
  txt('(a)',14,84,8.5,'bold')
  txt('(b)',107,84,8.5,'bold')
  legend_line('Binary connectivity',7,12.5,'DemandSideConnectivity',16)
  legend_line('Pairwise accessibility',51,12.5,'PairwiseAccessNormalized',15)
  legend_line('Nearest-supply accessibility',7,4.8,'NearestSupplyAccessNormalized',17)
  legend_line('Supply-removal reference',51,4.8,'SupplyOnlyReference',NA,2)
  loss_labels <- c('Direct supplier loss','Surviving-pair disconnection','Connected-pair detour')
  legend_x <- c(102,144,102)
  legend_y <- c(12.5,12.5,4.8)
  for(j in 1:3) {
    xx <- legend_x[j]; yy <- legend_y[j]
    grid.rect(u(xx+3),u(yy),u(6),u(3.1),gp=gpar(fill=LFILL[j],col=LC[j],lwd=.6))
    legend_text(loss_labels[j],xx+7.5,yy,8.3,width=31)
  }
  # Panel a. Actual removal fractions; ribbon is an empirical trajectory interval.
  ax <- 15; ay <- 27; aw <- 71; ah <- 53
  X <- function(v) ax+v*aw
  Y <- function(v) ay+v*ah
  for(v in seq(.2,1,.2)) seg(ax,Y(v),ax+aw,Y(v),'#ECECEC',.3)
  seg(ax,Y(.5),ax+aw,Y(.5),'#B9B9B9',.5,3)
  p <- curve_summary[curve_summary$Metric=='PairwiseAccessNormalized',]
  grid.polygon(u(X(c(p$RemovalFraction,rev(p$RemovalFraction)))),
               u(Y(c(p$Q025,rev(p$Q975)))),
               gp=gpar(col=NA,fill=adjustcolor(MC['PairwiseAccessNormalized'],alpha.f=.12)))
  for(m in c('SupplyOnlyReference','DemandSideConnectivity','NearestSupplyAccessNormalized','PairwiseAccessNormalized')) {
    z <- curve_summary[curve_summary$Metric==m,]
    grid.lines(u(X(z$RemovalFraction)),u(Y(z$Mean)),
               gp=gpar(col=MC[m],lwd=if(m=='PairwiseAccessNormalized')1.7 else 1.35,
                       lty=if(m=='SupplyOnlyReference')2 else 1))
    if(m!='SupplyOnlyReference') {
      ids <- unique(vapply(seq(.2,.8,.2),function(v)which.min(abs(z$RemovalFraction-v)),integer(1)))
      shape <- c(DemandSideConnectivity=16,PairwiseAccessNormalized=15,NearestSupplyAccessNormalized=17)[m]
      grid.points(u(X(z$RemovalFraction[ids])),u(Y(z$Mean[ids])),pch=shape,
                  size=unit(1.2,'mm'),gp=gpar(col=MC[m],fill=MC[m]))
    }
  }
  seg(ax,ay,ax+aw,ay);seg(ax,ay,ax,ay+ah)
  for(v in seq(0,1,.2)) {
    seg(X(v),ay,X(v),ay-1.25);txt(sprintf('%d',round(v*100)),X(v),ay-3.1,7.4,just='centre')
    seg(ax-1.25,Y(v),ax,Y(v));txt(sprintf('%.1f',v),ax-2.2,Y(v),7.4,just='right')
  }
  txt('Supply nodes removed (%)',ax+aw/2,ay-8.2,8,just='centre')
  txt('Relative performance',3.2,ay+ah/2,8,just='centre',rot=90)
  # Panel b. Type-7 quartiles, median, 1.5-IQR whiskers and all outlying observations.
  bx <- 108; by <- ay; bw <- 71; bh <- ah
  # Include every observation; derive the upper axis limit from the raw values.
  bmax <- ceiling(max(as.matrix(loss[loss_cols]))*100/10)*10
  # Display padding below zero separates zero-valued whiskers and outliers
  # from the x-axis. All ticks and loss observations remain non-negative.
  bmin <- -2
  brange <- bmax-bmin
  BY <- function(v) by+(v-bmin)/brange*bh
  centers <- bx+bw*c(.13,.377,.623,.87)
  for(v in seq(10,bmax,10)) seg(bx,BY(v),bx+bw,BY(v),'#ECECEC',.3)
  for(i in 1:4) {
    for(j in 1:3) {
      z <- box_summary[box_summary$Year==loss_summary$Year[i] & box_summary$Component==loss_cols[j],]
      xx <- centers[i]+c(-4.1,0,4.1)[j]
      halfwidth <- 1.65
      seg(xx,BY(z$LowerWhisker),xx,BY(z$UpperWhisker),LC[j],.8)
      seg(xx-1.1,BY(z$LowerWhisker),xx+1.1,BY(z$LowerWhisker),LC[j],.8)
      seg(xx-1.1,BY(z$UpperWhisker),xx+1.1,BY(z$UpperWhisker),LC[j],.8)
      grid.rect(u(xx),u(BY((z$Q1+z$Q3)/2)),u(halfwidth*2),u((z$Q3-z$Q1)/brange*bh),
                  gp=gpar(fill=LFILL[j],col=LC[j],lwd=.8))
      seg(xx-halfwidth,BY(z$Median),xx+halfwidth,BY(z$Median),INK,1)
      values <- loss[loss$Year==loss_summary$Year[i],loss_cols[j]]*100
      outside <- values[values<z$LowerWhisker | values>z$UpperWhisker]
      if(length(outside)) grid.points(u(rep(xx,length(outside))),u(BY(outside)),pch=1,
                                      size=unit(1,'mm'),gp=gpar(col=LC[j],lwd=.6))
    }
    seg(centers[i],by,centers[i],by-1.25)
    txt(loss_summary$Year[i],centers[i],by-3.1,7.4,just='centre')
  }
  seg(bx,by,bx+bw,by);seg(bx,by,bx,by+bh)
  for(v in seq(0,bmax,10)) {
    seg(bx-1.25,BY(v),bx,BY(v));txt(v,bx-2.2,BY(v),7.4,just='right')
  }
  txt('Year',bx+bw/2,by-8.2,8,just='centre')
  txt('Loss of baseline pairwise access (pp)',96,by+bh/2,8,just='centre',rot=90)
  popViewport()
}

cairo_pdf(file.path(output,'Figure6.pdf'),width=W/25.4,height=H/25.4,family=FONT,bg='white'); draw_plot(); dev.off()
