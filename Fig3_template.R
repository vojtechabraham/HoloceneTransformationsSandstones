library("xlsx")
library("pgirmess")
tbl <- read.xlsx("Fig3_random_data.xlsx", sheetIndex = 1)[1:127,]
tbl <- tbl[order(tbl$NA.),-1]
barplot(t(as.matrix((tbl[,3:19]))), horiz = T)
bb <- read.table("col.txt", sep=";", stringsAsFactors = F)[-18,]



tiff("Fig3_template.tiff", compression = "lzw", height = 1000, width=1600, pointsize = 30)

nf <- layout(t(matrix( 1:4,byrow = T)), width=c(4,1,1,2), height=c(5))
layout.show(nf)

par(mar=c(5,8,4,1))
ee <- barplot(t(as.matrix((tbl[,3:19]))), horiz = T, col=bb$V2,border = "transparent" , main="", names.arg=rep("",127),las=2, cex.names = 0.5, axes = F)
title(main="Species composition (%)", line=3)
axis(3, at=seq(0,100, 20), labels=seq(0,100, 20), las=2, line = -1)
axis(2, at=c(ee[!is.na(tbl$axis)]-0.6 ), labels=c(tbl[!is.na(tbl$axis),25]), las=2, line = 0, tick=T, font = 1, xpd=T, cex.axis=0.7, col = "transparent", col.ticks = "black")
#axis(2, at=ee[seq(1,127,2)] , labels=tbl[seq(1,127,2),2], las=2, line = 0, tick=F, font = 1, xpd=T, cex.axis=0.7)
text(2,175, "Depth (cm)", font=2, adj=1, cex=0.7, xpd=T)

ag <- aggregate( ee, by=list(as.character(tbl[,1])), FUN=mean)
axis(2, at=c(ag$x,175), labels=c(ag$Group.1, "Site"), las=2, line = 2, tick=F, font = 2, xpd=T)
abline(h=ee[which(is.na(tbl[,1]))], lty=2, xpd=T)
rect(ybottom = -50,ytop=190,xleft = -100,xright = -22, border = "transparent", xpd=T, col = "white")
aa <- aggregate( ee, by=list(tbl$labl, tbl$labl2), FUN=mean)
axis(2, at=c(aa$x,170), labels=c(as.character(aa$Group.1), "Location  "), las=0, line = 4.5, tick=F, font = 2, xpd=T)
abline(h=ee[which(is.na(tbl$labl2))], lty=2, xpd=T)

rect(ybottom = -50,ytop=190,xleft = -100,xright = -27, border = "transparent", xpd=T, col = "white")
al <- aggregate( ee, by=list(tbl$NA..1), FUN=mean)
axis(2, at=c(al$x,175), labels=c(as.character(al$Group.1), "Area      "), las=0, line = 6, tick=F, font = 2, xpd=T)
segments(x0=-50, x1=-27, y0=ee[which(is.na(tbl$NA..1))], lty=2, xpd=T)


points(x=seq(2,98,96/16),y=rep(-25,17), pch=15,cex=3, col=bb$V2, xpd=T)
text(x=seq(2,98,96/16),y=rep(-20,17),labels= bb$V1, srt=90, adj=0, xpd=T, font=3)


par(mar=c(5,0,4,1))
barplot(t(as.matrix((tbl[,20]))), horiz = T, col="black",axes=F,border = "transparent", main="" )
title(main="SAL (mg/kg)", line=3)
axis(3, at=seq(0,12000, 2000), labels=seq(0,12000, 2000), las=2, line = -1)
abline(h=ee[which(is.na(tbl[,1]))], lty=2, xpd=T)

ee <- barplot(t(as.matrix((tbl[,21]))), horiz = T, col="black",axes=F,border = "transparent", main="" )
title(main="Charc.analyzed", line=3)
axis(3, at=seq(0,350, 50), labels=seq(0,350, 50), las=2, line = -1)
abline(h=ee[which(is.na(tbl[,1]))], lty=2, xpd=T)
text(450,-13, "Fire density:", font=2, adj=1, cex=1, xpd=T)

plot(tbl[,24]+((tbl[,23]- tbl[,24])/2), ee , col="black",pch=16, las=2, bty="n", axes=F, ylab = "", xlab = "", main="")
pdv <- !(tbl$Lab..code. %in% c("DeA-13717","Poz-97501"))
text(tbl[pdv,24]+((tbl[pdv,23]- tbl[pdv,24])/2), ee[pdv] , tbl[pdv,22], adj=-0.1, xpd=T, cex = 0.7)
text(tbl[c(106,76),24]+((tbl[c(106,76),23]- tbl[c(106,76),24])/2), ee[c(106,76)] , tbl[c(106,76),22], adj=1.1, xpd=T, cex = 0.7)
title(main="14C Age (cal BP)", line=3)
segments(x0=tbl[,23], x1=tbl[,24], y0=ee,y1=ee , col="black" )
axis(3, at=seq(0,12000, 2000), labels=seq(0,12000, 2000), las=2, line = -1)
abline(h=ee[which(is.na(tbl[,1]))], lty=2, xpd=T)
abline(v=seq(0,12000, 2000),  lty=2)
abline(v=0 )
rect(xleft = 0, ybottom = -20, xright = 12500, ytop = -10, xpd=T)
fir <- tbl[,24]+((tbl[,23]- tbl[,24])/2)
fir <- fir[!is.na(fir)]
segments(x0=fir,x1=fir, y0=rep(-20,20), y1=rep(-10, 20), col="red", xpd=T)
axis(1, at=seq(0,12000, 4000), labels=seq(0,12000, 4000), line = 3, xpd=T)

dev.off()

