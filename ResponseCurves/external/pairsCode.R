## put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "blue", ...)
}

## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor <- function(x, y, digits=2, prefix="", ...)
{
  cor.range=.7
  cor.mult=4
  a<-colors()
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  spear<-abs(cor(x,y,method="spearman",use="pairwise.complete.obs"))
  ken<- abs(cor(x,y,method="kendall",use="pairwise.complete.obs"))
  all.cor<-max(r,spear,ken)
  ramp<-heat.colors(20, alpha = .7)[20:1]
  if(all.cor>=.6){
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =
           ramp[which.min(abs(all.cor-seq(from=.65,to=1,length=20)))])}
  r<-max(all.cor)
  cex.cor=3*cor.mult
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  #if(missing(cex.cor)) cex.cor <- 1.2/strwidth(txt)
  
  txt2=""
  if(max(all.cor)>cor.range){
    if(spear==max(all.cor) && spear!=cor(x,y,use="pairwise.complete.obs")) {txt2 <- " s"
    } else if(ken==max(all.cor) && ken!=cor(x,y,use="pairwise.complete.obs")){
      txt2 <-" k"
    }
    
  }
  text(0.5, 0.5, txt, cex = .7+cex.cor * (r-min(cor.range))/(max(cor.range)-min(cor.range)))
  text(.9,.1,txt2,cex=cor.mult)
}

panel.smooth<-
function (x, y, resp,col = par("col"), bg = NA, pch = par("pch"), 
          cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...) 
{
  
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
          col = col.smooth, ...)
}


