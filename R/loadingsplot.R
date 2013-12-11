#' @title loadings plot
#' @description plots of variables (loadings)
#' @param x results of the proposed multi-group methods in the package
#' @param axes a vector of two selected components 
#' @param cex character expansion for text by default .85
#' @param font.lab type of font by default 3
#' @return loadings plot
#' @export
#' 
#'  
#' @examples
#' Data = iris[,-5]
#' Group = iris[,5]
#' res.mgPCA = mgPCA (Data, Group, graph=TRUE)
#' loadingsplot(res.mgPCA, axes=c(1,2))
loadingsplot <- function(x, axes=c(1,2), cex=NULL, font.lab= NULL){
  #=========================================================================
  #                       Preparing inputs
  #=========================================================================
  AA = x$loadings.common
  if (max(axes)>ncol(AA))
   stop("\nOops one of the axes value is greater than number of existing dimensions")
  
       
  if(is.null(rownames(AA))) {
    rownames(AA) = paste('V', 1:nrow(AA), sep='')
  }
  
  if(is.null(cex)) {cex = .85}
  if(is.null(font.lab)) {font.lab = 3}
  
  xax=axes[1]
  yax=axes[2]
  lab.x <- paste("Dim ", axes[1],sep = "")
  lab.y <- paste("Dim ", axes[2],sep = "")
  #=========================================================================
  #                            loadings plot
  #=========================================================================
  AA = x$loadings.common
  PQ = nrow(AA)
  w1 = AA[,xax]
  w1 = matrix(w1, ncol=1)
  w2 = AA[,yax]
  w2 = matrix(w2, ncol=1)
  vv = c(rep(0,PQ))
  uu = c(rep(0,PQ))
  minlimx   <- min(c(w1,w2))
  maxlimx   <- max(c(w1,w2))
  lim = c(minlimx, maxlimx)
  plot(w1,w2, type="n",ylim=lim ,xlim=lim ,xlab =lab.x, 
       ylab=lab.y,main="Loading plot",asp= 1)
  abline(h = 0, v = 0)
  www = cbind(w1,w2)
  text(www,labels=rownames(AA), cex=cex, font.lab= font.lab) 
}
