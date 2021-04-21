## R-code for producing plots of Richard Prices examples
## in Bayes's original paper
## Bayes, Mr; Price, Mr (1763).
## "An Essay towards Solving a Problem in the Doctrine of Chances.
## Philosophical Transactions of the Royal Society of London. Vol. 53
## p. 401 ff.


## Beta CDF wrapper for
## for x (probability),
## k (failures)
## n (successes)
knpbeta <- function(x,k,n) {
    pbeta(x,k+1,n-k+1)
}

## Beta distribution wrapper for
## for x, k, n

kndbeta <- function(x,k,n) {
    dbeta(x,k+1,n-k+1)
}

## Intervals between which probabilities
## are to be estimated in the examples
pLo <- 1 - 11/12
pHi <- 1- 9/10

## Ranges for x and y axes of subplots
xLim <- c(0.0,0.21)
yLim <- c(-0.3,10.9)

## k (failures) and
## n (successes) of the examples
k <- c(1,2,100,4)
n <- k*11

## Color palette
cols <- colorRampPalette(RColorBrewer::brewer.pal(4,"Spectral"))(4)

##' Draw one of three subplots
##'
##' ... using only R-base graphics. The
##' function uses global variables (ugly)
##' @title draw on subplot
##' @param i subplot index, at the same time index
##' into number k/n vectors above
##' @param dax draw axis, if "n" no axis is drawn
##' @return nothing
##' @author Fredrik wartenberg, 2021

oneCurve <- function(i,dax="n") {

    if(dax=="n")
        plt <- c(0,1,0.06482166,0.94788847)
    else
        plt <- c(0.08118,1,0.06482166,0.94788847)
    par("plt"=plt)

    ## The curve
    curve(
        ## function (beta distribution)
        kndbeta(x,
                k[i],
                n[i]),
        from=xLim[1],
        to=xLim[2],

        ## Line color and width
        lwd=4,
        col=cols[i],

        # Box and axis
        bty="n",

        yaxt=dax,
        ylim=yLim,
        xlab="p of success",
        ylab="",
        cex.lab=2,
        cex.axis=2
    )

    ## Box around subplot
    box(lwd=4)

    ## Y label
    if(dax=="s"){
        text(x=0,y=(yLim[2]-yLim[1])/3,labels="PDF",cex=2,srt=90)
    }


    ## calculate area (total probability from the examples)
    area=round(knpbeta(pHi,k[i],n[i])-knpbeta(pLo,k[i],n[i]),3)

    ## Place Text (area)
    text(x=pHi+0.01,
         y=kndbeta(pHi,k[i],n[i])/1,
         adj=c(0,0),
         labels=paste0("p(pLow<p<pHigh)=",area),
         cex=1.8)

    ## Place Text (k and n)
    text(x=pLo/4.5,
         y=0.1,
         adj=c(0,0),
         labels=paste(k[i]," of",n[i]),
         cex=1.8)

    ## Polygon fill to color area
    cord.x <- c(seq(pLo,pHi,length.out=60))
    cord.y <- c(kndbeta(
        cord.x,
        k[i],
        n[i]))
    cord.x <- c(pLo,cord.x,pHi)
    cord.y <- c(0,cord.y,0)
    polygon(cord.x,cord.y,col=cols[i],border=NA,density=35)

    # region lines
    abline(v=pHi,col="gray")
    abline(h=0,col="gray")
    abline(v=pLo,col="gray")

    # frequentist error bars
    yOne = yLim[1]*1
    arrows(x0=(-sqrt(k[i])+k[i])/n[i],
           x1=(sqrt(k[i])+k[i])/n[i],
           y0=yOne,y1=yOne,
           code=3,
           angle=90,
           length=0.06,
           col=cols[i],
           lwd=2)
    points(x=k[i]/n[i],y=yOne,
           col=cols[i])

}

# Layout for three subplots in one row
par(mfrow=c(1,3))

# render subplots
oneCurve(1,dax="s")
oneCurve(2)
oneCurve(4)


