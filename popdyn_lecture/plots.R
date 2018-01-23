w <- 1000
h <- 300

png("popdyn_figure.png", width=w, height=h, pointsize=20)

par(mgp=c(1,0,0),
    mar=c(0,0,0,0))

t <- seq(0, 10, length.out=1000)

nfunc <- function(t, mean, amp) {
    return(mean + amp*sin(t/5*pi))
}

N <- nfunc(t, 60, -50)

plot(t, N, 'l', ylim=c(0,110), xaxt='n', yaxt='n', frame.plot=F,
     xlab="", ylab="", lwd=6)

dev.off()

png("popdyn_figure_samp.png", width=w, height=h, pointsize=20)

par(mgp=c(1,0,0),
    mar=c(0,0,0,0))

t <- seq(0, 10, length.out=1000)

N <- 60 + -50*sin(t/5*pi)

plot(t, N, 'l', ylim=c(0,110), xaxt='n', yaxt='n', frame.plot=F,
     xlab="", ylab="", lwd=6)

samps <- sample(t, size=100, prob=N, replace=T)

points(samps, runif(length(samps), min=3, max=7), col='blue', lwd=4)

dev.off()

png("popdyn_figure_treesamp.png", width=w, height=h, pointsize=20)

par(mgp=c(1,0,0),
    mar=c(0,0,0,0))

t <- seq(0, 10, length.out=1000)

N <- 60 + -50*sin(t/5*pi)

plot(t, N, 'l', ylim=c(0,110), xaxt='n', yaxt='n', frame.plot=F,
     xlab="", ylab="", lwd=6)

lambda <- function(t) { nfunc(t, 0.7, -0.5) }
tree <- rbdtree(lambda, 0.7, 10)
nleaves <- length(tree$tip.label)
ages <- node.depth.edgelength(tree)
ages_internal <- ages[ages<10.0]
ninternal <- length(ages_internal)

points(ages_internal, runif(ninternal, min=3, max=7), col='red', lwd=4)
points(rep(10, nleaves), runif(nleaves, min=3, max=7), col='blue', lwd=4)

par(new = TRUE)
plot(tree, root.edge=TRUE, show.tip.label=FALSE, x.lim=c(0,10), y.lim=c(-nleaves,2*nleaves))

dev.off()

