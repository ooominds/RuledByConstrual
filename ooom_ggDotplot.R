
### This is a modified version of ggCaterpillar ###
    # re -> object of class ranef.mer
    # QQ -> quantile-quantile or dotplot
    # main -> main title
ggDotplot <- function(re, QQ=TRUE, ylim=c(-3,+3), main=' ') {
    require(ggplot2)
    f <- function(x) {
        pv   <- attr(x, 'postVar')
        cols <- 1:(dim(pv)[1])
        se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
        ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
        pDf  <- data.frame(
            y=unlist(x)[ord],
            ci=1.96*se[ord],
            nQQ=rep(qnorm(ppoints(nrow(x))), ncol(x)),
            ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
            ind=gl(ncol(x), nrow(x), labels=main)
        )
        if (QQ) { # normal QQ-plot
            p <- ggplot(pDf, aes(nQQ, y)) +
                coord_cartesian(ylim=ylim) +
                facet_wrap(~ ind, scales='free') +
                xlab('Standard normal quantiles') +
                ylab('Random effect quantiles')
        } else { # caterpillar dotplot
            p <- ggplot(pDf, aes(ID, y)) +
                coord_cartesian(ylim=ylim) +
                coord_flip() +
                facet_wrap(~ ind) +
                xlab('Levels') +
                ylab('Random effects')
        }
        p <- p + theme(legend.position='none') +
            geom_hline(yintercept=0) +
            geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=0, colour='black') +
            geom_point(shape=16, size=1.2, colour='black')
        return(p)
    }
    lapply(re, f)
}
