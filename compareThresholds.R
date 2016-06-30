library(zoo)
library(splines)

load("./output/latest_fits.RData")

plotAll <- function(fit, ymax=NA, title=NA, lang="EN", goback=28, plottype="l", pdValLab=97.5) {
    # Determine time window
    xlim <- nrow(fit) - c(goback,0)
    if (xlim[1]<1) xlim[1] <- 1
    xlim[3] <- xlim[2] + 7
    dates <- c(fit$dates, seq(fit$dates[nrow(fit)], length.out=7, by=1))
    mondays <- which(format(dates, "%w")==1)
    mondays <- mondays[mondays>=xlim[1]]
    

    fit$rollx <- c(rep(NA,7), rollsum(fit$x, 7)[-nrow(fit)+6])
    fit$rolln <- c(rep(NA,7), rollsum(fit$n, 7)[-nrow(fit)+6])
    fit$pexp <- with(fit, rollx/rolln)
    fit$rollpmean <- c(rep(NA,7), rollapply(fit$p, 7, mean)[-nrow(fit)+6])
    fit$rollpsd <- c(rep(NA,7), rollapply(fit$p, 7, sd)[-nrow(fit)+6])
    
    fit$uci <- c(rep(NA,7), qbinom(0.975, fit$n[-(1:7)], fit$pexp[-(1:7)])/fit$n[-(1:7)])
    fit$lci <- c(rep(NA,7), qbinom(0.025, fit$n[-(1:7)], fit$pexp[-(1:7)])/fit$n[-(1:7)])

    fit$rollpmean <- c(rep(NA,7), rollapply(fit$p, 7, mean)[-nrow(fit)+6])
    fit$rollpsd <- c(rep(NA,7), rollapply(fit$p, 7, sd)[-nrow(fit)+6])


    
    xlab <- c("EN" = "Date", "GR" = "Ημερομηνία")
    ylab <- c("EN" = "Proportion of clinic visits (%)", "GR" = "Αναλογία επισκέψεων στο ιατρείο (%)")
    main <- c("EN" = "Point of care surveillance in refugee/migrant reception centres, Greece",
                "GR" = "Συνδρομική επιτήρηση στα κέντρα φιλοξενίας προσφύγων & μεταναστών, Ελλάδα")
    
    # Determine max for y-axis
    if (is.na(ymax)) {
        ymax <- with(fit[xlim[1]:xlim[2],], max(c(p, UPI/n), na.rm=TRUE))
        ymax <-  ceiling(ymax*1.2*100)/100
    }
    if (ymax==0) ymax <- 0.03
    
    # Check if plottype="o" is needed...
    a <- c(NA, fit$p, NA)
    if (sum(is.na(a[1:(length(a)-2)]) & !is.na(a[2:(length(a)-1)]) & is.na(a[-(1:2)]))>0) plottype <- "o"
    
    if (is.na(title)) title <- attr(fit, "title")
    
    par(family="Fira Sans")

    with(fit, plot(0, type="n", bty="l", 
                ylim=c(-ymax*3/20,ymax), xlim=xlim[c(1,3)], 
                xlab=xlab[lang], ylab=ylab[lang], xaxt="n", yaxt="n"))
    axis(1, at=xlim[1]:xlim[3], labels=NA, tcl=-0.25)
    axis(1, at=mondays, labels=fmtDate(dates[mondays], lang=lang))
    axtck2 <- axTicks(2); axtck2 <- axtck2[axtck2>=0]; axtck2 <- unique(c(0, axtck2))
    axis(2, at=axtck2, labels=axtck2*100, las=1)
    # Farrington
    with(fit, points(y=(Pnb/n)[xlim[1]:xlim[2]], x=xlim[1]:xlim[2], 
                type=plottype, cex=0.6, col="blue", lwd=2, lty="dashed"))
    with(fit, points(y=(UPI/n)[xlim[1]:xlim[2]], x=xlim[1]:xlim[2], 
                type=plottype, cex=0.6, col="red", lwd=2, lty="dashed"))

    # EARS C1
    with(fit, points(y=rollpmean[xlim[1]:xlim[2]], x=xlim[1]:xlim[2], 
                type=plottype, cex=0.6, col="turquoise", lwd=2, lty="dashed"))
    with(fit, points(y=(rollpmean + 2*rollpsd)[xlim[1]:xlim[2]], x=xlim[1]:xlim[2], 
                type=plottype, cex=0.6, col="pink", lwd=2, lty="dashed"))

    # Current arrangement
    with(fit, points(y=(pexp)[xlim[1]:xlim[2]], x=xlim[1]:xlim[2], 
                type=plottype, cex=0.6, col="green", lwd=2, lty="dashed"))
    with(fit, points(y=(uci)[xlim[1]:xlim[2]], x=xlim[1]:xlim[2], 
                type=plottype, cex=0.6, col="violet", lwd=2, lty="dashed"))

    with(fit, points(y=p[xlim[1]:xlim[2]], x=xlim[1]:xlim[2],
                type=plottype, cex=0.6, col="black", lwd=2, lty="solid"))

    fit$alerts2 <- 0
    fit$alerts2[with(fit, which(p > rollpmean + 2*rollpsd))] <- 1
    fit$alerts3 <- 0
    fit$alerts3[with(fit, which(p > uci))] <- 1
    
    with(fit, points(y=rep(-ymax*0.5/20, sum(alerts==1)), x=which(alerts==1), col="red", pch=19))
#    with(fit, points(y=rep(-ymax*0.8/20, sum(alarms==1)), x=which(alarms==1), col="red", pch=17))
    with(fit, points(y=rep(-ymax*1.5/20, sum(alerts2==1)), x=which(alerts2==1), col="pink", pch=19))
    with(fit, points(y=rep(-ymax*2.5/20, sum(alerts3==1)), x=which(alerts3==1), col="violet", pch=19))


    mtext(main[lang], side=3, line=2, cex=1.2)
    mtext(title[lang], side=3, line=1, cex=1.2, col="blue")
    
    legend("topright", c("Observed", "Farrington - expected", "Farrington - alert", "EARS C1 - expected", 
            "EARS C1 - alert", "Current - expected", "Current - alert"), lwd=2, seg.len=4, bty="n",
            col=c("black", "blue", "red", "turquoise", "pink", "green", "violet"),
            lty=c("solid", rep("dashed",6)))
    legend("topright", c("Alerts (Farrington)", "Alerts (EARS CI)", "Alerts (current)"), pch=19, 
            col=c("red", "pink", "violet"), inset=c(0.25,0), bty="n")

    legend("topleft", c(
            sprintf("Farrington: %s alerts / %s = %1.2f%%", 
                        sum(fit$alerts), nrow(fit), 100*sum(fit$alerts)/nrow(fit)),
            sprintf("EARS C1: %s alerts / %s = %1.2f%%", 
                        sum(fit$alerts2), nrow(fit), 100*sum(fit$alerts2)/nrow(fit)),
            sprintf("Current: %s alerts / %s = %1.2f%%", 
                        sum(fit$alerts3), nrow(fit), 100*sum(fit$alerts3)/nrow(fit))
            ), bty="n", inset=c(0.33,0))    
}


dir.create("output", showWarnings=FALSE)
dir.create("output/comparisons", showWarnings=FALSE)

for (i in c(1:14)) {
    png(sprintf("output/comparisons/algoComparison%s.png", i), width=5000, height=2500, res=400)
    plotAll(fits[[i]], goback=2000)
    dev.off()
}




plotSplines <- function(num=1, ymax=NA, title=NA, lang="EN", goback=28, plottype="l", pdValLab=97.5) {
    xv <- paste("n", num, "sum", sep="")
    fit <- fitOne(aggr[,xv], aggr$vissum, aggr$hmedil, title=attr(fits[[num]], "title"), splineDenominator=30)
    fit15 <- fitOne(aggr[,xv], aggr$vissum, aggr$hmedil, title=attr(fits[[num]], "title"), splineDenominator=15)
    fit7 <- fitOne(aggr[,xv], aggr$vissum, aggr$hmedil, title=attr(fits[[num]], "title"), splineDenominator=7)
    fit3 <- fitOne(aggr[,xv], aggr$vissum, aggr$hmedil, title=attr(fits[[num]], "title"), splineDenominator=3)

    # Determine time window
    xlim <- nrow(fit) - c(goback,0)
    if (xlim[1]<1) xlim[1] <- 1
    xlim[3] <- xlim[2] + 7
    dates <- c(fit$dates, seq(fit$dates[nrow(fit)], length.out=7, by=1))
    mondays <- which(format(dates, "%w")==1)
    mondays <- mondays[mondays>=xlim[1]]

    
    xlab <- c("EN" = "Date", "GR" = "Ημερομηνία")
    ylab <- c("EN" = "Proportion of clinic visits (%)", "GR" = "Αναλογία επισκέψεων στο ιατρείο (%)")
    main <- c("EN" = "Point of care surveillance in refugee/migrant reception centres, Greece",
                "GR" = "Συνδρομική επιτήρηση στα κέντρα φιλοξενίας προσφύγων & μεταναστών, Ελλάδα")
    
    # Determine max for y-axis
    if (is.na(ymax)) {
        ymax <- with(fit7[xlim[1]:xlim[2],], max(p, UPI/n, na.rm=TRUE))
        ymax <-  ceiling(ymax*1.2*100)/100
    }
    if (ymax==0) ymax <- 0.03
    
    # Check if plottype="o" is needed...
    a <- c(NA, fit$p, NA)
    if (sum(is.na(a[1:(length(a)-2)]) & !is.na(a[2:(length(a)-1)]) & is.na(a[-(1:2)]))>0) plottype <- "o"
    
    if (is.na(title)) title <- attr(fit, "title")
    
    par(family="Fira Sans")

    with(fit, plot(0, type="n", bty="l", 
                ylim=c(-ymax*3/20,ymax), xlim=xlim[c(1,3)], 
                xlab=xlab[lang], ylab=ylab[lang], xaxt="n", yaxt="n"))
    axis(1, at=xlim[1]:xlim[3], labels=NA, tcl=-0.25)
    axis(1, at=mondays, labels=fmtDate(dates[mondays], lang=lang))
    axis(2, at=axTicks(2), labels=axTicks(2)*100, las=1)
    # Farrington
    with(fit, points(y=(Pnb/n)[xlim[1]:xlim[2]], x=xlim[1]:xlim[2], 
                type=plottype, cex=0.6, col="cadetblue4", lwd=2, lty="solid"))
    with(fit, points(y=(UPI/n)[xlim[1]:xlim[2]], x=xlim[1]:xlim[2], 
                type=plottype, cex=0.6, col="red4", lwd=2, lty="solid"))

    with(fit15, points(y=(Pnb/n)[xlim[1]:xlim[2]], x=xlim[1]:xlim[2], 
                type=plottype, cex=0.6, col="cadetblue3", lwd=2, lty="solid"))
    with(fit15, points(y=(UPI/n)[xlim[1]:xlim[2]], x=xlim[1]:xlim[2], 
                type=plottype, cex=0.6, col="red3", lwd=2, lty="solid"))

    with(fit7, points(y=(Pnb/n)[xlim[1]:xlim[2]], x=xlim[1]:xlim[2], 
                type=plottype, cex=0.6, col="cadetblue2", lwd=2, lty="solid"))
    with(fit7, points(y=(UPI/n)[xlim[1]:xlim[2]], x=xlim[1]:xlim[2], 
                type=plottype, cex=0.6, col="red1", lwd=2, lty="solid"))

    with(fit3, points(y=(Pnb/n)[xlim[1]:xlim[2]], x=xlim[1]:xlim[2], 
                type=plottype, cex=0.6, col="cadetblue1", lwd=2, lty="solid"))
    with(fit3, points(y=(UPI/n)[xlim[1]:xlim[2]], x=xlim[1]:xlim[2], 
                type=plottype, cex=0.6, col="pink", lwd=2, lty="solid"))

    with(fit, points(y=p[xlim[1]:xlim[2]], x=xlim[1]:xlim[2],
                type=plottype, cex=0.6, col="black", lwd=2, lty="solid"))

    with(fit, points(y=rep(-ymax*0.5/20, sum(alerts==1)), x=which(alerts==1), col="red4", pch=19))
    with(fit15, points(y=rep(-ymax*1.2/20, sum(alerts==1)), x=which(alerts==1), col="red3", pch=19))
    with(fit7, points(y=rep(-ymax*1.9/20, sum(alerts==1)), x=which(alerts==1), col="red1", pch=19))
    with(fit3, points(y=rep(-ymax*2.6/20, sum(alerts==1)), x=which(alerts==1), col="pink", pch=19))


    mtext(main[lang], side=3, line=2, cex=1.2)
    mtext(title[lang], side=3, line=1, cex=1.2, col="blue")

    legend("topright", c("Spline knots per 30 days - Expected", "Spline knots per 15 days - Expected", 
            "Spline knots per 7 days - Expected", "Spline knots per 3 days - Expected"), 
            lwd=2, seg.len=4, bty="n",
            col=c("cadetblue4", "cadetblue3", "cadetblue2", "cadetblue1"), lty="solid")
    legend("topright", c("Spline knots per 30 days - Alert", "Spline knots per 15 days - Alert",
            "Spline knots per 7 days - Alert", "Spline knots per 3 days - Alert"), lwd=2, seg.len=4, bty="n",
            col=c("red4", "red3", "red1", "pink"), lty="solid", inset=c(0.35,0))

}


for (i in c(1,2,4,5)) {
    png(sprintf("output/comparisons/splineComparison%s.png", i), width=5000, height=2500, res=400)
    plotSplines(i, goback=2000)
    dev.off()
}

