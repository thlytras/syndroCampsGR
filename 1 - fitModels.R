library(foreign)
library(splines)
library(stringr)

if (!exists("tgtdate")) stop("\nERROR: No target date selected!\nPut the target date on an object named 'tgtdate' to continue...\n")

syndroDesc <- read.csv("input/syndromes.csv", stringsAsFactors=FALSE)
syndroDescList <- lapply(1:nrow(syndroDesc), function(i)unlist(syndroDesc[i,c("EN","GR")]))

fmtDate <- function(date, lang="EN") {
    en.mon <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    gr.mon <- c("Ιαν", "Φεβ", "Μαρ", "Απρ", "Μαϊ", "Ιον", "Ιολ", "Αυγ", "Σεπ", "Οκτ", "Νοε", "Δεκ")
    if (lang=="GR") return(paste(format(date, "%d"), gr.mon[as.integer(format(date, "%m"))]))
    return(paste(format(date, "%d"), en.mon[as.integer(format(date, "%m"))]))
}

suppressWarnings(
    dat <- read.epiinfo(file("input/KATAV1.rec", encoding="windows-1253"))
)
dat$camp <- str_trim(dat$camp)
dat$foreas <- str_trim(dat$foreas)
dat$campname <- str_trim(dat$campname)

dat <- subset(dat, hmedil<tgtdate)

camps <- data.frame(codecamp = unique(dat$codecamp),
    EN = sapply(unique(dat$codecamp), function(x)rev(dat$campname[dat$codecamp==x])[1]),
    GR = sapply(unique(dat$codecamp), function(x)rev(dat$camp[dat$codecamp==x])[1]))
camps <- camps[order(camps$codecamp),]


aggr <- aggregate(dat[,15:49], by=list(hmedil=dat$hmedil), sum, na.rm=TRUE)
aggrD <- aggregate(dat[,15:49], by=list(hmedil=dat$hmedil, codecamp=dat$codecamp), sum, na.rm=TRUE)
# aggr[nrow(aggr), "n1sum"] <- 200   # Artificially create an alert...


# Συνάρτηση υπολογισμού της εβδομάδας κατά ISO (v2.0)
isoweek <- function(x, type="week", sep="-", inv=FALSE, colnames=c("isoyear","isoweek")) {
  alts=c("week","year","both_text","both_num","matrix")
  if(!(type %in% alts)) stop("Unknown isoweek type requested!")
  x.date<-as.Date(x)
  x.weekday<-as.integer(format(x.date,"%w"))
  x.weekday[x.weekday==0]=7
  x.nearest.thu<-x.date-x.weekday+4
  x.isoyear<-as.integer(substring(x.nearest.thu,1,4)) # Μπορεί οι πρώτες μέρες του χρόνου να ανήκουν (κατά ISO) στην προηγούμενη χρονιά!
  x.isoweek<-(as.integer(x.nearest.thu-as.Date(paste(x.isoyear,"-1-1",sep="")))%/%7)+1
  switch(type,
    week = x.isoweek,
    year = x.isoyear,
    both_text = if (inv) {
      ifelse((is.na(x.isoyear) | is.na(x.isoweek)),NA,paste(x.isoweek,x.isoyear,sep=sep))
    } else {
      ifelse((is.na(x.isoyear) | is.na(x.isoweek)),NA,paste(x.isoyear,x.isoweek,sep=sep))
    },
    both_num = ifelse((is.na(x.isoyear) | is.na(x.isoweek)),NA,x.isoyear*100+x.isoweek),
    matrix = if (inv) {
      `colnames<-`(cbind(x.isoweek, x.isoyear), rev(colnames))
    } else {
      `colnames<-`(cbind(x.isoyear, x.isoweek), colnames)
    }
  )
}



fitOne <- function(x, n, dates, title=c(EN=NA, GR=NA), wkly=FALSE, Z=2, excludeOutbreaks=TRUE, 
                    spline=TRUE, splineDenominator=c(30,4)[wkly+1], forceSpline=FALSE) {
    if (length(x)!=length(n)) stop("x and n must be same length integer vectors")
    alldates <- seq(min(dates, na.rm=TRUE), max(dates, na.rm=TRUE), by=1)
    n[n==0] <- NA
    temp <- NA; temp[match(dates, alldates)] <- n; n <- temp
    temp <- NA; temp[match(dates, alldates)] <- x; x <- temp
    
    if (wkly) {
        allweeks <- isoweek(alldates, "both_num")
        x <- tapply(x, allweeks, sum, na.rm=TRUE)
        n <- tapply(n, allweeks, sum, na.rm=TRUE)
        allweeks <- as.integer(names(n))
    }
    
    p <- x/n
    t <- (1:length(x))-1
    x.fit <- x
    while(TRUE) {
        if ((spline && length(x.fit)>c(30,4)[wkly+1] && sum(x.fit==0, na.rm=TRUE)/sum(x.fit>0, na.rm=TRUE)<3) || forceSpline) {
            # Spline, 1 knot/month
            m <- glm(x.fit ~ ns(t,df=floor(length(x)/splineDenominator)), offset=log(n), family="quasipoisson")
        } else {
            # Intercept only
            m <- glm(x ~ 1, offset=log(n), family="quasipoisson")
        }
        od <- max(1,sum(m$weights * m$residuals^2)/m$df.r)
        Pnb <- predict(m, data.frame(x.fit=x,t=t,n=n), type="response")
        stdp <- predict(m, data.frame(x.fit=x,t=t,n=n), se.fit=TRUE)$se.fit
        stdpr <- predict(m, data.frame(x.fit=x,t=t,n=n), type="response", se.fit=TRUE)$se.fit
        UPI <- (Pnb^(2/3)+ Z*((4/9)*(Pnb^(1/3))*(od+(stdp^2)*(Pnb)))^(1/2))^(3/2)
        UPIr <- Pnb *(1 + (2/3)*Z* ( (od + stdpr^2/Pnb) /Pnb ) ^(1/2))^(3/2)
        zscore <- (x^(2/3) - Pnb^(2/3)) / ((4/9)*(Pnb^(1/3))*(od+Pnb*(stdp^2)))^(1/2)
        
        UPI3 <- (Pnb^(2/3)+ 3 * ((4/9)*(Pnb^(1/3))*(od+(stdp^2)*(Pnb)))^(1/2))^(3/2)
        
        alerts3 <- which(x.fit>UPI3)
        if (!excludeOutbreaks || length(alerts3)==0) break
        x.fit[alerts3] <- NA
    }
    if (wkly) {
        fit <- data.frame(weeks=allweeks, x=x, n=n, p=p, Pnb=Pnb, stdp=stdp, stdpr=stdpr, zscore=zscore, UPI=UPI, alerts=0, alarms=0)
    } else {
        fit <- data.frame(dates=alldates, x=x, n=n, p=p, Pnb=Pnb, stdp=stdp, stdpr=stdpr, zscore=zscore, UPI=UPI, alerts=0, alarms=0)
    }
    alerts <- which(p>UPI/n); fit$alerts[alerts] <- 1
    alarms <- alerts[c(0,diff(alerts))==1]; fit$alarms[alarms] <- 1
    attr(fit, "model") <- m
    attr(fit, "od") <- od
    attr(fit, "title") <- title
    return(fit)
}



cat("Fitting models for all camps...\n")
fits <- mapply(function(x, title, spline){
    suppressWarnings( fitOne(aggr[,x], aggr$vissum, aggr$hmedil, title=title, spline=spline) )
}, x=c(paste("n",1:5,"sum", sep=""), paste("n",6:14, sep="")), spline=TRUE,
    title=syndroDescList, 
    SIMPLIFY=FALSE)


cat("Fitting weekly models for all camps...\n")
fitsW <- mapply(function(x, title, spline){
    suppressWarnings( fitOne(aggr[,x], aggr$vissum, aggr$hmedil, title=title, spline=spline, wkly=TRUE) )
}, x=c(paste("n",1:5,"sum", sep=""), paste("n",6:14, sep="")), spline=TRUE,
    title=syndroDescList, 
    SIMPLIFY=FALSE)


cat("Fitting models for each camp, please wait... ")
fitsD <- lapply(camps$codecamp, function(y) {
    cat(".")
    aggrsub <- subset(aggrD, codecamp==y)
    mapply(function(x, title, spline){
        suppressWarnings(fitOne(aggrsub[,x], aggrsub$vissum, aggrsub$hmedil, title=title, spline=spline))
    }, x=c(paste("n",1:5,"sum", sep=""), paste("n",6:14, sep="")), spline=TRUE,
    title=lapply(syndroDescList, function(syndrome){
            c(EN=paste(y, ": ", camps[match(y, camps$codecamp), "EN"], ". ", syndrome["EN"], sep=""),
                GR=paste(y, ": ", camps[match(y, camps$codecamp), "GR"], ". ", syndrome["GR"], sep=""))
            }),
    SIMPLIFY=FALSE)
})
names(fitsD) <- camps$codecamp
cat("\n")



plotOne <- function(fit, ymax=NA, main=NA, title=NA, lang="EN", legend=TRUE, wkly=FALSE, goback=c(28,6)[wkly+1], plottype="l", pdValLab=97.5, mar=NA, lwd=2, legend.y=-0.65) {
    # Determine time window
    xlim <- nrow(fit) - c(goback,0)
    if (xlim[1]<1) xlim[1] <- 1
    xlim[3] <- xlim[2] + c(7,2)[wkly+1]
    
    if (!wkly){
        dates <- c(fit$dates, seq(fit$dates[nrow(fit)], length.out=7, by=1))
        mondays <- which(format(dates, "%w")==1)
        mondays <- mondays[mondays>=xlim[1]]
    }
    
    xlab <- c("EN" = "Date", "GR" = "Ημερομηνία")
    ylab <- c("EN" = "Proportion of clinic visits (%)", "GR" = "Αναλογία επισκέψεων στο ιατρείο (%)")
    if (length(main)==1 && is.na(main)) main <- c("EN" = "Point of care surveillance in refugee/migrant reception centres, Greece",
                "GR" = "Συνδρομική επιτήρηση στα κέντρα φιλοξενίας προσφύγων & μεταναστών, Ελλάδα")
    if (length(title)==1 && is.na(title)) title <- attr(fit, "title")
        
    # Determine max for y-axis
    if (is.na(ymax)) {
        ymax <- with(fit[xlim[1]:xlim[2],], max(c(p, UPI/n), na.rm=TRUE))
        ymax <-  ceiling(ymax*1.2*100)/100
    }
    if (ymax==0) ymax <- 0.03
    
    # Check if plottype="o" is needed...
    a <- c(NA, fit$p, NA)
    if (sum(is.na(a[1:(length(a)-2)]) & !is.na(a[2:(length(a)-1)]) & is.na(a[-(1:2)]))>0) plottype <- "o"
    
    if (legend && (length(mar)==1 && is.na(mar))) par(mar=c(10,4,4,2))
    if (!(length(mar)==1 && is.na(mar))) par(mar=mar)
    
    par(family="Fira Sans")

    with(fit, plot(0, type="n", bty="l", 
                ylim=c(-ymax/15,ymax), xlim=xlim[c(1,3)], 
                xlab=xlab[lang], ylab=ylab[lang], xaxt="n", yaxt="n"))
    if (wkly) {
        axis1lab <- (min(fit$weeks):(max(fit$weeks)+2))[xlim[1]:xlim[3]]
        axis1lab <- paste(axis1lab%/%100, axis1lab%%100, sep="-")
        axis(1, at=xlim[1]:xlim[3], labels=axis1lab, tcl=-0.25)
    } else {
        axis(1, at=xlim[1]:xlim[3], labels=NA, tcl=-0.25)
        axis(1, at=mondays, labels=fmtDate(dates[mondays], lang=lang))
    }
    axis(2, at=axTicks(2), labels=axTicks(2)*100, las=1)
    with(fit, points(y=(Pnb/n)[xlim[1]:xlim[2]], x=xlim[1]:xlim[2], 
                type=plottype, cex=0.6, col="blue", lwd=lwd, lty="dashed"))
    with(fit, points(y=(UPI/n)[xlim[1]:xlim[2]], x=xlim[1]:xlim[2], 
                type=plottype, cex=0.6, col="red", lwd=lwd, lty="dashed"))
    with(fit, points(y=p[xlim[1]:xlim[2]], x=xlim[1]:xlim[2],
                type=plottype, cex=0.6, col="black", lwd=lwd, lty="solid"))
    
    alerts <- which(fit$alerts==1)
    alerts <- alerts[alerts>=xlim[1]]
    points(y=rep(-ymax/15*0.5, length(alerts)), x=alerts, col="blue", pch=19)
    alarms <- which(fit$alarms==1)
    alarms <- alarms[alarms>=xlim[1]]
    points(y=rep(-ymax/15*0.9, length(alarms)), x=alarms, col="red", pch=17)

    mtext(main[lang], side=3, line=2, cex=1.2)
    mtext(title[lang], side=3, line=1, cex=1.2, col="blue")
    
    legendtxt <- list(
        "EN" = c("Observed proportional morbidity", "Expected proportional morbidity",
            paste("Alert level (", pdValLab, "% prediction interval)", sep=""), "Alert", "Warning"),
        "GR" = c("Παρατηρούμενη αναλογική νοσηρότητα", "Αναμενόμενη αναλογική νοσηρότητα",
            paste("Όριο επιφυλακής (", pdValLab, "% διάστημα πρόβλεψης)", sep=""), "Επιφυλακή", "Συναγερμός"))
    
    if (legend) {
        legend("bottomleft", legend=legendtxt[[lang]][1:3], lwd=lwd, 
            lty=c("solid", "dashed", "dashed"), col=c("black","blue","red"), 
            inset=c(0.12,legend.y), bty="n", seg.len=4, xpd=TRUE)
        legend("bottomright", legend=legendtxt[[lang]][4:6], pch=c(19,17,NA), col=c("blue","red"), 
            inset=c(0.25,legend.y), bty="n", seg.len=4, xpd=TRUE)
    }

}



plotAges <- function(syndrome, camp=NA, ymax=NA, lang="EN", legend=TRUE, goback=28, plottype="l") {
    if (is.na(camp)) {
        aggrsub <- aggr[,c(paste("n",syndrome,letters[1:3], sep=""), paste("vis",letters[1:3], sep=""), "hmedil")]
        fitsub <- fits[[syndrome]][,c("dates","alerts","alarms")]
    } else {
        aggrsub <- subset(aggrD, codecamp==camp)[,c(paste("n",syndrome,letters[1:3], sep=""), paste("vis",letters[1:3], sep=""), "hmedil")]
        fitsub <- fitsD[[camp]][[syndrome]][,c("dates","alerts","alarms")]
    }
    colnames(aggrsub)[1:3] <- c("na", "nb", "nc")
    a <- merge(fitsub, aggrsub, by.x="dates", by.y="hmedil", all.x=TRUE)
    a$pa <- a$na/a$visa
    a$pb <- a$nb/a$visb
    a$pc <- a$nc/a$visc
    
    # Determine time window
    xlim <- nrow(a) - c(goback,0)
    if (xlim[1]<1) xlim[1] <- 1
    xlim[3] <- xlim[2] + 7
    dates <- c(a$dates, seq(a$dates[nrow(a)], length.out=7, by=1))
    mondays <- which(format(dates, "%w")==1)
    mondays <- mondays[mondays>=xlim[1]]
    
    xlab <- c("EN" = "Date", "GR" = "Ημερομηνία")
    ylab <- c("EN" = "Number of cases reported", "GR" = "Αριθμός δηλωθέντων περιστατικών")
    main <- c("EN" = "Distribution of cases by age group",
                "GR" = "Κατανομή περιστατικών κατά ηλικιακή ομάδα")
    
    # Determine max for y-axis
    if (is.na(ymax)) {
        ymax <- with(a[xlim[1]:xlim[2],], max(c(na, nb, nc), na.rm=TRUE))
        ymax <-  ceiling(ymax*1.2*100)/100
    }
    if (ymax==0) ymax <- 3
    
    # Check if plottype="o" is needed...
    b <- c(NA, a$na, NA)
    if (sum(is.na(b[1:(length(b)-2)]) & !is.na(b[2:(length(b)-1)]) & is.na(b[-(1:2)]))>0) plottype <- "o"
    b <- c(NA, a$nb, NA)
    if (sum(is.na(b[1:(length(b)-2)]) & !is.na(b[2:(length(b)-1)]) & is.na(b[-(1:2)]))>0) plottype <- "o"
    b <- c(NA, a$nc, NA)
    if (sum(is.na(b[1:(length(b)-2)]) & !is.na(b[2:(length(b)-1)]) & is.na(b[-(1:2)]))>0) plottype <- "o"
    
    
    if (legend) par(mar=c(10,4,4,2))
    par(family="Fira Sans")

    with(a, plot(0, type="n", bty="l", 
                ylim=c(-ymax/15,ymax), xlim=xlim[c(1,3)], 
                xlab=xlab[lang], ylab=ylab[lang], xaxt="n", yaxt="n"))
    axis(1, at=xlim[1]:xlim[3], labels=NA, tcl=-0.25)
    axis(1, at=mondays, labels=fmtDate(dates[mondays], lang=lang))
    axis(2, at=axTicks(2), labels=axTicks(2), las=1)
    with(a, points(y=na[xlim[1]:xlim[2]], x=xlim[1]:xlim[2],
                type=plottype, cex=0.6, col="green", lwd=2, lty="solid"))
    with(a, points(y=nb[xlim[1]:xlim[2]], x=xlim[1]:xlim[2],
                type=plottype, cex=0.6, col="orange", lwd=2, lty="solid"))
    with(a, points(y=nc[xlim[1]:xlim[2]], x=xlim[1]:xlim[2],
                type=plottype, cex=0.6, col="thistle", lwd=2, lty="solid"))
    
    with(a, points(y=rep(-ymax/15*0.4, sum(alerts==1)), x=which(alerts==1), col="blue", pch=19))
    with(a, points(y=rep(-ymax/15*0.9, sum(alarms==1)), x=which(alarms==1), col="red", pch=17))

    mtext(main[lang], side=3, line=2, cex=1.2)
    #mtext(title[lang], side=3, line=1, cex=1.2, col="blue")
    
    legendtxt <- list(
        "EN" = c("Age 0-4", "Agr 5-17", "Age 18+", "Alert", "Warning"),
        "GR" = c("0-4 ετών", "5-17 ετών", "18+ ετών", "Επιφυλακή", "Συναγερμός"))
    
    if (legend) {
        legend("bottomleft", legend=legendtxt[[lang]][1:3], lwd=2, 
            lty="solid", col=c("green", "orange", "thistle"), 
            inset=c(0.12,-0.65), bty="n", seg.len=4, xpd=TRUE)
        legend("bottomright", legend=legendtxt[[lang]][4:6], pch=c(19,17,NA), col=c("blue","red"), 
            inset=c(0.25,-0.65), bty="n", seg.len=4, xpd=TRUE)
    }

}


save.image("./output/latest_fits.RData")
