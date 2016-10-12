
fmtDate <- function(date, lang="EN") {
    en.mon <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    gr.mon <- c("Ιαν", "Φεβ", "Μαρ", "Απρ", "Μαϊ", "Ιον", "Ιολ", "Αυγ", "Σεπ", "Οκτ", "Νοε", "Δεκ")
    if (lang=="GR") return(paste(format(date, "%d"), gr.mon[as.integer(format(date, "%m"))]))
    return(paste(format(date, "%d"), en.mon[as.integer(format(date, "%m"))]))
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

