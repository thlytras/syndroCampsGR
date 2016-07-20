library(zoo)
library(splines)

load("./output/latest_fits.RData")

na0 <- function(x) { x[is.na(x)] <- 0; x }

a <- do.call("rbind", lapply(c(1:6,9), function(i){
    fit <- fits[[i]]
    if (nrow(fit)<7) {
        res <- subset(fit, alerts==1)
        if (nrow(res)>0) res <- cbind(res, alerts3=0, syndro=i)
        return(res)
    }
    fit$rollx <- c(rep(NA,7), rollsum(na0(fit$x), 7)[-nrow(fit)+6])
    fit$rolln <- c(rep(NA,7), rollsum(na0(fit$n), 7)[-nrow(fit)+6])
    fit$pexp <- with(fit, rollx/rolln)
    fit$rollpmean <- c(rep(NA,7), rollapply(fit$p, 7, mean, na.rm=TRUE)[-nrow(fit)+6])
    fit$rollpsd <- c(rep(NA,7), rollapply(fit$p, 7, sd, na.rm=TRUE)[-nrow(fit)+6])
    
    fit$uci <- c(rep(NA,7), qbinom(0.975, fit$n[-(1:7)], fit$pexp[-(1:7)])/fit$n[-(1:7)])
    fit$lci <- c(rep(NA,7), qbinom(0.025, fit$n[-(1:7)], fit$pexp[-(1:7)])/fit$n[-(1:7)])

    fit$alerts2 <- 0
    fit$alerts2[with(fit, which(p > rollpmean + 2*rollpsd))] <- 1
    fit$alerts3 <- 0
    fit$alerts3[with(fit, which(p > uci))] <- 1
    
    res <- subset(fit, alerts==1 | alerts3==1)
    if (nrow(res)>0) res <- cbind(res, syndro=i)
}))[,c(21,1,2,3,4,8,10,20)]

a$syndro <- syndroDesc$GR[a$syndro]
a$centre <- "Σύνολο κέντρων"
a <- a[,c(1,9,2:8)]


b <- do.call("rbind", lapply(1:length(fitsD), function(j) 
    do.call("rbind", lapply(c(1:6,9), function(i){
    fit <- fitsD[[j]][[i]]
    if (nrow(fit)<7) {
        res <- subset(fit, alerts==1)
        if (nrow(res)>0) res <- cbind(res, alerts3=0, syndro=i, centre=j)
        return(res)
    }
    fit$rollx <- c(rep(NA,7), rollsum(na0(fit$x), 7)[-nrow(fit)+6])
    fit$rolln <- c(rep(NA,7), rollsum(na0(fit$n), 7)[-nrow(fit)+6])
    fit$pexp <- with(fit, rollx/rolln)
    fit$rollpmean <- c(rep(NA,7), rollapply(fit$p, 7, mean, na.rm=TRUE)[-nrow(fit)+6])
    fit$rollpsd <- c(rep(NA,7), rollapply(fit$p, 7, sd, na.rm=TRUE)[-nrow(fit)+6])
    
    fit$uci <- c(rep(NA,7), qbinom(0.975, fit$n[-(1:7)], fit$pexp[-(1:7)])/fit$n[-(1:7)])
    fit$lci <- c(rep(NA,7), qbinom(0.025, fit$n[-(1:7)], fit$pexp[-(1:7)])/fit$n[-(1:7)])

    fit$alerts2 <- 0
    fit$alerts2[with(fit, which(p > rollpmean + 2*rollpsd))] <- 1
    fit$alerts3 <- 0
    fit$alerts3[with(fit, which(p > uci))] <- 1
    
    res <- subset(fit, alerts==1 | alerts3==1)
    if (nrow(res)>0) res <- cbind(res, syndro=i, centre=j)
    return(res)
}))))[,c(21,22,1,2,3,4,8,10,20)]

b$syndro <- syndroDesc$GR[b$syndro]
b$centre <- names(fitsD)[b$centre]
b$centre <- paste(b$centre, "-", camps$GR[match(b$centre, camps$codecamp)])

ab <- rbind(a, b)

colnames(ab) <- c("Σύνδρομο", "Κέντρο", "Ημ/νία", "Αρ.περιστατικών", "Σύνολο επισκέψεων", "Παρατηρ. αναλογική νοσηρότητα", "Z-score", "Alert (νέο)", "Alert (παλιό)")

write.csv2(ab, file="output/AlertsOldNew.csv", na="", row.names=FALSE)

