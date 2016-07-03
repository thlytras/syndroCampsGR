# Προσοχή: στον κώδικα έχουμε alert & alarm.
# Οι αντίστοιχες διατυπώσεις στην αναφορά είναι warning & alert !!!
# (Στα ελληνικά, ειδοποίηση & εγρήγορση)

library(odfWeave)
library(WriteXLS)
library(gplots)

styleDefs <- getStyleDefs()

styleDefs$RTable0 <- styleDefs$RTable1
styleDefs$RTable0$marginLeft <- "0"
styleDefs$RTable0$marginRight <- "0"
styleDefs$RTable0$marginTop <- "0"
styleDefs$RTable0$marginBottom <- "0"

styleDefs$MyNormal <- styleDefs$ArialNormal
styleDefs$MyNormal$fontName <- "Fira Sans"
styleDefs$MyNormal$fontSize <- "11pt"

styleDefs$MyHeader <- styleDefs$MyNormal
styleDefs$MyHeader$fontSize <- "12.1pt"
styleDefs$MyHeader$fontType <- "bold"
styleDefs$MyHeader$spaceAfter <- "0.1in"
styleDefs$MyHeader$marginBottom <- "0.1in"


styleDefs$MySmall <- styleDefs$ArialNormal
styleDefs$MySmall$fontName <- "Fira Sans"
styleDefs$MySmall$fontSize <- "9pt"

styleDefs$MyCenteredNormal <- styleDefs$MyNormal
styleDefs$MyCenteredNormal$textAlign <- "center"

styleDefs$MyCenteredSmall <- styleDefs$MySmall
styleDefs$MyCenteredSmall$textAlign <- "center"

setStyleDefs(styleDefs)

styles <- getStyles()
styles$paragraph <- "MyCenteredNormal"
styles$cellText <- "MyCenteredNormal"
styles$headerText <- "MyCenteredNormal"
styles$table <- "RTable0"
setStyles(styles)



if (!exists("fits")) load("output/latest_fits.RData")

if (!exists("tgtdate")) stop("\nERROR: No target date selected!\nPut the target date on an object named 'tgtdate' to continue...\n")

tgtdatef <- format(tgtdate, "%d-%m-%Y")
tgtdatef2 <- format(tgtdate, "%Y%m%d")


noAlertMsg <- data.frame(
    EN=c("*** There are no across-camp warnings today ***", 
        "*** There are no camp-specific warnings today ***",
        "*** There are no across-camp warnings for the past 7 days ***",
        "*** There are no camp-specific warnings for the past 7 days ***",
        "*** There are no warnings today ***"),
    GR=c("*** Δεν υπάρχουν ειδοποιήσεις για κανένα σύνδρομο σήμερα ***",
        "*** Δεν υπάρχουν ειδοποιήσεις από κανένα κέντρο σήμερα ***",
        "*** Δεν υπάρχουν ειδοποιήσεις για κανένα σύνδρομο τις τελευταίες 7 ημέρες ***",
        "*** Δεν υπάρχουν ειδοποιήσεις από κανένα κέντρο τις τελευταίες 7 ημέρες ***",
        "*** Δεν υπάρχει καμμία ειδοποίηση σήμερα ***"))
alDayLabels <-data.frame(
    EN=c("Graph %s: Proportional morbidity of %s, based on reports from all camps",
        "Graph %s: Proportional morbidity of %s, based on reports from camp %s - %s"),
    GR=c("Διάγραμμα %s: Αναλογική νοσηρότητα για %s, βάσει δηλώσεων από το σύνολο των κέντρων",
        "Διάγραμμα %s: Αναλογική νοσηρότητα για %s, βάσει δηλώσεων από το κέντρο %s - %s"),
    stringsAsFactors=FALSE)
syndroShort <- c("1 - RespInf w Fever", "2 - Gastroenteritis NO blood", "3 - Bloody diarrhoea",
        "4 - Rash w Fever", "5 - susp scabies", "6 - susp pulm TB", "7 - Malaria pos RDT",
        "8 - susp diphtheria", "9 - Jaundice acute onset", "10 - Neuro acute onset",
        "11 - Meningitis Encephalitis", "12 - Hemorragic w fever", "13 - Sepsis or shock", 
        "14 - Death unknown cause")


Ncamps1 <- length(unique(subset(aggrD, hmedil==tgtdate-1)$codecamp))
Ncamps7 <- length(unique(subset(aggrD, hmedil>=tgtdate-7)$codecamp))


makeTable1 <- function(lang, wkly=FALSE) {
    if (wkly) {
        table1 <- do.call("rbind", lapply(1:14, function(i) subset(fitsW[[i]], weeks==tgtweek)))
    } else {
        table1 <- do.call("rbind", lapply(1:14, function(i) subset(fits[[i]], dates==tgtdate-1)))
    }
    rownames(table1) <- NULL
    if(nrow(table1)==0) stop(sprintf("ΣΦΑΛΜΑ: Δεν υπάρχει ούτε μια δήλωση για τις %s.\n Μήπως το αρχείο εισόδου είναι παλιό??", tgtdate-1))
    table1$syndrome <- syndroDesc[,lang]
    table1$pexp <- with(table1, Pnb/n)
    table1$p <- round(table1$p*100, 1)
    table1$pexp <- round(table1$pexp*100, 1)
    table1$zscore <- round(table1$zscore,3)
    table1$alerts <- list(EN=c("no","yes"), GR=c("όχι","ναι"))[[lang]][table1$alerts+1]
    table1$alarms <- list(EN=c("no","yes"), GR=c("όχι","ναι"))[[lang]][table1$alarms+1]
    table1 <- table1[,c("syndrome", "x", "p", "pexp", "zscore", "alerts", "alarms")]
    colnames(table1) <- list(
        EN = c("Syndrome", "No of cases", "Obs. prop. morbidity", 
            "Exp. prop. morbidity", "Z-score*", "Warning", "Alert"),
        GR = c("Σύνδρομο", "αρ. περιστατικών", "Παρατηρούμενη αναλ. νοσηρότητα", 
            "Αναμενόμενη αναλ. νοσηρότητα", "Z-score*", "Ειδοποίηση", "Εγρήγορση"))[[lang]]
    table1
}

table2a <- do.call("rbind", lapply(1:14, function(i) {
    res <- subset(fits[[i]], dates==tgtdate-1 & alerts==1)
    if (nrow(res)>0) res$syndro <- i
    res
}))

table2b <- do.call("rbind", lapply(1:length(fitsD), function(j){
    res2 <- do.call("rbind", lapply(1:14, function(i) {
        res <- subset(fitsD[[j]][[i]], dates==tgtdate-1 & alerts==1)
        if (nrow(res)>0) res$syndro <- i
        res
    }))
    if (nrow(res2)>0) res2$camp <- names(fitsD)[j]
    res2
}))

table3a <- do.call("rbind", lapply(1:14, function(i) {
    res <- subset(fits[[i]], dates>=tgtdate-7 & alerts==1)
    if (nrow(res)>0) res$syndro <- i
    res
}))

table3b <- do.call("rbind", lapply(1:length(fitsD), function(j){
    res2 <- do.call("rbind", lapply(1:14, function(i) {
        res <- subset(fitsD[[j]][[i]], dates>=tgtdate-7 & alerts==1)
        if (nrow(res)>0) res$syndro <- i
        res
    }))
    if (nrow(res2)>0) res2$camp <- names(fitsD)[j]
    res2
}))

AllAlerts <- do.call("rbind", lapply(1:14, function(i) {
    res <- subset(fits[[i]], alerts==1)
    if (nrow(res)>0) res$syndro <- i
    res
}))

AllAlertsD <- do.call("rbind", lapply(1:length(fitsD), function(j){
    res2 <- do.call("rbind", lapply(1:14, function(i) {
        res <- subset(fitsD[[j]][[i]], alerts==1)
        if (nrow(res)>0) res$syndro <- i
        res
    }))
    if (nrow(res2)>0) res2$camp <- names(fitsD)[j]
    res2
}))


formatTableA <- function(tb, lang="EN") {
    if(nrow(tb)==0) return(tb)
    tb <- tb[order(tb$syndro, -as.integer(tb$dates), decreasing=c(FALSE, TRUE, FALSE)),]
    rownames(tb) <- NULL
    tb$syndro <- syndroDesc[tb$syndro,lang]
    tb$pexp <- with(tb, Pnb/n)
    tb$p <- round(tb$p*100, 1)
    tb$pexp <- round(tb$pexp*100, 1)
    tb$zscore <- round(tb$zscore,3)
    tb <- tb[,c("syndro", "dates", "x", "p", "pexp", "zscore")]
    colnames(tb) <- list(
        EN = c("Syndrome", "Reporting date", "No of cases", 
            "Obs. prop. morbidity", "Exp. prop. morbidity", "Z-score"),
        GR = c("Σύνδρομο", "Ημ. δήλωσης", "αρ. περιστατικών", 
            "Παρατηρούμενη αναλ. νοσηρότητα", "Αναμενόμενη αναλ. νοσηρότητα", "Z-score"))[[lang]]
    tb
}


formatTableB <- function(tb, lang="EN") {
    if(nrow(tb)==0) return(tb)
    tb <- tb[order(tb$syndro, -as.integer(tb$dates), tb$camp, decreasing=c(FALSE, TRUE, FALSE)),]
    rownames(tb) <- NULL
    tb$syndro <- syndroDesc[tb$syndro,lang]
    tb$pexp <- with(tb, Pnb/n)
    tb$p <- round(tb$p*100, 1)
    tb$pexp <- round(tb$pexp*100, 1)
    tb$zscore <- round(tb$zscore,3)
    tb$campname <- camps[match(tb$camp, camps$codecamp), lang]
    tb <- tb[,c("syndro", "camp", "campname", "dates", "x", "p", "pexp", "zscore")]
    colnames(tb) <- list(
        EN = c("Syndrome", "Camp code", "Camp name", "Reporting date", "No of cases", 
            "Obs. prop. morbidity", "Exp. prop. morbidity", "Z-score"),
        GR = c("Σύνδρομο", "Κωδικός κέντρου", "Όνομα κέντρου", "Ημ. δήλωσης", "αρ. περιστατικών", 
            "Παρατηρούμενη αναλ. νοσηρότητα", "Αναμενόμενη αναλ. νοσηρότητα", "Z-score"))[[lang]]
    tb
}


syndroPerCamp <- function(date, syndro) {
    res <- do.call("rbind", lapply(1:length(fitsD), function(j){
        res2 <- subset(fitsD[[j]][[syndro]], dates==date)
        if (nrow(res2)>0) {
            res2$camp <- names(fitsD)[j]
            res2$syndro <- syndro
        }
        res2
    }))
}


detailsPerCamp <- lapply(1:14, function(i) {
    formatTableB(syndroPerCamp(tgtdate-1, i), lang)
})


plotReport <- function(camp, tgtdate, filename, lang) {
    if (camp=="") {
        fts <- fits
    } else {
        fts <- fitsD[[camp]]
    }
    plotRow <- function(i, legend.y=-1, mar=c(12,5,2,2)) {
        plotOne(fts[[i]], lwd=1, mar=mar, legend.y=legend.y, 
            main=c(EN="", GR=""), title=syndroDescList[[i]], lang=lang, stoptgt=TRUE)
        
        a <- tail(subset(fts[[i]], dates<tgtdate), 7)
        rownames(a) <- fmtDate(a$dates, lang)
        a <- a[,c("x","n","p","zscore","alerts", "alarms")]
        a$p <- round(a$p*100, 1)
        a$zscore <- round(a$zscore, 3)
        a$alerts[a$alerts==0] <- "-"; a$alerts[a$alerts==1] <- "NAI"
        a$alarms[a$alarms==0] <- "-"; a$alarms[a$alarms==1] <- "NAI"
        colnames(a) <- list(
            EN=c("Number of cases", "Total visits", "Proportion (%)", "Z-score", "Warning", "Alert"),
            GR=c("Αρ. περιστατικών", "Αρ. επισκέψεων", "Αναλογία (%)", "Z-score", "Ειδοποίηση", "Εγρήγορση"))[[lang]]
        par(mar=c(0,0,0,0), family="Fira Sans")
        textplot(t(a), valign="top", rmar=1.5, cmar=0.8)
        mtext(c(EN="Details for the past 7 days", GR="Λεπτομέρειες για τις τελευταίες 7 ημέρες")[lang], 
                side=3, line=-3)
    }
    plotFooter <- function() {
        mtext(c("EN"="Source: Hellenic Centre of Disease Control and Prevention (KEELPNO)", 
                "GR"="Πηγή: Κέντρο Ελέγχου και Πρόληψης Νοσημάτων (ΚΕΕΛΠΝΟ)")[lang], 
                side=1, cex=1.2, outer=TRUE, adj=1, line=2, font=3)
        mtext("__________________________", side=1, outer=TRUE, adj=1)
    }
    cairo_pdf(filename, width=21/2.54, height=29.7/2.54, pointsize=8, onefile=TRUE)
    par(oma=c(10,7,18,7), mfrow=c(4,2))
    for (i in 1:4) plotRow(i, -0.7)
    if (camp=="") {
        mtext(c("EN"="Daily report from all camps", "GR"="Ημερήσια αναφορά από όλα τα κέντρα")[lang], 
            side=3, cex=2.8, outer=TRUE, line=9, adj=0)
    } else {
        mtext(sprintf("%s %s: %s", c("EN"="Camp", "GR"="Κέντρο")[lang], camp, camps[match(x, camps$codecamp), lang]), side=3, cex=2.8, outer=TRUE, line=9, adj=0)
    }
    mtext(sprintf("%s: %s", c("EN"="Date", "GR"="Ημερομηνία")[lang], tgtdate), 
            side=3, cex=2, outer=TRUE, line=5, adj=0)
    plotFooter()

    par(oma=c(10,7,12,7), mfrow=c(5,2))
    for (i in 5:9) plotRow(i, mar=c(12,5,2,2))
    mtext(sprintf("%s: %s", c("EN"="Date", "GR"="Ημερομηνία")[lang], tgtdate), 
            side=3, cex=2, outer=TRUE, line=5, adj=0)
    plotFooter()
    
    par(oma=c(10,7,12,7), mfrow=c(5,2))
    for (i in 10:14) plotRow(i, mar=c(12,5,2,2))
       mtext(sprintf("%s: %s", c("EN"="Date", "GR"="Ημερομηνία")[lang], tgtdate), 
            side=3, cex=2, outer=TRUE, line=5, adj=0)
    plotFooter()

    dev.off()
}



dir.create("output", showWarnings=FALSE)
dir.create("output/daily", showWarnings=FALSE)
dir.create(sprintf("output/daily/%s", tgtdatef2), showWarnings=FALSE)

cat("Φτιάχνω τις αναφορές (σε δύο γλώσσες)...\n")
lang <- "EN"
odfWeave("input/daily-en-template.odt", sprintf("output/daily/%s/daily-en-%s.odt", tgtdatef2, tgtdatef2))
WriteXLS("detailsPerCamp", sprintf("output/daily/%s/syndroDetails-en-%s.xls", tgtdatef2, tgtdatef2), SheetNames=syndroShort, FreezeRow=1)
lang <- "GR"
odfWeave("input/daily-gr-template.odt", sprintf("output/daily/%s/daily-gr-%s.odt", tgtdatef2, tgtdatef2))
WriteXLS("detailsPerCamp", sprintf("output/daily/%s/syndroDetails-gr-%s.xls", tgtdatef2, tgtdatef2), SheetNames=syndroShort, FreezeRow=1)


if (tgtweek>0) {
    dir.create("output", showWarnings=FALSE)
    dir.create("output/weekly", showWarnings=FALSE)
    dir.create(sprintf("output/weekly/%s", tgtweek), showWarnings=FALSE)

    NcampsW <- length(unique(subset(aggrD, isoweek(hmedil, "both_num")==tgtweek)$codecamp))
    tgtweekR <- paste(format(isoweekStart(tgtweek), "%d/%m"), c(EN="to", GR="έως")[lang], format(isoweekStart(tgtweek)+6, "%d/%m"))
    
    cat("Φτιάχνω τις εβδομαδιαίες αναφορές (σε δύο γλώσσες)...\n")
    #lang <- "EN"
    #odfWeave("input/weekly-en-template.odt", paste("output/weekly/", tgtweek, "/weekly-en-", tgtweek, ".odt", sep=""))
    lang <- "GR"
    odfWeave("input/weekly-gr-template.odt", paste("output/weekly/", tgtweek, "/weekly-gr-", tgtweek, ".odt", sep=""))
}


cat("Ολοκληρώθηκαν οι αναφορές (σε δύο γλώσσες).\n")
cat("Φτιάχνω τώρα αναλυτικό output ανά κέντρο, παρακαλώ περιμένετε...\n")

makePNG <- function(lang="EN") {
    dir.create(sprintf("output/daily/%s", tgtdatef2), showWarnings=FALSE)   # To be on the safe side...
    dir.create(sprintf("output/daily/%s/%s", tgtdatef2, lang), showWarnings=FALSE)   # To be on the safe side...
    for (x in camps$codecamp) {
        dir.create(sprintf("output/daily/%s/%s/%s", tgtdatef2, lang, x), showWarnings=FALSE)
        for (i in 1:14) {
            png(sprintf("output/daily/%s/%s/%s/camp%s-%s-%s.png", tgtdatef2, lang, x, x, i, tgtdatef2), width=3800, height=2200, res=400)
            plotOne(fitsD[[x]][[i]], lang=lang, stoptgt=TRUE)
            dev.off()
        }
    }
    for (i in 1:14) {
        png(sprintf("output/daily/%s/%s/allcamps-%s-%s.png", tgtdatef2, lang, i, tgtdatef2), width=3800, height=2200, res=400)
        plotOne(fits[[i]], lang=lang, stoptgt=TRUE)
        dev.off()
    }
}


dir.create(sprintf("output/daily/%s", tgtdatef2), showWarnings=FALSE)
for (lang in c("EN", "GR")) {
    # Setting up directories
    dir.create(sprintf("output/daily/%s/%s", tgtdatef2, lang), showWarnings=FALSE)
    for (x in camps$codecamp) {
        plotReport(x, tgtdate, sprintf("output/daily/%s/%s/report%s-%s-%s.pdf", tgtdatef2, lang, lang, x, tgtdatef2), lang)
    }
    plotReport("", tgtdate, sprintf("output/daily/%s/%s/report%s-All-%s.pdf", tgtdatef2, lang, lang, tgtdatef2), lang)
    
    a <- formatTableA(AllAlerts, lang=lang); a <- a[order(a[,4], a[,1], a[,2]),]
    write.csv2(a, file=sprintf("output/daily/%s/%s/AlertsAllCamps%s-%s.csv", tgtdatef2, lang, lang, tgtdatef2), row.names=FALSE, na="")
    a <- formatTableB(AllAlertsD, lang=lang); a <- a[order(a[,4], a[,1], a[,2]),]
    write.csv2(a, file=sprintf("output/daily/%s/%s/AlertsByCamp%s-%s.csv", tgtdatef2, lang, lang, tgtdatef2), row.names=FALSE, na="")
    rm(a)
}


lang <- "EN"

save.image(sprintf("output/daily/%s/output %s.RData", tgtdatef2, tgtdatef2))
save.image("output/latest_output.RData")

cat("Ολοκληρώθηκε!\n\n")
