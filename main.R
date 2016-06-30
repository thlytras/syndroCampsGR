
cat("\nΚαλωσήλθατε στο πρόγραμμα επεξεργασίας δεδομένων συνδρομικής επιτήρησης.\n\n")


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


isoweekStart <- function(x) {
  year <- x %/% 100
  week <- x %% 100
  x.date <- as.Date(paste(year,"-6-1", sep=""))
  x.weekday <- as.integer(format(x.date,"%w"))
  x.weekday[x.weekday==0]=7
  x.nearest.thu <- x.date-x.weekday+4
  x.isoweek <- isoweek(x.nearest.thu)
  res <- x.nearest.thu + 7*(week-x.isoweek) - 3
  if (sum(isoweek(res, type="both_num") != x)>0) stop("Error specifying ISO week number")
  return(res)
}


tgtdate <- Sys.Date()
lang <- "EN"


repeat {
  input <- readline(paste("Ημερομηνία συλλογής στοιχείων? (YYYY-MM-DD) [", tgtdate, 
                ", δεδομένα μέχρι ", tgtdate-1, "] ",sep=""))
  if(input=="") { break }
  else {
    suppressWarnings( input <- as.Date(input) )
    if (!is.na(input)) { tgtdate <- input; break }
    cat("\nΕσφαλμένη εισαγωγή - ξαναπροσπαθήστε!\n")
  }
}
cat("\n")

makeOutputs <- TRUE
repeat {
  input <- readline(paste("(Aν όχι, κάνω μόνο fit τα μοντέλα). Να βγάλω την αναφορά? (y/n) [y] ",sep=""))
  if(input=="") { break }
  else {
    suppressWarnings( input <- tolower(input) )
    if (input %in% c("n","y")) { makeOutputs <- c("n"=FALSE, "y"=TRUE)[input]; break }
    cat("\nΕσφαλμένη εισαγωγή - ξαναπροσπαθήστε!\n")
  }
}
cat("\n")

tgtweek <- 0

if (makeOutputs) {
    tgtweek <- isoweek(Sys.Date()-7,"both_num")
    repeat {
    input <- readline(paste("Να βγάλω εβδομαδιαία αναφορά? Κι αν ναι, για ποιά εβδομάδα?\n   (YYYYWW, 0 = όχι αναφορά) [", tgtweek, "] ",sep=""))
    if(input=="") { break }
    else {
        suppressWarnings(input<-as.integer(input))
        if (!is.na(input) && (input%%100<54) && input<=tgtweek) { tgtweek<-input; break }
        cat("\nΕσφαλμένη εισαγωγή - ξαναπροσπαθήστε!\n")
    }
    }
    cat("\n")
}

source("1 - fitModels.R")
save.image("output/latest_fits.RData")

if (makeOutputs) {
    source("2 - makeOutput.R")
    save.image("output/latest_output.RData")
}

