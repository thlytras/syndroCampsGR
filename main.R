
cat("\nΚαλωσήλθατε στο πρόγραμμα επεξεργασίας δεδομένων συνδρομικής επιτήρησης.\n\n")

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


source("1 - fitModels.R")
save.image("output/latest_fits.RData")

if (makeOutputs) {
    source("2 - makeOutput.R")
    save.image("output/latest_output.RData")
}

