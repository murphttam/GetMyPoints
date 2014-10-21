GetMyPoints
===========

Exercise that allows me to get my points for ISAT252

function(me) {

source_https <- function(u, unlink.tmp.certs = FALSE) {

 # load package

 require(RCurl)

 # read script lines from website using a security certificate

 if(!file.exists("cacert.pem")) {

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")

 }

 script <- getURL(u, followlocation = TRUE, cainfo = "cacert.pem")

 if(unlink.tmp.certs) unlink("cacert.pem")

 # parse lines and evaluate in the global environment

 eval(parse(text = script), envir= .GlobalEnv)

}
library(XML)

readSpreadsheet <- function(url, sheet = 1){
   library(httr)
   r <- GET(url)
   html <- content(r)
   sheets <- readHTMLTable(html, header=FALSE, stringsAsFactors=FALSE)
   df <- sheets[[sheet]]
   dfClean <- function(df){
     nms <- t(df[1,])
     names(df) <- nms
     df <- df[-1,-1] 
     row.names(df) <- seq(1,nrow(df))
     ##df[,5] <- as.numeric(as.character(df[,5]))
     df
   }
   dfClean(df)
 }

url <- "https://docs.google.com/spreadsheets/d/1IUNwWVd9tHQeibzGWiAT1EJ6KxeHVo_XDvJHZtesRS0/pubhtml"
df <- readSpreadsheet(url)

courses <- unique(df$class)
sc.pairs <- unique(df[,c("student","class")])
lookup.class <- new.env()
for (i in 1:nrow(sc.pairs)) {

 lookup.class[[ sc.pairs$student[i] ]] <- sc.pairs$class[i] 

}
lookup.class[["murph4ma"]]
me = "murph4ma"
which(courses==lookup.class[[me]])

my.course <- which(courses==lookup.class[[me]])
if (nrow(df[df$student=="ALL" & df$class==courses[my.course],]) > 0) {

 current.all <- sum(as.integer(df[df$student=="ALL" & 
 	df$class==courses[my.course],]$points))

 } else {

 current.all <- 0

 }
 for (this.course in 1:length(my.course)) {

 for (i in 1:nrow(sc.pairs)) {

 lookup.class[[ sc.pairs$student[i] ]] <- sc.pairs$class[i] 

}

for (this.course in 1:length(my.course)) {
 current.totals <- aggregate(as.integer(df[df$class==courses[this.course],]$points), 
 	by=list(df[df$class==courses[this.course],]$student), FUN=sum)
 writeLines(paste("Leaderboard for",courses[this.course]), con=stdout(), sep="\n")
 print(current.totals[rev(order(current.totals$x)),], quote=FALSE, row.names=FALSE)
}

current.all

for (s in me) {
 my.stuff <- rbind(df[df$student=="ALL" &
 	df$class==courses[my.course],],df[df$student==s,])
 my.points <- sum(as.integer(my.stuff$points))
 writeLines(paste(s,"-- you currently have", my.points,
 "points! The tasks you have completed are:"), con=stdout(), sep="\n\n")
 print(my.stuff, quote = FALSE, row.names = FALSE)
 }
}

getMyCurrentPoints
