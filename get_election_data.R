#Get Election Data.R
#Author: Dann Hekman
#www.github.com/dannhek/presidential_elections



if (!require(XML)) install.packages('XML')
library(XML)

#Function to get one year from 270toWin.com
webSiteToDF <- function(year) {
     url <- paste0('http://www.270towin.com/',year,'_Election/')
     x <- readHTMLTable(url)[[4]]
     oneElection <- data.frame(
           candidate = iconv(as.character(x$Candidate),"latin1","ASCII",sub="")
          ,party     = as.factor(gsub("^[[:space:]]","",x$Party))     
          ,elecVotes = as.numeric(gsub("^[[:space:]]","",x$`Electoral Votes`))     
     )
     if (!is.null(x$`Popular Votes`)) {
          oneElection$popuVotes <- as.numeric(gsub('[^0-9]',"",as.character(x$`Popular Votes`)))
     } else {oneElection$popuVotes <- NaN}
     
     #Special Handling for Historical Anomolies
     if (year==1912) levels(oneElection$party)[2] <- "Bull Moose" #Bull Moose Instead of Progressive
     if (year==1800) oneElection[oneElection$candidate=="Aaron Burr",]$elecVotes <- 72.99  #Control for Jefferson/Burr tie and the 12th amendment
     
     oneElection$year <- year
     return(oneElection)
     
}

#webSiteToDF(1800)

allElections <- data.frame(
                     candiate = NULL
                    ,party = NULL
                    ,elecVotes = NULL
                    ,popuVotes = NULL
                    ,year = NULL
)
for (year in c(1789, seq(from = 1792, to = 2016, by = 4))) {
     oneElection <- webSiteToDF(year)
     #print(oneElection)
     allElections <- rbind(allElections,oneElection)
}

write.csv(allElections,"all_elections.csv")
