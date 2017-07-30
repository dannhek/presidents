#Get Election Data.R
#Author: Dann Hekman
#Project: Presidential Election Trends
#     Using Data from www.270toWin.com to examine trends in
#     US Presidential Party Politics. Specifically, looking at
#     how long one party maintains its hold on the White House
#     and how, if it all, that has changed over the last 250 years.
#   www.github.com/dannhek/presidents
#   http://rpubs.com/tattooed_economist/party_dynasties
#   tattooedeconomist.wordpress.com
#File: get_election_data.R
#     This file pulls all the data from 270toWin and formats it into 2 dataframes:
#       1.) allElections - a formatted version of 270toWin with all years in one file.
#             includes all candidates and raw data for electoral college and popular Votes
#             incumbant candidates (if they are running) are denoted with an appended (I)
#       2.) electionsByYear - summarizes each election with the columns needed in length_of_party_control.R
#     Then, both files are saved off to CSVs in the CWD to improve performance in the future.
#Dependencies
#   Packages: See Below
#   This file depends on:
#       http://www.270toWin.com as it was formated in July of 2017
#   Files that rely on this file:
#       length_of_party_control.R - does the actual calculations to answer my questions
#       [filename].rmd - Write-up for this Project

#Load Required Packages
for (pkg in c('XML')) {
     if (!require(pkg,character.only = T)) {
          install.packages(pkg)
          library(pkg,character.only = T)
     }
}

#Function to get one year from 270toWin.com
#   internal function, so no data validation to make sure the inputted year actually was an election year.
webSiteToDF <- function(year) {
     url <- paste0('http://www.270towin.com/',year,'_Election/')
     x <- readHTMLTable(url)[[4]]
     oneElection <- data.frame(
           candidate = iconv(as.character(x$Candidate),"latin1","ASCII",sub="")
          ,party     = as.factor(gsub("^[[:space:]]","",x$Party))
          ,elecVotes = as.numeric(gsub("^[[:space:]]","",x$`Electoral Votes`))
          ,winner    = x[,1]!=''
     )
     if (!is.null(x$`Popular Votes`)) {
          oneElection$popuVotes <- as.numeric(gsub('[^0-9]',"",as.character(x$`Popular Votes`)))
     } else {oneElection$popuVotes <- NaN}

     #Special Handling for Historical Anomolies
     if (year==1912) levels(oneElection$party)[2] <- "Bull Moose" #Bull Moose Instead of Progressive

     oneElection$year <- year
     return(oneElection)
}

#Test me!
#webSiteToDF(1824)

#Compile all election years into a new data frame with all candidates
allElections <- data.frame(
                     candiate = NULL
                    ,party = NULL
                    ,elecVotes = NULL
                    ,popuVotes = NULL
                    ,year = NULL
                    ,winner = NULL
)
for (year in c(1789, seq(from = 1792, to = 2016, by = 4))) {
     oneElection <- webSiteToDF(year)
     #print(oneElection)
     allElections <- rbind(allElections,oneElection)
}

#Summarize elections by year. Includes information about the previous administration and new administration
#   like party affiliation and strength of popular mandate
#First, create two subsets from allElections and join them on year
electionsByYear <- subset(inner_join(ddply(allElections,.(year),summarize,
                                           pctElec = sum(elecVotes*winner) / sum(elecVotes),
                                           pctPopu = sum(popuVotes*winner) / sum(popuVotes),
                                           hasIncombant = as.logical(sum(grepl('\\(I\\)$',candidate)))
                                          ),
                                    subset(allElections,winner==TRUE,TRUE),
                                      by=c('year'),
                                      match='first'
                                      ), TRUE,
                            c('year','candidate','party','elecVotes','pctElec','pctPopu','hasIncombant')
                            )
#Now, add a bunch of calculated columns and remove some obsolete ones
electionsByYear <- subset(within(electionsByYear,{
     incumbantParty = lag(electionsByYear$party,1)  #already sorted by year.
     incumbantPresWin = (hasIncombant & grepl('\\(I\\)$',candidate))
     incumbantPresUpset = (hasIncombant & !grepl('\\(I\\)$',candidate))
     president = gsub('\\(I\\)','',candidate)
}),TRUE,c('year','president','party','incumbantParty','incumbantPresWin','incumbantPresUpset','elecVotes','pctElec','pctPopu')
)

#Save the dataframes as CSVs to the working directory
write.csv(allElections,"all_elections.csv")
write.csv(electionsByYear,'elections_by_year.csv')
