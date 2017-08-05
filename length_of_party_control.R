#Length of Party Control.R
#Author: Dann Hekman
#Project: Presidential Election Trends
#     Using Data from www.270toWin.com to examine trends in
#     US Presidential Party Politics. Specifically, looking at
#     how long one party maintains its hold on the White House
#     and how, if it all, that has changed over the last 250 years.
#   www.github.com/dannhek/presidents
#   http://rpubs.com/tattooed_economist/party_dynasties
#   tattooedeconomist.wordpress.com
#File: length_of_party_control.R
#     This file does the heavy lifting for calculating the answer to
#     four questions:
#       1.) How often are incumbant presidents re-elected?
#       2.) How Long does a single party stay in the White House?
#       3.) Given the current number of consecutive terms,
#             what's the probability of the incumbant party winning?
#       4.) Are 'runs' getting shorter? I.e. do party changes in the
#             White House occur more or less frequently now than previously?
#       5.) How long do political parties last?
#Dependencies
#   Packages: See Below
#   This file depends on:
#       get_election_data.R - Gets the data from 270toWin and formats it
#
#   Files that rely on this file:
#       [filename].rmd - Write-up for this Project

#Load Required Packages
for (pkg in c('plyr','dplyr','ggplot2','XML','zoo','knitr')) {
     if (!is.element(pkg,installed.packages()[,1])) {
          r <- getOption("repos")
          r["CRAN"] <- "http://cran.us.r-project.org"
          options(repos = r)
          rm(r)
          install.packages(pkg)
        }
      library(pkg,character.only = T)
}

#Load the Data. Pull it from Local files if they exist
#   or from http://www.270toWin.com using get_election_data.R if necessary
#setwd("C:/Users/dhek/Google Drive/Personal Projects/R and AWS/Presidents")
rm(list=ls()) #clear whatever else is currently loaded from previous sessions
if (file.exists("all_elections.csv")){
     allElections <- read.csv("all_elections.csv")
     electionsByYear <- read.csv('elections_by_year.csv')
} else {source("get_election_data.R")}


#Question 1: How often are incumbant presidents re-elected?
#   Get cases where an incumbant was running, and calculate the proportion
#   where the incumbant won.
x <- subset(electionsByYear,incumbantPresWin==TRUE | incumbantPresUpset==TRUE)
question1answer <- list(
    all_time  = sum(x$incumbantPresWin)/nrow(x)
   ,since1950 = sum(subset(x,year>1950,TRUE)$incumbantPresWin)/nrow(subset(x,year>1950,TRUE))
   ,befor1950 = sum(subset(x,year<1950,TRUE)$incumbantPresWin)/nrow(subset(x,year<1950,TRUE))
   )
rm(x)

#Question 2: How Long do parties stay in the whitehouse?
#   Create a subset dataframe and loop through, calculating a running count
#   of how long that party has been in the White House, getting reset with each
#   party change (i.e. previous winner's party != current winner's party)
x <- subset(electionsByYear,TRUE,c('year','president','party','incumbantParty'))
x$run <- NA ; x$q2 <- NA ; x$q3 <- NA
for (ln in 1:nrow(x)) {
     change <- (x[ln,3]!=x[ln,4])
     #'run' is the 'dynasty' length so far
     x$run[ln] <- ifelse(change|is.na(change),1,(x$run[ln-1]+1))
     #'q2' is the run length of the previous 'dynasty'. Only populated when parties change
     x$q2[ln] <- ifelse(change|is.na(change),(x$run[ln-1]),NA)
     #'q3' is the run length of the previous 'dynasty'. Only populated on the last presidency before parties change
     x$q3[ln-1] <- ifelse(change|is.na(change),x$run[ln-1],NA)
}
question2answer <- ggplot(subset(x,!is.na(q2)),aes(x=q2)) +
                    ggtitle("Figure 1: Frequency of Consecutive Terms before Turnover") +
                    scale_x_continuous(breaks=c(0:8)) +
                    geom_histogram(binwidth=1,fill='darkred',size=1.2,color='lightgray') +
                    xlab("Consecutive Terms") +
                    ylab("Count") +
                    theme(
                         panel.background = element_rect(fill="lightgray"),
                         panel.grid.minor.x = element_line(FALSE),
                         panel.grid.major.x = element_line(FALSE)
                         )

#Question 3: Given the current number of consecutive terms, what's the probability of the incumbant party winning?
#   Obviously, this doesn't take polling or political situation into account; just looking at historical frequencies
question3answer <- rbind(ddply(x, .(run), summarize,
           count_den = length(run),
           count_num = sum(!is.na(q3))
           ),c(8,0,0))
question3answer$q3 <- (1-(question3answer$count_num/question3answer$count_den))

#Question 4: Are runs getting shorter?
#     What's the running average length of how many terms a party has held the White House?
#     Calculates both the "noisy" answer looking at how long the previous party has been in the WH for each election
#     as well as the "smooth" answer that only looks at the the last 5 'dynasties'
numPeriods <- 5
x$q4a <- c(rep(NA,numPeriods-1),rollmean(x$run,numPeriods))
#question4answerA <- ggplot(data=x,aes(x=year)) +
#                         ylim(c(0,7)) +
#                         geom_line(aes(y=q4a)) +
#                         geom_point(aes(y=q3))

x <- subset(x,!is.na(q3))
x$q4b <- c(rep(NA,numPeriods-1),rollmean(x$q3,numPeriods))
# question4answerB <- ggplot(data=x,aes(x=year)) +
#      ylim(c(0,7)) +
#      geom_line(aes(y=q4b,colour='Avg. Run Length of Last 3 \'Dynasties\'')) +
#      geom_point(aes(y=q3))

question4answerC <- ggplot(data=x,aes(x=year)) +
     ylim(c(0,7)) + ylab('Consecutive Terms by One Party') + xlab('Year') +
     geom_line(aes(y=q4b,colour=paste0('Avg. Run Length of Last ',numPeriods,' \'Dynasties\''))) +
     geom_line(aes(y=q4a,colour=paste0('Avg. Run Length within Last ',numPeriods,' Elections'))) +
     geom_smooth(aes(y=q4b),colour='black',method='lm',size=0.2) +
     geom_point(aes(y=q3)) +
     theme(
          panel.background = element_rect(fill="lightgray"),
          panel.grid.minor.x = element_line(FALSE),
          panel.grid.major.x = element_line(FALSE),
          legend.position = 'bottom'
     ) + ggtitle('Figure 2: Length of Political Party Dynasties over Time') +
     labs(color='')

question4answerC


question4answerD <- list(
      sd_AllYears  = sd(x$q3)
     ,sd_Befor1900 = sd(subset(x,year<1900)$q3)
     ,sd_since1900 = sd(subset(x,year>=1900)$q3)
)

#Question 5: How long do Parties Last?
y<-ddply(electionsByYear,.(party),summarize,start=min(year),end=max(year)+4)
y$length <- y$end-y$start
question5answer <- kable(y[order(y$start),],format='pandoc',row.names=FALSE,
                      col.names=c('Political Party Name','Year of First Presidential Win','Year of Most Recent President in Office','Age (Years)'),
                      caption='Age of Political Parties; Source = www.270toWin.com'
)

#Save off a couple PNGs
png('histogram_of_runs.png') ; question2answer ; dev.off()
png('moving_avg_plot.png') ; question4answerC ; dev.off()
