*Author: Dann Hekman*  
*Project: Presidential Election Trends*  
##Data
Using Data from www.270toWin.com to examine trends in US Presidential Party Politics. Specifically, looking at how long one party maintains its hold on the White House and how, if it all, that has changed over the last 250 years.  

##Disseminations  
https://www.github.com/dannhek/presidents  
http://rpubs.com/tattooed_economist/party_dynasties  
https://tattooedeconomist.wordpress.com/2017/08/01/how-long-does-one-party-control-the-white-house-in-us-politics/  

##Files
**length_of_party_control.R** - This file does the heavy lifting for calculating the answer to four questions:  
1. How often are incumbent presidents re-elected?  
2. How Long does a single party stay in the White House?  
3. Given the current number of consecutive terms, what's the probability of the incumbent party winning?  
4. Are 'runs' getting shorter? I.e. do party changes in the White House occur more or less frequently now than previously?  
5. How long do political parties last?  
**get_election_data** - This file does the web scrubbing from www.270towin.com and writes two dataframes as csv files to the current working directory.  
**How long does one party control the White House in US Politics.rmd** - R Markdown File for writing up this project. 
