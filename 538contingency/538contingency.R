GSS = read.csv("C:/GSS.csv") #This csv files was provided by D. Nola from Slack, it contains every year from 1974 up to present data.

library(vcd) #This import allows us to create mosaics.


new = GSS[GSS[1]==2010,] #We would like to recreate a chart using the 2010 and 2012 data.
View(new)
new2 = GSS[GSS[1]==2012,]
View(new2)
new[2045:4018,] = new2 # Add all the rows from new2 to new1 starting at the next open row.
View(new)


summary(grepl('Cant', new[,4]))
# Mode   FALSE    TRUE    NA's 
#logical    3991      27       0 
summary(grepl('Neither', new[,4]))
#Mode   FALSE    TRUE    NA's 
#logical    3699     319       0 
summary(grepl('Not applicable', new[,4]))
#Mode   FALSE    TRUE    NA's 
#logical    2589    1429       0 

#This let's us realize that 1775/4018 people do not have an opinion about Gay Marraige. This is important because it represents 44.176%
#of the surveyed observations. It will be important when trying to determine the scale of the entire chart.

Notapplic = new[grepl('Not applicable', new[,4]),]
Notapplic2 =  new[grepl('Neither', new[,4]),]
Notapplic3 = new[grepl('Cant', new[,4]),]
Notapplic[1430:1748,] = Notapplic2
Notapplic[1749:1775,] = Notapplic3
summary(Notapplic)
# Gss.year.for.this.respondent Respondent.id.number Should.govt.reduce.income.differences        Homosexuals.should.have.right.to.marry
#2010   :763                  1001   :   2         Govt reduce diff:297                  Not applicable            :1429              
#2012   :666                  1007   :   2         4               :256                                            :   0              
#:  0                  1022   :   2         3               :228                  Agree                     :   0              
#1972   :  0                  1034   :   2         No govt action  :217                  Cant choose               :   0              
#1973   :  0                  1042   :   2         5               :194                  Disagree                  :   0              
#1974   :  0                  105    :   2         2               :133                  Neither agree nor disagree:   0              
#(Other):  0                  (Other):1417         (Other)         :104                  (Other)                   :   0         

#This summary is important because it shows the change from 2010 to 2012, in that less people are unopinionated with regards to Gay Marraige Rights.

#Now we should be able to filter down all the people who are unopinionated about this into categories about economics.


Notapplic_income1 = Notapplic[grepl('Govt reduce diff', Notapplic[,3]),]
#This table now shows all the people who are unopinionated about Rights but opinionated about income disparity.
Notapplic_no = Notapplic[grepl('No govt action', Notapplic[,3]),]
#This table now shows all the people who have no opinion of Gay Marraige Rights and no interest in government sanctions.

Notapplic_notap = Notapplic[grepl('Not applicable', Notapplic[,3]),]
#This table shows all the people who have no opinions

Notapplic_answer = Notapplic[grepl('answer', Notapplic[,3]),] 
#No answer -> no is too common

Notapplic_know = Notapplic[grepl('know', Notapplic[,3]),] 
#Don't know -> know is unique string
Notapplic_six = Notapplic[grepl('6', Notapplic[,3]),] # Sort the different factors
Notapplic_five = Notapplic[grepl('5', Notapplic[,3]),] # Sort the different factors
Notapplic_four = Notapplic[grepl('4', Notapplic[,3]),] # Sort the different factors
Notapplic_three = Notapplic[grepl('3', Notapplic[,3]),] # Sort the different factors
Notapplic_two = Notapplic[grepl('2', Notapplic[,3]),] # Sort the different factors

sum(nrow(Notapplic_six),nrow(Notapplic_five))
#Shows us 1012 people felt like using a scale coefic. to represent the amount of govt action.
#2 is low to none, and 6 is high. This means 4 is equivalent to not applicable since its the median.

#The first sum is people who were in some kind of favor towards action
sum(nrow(Notapplic_three),nrow(Notapplic_two))
#This next sum is people who were in some kind of favor against gov action

#So now we know 397 people were against, and 318 were for. Remember this only applies to unopinionated.

summary(new[grepl('Not applicable', new[,3]),])
summary(new[grepl('answer', new[,3]),]) #8 No Answer
summary(new[grepl('know', new[,3]),]) #24 Don't knows
summary(new[grepl('6', new[,3]),]) #183 6's across the board
summary(new[grepl('5', new[,3]),]) #363
summary(new[grepl('4', new[,3]),]) #Median: 489 Similar to no opinion; but signifies the government is currently on track.
summary(new[grepl('3', new[,3]),]) #413
summary(new[grepl('2', new[,3]),]) #238
