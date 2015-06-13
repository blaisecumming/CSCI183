GSS = read.csv("C:/GSS.csv") #This csv files was provided by D. Nola from Slack, it contains every year from 1974 up to present data.

library(vcd) #This import allows us to create mosaics.


new = GSS[GSS[1]==2010,] #We would like to recreate a chart using the 2010 and 2012 data.
View(new)
new2 = GSS[GSS[1]==2012,]
View(new2)
new[2045:4018,] = new2 # Add all the rows from new2 to new1 starting at the next open row.
View(new)




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




#MIDDLers
summary(grepl('Cant', new[,4]))
# Mode   FALSE    TRUE    NA's 
#logical    3991      27       0 
summary(grepl('Neither', new[,4]))
#Mode   FALSE    TRUE    NA's 
#logical    3699     319       0 
summary(grepl('Not applicable', new[,4]))
#Mode   FALSE    TRUE    NA's 
#logical    2589    1429       0

#BOTTOMers
summary(grepl('Strongly dis', new[,4]))
#Mode   FALSE    TRUE    NA's 
#logical    3386     632       0 
summary(grepl('Disag', new[,4]))
#Mode   FALSE    TRUE    NA's 
#logical    3644     374       0 


#TOPPERs
summary(grepl('Strongly a', new[,4]))
#Mode   FALSE    TRUE    NA's 
#logical    3425     593       0 
summary(grepl('Agr', new[,4]))
#Mode   FALSE    TRUE    NA's 
#logical    3387     631       0 


#Those who agree with Gay Marraige Rights would be the categories Agree, Strongly agree. 593+631 = 1224
#Those who disagree with Gay Marraige Rights would be the categories Disagree, Strongly disagree. 632+374 = 1006

#These metrics are important, because they tell us if we are on track. The ratio of the two compared to the total is 1.21~ which is the same ratio
#on FiveThirtyEight's site (56% FOR versus 45% AGAINST - 101% though?)

#Now that we have accurately sorted by Rights, need to compare to economic.


#Combine the rows which Agree and Strongly agree, and determine the metrics for the upper half.
#Combine the rows which Disagree and Strongly Disagree, and determine the metrics for the lower half.


#LEFT
summary(new[grepl('Govt reduce diff', new[,3]),]) #568
summary(new[grepl('6', new[,3]),]) #183 6's across the board
summary(new[grepl('5', new[,3]),]) #363
#1114 Total LEFTers

#RIGHT
summary(new[grepl('No govt action', new[,3]),]) #429
summary(new[grepl('3', new[,3]),]) #413
summary(new[grepl('2', new[,3]),]) #238
#1080 Total RIGHTers

#MIDDLE
summary(new[grepl('Not applicable', new[,3]),]) #1303
summary(new[grepl('answer', new[,3]),]) #8 No Answer
summary(new[grepl('know', new[,3]),]) #24 Don't knows
summary(new[grepl('4', new[,3]),]) #Median: 489 Similar to no opinion; but signifies the government is currently on track.
#1824 Total MIDDLers


#Our numbers total up to 4018 which is the number of observations that we had for 2010-2012. Good.

#Observations which were not one way or another (4018-1114-1080) = 1824

#Those who want no government involvement or voted below the median on a scale 2-6 for government involvement would be far right (429 + 238 + 413) = 1080 = 26.9%

#Those who voted 5 or 6 on the scale or for govt to reduce diff would be left (183 + 363 + 568) = 1114 = 27.7% far left

#The chart shows 54% left versus 47% right. (Once again 101%). 1114/1080 ~ 54/47 ? 


#In favor of economic
left_1 = summary(new[grepl('Govt reduce diff', new[,3]),])[,4]
left_2 = summary(new[grepl('6', new[,3]),])[,4]
left_3 = summary(new[grepl('5', new[,3]),])[,4]

right_1 = summary(new[grepl('No govt action', new[,3]),])[,4]
right_2 = summary(new[grepl('2', new[,3]),])[,4]
right_3 = summary(new[grepl('3', new[,3]),])[,4]

middle_1 = summary(new[grepl('Not applicable', new[,3]),])[,4]
middle_2 = summary(new[grepl('answer', new[,3]),])[,4]
middle_3 = summary(new[grepl('know', new[,3]),])[,4]
middle_4 = summary(new[grepl('4', new[,3]),])[,4]

#In favor of Rights
midd_1 = summary(new[grepl('Cant', new[,4]),], maxsum = 12)[,3]
midd_2 = summary(new[grepl('Neither', new[,4]),], maxsum = 12)[,3]
midd_3 = summary(new[grepl('Not applicable', new[,4]),], maxsum = 12)[,3]

bottom_1 = summary(new[grepl('Strongly dis', new[,4]),], maxsum = 12)[,3]
bottom_2 = summary(new[grepl('Disag', new[,4]),], maxsum = 12)[,3]


top_1 = summary(new[grepl('Strongly a', new[,4]),], maxsum = 12)[,3]
top_2 = summary(new[grepl('Agr', new[,4]),], maxsum = 12)[,3]


#Having trouble getting Mosaic list to apply itself on a LIST object.