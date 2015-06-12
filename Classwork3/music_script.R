#Load our CSV
music = read.csv("C:/music-all.csv")
#Impute NAs as Means
music$rfreq15[is.na(music$rfreq15)] = mean(music$rfreq15, na.rm=TRUE)
music$rfreq14[is.na(music$rfreq14)] = mean(music$rfreq14, na.rm=TRUE)
music$rfreq13[is.na(music$rfreq13)] = mean(music$rfreq13, na.rm=TRUE)
music$rfreq12[is.na(music$rfreq12)] = mean(music$rfreq12, na.rm=TRUE)
music$rfreq11[is.na(music$rfreq11)] = mean(music$rfreq11, na.rm=TRUE)
music$rfreq10[is.na(music$rfreq10)] = mean(music$rfreq10, na.rm=TRUE)
music$rfreq9[is.na(music$rfreq9)] = mean(music$rfreq9, na.rm=TRUE)
music$rfreq8[is.na(music$rfreq8)] = mean(music$rfreq8, na.rm=TRUE)
music$rfreq7[is.na(music$rfreq7)] = mean(music$rfreq7, na.rm=TRUE)
music$rfreq6[is.na(music$rfreq6)] = mean(music$rfreq6, na.rm=TRUE)
music$rfreq5[is.na(music$rfreq5)] = mean(music$rfreq5, na.rm=TRUE)
music$rfreq4[is.na(music$rfreq4)] = mean(music$rfreq4, na.rm=TRUE)
music$rfreq3[is.na(music$rfreq3)] = mean(music$rfreq3, na.rm=TRUE)
music$rfreq2[is.na(music$rfreq2)] = mean(music$rfreq2, na.rm=TRUE)
music$rfreq1[is.na(music$rfreq1)] = mean(music$rfreq1, na.rm=TRUE)

music$lfreq15[is.na(music$lfreq15)] = mean(music$lfreq15, na.rm=TRUE)
music$lfreq14[is.na(music$lfreq14)] = mean(music$lfreq14, na.rm=TRUE)
music$lfreq13[is.na(music$lfreq13)] = mean(music$lfreq13, na.rm=TRUE)
music$lfreq12[is.na(music$lfreq12)] = mean(music$lfreq12, na.rm=TRUE)
music$lfreq11[is.na(music$lfreq11)] = mean(music$lfreq11, na.rm=TRUE)
music$lfreq10[is.na(music$lfreq10)] = mean(music$lfreq10, na.rm=TRUE)
music$lfreq9[is.na(music$lfreq9)] = mean(music$lfreq9, na.rm=TRUE)
music$lfreq8[is.na(music$lfreq8)] = mean(music$lfreq8, na.rm=TRUE)
music$lfreq7[is.na(music$lfreq7)] = mean(music$lfreq7, na.rm=TRUE)
music$lfreq6[is.na(music$lfreq6)] = mean(music$lfreq6, na.rm=TRUE)
music$lfreq4[is.na(music$lfreq4)] = mean(music$lfreq4, na.rm=TRUE)
music$lfreq3[is.na(music$lfreq3)] = mean(music$lfreq3, na.rm=TRUE)
music$lfreq2[is.na(music$lfreq2)] = mean(music$lfreq2, na.rm=TRUE)
music$lfreq1[is.na(music$lfreq1)] = mean(music$lfreq1, na.rm=TRUE)

music$rper12[is.na(music$rper12)] = mean(music$rper12, na.rm=TRUE)
music$rper9[is.na(music$rper9)] = mean(music$rper9, na.rm=TRUE)
music$rper7[is.na(music$rper7)] = mean(music$rper7, na.rm=TRUE)
music$rper4[is.na(music$rper4)] = mean(music$rper4, na.rm=TRUE)

music$lper15[is.na(music$lper15)] = mean(music$lper15, na.rm=TRUE)
music$lper7[is.na(music$lper7)] = mean(music$lper7, na.rm=TRUE)

#Since table is already ordered by artist, and it's short no need to run a complex command to extract tables
Abba <- music[1:10,] #10 rows
Vivaldi <- music[11:20,] #10 rows
Mozart <- music[21:26,] #6 rows
Eels <- music[27:36,] #10 rows
Beatles <- music[37:46,] #10 rows
Beethoven <- music[47:54,] #8 rows
Enya <- music[55:57,] #3 rows

#Now that we have a table per artists, it will provide a useful prcomb, for each artist.
Abbapr = prcomp(Abba[4:73], retx = TRUE)
Vivaldipr = prcomp(Vivaldi[4:73], retx = TRUE)
Mozartpr = prcomp(Mozart[4:73], retx = TRUE)
Eelspr = prcomp(Eels[4:73], retx = TRUE)
Beatlespr = prcomp(Beatles[4:73], retx = TRUE)
Beethovenpr = prcomp(Beethoven[4:73], retx = TRUE)
Enyapr = prcomp(Enya[4:73], retx = TRUE)

Allpr = prcomp(music[4:73], retx = TRUE)

Abbaplotsheet = data.frame(Abbapr[1], 1:nrow(Abba))
Vivaldiplotsheet = data.frame(Vivaldipr[1], 1:nrow(Vivaldi))
Mozartplotsheet = data.frame(Mozartpr[1], 1:nrow(Mozart))
Eelsplotsheet = data.frame(Eelspr[1], 1:nrow(Eels))
Beatlesplotsheet = data.frame(Beatlespr[1], 1:nrow(Beatles))
Beethovenplotsheet = data.frame(Beethovenpr[1], 1:nrow(Beethoven))
Enyaplotsheet = data.frame(Enyapr[1], 1:nrow(Enya))


plot(Abbaplotsheet)
plot(Vivaldiplotsheet)
plot(Mozartplotsheet)
plot(Eelsplotsheet)
plot(Beatlesplotsheet)
plot(Beethovenplotsheet)
plot(Enyaplotsheet)

install.packages("ggplot2")
library(ggplot2)

ListPR = Abbaplotsheet[1:2,] # this creates 3 collumns, sdev, X1.nrow.Abba.
ListPR[3:4,] = Beatlesplotsheet[1:2,]
ListPR[5:6,] = Beethovenplotsheet[1:2,]
ListPR[7:8,] = Eelsplotsheet[1:2,]
ListPR[9:10,] = Enyaplotsheet[1:2,]
ListPR[11:12,] = Mozartplotsheet[1:2,]
ListPR[13:14,] = Vivaldiplotsheet[1:2,]

names = list("Abba", "Abba", "Beatles", "Beatles", "Beethoven", "Beethoven", "Eels", "Eels", "Enya", "Enya", "Mozart", "Mozart", "Vivaldi", "Vivaldi")
#Append the name of the Artist each PC was sampled from, and make a list of it.
ListPR$name = names
#Set the entiriety of a new column name in ListPR to be this new list of names. Each row has an artist name now.

#manual sort of the PCs
PC1_PR <- ListPR[ListPR[2]==1,] #All PC1's
PC2_PR <- ListPR[ListPR[2]==2,] #All PC2's
PC_PR = PC1_PR
PC_PC[8:14] = PC2_PR[1:7,]

PC_PR2 = PC_PR
PC_PR2$name2 <- ""
#ggplot(data.df, aes(x = Axis1, y = Axis2, shape = Plant, color = Type)) + geom_point(size = 5)
for(i in 1:14)
{
  PC_PR2[i,4] = toString(PC_PR2[i,3])
}
whole2 <- ggplot(PC_PR, aes(x = PC_PR2$sdev, y= PC_PR2$name2, label = PC_PR2$'X1.nrow.Abba.')) + xlab("Standard Deviation")+ylab("Artist")+ggtitle("Standard Deviation of PC1 and PC2 by Artist")
whole2 + geom_point(size = 1) +  geom_text(angle = 30, hjust = 0, vjust = 1)
