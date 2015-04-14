# Class work 4-11-15 Blaise Cumming


# read in file data
btl <- read.csv("C:/Users/BLC/Desktop/btl.csv")


# generate subtables using search qualifications on the LOCATION index of btl for each dorm
casalist <- btl[grep("Casa", btl$LOCATION),]
swiglist <- btl[grep("Swig", btl$LOCATION),]
soblist <- btl[grep("Sob", btl$LOCATION),]
dunlist <- btl[grep("Dun", btl$LOCATION),]
mclist <- btl[grep("Mc", btl$LOCATION),]
grahamclist <- btl[grep("Graham", btl$LOCATION),]
sanfilliplist <- btl[grep("San F", btl$LOCATION),]
campisilist <- btl[grep("Campisi", btl$LOCATION),]
noblist <- btl[grep("Nob", btl$LOCATION),]


# count the number of rows in each new table
nrow(casalist)
nrow(swiglist)
nrow(soblist)
nrow(dunlist)
nrow(mclist)
nrow(grahamclist)
nrow(sanfilliplist)
nrow(campisilist)
nrow(noblist)
# Highest number returned above will be the dorm with most incidents



# Format a second new index DATE3 which will store the shorthand day of the week
btl$DATE2 <- format(as.Date(btl$DATE, "%m/%d/%Y"), "%a")

# generate subtables using search qualifications on the DATE2 index of btl for each day of the week
monlist = btl[grep("Mon", btl$DATE2),]
tuelist = btl[grep("Tue", btl$DATE2),]
wedlist = btl[grep("Wed", btl$DATE2),]
thulist = btl[grep("Thu", btl$DATE2),]
frilist = btl[grep("Fri", btl$DATE2),]
satlist = btl[grep("Sat", btl$DATE2),]
sunlist = btl[grep("Sun", btl$DATE2),]


# count the number of rows in each new tables
nrow(monlist)
nrow(tuelist)
nrow(wedlist)
nrow(thulist)
nrow(frilist)
nrow(satlist)
nrow(sunlist)
# Highest number returned above will be the dorm with most incidents

btl$DATE3 <- format(as.Date(btl$DATE, "%m/%d/%Y"), "%m")

jan = btl[grep("01", btl$DATE3),]
feb = btl[grep("02", btl$DATE3),]
mar = btl[grep("03", btl$DATE3),]
apr = btl[grep("04", btl$DATE3),]
may = btl[grep("05", btl$DATE3),]
jun = btl[grep("06", btl$DATE3),]
jul = btl[grep("07", btl$DATE3),]
aug = btl[grep("08", btl$DATE3),]
sep = btl[grep("09", btl$DATE3),]
oct = btl[grep("10", btl$DATE3),]
nov = btl[grep("11", btl$DATE3),]
dec = btl[grep("12", btl$DATE3),]

nrow(jan)
nrow(feb)
nrow(mar)
nrow(apr)
nrow(may)
nrow(jun)
nrow(jul)
nrow(aug)
nrow(sep)
nrow(oct)
nrow(nov)
nrow(dec)


# Answers I got were Swig, Monday, February