mvt = read.csv("mvtWeek1.csv")
str(mvt)
# 191641 obs. of  11 variables
max(mvt$ID)
# 9181151
min(mvt$Beat)
# 111
nrow(mvt[mvt$Arrest=="TRUE",])
# 15536
nrow(mvt[mvt$LocationDescription=="ALLEY",])
# 2308
head(mvt$Date)
# Month/Day/Year Hour:Minute
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
# May 2006
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
table(mvt$Month)
min(table(mvt$Month))
which.min(table(mvt$Month))
# February
table(mvt$Weekday)
max(table(mvt$Weekday))
which.max(table(mvt$Weekday))
# Friday
