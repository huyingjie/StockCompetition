rawData <- list.files(path = "3_DailyRawStatement/2016.04.01")

rawData_delete <- substr(rawData, 1, nchar(rawData)-4) # delete extension of file name
rawData_delete_split <- strsplit(rawData_delete, split = "#")
n_raw <- length(rawData_delete_split)
cleanDataSet <- data.frame(id = integer(n_raw),
                           today_amount = integer(n_raw),
                           transfer_amount = integer(n_raw))

cleanData <- function(oneRawAttendee){
  
  
}

for( i in 1:n_raw) {
  
  oneRawAttendee <-  rawData_delete_split[[i]]
  cleanDataSet[i, 1:2] <- sapply(oneRawAttendee[2:3], as.integer)
}

uniqueAttendee <- unique(cleanDataSet) # 575
orderedAttendee <- uniqueAttendee[order(uniqueAttendee$id),]

hist(orderedAttendee$today_amount, breaks = 300, main = "histogram of 2016/04/01 money", xlab = "money",
     xlim = c(0, 2e7), axes = FALSE, xaxs = "i", yaxs = "i")
axis(1, at = seq(from = 0, to = 2e7, by = 5e6))
axis(2, at = seq(0, 50, by = 5))
abline(v = median(orderedAttendee$today_amount), col = "red")
abline(v = quantile(orderedAttendee$today_amount)[4], col = "blue")

boxplot(orderedAttendee$today_amount) # 3 people have much more money than other attendees


hist(orderedAttendee$today_amount[orderedAttendee$today_amount< 5e6], breaks = 300,
     main = "histogram of 2016/04/01 money (<5*10^6)", xlab = "money",
     xlim = c(0, 5e6), axes = FALSE, xaxs = "i", yaxs = "i")
axis(1, at = seq(from = 0, to = 5e6, by = 500000))
axis(2, at = seq(0, 50, by = 5))
abline(v = median(orderedAttendee$today_amount), col = "red")
abline(v = quantile(orderedAttendee$today_amount)[4], col = "blue")

quantile(orderedAttendee$today_amount)
