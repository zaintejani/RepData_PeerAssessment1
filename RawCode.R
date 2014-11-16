#### PART 0: LOADING THE DATA ####

x<-read.csv("activity.csv")

class(x$date)

#### PART 1: HISTOGRAM OF STEPS BY DAY, MEAN, and MEDIAN ####

i <- 1

step<-levels(x$date)

s<-vector("numeric")

for (i in 1:length(step)){
  y<-x[x$date==step[i],]
  
  q<-sum(y[,1], na.rm=TRUE)
  
  s<-c(s,q)
  
}

plot(as.Date(step),s,type="s", xlab="Date", ylab="Steps taken")

mean(s)

median(s)

#### PART 2: AVERAGE DAILY ACTIVITY PATTERN ####

i<-1

p<-vector("numeric")

step2<-levels(as.factor(x$interval))

for (i in 1:length(step2)){
  y<-x[x$interval==step2[i],]
  
  q<-mean(y[,1], na.rm=TRUE)
  
  p<-c(p,q)
}

plot(step2,p,type="l", xlab="Time interval", ylab="Average steps taken", main="Average Daily Activity Pattern")

step2[p==max(p)]

#### PART 3: INPUTTING MISSING VALUES ####

u<-x

d<-u[,1]

d[is.na(d)]<-mean(d, na.rm=TRUE)

u[,1]<-d

i <- 1

n<-vector("numeric")

for (i in 1:length(step)){
  y<-u[u$date==step[i],]
  
  q<-sum(y[,1], na.rm=TRUE)
  
  n<-c(n,q)
}

plot(as.Date(step),n,type="s", xlab="Date",ylab="Steps taken (adjusted)", main="Steps Taken Per Day (Adjusted)")

mean(n)

median(n)

#### PART 4: DIFFERENCES BETWEEN WEEKDAYS AND WEEKENDS ####

w<-weekdays(as.Date(x$date))

x<-cbind(x,w)

wdays1<-x[x$w==c("Monday","Tuesday","Wednesday","Thursday"),]

wdays2<-x[x$w=="Friday",]

wdays<-rbind(wdays1,wdays2)

wends<-x[x$w==c("Saturday","Sunday"),]

i<-1

v<-vector("numeric")

for (i in 1:length(step2)){
  y<-wdays[wdays$interval==step2[i],]
  
  q<-mean(y[,1], na.rm=TRUE)
  
  v<-c(v,q)
}

i<-1

j<-vector("numeric")

for (i in 1:length(step2)){
  y<-wends[wends$interval==step2[i],]
  
  q<-mean(y[,1], na.rm=TRUE)
  
  j<-c(j,q)
}

par(mfrow=c(2,1), mar=c(4,4,2,1))

plot(step2,v,type="l", xlab="Time interval", ylab="Average steps taken", main="Average Daily Activity Pattern (Weekdays)")

plot(step2,j,type="l", xlab="Time interval", ylab="Average steps taken", main="Average Daily Activity Pattern (Weekends)")