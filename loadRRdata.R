## R code to download data from Rust & Rothwell (1995 & 1996)

################################################################################
## Load Data
# download data if necessary
if (!file.exists("rr-data.zip")) {
  download.file(url="http://qed.econ.queensu.ca/jae/1995-v10.S/rust-rothwell/rr-data.zip",
                destfile="rr-data.zip")
}
# unzip data if necessary
if (!file.exists("rr-data/yank1.mon")) {
  unzip(zipfile="rr-data.zip",exdir="rr-data")
}

# read time-varying plant data
plantFiles <- dir("rr-data",pattern="*.mon")
plantData <- data.frame()
for (file in plantFiles) {
  pd <- read.csv(file=paste("rr-data/",file,sep=""), sep="",
                 header=FALSE,
                 col.names=c("name","ID","steam.system","year","month",
                     "vintage","age","hrs.refuel","hrs.plan.out",
                     "hrs.forced.out","hrs.total",
                     "scram.in","scram.out","num.forced.out"))
  plantData <- rbind(plantData,pd)
}
# label categorical variables
plantData$steam.system <- factor(plantData$steam.system,
                                 labels=c("Babcock.Wilcox","Combustion",
                                     "GE","Westinghouse","Other1","Other2"))
plantData$vintage <- factor(plantData$vintage,
                            labels=c("preTMI","postTMI"))
# read constant data
constData <- read.csv("rr-data/nukesum.asc",sep="",header=FALSE,
                      col.names=c("name","firm",
                          "start.month","start.day","start.year",
                          "thermal.capacity","nameplate.rating",
                          "net.power","arch.engineer","steam.system",
                          "builder","turbine","State"))

# Create state and control variables
plantData$hrs.operating <- plantData$hrs.total -
  (plantData$hrs.refuel + plantData$hrs.plan.out +
   plantData$hrs.forced.out)
plantData$hrs.operating[plantData$hrs.operating<0] <- 0
plantData$hrs.operating[plantData$num.forced.out==-1] <- 0 # these plants exited

plantData$action <- NA
plantData$action[plantData$hrs.operating<=0] <- 3
plantData$action[plantData$hrs.operating>0] <- 4
plantData$action[plantData$hrs.operating/plantData$hrs.total>0.25]<-5
plantData$action[plantData$hrs.operating/plantData$hrs.total>0.50]<-6
plantData$action[plantData$hrs.operating/plantData$hrs.total>0.75]<-7
plantData$action[plantData$hrs.operating/plantData$hrs.total>0.99]<-8
plantData$action[plantData$hrs.refuel>0] <- 2
plantData$action[plantData$num.forced.out==-1] <- 1
plantData$action <- factor(plantData$action,levels=1:8,
                           labels=c("exit", "refuel",
                               "shutdown", "run1.25", "run26.50",
                               "run51.75", "run76.99", "run100"))
#library(plm)
# The plm packages deals with factors strangely, so not going to use it
plantData$time <- (plantData$year-min(plantData$year))*12 + plantData$month
#plantData <- pdata.frame(plantData,index=c("ID","time"))
plantData <- plantData[order(plantData$ID,plantData$time),]
lag.panel <- function(x,id,time,data=NULL,k=1) {
  i <- eval(substitute(id),data)
  t <- eval(substitute(time),data)
  x <- eval(substitute(x),data)
  stopifnot(all.equal(order(i,t),1:nrow(data)))
  shift <- function(x,k) {
    if (k>0) return(c(rep(NA,k),x[1:(length(x)-k)]))
    else if (k<0) return(c(x[(abs(k)+1):length(x)],rep(NA,abs(k))))
    else return(x)
  }
  l.x <- shift(x,k)
  l.x[i!=shift(i,k)] <- NA
  if (is.factor(x)) { # make sure levels are preserved
    ind <- sort(unique(l.x))
    ind <- ind[!is.na(ind)]
    l.x <- factor(l.x,labels=levels(x)[ind])
    levels(l.x) <- levels(x)
  }
  return(l.x)
}

plantData$spell.type <- 1
plantData$spell.type[plantData$hrs.refuel>0] <- 0
plantData$spell.type[plantData$num.forced.out==-1] <- 2
plantData$spell.type <- factor(plantData$spell.type,
                               labels=c("refueling","operating","exit"))
plantData$npp.signal <- 0
plantData$npp.signal[plantData$hrs.forced.out>0] <- 1
plantData$npp.signal[plantData$hrs.refuel>0 &
                     lag.panel(hrs.refuel,ID,time,plantData)>0] <- 2
# shift signal by 1 period to match model timing convention (signal
# and action at time t determines state at time t+1)
plantData$npp.signal <- lag.panel(npp.signal,ID,time,plantData,k=-1)
plantData$npp.signal <- factor(plantData$npp.signal,
                               labels=c("none","forced.outage","cont.refuel"))
plantData$action <- lag.panel(action,ID,time,plantData,k=-1)

plantData$duration <- 1 # Rust & Rothwell have durations start from 1
T <- max(as.numeric(plantData$time))
for(t in 2:T) {
  plantData$duration[plantData$time==t] <-
    ifelse(plantData$spell.type[plantData$time==t]==lag.panel(spell.type,ID,time,plantData)[plantData$time==t],
           lag.panel(duration,ID,time,plantData)[plantData$time==t]+1,1)
  plantData$duration[is.na(plantData$duration)] <- 1 # for when lag()=NA
}

# create indicator for "major problem spells" (outages lasting longer than 9 months)
nop <- plantData$hrs.operating==0
tmp <- (nop[-1]!=nop[-length(nop)] | plantData$ID[-1] != plantData$ID[-nrow(plantData)])
tmp[is.na(tmp)] <- FALSE
op.id <- cumsum(c(TRUE,tmp))
op.length <- ave(op.id,factor(op.id), FUN=length)
tmp <- op.length>9 & (nop==TRUE)
tmp[is.na(tmp)] <- FALSE
plantData$major.problem.spell <- tmp

