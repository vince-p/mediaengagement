if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse,psych,lsr)
p_load_gh("vince-p/vtools")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("multtest")

alldata<-read_csv("cleandata/alldata.csv")
totals<-read.csv("cleandata/totals.csv")
general<-read_csv("cleandata/general.csv")

dp=2
subscales<-sum((startsWith(names(totals),"Games"))) #Calculate # of distinct subscales in totals dataframe. This is the number of tests.



######################
# DEMOGRAPHICS
#
#
#COUNTRY TEXT
for (i in 1:length(table(alldata$demo_4))){
  
  if (sort(table(alldata$demo_4),decreasing=TRUE)[[i]]>1) {
    joiner<-" participants were from "
  } else {
    joiner<-" participant was from "
  }
  
  if (i==length(table(alldata$demo_4))) {
    ending<-"."
  } else if (i==length(table(alldata$demo_4))-1) {
    ending<-", and "
  } else
    ending<-", "
  
  cat(paste0(sort(table(alldata$demo_4),decreasing=TRUE)[[i]],joiner,simpleCap(names(sort(table(alldata$demo_4),decreasing=TRUE)[i])),ending))
}

# AGE
fq(alldata$demo_1)
# GENDER
table(alldata$demo_2)
# Education
fq(alldata$demo_3)
#
#
#
#
#TIME PLAYED
d<-psych::describe(select(alldata,GamesGeneral_5,GamesGeneral_6,MoviesGeneral_5,MoviesGeneral_6))
out.time<-tibble(var=c("Games","Movies"),
            days=c(r(d$mean[1]),r(d$mean[3])),
            sd_days=c(r(d$sd[1]),r(d$sd[3])),
            min_days=c(d$min[1],d$min[3]),
            max_days=c(d$max[1],d$max[3]),
            hours=c(r(d$mean[2]),r(d$mean[4])),
            sd_hours=c(r(d$sd[2]),r(d$sd[4])),
            min_hours=c(d$min[2],d$min[4]),
            max_hours=c(d$max[2],d$max[4]))
# t test days
v.test(x=alldata$GamesGeneral_5,y=alldata$MoviesGeneral_5)
# t test hours
v.test(x=alldata$GamesGeneral_6,y=alldata$MoviesGeneral_6)

#####################
#GENRE
out.genre<-select(alldata,ResponseId,Game=GamesGeneral_2,Genre=GamesGeneral_3,Plot=GamesGeneral_4,FavCharacter=GamesGeneral_16, CharGoodBad=GamesGeneral_17, CharMission=GamesGeneral_18,Movie=MoviesGeneral_2,Genre=MoviesGeneral_3,Plot=MoviesGeneral_4,FavCharacter=MoviesGeneral_16, CharGoodBad=MoviesGeneral_17, CharMission=MoviesGeneral_18)
#can write this to a CSV

#######################

#####################
#SOCIAL
out.social<-data.frame(var=c("Games","Movies"),
                       Alone=c(table(alldata$GamesGeneral_13),table(alldata$MoviesGeneral_13)),
                       WithOthers=c(table(alldata$GamesGeneral_14),table(alldata$MoviesGeneral_14)),
                       Remotely=c(table(alldata$GamesGeneral_15),table(alldata$MoviesGeneral_15))
)
# this code just enters the social values into a matrix and then runs chi-squared on it
socialm<-matrix(c(table(alldata$GamesGeneral_13),table(alldata$MoviesGeneral_13),table(alldata$GamesGeneral_14),table(alldata$MoviesGeneral_14),table(alldata$GamesGeneral_15),table(alldata$MoviesGeneral_15)),nrow=2)

chisq.test(socialm)


#DEVICE
out.device<-tibble(var=c("Games","Movies"),
               Mobile=c(table(alldata$GamesGeneral_7),table(alldata$MoviesGeneral_7)),
               Tablet=c(table(alldata$GamesGeneral_8),table(alldata$MoviesGeneral_8)),
               Laptop=c(table(alldata$GamesGeneral_9),table(alldata$MoviesGeneral_9)),
               TV=c(table(alldata$GamesGeneral_10),table(alldata$MoviesGeneral_10)),
               Projector=c(table(alldata$GamesGeneral_11),table(alldata$MoviesGeneral_11)),
               Desktop=c(table(alldata$GamesGeneral_12),table(alldata$MoviesGeneral_12))
               )





####################
# MAIN STATS

#create empty tibbles to fill in following loop
diff<-tibble(id=alldata$ResponseId) #tibble for difference scores

table1<-tibble("Variable"=character(),"MeanGames"=character(),"MeanMovies"=character(),"t"=numeric(),"df"=numeric(),"CI"=character(),"d"=numeric(),"p"=numeric())

for (i in seq(2,subscales+1)){ #Main loop to run stats. Iterated for each subscale
  t<-t.test(totals[,i],totals[,i+subscales],paired=TRUE) #temporary object to hold t test data
  
  table1<-rbind(table1,c(sub("Games_","",names(totals[i])),
                paste(format(round(mean(totals[[i]],na.rm=TRUE),dp),nsmall=dp)," (",sd=format(round(sd(totals[[i]],na.rm=TRUE),dp),nsmall=dp),")"),# Games M (SD)
                paste(format(round(mean(totals[[i+subscales]],na.rm=TRUE),dp),nsmall=dp)," (",sd=format(round(sd(totals[[i+subscales]],na.rm=TRUE),dp),nsmall=dp),")"),# Movies M (SD)
                format(round(t$statistic,dp),nsmall=dp), #t
                t$parameter, #df
                paste(format(round(t$conf.int[1],dp),nsmall=dp)," : ", format(round(t$conf.int[2],dp),nsmall=dp)), #CI
                r(cohensD(totals[,i],totals[,i+subscales],method="paired")), #d
                #paste0(t$p.value %>% formatC(digits = 3, format = "f")),#p
                #round(t$p.value,3))
                t$p.value)
                ,stringsAsFactors = FALSE,make.row.names=FALSE)
  names(table1)<-c("Variable","MeanGames","MeanMovies","t","df","CI","d","rawp")
  # diff<-cbind(diff,totals[,i]-totals[,i+subscales]) # calculate diffscore. Pos value means games are greater than movies.
  # colnames(diff)[i]<-sub("Games_","DIFF_",names(totals[i])) # generate proper diff label (replace "Games_" string with "Diff_" string
}
#table1$uncorrected<-pv(table1$rawp,method="none",n=subscales)
table1$holm<-pv(table1$rawp,method="holm",n=subscales)
#table1$bonf<-pv(table1$rawp,method="bonferroni",n=subscales)



#################
  # GENERATE A TABLE OF MEAN SELF REP SCORES FOR CORRELATIONS
  
meanstable<-tibble(id=alldata$ResponseId)
for (i in seq(2,subscales+1)){ #Main loop to run stats. Iterated for each subscale
  meanstable<-cbind(meanstable,rowMeans(cbind(totals[,i],totals[,i+subscales]))) 
  colnames(meanstable)[i]<-sub("Games_","Mean_",names(totals[i])) # generate proper label (replace "Games_" string with "Mean_" string)
}

meanstable<-cbind(general[-1],meanstable[-1])
names(meanstable)<-gsub("General_|Mean_","",names(meanstable))

#lsr::correlate(meanstable[-1:-6,-1:-6],test=TRUE) # Use this to get p-values
cor.state<- megacor(meanstable[-1:-6,-1:-6],p.adjust.method="holm",removeTriangle = "lower")

#lsr::correlate(meanstable[c(2:6,1)],test=TRUE)  # Use this to get p-values
cor.trait<-megacor(meanstable[c(2:6,1)],meanstable[7:16],p.adjust.method="holm")

