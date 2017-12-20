
library(readr)
dat <- read_delim("journal.pone.0060188.s004.TXT", 
                                        "\t", escape_double = FALSE, trim_ws = TRUE)
library(ggplot2)
names(dat)[c(1,6:11)]<-c("id","cheerful","pleasantness","worry","fearful","sad","relaxed")
dat$time<-rep(1:table(dat$id)[1],length(unique(dat$id)))
dat$id<-rep(1:length(unique(dat$id)),each=table(dat$id)[1])


dat2<-subset(dat, id<10)
dat3<-dat2[,c("id","time","cheerful","worry")]


library(ctsem)

?ctIntervalise


#Then convert the absolute times to intervals, using the Tpoints reported from the prior step.

wide.dat3<-ctsem::ctLongToWide(datalong=dat3,
                    id="id",
                    time="time",
                    manifestNames=c("cheerful","worry"))

wide.dat3.2 <- ctIntervalise(datawide = wide.dat3, Tpoints = 220, n.manifest = 2)



model1<-ctModel(n.manifest = 2, n.latent = 2,  
        Tpoints = 220, manifestNames = c('Y1', 'Y2'), 
        TRAITVAR = NULL,
        LAMBDA=diag(2))

fit <- ctFit(dat = wide.dat3.2, ctmodelobj = model1)

?ctFit
