#!/usr/bin/env Rscript

library(ggplot2)
library(lme4)

data <- read.csv("../data/small_hits_grouped.csv")

data$rank <- data$pos + (data$page-1)*10
data$ordering_factor <- data$rank
data$rank <- factor(data$rank)
data$rank <- with(data, reorder(rank,ordering_factor, mean))

data$T0 <- unclass(as.POSIXct(strptime(data$t0, "%Y-%m-%d %H:%M:%S")))
data$T1 <- unclass(as.POSIXct(strptime(data$t1, "%Y-%m-%d %H:%M:%S")))
data$deltat <- with(data, T1-T0)
data <- subset(data, deltat < 100 & deltat > 0)
data$y <- 0
#data$y[data$deltaY < 0] <-  1/(data$deltat[data$deltaY < 0]/60)
data$y[data$deltaY < 0] <- 1
data$temp.t <- strptime(data$t0, "%Y-%m-%d %H:%M:%S")
data$temp.t2 <- as.POSIXct(data$temp.t)
l = length(data$temp.ts)

x <- strftime(data$temp.t2, "%H")

data$H <- strftime(as.POSIXct(strptime(data$t0, "%Y-%m-%d %H:%M:%S")), "%H")


#################################################
# Rank Based - Multilevel Model 
#################################################

df.rank <- function(category,label){
  m <- lmer(y ~ factor(rank) - 1 + (1|id) + (1|H), data = subset(data, cat==category))
  r.output <- data.frame(
                       reg = names(fixef(m)), 
                       coef = fixef(m),
                       rank = 1:length(fixef(m)),
                       se = sqrt(diag(vcov(m)))
                       )
  r.output$model <- label
  r.output$page <- 3
  r.output$page[r.output$rank < 21] <- 2
  r.output$page[r.output$rank < 11] <- 1
  r.output$avg <- mean(r.output$coef)
  r.output
}

views = c("AssignmentDurationInSeconds1", "LastUpdatedTime1", "LatestExpiration1","NumHITs1", "Reward1","Title1")
r.com <- rbind(
               df.rank(views[4],"# of HITs (most)"),
               df.rank(views[2],"Time (newest)"),
               df.rank(views[5],"Reward (highest)"),
               df.rank(views[6],"Title (a-z)")
               )
#r.com$reg <- factor(r.com$reg, levels(r.com$reg)[c(2,10:3,1)])

r.com$reg <- with(r.com, reorder(reg, -1*rank, mean))

limits <- aes(xmax = coef + se, xmin=coef - se, height=.1,colour=factor(page)) 
g <- ggplot(r.com, aes(y = I(-1*rank), x = coef))  +  geom_point()
g <- g +  geom_errorbarh(limits) + geom_vline(aes(xintercept=avg), col="black")
g <- g + facet_wrap(~model, ncol=1)
g <- g + labs(x="Expected HIT-Disappearing Event Per Scrape Iteration", y="Rank", title="")

print(g)

pdf("../writeup/images/FIGA_coef_re.pdf")
print(g)
dev.off()

################################################
# Linear Model 
###############################################

df.pos.lm <- function(category,label){
  m <- lmer(y ~ factor(rank) - 1 + (1|H), data = subset(data, cat==category))
  r.output <- data.frame(
                       reg = names(fixef(m)), 
                       coef = fixef(m),
                       rank = 1:length(fixef(m)),
                       se = sqrt(diag(vcov(m)))
                       )
  r.output$model <- label
  r.output$page <- 3
  r.output$page[r.output$rank < 21] <- 2
  r.output$page[r.output$rank < 11] <- 1
  r.output$avg <- mean(r.output$coef)
  r.output
}

r.com <- rbind(
               df.pos.lm(views[4],"# of HITs (most)"),
               df.pos.lm(views[2],"Time (newest)"),
               df.pos.lm(views[5],"Reward (highest)"),
               df.pos.lm(views[6],"Title (a-z)")
               )

r.com$reg <- with(r.com, reorder(reg, -1*rank, mean))

limits <- aes(xmax = coef + se, xmin=coef - se, height=.1,colour=factor(page)) 
g <- ggplot(r.com, aes(y = I(-1*rank), x = coef))  +  geom_point()
g <- g +  geom_errorbarh(limits) + geom_vline(aes(xintercept=avg), col="black")
g <- g + facet_wrap(~model, ncol=1)
g <- g + labs(x="Expected HIT-Disappearing Event Per Scrape Iteration", y="Rank", title="")

print(g)

pdf("../writeup/images/FIGB_coef_pooled.pdf")
print(g)
dev.off()

# explaining the "top spot" effects

#groups holding the top spot 
top_spot <- with(data, id[pos==1 & page==1 & cat=="LastUpdatedTime1"])
x = as.numeric(table(top_spot))

tmp.df <- as.data.frame(table(top_spot))
qplot(tmp.df$Freq)
bad.ids <- as.character(subset(tmp.df, Freq > 75)$top_spot)

## drop.levels <- function(dat){
## dat[] <- lapply(dat, function(x) x[,drop=TRUE])
## return(dat)
## }


# Position of Requestors at the Top 
tmp <- subset(data, cat=="LastUpdatedTime1" & id %in% bad.ids)
tmp$rank <- -1*((as.numeric(tmp$page)-1)*10 + as.numeric(tmp$pos))

tmp$p <- -1*as.numeric(as.character(tmp$pos))
tmp$time <- unclass(as.POSIXct(strptime(tmp$t1, "%Y-%m-%d %H:%M:%S")))
tmp$time <- (tmp$time - min(tmp$time))/(3600*24)
tmp <- droplevels(tmp)
#levels(tmp$id) <- 1:10
theme_set(theme_bw())
g <- qplot(time,rank,geom="line", data = tmp) +
    geom_point(aes(colour=factor(page)),alpha=I(.5)) +
    facet_wrap(~id,ncol=1)
print(g)

pdf("../writeup/images/FIGC.pdf", width=9, heigh=7)
print(g)
dev.off()


###################
## Other stuff 
##################

data <- read.csv("../data/best_case.csv")

data$category <- with(data, factor(category))

data$search <- factor(data$search)

g <- ggplot(data, aes(x=count, y=search)) +
  geom_segment(aes(yend=search,xend=0,colour=case), size=I(5)) +
  facet_grid(category ~ case, scale="free") +
  theme(legend.position="none") + ylab("")

print(g)

pdf("../writeup/images/rcm_combined.pdf")
theme_set(theme_bw())
print(g)
dev.off()

time.data <- read.csv("../data/timing.csv")

#qplot(time,count,colour=case, data = time.data)

#today <- as.POSIXlt("2010-05-06 00:00:00")
#time.data$t <- as.numeric(strptime(time.data$time,"%H:%M:%S") - today)
#time.data$hour <- time.data$t/6600

g <- ggplot(time.data, aes(x=time,y=count)) +
    geom_point(aes(colour=case,shape=case),size=I(3)) +
    geom_line(aes(linetype=case)) +
    xlab("time in hours") + ylab("number of workers") + 
    geom_text(aes(x=5, y=72, label="BEST")) +
    geom_text(aes(x=22, y=37, label="A-Z")) +
    geom_text(aes(x=22.5, y=28, label="NEWEST")) +
    geom_text(aes(x=23, y=15, label="WORST")) +
    theme(legend.position="none")

print(g)

pdf("../writeup/images/rcm_timing.pdf")
theme_set(theme_bw())
print(g)
dev.off()


# Make Histogram Plot
pages.data <- read.csv("../data/pages.csv")
pages.data$page <- with(pages.data, reorder(page,-order,mean))

## g <- qplot(page,count, geom="bar", data = pages.data) +
##   ylab("number of workers") +
##   xlab("result page number") + 
##   coord_flip()

g <- ggplot(data = pages.data, aes(x = page, y = count)) +
    geom_bar(stat = "identity") +
    ylab("number of workers") +
    xlab("result page number") + 
    coord_flip()

print(g)

pdf("../writeup/images/rcm_pages.pdf")
theme_set(theme_bw())
print(g)
dev.off()





# 7 day plot
day7.data <- read.csv("../data/timing_7day.csv")
g <- ggplot(day7.data, aes(x=time,y=count)) +
  xlab("time in days") + ylab("number of workers") + 
  geom_text(aes(x=.2, y=71.55, label="BEST")) +
  geom_text(aes(x=2.3, y=71.5, label="A-Z")) +
  geom_text(aes(x=4.3, y=60, label="NEWEST")) +
  geom_text(aes(x=5.2, y=60, label="WORST")) + theme(legend.position="none") +
  geom_point(aes(colour=case,shape=case),size=I(3)) +
  geom_line(aes(linetype=case)) 
print(g)

pdf("../writeup/images/rcm_timing_7day.pdf")
theme_set(theme_bw())
print(g)
dev.off()


