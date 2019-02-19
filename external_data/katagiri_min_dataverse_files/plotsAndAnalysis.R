setwd("[current directory]/data")

#%%%%%%%%%%%%%%%%%
####  SET-UP  ####
#%%%%%%%%%%%%%%%%%

# Load packages
require(ggplot2)
require(stargazer)
require(Hmisc)
require(dplyr)
require(MASS)
source("./../PAR_Code.R") # PAR Code from Brandt and Williams (2001)

# All predicted data
wa = read.csv("whAllPred.csv")
da = read.csv("dosAllPred.csv")
fa = read.csv("fbisAllPred.csv")
wa$date = as.Date(wa$date)
da$date = as.Date(da$date)
fa$date = as.Date(fa$date)

# Subsets of predicted data (see Appendix D)
dah = da[which(da$highLevel==1),] # DOS high-level 
fam = fa[which(fa$midLevel==1),]  # FBIS governmental-level
fah = fa[which(fa$highLevel==1),] # FBIS high-level 

# All weekly predicted data
ww = read.csv("whWeekPred.csv")
dw = read.csv("dosWeekPred.csv")       
fw = read.csv("fbisWeekPred.csv")      
ww$week = as.Date(ww$week)
dw$week = as.Date(dw$week)
fw$week = as.Date(fw$week)

# Subsets of weekly predicted data (see Appendix D)
dwh = read.csv("dosWeekPredHigh.csv")  # DOS high-level
fwm = read.csv("fbisWeekPredMid.csv")  # FBIS governmental-level
fwh = read.csv("fbisWeekPredHigh.csv") # FBIS high-level
dwh$week = as.Date(dwh$week)
fwm$week = as.Date(fwm$week)
fwh$week = as.Date(fwh$week)

#%%%%%%%%%%%%%%%%%%%%
####  MAKE DATA  ####
#%%%%%%%%%%%%%%%%%%%%

weekdata = data.frame(week = ww$week, wh = ww$wsumAvg, 
                      dos=dw$wsumAvg, dosh=dwh$wsumAvg,
                      fbis=fw$wsumAvg, fbism=fwm$wsumAvg, fbish=fwh$wsumAvg)
weekdata$week = as.Date(weekdata$week)
weekdata$year = format(weekdata$week, "%Y")

# Create WH lags
wh.lags = cbind(wh1=lag(weekdata$wh, 1), wh2=lag(weekdata$wh, 2), wh3=lag(weekdata$wh, 3))
weekdata = data.frame(weekdata, wh.lags)

# Add NYT data and controls
nytcont = read.csv("nytAndControls.csv")
nytcont$week = as.Date(nytcont$week, "%m/%d/%Y")
weekdata = merge(weekdata, nytcont, by="week")

# Weekly data is complete at this point. 
write.csv(weekdata, "./../supplementary/weekdata.csv")


#%%%%%%%%%%%%%%%%%%%%%
####  MAKE PLOTS  ####
#%%%%%%%%%%%%%%%%%%%%%

# DOS data
dosp = data.frame(week=rep(weekdata$week, 2),
                  measure=c(weekdata$dos, weekdata$dosh),
                  lev=c(rep("All", nrow(weekdata)), rep("High", nrow(weekdata))))
# FBIS data
fbisp = data.frame(week=rep(weekdata$week, 2),
                   measure=c(weekdata$fbis, weekdata$fbish),
                   lev=c(rep("All", nrow(weekdata)), rep("High", nrow(weekdata))))


#### Figure 2 ####
### Plots of all four weekly variables
# (Figure 2 is made by combining these four figures in Photoshop.)

nytLine = qplot(weekdata$week, weekdata$nyt, geom="line") + theme_bw() + xlab("Year") + ylab("NYT") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12)) + 
  scale_x_date(breaks=seq.Date(as.Date("1958-01-01"), as.Date("1964-01-01"), by="year"), date_labels = "%Y") +
  scale_y_continuous(limits=c(0,8), breaks=c(0,2,4,6,8))

ggsave("./../figures/fig2a.pdf", nytLine, width=7.4, height=2.45)

dosLine = ggplot(dosp, aes(x=week, y=measure)) + geom_line(aes(color=lev)) + theme_bw() + xlab("Year") + ylab("DOS") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12)) +
  scale_color_manual("Level", values=c("black", "gray50")) + guides(color=FALSE) +
  scale_x_date(breaks=seq.Date(as.Date("1958-01-01"), as.Date("1964-01-01"), by="year"), date_labels = "%Y")

ggsave("./../figures/fig2b.pdf", dosLine, width=7.475, height=2.45)

fbisLine = ggplot(fbisp, aes(x=week, y=measure)) + geom_line(aes(color=lev)) + theme_bw() + xlab("Year") + ylab("FBIS") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12)) +
  scale_color_manual("Level", values=c("black", "gray50")) + guides(color=FALSE) +
  scale_x_date(breaks=seq.Date(as.Date("1958-01-01"), as.Date("1964-01-01"), by="year"), date_labels = "%Y")

ggsave("./../figures/fig2c.pdf", fbisLine, width=7.475, height=2.45)

whLine = qplot(weekdata$week, weekdata$wh, geom="line") + theme_bw() + xlab("Year") + ylab("WH") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12)) +
  scale_x_date(breaks=seq.Date(as.Date("1958-01-01"), as.Date("1964-01-01"), by="year"), date_labels = "%Y")

ggsave("./../figures/fig2d.pdf", whLine, width=7.475, height=2.45)



#%%%%%%%%%%%%%%%%%%%
####  ANALYSIS  ####
#%%%%%%%%%%%%%%%%%%%

#### Figure 3 ####
## Compare Soviet resolve between public and private statements

# High-level only (Figure 3 in main text)
signalsHigh = data.frame(Prediction=c(dah$predMean, fah$predMean), 
                         Source=c(rep("DOS (Private)", nrow(dah)), rep("FBIS (Public)", nrow(fah))))

fig3 = ggplot(signalsHigh, aes(Prediction)) + geom_density(aes(fill=Source), alpha=0.5) + theme_bw() + 
  xlab("Soviet Resolve") + ylab("Density") + scale_fill_manual(values=c("gray10", "gray70")) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12),
        legend.text=element_text(size=9), legend.title=element_text(size=10))

ggsave("./../figures/fig3.pdf", fig3, width=6, height=3)

fligner.test(signalsHigh$Prediction, signalsHigh$Source)

# All documents (Figure A3b in the Supplementary Material)
signals = data.frame(Prediction=c(da$predMean, fa$predMean), 
                     Source=c(rep("DOS (Private)", nrow(da)), rep("FBIS (Public)", nrow(fa))))

figA3b = ggplot(signals, aes(Prediction)) + geom_density(aes(fill=Source), alpha=0.5) + theme_bw() + 
  xlab("Soviet Resolve") + ylab("Density") + scale_fill_manual(values=c("gray10", "gray70")) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12),
        legend.text=element_text(size=9), legend.title=element_text(size=10))

ggsave("./../figures/figA3b.pdf", figA3b, width=6, height=3)

fligner.test(signals$Prediction, signals$Source)


## Kolmogorov-Smirnov test to see whether distributions are alike
ks.test(dah$predMean, fah$predMean)  # High-level only; D = 0.334, p << 0.001
ks.test(da$predMean, fa$predMean) # All DOS/FBIS; D = 0.241, p << 0.001

## Find variances across signals of each
# High-level only
var(fah$predMean) # 0.032
var(dah$predMean) # 0.027

# All DOS/FBIS
var(fa$predMean) # 0.038
var(da$predMean) # 0.032


#### Table 2 ####
### Negative binomial regressions

# Negative binomial with 3 lags (best in AIC)
# All DOS, all FBIS
nbFit3aa = glm.nb(wh ~ wh1 + wh2 + wh3 +
                    dos + fbis + nyt + factor(year), data=weekdata, control=glm.control(maxit=100))
summary(nbFit3aa)

# All DOS, governmental-level FBIS
nbFit3am = glm.nb(wh ~ wh1 + wh2 + wh3 + 
                    dos + fbism + nyt + factor(year), data=weekdata, control=glm.control(maxit=100))
summary(nbFit3am)

# All DOS, high-level FBIS
nbFit3ah = glm.nb(wh ~ wh1 + wh2 + wh3 + 
                    dos + fbish + nyt + factor(year), data=weekdata, control=glm.control(maxit=100))
summary(nbFit3ah)

# High-level DOS, all FBIS
nbFit3ha = glm.nb(wh ~ wh1 + wh2 + wh3+ 
                    dosh + fbis + nyt + factor(year), data=weekdata, control=glm.control(maxit=100))
summary(nbFit3ha)

# High-level DOS, governmental-level FBIS
nbFit3hm = glm.nb(wh ~ wh1 + wh2 + wh3 + 
                    dosh + fbism + nyt + factor(year), data=weekdata, control=glm.control(maxit=100))
summary(nbFit3hm)

# High-level DOS, high-level FBIS
nbFit3hh = glm.nb(wh ~ wh1 + wh2 + wh3 + 
                    dosh + fbish + nyt + factor(year), data=weekdata, control=glm.control(maxit=100))
summary(nbFit3hh)

stargazer(nbFit3aa, nbFit3am, nbFit3ah, nbFit3ha, nbFit3hm, nbFit3hh, column.sep.width = "0pt", no.space=T, align=T, df=F)


#### Table 3 ####
### PAR(7) models

# All DOS, all FBIS, no controls
parFit7 = Parp(weekdata$wh ~ weekdata$dos + weekdata$fbis + weekdata$nyt, p=7)
summary(parFit7)

# All DOS, all FBIS, controls
parFit7c = Parp(weekdata$wh ~ weekdata$dos + weekdata$fbis + weekdata$nyt +
                  weekdata$election + weekdata$mids + weekdata$kennedy, p=7)
summary(parFit7c)

# High-level DOS, high-level FBIS, no controls
parFit7h = Parp(weekdata$wh ~ weekdata$dosh + weekdata$fbish + weekdata$nyt, p=7)
summary(parFit7h)

# High-level DOS, high-level FBIS, controls
parFit7hc = Parp(weekdata$wh ~ weekdata$dosh + weekdata$fbish + weekdata$nyt + 
                   weekdata$election + weekdata$mids + weekdata$kennedy, p=7)
summary(parFit7hc)

# These four models together constitute Table 3.


#### Table 4 ####
### Long-term and short-term effects on WH variable

# All DOS, all FBIS
parp.multipliers(parFit7)
parp.multipliers(parFit7c)

# High-level DOS, high-level FBIS
parp.multipliers(parFit7h)
parp.multipliers(parFit7hc)


#### Table 5, Table 6, Figure 4 ####
### The Core of the Crisis

# Peak crisis times
dcrisis = dah[which(dah$date >= "1961-06-01" & dah$date <= "1961-11-15"),]
fcrisis = fah[which(fah$date >= "1961-06-01" & fah$date <= "1961-11-15"),]

# Outside of peak crisis time
dnoncrisis = dah[which(dah$date < "1961-06-01" | dah$date > "1961-11-15"),]
fnoncrisis = fah[which(fah$date < "1961-06-01" | fah$date > "1961-11-15"),]

# Get means and variances
mean(dcrisis$predMean) # 0.437
mean(fcrisis$predMean) # 0.350
mean(dnoncrisis$predMean) # 0.418
mean(fnoncrisis$predMean) # 0.291
var(dcrisis$predMean) # 0.021
var(fcrisis$predMean) # 0.035
var(dnoncrisis$predMean) # 0.027
var(fnoncrisis$predMean) # 0.031

# Compare public and private during the main crisis
snrCrisis1 = data.frame(Source=c(rep("Private", nrow(dcrisis)), rep("Public", nrow(fcrisis))), 
                       predMean=c(dcrisis$predMean, fcrisis$predMean))
snrCrisis1$Source = factor(snrCrisis1$Source, labels=c("Private   ", "Public")) # To make better spaced labels

fig4a = ggplot(snrCrisis1, aes(predMean)) + geom_density(aes(fill=Source), alpha=0.5) + theme_bw() + xlim(0, 0.875)+
  xlab("Soviet Resolve") + ylab("Density") + scale_fill_manual(values=c("gray10", "gray70")) +
  theme(legend.position="top", axis.text=element_text(size=11), axis.title=element_text(size=12),
        legend.text=element_text(size=11), legend.title=element_text(size=11))

ggsave("./../figures/fig4a.pdf", fig4a, width=4, height=3.2)


t.test(dcrisis$predMean, fcrisis$predMean)
ks.test(dcrisis$predMean, fcrisis$predMean)
fligner.test(snrCrisis1$predMean, snrCrisis1$Source)


# Compare public and private outside of main crisis
snrCrisis2 = data.frame(Source=c(rep("Private", nrow(dnoncrisis)), rep("Public", nrow(fnoncrisis))),
                        predMean=c(dnoncrisis$predMean, fnoncrisis$predMean))
snrCrisis2$Source = factor(snrCrisis2$Source, labels=c("Private   ", "Public")) # To make better spaced labels

fig4b = ggplot(snrCrisis2, aes(predMean)) + geom_density(aes(fill=Source), alpha=0.5) + theme_bw() + 
  xlab("Soviet Resolve") + ylab("Density") + scale_fill_manual(values=c("gray10", "gray70")) +
  theme(legend.position="top", axis.text=element_text(size=11), axis.title=element_text(size=12),
        legend.text=element_text(size=11), legend.title=element_text(size=11))

ggsave("./../figures/fig4b.pdf", fig4b, width=4, height=3.2)

t.test(dnoncrisis$predMean, fnoncrisis$predMean)
ks.test(dnoncrisis$predMean, fnoncrisis$predMean)
fligner.test(snrCrisis2$predMean, snrCrisis2$Source)


# Compare private signals during and outside of main crisis
snrCrisis3 = data.frame(Period=c(rep("Late 1961", nrow(dcrisis)), rep("Other", nrow(dnoncrisis))),
                        predMean=c(dcrisis$predMean, dnoncrisis$predMean))
snrCrisis3$Period = factor(snrCrisis3$Period, labels=c("Late 1961   ", "Other")) # To make better spaced labels

fig4c = ggplot(snrCrisis3, aes(predMean)) + geom_density(aes(fill=Period), alpha=0.5) + theme_bw() + 
  xlab("Soviet Resolve") + ylab("Density") + scale_fill_manual(values=c("gray10", "gray70")) +
  theme(legend.position="top", axis.text=element_text(size=11), axis.title=element_text(size=12),
        legend.text=element_text(size=11), legend.title=element_text(size=11))

ggsave("./../figures/fig4c.pdf", fig4c, width=4, height=3.2)

t.test(dcrisis$predMean, dnoncrisis$predMean)
ks.test(dcrisis$predMean, dnoncrisis$predMean)
fligner.test(snrCrisis3$predMean, snrCrisis3$Period)


# Compare public signals during and outside of main crisis
snrCrisis4 = data.frame(Period=c(rep("Late 1961", nrow(fcrisis)), rep("Other", nrow(fnoncrisis))),
                        predMean=c(fcrisis$predMean, fnoncrisis$predMean))
snrCrisis4$Period = factor(snrCrisis4$Period, labels=c("Late 1961   ", "Other")) # To make better spaced labels

fig4d = ggplot(snrCrisis4, aes(predMean)) + geom_density(aes(fill=Period), alpha=0.5) + theme_bw() + 
  xlab("Soviet Resolve") + ylab("Density") + scale_fill_manual(values=c("gray10", "gray70")) + 
  theme(legend.position="top", axis.text=element_text(size=11), axis.title=element_text(size=12),
        legend.text=element_text(size=11), legend.title=element_text(size=11))

ggsave("./../figures/fig4d.pdf", fig4d, width=4, height=3.2)

t.test(fcrisis$predMean, fnoncrisis$predMean)
ks.test(fcrisis$predMean, fnoncrisis$predMean)
fligner.test(snrCrisis4$predMean, snrCrisis4$Period)


