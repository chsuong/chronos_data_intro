# erin baggott
# july 24, 2014
# turn TABARI output into events data to load in stata

library(events)
setwd('/Users/ebaggott/Dropbox/GlobalDiplomacyDataJune2014/')

################################################################

# load event data from TABARI text output file
dat <- read_eventdata('globalDiplomaticEvents.txt', sep='\t', col.format="DSTCLQ", one.a.day=T, date.format="%Y%m%d")

dat$topic = as.numeric(substr(dat$quote,6,6)) # 1 = polmil, 2 = economic
nrow(dat) # 132,429

# collapse all entities into national governments -- ignoring LEG, EXEC, MIL, etc
dat$source = substr(dat$source,1,3)
dat$target = substr(dat$target,1,3)

# restrict to US events 
dat <- dat[dat$source=="USA" | dat$target=="USA",] 
dat <- dat[(dat$source=="USA" & dat$target=="USA")==F,] # no US-US aggression

# limit to 1946-2010
dat <- dat[dat$date>"1946-01-01",]
nrow(dat) # obvious temporal bias

################################################################


# apply cameo scores (the ones that appear in this dataset)

sort(unique(dat$code))
dat$scale[dat$code=="14O"] <- -6.5  # fix one botched code here before numericizing
dat$code <- as.numeric(as.vector(dat$code))
dat$scale[dat$code==010] <- 0
dat$scale[dat$code==011] <- -.1
dat$scale[dat$code==012] <- -.4
dat$scale[dat$code==013] <- .4
dat$scale[dat$code==014] <- 0
dat$scale[dat$code==015] <- 0
dat$scale[dat$code==016] <- 3.4
dat$scale[dat$code==017] <- 0
dat$scale[dat$code==018] <- 3.4
dat$scale[dat$code==019] <- 3.4#
dat$scale[dat$code==020] <- 3
dat$scale[dat$code==021] <- 3.4
dat$scale[dat$code==0211] <- 3.4
dat$scale[dat$code==0213] <- 3.4#
dat$scale[dat$code==0214] <- 3.4#
dat$scale[dat$code==022] <- 3.4
dat$scale[dat$code==023] <- 3.4
dat$scale[dat$code==0231] <- 3.4
dat$scale[dat$code==0232] <- 3.4
dat$scale[dat$code==0233] <- 3.4
dat$scale[dat$code==0234] <- 3.4
dat$scale[dat$code==024] <- -.3
dat$scale[dat$code==0241] <- -.3
dat$scale[dat$code==0242] <- -.3
dat$scale[dat$code==0243] <- -.3
dat$scale[dat$code==0244] <- -.3
dat$scale[dat$code==025] <- -.3
dat$scale[dat$code==0253] <- -.3#
dat$scale[dat$code==0254] <- -.3#
dat$scale[dat$code==0256] <- -.3#
dat$scale[dat$code==026] <- 4
dat$scale[dat$code==027] <- 4
dat$scale[dat$code==028] <- 4
dat$scale[dat$code==030] <- 4
dat$scale[dat$code==031] <- 5.2
dat$scale[dat$code==0311] <- 5.2
dat$scale[dat$code==0312] <- 5.2
dat$scale[dat$code==032] <- 4.5
dat$scale[dat$code==033] <- 5.2
dat$scale[dat$code==0331] <- 5.2
dat$scale[dat$code==0332] <- 5.2
dat$scale[dat$code==0333] <- 5.2
dat$scale[dat$code==0334] <- 6
dat$scale[dat$code==034] <- 7
dat$scale[dat$code==0341] <- 7
dat$scale[dat$code==0344] <- 7
dat$scale[dat$code==035] <- 7
dat$scale[dat$code==0351] <- 7
dat$scale[dat$code==0353] <- 7
dat$scale[dat$code==0355] <- 7
dat$scale[dat$code==0356] <- 7
dat$scale[dat$code==036] <- 4
dat$scale[dat$code==037] <- 5
dat$scale[dat$code==038] <- 7
dat$scale[dat$code==039] <- 5
dat$scale[dat$code==040] <- 1
dat$scale[dat$code==041] <- 1
dat$scale[dat$code==042] <- 1.9
dat$scale[dat$code==043] <- 2.8
dat$scale[dat$code==044] <- 2.5
dat$scale[dat$code==045] <- 5
dat$scale[dat$code==046] <- 7
dat$scale[dat$code==050] <- 3.5
dat$scale[dat$code==051] <- 3.4
dat$scale[dat$code==052] <- 3.5
dat$scale[dat$code==053] <- 3.8
dat$scale[dat$code==054] <- 6
dat$scale[dat$code==055] <- 7
dat$scale[dat$code==056] <- 7
dat$scale[dat$code==057] <- 8
dat$scale[dat$code==060] <- 6
dat$scale[dat$code==061] <- 6.4
dat$scale[dat$code==062] <- 7.4
dat$scale[dat$code==063] <- 7.4
dat$scale[dat$code==064] <- 7
dat$scale[dat$code==070] <- 7
dat$scale[dat$code==071] <- 7.4
dat$scale[dat$code==072] <- 8.3
dat$scale[dat$code==073] <- 7.4
dat$scale[dat$code==074] <- 8.5
dat$scale[dat$code==075] <- 7
dat$scale[dat$code==080] <- 5
dat$scale[dat$code==081] <- 5
dat$scale[dat$code==0811] <- 5
dat$scale[dat$code==0812] <- 5
dat$scale[dat$code==0813] <- 5
dat$scale[dat$code==0814] <- 5
dat$scale[dat$code==082] <- 5
dat$scale[dat$code==083] <- 5
dat$scale[dat$code==0831] <- 5
dat$scale[dat$code==0833] <- 5
dat$scale[dat$code==0834] <- 5
dat$scale[dat$code==084] <- 7
dat$scale[dat$code==0841] <- 7
dat$scale[dat$code==0842] <- 7
dat$scale[dat$code==085] <- 7
dat$scale[dat$code==086] <- 9
dat$scale[dat$code==0861] <- 9
dat$scale[dat$code==0862] <- 9
dat$scale[dat$code==0863] <- 9
dat$scale[dat$code==087] <- 9
dat$scale[dat$code==0871] <- 9
dat$scale[dat$code==0872] <- 9
dat$scale[dat$code==0873] <- 9
dat$scale[dat$code==0874] <- 10
dat$scale[dat$code==090] <- -2
dat$scale[dat$code==091] <- -2
dat$scale[dat$code==092] <- -2
dat$scale[dat$code==093] <- -2
dat$scale[dat$code==094] <- -2
dat$scale[dat$code==100] <- -5
dat$scale[dat$code==101] <- -5
dat$scale[dat$code==1013] <- -5#
dat$scale[dat$code==1014] <- -5#
dat$scale[dat$code==102] <- -5
dat$scale[dat$code==1031] <- -5#
dat$scale[dat$code==1033] <- -5#
dat$scale[dat$code==1041] <- -5
dat$scale[dat$code==1042] <- -5
dat$scale[dat$code==1043] <- -5
dat$scale[dat$code==1044] <- -5
dat$scale[dat$code==105] <- -5
dat$scale[dat$code==1053] <- -5#
dat$scale[dat$code==1056] <- -5#
dat$scale[dat$code==106] <- -5
dat$scale[dat$code==107] <- -5
dat$scale[dat$code==109] <- -5#
dat$scale[dat$code==110] <- -2
dat$scale[dat$code==111] <- -2
dat$scale[dat$code==112] <- -2
dat$scale[dat$code==1121] <- -2
dat$scale[dat$code==1122] <- -2
dat$scale[dat$code==1123] <- -2
dat$scale[dat$code==1124] <- -2
dat$scale[dat$code==1125] <- -2
dat$scale[dat$code==113] <- -2
dat$scale[dat$code==114] <- -2
dat$scale[dat$code==115] <- -2
dat$scale[dat$code==116] <- -2#
dat$scale[dat$code==120] <- -4
dat$scale[dat$code==121] <- -4
dat$scale[dat$code==1211] <- -4
dat$scale[dat$code==1212] <- -4
dat$scale[dat$code==1213] <- -4
dat$scale[dat$code==122] <- -4
dat$scale[dat$code==1222] <- -4#
dat$scale[dat$code==123] <- -4
dat$scale[dat$code==1231] <- -4
dat$scale[dat$code==1233] <- -4
dat$scale[dat$code==124] <- -5
dat$scale[dat$code==1241] <- -5#
dat$scale[dat$code==1243] <- -5#
dat$scale[dat$code==1244] <- -5#
dat$scale[dat$code==1246] <- -5#
dat$scale[dat$code==125] <- -5
dat$scale[dat$code==127] <- -5
dat$scale[dat$code==128] <- -5
dat$scale[dat$code==129] <- -5#
dat$scale[dat$code==130] <- -4.4
dat$scale[dat$code==131] <- -5.8
dat$scale[dat$code==1311] <- -5.8
dat$scale[dat$code==1312] <- -5.8
dat$scale[dat$code==1313] <- -5.8
dat$scale[dat$code==132] <- -5.8
dat$scale[dat$code==1322] <- -5.8
dat$scale[dat$code==133] <- -5.8
dat$scale[dat$code==134] <- -5.8
dat$scale[dat$code==136] <- -7
dat$scale[dat$code==137] <- -7
dat$scale[dat$code==138] <- -7
dat$scale[dat$code==1383] <- -7
dat$scale[dat$code==1384] <- -7
dat$scale[dat$code==139] <- -7
dat$scale[dat$code==140] <- -6.5
dat$scale[dat$code==141] <- -6.5
dat$scale[dat$code==1411] <- -6.5
dat$scale[dat$code==1412] <- -6.5
dat$scale[dat$code==1413] <- -6.5
dat$scale[dat$code==1414] <- -6.5
dat$scale[dat$code==142] <- -6.5
dat$scale[dat$code==143] <- -6.5
dat$scale[dat$code==144] <- -7.5
dat$scale[dat$code==145] <- -7.5
dat$scale[dat$code==150] <- -7.2
dat$scale[dat$code==151] <- -7.2
dat$scale[dat$code==152] <- -7.2
dat$scale[dat$code==153] <- -7.2
dat$scale[dat$code==154] <- -7.2
dat$scale[dat$code==160] <- -4
dat$scale[dat$code==161] <- -4
dat$scale[dat$code==162] <- -5.6
dat$scale[dat$code==1621] <- -5.6
dat$scale[dat$code==1622] <- -5.6
dat$scale[dat$code==163] <- -6.5
dat$scale[dat$code==164] <- -7
dat$scale[dat$code==166] <- -8
dat$scale[dat$code==1662] <- -8#
dat$scale[dat$code==170] <- -7
dat$scale[dat$code==171] <- -9.2
dat$scale[dat$code==1711] <- -9.2
dat$scale[dat$code==1712] <- -9.2
dat$scale[dat$code==172] <- -5
dat$scale[dat$code==1721] <- -5
dat$scale[dat$code==1722] <- -5
dat$scale[dat$code==1723] <- -5
dat$scale[dat$code==1724] <- -5
dat$scale[dat$code==173] <- -5
dat$scale[dat$code==174] <- -5
dat$scale[dat$code==175] <- -9
dat$scale[dat$code==180] <- -9
dat$scale[dat$code==181] <- -9
dat$scale[dat$code==182] <- -9.5
dat$scale[dat$code==1821] <- -9
dat$scale[dat$code==1822] <- -9
dat$scale[dat$code==1823] <- -10
dat$scale[dat$code==183] <- -10
dat$scale[dat$code==1831] <- -10
dat$scale[dat$code==1832] <- -10
dat$scale[dat$code==185] <- -8
dat$scale[dat$code==186] <- -10
dat$scale[dat$code==190] <- -10
dat$scale[dat$code==191] <- -9.5
dat$scale[dat$code==192] <- -9.5
dat$scale[dat$code==193] <- -10
dat$scale[dat$code==194] <- -10
dat$scale[dat$code==195] <- -10
dat$scale[dat$code==196] <- -9.5
dat$scale[dat$code==201] <- -9.5
dat$scale[dat$code==202] <- -10
dat$scale[dat$code==203] <- -10

sort(unique(dat$code)) # no NAs


################################################################

# assign verbal and material cooperation labels

dat$vcoop <- 0
dat$mcoop <- 0
dat$vcon <- 0
dat$mcon <- 0
dat$vcoop[dat$code >=1 & dat$code <=5] <- 1
dat$mcoop[dat$code >=6 & dat$code <=9] <- 1
dat$vcoop[dat$code >=10 & dat$code <=59] <- 1
dat$mcoop[dat$code >=60 & dat$code <=99] <- 1
dat$vcon[dat$code >=100 & dat$code <= 142] <- 1
dat$mcon[dat$code >=143 & dat$code <=204] <- 1
dat$vcoop[dat$code >=211 & dat$code <=356] <- 1
dat$mcoop[dat$code >=811 & dat$code <= 874] <- 1
dat$vcon[dat$code >= 1011 & dat$code <=1424] <- 1
dat$mcon[dat$code >=1431 & dat$code <=2042] <- 1

# exclude all threats
dat$threat = 0
dat$threat[dat$code==13] = 1
dat$threat[dat$code >= 130 & dat$ccode <= 139] = 1
dat$threat[dat$code >= 1311 & dat$ccode <= 1385] = 1
dat$vcon[dat$threat==1] = 0

# exclude threats of war only
#dat$threat = 0
#warthreats = c(137,138,1381,1382,1383,1384,1385,139)
#dat$threat[dat$code %in% warthreats] = 1
#dat$vcon[dat$threat==1] = 0

#13:[-6.0] THREATEN
#130:[-4.4] Threaten, not specified below
#131:[-5.8] Threaten non-force, not specified below
#  1311:[-5.8] Threaten to reduce or stop aid
#  1312:[-5.8] Threaten to boycott, embargo, or sanction
#  1313:[-5.8] Threaten to reduce or break relations
#132:[-5.8] Threaten with administrative sanctions, not specified below
#  1321:[-5.8] Threaten to impose restrictions on freedoms of speech and expression 
#  1322:[-5.8] Threaten to ban political parties or politicians
#  1323:[-5.8] Threaten to impose curfew
#  1324:[-5.8] Threaten to impose state of emergency or martial law
#133:[-5.8] Threaten collective dissent
#134:[-5.8] Threaten to halt negotiations
#135:[-5.8] Threaten to halt mediation
#136:[-7.0] Threaten to expel or withdraw peacekeepers
#137:[-7.0] Threaten with violent repression
#138:[-7.0] Threaten to use military force, not specified below
#  1381:[-7.0] Threaten blockade
#  1382:[-7.0] Threaten occupation 
#  1383:[-7.0] Threaten unconventional violence 
#  1384:[-7.0] Threaten conventional attack  
#  1385:[-7.0] Threaten attack with WMD
#139:[-7.0] Give ultimatum

# note economic verbal conflict -- plausibly NOT diversion

dat$vconeconomic = 0
dat$vconeconomic[dat$code==103 | dat$code==102 | dat$code==1042 | dat$code==115] = 1

#0242:[-0.3] Appeal for policy change
#103:[-5.0] Demand aid, protection, or peacekeeping
#102:[-5.0] Demand policy support
#  1042:[-5.0] Demand policy change
#115:[-2.0] Bring lawsuit against 

# note economic material conflict 
#162:[-5.6] Reduce or stop aid, not specified below
#  1621:[-5.6] Reduce or stop economic assistance
#163:[-6.5] Halt negotiations
#  1643:[-7.0] Expel or withdraw aid agencies
#166:[-8.0] Impose embargo, boycott, or sanctions

dat$mconeconomic = 0
dat$mconeconomic[dat$code==162 | dat$code==1621 | dat$code==1643 | dat$code==166] = 1


################################################################

# set up net conflict, cooperation

library(plyr)
c = as.data.frame(sort(unique(dat$target)))
colnames(c) = "country"
head(c)

for(i in 1:nrow(c)){
	c$sum_vcon[i] = sum(dat$vcon[dat$target==c$country[i]])
	c$sum_vcoop[i] = sum(dat$vcoop[dat$target==c$country[i]])
	c$sum_mcon[i] = sum(dat$mcon[dat$target==c$country[i]])
	c$sum_mcoop[i] = sum(dat$mcoop[dat$target==c$country[i]])
	# new pol
	c$sum_vcon_pol[i] = sum(dat$vcon[dat$target==c$country[i] & dat$topic[i]==1])
	c$sum_vcoop_pol[i] = sum(dat$vcoop[dat$target==c$country[i] & dat$topic[i]==1])
	c$sum_mcon_pol[i] = sum(dat$mcon[dat$target==c$country[i] & dat$topic[i]==1])
	c$sum_mcoop_pol[i] = sum(dat$mcoop[dat$target==c$country[i] & dat$topic[i]==1])	
	# new econ
	c$sum_vcon_econ[i] = sum(dat$vcon[dat$target==c$country[i] & dat$topic[i]==2])
	c$sum_vcoop_econ[i] = sum(dat$vcoop[dat$target==c$country[i] & dat$topic[i]==2])
	c$sum_mcon_econ[i] = sum(dat$mcon[dat$target==c$country[i] & dat$topic[i]==2])
	c$sum_mcoop_econ[i] = sum(dat$mcoop[dat$target==c$country[i] & dat$topic[i]==2])	
}
c$net_mcon = c$sum_mcon - c$sum_mcoop
c$net_vcon = c$sum_vcon - c$sum_vcoop
c$net_vcoop = c$sum_vcoop - c$sum_vcon
c$net_mcoop = c$sum_mcoop - c$sum_mcon
# new pol
c$net_mcon_pol = c$sum_mcon_pol - c$sum_mcoop_pol
c$net_vcon_pol = c$sum_vcon_pol - c$sum_vcoop_pol
c$net_vcoop_pol = c$sum_vcoop_pol - c$sum_vcon_pol
c$net_mcoop_pol = c$sum_mcoop_pol - c$sum_mcon_pol
# new econ
c$net_mcon_econ = c$sum_mcon_econ - c$sum_mcoop_econ
c$net_vcon_econ = c$sum_vcon_econ - c$sum_vcoop_econ
c$net_vcoop_econ = c$sum_vcoop_econ - c$sum_vcon_econ
c$net_mcoop_econ = c$sum_mcoop_econ - c$sum_mcon_econ

################################################################

# save 
write.csv(dat,file="AmericanDiplomacyDataset.csv")

