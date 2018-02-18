####This script created to reshape data for MLSA/ANOVA Analysis####
###Author: Caleb Marshall/Created 11/21/2017###
library(reshape)
setwd("~/Desktop/Source/")
romantic = read.csv("romantic gender cosines.csv")
hobby = read.csv("hobbies gender cosines.csv")

#Removing unnecessary first column#
romnew = romantic[,-1]
hobnew = hobby[,-1]

####Reshaping Romantic Data####
###Isolating by gender-gender comparsion###
rom_fem = melt(romnew[1:53, 1:53])
rom_mal = melt(romnew[54:107, 54:107])
rom_mix = melt(romnew[1:53, 54:107])

#View(rom_fem)
#View(rom_mal)
#View(rom_mix)

###Adding ID Variable for later Analysis###
romfem = cbind(id = 0, rom_fem)
rommal = cbind(id = 1, rom_mal)
rommix = cbind(id = 2, rom_mix)

totalrom = rbind(romfem, rommal, rommix)
View(totalrom)
write.csv(x = totalrom, file = "total rom.csv")
####Reshaping Hobby Data####
###This follows the same procedure as the romantic data###
hob_fem = melt(hobnew[1:53, 1:53])
hob_mal = melt(hobnew[54:106, 54:106])
hob_mix = melt(hobnew[1:53, 54:106])
###Id Variable###
hobfem = cbind(id = 0, hob_fem)
hobmal = cbind(id = 1, hob_mal)
hobmix = cbind(id = 2, hob_mix)

###Combining all conditions into full dataset###
totalhob = rbind(hobfem, hobmal, hobmix)
View(totalhob)

####ANOVA analysis of Cosines Differences by Gender####
###This portion created by Caleb Marshall on 11/22/17###


####
###t-test for Within-Gender Differences###
options(scipen = 999)
t.test(romfem$value,
       rommal$value,
       paired = F,
       conf.level = .95)
##effect size for t-test##
d.indtt(t = 4.8599, n1 = 2809, n2 = 2916, a = .05, k = 2)
###t-test for Mixed to Homogenous Gender###
t.test(rommix$value,
       romfem$value,
       paired = F,
       conf.level = .95)
t.test(rommix$value,
       rommal$value,
       paired = F,
       conf.level = .95)
##effect size for female-to-mix##
d.indtt(t = -5.5085, n1 = 2862, n2 = 2809, a = .05, k = 2)
##effect size for male-to-mix##
d.indtt(t = -0.34631, n1 = 2862, n2 = 2916, a = .05, k = 2)

