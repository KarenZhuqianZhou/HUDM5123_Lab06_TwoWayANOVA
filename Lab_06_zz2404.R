load("Lab_06_therapy.Rdata")

options(contrasts=c("contr.sum", "contr.poly"))

thp1$Severity_fac <- factor(x=thp1$Severity, 
                            levels=1:3,
                            labels=c("mild","moderate","severe"))
thp1$Therapy_fac <- factor(x=thp1$Therapy, 
                            levels=1:3,
                            labels=c("CBT","Rogerian","Assertiveness"))
thp2$Severity_fac <- factor(x=thp2$Severity, 
                            levels=1:3,
                            labels=c("mild","moderate","severe"))
thp2$Therapy_fac <- factor(x=thp2$Therapy, 
                            levels=1:3,
                            labels=c("CBT","Rogerian","Assertiveness"))

lm1 <- lm(formula = MMPI_Depression ~ Severity_fac*Therapy_fac, data = thp1)
lm2 <- lm(formula = MMPI_Depression ~ Severity_fac*Therapy_fac, data = thp2)

#Task 1: Two-way ANOVA Source Tables
library(car)
Anova(lm1, type = 3)
Anova(lm2, type = 3)

#Task 2: Interaction Plots
library(emmeans)
emm1<-emmeans(object = lm1, spec=~Severity_fac*Therapy_fac)
ip1<-emmip(object=emm1, formula = Severity_fac ~ Therapy_fac, xlab=c("Type of Therapy"), ylab=c("Post-treamtment Depression Scores"))
ip1$labels$colour <-  "Baseline Depression Severity"
print(ip1)

emm2<-emmeans(object = lm2, spec=~Severity_fac*Therapy_fac)
ip2<-emmip(object=emm2, formula = Severity_fac ~ Therapy_fac, xlab=c("Type of Therapy"), ylab=c("Post-treamtment Depression Scores"))
ip2$labels$colour <-  "Baseline Depression Severity"
print(ip2)

#Task 3&4: Simple or Main Effects and Pairwise Comparisons
emm1.m<-emmeans(object = lm1, spec=~Therapy_fac) # main effects of therapy
joint_tests(emm1.m) # similar as Anova(lm1, type = 3) above
pairs(emm1.m, adjust="none")

joint_tests(emm2,by="Severity_fac")
emm2.s<-emmeans(object = lm2, spec=~Therapy_fac|Severity_fac) # simple effects of therapy
pairs(emm2.s, adjust="none")
