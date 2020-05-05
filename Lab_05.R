dat <- read.csv(file = "/Users/keller/Dropbox/Teaching/Linear Models and Experimental Design/2020 SPRING/05 Two Way/study2.csv")
dim(dat)
head(dat)

# 29 people failed attention check.
table(dat$distract_failed, 
      useNA = "always")

# Delete their data
dim(dat)
dat <- dat[which(dat$distract_failed == 0), ]
dim(dat)

# We will focus on the following outcome variables:
#  - compassion
#  - optimism
#  - gratitude
#  - awe
#  - love

names(dat)

# Identify and average compassion items.
#################
# COMPASSION
grep(pattern = "comp", 
     x = names(dat), 
     ignore.case = TRUE,
     value = TRUE)
dat$comp_avg <- (dat$selfTrnsdEmo_compass_4 + 
                   dat$selfTrnsdEmo_compass_9 + 
                   dat$selfTrnsdEmo_compass_14) / 3
# Compare means by condition
by(data = dat$comp_avg, INDICES = dat$Condition, FUN = mean)
by(data = dat$comp_avg, INDICES = dat$Condition, FUN = sd)

#################
# OPTIMISM
grep(pattern = "opt", 
     x = names(dat), 
     ignore.case = TRUE,
     value = TRUE)
dat$optim_avg <- (dat$selfTrnsdEmo_optimsism_5 + 
                   dat$selfTrnsdEmo_optimsism_10 + 
                   dat$selfTrnsdEmo_optimsism_15) / 3
# Compare means by condition
by(data = dat$optim_avg, INDICES = dat$Condition, FUN = mean)
by(data = dat$optim_avg, INDICES = dat$Condition, FUN = sd)

#################
# GRATITUDE
grep(pattern = "grat", 
     x = names(dat), 
     ignore.case = TRUE,
     value = TRUE)
dat$grat_avg <- (dat$selfTrnsdEmo_gratitude_2 + 
                 dat$selfTrnsdEmo_gratitude_7 + 
                 dat$selfTrnsdEmo_gratitude_12) / 3
# Compare means by condition
by(data = dat$grat_avg, INDICES = dat$Condition, FUN = mean)
by(data = dat$grat_avg, INDICES = dat$Condition, FUN = sd)

#################
# AWE
grep(pattern = "awe", 
     x = names(dat), 
     ignore.case = TRUE,
     value = TRUE)
dat$awe_avg <- (dat$selfTrnsdEmo_awe_1+ 
                   dat$selfTrnsdEmo_awe_6 + 
                   dat$selfTrnsdEmo_awe_11) / 3
# Compare means by condition
by(data = dat$awe_avg, INDICES = dat$Condition, FUN = mean)
by(data = dat$awe_avg, INDICES = dat$Condition, FUN = sd)

#################
# LOVE
grep(pattern = "love", 
     x = names(dat), 
     ignore.case = TRUE,
     value = TRUE)
dat$love_avg <- (dat$selfTrnsdEmo_love_3 + 
                  dat$selfTrnsdEmo_love_8 + 
                  dat$selfTrnsdEmo_love_13) / 3
# Compare means by condition
by(data = dat$love_avg, INDICES = dat$Condition, FUN = mean)
by(data = dat$love_avg, INDICES = dat$Condition, FUN = sd)


# And the following covariates
#  - age
#  - sex
#  - marital/relationship status
#  - political affiliation

vars <- c("Age.x", "Marital.status", "sex", 
          "Condition", "Political.Affiliation..US.",
          "comp_avg", "optim_avg", "grat_avg", 
          "awe_avg", "love_avg")
rct <- subset(x = dat, select = vars)
names(rct)[2] <- "mar_status"
names(rct)[5] <- "pol_affil"
dim(rct)
head(rct)

interaction.plot(x.factor = rct$Condition,
                 trace.factor = rct$sex,
                 response = rct$comp_avg, 
                 trace.label = "Sex", 
                 xlab = "Condition",
                 ylab = "Compassion")

rct$Condition <- relevel(x = rct$Condition,
                         ref = "Control")
levels(rct$Condition)

levels(rct$sex)
table(rct$sex, useNA = "always")
rct$sex <- factor(rct$sex)
levels(rct$sex)

# Political party affiliation
table(rct$pol_affil, useNA = "always")

miss_ind <- which(rct$pol_affil == "N/A" | rct$pol_affil == "")
rct$pol_affil[miss_ind] <- NA
table(rct$pol_affil, useNA = "always")
rct$pol_affil <- factor(rct$pol_affil)
table(rct$pol_affil, useNA = "always")

interaction.plot(x.factor = rct$pol_affil,
                 trace.factor = rct$Condition,
                 response = rct$comp_avg, 
                 trace.label = "Condition", 
                 xlab = "Political Affiliation",
                 ylab = "Compassion")

interaction.plot(x.factor = rct$Condition,
                 trace.factor = rct$pol_affil,
                 response = rct$comp_avg, 
                 trace.label = "Political\n  Affiliation", 
                 xlab = "Condition",
                 ylab = "Compassion")


# Marital status
table(rct$mar_status, useNA = "always")
rct$mar_status2 <- NA
rct$mar_status2[which(rct$mar_status == "Married")] <- "Married"
rct$mar_status2[which(rct$mar_status == "Single" | 
                        rct$mar_status == "Never married" | 
                        rct$mar_status == "Widowed" | 
                        rct$mar_status == "Separated" | 
                        rct$mar_status == "Divorced")] <- "Single"
rct$mar_status2[which(rct$mar_status == "In a relationship")] <- "Relationship"
table(rct$mar_status2, useNA = "always")

interaction.plot(x.factor = rct$Condition,
                 trace.factor = rct$mar_status2,
                 response = rct$comp_avg, 
                 trace.label = "Marital\nStatus", 
                 xlab = "Condition",
                 ylab = "Compassion")

interaction.plot(x.factor = rct$mar_status2,
                 trace.factor = rct$Condition,
                 response = rct$comp_avg, 
                 trace.label = "Marital\nStatus", 
                 xlab = "Condition",
                 ylab = "Compassion")

# Create a data set with no "other" political affiliations
rct2 <- rct[-which(rct$pol_affil == "Other"),]
dim(rct2)

# Is there an effect for political affiliation?
lm1 <- lm(formula = comp_avg ~ pol_affil,
          data = rct2)
library(car)
Anova(lm1, type = 3)
emm1 <- emmeans::emmeans(object = lm1, 
                 specs = ~ pol_affil)
pairs(emm1, adjust = "none")
pairs(emm1, adjust = "Tukey")
pairs(emm1, adjust = "Bonferroni")
pairs(emm1, adjust = "Holm")
pairs(emm1, adjust = "Scheffe")
pairs(emm1, adjust = "FDR")
