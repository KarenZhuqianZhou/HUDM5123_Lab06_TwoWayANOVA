# Load dat1 and dat2 by loading file "Lab_06_Prestige.Rdata"
load(file = "Lab_06_Prestige.Rdata")

# Start with dat1
head(dat1)

# Reorder the levels of the type factor
levels(dat1$type)
dat1$type <- factor(x = dat1$type,
                    levels = c("bc", "wc", "prof"))
dat2$type <- factor(x = dat2$type,
                    levels = c("bc", "wc", "prof"))

# Table of type by income factor
table(dat1$type, dat1$incomeF) 

# Inspect settings
options("contrasts")

# Set the contrast coding for undordered factors
# to deviation coding (contr.sum)
options(contrasts = c("contr.sum", "contr.poly"))

# check the contrast coding
contrasts(x = dat1$type)
contrasts(x = dat1$incomeF)


# Fit the full model (interaction and both main effects)
# with dat1
lm1 <- lm(formula = prestige ~ incomeF*type, data = dat1)
summary(lm1)

# examine the actual design matrix for lm1
model.matrix(lm1)

# Create the Two-Way ANOVA table
library(car)
Anova(lm1, type = 3)

# Create an interaction plot using emmeans
library(emmeans)
emm1 <- emmeans(object = lm1,
                specs = ~ incomeF*type)
emm1

p <- emmip(object = emm1, 
      formula = incomeF ~ type,
      xlab = "Type of Profession",
      ylab = "Mean Prestige Score")
p$labels$colour <- "Income" # Change legend title
print(p)

# Interaction is not significant nor visually
# important, so examine main effects.

# Main effects of income factor, averaging over
# levels of job type:
emm2 <- emmeans(object = lm1,
                specs = ~ incomeF)
emm2

# Main effects pairwise comparisons of income,
# averaging over job type levels:
pairs(emm2, adjust = "none")

# Main effects of job type factor, averaging over
# levels of income factor:
emm3 <- emmeans(object = lm1,
                specs = ~ type)
emm3

# Main effects pairwise comparisons of job 
# type, averaging over income levels:
pairs(emm3, adjust = "none")

################
### Now move to data set 2 (real Prestige data)
################

# Fit the full model with dat2
lm2 <- lm(formula = prestige ~ incomeF*type, data = dat2)
summary(lm2)

# Produce the Two-Way ANOVA table
Anova(lm2, type = 3)

# Create an interaction plot
emm4 <- emmeans(object = lm2,
                specs = ~ incomeF*type)
emm4
emmip(object = emm4,
      formula = incomeF ~ type,
      xlab = "Type of Profession",
      ylab = "Mean Prestige Score")



# Test the simple (main) effects of job type at
# both levels of income separately. We have to 
# do conditional tests because of the significant
# two-way interaction.
joint_tests(lm2, by = "incomeF")

# Create an emm object that examines type conditional
# on level of income and use it to do simple
# pairwise comparisons.
emm5 <- emmeans(object = lm2,
                specs = ~ type | incomeF)
emm5
pairs(emm5, adjust = "none")

# Look at the job titles that are bc and high income.
dat2$X[which(dat2$type == "bc" & dat2$incomeF == "high")]

# both blue collar and in the lower income category:
dat2$X[which(dat2$type == "bc" & dat2$incomeF == "low")]


# Could go the other way and test the simple (main) effects
# of income category at each of the three job types.
joint_tests(object = lm2, by = "type")

# Follow up with simple pairwise comparisons (though these
# will be the same as the joint test results because the 
# income factor only has two levels).
emm6 <- emmeans(object = lm2,
                specs = ~ incomeF | type)
pairs(emm6, adjust = "none")

