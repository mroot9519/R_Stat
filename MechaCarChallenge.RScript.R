challenge_tabe <- read.csv(file='MechaCar_mpg.csv', check.names=F, stringsAsFactors = F)
challenge_matrix <- as.matrix(challenge_table[,c('vehicle length','vehicle weight','spoiler angle','ground clearance','AWD','mpg')])
cor(challenge_matrix)
lm(`mpg` ~ `vehicle length` + `vehicle weight` + `spoiler angle` + `ground clearance` + `AWD`, data = challenge_table)
summary(lm(`mpg` ~ `vehicle length` + `vehicle weight` + `spoiler angle` + `ground clearance` + `AWD`, data = challenge_table))
challenge_model <- lm(`mpg` ~ `vehicle length` + `vehicle weight` + `spoiler angle` + `ground clearance` + `AWD`, data = challenge_table)

#coils below
coils_table <- read.csv(file='Suspension_Coil.csv', check.names=F, stringsAsFactors = F)
ggplot(coils_table, aes(x=PSI)) + geom_density() #graphs distribution
shapiro.test(coils_table$PSI) #test for normality
coils_summary <- summary(coils_table$PSI)
sd(coils_table$PSI)
var(coils_table$PSI)
sample_coils <- coils_table %>% sample_n(50)
t.test(log10(sample_coils$PSI),mu=mean(log10(coils_table$PSI)))