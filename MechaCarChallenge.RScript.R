challenge_table <- read.csv(file='MechaCar_mpg.csv', check.names=F, stringsAsFactors = F)
challenge_matrix <- as.matrix(challenge_table[,c('vehicle length','vehicle weight','spoiler angle','ground clearance','AWD','mpg')])
cor(challenge_matrix)
lm(`mpg` ~ `vehicle length` + `vehicle weight` + `spoiler angle` + `ground clearance` + `AWD`, data = challenge_table)
summary(lm(`mpg` ~ `vehicle length` + `vehicle weight` + `spoiler angle` + `ground clearance` + `AWD`, data = challenge_table))
challenge_model <- lm(`mpg` ~ `vehicle length` + `vehicle weight` + `spoiler angle` + `ground clearance` + `AWD`, data = challenge_table)

#coils below
coils_table <- read.csv(file='Suspension_Coil.csv', check.names=F, stringsAsFactors = F)
ggplot(coils_table, aes(x=PSI)) + geom_density() #graphs distribution
shapiro.test(coils_table$PSI) #test for normality
#coil summary stats
coil_stats <- matrix(c(mean(coils_table$PSI), median(coils_table$PSI), sd(coils_table$PSI), var(coils_table$PSI)),ncol=1, byrow=TRUE)
colnames(coil_stats) <- "PSI"
rownames(coil_stats) <- c("mean","median","standard deviation", "variance")
coil_stats <- as.table(coil_stats)
coil_stats

#sampel test
sample_coils <- coils_table %>% sample_n(50)
t.test(sample_coils$PSI,mu=mean(coils_table$PSI))