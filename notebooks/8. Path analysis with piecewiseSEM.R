install.packages('piecewiseSEM')
library('piecewiseSEM')
data <- read.table('For_SEM2_del2N.csv', sep=',', header=TRUE)
head(data)

### Final model
modellist <- psem(
	### food 2 met/asv
	lm(asv_Factor2 ~ food_Factor3, data),
	lm(asv_Factor5 ~ food_Factor1, data),
	lm(met_Factor1 ~ food_Factor3, data),
	lm(met_Factor2 ~ food_Factor1 + food_Factor8, data),
	lm(met_Factor3 ~ food_Factor4, data), ### ¸¨Ê³£¬¿ÉÈ¥µô
	lm(met_Factor10 ~ food_Factor4 + food_Factor5 + food_Factor6, data),
	lm(met_Factor12 ~ food_Factor6, data),

	### ASV/Met -> Blood
	lm(LH ~ met_Factor10, data),
	lm(E2 ~ asv_Factor9 + met_Factor1 + met_Factor11, data),
	lm(FSH ~ asv_Factor7, data),
	#lm(PRL ~ met_Factor11, data),
	##
	#lm(Cu ~ met_Factor6, data),
	lm(Zn ~ asv_Factor8 + met_Factor12, data),
	#lm(Mg ~ met_Factor7, data),
	#lm(pb ~ asv_Factor10 + met_Factor12, data),
	lm(Fe ~ met_Factor9, data),
	lm(Ca  ~ asv_Factor7 + met_Factor3 + met_Factor5, data),
	
	### Blood-> B³¬
	glm(Uterine.volume ~ LH, family=poisson(link = "sqrt"), data),
	#lm(Uterine.volume ~ LH, data),
	lm(Right.ovarian.volume ~ E2 + met_Factor2, data),
	lm(Left.ovarian.volume ~ E2 + met_Factor2, data),
	lm(Right.breast.volume ~ met_Factor2, data),
	#lm(Left.breast.volume ~ met_Factor2, data),
	glm(Bone.age ~ LH, family=poisson(link = "sqrt"), data),
	#lm(Bone.age ~ LH, data),
	###
	lm(Follicles.max.right. ~ asv_Factor2, data),
	lm(Follicles.max.left. ~ asv_Factor2, data),
	data
)
summary(modellist)
plot(modellist)




