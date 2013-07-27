#Load the initial dataset
titanic <- read.csv("/home/predictiveds/kaggle-in-R/Titanic/data/train_full.csv")

View(titanic)

#Feature Engineering
titanic$survived <- factor(titanic$survived)
titanic$pclass <- factor(titanic$pclass)
titanic$sibsp <- factor(titanic$sibsp)
titanic$parch <- factor(titanic$parch)
titanic$embarked <- factor(titanic$embarked)

#Feature Analysis
summary(titanic)
layout(matrix(1:8,ncol=2))
barplot(table(titanic$survived), main="Survived")
barplot(table(titanic$pclass), main="pClass")
barplot(table(titanic$sex), main="Sex")
hist(titanic$age, main="Age")
barplot(table(titanic$sibsp), main="Sibsp")
barplot(table(titanic$parch), main="Parch")
hist(titanic$fare, main="Fare")
barplot(table(titanic$emparked), main="Embarked")

layout(1)

#pie(c(summary(titanic$survived)['0'],summary(titanic$survived)['1']),labels=c("Dead","Survivors"),main="survived")

#Feature selection
layout(matrix(1:8,ncol=2))
mosaicplot(titanic$pclass ~ titanic$survived, main='pClass ~ Survived',xlab="pClass",ylab="Survived")
mosaicplot(titanic$sex ~ titanic$survived, main='Sex ~ Survived',xlab="Sex",ylab="Survived")
boxplot(titanic$age ~ titanic$survived, main='Age ~ Survived',xlab="Age",ylab="Survived")
mosaicplot(titanic$sibsp ~ titanic$survived, main='Sibsp ~ Survived',xlab="Sibsp",ylab="Survived")
mosaicplot(titanic$parch ~ titanic$survived, main='Parch ~ Survived',xlab="Parch",ylab="Survived")
boxplot(titanic$fare ~ titanic$survived, main='Fare ~ Survived',xlab="Fare",ylab="Survived")
mosaicplot(titanic$embarked ~ titanic$survived, main='Embarked ~ Survived',xlab="Embarked",ylab="Survived")
layout(1)


#Rank the best feature to predict Survived
library("FSelector")

display_feature_importance <- function(feature_importance, title) {
  feature_importance$name = rownames(feature_importance)
  feature_importance <- feature_importance[order(-feature_importance$attr_importance),]
  print(feature_importance)
  barplot(feature_importance$attr_importance, main=title,names.arg=row.names(feature_importance))
  
}

feature_selection <- function(subset,discrete_formula,formula,name="All") {
  #compute feature importance for discrete data (factor)
  feature_importance <- chi.squared(discrete_feature_formula, subset)
  display_feature_importance(feature_importance,paste(name, "- Chi² Feature importance ~ Survived"))
  
  #compute feature importance for discrete and continuous data
  feature_importance <- random.forest.importance(feature_formula, subset, importance.type=1)
  display_feature_importance(feature_importance,paste(name, "- RF Feature importance ~ Survived"))
  
}

#Display feature selection for all cluster
layout(matrix(1:2,ncol=1))
feature_formula <- survived ~ pclass + sex + age + fare  + sibsp + parch + embarked
discrete_feature_formula <- survived ~ pclass + sex  + sibsp + parch + embarked
feature_selection(titanic,discrete_feature_formula,feature_formula)
layout(1)

#Display feature selection by cluster
layout(matrix(1:4,ncol=2))
feature_formula <- survived ~ pclass  + age + fare  + sibsp + parch + embarked
discrete_feature_formula <- survived ~ pclass  + sibsp + parch + embarked
male_titanic <- titanic[which(titanic$sex=="male"),]
female_titanic <- titanic[which(titanic$sex=="female"),]

feature_selection(male_titanic,discrete_feature_formula,feature_formula,"Male")
feature_selection(female_titanic,discrete_feature_formula,feature_formula,"Female")
layout(1)

#RF model
library("randomForest")
rf.all <- randomForest(survived~ sex+ pclass + fare,titanic)
rf.female <- randomForest(survived~ pclass + fare,female_titanic)
male_titanic.imputed <- rfImpute(survived~ age + fare + sibsp + pclass,male_titanic)
rf.male <- randomForest(survived~ age + fare + sibsp + pclass,male_titanic.imputed, proximity=TRUE)

#Blend models
model_blender <- function(models,class_name='BlendedModel') {
  model <- list(
    models = models
    )
  
  class(model) <- class_name
  return(model)
}

titanic_model <- model_blender(c(rf.all, rf.female, rf.male),'TitanicModel')
predict.TitanicModel <- function(object,newdata=NULL,...) {
  return(predict(rf.all))
}



predict(titanic_model);

