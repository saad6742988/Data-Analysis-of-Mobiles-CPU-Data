data <- read.csv("ML_ALL_benchmarks.csv")
cpuScore<-data$cpuScore
summary(cpuScore)
# histogram and normal curve of orignal data
OrignalDataPlot<-function()
{
  hist(data$cpuScore)
  mean(data$cpuScore)
  sd(data$cpuScore)
  plotNormalCurve(data$cpuScore,0,1050,1)
}


#Remove outliers from data
removeOutliers<-function(dataList)
{
  q1=quantile(dataList,0.25)
  q3=quantile(dataList,0.75)
  iqr=IQR(dataList)
  lower_limit=q1-1.5*iqr
  upper_limit=q3+1.5*iqr
  lower_limit
  upper_limit
  #Data without outlairs
  without_outlairs<-subset(dataList, dataList>lower_limit & dataList<upper_limit)
  #cpuScore[cpuScore>lower_limit & cpuScore<upper_limit ]
  #cpuScore_without_outlairs
  return(without_outlairs)
}

#standardize data
standardizeData<-function(dataList)
{
  #Z-score implementation to standardize data
  standardData <-((dataList  - mean(dataList)) / sd(dataList))
  return(standardData)
  
}

#plot normal curve
plotNormalCurve<-function(dataList,lowerLimit,upperLimit,pointsDiff)
{
  xAxis<-seq(lowerLimit,upperLimit,pointsDiff)
  yAxis<-dnorm(xAxis,mean(dataList),sd(dataList))
  plot(xAxis,yAxis,type='l')
}

#component bar graph for cpu cores
DisplayComponentBarChart <- function()
{
  companies<<-unique(data$company) #get unique company name
  cores<<-unique(data$cores)[order(unique(data$cores))]#get unique cores
  coresDisplay<-c()
  for (i in 1:5){
    temp1<-paste(toString(cores[i]),"cores")
    coresDisplay<-append(coresDisplay,temp1)
    
  }
  #length(cores)
  #length(subset(data$cores, data$company=="Apple" & data$cores==3))
  processorCount<<-c()
  processorCount
  for (core in cores) {
    for (company in companies) {
      temp<-length(subset(data$cores, data$company==company & data$cores==core))
      processorCount<<-append(processorCount,temp)
    }
  }
  #processorCount
  colors1 = c("green","orange","brown","red","blue")
  # Create the matrix of the values.
  processorCountMatrix <- matrix(processorCount, nrow = length(cores), ncol = length(companies), byrow = TRUE)
  processorCountMatrix
  # Create the bar chart
  barplot(processorCountMatrix,legend=TRUE, main = "Total CPUs", names.arg = companies, xlab = "Companies", ylab = "CPUs",ylim=c(0,105),col=colors1)
  # Add the legend to the chart
  #legend("topleft", cores, cex = 1.3,fill=colors1)
  legend(x = "right", legend = coresDisplay, fill = colors1)
}

#PLot pie chart
PlotPieChart<-function()
{
  totalProcessors<<-c()
  for (company in companies) {
    temp<-length(subset(data$cores, data$company==company))
    totalProcessors<<-append(totalProcessors,temp)
  }
  totalProcessors
  piepercent<- round(100*totalProcessors/sum(totalProcessors), 1)
  pie(totalProcessors, labels = piepercent, main = "Processors pie chart",col = rainbow(length(companies)))
  legend("topright", companies, cex = 0.8,fill = rainbow(length(companies)))
}

#probability of getting less score less than X specified by company and cores
lessProbability<-function(companyName,cpuCore,score)
{
  cpuScores_of_specific_CompanyAndCores<-subset(data$cpuScore, data$company==companyName & data$cores==cpuCore)
  #print(cpuScores_of_specific_CompanyAndCores)
  normalData<-removeOutliers(cpuScores_of_specific_CompanyAndCores)
  #print(normalData)
  prob<-pnorm(score,mean(normalData),sd(normalData))
  #prob<-pnorm(60,50,10)
  prob<-prob*100
  cat("if",companyName,"makes cpu with", cpuCore, "cores,there's",prob,"% chance of getting Cpu score less than",score)
}
#probability of getting less score greater than X specified by company and cores
greaterProbability<-function(companyName,cpuCore,score)
{
  cpuScores_of_specific_CompanyAndCores<-subset(data$cpuScore, data$company==companyName & data$cores==cpuCore)
  #print(cpuScores_of_specific_CompanyAndCores)
  normalData<-removeOutliers(cpuScores_of_specific_CompanyAndCores)
  #print(normalData)
  #print(summary(normalData))
  prob<-pnorm(score,mean(normalData),sd(normalData))
  #prob<-pnorm(3.2,3,0.5)
  prob<-1-prob
  prob<-prob*100
  cat("if",companyName,"makes cpu with", cpuCore, "cores,there's",prob,"% chance of getting Cpu score less than",score)
}
#probability of getting less score between X and Y specified by company and cores
betweenProbability<-function(companyName,cpuCore,score1,score2)
{
  cpuScores_of_specific_CompanyAndCores<-subset(data$cpuScore, data$company==companyName & data$cores==cpuCore)
  #print(cpuScores_of_specific_CompanyAndCores)
  normalData<-removeOutliers(cpuScores_of_specific_CompanyAndCores)
  #print(normalData)
  #print(summary(normalData))
  prob1<-pnorm(score1,mean(normalData),sd(normalData))
  prob2<-pnorm(score2,mean(normalData),sd(normalData))
  #prob1<-pnorm(20,30,5)
  #prob2<-pnorm(40,30,5)
  prob<-prob2-prob1
  prob<-prob*100
  cat("if",companyName,"makes cpu with", cpuCore, "cores,there's",prob,"% chance of getting Cpu score between",score1,"and",score2)
}
#what value of CPU score corresponds to X% of abc company for Y cores
correspondingValue<-function(companyName,cpuCore,percentile)
{
  cpuScores_of_specific_CompanyAndCores<-subset(data$cpuScore, data$company==companyName & data$cores==cpuCore)
  #print(cpuScores_of_specific_CompanyAndCores)
  normalData<-removeOutliers(cpuScores_of_specific_CompanyAndCores)
  #print(normalData)
  temp<-percentile/100
  value<-qnorm(temp,mean(normalData),sd(normalData))
  #prob<-pnorm(60,50,10)
  cat("for",companyName,"cpus with", cpuCore, "cores,",value,"corresponds to",percentile,"%")
}

#come important data variables used in functions
cpuScore_without_outlairs
cpuScore_standardize
processorCount
totalProcessors
companies
cores
cpuScore


OrignalDataPlot()
#normalize cpuScore
cpuScore_without_outlairs<-removeOutliers(data$cpuScore)
#standardized cpu score
cpuScore_without_outlairs
cpuScore_standardize<-standardizeData(cpuScore_without_outlairs)
#plot normal curve of standardized Cpu sccore
plotNormalCurve(cpuScore_standardize,-3.5,3.5,0.1)
#plotting charts
DisplayComponentBarChart()
PlotPieChart()
#if abc company makes cpu with X cores,what's the probabilty of getting Cpu score less than Y.
lessProbability('Apple',8,900)
#if abc company makes cpu with X cores,what's the probabilty of getting Cpu score greater than Y.
greaterProbability('Apple',8,1000)
#if abc company makes cpu with X cores,what's the probabilty of getting Cpu score between A and B.
betweenProbability('Apple',8,600,1000)
#what value of CPU score corresponds to X% of abc company for Y cores
correspondingValue('Apple',8,99.99)


#temporary codes
summary(cpuScore_without_outlairs)
summary(cpuScore_standardize)
summary(cpuScore)
hist(cpuScore_without_outlairs)
xtemp<-seq(0,650,1)
ytemp<-dnorm(xtemp,mean(cpuScore_without_outlairs),sd(cpuScore_without_outlairs))
plot(xtemp,ytemp,type='l')



cpuScore_standardize1 <<-((cpuScore  - mean(cpuScore)) / sd(cpuScore))
cpuScore_standardize1
summary(cpuScore_standardize1)
x21<-seq(-3.5,3.5,0.1)
y21<-dnorm(x21,mean(cpuScore_standardize1),sd(cpuScore_standardize1))
plot(x21,y21,type='l')

#abc = subset(data$cpuScore, data$company=='Qualcomm' & data$cores==8)
abc = cpuScore_without_outlairs
sort(abc)
q12=quantile(abc,0.25)
q32=quantile(abc,0.75)
iqr=IQR(abc)
lower_limit1=q12-1.5*iqr
upper_limit1=q32+1.5*iqr
lower_limit1
upper_limit1
#Data without outlairs
abc_without_outlairs<<-subset(abc, abc>lower_limit1 & abc<upper_limit1)
sort(abc_without_outlairs)

qnorm(0.1281362,792.8333,169.8604)
pnorm(8355.146,10000,1000)
 


