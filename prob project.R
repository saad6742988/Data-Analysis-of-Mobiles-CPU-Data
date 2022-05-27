data <- read.csv("C:\\Users\\Muhammad Saad\\Desktop\\ML_ALL_benchmarks.csv")
#summary(data)
gpuScore<-data[,c(7)]

# histogram and normal curve

summary(data[,c(7)])
hist(data[,c(7)])
mean(data[,c(7)])
sd(data[,c(7)])
x<-seq(-3000,3000,0.1)
y<-dnorm(x,mean(data[,c(7)]),sd(data[,c(7)]))
plot(x,y,type='l')


#define Min-Max normalization function
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#apply Min-Max normalization to first four columns in iris dataset
normal_data <- as.data.frame(lapply(data[7], min_max_norm))
normal_data[,c(1)]
summary(normal_data[,c(1)])
mean(normal_data[,c(1)])
sd(normal_data[,c(1)])
x1<-seq(-1,1,0.1)
y1<-dnorm(x1,mean(normal_data[,c(1)]),sd(normal_data[,c(1)]))
plot(x1,y1,type='l')


q1=quantile(gpuScore,0.25)
q3=quantile(gpuScore,0.75)
iqr=IQR(gpuScore)
lower_limit=q1-1.5*iqr
upper_limit=q3+1.5*iqr
lower_limit
upper_limit
#Data without outlairs
gpuScore_without_outlairs<-subset(gpuScore, gpuScore>lower_limit & gpuScore<upper_limit)
#gpuScore[gpuScore>lower_limit & gpuScore<upper_limit ]
gpuScore_without_outlairs

#Z-score implementation to standardize data
gpuScore_standardize <-((gpuScore_without_outlairs  - mean(gpuScore_without_outlairs)) / sd(gpuScore_without_outlairs))
gpuScore_standardize
summary(gpuScore_standardize)
x2<-seq(-3.5,3.5,0.1)
y2<-dnorm(x2,mean(gpuScore_standardize),sd(gpuScore_standardize))
plot(x2,y2,type='l')



#component bar graph for cpu cores
companies<-unique(data$company)
cores<-unique(data$cores)[order(unique(data$cores))]
cores
#length(cores)
#length(subset(data$cores, data$company=="Apple" & data$cores==3))
processorCount<-c()
processorCount
for (core in cores) {
  for (company in companies) {
    temp<-length(subset(data$cores, data$company==company & data$cores==core))
    cat(company,core,temp)
    processorCount<-append(processorCount,temp)
  }
}
processorCount
colors1 = c("green","orange","brown","red","blue")
# Create the matrix of the values.
processorCountMatrix <- matrix(processorCount, nrow = length(cores), ncol = length(companies), byrow = TRUE)
processorCountMatrix
# Create the bar chart
barplot(processorCountMatrix,legend=TRUE, main = "Total CPUs", names.arg = companies, xlab = "Companies", ylab = "CPUs",ylim=c(0,105),col=colors1)

# Add the legend to the chart
coresName
#legend("topleft", cores, cex = 1.3,fill=colors1)
legend(x = "right", legend = cores, fill = colors1)


