#===================生成  predict-data
predict_data = read.table("predict_date_1.txt",header = FALSE, colClasses = "character")
predict_data$date = as.character(as.POSIXct(predict_data$V1,format="%Y-%m-%d"))
predict_data$weekday = weekdays(as.POSIXct(predict_data$V1,format="%Y-%m-%d"))
predict_data$time = as.integer(sapply(strsplit(predict_data$V1, split = "-"),`[`,4))

tmp = data.frame(hashid=unique(supply$hashid))
predict_data = sqldf("select * from tmp,predict_data")

#===================       traffic
#traffic = read.table("features/traffic.csv",sep = ",", header = TRUE, colClasses = c("character","character","integer","numeric"))

#==================      weather


#==================      predict-supply
test_supply = read.table("features/demand_supply_gap_v1/test_supply.txt", sep = ",",header = TRUE, colClasses = "character")
test_supply$TimePiece = as.integer(test_supply$TimePiece)
test_supply$test_supply = as.integer(test_supply$test_supply)
test_supply = test_supply[test_supply$TimePiece > 43 & test_supply$TimePiece < 143,]
test_supply = test_supply[order(test_supply$hashid, test_supply$date, test_supply$TimePiece),]

#==================      predict-demand
test_demand = read.table("features/demand_supply_gap_v1/test_demand.txt", sep = ",",header = TRUE, colClasses = "character")
test_demand$TimePiece = as.integer(test_demand$TimePiece)
test_demand$test_demand = as.integer(test_demand$test_demand)
test_demand = test_demand[test_demand$TimePiece > 43 & test_demand$TimePiece < 143,]
test_demand = test_demand[order(test_demand$hashid, test_demand$date, test_demand$TimePiece),]

#生成预测点的 前当前时间片特征值
supply_feature = c()
for(index in 1:nrow(predict_data)){
  pd = predict_data[index,]
  i = which(pd$hashid==test_supply$hashid & pd$date==test_supply$date & pd$time>test_supply$TimePiece )
  if(length(i)==0){
    print(pd)
    #使用上一时刻的中位值
    supply_feature = c(supply_feature, 
         median_supply$median_supply[which(median_supply$hashid==pd$hashid & median_supply$TimePiece==(pd$time-1))])
  }else
  #取最近的一个supply值
    supply_feature = c(supply_feature, test_supply$test_supply[i[length(i)]])
}
predict_data$supply_feature = supply_feature

#生成预测点的  traffic 值
traffic_feature = c()
for(index in 1:nrow(predict_data)){
  pd = predict_data[index,]
  i = which(pd$hashid==traffic$hashid & pd$date==traffic$Date & pd$time>traffic$TimePiece )
  if(length(i)==0){
    print(pd)
    md = which(median_traffic$hashid==pd$hashid & median_traffic$TimePiece==(pd$time-1))
    if(length(md)==0)
      #使用全局中位值
      traffic_feature = c(traffic_feature, median(median_traffic$median_traffic))
    else
      #使用区域 中位值
      traffic_feature = c(traffic_feature, median_traffic$median_traffic[md])
  }else
    #取最近的一个traffic值
    traffic_feature = c(traffic_feature, traffic$traffic[i[length(i)]])
}
predict_data$traffic_feature = traffic_feature


colnames(predict_data) = c("hashid","V1","Date","weekday","TimePiece","supply_feature","traffic_feature")
predict_data = predict_data[,c("hashid","V1","Date","TimePiece","weekday","supply_feature","traffic_feature")]

#生成预测点的 前一时间片 demand 值
demand_feature = c()
for(index in 1:nrow(predict_data)){
  pd = predict_data[index,]
  i = which(pd$hashid==test_demand$hashid & pd$Date==test_demand$date & pd$TimePiece>test_demand$TimePiece )
  if(length(i)==0){
    print(pd)
    #使用上一时刻的中位值
    demand_feature = c(demand_feature, 
                median_demand$median_demand[which(median_demand$hashid==pd$hashid & median_demand$TimePiece==(pd$TimePiece-1))])
  }else
    #取最近的一个demand值
    demand_feature = c(demand_feature, test_demand$test_demand[i[length(i)]])
}
predict_data$demand_feature = demand_feature

#weather feature
weather_feature_index = c()
for(index in 1:nrow(predict_data)){
  pd = predict_data[index,]
  windex = which(pd$Date==test_weather_data$date & (pd$TimePiece>test_weather_data$time | pd$TimePiece>(test_weather_data$time-11) ))
  if(length(windex)==0)
    print(pd)
  weather_feature_index = c(weather_feature_index, windex[length(windex)])
}
predict_data = cbind(predict_data,test_weather_data[weather_feature_index,c(2,3,4)])

# ======== 
predict_data$TimePiece = as.factor(predict_data$TimePiece)
predict_data$weekday = sapply(predict_data$weekday, function(x){
  if(x=="星期一")
    factor(1)
  else if(x=="星期二")
    factor(2)
  else if(x=="星期三")
    factor(3)
  else if(x=="星期四")
    factor(4)
  else if(x=="星期五")
    factor(5)
  else if(x=="星期六")
    factor(6)
  else
    factor(7)
})
predict_data$weather = as.integer(predict_data$weather)

predict_data$gap_feature = (predict_data$demand_feature - predict_data$supply_feature)




