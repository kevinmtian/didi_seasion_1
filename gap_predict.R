source("train.R")
source("predict.R")

train_data_gap = cbind(train_data_demand[,c(1,2,3,4,6)],data.frame(gap=(train_data_demand$demand - train_data_supply$supply)))
#weather 
#tmp = sqldf("select * from train_weather_data w, train_data_gap t where ")
#combine last3mean
#tmp = sqldf("select * from last3mean_gap l, train_data_gap p where l.hashid=p.hashid and l.date=p.date and l.slot=p.TimePiece")
#tmp = tmp[,c(4:ncol(tmp))]


hashids = unique(train_data_gap$hashid)
for(h_i in 1:length(hashids)){
  #对没一个区域逐个训练-预测
  train_feature = train_data_gap[train_data_gap$hashid==hashids[h_i],]
  #train_feature = train_data_gap[train_data_gap$hashid=="d4ec2125aff74eded207d2d915ef682f",]
#==================  特征预处理开始 ======================  
  gap_valu = c(0)
  traffic_value = c(0)
  weather_feature_index = c(1)
  delete_index = c(1)
  for(i in 2:nrow(train_feature)){
    tf = train_feature[i,]
    gv = train_feature$TimePiece[i] - train_feature$TimePiece[i-1]
    if(gv>0){
      #当前时间片的gap特征值， 为上一时间片的gap值
      gap_valu = c(gap_valu, train_feature$gap[i-1])
      #当前时间片的traffic特征值， 为上一时间片的traffic值
      traffic_value = c(traffic_value, train_feature$traffic[i-1])
      #天气特征
      windex = which(tf$date==train_weather_data$date & tf$TimePiece>train_weather_data$time)
      if(length(windex)==0)
        print(tf)
      weather_feature_index = c(weather_feature_index, windex[length(windex)])
    }else{
      #每天的起始时刻时间片是无效的，删除之
      gap_valu = c(gap_valu, 0)
      traffic_value = c(traffic_value, 0)
      weather_feature_index = c(weather_feature_index, 1)
      delete_index = c(delete_index, i)
    }
  }
  train_feature = cbind(train_feature, train_weather_data[weather_feature_index,c(2,3,4)])
  train_feature$gap_feature = gap_valu
  train_feature$traffic_feature = traffic_value
  train_feature = train_feature[-delete_index,]
  train_feature = train_feature[,c("TimePiece","weekday","gap_feature","traffic_feature",
                                   "weather","temperature","PM","gap")]
  train_feature$weekday = sapply(train_feature$weekday, function(x){
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
  train_feature$TimePiece = as.character(train_feature$TimePiece)
  train_feature$weather = as.integer(train_feature$weather)
#==================  特征预处理结束 ======================  
  
  fit = rpart(gap ~ TimePiece + weekday + traffic_feature + gap_feature + weather + temperature + PM, data = train_feature)
  tmp = predict_data[predict_data$hashid==hashids[h_i] ,]
  #tmp = predict_data[predict_data$hashid=="d4ec2125aff74eded207d2d915ef682f",]
  p_res = predict(fit, tmp[,c("TimePiece","weekday","gap_feature","traffic_feature","weather","temperature","PM")])
  if(is.null(predict_res))
    predict_res = cbind(tmp[,c(1,2,4)],data.frame(pre=p_res))
  else
    predict_res = rbind(predict_res, cbind(tmp[,c(1,2,4)],data.frame(pre=p_res)))
}


#====== 生成提交结果 =======
tmp = predict_res
tmp$pre = floor(tmp$pre)
tmp = sqldf("select h.V2 hashid, p.V1 time, TimePiece, pre from hash_id h, tmp p where h.V1=p.hashid")
write.table(tmp[,c(1,2,4)],file = "res/submit_res_v2.csv",quote = FALSE,sep = ",", row.names = FALSE, col.names = FALSE)




