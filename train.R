# hash map
hash_id = read.table("training_data/cluster_map/cluster_map", sep = "\t", header = FALSE, colClasses = "character")

#last three piece mean, 暂不可用
last3mean_supply = read.table("aver_supply.csv",sep = ",", header = TRUE)
last3mean_supply = sqldf("select h.V1 hashid,l.date,l.slot,l.supply last3mean from last3mean_supply l, hash_id h where l.districtId=h.V2")
last3mean_demand = read.table("aver_request.csv",sep = ",",header = TRUE)
last3mean_demand = sqldf("select h.V1 hashid,l.date,l.slot,l.request last3mean from last3mean_demand l, hash_id h where l.districtId=h.V2")

last3mean_gap = cbind(last3mean_demand[,c(1:3)], data.frame(last3mean_gap=(last3mean_demand$last3mean - last3mean_supply$last3mean)))
#===================       traffic
traffic = read.table("features/traffic.csv",sep = ",", header = TRUE, colClasses = c("character","character","integer","numeric"))
traffic = traffic[traffic$TimePiece > 43 & traffic$TimePiece < 143,]
median_traffic = aggregate(traffic ~ hashid + TimePiece, median, data = traffic)
colnames(median_traffic)[3] = "median_traffic"

#===================       poi
poi = read.table("features/poi/general_poi.txt", sep = ",", header = TRUE, colClasses = "character", comment.char = "^")
poi = poi[,c(1,2,3,ncol(poi))]

#==================      train-supply
supply = read.table("features/demand_supply_gap_v1/supply.txt", sep = ",",header = TRUE, colClasses = "character")
if(FALSE){
  #一次性将所有数据加入，训练好
  test_supply = read.table("features/demand_supply_gap_v1/test_supply.txt", sep = ",",header = TRUE, colClasses = "character")
  colnames(test_supply) = colnames(supply)
  supply = rbind(supply, test_supply)
}
supply$TimePiece = as.integer(supply$TimePiece)
supply$supply = as.integer(supply$supply)
supply = supply[supply$TimePiece > 43 & supply$TimePiece < 143,]
supply = supply[order(supply$hashid, supply$date, supply$TimePiece),]


median_supply = aggregate(supply ~ hashid + TimePiece, median, data = supply)
colnames(median_supply)[3] = "median_supply"

#==================      train-demand
demand = read.table("features/demand_supply_gap_v1/demand.txt", sep = ",",header = TRUE, colClasses = "character")
if(FALSE){
  #一次性将所有数据加入，训练好
  test_demand = read.table("features/demand_supply_gap_v1/test_demand.txt", sep = ",",header = TRUE, colClasses = "character")
  colnames(test_demand) = colnames(demand)
  demand = rbind(demand, test_demand)
}
demand$TimePiece = as.integer(demand$TimePiece)
demand$demand = as.integer(demand$demand)
demand = demand[demand$TimePiece > 43 & demand$TimePiece < 143,]
demand = demand[order(demand$hashid, demand$date, demand$TimePiece),]
#计算中各个区域+时间片 的中位值
median_demand = aggregate(demand ~ hashid + TimePiece, median, data = demand)
colnames(median_demand)[3] = "median_demand"



require(sqldf)
#加入traffic特征
#supply-traffic, 
train_data_supply = sqldf(paste0("select s.hashid, s.date, s.TimePiece, weekday, supply, t.traffic",
             " from supply s, traffic t where s.hashid=t.hashid and s.date=t.Date and s.TimePiece=t.TimePiece"))

#demand-traffic
train_data_demand = sqldf(paste0("select s.hashid, s.date, s.TimePiece, weekday, demand, t.traffic",
                          " from demand s, traffic t where s.hashid=t.hashid and s.date=t.Date and s.TimePiece=t.TimePiece"))

#加入weather特征
