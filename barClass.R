
source('predict.R')
initGroup<-function(n=100,stg,m,m_select){
  ##初始化一个关于100人El Farol Bar状态的 class
  initgroup<-list()
  class(initgroup)<-'BarGroup'
  ## 初始历史每周在Bar人数
  initgroup$num_history<-c(44,78,56,15,23,67,84,34,45,76,40,56,22,35)

  ## 初始化每个人所用有的策略组合
  initgroup$per_list<-list()
  for(i in c(1:n)){
    #set.seed(10*i)  ## 为了可以实验结果的可重复性
    initgroup$per_list[[i]]<-sample(c(1:m),m_select,replace=T)
  }
  ## 初始每个策略当前回测胜利个数
  initgroup$stg_win<-rep(0,m)
  ## 记录当前周数
  initgroup$n_week<-0
  ## 策略集合
  initgroup$stg<-stg
  ## 各个策略当前预测结果
  initgroup$pred_now<-sapply(initgroup$stg, do.call,list(initgroup$num_history))
  return(initgroup)
}


updateGroup<-function(initgroup){
  ## 更新周数
initgroup$n_week<-initgroup$n_week+1
 ## 当前所有策略预测情况
pred_now<-sapply(initgroup$stg, do.call,list(initgroup$num_history))
## 更新各个策略的回测情况
if(length(initgroup$num_history)<1){
  status_last<-0
}else{
  status_last<-ifelse(tail(initgroup$num_history,1)>60,0,1)
}
##各个策略判断情况
status_judge<-1-abs(status_last-initgroup$pred_now)
## 更新各个策略的回测胜利数
#print(status_judge)
initgroup$stg_win<-status_judge+initgroup$stg_win
#print(initgroup$stg_win)
 ## 计算当前每个人的策略选择（回测）和是否上Bar
sum_bar<-0
  for(i in seq(initgroup$per_list)){
    ## 当前所选策略回测胜利数
    selected_stg_win<-(initgroup$stg_win)[(initgroup$per_list)[[i]]]
    ## 最好策略index
    index<-which.max(selected_stg_win)
    ##index_stg<-initgroup$per_list[[i]][index]
    ## 计算去Bar人数
    sum_bar<-sum_bar+pred_now[initgroup$per_list[[i]][index]]
  } 
##更新历史去Bar 人数
initgroup$num_history<-c(initgroup$num_history,sum_bar)
## 更新当前各个策略的预测情况
initgroup$pred_now<-pred_now
return(initgroup)
}



## 模拟每天去Bar情况

simulatingBar<-function(stg,m_select=3,maxism_week=100,n=100,m=6){
  init_g<-initGroup(n=100,stg,m,m_select)
  while(init_g$n_week<=maxism_week){
    init_g<-updateGroup(init_g)
  }
  #print(init_g$num_history)
  return(init_g)
}
















