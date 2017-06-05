library(RORC)

calulate_mape_lm <- fucntion(lm,df,response)
{
  mape <- mean(abs(predict(lm,df) - response)/abs(response),na.rm = T) *100
  return(mape)
}

calculate_plot_auc_f1_glmnet <- function(fit,target,newdata,s="lambda.min")
{
  predictions <- predict(fit,newx=newdata,s=s,type="response")
  pred <- prediction(predictions,target)
  
  perf_AUC <- performance(pred,"auc")
  AUC <- perf_AUC@y.values[[1]]
  perf_ROC <- performance(pred,"tpr","fpr")
  plot(perf_ROC,main="ROC plot")
  text(0.5,0.5,paste("AUC = ",format(AUC,digits=5, scientific=FALSE)))
  
  predictions_binary <- predict(fit,newx=newdata,s=s,type="class")
  cm <- confusionMatrix(predictions_binary,target)$table
  recall <- cm[2,2]/(cm[1,2]+cm[2,2])
  precision <- cm[2,2]/(cm[2,1]+cm[2,2])
  F1 <- 2*precision*recall/(precision+recall)
  print(cm)
  cat('Precision:',precision,'\n')
  cat('Recall:',recall,'\n')
  cat('F1:',F1,'\n')
  cat('AUC:',AUC,'\n')
  
}

calculate_plot_auc_f1_rf <- function(fit,target,newdata=NULL)
{
  if(!is.null(newdata))
  {
    predictions=predict(fit,newdata,type="prob")[,2]
  } else {
    predictions=predict(fit,type="prob")[,2]
    pred=ROCR::prediction(predictions,target)
    
    perf_AUC <- performance(pred,"auc")
    AUC <- perf_AUC@y.values[[1]]
    perf_ROC <- performance(pred,"tpr","fpr")
    plot(perf_ROC,main="ROC plot")
    text(0.5,0.5,paste("AUC = ",format(AUC,digits=5, scientific=FALSE)))
    
    if(is.null(newdata))
    {
      prediction_binary <- predict(fit,type="response")
    } else {
      prediction_binary <- predict(fit,newdata=newdata,type="response")
    }
    
    cm <- confusionMatrix(predictions_binary,target)$table
    recall <- cm[2,2]/(cm[1,2]+cm[2,2])
    precision <- cm[2,2]/(cm[2,1]+cm[2,2])
    F1 <- 2*precision*recall/(precision+recall)
    print(cm)
    cat('Precision:',precision,'\n')
    cat('Recall:',recall,'\n')
    cat('F1:',F1,'\n')
    cat('AUC:',AUC,'\n')
    
  }
}