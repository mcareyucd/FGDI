rescale_d <- function(data){
  
  r_data = ((1)*(data-min(data))/(max(data)-min(data))+0)
  
  return(r_data)
  
}  