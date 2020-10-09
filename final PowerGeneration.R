PowerGeneration = function(X,B,M)  #X=power ,M= money,B = use
#param = {M}
{
  Y = matrix (data = (M/760000)*210, nrow=24,ncol=365) ##儲電matrix
  for (t in 1:24)   ##365*24的資料
  {
    for(g in 1:365)
     {
       if (X[t,g]-Y[t,g]<0) Y[t,g]=X[t,g]  ##條件式，儲最大發電
     }
  }
  Y=Y*0.89  ##轉換率
  re = matrix (data = 0,nrow=24, ncol = 365)
 
  re[1,1] = re[1,1] +Y[1,1] - B[1,1]
  for (j in 2:24)   ##第一天的24小時總剩餘電量
    {   
      re[j,1]=re[j,1]+Y[j,1] - B[j,1]
      re[j,1]= re[j,1]+re[j-1,1]
    }

  for (i in 2:365)   ##第二天之後的每天24小時剩餘電量累加
  {
    re[1,i]=re[1,i]+Y[1,i]-B[1,i]
    
    
    re[1,i]= re[24,i-1] + re[1,i]

    
    for (j in 2:24)
    {
     
      re[j,i]=re[j,i]+Y[j,i]-B[j,i]

      re[j,i]= re[j,i]+re[j-1,i]
    }
    if (re[24,i] < 0)  ##條件式，負的電用其他能源替代

      re[24,i] = 0;
  }
  re
}
##############

Sum = function(Any) #Anything want to sum
  
{
  re = matrix (data = 0,nrow = 24,ncol = 365)
  
  for(j in 1:365){
    re[1,j] = Any[1,j]   
    for(i in 2:24){
      re[i,j] = Any[i,j]
      re[i,j] = re[i,j] + re[i-1,j]
    }
  }
  re 
}
##############

Times = function(X,B,M)#X=power ,M= money,B = use
  #param = {M}
  #re = [M,times]
{
  re = matrix (data =0 , nrow = length(M),ncol = 4)
  re[,1] = M
  for(a in 1:length(M)){
    A = PowerGeneration(X,B,M[a])
    for (i in 1:365){
      if ( A[24,i] == 0 )
        re[a,2] = re[a,2]+1
      if ( i > 150 && i <= 270 ){
        if( A [24,i] == 0 )
          re[a,4] = re [a,4] + 1
      }
      if ( (i > 1 && i <= 90) || (i > 330 && i <= 360)){
        if( A[24,i] == 0 )
          re[a,3] = re[a,3] + 1 
      }
    }
   
  }
  
  re
}
###############
PowerLong = function(X,B,M) #X=power,B = use M = money
#re[nrow = 8760,ncol=2,>one hour ,sum]
  {
  L = length((X[,1]))
  re = matrix(data = 0,nrow = L,ncol = 2)
  Y = matrix (data = ((M/760000)*210),nrow = 8760,ncol = 2)
  for (t in 1:L)
  {
      if (X[t,1]-Y[t,1]<0) Y[t,1]=X[t,1]
    
    
  }
  Y=Y*0.89
  
  re[1,1] = re[1,1] +Y[1,1] - B[1,1]
  re[1,2] = re[1,2] +Y[1,1] -B[1,1]
  
  for (j in 2:L)
  {
    
    re[j,1]=re[j,1]+Y[j,1] - B[j,1]
    re[j,2]=re[j,2]+Y[j,1] -B[j,1]
    re[j,1]= re[j,1]+re[j-1,1]
    if(re[j,1] < 0 ) re[j,1] = 0
  }
  re
}
#################