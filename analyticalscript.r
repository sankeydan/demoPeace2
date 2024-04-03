rm(list=ls())
model = function (
    C   = NULL,
    dc  = NULL,
    dv  = NULL,
    eps = NULL,
    num.increments = 100){

  # C = 2
  # dc = 0.7
  # dv = 0.6
  # eps = 0.6
  # num.increments = 100


  CF = C*(C * dc)    / (( (C * dc) * (1-eps)) +  ( (C * (1-dc))* eps)  )
  CL = C*(C * (1-dc))/ (( (C * dc) * (1-eps)) +  ( (C * (1-dc))* eps)  )
  VF = (1-dv)/ (( dv * eps    ) +  ( (1-dv)*  (1-eps))  )
  VL = dv    / (( dv * eps    ) +  ( (1-dv)*  (1-eps))  )
  VLtil= VL/CL
  VFtil= VF/CF

  dem = seq(0,1,length.out = num.increments)
  leadercontrol =  (eps * (1-VLtil)) /  ((1-eps)*VLtil)
  followercontrol = (eps * (1-VFtil)) / ((1-eps)*VFtil)
  mat=NULL
  for ( i in 1:length( dem)){
    #i=1
    oga = dem[i]


    ## PL
    if ( oga <
         (eps * (1-VLtil)) /  # LEADER POWER -  proportion leaders and difference between 1 and VLtil
         ((1-eps)*VLtil)  # FOLLOWER POWER - proportion followers times the difference between 0 and VLtil
    ){
      PL = VLtil *  ( 1 + (((1-eps)/eps)*oga) )
    } else {
      PL = 1
    }
    ## PF
    if ( oga <
         (eps * (1-VFtil)) /  # LEADER POWER -  proportion leaders and difference between 1 and VFtil
         ((1-eps)*VFtil)  # FOLLOWER POWER - proportion followers times the difference between 0 and VFtil
    ){
      PF = 0
    } else {
      PF = VFtil - ((eps * (1-VFtil))/((1-eps)*oga))
    }
    ## P
    if ( oga < leadercontrol ){
      P = VLtil
    }
    if  ( leadercontrol <= oga & oga <= followercontrol){
      P = eps /  ( eps + oga * (1- eps) )
    }
    if ( followercontrol < oga){
      P = VFtil
    }
    mat = rbind ( mat , c ( PL= PL,PF = PF,P= P,VFtil=VFtil,VLtil=VLtil))
  }
  return(list ( mat,dem))
}
mat = model(C=2, dc=0.55, dv=0.55, eps=0.3)
dem = mat[[2]]
mat = mat[[1]]
plot ( mat [,1]~dem, col = 1 , ylim = c(0,1), type = "l",ylab="P")
for ( i in 2:ncol(mat)){
 lines( mat [,i]~dem, col = i , ylim = c(0,1), type = "l")
}
