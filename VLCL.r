VLCL = function( C  =NULL,
                 dc =NULL,
                 dv =NULL,
                 lam=NULL){

  return(c(
    CF = C*(C * dc)    / (( (C * dc) * (1-lam)) +  ( (C * (1-dc))* lam)  ),
    CL = C*(C * (1-dc))/ (( (C * dc) * (1-lam)) +  ( (C * (1-dc))* lam)  ),
    VF = (1-dv)/ (( dv * lam    ) +  ( (1-dv)*  (1-lam))  ),
    VL = dv    / (( dv * lam    ) +  ( (1-dv)*  (1-lam))  )))

}
