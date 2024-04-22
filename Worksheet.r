#HOUSEKEEPING
{
rm(list= ls())
library(ggplot2)
library(gridExtra)
  # library(grid)
  # library(gtable)
  # library(scales)
}

### FUNCTIONS
{
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

model = function (
    lam = NULL,
    CL   = NULL,
    VL   = NULL,
    VF  = NULL,
    CF  = NULL,
    dem = NULL,
    pL.init  = 0.01,
    pF.init  = 0.01,
    n.gen = 100
) {

  # vars
  # lam = df$lam[i]
  # CL  = CL
  # VL  = VL
  # VF  = VF
  # CF  = CF
  # dem = df$dem[i]
  # pL.init  = NULL
  # pF.init  = NULL
  # n.gen = 100

  # variables
  pLs = NULL
  pFs = NULL
  ps = NULL
  pL = pL.init
  pF = pF.init
  k=0.08

  for ( i in 1:n.gen){
    delta = 0.7*exp(-k * i)

    # vote on hawkishness
    p =   ( ( pL *lam ) + # proportion of leaders playing hawk * leader sway * Num. leaders
              (pF * dem * (1-lam)) ) / # what followers want * how much they're allowed per follower * number of followers
      ( (  lam) + # total say that everyone has, split into leaders
          ( dem * (1-lam))) # and followers

    # save before any changes are made
    pLs = c(pLs,pL) ## preference of leaders
    pFs = c(pFs,pF) ## preference of followers
    ps  = c(ps ,p ) ## played strategy by group

    # leader outcome
    lWh = p*( ( VL - CL)/2 ) +  ((1-p) * VL) ## leader payoff if hawk
    lWd = p*0 + ( (1-p)* VL/2) ## leader payoff if dove

    # follower outcome
    fWh =  (p*((VF - CF)/2)) +  ((1-p) * VF) ## follower payoff if hawk
    fWd =  p*0 + ((1-p)* VF/2) ## follower payoff if dove

    pL = pL + sign( lWh - lWd) * delta  ## update leader preference
    pF = pF + sign( fWh - fWd) * delta ## update follower preference
    if ( pL > 1) {
      pL = 1
    }
    if ( pF > 1) {
      pF = 1
    }
    if ( pL < 0) {
      pL = 0
    }
    if ( pF < 0) {
      pF = 0
    }
  }
  len = length(pLs)
  vec = c(  pL = pLs[(len)],
            pF = pFs[(len)],
            p  = ps[(len)])
  names (  vec) = c ( "pL", "pF","p")
  vec
  return ( vec )
}

plotfunct = function( Var = "dem",
                      Var2 = "lam",
                      lam = 0.30,
                      C =  2,
                      V = 1,
                      dv= 0.50,
                      dc= 0.50,
                      dem = 0.5,
                      pL.init=  0.01,
                      pF.init=  0.01,
                      n.gen = 300,
                      onOff = "No",
                      sq.len = 20,
                      BASESIZE = 12){
  # Var = "dem"
  # Var2 = "lam"
  # lam = 0.30
  # C =  2
  # V = 1
  # dv= 0.5
  # dc= 0.5
  # dem = 0.5
  # pL.init=  0.01
  # pF.init=  0.01
  # n.gen = 300
  # onOff = "No"
  # sq.len = 20
  #  BASESIZE = 12
  li = list(
    lam = seq (0.001,0.999,length.out=sq.len),
    C = seq(1,10,length.out = sq.len),
    V = seq(1,10,length.out = sq.len),
    dv= seq(0.001,1.5  ,length.out=sq.len),
    dc= seq(0.001,1.5,length.out=sq.len),
    dem=seq(0.001,1,length.out=sq.len),
    pL.init =seq(0.01,0.99,length.out=sq.len),
    pF.init =seq(0.01,0.99,length.out=sq.len)
  )
  {
    whi= which ( names( li) == Var)
    whi2= which ( names( li) == Var2 )
    var  = li[[whi]]
    var2 = li[[whi2]]
    eg = expand.grid(var, var2)
    mat = as.data.frame(matrix( NA , nrow= nrow(eg),  ncol = length(li)  ))
    names( mat ) = names(li)
    mat[,1] = lam
    mat[,2] = C
    mat[,3] = V
    mat[,4] = dv
    mat[,5] = dc
    mat[,6] = dem
    mat[,7] = pL.init
    mat[,8] = pF.init
    mat[,whi] = eg$Var1
    mat[,whi2]= eg$Var2

    input2 = list (
      lam = lam,
      C = C,
      V = V,
      dv = dv,
      dc = dc,
      dem = dem,
      pL.init = pL.init,
      pF.init = pF.init)
    vec = abs( mat[,whi2] - as.numeric( input2[whi2]))
    vec = which(vec == min(vec))
  }


  {
    pLend  = rep ( NA, nrow(eg ) )
    pFend  = rep ( NA, nrow(eg ) )
    pend   = rep ( NA, nrow(eg ) )
    pLpref = rep ( NA, nrow(eg ) )
    pFpref = rep ( NA, nrow(eg ) )
    pWOpref= rep ( NA, nrow(eg ) )
    i=1
  }


  for ( i in 1:nrow(mat)){
    #i=1

    lf = VLCL(C=mat$C[i],
              dc=  mat$dc[i],
              dv=  mat$dv[i],
              lam= mat$lam[i])

    VL = lf [ "VL"]#leader benefit
    VF = lf [ "VF"]#follower benefit
    CL = lf [ "CL"]# leader cost
    CF = lf [ "CF"]# follower cost
    grpC = c(CL,CF)
    grpV = c(VL,VF)
    opt = grpV[1] / grpC[1]
    opt2 = grpV[2] / grpC[2]
    opt = ifelse ( opt > 1, 1,opt)
    opt2 = ifelse ( opt2 >1 , 1 , opt2 )


    out = model(lam       = mat$lam[i]      ,
                CL          =CL  ,
                VL          =VL  ,
                CF         = CF ,
                VF         = VF ,
                dem        = mat$dem[i]     ,
                pL.init    = mat$pL.init[i] ,
                pF.init    = mat$pF.init[i] ,
                n.gen      = 300      ,
                leader.sway = 1               ,
                pref.can.change = T            )


    pLend  [i] = out["pL"]
    pFend  [i] = out["pF"]
    pend   [i] = out["p"]
    pLpref [i] = opt
    pFpref [i] = opt2


  }

  ggdata = data.frame( prop.hawk = c(pLend[vec],pFend[vec],pend[vec],pLpref[vec],pFpref[vec],pWOpref[vec]   ),
                       type = rep( c("Leaders","Followers","Population","Leader Preference","Follower Preference","No Extreme Votes" ),each=length(pend[vec])),
                       xvar =      var)




  # g2
  df = data.frame ( mat[,c(whi,whi2)], pend)
  names( df ) = c("var1" , "var2" , "p")
  df$pFO = pFpref
  df$pLO = pLpref
  maT = matrix(c("both optimum", "follower optimum" , "leader optimum", "transition"),2,2)
  df$Outcome =
    apply(df[,c("pFO","pLO","p")],1,function(x){
      #  x=df[1,c("pFO","pLO","p")]
      x1 = ifelse ( abs(x[3]-x[2]) < 0.01,1,2)
      x2 = ifelse ( abs(x[3]-x[1]) < 0.01,1,2)
      maT[x1,x2]

    })

  df$Outcomen = as.numeric(as.factor(df$Outcome))

  return (   df)
}
}

### FIGURE 1.
{
  par(mfrow= c(1,1))
VLCLvar = VLCL ( C = 2, dv =  0.55, dc =  0.55, lam =  0.3)
dem = seq( 0 , 1, 0.01)
outs = NULL
for  ( i in 1:length ( dem)){
  out = model(lam       = 0.3     ,
              CL          =VLCLvar [ "CL" ],
              VL          =VLCLvar [ "VL" ] ,
              CF         = VLCLvar [ "CF" ],
              VF         = VLCLvar [ "VF" ],
              dem        = dem[i]     ,
              pL.init    = 0.01,
              pF.init    = 0.01,
              n.gen      = 300    )
  outs = rbind ( outs , out)
}
plot ( outs[,1]~c(1:nrow(outs)/nrow(outs)), col  = 1, ylim = c(0,1) , type = "l",ylab = "P", xlab = "Omega")
abline(h = VLCLvar["VL"]/VLCLvar["CL"], lty=2)
abline(h = VLCLvar["VF"]/VLCLvar["CF"], lty=2)
lines( outs[,2]~c(1:nrow(outs)/nrow(outs)), col  = 2)
lines( outs[,3]~c(1:nrow(outs)/nrow(outs)), col  = 3)
}

# Figure 2
{

  run.code  = F
  if(run.code){

    ex = expand.grid(
      dv = c(0.55),
      dc = c(0.55)
    )
    li  = list()
    for ( i in 1:nrow(ex)){
      li[[i]] = plotfunct( dv = ex$dv[i],dc= ex$dc[i], C=2,sq.len = 500 )
    }


    save ( li, file = file.path ( getwd(), "plotdata" , "p02.1.rda"))
    save( ex , file = file.path ( getwd(), "plotdata" , "p02.1Extra.rda"))
  } else {
    load( file.path ( getwd(), "plotdata" , "p02.1.rda"))
    load( file.path ( getwd(), "plotdata" , "p02.1Extra.rda"))
  }



  ggli = NULL
  for ( i in 1:length(li)){
    #i=1
    df = li[[i]]
    df$dc = ex$dc[i]
    df$dv = ex$dv[i]
    ggli = rbind( ggli , df)

  }


  p <- ggplot( ggli , aes ( var1 , var2 ,fill=Outcome))+
    geom_tile () +
    #facet_grid(rows = vars(dv) ,cols= vars(dc))+
    #scale_y_continuous(breaks = scales::pretty_breaks(n = 2),sec.axis = sec_axis(~ . , name = "dv", breaks = NULL, labels = NULL))+
    #scale_x_continuous(breaks = scales::pretty_breaks(n = 2),sec.axis = sec_axis(~ . , name = "dc", breaks = NULL, labels = NULL))+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
    # scale_y_continuous() +
    # scale_y_continuous() +
    # scale_x_continuous()+
    scale_fill_manual( values = c( "follower optimum" = "darkorange", "leader optimum" = "steelblue3", "transition" = "darkgreen"  ))+
    theme_classic(base_size = 22)+
    labs ( x = paste("Shared decision parameter") , y = "Proportion of leaders ")

}
p

## Figure S1
{
  mat1 = expand.grid( lam = c(0.3,0.7), N = c(5,10,20,30,50,100), dc = 0.5, dv = seq(0,1,0.01))
  mat2 = expand.grid( lam = c(0.3,0.7), N = c(5,10,20,30,50,100), dc = seq(0,1,0.01), dv = 0.5)
  VLs = NULL
  VFs = NULL
  for ( i in 1:nrow(mat1)){
    #i=41
    v = VLCL(C = 2 , dc = mat1$dc[i], dv = mat1$dv [i] , lam = mat1$lam[i])
    v = v/mat1$N[i]
    VLs = c ( VLs , v["VL"])
    VFs = c ( VFs , v["VF"])
    print( length(VLs))
    print( length(VFs))
  }
  mat1 = rbind( mat1, mat1)
  mat1$V = c( VLs, VFs)
  mat1$Class  = rep(c("leader" , "follower"),each = 1212)
  mat1$lam = as.factor(mat1$lam)
  g1 = ggplot ( mat1 , aes ( y=V , x=dv))+
    geom_line(mapping = aes(linetype = lam, color = Class),linewidth = 1.5)+
    facet_wrap(~N, scales="free_y")+
    scale_x_continuous(sec.axis = sec_axis(~ . , name = "N", breaks = NULL, labels = NULL))+
    theme_classic( base_size = 17)
  CLs = NULL
  CFs = NULL
  for ( i in 1:nrow(mat2)){
    #i=1
    C = VLCL(C = 2 , dc = mat2$dc[i], dv = mat2$dv [i] , lam = mat2$lam[i])
    C = C/mat2$N[i]
    CLs = c ( CLs , C["CL"])
    CFs = c ( CFs , C["CF"])
    print( length(CLs))
    print( length(CFs))
  }
  mat2 = rbind( mat2, mat2)
  mat2$C = c( CLs, CFs)
  mat2$Class  = rep(c("leader" , "follower"),each = 1212)
  mat2$lam = as.factor(mat2$lam)
  g2 = ggplot ( mat2 , aes ( y=C , x=dc))+
    geom_line(mapping = aes(linetype = lam, color = Class),linewidth=1.5)+
    facet_wrap(~N, scales="free_y")+
    labs(y = "C = 2V")+
    scale_x_continuous(sec.axis = sec_axis(~ . , name = "N", breaks = NULL, labels = NULL))+
    theme_classic( base_size = 17)

}
grid.arrange(grobs = list( g1,g2),ncol=1)


# Figure S2
{
dcs = seq( 0.0001, 0.9999 ,length.out=80)
dvs = seq( 0.0001, 0.9999 ,length.out=80)

df  = expand.grid( dcs= dcs,
                   dvs= dvs)
i=103
df$Outcome = NA
for ( i in 1: nrow(df)){
  result  = VLCL ( C= 2, dc =  df$dcs[i] , dv= df$dvs[i] , lam  = 0.1)
  vlcl = result["VL"] / result["CL"]
  vfcf = result["VF"] / result["CF"]
  vlcl = round ( vlcl , 4)
  vfcf = round ( vfcf , 4)
  if ( vlcl > vfcf) {
    var = "Leader"
  }
  if ( vlcl < vfcf) {
    var = "Follower"
  }
  if ( vlcl == vfcf) {
    var = "Both equal"
  }
  df$Outcome[i] = var
}
ggplot ( df , aes  (dcs, dvs , fill = Outcome))+
  geom_tile()+
  theme_classic(base_size = 12)+
  labs ( x = substitute(paste(italic('dc'))), y = substitute(paste(italic('dv')))) +
  scale_fill_manual( values = c( "Follower" = "darkorange", "Leader" = "steelblue1", "Both equal" = "darkgreen"  ))+
  xlim(0,1)+
  ylim(0,1)
}
# Figure S3
{
    par(mfrow= c(1,1))
    VLCLvar = VLCL ( C = 2, dv =  0.45, dc =  0.45, lam =  0.3)
    dem = seq( 0 , 1, 0.01)
    outs = NULL
    for  ( i in 1:length ( dem)){
      out = model(lam       = 0.3     ,
                  CL          =VLCLvar [ "CL" ],
                  VL          =VLCLvar [ "VL" ] ,
                  CF         = VLCLvar [ "CF" ],
                  VF         = VLCLvar [ "VF" ],
                  dem        = dem[i]     ,
                  pL.init    = 0.01,
                  pF.init    = 0.01,
                  n.gen      = 300    )
      outs = rbind ( outs , out)
    }
    plot ( outs[,1]~c(1:nrow(outs)/nrow(outs)), col  = 1, ylim = c(0,1) , type = "l",ylab = "P", xlab = "Omega")
    abline(h = VLCLvar["VL"]/VLCLvar["CL"], lty=2)
    abline(h = VLCLvar["VF"]/VLCLvar["CF"], lty=2)
    lines( outs[,2]~c(1:nrow(outs)/nrow(outs)), col  = 2)
    lines( outs[,3]~c(1:nrow(outs)/nrow(outs)), col  = 3)
  }

# Figure S4
{
df = expand.grid( lam = seq(0.01,1,0.02),  dc = seq(0.1,0.9,0.2), dv = seq(0.1,0.9,0.2), dem = seq(0,1,0.02),C=c(2,5))
run.script=F
if ( run.script){
  i = 1
  mods = NULL
  df$pL = NA
  df$pF = NA
  df$p = NA
  df$lOpt = NA
  df$fOpt = NA
  for ( i in 1:nrow(df)){
    vl = VLCL(C = df$C[i], dc=df$dc[i], dv=df$dv[i], lam = df$lam[i])
    CL  = vl["CL"]
    VL  = vl["VL"]
    VF  = vl["VF"]
    CF  = vl["CF"]
    mod = model (  lam = df$lam[i],
                   CL  = CL,
                   VL  = VL,
                   VF  = VF,
                   CF  = CF,
                   dem = df$dem[i])
    df$pL[i] = mod["pL"]
    df$pF[i] = mod["pF"]
    df$p[i]  = mod["p"]
    df$lOpt[i]  = VL/CL
    df$fOpt[i]  = VF/CF
    print(paste ( i , "/" , nrow(df) ))
  }

  save ( df , file = file.path ( getwd(), "plotdata", "p11.rda"))
} else {
  load ( file.path ( getwd(),"plotdata", "p11.rda"))
}
df$outcome=  NA
for ( i in 1:nrow(df)){
  if ( abs( df$lOpt[i] - df$p[i] ) < 0.01) {
    PL = T
  } else {
    PL = F
  }
  if ( abs( df$fOpt[i] - df$p[i] ) < 0.01) {    PF = T
  } else {
    PF = F
  }
  if ( PF == T & PL == T){
    df$outcome[i] = "both optimum"
  }
  if ( PF == T & PL == F){
    df$outcome[i] = "follower optimum"
  }
  if ( PF == F & PL == T){
    df$outcome[i] = "leader optimum"
  }
  if ( PF == F & PL == F){
    df$outcome[i] = "compromise"
  }
  print(i)
}


df2 = df [  df$C == 5,]
df3 = df [  df$C == 2,]
g1 = ggplot ( df2 , aes (y = lam , x = dem , fill = outcome))+
  geom_tile()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 2),sec.axis = sec_axis(~ . , name = "dc", breaks = NULL, labels = NULL))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 2),sec.axis = sec_axis(~ . , name = "dv", breaks = NULL, labels = NULL))+
  scale_fill_manual( values = c( "follower optimum" = "darkorange", "leader optimum" = "steelblue3", "compromise" = "darkgreen" , "both optimum" = "salmon" ))+
  facet_grid(dv  ~ dc)+
  theme_classic(base_size=10)
g2 = ggplot ( df3 , aes (y = lam , x = dem , fill = outcome))+
  geom_tile()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 2),sec.axis = sec_axis(~ . , name = "dc", breaks = NULL, labels = NULL))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 2),sec.axis = sec_axis(~ . , name = "dv", breaks = NULL, labels = NULL))+
  scale_fill_manual( values = c( "follower optimum" = "darkorange", "leader optimum" = "steelblue3", "compromise" = "darkgreen" , "both optimum" = "salmon" ))+
  facet_grid(dv  ~ dc)+
  theme_classic(base_size=10)
}
grid.arrange(grobs = list ( g2, g1) , ncol = 2)


# Figure S5
{

load( file.path ( getwd( ) ,"plotdata" , "p12.rda"))
df1 = savedf [[1]]
df2 = savedf [[2]]
par ( mfrow = c(2,1))
plot ( df1[,1], lwd= 2, type = "l" , ylim  =c(0,1), col = "darkorange3" ,ylab = "Hawk-playing probability", xlab = "Rounds")
lines ( df1[,2],lwd= 2,  ylim  =c(0,1), col = "steelblue4" )
lines ( df1[,3],lwd= 2,  ylim  =c(0,1), col = "darkgreen" )
plot (  df2[,1], lwd = 2 , type = "l" , ylim  =c(0,1), col = "darkorange3" ,ylab = "Hawk-playing probability", xlab = "Rounds")
lines ( df2[,2], lwd = 2 , ylim  =c(0,1), col = "steelblue4" )
lines ( df2[,3], lwd = 2 , ylim  =c(0,1), col = "darkgreen" )
legend( "topleft", legend = c("followers", "leaders", "population"), lty = 1, lwd = 2, col = c( "darkorange3","steelblue4","darkgreen" ))
}
