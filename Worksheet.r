#HOUSEKEEPING
{
  rm(list= ls())
  library(ggplot2)
  library(gridExtra)
  library(dplyr)
  library(viridisLite)
  library(extrafont)
}

diFF=0.04
ViR = viridis(4,begin=0 + diFF , end = 1-diFF)
plot ( c(1:4),c(1:4), col = ViR,pch=19)


.codeOcean = F # please mark as True if running script within Code Ocean
# This code is here to reproduce the plots using the model.

### FUNCTIONS

# 1. VLCL, function to calculate the unequal shares of the costs and benefits of conflict
# 2. simulation model, where proportion of hawk is updated after each generation
# 3. analytical model, which provides the same results instantaneously
# 4. plot function. Helps plot the model over 2 variables. As seen in Fig. 2, and a couple of supplemental plots


{ # Function code - run as chunk

  # 1. VLCL, unequal shares function
  VLCL = function( C  =NULL,
                   dc =NULL,
                   dv =NULL,
                   eps=NULL){
    return(c(
      CF = C*(C * dc)    / (( (C * dc) * (1-eps)) +  ( (C * (1-dc))* eps)  ),
      CL = C*(C * (1-dc))/ (( (C * dc) * (1-eps)) +  ( (C * (1-dc))* eps)  ),
      VF = (1-dv)/ (( dv * eps    ) +  ( (1-dv)*  (1-eps))  ),
      VL = dv    / (( dv * eps    ) +  ( (1-dv)*  (1-eps))  )))
  }

  # 2. simulation model
  simmodel = function (
    eps = NULL,
    CL   = NULL,
    VL   = NULL,
    VF  = NULL,
    CF  = NULL,
    dem = NULL,
    pL.init  = 0.01,
    pF.init  = 0.01,
    n.gen = 100,
    method = "optimise",
    k = 0.08,
    k2 = 0.7,
    d = 0.01
  ) {

    # variables
    pLs = NULL
    pFs = NULL
    ps = NULL
    pL = pL.init
    pF = pF.init

    # loop by generation
    for ( i in 1:n.gen){
      if ( method == "optimise"){
        delta = k2*exp(-k * i)
      } else{
        delta = d
      }


      # vote on hawkishness
      p =   ( ( pL *eps ) + # proportion of leaders playing hawk * leader sway * Num. leaders
                (pF * dem * (1-eps)) ) / # what followers want * how much they're allowed per follower * number of followers
        ( (  eps) + # total say that everyone has, split into leaders
            ( dem * (1-eps))) # and followers

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
    mat = cbind(  pL = pLs,
                  pF = pFs,
                  p  = ps)
    names (  mat) = c ( "PL", "PF","P")
    return ( mat )
  }

  #### 3. ANALYTICAL MODEL

  anamodel = function (
    CL   = NULL,
    VL   = NULL,
    VF  = NULL,
    CF  = NULL,
    eps = NULL,
    dem=NULL){
    oga = dem

    # see manuscript for definitions of VLtil, VL, CL, etc.
    VLtil= VL/CL
    VFtil= VF/CF
    VLtil= ifelse ( VLtil>1,1,VLtil)
    VFtil= ifelse ( VFtil>1,1,VFtil)
    if ( VLtil >= VFtil){
      leadercontrol =  (eps * (1-VLtil)) /  ((1-eps)*VLtil) # see section, derivation of the model solution
      followercontrol = (eps * (1-VFtil)) / ((1-eps)*VFtil)
      # PL
      if ( oga < leadercontrol){
        PL = VLtil- (((1-eps)*oga)/eps ) * (0 - VLtil)
      } else {
        PL = 1
      }
      ## PF
      if ( oga < followercontrol  ){
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
    }
    if ( VFtil > VLtil){
      leadercontrol =  eps*VLtil/((1-eps)*(1-VLtil)) # see section, derivation of the model solution
      followercontrol = eps*VFtil/((1-eps)*(1-VFtil))
      ## Model solution.
      # PL
      if ( oga < leadercontrol){
        PL = VLtil- (((1-eps)*oga)/eps ) * (1 - VLtil)
      } else {
        PL = 0
      }
      ## PF
      if ( oga < followercontrol  ){
        PF = 1
      } else {
        PF = VFtil - ((eps * (0-VFtil))/((1-eps)*oga))
      }
      ## P
      if ( oga < leadercontrol ){
        P = VLtil
      }
      if  ( leadercontrol <= oga & oga <= followercontrol){
        P = 1-(eps /  ( eps + oga * (1- eps) ))
      }
      if ( followercontrol < oga){
        P = VFtil
      }
    }
    Cret = c ( PL,PF,P,VFtil,VLtil)
    names(Cret) = c('PL','PF','P','VFtil','VLtil')
    return(   Cret )
  }

  ##### 4. plot function, Just a useful way of running the model over 2 variables.
  plotfunct = function( Var = "dem",
                        Var2 = "eps",
                        eps = 0.30,
                        C =  2,
                        V = 1,
                        dv= 0.50,
                        dc= 0.50,
                        dem = 0.5,
                        pL.init=  0.01,
                        pF.init=  0.01,
                        n.gen = 300,
                        onOff = "No",
                        sq.len = 200,
                        BASESIZE = 12){

    #  BASESIZE = 12
    li = list(
      eps = seq (0.001,0.999,length.out=sq.len),
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
      mat[,1] = eps
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
        eps = eps,
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
                eps= mat$eps[i])
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
      out = anamodel(eps       = mat$eps[i]      ,
                     CL          =CL  ,
                     VL          =VL  ,
                     CF         = CF ,
                     VF         = VF ,
                     dem        = mat$dem[i]    )
      pLend  [i] = out["PL"]
      pFend  [i] = out["PF"]
      pend   [i] = out["P"]
      pLpref [i] = opt
      pFpref [i] = opt2
      if(  i %in% round(seq(1,sq.len^2,length.out=100))){
        print(paste ( i, "/" , sq.len^2))
      }
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

###############################
### FIGURE 1.
###############################
{
  # Unequal shares function for chosen parameters
  VLCLvar = VLCL ( C = 2, dv =  0.55, dc =  0.55, eps =  0.3) # parameter values as specified in Fig. 1

  # run the model over a broad spectrum of democracy scores
  dem = seq( 0 , 1, 0.01)
  outs = NULL
  for  ( i in 1:length ( dem)){
    out = anamodel(eps       = 0.3     ,
                   CL          =VLCLvar [ "CL" ],
                   VL          =VLCLvar [ "VL" ] ,
                   CF         = VLCLvar [ "CF" ],
                   VF         = VLCLvar [ "VF" ],
                   dem        = dem[i]
    )
    outs = rbind ( outs , out)
  }

  # pdf or code ocrean
  if ( .codeOcean){
    png('../results/figure1.png')
  } else {
    pdf( file.path  (getwd() , "plots", "figure1.pdf"),width = 5.11811, height = 6.28346)
  }

  # Create the ggplot
  df = data.frame ( p = c(outs[,"P"],
                          outs[,"PL"],
                          outs[,"PF"]),
                    name= c( rep ("P",nrow(outs)),
                             rep ("PL",nrow(outs)),
                             rep ("PF",nrow(outs))),
                    dem = rep(dem,3))
  df$name =factor(df$name, levels = c("P","PF","PL"))
  p = ggplot(df, aes(x = dem, y = p, group = name, color =  name )) +
    geom_line(size=1) +
    scale_color_manual(values = c( "P" = ViR[3], "PF" = ViR[4],"PL" = ViR[1])) + # Replace with actual name-color pairs
    geom_hline(yintercept = VLCLvar["VL"] / VLCLvar["CL"], linetype = "dashed") +
    geom_hline(yintercept = VLCLvar["VF"] / VLCLvar["CF"], linetype = "dashed") +
    labs(x = "Omega", y = "P") +
    theme_minimal()+
    theme(text = element_text(family = "sans"))
  print(p)
  dev.off()
}


####################################
# Figure 2. Plotting proportion of leaders over democracy
####################################

{
  # run the 2Dimensional plotting function
  df = plotfunct( dv = 0.55,dc= 0.55, C=2,sq.len = 700 ) # parameter values from Fig. 2 in manuscript
  df$dc = 0.55
  df$dv = 0.55
  df$outcome = df$Outcome
  df$outcome[df$Outcome == "follower optimum"] = "follower control"
  df$outcome[df$Outcome == "leader optimum"] = "leader control"


  p <- ggplot(df, aes(var1, var2, fill = outcome)) +
    geom_tile() +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_fill_manual(name=NULL,values = c("follower control" = ViR[4], "leader control" = ViR[1], "transition" = ViR[3])) +
    theme_classic(base_size = 12) +
    labs(x = paste("shared decision parameter", "\u03A9"), # \u03A9 is the Unicode for omega
         y = paste("proportion of leaders", "\u03B5")) +  # \u03B5 is the Unicode for epsilon
    theme(strip.text.x = element_blank(),
          strip.background = element_rect(colour = "white", fill = "white"),
          legend.position = c(.3, .7),
          text = element_text(family = "sans"))+
    geom_hline(yintercept = 0.3, linetype = "dashed") +
    #annotate("text", 0.7,0.25, "trend from Fig. 1")
    annotate("text", x = 1.01, y = 0.28, label = "trend from Fig. 1",
             hjust = 1.1, vjust = 1.1, size = 4, color = "black",
             family = "sans"             )


  if ( .codeOcean){
    png('../results/figure2.png')
    print(p)
    dev.off()
  } else {
    cairo_pdf( file.path  (getwd() , "plots", "figure2.pdf"),width = 5.11811, height = 5.11811)
    print(p)
  dev.off()
  }

}

##########################
# Figure S1. Why group size N is not important
#############################
{
  # expand over various group sizes and other key illustrative parameters
  mat1 = expand.grid( eps = c(0.3,0.7), N = c(5,10,20,30,50,100), dc = 0.5, dv = seq(0,1,0.01))
  mat2 = expand.grid( eps = c(0.3,0.7), N = c(5,10,20,30,50,100), dc = seq(0,1,0.01), dv = 0.5)
  VLs = NULL
  VFs = NULL
  # run through and calculate cost functions
  for ( i in 1:nrow(mat1)){
    #i=41
    v = VLCL(C = 2 , dc = mat1$dc[i], dv = mat1$dv [i] , eps = mat1$eps[i])
    v = v/mat1$N[i]
    VLs = c ( VLs , v["VL"])
    VFs = c ( VFs , v["VF"])
    print( length(VLs))
    print( length(VFs))
  }
  mat1 = rbind( mat1, mat1)
  mat1$V = c( VLs, VFs)
  mat1$Class  = rep(c("leader" , "follower"),each = 1212)
  mat1$eps = as.factor(mat1$eps)
  g1 = ggplot ( mat1 , aes ( y=V , x=dv))+
    geom_line(mapping = aes(linetype = eps, color = Class),linewidth = 1.5)+
    facet_wrap(~N, scales="free_y")+
    scale_x_continuous(sec.axis = sec_axis(~ . , name = "N", breaks = NULL, labels = NULL))+
    theme_classic( base_size = 17)
  CLs = NULL
  CFs = NULL
  for ( i in 1:nrow(mat2)){
    #i=1
    C = VLCL(C = 2 , dc = mat2$dc[i], dv = mat2$dv [i] , eps = mat2$eps[i])
    C = C/mat2$N[i]
    CLs = c ( CLs , C["CL"])
    CFs = c ( CFs , C["CF"])
    print( length(CLs))
    print( length(CFs))
  }
  mat2 = rbind( mat2, mat2)
  mat2$C = c( CLs, CFs)
  mat2$Class  = rep(c("leader" , "follower"),each = 1212)
  mat2$eps = as.factor(mat2$eps)
  g2 = ggplot ( mat2 , aes ( y=C , x=dc))+
    geom_line(mapping = aes(linetype = eps, color = Class),linewidth=1.5)+
    facet_wrap(~N, scales="free_y")+
    labs(y = "C = 2V")+
    scale_x_continuous(sec.axis = sec_axis(~ . , name = "N", breaks = NULL, labels = NULL))+
    theme_classic( base_size = 17)


  # PDF or codeOcean?
  if ( .codeOcean){
    png('../results/figureS1.png')
  } else {
    pdf( file.path  (getwd() , "plots", "figureS1.pdf"),width = 10, height = 10)
  }

  # Plot the figure
  grid.arrange(grobs = list( g1,g2),ncol=1)
  dev.off()
}

########################
# Figure S2
########################
{
  dcs = seq( 0.0001, 0.9999 ,length.out=80)
  dvs = seq( 0.0001, 0.9999 ,length.out=80)
  df  = expand.grid( dcs= dcs,
                     dvs= dvs)
  i=103
  df$Outcome = NA
  for ( i in 1: nrow(df)){
    result  = VLCL ( C= 2, dc =  df$dcs[i] , dv= df$dvs[i] , eps  = 0.1)
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
  if ( .codeOcean){
    png('../results/figureS2.png')
  } else {
    pdf( file.path  (getwd() , "plots", "figureS2.pdf"),width = 10, height = 10)
  }
  p = ggplot ( df , aes  (dcs, dvs , fill = Outcome))+
    geom_tile()+
    theme_classic(base_size = 12)+
    labs ( x = substitute(paste(italic('dc'))), y = substitute(paste(italic('dv')))) +
    scale_fill_manual( values = c( "Follower" = ViR[4], "Leader" = ViR[1], "Both equal" = ViR[3]  ))+
    xlim(0,1)+
    ylim(0,1)
  print(p)
  dev.off()
}


#########################
# Figure S3 - democratic war
#########################
{
  VLCLvar = VLCL ( C = 2, dv =  0.45, dc =  0.45, eps =  0.3) # notice dv and dc values lower than 0.5 as specified in Fig. S3
  dem = seq( 0 , 1, 0.01)
  outs = NULL
  for  ( i in 1:length ( dem)){
    out = anamodel(eps       = 0.3     ,
                   CL          =VLCLvar [ "CL" ],
                   VL          =VLCLvar [ "VL" ] ,
                   CF         = VLCLvar [ "CF" ],
                   VF         = VLCLvar [ "VF" ],
                   dem        = dem[i]
    )
    outs = rbind ( outs , out)
  }

  # pdf or code ocean ?
  if ( .codeOcean){
    png('../results/figureS3.png')
  } else {
    pdf( file.path  (getwd() , "plots", "figureS3.pdf"),width = 5, height = 7)
  }

  # Create the ggplot
  df = data.frame ( p = c(outs[,"P"],
                          outs[,"PL"],
                          outs[,"PF"]),
                    name= c( rep ("P",nrow(outs)),
                             rep ("PL",nrow(outs)),
                             rep ("PF",nrow(outs))),
                    dem = rep(dem,3))
  df$name =factor(df$name, levels = c("P","PF","PL"))
  p = ggplot(df, aes(x = dem, y = p, group = name, color =  name )) +
    geom_line() +
    scale_color_manual(values = c( "P" = ViR[3], "PF" = ViR[4],"PL" = ViR[1])) + # Replace with actual name-color pairs
    geom_hline(yintercept = VLCLvar["VL"] / VLCLvar["CL"], linetype = "dashed") +
    geom_hline(yintercept = VLCLvar["VF"] / VLCLvar["CF"], linetype = "dashed") +
    labs(x = "Omega", y = "P") +
    theme_minimal()
  print(p)
  dev.off()
}

###############################
# Figure S4 - plot the model over a broader spectrum of parameter values
###############################
{
  # choose how clean you would like the plots. 20 works OK for illustrative purposes
  sq.len = 20
  df = expand.grid( eps = seq(0.01,1,length.out=sq.len),  dc = seq(0.1,0.9,0.2), dv = seq(0.1,0.9,0.2), dem = seq(0,1,length.out=sq.len),C=c(2,5))
  i = 1
  mods = NULL
  df$pL = NA
  df$pF = NA
  df$p = NA
  df$lOpt = NA
  df$fOpt = NA
  for ( i in 1:nrow(df)){
    #i=10
    vl = VLCL(C = df$C[i], dc=df$dc[i], dv=df$dv[i], eps = df$eps[i])
    CL  = vl["CL"]
    VL  = vl["VL"]
    VF  = vl["VF"]
    CF  = vl["CF"]
    mod = anamodel (  eps = df$eps[i],
                      CL  = CL,
                      VL  = VL,
                      VF  = VF,
                      CF  = CF,
                      dem = df$dem[i])
    df$pL[i] = mod["PL"]
    df$pF[i] = mod["PF"]
    df$p[i]  = mod["P"]
    df$lOpt[i]  = VL/CL
    df$fOpt[i]  = VF/CF
    if(  i %in% round(seq(1,nrow(df),length.out=100))){
      print(paste ( i , "/" , nrow(df) ))
    }
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

  # PDF or code ocean?
  if ( .codeOcean){
    png('../results/figureS4.png')
  } else {
    pdf( file.path  (getwd() , "plots", "figureS4.pdf"),width = 26, height = 10)
  }

  # create the ggplot
  g1 = ggplot ( df2 , aes (y = eps , x = dem , fill = outcome))+
    geom_tile()+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 1),sec.axis = sec_axis(~ . , name = "dc", breaks = NULL, labels = NULL))+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 1),sec.axis = sec_axis(~ . , name = "dv", breaks = NULL, labels = NULL))+
    scale_fill_manual( values = c( "follower optimum" = ViR[4], "leader optimum" = ViR[1], "compromise" = ViR[3] , "both optimum" = "salmon" ))+
    facet_grid(dv  ~ dc)+
    theme_classic(base_size=20)
  g2 = ggplot ( df3 , aes (y = eps , x = dem , fill = outcome))+
    geom_tile()+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 1),sec.axis = sec_axis(~ . , name = "dc", breaks = NULL, labels = NULL))+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 1),sec.axis = sec_axis(~ . , name = "dv", breaks = NULL, labels = NULL))+
    scale_fill_manual( values = c( "follower optimum" = ViR[4], "leader optimum" = ViR[1], "compromise" = ViR[3] , "both optimum" = "salmon" ))+
    facet_grid(dv  ~ dc)+
    theme_classic(base_size=20)
  grid.arrange(grobs = list ( g2, g1) , ncol = 2)
  dev.off()
}
###############################################
# Figure S5 - algorithm optimisation.
##############################################
{
  vls = VLCL(C=8,dc=0.75,dv=0.75,eps=0.5)
  df1 = simmodel(eps=0.5,
                 CL  = vls["CL"],
                 VL  = vls["VL"],
                 VF  = vls["VF"],
                 CF  = vls["CF"],
                 n.gen = 1000,
                 pL.init = 0.01,
                 pF.init = 0.01,
                 dem=1,
                 method= "not",
                 d= 0.001
  )
  df2 = simmodel(eps=0.5,
                 CL  = vls["CL"],
                 VL  = vls["VL"],
                 VF  = vls["VF"],
                 CF  = vls["CF"],
                 n.gen = 1000,
                 pL.init = 0.01,
                 pF.init = 0.01,
                 dem=1,
                 method= "optimise",
                 d= 0.001
  )

  if ( .codeOcean){
    png('../results/figureS5.png')
  } else {
    pdf( file.path  (getwd() , "plots", "figureS5.pdf"),width = 6, height = 9)
  }
  par ( mfrow = c(2,1))
  plot ( df1[,1], lwd= 2, type = "l" , ylim  =c(0,1), col = ViR[1] ,ylab = "Hawk-playing probability", xlab = "Rounds")
  lines ( df1[,2],lwd= 2,  ylim  =c(0,1), col = ViR[4] )
  lines ( df1[,3],lwd= 2,  ylim  =c(0,1), col = ViR[3] )
  legend( "topleft", legend = c("leaders", "followers", "population"), lty = 1, lwd = 2, col = c( ViR[1],ViR[4],ViR[3] ))

  plot (  df2[,1], lwd = 2 , type = "l" , ylim  =c(0,1), col = ViR[1] ,ylab = "Hawk-playing probability", xlab = "Rounds")
  lines ( df2[,2], lwd = 2 , ylim  =c(0,1), col = ViR[4] )
  lines ( df2[,3], lwd = 2 , ylim  =c(0,1), col = ViR[3] )

  dev.off()

}
