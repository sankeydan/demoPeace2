source( file.path ( getwd( ), "VLCL.r"))
source( file.path ( getwd( ), "model.r"))

VLCL = VLCL ( C = 2, dv =  0.55, dc =  0.55, lam =  0.3)

dem = seq( 0 , 1, 0.01)

outs = NULL
for  ( i in 1:length ( dem)){
out = model(lam       = 0.3     ,
            CL          =VLCL [ "CL" ],
            VL          =VLCL [ "VL" ] ,
            CF         = VLCL [ "CF" ],
            VF         = VLCL [ "VF" ],
            dem        = dem[i]     ,
            pL.init    = 0.01,
            pF.init    = 0.01,
            n.gen      = 300    )
outs = rbind ( outs , out)
}

plot ( outs[,1], col  = 1, ylim = c(0,1) , type = "l")
lines( outs[,2], col  = 2)
lines( outs[,3], col  = 3)
