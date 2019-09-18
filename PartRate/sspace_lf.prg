'___________________________________________________________________________________________________________________________________________________________________________

'  ESTIMATE UNOBSERVED COMPONENT MODELS OF AUS LABOR MARKET
' email: david.stephan@gmail.com ;
' reference: https://www.rba.gov.au/publications/bulletin/2018/sep/the-cyclical-behaviour-of-labour-force-participation.html

'___________________________________________________________________________________________________________________________________________________________________________
close @all
%path = @runpath
cd %path

'Read Data File
import lf_hist.xlsx range="EVIEWS" @freq q 1964q1
delete(noerr) series01

'Set Estimation Period
sample ssest 1966q3 2019q2

series lrgdppc = log(rgdppc)*100
series dlrgdppc = d(lrgdppc)

series dum_mod = 0
smpl @first 1983q4 
dum_mod =1 

'Creat initial series for initialzing state space
smpl @all
lrgdppc.hpf(lambda=1600) lrgdppc_ini
unr.hpf(lambda=1600) unr_ini
prt.hpf(lambda=1600) prt_ini
series cycle_ini = lrgdppc - lrgdppc_ini

vector(5) mprior = 0
mprior(1) = lrgdppc_ini(@ifirst(lrgdppc_ini)+1)
mprior(2) = unr_ini(@ifirst(unr_ini)+1)
mprior(3) = prt_ini(@ifirst(prt_ini)+1)
mprior(4) = cycle_ini(@ifirst(cycle_ini)+1)
mprior(5) = cycle_ini(@ifirst(cycle_ini))

' Setup coefficient vectors
coef(1) delta
coef(2) phi
coef(13) sigma
coef(2) kappa
coef(2) theta
coef(2) rho

' Estimate initial coefficients
' Trend
smpl ssest
equation eq_lrgdppc.ls lrgdppc_ini =delta(1)+lrgdppc_ini(-1)

' Store estimates for later use
!delta1=delta(1)
!sigma1=eq_lrgdppc.@se

' UNR Rate
smpl ssest
equation eq_unr.ls unr = unr_ini + kappa(1)*cycle_ini + kappa(2)*cycle_ini(-1)

' Store estimates for later use
!kappa1=kappa(1)
!kappa2=kappa(2)
!sigma2=eq_unr.@se 

' UNR Trend Rate
smpl ssest
equation eq_unrt.ls d(unr_ini) = c(1)
!sigma2=eq_unrt.@se 

' PRT rate
smpl ssest
equation eq_prt.ls prt = prt_ini + theta(1)*cycle_ini + theta(2)*cycle_ini(-1)

' Store estimates for later use
!theta1=theta(1)
!theta2=theta(2)

' PRT Trend
smpl ssest
equation eq_prtt.ls d(prt_ini) = c(1)
!sigma3=eq_prtt.@se 

' Cycle
smpl ssest
equation eq_phi.ls cycle_ini = phi(1)*cycle_ini(-1) + phi(2)*cycle_ini(-2) 

' Store estimates for later use
!phi1 = phi(1)
!phi2 =phi(2)
!sigma4=eq_phi.@se

'State Space System of GDP/ UR Rate and Part Rate
sspace ss_labour
ss_labour.append @param delta(1) !delta1 
ss_labour.append @param kappa(1) !kappa1 kappa(2) !kappa2 
ss_labour.append @param theta(1) !theta1 theta(2) !theta2
ss_labour.append @param phi(1) !phi1 phi(2) !phi2 
ss_labour.append @param sigma(1) !sigma1 sigma(2) !sigma2 sigma(3) !sigma3 sigma(4) !sigma4 
ss_labour.append @param rho(1) 0.2

ss_labour.append @signal lrgdppc = ystar + cycle
ss_labour.append @signal unr = unrstar + kappa(1)*cycle + kappa(2)*cyclelag
ss_labour.append @signal prt = prtstar + theta(1)*cycle + theta(2)*cyclelag

ss_labour.append @state ystar = delta(1) + ystar(-1) + [ename = e1, var = (sigma(1)^2)]
ss_labour.append @state unrstar = unrstar(-1) + [var=(sigma(2)^2)]
ss_labour.append @state prtstar = prtstar(-1) + [var=(sigma(3)^2)]
ss_labour.append @state cycle = phi(1)*cycle(-1) + phi(2)*cyclelag(-1) + [ename = e4, var=(sigma(4)^2)]
ss_labour.append @state cyclelag = cycle(-1)

ss_labour.append @evar cov(e1, e4) = sigma(1)*sigma(4)*rho(1)

ss_labour.append @mprior mprior

smpl ssest
ss_labour.ml(optmethod=legacy)
ss_labour.makestates(t=smooth) *

smpl @all
series prt_cyclical = prt - prtstar

'State Space System of GDP/ UR Rate and Part Rate with Break in Var-Covariance Matrix
sspace ss_break
ss_break.append @param delta(1) !delta1 
ss_break.append @param kappa(1) !kappa1 kappa(2) !kappa2 
ss_break.append @param theta(1) !theta1 theta(2) !theta2
ss_break.append @param phi(1) !phi1 phi(2) !phi2 
ss_break.append @param sigma(1) !sigma1 sigma(2) !sigma2 sigma(3) !sigma3 sigma(4) !sigma4 
ss_break.append @param rho(1) 0.8 rho(2) -0.4

ss_break.append @signal lrgdppc = ystar + cycle
ss_break.append @signal unr = unrstar + kappa(1)*cycle + kappa(2)*cyclelag
ss_break.append @signal prt = prtstar + theta(1)*cycle + theta(2)*cyclelag

ss_break.append @state ystar = delta(1) + ystar(-1) + [ename = e1, var = (sigma(1)+sigma(10)*dum_mod)^2]
ss_break.append @state unrstar = unrstar(-1) + [var=(sigma(2)+sigma(11)*dum_mod)^2]
ss_break.append @state prtstar = prtstar(-1) + [var=(sigma(3)+sigma(12)*dum_mod)^2]
ss_break.append @state cycle = phi(1)*cycle(-1) + phi(2)*cyclelag(-1) + [ename = e4,  var = (sigma(4)+sigma(13)*dum_mod)^2]
ss_break.append @state cyclelag = cycle(-1)

ss_break.append @evar cov(e1, e4) = (sigma(1)+sigma(10))*(sigma(4)+sigma(13))*(rho(1)+rho(2)*dum_mod)

ss_break.append @mprior mprior

smpl ssest
ss_break.ml(optmethod=legacy)
ss_break.makestates(t=smooth) *_break

smpl @all
series prt_cyclical_break = prt - prtstar

smpl @all
plot prt_cyclical prt_cyclical_break


