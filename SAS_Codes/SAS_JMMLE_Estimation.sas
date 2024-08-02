libname r '/home/u63783224/';

proc import datafile="/home/u63783224/sasuser.v94/jmmle_param_est_init_whole.csv"
    out=r.param_est
    dbms=csv
    replace;
    getnames=yes;
run;

data _null_;
    set r.param_est;
    call symputx(strip(Parameter), Estimate);
run;


proc import datafile="/home/u63783224/sasuser.v94/jmmle_real_whole.csv"
OUT=r.R3
DBMS=csv
REPLACE;
GETNAMES=YES;
RUN;

proc  sort data=r.R3;
 by USUBJID Month_scaled;
 run;

Data r.R3;  
set r.R3;              
 BY USUBJID;    
   if last.USUBJID then
      last = 1;  else last=0; 
run; 
  
ods output ParameterEstimates=r.output; 
title1 'Two Random Effects Joint Model';

proc nlmixed data=r.R3 maxiter=1000 maxfunc=10000;    
parameters beta0M=&Beta0_M beta1M=&Beta1_M s2M=&Sigma_res_M
a11M=&d11_M a12M=&d12_M a22M=&d22_M
beta0T=&Beta0_T beta1T=&Beta1_T s2T=&Sigma_res_T
a11T=&d11_T a12T=&d12_T a22T=&d22_T
a13=0 a23=0 a14=0 a24=0 
alpha0=&Alpha0 alpha1=&Alpha1 alpha2=&Alpha2
alpha3=&Alpha3 alpha4=&Alpha4; 
               
bounds 0 < a11M, a22M, s2M, a11T, a22T, s2T; 

*The longitudinal model;
M_fit = (beta0M + intercept1) + (beta1M + slope1)*Month_scaled;
M_resid= TruGraf - M_fit; 
T_fit = (beta0T + intercept2) + (beta1T + slope2)*Month_scaled;
T_resid= TRAC - T_fit;
if (abs(M_resid) > 1.3E100) or (s2M < 1e-12) then do;
M_lllong = -1e20;
end; else do;
M_lllong = -0.5*(1.837876 + M_resid**2 / s2M + log(s2M));
end;
if (abs(T_resid) > 1.3E100) or (s2T < 1e-12) then do;
T_lllong = -1e20;
end; else do;
T_lllong = -0.5*(1.837876 + T_resid**2 / s2T + log(s2T));
end;
lllong = M_lllong + T_lllong;

if (last=1) then do;
 xb=alpha0+alpha1*intercept1+alpha2*slope1+alpha3*intercept2+alpha4*slope2; 
 
 prob = exp(xb)/(1+exp(xb));
 
 liklhd = (prob**two_year_rej)*((1-prob)**(1-two_year_rej));
 
 llbin = log(liklhd);
end; 
else llbin=0;

model two_year_rej ~ general(lllong + llbin);
random intercept1 slope1 intercept2 slope2 ~ normal([0, 0, 0, 0], [a11M, a12M, a22M, a13, a23, a11T, a14, a24, a12T, a22T]) subject=USUBJID out=rand; 
predict prob  out=pred;

quit;


DATA r.output2; 
   SET r.output;
   KEEP parameter Estimate Lower Upper;
RUN;


DATA r.output3; 
SET r.output2;
If parameter = "alpha1" then  Estimate = Estimate;
If parameter = "alpha1" then  Lower = Lower;
If parameter = "alpha1" then  Upper = Upper;
If parameter = "alpha2" then  Estimate = Estimate;
If parameter = "alpha2" then Lower = Lower;
If parameter = "alpha2" then Upper =Upper;
If parameter = "alpha3" then  Estimate = Estimate;
If parameter = "alpha3" then  Lower = Lower ;
If parameter = "alpha3" then  Upper = Upper;
If parameter = "alpha4" then  Estimate = Estimate;
If parameter = "alpha4" then Lower = Lower;
If parameter = "alpha4" then Upper =Upper;
If parameter = "a11M" then  Estimate = Estimate;
If parameter = "a11M" then  Lower = Lower;
If parameter = "a11M" then  Upper = Upper;
If parameter = "a22M" then  Estimate = Estimate;
If parameter = "a22M" then  Lower = Lower;
If parameter = "a22M" then  Upper = Upper;
If parameter = "s2M" then  Estimate = Estimate;
If parameter = "s2M" then  Lower = Lower;
If parameter = "s2M" then  Upper = Upper;
If parameter = "a11T" then  Estimate = Estimate;
If parameter = "a11T" then  Lower = Lower;
If parameter = "a11T" then  Upper = Upper;
If parameter = "a22T" then  Estimate = Estimate;
If parameter = "a22T" then  Lower = Lower;
If parameter = "a22T" then  Upper = Upper;
If parameter = "s2T" then  Estimate = Estimate;
If parameter = "s2T" then  Lower = Lower;
If parameter = "s2T" then  Upper = Upper;
run;


*Save the output of the JMMLE ;
proc export data=r.output3   
     outfile=".../Table_Estimates_JMMLE.csv"
     replace
     dbms=dlm;
     delimiter=',';
run;
