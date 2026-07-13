/* Derived from utl-altair-slc-...-lp-programming.sas (rogerjdeangelis).       */
/* The requirements table and the solution-verification logic are the author's; */
/* the original libname workx "d:/wpswrkx" is redirected to WORK so the bundle  */
/* is self-contained, exactly as the author's own comment suggests.            */

data requirements;
 input cost wesco graybar ced requirement parts  $16.;
cards4;
200 11 20 33 1800 batteries
300 23 30 20 2500 switches
400 32 20 15 2000 motors
;;;;
run;quit;

proc print data=requirements;
title "Requirements table";
run;quit;

/* Optimum rounded LP solution stated in the repo: 19 wesco, 62 graybar, 11 ced */
%let wesco=19;
%let graybar=62;
%let ced=11;

data results;
  set requirements;
    computed_number = &wesco*wesco + &graybar*graybar + &ced*ced;
run;

proc print data=results;
title "minimum cost = &wesco*200 + &graybar*300 + &ced*400 = %left(%sysevalf(&wesco*200 + &graybar*300 + &ced*400))";
format computed_number requirement comma8.;
run;quit;
