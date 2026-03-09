# utl-altair-slc-minimize-cost-purchasing-interchangeable-parts-from-mutiple-companies-lp-programming
Altair slc minimize cost when purchasing interchangeable parts from multiple companies lp programming
    %let pgm=utl-altair-slc-minimize-cost-purchasing-interchangeable-parts-from-mutiple-companies-lp-programming;

    %stop_submission;

    Altair slc minimize cost when purchasing interchangeable parts from mutiple companies lp programming

    Too long to post on a list,see github
    https://github.com/rogerjdeangelis/utl-altair-slc-minimize-cost-purchasing-interchangeable-parts-from-mutiple-companies-lp-programming

    relate repo (classic cattle feed problem)
    https://github.com/rogerjdeangelis/utl-minimize-cost-of-cattle-feed-with-nutritional-requirements-and-multiple-food-sources-lp-program

    PROBLEM
    =======

    WHAT I NEED
    -----------

    I need to minmize the cost when puchasing the following

       1,800 batteries
       2,500 switches
       2,000 motors


    SUPPLIER COSTS
    --------------

    I have three supplies who sell package deals of batteries, switches and motors

                       companies
                 wesco graybar   ced

    battery        11     20    33
    switch         23     30    20
    motor          32     20    15
                 ====   ====  ====
    Package Cost $200   $300  $400

    Wesco sells a package with 11 batteries and 23 switches, and 32 motors for $200;


    SOLUTION  (Identify how I can purchase parts from multiple companies to reduce my overall cost)
    --------

    Lowest cost 19 packages from Wesco, 62 packages from graybar, and 11 from ced for $26,800. (save over $3,000)

                                #packages*price
                    ---------------------------------
                      wesco       graybar       ced     Total cost

      Combination   18.91*200 + 61.54*300 + 10.95*400     $26,624

      Rounded          19*200 +    62*300 +    11*400     $26,800 --> Optimum rounded. Save $3,200 over graybar only

      wesco only      164*$200~                           $32,800
      graybar only               100*$300                 $30,000
      ced only                               133*$400     $53,200



    ~ see end of message for calculation

    VERIFICATION
    ------------

    Number of packages package costs

    minimum cost = 18.91*200 + 61.54*300 + 10.95*400 =$26,624
    rounded cost =    19*200 +    62*300 +    11*400 =$26,800

    Number of parts in a package time number of packages (rounded up)

    Batteries = 19*11 +    62*20 +    11*33 = 1812
    Switches =  19*23 +    62*30 +    11*20 = 2517
    Motors   =  19*32 +    62*20 +    11*15 = 2013



    NUMBER OF PACKAGES AND TOTAL COST USING JUST ONE SUPPLIER
    ---------------------------------------------------------

    ~For example I buy only from wesco

    Identify the number of packages  required to reach 1,800 batteries, 2,500 wwitches, and 2,00 motors.

     wesco only
      To get 1,800 batteries I would need to buy 1800/11 packages =164 packages (each package has 11 switches)
      To get 2,500 switches I would need to buy 2500/23 packages = 108 packages (each package has 23 switches)
      To get 2,000 motors  I would need to buy 2000/32 packages =63 packages (each package has 32 switches)

      since we need 164 packages to ge 1,800 batteries and 164 is the largest number, the total cost using just wesco is
      $32,800 = $200*164

     graybar only
      To get 1,800 batteries I would need to buy 1800/20 packages =90 packages (each package has 21 switches)
      To get 2,500 switches I would need to buy 2500/30 packages = 83 packages (each package has 30 switches)
      To get 2,000 motors  I would need to buy 2000/20 packages =100 packages (each package has 23 switches)

      since we need 100 packages to ge 2,000 batteries the total cost using just wesco is
      $30,000 = $300*100

     ced only
      To get 1,800 batteries I would need to buy 1800/33 packages = 54 packages (each package has 33 switches)
      To get 2,500 switches I would need to buy 2500/20 packages = 125 packages (each package has 20 switches)
      To get 2,000 motors  I would need to buy 2000/15 packages = 133 packages (each package has 15 switches)

      since we need 87 packages to ge 2,000 batteries the total cost using just wesco is
      $53,200 = $400*133


    /*                   _
    (_)_ __  _ __  _   _| |_
    | | `_ \| `_ \| | | | __|
    | | | | | |_) | |_| | |_
    |_|_| |_| .__/ \__,_|\__|
            |_|
    */

    libname workx "d:/wpswrkx"; /*--- put this in your autoecec ---*/

    proc datasets lib=workx kill nodetails nolist;
    run;

    options validvarname=v7;
    data workx.requirements;
     input cost wesco graybar ced requirement parts  $16.;
    cards4;
    200 11 20 33 1800 batteries
    300 23 30 20 2500 switches
    400 32 20 15 2000 motors
    ;;;;
    run;quit;

    /**************************************************************************************************************************/
    /* WORKX.REQUIREMENTS total obs=3                                                                                         */
    /*                                                                                                                        */
    /* Obs    cost    wesco    graybar    ced    requirement    parts                                                         */
    /*                                                                                                                        */
    /*  1      200      11        20       33        1800       batteries                                                     */
    /*  2      300      23        30       20        2500       switches                                                      */
    /*  3      400      32        20       15        2000       motors                                                        */
    /**************************************************************************************************************************/

    /*
     _ __  _ __ ___   ___ ___  ___ ___
    | `_ \| `__/ _ \ / __/ _ \/ __/ __|
    | |_) | | | (_) | (_|  __/\__ \__ \
    | .__/|_|  \___/ \___\___||___/___/
    |_|
    */

    options validvarname=v7;
    options set=RHOME "C:\Progra~1\R\R-4.5.2\bin\r";
    proc r;
    export data=workx.requirements r=requirements;
    submit;
    library(lpSolve)

    # Define the cost vector

    cost <-  requirements$cost;

    # Define the constraint matrix
    # Each row represents a constraint (Protein, Fiber, Energy)
    # Each column represents an ingredient (Corn, Soybean, Hay)

    constraints <- as.matrix(requirements[,2:4]);

    # Define the direction of the constraints
    # ">=" for Protein and Energy, "<=" for Fiber
    direction <- c(">=", ">=", ">=")

    # Define the right-hand side of the constraints
      rhs <- requirements$requirement

    # Solve the linear programming problem
    solution <- lp("min", cost, constraints, direction, rhs)

    solution$solution
    res=data.frame(cbind(t(solution$solution),solution$objval))
    colnames(res)<-c("num_wesco","num_graybar","num_ced","mincost")
    "RESULTS"
    res;
    endsubmit;
    import data=workx.solution r=res;
    run;

    symdel wesco graybar ded / nowarn;

    options validvarname=v7;
    proc print data=workx.solution;
    run;quit;

    data null_;;
      set workx.solution;
      call symputx('wesco',put(num_wesco,6.2));
      call symputx('graybar',put(num_graybar,6.2));
      call symputx('ced',put(num_ced,6.2));
      rc=dosubl('
        options validvarname=v7;
        data workx.results;
          set workx.requirements;
            computed_number = &wesco*wesco + &graybar*graybar + &ced*ced;
        run;');
    run;

    proc print data=workx.results;
    title "minimum cost = &wesco*200 + &graybar*300 + &ced*400 = %left(%sysevalf(&wesco*200 + &graybar*300 + &ced*400))";
    format computed_number requirement comma8.;
    run;quit;

    /**************************************************************************************************************************/
    /* SLC                                                    computed_ | R   [1] "RESULTS"                                   */
    /*  minimum cost = 18.91*200 + 61.54*300 + 10.95*400 = 26,624       |                                                     */
    /*                                                        computed_ |    [1] "RESULTS"                                    */
    /* Obs cost wesco  graybar  ced  requirement  parts       number    |                                                     */
    /*                                                                  |      num_wesco num_graybar  num_ced  mincost        */
    /*  1   200   11      20     33      1,800    batteries     1,800   |                                                     */
    /*  2   300   23      30     20      2,500    switches      2,500   |   1  18.90547    61.54229 10.94527 26621.89         */
    /*  3   400   32      20     15      2,000    motors        2,000   |                                                     */
    /**************************************************************************************************************************/

    /*
    | | ___   __ _
    | |/ _ \ / _` |
    | | (_) | (_| |
    |_|\___/ \__, |
             |___/
    */
    1                                          Altair SLC          15:49 Sunday, March  8, 2026

    NOTE: Copyright 2002-2025 World Programming, an Altair Company
    NOTE: Altair SLC 2026 (05.26.01.00.000758)
          Licensed to Roger DeAngelis
    NOTE: This session is executing on the X64_WIN11PRO platform and is running in 64 bit mode

    NOTE: AUTOEXEC processing beginning; file is C:\wpsoto\autoexec.sas
    NOTE: AUTOEXEC source line
    1       +  ï»¿ods _all_ close;
               ^
    ERROR: Expected a statement keyword : found "?"
    NOTE: Library workx assigned as follows:
          Engine:        SAS7BDAT
          Physical Name: d:\wpswrkx

    NOTE: Library slchelp assigned as follows:
          Engine:        WPD
          Physical Name: C:\Progra~1\Altair\SLC\2026\sashelp

    NOTE: Library worksas assigned as follows:
          Engine:        SAS7BDAT
          Physical Name: d:\worksas

    NOTE: Library workwpd assigned as follows:
          Engine:        WPD
          Physical Name: d:\workwpd


    LOG:  15:49:54
    NOTE: 1 record was written to file PRINT

    NOTE: The data step took :
          real time : 0.016
          cpu time  : 0.000


    NOTE: AUTOEXEC processing completed

    NOTE: Directory contains files of mixed engine types
    1          /*                   _
    2         (_)_ __  _ __  _   _| |_
    3         | | `_ \| `_ \| | | | __|
    4         | | | | | |_) | |_| | |_
    5         |_|_| |_| .__/ \__,_|\__|
    6                 |_|
    7         */
    8
    9         libname workx "d:/wpswrkx"; /*--- put this in your autoecec ---*/
    NOTE: Library workx assigned as follows:
          Engine:        WPD
          Physical Name: d:\wpswrkx

    10
    11        proc datasets lib=workx kill nodetails nolist;
    12        run;
    NOTE: Deleting WORKX.REQUIREMENTS (type=DATA)
    NOTE: Deleting WORKX.RESULTS (type=DATA)
    NOTE: Deleting WORKX.SOLUTION (type=DATA)
    13
    14        options validvarname=v7;
    NOTE: Procedure datasets step took :
          real time : 0.002
          cpu time  : 0.000


    15        data workx.requirements;
    16         input cost wesco graybar ced requirement parts  $16.;
    17        cards4;

    NOTE: Data set "WORKX.requirements" has 3 observation(s) and 6 variable(s)
    NOTE: The data step took :
          real time : 0.006
          cpu time  : 0.015


    18        200 11 20 33 1800 batteries
    19        300 23 30 20 2500 switches
    20        400 32 20 15 2000 motors
    21        ;;;;
    22        run;quit;
    23
    24        /**************************************************************************************************************************/
    25        /* WORKX.REQUIREMENTS total obs=3                                                                                         */
    26        /*                                                                                                                        */
    27        /* Obs    cost    wesco    graybar    ced    requirement    parts                                                         */
    28        /*                                                                                                                        */
    29        /*  1      200      11        20       33        1800       batteries                                                     */
    30        /*  2      300      23        30       20        2500       switches                                                      */
    31        /*  3      400      32        20       15        2000       motors                                                        */
    32        /**************************************************************************************************************************/
    33
    34        /*
    35         _ __  _ __ ___   ___ ___  ___ ___
    36        | `_ \| `__/ _ \ / __/ _ \/ __/ __|
    37        | |_) | | | (_) | (_|  __/\__ \__ \
    38        | .__/|_|  \___/ \___\___||___/___/
    39        |_|
    40        */
    41
    42        options validvarname=v7;
    43        options set=RHOME "C:\Progra~1\R\R-4.5.2\bin\r";
    44        proc r;
    NOTE: Using R version 4.5.2 (2025-10-31 ucrt) from C:\Program Files\R\R-4.5.2
    45        export data=workx.requirements r=requirements;
    NOTE: Creating R data frame 'requirements' from data set 'WORKX.requirements'

    46        submit;
    47        library(lpSolve)
    48
    49        # Define the cost vector
    50
    51        cost <-  requirements$cost;
    52
    53        # Define the constraint matrix
    54        # Each row represents a constraint (Protein, Fiber, Energy)
    55        # Each column represents an ingredient (Corn, Soybean, Hay)
    56
    57        constraints <- as.matrix(requirements[,2:4]);
    58
    59        # Define the direction of the constraints
    60        # ">=" for Protein and Energy, "<=" for Fiber
    61        direction <- c(">=", ">=", ">=")
    62
    63        # Define the right-hand side of the constraints
    64        rhs <- c(1800, 2500, 2000)
    65
    66        # Solve the linear programming problem
    67        solution <- lp("min", cost, constraints, direction, rhs)
    68
    69        solution$solution
    70        res=data.frame(cbind(t(solution$solution),solution$objval))
    71        colnames(res)<-c("num_wesco","num_graybar","num_ced","mincost")
    72        "RESULTS"
    73        res;
    74        endsubmit;

    NOTE: Submitting statements to R:

    > library(lpSolve)
    >
    > # Define the cost vector
    >
    > cost <-  requirements$cost;
    >
    > # Define the constraint matrix
    > # Each row represents a constraint (Protein, Fiber, Energy)
    > # Each column represents an ingredient (Corn, Soybean, Hay)
    >
    > constraints <- as.matrix(requirements[,2:4]);
    >
    > # Define the direction of the constraints
    > # ">=" for Protein and Energy, "<=" for Fiber
    > direction <- c(">=", ">=", ">=")
    >
    > # Define the right-hand side of the constraints
    > rhs <- c(1800, 2500, 2000)
    >
    > # Solve the linear programming problem
    > solution <- lp("min", cost, constraints, direction, rhs)
    >
    > solution$solution
    > res=data.frame(cbind(t(solution$solution),solution$objval))
    > colnames(res)<-c("num_wesco","num_graybar","num_ced","mincost")
    > "RESULTS"
    > res;

    NOTE: Processing of R statements complete

    75        import data=workx.solution r=res;
    NOTE: Creating data set 'WORKX.solution' from R data frame 'res'
    NOTE: Data set "WORKX.solution" has 1 observation(s) and 4 variable(s)

    76        run;
    NOTE: Procedure r step took :
          real time : 0.339
          cpu time  : 0.015


    77
    78        symdel wesco graybar ded / nowarn;
              ^
    ERROR: Expected a statement keyword : found "symdel"
    79
    80        options validvarname=v7;
    81        proc print data=workx.solution;
    82        run;quit;
    NOTE: 1 observations were read from "WORKX.solution"
    NOTE: Procedure print step took :
          real time : 0.007
          cpu time  : 0.000


    83
    84        data null_;;
    85          set workx.solution;
    86          call symputx('wesco',put(num_wesco,6.2));
    87          call symputx('graybar',put(num_graybar,6.2));
    88          call symputx('ced',put(num_ced,6.2));
    89          rc=dosubl('
    90            options validvarname=v7;
    91            data workx.results;
    92              set workx.requirements;
    93                computed_number = &wesco*wesco + &graybar*graybar + &ced*ced;
    94            run;');
    95        run;

    96            options validvarname=v7;    data workx.results;      set workx.requirements;        computed_number = &wesco*wesco + &graybar*graybar + &ced*ced;    run;

    NOTE: 3 observations were read from "WORKX.requirements"
    NOTE: Data set "WORKX.results" has 3 observation(s) and 7 variable(s)
    NOTE: The data step took :
          real time : 0.006
          cpu time  : 0.015


    ERROR: Error printed on page 1

    NOTE: Submitted statements took :
          real time : 0.007
          cpu time  : 0.015
    NOTE: 1 observations were read from "WORKX.solution"
    NOTE: Data set "WORK.null_" has 1 observation(s) and 5 variable(s)
    NOTE: The data step took :
          real time : 0.015
          cpu time  : 0.015


    97
    98        proc print data=workx.results;
    99        title "minimum cost = &wesco*200 + &graybar*300 + &ced*400 = %left(%sysevalf(&wesco*200 + &graybar*300 + &ced*400))";
    100       format computed_number requirement comma8.;
    101       run;quit;
    NOTE: 3 observations were read from "WORKX.results"
    NOTE: Procedure print step took :
          real time : 0.010
          cpu time  : 0.015


    ERROR: Error printed on page 1

    NOTE: Submitted statements took :
          real time : 0.449
          cpu time  : 0.156

    /*              _
      ___ _ __   __| |
     / _ \ `_ \ / _` |
    |  __/ | | | (_| |
     \___|_| |_|\__,_|

    */
