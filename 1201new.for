       SUBROUTINE TYPE70 (TIME,XIN,OUT,T,DTDT,PAR,INFO,ICNTRL,*)
C      SUBROUTINE FANSIM (Prop,P,M,TEnt,WEnt,
C     &                   TLvg,WLvg,Power,ErrStat)
C***********************************************************************
C*    Copyright ASHRAE.  Toolkit for HVAC System Energy Calculations
C*     UPDATED FOR THE TRNSYS-LIBRARY BY RUEDIGER SCHWARZ AND NATE BLAIR
C***********************************************************************
C*    SUBROUTINE:             FANSIM
C*
C*    LANGUAGE:               FORTRAN 77
C*
C*    PURPOSE:                Calculate the fan power and leaving
C*                            air temperature and humidity for fan
C*                            using simple part load characteristics.
C***********************************************************************
C*    INPUT VARIABLES DESCRIPTION(UNITS)                    SAMPLE VALUE
C*    XIN(1)    M     Dry air mass flow rate(kg/s)             3.4
C*    XIN(2)  TEnt    Entering air dry bulb temperature(C)    12.78
C*    XIN(3)  WEnt    Entering air humidity ratio(-)            .00835
C*
C*    OUTPUT VARIABLES
C*    OUT(1)  TLvg    Leaving air dry bulb temperature(C)     14.3358
C*    OUT(2)  WLvg    Leaving air humidity ratio(-)             .00835
C*    OUT(3)  Power   Fan power(W)                          5401.19
C*    OUT(4) ErrStat  Error status indicator,0=ok,1=error(-)   0.0
C*
C*    Note:   If M<0, TEnt and WEnt are assumed to be fan outlet air
C*            conditions, TLvg and WLvg are calculated inlet conditions
C*
C*    PARAMETERS
C*    PAR(1) EffMot    Motor drive efficiency(-)                    .85
C*    PAR(2) MotorLoss Fraction of motor heat loss to fluid stream  1.0
C*    PAR(3) FlowRated Rated volumetric flow rate(m3/s)             5.664
C*    PAR(4) PowRated  Rated shaft power(W)                     17700.0
C*    PAR(5) PlrContl  Mode for fan control(-)                      3.0
C*                     PlrContl = 1:  Discharge dampers
C*                     PlrContl = 2:  Inlet vanes
C*                     PlrContl = 3:  Variable speed drive
C***********************************************************************
C     MAJOR RESTRICTIONS:     Fan curve fits are independent of pressure
C
C     DEVELOPER:              Shauna Gabel
C                             Michael J. Brandemuehl, PhD, PE
C                             University of Colorado at Boulder
C
C     DATE:                   January 1, 1992
C
C     INCLUDE FILES:          fanpmp.inc
C     SUBROUTINES CALLED:     None
C     FUNCTIONS CALLED:       DRYBULB
C                             ENTHALPY
C                             RHODRY
C
C     REVISION HISTORY:       None
C
C     REFERENCE:              BLAST.  1986.  Building Loads Analysis
C                             and System Thermodynamics Program:
C                             User's Manual, Version 3.0. U.S. Army
C                             Construction Engineering Research
C                             Laboratory, Champaign, IL. pp.5-26-5-27.
C***********************************************************************
C     INTERNAL VARIABLES:
C     effFan        Fan efficiency                                   (-)
C     hEnt          Entering air enthalpy                         (J/kg)
C     rho           Entering moist air density                   (kg/m3)
C     fflp          Fraction of full-load fan power                  (-)
C     plr           Part load flow ratio                             (-)
C     powShaft      Shaft power                                      (W)
C     qLoss         Heat transfer to fluid stream                    (W)
C     c(i,PlrContl) Regression coefficients
C     small         Small number used in place of zero
C***********************************************************************

C    $INCLUDE: 'fanpmp.inc'

C Simply adding a comment to test all of this out 
      
      DATA PATM/101325.0/,CPAIR/1006.0/,CPVAP/1805.0/,HFG/2501000/,
     & RAIR/287.055/, TABSADD/273.15/ 

	DOUBLE PRECISION XIN, OUT
      DIMENSION XIN(3), OUT(4), PAR(5)
      DIMENSION C(4,3), INFO(15)
      INTEGER ErrStat, IOPT, NI, NP, ND, INFO
      REAL M, PAR
	CHARACTER*3 YCHECK(3), OCHECK(4)
	DATA YCHECK/'MF2','TE1','DM1'/
	DATA OCHECK/'TE1','DM1','PW2','DM1'/

	IOPT	= -1.	
	NI      = 3.	!CORRECT NUMBER OF INPUTS 
	NP		= 5.	!CORRECT NUMBER OF PARAMETERS 
	ND		= 0.	!CORRECT NUMBER OF DERIVATIVES 

      M       = XIN(1)
      TENT    = XIN(2)
      WENT    = XIN(3)
 
      TLVG    = OUT(1)
      WLVG    = OUT(2)
      POWER   = OUT(3)
      ERRSTAT = OUT(4)

      EFFMOT    = PAR(1)
      MOTORLOSS = PAR(2)
      FLOWRATED = PAR(3)
      POWRATED  = PAR(4)
      PLRCONTL  = PAR(5)

	IF (INFO(7).EQ.-1) THEN
		CALL TYPECK(IOPT,INFO,NI,NP,ND)
C	CHECKS #S IN USER SUPLLIED INFO ARRAY W/ NI, NP, AND ND
		CALL RCHECK(INFO,YCHECK,OCHECK)
C     CHECKS TO SEE IF THE UNITS ARE CONSISTENT
	ENDIF

! Another comment to test branching


C      DIMENSION P(NPFANPMP)

C2*** Set regression coefficients for fraction of full load power
C2***        Discharge dampers
      DATA C/0.3507123,   0.3085, -0.54137, 0.871988,
C2***        Inlet vanes
     &          0.3707,   0.9725,  -0.3424,      0.0,
C2***        Variable speed
     &         0.00153, 0.005208,   1.1086, -0.11635563/
      DATA small/1.E-9/

      ErrStat = 0

C1*** Calculate entering moist air properties

      hEnt = ENTHALPY(CPAIR,HFG,CPVAP,TEnt,WEnt)
      rho = RHODRY(PATM,RAIR,TABSADD,TEnt,WEnt)

C1*** Calculate the part load ratio based on rated flow

      plr=ABS(M)/rho/FlowRated

C1*** Calculate the fraction of full-load power based on rating point
C2*** fflp = c(1) + c(2)*plr + c(3)*plr**2 + c(4)*plr**3
C2*** Regression coefficients, c(i), vary with control mode

      fflp =c(1,PlrContl)+plr*( c(2,PlrContl)
     &                          +plr*( c(3,PlrContl)
     &                              +plr* c(4,PlrContl) ) )

C1*** Calculate the actual fan shaft power and motor power

      powShaft = PowRated*fflp
      Power = powShaft/EffMot

C1*** Calculate the leaving air conditions
C2*** If flow is zero, ABS(M) < small, the value of M is replaces with
C2    small of the same sign as M in calculating hLvg

      qLoss = powShaft + (Power-powShaft)*MotorLoss
      hLvg = hEnt + qLoss/SIGN(MAX(ABS(M),small),M)
      WLvg = WEnt
      TLvg = DRYBULB(CPAIR,CPVAP,HFG,hLvg,WLvg)

      OUT(1) = TLVG
      OUT(2) = WLVG
      OUT(3) = POWER
      OUT(4) = ERRSTAT

      RETURN 1
      END

      REAL FUNCTION DRYBULB (CPAIR,CPVAP,HFG,H,W)
C***********************************************************************
C*    Copyright ASHRAE.  Toolkit for HVAC System Energy Calculations
C***********************************************************************
C*    FUNCTION:               DRYBULB
C*
C*    LANGUAGE:               FORTRAN 77
C*
C*    PURPOSE:                Calculate the dry bulb temperature of
C*                            moist air from enthalpy and humidity.
C***********************************************************************
C*    INPUT VARIABLES:
C*    H             Enthalpy                                      (J/kg)
C*    W             Humidity ratio                                   (-)
C*
C*    OUTPUT VARIABLES:
C*    Drybulb       Dry bulb temperature                             (C)
C*
C*    PROPERTIES:
C*    CpAir         Specific heat of air                        (J/kg C)
C*    CpVap         Specific heat of water vapor                (J/kg C)
C*    Hfg           Reference heat of vaporization of water       (J/kg)
C***********************************************************************
C     MAJOR RESTRICTIONS:     Uses perfect gas relationships
C                             Fit for enthalpy of saturated water vapor
C
C     DEVELOPER:              Shauna Gabel
C                             Michael J. Brandemuehl, PhD, PE
C                             University of Colorado at Boulder
C
C     DATE:                   January 1, 1992
C
C     INCLUDE FILES:          PROP.INC
C     SUBROUTINES CALLED:     None
C     FUNCTIONS CALLED:       None
C
C     REVISION HISTORY:       None
C
C     REFERENCE:              1989 ASHRAE Handbook - Fundamentals
C***********************************************************************

C $INCLUDE: 'prop.inc'

C1*** Calculate the dry bulb temperature as a function of enthalpy and
C1*** humidity ratio.
C2*** hDryAir = Prop(CpAir)*TDB
C2*** hSatVap = Prop(Hfg) + Prop(CpVap)*TDB
C2*** Enthalpy = hDryAir + W*hSatVap

      Drybulb = (H-Hfg*W)/(CpAir+CpVap*W)

      RETURN 
      END
      REAL FUNCTION ENTHALPY (CPAIR,HFG,CPVAP,TDB,W)
C***********************************************************************
C*    Copyright ASHRAE.  Toolkit for HVAC System Energy Calculations
C***********************************************************************
C*    FUNCTION:               ENTHALPY
C*
C*    LANGUAGE:               FORTRAN 77
C*
C*    PURPOSE:                Calculate the enthalpy of moist air.
C***********************************************************************
C*    INPUT VARIABLES:
C*    TDB           Dry bulb temperature                             (C)
C*    W             Humidity ratio                                   (-)
C*
C*    OUTPUT VARIABLES:
C*    Enthalpy      Enthalpy of moist air                         (J/kg)
C*
C*    PROPERTIES:
C*    CpAir         Specific heat of air                        (J/kg C)
C*    CpVap         Specific heat of water vapor                (J/kg C)
C*    Hfg           Reference heat of vaporization of water       (J/kg)
C***********************************************************************
C     MAJOR RESTRICTIONS      Uses perfect gas relationships
C                             Fit for enthalpy of saturated water vapor
C
C     DEVELOPER:              Shauna Gabel
C                             Michael J. Brandemuehl, PhD, PE
C                             University of Colorado at Boulder
C
C     DATE:                   January 1, 1992
C
C     INCLUDE FILES:          PROP.INC
C     SUBROUTINES CALLED:     None
C     FUNCTIONS CALLED:       None
C
C     REVISION HISTORY:       None
C
C     REFERENCE:              1989 ASHRAE Handbook - Fundamentals
C***********************************************************************

C $INCLUDE: 'prop.inc'

C1*** Calculate the enthalpy as a function of dry bulb temperature and
C1*** humidity ratio.

      hDryAir = CpAir*TDB
      hSatVap = Hfg + CpVap*TDB
      Enthalpy = hDryAir + W*hSatVap

      RETURN 
      END

      REAL FUNCTION RHODRY (PATM,RAIR,TABSADD,TDB,W)
C***********************************************************************
C*    Copyright ASHRAE.  Toolkit for HVAC System Energy Calculations
C***********************************************************************
C*    FUNCTION:               RHODRY
C*
C*    LANGUAGE:               FORTRAN 77
C*
C*    PURPOSE:                Calculate dry air density.
C***********************************************************************
C*    INPUT VARIABLES
C*    TDB           Dry bulb temperature                             (C)
C*    W             Humidity ratio                                   (-)
C*
C*    OUTPUT VARIABLES
C*    RhoDry        Density of dry air                           (kg/m3)
C*
C*    PROPERTIES
C*    Patm          Atmospheric pressure                            (Pa)
C*    RAir          Gas constant for air                        (J/kg C)
C*    TAbsAdd       Additive constant to convert user T to absolute T
C***********************************************************************
C     MAJOR RESTRICTIONS:     Perfect gas relationships
C
C     DEVELOPER:              Shauna Gabel
C                             Michael J. Brandemuehl, PhD, PE
C                             University of Colorado at Boulder
C
C     DATE:                   January 1, 1992
C
C     INCLUDE FILES:          prop.inc
C     SUBROUTINES CALLED:     None
C     FUNCTIONS CALLED:       None
C
C     REVISION HISTORY:       None
C
C     REFERENCE:              1989 ASHRAE Handbook - Fundamentals
C***********************************************************************
C     INTERNAL VARIABLES:
C     pAir          Partial pressure of dry air                     (Pa)
C***********************************************************************

C $INCLUDE: 'prop.inc'

C1*** Calculate the dry air density from perfect gas laws.

      pAir = 0.62198*Patm/(0.62198+W)
      RhoDry = pAir/RAir/(TDB+TAbsAdd)

      RETURN 
      END
