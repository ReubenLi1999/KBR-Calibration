
      SUBROUTINE FSIZER3(NRECL,KSIZE,NRFILE,NAMFIL)
            use xml_module
 

C  THE SUBROUTINE SETS THE VALUES OF  NRECL, KSIZE, NRFILE, AND NAMFIL.

      SAVE

      CHARACTER*80 NAMFIL
      CHARACTER(len=:), allocatable                   :: config_path
      integer                                         :: fplerror

      fplerror = xml_i%urlpaths%GetAsString('ConfigPath', config_path)

C  *****************************************************************
C  *****************************************************************
C
C  THE PARAMETERS NRECL, NRFILE, AND NAMFIL ARE TO BE SET BY THE USER

C  *****************************************************************

C  NRECL=1 IF "RECL" IN THE OPEN STATEMENT IS THE RECORD LENGTH IN S.P. WORDS
C  NRECL=4 IF "RECL" IN THE OPEN STATEMENT IS THE RECORD LENGTH IN BYTES

      NRECL=1

C  *****************************************************************

C  NRFILE IS THE INTERNAL UNIT NUMBER USED FOR THE EPHEMERIS FILE (DEFAULT: 12)

      NRFILE=44

C  *****************************************************************

C  NAMFIL IS THE EXTERNAL NAME OF THE BINARY EPHEMERIS FILE

      NAMFIL=trim(config_path)//'JPLEPH'

C  *****************************************************************

C  KSIZE must be set by the user according to the ephemeris to be read

C  For  de200, set KSIZE to 1652
C  For  de405, set KSIZE to 2036
C  For  de406, set KSIZE to 1456
C  For  de414, set KSIZE to 2036
C  For  de418, set KSIZE to 2036
C  For  de421, set KSIZE to 2036
C  For  de422, set KSIZE to 2036
C  For  de423, set KSIZE to 2036
C  For  de424, set KSIZE to 2036
C  For  de430, set KSIZE to 2036

      KSIZE =2036

C  *******************************************************************

      RETURN

      END
 

        SUBROUTINE PLEPH ( ET, NTARG, NCENT, RRD)
C
C++++++++++++++++++++++++++

C
C                1 = MERCURY           8 = NEPTUNE
C                2 = VENUS             9 = PLUTO
C                3 = EARTH            10 = MOON
C                4 = MARS             11 = SUN
C                5 = JUPITER          12 = SOLAR-SYSTEM BARYCENTER
C                6 = SATURN           13 = EARTH-MOON BARYCENTER
C                7 = URANUS           14 = NUTATIONS (LONGITUDE AND OBLIQ)


C
C             (IF NUTATIONS ARE WANTED, SET NTARG = 14. FOR LIBRATIONS,
C              SET NTARG = 15. SET NCENT=0.)(如果需要章动，则设置NTARG = 14。
c              平动,设置NTARG = 15。设置NCENT = 0)。
C
C      RRD = OUTPUT 6-WORD D.P. ARRAY CONTAINING POSITION AND VELOCITY
C            OF POINT 'NTARG' RELATIVE TO 'NCENT'. THE UNITS ARE AU AND
C            AU/DAY. FOR LIBRATIONS THE UNITS ARE RADIANS AND RADIANS
C            PER DAY. IN THE CASE OF NUTATIONS THE FIRST FOUR WORDS OF
C            RRD WILL BE SET TO NUTATIONS AND RATES, HAVING UNITS OF
C            RADIANS AND RADIANS/DAY.
C
      use Vars_wlm
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      INTEGER NMAX
      PARAMETER (NMAX = 1000)

      DIMENSION RRD(6),ET2Z(2),ET2(2),PV(6,13)
      DIMENSION PVST(6,11),PNUT(4)
      DIMENSION ZIPS(2)!,SS(3),CVAL(NMAX),PVSUN(6)
      DATA ZIPS/2*0.d0/

      LOGICAL BSAVE!,KM,BARY
      LOGICAL FIRST
      DATA FIRST/.TRUE./

      INTEGER LIST(12)!, IPL(3,13)!,IPT(39),DENUM

      ! COMMON/EPHHDR/CVAL,SS,AU,EMRAT,DENUM,NCON,IPT

      ! COMMON/STCOMX/KM,BARY,PVSUN
       
C
      ET2(1)=ET
      ET2(2)=0.D0
      GO TO 11


      ENTRY DPLEPH(ET2Z,NTARG,NCENT,RRD)

      ET2(1)=ET2Z(1)
      ET2(2)=ET2Z(2)

  11  IF(FIRST) CALL STATE(ZIPS,LIST,PVST,PNUT)
      FIRST=.FALSE.

  96  IF(NTARG .EQ. NCENT) RETURN

      DO I=1,12
        LIST(I)=0
      ENDDO
C     CHECK FOR NUTATION CALL

      IF(NTARG.NE.14) GO TO 97
        IF(IPT(2, 12).GT.0) THEN !35
          LIST(11)=2
          CALL STATE(ET2,LIST,PVST,PNUT)
          DO I=1,4
            RRD(I)=PNUT(I)
          ENDDO
          RRD(5) = 0.d0
          RRD(6) = 0.d0
          RETURN
        ELSE
          DO I=1,4
            RRD(I)=0.d0
          ENDDO
          WRITE(6,297)
  297     FORMAT(' *****  NO NUTATIONS ON THE EPHEMERIS FILE  *****')
          STOP
        ENDIF


  97  CONTINUE
      DO I=1,6
        RRD(I)=0.d0
      ENDDO

      IF(NTARG.NE.15) GO TO 98
        IF(IPT(2, 13).GT.0) THEN ! 38
          LIST(12)=2
          CALL STATE(ET2,LIST,PVST,PNUT)
          DO I=1,6
            RRD(I)=PVST(I,11)
          ENDDO
          RETURN
        ELSE
          WRITE(6,298)
  298     FORMAT(' *****  NO LIBRATIONS ON THE EPHEMERIS FILE  *****')
          STOP
        ENDIF

C       FORCE BARYCENTRIC OUTPUT BY 'STATE' 

  98  BSAVE=BARY
      BARY=.TRUE.

C       SET UP PROPER ENTRIES IN 'LIST' ARRAY FOR STATE CALL

      DO I=1,2
        K=NTARG
        IF(I .EQ. 2) K=NCENT
        IF(K .LE. 10) LIST(K)=2
        IF(K .EQ. 10) LIST(3)=2
        IF(K .EQ. 3) LIST(10)=2
        IF(K .EQ. 13) LIST(3)=2
      ENDDO

C       MAKE CALL TO STATE

      CALL STATE(ET2,LIST,PVST,PNUT)

      DO I=1,10
        DO J = 1,6
          PV(J,I) = PVST(J,I)
        ENDDO
      ENDDO

      IF(NTARG .EQ. 11 .OR. NCENT .EQ. 11) THEN
      DO I=1,6
        PV(I,11)=PVSUN(I)
      ENDDO
      ENDIF

      IF(NTARG .EQ. 12 .OR. NCENT .EQ. 12) THEN
        DO I=1,6
          PV(I,12)=0.D0
        ENDDO
      ENDIF

      IF(NTARG .EQ. 13 .OR. NCENT .EQ. 13) THEN
        DO I=1,6
          PV(I,13) = PVST(I,3)
        ENDDO
      ENDIF

      IF(NTARG*NCENT .EQ. 30 .AND. NTARG+NCENT .EQ. 13) THEN
        DO I=1,6
          PV(I,3)=0.D0
        ENDDO
        GO TO 99
      ENDIF

      IF(LIST(3) .EQ. 2) THEN
        DO I=1,6
          PV(I,3)=PVST(I,3)-PVST(I,10)/(1.D0+EMRAT)
        ENDDO
      ENDIF

      IF(LIST(10) .EQ. 2) THEN
        DO I=1,6
          PV(I,10) = PV(I,3)+PVST(I,10)
        ENDDO
      ENDIF

  99  DO I=1,6
        RRD(I)=PV(I,NTARG)-PV(I,NCENT)
        
         
      ENDDO
      
     
      BARY=BSAVE
      
  
      RETURN
      
        
      
      END
 
C
      SUBROUTINE INTERP(BUF,T,NCF,NCM,NA,IFL,PV)
 
C

C         BUF   1ST LOCATION OF ARRAY OF D.P. CHEBYSHEV COEFFICIENTS OF POSITION
C
C           T   T(1) IS DP FRACTIONAL TIME IN INTERVAL COVERED BY
C               COEFFICIENTS AT WHICH INTERPOLATION IS WANTED
C               (0 .LE. T(1) .LE. 1).  T(2) IS DP LENGTH OF WHOLE
C               INTERVAL IN INPUT TIME UNITS.
C
C         NCF   # OF COEFFICIENTS PER COMPONENT
C
C         NCM   # OF COMPONENTS PER SET OF COEFFICIENTS
C
C          NA   # OF SETS OF COEFFICIENTS IN FULL ARRAY
C               (I.E., # OF SUB-INTERVALS IN FULL INTERVAL)
C
C          IFL  INTEGER FLAG: =1 FOR POSITIONS ONLY
C                             =2 FOR POS AND VEL
C

C      
C
C       OUTPUT:
C
C         PV   INTERPOLATED QUANTITIES REQUESTED.  DIMENSION
C               EXPECTED IS PV(NCM,IFL), DP.
C
C
C
C      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      SAVE
C
      DOUBLE PRECISION BUF(NCF,NCM,*),T(2),PV(NCM,*),PC(18),VC(18)

C
      DATA NP/2/
      DATA NV/3/
      DATA TWOT/0.D0/
      DATA PC(1),PC(2)/1.D0,0.D0/
      DATA VC(2)/1.D0/
C
C
      DNA=DBLE(NA)
      DT1=DINT(T(1))
      TEMP=DNA*T(1)
      L=IDINT(TEMP-DT1)+1

C         TC IS THE NORMALIZED CHEBYSHEV TIME (-1 .LE. TC .LE. 1)

      TC=2.D0*(DMOD(TEMP,1.D0)+DT1)-1.D0



      IF(TC.NE.PC(2)) THEN
        NP=2
        NV=3
        PC(2)=TC
        TWOT=TC+TC
      ENDIF

C
      IF(NP.LT.NCF) THEN
        DO 1 I=NP+1,NCF
        PC(I)=TWOT*PC(I-1)-PC(I-2)
    1   CONTINUE
        NP=NCF
      ENDIF
C

C
      DO 2 I=1,NCM
      PV(I,1)=0.D0
      DO 3 J=NCF,1,-1
      PV(I,1)=PV(I,1)+PC(J)*BUF(J,I,L)
    3 CONTINUE
    2 CONTINUE
      IF(IFL.LE.1) RETURN
C

      VFAC=(DNA+DNA)/T(2)
      VC(3)=TWOT+TWOT
      IF(NV.LT.NCF) THEN
        DO 4 I=NV+1,NCF
        VC(I)=TWOT*VC(I-1)+PC(I-1)+PC(I-1)-VC(I-2)
    4   CONTINUE
        NV=NCF
      ENDIF
C

C
      DO 5 I=1,NCM
      PV(I,2)=0.D0
      DO 6 J=NCF,2,-1
      PV(I,2)=PV(I,2)+VC(J)*BUF(J,I,L)
    6 CONTINUE
      PV(I,2)=PV(I,2)*VFAC
    5 CONTINUE
C
      RETURN
C
      END

C
      SUBROUTINE SPLIT(TT,FR)
C
C+++++++++++++++++++++++++

C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      DIMENSION FR(2)



      FR(1)=DINT(TT)
      FR(2)=TT-FR(1)

      IF(TT.GE.0.D0 .OR. FR(2).EQ.0.D0) RETURN


      FR(1)=FR(1)-1.D0
      FR(2)=FR(2)+1.D0

      RETURN

      END
c
      SUBROUTINE STATE(ET2,LIST,PV,PNUT)
C

C        LIST   12-WORD INTEGER ARRAY SPECIFYING WHAT INTERPOLATION
C               IS WANTED FOR EACH OF THE BODIES ON THE FILE.
C
C                         LIST(I)=0, NO INTERPOLATION FOR BODY I
C                                =1, POSITION ONLY
C                                =2, POSITION AND VELOCITY
C
C               THE DESIGNATION OF THE ASTRONOMICAL BODIES BY I IS:
C
C                         I = 1: MERCURY
C                           = 2: VENUS
C                           = 3: EARTH-MOON BARYCENTER
C                           = 4: MARS
C                           = 5: JUPITER
C                           = 6: SATURN
C                           = 7: URANUS
C                           = 8: NEPTUNE
C                           = 9: PLUTO
C                           =10: GEOCENTRIC MOON
C                           =11: NUTATIONS IN LONGITUDE AND OBLIQUITY

C     OUTPUT:
C
C          PV   DP 6 X 11 ARRAY THAT WILL CONTAIN REQUESTED INTERPOLATED
C               QUANTITIES (OTHER THAN NUTATION, STOERD IN PNUT).  
C               THE BODY SPECIFIED BY LIST(I) WILL HAVE ITS
C               STATE IN THE ARRAY STARTING AT PV(1,I).  
C               (ON ANY GIVEN CALL, ONLY THOSE WORDS IN 'PV' WHICH ARE 
C                AFFECTED BY THE  FIRST 10 'LIST' ENTRIES, AND BY LIST(12)
C                IF LIBRATIONS ARE ON THE FILE, ARE SET.  
C                THE REST OF THE 'PV' ARRAYIS UNTOUCHED.)  
C               THE ORDER OF COMPONENTS STARTING IN PV(1,I) IS: X,Y,Z,DX,DY,DZ.
C输出:
C
c  pv DP 6x11阵列，将包含所请求的内插
C  量(除了章动，PNUT中的STOERD)。
C   列表(I)中指定的主体将有它的
C  表示数组中从PV(1,I)开始的状态。
C  (在任何给定的调用中，只有“PV”中的单词是
C   受前10个“列表”条目影响，并受列表影响(12)如果文件上有库，则设置。
C  “PV”数组的其余部分未受影响。)
C   从PV(1,i)开始的分量的顺序是:X,Y,Z,DX,DY,DZ。
C
C               ALL OUTPUT VECTORS ARE REFERENCED TO THE EARTH MEAN
C               EQUATOR AND EQUINOX OF J2000 IF THE DE NUMBER IS 200 OR
C               GREATER; OF B1950 IF THE DE NUMBER IS LESS THAN 200. 
C
C               THE MOON STATE IS ALWAYS GEOCENTRIC; THE OTHER NINE STATES 
C               ARE EITHER HELIOCENTRIC OR SOLAR-SYSTEM BARYCENTRIC, 
C               DEPENDING ON THE SETTING OF COMMON FLAGS (SEE BELOW).
C               DEPENDING ON THE SETTING OF LIST(11).  THE ORDER OF
C               QUANTITIES IN NUT IS:
C
C                        D PSI  (NUTATION IN LONGITUDE)
C                        D EPSILON (NUTATION IN OBLIQUITY)
C                        D PSI DOT
C                        D EPSILON DOT
C
C           *   STATEMENT # FOR ERROR RETURN, IN CASE OF EPOCH OUT OF
C               RANGE OR I/O ERRORS.
 
C     COMMON AREA STCOMX:
C
C          KM   LOGICAL FLAG DEFINING PHYSICAL UNITS OF THE OUTPUT
C               STATES. KM = .TRUE., KM AND KM/SEC
C                          = .FALSE., AU AND AU/DAY
C               DEFAULT VALUE = .FALSE.  (KM DETERMINES TIME UNIT
C               FOR NUTATIONS AND LIBRATIONS.  ANGLE UNIT IS ALWAYS RADIANS.)
C
C        BARY   LOGICAL FLAG DEFINING OUTPUT CENTER.
C               ONLY THE 9 PLANETS ARE AFFECTED.
C                        BARY = .TRUE. =\ CENTER IS SOLAR-SYSTEM BARYCENTER
C                             = .FALSE. =\ CENTER IS SUN
C               DEFAULT VALUE = .FALSE.
C
C       PVSUN   DP 6-WORD ARRAY CONTAINING THE BARYCENTRIC POSITION AND
C                VELOCITY OF THE SUN.
C   公共区域STCOMX:
C   定义输出物理单位的逻辑KM标志
C   状态。公里= .TRUE. KM和KM/ s
C   = .FALSE.， AU和AU/DAY
C   默认值= . false。(KM决定时间单位
C   表示章动和振动。角度单位总是弧度。
C    BARY逻辑标志定义输出中心。
C   只有9颗行星受到影响。
C B   ARY = . true。=\ CENTER是太阳系的重心
C    = .FALSE。=\中心是太阳
C    默认值= . false。
C
C    PVSUN DP 6字数组包含重心位置和
C   太阳的速度。
C
      use Vars_wlm
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      SAVE

      INTEGER OLDMAX
      PARAMETER ( OLDMAX = 400)
      INTEGER NMAX
      PARAMETER ( NMAX = 1000)
      real(kind=8) :: buf
      DIMENSION ET2(2),PV(6,11),PNUT(4),T(2),PJD(4),BUF(1500)!,
!      . SS(3),CVAL(NMAX)!,PVSUN(6)

      INTEGER LIST(12)!,IPT(3,13)!,IPT(3,13)

      LOGICAL FIRST
      DATA FIRST/.TRUE./

      ! CHARACTER*6 TTL(14,3),CNAM(NMAX)
      CHARACTER*80 NAMFIL
      

      !LOGICAL KM,BARY

      ! COMMON/EPHHDR/CVAL,SS,AU,EMRAT,NUMDE,NCON,IPT
      ! COMMON/EPHHDR/CVAL,SS,AU,EMRAT,DENUM,NCON,IPT
      NUMDE = DENUM
      ! COMMON/CHRHDR/CNAM,TTL
      !COMMON/STCOMX/KM,BARY,PVSUN

C
C       ENTRY POINT - 1ST TIME IN, GET POINTER DATA, ETC., FROM EPH FILE
C进入点-第一次进入，从EPH文件中获取指针数据等
      IF(FIRST) THEN
        FIRST=.FALSE.

C ************************************************************************
C ************************************************************************

C THE USER MUST SELECT ONE OF THE FOLLOWING BY DELETING THE 'C' IN COLUMN 1
c   用户必须通过删除列1中的“C”来选择以下选项之一
C ************************************************************************

C        CALL FSIZER1(NRECL,KSIZE,NRFILE,NAMFIL)
C        CALL FSIZER2(NRECL,KSIZE,NRFILE,NAMFIL)
         CALL FSIZER3(NRECL,KSIZE,NRFILE,NAMFIL)

      IF(NRECL .EQ. 0) WRITE(*,*)'  ***** FSIZER IS NOT WORKING *****'

C ************************************************************************
C ************************************************************************

      IRECSZ=NRECL*KSIZE
      NCOEFFS=KSIZE/2

        OPEN(NRFILE,
     *       FILE=NAMFIL,
     *       ACCESS='DIRECT',
     *       FORM='UNFORMATTED',
     *       RECL=IRECSZ,
     *       STATUS='OLD')

      READ(NRFILE,REC=1)TTL,(CNAM(K),K=1,OLDMAX),SS,NCON,AU,EMRAT,
     & ((IPT(I,J),I=1,3),J=1,12),NUMDE,(IPT(I,13),I=1,3)
     & ,(CNAM(L),L=OLDMAX+1,NCON)

      IF(NCON .LE. OLDMAX)THEN
        READ(NRFILE,REC=2)(CVAL(I),I=1,OLDMAX)
      ELSE
        READ(NRFILE,REC=2)(CVAL(I),I=1,NCON)
      ENDIF
      NRL=0

    
      ENDIF
C       ********** MAIN ENTRY POINT **********

      IF(ET2(1) .EQ. 0.D0) RETURN

      S=ET2(1)-.5D0
      CALL SPLIT(S,PJD(1))
      CALL SPLIT(ET2(2),PJD(3))
      PJD(1)=PJD(1)+PJD(3)+.5D0
      PJD(2)=PJD(2)+PJD(4)
      CALL SPLIT(PJD(2),PJD(3))
      PJD(1)=PJD(1)+PJD(3)

C       ERROR RETURN FOR EPOCH OUT OF RANGE
c历元错误返回超出范围

      IF(PJD(1)+PJD(4).LT.SS(1) .OR. PJD(1)+PJD(4).GT.SS(2)) GO TO 98

C       CALCULATE RECORD # AND RELATIVE TIME IN INTERVAL
c 计算记录号和间隔中的相对时间
      NR=IDINT((PJD(1)-SS(1))/SS(3))+3
      IF(PJD(1).EQ.SS(2)) NR=NR-1

        tmp1 = DBLE(NR-3)*SS(3) + SS(1)
        tmp2 = PJD(1) - tmp1
        T(1) = (tmp2 + PJD(4))/SS(3)

C       READ CORRECT RECORD IF NOT IN CORE
c读取正确的记录，如果不是在核心
      IF(NR.NE.NRL) THEN
        NRL=NR
        READ(NRFILE,REC=NR,ERR=99)(BUF(K),K=1,NCOEFFS)
      ENDIF

      IF(KM) THEN
      T(2)=SS(3)*86400.D0
      AUFAC=1.D0
      ELSE
      T(2)=SS(3)
      AUFAC=1.D0/AU
      ENDIF

C   INTERPOLATE SSBARY SUN插入SSBARY太阳

      CALL INTERP(BUF(IPT(1,11)), T, IPT(2,11),3,IPT(3,11), 2, PVSUN)
 
      DO I=1,6
      PVSUN(I)=PVSUN(I)*AUFAC
      ENDDO

C   CHECK AND INTERPOLATE WHICHEVER BODIES ARE REQUESTED
c检查并插入任何被要求的主体
      DO 4 I=1,10
      IF(LIST(I).EQ.0) GO TO 4

      CALL INTERP(BUF(IPT(1,I)),T,IPT(2,I),3,IPT(3,I),
     & LIST(I),PV(1,I))

      DO J=1,6
       IF(I.LE.9 .AND. .NOT.BARY) THEN
       PV(J,I)=PV(J,I)*AUFAC-PVSUN(J)
       ELSE
       PV(J,I)=PV(J,I)*AUFAC
       ENDIF
       !open(7,file='outdata')
       !write(7,*) list(i),pv(j,i)
      ENDDO

   4  CONTINUE

C       DO NUTATIONS IF REQUESTED (AND IF ON FILE)
c按要求做章动(如果已存档)
      IF(LIST(11).GT.0 .AND. IPT(2,12).GT.0)
     * CALL INTERP(BUF(IPT(1,12)),T,IPT(2,12),2,IPT(3,12),
     * LIST(11),PNUT)

C       GET LIBRATIONS IF REQUESTED (AND IF ON FILE)
c如果被请求(如果在文件中)，获取库
      IF(LIST(12).GT.0 .AND. IPT(2,13).GT.0)
     * CALL INTERP(BUF(IPT(1,13)),T,IPT(2,13),3,IPT(3,13),
     * LIST(12),PV(1,11))

      RETURN

  98  WRITE(*,198)ET2(1)+ET2(2),SS(1),SS(2)
 198  FORMAT(' ***  Requested JED,',f12.2,
     * ' not within ephemeris limits,',2f12.2,'  ***')

      STOP

   99 WRITE(*,'(2F12.2,A80)')ET2,'ERROR RETURN IN STATE'
      
      

      STOP


      
      END
 
C
      SUBROUTINE CONST(NAM,VAL,SSS,N)
C
C+++++++++++++++++++++++++++++
C
C     THIS ENTRY OBTAINS THE CONSTANTS FROM THE EPHEMERIS FILE
C
C     CALLING SEQEUNCE PARAMETERS (ALL OUTPUT):
C
C       NAM = CHARACTER*6 ARRAY OF CONSTANT NAMES
C
C       VAL = D.P. ARRAY OF VALUES OF CONSTANTS
C
C       SSS = D.P. JD START, JD STOP, STEP OF EPHEMERIS
C
C         N = INTEGER NUMBER OF ENTRIES IN 'NAM' AND 'VAL' ARRAYS
C    这个条目从星历文件中获取常量
C
C   调用SEQEUNCE参数(全部输出):
C
C    NAM =字符*6数组的常量名称
C    常量值的数组
C
C   SSS = D.P. JD开始，JD停止，星历的脚步
C
C    N = 'NAM'和'VAL'数组中条目的整数
C
      use Vars_wlm
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      SAVE

      INTEGER NMAX
      PARAMETER (NMAX = 1000)

      CHARACTER*6 NAM(*)!,TTL(14,3),CNAM(NMAX)

      DOUBLE PRECISION VAL(*),SSS(3),ZIPS(2)!,SS(3),CVAL(NMAX)
      DOUBLE PRECISION PVST(6,11),PNUT(4)
      DATA ZIPS/2*0.d0/

      INTEGER LIST(12)!,IPT(3,13)!IPT(3,13),DENUM
      logical first
      data first/.true./

      ! COMMON/EPHHDR/CVAL,SS,AU,EMRAT,DENUM,NCON,IPT
      ! COMMON/CHRHDR/CNAM,TTL

C  CALL STATE TO INITIALIZE THE EPHEMERIS AND READ IN THE CONSTANTS
c   调用状态初始化星历并读取常量
      IF(FIRST) CALL STATE(ZIPS,LIST,PVST,PNUT)
      first=.false.

      N=NCON

      DO I=1,3
        SSS(I)=SS(I)
      ENDDO

      DO I=1,N
        NAM(I)=CNAM(I)
        VAL(I)=CVAL(I)
         
        write(16,*) nam(i),val(i)
      ENDDO


      RETURN

      END

       subroutine outputdata( ET, NTARG, NCENT) 
         
       INTEGER  NTARG,NCTR 
        DOUBLE PRECISION  ET
        DOUBLE  PRECISION  RRD(6)
        call PLEPH ( ET, NTARG, NCENT, RRD )
       write(15,*) ET,NTARG,NCENT    !输出儒略星历日期,目标星体编号，中心天体编号
       WRITE(15,*) RRD(1),RRD(2),RRD(3),RRD(4),RRD(5),RRD(6) 
       end
