C **********************************************************************
C
C	          MINCOST GRID PROBLEM GENERATOR
C	              BY DIMITRI P. BERTSEKAS
C                        NOVEMBER 1994 
C
C **********************************************************************
C
C     THIS CODE GENERATES A GRID MINCOST PROBLEM AND WRITES THE 
C     PROBLEM DATA IN STANDARD FORMAT TO THE ASCII FILE "RELAX4.INP".
C
C
      IMPLICIT INTEGER (A-Z)
      LOGICAL MARK
      DIMENSION STARTN(100000),ENDN(100000),C(100000),U(100000)
      DIMENSION B(15000)
      DIMENSION MARK(15000)


C     THIS CODE INCLUDES A UNIFORM RANDOM NUMBER GENERATOR 
C     WHICH RETURNS A VALUE IN (0,1)
 
C   RANDOM GENERATOR STUFF
 
      COMMON /RANDM/ MULT,MODUL,I15,I16,JRAN                              
      REAL RRAN
 
C     INITIALIZE RANDOM GENERATOR
 
      ISEED=13502460                                     
      CALL SETRAN(ISEED)
      
      PRINT*,'CREATING A 2-D GRID MINCOST FLOW PROBLEM GRAPH'
      PRINT*,'**********************************************'
5     PRINT*,'ENTER # OF NODES IN EACH OF THE TWO DIMENSIONS'
      READ*,DIM1,DIM2
      
      IF (DIM1.LT.1) THEN 
        PRINT*,'DIMENSION 1 MUST BE GREATER OR EQUAL TO 1'
	GO TO 5
      END IF
      IF (DIM2.LT.1) THEN 
        PRINT*,'DIMENSION 2 MUST BE GREATER OR EQUAL TO 1'
	GO TO 5
      END IF
      
      
      N=DIM1*DIM2

      PRINT*,'TOTAL SUPPLY,# OF SOURCES,# OF SINKS'
      READ*,TOTSUPPLY,SOURCES,SINKS
      
      PRINT*,'ENTER MINIMUM AND MAXIMUM COST COEFFICIENT'
      READ*,MINCOST,MAXCOST

      PRINT*,'ENTER MIN AND MAX ARC CAPACITY'
      READ*,MINCAP,MAXCAP

      

      ARC=0
      
      DO 10 J=1,DIM2
      DO 15 I=1,DIM1
        NODE=I+(J-1)*DIM1

C       CREATE HORIZONTAL ARCS

	IF ((I.GT.1).AND.(I.LT.DIM1)) THEN
	  ARC=ARC+1
	  STARTN(ARC)=NODE
	  ENDN(ARC)=NODE+1
	  C(ARC)=MAXCOST
          U(ARC)=TOTSUPPLY
	  
	  ARC=ARC+1
	  STARTN(ARC)=NODE
	  ENDN(ARC)=NODE-1
	  C(ARC)=MAXCOST
          U(ARC)=TOTSUPPLY
	ELSE
	  IF (DIM1.GT.1) THEN
	    IF (I.EQ.1) THEN
	      ARC=ARC+1
	      STARTN(ARC)=NODE
	      ENDN(ARC)=NODE+1
	      C(ARC)=MAXCOST
	      U(ARC)=TOTSUPPLY
	      
	      ARC=ARC+1
	      STARTN(ARC)=NODE
	      ENDN(ARC)=NODE+(DIM1-1)
	      C(ARC)=MAXCOST
	      U(ARC)=TOTSUPPLY
	    ELSE
	      ARC=ARC+1
	      STARTN(ARC)=NODE
	      ENDN(ARC)=NODE-(DIM1-1)
	      C(ARC)=MAXCOST
	      U(ARC)=TOTSUPPLY
	      
	      ARC=ARC+1
	      STARTN(ARC)=NODE
	      ENDN(ARC)=NODE-1
	      C(ARC)=MAXCOST
	      U(ARC)=TOTSUPPLY
	    END IF
	  END IF
	END IF
	  
	  

C       CREATE VERTICAL ARCS


	IF ((J.GT.1).AND.(J.LT.DIM2)) THEN
	  ARC=ARC+1
	  STARTN(ARC)=NODE
	  ENDN(ARC)=NODE+DIM1
	  C(ARC)=MAXCOST
          U(ARC)=TOTSUPPLY
	  
	  ARC=ARC+1
	  STARTN(ARC)=NODE
	  ENDN(ARC)=NODE-DIM1
	  C(ARC)=MAXCOST
          U(ARC)=TOTSUPPLY
	ELSE
	  IF (DIM2.GT.1) THEN
	    IF (J.EQ.1) THEN
	      ARC=ARC+1
	      STARTN(ARC)=NODE
	      ENDN(ARC)=NODE+DIM1
	      C(ARC)=MAXCOST
	      U(ARC)=TOTSUPPLY
	      
	      ARC=ARC+1
	      STARTN(ARC)=NODE
	      ENDN(ARC)=NODE+DIM1*(DIM2-1)
	      C(ARC)=MAXCOST
	      U(ARC)=TOTSUPPLY
	    ELSE
	      ARC=ARC+1
	      STARTN(ARC)=NODE
	      ENDN(ARC)=I
	      C(ARC)=MAXCOST
	      U(ARC)=TOTSUPPLY
	      
	      ARC=ARC+1
	      STARTN(ARC)=NODE
	      ENDN(ARC)=NODE-DIM1
	      C(ARC)=MAXCOST
	      U(ARC)=TOTSUPPLY
	    END IF
	  END IF
	END IF
	  

C       CREATE 2 ARCS WITH RANDOM END NODE
      
        DO 8 K=1,2

	  ARC=ARC+1
	  STARTN(ARC)=NODE
7	  ENDN(ARC)=1+INT(RRAN()*N)
	  C(ARC)=MINCOST+RRAN()*(MAXCOST-MINCOST)
	  U(ARC)=MINCAP+RRAN()*(MAXCAP-MINCAP)
	  IF (ENDN(ARC).EQ.NODE) GOTO 7
8       CONTINUE

15    CONTINUE
10    CONTINUE

      PRINT*,'NUMBER OF ARCS SO FAR ',ARC
      PRINT*,'ENTER # OF ADDITIONAL ARCS'
      READ*,ADDARCS
      
C       CREATE ADDARCS NEW ARCS WITH RANDOM START AND END NODE
      
      IF (ADDARCS.GT.0) THEN
        DO 12 K=1,ADDARCS
	  ARC=ARC+1
	  NODE=1+INT(RRAN()*N)
	  STARTN(ARC)=NODE
13	  ENDN(ARC)=1+INT(RRAN()*N)
	  C(ARC)=MINCOST+RRAN()*(MAXCOST-MINCOST)
	  U(ARC)=MINCAP+RRAN()*(MAXCAP-MINCAP)
	  IF (ENDN(ARC).EQ.NODE) GOTO 13
12      CONTINUE
      END IF
      
      NA=ARC


C    GENERATE THE SINKS AND SOURCES

      DO 17 I=1,N
        B(I)=0
        MARK(I)=.FALSE.
17    CONTINUE
      
      SUPL=0
      DO 20 I=1,SOURCES
18	  NODE=1+INT(RRAN()*N)
	  IF (MARK(NODE)) THEN
	    GOTO 18
	  ELSE
	    MARK(NODE)=.TRUE.
	    B(NODE)=INT(TOTSUPPLY/SOURCES)
	    SUPL=SUPL+B(NODE)
	    LAST=NODE
	  END IF
20    CONTINUE

      B(LAST)=B(LAST)+TOTSUPPLY-SUPL
      SUPL=TOTSUPPLY

      DEM=0
      DO 25 I=1,SINKS
22	  NODE=1+INT(RRAN()*N)
	  IF (MARK(NODE)) THEN
	    GOTO 22
	  ELSE
	    MARK(NODE)=.TRUE.
	    B(NODE)=-INT(TOTSUPPLY/SINKS)
	    DEM=DEM-B(NODE)
	  END IF
25    CONTINUE


C     EQUALIZE SUPPLY AND DEMAND

      IF (SUPL.GT.DEM) THEN
        DO 30 NODE=1,N
	  IF (B(NODE).LT.0) THEN
	    B(NODE)=B(NODE)-(SUPL-DEM)
	    GOTO 38
	  END IF
30      CONTINUE
       ELSE
	 IF (SUPL.LT.DEM) THEN
	   DO 35 NODE=1,N
	     IF (B(NODE).GT.0) THEN
	       B(NODE)=B(NODE)+(DEM-SUPL)
	       GOTO 38
	     END IF
35         CONTINUE
         END IF
       END IF
       
38     CONTINUE

C
      PRINT*,'**********************************************'
      PRINT*,'WRITING THE PROBLEM ON FILE RELAX4.INP'
      OPEN(13,FILE='RELAX4.INP',STATUS='NEW')
      REWIND(13)
      
C     WRITE NUMBER OF NODES AND ARCS

      WRITE(13,1010) N,NA
    
C     WRITE START, END, COST, AND CAPACITY OF EACH ARC

      DO 40 I=1,NA
        WRITE(13,1020) STARTN(I),ENDN(I),C(I),U(I)     
40    CONTINUE
    
C     WRITE SUPPLY OF EACH NODE

      DO 50 I=1,N
        WRITE(13,1000) B(I)
50    CONTINUE

      ENDFILE(13)
      REWIND(13)

      PRINT*,'END OF WRITING'

1000  FORMAT(1I8)
1010  FORMAT(2I8)
1020  FORMAT(4I8)

      STOP
      END

      SUBROUTINE SETRAN(ISEED)                                          
      IMPLICIT REAL*8 (A-H,O-Z) , INTEGER*4 (I-N)                       
C***********************************************************************
C PORTABLE CONGRUENTIAL (UNIFORM) RANDOM NUMBER GENERATOR:              
C     NEXT_VALUE = [(7**5) * PREVIOUS_VALUE] MODULO[(2**31)-1]          
C                                                                       
C THIS GENERATOR CONSISTS OF TWO ROUTINES:                            
C   (1) SETRAN - INITIALIZES CONSTANTS AND SEED                         
C   (2) RRAN   - GENERATES A REAL RANDOM NUMBER                         
C                                                                       
C THE GENERATOR REQUIRES A MACHINE WITH AT LEAST 32 BITS OF PRECISION.  
C THE SEED (ISEED) MUST BE IN THE RANGE (1,(2**31)-1).                  
C***********************************************************************
      COMMON /RANDM/ MULT,MODUL,I15,I16,JRAN                              
      IF(ISEED.LT.1) STOP 77                                            
      MULT=16807                                                        
      MODUL=2147483647                                                  
      I15=2**15                                                         
      I16=2**16                                                         
      JRAN=ISEED                                                        
      RETURN                                                            
      END                                                               
C                                                                       
                                               
      REAL FUNCTION RRAN()                                         
      IMPLICIT REAL*4 (A-H,O-Z) , INTEGER*4 (I-N)                       
C***********************************************************************
C RRAN GENERATES A REAL RANDOM NUMBER BETWEEN 0 AND 1                  
C***********************************************************************
      COMMON /RANDM/ MULT,MODUL,I15,I16,JRAN                              
      IXHI=JRAN/I16                                                    
      IXLO=JRAN-IXHI*I16  
      IXALO=IXLO*MULT                                                   
      LEFTLO=IXALO/I16                                                  
      IXAHI=IXHI*MULT                                                   
      IFULHI=IXAHI+LEFTLO                                               
      IRTLO=IXALO-LEFTLO*I16                                            
      IOVER=IFULHI/I15                                                  
      IRTHI=IFULHI-IOVER*I15                                            
      JRAN=((IRTLO-MODUL)+IRTHI*I16)+IOVER                              
      IF(JRAN.LT.0) JRAN=JRAN+MODUL
      RRAN = FLOAT(JRAN)/FLOAT(MODUL) 
      RETURN                                                            
      END                                                               

