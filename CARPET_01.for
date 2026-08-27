$DEBUG
      PROGRAM CARPET_MAN DGU 2026
**********************************************************************
*                                                                    *
*        CARPET_MAN DGU: ���������� ���������� �����������           *
*					   ����������� �� ��������� ������ ���	       *
*                        �������-���� 2026                           *
*                                                                    *
**********************************************************************
      PARAMETER(NJ=100,N_SKOKA=10+NJ*10)

	COMMON/JJJ/	J_HEAT,J_NAPRIAM,J_FRONT,J_FLOW(NJ),J_MET
	COMMON/TRUBA/ DZ,DR,PW_P,A_P,D_P,ZZZ,  
	*              RO_LO,DM_DT,ALF_EXP,TW_MID,TL1,U_LO
	COMMON/VARIABLE/ PPPT(NJ+1),PPPP(NJ+1),FI_P(NJ),X_P(NJ),DX_P(NJ),
	*TW1(NJ+1),TW2(NJ+1),TW3(NJ+1),TW4(NJ+1),
	*UL(NJ),TL(NJ+1),ROL(NJ+1), !GL_P(NJ),
     *UV(NJ),TV(NJ+1),ROV(NJ+1), !,GV_P(NJ)
     *ALF_INT(NJ),T1KR(NJ+1),DU_V(NJ),T2KR(NJ+1),D_FILM(NJ)

	COMMON/ZAGAL/ TROM(53),PROM(30),ROM(53,30),
	*              TCPM(53),PCPM(30),CPM(53,30),
	*              TLAM(53),PLAM(30),LAM(53,30),
     *              TNUM(53),PNUM(30),NUM(53,30),
	*              TSIM(39),SIM(39),
 	*              PRM(30),RM(30),TSM(30),
     *              TFEM(3),ROFEM(3),CFEM(3),LAFEM(3),
     *              TCUM(16),ROCUM(16),CCUM(16),LACUM(16)
     	COMMON/KLAPA/ TKL_M(4),AKL_M(4)
	COMMON/ENVIR/ P_TO,DP_P,P_ENV,AJO,AJT,AKSIO,AKSIT

	COMMON/SWITCH/ TSL(NJ+1),WE_P(NJ),WE_PD(NJ),TV_T(NJ+1),
	*D_FILM_T(NJ+1) 

	COMMON/PRINT/ GGG,Z_FR
**********************************************************************
      REAL*8 Y(N_SKOKA),DY(N_SKOKA),YK(N_SKOKA),DYK(4,N_SKOKA)
					
	REAL TROM,PROM,ROM,	 TNUM,PNUM,NUM,	TSIM,SIM, PRM,RM,TSM, 
	*TFEM,ROFEM,CFEM,LAFEM, TCUM,ROCUM,CCUM,LACUM, LAM

	REAL Z_FRONT(NJ),
     *T6(8),PT6(8,NJ),FIT6(8,NJ),XT6(8,NJ),GLT6(8,NJ),GVT6(8,NJ),
	*TZFRM(NJ),TZLM(NJ)
	INTEGER
     *J_SPEKTR_FLOW_1(NJ),J_SPEKTR_FLOW_2(NJ),J_SPEKTR_FLOW_3(NJ),
     *J_SPEKTR_FLOW_4(NJ),J_SPEKTR_FLOW_5(NJ),J_SPEKTR_FLOW_6(NJ),
     *J_SPEKTR_FLOW_7(NJ)
	INTEGER NOM(10)
      DATA NOM/1,2,3,4,5,6,7,8,9,10/

      CHARACTER PPP*130
**********************************************************************
C6      PP=(0.,1.)
      PI=3.14159
	G0=9.81

      OPEN(1,FILE='CARPET_01.TXT')
      OPEN(2,FILE='KONTROL.TXT')
      OPEN(3,FILE='CARPET_zagal_01.TXT')

      OPEN(4,FILE='00_ITOG_MAIN.TXT')
      OPEN(5,FILE='01_ITOG_MAIN_J10.TXT')
      OPEN(6,FILE='02_ITOG_MAIN_J30.TXT')
      OPEN(7,FILE='03_ITOG_MAIN_J50.TXT')
      OPEN(8,FILE='04_ITOG_MAIN_J70.TXT')
      OPEN(9,FILE='05_ITOG_MAIN_J90.TXT')
      OPEN(10,FILE='06_ITOG_P_FI_X_GL_GV.TXT')
      OPEN(11,FILE='07_ITOG_FRONT.TXT')
      OPEN(12,FILE='08_ITOG_TW_1234.TXT')
      OPEN(13,FILE='09_ITOG_FLOW_7.TXT')
************** ���� �������� ������ ������ ���������� ****************
      READ  (3,100) PPP
      WRITE (2,100) PPP
******************* ROM(53,30),��/�3	 TROM(53),�     PROM(30),���
      READ  (3,100) PPP
      WRITE (2,100) PPP

      READ  (3,100) PPP
      WRITE (2,100) PPP

	READ (3,*) (PROM(I),I=1,30)
      WRITE(2,'(7X,30F9.2)') (PROM(I),I=1,30)
	DO 461 J=1,53
	READ (3,*) TROM(J),(ROM(J,I),I=1,30)
461	WRITE(2,'(F7.1,30E9.2)')  TROM(J),(ROM(J,I),I=1,30)
******************* CPM(53,30),��/(�� �)=�2/(�2 �)	TCPM(53),�     PCPM(30),���
      READ  (3,100) PPP
      WRITE (2,100) PPP

      READ  (3,100) PPP
      WRITE (2,100) PPP

	READ (3,*) (PCPM(I),I=1,30)
      WRITE(2,'(7X,30F9.2)') (PCPM(I),I=1,30)
	DO 4611 J=1,53
	READ (3,*) TCPM(J),(CPM(J,I),I=1,30)
4611	WRITE(2,'(F7.1,30E9.2)')  TCPM(J),(CPM(J,I),I=1,30)
******************* LAM(53,30),��/(� �) TLAM(53),�     PLAM(30),���
      READ  (3,100) PPP
      WRITE (2,100) PPP

      READ  (3,100) PPP
      WRITE (2,100) PPP

	READ (3,*) (PLAM(I),I=1,30)
      WRITE(2,'(7X,30F9.2)') (PLAM(I),I=1,30)
	DO 4612 J=1,53
	READ (3,*) TLAM(J),(LAM(J,I),I=1,30)
4612	WRITE(2,'(F7.1,30E9.2)')  TLAM(J),(LAM(J,I),I=1,30)
*******************  NUM(53,30),�2/�    TNUM(53),�     PNUM(30),���
      READ  (3,100) PPP
      WRITE (2,100) PPP

      READ  (3,100) PPP
      WRITE (2,100) PPP

	READ (3,*) (PNUM(I),I=1,30)
      WRITE(2,'(7X,30F10.2)') (PNUM(I),I=1,30)
	DO 462 J=1,53
	READ (3,*) TNUM(J),(NUM(J,I),I=1,30)
462	WRITE(2,'(F7.1,30E10.3)')  TNUM(J),(NUM(J,I),I=1,30)		 
******************* TSIM(39),K   SIM(39),�/�
      READ  (3,100) PPP
      WRITE (2,100) PPP

      READ  (3,100) PPP
      WRITE (2,100) PPP

      READ  (3,100) PPP
      WRITE (2,100) PPP

	DO 463 J=1,39
	READ (3,*) TSIM(J),SIM(J)
463	WRITE(2,'(F6.1,E11.3)')  TSIM(J),SIM(J)		 
******************* PRM(30),���  RM(30),��/��   TSM(30),K
      READ  (3,100) PPP
      WRITE (2,100) PPP

      READ  (3,100) PPP
      WRITE (2,100) PPP

      READ  (3,100) PPP
      WRITE (2,100) PPP

	DO 464 J=1,30
	READ (3,*) PRM(J),RM(J),TSM(J)
464	WRITE(2,'(F6.1,2F8.1)')  PRM(J),RM(J),TSM(J)		 
******************* ����� 1�18�9� �FEM(3),K  ROFEM(3),��/�3  CFEM(3),��/(�� �)  LAFEM(3),��/(� �)
      READ  (3,100) PPP
      WRITE (2,100) PPP

      READ  (3,100) PPP
      WRITE (2,100) PPP

      READ  (3,100) PPP
      WRITE (2,100) PPP

      READ  (3,100) PPP
      WRITE (2,100) PPP

	DO 465 J=1,3
	READ (3,*) TFEM(J),ROFEM(J),CFEM(J),LAFEM(J)
465	WRITE(2,'(F6.1,3F8.1)') TFEM(J),ROFEM(J),CFEM(J),LAFEM(J)		 
******************* ����  �CUM(16),K   ROCUM(16),��/�3  CCUM(16),��/(�� �)   LACUM(16),��/(� �)
      READ  (3,100) PPP
      WRITE (2,100) PPP

      READ  (3,100) PPP
      WRITE (2,100) PPP

      READ  (3,100) PPP
      WRITE (2,100) PPP

	DO 466 J=1,16
	READ (3,*) TCUM(J),ROCUM(J),CCUM(J),LACUM(J)
466	WRITE(2,'(F6.1,3F8.1)') TCUM(J),ROCUM(J),CCUM(J),LACUM(J)		 

      CLOSE(3)
******** ����� ����� �������� ������ ������ ���������� ***************
**********************************************************************
******** ������ ����� �������� ������ �� �����  **********************
      READ  (1,100) PPP
      WRITE (2,100) PPP

      READ(1,*) T,TKOH,H,KPRINT
      WRITE(2,'(3E12.5,I4,16H T,TKOH,H,KPRINT)') T,TKOH,H,KPRINT

      READ  (1,*) (T6(I),I=1,8)
      WRITE (2,'(8F6.1,7H :T6(8))') (T6(I),I=1,8)

      READ  (1,*) J_HEAT
      WRITE (2,'(I3,8H :J_HEAT)') J_HEAT

      READ  (1,*) J_NAPRIAM
      WRITE (2,'(I3,11H :J_NAPRIAM)') J_NAPRIAM

      READ  (1,*) J_TEST
      WRITE (2,'(I3,8H :J_TEST)') J_TEST

      READ  (1,100) PPP
      WRITE (2,100) PPP

	DO 21 I=1,9
	IF(I.EQ.J_TEST) THEN
      READ  (1,*) II,PL1,TLC1,DTL1,GGG_0,RO_U,ZZZ,D_P,DEL,J_MET,TW_MID
	TL1=TLC1+273.15
	PL1B=PL1*1.E-5
	                ELSE
      READ  (1,100) PPP
	                END IF
21	CONTINUE
      WRITE (2,'(I3,8E12.5,I3,E12.5)') 
	*           II,PL1,TLC1,DTL1,GGG_0,RO_U,ZZZ,D_P,DEL,J_MET,TW_MID
	DZ=ZZZ/NJ
	DR=DEL/4.
	PW_P=PI*D_P
	A_P=PI*D_P**2/4.

 	DO 22 I=1,NJ									 !Z_FRONT(NJ)
	Z_FRONT(I)=DZ*I
22	CONTINUE

      RO_LO=YP2(TL1,PL1B,TROM,PROM,ROM,53,30,53,30)
      ANU_LO=YP2(TL1,PL1B,TNUM,PNUM,NUM,53,30,53,30)
      WRITE (2,'(2E12.5,14H :RO_LO,ANU_LO)') RO_LO,ANU_LO

	U_LO=GGG_0/(A_P*RO_LO)
	RE_L=U_LO*D_P/ANU_LO
	TAU_W=0.0395*RO_LO*U_LO**2*RE_L**(-0.25)
	DP_TEOR=TAU_W*PW_P*ZZZ/A_P
      WRITE (2,'(4E12.5,25H :U_LO,RE_L,TAU_W,DP_TEOR)')
	*                       U_LO,RE_L,TAU_W,(DP_TEOR/1.E5)
*********************** ��������� ��������� ����� ********************
      READ  (1,100) PPP
      WRITE (2,100) PPP

      READ  (1,*) P_ENV
      WRITE (2,'(E12.5,7H :P_ENV)') P_ENV

      READ  (1,*) ALF_EXP
      WRITE (2,'(E12.5,9H :ALF_EXP)') ALF_EXP

C6	AKSI_VAR=(PL1-P_ENV)*RO_LO/GGG_0**2
C6	AKOR_L=(PL1-P_ENV-G0*RO_LO*ZZZ)*A_P/(TAU_W*PW_P*ZZZ)
C6      WRITE (2,'(2E12.5,17H :AKSI_VAR,AKOR_L)') AKSI_VAR,AKOR_L
*******************	����� �� �������� �������				   P_TO,DP_P,P_ENV,AJO,AJT,AKSIO,AKSIT
      READ  (1,100) PPP
      WRITE (2,100) PPP

      READ  (1,*) AJO_K     !������������� ������� ����� ������
      READ  (1,*) AJT_K     !������������� ��������� ����� ������
      READ  (1,*) AKSIO_K   !����� ������ ������� ����� ������

	AJ_P=ZZZ/A_P
	AJO=AJO_K*AJ_P
	AJT=AJT_K*AJ_P
      WRITE (2,'(5E12.5,25H AJ_P,AJO,AJO_K,AJT,AJT_K)')
	*                      AJ_P,AJO,AJO_K,AJT,AJT_K
	AKSI_P=DP_TEOR*RO_LO/GGG_0**2
	AKSIT=AKSI_P
	AKSIO=AKSIO_K*AKSI_P
      WRITE (2,'(3E12.5,19H AKSI_P,AKSIO,AKSIT)') AKSI_P,AKSIO,AKSIT

	DP_P=(AKSIO+AKSIT)/RO_LO*GGG_0**2
	P_TO=PL1-DP_P+AKSIO/RO_LO*GGG_0**2
      WRITE (2,'(4E12.5,20H P_TO,DP_P,PL1,P_ENV)') P_TO,DP_P,PL1,P_ENV
***** ��������� ����������� ������� 
      READ  (1,100) PPP
      WRITE (2,100) PPP

      READ  (1,*) TKL     !����� ����������� �������
      READ  (1,*) DTKL    !����������������� ����������� �������
	TKL_M(1)=0.
	TKL_M(2)=TKL
	TKL_M(3)=TKL+DTKL
	TKL_M(4)=90.

	AKL_M(1)=1.
	AKL_M(2)=1.
	AKL_M(3)=0.
	AKL_M(4)=0.

	DO 23 I=1,4									 !TKL_M(4),AKL_M(4)
      WRITE (2,'(2F8.3)') TKL_M(I),AKL_M(I)
23	CONTINUE

      WRITE (2,'(47H*********** ����� ����� �������� ������  ******)')

      CLOSE(1)
******************* ���� �������� ������ *****************************
************************ ����� ***************************************
**********************************************************************

*******************   ��������� ��������   ***************************
      WRITE (2,'(47H************ ��������� ��������  **************)')

	DO 650 I=1,N_SKOKA
650   Y(I)=0.											   
**********************************************************************
      DO 651 IJ=1,NJ+1
	TW1(IJ)=TW_MID						  !TW1(IJ)
	TW2(IJ)=TW_MID						  !TW2(IJ)
	TW3(IJ)=TW_MID						  !TW3(IJ)
	TW4(IJ)=TW_MID						  !TW4(IJ)

	TL(IJ)=TW_MID !TL1				      !TL(IJ)
	TV(IJ)=TW_MID !TL1					  !TV(IJ)

	ROL(IJ)=YP2(TL1,PL1B,TROM,PROM,ROM,53,30,53,30)	!ROL(IJ)
	ROV(IJ)=YP2(TL1,PL1B,TROM,PROM,ROM,53,30,53,30)	!ROV(IJ)

	PPPT(IJ)=P_TO !P_ENV				   !PPPT(IJ)
	PPPP(IJ)=P_TO !P_ENV				   !PPPP(IJ)

	IF(IJ.LE.NJ) THEN
	             UV(IJ)=0.				   !UV(IJ)
	             UL(IJ)=0.				   !UL(IJ)
	             FI_P(IJ)=0				   !FI_P(IJ)
	             X_P(IJ)=0.				   !X_P(IJ)
	J_FLOW(IJ)=0
	             END IF
 651  CONTINUE
**********************************************************************
	Y(3)=GGG_0										  !GGG
	Y(4)=0.										      !Z_FR
***
	JU=10
      DO 652 IJ=1,NJ
	Y(10+(IJ-1)*JU+1)=TW_MID						  !TW1(IJ)
	Y(10+(IJ-1)*JU+2)=TW_MID						  !TW2(IJ)
	Y(10+(IJ-1)*JU+3)=TW_MID						  !TW3(IJ)
	Y(10+(IJ-1)*JU+4)=TW_MID						  !TW4(IJ)

	Y(10+(IJ-1)*JU+7)=0.					          !UV(IJ)
	Y(10+(IJ-1)*JU+8)=TW_MID !TL1					  !TV(IJ)
	Y(10+(IJ-1)*JU+9)=0.					          !UL(IJ)
	Y(10+(IJ-1)*JU+10)=TW_MID !TL1 					  !TL(IJ)
 652  CONTINUE
	J_FRONT=0

      J6=1
	J_SPEKTR_FR=0

	DO 653 I=1,NJ
	J_SPEKTR_FLOW_1(I)=0 
	J_SPEKTR_FLOW_2(I)=0 
	J_SPEKTR_FLOW_3(I)=0 
	J_SPEKTR_FLOW_4(I)=0 
	J_SPEKTR_FLOW_5(I)=0 
	J_SPEKTR_FLOW_6(I)=0 
	J_SPEKTR_FLOW_7(I)=0 
653   CONTINUE

	I_FRONT=0
	I_ZAPOLNO=0
**********************************************************************		 
      WRITE (4,304) 		!00_ITOG_MAIN.TXT
      WRITE (5,305) 		!01_ITOG_MAIN_J10.TXT
      WRITE (6,306) 		!02_ITOG_MAIN_J30.TXT
      WRITE (7,307) 		!03_ITOG_MAIN_J50.TXT
      WRITE (8,308) 		!04_ITOG_MAIN_J70.TXT
      WRITE (9,309) 		!05_ITOG_MAIN_J90.TXT
      WRITE (10,310) 		!06_ITOG_P_FI_X_GL_GV.TXT
      WRITE (11,311) 		!07_ITOG_FRONT.TXT
      WRITE (12,312) 		!08_ITOG_TW_1234.TXT
      WRITE (13,313) 		!09_ITOG_FLOW_7.TXT
304   FORMAT(7X,'T',6X,'TW4_10',7X,'TL_10',7X,'TV_10',8X,'P_10',
     *7X,'UL_10',7X,'UV_10',8X,'X_10',7X,'FI_10',
     *6X,'TW4_30',7X,'TL_30',7X,'TV_30',8X,'P_30',
     *7X,'UL_30',7X,'UV_30',8X,'X_30',7X,'FI_30',
     *6X,'TW4_50',7X,'TL_50',7X,'TV_50',8X,'P_50',
     *7X,'UL_50',7X,'UV_50',8X,'X_50',7X,'FI_50',
     *6X,'TW4_70',7X,'TL_70',7X,'TV_70',8X,'P_70',
     *7X,'UL_70',7X,'UV_70',8X,'X_70',7X,'FI_70',
     *6X,'TW4_90',7X,'TL_90',7X,'TV_90',8X,'P_90',
     *7X,'UL_90',7X,'UV_90',8X,'X_90',7X,'FI_90')

305   FORMAT(7X,'T',9X,'GGG',9X,'ZFR',6X,'TW4_10',7X,'TL_10',7X,'TV_10',
     *8X,'P_10',7X,'UL_10',7X,'UV_10',8X,'X_10',7X,'FI_10')
306   FORMAT(7X,'T',9X,'GGG',9X,'ZFR',6X,'TW4_30',7X,'TL_30',7X,'TV_30',
     *8X,'P_30',7X,'UL_30',7X,'UV_30',8X,'X_30',7X,'FI_30')
307   FORMAT(7X,'T',9X,'GGG',9X,'ZFR',6X,'TW4_50',7X,'TL_50',7X,'TV_50',
     *8X,'P_50',7X,'UL_50',7X,'UV_50',8X,'X_50',7X,'FI_50')
308   FORMAT(7X,'T',9X,'GGG',9X,'ZFR',6X,'TW4_70',7X,'TL_70',7X,'TV_70',
     *8X,'P_70',7X,'UL_70',7X,'UV_70',8X,'X_70',7X,'FI_70')
309   FORMAT(7X,'T',9X,'GGG',9X,'ZFR',6X,'TW4_90',7X,'TL_90',7X,'TV_90',
     *8X,'P_90',7X,'UL_90',7X,'UV_90',8X,'X_90',7X,'FI_90')

310   FORMAT(4X,'XN',
     *7X,'PT6_1',7X,'PT6_2',7X,'PT6_3',7X,'PT6_4',7X,'PT6_5',7X,'PT6_6',
     *7X,'PT6_7',7X,'PT6_8',
     *7X,'FI6_1',7X,'FI6_2',7X,'FI6_3',7X,'FI6_4',7X,'FI6_5',7X,'FI6_6',
     *7X,'FI6_7',7X,'FI6_8',
     *7X,'XT6_1',7X,'XT6_2',7X,'XT6_3',7X,'XT6_4',7X,'XT6_5',7X,'XT6_6',
     *7X,'XT6_7',7X,'XT6_8',
     *7X,'GL6_1',7X,'GL6_2',7X,'GL6_3',7X,'GL6_4',7X,'GL6_5',7X,'GL6_6',
     *7X,'GL6_7',7X,'GL6_8',
     *7X,'GV6_1',7X,'GV6_2',7X,'GV6_3',7X,'GV6_4',7X,'GV6_5',7X,'GV6_6',
     *7X,'GV6_7',7X,'GV6_8')
311   FORMAT(3X,'TZFRM',4X,'ZFRM',4X,'TZLM',5X,'ZLM')
312   FORMAT(7X,'T',
     *6X,'TW1_10',6X,'TW2_10',6X,'TW3_10',6X,'TW4_10',6X,'TLV_10',
     *6X,'TW1_30',6X,'TW2_30',6X,'TW3_30',6X,'TW4_30',6X,'TLV_30',
     *6X,'TW1_50',6X,'TW2_50',6X,'TW3_50',6X,'TW4_50',6X,'TLV_50',
     *6X,'TW1_70',6X,'TW2_70',6X,'TW3_70',6X,'TW4_70',6X,'TLV_70',
     *6X,'TW1_90',6X,'TW2_90',6X,'TW3_90',6X,'TW4_90',6X,'TLV_90')
313   FORMAT(7X,'T',2X,'FLOW_1',2X,'FLOW_2',2X,'FLOW_3',2X,'FLOW_4',
     *2X,'FLOW_5',2X,'FLOW_6',2X,'FLOW_7',2X,'FLOW_S')
**********************************************************************
      KTEK=-1
***   ��������� ������ ������   ***
      DO 11 ISK=1,N_SKOKA
	DY(ISK)=0.
11    CONTINUE
****   �������������� ������� �����-�����  ***
12    CONTINUE
      KTEK=KTEK+1
      T=KTEK*H
      
	KPR_E=KPRINT*10        
C      IF(KTEK/1000*1000.EQ.KTEK) THEN
      IF(KTEK/KPR_E*KPR_E.EQ.KTEK) THEN
                                 PRINT 889,T
	                           WRITE(2,889) T
	                           END IF
889   FORMAT(' t= ',F7.4)
**********************************************************************
**********************************************************************   
	IF(T.GT.TKL.AND.I_FRONT.EQ.0) THEN
	                               J_FRONT=1
	                               J_FLOW(J_FRONT)=1

	                               I_FRONT=1
	                              END IF

      GOTO 224
      CALL FUN(T,Y,DY,N_SKOKA)
	WRITE(2,'(4X,10(I3,9X))') (NOM(JK),JK=1,10)
	WRITE(2,223) (DY(JK),JK=1,N_SKOKA)
223   FORMAT(10E12.5)
	STOP
224   CONTINUE
**********************************************************************	   

      CALL RUNGE_KUTTA(T,Y,DY,YK,DYK,N_SKOKA,H)

      CALL FUN(T,Y,DY,N_SKOKA)

**********************************************************************
	DM_DT=DY(3)
	IF(Z_FR.GT.ZZZ) Y(4)=ZZZ
********************* ����������� dx/dz ******************************
	DO 225 J=1,NJ			 
	IF(J_FRONT.EQ.1.OR.J.GT.J_FRONT) THEN
	                                 DX_P(J)=0.
	                                 GOTO 225
	                                 END IF
	IF(J_FRONT.GT.1) THEN
	                 IF(J.EQ.1)       DX_P(J)=(X_P(J+1)-X_P(J))/DZ
	                 IF(J.EQ.J_FRONT) DX_P(J)=(X_P(J-1)-X_P(J))/DZ
      IF(J.GT.1.AND.J.LT.J_FRONT) DX_P(J)=(X_P(J+1)-X_P(J-1))/(2.*DZ)
					 END IF
225   CONTINUE
*************** ������������ ������� ������� *************************
	IF(J_FRONT.GE.1) THEN
	                 DO 226 J=1,J_FRONT
**********************************************************************	 
*** J_FLOW(J)=1	   ���������� ��������� ���������� ��������
	        IF(J_FLOW(J).EQ.1) THEN
	                           IF(TL(J+1).GE.TSL(J+1)) THEN
						                   J_FLOW(J)=J_FLOW(J)+1
      WRITE(2,'(39H ������������ ��� ������� T,J,J_FLOW(J),F8.4,3I4)') 
	                                       T,J,J_FLOW(J)-1,J_FLOW(J)
	                                                   END IF
	                           END IF
**********************************************************************	 
*** J_FLOW(J)=2	   ����������� ����� �������
	        IF(J_FLOW(J).EQ.2) THEN
	                           IF(TL(J+1).GE.T1KR(J+1)) THEN
						                   J_FLOW(J)=J_FLOW(J)+1
      WRITE(2,'(39H ������������ ��� ������� T,J,J_FLOW(J),F8.4,3I4)') 
	                                       T,J,J_FLOW(J)-1,J_FLOW(J)
	                                                    END IF
***
	                           IF(TL(J+1).LT.TSL(J+1)) THEN
						                   J_FLOW(J)=J_FLOW(J)-1
      WRITE(2,'(39H ������������ ��� ������� T,J,J_FLOW(J),F8.4,3I4)') 
	                                       T,J,J_FLOW(J)+1,J_FLOW(J)
	                                                    END IF
	                            END IF
**********************************************************************	 
*** J_FLOW(J)=3	   ���������� ����� �� ������������ � ����������
	        IF(J_FLOW(J).EQ.3) THEN
	                           IF(TL(J+1).GE.T2KR(J+1)) THEN
						                   J_FLOW(J)=J_FLOW(J)+1
      WRITE(2,'(39H ������������ ��� ������� T,J,J_FLOW(J),F8.4,3I4)') 
	                                       T,J,J_FLOW(J)-1,J_FLOW(J)
	                                                    END IF
***
	                           IF(TL(J+1).LT.T1KR(J+1)) THEN
						                   J_FLOW(J)=J_FLOW(J)-1
      WRITE(2,'(39H ������������ ��� ������� T,J,J_FLOW(J),F8.4,3I4)') 
	                                       T,J,J_FLOW(J)+1,J_FLOW(J)
	                                                    END IF
	                            END IF
**********************************************************************	 
*** J_FLOW(J)=4	   ��������� ������� � ���������� ������ ������� (���� ������)
	        IF(J_FLOW(J).EQ.4) THEN
	                           IF(WE_P(J).GE.1.) THEN
						                   J_FLOW(J)=J_FLOW(J)+1
      WRITE(2,'(39H ������������ ��� ������� T,J,J_FLOW(J),F8.4,3I4)') 
	                                       T,J,J_FLOW(J)-1,J_FLOW(J)
	                                             END IF
***
	                           IF(TL(J+1).LT.T2KR(J+1)) THEN
						                   J_FLOW(J)=J_FLOW(J)-1
      WRITE(2,'(39H ������������ ��� ������� T,J,J_FLOW(J),F8.4,3I4)') 
	                                       T,J,J_FLOW(J)+1,J_FLOW(J)
	                                                    END IF
	                            END IF
**********************************************************************	 
*** J_FLOW(J)=5	   ��������� ������� � ���������� ������ ������� (���� ����������� ���������)
	        IF(J_FLOW(J).EQ.5) THEN
	                           IF(FI_P(J).GE.0.7) THEN
						                   J_FLOW(J)=J_FLOW(J)+1
      WRITE(2,'(39H ������������ ��� ������� T,J,J_FLOW(J),F8.4,3I4)') 
	                                       T,J,J_FLOW(J)-1,J_FLOW(J)
	                                              END IF
***
	                           IF(WE_P(J).LT.1.) THEN
						                   J_FLOW(J)=J_FLOW(J)-1
      WRITE(2,'(39H ������������ ��� ������� T,J,J_FLOW(J),F8.4,3I4)') 
	                                       T,J,J_FLOW(J)+1,J_FLOW(J)
	                                             END IF
	                            END IF
**********************************************************************	 
*** J_FLOW(J)=6	   ��������� ������� � ���������� ������ ������� 
	        IF(J_FLOW(J).EQ.6) THEN
	                           IF(X_P(J).GE.0.99) THEN
						                   J_FLOW(J)=J_FLOW(J)+1
      WRITE(2,'(39H ������������ ��� ������� T,J,J_FLOW(J),F8.4,3I4)') 
	                                       T,J,J_FLOW(J)-1,J_FLOW(J)

	                                       TV_T(J+1)=TV(J+1)
			                               D_FILM_T(J)=D_FILM(J)
	                                              END IF
***
	                           IF(FI_P(J).GE.0.7) THEN
						                   J_FLOW(J)=J_FLOW(J)-1
      WRITE(2,'(39H ������������ ��� ������� T,J,J_FLOW(J),F8.4,3I4)') 
	                                       T,J,J_FLOW(J)+1,J_FLOW(J)
	                                             END IF
	                            END IF
**********************************************************************	 
*** J_FLOW(J)=7	   ���������� ��������� ������������� ����
	        IF(J_FLOW(J).EQ.7) THEN
	                           IF(TV(J+1).LE.TV_T(J+1)) THEN
						                   J_FLOW(J)=J_FLOW(J)-1
      WRITE(2,'(39H ������������ ��� ������� T,J,J_FLOW(J),F8.4,3I4)') 
	                                       T,J,J_FLOW(J)+1,J_FLOW(J)

			                               D_FILM(J)=D_FILM_T(J)
	                                                    END IF
	                            END IF
**********************************************************************	 
226                    CONTINUE
	                 END IF
*********************** ����������� �������� *************************	   
	IF(J_FRONT.EQ.0) GOTO 24
	IF(Z_FR.GT.Z_FRONT(NJ).AND.I_ZAPOLNO.EQ.0) THEN
                         WRITE(2,'(15H ����� ��������,3H T=,F8.4)') T
	                                           I_ZAPOLNO=1
	                                           GOTO 24
	                                           END IF
	IF(J_FRONT.EQ.NJ) GOTO 24
	IF(Z_FR.GT.Z_FRONT(J_FRONT)) THEN
	                             J_FRONT=J_FRONT+1
	                             TZFRM(J_FRONT)=T
	WRITE(2,'(9H J_FRONT=,I3,3H T=,3F8.4)')
	*                             J_FRONT,T,Z_FR,Z_FRONT(J_FRONT)
	                             J_FLOW(J_FRONT)=J_FLOW(J_FRONT-1) !������� �� ����� ������� ����� ��� �� ����������
	IF(J_HEAT.EQ.1) THEN
	                TL(J_FRONT)=TL(J_FRONT-1)				       !TL(J_FRONT)
	                TV(J_FRONT)=TV(J_FRONT-1)					   !TV(J_FRONT)
	                JU=10
	                Y(10+(J_FRONT-1)*JU+8)=TV(J_FRONT-1)		   !TV(J_FRONT)
	                Y(10+(J_FRONT-1)*JU+10)=TL(J_FRONT-1) 		   !TL(J_FRONT)
	                END IF
	                             J_SPEKTR_FR=1
	                             GOTO 24
	                             END IF
 24    CONTINUE
**********************************************************************	   
	DO 25 I=1,NJ+1			 
	PPPT(I)=PPPP(I)	   !PPPT(I)-�������� ������������ �� ���� ����
25    CONTINUE
*********  ��������� � ����� �����������  ****************************
      IF(KTEK/KPRINT*KPRINT.EQ.KTEK) THEN
	TW4_10=TW4(10)/TL1
	TL_10=TL(10)/TL1
	TV_10=TV(10)/TL1
	P_10=PPPP(10)/PL1
	UL_10=UL(10)/U_LO
	UV_10=UV(10)/U_LO

	TW4_30=TW4(30)/TL1
	TL_30=TL(30)/TL1
	TV_30=TV(30)/TL1
	P_30=PPPP(30)/PL1
	UL_30=UL(30)/U_LO
	UV_30=UV(30)/U_LO

	TW4_50=TW4(50)/TL1
	TL_50=TL(50)/TL1
	TV_50=TV(50)/TL1
	P_50=PPPP(50)/PL1
	UL_50=UL(50)/U_LO
	UV_50=UV(50)/U_LO

	TW4_70=TW4(70)/TL1
	TL_70=TL(70)/TL1
	TV_70=TV(70)/TL1
	P_70=PPPP(70)/PL1
	UL_70=UL(70)/U_LO
	UV_70=UV(70)/U_LO

	TW4_90=TW4(90)/TL1
	TL_90=TL(90)/TL1
	TV_90=TV(90)/TL1
	P_90=PPPP(90)/PL1
	UL_90=UL(90)/U_LO
	UV_90=UV(90)/U_LO

	WRITE(4,321) T, 	                                           !FILE='00_ITOG_MAIN.TXT	��� ��������� �� 5 ��������
     *      TW4_10,TL_10,TV_10,P_10,UL_10,UV_10,X_P(10),FI_P(10),
     *      TW4_30,TL_30,TV_30,P_30,UL_30,UV_30,X_P(30),FI_P(30),
     *      TW4_50,TL_50,TV_50,P_50,UL_50,UV_50,X_P(50),FI_P(50),
     *      TW4_70,TL_70,TV_70,P_70,UL_70,UV_70,X_P(70),FI_P(70),
     *      TW4_90,TL_90,TV_90,P_90,UL_90,UV_90,X_P(90),FI_P(90)

	WRITE(5,321) T,GGG,Z_FR,TW4(10),TL(10),TV(10),PPPP(10)/1.E5,   !FILE='01_ITOG_MAIN_J10.TXT	��������� �� 1 ������� (10��)
	*               UL(10),UV(10),X_P(10),FI_P(10)
	WRITE(6,321) T,GGG,Z_FR,TW4(30),TL(30),TV(30),PPPP(30)/1.E5,   !FILE='02_ITOG_MAIN_J30.TXT	��������� �� 2 ������� (30��)
	*               UL(30),UV(30),X_P(30),FI_P(30)
	WRITE(7,321) T,GGG,Z_FR,TW4(50),TL(50),TV(50),PPPP(50)/1.E5,   !FILE='03_ITOG_MAIN_J50.TXT	��������� �� 3 ������� (50��)
	*               UL(50),UV(50),X_P(50),FI_P(50)
	WRITE(8,321) T,GGG,Z_FR,TW4(70),TL(70),TV(70),PPPP(70)/1.E5,   !FILE='04_ITOG_MAIN_J70.TXT	��������� �� 4 ������� (70��)
	*               UL(70),UV(70),X_P(70),FI_P(70)
	WRITE(9,321) T,GGG,Z_FR,TW4(90),TL(90),TV(90),PPPP(90)/1.E5,   !FILE='05_ITOG_MAIN_J90.TXT	��������� �� 5 ������� (90��)
	*               UL(90),UV(90),X_P(90),FI_P(90)

	WRITE(12,321) T,TW1(10),TW2(10),TW3(10),TW4(10), 	           !FILE='08_ITOG_TW_1234.TXT	����������� ����� �� 5 ��������
     *TW1(30),TW2(30),TW3(30),TW4(30),TW1(50),TW2(50),TW3(50),TW4(50),                
     *TW1(70),TW2(70),TW3(70),TW4(70),TW1(90),TW2(90),TW3(90),TW4(90)                
	                               END IF											   
321   FORMAT(F8.4,50E12.4)  !7X,'T'
**********************************************************************   
	IF(J6.GE.8) GOTO 371
	IF(ABS(T-T6(J6)).LT.H/2) THEN
	                         DO 370 I6=1,NJ
  	                         PT6(J6,I6)=PPPP(I6)/1.E5
  	                         FIT6(J6,I6)=FI_P(I6)
  	                         XT6(J6,I6)=X_P(I6)
  	             GLT6(J6,I6)=ROL(I6)*A_P*(1.-FI_P(I6))*UL(I6)/GGG_0	!@
  	             GVT6(J6,I6)=ROV(I6)*A_P*FI_P(I6)*UV(I6)/GGG_0		!@
370                            CONTINUE
                               J6=J6+1
	                         END IF
371   CONTINUE
************************************		   
	IF(J_SPEKTR_FR.EQ.0) GOTO 373
	DO 372 I=1,J_FRONT
	IF(J_FLOW(I).EQ.1) J_SPEKTR_FLOW_1(I)=J_SPEKTR_FLOW_1(I)+1 
	IF(J_FLOW(I).EQ.2) J_SPEKTR_FLOW_2(I)=J_SPEKTR_FLOW_2(I)+1 
	IF(J_FLOW(I).EQ.3) J_SPEKTR_FLOW_3(I)=J_SPEKTR_FLOW_3(I)+1 
	IF(J_FLOW(I).EQ.4) J_SPEKTR_FLOW_4(I)=J_SPEKTR_FLOW_4(I)+1 
	IF(J_FLOW(I).EQ.5) J_SPEKTR_FLOW_5(I)=J_SPEKTR_FLOW_5(I)+1 
	IF(J_FLOW(I).EQ.6) J_SPEKTR_FLOW_6(I)=J_SPEKTR_FLOW_6(I)+1 
	IF(J_FLOW(I).EQ.7) J_SPEKTR_FLOW_7(I)=J_SPEKTR_FLOW_7(I)+1 
372   CONTINUE
	J_SPEKTR_FR=0
373   CONTINUE
**********************************************************************
      IF(T.LT.TKOH) GOTO 12
**********************************************************************
**********************************************************************
	DO 375 I=1,NJ
	XN=I
	WRITE(10,322) XN,(PT6(J6,I),J6=1,8),(FIT6(J6,I),J6=1,8),	   !FILE='06_ITOG_P_FI_X_GL_GV.TXT	��� 6 ������ ��������� �� �����
	*    (XT6(J6,I),J6=1,8),(GLT6(J6,I),J6=1,8),(GVT6(J6,I),J6=1,8)
375   CONTINUE

322   FORMAT(F6.1,50E12.4)  !4X,'XN'
************************************		   
	DO 376 I=1,NJ
	XN=I*DZ
	XZL=(J_SPEKTR_FLOW_1(I)+J_SPEKTR_FLOW_2(I))*DZ	!@
	TZLM(I)=TZFRM(I)
	WRITE(11,323) TZFRM(I),XN,TZLM(I),XZL	                       !FILE='07_ITOG_FRONT.TXT	 ����� ���������� � ����� ���������� ���������
376   CONTINUE

323   FORMAT(2(F8.4,F8.2))  !3X,'TZFRM',4X,'ZFRM',4X,'TZLM',5X,'ZLM'
************************************		   
	DO 377 I=1,NJ
	J_SPEKTR_FLOW_2(I)=J_SPEKTR_FLOW_1(I)+J_SPEKTR_FLOW_2(I)
	J_SPEKTR_FLOW_3(I)=J_SPEKTR_FLOW_2(I)+J_SPEKTR_FLOW_3(I) 
	J_SPEKTR_FLOW_4(I)=J_SPEKTR_FLOW_3(I)+J_SPEKTR_FLOW_4(I) 
	J_SPEKTR_FLOW_5(I)=J_SPEKTR_FLOW_4(I)+J_SPEKTR_FLOW_5(I) 
	J_SPEKTR_FLOW_6(I)=J_SPEKTR_FLOW_5(I)+J_SPEKTR_FLOW_6(I) 
	J_SPEKTR_FLOW_7(I)=J_SPEKTR_FLOW_6(I)+J_SPEKTR_FLOW_7(I) 
	WRITE(13,324) TZFRM(I),J_SPEKTR_FLOW_1(I),J_SPEKTR_FLOW_2(I),  !FILE='09_ITOG_FLOW_7.TXT  ����� ����� ������� �� �������
	*J_SPEKTR_FLOW_3(I),J_SPEKTR_FLOW_4(I),J_SPEKTR_FLOW_5(I),	   !@
	*J_SPEKTR_FLOW_6(I),J_SPEKTR_FLOW_7(I)
377   CONTINUE
324   FORMAT(F8.4,10I8)   !7X,'T',2X,'FLOW_1'
**********************************************************************
C1      CLOSE(1)
      CLOSE(2)
C1      CLOSE(3)

      CLOSE(4)
      CLOSE(5)
      CLOSE(6)
      CLOSE(7)
      CLOSE(8)
      CLOSE(9)
      CLOSE(10)
      CLOSE(11)
      CLOSE(12)
      CLOSE(13)
**********************************************************************
      STOP
100   FORMAT(A)
      END
**********************************************************************
      SUBROUTINE RUNGE_KUTTA(T,Y,DY,YK,DYK,N,H)
**   �������������� ������� �����-�����   **
      REAL*8 Y(N),DY(N),YK(N),DYK(4,N)
      DO 3 I=1,4
      IF(I.EQ.1) DT=0.
      IF(I.EQ.2.OR.I.EQ.3) DT=H*0.5
      IF(I.EQ.4) DT=H
      TEK=T+DT
      DO 1 J=1,N
      IF(I.EQ.1) THEN
                  YK(J)=Y(J)
                 ELSE
                  YK(J)=Y(J)+DYK(I-1,J)*DT
                 END IF
1     CONTINUE

      CALL FUN(TEK,YK,DY,N)
      DO 2 J=1,N
2     DYK(I,J)=DY(J)
3     CONTINUE
      DO 4 J=1,N
      DY(J)=H/6.*(DYK(1,J)+2.*DYK(2,J)+2.*DYK(3,J)+DYK(4,J))
4     Y(J)=Y(J)+DY(J)
      RETURN
      END
**********************************************************************
      SUBROUTINE FUN(T,Y,DY,N)
**   ���������� ������ ������ ���. ���������   **
      PARAMETER(NJ=100)

	COMMON/JJJ/	J_HEAT,J_NAPRIAM,J_FRONT,J_FLOW(NJ),J_MET
	COMMON/TRUBA/ DZ,DR,PW_P,A_P,D_P,ZZZ,  
	*              RO_LO,DM_DT,ALF_EXP,TW_MID,TL1,U_LO
	COMMON/VARIABLE/ PPPT(NJ+1),PPPP(NJ+1),FI_P(NJ),X_P(NJ),DX_P(NJ),
	*TW1(NJ+1),TW2(NJ+1),TW3(NJ+1),TW4(NJ+1),
	*UL(NJ),TL(NJ+1),ROL(NJ+1), !GL_P(NJ),
     *UV(NJ),TV(NJ+1),ROV(NJ+1), !,GV_P(NJ)
     *ALF_INT(NJ),T1KR(NJ+1),DU_V(NJ),T2KR(NJ+1),D_FILM(NJ)

	COMMON/ZAGAL/ TROM(53),PROM(30),ROM(53,30),
	*              TCPM(53),PCPM(30),CPM(53,30),
	*              TLAM(53),PLAM(30),LAM(53,30),
     *              TNUM(53),PNUM(30),NUM(53,30),
	*              TSIM(39),SIM(39),
 	*              PRM(30),RM(30),TSM(30),
     *              TFEM(3),ROFEM(3),CFEM(3),LAFEM(3),
     *              TCUM(16),ROCUM(16),CCUM(16),LACUM(16)
	COMMON/KLAPA/ TKL_M(4),AKL_M(4)
	COMMON/ENVIR/ P_TO,DP_P,P_ENV,AJO,AJT,AKSIO,AKSIT

	COMMON/SWITCH/ TSL(NJ+1),WE_P(NJ),WE_PD(NJ),TV_T(NJ+1),
	*D_FILM_T(NJ+1) 

	COMMON/PRINT/ GGG,Z_FR
**********************************************************************
      REAL*8 Y(N),DY(N)
	REAL TROM,PROM,ROM,	 TNUM,PNUM,NUM,	TSIM,SIM, PRM,RM,TSM, 
	*TFEM,ROFEM,CFEM,LAFEM, TCUM,ROCUM,CCUM,LACUM, LAM
	REAL A_EL(NJ),B_EL(NJ),ALF_EL(NJ),BET_EL(NJ)
	REAL A_ELM(NJ),B_ELM(NJ),ALF_ELM(NJ),BET_ELM(NJ)
	REAL NUL(NJ+1),NUV(NJ+1),CPL(NJ+1),CPV(NJ+1),
	*ALAML(NJ+1),ALAMV(NJ+1),SIGMA(NJ+1),RRR(NJ+1),TSSS(NJ+1)

	REAL A_PPP(NJ+1,NJ+1),B_PPP(NJ+1)
**********************************************************************
      PI=3.149159
      G0=9.81
**********************************************************************
	DO 41 I=1,N
41	DY(I)=0.
**********************************************************************
      DO 411 J=1,NJ+1
      T1KR(J)=0.
411   CONTINUE
**********************************************************************
	GGG=Y(3)										         !GGG
	Z_FR=Y(4)										         !Z_FR
****************************************
	JU=10
      DO 42 J=1,NJ
	IF(J.EQ.NJ) GOTO 421
	TW1(J+1)=Y(10+(J-1)*JU+1)						         !TW1(J+1)
	TW2(J+1)=Y(10+(J-1)*JU+2)						         !TW2(J+1)
	TW3(J+1)=Y(10+(J-1)*JU+3)						         !TW3(J+1)
C1	TW4(J+1)=Y(10+(J-1)*JU+4)						         !TW4(J+1)
421   CONTINUE

	UV(J)=Y(10+(J-1)*JU+7)					                 !UV(J)
	IF(J.NE.NJ) TV(J+1)=Y(10+(J-1)*JU+8)					 !TV(J+1)	@
	UL(J)=Y(10+(J-1)*JU+9)					                 !UL(J)
	IF(J.NE.NJ) TL(J+1)=Y(10+(J-1)*JU+10)					 !TL(J+1)	@
 
C1	IF(J.EQ.NJ) GOTO 42
 	PPPB=PPPT(J+1)*1.E-5
	ROL(J+1)=YP2(TL(J+1),PPPB,TROM,PROM,ROM,53,30,53,30)     !ROL(J+1)		
 	ROV(J+1)=YP2(TV(J+1),PPPB,TROM,PROM,ROM,53,30,53,30)	 !ROV(J+1)		 

  	NUL(J+1)=YP2(TL(J+1),PPPB,TNUM,PNUM,NUM,53,30,53,30)     !NUL(J+1)
 	NUV(J+1)=YP2(TV(J+1),PPPB,TNUM,PNUM,NUM,53,30,53,30)	 !NUV(J+1)

  	CPL(J+1)=YP2(TL(J+1),PPPB,TCPM,PCPM,CPM,53,30,53,30)     !CPL(J+1)		  
 	CPV(J+1)=YP2(TV(J+1),PPPB,TCPM,PCPM,CPM,53,30,53,30)	 !CPV(J+1)

  	ALAML(J+1)=YP2(TL(J+1),PPPB,TLAM,PLAM,LAM,53,30,53,30)   !ALAML(J+1)		  
 	ALAMV(J+1)=YP2(TV(J+1),PPPB,TLAM,PLAM,LAM,53,30,53,30)	 !ALAMV(J+1)		   

      SIGMA(J+1)=YP(TL(J+1),TSIM,SIM,39)						 !SIGMA(J+1)	  
      RRR(J+1)=YP(PPPB,PRM,RM,30)						         !RRR(J+1)
      TSSS(J+1)=YP(PPPB,TSM,RM,30)						     !TSSS(J+1)
42    CONTINUE

****************************************
      DO 422 J=1,NJ-1
	IF(J_MET.EQ.1) ALAM_W=YP(TW3(J+1),TFEM,LAFEM,3)
	IF(J_MET.EQ.2) ALAM_W=YP(TW3(J+1),TCUM,LACUM,16)
	A_EXP=ALF_EXP*DR/ALAM_W
	TW4(J+1)=(TW3(J+1)-A_EXP*TW_MID)/(1.-A_EXP)		         !TW4(J+1)
422   CONTINUE
****************************************
	TV(1)=TL1					                             !TV(1)
	TL(1)=TL1					                             !TL(1)

 	PPPB=PPPT(1)*1.E-5
	ROL(1)=YP2(TL(1),PPPB,TROM,PROM,ROM,53,30,53,30)         !ROL(1)
 	ROV(1)=YP2(TV(1),PPPB,TROM,PROM,ROM,53,30,53,30)	     !ROV(1)

  	NUL(1)=YP2(TL(1),PPPB,TNUM,PNUM,NUM,53,30,53,30)         !NUL(1)
 	NUV(1)=YP2(TV(1),PPPB,TNUM,PNUM,NUM,53,30,53,30)	     !NUV(1)

  	CPL(1)=YP2(TL(1),PPPB,TCPM,PCPM,CPM,53,30,53,30)         !CPL(1)		  
 	CPV(1)=YP2(TV(1),PPPB,TCPM,PCPM,CPM,53,30,53,30)	     !CPV(1)

  	ALAML(1)=YP2(TL(1),PPPB,TLAM,PLAM,LAM,53,30,53,30)       !ALAML(1)		  
 	ALAMV(1)=YP2(TV(1),PPPB,TLAM,PLAM,LAM,53,30,53,30)	     !ALAMV(1)		   

      SIGMA(1)=YP(TL(1),TSIM,SIM,39)						     !SIGMA(J+1)
      RRR(1)=YP(PPPB,PRM,RM,30)						         !RRR(J+1)
      TSSS(1)=YP(PPPB,TSM,RM,30)						         !TSSS(J+1)

***
C1	TV(NJ+1)=TW_MID					                         !TV(NJ+1)
C1	TL(NJ+1)=TW_MID					                         !TL(NJ+1)

C1 	PPPB=PPPT(NJ+1)*1.E-5
C1	ROL(NJ+1)=YP2(TL(NJ+1),PPPB,TROM,PROM,ROM,53,30,53,30)   !ROL(NJ+1)
C1 	ROV(NJ+1)=YP2(TV(NJ+1),PPPB,TROM,PROM,ROM,53,30,53,30)	 !ROV(NJ+1)

C1  	NUL(NJ+1)=YP2(TL(NJ+1),PPPB,TNUM,PNUM,NUM,53,30,53,30)   !NUL(NJ+1)
C1 	NUV(NJ+1)=YP2(TV(NJ+1),PPPB,TNUM,PNUM,NUM,53,30,53,30)	 !NUV(NJ+1)
****************************************
	IF(J_MET.EQ.1) ALAM_W=YP(TW1(2),TFEM,LAFEM,3)
	IF(J_MET.EQ.2) ALAM_W=YP(TW1(2),TCUM,LACUM,16)
	A_EXP=ALF_EXP*DZ/ALAM_W
	TW1(1)=(TW1(2)-A_EXP*TW_MID)/(1.-A_EXP)		             !TW1(1)
***
	IF(J_MET.EQ.1) ALAM_W=YP(TW2(2),TFEM,LAFEM,3)
	IF(J_MET.EQ.2) ALAM_W=YP(TW2(2),TCUM,LACUM,16)
	A_EXP=ALF_EXP*DZ/ALAM_W
	TW2(1)=(TW2(2)-A_EXP*TW_MID)/(1.-A_EXP)		             !TW2(1)
***
	IF(J_MET.EQ.1) ALAM_W=YP(TW3(2),TFEM,LAFEM,3)
	IF(J_MET.EQ.2) ALAM_W=YP(TW3(2),TCUM,LACUM,16)
	A_EXP=ALF_EXP*DZ/ALAM_W
	TW3(1)=(TW3(2)-A_EXP*TW_MID)/(1.-A_EXP)		             !TW3(1)
***
	IF(J_MET.EQ.1) ALAM_W=YP(TW4(2),TFEM,LAFEM,3)
	IF(J_MET.EQ.2) ALAM_W=YP(TW4(2),TCUM,LACUM,16)
	A_EXP=ALF_EXP*DZ/ALAM_W
	TW4(1)=(TW4(2)-A_EXP*TW_MID)/(1.-A_EXP)		             !TW4(1)
****************************
	IF(J_MET.EQ.1) ALAM_W=YP(TW1(NJ),TFEM,LAFEM,3)
	IF(J_MET.EQ.2) ALAM_W=YP(TW1(NJ),TCUM,LACUM,16)
	A_EXP=ALF_EXP*DZ/ALAM_W
	TW1(NJ+1)=(TW1(NJ)-A_EXP*TW_MID)/(1.-A_EXP)		         !TW1(NJ+1)
***
	IF(J_MET.EQ.1) ALAM_W=YP(TW2(NJ),TFEM,LAFEM,3)
	IF(J_MET.EQ.2) ALAM_W=YP(TW2(NJ),TCUM,LACUM,16)
	A_EXP=ALF_EXP*DZ/ALAM_W
	TW2(NJ+1)=(TW2(NJ)-A_EXP*TW_MID)/(1.-A_EXP)		         !TW2(NJ+1)
***
	IF(J_MET.EQ.1) ALAM_W=YP(TW3(NJ),TFEM,LAFEM,3)
	IF(J_MET.EQ.2) ALAM_W=YP(TW3(NJ),TCUM,LACUM,16)
	A_EXP=ALF_EXP*DZ/ALAM_W
	TW3(NJ+1)=(TW3(NJ)-A_EXP*TW_MID)/(1.-A_EXP)		         !TW3(NJ+1)
***
	IF(J_MET.EQ.1) ALAM_W=YP(TW4(NJ),TFEM,LAFEM,3)
	IF(J_MET.EQ.2) ALAM_W=YP(TW4(NJ),TCUM,LACUM,16)
	A_EXP=ALF_EXP*DZ/ALAM_W
	TW4(NJ+1)=(TW4(NJ)-A_EXP*TW_MID)/(1.-A_EXP)		         !TW4(NJ+1)
**********************************************************************  
********************** ������ ����� **********************************
	IF(J_FRONT.EQ.0) THEN
	                  DP_V=0.
	                  DP_L=0.
	                  D_J=0.
	                 GOTO 488
	                 END IF
**********************************************************************	 
      DO 43 J=1,NJ					  
	A_EL(J)=0.
	B_EL(J)=0.

	ALF_EL(J)=0.
	BET_EL(J)=0.
**********
	A_ELM(J)=0.
	B_ELM(J)=0.

	ALF_ELM(J)=0.
	BET_ELM(J)=0.
43    CONTINUE
**********************************************************************	 
	JU=10
      DO 44 J=1,J_FRONT   !NJ					  
C6	IF(J.GT.J_FRONT) GOTO 44

	GGG_FI=GGG
	IF(GGG.LT.0.0001) GGG_FI=0.0001 
**********************************************************************	 
*** J_FLOW(J)=1	   ���������� ��������� ���������� ��������
	IF(J_FLOW(J).EQ.1) THEN

	ROL_MID=(ROL(J)+ROL(J+1))/2.
	ANUL_MID=(NUL(J)+NUL(J+1))/2.
C1	U_L=GGG/(A_P*ROL_MID)
C1	RE_L=U_L*D_P/ANUL_MID
	RE_L=UL(J)*D_P/ANUL_MID
	TAU_W=0.0395*ROL_MID*UL(J)**2*RE_L**(-0.25)					   !TAU_W

	A_EL(J)=1./(ROL_MID*DZ)
	B_EL(J)=J_NAPRIAM*G0*COS(0.)-TAU_W*PW_P/(A_P*ROL_MID)

	ALF_EL(J)=0.
	BET_EL(J)=0.
**********************************************************************
	X_P(J)=0.
      FI_P(J)=0.
**********
	RE_L=UL(J)*D_P/NUL(J+1)

	A_TEM=ALAML(J+1)/(ROL(J+1)*CPL(J+1))
	PR_L=NUL(J+1)/A_TEM

	ALF_INT(J)=0.021*ALAML(J+1)/D_P*RE_L**0.8*PR_L**0.4			   !ALF_INT(J)
**********
	DY(10+(J-1)*JU+8)=0.					                       !TV(J+1)
	DY(10+(J-1)*JU+10)=ALF_INT(J)*(TW1(J+1)-TL(J+1))*
	*PW_P/(CPL(J+1)*A_P*ROL(J+1))					               !TL(J+1)
**********
	                   END IF
**********************************************************************	 
*** J_FLOW(J)=2	   ����������� ����� �������
	IF(J_FLOW(J).EQ.2) THEN

	ROL_MID=(ROL(J)+ROL(J+1))/2.
	ROV_MID=(ROV(J)+ROV(J+1))/2.
      FI_P(J)=(ROL_MID*UL(J)-GGG_FI/A_P)/(ROL_MID*UL(J)-ROV_MID*UV(J))
	X_P(J)=ROV_MID*UV(J)*A_P*FI_P(J)/GGG_FI

	A_PL=A_P*(1.-FI_P(J))
	D_PL=SQRT(4.*A_PL/PI)
	PL_P=PI*D_PL
**********
	RO_MIX=FI_P(J)*ROV_MID+(1.-FI_P(J))*ROL_MID
********** �������� �������� ������
	DU_V(J)=2.*(ROL_MID/ROV_MID)**0.2*(ROV_MID/ROL_MID)**5.*	   !DU_V(J)
	*(SIGMA(J+1)*G0*(ROL_MID-ROV_MID)/ROV_MID**2)**0.25
**********
	ANUL_MID=(NUL(J)+NUL(J+1))/2.
 	RE_L=UL(J)*D_P/ANUL_MID
	TAU_WL=0.0395*ROL_MID*UL(J)**2*RE_L**(-0.25)					   
	TAU_W=TAU_WL*(1.-FI_P(J)*(1.-ROV_MID/ROL_MID))**0.75*
	*             (1.-X_P(J)*(1.-ROL_MID/ROV_MID))                  !TAU_W
***
	A_EL(J)=1./(ROL_MID*DZ)
	B_EL(J)=J_NAPRIAM*G0*COS(0.)-TAU_W*PW_P/(A_P*RO_MIX)

	ALF_EL(J)=0.
	BET_EL(J)=0.
**********************************************************************
 	PPPB=PPPT(J+1)*1.E-5
	Q1KR=0.14*RRR(J+1)*ROV_MID**0.5*
	*                 (G0*SIGMA(J+1)*(ROL_MID-ROV_MID))**0.25
	T1KR(J+1)=TSSS(J+1)+0.0227*Q1KR**0.5*EXP(-PPPB/86.87)		   !T1KR(J+1)
**********
	X_FC=-CPL(J+1)*(TSSS(J+1)-TL(J+1))/RRR(J+1)
	AMU_L=ROL(J+1)*NUL(J+1)
	AMU_V=ROV(J+1)*NUV(J+1)
	XTT_FC=((1.-X_FC)/X_FC)**0.9*(ROV(J+1)/ROL(J+1))**0.5*
	*                                   (AMU_L/AMU_V)**0.5
	F_FC=1.
	IF(1./XTT_FC.GT.O.1) F_FC=2.35*(1./XTT_FC+0.213)**0.736
***
 	PPPB=PPPT(J+1)*1.E-5
	ROW=YP2(TW1(J+1),PPPB,TROM,PROM,ROM,53,30,53,30)      !ROW		
   	ANUW=YP2(TW1(J+1),PPPB,TNUM,PNUM,NUM,53,30,53,30)     !ANUW
	AMU_W=ROW*ANUW

  	RE_L=UL(J)*D_P/NUL(J+1)
	A_TEM=ALAML(J+1)/(ROL(J+1)*CPL(J+1))
	PR_L=NUL(J+1)/A_TEM

	ALF_FC=0.023*RE_L**0.8*PR_L**0.4*ALAML(J+1)/D_P*
	*                                     (AMU_L/AMU_W)**0.14
***
	S_BN=1./(1.+2.53E-6*RE_L**1.17)							
***
	PS_L=YP(TL(J+1),TSM,PRM,30)
	PS_W=YP(TW1(J+1),TSM,PRM,30)

	ALF_NB=0.00122*ALAML(J+1)**0.79*CPL(J+1)**0.45*ROL(J+1)**0.49*
	*G0**0.25/((SIGMA(J+1)*AMU_L)**0.5*(RRR(J+1)*ROV(J+1))**0.24)*
	*(TW1(J+1)-TSSS(J+1))**0.24*(PS_W-PS_L)**0.75
***
	Q_INT=F_FC*ALF_FC*(TW1(J+1)-TL(J+1))+
	*      S_BN*ALF_NB*(TW1(J+1)-TSSS(J+1))

 	ALF_INT(J)=Q_INT/(TW1(J+1)-TL(J+1))			                   !ALF_INT(J)
**************************************************
	WX=GGG/(PW_P*ROV_MID)*DX_P(J)
	BET_X=0.14										   !@
	YX=D_P*ROL_MID*UL(J)*SQRT(0.125*BET_X)/AMU_L
	YX60=YX/60.
	YX601=1.-YX60
 	ALF_CONV=CPL(J+1)*ROL_MID*UL(J)*SQRT(0.125*BET_X)/2.5*
	*(1./(ALOG(YX60)/YX601-1.))*(1./YX601)			               !ALF_CONV

 	ALF_COND=ALF_CONV*   0.2									   !ALF_COND   !@

 	Q_L=(0.16*(Q_INT/(UV(J)*PPPT(J+1)))**0.15*
	*(1.-WX*RRR(J+1)*ROV_MID/Q_INT)**0.7*ALF_CONV+ALF_COND)*
     *(TSSS(J+1)-TL(J+1))									   
**************************************************
	Q_EN=(Q_INT-Q_L)*PW_P/PL_P
**************************************************
	DY(10+(J-1)*JU+8)=0.					                       !TV(J+1)
	DY(10+(J-1)*JU+10)=Q_L*PW_P/(CPL(J+1)*A_PL*ROL(J+1))+
	*Q_EN*PL_P*(TSSS(J+1)-TL(J+1))/(RRR(J+1)*A_PL*ROL(J+1))		   !TL(J+1)
**********
	                   END IF
**********************************************************************	 
*** J_FLOW(J)=3	   ���������� ����� �� ������������ � ����������
	IF(J_FLOW(J).EQ.3) THEN

	ROL_MID=(ROL(J)+ROL(J+1))/2.
	ROV_MID=(ROV(J)+ROV(J+1))/2.
      FI_P(J)=(ROL_MID*UL(J)-GGG_FI/A_P)/(ROL_MID*UL(J)-ROV_MID*UV(J))
	X_P(J)=ROV_MID*UV(J)*A_P*FI_P(J)/GGG_FI

	A_PL=A_P*(1.-FI_P(J))
	D_PL=SQRT(4.*A_PL/PI)
	PL_P=PI*D_PL
**********
	RO_MIX=FI_P(J)*ROV_MID+(1.-FI_P(J))*ROL_MID
********** �������� �������� ������
	DU_V(J)=2.*(ROL_MID/ROV_MID)**0.2*(ROV_MID/ROL_MID)**5.*	   !DU_V(J)
	*(SIGMA(J+1)*G0*(ROL_MID-ROV_MID)/ROV_MID**2)**0.25
**********
	ANUL_MID=(NUL(J)+NUL(J+1))/2.
 	RE_L=UL(J)*D_P/ANUL_MID
	TAU_WL=0.0395*ROL_MID*UL(J)**2*RE_L**(-0.25)					   
	TAU_W=TAU_WL*(1.-FI_P(J)*(1.-ROV_MID/ROL_MID))**0.75*
	*             (1.-X_P(J)*(1.-ROL_MID/ROV_MID))                  !TAU_W
***
	A_EL(J)=1./(ROL_MID*DZ)
	B_EL(J)=J_NAPRIAM*G0*COS(0.)-TAU_W*PW_P/(A_P*RO_MIX)

	ALF_EL(J)=0.
	BET_EL(J)=0.
**********************************************************************
 	PPPB=PPPT(J+1)*1.E-5
	Q1KR=0.14*RRR(J+1)*ROV_MID**0.5*
	*                 (G0*SIGMA(J+1)*(ROL_MID-ROV_MID))**0.25
	T1KR(J+1)=TSSS(J+1)+0.0227*Q1KR**0.5*EXP(-PPPB/86.87)		   !T1KR(J+1)
**********************************************************************
	A_VDV=0.5537               !H2O					 !@	 ������� � ��� ���������
	B_VDV=30.5E-6			   !H2O
	R_VDV=8.314
	T_K=8.*A_VDV/(27.*B_VDV*R_VDV)

	RCL_L=ROL(J+1)*CPL(J+1)*ALAML(J+1)
	IF(J_MET.EQ.1) THEN
	               RO_W=YP(TW1(J+1,TFEM,ROFEM,3)
	               C_W=YP(TW1(J+1,TFEM,CFEM,3)
	               ALAM_W=YP(TW1(J+1,TFEM,LAFEM,3)
	               END IF
	IF(J_MET.EQ.2) THEN
	               RO_W=YP(TW1(J+1,�CUM,ROCUM,16)
	               C_W=YP(TW1(J+1,�CUM,CCUM,16)
	               ALAM_W=YP(TW1(J+1,�CUM,LACUM,16)
	               END IF
	RCL_W=RO_W*C_W*ALAM_W
	T2KR(J+1)=TSSS(J+1)+(T_K-TL(J+1))*
	*              (0.165+2.5*(RCL_L/RCL_W)**0.25+RCL_L/RCL_W)	   !T2KR(J+1
	Q2KR=0.10*RRR(J+1)*ROV_MID*								       !0.05-0.09 or 0.11-0.14 
	*      (G0*SIGMA(J+1)*(ROL_MID-ROV_MID)/ROL_MID**2)**0.25
**********************************************************************
	F_TRANS=((TW1(J+1)-T2KR(J+1))/(T2KR(J+1)-T1KR(J+1)))**2
	Q_INT=F_TRANS*Q1KR+(1.-F_TRANS)*Q2KR

 	ALF_INT(J)=Q_INT/(TW1(J+1)-TL(J+1))			                   !ALF_INT(J)
**********************************************************************
	AMU_L=ROL(J+1)*NUL(J+1)
	AMU_V=ROV(J+1)*NUV(J+1)
**************************************************
	WX=GGG/(PW_P*ROV_MID)*DX_P(J)
	BET_X=0.14										   !@
	YX=D_P*ROL_MID*UL(J)*SQRT(0.125*BET_X)/AMU_L
	YX60=YX/60.
	YX601=1.-YX60
 	ALF_CONV=CPL(J+1)*ROL_MID*UL(J)*SQRT(0.125*BET_X)/2.5*
	*(1./(ALOG(YX60)/YX601-1.))*(1./YX601)			               !ALF_CONV

 	ALF_COND=ALF_CONV*   0.2									   !ALF_COND   !@

 	Q_L=(0.16*(Q_INT/(UV(J)*PPPT(J+1)))**0.15*
	*(1.-WX*RRR(J+1)*ROV_MID/Q_INT)**0.7*ALF_CONV+ALF_COND)*
     *(TSSS(J+1)-TL(J+1))									   
**************************************************
	Q_EN=(Q_INT-Q_L)*PW_P/PL_P
**************************************************
	DY(10+(J-1)*JU+8)=0.					                       !TV(J+1)
	DY(10+(J-1)*JU+10)=Q_L*PW_P/(CPL(J+1)*A_PL*ROL(J+1))+
	*Q_EN*PL_P*(TSSS(J+1)-TL(J+1))/(RRR(J+1)*A_PL*ROL(J+1))		   !TL(J+1)
**********
	                   END IF
**********************************************************************	 
*** J_FLOW(J)=4	   ��������� ������� � ���������� ������ ������� (���� ������)
	IF(J_FLOW(J).EQ.4) THEN
**********************************************************************	 
	A_VDV=0.5537               !H2O					 !@	 ������� � ��� ���������
	B_VDV=30.5E-6			   !H2O
	R_VDV=8.314
	T_K=8.*A_VDV/(27.*B_VDV*R_VDV)

	RCL_L=ROL(J+1)*CPL(J+1)*ALAML(J+1)
	IF(J_MET.EQ.1) THEN
	               RO_W=YP(TW1(J+1,TFEM,ROFEM,3)
	               C_W=YP(TW1(J+1,TFEM,CFEM,3)
	               ALAM_W=YP(TW1(J+1,TFEM,LAFEM,3)
	               END IF
	IF(J_MET.EQ.2) THEN
	               RO_W=YP(TW1(J+1,�CUM,ROCUM,16)
	               C_W=YP(TW1(J+1,�CUM,CCUM,16)
	               ALAM_W=YP(TW1(J+1,�CUM,LACUM,16)
	               END IF
	RCL_W=RO_W*C_W*ALAM_W
	T2KR(J+1)=TSSS(J+1)+(T_K-TL(J+1))*
	*              (0.165+2.5*(RCL_L/RCL_W)**0.25+RCL_L/RCL_W)	   !T2KR(J+1)
**********************************************************************	 
	ROL_MID=(ROL(J)+ROL(J+1))/2.
	ROV_MID=(ROV(J)+ROV(J+1))/2.

	D_PL2=(GGG_FI-ROV_MID*UV(J)*A_P)/
	*(PI/4.*(ROL_MID*UL(J)-ROV_MID*UV(J)))
	IF(D_PL2.LT.0.) WRITE(2,'(F10.I5,E12.5,10H T,J,D_PL2)') T,J,D_PL2
	D_PL=SQRT(ABS(D_PL2))

      FI_P(J)=1.-(D_PL/D_P)**2
	X_P(J)=ROV_MID*UV(J)/GGG_FI*PI/4.*(D_P**2-D_PL**2)

	A_PL=PI/4.*D_PL**2
	PL_P=PI*D_PL
	A_PV=A_P-A_PL
**********
	RO_MIX=FI_P(J)*ROV_MID+(1.-FI_P(J))*ROL_MID
**********
	ANUV_MID=(NUV(J)+NUV(J+1))/2.
 	RE_V=UV(J)*D_P/ANUV_MID
	TAU_W=0.0395*ROV_MID*UV(J)**2*RE_V**(-0.25)				       !TAU_W	   
**********
	AMU_V=ROV(J+1)*NUV(J+1)
	RRR_T=RRR(J+1)+0.25*CPV(J+1)*(TW1(J+1)-TSSS(J+1))
 	ALF_INT_0=(ALAMV(J+1)**3*ROV(J+1)*(ROL(J+1)-ROV(J+1))*G0*RRR_T/
	*(AMU_V*(TW1(J+1)-TSSS(J+1))))**0.25*U_LO**0.4      
	DZ_FR=Z_FR-DZ*(J-0.5)

	DZ_FRT=DZ_FR
	IF(DZ_FRT.LT.0.05) DZ_FRT=0.05
 	ALF_INT(J)=3.566*ALF_INT_0*DZ_FRT**(-0.25)	                   !ALF_INT(J)

	Q_INT=ALF_INT(J)*(TW1(J+1)-TV(J+1))
**************************************************
	BET_L=0.005*(1.+150.*(D_P-D_PL)/D_P)		
	TAU_L=BET_L*ROL(J+1)*(UV(J)-UL(J))**2/2.+					   !@ ROL(J+1)
	*               Q_INT*(UV(J)-UL(J))/RRR(J+1)				       !TAU_L	   
***
	A_EL(J)=1./(ROL_MID*DZ)
	B_EL(J)=J_NAPRIAM*G0*COS(0.)+TAU_L*PL_P/(A_PL*ROL(J+1))

	ALF_EL(J)=1./(ROV_MID*DZ)
	BET_EL(J)=J_NAPRIAM*G0*COS(0.)-(TAU_W*PW_P+TAU_L*PL_P)/
	*(A_PV*ROV(J+1))
**********************************************************************
	A_TEM=ALAML(J+1)/(ROL(J+1)*CPL(J+1))
	PR_L=NUL(J+1)/A_TEM
	Q_L=0.0012*(1.+1.22*EXP(-0.038*DZ_FRT/D_P))*PR_L**(-0.6)*
	*ROL(J+1)*CPL(J+1)*UL(J)*(TSSS(J+1)-TL(J+1))					   !Q_L
**************************************************
	Q_EV=Q_INT*PW_P/PL_P-Q_L									   !Q_EV
**************************************************
	DY(10+(J-1)*JU+8)=0.					                       !TV(J+1)
	DY(10+(J-1)*JU+10)=Q_L*PL_P/(CPL(J+1)*A_PL*ROL(J+1))+
	*Q_EN*PL_P*(TSSS(J+1)-TL(J+1))/(RRR(J+1)*A_PL*ROL(J+1))		   !TL(J+1)
**********
	                   END IF
**********************************************************************	 
*** J_FLOW(J)=5	   ��������� ������� � ���������� ������ ������� (���� ����������� ���������)
	IF(J_FLOW(J).EQ.5) THEN

	A_VDV=0.5537               !H2O					 !@	 ������� � ��� ���������
	B_VDV=30.5E-6			   !H2O
	R_VDV=8.314
	T_K=8.*A_VDV/(27.*B_VDV*R_VDV)

	RCL_L=ROL(J+1)*CPL(J+1)*ALAML(J+1)
	IF(J_MET.EQ.1) THEN
	               RO_W=YP(TW1(J+1,TFEM,ROFEM,3)
	               C_W=YP(TW1(J+1,TFEM,CFEM,3)
	               ALAM_W=YP(TW1(J+1,TFEM,LAFEM,3)
	               END IF
	IF(J_MET.EQ.2) THEN
	               RO_W=YP(TW1(J+1,�CUM,ROCUM,16)
	               C_W=YP(TW1(J+1,�CUM,CCUM,16)
	               ALAM_W=YP(TW1(J+1,�CUM,LACUM,16)
	               END IF
	RCL_W=RO_W*C_W*ALAM_W
	T2KR(J+1)=TSSS(J+1)+(T_K-TL(J+1))*
	*              (0.165+2.5*(RCL_L/RCL_W)**0.25+RCL_L/RCL_W)	   !T2KR(J+1)
**********************************************************************	 
	ROL_MID=(ROL(J)+ROL(J+1))/2.
	ROV_MID=(ROV(J)+ROV(J+1))/2.

	D_PL2=(GGG_FI-ROV_MID*UV(J)*A_P)/
	*(PI/4.*(ROL_MID*UL(J)-ROV_MID*UV(J)))
	IF(D_PL2.LT.0.) WRITE(2,'(F10.I5,E12.5,10H T,J,D_PL2)') T,J,D_PL2
	D_PL=SQRT(ABS(D_PL2))

      FI_P(J)=1.-(D_PL/D_P)**2
	X_P(J)=ROV_MID*UV(J)/GGG_FI*PI/4.*(D_P**2-D_PL**2)

	A_PL=PI/4.*D_PL**2
	PL_P=PI*D_PL
	A_PV=A_P-A_PL
**********
	RO_MIX=FI_P(J)*ROV_MID+(1.-FI_P(J))*ROL_MID
**********
	ANUV_MID=(NUV(J)+NUV(J+1))/2.
 	RE_V=UV(J)*D_P/ANUV_MID
	TAU_W=0.0395*ROV_MID*UV(J)**2*RE_V**(-0.25)				       !TAU_W	   
**********
	AMU_V=ROV(J+1)*NUV(J+1)
	RRR_T=RRR(J+1)+0.25*CPV(J+1)*(TW1(J+1)-TSSS(J+1))
 	ALF_INT_0=(ALAMV(J+1)**3*ROV(J+1)*(ROL(J+1)-ROV(J+1))*G0*RRR_T/
	*(AMU_V*(TW1(J+1)-TSSS(J+1))))**0.25*U_LO**0.4      
	DZ_FR=Z_FR-DZ*(J-0.5)

	DZ_FRT=DZ_FR
	IF(DZ_FRT.LT.0.05) DZ_FRT=0.05
 	ALF_INT(J)=3.566*ALF_INT_0*DZ_FRT**(-0.25)	                   !ALF_INT(J)

	Q_INT=ALF_INT(J)*(TW1(J+1)-TV(J+1))
**************************************************
	BET_L=0.005*(1.+150.*(D_P-D_PL)/D_P)		
	TAU_L=BET_L*ROL(J+1)*(UV(J)-UL(J))**2/2.+					   !@ ROL(J+1)
	*               Q_INT*(UV(J)-UL(J))/RRR(J+1)				       !TAU_L	   
***
	A_EL(J)=1./(ROL_MID*DZ)
	B_EL(J)=J_NAPRIAM*G0*COS(0.)+TAU_L*PL_P/(A_PL*ROL(J+1))

	ALF_EL(J)=1./(ROV_MID*DZ)
	BET_EL(J)=J_NAPRIAM*G0*COS(0.)-(TAU_W*PW_P+TAU_L*PL_P)/
	*(A_PV*ROV(J+1))
**********************************************************************
	A_TEM=ALAML(J+1)/(ROL(J+1)*CPL(J+1))
	PR_L=NUL(J+1)/A_TEM
	Q_L=0.0012*(1.+1.22*EXP(-0.038*DZ_FRT/D_P))*PR_L**(-0.6)*
	*ROL(J+1)*CPL(J+1)*UL(J)*(TSSS(J+1)-TL(J+1))					   !Q_L
**************************************************
	Q_EV=Q_INT*PW_P/PL_P-Q_L									   !Q_EV
**************************************************
	DY(10+(J-1)*JU+8)=0.					                       !TV(J+1)
	DY(10+(J-1)*JU+10)=Q_L*PL_P/(CPL(J+1)*A_PL*ROL(J+1))+
	*Q_EN*PL_P*(TSSS(J+1)-TL(J+1))/(RRR(J+1)*A_PL*ROL(J+1))		   !TL(J+1)
**********
	                   END IF
**********************************************************************	 
*** J_FLOW(J)=6	   ��������� ������� � ���������� ������ ������� 
	IF(J_FLOW(J).EQ.6) THEN

	ROL_MID=(ROL(J)+ROL(J+1))/2.
	ROV_MID=(ROV(J)+ROV(J+1))/2.
**********
	ANUV_MID=(NUV(J)+NUV(J+1))/2.
 	RE_V=UV(J)*D_P/ANUV_MID
	TAU_W=0.0395*ROV_MID*UV(J)**2*RE_V**(-0.25)				       !TAU_W	   
**********
 	RE_VD=(UV(J)-UL(J))*D_FILM(J)/ANUV_MID							 !D_FILM(J) @
	S_FILM=0.45
	IF(RE_VD.LT.2000.) S_FILM=24./RE_VD*(1.+0.142*RE_VD**0.698) 

	TAU_L=0.125*ROV_MID*(UV(J)-UL(J))**2*S_FILM				       !TAU_L	   
**********************************************************************	 
      FI_P(J)=(ROL_MID*UL(J)-GGG_FI/A_P)/(ROL_MID*UL(J)-ROV_MID*UV(J))
	X_P(J)=ROV_MID*UV(J)*A_P*FI_P(J)/GGG_FI

	A_PV=FI_P(J)*A_P
	A_PL=(1.-FI_P(J))*A_P
	PL_P=6.*A_L/D_FILM(J)

	WE_V(J)=ROV(J+1)*(UV(J)-UL(J))**2*D_FILM(J)/SIGMA(J+1)			 !WE_V(J)
**********
	RO_MIX=FI_P(J)*ROV_MID+(1.-FI_P(J))*ROL_MID
**********************************************************************	 
	AMU_V=ROV(J+1)*NUV(J+1)
 	RE_VG=GGG*D_P/(AMU_V*A_P)
							 
	A_TEM=ALAMV(J+1)/(ROV(J+1)*CPV(J+1))
	PR_V=NUV(J+1)/A_TEM
 	ALF_INT(J)=8.348E-3*ALAMV(J+1)/D_P*
	*(RE_VG*(X_P(J)+ROV_MID/ROL_MID*(1-X_P(J)))**0.8774*PR_V**0.6112 !ALF_INT(J)
C6	Y_MIT=1.-0.1*(ROL_MID/ROV_MID-1.)**0.4*(1-X_P(J))**0.4
C6 	ALF_INT(J)=8.348E-3*ALAMV(J+1)/D_P*
C6	*(RE_VG*(X_P(J)+ROV_MID/ROL_MID*(1-X_P(J)))**0.989*
C6     *PR_V**1.41*Y_MIT**(-1.15)	                                   !ALF_INT(J)
**********
	G_FILM=ALAMV(J+1)/(D_P*CPV(J+1))*
	*(2.+0.55*(ROV(J+1)*(UV(J)-UL(J))*D_FILM(J))**0.5*PR_V**0.33)

	CT=ALOG(1.+(CPV(J+1)*(TV(J+1)-TSSS(J+1)))/
	*(CPV(J+1)*TSSS(J+1)-CPL(J+1)*TL(J+1)))					
*** ��������� ��������� ������ � ������ �����
	Q_DR=G_FILM*CT*(CPV(J+1)*TSSS(J+1)-CPL(J+1)*TL(J+1))
*** ��������� ��������� ������ �� �������
	Q_EV=G_FILM*CT*(CPV(J+1)*TSSS(J+1)-CPL(J+1)*TL(J+1))	!@	CPL(J+1)*TL(J+1)
**************************************************
	A_EL(J)=1./(ROL_MID*DZ)
	B_EL(J)=J_NAPRIAM*G0*COS(0.)+SIGN(UV(J)-
	*UL(J))*TAU_L*PL_P/(A_PL*ROL(J+1))

	ALF_EL(J)=1./(ROV_MID*DZ)
	BET_EL(J)=J_NAPRIAM*G0*COS(0.)-TAU_W*PW_P/(A_PV*ROV(J+1))-
	*Q_EV*PL_P/RRR(J+1)*(UV(J)-UL(J))/(A_PV*ROV(J+1))-
     *SIGN(UV(J)-UL(J))*TAU_L*PL_P/(A_PV*ROV(J+1))			  
**************************************************
	DY(10+(J-1)*JU+8)=(ALF_INT(J)*(TW1(J+1)-TV(J+1))*PW_P-
	*Q_DR*PL_P-Q_EV*CPV(J+1)*(TV(J+1)-TSSS(J+1))*PL_P/RRR(J+1))/
     *(CPV(J+1)*ROV_MID*A_PV))				                       !TV(J+1)		 
	DY(10+(J-1)*JU+10)=(Q_DR*PL_P-Q_EV*CPL(J+1)*(TSSS(J+1)-TL(J+1))*		   !@
	*PL_P/RRR(J+1))/(CPL(J+1)*ROL_MID*A_PL))				           !TL(J+1)		 
**********
	                   END IF
**********************************************************************	 
*** J_FLOW(J)=7	   ���������� ��������� ������������� ����
	IF(J_FLOW(J).EQ.7) THEN

	ROV_MID=(ROV(J)+ROV(J+1))/2.
	ANUV_MID=(NUV(J)+NUV(J+1))/2.
C1	U_L=GGG/(A_P*ROL_MID)
C1	RE_L=U_L*D_P/ANUL_MID
	RE_V=UV(J)*D_P/ANUV_MID
	TAU_W=0.0395*ROV_MID*UV(J)**2*RE_V**(-0.25)					   !TAU_W

	A_EL(J)=O.
	B_EL(J)=0.

	ALF_EL(J)=1./(ROV_MID*DZ)
	BET_EL(J)=J_NAPRIAM*G0*COS(0.)-TAU_W*PW_P/(A_P*ROV_MID)
**********************************************************************
	X_P(J)=1.
      FI_P(J)=1.
**********
	RE_V=UV(J)*D_P/NUV(J+1)

	A_TEM=ALAMV(J+1)/(ROV(J+1)*CPV(J+1))
	PR_V=NUV(J+1)/A_TEM

	AMU_V=ROV(J+1)*NUV(J+1)
 	ROVW=YP2(TW1(J+1),PPPB,TROM,PROM,ROM,53,30,53,30)	 !		 
  	ANUVW=YP2(TW1(J+1),PPPB,TNUM,PNUM,NUM,53,30,53,30)	 !
	AMU_VW=ROVW*ANUVW

	ALF_INT(J)=0.023*ALAMV(J+1)/D_P*RE_V**0.8*PR_V**0.33*
	*           (AMU_V/AMU_VW)**0.14			                       !ALF_INT(J)
**********
	DY(10+(J-1)*JU+8)=ALF_INT(J)*(TW1(J+1)-TV(J+1))*
	*PW_P/(CPV(J+1)*A_P*ROV(J+1))					               !TV(J+1)
	DY(10+(J-1)*JU+10)=0.					                       !TL(J+1)
**********
* ROL(J+1),ROV(J+1),NUL(J+1),NUV(J+1),CPL(J+1),CPV(J+1),ALAML(J+1),ALAMV(J+1),SIGMA(J+1),RRR(J+1),TSSS(J+1)
**********
	                   END IF
**********************************************************************
	A_ELM(J)=A_EL(J)*ROL(J)*A_P*(1.-FI_P(J))
	B_ELM(J)=B_EL(J)*ROL(J)*A_P*(1.-FI_P(J))

	ALF_ELM(J)=ALF_EL(J)*ROV(J)*A_P*FI_P(J)
	BET_ELM(J)=BET_EL(J)*ROV(J)*A_P*FI_P(J)
**********************************************************************
C1	DY(10+(J-1)*JU+1)=XXX						         !TW1(J+1)	   
	IF(J_MET.EQ.1) THEN
	               RO_W=YP(TW2(J+1,TFEM,ROFEM,3)
	               C_W=YP(TW2(J+1,TFEM,CFEM,3)
	               ALAM_W=YP(TW2(J+1,TFEM,LAFEM,3)
	               END IF
	IF(J_MET.EQ.2) THEN
	               RO_W=YP(TW2(J+1,�CUM,ROCUM,16)
	               C_W=YP(TW2(J+1,�CUM,CCUM,16)
	               ALAM_W=YP(TW2(J+1,�CUM,LACUM,16)
	               END IF
	A_W=ALAM_W/(RO_W*C_W)
	R_2=D_P/2.+DR
	DY(10+(J-1)*JU+2)=((TW3(J+1)-TW1(J+1))/(R_2*2.*DR)+
	*(TW3(J+1)-2.*TW2(J+1)+TW1(J+1))/DR**2+
     *(TW2(J+2)-2.*TW2(J+1)+TW2(J))/DZ**2)*A_W			 !TW2(J+1)
***
	IF(J_MET.EQ.1) THEN
	               RO_W=YP(TW3(J+1,TFEM,ROFEM,3)
	               C_W=YP(TW3(J+1,TFEM,CFEM,3)
	               ALAM_W=YP(TW3(J+1,TFEM,LAFEM,3)
	               END IF
	IF(J_MET.EQ.2) THEN
	               RO_W=YP(TW3(J+1,�CUM,ROCUM,16)
	               C_W=YP(TW3(J+1,�CUM,CCUM,16)
	               ALAM_W=YP(TW3(J+1,�CUM,LACUM,16)
	               END IF
	A_W=ALAM_W/(RO_W*C_W)
	R_3=D_P/2.+DR+DR
	DY(10+(J-1)*JU+3)=((TW4(J+1)-TW2(J+1))/(R_3*2.*DR)+
	*(TW4(J+1)-2.*TW3(J+1)+TW2(J+1))/DR**2+
     *(TW3(J+2)-2.*TW3(J+1)+TW3(J))/DZ**2)*A_W			 !TW3(J+1)
***
	DY(10+(J-1)*JU+4)=0.						         !TW4(J+1)
*****************
44    CONTINUE
********************* ����������� ������������� �������� ************* 
	IF(J_FRONT.EQ.1) THEN
	     PPPP(1)=P_TO+DP_P-AKSIO/RO_LO*ABS(GGG)*GGG-AJO*DM_DT
	     PPPP(2)=P_TO !P_ENV
	     GOTO 477
		             END IF
********************* ���������� ������� ��������	
**                       A(K,K)*X(K)=B(K)

      DO 46 J=1,J_FRONT+1	 !1,NJ+1
	B_PPP(J)=0.
      DO 45 I=1,J_FRONT+1	 !1,NJ+1
	A_PPP(J,I)=0.
45    CONTINUE
46    CONTINUE								    
***
      DO 47 J=1,J_FRONT+1
	IF(J.EQ.1) THEN															                              
	           A_PPP(J,J)=1.
	           B_PPP(J)=P_TO+DP_P-AKSIO/RO_LO*ABS(GGG)*GGG-AJO*DM_DT
	           GOTO 47
	           END IF
	IF(J.EQ.J_FRONT+1) THEN
	                   A_PPP(J,J)=1.
	                   B_PPP(J)=P_TO     !P_ENV
	                   GOTO 47
	                   END IF

	A_PPP(J,J-1)=A_ELM(J-1)+ALF_ELM(J-1)
	A_PPP(J,J)=-A_ELM(J-1)-ALF_ELM(J-1)-A_ELM(J)-ALF_ELM(J)
	A_PPP(J,J+1)=A_ELM(J)+ALF_ELM(J)

	B_PPP(J)=-B_ELM(J-1)-BET_ELM(J-1)+B_ELM(J)+BET_ELM(J)
47    CONTINUE

C	IF(T.GT.0.1) THEN
C      DO 478 J=1,J_FRONT+1
C      WRITE(2,'(30E12.5)') (A_PPP(J,I),I=1,J_FRONT+1)
C478   CONTINUE
C      WRITE(2,'(30E12.5)') (B_PPP(I),I=1,J_FRONT+1)
C	             END IF

**********************************************************************
      CALL GAUSS_PPP_2026(A_PPP,B_PPP,PPPP,NJ+1,J_FRONT+1)

C	IF(T.GT.0.1) THEN
C      WRITE(2,'(30E12.5)') (PPPP(I),I=1,J_FRONT+1)
C	             STOP
C	             END IF
**********************************************************************
CX	MAX PPPP
***********************
477   CONTINUE

      DO 481 J=J_FRONT+1,NJ
	PPPP(J)=P_TO  ! P_ENV
481   CONTINUE

	JU=10
      DO 48 J=1,J_FRONT    !NJ
	DY(10+(J-1)*JU+7)=(PPPP(J)-PPPP(J+1))*ALF_EL(J)+BET_EL(J)	   !UV(J)
CX	DY(10+(J-1)*JU+8)=XXX					                       !TV(J+1)
	DY(10+(J-1)*JU+9)=(PPPP(J)-PPPP(J+1))*A_EL(J)+B_EL(J)		   !UL(J)
CX	DY(10+(J-1)*JU+10)=XXX					                       !TL(J+1)
48    CONTINUE

***


CX	DY(10+(J-1)*JU+7)=XXX					             !UV(J)		@
	DY(10+(J-1)*JU+8)=XXX					             !TV(J+1)
CX	DY(10+(J-1)*JU+9)=XXX					             !UL(J)
	DY(10+(J-1)*JU+10)=XXX					             !TL(J+1)


**********************************************************************  	 
	ROMIX=0.
      DO 51 I=1,J_FRONT
51	ROMIX=ROMIX+FI_P(I)*ROV(I)+(1-FI_P(I))*ROL(I)
	ROMIX=ROMIX/J_FRONT

      A_KLAP=YP(T,TKL_M,AKL_M,4)
	DY(4)=GGG/(ROMIX*A_P) *(1.- A_KLAP)						       !Z_FR
	IF(Z_FR.GT.ZZZ) DY(4)=0.
**********************************************************************	 
**********************************************************************  	 
488   CONTINUE
      A_KLAP=YP(T,TKL_M,AKL_M,4)

	AJT_T=AJT*A_KLAP
	AKSIT_T=AKSIT*A_KLAP
	P_OUT=P_TO                 !P_ENV+(P_TO-P_ENV)*A_KLAP
***********************
CX	MAX P_OUT
***********************
	DY(3)=(P_TO+DP_P-(AKSIO+AKSIT_T)/RO_LO*ABS(GGG)*GGG-DP_L-DP_V-
	*P_OUT)/(AJO+AJT_T+D_J)										   !GGG	 
**********************************************************************
      RETURN
      END
**********************************************************************
      FUNCTION YP(X,X1,Y,N1)
***  �������� ������������ ������� Y(N1) ***
      DIMENSION X1(N1),Y(N1)
      XX1=X
      IF(X.LT.X1(1))  XX1=X1(1)
      IF(X.GT.X1(N1)) XX1=X1(N1)
         DO 1 I=2,N1
         IF(X1(I).LT.XX1) GOTO 1
         K1=I-1
         K2=I
         GOTO 2
1        CONTINUE
2     A1=(Y(K1)-Y(K2))/(X1(K1)-X1(K2))
      B1=Y(K1)-A1*X1(K1)
      YP=A1*XX1+B1
      RETURN
      END
**********************************************************************
      FUNCTION YP2(XX1,XX2,XM1,XM2,Y,N1,N2,K1,K2)
***  �������� ������������ ������� Y(N1,N2) �� 2-� ���������� ***
      REAL XM1(N1),XM2(N2),Y(N1,N2)
      X1=XX1
      IF(X1.LT.XM1(1))  X1=XM1(1)
      IF(X1.GT.XM1(K1)) X1=XM1(K1)
      X2=XX2
      IF(X2.LT.XM2(1))  X2=XM2(1)
      IF(X2.GT.XM2(K2)) X2=XM2(K2)
         DO 1 I1=2,K1
         IF(XM1(I1).LT.X1) GOTO 1
         K1J1=I1-1
         K1J2=I1
         GOTO 2
1        CONTINUE
2        DO 3 I2=2,K2
         IF(XM2(I2).LT.X2) GOTO 3
         K2J1=I2-1
         K2J2=I2
         GOTO 4
3        CONTINUE
4        CONTINUE
      Y1=Y(K1J1,K2J1)+(Y(K1J2,K2J1)-Y(K1J1,K2J1))/
     *(XM1(K1J2)-XM1(K1J1))*(X1-XM1(K1J1))
      Y2=Y(K1J1,K2J2)+(Y(K1J2,K2J2)-Y(K1J1,K2J2))/
     *(XM1(K1J2)-XM1(K1J1))*(X1-XM1(K1J1))

      YP2=Y1+(Y2-Y1)/(XM2(K2J2)-XM2(K2J1))*(X2-XM2(K2J1))
      RETURN
      END
**********************************************************************
      SUBROUTINE GAUSS_PPP_2026(A,B,X,N,K)
** ������� ����������� ��������� ������� �������������� ��������� ������� ������      
**              A(K,K)*X(K)=B(K)

      REAL A(N,N),B(N),X(N)

C      DO 478 J=1,K
C      WRITE(2,'(30E12.5)') (A(J,I),I=1,K)
C478   CONTINUE
C      WRITE(2,'(30E12.5)') (B(I),I=1,K)

*** ���������� � ������������ ����
      DO 14 I=2,K
	AAA=A(I,I-1)/A(I-1,I-1)
      A(I,I)=A(I,I)-AAA*A(I-1,I)
      B(I)=B(I)-AAA*B(I-1)
14    CONTINUE

C      DO 479 J=1,K
C      WRITE(2,'(30E12.5)') (A(J,I),I=1,K)
C479   CONTINUE
C      WRITE(2,'(30E12.5)') (B(I),I=1,K)

*** ���������� X(K)
	X(K)=B(K)
      DO 15 I=2,K
	J=K-I+1
      B(J)=B(J)-A(J,J+1)*X(J+1)
 	X(J)=B(J)/A(J,J)
15    CONTINUE
***
C      WRITE(2,'(30E12.5)') (X(I),I=1,K)

      RETURN
      END
**********************************************************************
