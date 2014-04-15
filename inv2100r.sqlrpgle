        // ============================================================
     H BndDir('OTHER':'DLL')
     H Option(*SrcStmt : *NoDebugIO)
        // ============================================================
      ***                                                           ***
      ***                   Heartland Co-op                         ***
      ***           2829 Westown Parkway, Suite 350                 ***
      ***             West Des Moines, Iowa  50266                  ***
      ***                                                           ***
        // ============================================================
      ***                                                           ***
      *** Program: Maintain Location Item Storage Capacity          ***
      ***                                                           ***
        // ============================================================
      *                PROGRAM MODIFICATIONS SUMMARY                  *
      *                                                               *
      *   Date    I.D.  Comments                                      *
      * --------  --- ------------------------------------------------*
      * 01/02/14  MDA  Created.                                       *
        // ============================================================
      * Indicator Summary                                             *
      * INDICATOR 90: CLEAR SUBFILE  N90: DISPLAY SUBFILE CONTROL     *
      * INDICATOR 91: END OF FILE - NO PAGING OR + SIGN               *
      * INDICATOR 92: DISPLAY THE SUBFILE                             *
      * INDICATOR 95: FIELD IN ERROR - FLAG RECORD AS CHANGED         *
      * INDICATOR 98: DO NOT DISPLAY THE ERROR SUBFILE CONTROL        *
      * Indicator 99 - Error                                          *
        // ============================================================
     FINV2100D  CF   E             Workstn InfDS(InfDS)
     F                                     SFile(SCR03S:SflRRN)
     F                                     SFile(Scr04S:SflRRN)
     FLocMast01 IF   E           K Disk                                         Location Master
     FInvMast   UF   E           K Disk                                         Inventory Master
     FLOCSTORG  UF A E           K Disk                                         Location Storage
      *                                Key: LONUM LOTITEM
        // =====================================================================
     D/Copy QCpySRC,@STDSDS
     D/Copy QCpySRC,@STDAIB
     D/Copy QCpySRC,@STDPOSCSR
     D/Copy QCpysrc,@Stdfld
     D/COPY QCPYSRC,SRVDLLPR
     D/COPY QCPYSRC,GETPOS
        // =====================================================================

        //    P r o g r a m   C a l l s                         (alphabetical)
     DLocationSearch   PR                  ExtPgm('GRN091')
     D PLoc                           2P 0
     D Line                           2P 0
     D Pos                            2P 0

     DProdSearch       PR                  ExtPgm('INV000')
     D Pitem                          7  0
        // ---------------------------------------------------------------------
        //   A R R A Y S  &  T A B L E S                        (alphabetical)
     D SflA            S              4  0 Dim(2)
     D SflC            S              1    Dim(2)
     D SflD            S              1    Dim(2)
     D SflE            S              1    Dim(2)
     D SflS            S              4  0 Dim(2)
     D SFLH            S              4  0 Dim(2)
     D SFLP            S              4  0 Dim(2)
        // ---------------------------------------------------------------------
        //   V A R I A B L E S
     D @Item           S                   Like(InItem)    Inz
     D @Line           S              2P 0                 Inz(4)
     D @Loc2           S              2P 0                 Inz                  2-Digit Location
     D @Position       S              2P 0                 Inz(50)              Grain Position ???
     D S               S              1  0
     D I_Say_So        S               N
     D Error_Offset    S              2  0
     D FirstError      S               N
     D Pitem           S              7  0
     D PLoc            S              2P 0
     D Line            S              2P 0 Inz(4)
     D Pos             S              2P 0 Inz(50)
     D SFLRead         S                   Like(Sflrrn)
     D SetDate         S               D
     D V_ScrFmt        S             10A   INZ(*BLANKS)
     D V_ScrFld        S             10A   INZ(*BLANKS)
     D N_Position      S               N   INZ(*OFF)
        // ---------------------------------------------------------------------
        //   C O N S T A N T S                                  (alphabetical)
     D CompanyDefault  C                   Const('Company Default')
     D F5Key           C                   'F5=Proceed  '
     D F9Key           C                   'F9=Search  '
     D F12Key          C                   'F12=Previous  '
        // ---------------------------------------------------------------------
        //   2 Error codes. S999999 open MSGF variable. You put in your
        //   info. Also newer logic to ID the cursor position in the event
        //   an error is generated during Db maintenance.

     D ErrorCodes      DS
     D                                7    Inz('S999001')                       01
     D                                7    Inz('S999999')                       02
     D Error                          7    Dim(2)
     D                                     Overlay(ErrorCodes)
      ******************************************
     D @RTNINFO        DS
     D   @ROW                         3S 0 INZ
     D   @COL                         3S 0 INZ
      ******************************************
      * Indicators
     D Indicators      DS                  Based(IndicatorPtr)
     D  ProtectFld            25     25N
     D  Sfl_NxtChg            95     95N
     D Screen_Err             99     99N
      ******************************************
      * Key Data Structures
     D KeyLoc          DS                  LikeRec(LocMastR:*Key)

     D PrntCdeDS       DS                  Inz
     D InPrnt
     D  ChkPrtCode                    4  0 Overlay(Inprnt)
      ******************************************
     D LOItmPriDS      DS                  LikeRec(LOCSTORR:*Input)
      *************************************
     D SetDateDS       DS                  Inz
     D SetYear                        4S 0
     D  SetCC                         2S 0 Overlay(SetYear)
     D  SetYY                         2S 0 Overlay(SetYear:*Next)
        // =====================================================================

        // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
        //  M A I N L I N E
        // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

      /Free

       @MsgF = 'SALESERRS';
       @MsgLib = '*LIBL';

       Clear WSItem;
       Clear WSLoc;

       Dou AIB = Exit;
         Write MsgCtl;
         Write SCR00;
         Exfmt SCR01;

         Exsr ClrMsg;


         // Process Function Key Requests;
         Select;
         When AIB = Search;
           Exsr Search01;
           Iter;
         When AIB = Exit;
           Leave;
         ENDSL;


         V_Scrfld = 'WSItem';
         If WSitem   = 0   and Wsloc  = 0;
           E =  2;
           @MSgdta = 'Both Item and Location Cannot be Blank';
           @Dtalen = %Len(%Trim(@MSgdta));
           Exsr Sndmsg;
           iter;
         Endif;

         Exsr Chk01;
         If Scr_Error;
           Iter;
         ENDIF;

         // Display Proper Maintanence screen
         Select;
         When WSItem <> 0 and WSLoc <> 0;
           Exsr DspSCR02;
         When WSItem <> 0;
           Exsr DspSCR03;
         Other;
           Exsr DspSCR04;
         ENDSL;
       ENDDO;

       *InLR=*On;

        // ============================================================
       // Validate Scr01
       Begsr Chk01;

       V_ScrFmt = 'SCR01';
       N_Position = *OFF;
       If WSItem <> 0;
         Chain WsItem InvMast;
         If  NOT %Found(InvMast);
           E = 2;
           @MSgdta = 'Item Not Found';
           @Dtalen = %Len(%Trim(@MSgdta));
           ExSR SndMsg;
         EndIF;
       Endif;

       If WSLoc <> 999 and WSLoc <> 0;
         Keyloc.BrStatus = *Blanks;
         KeyLoc.BrNumb = WSLoc;
         Chain %KDS(KeyLoc) Locmast01;
         If Not %Found(Locmast01);
           E = 2;
           @MSgdta = 'Location/Branch Not Found';
           @Dtalen = %Len(%Trim(@MSgdta));
           Exsr Sndmsg;
         ENDIF;
       ENDIF;

       ENDSR;

       // You took them to one Item & Location,only Capacity is checked

       Begsr Chk02;

         V_ScrFld = 'LOSTORAGE';
         If LOSTORAGE < 0;
           E = 2;
           @Msgdta = 'Invalid Storage Value';
           @Dtalen = %Len(%Trim(@Msgdta));
           Exsr Sndmsg;
         ENDIF;


       ENDSR;

       // ******************************************

       Begsr Chk03;

       Sfl_nxtChg = *On;
       V_ScrFmt = 'SCR03S';
       N_Position = *OFF;

       Dou %EOF(INV2100D);
         ReadC SCR03S;
         If %EOF(INV2100D);
           Leave;
         ENDIF;

         If (LOSTORAGE = 0);
           Update SCR03S;
           Iter;
         ENDIF;

         Clear BrName;
           V_ScrFld = 'LONUM';
           Keyloc.BrStatus = *Blanks;
           KeyLoc.BrNumb = LONUM;
           Chain %KDS(KeyLoc) Locmast01;
           If Not %Found(Locmast01);
             E = 2;
             @MSgdta = 'Location Not Found';
             @Dtalen = %Len(%Trim(@MSgdta));
             Exsr SndMsg;
         ENDIF;

         // Invalid or negative storage value


         V_ScrFld = 'LOSTORAGE';
         If LOSTORAGE < 0;
           E = 2;
           @Msgdta = 'Invalid Storage Value';
           @Dtalen = %Len(%Trim(@Msgdta));
           Exsr Sndmsg;
         ENDIF;

         Update SCR03S;

       ENDDO;

       Sfl_NxtChg = *Off;

       ENDSR;

       // Validate SCR04

       Begsr Chk04;

       Sfl_nxtChg = *On;
       V_ScrFmt = 'SCR04S';
       N_Position = *OFF;

       Dou %EOF(INV2100D);
         ReadC SCR04S;
         If %EOF(INV2100D);
           Leave;
         ENDIF;

         If (LOStorage = 0 or LOITEM  <> 0);
           Chain(N) LOItem InvMast;
           Update SCR04S;
         ENDIF;

         If (LOITEM <> 0 AND LOSTORAGE <> 0);
         Clear InDesc;
         V_ScrFld = 'LOITEM';
         Chain(N) LOItem InvMast;
           If  NOT %Found(InvMast);
             E =  2;
             @MsgDta = 'Item Does Not Exist';
             @Dtalen = %Len(%Trim(@Msgdta));
             Exsr Sndmsg;
           ENDIF;

         V_ScrFld = 'LOITEM';
         Setll (0:LOItem) LOCSTORG;
           If %Equal;
             E = 2;
             @MsgDta = 'Item already exists';
             @Dtalen = %Len(%Trim(@Msgdta));
             Exsr Sndmsg;
           EndIf;

         V_ScrFld = 'LOSTORAGE';
           If LOSTORAGE < 0;
             E = 2;
             @Msgdta = 'Invalid Storage Value';
             @Dtalen = %Len(%Trim(@Msgdta));
             Exsr Sndmsg;
           ENDIF;
         ENDIF;

       ENDDO;

       Sfl_NxtChg = *Off;

       ENDSR;


       // Display SCR02 (Item/Loc Record)

       Begsr DspSCR02;

       // Get Record Requested

       Chain (WSLoc:WSitem) LOCSTORG;
       If Not %Found(LOCSTORG);
         LOItem = WSItem;
         LONUM = WSLoc;
       Endif;

       KeyLoc.BrNumb   = WsLoc;
       Chain %KDS(KeyLoc) LocMast01;
       If NOT %Found(LocMast01);
         BrLocName = CompanyDefault;
       EndIF;

       Dou I_Say_So;
         Cmdkey = F5Key + F12Key;
         Write MsgCtl;
         Write SCR00;
         Write Footer;
         Exfmt SCR02;

         Exsr ClrMsg;

         Select;
         When AIB = Prev;
           Leave;
         ENDSL;

         Exsr Chk02;

         If AIB = Proceed and Not SCR_Error;
           Exsr UpdateDB;
           Leave;
         ENDIF;

       ENDDO;

       ENDSR;

       // Display SCR03S

       Begsr DspSCR03;

       S = 1;
       Clear SFLC(S);
       SFLH(S) = 6;
       SFLP(S) = 15;
       Dou I_Say_So;
         CmdKey = F5Key + F9Key + F12Key;
         Exsr Sflsts;
         Write MsgCtl;
         Write SCR00;
         Write Footer;
         Exfmt SCR03C;

         Exsr SflSav;
         Exsr Clrmsg;

         Select;
         When AIB = Search;
           Exsr Search03;
         When AIB = Prev;
           Leave;
         Other;
           Exsr Chk03;
         ENDSL;

         If AIB = Proceed and Not Scr_Error;
           Exsr Upd03;
           Leave;
         ENDIF;
       ENDDO;

       ENDSR;

       // Display SCR04S (Capcities for 1 location all Items)

       Begsr DspSCR04;

       S = 2;
       Clear SFLC(S);
       SFLH(S) = 6;
       SFLP(S) = 15;
       Dou I_Say_So;
         CmdKey = F5Key + F9Key + F12Key;
         Exsr Sflsts;
         Write MsgCtl;
         Write SCR00;
         Write Footer;
         Exfmt SCR04C;

         Exsr SflSav;
         Exsr Clrmsg;

         Select;
         When AIB = Search;
           Exsr Search04;
         When AIB = Prev;
           Leave;
         Other;
           Exsr Chk04;
         ENDSL;

         If AIB = Proceed and Not Scr_Error;
           Exsr Upd04;
           Leave;
         ENDIF;
       ENDDO;

       ENDSR;
       // ******************************************
       Begsr Search01;

       Select;
       When @FLD = 'WSITEM';
         ProdSearch(PItem);
         If PItem <> 0;
           WSItem = PItem;
         ENDIF;
       When @FLD = 'WSLOC';
         LocationSearch(PLoc:Line:Pos);
         If PLoc <> 0;
           WSLoc = PLoc;
         ENDIF;
       ENDSL;

       ENDSR;

       // ******************************************

       Begsr Search03;

       Chain Sflcsr SCR03s;
       If %Found(INV2100D);
         Update SCR03S;
         E = 2;
         @MSgdta = 'Search is not allowed on this line';
         @Dtalen = %Len(%Trim(@MSgdta));
         Exsr Sndmsg;
       Else;
         Exsr Clrmsg;
         Clear SCR03S;
         LocationSearch(Ploc:Line:Pos);
         If PLoc <> 0;
           LONUM = Ploc;
           Keyloc.BrStatus = *Blanks;
           KeyLoc.BrNumb = LONUM;
           Chain %KDS(KeyLoc) Locmast01;
           Sflrrn = Sflcsr;
           Protectfld = *Off;
           Sfl_NxtChg = *On;
           Write SCR03S;
         ENDIF;
       ENDIF;

       SFL_NxtChg = *Off;
       ENDSR;
       // ******************************************
       // ******************************************
       Begsr Search04;

       Chain Sflcsr SCR04S;
       If %Found(INV2100D);
         Update SCR04S;
         E = 2;
         @MSgdta = 'Search is not allowed on this line';
         @Dtalen = %Len(%Trim(@MSgdta));
         Exsr Sndmsg;
         Row = 7;
       Else;
         Exsr Clrmsg;
         Clear SCR04S;
         ProdSearch(PItem);
         If PItem <> 0;
           LOItem = PItem;
           Chain(N) LOITEM INVMAST;
           Sflrrn = Sflcsr;
           Protectfld = *Off;
           Sfl_NxtChg = *On;
           Write SCR04S;
         ENDIF;
       ENDIF;

       SFL_NxtChg = *Off;
       ENDSR;

       // Write SCR03

       Begsr WRT03;

         EXEC SQL
          DECLARE PRODCSR SCROLL CURSOR FOR SELECT LONUM, LOSTORAGE
          FROM LOCSTORG, LOCMAST01
          WHERE LOITEM = :wsitem
          AND   LONUM = BRNUMB
          AND   BRSTATUS = ' '
          ORDER BY LOCSTORG.LONUM
          FOR READ ONLY WITH NC;
         EXEC SQL
          OPEN Prodcsr;

         Chain(N) WSItem Invmast;
         Clear SFLRRN;
         Sfl_NxtChg = *OFF;
         ProtectFld = *On;
         Dou SQLCod <> 0;

         EXEC SQL
          FETCH NEXT FROM Prodcsr INTO
          :LONUM, :LOSTORAGE;


         If SQLCod <> 0;
           Leave;
         ENDIF;

         Keyloc.BrStatus = *Blanks;
         KeyLoc.BrNumb = LONum;
         Chain %KDS(KeyLoc) Locmast01;

         LOITEMH = WSITEM;
         LONUMH  = BRNUMB;

         SFLRRn += 1;
         Write SCR03S;
       ENDDO;

         EXEC SQL
          CLOSE Prodcsr;

       ProtectFld = *OFF;
       Sfl_NxtChg = *Off;
       SflE(S) = 'Y';
       If SflA(S) <> SflRRN;
         Eval SflA(S) = SflRRN;
         SflPos = SflA(S);
       EndIf;
       Sflpos = 1;

       ENDSR;

       // Write SCR04

       Begsr WRT04;

       EXEC SQL
         DECLARE LOCCSR SCROLL CURSOR FOR SELECT LOCSTORG.LOITEM, LOSTORAGE
          FROM LOCSTORG
          WHERE LONUM = :WsLoc ORDER BY
          LOCSTORG.LOItem FOR READ ONLY WITH NC;
         EXEC SQL
          OPEN Loccsr;

       If WSLoc <> 999;
         Keyloc.BrStatus = *Blanks;
         KeyLoc.BrNumb = WSLoc;
         Chain %KDS(KeyLoc) Locmast01;
       Endif;

       Clear SFLRRN;
       ProtectFld = *ON;
       Sfl_NxtChg = *Off;
       Dou SQLCod <> 0;

         EXEC SQL
          FETCH NEXT FROM Loccsr INTO
          :LOItem, :LOSTORAGE;

         If SQLCod <> 0;
           Leave;
         ENDIF;

         Chain(N) LOItem Invmast;

         LOITEMH = LOITEM;
         LONUMH  = BRNUMB;

         SFLRRn += 1;
         Write SCR04S;
       ENDDO;

       EXEC SQL
        CLOSE Loccsr;

       ProtectFld = *OFF;
       Sfl_NxtChg = *Off;
       SflE(S) = 'Y';
       If SflA(S) <> SflRRN;
         Eval SflA(S) = SflRRN;
         SflPos = SflA(S);
       EndIf;

       Sflpos = 1;

       ENDSR;

       // Update File from SCR03 By Location Number

       Begsr Upd03;

       Sfl_NxtChg = *On;
       Dou %EOF(INV2100D);
         Readc SCR03S;
         If %EOF(INV2100D);
           Leave;
         ENDIF;

         Chain (LONUM:INITEM) LOCSTORG LOItmPriDS;
         LOItem = InItem;
         Exsr UpdateDB;

       ENDDO;

       ENDSR;

       // Update File from SCR04

       Begsr Upd04;

       Sfl_NxtChg = *On;
       Dou %EOF(INV2100D);
         Readc SCR04S;
         If %EOF(INV2100D);
           Leave;
         ENDIF;

         Chain (BrNumb:LOItem) LOCSTORG LOItmPriDS;
           LONUM = BrNumb;
           Exsr UpdateDB;

       ENDDO;

       ENDSR;

       // Update Database File

       Begsr UpdateDB;


       If LOITEM <> 0;
         If %Found(LOCSTORG);
           Eval      LOCHGPGM = 'INV2100R';
           Eval      LOCHGDATE = %Date;
           Eval      LOCHGTIME = %Time;
           Eval      LOCHGUSER = @USer;
           Update LOCSTORR;
         Else;
           If LONUM <> 0;
           Eval      LOADDPGM = 'INV2100R';
           Eval      LOADDDATE = %Date;
           Eval      LOADDTIME = %Time;
           Eval      LOADDUSER = @USer;
           Write LOCSTORR;
       ENDIF;
         ENDIF;
           ENDIF;

       If LOItem = 0 and LOStorage = 0
         or lostorage = 0 and lonum = 0;
         Exec Sql
          DELETE FROM LOCSTORG
          WHERE LOCSTORG.LONUM = :LONUMH AND LOCSTORG.LOITEM = :LOITEMH
          OR LOCSTORG.LONUM = :LONUMH AND LOCSTORG.LOITEM = 0;
       Endif;

       ENDSR;

       // ******************************************

       BegSr ClrSfl;

       SflC(S) = 'Y';
       Clear SflA(S);
       Clear SflE(S);
       Clear SflS(S);
       *IN90 = *ON;

       Select;
       When S = 1;
         Sfld(S) = Yes;
         Write SCR03C;
         Exsr Wrt03;
       When S = 2;
         Sfld(S) = Yes;
         Write SCR04C;
         Exsr Wrt04;
       EndSl;

       *IN90 = *OFF;

       EndSr;
       //*****************************************
       // Save Subfile Cursor RRN
       BegSr SflSav;
       Clear SflPos;
       If SflCsr > 0;
         SflS(S) = SflCsr;
       Else;
         SflS(S) = SflRec;
       EndIf;
       EndSr;
       //*****************************************
       // Check SubFile Status
       BegSr SflSts;
         // Clear The SubFile If Needed
         *IN92 = *OFF;
       // See If SubFile Needs To Be Displayed
       If SFLC(S) = *Blank;
         Exsr ClrSfl;
       Endif;

       If (SflA(S) > 0) Or
             (SflD(S) = 'Y');
         *In92 = *On;
       EndIf;
       // Position The Cursor If Not Already Done
       If *In92;
         If SflPos = 0;
           If SflS(S) > 0;
             SflPos = SflS(S);
           Else;
             SflPos = 1;
           EndIf;
         EndIf;
       EndIf;

         // For Error Positioning
         SFLStart = SFLH(S);
       //  See If End Of File
       *In91 = (SflE(S) = 'Y');
       EndSr;
       // ******************************************
       BEGSR *INZSR;

       // get Position pointer
       If #GetPos = *Null;
           #GetPos = GetProcPtr('SRVUTIL   ':
                                '*LIBL     ':
                                'GETPOS    ');
       EndIf;

       Endsr;

        // ============================================================
         BegSr $Position;  // Set Cursor Position for a Display Field

           // Set Parm Values
           ScrFmt = V_ScrFmt;
           ScrFld = V_ScrFld;

           // Retrieve The Beginning Position For The Specified Field
           @RTNInfo = GetPos(DvFile: ScrFmt: ScrFld);

           // Indicate Positioning Completion and Set The Location
           N_Position = *On;
           If SFLRRN <> 0;
             @Row = (%Rem(Sflrrn:SFLP(S)) + SFLH(S));
           ENDIF;
           Row = @Row;
           Col = @Col;
         EndSr;
       // ******************************************
      /End-Free
      ******************************************
      * CLEAR MESSAGE SUBFILE
     C     CLRMSG        BEGSR
     C                   CALL      'QMHRMVPM'
     C                   PARM                    @PGMQ
     C                   PARM                    @STKCNT
     C                   PARM                    @MSGKY
     C                   PARM                    @MSGRMV
     C                   PARM                    @ERRCOD
     C                   Clear                   Row
     C                   Clear                   Col
     C                   Eval      Screen_Err = *Off
     C                   ENDSR
      ******************************************
      * SEND MESSAGE TO MESSAGE SUBFILE
     C     SNDMSG        BEGSR
      /FREE
       Error_Ind = 'Y';
       Screen_Err = *ON;
       Err = Error(E);
       @MsgID = MsgID;
       @DtaLen = %Len(%Trim(@MsgDta));
       If Not N_Position;
         Exsr $Position;
       EndIf;

      /END-FREE
     C                   CALL      'QMHSNDPM'
     C                   PARM                    @MSGID
     C                   PARM                    @MSGF
     C                   PARM                    @MSGDTA
     C                   PARM                    @DTALEN
     C                   PARM                    @MSGTYP
     C                   PARM                    @PGMQ
     C                   PARM                    @STKCNT
     C                   PARM                    @MSGKEY
     C                   PARM                    @ERRCOD
     C                   Endsr
      * End standard program - Program errors
      ****************************************** 
