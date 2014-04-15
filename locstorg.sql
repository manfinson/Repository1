     /****************************************************         */
     /*                                                                       */
     /*  File Name...: LocStorg                                               */
     /*  Author/Date.: Mike Anfinson /Decenber 2013                           */
     /*  Project#....: Create a table/file called LocStorg.                   */
     /*  Text........: Create the Heartland Coop LocStorg file. Originally    */
     /*                it was created by a SEQUEL view apparently.            */
     /*                Also it evolved thru adding additional data from a     */
     /*                spreadsheet converted/massaged into the file.          */
     /*                But in the end had no way of updating/maintaining thru */
     /*                HC's standard methods. This should correct that.       */
     /*  Narrative...: Contains 3 simple fields besides audit area.           */
     /***************************************************************         */
CREATE TABLE LocStorg (
             LoNum     DECIMAL(3, 0)  NOT NULL,
             LoItem    DECIMAL(7, 0)  NOT NULL,
             LoStorage DECIMAL(11, 4) DEFAULT NULL,

/********    Audit Fields    ********************************/

             LoAddPgm  CHAR ( 10) CCSID 37 NOT NULL WITH DEFAULT ' ',
             LoChgPgm  CHAR ( 10) CCSID 37 NOT NULL WITH DEFAULT ' ',
             LoAddDate DATE                NOT NULL WITH DEFAULT CURRENT_DATE,
             LoAddTime TIME                NOT NULL WITH DEFAULT CURRENT_TIME,
             LoAddUser CHAR ( 10) CCSID 37 NOT NULL WITH DEFAULT ' ',
             LoChgDate DATE                NOT NULL WITH DEFAULT CURRENT_DATE,
             LoChgTime TIME                NOT NULL WITH DEFAULT CURRENT_TIME,
             LoChgUser CHAR ( 10) CCSID 37 NOT NULL WITH DEFAULT ' ')

             RCDFMT LocStorR;

LABEL ON COLUMN LocStorg.LoNum     IS 'Location Number';
LABEL ON COLUMN LocStorg.LoItem    IS 'Item Number';
LABEL ON COLUMN LocStorg.LoStorage IS 'Storage Capacity';
/*         ( LoNum     TEXT IS 'LOCATION NUMBER' ,         */
/*           LoItem    TEXT IS 'ITEM' ,                    */
/*           LoStorage TEXT IS 'STORAGE CAPACITY' );       */

/********    Audit Fields    ********************************/

LABEL ON COLUMN LocStorg.LoAddPgm  IS 'ADD Program';
LABEL ON COLUMN LocStorg.LoAddPgm  IS 'ADD Program';

LABEL ON COLUMN LocStorg.LoAddDate IS 'ADD Date';
LABEL ON COLUMN LocStorg.LoAddDate IS 'ADD Date';

LABEL ON COLUMN LocStorg.LoAddTime IS 'ADD Time';
LABEL ON COLUMN LocStorg.LoAddTime IS 'ADD Time';

LABEL ON COLUMN LocStorg.LoAddUser IS 'ADD User';
LABEL ON COLUMN LocStorg.LoAddUser IS 'ADD User';

LABEL ON COLUMN LocStorg.LoChgPgm  IS 'Change Program';
LABEL ON COLUMN LocStorg.LoChgPgm  IS 'Change Program';

LABEL ON COLUMN LocStorg.LoChgDate IS 'Change Date';
LABEL ON COLUMN LocStorg.LoChgDate IS 'Change Date';

LABEL ON COLUMN LocStorg.LoChgTime IS 'Change Time';
LABEL ON COLUMN LocStorg.LoChgTime IS 'Change Time';

LABEL ON COLUMN LocStorg.LoChgUser IS 'Change User';
LABEL ON COLUMN LocStorg.LoChgUser IS 'Change User';

ALTER TABLE LocStorg
ADD PRIMARY KEY (LoNum, LoItem); 
