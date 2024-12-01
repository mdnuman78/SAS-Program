/*======================================================================
 *
 * PROGRAM              : CDISC_ATTR.sas
 *
 * CURRENT VERSION      : 1
 *
 * PROGRAM LOCATION     :
 *
 * PROGRAMMER           : Mohammed Numan
 *
 * DESCRIPTION          : This macro program can read cdisc specification
 *                        and put the variables list and attributes in SQL
 *                        which could be useful at the end of program.
 *
 * USER REQUIREMENTS    : CDISC specification (SDTM or ADaM define)
 *
 * SAS VERSION
 *   in which validated : SASV9.4
 *
 * COMMENTS             :
 *
 *-----------------------------------------------------------------------
 *
 * REVISION HISTORY (COPY FOR EACH REVISION):
 *
 * DATE                 :
 * PROGRAMMER           :
 * REASON FOR CHANGE    :
 *
 *=======================================================================*/
OPTIONS MPRINT MLOGIC LS=150;

******************************************************************;
***                                                            ***;
*** MACRO C_ATTRIB:                                            ***;
*** This macro is useful at the end of SDTM and ADaM program   ***;
***   mostly when we need to finalize our main out dataset with***;
***   proper attributes from the specification. This macro is  ***;
***   will list the variables and their attributes in a SQL and***;
***   print these in log. So that one can just copy and paste  ***;
***   this from SAS log to SAS program editor. This macro will ***;
***   collect variables list and attributes from "Variable"    ***;
***   sheet and key variable information from "Dataset" sheet  ***;
***   from the xlsx specification file and arrange them in SQL ***;
***   for listing and print them in SAS log.                   ***;
***                                                            ***;
*** Macro Parametr:                                            ***;
*** DATASET : dataset name that for which variables are listed.***;
*** PATH    : Path or location of the specification.           ***;
*** XLSX_FILE: Specification xlsx file name with extension.    ***;
*** FROM : dataset name from which main output dataset need to ***;
***        create in the SQL.                                  ***;
*** DEL  : Default value is NO. If one set as YES then         ***;
***        temporary datasets will be deleted from workspace.  ***;
***                                                            ***;

******************************************************************;
%MACRO c_attrib(dataset=, path=, xlsx_file=, from=, del=);

<<<<<<< HEAD
	%******************************************************************;
	%*** Importing "Dataset" sheet from the xlsx specification file.***;
=======
  %******************************************************************;
  %*** Importing "Dataset" sheet from the xlsx specification file.***;
  %******************************************************************;
>>>>>>> efae65bddb32a9cef62615b77275a69bd4a37b70

	%******************************************************************;
	PROC IMPORT DATAFILE="&path.\&xlsx_file." OUT=spec_datasets DBMS=xlsx
		REPLACE ;
		SHEET='Datasets';
	RUN;

<<<<<<< HEAD
	%******************************************************************;
	%*** Key Variable: Getting dataset descriptions (LABEL) and key ***;
	%*** variables.                                                 ***;
	%***                                                            ***;
	%*** Checking: Checking if the main dataset information (Key    ***;
	%*** Variable) are are in the "Dataset" sheet from the xlsx     ***;
	%*** specification file.                                        ***;
	%******************************************************************;
	DATA _NULL_;
		LENGTH key $100;
		SET spec_datasets END=eof;
=======

  %******************************************************************;
  %*** Key Variable: Getting dataset descriptions (LABEL) and key ***;
  %*** variables.                                                 ***;
  %***                                                            ***;
  %*** Checking: Checking if the main dataset information (Key    ***;
  %*** Variable) are are in the "Dataset" sheet from the xlsx     ***;
  %*** specification file.                                        ***;
  %******************************************************************;

   DATA _NULL_;
	  LENGTH key $100;
    SET spec_datasets END = eof;
>>>>>>> efae65bddb32a9cef62615b77275a69bd4a37b70

		RETAIN flag 0;

		*Key Variable;
		IF UPCASE(STRIP(dataset))=UPCASE("&dataset.") THEN DO;

			flag=1;

			key=TRANWRD( COMPRESS(LOWCASE(key_variables)) , ",", ", " );
			CALL SYMPUTX("byvar", key);

			data_label=STRIP(description);
			CALL SYMPUTX("datalabel", data_label);

		END;

		*Checking;
		IF eof AND flag=0 THEN CALL SYMPUTX("end_flag", 0);
		ELSE CALL SYMPUTX("end_flag", 1);

	RUN;

<<<<<<< HEAD
	%******************************************************************;
	%*** Abort macro if no information found in the "Dataset" sheet.***;
	%******************************************************************;
	%IF &end_flag.=0 %THEN %DO;
		%PUT WARNING: Dataset %UPCASE(&dataset.) is not found in the "Dataset"
			sheet in XLSX specification file. ;
		%ABORT CANCEL;
=======

  %******************************************************************;
  %*** Abort macro if no information found in the "Dataset" sheet.***;
  %******************************************************************;

	%IF &end_flag. = 0 %THEN %DO;
	  %PUT WARNING: Dataset %UPCASE(&dataset.) is not found in the "Dataset" sheet in XLSX specification file. ;
    %ABORT CANCEL;
>>>>>>> efae65bddb32a9cef62615b77275a69bd4a37b70
	%END;

	%******************************************************************;
	%*** Importing "Variable" sheet from the xlsx specification     ***;
	%*** file.                                                      ***;

	%******************************************************************;
	PROC IMPORT DATAFILE="&path.\&xlsx_file." OUT=spec_var DBMS=xlsx REPLACE ;
		SHEET='Variables';
	RUN;

	%******************************************************************;
	%*** Checking if "Variable" sheet has the information of the    ***;
	%*** dataset.                                                   ***;

	%******************************************************************;
	DATA _NULL_;
		SET spec_var END=eof;

		RETAIN flag 0;

		IF UPCASE(STRIP(dataset))=UPCASE("&dataset.") THEN DO;

			flag=1;
			CALL SYMPUTX("var_end_fl", 1);
			STOP;

		END;

		IF eof AND flag=0 THEN CALL SYMPUTX("var_end_fl", 0);
	RUN;

	%IF &var_end_fl.=0 %THEN %DO;
		%PUT WARNING: Dataset %UPCASE(&dataset.) is not found in the "Variable"
			sheet in XLSX specification file.;
		%ABORT CANCEL;
	%END;

	%******************************************************************;
	%*** IF there might have chance that proc import reads some     ***;
	%*** some variables such as LENGTH, FORMAT, SIGNIFICANT_DIGITS  ***;
	%*** as numeric, then converting these variables to character   ***;
	%*** type.                                                      ***;
	%***                                                            ***;
	%*** Using macro variable to trac which variables are read as   ***;
	%*** as numeric.                                                ***;

	%******************************************************************;
	DATA _NULL_;
		SET spec_var;

		IF _N_=1 THEN DO;

			CALL SYMPUTX("change_type", 0);

			IF VTYPE(length)="N" THEN CALL SYMPUTX("length_num", 1);
			ELSE IF VTYPE(length)="C" THEN CALL SYMPUTX("length_num", 0);

			IF VTYPE(format)="N" THEN CALL SYMPUTX("format_num", 1);
			ELSE IF VTYPE(format)="C" THEN CALL SYMPUTX("format_num", 0);

			IF VTYPE(significant_digits)="N" THEN CALL SYMPUTX("decimal_num",
				1);
			ELSE IF VTYPE(significant_digits)="C" THEN CALL
				SYMPUTX("decimal_num", 0);

			STOP;

		END;
	RUN;

	%PUT &length_num.;
	%PUT &format_num.;
	%PUT &decimal_num.;

	%******************************************************************;
	%*** Converting the numeric variables according to the tracker  ***;
	%*** macro variable.                                            ***;
	%******************************************************************;
	%*Converting length variable to character type;
	%IF &length_num.=1 AND &format_num.=0 AND &decimal_num.=0 %THEN %DO;

		%LET change_type=1;

		DATA specvartype_chng ( RENAME=( new_len=length ) );
			LENGTH new_len $ 10;
			SET spec_var ( RENAME=(length=old_len) );

			IF old_len ^= . THEN new_len=STRIP(PUT(old_len, BEST.));
			ELSE new_len="";

		RUN;

	%END;
	%*Converting format variable to character type;
	%ELSE %IF &length_num.=0 AND &format_num.=1 AND &decimal_num.=0 %THEN %DO;

		%LET change_type=1;

		DATA specvartype_chng ( RENAME=( new_fmt=format ) );
			LENGTH new_fmt $ 50;
			SET spec_var ( RENAME=(format=old_fmt) );

			IF old_fmt ^= . THEN new_fmt=STRIP(PUT(old_fmt, BEST.));
			ELSE new_fmt="";

			IF STRIP(UPCASE(data_type)) IN("TEXT", "DATETIME", "CHAR") THEN DO;

				IF STRIP(new_fmt) ^= "" AND SUBSTR(STRIP(new_fmt), 1,1) ^= "$"
					THEN new_fmt="$"||STRIP(new_fmt);

			END;

		RUN;

	%END;
	%*Converting significant_digits variable to character type;
	%ELSE %IF &length_num.=0 AND &format_num.=0 AND &decimal_num.=1 %THEN %DO;

		%LET change_type=1;

		DATA specvartype_chng ( RENAME=( new_decimal=significant_digits ) );
			LENGTH new_decimal $ 10;
			SET spec_var ( RENAME=(significant_digits=old_decimal) );

			IF old_decimal ^= . THEN new_decimal=STRIP(PUT(old_decimal, BEST.));
			ELSE new_decimal="";

		RUN;

	%END;
	%*Converting length, format variable to character type;
	%ELSE %IF &length_num.=1 AND &format_num.=1 AND &decimal_num.=0 %THEN %DO;

		%LET change_type=1;

		DATA specvartype_chng ( RENAME=( new_len=length new_fmt=format ) );
			LENGTH new_len $ 10 new_fmt $ 50;
			SET spec_var ( RENAME=( length=old_len format=old_fmt ) );

			IF old_len ^= . THEN new_len=STRIP(PUT(old_len, BEST.));
			ELSE new_len="";

			IF old_fmt ^= . THEN new_fmt=STRIP(PUT(old_fmt, BEST.));
			ELSE new_fmt="";

			IF STRIP(UPCASE(data_type)) IN("TEXT", "DATETIME", "CHAR") THEN DO;

				IF STRIP(new_fmt) ^= "" AND SUBSTR(STRIP(new_fmt), 1,1) ^= "$"
					THEN new_fmt="$"||STRIP(new_fmt);

			END;

		RUN;

	%END;
	%*Converting format , significant digits variable to character type;
	%ELSE %IF &length_num.=0 AND &format_num.=1 AND &decimal_num.=1 %THEN %DO;

		%LET change_type=1;

		DATA specvartype_chng ( RENAME=( new_fmt=format new_decimal=
			significant_digits ) );
			LENGTH new_fmt $ 50 new_decimal $ 10;
			SET spec_var ( RENAME=( format=old_fmt significant_digits=
				old_decimal ) );

			IF old_fmt ^= . THEN new_fmt=STRIP(PUT(old_fmt, BEST.));
			ELSE new_fmt="";

			IF STRIP(UPCASE(data_type)) IN("TEXT", "DATETIME", "CHAR") THEN DO;

				IF STRIP(new_fmt) ^= "" AND SUBSTR(STRIP(new_fmt), 1,1) ^= "$"
					THEN new_fmt="$"||STRIP(new_fmt);

			END;

			IF old_decimal ^= . THEN new_decimal=STRIP(PUT(old_decimal, BEST.));
			ELSE new_decimal="";

		RUN;

	%END;
	%*Converting length , significant digits variable to character type;
	%ELSE %IF &length_num.=1 AND &format_num.=0 AND &decimal_num.=1 %THEN %DO;

		%LET change_type=1;

		DATA specvartype_chng ( RENAME=( new_len=length new_decimal=
			significant_digits ) );
			LENGTH new_len $ 10 new_decimal $ 10;
			SET spec_var ( RENAME=( length=old_len significant_digits=
				old_decimal ) );

			IF old_len ^= . THEN new_len=STRIP(PUT(old_len, BEST.));
			ELSE new_len="";

			IF old_decimal ^= . THEN new_decimal=STRIP(PUT(old_decimal, BEST.));
			ELSE new_decimal="";

		RUN;

	%END;
	%*Converting length ,format and significant digits variable to character type;
	%ELSE %IF &length_num.=1 AND &format_num.=1 AND &decimal_num.=1 %THEN %DO;

		%LET change_type=1;

		DATA specvartype_chng ( RENAME=( new_len=length new_fmt=format
			new_decimal=significant_digits ) );
			LENGTH new_len $ 10 new_fmt $ 50 new_decimal $ 10;
			SET spec_var ( RENAME=( length=old_len format=old_fmt
				significant_digits=old_decimal ) );

			IF old_len ^= . THEN new_len=STRIP(PUT(old_len, BEST.));
			ELSE new_len="";

			IF old_fmt ^= . THEN new_fmt=STRIP(PUT(old_fmt, BEST.));
			ELSE new_fmt="";

			IF STRIP(UPCASE(data_type)) IN("TEXT", "DATETIME", "CHAR") THEN DO;

				IF STRIP(new_fmt) ^= "" AND SUBSTR(STRIP(new_fmt), 1,1) ^= "$"
					THEN new_fmt="$"||STRIP(new_fmt);

			END;

			IF old_decimal ^= . THEN new_decimal=STRIP(PUT(old_decimal, BEST.));
			ELSE new_decimal="";

		RUN;

	%END;

	%******************************************************************;
	%***                                                            ***;
	%*** Formatting essential data. Since ISO8601 format for        ***;
	%*** datetime --DTC variable are character, taking it as blank. ***;
	%*** If there is not dot after some standard SAS built in format***;
	%*** e.g. (TIME8), then putting dot at the end. Also counting   ***;
	%*** length of the essential variable so that at the end maximum***;
	%*** length can be used for space alignment at the end of       ***;
	%*** result.                                                    ***;
	%***                                                            ***;
	%*** Checking: IF there is no information in "Variable" sheet,  ***;
	%*** then putting these information in SAS log and abort.       ***;
	%***                                                            ***;

	%******************************************************************;
	DATA formatting;
		LENGTH len $ 10 fmt $ 50 significant_digits $10 label $ 100;

		%IF &change_type.=1 %THEN %DO;

			SET specvartype_chng ( KEEP=order dataset variable data_type label
				length format significant_digits RENAME=(format=fmt length=len )
				WHERE=( UPCASE(STRIP(dataset))=UPCASE("&dataset.")) ) ;

		%END;
		%ELSE %IF &change_type.=0 %THEN %DO;

			SET spec_var( KEEP=order dataset variable data_type label length
				format significant_digits RENAME=(format=fmt length=len ) WHERE=
				( UPCASE(STRIP(dataset))=UPCASE("&dataset.")) ) ;
		%END;

		variable=STRIP(LOWCASE(variable));
		IF STRIP(label) ^= "" THEN label=QUOTE(STRIP(label));
		len=STRIP(len);
		fmt=STRIP(fmt);

		IF UPCASE(COMPRESS(fmt))="ISO8601" THEN fmt="";

		IF UPCASE(STRIP(data_type))="FLOAT" AND STRIP(fmt)="" AND STRIP(len) ^=
			"" AND STRIP(significant_digits) ^= "" THEN fmt=STRIP(len)|| "."||
			STRIP(significant_digits);

		IF STRIP(fmt) ^= "" THEN DO;

			IF FIND(STRIP(fmt), ".")=0 THEN DO;
				IF SUBSTR(STRIP(fmt), LENGTHN(STRIP(fmt)) ) ^= "." THEN fmt=
					STRIP(fmt)||".";
			END;

			IF STRIP(UPCASE(data_type)) IN("TEXT", "DATETIME", "CHAR") THEN DO;

				IF SUBSTR(STRIP(fmt), 1,1) ^= "$" THEN fmt="$"||STRIP(fmt);

			END;

		END;

		*Measuring length;
		varlen=LENGTHN(STRIP(variable));
		lenlen=LENGTHN(STRIP(len));
		fmtlen=LENGTHN(STRIP(fmt));
		lablen=LENGTHN(STRIP(label));

		FORMAT _ALL_;
		INFORMAT _ALL_;
	RUN;

	*Maximum length;
	PROC SQL NOPRINT;
		SELECT MAX(varlen) AS mvarlen , MAX(lenlen) AS mlenlen , MAX(fmtlen) AS
			mfmtlen , MAX(lablen) AS mlablen INTO : mvlen , : mlen , : mflen , :
			mlblen FROM formatting;
	QUIT;

	%PUT &mvlen.;
	%PUT &mlen.;
	%PUT &mflen.;
	%PUT &mlblen.;

	%******************************************************************;
	%*** Final output in the SAS log.                               ***;

	%******************************************************************;
	DATA _NULL_;
		SET formatting END=eof;

		IF _N_=1 THEN PUT "PROC SQL;";

		IF _N_=1 THEN PUT @3 "CREATE TABLE &dataset. " %IF %LENGTH(&datalabel.)
			%THEN "(LABEL = ""&datalabel."") " ;
		"AS" ;

		%******************************************************************;
		%*** IF length, format and label all are missing for all        ***;
		%*** variables.                                                 ***;
		%******************************************************************;
		%IF &mlen.=0 AND &mflen.=0 AND &mlblen.=0 %THEN %DO;

			IF _N_=1 THEN PUT @3 "SELECT" @12 variable;
			ELSE PUT @9 "," @12 variable;

		%END;

		%******************************************************************;
		%*** IF lengths, formats are missing and labels are not missing.***;
		%******************************************************************;
		%ELSE %IF &mlen.=0 AND &mflen.=0 AND &mlblen. ^= 0 %THEN %DO;

			IF _N_=1 THEN DO;

				IF STRIP(label)="" THEN DO;
					PUT @3 "SELECT" @12 variable ;
				END;
				ELSE PUT @3 "SELECT" @12 variable @%EVAL(14+&mvlen.) "LABEL = "
					@%EVAL(22+&mvlen.) label;

			END;
			ELSE DO;

				IF STRIP(label)="" THEN DO;
					PUT @9 "," @12 variable ;
				END;
				ELSE PUT @9 "," @12 variable @%EVAL(14+&mvlen.) "LABEL = "
					@%EVAL(22+&mvlen.) label;

			END;

		%END;

		%******************************************************************;
		%*** IF lengths, labels are missing and formats are not missing.***;
		%******************************************************************;
		%ELSE %IF &mlen.=0 AND &mflen. ^= 0 AND &mlblen.=0 %THEN %DO;

			IF _N_=1 THEN DO;

				IF STRIP(fmt)="" THEN DO;
					PUT @3 "SELECT" @12 variable ;
				END;
				ELSE PUT @3 "SELECT" @12 variable @%EVAL(14+&mvlen.) "FORMAT = "
					@%EVAL(23+&mvlen.) fmt;

			END;
			ELSE DO;

				IF STRIP(fmt)="" THEN DO;
					PUT @9 "," @12 variable;
				END;
				ELSE PUT @9 "," @12 variable @%EVAL(14+&mvlen.) "FORMAT = "
					@%EVAL(23+&mvlen.) fmt;

			END;

		%END;

		%******************************************************************;
		%*** IF formats, labels are missing and lengths are not missing.***;
		%******************************************************************;
		%ELSE %IF &mlen. ^= 0 AND &mflen.=0 AND &mlblen.=0 %THEN %DO;

			IF _N_=1 THEN DO;

				IF STRIP(len)="" THEN DO;
					PUT @3 "SELECT" @12 variable ;
				END;
				ELSE PUT @3 "SELECT" @12 variable @%EVAL(14+&mvlen.) "LENGTH = "
					@%EVAL(23+&mvlen.) len;

			END;
			ELSE DO;

				IF STRIP(len)="" THEN DO;
					PUT @9 "," @12 variable ;
				END;
				ELSE PUT @9 "," @12 variable @%EVAL(14+&mvlen.) "LENGTH = "
					@%EVAL(23+&mvlen.) len;

			END;

		%END;

		%******************************************************************;
		%*** IF lengths, formats are not missing and labels are missing.***;
		%******************************************************************;
		%ELSE %IF &mlen. ^= 0 AND &mflen. ^= 0 AND &mlblen.=0 %THEN %DO;

			IF _N_=1 THEN DO;

				IF STRIP(len)="" AND STRIP(fmt)="" THEN DO;
					PUT @3 "SELECT" @12 variable;
				END;
				ELSE IF STRIP(len) ^= "" AND STRIP(fmt)="" THEN DO;
					PUT @3 "SELECT" @12 variable @%EVAL(14+&mvlen.) "LENGTH = "
						@%EVAL(23+&mvlen.) len ;

				END;
				ELSE IF STRIP(len)="" AND STRIP(fmt) ^= "" THEN DO;
					PUT @3 "SELECT" @12 variable @%EVAL(26+&mvlen.+&mlen.)
						"FROMAT = " @%EVAL(35+&mvlen.+&mlen.) fmt;

				END;
				ELSE PUT @3 "SELECT" @12 variable @%EVAL(14+&mvlen.) "LENGTH = "
					@%EVAL(23+&mvlen.) len @%EVAL(26+&mvlen.+&mlen.) "FROMAT = "
					@%EVAL(35+&mvlen.+&mlen.) fmt;

			END;
			ELSE DO;

				IF STRIP(len)="" AND STRIP(fmt)="" THEN DO;
					PUT @9 "," @12 variable;
				END;
				ELSE IF STRIP(len) ^= "" AND STRIP(fmt)="" THEN DO;
					PUT @9 "," @12 variable @%EVAL(14+&mvlen.) "LENGTH = "
						@%EVAL(23+&mvlen.) len ;

				END;
				ELSE IF STRIP(len)="" AND STRIP(fmt) ^= "" THEN DO;
					PUT @9 "," @12 variable @%EVAL(26+&mvlen.+&mlen.)
						"FROMAT = " @%EVAL(35+&mvlen.+&mlen.) fmt;

				END;
				ELSE PUT @9 "," @12 variable @%EVAL(14+&mvlen.) "LENGTH = "
					@%EVAL(23+&mvlen.) len @%EVAL(26+&mvlen.+&mlen.) "FROMAT = "
					@%EVAL(35+&mvlen.+&mlen.) fmt;

			END;

		%END;

		%******************************************************************;
		%*** IF formats, labels are not missing and lengths are missing.***;
		%******************************************************************;
		%ELSE %IF &mlen.=0 AND &mflen. ^= 0 AND &mlblen. ^= 0 %THEN %DO;

			IF _N_=1 THEN DO;

				IF STRIP(fmt)="" AND STRIP(label)="" THEN DO;
					PUT @3 "SELECT" @12 variable;
				END;
				ELSE IF STRIP(fmt) ^= "" AND STRIP(label)="" THEN DO;
					PUT @3 "SELECT" @12 variable @%EVAL(14+&mvlen.) "FORMAT = "
						@%EVAL(23+&mvlen.) fmt ;

				END;
				ELSE IF STRIP(fmt)="" AND STRIP(label) ^= "" THEN DO;
					PUT @3 "SELECT" @12 variable @%EVAL(25+&mvlen.+&mflen.)
						"LABEL = " @%EVAL(33+&mvlen.+&mflen.) label;

				END;
				ELSE PUT @3 "SELECT" @12 variable @%EVAL(14+&mvlen.) "FORMAT = "
					@%EVAL(23+&mvlen.) fmt @%EVAL(25+&mvlen.+&mflen.) "LABEL = "
					@%EVAL(33+&mvlen.+&mflen.) label;

			END;
			ELSE DO;

				IF STRIP(fmt)="" AND STRIP(label)="" THEN DO;
					PUT @9 "," @12 variable;
				END;
				ELSE IF STRIP(fmt) ^= "" AND STRIP(label)="" THEN DO;
					PUT @9 "," @12 variable @%EVAL(14+&mvlen.) "FORMAT = "
						@%EVAL(23+&mvlen.) fmt ;

				END;
				ELSE IF STRIP(fmt)="" AND STRIP(label) ^= "" THEN DO;
					PUT @9 "," @12 variable @%EVAL(25+&mvlen.+&mflen.)
						"LABEL = " @%EVAL(33+&mvlen.+&mflen.) label;

				END;
				ELSE PUT @9 "," @12 variable @%EVAL(14+&mvlen.) "FORMAT = "
					@%EVAL(23+&mvlen.) fmt @%EVAL(25+&mvlen.+&mflen.) "LABEL = "
					@%EVAL(33+&mvlen.+&mflen.) label;

			END;

		%END;

		%******************************************************************;
		%*** IF lengths, labels are not missing and formats are missing.***;
		%******************************************************************;
		%ELSE %IF &mlen. ^= 0 AND &mflen.=0 AND &mlblen. ^= 0 %THEN %DO;

			IF _N_=1 THEN DO;

				IF STRIP(len)="" AND STRIP(label)="" THEN DO;
					PUT @3 "SELECT" @12 variable;
				END;
				ELSE IF STRIP(len) ^= "" AND STRIP(label)="" THEN DO;
					PUT @3 "SELECT" @12 variable @%EVAL(14+&mvlen.) "LENGTH = "
						@%EVAL(23+&mvlen.) len ;

				END;
				ELSE IF STRIP(len)="" AND STRIP(label) ^= "" THEN DO;
					PUT @3 "SELECT" @12 variable @%EVAL(25+&mvlen.+&mlen.)
						"LABEL = " @%EVAL(33+&mvlen.+&mlen.) label;

				END;
				ELSE PUT @3 "SELECT" @12 variable @%EVAL(14+&mvlen.) "LENGTH = "
					@%EVAL(23+&mvlen.) len @%EVAL(25+&mvlen.+&mlen.) "LABEL = "
					@%EVAL(33+&mvlen.+&mlen.) label;

			END;
			ELSE DO;

				IF STRIP(len)="" AND STRIP(label)="" THEN DO;
					PUT @9 "," @12 variable;
				END;
				ELSE IF STRIP(len) ^= "" AND STRIP(label)="" THEN DO;
					PUT @9 "," @12 variable;
					@%EVAL(14+&mvlen.) "LENGTH = " @%EVAL(23+&mvlen.) len ;

				END;
				ELSE IF STRIP(len)="" AND STRIP(label) ^= "" THEN DO;
					PUT @9 "," @12 variable;
					@%EVAL(25+&mvlen.+&mlen.) "LABEL = "
						@%EVAL(33+&mvlen.+&mlen.) label;

				END;
				ELSE PUT @9 "," @12 variable;
				@%EVAL(14+&mvlen.) "LENGTH = " @%EVAL(23+&mvlen.) len
					@%EVAL(25+&mvlen.+&mlen.) "LABEL = "
					@%EVAL(33+&mvlen.+&mlen.) label;

			END;

		%END;

		%******************************************************************;
		%*** IF lengths, formats and labels are not missing.            ***;
		%******************************************************************;
		%ELSE %IF &mlen. ^= 0 AND &mflen. ^= 0 AND &mlblen. ^= 0 %THEN %DO;

			IF _N_=1 THEN DO;

				IF STRIP(len)="" AND STRIP(fmt)="" AND STRIP(label)="" THEN DO;
					PUT @3 "SELECT" @12 variable;
				END;
				ELSE IF STRIP(len) ^= "" AND STRIP(fmt)="" AND STRIP(label)=""
					THEN DO;
					PUT @3 "SELECT" @12 variable @%EVAL(14+&mvlen.) "LENGTH = "
						@%EVAL(23+&mvlen.) len ;

				END;
				ELSE IF STRIP(len)="" AND STRIP(fmt) ^= "" AND STRIP(label)=""
					THEN DO;
					PUT @3 "SELECT" @12 variable @%EVAL(26+&mvlen.+&mlen.)
						"FORMAT = " @%EVAL(35+&mvlen.+&mlen.) fmt;

				END;
				ELSE IF STRIP(len)="" AND STRIP(fmt)="" AND STRIP(label) ^= ""
					THEN DO;
					PUT @3 "SELECT" @12 variable
						@%EVAL(36+&mvlen.+&mlen.+&mflen.) "LABEL = "
						@%EVAL(44+&mvlen.+&mlen.+&mflen.) label;

				END;
				ELSE IF STRIP(len) ^= "" AND STRIP(fmt) ^= "" AND STRIP(label)=
					"" THEN DO;
					PUT @3 "SELECT" @12 variable @%EVAL(14+&mvlen.) "LENGTH = "
						@%EVAL(23+&mvlen.) len @%EVAL(26+&mvlen.+&mlen.)
						"FORMAT = " @%EVAL(35+&mvlen.+&mlen.) fmt;

				END;
				ELSE IF STRIP(len)="" AND STRIP(fmt) ^= "" AND STRIP(label) ^=
					"" THEN DO;
					PUT @3 "SELECT" @12 variable @%EVAL(26+&mvlen.+&mlen.)
						"FORMAT = " @%EVAL(35+&mvlen.+&mlen.) fmt
						@%EVAL(36+&mvlen.+&mlen.+&mflen.) "LABEL = "
						@%EVAL(44+&mvlen.+&mlen.+&mflen.) label;

				END;
				ELSE IF STRIP(len) ^= "" AND STRIP(fmt)="" AND STRIP(label) ^=
					"" THEN DO;
					PUT @3 "SELECT" @12 variable @%EVAL(14+&mvlen.) "LENGTH = "
						@%EVAL(23+&mvlen.) len @%EVAL(36+&mvlen.+&mlen.+&mflen.)
						"LABEL = " @%EVAL(44+&mvlen.+&mlen.+&mflen.) label;

				END;
				ELSE IF STRIP(len) ^= "" AND STRIP(fmt) ^= "" AND STRIP(label)
					^= "" THEN DO;
					PUT @3 "SELECT" @12 variable @%EVAL(14+&mvlen.) "LENGTH = "
						@%EVAL(23+&mvlen.) len @%EVAL(26+&mvlen.+&mlen.)
						"FORMAT = " @%EVAL(35+&mvlen.+&mlen.) fmt
						@%EVAL(36+&mvlen.+&mlen.+&mflen.) "LABEL = "
						@%EVAL(44+&mvlen.+&mlen.+&mflen.) label;

				END;

			END;
			ELSE DO;

				IF STRIP(len)="" AND STRIP(fmt)="" AND STRIP(label)="" THEN DO;
					PUT @9 "," @12 variable;
				END;
				ELSE IF STRIP(len) ^= "" AND STRIP(fmt)="" AND STRIP(label)=""
					THEN DO;
					PUT @9 "," @12 variable @%EVAL(14+&mvlen.) "LENGTH = "
						@%EVAL(23+&mvlen.) len ;

				END;
				ELSE IF STRIP(len)="" AND STRIP(fmt) ^= "" AND STRIP(label)=""
					THEN DO;
					PUT @9 "," @12 variable @%EVAL(26+&mvlen.+&mlen.)
						"FORMAT = " @%EVAL(35+&mvlen.+&mlen.) fmt;

				END;
				ELSE IF STRIP(len)="" AND STRIP(fmt)="" AND STRIP(label) ^= ""
					THEN DO;
					PUT @9 "," @12 variable @%EVAL(36+&mvlen.+&mlen.+&mflen.)
						"LABEL = " @%EVAL(44+&mvlen.+&mlen.+&mflen.) label;

				END;
				ELSE IF STRIP(len) ^= "" AND STRIP(fmt) ^= "" AND STRIP(label)=
					"" THEN DO;
					PUT @9 "," @12 variable @%EVAL(14+&mvlen.) "LENGTH = "
						@%EVAL(23+&mvlen.) len @%EVAL(26+&mvlen.+&mlen.)
						"FORMAT = " @%EVAL(35+&mvlen.+&mlen.) fmt;

				END;
				ELSE IF STRIP(len)="" AND STRIP(fmt) ^= "" AND STRIP(label) ^=
					"" THEN DO;
					PUT @9 "," @12 variable @%EVAL(26+&mvlen.+&mlen.)
						"FORMAT = " @%EVAL(35+&mvlen.+&mlen.) fmt
						@%EVAL(36+&mvlen.+&mlen.+&mflen.) "LABEL = "
						@%EVAL(44+&mvlen.+&mlen.+&mflen.) label;

				END;
				ELSE IF STRIP(len) ^= "" AND STRIP(fmt)="" AND STRIP(label) ^=
					"" THEN DO;
					PUT @9 "," @12 variable @%EVAL(14+&mvlen.) "LENGTH = "
						@%EVAL(23+&mvlen.) len @%EVAL(36+&mvlen.+&mlen.+&mflen.)
						"LABEL = " @%EVAL(44+&mvlen.+&mlen.+&mflen.) label;

				END;
				ELSE IF STRIP(len) ^= "" AND STRIP(fmt) ^= "" AND STRIP(label)
					^= "" THEN DO;
					PUT @9 "," @12 variable @%EVAL(14+&mvlen.) "LENGTH = "
						@%EVAL(23+&mvlen.) len @%EVAL(26+&mvlen.+&mlen.)
						"FORMAT = " @%EVAL(35+&mvlen.+&mlen.) fmt
						@%EVAL(36+&mvlen.+&mlen.+&mflen.) "LABEL = "
						@%EVAL(44+&mvlen.+&mlen.+&mflen.) label;
				END;
			END;

		%END;

		IF eof THEN PUT @3 "FROM &from.";
		IF eof THEN PUT @3 "ORDER BY &byvar.;";
		IF eof THEN PUT "QUIT;";
	RUN;

	%******************************************************************;
	%*** If macro parameter DEL is set as YES, then the following   ***;
	%*** program will delete the temporary datasets used in the     ***;
	%*** macro                                                      ***;
	%******************************************************************;
	%*Deleting the temporary dataset;
	%IF %UPCASE(&del.)=YES %THEN %DO;

		PROC DATASETS LIB=WORK MEMTYPE=DATA NOWARN NOLIST ;
			DELETE formatting Specvartype_chng Spec_datasets Spec_var;
		QUIT;

	%END;

%MEND c_attrib;

*TESTING*;
%c_attrib(dataset=adsl , path=
	\\server\Projects\Agilis-SA\21LAQ01\ADaMF\specification, xlsx_file=ADAM
	define-21LAQ01_v2.xlsx , from=pre_adae);

/*%c_attrib(dataset    = ae , */
/*          path       = C:\Users\numan\Downloads\Macro, */
/*          xlsx_file  = SDTM define - Copy.xlsx  ,*/
/*          from       = pre_ae,*/
/*          del        = yes );*/
*Deleting this macro;
/*PROC CATALOG CAT=sasmacr;*/
/*  DELETE c_attrib/ET=macro;*/
/*QUIT;*/
