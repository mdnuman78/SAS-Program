  ******************************************************************;
  ***                                                            ***;
  *** Macro DTC2DTM:                                             ***;
  *** This macro is able to convert any standard chracter        ***;
  ***  datetime variable _DTC to numeric variable _DTM defined   ***;
  ***  by users. It is also able to give datepart and timepart   ***;
  ***  from _DTC variable to user defined numeric variables.     ***;
  ***  This macro will also need to be used inside a datastep.   ***;
  ***                                                            ***;
  *** Macro Parameters:                                          ***;
  *** DTC    = Character _DTC variable name need to sent by user.***;
  *** DA     = Output numeric date variable name.                ***;
  *** DAFMT  = Format name for the variable defined in the macro ***; 
  ***          parameter DA.                                     ***;
  *** TM     = Output numeric time variable name.                ***;
  *** TMFMT  = Format name for the variable defined in the macro ***;
  ***          parameter TM.                                     ***;
  *** DTM    = Output numeric datetime variable name.            ***;
  *** DTMFMT = Format name for the variable defined in the macro ***;
  ***          parameter DTM.                                    ***;
  ***                                                            ***;
  ******************************************************************;

%MACRO dtc2dtm( dtc = , da = , dafmt =, tm = , tmfmt = , dtm = , dtmfmt = );

	IF STRIP(&dtc.) ^= "" THEN DO;
   
	 %*DA derivation;
    %IF %LENGTH(&da.) %THEN %DO;
      &da. = INPUT( SUBSTR(STRIP(&dtc.), 1, 10) , %IF %LENGTH(&dafmt.) %THEN %UPCASE(&dafmt.); %ELSE IS8601DA. ; );
		%END;
    
		%*TM derivation;
		%IF %LENGTH(&tm.) %THEN %DO;
		  IF LENGTH(STRIP(&dtc.)) > 10 THEN DO; 
        &tm. = INPUT( SUBSTR(STRIP(&dtc.), 12) , %IF %LENGTH(&tmfmt.) %THEN %UPCASE(&tmfmt.); %ELSE TIME8. ; );
		  END;
		%END;

    %*DTM derivation;
		%IF %LENGTH(&dtm.) ^= 0 %THEN %DO;
		  IF LENGTH(STRIP(&dtc.)) > 10 THEN DO; 
        &dtm. = INPUT(STRIP(&dtc.), %IF %LENGTH(&dtmfmt.) %THEN %UPCASE(&dtmfmt.); %ELSE IS8601DT. ; );
			END;
    %END;

	END;

	FORMAT %IF %LENGTH(&da.)  AND %LENGTH(&dafmt.)  %THEN &da.  %UPCASE(&dafmt.) ;  %ELSE %IF %LENGTH(&da.)  AND %LENGTH(&dafmt.)  = 0 %THEN &da.  IS8601DA.;
	       %IF %LENGTH(&tm.)  AND %LENGTH(&tmfmt.)  %THEN &tm.  %UPCASE(&tmfmt.) ;  %ELSE %IF %LENGTH(&tm.)  AND %LENGTH(&tmfmt.)  = 0 %THEN &tm.  TIME8.   ;
				 %IF %LENGTH(&dtm.) AND %LENGTH(&dtmfmt.) %THEN &dtm. %UPCASE(&dtmfmt.);  %ELSE %IF %LENGTH(&dtm.) AND %LENGTH(&dtmfmt.) = 0 %THEN &dtm. IS8601DT.;  ;

%MEND dtc2dtm;


  ******************************************************************;
  *** MACRO SELECT_RAND_SUBJ:                                    ***;
  *** The macro will create to find random subjects for QC to    ***;
  *** check and validation for random distinct subject.          ***;
  ***                                                            ***;
  *** Parameters:                                                ***;
  *** DIN    = incoming dataset name,                            ***;
  *** VNAME  = variable name from where random subject will be   ***;
  ***          selected.                                         ***;
  *** SELECT = number to select how many number of random subject***;
  ***          to choose.                                        ***;
  ***                                                            ***;
  *** "RANDSUB" this is the final output of the macro, this      ***;
  *** global macro variable will contain the random subjects     ***;
  *** separated by space .                                       ***;
  ******************************************************************;

*Selecting subjects randomly*;
%MACRO select_rand_subj(din=, vname= , select=) / MINOPERATOR MINDELIMITER = '#';

  %GLOBAL randsub;

	%******************************************************************;
  %*** Cheking if the dataset DIN exist or not.                   ***;
  %******************************************************************;

	%IF NOT %SYSFUNC(EXIST(&din.)) %THEN %DO;
	  %PUT %SYSFUNC(CATX(%STR(), WAR, %STR(NING:)))%STR( Dataset )%UPCASE(&din.) does not exist.;
	  %PUT %SYSFUNC(CATX(%STR(), ERR, %STR(OR:)))%STR( SELECT_RAND_SUBJ )macro has stopped working.;
		%GOTO EXIT;
  %END; 

	%******************************************************************;
  %*** A) Checking if the variable is character type or numeric.  ***;
	%*** B) Taking all distinct values for VNAMe in a macro variable***;
  %******************************************************************;

	%*A);
	DATA _null_;
		SET &din.;
		IF _N_ = 1 THEN DO;
      typ = UPCASE(VTYPE(&vname.));
		  CALL SYMPUTX("vname_typ", typ);
		END;
	RUN;

	%*B);
	PROC SQL NOPRINT;
		SELECT DISTINCT 
		%IF &vname_typ. = C %THEN %DO;
			QUOTE(COMPRESS(&vname.))
		%END;
		%ELSE %DO;
			&vname.
		%END;
		INTO: _ptno SEPARATED BY ' '
		FROM &din.;
	QUIT;


	%******************************************************************;
  %*** Getting Total distinct subjects and checking if the .      ***;
  %***   total selected subject number exceeds the total distinct ***; 
	%***   subjects.                                                ***;
  %******************************************************************;

	%*Total distinct subject;
	%LET count = %SYSFUNC(COUNTW(&_ptno., " "));

	%IF &count. < &select. %THEN %DO;
	  %PUT %SYSFUNC(CATX(%STR(), WAR, %STR(NING:)))%STR( Your )total selected subject number exceeds the total distinct subjects in %UPCASE(&vname.).;
	  %PUT %SYSFUNC(CATX(%STR(), ERR, %STR(OR:)))%STR( SELECT_RAND_SUBJ )macro has stopped working.;
		%GOTO EXIT;
  %END; 


  %******************************************************************;
  %*** Getting the random subjects and checking if they are       ***;
  %***   repeated or not. At first taking one random subject      ***;
	%***   and the addinf other subjects according after checking   ***;
	%***   if the random index is already selected or not.          ***;
  %******************************************************************;

  %IF &select. > 1 %THEN %DO;

		%LET randsub = %STR( );
		%LET i = 1;

		%*First taking one subject;
		%IF &i. = 1 %THEN %DO;
      %LET random = %SYSFUNC(RAND(INTEGER, 1, &count.));
			%LET randsub = %SCAN(&_ptno., &random., %STR( )) &randsub.;
		  %LET rand_index = &random.;
			%LET i = &i. +1;
		%END;

    %*Taking other subjects after random index checking;
		%DO i = 2 %TO &select.;

		  %LET k = 0;
		  %DO %WHILE(&k. ^= 1); 

        %LET random = %SYSFUNC(RAND(INTEGER, 1, &count.));

				%IF &random. IN &rand_index. %THEN %DO;
          %GOTO CONTINUE;
				%END;
				%ELSE %DO;
				  %LET rand_index = &rand_index.#&random.;
					%LET randsub    = %SCAN(&_ptno., &random., %STR( )) &randsub.;
					%LET k = 1;
				%END;
       
				%CONTINUE:
			%END;

		%END;

  %END;
	%ELSE %IF &select. = 1 %THEN %DO;
		%LET random = %SYSFUNC(RAND(INTEGER, 1, &count.));
		%LET randsub = %SCAN(&_ptno., &random., %STR( ));
	%END;

	%PUT &randsub.;

	%EXIT:
%MEND select_rand_subj;


/*%select_rand_subj(din = n , vname = x, select = 2 );*/


  ******************************************************************;
  ***                                                            ***;
  *** %DUP_CHECK Macro:                                          ***;
  ***   This macro will check if there is any duplicate records  ***;
  ***   /observations with the same key variables. There will be ***;
  ***   a table which shows the result with DUPLICATE flag       ***;
  ***   variable along with duplicate frequency if there is any. ***;
  ***                                                            ***;
  *** %DUP_CHECK macro parameter:                                ***;
  ***   INDATA = Input dataset name for which the duplicacy  will***;
  ***            be checked.                                     ***;
  ***   BYVAR  = List of key variables seperated by space.       ***;
  ***            Correct sequenc of the variables is must. Based ***;
  ***            on the last variable name in the list, the      ***;
  ***            duplicacy will be checked.                      ***;
  ***   DUP_ONLY = Indicates if only the duplicates information  ***;
  ***              with frequency will show or not. Value is     ***;
  ***              either "y" or "Y", "yes" or "YES" if one wants***;
  ***              to show whole result except duplicate. This   ***;
  ***              is a optional parameter.                      ***;
  ***                                                            ***;
  ******************************************************************;

%MACRO dup_check( indata = , byvar = , dup_only = );

  %*Checking if INDATA is missing;
  %IF %LENGTH(&indata.) EQ 0 %THEN %DO;

		DATA _NULL_;
		  PUT "WAR" "NING: (DUP_CHECK macro)==> Please insert a valid dataset name in the parameter INDATA.";
		  PUT "ERR" "OR: DUP_CHECK macro has stopped working.";
		RUN;
    %GOTO EXIT;

  %END;

	%*Setting DUP_ONLY flag if it is missing;
  %IF %LENGTH(&dup_only.) EQ 0 %THEN %DO;
    %LET dup_only = N;
  %END;

  %*Taking last key variable.;
  %LET last_keyvar = %SCAN(&byvar. , -1);


	%******************************************************************;
  %*** Modifying the by variable list with a star(*) inside for   ***;
  %***  the using this in creating frequency table by PROC FREQ.  ***;
  %******************************************************************;

  %LET mod_var  = %SYSFUNC( TRANWRD( &byvar. , %NRSTR( ), * ) ) ;

	%*Sorting with by variables.;
  PROC SORT DATA = &indata. OUT = dupchek_data_srt;
    BY &byvar.;
  RUN; 

	%*Duplicacy checking with DUPLICATE variable.;
  DATA dupchek_data_1;
    SET dupchek_data_srt END = eof;
    BY &byvar.;

		LENGTH duplicate $3;
    RETAIN dup_flag 0;

	  IF ( FIRST.&last_keyvar. ^= LAST.&last_keyvar. ) OR ( FIRST.&last_keyvar. = LAST.&last_keyvar. = 0 ) THEN DO;
      duplicate = "YES";
      dup_flag  = 1;
		END;

		IF eof AND dup_flag  = 0 THEN DO;
		  CALL SYMPUTX ("dup_flag" , dup_flag);
		  CALL SYMPUTX ("dup_comment" , "==> No duplicate records found for the key variables in dataset %UPCASE(&indata.)");
    END;
    ELSE IF eof AND dup_flag  = 1 THEN DO;
		  CALL SYMPUTX ("dup_flag" , dup_flag);
		  CALL SYMPUTX ("dup_comment" , "==> Duplicate records found for the key variables in dataset %UPCASE(&indata.). See the result on output.");
    END; 
  RUN;

	%*Frequency table for duplicate.;
	%IF &dup_flag. = 1 %THEN %DO;
	  PROC FREQ DATA = dupchek_data_1;
	    TABLE &mod_var.*duplicate / LIST MISSING;
		  %IF %UPCASE(&dup_only.) = YES OR %UPCASE(&dup_only.) = Y %THEN WHERE duplicate = "YES";;
	  RUN;
	%END;

	%*Deleting intermediate datasets.;
  PROC DATASETS LIB = WORK MEMTYPE = DATA NOLIST;
    DELETE dupchek_data_srt 
           dupchek_data_1;
  QUIT;
  RUN;

  %*Duplicate checking result as comment in LOG;
  %PUT &dup_comment.;

	%EXIT:

%MEND dup_check;

%dup_check( indata = nn  , byvar = x y z , dup_only = y );



  ******************************************************************;
  ***                                                            ***;
  *** %CHANGE_CHECK Macro:                                       ***;
  ***   This macro will check if there are different values in a ***;
  ***   variable for a(single) value of another variable or some ***;
  ***   combinations of variables which should not be placed.    ***;
  ***                                                            ***;
  ***   For example, suppose in a case a single date should have ***;
  ***   a single visit for a clinical trail data and there won't ***;
  ***   another visit in the same date. This macro will check    ***;
  ***   these and show the result in a table.                    ***;
  ***                                                            ***;
  ***   If there is no changes found in the values of a variable ***;
  ***   for the other values/combinations of values of variables ***;
  ***   then there will output window will show only one         ***;
  ***   records showing message that there is no chnages.        ***;
  ***                                                            ***;
  *** %Macro parameters:                                         ***;
  ***   INDATA = Input dataset name for which the changes will   ***;
  ***            be checked if there is any.                     ***;
  ***   BYVAR  = List of key variables seperated by space.       ***;
  ***            Correct sequenc of the variables is must. last  ***;
  ***            variable name in the list, will be variables    ***;
  ***            for which the changes will be checked for other ***;
  ***            variables combinations.                         ***;
  ***                                                            ***;
  ******************************************************************;

	******************************************************************;
  *** Modify:                                                    ***;
  *** Modifying the by variable list with a equal(=) inside and  ***;
  ***  at the end of the list. In SAS macro, CATX concatanation  ***;
  ***  cannot work on "=" sign at the end. That's why giving     ***;
  ***  a "#" at the end with SAS CATX function and then changing ***;
  ***  the "#" at the last step with a space by TRANWRD function.***;
  ***  This modifying (MOD_KEYVAR) is to be used in the put      ***;
	***  statment to show result at LOG.                           ***;
  ******************************************************************;

%MACRO change_check( indata = , byvar =    );

  %*Checking if INDATA is missing;
  %IF %LENGTH(&indata.) EQ 0 %THEN %DO;
		DATA _NULL_;
		  PUT "WAR" "NING: (CHANGE_CHECK macro)==> Please insert a valid dataset name in the parameter INDATA.";
		  PUT "ERR" "OR: CHANGE_CHECK macro has stopped working.";
		RUN;
    %GOTO EXIT;
  %END;

  %*Taking last key variable to check change.;
  %LET last_keyvar = %SCAN(&byvar. , -1);
/*	%PUT &last_keyvar.;*/


	%******************************************************************;
  %*** Taking the second to last variable as base in the BYVAR    ***;
  %***  list as this is to be used to check the changes for the   ***;
  %***  variables combinations up to this. This is same to check  ***;
  %***  variables combinations prior to next to last variable.    ***;
  %******************************************************************;

  %LET base_keyvar = %SCAN(&byvar. , -2);
/*	%PUT &base_keyvar.;*/


  *Modify;
  %LET mod_var1   = %SYSFUNC( CATX( %NRSTR(=) , &byvar. , # ) );
	%LET mod_var2   = %SYSFUNC( TRANWRD( &mod_var1. , %NRSTR( ), %NRSTR(= )  ) ) ;
	%LET mod_keyvar = %SYSFUNC( TRANWRD( &mod_var2. , #, %NRSTR( )  ) ) ;
/*	%PUT &mod_keyvar.;*/

	%*Sorting with by variables.;
  PROC SORT DATA = &indata. OUT = change_check_srt;
    BY &byvar.;
  RUN; 

	%*Only taking FIRST observations after sorting with by variables.;
  DATA chng_check_1;
    SET change_check_srt;
	  BY &byvar.;
    IF FIRST.&last_keyvar.;
  RUN;

	%******************************************************************;
  %*** Checking changes with a flag variable.                     ***;
	%***                                                            ***;
	%***  *Change:                                                  ***;
  %***    Checking is being done on based key variable described  ***;
  %***    above.                                                  ***;
	%***  *END Checking:                                            ***;
	%***    Using IS_CHANGE flag variables to track the hanges after***;
  %***    giving message if there is no changes found. Then a     ***;
  %***    messgage is set to show at the end of the datastep.     ***;
  %******************************************************************;

	%*Checking changes with a flag variable.;
  DATA chng_check_show ( KEEP = &byvar. change_in_&last_keyvar. );
    SET chng_check_1  END = eof;
	  BY &byvar.;
   
		LENGTH change_in_&last_keyvar. $20;
		RETAIN  is_change 0 ;

		%*Change;
    IF ( FIRST.&base_keyvar. ^= LAST.&base_keyvar. ) OR ( FIRST.&base_keyvar. = LAST.&base_keyvar. = 0 ) THEN DO;

		  IF FIRST.&last_keyvar.  THEN DO;

        change_in_&last_keyvar. = "Yes";
        is_change = 1;

        PUT &mod_keyvar. change_in_&last_keyvar. = ;
			  OUTPUT;

			END;

		END;

		%*END Checking;
		IF eof and is_change = 1 THEN DO;

      CALL SYMPUTX ("is_change" , is_change );
		  CALL SYMPUTX ("chng_comment" , "==> Changes found in the variable %UPCASE(&last_keyvar.) for other key variables in the dataset %UPCASE(&indata.)");

		END;
		ELSE IF eof and is_change = 0 THEN DO;

      change_in_&last_keyvar. = "No changes found in %UPCASE(&last_keyvar.) for other key variables.";
			CALL SYMPUTX ("is_change" , is_change );
		  CALL SYMPUTX ("chng_comment" , "==> No changes found in the variable %UPCASE(&last_keyvar.) for other key variables in the dataset %UPCASE(&indata.)");
			OUTPUT;

		END;
  RUN;

	%*Showing the result in output if there is change;
	%IF &is_change. = 1 %THEN %DO;
/*	  PROC PRINT DATA = chng_check_show;*/
/*	  RUN;*/

	  TITLE3 "Changes of %UPCASE(&last_keyvar.) are found. See the changes.";
    PROC REPORT DATA = chng_check_show NOWD HEADLINE HEADSKIP SPACING=3 ; 
	    COLUMN ("Changes of %UPCASE(&last_keyvar.)"  &byvar. change_in_&last_keyvar. ); 
    RUN;
	  TITLE3;

  %END;

	%*Deleting intermediate datasets.;
  PROC DATASETS LIB = WORK MEMTYPE = DATA NOLIST;
    DELETE change_check_srt 
           chng_check_1
           chng_check_show;
  QUIT;
  RUN;

  %*Changes checking result as comment in LOG;
	%PUT &chng_comment.;

	%EXIT:

%MEND change_check;


%change_check( indata = n, byvar = x y z  );



  ******************************************************************;
  ***                                                            ***;
  *** %ADS_ORDER_CHECK Macro:                                    ***;
  ***   This macro will check the order of the ADS (main)        ***;
  ***   dataset's variables with the QC dataset in ADS and put   ***;
  ***   the results in the LOG. This is helpful for validation.  ***;
  ***                                                            ***;
  *** Parameters:                                                ***;
  ***   LIBNAME = Library (ADS)name of the main and qc datset.   ***;
  ***   QC_DATA = QC dataset name.                               ***;                               
  ***   LIBNAME = Main dataset name.                             ***;                                
  ***                                                            ***;
  ******************************************************************;

%MACRO ads_order_check( libname = , qc_data = , main_data = );

	%******************************************************************;
  %*** Taking variables from QC dataset and main datasets in      ***;
  %***   different macro variables.                               ***;
  %******************************************************************;

  PROC SQL NOPRINT;
	  SELECT STRIP(name) INTO : qc_vlist SEPARATED BY "@@@"
    FROM sashelp.vcolumn
    WHERE UPCASE(libname) = UPCASE("&libname.") AND UPCASE(memname) = UPCASE("&qc_data.");

	  SELECT STRIP(name) INTO : main_vlist SEPARATED BY "@@@"
    FROM sashelp.vcolumn
    WHERE UPCASE(libname) = UPCASE("&libname.") AND UPCASE(memname) = UPCASE("&main_data.");
  QUIT;


	%******************************************************************;
  %*** Checking the number of variables in the QC and main data.  ***;
  %***   If they are not same macro will stop working.            ***;
  %******************************************************************;

	%IF %SYSFUNC( COUNTW(&qc_vlist., %STR(@@@)) ) ^= %SYSFUNC( COUNTW(&main_vlist., %STR(@@@)) ) %THEN %DO;
	  DATA _NULL_;
		  PUT "WAR" "NING: Number of variables are not same in the QC and main data.";
		  PUT "ERR" "OR: ADS_ORDER_CHECK macro has stopped working.";
		RUN;
		%GOTO EXIT;
	%END;

	%*Checking the order of the variables.;
	DATA _NULL_;

	  %DO i = 1 %TO %SYSFUNC(COUNTW(&qc_vlist., %STR(@@@)));

		  %LET qc_var   = %SCAN(&qc_vlist., &i. , %STR(@@@));
		  %LET main_var = %SCAN(&main_vlist., &i. , %STR(@@@));

		  %IF &qc_var. = &main_var. %THEN %DO;
			  PUT "Order = &i." @14"QC variable = &qc_var."  @45"| Main variable = &main_var."  @80"| Variable is in order with QC in ADS";
		  %END;
		  %ELSE %DO;
			  PUT "Order = &i." @14"QC variable = &qc_var."  @45"| Main variable = &main_var."  @80"| Variable is not in order or case issue in ADS (Caution)";
		  %END;

	  %END;

	RUN;

	%EXIT:

%MEND ads_order_check;

%ads_order_check( libname = WORK, qc_data = n, main_data = nn);





  ******************************************************************;
  ***                                                            ***;
  *** BASIC_LOG_CHECKER Macro.                                   ***;
  ***   This macro can check the specified SAS LOG file for basic***;
  ***   checks and will show the summary with number of basic    ***;
  ***   alerts in the current programs log. It's specially       ***;
  ***   helpful for the validation while QC may need to check    ***;
  ***   the Main programmer's log or even his own log in a       ***;
  ***   speparate program.                                       ***;
  ***                                                            ***;
  *** Parameters:                                                ***;
  ***   LOG_FILE   = Log file name with the reference locatioin. ***;
  ***                Example: Reference\log_file_name.log        ***;
  ***   PROGRAMMER = Name of the type of programmer. That is for ***;
  ***                main programmer the value should be MAIN    ***;
  ***                and for qc programmer value should be QC.   ***;
  ***                This is an optional parameter.              ***; 
  ***                                                            ***;
  ******************************************************************;

* Version - 1;

%MACRO basic_log_checker( log_file = , programmer = );


	%******************************************************************;
  %*** A) Creating FILENAME to reference the log file location.   ***;
	%***    MISSOVER is used to read every line without input       ***;
  %***    interruption and PAD is used to read each line with same***;
	%***    length. The END = option specifies a variable that SAS  ***;
  %***    sets to 1 when the current input data record is the last***;
  %***    in the input file.                                      ***;
	%***                                                            ***;
	%*** B) Read the log file in a _NULL_ datastep. The INPUT       ***;
  %***    statement reads each line of the Log into the variable  ***;
  %***    LINE.                                                   ***;
	%***                                                            ***;
	%*** C) Storing the number of alerts in the macro variables at  ***;
	%***    the end of the datastep.                                ***;
  %******************************************************************;

	%*A) File name reference location;
  FILENAME logfile "&log_file.";

  DATA _NULL_;

	  %*B) Read the log file in dataset;
    INFILE logfile END = eof MISSOVER PAD;
    INPUT line $250.;

    IF UPCASE(COMPRESS(SUBSTR(line,1,6))) = "ERR" || "OR:" THEN DO;
      err + 1;
	  END;
    ELSE IF UPCASE(COMPRESS(SUBSTR(line,1,8))) = "WAR" || "NING:" THEN DO;
      warn + 1;
	  END;
    ELSE IF INDEX(UPCASE(line), ( "UNIN" ||  "ITIA" || "LIZED" ) ) THEN DO;
      un_inti + 1;
	  END;
    ELSE IF INDEX(UPCASE(COMPBL(line)), ( "AT LEAST ONE W" || ".D FORMAT WAS TOO SMALL") ) THEN DO;
      wd + 1;
    END;
    ELSE IF INDEX(UPCASE(line), ( "REP" || "EATS OF B" || "Y VALUES" ) ) THEN DO;
      by_repeat + 1;
    END;
    ELSE IF INDEX(UPCASE(line), ( "OVER" || "WRITTEN" ) ) THEN DO;
      by_overwrite + 1;
    END;
    ELSE IF FIND(UPCASE(COMPRESS(line)), ("MSG"||"LEVEL=I")) THEN DO;
      msg_level + 1;
    END;
    ELSE IF INDEX(UPCASE(COMPBL(line)), ( "NUM" || "ERIC VALUES HAVE BE" || "EN CONVE" || "RTED TO CHA" || "RACTER VALUES AT THE PLA" || "CES GIVEN BY" ) ) THEN DO;
      num_to_char + 1;
    END;
    ELSE IF INDEX(UPCASE(COMPBL(line)), ( "CHA" || "RACTER VALUES HAVE BE" || "EN CONVE" || "RTED TO NUM" || "ERIC VALUES AT THE PLA" || "CES GIVEN BY" ) ) THEN DO;
      char_to_nam + 1;
    END;
    ELSE IF INDEX(UPCASE(COMPBL(line)), ( "MIS" || "SING VALUES WE" || "RE GENE" || "RATED" ) ) OR INDEX(UPCASE(COMPBL(line)), ( "PERFO" || " RMING AN OPER" || "ATION ON MIS" || "SING VALUES" ) ) THEN DO;
      generate_missing + 1;
    END;
    ELSE IF ( ( UPCASE(COMPRESS(SUBSTR(line,1,6))) ^= "ERR" || "OR:") AND (UPCASE(COMPRESS(SUBSTR(line,1,8))) ^= "WAR" || "NING:" ) )  AND ( INDEX(UPCASE(COMPBL(line)), "INV" || "ALID DATA FOR") OR INDEX(UPCASE(COMPRESS(line)), "---+----1----+---") ) THEN DO;
      in_valid + 1;
	  END;

    %*C) Store the number of alerts in macro variables;
		IF eof THEN DO;

      CALL SYMPUTX('err',  err);
      CALL SYMPUTX('warn', warn);
      CALL SYMPUTX('un_inti', un_inti);
      CALL SYMPUTX('wd', wd);
      CALL SYMPUTX('by_repeat', by_repeat);
      CALL SYMPUTX('by_overwrite', by_overwrite);
      CALL SYMPUTX('num_to_char', num_to_char);
      CALL SYMPUTX('char_to_nam', char_to_nam);
      CALL SYMPUTX('generate_missing', generate_missing);
      CALL SYMPUTX('in_valid', in_valid);
      CALL SYMPUTX('msg_level', msg_level);

		END;

  RUN;


	%******************************************************************;
  %*** Showing the summary result in current SAS LOG.             ***;
  %******************************************************************;

  DATA _NULL_;

    PUT "**********************************************************************;";
    PUT "*              GENERAL SAS ALERTS IN LOG:                            *;";
		%IF	%UPCASE(&programmer.) = MAIN %THEN PUT "*"  @22"Main Programmer"  @70"*;";;
		%IF	%UPCASE(&programmer.) = QC   %THEN PUT "*"  @22"QC Programmer"    @70"*;";;
	  PUT "*                                                                   *;";
    PUT "* LOG: &log_file."                                                   @70"*;";
    PUT "*                                                                    *;";
    PUT "**********************************************************************;";
	  PUT "*                                                               "; 
    PUT "* NUMBER OF --> ERR" "ORS"                                        @59"= &err.";
    PUT "* NUMBER OF --> WAR" "NINGS"                                      @59"= &warn.";
    PUT "* NUMBER OF --> UNIN" "ITIALIZED VARIABLES"                       @59"= &un_inti.";
    PUT "* NUMBER OF --> GENE" "RATE MIS" "SING VALUES ISSUE"              @59"= &generate_missing.";
    PUT "* NUMBER OF --> W" ".D FORMAT ISSUE"                              @59"= &wd.";
    PUT "* NUMBER OF --> RE" "PEAT OF BY VAL" "UES ISSUE"                  @59"= &by_repeat.";
    PUT "* NUMBER OF --> VARIABLE OVE" "RWRITTEN ISSUE"                    @59"= &by_overwrite.";
    PUT "* NUMBER OF --> NUM" "MERIC VARIABLE HAVE BEEN CONV" "ERTED..."   @59"= &num_to_char.";
    PUT "* NUMBER OF --> CHAR" "ACTER VARIABLE HAVE BEEN CONV" "ERTED..."  @59"= &char_to_nam.";
    PUT "* NUMBER OF --> INV" "ALID DATA ISSUE"                            @59"= &in_valid.";
	  PUT "*";
    %IF &msg_level. = 0 %THEN PUT "* NOTE: 'OPTION MSG" "LEVEL = I' IS NOT FO" "UND.";;
	  PUT "*";
	  PUT "*";
    PUT "*********************************************************************;";
    PUT "*                                                                   *;";
    PUT "*********************************************************************;";

  RUN;

%MEND basic_log_checker;


%basic_log_checker(  log_file   = C:\Users\numan\Downloads\TEST NEW\test.log 
                    ,programmer = qc
                   );



  ******************************************************************;
  ***                                                            ***;
  *** BASIC_LOG_CHECKER Macro.                                   ***;
  ***   This macro can check the specified SAS LOG file for basic***;
  ***   checks and will show the summary with number of basic    ***;
  ***   alerts in the current programs log. It's specially       ***;
  ***   helpful for the validation while QC may need to check    ***;
  ***   the Main programmer's log or even his own log in a       ***;
  ***   speparate program.                                       ***;
  ***                                                            ***;
  *** Parameters:                                                ***;
  ***   LOG_FILE   = Log file name with the reference locatioin. ***;
  ***                Example: Reference\log_file_name.log        ***;
  ***   PROGRAMMER = Name of the type of programmer. That is for ***;
  ***                main programmer the value should be MAIN    ***;
  ***                and for qc programmer value should be QC.   ***;
  ***                This is an optional parameter.              ***; 
  ***   SUMMARY_INLOG = If one wants to see the result in the log***;
  ***                   then he has to pass y or Y or Y, YES to  ***;
  ***                   parameter. This is optional and by       ***;
  ***                   default result will be shown in the      ***;
  ***                   output.                                  ***;
  ***                                                            ***;
  ******************************************************************;

* Version - 2;

%MACRO basic_log_checker( log_file = , programmer = , summary_inlog =  ) / MINOPERATOR MINDELIMITER = '#';

  %IF %LENGTH(&programmer.) = 0 %THEN %LET programmer = %STR( );
  
	%******************************************************************;
  %*** A) Creating FILENAME to reference the log file location.   ***;
	%***    MISSOVER is used to read every line without input       ***;
  %***    interruption and PAD is used to read each line with same***;
	%***    length. The END = option specifies a variable that SAS  ***;
  %***    sets to 1 when the current input data record is the last***;
  %***    in the input file.                                      ***;
	%***                                                            ***;
	%*** B) Read the log file in a _NULL_ datastep. The INPUT       ***;
  %***    statement reads each line of the Log into the variable  ***;
  %***    LINE.                                                   ***;
	%***                                                            ***;
	%*** C) Storing the number of alerts in the macro variables at  ***;
	%***    the end of the datastep.                                ***;
  %******************************************************************;

	%*A) File name reference location;
  FILENAME logfile "&log_file.";

  DATA log_data ;

	  %*B) Read the log file in dataset;
    INFILE logfile END = eof MISSOVER PAD;
    INPUT line $250.;

		LENGTH message $100;
		RETAIN serial 0 alerts_fl 0;

    IF UPCASE(COMPRESS(SUBSTR(line,1,6))) = "ERR" || "OR:" THEN DO;
      err + 1;
			message   = "ER"||"ROR";
			serial    = serial + 1;
			alerts_fl = 1;
			OUTPUT;
	  END;
    ELSE IF UPCASE(COMPRESS(SUBSTR(line,1,8))) = "WAR" || "NING:" THEN DO;
      warn + 1;
			message   = "WAR"||"NING";
			serial    = serial + 1;
			alerts_fl = 1;
			OUTPUT;
	  END;
    ELSE IF INDEX(UPCASE(line), ( "UNIN" ||  "ITIA" || "LIZED" ) ) THEN DO;
      un_inti + 1;
			message   = "UNIN" ||  "ITIA" || "LIZED";
			serial    = serial + 1;
			alerts_fl = 1;
			OUTPUT;
	  END;
    ELSE IF INDEX(UPCASE(COMPBL(line)), ( "AT LEAST ONE W" || ".D FORMAT WAS TOO SMALL") ) THEN DO;
      wd + 1;
			message   = "AT LEAST ONE W" || ".D FORMAT WAS TOO SMALL";
			serial    = serial + 1;
			alerts_fl = 1;
		  OUTPUT;
    END;
    ELSE IF INDEX(UPCASE(line), ( "REP" || "EATS OF B" || "Y VALUES" ) ) THEN DO;
      by_repeat + 1;
			message   = "REP" || "EATS OF B" || "Y VALUES";
			serial    = serial + 1;
			alerts_fl = 1;
			OUTPUT;
    END;
    ELSE IF INDEX(UPCASE(line), ( "OVER" || "WRITTEN" ) ) THEN DO;
      by_overwrite + 1;
			message   = "OVER" || "WRITTEN" ;
			serial    = serial + 1;
			alerts_fl = 1;
			OUTPUT;
    END;
    ELSE IF FIND(UPCASE(COMPRESS(line)), ("MSG"||"LEVEL=I")) THEN DO;
      msg_level + 1;
			message   = "MSG"||"LEVEL=I" || "CONCERN" ;
			serial    = serial + 1;
			alerts_fl = 1;
			OUTPUT;
    END;
    ELSE IF INDEX(UPCASE(COMPBL(line)), ( "NUM" || "ERIC VALUES HAVE BE" || "EN CONVE" || "RTED TO CHA" || "RACTER VALUES AT THE PLA" || "CES GIVEN BY" ) ) THEN DO;
      num_to_char + 1;
			message   = "NUM" || "ERIC VALUE CONVE"|| "RTION" ;
			serial    = serial + 1;
			alerts_fl = 1;
			OUTPUT;
    END;
    ELSE IF INDEX(UPCASE(COMPBL(line)), ( "CHA" || "RACTER VALUES HAVE BE" || "EN CONVE" || "RTED TO NUM" || "ERIC VALUES AT THE PLA" || "CES GIVEN BY" ) ) THEN DO;
      char_to_nam + 1;
			message   = "CHA" || "RACTER VALUE CONVE"|| "RTION" ;
			serial    = serial + 1;
			alerts_fl = 1;
			OUTPUT;
    END;
    ELSE IF INDEX(UPCASE(COMPBL(line)), ( "MIS" || "SING VALUES WE" || "RE GENE" || "RATED" ) ) OR INDEX(UPCASE(COMPBL(line)), ( "PERFO" || " RMING AN OPER" || "ATION ON MIS" || "SING VALUES" ) ) THEN DO;
      generate_missing + 1;
			message   = "MIS" || "SING VALUES" || " GENE" || "RATE" ;
			serial    = serial + 1;
			alerts_fl = 1;
			OUTPUT;
    END;
    ELSE IF INDEX(UPCASE(COMPBL(line)), ( "DU" || "PLI" || "CATE BY VARI" || "ABLE(S) SPE" || "CIFIED" ) )  THEN DO;
      dup_by_var + 1;
			message   = "DU" || "PLI" || "CATE BY VARI" || "ABLE(S) SPE" || "CIFIED" ;
			serial    = serial + 1;
			alerts_fl = 1;
			OUTPUT;
    END;
    ELSE IF INDEX(UPCASE(COMPBL(line)), ( "DIVI" || "SION BY ZE" || "RO DET" || "ECTED") )  THEN DO;
      div_by_zero + 1;
			message   = "DIVI" || "SION BY ZE" || "RO DET" || "ECTED" ;
			serial    = serial + 1;
			alerts_fl = 1;
			OUTPUT;
    END;
    ELSE IF INDEX(UPCASE(COMPBL(line)), ( "MATHEM" || "ATICAL OPE" || "RATIONS COULD N" || "OT BE PER" || "FORMED" ) )  THEN DO;
      no_math_op + 1;
			message   = "MATHEM" || "ATICAL OPE" || "RATIONS COULD N" || "OT BE PER" || "FORMED" ;
			serial    = serial + 1;
			alerts_fl = 1;
			OUTPUT;
    END;
    ELSE IF ( ( UPCASE(COMPRESS(SUBSTR(line,1,6))) ^= "ERR" || "OR:") AND (UPCASE(COMPRESS(SUBSTR(line,1,8))) ^= "WAR" || "NING:" ) )  AND ( INDEX(UPCASE(COMPBL(line)), "INV" || "ALID DATA FOR") OR INDEX(UPCASE(COMPRESS(line)), ( "--" || "-+" || "---" || "-1" || "---" || "-+" || "--" ) ) ) THEN DO;
      in_valid + 1;
			message   = "INV" || "ALID DATA OR " || "--" || "-+" || "---" || "-1" || "---" || "-+" || "--- ISSUE";
			serial    = serial + 1;
			alerts_fl = 1;
			OUTPUT;
	  END;
    ELSE IF INDEX(UPCASE(COMPBL(line)), ( "INV" || "ALID" ) )  THEN DO;
      inv_alid_stat + 1;
			message   = "SOMETHING REGARDING INV" || "ALID STATMENT" ;
			serial    = serial + 1;
			alerts_fl = 1;
			OUTPUT;
    END;
    ELSE IF INDEX(UPCASE(COMPBL(line)), ( "UNA" || "BLE TO" || " OPEN" ) ) OR INDEX(UPCASE(COMPBL(line)), ( "WILL BE OPE" || "NED INS" || "TEAD" ) ) THEN DO;
      una_ble + 1;
			message   = "UNA" || "BLE TO" || " OPEN ISSUE" ;
			serial    = serial + 1;
			alerts_fl = 1;
			OUTPUT;
    END;
    ELSE IF INDEX(UPCASE(COMPBL(line)), ( "ERR" || "OR" ) ) OR INDEX(UPCASE(COMPBL(line)), ( "WAR" || "NING" ) )  THEN DO;
      pgm_err + 1;
			message   = "PROGRAMMER'S WRITTEN ER" || "ROR OR WAR" || "NING";
			serial    = serial + 1;
			alerts_fl = 1;
			OUTPUT;
    END;

    %*C) Store the number of alerts in macro variables;
		IF eof THEN DO;

      CALL SYMPUTX('err',  err);
      CALL SYMPUTX('warn', warn);
      CALL SYMPUTX('un_inti', un_inti);
      CALL SYMPUTX('wd', wd);
      CALL SYMPUTX('by_repeat', by_repeat);
      CALL SYMPUTX('by_overwrite', by_overwrite);
      CALL SYMPUTX('num_to_char', num_to_char);
      CALL SYMPUTX('char_to_nam', char_to_nam);
      CALL SYMPUTX('generate_missing', generate_missing);
      CALL SYMPUTX('dup_by_var', dup_by_var);
			CALL SYMPUTX('div_by_zero', div_by_zero);
			CALL SYMPUTX('no_math_op', no_math_op);
			CALL SYMPUTX('inv_alid_stat', inv_alid_stat);
      CALL SYMPUTX('in_valid', in_valid);
      CALL SYMPUTX('msg_level', msg_level);
			CALL SYMPUTX('una_ble', una_ble);
			CALL SYMPUTX('pgm_err', pgm_err);
			CALL SYMPUTX('alerts_fl', alerts_fl);
			
			IF alerts_fl = 0 THEN DO;
			  message   = "No Basic Is" || "sue Found";
				line      = "Log has no basic alerts to check.";
			  serial    = 1;
			  OUTPUT;
      END;

		END;

  RUN;

	%*Printing the alerts list on output;
	TITLE2 "GENERAL SAS LOG ALERTS LIST";
  PROC REPORT DATA = log_data NOWD HEADLINE HEADSKIP SPACING=3 ; 
	  COLUMN ("GENERAL SAS ALERTS LIST IN %UPCASE(&programmer.) - LOG FILE" " " "LOG: &log_file." " " serial message line); 
    DEFINE serial  / "SL. No" ; 
    DEFINE message / "Message"                     WIDTH=30 FLOW LEFT;  
	  DEFINE line    / "Log Lines For The Message"   WIDTH=80 FLOW LEFT;   
  RUN;
	TITLE2;

	%*Creating the summary;
	DATA log_summary;

	  LENGTH message $100;
	  
	  message = "ERR" ||"ORS | (Check the list to be ensure that they are not programmer's written)";
    freq    = &err.;
		serial  = 1;
		OUTPUT;
    message = "WAR" || "NINGS | (Check the list to be ensure that they are not programmer's written)";
    freq    = &warn.;
		serial  = 2;
		OUTPUT;
    message = "UNIN" || "ITIALIZED VARIABLES";
    freq    = &un_inti.;
		serial  = 3;
		OUTPUT;
    message = "GENE" || "RATE MIS" || "SING VALUES ISSUE";
    freq    = &generate_missing.;
		serial  = 4;
		OUTPUT;
    message = "DU" || "PLI" || "CATE BY VARI" || "ABLE(S) SPE" || "CIFIED";
    freq    = &dup_by_var.;
		serial  = 5;
		OUTPUT;
    message = "MATHEM" || "ATICAL OPE" || "RATIONS N" || "OT PER" || "FORMED";
    freq    = &div_by_zero.;
		serial  = 6;
		OUTPUT;
    message = "W" || ".D FORMAT ISSUE";
    freq    = &wd.;
		serial  = 7;
		OUTPUT;
    message = "RE" || "PEAT OF BY VAL" || "UES ISSUE" ;
    freq    = &by_repeat.;
		serial  = 8;
		OUTPUT;
    message = "VARIABLE OVE" || "RWRITTEN ISSUE" ;
    freq    = &by_overwrite.;
		serial  = 9;
		OUTPUT;
    message = "NUM" || "MERIC VARIABLE HAVE BEEN CONV" || "ERTED..." ;
    freq    = &num_to_char.;
		serial  = 10;
		OUTPUT;
    message = "CHAR" || "ACTER VARIABLE HAVE BEEN CONV" || "ERTED...";
    freq    = &char_to_nam.;
		serial  = 11;
		OUTPUT;
    message = "INV" || "ALID DATA ISSUE" ;
    freq    = &in_valid.;
		serial  = 12;
		OUTPUT;
    message = "SOMETHING REGARDING INV" || "ALID STATMENTS" ;
    freq    = &inv_alid_stat.;
		serial  = 13;
		OUTPUT;
    message = "UNA" || "ABLE TO OPEN ISSUE";
    freq    = &una_ble.;
		serial  = 14;
		OUTPUT;
    message = "PROGRAMMER'S WRITTEN ER" || "ROR OR WAR" || "NING" ;
    freq    = &pgm_err.;
		serial  = 15;
    OUTPUT;

    %IF &msg_level. = 0 %THEN %DO;
      message = "NOTE: 'OPTION MSG" || "LEVEL = I' IS NOT FO" || "UND.";
			freq    = .;
		  serial  = 16;
		  OUTPUT;
		%END;

	RUN;

	%*Printing the summary on output;
	TITLE2 "SUMMARY OF ALERTS IN SAS LOG";
  PROC REPORT DATA = log_summary  NOWD HEADLINE HEADSKIP SPACING=3 ; 
	  COLUMN ("SUMMARY OF GENERAL SAS ALERTS LIST IN %UPCASE(&programmer.) - LOG FILE" " " "LOG: &log_file." "" serial message freq); 
    DEFINE serial  / "SL. No" ; 
    DEFINE message / "Alerts"   WIDTH = 50 FLOW LEFT;  
	  DEFINE freq    / "Frequency" WIDTH = 10 FLOW CENTER;   
  RUN;
	TITLE2;

	%*Deleting intermediate datasets.;
  PROC DATASETS LIB = WORK MEMTYPE = DATA NOLIST;
    DELETE log_data 
           log_summary;
  QUIT;
  RUN;

	%*Message about alerts in LOG;
  %IF &alerts_fl. = 0 %THEN %PUT ==> Log has no basic alerts. (&log_file.);
	%ELSE %PUT ==> Log has some basic alerts. See the output. (&log_file.);


	%******************************************************************;
  %*** Showing the summary result of log alerts in current SAS    ***;
  %***   LOG if SUMMARY_INLOG is given as y or Y or yes or YES.   ***;
  %******************************************************************;

	%IF %LENGTH(&summary_inlog.) = 0 %THEN %LET summary_inlog = N;

  %IF %UPCASE(&summary_inlog.) IN(y#yes#Y#YES) %THEN %DO;

    DATA _NULL_;

      PUT "**********************************************************************;";
      PUT "*              GENERAL SAS ALERTS IN LOG:                            *;";
		  %IF	%UPCASE(&programmer.) = MAIN %THEN PUT "*"  @22"Main Programmer"  @70"*;";;
		  %IF	%UPCASE(&programmer.) = QC   %THEN PUT "*"  @22"QC Programmer"    @70"*;";;
	    PUT "*                                                                    *;";
      PUT "* LOG: &log_file."                                               @70"*;";
      PUT "*                                                                    *;";
      PUT "**********************************************************************;";
	    PUT "*                                                               "; 
      PUT "* NUMBER OF --> ERR" "ORS"                                        @59"= &err.";
      PUT "* NUMBER OF --> WAR" "NINGS"                                      @59"= &warn.";
      PUT "* NUMBER OF --> UNIN" "ITIALIZED VARIABLES"                       @59"= &un_inti.";
      PUT "* NUMBER OF --> GENE" "RATE MIS" "SING VALUES ISSUE"              @59"= &generate_missing.";
      PUT "* NUMBER OF --> DU" "PLI" "CATE BY VARI" "ABLE(S) SPE" "CIFIED"   @59"= &dup_by_var.";
      PUT "* NUMBER OF --> DIVI" "SION BY ZE" "RO DET" "ECTED"               @59"= &div_by_zero.";
      PUT "* NUMBER OF --> MATHEM" "ATICAL OPE" "RATIONS N" "OT PER" "FORMED"@59"= &div_by_zero.";
      PUT "* NUMBER OF --> W" ".D FORMAT ISSUE"                              @59"= &wd.";
      PUT "* NUMBER OF --> RE" "PEAT OF BY VAL" "UES ISSUE"                  @59"= &by_repeat.";
      PUT "* NUMBER OF --> VARIABLE OVE" "RWRITTEN ISSUE"                    @59"= &by_overwrite.";
      PUT "* NUMBER OF --> NUM" "MERIC VARIABLE HAVE BEEN CONV" "ERTED..."   @59"= &num_to_char.";
      PUT "* NUMBER OF --> CHAR" "ACTER VARIABLE HAVE BEEN CONV" "ERTED..."  @59"= &char_to_nam.";
      PUT "* NUMBER OF --> INV" "ALID DATA ISSUE"                            @59"= &in_valid.";
      PUT "* NUMBER OF --> SOMETHING REGARDING INV" "ALID STATMENTS"         @59"= &inv_alid_stat.";
      PUT "* NUMBER OF --> PROGRAMMER'S WRITTEN ER" "ROR OR WAR" "NING"      @59"= &pgm_err.";
      PUT "* NUMBER OF --> UNA" "ABLE TO OPEN ISSUE"                         @59"= &una_ble.";
	    PUT "*";
      %IF &msg_level. = 0 %THEN PUT "* NOTE: 'OPTION MSG" "LEVEL = I' IS NOT FO" "UND.";;
	    PUT "*";
	    PUT "*";
      PUT "*********************************************************************;";
      PUT "*                                                                   *;";
      PUT "*********************************************************************;";

    RUN;

	%END;

%MEND basic_log_checker;

%basic_log_checker(  log_file   = C:\Users\numan\Downloads\TEST NEW\test.log 
                    , programmer = main
										, summary_inlog = 
                   );
