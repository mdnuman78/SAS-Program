/*!Clinrep\Agilis-SA\21LAQ01\AMerged\specification\ADAM define-21LAQ01_v2.xlsx*/
OPTIONS MPRINT;

%MACRO spec_test(libnm=, spec_path=, spec_name=);

	PROC IMPORT DATAFILE="&spec_path.\&spec_name..xlsx" OUT=spec DBMS=xlsx
		REPLACE ;
		SHEET='Variables';
	RUN;

	DATA vcol ( KEEP=libname memname name len label format lib_data var RENAME=
		(memname=dataset name=variable ) WHERE=( UPCASE(STRIP(libname))=
		%UPCASE("&libnm.") ));

		LENGTH memname $ 50 name $ 50 ;

		SET sashelp.vcolumn;

		lib_data=memname;
		var=name ;
		len=STRIP(PUT(length, BEST.));

	RUN;

	DATA spec_f ( KEEP=dataset variable label length format RENAME=(label=
		spec_label length=spec_length format=spec_format ));

		LENGTH dataset variable $ 50 format $ 20;
		SET spec;

		IF STRIP(format) ^= "" AND SUBSTR( STRIP(format), LENGTH(STRIP(format))
			) ^= "." THEN format=STRIP(format)||".";
	RUN;

	PROC SORT DATA=spec_f (WHERE=(STRIP(dataset) NE "")) OUT=spec_srt NODUPKEY;
		BY dataset variable;
	RUN;

	PROC SORT DATA=vcol OUT=vcol_srt NODUPKEY;
		BY dataset variable;
	RUN;

	DATA check (DROP=length_flag);

		LENGTH missname status $200;

		MERGE spec_srt (IN=ina) vcol_srt (IN=inb);
		BY dataset variable;

		IF inb;

		missname="";

		length_flag=0;

		IF STRIP(spec_label) ^= STRIP(label) THEN DO;

			status="MISMATCH";
			missname=STRIP(missname) || " Label";
			PUT "Mismatch in LABEL." @22 dataset=@40 var=@55 label=100
				spec_label=;

		END;

		IF STRIP(spec_length)="" THEN DO;

			status="Spec Length Missing";

			IF length_flag=0 THEN DO;
				missname=STRIP(missname) || " Length";
				length_flag=1;
			END;

			PUT "Mismatch in LENGTH." @22 dataset=@40 var=@55 len=@90
				spec_length=;

		END;
		ELSE IF STRIP(spec_length) ^= STRIP(len) THEN DO;

			status="MISMATCH";

			IF length_flag=0 THEN DO;
				missname=STRIP(missname) || " Length";
				length_flag=1;
			END;

			PUT "Mismatch in LENGTH." @22 dataset=@40 var=@55 len=@90
				spec_length=;

		END;

		IF STRIP(spec_format)="ISO 8601." THEN DO;

			IF STRIP(spec_length) ^= "" AND STRIP(spec_length) ^= STRIP(len)
				THEN startus="ISO length matched";

			ELSE DO;
				status="Cheack spec length for ISO 8601.";

				IF length_flag=0 THEN DO;
					missname=STRIP(missname) || " Length";
					length_flag=1;
				END;

			END;

		END;
		ELSE IF STRIP(spec_format) ^= STRIP(format) THEN DO;

			status="MISMATCH";
			missname=STRIP(missname) || " Format ";
			PUT "Mismatch in LENGTH." @22 dataset=@40 var=@55 format=@90
				spec_format=;

		END;

	RUN;

	PROC SQL;
		CREATE TABLE final_check AS SELECT dataset , variable , spec_label ,
			spec_length , spec_format , lib_data , var , len , label , format ,
			status , missname FROM check;
	QUIT;

	/*  PROC SQL NOPRINT;*/
	/*	  SELECT  DISTINCT STRIP(dataset)*/
	/*          , COUNT(DISTINCT STRIP(dataset))*/
	/*		INTO    : spec_ds SEPARATED BY " " */
	/*		      , : spec_dsnum*/
	/*		FROM spec;*/
	/**/
	/*		%PUT &spec_ds.; */
	/*    %PUT &spec_dsnum;*/
	/**/
	/**/
	/*	  SELECT  DISTINCT STRIP(memname)*/
	/*          , COUNT(DISTINCT STRIP(memname))*/
	/*		INTO    : dsv SEPARATED BY " " */
	/*		      , : dsvnum*/
	/*		FROM vcol;*/
	/**/
	/*		%PUT &dsv.; */
	/*    %PUT &dsvnum;*/
	/**/
	/*	QUIT;*/
%MEND spec_test;

%spec_test( libnm=sdtmf , spec_path=
	!Clinrep\Agilis-SA\21LAQ01\Pgm\amerged\For_Supportive_Programmer\Numan ,
	spec_name=define-2022-10-10T10-29-30-976_v8 );

/*  PROC IMPORT DATAFILE ="!Clinrep\Agilis-SA\21LAQ01\AMerged\specification\ADAM define-21LAQ01_v2.xlsx" OUT = spec DBMS = xlsx REPLACE ;*/
/*	  SHEET = 'Variables';*/
/*	RUN;*/
