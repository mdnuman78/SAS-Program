


	PROC IMPORT DATAFILE ="\\server\Projects\Agilis-SA\21LAQ01\Pgm\amerged\For_Supportive_Programmer\Numan\SDTM Terminology.xlsx"  OUT = ct DBMS = xlsx REPLACE ;
	  SHEET = 'SDTM Terminology 2022-09-30';
	RUN;


	PROC IMPORT DATAFILE ="\\server\Projects\Agilis-SA\21LAQ01\Pgm\amerged\For_Supportive_Programmer\Numan\define-2022-10-10T10-29-30-976_v8.xlsx"  OUT = define_code DBMS = xlsx REPLACE ;
	  SHEET = 'Codelists';
	RUN;

OPTION MSGLEVEL = I;






DATA ct_extend;
  LENGTH var3 extensible $ 12 ;
	RETAIN extensible "";
  SET ct ;

	IF STRIP(var3) ^= "" THEN extensible = var3; 

	IF STRIP(codelist_code) = "" THEN DELETE;
RUN;



DATA ct_test1 (KEEP = code codelist_code codelist_name cdisc_submission_value extensible );
  LENGTH code  codelist_code $ 12  var3 extensible $ 12;

  SET ct_extend ;
  
  codelist_code = STRIP(codelist_code);
	code = STRIP(code);

  FORMAT _ALL_; 
  INFORMAT _ALL_;
RUN;


/*PROC FREQ DATA = ct_test1;*/
/*  TABLE codelist_code*code/ LIST MISSING NOPERCENT NOCUM ;*/
/*RUN;*/



PROC SORT DATA = ct_test1  OUT = ct_srt;
  BY codelist_code code;
RUN;



DATA define_list;
  SET define_code (KEEP= nci_codelist_code   nci_term_code term  id  name  ) ;

	IF  STRIP(nci_codelist_code) ^= "" AND STRIP(nci_term_code) ^= "" ;

	codelist_code = STRIP(nci_codelist_code);
	code = STRIP(nci_term_code);

	FORMAT _ALL_; INFORMAT _ALL_;
RUN;


PROC SORT DATA = define_list  OUT = define_srt NODUPKEY;
  BY codelist_code code;
RUN;



/*PROC FREQ DATA = define_srt ;*/
/*  TABLE codelist_code*code/ LIST MISSING NOPERCENT NOCUM ;*/
/*RUN;*/



PROC SQL;
  CREATE TABLE check1 AS
	SELECT a.id,
	       a.name,
				 a.nci_codelist_code,
				 a.nci_term_code,
				 a.term,
				 b.cdisc_submission_value,
				 b.codelist_name,
				 b.extensible,
				 b.codelist_code,
				 b.code
	FROM define_srt AS a LEFT JOIN ct_srt as b
	ON STRIP(a.codelist_code)=STRIP(b.codelist_code) AND STRIP(a.code) = STRIP(b.code)
  ORDER BY b.codelist_code, b.code;
QUIT;



DATA check_missmatch;
  LENGTH status $ 20;
  SET check1;
	IF cdisc_submission_value = term THEN status = "MATCH";
	ELSE status = "MISSMATCH";
RUN;







