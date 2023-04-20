/***********************
	Importing Dataset
***********************/

proc import datafile="/home/u61324950/raw_data.csv"
	out = train_data
	dbms=csv
	replace;
	getnames=yes;
run;

/* Checking and Removing the duplicates */

proc sort data=train_data nodupkey;
by id;
run;

/***********************************************
       2.1 Data Merge = Internal + Bureau
***********************************************/

/* proc sql; */
/* create table table_1 as */
/* select * from train_data */
/* where id<=84000; */
/* quit; */
/*  */
/* proc sql; */
/* create table table_2 as */
/* select * from train_data */
/* where id>84000 and id<=168000; */
/* quit; */
/*  */
/* proc sql; */
/* create table table_3 as */
/* select * from train_data */
/* where id>168000 and id<=252000; */
/* quit; */

/************************************************************************************************************
2.2 Deleting insignificant records & variables i.e. variables such as IDs, rows with a high % of null values 
************************************************************************************************************/

data train_data_2;
	set train_data;
	drop id city state;
run;

proc contents data=train_data_2 out=cols noprint;
run;

data _null_;
	set cols nobs=total;
	call symputx('totvar', total);
run;

data train_data_2;
	set train_data_2;
	miss_count=cmiss(of _all_)-1;
run;

data train_data_2;
	set train_data_2;
	if miss_count>round(&totvar/2) then delete;
run;

data train_data_2;
	set train_data_2;
	drop miss_count;
run;

/*********************************************
	 3.1 Encoding Categorical Variables 
**********************************************/

/* Creating separate dataset for categorical variables */

data cat_data(drop =_numeric_);
	set train_data_2;
run;

/* Checking number of unique values in each categorical variable */

ods output nlevels=nunique_var;
proc freq data=cat_data nlevels;
	tables _all_/noprint;
run;

data nunique_var(drop=nmisslevels nlevels);
	set nunique_var;
run;

/* Extracting binary categorical variables */

data binary_var;
	set nunique_var;
	if nnonmisslevels>2 then delete;
run;

/* Saving Dataset */

data binary_encoded_data;
	set train_data_2;
run;

/* Binary Encoding */

%macro calling_binary();
	proc sql noprint;
		select distinct(tablevar) 
		into:cat_var1- 
		from binary_var; 
		select count(distinct(tablevar)) 
		into:max_itr
		from binary_var;
	quit;
	data _null_;
		%do i=1 %to &max_itr;
			proc sql noprint; 
				select distinct(&&&cat_var&i) into:value1- from binary_encoded_data where &&&cat_var&i <>""; 
				select count(distinct(&&&cat_var&i)) into:mx from binary_encoded_data; 
			quit; 
			data binary_encoded_data; 
				set binary_encoded_data; 
				%do j=1 %to &mx; 
					if &&&cat_var&i="&&&value&j" then &&&cat_var&i=&j-1;
				%end; 
			run;
		%end;
	run;
%mend;

%calling_binary();

/* Extracting variables which we can apply One-Hot Encoding */

data one_hot_var;
	set nunique_var;
	if nnonmisslevels<3  then delete;
	if  nnonmisslevels>5 then delete;
run;

/* Saving Dataset */

data OneHot_encoded_data;
	set binary_encoded_data;
run;

/* One-Hot Encoding */

%macro calling_One_Hot();
	proc sql noprint;
		select distinct(tablevar) 
		into:cat_var1- 
		from one_hot_var;
		
		select count(distinct(tablevar)) 
		into:max_itr
		from one_hot_var;
	quit;
	
	data _null_;
		%do i=1 %to &max_itr;
			proc sql noprint;
				select distinct(&&&cat_var&i) into:val1- from OneHot_encoded_data;
			 	select count(distinct(&&&cat_var&i)) into:len from OneHot_encoded_data;
			quit;
			data OneHot_encoded_data;
				set OneHot_encoded_data;
			   	%do j=2 %to &len;
			    	if &&&cat_var&i="&&&val&j" then %sysfunc(compress(&&&val&j,'$ - /'))=1 ;
			        else  %sysfunc(compress(&&&val&j,'$ - /'))=0;
			    %end;
			run;
		%end;
	run;
	data OneHot_encoded_data;
		set OneHot_encoded_data;
		%do i=1 %to &max_itr;
			drop &&&cat_var&i;
		%end;
	run;
%mend;

%calling_One_Hot();

/* Extracting variables which we can apply Frequency Encoding */

data freq_var;
	set nunique_var;
	if  nnonmisslevels<5 then delete;
run;

/* Saving Dataset */

data freq_encoded_data;
	set OneHot_encoded_data;
run;

/* Frequency Encoding */

%macro calling_Freq(dataset);
	proc sql noprint;
		select distinct(tablevar) 
		into:cat_var1- 
		from freq_var; 
		select count(distinct(tablevar)) 
		into:max_itr
		from freq_var;
	quit;
	data _null_;
		%do i=1 %to &max_itr;
			proc sql noprint; 
				create table freq as select distinct(&&&cat_var&i) as values, count(&&&cat_var&i) as number 
				from &dataset 
				group by Values ; 
				create table &dataset as select *, round(freq.number/count(&&&cat_var&i),00.01) As freq_encoded_&i 
				from &dataset left join freq on &&&cat_var&i=freq.values;
			quit;
			%put &&&cat_var&i;
			%put &max_itr;
			data &dataset(drop=values number &&&cat_var&i); 
				set &dataset; 
			run; 
		%end;
	run;
%mend;
%calling_Freq(freq_encoded_data);

/***************************************
		3.2 Finding Correlation 
****************************************/

/* Saving dataset */

data correlation_data;
	set freq_encoded_data;
run;

proc corr data=correlation_data
	outp=corr_data;
run;

/* Filtering out only correlation values  */

data drop_var(drop=_type_ );
	set corr_data;
	where _type_='CORR';
run;

/* Considering only lower triangle correlated values */

data lower_tri(drop=__i);
   set drop_var end=__eof;
   array __n[*] _numeric_;
   do __i = _n_ to dim(__n); __n[__i] = ._; end;
   if _n_ = 1 then do;
      call execute('data _null_; set lower_tri;');
      call execute('file print ods=(template="Base.Corr.StackedMatrix"');
      call execute('columns=(rowname=variable');
   end;
   call execute(cats('matrix=',vname(__n[_n_]),'(generic)'));
   if __eof then call execute(')); put _ods_; run;');
run;

/* Extracting correlated variables */

data Corr_Vars(keep=val); 
	set lower_tri; 
	array col income -- freq_encoded_1; 
	do over col; 
		if col>.60 then val=_name_;
	end;
run;

/****************************************************************
 			 3.3 Droping Insignificant Variables 
 ****************************************************************/

%macro delete_corr_vars(dataset);
	proc sql;
		select distinct(val) into:var1-
		from corr_vars;
		
		select count(distinct(val)) into:max_itr
		from corr_vars;
	quit;
	data _null_;
		%do i=1 %to &max_itr;
			%put &&&var&i;
			
			data &dataset(drop=&&&var&i); 
				set &dataset; 
			run; 
		%end;
	run;
%mend;

%delete_corr_vars(correlation_data);

/********************************************************  
			3.4 Missing Value Treatment
*********************************************************/

/* Saving Dataset */

data MVT;
	set correlation_data;
run;

/* Checking datatypes of the variables */

proc contents data= MVT out=variable_names(keep=name);
run; 

/* Checking Missing Values in Categorical Variables */

data cat_data(drop =_numeric_);
	set train_data_2;
run;

proc freq data=cat_data order=freq;
run;

/* Checking Missing Values in Numerical Variables */
proc means data=train_data n nmiss;
run;
/* Found no missing values in both Numerical and Categorical Variables*/

/* If there are any missing values in a variable we can drop/impute that variable
	based on the percentage of missing values and the importance */
	
/* CASE-1 Imputing missing values with mean/median/mode based on the datatype and distibution*/
/* proc sgplot data=data; */
/* 	histogram variable; */
/* 	density variable; */
/* run; */

/* if numeric -> normally distributed -> impute mean */
/* 		   -> skewed -> impute median */
/* if categoric -> impute mode */

/*proc stdize data=train_data out=imputed_train_data*/
/*	reponly*/
/*	method=median;*/
/*	var variable_1 ;*/
/*run;*/

/* CASE-2 Imputing with our own value*/

/* data imputed_train_data; */
/* 	set train_data; */
/* 	if missing(variable) then variable=value; */
/* 	else variable=variable; */
/* run; */

/* CASE-3 Drop the variable if it has more than 50% of missing values*/
/* data new_data; */
/* 	set data; */
/* 	drop variable; */
/* run; */


/* Handling outliers*/

proc sgplot data=encoded_data;
	vbox income;
run;
proc sgplot data=encoded_data;
	vbox Age;
run;
proc sgplot data=encoded_data;
	vbox Experience;
run;
proc sgplot data=encoded_data;
	vbox Current_Job_yrs;
run;
proc sgplot data=encoded_data;
	vbox Current_House_yrs;
run;
/* Found no outliers */
/* If there are outliers, we can either remove them or replace them with other values*/

/*proc standard data=encoded_data mean=0 std=1 out=std_data;*/
/*run;*/
/**/
/*data new_std_data;*/
/*	set std_data;*/
/*	where variable <-3 or variable > 3;*/
/*run;*/

/*proc means data=encoded_Data p5 p95;*/
/*	var _all_;*/
/*	output out=p5_p95_table;*/
/*run;*/
/*proc print data=p5_p95_table;*/
/*run;*/

proc summary data=encoded_data;
var _all_;
output out=Loan_Summary;
run;
proc print data=Loan_Summary;

/* Standerdizing the Data */
proc stdize data=encoded_data out=std_data;
run;


/*Standardizing the data*/
data cat_data;
	set encoded_data;
	keep cat:;
run;
data num_data;
	set encoded_data;
	drop cat: risk_flag;
run;

proc sgplot data=encoded_data;
	histogram income;
	density income;
run; 
proc sgplot data=encoded_data;
	histogram age;
	density age;
run;
proc sgplot data=encoded_data;
	histogram experience;
	density experience;
run;
proc sgplot data=encoded_data;
	histogram current_job_years;
	density current_job_years;
run;
proc sgplot data=encoded_data;
	histogram current_house_years;
	density current_house_years;
run;


proc standard data=num_data mean=0 std=1 out=std_data;
run;	

proc stdize data=num_data out=std_data method=std;
	var _all_;
run;

data final_data2;
	set cat_data;
	set std_data;
run;


/* Principle Component Analysis */
proc princomp data=encoded_data;
run;

/* Considering eigen values greater than 1*/
proc factor data=std_data
	method=principal
	priors=one
	rotate=none;
run;

proc factor data=encoded_data
	method=principal
	n=55
	out=pca_data;
run;

data new_data;
	set pca_data(keep=factor1-factor55);
run;

proc fastclus data=new_data maxc=2 maxiter=25 out=clus outstat=stat;
run;

/*proc freq data=clus;*/
/*	table cluster*risk_flag;*/
/*run;*/
proc candisc data=clus anova out=can;
      class cluster;
run;
   
proc sgplot data=Can;
      scatter y=Can2 x=Can1 /group=Cluster ;
run;
/* Finiding optimal K value */

%macro k_means_clus(in_dns, maxiter);
	** run k means with different number of clusters;
	%do k=2 %to &maxiter.;
	ods output ccc=ccc&k.(keep = value rename = (value= ccc));
	proc fastclus data = std_data maxiter=100 maxc=&k. out=clut_clus&k. outstat=stat_&k.;
		var _all_;
	run;
	data ccc&k.;
		set ccc&k.;
		nclust=&k.;
	run;
	data fstat_&k.;
		set stat_&k.;
		if _type_='PSEUDO_F';
		keep over_all ;
	run;
	data ccc_&k.;
		set stat_&k.;
		if _type_ = 'CCC';
		keep over_all ;
	run;
	data Rsqr_&k.;
		set stat_&k.;
		if _type_ ='RSQ';
		keep over_all;
	run;
	proc append base = ccc data = ccc&k.;
	run;
	proc delete data = ccc&k.;
	run;
	proc append base = fstat data= fstat_&k.;
	run;
	proc delete data = fstat_&k.;
	run;
	proc append base = ccc_ data= ccc_&k.;
	run;
	proc delete data = ccc_&k.;
	run;
	proc append base = rsq data= Rsqr_&k.;
	run;
	proc delete data = Rsqr_&k.;
	run;
%end;
data final_ccc;
set ccc;
set ccc_;
run;
data final_fstat;
set ccc;
set fstat;
run;
data final_rsq;
set ccc;
set rsq;
run;
proc sgplot data=final_ccc;
	series y=over_all x=nclust;
	yaxis label = "CCC";
	xaxis label = "no of clusters";
quit;
proc sgplot data=final_fstat;
	series y=over_all x=nclust;
	yaxis label = "Pseudo F stat";
	xaxis label = "no of clusters";
quit;
proc sgplot data=final_rsq;
	series y=over_all x=nclust;
	yaxis label = "R-Square";
	xaxis label = "no of clusters";
quit;

%mend k_means_clus;
%k_means_clus(new_data,15);


%macro dif_clus(k);
%do k=4 %to 14;
	proc fastclus data=new_data maxc=&k. outstat=clus_stat_&k.;
	run;
%end;
%mend;
%dif_clus;

data clus_4;
	set clus_stat_4;
	nclust=4;
	if _type_ = 'RSQ';
	keep nclust over_all;
run;
data clus_5;
	set clus_stat_5;
	nclust=5;
	if _type_ = 'RSQ';
	keep nclust over_all;
run;
data clus_6;
	set clus_stat_6;
	nclust=6;
	if _type_ = 'RSQ';
	keep nclust over_all;
run;
data clus_7;
	set clus_stat_7;
	nclust=7;
	if _type_ = 'RSQ';
	keep nclust over_all;
run;
data clus_8;
	set clus_stat_8;
	nclust=8;
	if _type_ = 'RSQ';
	keep nclust over_all;
run;
data clus_9;
	set clus_stat_9;
	nclust=9;
	if _type_ = 'RSQ';
	keep nclust over_all;
run;
data clus_10;
	set clus_stat_10;
	nclust=10;
	if _type_ = 'RSQ';
	keep nclust over_all;
run;
data clus_11;
	set clus_stat_11;
	nclust=11;
	if _type_ = 'RSQ';
	keep nclust over_all;
run;
data clus_12;
	set clus_stat_12;
	nclust=12;
	if _type_ = 'RSQ';
	keep nclust over_all;
run;

data clus_13;
	set clus_stat_13;
	nclust=13;
	if _type_ = 'RSQ';
	keep nclust over_all;
run;

data clus_14;
	set clus_stat_14;
	nclust=14;
	if _type_ = 'RSQ';
	keep nclust over_all;
run;

data all_clus;
	set clus_2 clus_3 clus_4 clus_5 clus_6 clus_7 clus_8 clus_9 clus_10 clus_11 clus_12 clus_13 clus_14;
run;

proc sgplot data = all_clus;
series x=nclust y=over_all;
run;

/* Hierarchical Clustering */
ods graphics on;
proc cluster data=new_data method=ward ccc pseudo print=5;
	var _all_;
run;
ods graphics off;

ods graphics on;
   
proc cluster plots=all;
run;
   
ods graphics off;
/* ---------------------------------------------------------------- */

proc hpclus 
	data=final_data2
	maxclusters=10
	maxiter=100
	seed=54321
	NOC = ABC(B=1 minclusters=2 align=PCA);
	score out = Outscore;
	input _all_;
	ods output ABCStats=ABC;
run;

proc sgplot
     data= ABC;
     scatter x= K y= Gap / markerattrs= (color= 'STPK' symbol= 'circleFilled');
     xaxis grid integer values= (3 to 8 by 1);
     yaxis label= 'ABC Value';
run;
/* ------------------------------------------------------------------------ */
