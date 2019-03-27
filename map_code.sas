libname intern "M:\intern";
proc import
datafile='M:\intern\AMA.xlsx'
out=intern.AMA
dbms=xlsx;
run;
*rural data;
proc import
datafile='M:\intern\2016 Non-Metro Counties List.xlsx'
out=intern.counties
dbms=xlsx;
run;
*cory's data;
proc import
datafile='M:\intern\fulldata.xlsx'
out=intern.fulldata
dbms=xlsx;
run;
*all data;
proc import
datafile='M:\intern\County Population.xlsx'
out=intern.population
dbms=xlsx;
run;
proc sort data=intern.AMA; by COUNTY; run;
proc sort data=intern.counties; by COUNTY; run;
proc sort data=intern.population; by COUNTY; run;
*all are rual;
data intern.counties_rural;
set intern.counties;
rural=1;
run;
*all counties and ama data/ratios;
proc import
datafile='M:\intern\dat_merged.xlsx'
out=intern.dat_merged
dbms=xlsx;
run;
*removing duplicates from counties;
proc sort data=intern.counties_rural
dupout=intern.counties_final
NODUPKEYS;
By CTY_FIPS;
run;
*create STNAME variable to merge by county and statename;
data intern.counties_rural;
length STNAME $20;
set intern.counties_rural;
if ST='AL' then STNAME='Alabama';
else if ST='AK' then STNAME='Alaska';
else if ST='AZ' then STNAME='Arizona';
else if ST='AR' then STNAME='Arkansas';
else if ST='CA' then STNAME='California';
else if ST='CO' then STNAME='Colorado';
else if ST='CT' then STNAME='Connecticut';
else if ST='DE' then STNAME='Delaware';
else if ST='DC' then STNAME='District of Columbia';
else if ST='FL' then STNAME='Florida';
else if ST='GA' then STNAME='Georgia';
else if ST='HI' then STNAME='Hawaii';
else if ST='ID' then STNAME='Idaho';
else if ST='IL' then STNAME='Illinois';
else if ST='IN' then STNAME='Indiana';
else if ST='IA' then STNAME='Iowa';
else if ST='KS' then STNAME='Kansas';
else if ST='KY' then STNAME='Kentucky';
else if ST='LA' then STNAME='Louisiana';
else if ST='ME' then STNAME='Maine';
else if ST='MD' then STNAME='Maryland';
else if ST='MA' then STNAME='Massachusetts';
else if ST='MI' then STNAME='Michigan';
else if ST='MN' then STNAME='Minnesota';
else if ST='MS' then STNAME='Mississippi';
else if ST='MO' then STNAME='Missouri';
else if ST='MT' then STNAME='Montana';
else if ST='NE' then STNAME='Nebraska';
else if ST='NV' then STNAME='Nevada';
else if ST='NH' then STNAME='New Hampshire';
else if ST='NJ' then STNAME='New Jersey';
else if ST='NM' then STNAME='New Mexico';
else if ST='NY' then STNAME='New York';
else if ST='NC' then STNAME='North Carolina';
else if ST='ND' then STNAME='North Dakota';
else if ST='OH' then STNAME='Ohio';
else if ST='OK' then STNAME='Oklahoma';
else if ST='OR' then STNAME='Oregon';
else if ST='PA' then STNAME='Pennsylvania';
else if ST='RI' then STNAME='Rhode Island';
else if ST='SC' then STNAME='South Carolina';
else if ST='SD' then STNAME='South Dakota';
else if ST='TN' then STNAME='Tennessee';
else if ST='TX' then STNAME='Texas';
else if ST='UT' then STNAME='Utah';
else if ST='VT' then STNAME='Vermont';
else if ST='VA' then STNAME='Virginia';
else if ST='WA' then STNAME='Washington';
else if ST='WV' then STNAME='West Virginia';
else if ST='WI' then STNAME='Wisconsin';
else if ST='WY' then STNAME='Wyoming';
else if ST='PR' then STNAME='Puerto Rico';
else if ST=. then STNAME=.;
run;
proc sort data=intern.counties_rural; by COUNTY STNAME; run;
proc sort data=intern.dat_merged; by COUNTY STNAME; run;
*merging by county but also by state;
data intern.full_merge;
merge intern.counties_rural intern.dat_merged;
by COUNTY STNAME;
run;
data intern.full_merge;
set intern.full_merge;
if rural=. then rural=0;
run;
*t test rural and ratio.addiction;
ods graphics on;
proc ttest data=intern.fulldata ci=equal umpu;
class rural;
var ratio_addiction;
run;
ods graphics off;

*rural vs child;
ods graphics on;
proc ttest data=intern.fulldata ci=equal umpu;
class rural;
var ratio_child;
run;
ods graphics off;

*rural vs ger;
ods graphics on;
proc ttest data=intern.fulldata ci=equal umpu;
class rural;
var ratio_geriatric;
run;
ods graphics off;

*rural vs all;
ods graphics on;
proc ttest data=intern.fulldata ci=equal umpu;
class rural;
var ratio_psychiatrist;
run;
ods graphics off;
proc export
data=intern.full_merge
outfile='M:\intern\fulldata.xlsx'
dbms=xlsx;
run;


*duplicates?;
data intern.new;
set intern.fulldata;
if CTY_FIPS=. then delete;
run;
proc freq data=intern.new;
tables CTY_FIPS/noprint out=intern.key;
run;
proc print data=intern.key;
where count>1;
run;
