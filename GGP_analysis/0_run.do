/******************************************************************************
 * SOME PRELIMINARY DEFINITIONS
 ******************************************************************************/


clear all
set maxvar 30000
set more off
ssc install estout

* the storage folder for RAW data (copy microdata from https://ggp-i.org here)
global raw = "81_RAW"

* the data output folder
global repo = "82_data_repo"

* create graphs are stored here
global graphs = "91_graphs"

* create regression tables are stored here
global texres = "92_texres"

* set colors
global lcolor = "gs6"
global scolor1 = "midblue"
global scolor2 = "orange_red"
global symb2 = "t"


/******************************************************************************
 * GENERATE THE DATASET NEEDED FOR THE ANALYSIS FROM RAW GGP DATA
 ******************************************************************************/

* recode, select, merge data from raw files that should be copied to 81_RAW
do "1_retrieve/1_retrieve.do"


/******************************************************************************
 * ALL TABLES AND FIGURES IN SECTION 2
 ******************************************************************************/

* Table 1
do "2_analysis/table_1.do"

* Figure 1
do "2_analysis/figure_1.do"

* Table 2
do "2_analysis/table_2.do"

* Figure 2
do "2_analysis/figure_2.do"

* Figure 3
do "2_analysis/figure_3.do"



/******************************************************************************
 * ALL TABLES AND FIGURES IN SECTION 5
 ******************************************************************************/
 
* Table 3: input data for model in Fortran format
do "3_calibration/table_3.do"

* Table 4: input data for model in Fortran format
do "3_calibration/table_4.do"

* Table 5: input data for model in Fortran format
do "3_calibration/table_5.do"

* Table 6: input data for model
do "3_calibration/table_6.do"

* Figure 5
do "3_calibration/figure_5.do"


/******************************************************************************
 * ALL TABLES AND FIGURES IN APPENDIX E
 ******************************************************************************/

* Table A.1
do "4_appendix/table_A1.do"

* Table A.2
do "4_appendix/table_A2.do"

* Table A.3
do "4_appendix/table_A3.do"

* Table A.4
do "4_appendix/table_A4.do"

* Table A.5
do "4_appendix/table_A5.do"

* Figure A.1
do "4_appendix/figure_A1.do"

* Table A.6
do "4_appendix/table_A6.do"

* Table A.7
do "4_appendix/table_A7.do"

* calculate birth rates in comparable single women dataset
do "4_appendix/compare_singles.do"
