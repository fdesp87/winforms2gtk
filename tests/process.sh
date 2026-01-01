#!/bin/bash

winforms2gtk="/home/jlfreniche/Projects/winforms2gtk/w2gtk"

gladepath="/home/jlfreniche/Projects/winforms2gtk/tests/glade"
iconpath="/home/jlfreniche/Projects/afpms/glade/icons/"
adapath="/home/jlfreniche/Projects/winforms2gtk/tests/ada"


function Process {
${winforms2gtk} -rp ${1} -rf ${2} \
 -glade -gp ${gladepath} -ip ${iconpath} \
 -dump -log \
 -ap ${adapath} -ada ${2}
}

set -x

Process /home/jlfreniche/Projects/fpms/fpms/ fpms

Process /home/jlfreniche/Projects/fpms/fpms/Forms About
Process /home/jlfreniche/Projects/fpms/fpms/Forms DB_Connect
Process /home/jlfreniche/Projects/fpms/fpms/Forms Operation_Report

Process /home/jlfreniche/Projects/fpms/fpms/Forms/Filters Filter_Register

Process /home/jlfreniche/Projects/fpms/fpms/Forms/New New_Asset
Process /home/jlfreniche/Projects/fpms/fpms/Forms/New New_Exch_Rate
Process /home/jlfreniche/Projects/fpms/fpms/Forms/New New_Index_Quote
Process /home/jlfreniche/Projects/fpms/fpms/Forms/New New_Periodic
Process /home/jlfreniche/Projects/fpms/fpms/Forms/New New_Quote
Process /home/jlfreniche/Projects/fpms/fpms/Forms/New New_Register_Record
Process /home/jlfreniche/Projects/fpms/fpms/Forms/New New_Transaction
Process /home/jlfreniche/Projects/fpms/fpms/Forms/New New_Transfer

Process /home/jlfreniche/Projects/fpms/fpms/Forms/Upd Upd_Asset
Process /home/jlfreniche/Projects/fpms/fpms/Forms/Upd Upd_ExchRate
Process /home/jlfreniche/Projects/fpms/fpms/Forms/Upd Upd_Index_Quote
Process /home/jlfreniche/Projects/fpms/fpms/Forms/Upd Upd_Periodic
Process /home/jlfreniche/Projects/fpms/fpms/Forms/Upd Upd_Quote
Process /home/jlfreniche/Projects/fpms/fpms/Forms/Upd Upd_Register_Record
Process /home/jlfreniche/Projects/fpms/fpms/Forms/Upd Upd_Transaction
Process /home/jlfreniche/Projects/fpms/fpms/Forms/Upd Upd_Transfer
