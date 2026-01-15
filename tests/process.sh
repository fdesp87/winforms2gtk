#!/bin/bash

winforms2gtk="/home/jlfreniche/Projects/winforms2gtk/w2gtk"

gladepath="/home/jlfreniche/Projects/winforms2gtk/tests/glade/"
iconpath="/home/jlfreniche/Projects/afpms/glade/icons/"
adapath="/home/jlfreniche/Projects/winforms2gtk/tests/ada/"
winbase="/home/jlfreniche/Projects/fpms/fpms/"


function Process {
${winforms2gtk} -rp ${winbase}${1} -rf ${2} \
 -glade -gp ${gladepath}${1} -ip ${iconpath} \
 -dump -log \
 -ap ${adapath}${1} -ada ${2}
}

set -x

Process . fpms

Process Forms About
Process Forms DB_Connect
Process Forms Operation_Report
Process Forms Balance
Process Forms Clone_Portfolio
Process Forms Clone_Transaction
Process Forms DBMS_Credentials
Process Forms DB_New
Process Forms MyConsole
Process Forms Splash
Process Forms View_Asset
Process Forms View_Trans

Process Forms/Filters Filter_Register

Process Forms/Filters Filter_Exchange_Rates
Process Forms/Filters Filter_Holdings
Process Forms/Filters Filter_Index_Quotes
Process Forms/Filters Filter_Periodic
Process Forms/Filters Filter_Quotes
Process Forms/Filters Filter_Transactions

Process Forms/New New_Asset
Process Forms/New New_Exch_Rate
Process Forms/New New_Index_Quote
Process Forms/New New_Periodic
Process Forms/New New_Quote
Process Forms/New New_Register_Record
Process Forms/New New_Transaction
Process Forms/New New_Transfer

Process Forms/New New_Account
Process Forms/New New_Bank
Process Forms/New New_Change_Portfolio
Process Forms/New New_Currency
Process Forms/New New_Customer
Process Forms/New New_Index
Process Forms/New New_Manager
Process Forms/New New_Portfolio
Process Forms/New New_Portfolio_in_Set
Process Forms/New New_Set

Process Forms/Upd Upd_Asset
Process Forms/Upd Upd_ExchRate
Process Forms/Upd Upd_Index_Quote
Process Forms/Upd Upd_Periodic
Process Forms/Upd Upd_Quote
Process Forms/Upd Upd_Register_Record
Process Forms/Upd Upd_Transaction
Process Forms/Upd Upd_Transfer

Process Forms/Upd Upd_Account
Process Forms/Upd Upd_Bank
Process Forms/Upd Upd_Currency
Process Forms/Upd Upd_Customer
Process Forms/Upd Upd_Index
Process Forms/Upd Upd_Manager
Process Forms/Upd Upd_Portfolio
Process Forms/Upd Upd_Portfolio_in_Set
Process Forms/Upd Upd_Set
Process Forms/Upd Delete_Transaction
Process Forms/Upd internet_parameters
Process Forms/Upd Update_Parameters
Process Forms/Upd Upd_Initfile
Process Forms/Upd Upd_TransPer

Process Forms/Charts Chart
Process Forms/Charts Chart_Full_Sto_Parameters
Process Forms/Charts Chart_Options
Process Forms/Charts PF_Distrib
Process Forms/Charts Chart_Technical_Indicators

Process Forms/Transfers Import_Historical
Process Forms/Transfers Transfer_from_CSV
Process Forms/Transfers Transfer_ISINs_Tickers
Process Forms/Transfers Transfer_to_CSV
Process Forms/Transfers Transfer_to_DB
Process Forms/Transfers Transfer_to_DB_Connect


