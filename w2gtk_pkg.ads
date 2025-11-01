------------------------------------------------------------------------------
--                                 W 2 G T K                                --
--                                                                          --
--                     Copyright (C) 2022 Juan L. Freniche                  --
--                                                                          --
-- This program is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. It is is distributed in the hope that it will be useful,        --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License along  --
-- with this program; see the file COPYING3.                               --
-- If not, see <http://www.gnu.org/licenses/>.                             --
------------------------------------------------------------------------------
package W2gtk_Pkg is

   function Parse_VS_File (Log_Switch     : Boolean;
                           Do_Dump        : Boolean;
                           Glade_Switch   : Boolean;
                           Resx_Path      : String;
                           Resx_File_Name : String;
                           Glade_Path     : String;
                           Icon_Path      : String;
                           Ada_Path       : String) return Integer;

   function Generate_Glade_File (Glade_Path      : String;
                                 Glade_File_Name : String) return Integer;
end W2gtk_Pkg;
