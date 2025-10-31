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
package W2gtk_Backups is

   function Perform_Diff (The_Path          : String;
                          Filename_With_Ext : String;
                          Max_Gen           : Integer;
                          Use_Debug         : Boolean) return Integer;

   procedure Get_Max_Gen (Gen_Filename : String;
                          The_Max_Gen  : in out Integer);

   procedure Make_Backup (Result            : out Integer;
                          Complete_Filename : String;
                          Max_Gen           : Integer;
                          Use_Debug         : Boolean);
end W2gtk_Backups;
