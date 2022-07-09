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
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the file COPYING3.  If not, see <http://www.gnu.org/licenses/>.      --
------------------------------------------------------------------------------
with W2gtk_Pkg;             use W2gtk_Pkg;
with Ada.Text_IO;
with Ada.Command_Line;      use Ada.Command_Line;
with GNAT.Command_Line;     use GNAT.Command_Line;
with GNAT.OS_Lib;           use GNAT.OS_Lib;

procedure W2gtk is
   package TIO renames Ada.Text_IO;

   Resx_Path       : String_Access;
   Resx_File_Name  : String_Access;
   Glade_Path      : String_Access;
   Glade_File_Name : String_Access;
   Result          : Integer;
   Debug           : Boolean := False;
   Dump            : Boolean := False;
   Glade           : Boolean := False;

   procedure Print_Help;
   procedure Print_Help is
   begin
      TIO.Put_Line ("w2gtk -rp wpath -rf wfile -gp gpath -gf gfile -h --help"
                    & " -glade -dump -debug");
      TIO.Put_Line ("-h        produces this short help");
      TIO.Put_Line ("--help    produces this short help");
      TIO.Put_Line ("-rp wpath indicates the windows form path");
      TIO.Put_Line ("-rf wfile indicates the windows file name with"
                    & " no extension");
      TIO.Put_Line ("-gp gpath indicates the glade path");
      TIO.Put_Line ("-gf gfile indicates the generates glade file name with"
                    & " no extension");
      TIO.Put_Line ("-glade    generate the glade file");
      TIO.Put_Line ("-dump     generate a dump file");
      TIO.Put_Line ("-debug    generate lots of debug messages");
      GNAT.OS_Lib.OS_Exit (0);
   end Print_Help;
begin
   loop
      case Getopt ("h -help rp= rf= gp= gf= debug dump glade") is
         when 'h' | '-' =>
            Print_Help;
         when 'r' =>
            if Full_Switch = "rp" then
               if Parameter /= "" then
                  Resx_Path := new String'(Parameter);
               end if;
            elsif Full_Switch = "rf" then
               if Parameter /= "" then
                  Resx_File_Name := new String'(Parameter);
               end if;
            end if;
         when 'g' =>
            if Full_Switch = "gp" then
               if Parameter /= "" then
                  Glade_Path := new String'(Parameter);
               end if;
            elsif Full_Switch = "gf" then
               if Parameter /= "" then
                  Glade_File_Name := new String'(Parameter);
               end if;
            elsif Full_Switch = "glade" then
               Glade := True;
            end if;
         when 'd' =>
            if Full_Switch = "dump" then
               Dump := True;
            elsif Full_Switch = "debug" then
               Debug := True;
            end if;
         when others =>
            exit;
      end case;
   end loop;

   if Resx_Path = null then
      TIO.Put_Line ("Wrong options: missing -rp");
      GNAT.OS_Lib.OS_Exit (-1);
   end if;
   if Resx_File_Name = null then
      TIO.Put_Line ("Wrong options: missing -rf");
      GNAT.OS_Lib.OS_Exit (-1);
   end if;
   if Glade then
      if Glade_Path = null then
         TIO.Put_Line ("Wrong options: missing -gp");
         GNAT.OS_Lib.OS_Exit (-1);
      end if;
      if Glade_File_Name = null then
         TIO.Put_Line ("Wrong options: missing -gf");
         GNAT.OS_Lib.OS_Exit (-1);
      end if;
   end if;

   Result := Parse_VS_File (Debug, Dump,
                            Resx_Path.all, Resx_File_Name.all);

   if Result /= 0 then
      TIO.Put_Line ("Aborting...");
      GNAT.OS_Lib.OS_Exit (Result);
   end if;

   if Result = 0 and then Glade then
      Result := Generate_Glade_File (Glade_Path.all, Glade_File_Name.all);
   end if;

   Free (Resx_Path);
   Free (Resx_File_Name);
   Free (Glade_Path);
   Free (Glade_File_Name);

   GNAT.OS_Lib.OS_Exit (Result);
exception
   when GNAT.Command_Line.Invalid_Switch
      | GNAT.Command_Line.Invalid_Parameter =>
      TIO.Put_Line ("w2gtk: unrecognized option '-"
                & Full_Switch & "'");
      TIO.Put_Line ("Try `w2gtk --help` for more information.");
      Set_Exit_Status (Failure);
end W2gtk;