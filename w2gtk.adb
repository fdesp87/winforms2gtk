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
with W2Gtk2Ada;             use W2Gtk2Ada;
with Symbol_Tables;
with W2gtk_Decls;           use W2gtk_Decls;
with Ada.Text_IO;
with Ada.Command_Line;      use Ada.Command_Line;
with GNAT.Command_Line;     use GNAT.Command_Line;
with GNAT.OS_Lib;           use GNAT.OS_Lib;

procedure W2gtk is
   package TIO renames Ada.Text_IO;

   Ada_Main        : String_Access;
   Ada_Path        : String_Access;
   Icon_Path       : String_Access;
   Resx_Path       : String_Access;
   Resx_FileName   : String_Access;
   Glade_Path      : String_Access;
   Result          : Integer;
   Produce_Log     : Boolean := False;
   Produce_Dump    : Boolean := False;
   Produce_Glade   : Boolean := False;
   Produce_Ada     : Boolean := False;

   procedure Print_Help;
   procedure Print_Help is
   begin
      TIO.Put_Line ("usage: w2gtk -rp wpath -rf wfile -gp gpath -gf gfile -h --help"
                    & " -glade -dump -log -ap apath -ada main");
      TIO.Put_Line ("-h        produces this short help");
      TIO.Put_Line ("--help    produces this short help");
      TIO.Put_Line ("-rp wpath path to the windows form");
      TIO.Put_Line ("-rf wfile windows form file name with"
                    & " no extension");
      TIO.Put_Line ("-ip ipath path to icons");
      TIO.Put_Line ("-ap apath path to the generated Ada files");
      TIO.Put_Line ("-gp gpath path to the generated glade file");
      TIO.Put_Line ("-gf gfile generated glade file name with"
                    & " no extension");
      TIO.Put_Line ("-ada main generate the Ada files being <main> the Main Ada program");
      TIO.Put_Line ("-glade    generate the glade file");
      TIO.Put_Line ("-dump     generate the dump file");
      TIO.Put_Line ("-log      generate the log file");
      GNAT.OS_Lib.OS_Exit (0);
   end Print_Help;
begin

   Symbol_Tables.Initialize;

   loop
      case Getopt ("h -help rp= rf= gp= ip= ap= "
                   & "ada= log dump glade") is
         when 'h' | '-' =>
            Print_Help;
         when 'r' =>
            if Full_Switch = "rp" then
               declare
                  P : constant String := Parameter;
               begin
                  if P /= "" then
                     if P (P'Last) = '/' then
                        Resx_Path := new String'(P (P'First .. P'Last - 1));
                     else
                        Resx_Path := new String'(P);
                     end if;
                  end if;
               end;
            elsif Full_Switch = "rf" then
               if Parameter /= "" then
                  Resx_FileName := new String'(Parameter);
               end if;
            end if;
         when 'g' =>
            if Full_Switch = "gp" then
               declare
                  P : constant String := Parameter;
               begin
                  if P /= "" then
                     if P (P'Last) = '/' then
                        Glade_Path := new String'(P (P'First .. P'Last - 1));
                     else
                        Glade_Path := new String'(P);
                     end if;
                  end if;
               end;
            elsif Full_Switch = "glade" then
               Produce_Glade := True;
            end if;
         when 'd' =>
            if Full_Switch = "dump" then
               Produce_Dump := True;
            end if;
         when 'l' =>
            if Full_Switch = "log" then
               Produce_Log := True;
            end if;
         when 'i' =>
            if Full_Switch = "ip" then
               declare
                  P : constant String := Parameter;
               begin
                  if P /= "" then
                     if P (P'Last) = '/' then
                        Icon_Path := new String'(P (P'First .. P'Last - 1));
                     else
                        Icon_Path := new String'(P);
                     end if;
                  end if;
               end;
            end if;
         when 'a' =>
            if Full_Switch = "ap" then
               declare
                  P : constant String := Parameter;
               begin
                  if P /= "" then
                     if P (P'Last) = '/' then
                        Ada_Path := new String'(P (P'First .. P'Last - 1));
                     else
                        Ada_Path := new String'(P);
                     end if;
                  end if;
               end;
            elsif Full_Switch = "ada" then
               if Parameter /= "" then
                  Ada_Main := new String'(Parameter);
               end if;
               Produce_Ada := True;
            end if;
         when others =>
            exit;
      end case;
   end loop;

   if Resx_Path = null then
      TIO.Put_Line ("Wrong options: missing -rp <resource_name path>");
      GNAT.OS_Lib.OS_Exit (-1);
   end if;
   if Resx_FileName = null then
      TIO.Put_Line ("Wrong options: missing -rf <resource_name>");
      GNAT.OS_Lib.OS_Exit (-1);
   end if;
   if Produce_Glade then
      if Glade_Path = null then
         TIO.Put_Line ("Wrong options: missing -gp <glade_path>");
         GNAT.OS_Lib.OS_Exit (-1);
      end if;
   end if;
   if Produce_Ada then
      if Ada_Path = null then
         TIO.Put_Line ("Wrong options: -ap requires <ada path>");
         GNAT.OS_Lib.OS_Exit (-1);
      end if;
      if Ada_Main = null then
         TIO.Put_Line ("Wrong options: -ada requires <main Ada program>");
         GNAT.OS_Lib.OS_Exit (-1);
      end if;
   end if;
   if Produce_Dump and then Glade_Path = null then
      TIO.Put_Line ("Wrong options: -dump requires -gp");
      GNAT.OS_Lib.OS_Exit (-1);
   end if;
   if Produce_Log and then Glade_Path = null then
      TIO.Put_Line ("Wrong options: -log requires -gp");
      GNAT.OS_Lib.OS_Exit (-1);
   end if;

   if Produce_Ada then
      if Produce_Glade or Produce_Log or Produce_Dump then
         Result := Parse_VS_File (Produce_Ada,
                                  Produce_Log,
                                  Produce_Dump,
                                  Produce_Glade,
                                  Resx_Path.all,
                                  Resx_FileName.all,
                                  Glade_Path.all,
                                  Icon_Path.all,
                                  Ada_Path.all);
      else
         Result := Parse_VS_File (Produce_Ada,
                                  Produce_Log,
                                  Produce_Dump,
                                  Produce_Glade,
                                  Resx_Path.all,
                                  Resx_FileName.all,
                                  "",
                                  Icon_Path.all,
                                  Ada_Path.all);
      end if;
   else
      if Produce_Glade or Produce_Log or Produce_Dump then
         Result := Parse_VS_File (Produce_Ada,
                                  Produce_Log,
                                  Produce_Dump,
                                  Produce_Glade,
                                  Resx_Path.all,
                                  Resx_FileName.all,
                                  Glade_Path.all,
                                  Icon_Path.all,
                                  "");
      else
         TIO.Put_Line ("Nothing to produce, exiting...");
         GNAT.OS_Lib.OS_Exit (Result);
      end if;
   end if;

   if Result < 0 then
      TIO.Put_Line ("Aborting...");
      GNAT.OS_Lib.OS_Exit (Result);
   end if;

   if Result >= 0 and then Produce_Glade then
      --  Generate_Backup (KKK.Glade);
      Result := Generate_Glade_File (Glade_Path => Glade_Path.all,
                                     FileName => Resx_FileName.all);
   end if;

   if Result >= 0 and then Produce_Ada and then Ada_Path /= null then
      Result := Generate_Ada_Packages (Ada_Path   => Ada_Path.all,
                                       Glade_Path => Glade_Path.all,
                                       Filename   => Resx_FileName.all,
                                       Ada_Main   => Ada_Main.all);
   end if;

   Debug (-1, "");
   Debug (-1, "=== End of Process ===");

   Free (Ada_Path);
   Free (Resx_Path);
   Free (Icon_Path);
   Free (Resx_FileName);
   Free (Glade_Path);

   GNAT.OS_Lib.OS_Exit (Result);
exception
   when GNAT.Command_Line.Invalid_Switch
      | GNAT.Command_Line.Invalid_Parameter =>
      TIO.Put_Line ("w2gtk: unrecognized option '-"
                & Full_Switch & "'");
      TIO.Put_Line ("Try `w2gtk --help` for more information.");
      Set_Exit_Status (Failure);
end W2gtk;
