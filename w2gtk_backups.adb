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
with Ada.Text_IO;
with Ada.Directories;            use Ada.Directories;
with GNATCOLL.Arg_Lists;         use GNATCOLL.Arg_Lists;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Expect;                use GNAT.Expect;
with GNATCOLL.Utils;             use GNATCOLL.Utils;

with W2gtk_Decls;                use W2gtk_Decls;

package body W2gtk_Backups is
   package TIO renames Ada.Text_IO;

   ---------------------
   --  Perform Patch  --
   ---------------------

   --  function Perform_Patch (The_Path          : String;
   --                          Filename_With_Ext : String) return Integer;
   --  function Perform_Patch (The_Path          : String;
   --                          Filename_With_Ext : String) return Integer is
   --     Status    : aliased Integer;
   --     Args      : Argument_List (1 .. 3);
   --  begin
   --     Args (1) := new String'("--verbose");
   --     --  Args (1) := new String'("-s");
   --     Args (2) := new String'(The_Path & "/" & Filename_With_Ext);
   --     Args (3) := new String'(Args (2).all & ".patch");
   --        Debug (-1, "patch " & Args (1).all
   --               & " " & Args (2).all
   --               & " " & Args (3).all);
   --     declare
   --        Output : constant String := Get_Command_Output
   --          (Command    => "/usr/bin/patch",
   --           Arguments  => Args,
   --           Input      => "",
   --           Status     => Status'Access,
   --           Err_To_Out => True);
   --     begin
   --        if Status /= 0 then
   --           Debug (-1, "FAILED to execute command: "
   --                  & "/usr/bin/patch "
   --                  & Argument_List_To_String (Args)
   --                  & " status " & Img (Status));
   --        else
   --           Debug (-1, Sp (3) & "patched " & Output);
   --        end if;
   --        Free (Args);
   --        return Status;
   --     exception
   --        when Invalid_Process =>
   --           Free (Args);
   --           Debug (-1, "FAILED to spawn patch");
   --           return -1;
   --     end;
   --  end Perform_Patch;

   --------------------
   --  Perform Diff  --
   --------------------

   function Perform_Diff (The_Path          : String;
                          Filename_With_Ext : String;
                          Max_Gen           : Integer) return Integer is
      Status    : aliased Integer;
      Args      : Argument_List (1 .. 2);
      PatchFile : TIO.File_Type;
   begin
      Args (1) := new String'(The_Path & "/" & Filename_With_Ext);
      Args (2) := new String'(Args (1).all & "~" & Img (Max_Gen));
      Debug (-1, "diff " & Args (1).all & " " & Args (2).all);
      declare
         Output : constant String := Get_Command_Output
           (Command    => "/usr/bin/diff",
            Arguments  => Args,
            Input      => "",
            Status     => Status'Access,
            Err_To_Out => True);
      begin
         case Status is
            when 0 =>
               Debug (-1, "  files identical, no patching");
            when 1 =>
               if Output /= "" then
                  TIO.Create (File => PatchFile,
                              Mode => TIO.Out_File,
                              Name => The_Path & "/"
                              & Filename_With_Ext & ".patch");
                  TIO.Put (PatchFile, Output);
                  TIO.Close (PatchFile);
                  --  Status := Perform_Patch (The_Path, Filename_With_Ext);
                  Debug (-1, "Patch file generated. Review the patch and use"
                         & " it to recover changes");
                  Debug (-1, "");
               end if;
            when 2 =>
               Debug (-1, "FAILED To Execute Command: "
                      & "/usr/bin/diff "
                      & Argument_List_To_String (Args)
                      & " status " & Img (Status));
            when others =>
               Debug (-1, "FAILED To Execute Command: "
                      & "/usr/bin/diff "
                      & Argument_List_To_String (Args)
                      & " status " & Img (Status));
         end case;
         Free (Args);
         return Status;
      exception
         when Invalid_Process =>
            Free (Args);
            Debug (-1, "FAILED to spawn patch");
            return -1;
      end;
   end Perform_Diff;

   -------------------
   --  Get_Max_Gen  --
   -------------------

   procedure Get_Max_Gen (Gen_Filename : String;
                          The_Max_Gen  : in out Integer) is
      Gen : Integer := The_Max_Gen - 1;
   begin
      loop
         Gen := Gen + 1;
         exit when not Exists (Gen_Filename & "~" & Img (Gen));
      end loop;
      The_Max_Gen := Integer'Max (The_Max_Gen, Gen);
   end Get_Max_Gen;

   -------------------
   --  Make_Backup  --
   -------------------

   procedure Make_Backup (Result            : out Integer;
                          Complete_Filename : String;
                          Max_Gen           : Integer;
                          Use_Log           : Boolean := True) is
   begin
      if Exists (Complete_Filename) then
         Rename (Complete_Filename,
                 Complete_Filename & "~" & Img (Max_Gen));
         if Use_Log then
            Debug (-1, Complete_Filename
                   & " renamed to "
                   & Complete_Filename & "~" & Img (Max_Gen));
         end if;
         Result := 1;
      else
         Result := 0;
      end if;
   end Make_Backup;

end W2gtk_Backups;
