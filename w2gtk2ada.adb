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
with W2gtk_Decls;                use W2gtk_Decls;
with Ada.Text_IO;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Directories;            use Ada.Directories;
with GNATCOLL.Arg_Lists;         use GNATCOLL.Arg_Lists;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Expect;                use GNAT.Expect;
with GNATCOLL.Utils;             use GNATCOLL.Utils;

package body W2Gtk2Ada is
   package TIO renames Ada.Text_IO;

   Max_Gen      : Integer := 1;
   AdaFile      : TIO.File_Type;
   TWin         : Window_Pointer :=  null;
   Main_Window  : Boolean := False;
   Signals : Boolean := False;
   Form_Closing : Boolean := False;

   --------------
   --  Quoted  --
   --------------

   function Quoted (M : String) return String;
   function Quoted (M : String) return String is
   begin
      return """" & M & """";
   end Quoted;

   ----------------
   --  Set_TWin  --
   ----------------

   procedure Set_TWin;
   procedure Set_TWin is
      Temp_Win : Window_Pointer := Win_List;
   begin
      while Temp_Win /= null loop
         if Temp_Win.Window_Type = GtkWindow then
            TWin := Temp_Win;
            if not TWin.Is_Dialog then
               Main_Window := True;
               exit;
            end if;
         end if;
         Temp_Win := Temp_Win.Next;
      end loop;
   end Set_TWin;

   --------------------
   --  Visit Widget  --
   --------------------

   procedure Visit_Widget
     (TWdg     : Widget_Pointer;
      Callback : access procedure (TWdg : Widget_Pointer));
   procedure Visit_Widget
     (TWdg     : Widget_Pointer;
      Callback : access procedure (TWdg : Widget_Pointer)) is
   begin
      if TWdg = null then
         return;
      end if;

      Callback (TWdg);

      Visit_Widget (TWdg.Child_List, Callback);
      Visit_Widget (TWdg.Next, Callback);
   end Visit_Widget;

   --------------------
   --  For Each Widget  --
   --------------------

   procedure For_Each_Widget
     (TWin : Window_Pointer;
      Callback : access procedure (TWdg : Widget_Pointer));
   procedure For_Each_Widget
     (TWin : Window_Pointer;
      Callback : access procedure (TWdg : Widget_Pointer)) is
      Temp_Win : Window_Pointer := TWin;
   begin
      while Temp_Win /= null loop
         if Temp_Win.Window_Type = GtkWindow then
            Visit_Widget (Temp_Win.Widget_List, Callback);
         end if;
         Temp_Win := Temp_Win.Next;
      end loop;
   end For_Each_Widget;

   ------------------------
   --  Set Have Signals  --
   ------------------------

   procedure Check_Signals (TWdg : Widget_Pointer);
   procedure Check_Signals (TWdg : Widget_Pointer) is
   begin
      if TWdg.Signal_List /= null then
         Signals := True;
      end if;
   end Check_Signals;

   procedure Set_Signals;
   procedure Set_Signals is
      Temp_Win : Window_Pointer := Win_List;
      TS       : Signal_Pointer;
   begin
      while Temp_Win /= null loop
         if Temp_Win.Signal_List /= null then
            Signals := True;
            TS := Temp_Win.Signal_List;
            while TS /= null loop
               if TS.Handler.all =
                 "On_" & Capitalize (Temp_Win.Name.all) & "_Formclosing"
               then
                  Form_Closing := True;
                  exit;
               end if;
               TS := TS.Next;
            end loop;
         end if;
         if Form_Closing then
            exit;
         end if;
         Temp_Win := Temp_Win.Next;
      end loop;
      if Signals then
         return;
      end if;
      For_Each_Widget (Win_List, Check_Signals'Access);
   end Set_Signals;

   -------------------------
   --  Emit Signal_Specs  --
   -------------------------

   procedure Emit_Signal_Specs (TWin0 : Window_Pointer);
   procedure Emit_Signal_Specs (TWin0 : Window_Pointer) is
      TS : Signal_Pointer := TWin0.Signal_List;
   begin
      while TS /= null loop
         TIO.Put_Line (Sp (3) & "procedure " & Capitalize (TS.Handler.all));
         TIO.Put_Line (Sp (5) & "(B : access Gtkada_Builder_Record'Class);");
         TIO.New_Line;
         TS := TS.Next;
      end loop;
   end Emit_Signal_Specs;

   procedure Emit_Signal_Specs (TWdg : Widget_Pointer);
   procedure Emit_Signal_Specs (TWdg : Widget_Pointer) is
      TS : Signal_Pointer := TWdg.Signal_List;
   begin
      while TS /= null loop
         if TS.Name.all = "leave" then
            TIO.Put_Line (Sp (3) & "function " & Capitalize (TS.Handler.all));
            TIO.Put_Line (Sp (5)
                          & "(B : access Gtkada_Builder_Record'Class) "
                          & "return Boolean;");
            TIO.New_Line;
         else
            TIO.Put_Line (Sp (3) & "procedure " & Capitalize (TS.Handler.all));
            TIO.Put_Line (Sp (5)
                          & "(B : access Gtkada_Builder_Record'Class);");
            TIO.New_Line;
         end if;
         TS := TS.Next;
      end loop;
   end Emit_Signal_Specs;

   --------------------------
   --  Emit Signal_Bodies  --
   --------------------------

   procedure Emit_Signal_Bodies (TWin0 : Window_Pointer);
   procedure Emit_Signal_Bodies (TWin0 : Window_Pointer) is
      TS : Signal_Pointer := TWin0.Signal_List;
   begin
      while TS /= null loop
         TIO.Put_Line (Sp (3) & "procedure " & Capitalize (TS.Handler.all));
         TIO.Put_Line (Sp (5) & "(B : access Gtkada_Builder_Record'Class)"
                       & " is");
         TIO.Put_Line (Sp (6) & "pragma Unreferenced (B);");
         TIO.Put_Line (Sp (3) & "begin");
         TIO.Put_Line (Sp (6) & "--  INSERT YOUR CODE HERE");
         if Form_Closing and then
           TS.Handler.all =
             "On_" & Capitalize (TWin0.Name.all) & "_Formclosing"
         then
            TIO.Put_Line (Sp (6) & "Gtk.Main.Main_Quit;");
         else
            TIO.Put_Line (Sp (6) & "null;");
         end if;
         TIO.Put_Line (Sp (3) & "end " & Capitalize (TS.Handler.all) & ";");
         TIO.New_Line;
         TS := TS.Next;
      end loop;
   end Emit_Signal_Bodies;

   procedure Emit_Signal_Bodies (TWdg : Widget_Pointer);
   procedure Emit_Signal_Bodies (TWdg : Widget_Pointer) is
      TS : Signal_Pointer := TWdg.Signal_List;
   begin
      while TS /= null loop
         if TS.Name.all = "leave" then
            TIO.Put_Line (Sp (3) & "function " & Capitalize (TS.Handler.all));
            TIO.Put_Line (Sp (5) & "(B : access Gtkada_Builder_Record'Class)"
                          & " return Boolean is");
            TIO.Put_Line (Sp (6) & "pragma Unreferenced (B);");
            TIO.Put_Line (Sp (3) & "begin");
            TIO.Put_Line (Sp (6) & "--  INSERT YOUR CODE HERE");
            TIO.Put_Line (Sp (6) & "Gtk.Widget.Grab_Focus");
            TIO.Put_Line (Sp (8) & "(Gtk_Widget (Me."
                          & Capitalize (TWdg.Next_Focus.Name.all)
                          & "));");
            TIO.Put_Line (Sp (6) & "return True;  --  signal processed");
            TIO.Put_Line (Sp (3) & "end " & Capitalize (TS.Handler.all) & ";");
            TIO.New_Line;
         else
            TIO.Put_Line (Sp (3) & "procedure " & Capitalize (TS.Handler.all));
            TIO.Put_Line (Sp (5) & "(B : access Gtkada_Builder_Record'Class)"
                          & " is");
            TIO.Put_Line (Sp (6) & "pragma Unreferenced (B);");
            TIO.Put_Line (Sp (3) & "begin");
            TIO.Put_Line (Sp (6) & "--  INSERT YOUR CODE HERE");
            TIO.Put_Line (Sp (6) & "null;");
            TIO.Put_Line (Sp (3) & "end " & Capitalize (TS.Handler.all) & ";");
            TIO.New_Line;
         end if;
         TS := TS.Next;
      end loop;
   end Emit_Signal_Bodies;

   --------------------
   --  Emit Signals  --
   --------------------

   procedure Emit_Signals (Filename : String);
   procedure Emit_Signals (Filename : String) is
      Temp_Win : Window_Pointer;
   begin
      TIO.Put_Line ("with Gtkada.Builder; use Gtkada.Builder;");
      TIO.New_Line;
      TIO.Put_Line ("package " & Filename & "_Pkg.Signals is");
      Temp_Win := Win_List;
      while Temp_Win /= null loop
         Emit_Signal_Specs (Temp_Win);
         Temp_Win := Temp_Win.Next;
      end loop;
      For_Each_Widget (Win_List, Emit_Signal_Specs'Access);
      TIO.Put_Line ("end " & Filename & "_Pkg.Signals;");

      if Form_Closing then
         TIO.Put_Line ("with Gtk.Main;");
      else
         TIO.Put_Line ("--  with Gtk.Main;");
      end if;
      if TWin.Window_Type = GtkWindow
        and then TWin.Is_Dialog
        and then TWin.TabFocusList /= null
      then
         TIO.Put_Line ("with " & Filename & "_Pkg.Object_Collection;");
         TIO.Put_Line ("use " & Filename & "_Pkg.Object_Collection;");
         TIO.Put_Line ("with Gtk.Widget; use Gtk.Widget;");
      end if;
      TIO.New_Line;
      TIO.Put_Line ("package body " & Filename & "_Pkg.Signals is");
      Temp_Win := Win_List;
      while Temp_Win /= null loop
         Emit_Signal_Bodies (Temp_Win);
         Temp_Win := Temp_Win.Next;
      end loop;
      For_Each_Widget (Win_List, Emit_Signal_Bodies'Access);
      TIO.Put_Line ("end " & Filename & "_Pkg.Signals;");
   end Emit_Signals;

   ---------------------------------
   --  Emit Register All Signals  --
   ---------------------------------

   procedure Emit_Register_All_Signals (TWin0 : Window_Pointer);
   procedure Emit_Register_All_Signals (TWin0 : Window_Pointer) is
      TS : Signal_Pointer;
   begin
      TS := TWin0.Signal_List;
      while TS /= null loop
         TIO.Put_Line (Sp (6) & "Register_Handler");
         TIO.Put_Line (Sp (9) & "(Builder      => Builder,");
         TIO.Put_Line (Sp (10) & "Handler_Name =>");
         TIO.Put_Line (Sp (13) & Quoted (TS.Handler.all) & ",");
         TIO.Put_Line (Sp (10) & "Handler      =>");
         TIO.Put_Line (Sp (13) & Capitalize (TS.Handler.all) & "'Access);");
         TS := TS.Next;
         TIO.New_Line;
      end loop;
   end Emit_Register_All_Signals;

   procedure Emit_Register_All_Signals (TWdg : Widget_Pointer);
   procedure Emit_Register_All_Signals (TWdg : Widget_Pointer) is
      TS : Signal_Pointer;
   begin
      TS := TWdg.Signal_List;
      while TS /= null loop
         TIO.Put_Line (Sp (6) & "Register_Handler");
         TIO.Put_Line (Sp (9) & "(Builder      => Builder,");
         TIO.Put_Line (Sp (10) & "Handler_Name =>");
         TIO.Put_Line (Sp (13) & Quoted (TS.Handler.all) & ",");
         TIO.Put_Line (Sp (10) & "Handler      =>");
         TIO.Put_Line (Sp (13) & Capitalize (TS.Handler.all) & "'Access);");
         TS := TS.Next;
         TIO.New_Line;
      end loop;
   end Emit_Register_All_Signals;

   ----------------------------
   --  Emit Register Signals  --
   ----------------------------

   procedure Emit_Register_Signals (Filename : String);
   procedure Emit_Register_Signals (Filename : String) is
      Temp_Win : Window_Pointer;
   begin
      TIO.Put_Line ("with Gtkada.Builder; use Gtkada.Builder;");
      TIO.New_Line;
      TIO.Put_Line ("package " & Filename & "_Pkg.Register_Signals is");
      TIO.Put_Line (Sp (3) & "procedure Register"
                    & " (Builder : Gtkada.Builder.Gtkada_Builder);");
      TIO.Put_Line ("end " & Filename & "_Pkg.Register_Signals;");

      TIO.Put_Line ("with " & Filename & "_Pkg.Signals; "
                    & "use " & Filename & "_Pkg.Signals;");
      TIO.New_Line;
      TIO.Put_Line ("package body " & Filename & "_Pkg.Register_Signals is");
      TIO.Put_Line (Sp (3) & "procedure Register"
                    & " (Builder : Gtkada.Builder.Gtkada_Builder) is");
      TIO.Put_Line (Sp (3) & "begin");
      Temp_Win := Win_List;
      while Temp_Win /= null loop
         Emit_Register_All_Signals (Temp_Win);
         Temp_Win := Temp_Win.Next;
      end loop;
      For_Each_Widget (Win_List, Emit_Register_All_Signals'Access);
      TIO.Put_Line (Sp (3) & "end Register;");
      TIO.Put_Line ("end " & Filename & "_Pkg.Register_Signals;");
   end Emit_Register_Signals;

   ------------------------
   --  Emit Main Window  --
   ------------------------

   procedure Emit_Main_Window (Filename   : String;
                               Ada_Path   : String;
                               Glade_Path : String);
   procedure Emit_Main_Window (Filename   : String;
                               Ada_Path   : String;
                               Glade_Path : String) is
   begin
      if Main_Window then
         TIO.Put_Line ("package " & Filename & "_Pkg.Main_Windows is");
         TIO.Put_Line (Sp (3) & "procedure Initialize;");
         TIO.Put_Line ("end " & Filename & "_Pkg.Main_Windows;");
      else
         TIO.Put_Line ("with Gtk.Window; use Gtk.Window;");
         TIO.Put_Line ("package " & Filename & "_Pkg.Main_Windows is");
         TIO.Put_Line (Sp (3) & "procedure Initialize "
                       & "(Parent : access Gtk_Window_Record'Class);");
         TIO.Put_Line ("end " & Filename & "_Pkg.Main_Windows;");
      end if;

      TIO.Put_Line ("with " & Filename & "_Pkg.Object_Collection; "
                    & "use " & Filename & "_Pkg.Object_Collection;");
      if Signals then
         TIO.Put_Line ("with " & Filename & "_Pkg.Register_Signals;");
      end if;
      TIO.Put_Line ("with Ada.Text_IO; use Ada.Text_IO;");
      TIO.Put_Line ("with Gtk.Main;");
      if Main_Window then
         TIO.Put_Line ("with Gtk.Style_Provider; use Gtk.Style_Provider;");
         TIO.Put_Line ("with Gtkada.Style; use Gtkada.Style;");
      end if;
      TIO.Put_Line ("with Gtk.Widget; use Gtk.Widget;");
      TIO.Put_Line ("with Gtkada.Builder; use Gtkada.Builder;");
      TIO.Put_Line ("with Gtk.Dialog; use Gtk.Dialog;");
      TIO.Put_Line ("with Gtkada.Dialogs; use Gtkada.Dialogs;");
      TIO.Put_Line ("with Glib;");
      TIO.Put_Line ("with Glib.Object; use Glib.Object;");
      TIO.Put_Line ("with Glib.Error; use Glib.Error;");
      TIO.New_Line;
      TIO.Put_Line ("package body " & Filename & "_Pkg.Main_Windows is");
      TIO.New_Line;
      TIO.Put_Line (Sp (3) & "Glade_Filename : constant String :=");
      TIO.Put_Line (Sp (6)
                    & Quoted (Glade_Path & "/" & TWin.Name.all  & ".glade")
                    & ";");
      TIO.New_Line;
      TIO.Put_Line (Sp (3) & "procedure Report_Glade_Error (Err : GError);");
      TIO.Put_Line (Sp (3) & "procedure Report_Glade_Error (Err : GError) is");
      TIO.Put_Line (Sp (6) & "Dialog : Gtk_Dialog;");
      TIO.Put_Line (Sp (6) & "Resp   : Gtk_Response_Type;");
      TIO.Put_Line (Sp (3) & "begin");
      TIO.Put_Line (Sp (6) & "Dialog := Create_Gtk_Dialog");
      TIO.Put_Line (Sp (8) & "(Msg         => "
                    & Quoted ("Error [Create_Builder]: ")
                    & " & Get_Message (Err),");
      TIO.Put_Line (Sp (8) & " Dialog_Type => Error,");
      TIO.Put_Line (Sp (8) & " Title       => "
                    & Quoted ("Glade Error") & ");");
      TIO.Put_Line (Sp (6) & "Dialog.Show_All;");
      TIO.Put_Line (Sp (6) & "Resp := Dialog.Run;");
      TIO.Put_Line (Sp (6) & "Dialog.Destroy;");
      TIO.Put_Line (Sp (3) & "end Report_Glade_Error;");
      TIO.New_Line;
      if Main_Window then
         TIO.Put_Line (Sp (3) & "procedure Initialize is");
      else
         TIO.Put_Line (Sp (3) & "procedure Initialize "
                       & "(Parent : access Gtk_Window_Record'Class) is");
      end if;
      TIO.Put_Line (Sp (6) & "Error : aliased GError;");
      TIO.Put_Line (Sp (6) & "use Glib;");
      TIO.Put_Line (Sp (3) & "begin");
      TIO.New_Line;
      TIO.Put_Line (Sp (6) & "--  create a builder");
      TIO.Put_Line (Sp (6) & "if Builder /= null then");
      TIO.Put_Line (Sp (9) & "Unref (Builder);");
      TIO.Put_Line (Sp (6) & "end if;");

      TIO.Put_Line (Sp (6) & "Gtk_New (Builder);");
      TIO.New_Line;
      TIO.Put_Line (Sp (6) & "--  read the glade");
      TIO.Put_Line (Sp (6) & "if Add_From_File (Builder, "
                    & "Glade_Filename, "
                    & "Error'Access) = 0 then");
      TIO.Put_Line (Sp (9) & "Put_Line (""Error [Create_Builder]: "" & "
                    & "Get_Message (Error));");
      TIO.Put_Line (Sp (9) & "Report_Glade_Error (Error);");
      TIO.Put_Line (Sp (9) & "Error_Free (Error);");
      TIO.Put_Line (Sp (9) & "Gtk.Main.Main_Quit;");
      TIO.Put_Line (Sp (6) & "end if;");
      TIO.New_Line;
      if Main_Window then
         TIO.Put_Line (Sp (6) & "--  load the css");
         TIO.Put_Line (Sp (6) & "Load_Css_File");
         TIO.Put_Line (Sp (8)
                       & "("
                       & Quoted (Ada_Path & "/" & To_Lower (Filename)
                         & ".css") & ",");
         TIO.Put_Line (Sp (9) & "Ada.Text_IO.Put_Line'Access,");
         TIO.Put_Line (Sp (9)
                       & "Priority_User);"
                       & " --  Priority_User is important!!!");
         TIO.New_Line;
      end if;
      TIO.Put_Line (Sp (6) & "--  Initialize Widgets_Collection");
      TIO.Put_Line (Sp (6) & "Me := "
                    & Filename & "_Pkg.Object_Collection."
                    & "New_Widget_Collection;");
      if Signals then
         TIO.New_Line;
         TIO.Put_Line (Sp (6) & "--  Register signal handlers");
         TIO.Put_Line (Sp (6) & "Register_Signals.Register (Builder);");
      end if;
      TIO.Put_Line (Sp (6) & "Builder.Do_Connect;");
      TIO.New_Line;
      TIO.Put_Line (Sp (6) & "--  set initial data");
      TIO.Put_Line (Sp (6) & "--     INSERT YOUR INITIAL DATA");
      TIO.New_Line;
      TIO.Put_Line (Sp (6) & "--  display");
      if not Main_Window then
         TIO.Put_Line (Sp (6) & "Gtk_Window");
         TIO.Put_Line (Sp (8) & "(Builder.Get_Object ("
                       & Quoted (TWin.Name.all)
                       & ")).Set_Transient_For (Parent);");
      end if;
      TIO.Put_Line (Sp (6) & "Gtk_Widget");
      TIO.Put_Line (Sp (8) & "(Builder.Get_Object ("
                    & Quoted (TWin.Name.all)
                    & ")).Show_All;");
      TIO.Put_Line (Sp (3) & "end Initialize;");
      TIO.Put_Line ("end " & Filename & "_Pkg.Main_Windows;");
   end Emit_Main_Window;

   ------------------------------
   --  Emit Object Collection  --
   ------------------------------

   procedure Initialize_Object (TWdg : Widget_Pointer);
   procedure Initialize_Object (TWdg : Widget_Pointer) is
   begin
      case TWdg.Widget_Type is
         when BackgroundWorker => null;
         when PrintDocument => null;
         when PrintDialog => null;
         when Chart => null;
         when FolderBrowserDialog => null;
         when PageSetupDialog => null;
         when others =>
            TIO.Put_Line (Sp (6) & "OC." & Capitalize (TWdg.Name.all) & " :=");
            TIO.Put_Line (Sp (8) & To_Gtk (TWdg)
                          & " (Builder.Get_Object (");
            TIO.Put_Line (Sp (8) & Quoted (TWdg.Name.all) & "));");
      end case;
   end Initialize_Object;

   procedure Emit_Object (TWdg : Widget_Pointer);
   procedure Emit_Object (TWdg : Widget_Pointer) is
   begin
      TIO.Put_Line (Sp (6) & Capitalize (TWdg.Name.all) & " : "
                    & To_Gtk (TWdg) & ";");
   end Emit_Object;

   procedure Emit_Object_Collection (Filename : String);
   procedure Emit_Object_Collection (Filename : String) is
      Temp_Win : Window_Pointer := Win_List;
   begin
      if Main_Window then
         TIO.Put_Line ("with Gtk.Window; use Gtk.Window;");
      else
         TIO.Put_Line ("with Gtk.Dialog; use Gtk.Dialog;");
      end if;
      if Have.ListStores > 0 then
         TIO.Put_Line ("with Gtk.List_Store; use Gtk.List_Store;");
      end if;
      if Have.FileFilters > 0 then
         TIO.Put_Line ("with Gtk.File_Filter; use Gtk.File_Filter;");
      end if;
      if Have.Filechooserdialogs > 0 then
         TIO.Put_Line ("with Gtk.File_Chooser_Dialog; "
                       & "use Gtk.File_Chooser_Dialog;");
      end if;
      if Have.Entrybuffers > 0 then
         TIO.Put_Line ("with Gtk.Entry_Buffer; use Gtk.Entry_Buffer;");
      end if;
      if Have.FileChooserButtons > 0 then
         TIO.Put_Line ("with Gtk.File_Chooser_Button; "
                       & "use Gtk.File_Chooser_Button;");
      end if;
      if Have.Images > 0 then
         TIO.Put_Line ("with Gtk.Image; use Gtk.Image;");
      end if;
      if Have.Buttons > 0 then
         TIO.Put_Line ("with Gtk.Button; use Gtk.Button;");
      end if;
      if Have.Labels > 0 then
         TIO.Put_Line ("with Gtk.Label; use Gtk.Label;");
      end if;
      if Have.Menus > 0 then
         TIO.Put_Line ("with Gtk.Menu_Bar; use Gtk.Menu_Bar;");
      end if;
      if Have.MenuImageItems > 0 then
         TIO.Put_Line ("with Gtk.Image_Menu_Item; use Gtk.Image_Menu_Item;");
      end if;
      if Have.MenuSeparators > 0 then
         TIO.Put_Line ("with Gtk.Separator_Menu_Item; "
                       & "use Gtk.Separator_Menu_Item;");
      end if;
      if Have.Toolbars > 0 then
         TIO.Put_Line ("with Gtk.Toolbar; use Gtk.Toolbar;");
      end if;
      if Have.ToolSeparators > 0 then
         TIO.Put_Line ("with Gtk.Separator_Tool_Item; "
                       & "use Gtk.Separator_Tool_Item;");
      end if;
      if Have.Notebooks > 0 then
         TIO.Put_Line ("with Gtk.Notebook; use Gtk.Notebook;");
      end if;
      if Have.TreeStores > 0 then
         TIO.Put_Line ("with Gtk.Tree_Store; use Gtk.Tree_Store;");
      end if;
      if Have.TreeViews > 0 then
         TIO.Put_Line ("with Gtk.Tree_View; use Gtk.Tree_View;");
      end if;
      if Have.TreeViewColumns > 0 then
         TIO.Put_Line ("with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;");
      end if;
      if Have.Entries > 0 then
         TIO.Put_Line ("with Gtk.GEntry; use Gtk.GEntry;");
      end if;
      if Have.ComboBoxes > 0 then
         TIO.Put_Line ("with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;");
      end if;
      if Have.Boxes > 0 then
         TIO.Put_Line ("with Gtk.Box; use Gtk.Box;");
      end if;
      TIO.Put_Line ("with Glib;");
      TIO.Put_Line ("with Glib.Object; use Glib.Object;");
      TIO.New_Line;
      TIO.Put_Line ("package " & Filename & "_Pkg.Object_Collection is");
      TIO.Put_Line (Sp (3) & "type Widget_Collection_Record is new "
                    & "Glib.Object.GObject_Record with record");
      while Temp_Win /= null loop
         TIO.Put_Line (Sp (6) & Capitalize (Temp_Win.Name.all) & " : "
                       & To_Gtk (Temp_Win) & ";");
         Temp_Win := Temp_Win.Next;
      end loop;
      For_Each_Widget (Win_List, Emit_Object'Access);
      TIO.Put_Line (Sp (3) & "end record;");
      TIO.Put_Line (Sp (3) & "type Widget_Collection is access all "
                    & "Widget_Collection_Record'Class;");
      TIO.New_Line;
      TIO.Put_Line (Sp (3) & "procedure New_Widget_Collection ("
                    & "OC : out Widget_Collection);");
      TIO.Put_Line (Sp (3) & "procedure Initialize ("
                    & "OC : not null access Widget_Collection_Record'Class);");
      TIO.Put_Line (Sp (3) & "function New_Widget_Collection return "
                    & "Widget_Collection;");
      TIO.New_Line;
      TIO.Put_Line (Sp (3) & "Me : Widget_Collection;");
      TIO.Put_Line ("end " & Filename & "_Pkg.Object_Collection;");

      TIO.Put_Line ("package body " & Filename & "_Pkg.Object_Collection is");
      TIO.New_Line;
      TIO.Put_Line (Sp (3) & "procedure New_Widget_Collection ("
                    & "OC : out Widget_Collection) is");
      TIO.Put_Line (Sp (3) & "begin");
      TIO.Put_Line (Sp (6) & "OC := new Widget_Collection_Record;");
      TIO.Put_Line (Sp (6) & Filename & "_Pkg.Object_Collection."
                    & "Initialize (OC);");
      TIO.Put_Line (Sp (3) & "end New_Widget_Collection;");
      TIO.New_Line;
      TIO.Put_Line (Sp (3) & "procedure Initialize ("
                    & "OC : not null access Widget_Collection_Record'Class)");
      TIO.Put_Line (Sp (3) & " is");
      TIO.Put_Line (Sp (3) & "begin");
      if Win_List = null then
         TIO.Put_Line (Sp (6) & "null;");
      else
         Temp_Win := Win_List;
         while Temp_Win /= null loop
            TIO.Put_Line (Sp (6) & "OC."
                          & Capitalize (Temp_Win.Name.all) & " :=");
            TIO.Put_Line (Sp (8) & To_Gtk (Temp_Win)
                          & " (Builder.Get_Object (");
            TIO.Put_Line (Sp (8) & Quoted (Temp_Win.Name.all) & "));");
            Temp_Win := Temp_Win.Next;
         end loop;
         For_Each_Widget (Win_List, Initialize_Object'Access);
      end if;
      TIO.Put_Line (Sp (3) & "end Initialize;");
      TIO.New_Line;
      TIO.Put_Line (Sp (3) & "function New_Widget_Collection return "
                    & "Widget_Collection is");
      TIO.Put_Line (Sp (6) & "OC : constant Widget_Collection := "
                   & "new Widget_Collection_Record;");
      TIO.Put_Line (Sp (3) & "begin");
      TIO.Put_Line (Sp (6) & Filename & "_Pkg.Object_Collection."
                    & "Initialize (OC);");
      TIO.Put_Line (Sp (6) & "return OC;");
      TIO.Put_Line (Sp (3) & "end New_Widget_Collection;");
      TIO.Put_Line ("end " & Filename & "_Pkg.Object_Collection;");
   end Emit_Object_Collection;

   ---------------------------
   --  Emit TreeStore_Enum  --
   ---------------------------
   procedure Emit_TreeStores_Enum (Filename : String);
   procedure Emit_TreeStores_Enum (Filename : String) is
      Temp_Win : Window_Pointer := Win_List;
      Col      : Widget_Pointer;
      TWdg     : Widget_Pointer;
   begin
      TIO.Put_Line ("with Glib; use Glib;");
      TIO.Put_Line ("package " & Filename & "_Pkg.Tree_Stores_Enum is");
      while Temp_Win /= null loop
         if Temp_Win.Window_Type = GtkTreeStore then
            TWdg := Temp_Win.Associated_Widget;
            Col := TWdg.Child_List;
            if Col /= null then
               TIO.New_Line;
               TIO.Put_Line (Sp (3) & "type "
                             & Capitalize (TWdg.Name.all) & "_Enum" & " is");
               TIO.Put (Sp (5) & "(" & Capitalize (Col.Name.all));
               Col := Col.Next;
            end if;
            while Col /= null loop
               TIO.Put_Line (",");
               TIO.Put (Sp (6) & Capitalize (Col.Name.all));
               Col := Col.Next;
            end loop;
            if TWdg.Child_List /= null then
               Col := TWdg.Child_List;
               while Col /= null loop
                  if Col.Widget_Type = DataGridViewCheckBoxColumn then
                     TIO.Put_Line (",");
                     TIO.Put (Sp (6)
                              & Capitalize (Col.Name.all) & "_Data");
                     if not Col.ReadOnly then
                        TIO.Put_Line (",");
                        TIO.Put (Sp (6)
                                 & Capitalize (Col.Name.all) & "_Activatable");
                     end if;
                  end if;
                  Col := Col.Next;
               end loop;
               TIO.Put_Line (");");
               TIO.New_Line;
               TIO.Put_Line (Sp (3) & "function ""+"" (X : "
                             & Capitalize (TWdg.Name.all) & "_Enum" & ")");
               TIO.Put_Line (Sp (16) & "return Gint is");
               TIO.Put_Line (Sp (5) & "("
                             & Capitalize (TWdg.Name.all) & "_Enum"
                             & "'Pos (X));");
            end if;
         end if;
         Temp_Win := Temp_Win.Next;
      end loop;
      TIO.Put_Line ("end " & Filename & "_Pkg.Tree_Stores_Enum;");
   end Emit_TreeStores_Enum;

   -------------------------
   --  Emit Main Program  --
   -------------------------

   procedure Emit_Main_Package (Filename : String);
   procedure Emit_Main_Package (Filename : String) is
   begin
      TIO.Put_Line ("with Gtkada.Builder; use Gtkada.Builder;");
      TIO.New_Line;
      TIO.Put_Line ("package " & Filename & "_Pkg is");
      TIO.Put_Line (Sp (3) & "Builder : Gtkada_Builder;");
      TIO.Put_Line ("end " & Filename & "_Pkg;");
   end Emit_Main_Package;

   -------------------------
   --  Emit Main Program  --
   -------------------------

   procedure Emit_Main_Program (Filename : String);
   procedure Emit_Main_Program (Filename : String) is
   begin
      TIO.Put_Line ("with Gtk.Main;");
      TIO.Put_Line ("with " & Filename & "_Pkg.Main_Windows;");
      TIO.Put_Line ("procedure " & Filename & " is");
      TIO.Put_Line ("begin");
      TIO.Put_Line (Sp (3) & "Gtk.Main.Init;");
      TIO.Put_Line (Sp (3) & Filename & "_Pkg.Main_Windows.Initialize;");
      TIO.Put_Line (Sp (3) & "Gtk.Main.Main;");
      TIO.Put_Line ("end " & Filename & ";");
   end Emit_Main_Program;

   ----------------
   --  Emit GPR  --
   ----------------

   function Emit_GPR (Ada_Path : String;
                      Filename : String;
                      Debug    : Boolean) return Integer;
   function Emit_GPR (Ada_Path : String;
                      Filename : String;
                      Debug    : Boolean) return Integer is
      pragma Unreferenced (Debug);
      GPRFile : TIO.File_Type;
   begin
      TIO.Create (File => GPRFile,
                  Mode => TIO.Out_File,
                  Name => Ada_Path & "/" & Filename & ".gpr");

      TIO.Put_Line (GPRFile, "with ""gnatcoll"";");
      TIO.Put_Line (GPRFile, "with ""gtkada"";");
      TIO.Put_Line (GPRFile, "project "
                    & Filename
                    & " is");
      TIO.Put_Line (GPRFile, "   for Source_Dirs use ("".""); "
                    & "-- source files");
      TIO.Put_Line (GPRFile, "   for Object_Dir use ""./obj"";");
      TIO.Put_Line (GPRFile, "   for Main use ("""
                    & Filename
                    & ".adb"");");
      TIO.Put_Line (GPRFile, "   for Exec_Dir use "".."";");
      TIO.Put_Line (GPRFile, "   package Compiler is");
      TIO.Put_Line (GPRFile, "      for Switches (""Ada"") use");
      TIO.Put_Line (GPRFile, "        (""-g"","
                    & "             --  debug");
      TIO.Put_Line (GPRFile, "        (""-gnat2022"","
                    & "             --  Ada 2022");
      TIO.Put_Line (GPRFile, "         ""-O0"","
                    & "            --  optimization level 0");
      TIO.Put_Line (GPRFile, "         ""-fstack-check"","
                    & "  --  stact check");
      TIO.Put_Line (GPRFile, "         ""-gnata"","
                    & "         --  enable assettions");
      TIO.Put_Line (GPRFile, "         ""-gnateE"","
                    & "        --  generate extra info in exception msgs");
      TIO.Put_Line (GPRFile, "         ""-gnatQ"","
                    & "         --  generate ALI even if illegalities");
      TIO.Put_Line (GPRFile, "         ""-gnatVa"","
                    & "        --  validity checking all");
      TIO.Put_Line (GPRFile, "         ""-gnaty"","
                    & "         --  enable built-in style checks");
      TIO.Put_Line (GPRFile, "         ""-gnaty3abcefhiklM79nprst"","
                    & " --  many style checks");
      TIO.Put_Line (GPRFile, "--         ""-gnatwe"","
                    & "        --  warnings and style checks are errors");
      TIO.Put_Line (GPRFile, "         ""-gnatwa"","
                    & "        --  all optional warnings");
      TIO.Put_Line (GPRFile, "         ""-gnatwC"","
                    & "        --  Suppress warnings on conditionals");
      TIO.Put_Line (GPRFile, "         ""-gnatwJ"","
                    & "        --  Disable warnings on obsolescent features");
      TIO.Put_Line (GPRFile, "         ""-gnatwM"""
                    & "         --  Disable warnings on modified but "
                    & "unreferenced variables");
      TIO.Put_Line (GPRFile, "        );");
      TIO.Put_Line (GPRFile, "   end Compiler;");
      TIO.Put_Line (GPRFile, "   package Binder is");
      TIO.Put_Line (GPRFile, "      for Switches (""Ada"") use (""-E"");");
      TIO.Put_Line (GPRFile, "   end Binder;");
      TIO.Put_Line (GPRFile, "end "
                    & Filename
                    & ";");

      TIO.Close (GPRFile);
      return 0;
   end Emit_GPR;

   ----------------
   --  Emit CSS  --
   ----------------

   function Emit_CSS (Ada_Path : String;
                      Filename : String;
                      Debug    : Boolean) return Integer;
   function Emit_CSS (Ada_Path : String;
                      Filename : String;
                      Debug    : Boolean) return Integer is
      pragma Unreferenced (Debug);
      CSSFile : TIO.File_Type;
   begin
      TIO.Create (File => CSSFile,
                  Mode => TIO.Out_File,
                  Name => Ada_Path & "/" & Filename & ".css");

      TIO.Put_Line (CSSFile, "/* Colors */");
      TIO.Put_Line (CSSFile, "");
      TIO.Put_Line (CSSFile, "@define-color bg_all        #CECECE;"
                    & " /* bg */");
      TIO.Put_Line (CSSFile, "@define-color bg_tab        #F8F8F8;"
                    & " /* notebook tab */");
      TIO.Put_Line (CSSFile, "@define-color bg_sel_tab    #A3B8CB;"
                    & " /* notebook selected tab */");
      TIO.Put_Line (CSSFile, "@define-color bg_hd         #EAEBEF;"
                    & " /* treeview header */");
      TIO.Put_Line (CSSFile, "@define-color bg_hover_hd   #F8F8F8;"
                    & " /* treeview header hovered */");
      TIO.Put_Line (CSSFile, "@define-color bg_tv_hd_ldg  #96B183;"
                    & " /* treeview header ledger */");
      TIO.Put_Line (CSSFile, "@define-color bg_even_row   #EEEEEE;"
                    & " /* treeview even row */");
      TIO.Put_Line (CSSFile, "@define-color bg_sel_row    #FFEF98;"
                    & " /* treeview selected row */");
      TIO.New_Line (CSSFile);
      TIO.Put_Line (CSSFile, "/* GEntry */");
      TIO.Put_Line (CSSFile, "/*entry {");
      TIO.Put_Line (CSSFile, "    min-height: 0px;");
      TIO.Put_Line (CSSFile, "}*/");
      TIO.Put_Line (CSSFile, "/* Scrollbar */");
      TIO.Put_Line (CSSFile, ".scrollbar {");
      TIO.Put_Line (CSSFile, "background-clip: padding-box;");
      TIO.Put_Line (CSSFile, "-GtkScrollbar-has-backward-stepper: 1;");
      TIO.Put_Line (CSSFile, "-GtkScrollbar-has-forward-stepper: 1;");
      TIO.Put_Line (CSSFile, "}");
      TIO.Put_Line (CSSFile, "/* Tooltip */");
      TIO.Put_Line (CSSFile, "tooltip, .tooltip {");
      TIO.Put_Line (CSSFile, "    background-color: black;");
      TIO.Put_Line (CSSFile, "    color: white;");
      TIO.Put_Line (CSSFile, "}");
      TIO.Put_Line (CSSFile, "/* Menubar and Toolbar */");
      TIO.Put_Line (CSSFile, "menubar, toolbar {");
      TIO.Put_Line (CSSFile, "    border-style: solid;");
      TIO.Put_Line (CSSFile, "    border-color: @bg_all;");
      TIO.Put_Line (CSSFile, "    border-bottom-width: 0.2px;");
      TIO.Put_Line (CSSFile, "    background-image: -gtk-gradient(linear,");
      TIO.Put_Line (CSSFile, "               left top, left bottom,");
      TIO.Put_Line (CSSFile, "               from (@bg_all), to (@bg_all));");
      TIO.Put_Line (CSSFile, "}");
      TIO.Put_Line (CSSFile, "/* GtkMenuItem */");
      TIO.Put_Line (CSSFile, "GtkMenuItem {");
      TIO.Put_Line (CSSFile, "    padding-top: 2px;");
      TIO.Put_Line (CSSFile, "    padding-bottom: 2px;");
      TIO.Put_Line (CSSFile, "}");
      TIO.Put_Line (CSSFile, "/* GtkNotebook */");
      TIO.Put_Line (CSSFile, "GtkNotebook {");
      TIO.Put_Line (CSSFile, "    background-image: -gtk-gradient(linear,");
      TIO.Put_Line (CSSFile, "               left top, left bottom,");
      TIO.Put_Line (CSSFile, "               from (@bg_tab), to (@bg_tab));");
      TIO.Put_Line (CSSFile, "}");
      TIO.Put_Line (CSSFile, "GtkNotebook tab {");
      TIO.Put_Line (CSSFile, "    border-style: solid;");
      TIO.Put_Line (CSSFile, "    border-left-width: 0.4px;");
      TIO.Put_Line (CSSFile, "    border-right-width: 0.4px;");
      TIO.Put_Line (CSSFile, "    border-radius: 5px;");
      TIO.Put_Line (CSSFile, "    padding: 0px 0px 0px 0px;");
      TIO.Put_Line (CSSFile, "    background-image: -gtk-gradient(linear,");
      TIO.Put_Line (CSSFile, "               left top, left bottom,");
      TIO.Put_Line (CSSFile, "               from (@bg_tab), to (@bg_tab));");
      TIO.Put_Line (CSSFile, "}");
      TIO.Put_Line (CSSFile, "GtkNotebook tab:active {");
      TIO.Put_Line (CSSFile, "    background-image: -gtk-gradient(linear,");
      TIO.Put_Line (CSSFile, "        left top, left bottom,");
      TIO.Put_Line (CSSFile, "        from (@bg_sel_tab), to (@bg_sel_tab));");
      TIO.Put_Line (CSSFile, "}");
      TIO.Put_Line (CSSFile, "GtkNotebook tab .button{");
      TIO.Put_Line (CSSFile, "    border-style: solid;");
      TIO.Put_Line (CSSFile, "    border-color: black;");
      TIO.Put_Line (CSSFile, "    border-width: 1px;");
      TIO.Put_Line (CSSFile, "    background-image: -gtk-gradient(linear,");
      TIO.Put_Line (CSSFile, "               left top, left bottom,");
      TIO.Put_Line (CSSFile, "               from (@bg_tab), to (@bg_tab));");
      TIO.Put_Line (CSSFile, "}");
      TIO.Put_Line (CSSFile, "/* GtkTreeView */");
      TIO.Put_Line (CSSFile, "GtkTreeView {");
      TIO.Put_Line (CSSFile, "    -GtkTreeView-vertical-separator: 0px;");
      TIO.Put_Line (CSSFile, "}");
      TIO.Put_Line (CSSFile, "GtkTreeView column-header .button {");
      TIO.Put_Line (CSSFile, "    background-image: -gtk-gradient(linear,");
      TIO.Put_Line (CSSFile, "               left top, left bottom,");
      TIO.Put_Line (CSSFile, "               from (@bg_hd), to (@bg_hd));");
      TIO.Put_Line (CSSFile, "    border-style: solid;");
      TIO.Put_Line (CSSFile, "    border-color: black;");
      TIO.Put_Line (CSSFile, "    border-top-width: 0.2px;");
      TIO.Put_Line (CSSFile, "    border-left-width: 0.4px;");
      TIO.Put_Line (CSSFile, "    border-bottom-width: 0.4px;");
      TIO.Put_Line (CSSFile, "    padding: 0px 0px 0px 0px;");
      TIO.Put_Line (CSSFile, "}");
      TIO.Put_Line (CSSFile, "GtkTreeView column-header .button:hover {");
      TIO.Put_Line (CSSFile, "     background-image: -gtk-gradient(linear,");
      TIO.Put_Line (CSSFile, "      left top, left bottom,");
      TIO.Put_Line (CSSFile, "      from (@bg_hover_hd), to (@bg_hover_hd));");
      TIO.Put_Line (CSSFile, "}");
      TIO.Put_Line (CSSFile, "GtkTreeView row:nth-child(even) {");
      TIO.Put_Line (CSSFile, "    background-color: @bg_even_row;");
      TIO.Put_Line (CSSFile, "}");
      TIO.Put_Line (CSSFile, "GtkTreeView row:nth-child(odd) {");
      TIO.Put_Line (CSSFile, "    background-color: white;");
      TIO.Put_Line (CSSFile, "}");
      TIO.Put_Line (CSSFile, "* {");
      TIO.Put_Line (CSSFile, "    -GtkTreeView-grid-line-pattern: ""\0\0"";");
      TIO.Put_Line (CSSFile, "    -GtkTreeView-grid-line-width: 1;");
      TIO.Put_Line (CSSFile, "}");
      TIO.Put_Line (CSSFile, "GtkTreeView row:selected {");
      TIO.Put_Line (CSSFile, "    background-color: @bg_sel_row;");
      TIO.Put_Line (CSSFile, "}");
      TIO.New_Line (CSSFile);

      TIO.Close (CSSFile);
      return 0;
   end Emit_CSS;

   --------------------
   --  Perform_Chop  --
   --------------------

   function Perform_Chop (Ada_Path : String;
                          Filename : String;
                          Debug    : Boolean) return Integer;
   function Perform_Chop (Ada_Path : String;
                          Filename : String;
                          Debug    : Boolean) return Integer is
      Status : aliased Integer;
      Args   : Argument_List (1 .. 4);
   begin
      if Debug then
         Args (1) := new String'("-v");
      else
         Args (1) := new String'("-q");
      end if;
      Args (2) := new String'("-w");
      Args (3) := new String'(Ada_Path & "/" & Filename & ".TempAda");
      Args (4) := new String'(Ada_Path);
      declare
         Output : constant String := Get_Command_Output
           (Command    => "/opt/GNAT/2021/bin/gnatchop",
            Arguments  => Args,
            Input      => "",
            Status     => Status'Access,
            Err_To_Out => True);
      begin
         if Output /= "" then
            TIO.Put_Line (Output);
         end if;

         if Status /= 0 then
            TIO.Put_Line ("FAILED to execute command: "
                          & "gnatchop "
                          & Argument_List_To_String (Args));
         end if;
         Free (Args);
         return Status;
      exception
         when Invalid_Process =>
            Free (Args);
            TIO.Put_Line ("FAILED to spawn gnatchop");
            return -1;
      end;
   end Perform_Chop;

   ---------------------
   --  Perform Patch  --
   ---------------------

   function Perform_Patch (Ada_Path          : String;
                           Filename_With_Ext : String;
                           Debug             : Boolean) return Integer;
   function Perform_Patch (Ada_Path          : String;
                           Filename_With_Ext : String;
                           Debug             : Boolean) return Integer is
      Status    : aliased Integer;
      Args      : Argument_List (1 .. 3);
   begin
      if Debug then
         Args (1) := new String'("--verbose");
      else
         Args (1) := new String'("-s");
      end if;
      Args (2) := new String'(Ada_Path & "/" & Filename_With_Ext);
      Args (3) := new String'(Args (2).all & ".patch");
      if Debug then
         TIO.Put_Line ("patch " & Args (1).all
                       & " " & Args (2).all
                       & " " & Args (3).all);
      end if;
      declare
         Output : constant String := Get_Command_Output
           (Command    => "/usr/bin/patch",
            Arguments  => Args,
            Input      => "",
            Status     => Status'Access,
            Err_To_Out => True);
      begin
         if Status /= 0 then
            TIO.Put_Line ("FAILED to execute command: "
                          & "/usr/bin/patch "
                          & Argument_List_To_String (Args)
                          & " status " & Img (Status));
         else
            if Debug then
               TIO.Put_Line (Sp (3) & "patched " & Output);
            end if;
         end if;
         Free (Args);
         return Status;
      exception
         when Invalid_Process =>
            Free (Args);
            TIO.Put_Line ("FAILED to spawn patch");
            return -1;
      end;
   end Perform_Patch;

   --------------------
   --  Perform Diff  --
   --------------------

   function Perform_Diff (Ada_Path          : String;
                          Filename_With_Ext : String;
                          Max_Gen           : Integer;
                          Debug    : Boolean) return Integer;
   function Perform_Diff (Ada_Path          : String;
                          Filename_With_Ext : String;
                          Max_Gen           : Integer;
                          Debug    : Boolean) return Integer is
      Status    : aliased Integer;
      Args      : Argument_List (1 .. 2);
      PatchFile : TIO.File_Type;
   begin
      Args (1) := new String'(Ada_Path & "/" & Filename_With_Ext);
      Args (2) := new String'(Args (1).all & "~" & Img (Max_Gen));
      if Debug then
         TIO.Put_Line ("diff " & Args (1).all & " " & Args (2).all);
      end if;
      declare
         Output : constant String := Get_Command_Output
           (Command    => "/usr/bin/diff",
            Arguments  => Args,
            Input      => "> kaka ",
            Status     => Status'Access,
            Err_To_Out => True);
      begin
         if Status = 2 then
            TIO.Put_Line (Sp (3)
                          & Args (1).all
                          & ": No previous version");
         elsif Status /= 0 and Status /= 1 then
            TIO.Put_Line ("FAILED to execute command: "
                          & "/usr/bin/diff "
                          & Argument_List_To_String (Args)
                          & " status " & Img (Status));
         else
            if Output /= "" then
               TIO.Create (File => PatchFile,
                           Mode => TIO.Out_File,
                           Name => Ada_Path & "/"
                           & Filename_With_Ext & ".patch");
               TIO.Put (PatchFile, Output);
               TIO.Close (PatchFile);

               Status := Perform_Patch (Ada_Path, Filename_With_Ext, Debug);
            else
               if Debug then
                  TIO.Put_Line ("  files identical, no patching");
               end if;
            end if;
         end if;
         Free (Args);
         return Status;
      exception
         when Invalid_Process =>
            Free (Args);
            TIO.Put_Line ("FAILED to spawn patch");
            return -1;
      end;
   end Perform_Diff;

   ------------------------
   --  Generate Backups  --
   ------------------------

   function Generate_Backups (Ada_Path : String;
                              Filename : String; --  lower case
                              Debug    : Boolean) return Integer;
   function Generate_Backups (Ada_Path : String;
                              Filename : String; --  lower case
                              Debug    : Boolean) return Integer is
      procedure Get_Max_Gen (Gen_Filename : String);
      procedure Get_Max_Gen (Gen_Filename : String) is
         Gen : Integer := Max_Gen - 1;
      begin
         loop
            Gen := Gen + 1;
            exit when not Exists (Gen_Filename & "~" & Img (Gen));
         end loop;
         Max_Gen := Integer'Max (Max_Gen, Gen);
      end Get_Max_Gen;

      procedure Make_Backup (Complete_Filename : String);
      procedure Make_Backup (Complete_Filename : String) is
      begin
         if Exists (Complete_Filename) then
            Rename (Complete_Filename,
                    Complete_Filename & "~" & Img (Max_Gen));
            if Debug then
               TIO.Put_Line (Complete_Filename
                             & " renamed to "
                             & Complete_Filename & "~" & Img (Max_Gen));
            end if;
         end if;
      end Make_Backup;

   begin
      if Main_Window then
         Get_Max_Gen (Ada_Path & "/" & Filename & ".gpr");
         Get_Max_Gen (Ada_Path & "/" & Filename & ".adb");
         Get_Max_Gen (Ada_Path & "/" & Filename & ".css");
      end if;
      Get_Max_Gen (Ada_Path & "/" & Filename & "_pkg.ads");
      Get_Max_Gen (Ada_Path & "/" & Filename & "_pkg-main_windows.ads");
      Get_Max_Gen (Ada_Path & "/" & Filename & "_pkg-main_windows.adb");
      Get_Max_Gen (Ada_Path & "/" & Filename & "_pkg-object_collection.ads");
      Get_Max_Gen (Ada_Path & "/" & Filename & "_pkg-object_collection.adb");
      if Signals then
         Get_Max_Gen (Ada_Path & "/" & Filename & "_pkg-register_signals.ads");
         Get_Max_Gen (Ada_Path & "/" & Filename & "_pkg-register_signals.adb");
         Get_Max_Gen (Ada_Path & "/" & Filename & "_pkg-signals.ads");
         Get_Max_Gen (Ada_Path & "/" & Filename & "_pkg-signals.adb");
      end if;
      if Have.TreeStores > 0 then
         Get_Max_Gen (Ada_Path & "/" & Filename & "_pkg-tree_store_enum.ads");
      end if;

      if Main_Window then
         Make_Backup (Ada_Path & "/" & Filename & ".gpr");
         Make_Backup (Ada_Path & "/" & Filename & ".adb");
         Make_Backup (Ada_Path & "/" & Filename & ".css");
      end if;
      Make_Backup (Ada_Path & "/" & Filename & "_pkg.ads");
      Make_Backup (Ada_Path & "/" & Filename & "_pkg-main_windows.ads");
      Make_Backup (Ada_Path & "/" & Filename & "_pkg-main_windows.adb");
      Make_Backup (Ada_Path & "/" & Filename & "_pkg-object_collection.ads");
      Make_Backup (Ada_Path & "/" & Filename & "_pkg-object_collection.adb");
      if Signals then
         Make_Backup (Ada_Path & "/" & Filename & "_pkg-register_signals.ads");
         Make_Backup (Ada_Path & "/" & Filename & "_pkg-register_signals.adb");
         Make_Backup (Ada_Path & "/" & Filename & "_pkg-signals.ads");
         Make_Backup (Ada_Path & "/" & Filename & "_pkg-signals.adb");
      end if;
      if Have.TreeStores > 0 then
         Make_Backup (Ada_Path & "/" & Filename & "_pkg-tree_store_enum.ads");
      end if;
      return 0;
   exception
      when Name_Error =>
         TIO.Put_Line ("Cannot rename old generated files");
         return -1;
   end Generate_Backups;

   -----------------------------
   --  Generate Ada Packages  --
   -----------------------------

   function Generate_Ada_Packages (Ada_Path   : String;
                                   Glade_Path : String;
                                   Filename   : String;
                                   Debug      : Boolean) return Integer is
      Status : Integer;
   begin
      Set_TWin;
      if TWin = null then
         TIO.Put_Line ("could not find a gtkwindow");
         return -1;
      end if;
      Set_Signals;

      if Debug then
         TIO.New_Line;
         TIO.Put_Line ("Generating Ada files...");
      end if;

      TIO.Create (File => AdaFile,
                  Mode => TIO.Out_File,
                  Name => Ada_Path & "/" & Filename & ".TempAda");

      TIO.Set_Output (AdaFile);
      if Main_Window then
         Emit_Main_Program (Capitalize (Filename));
      end if;
      Emit_Main_Package (Capitalize (Filename));
      Emit_Main_Window (Capitalize (Filename), Ada_Path, Glade_Path);
      Emit_Object_Collection (Capitalize (Filename));
      if Signals then
         Emit_Signals (Capitalize (Filename));
         Emit_Register_Signals (Capitalize (Filename));
      end if;
      if Have.TreeStores > 0 then
         Emit_TreeStores_Enum (Capitalize (Filename));
      end if;
      TIO.Set_Output (TIO.Standard_Output);

      TIO.Close (AdaFile);

      Status := Generate_Backups (Ada_Path,
                                  To_Lower (Filename), Debug);
      if Status < 0 then
         return Status;
      end if;

      if Main_Window then
         if Debug then
            TIO.Put_Line ("Generating gpr...");
         end if;
         Status := Emit_GPR (Ada_Path, To_Lower (Filename), Debug);
         if Status = 0 then
            if Debug then
               TIO.Put_Line ("Generating css...");
            end if;
            Status := Emit_CSS (Ada_Path, To_Lower (Filename), Debug);
         end if;
      else
         Status := 0;
      end if;

      if Status = 0 then
         Status := Perform_Chop (Ada_Path, Filename, Debug);
         if Status = 0 then
            Delete_File (Ada_Path & "/" & Filename & ".TempAda");
            if Debug then
               TIO.Put_Line ("Deleting "
                             & Ada_Path & "/" & Filename & ".TempAda");
            end if;

            if Main_Window then
               Status := Perform_Diff (Ada_Path,
                                       To_Lower (Filename)
                                       & ".gpr",
                                       Max_Gen, Debug);
               if Status = 0 or Status = 2 then
                  Status := Perform_Diff (Ada_Path,
                                          To_Lower (Filename)
                                          & ".adb",
                                          Max_Gen, Debug);
               end if;
               if Status = 0 or Status = 2 then
                  Status := Perform_Diff (Ada_Path,
                                          To_Lower (Filename)
                                          & ".css",
                                          Max_Gen, Debug);
               end if;
            end if;
            if Status = 0 or Status = 2 then
               Status := Perform_Diff (Ada_Path,
                                       To_Lower (Filename)
                                       & "_pkg.ads",
                                       Max_Gen, Debug);
            end if;
            if Status = 0 or Status = 2 then
               Status := Perform_Diff (Ada_Path,
                                       To_Lower (Filename)
                                       & "_pkg-main_windows.ads",
                                       Max_Gen, Debug);
            end if;
            if Status = 0 or Status = 2 then
               Status := Perform_Diff (Ada_Path,
                                       Filename
                                       & "_pkg-main_windows.adb",
                                       Max_Gen, Debug);
            end if;
            if Status = 0 or Status = 2 then
               Status := Perform_Diff (Ada_Path,
                                       To_Lower (Filename)
                                       & "_pkg-object_collection.ads",
                                       Max_Gen, Debug);
            end if;
            if Status = 0 or Status = 2 then
               Status := Perform_Diff (Ada_Path,
                                       To_Lower (Filename)
                                       & "_pkg-object_collection.adb",
                                       Max_Gen, Debug);
            end if;
            if Signals then
               if Status = 0 or Status = 2 then
                  Status := Perform_Diff (Ada_Path,
                                          To_Lower (Filename)
                                          & "_pkg-register_signals.ads",
                                          Max_Gen, Debug);
               end if;
               if Status = 0 or Status = 2 then
                  Status := Perform_Diff (Ada_Path,
                                          To_Lower (Filename)
                                          & "_pkg-register_signals.adb",
                                          Max_Gen, Debug);
               end if;
               if Status = 0 or Status = 2 then
                  Status := Perform_Diff (Ada_Path,
                                          To_Lower (Filename)
                                          & "_pkg-signals.ads",
                                          Max_Gen, Debug);
               end if;
               if Status = 0 or Status = 2 then
                  Status := Perform_Diff (Ada_Path,
                                          To_Lower (Filename)
                                          & "_pkg-signals.adb",
                                          Max_Gen, Debug);
               end if;
               if Status = 0 or Status = 2 then
                  if Have.TreeStores > 0 then
                     Status := Perform_Diff (Ada_Path,
                                             To_Lower (Filename)
                                             & "_pkg-tree_stores_enum.ads",
                                             Max_Gen, Debug);
                  end if;
               end if;
            end if;
         end if;
      end if;

      if Debug then
         TIO.Put_Line ("=== End of Process ===");
      end if;

      return Status;
   end Generate_Ada_Packages;

end W2Gtk2Ada;
