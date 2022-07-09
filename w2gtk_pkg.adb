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
with Ada.Calendar;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with GNAT.Strings;            use GNAT.Strings;
with GNAT.Calendar.Time_IO;
with GNATCOLL.Tribooleans;    use GNATCOLL.Tribooleans;
with W2gtk_Decls;             use W2gtk_Decls;
with W2gtk_Emit;              use W2gtk_Emit;

package body W2gtk_Pkg is
   package TIO renames Ada.Text_IO;

   Test2  : constant String := "<data name=""$this.";
   Test5  : constant String := "<data name=""";
   Test1  : constant String := "<metadata name=""";
   Test7  : constant String := "System.Windows.Forms.";
   Test8  : constant String := "System.Drawing.Printing.";
   Test9  : constant String := (1 .. 8 => ' ') & "'";
   Test10 : constant String := "New Decimal(New Integer() {";
   Test11 : constant String := "Handles";
   Test12 : constant String := ".ShowDialog()";
   Test13 : constant String := "System.Windows.Forms."
     & "DataVisualization.Charting";

   --  specs
   function Adjust_To_Gtk return Integer;
   function Parse_Resource_File (TWin           : Window_Pointer;
                                 Resx_Path      : String;
                                 Resx_File_Name : String) return Integer;
   function Parse2_Designer_File (TWin           : Window_Pointer;
                                  Resx_Path      : String;
                                  Resx_File_Name : String) return Integer;
   function Parse1_Designer_File (Resx_Path      : String;
                                  Resx_File_Name : String) return Integer;
   function Parse_VB_File (Resx_Path      : String;
                           Resx_File_Name : String) return Integer;
   procedure Dump;
   --  end of specs

   function Adjust_To_Gtk return Integer is
      TWin  : Window_Pointer;
      TWdg  : Widget_Pointer;
      TWdgP : Widget_Pointer;
      Temp  : Widget_Pointer;
      NWin0 : Window_Pointer;
      NWin1 : Window_Pointer;
   begin
      if Win_List = null then
         return -1;
      end if;

      TWin := Win_List;
      while TWin /= null loop
         --  set some properties of toolstripstatuslabel
         TWdg := TWin.Widget_List;
         while TWdg /= null loop
            if TWdg.Name /= null then
               if Contains (TWdg.Name.all, "ToolStripStatusLabel") then
                  Debug (NLin, "Adjusting to GTK: ToolStripStatusLabel");
                  Temp := Find_Widget (TWin.Widget_List, GtkStatusBar);
                  if Temp /= null then
                     TWdg.Location.From_Top :=
                       Integer'Max (0, TWdg.Location.From_Top) +
                       Temp.Location.From_Top;
                     TWdg.Location.From_Left :=
                       Integer'Max (0, TWdg.Location.From_Left) +
                       Temp.Location.From_Left;
                  end if;
               end if;
            end if;
            TWdg := TWdg.Next;
         end loop;
         TWin := TWin.Next;
      end loop;

      --  process inheritable attributes (font, others?)
      Debug (NLin, "");
      Debug (NLin, "Adjusting to GTK: inheritable attributes");
      TWin := Win_List;
      while TWin /= null loop
         Process_Inheritable (TWin);
         TWin := TWin.Next;
      end loop;

      --  generate auxiliary elements
      Debug (NLin, "");
      Debug (NLin, "Adjusting to GTK: generate auxiliary elements");
      TWin := Win_List;
      while TWin /= null loop
         TWdg := TWin.Widget_List;
         while TWdg /= null loop
            if TWdg.Widget_Type = GtkFileChooserButton then
               Num_Aux_Widgets := Num_Aux_Widgets + 1;
               if TWdg.OpenFileFilter /= null
                 and then TWdg.OpenFileFilter.all /= ""
               then
                  NWin1 := new Window_Properties (GtkFileFilter);
                  NWin1.Name := new String'("filefilter"
                                            & Img (Num_Aux_Widgets));
                  NWin1.FilterString := TWdg.OpenFileFilter;
               end if;
               NWin0 := new Window_Properties (GtkFileChooserDialog);
               NWin0.Name := new String'("filechooserdialog"
                                         & Img (Num_Aux_Widgets));
               NWin0.Title := TWdg.OpenFileTitle;
               NWin0.FilterName := NWin1.Name;
               NWin0.Transient_For := TWin;
               NWin0.Attached_To   := TWdg;

               if TWdg.OpenFileDialog = null then
                  TWdg.OpenFileDialog := NWin0.Name;
               end if;
               Insert_Front_Window (NWin0);
               Debug (NLin, "Generated filechooserdialog " & NWin0.Name.all);
               Insert_Front_Window (NWin1);
               Debug (NLin, "Generated filefilter " & NWin1.Name.all);
            elsif TWdg.Widget_Type = GtkCalendar then
               Num_Aux_Widgets := Num_Aux_Widgets + 1;
               NWin1 := new Window_Properties (GtkEntryBuffer);
               NWin1.Name := new String'("entrybuffer_"
                                         & Img (Num_Aux_Widgets));
               NWin1.Associated_Widget := TWdg;
               TWdg.Text_Buffer := NWin1;
               Insert_Front_Window (NWin1);
               Debug (NLin, "Generated entrybuffer " & NWin1.Name.all);
            elsif TWdg.Widget_Type = GtkListBox then
               Num_Aux_Widgets := Num_Aux_Widgets + 1;
               NWin1 := new Window_Properties (GtkListStore);
               NWin1.Name := new String'("liststore_"
                                         & Img (Num_Aux_Widgets));
               NWin1.Associated_Widget := TWdg;
               TWdg.ListStore := NWin1;
               Insert_Front_Window (NWin1);
               Debug (NLin, "Generated liststore " & NWin1.Name.all);
            end if;
            TWdg := TWdg.Next;
         end loop;
         TWin := TWin.Next;
      end loop;

      --  set correct parent from parent name and set Gparent
      Debug (NLin, "");
      Debug (NLin, "Adjusting to GTK: reparenting to containers");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            TWdg := TWin.Widget_List;
            while TWdg /= null loop
               if TWdg.Parent_Name /= null then
                  if TWdg.Parent_Name.all = "$this" then
                     TWdg.WParent := TWin;
                     Free (TWdg.Parent_Name);
                     TWdg.Parent_Name := new String'(TWdg.WParent.Name.all);
                     Debug (NLin, "Reparenting Widget " & TWdg.Name.all
                            & " to Window " & TWdg.WParent.Name.all);
                  else
                     TWdgP := Find_Widget (TWin.Widget_List,
                                           TWdg.Parent_Name.all);
                     if TWdgP = null then
                        TIO.Put_Line ("Widget " & TWdg.Name.all
                                      & " without parent");
                        raise TIO.Data_Error;
                     end if;
                     if TWdgP.Widget_Type = GtkFrame or else
                       TWdgP.Widget_Type = GtkToolBar
                     then
                        TWdg.GParent := TWdgP;
                        Debug (NLin, "Reparenting Widget " & TWdg.Name.all
                               & " to Container " & TWdg.GParent.Name.all);
                     end if;
                  end if;
               else
                  TWdg.WParent := TWin;
                  TWdg.Parent_Name := new String'(TWin.Name.all);
                  Debug (NLin, "Reparenting Widget " & TWdg.Name.all
                         & " to Window " & TWdg.WParent.Name.all);
               end if;
               TWdg := TWdg.Next;
            end loop;
         end if;
         TWin := TWin.Next;
      end loop;

      --  reorder the widgets placing each widget in the correct parent list
      Debug (NLin, "");
      Debug (NLin, "Adjusting to GTK: relinking to Containers");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            Relink_To_Containers (TWin);
         end if;
         TWin := TWin.Next;
      end loop;

      return 0;
   end Adjust_To_Gtk;

   procedure Dump is
      TWin  : Window_Pointer;
      use GNAT.Calendar.Time_IO;
      package WinIO is new Ada.Text_IO.Enumeration_IO (Enum => Window_Enum);
      package WdgIO is new Ada.Text_IO.Enumeration_IO (Enum => Widget_Enum);
      package PIO is new Ada.Text_IO.Enumeration_IO (Enum => Position_Enum);

      procedure Dump_Window (TWin  : Window_Pointer; Id : Integer);
      procedure Dump_Widget (TWdgP : Widget_Pointer; Id : Integer);

      procedure Dump_Window (TWin  : Window_Pointer; Id : Integer) is
         TWdg : Widget_Pointer;
         TS   : Signal_Pointer;
      begin
         TIO.New_Line;
         WinIO.Put (TWin.Window_Type); TIO.New_Line;
         TIO.Put (Sp (Id) & "Title         ");
         if TWin.Title /= null then
            TIO.Put_Line (TWin.Title.all);
         else
            TIO.New_Line;
         end if;
         TIO.Put (Sp (Id) & "Name          ");
         if TWin.Name /= null then
            TIO.Put_Line (TWin.Name.all);
         else
            TIO.New_Line;
         end if;

         case TWin.Window_Type is
            when GtkWindow =>
               TIO.Put (Sp (Id) & "Font          ");
               if TWin.Font_Name /= null then
                  TIO.Put (TWin.Font_Name.all);
                  if TWin.Font_Size /= null then
                     TIO.Put (" " & TWin.Font_Size.all);
                  end if;
                  if TWin.Font_Weight /= null then
                     TIO.Put (" " & TWin.Font_Weight.all);
                  end if;
               end if;
               TIO.New_Line;

               TIO.Put (Sp (Id) & "BGColor   ");
               if TWin.BgColor /= null and then TWin.BgColor.all /= "" then
                  TIO.Put (TWin.BgColor.all);
               end if;
               TIO.New_Line;
               TIO.Put (Sp (Id) & "FGColor   ");
               if TWin.BgColor /= null and then TWin.BgColor.all /= "" then
                  TIO.Put (TWin.FgColor.all);
               end if;
               TIO.New_Line;

               TIO.Put (Sp (Id) & "Icon          ");
               if TWin.Icon /= null then
                  TIO.Put_Line (TWin.Icon.all);
               else
                  TIO.New_Line;
               end if;

               TIO.Put (Sp (Id) & "ToolTip       ");
               if TWin.ToolTip /= null then
                  TIO.Put_Line (TWin.ToolTip.all);
               else
                  TIO.New_Line;
               end if;

               TIO.Put (Sp (Id) & "Position      ");
               PIO.Put (TWin.Start_Position);
               TIO.New_Line;

               TIO.Put (Sp (Id) & "Size          H="
                        & Img (TWin.Client_Size.Horiz));
               TIO.Put (", V=" & Img (TWin.Client_Size.Vert));
               TIO.New_Line;

               TIO.Put (Sp (Id) & "Margin        Top "
                        & Img (TWin.Margins (1))
                        & ", Bottom " & Img (TWin.Margins (3))
                        & ", Start " & Img (TWin.Margins (2))
                        & ", End " & Img (TWin.Margins (4)));
               TIO.New_Line;

               TIO.Put (Sp (Id) & "AutoScale     Horiz " &
                          Img (TWin.AutoScaleDim.Horiz));
               TIO.Put (", Vert " & Img (TWin.AutoScaleDim.Vert));
               TIO.New_Line;

               TIO.Put (Sp (Id) & "TryHeight     " &
                          Img (TWin.TrayHeight));
               TIO.New_Line;

               TS := TWin.Signal_List;
               while TS /= null loop
                  TIO.Put_Line (Sp (Id) & "Signal        " & TS.Name.all);
                  TS := TS.Next;
               end loop;

               TWdg := TWin.Widget_List;
               while TWdg /= null loop
                  Dump_Widget (TWdg, Id + 2);
                  TWdg := TWdg.Next;
               end loop;

            when GtkFileChooserDialog =>
               TIO.Put (Sp (Id) & "Filter Name   ");
               if TWin.FilterName /= null then
                  TIO.Put_Line (TWin.FilterName.all);
               else
                  TIO.New_Line;
               end if;
               TIO.Put_Line (Sp (Id) & "Transient for "
                             & TWin.Transient_For.Name.all);
               TIO.Put_Line (Sp (Id) & "Attached to   "
                             & TWin.Attached_To.Name.all);
               TS := TWin.Signal_List;
               while TS /= null loop
                  TIO.Put_Line (Sp (Id) & "Signal        " & TS.Name.all);
                  TS := TS.Next;
               end loop;

            when GtkFileFilter =>
               TIO.Put (Sp (Id) & "Filter String");
               if TWin.FilterString /= null then
                  TIO.Put_Line (TWin.FilterString.all);
               else
                  TIO.New_Line;
               end if;
               TS := TWin.Signal_List;
               while TS /= null loop
                  TIO.Put_Line (Sp (Id) & "Signal        " & TS.Name.all);
                  TS := TS.Next;
               end loop;

            when GtkEntryBuffer =>
               TIO.Put (Sp (Id) & "Assoc. Widget ");
               if TWin.Associated_Widget /= null and then
                 TWin.Associated_Widget.Name /= null
               then
                  TIO.Put (TWin.Associated_Widget.Name.all);
               end if;
               TIO.New_Line;
               TIO.Put_Line (Sp (Id) & "Min_Date      "
                             & Image (TWin.Associated_Widget.MinDate,
                               ISO_Date));
               TIO.Put_Line (Sp (Id) & "Max_Date      "
                             & Image (TWin.Associated_Widget.MaxDate,
                               ISO_Date));

            when GtkListStore =>
               TIO.Put (Sp (Id) & "Assoc. Widget ");
               if TWin.Associated_Widget /= null and then
                 TWin.Associated_Widget.Name /= null
               then
                  TIO.Put (TWin.Associated_Widget.Name.all);
               end if;
               TIO.New_Line;
         end case;
      end Dump_Window;

      procedure Dump_Widget (TWdgP : Widget_Pointer; Id : Integer) is
         TWdgChild : Widget_Pointer;
         TS   : Signal_Pointer;
      begin
         --  common fields
         TIO.Put (Sp (Id));
         WdgIO.Put (TWdgP.Widget_Type);
         TIO.New_Line;

         TIO.Put (Sp (Id + 2) & "Name         " & TWdgP.Name.all & " ("
                  & TWdgP.Windows_Type.all & ")");
         TIO.New_Line;

         TIO.Put (Sp (Id + 2) & "Parent       ");
         if TWdgP.Parent_Name /= null then
            TIO.Put (TWdgP.Parent_Name.all);
         end if;
         if TWdgP.GParent /= null
           and then TWdgP.GParent.Windows_Type /= null
         then
            TIO.Put (" (");
            WdgIO.Put (TWdgP.GParent.Widget_Type);
            TIO.Put (")");
         else
            if TWdgP.WParent /= null then
               TIO.Put (" (");
               WinIO.Put (TWdgP.WParent.Window_Type);
               TIO.Put (")");
            end if;
         end if;
         TIO.New_Line;

         TIO.Put (Sp (Id + 2) & "Text         ");
         if TWdgP.Text /= null then
            TIO.Put (TWdgP.Text.all);
         end if;
         TIO.New_Line;

         TIO.Put (Sp (Id + 2) & "MaxLength    ");
         TIO.Put_Line (Img (TWdgP.MaxLength));

         TIO.Put (Sp (Id + 2) & "Font         ");
         if TWdgP.Font_Name /= null then
            TIO.Put (TWdgP.Font_Name.all);
            if TWdgP.Font_Size /= null then
               TIO.Put (" " & TWdgP.Font_Size.all);
            end if;
            if TWdgP.Font_Weight /= null then
               TIO.Put (" " & TWdgP.Font_Weight.all);
            end if;
         end if;
         TIO.New_Line;

         TIO.Put (Sp (Id + 2) & "BGColor      ");
         if TWdgP.BgColor /= null and then TWdgP.BgColor.all /= "" then
            TIO.Put (TWdgP.BgColor.all);
         end if;
         TIO.New_Line;
         TIO.Put (Sp (Id + 2) & "FGColor      ");
         if TWdgP.FgColor /= null and then TWdgP.FgColor.all /= "" then
            TIO.Put (TWdgP.FgColor.all);
         end if;
         TIO.New_Line;

         TIO.Put (Sp (Id + 2) & "TabStop      ");
         if TWdgP.TabStop  then
            TIO.Put_Line ("True");
         else
            TIO.Put_Line ("False");
         end if;
         TIO.Put_Line (Sp (Id + 2) & "TabIndex     " & Img (TWdgP.TabIndex));

         TIO.Put_Line (Sp (Id + 2) & "Zorder       " & Img (TWdgP.Zorder));

         TIO.Put (Sp (Id + 2) & "ToolTip      ");
         if TWdgP.ToolTip /= null then
            TIO.Put_Line (TWdgP.ToolTip.all);
         else
            TIO.New_Line;
         end if;

         TIO.Put (Sp (Id + 2) & "Location     x=" &
                    Img (TWdgP.Location.From_Left) &
                    ", y=" & Img (TWdgP.Location.From_Top));
         TIO.New_Line;

         TIO.Put (Sp (Id + 2) & "Size         H=" & Img (TWdgP.Size.Horiz));
         TIO.Put (", V=" & Img (TWdgP.Size.Vert));
         TIO.New_Line;

         TIO.Put_Line (Sp (Id + 2) & "Margin       Top "
                       & Img (TWdgP.Margins (1))
                       & ", Bottom " & Img (TWdgP.Margins (3))
                       & ", Start " & Img (TWdgP.Margins (2))
                       & ", End " & Img (TWdgP.Margins (4)));

         TIO.Put (Sp (Id + 2) & "Visible      ");
         if TWdgP.Visible then
            TIO.Put_Line ("True");
         else
            TIO.Put_Line ("False");
         end if;

         TIO.Put (Sp (Id + 2) & "Enabled      ");
         if TWdgP.Enabled then
            TIO.Put_Line ("True");
         else
            TIO.Put_Line ("False");
         end if;

         TIO.Put (Sp (Id + 2) & "TextAlign    ");
         if TWdgP.TextAlign /= null then
            TIO.Put_Line (TWdgP.TextAlign.all);
         else
            TIO.New_Line;
         end if;

         TIO.Put (Sp (Id + 2) & "AutoSize     ");
         if To_Boolean (TWdgP.AutoSize) then
            TIO.Put_Line ("True");
         elsif not To_Boolean (TWdgP.AutoSize) then
            TIO.Put_Line ("False");
         else
            TIO.New_Line;
         end if;

         case TWdgP.Widget_Type is
         when None => null;
         when GtkEntry | GtkComboBox =>
            TIO.Put (Sp (Id + 2) & "Has_Frame    ");
            if TWdgP.Has_Frame then
               TIO.Put_Line ("True");
            else
               TIO.Put_Line ("False");
            end if;

            TIO.Put (Sp (Id + 2) & "Editable     ");
            if TWdgP.Editable then
               TIO.Put_Line ("True");
            else
               TIO.Put_Line ("False");
            end if;

            TIO.Put (Sp (Id + 2) & "PasswordChar ");
            if TWdgP.PasswordChar /= null then
               TIO.Put_Line (TWdgP.PasswordChar.all);
            else
               TIO.New_Line;
            end if;

         when GtkSpinButton =>
            TIO.Put_Line (Sp (Id + 2) & "Start        "
                          & Img (TWdgP.StartValue));
            TIO.Put_Line (Sp (Id + 2) & "MaxValue     "
                          & Img (TWdgP.MaxValue));
            TIO.Put_Line (Sp (Id + 2) & "MinValue     "
                          & Img (TWdgP.MinValue));
            TIO.Put_Line (Sp (Id + 2) & "Step         "
                          & Img (TWdgP.Step));

         when GtkFileChooserButton | PrintDocument | PrintDialog
            | FolderBrowserDialog  | GtkToolTip    | GtkColorButton
            | GtkStatusBar =>

            TIO.Put_Line (Sp (Id + 2) & "TrayLocation x=" &
                            Img (TWdgP.TrayLocation.From_Left) &
                            ", y=" & Img (TWdgP.TrayLocation.From_Top));

            case TWdgP.Widget_Type is
               when GtkFileChooserButton =>
                  TIO.Put (Sp (Id + 2) & "OpenFileDialog ");
                  if TWdgP.OpenFileDialog /= null and then
                    TWdgP.OpenFileDialog.all /= ""
                  then
                     TIO.Put (TWdgP.OpenFileDialog.all);
                  end if;
                  TIO.New_Line;
                  TIO.Put (Sp (Id + 2) & "TrayLocation x=" &
                             Img (TWdgP.TrayLocation.From_Left) &
                             ", y=" & Img (TWdgP.TrayLocation.From_Top));
                  TIO.New_Line;
                  TIO.Put (Sp (Id + 2) & "OpenFileFilter ");
                  if TWdgP.OpenFileFilter /= null and then
                    TWdgP.OpenFileFilter.all /= ""
                  then
                     TIO.Put (TWdgP.OpenFileFilter.all);
                  end if;
                  TIO.New_Line;
                  TIO.Put (Sp (Id + 2) & "OpenFileTitle  ");
                  if TWdgP.OpenFileTitle /= null and then
                    TWdgP.OpenFileTitle.all /= ""
                  then
                     TIO.Put (TWdgP.OpenFileTitle.all);
                  end if;
                  TIO.New_Line;

               when GtkColorButton =>
                  if TWdgP.AnyColor then
                     TIO.Put_Line (Sp (Id + 2) & "AnyColor     True");
                  else
                     TIO.Put_Line (Sp (Id + 2) & "AnyColor     False");
                  end if;
                  if TWdgP.FullOpen then
                     TIO.Put_Line (Sp (Id + 2) & "FullOpen     True");
                  else
                     TIO.Put_Line (Sp (Id + 2) & "FullOpen     False");
                  end if;
                  TIO.Put (Sp (Id + 2) & "Ass. Button  ");
                  if TWdgP.Associated_Button /= null then
                     TIO.Put (TWdgP.Associated_Button.Name.all);
                  end if;
                  TIO.New_Line;

               when others => null;
            end case;

         when GtkListBox =>
            TIO.Put (Sp (Id + 2) & "MultiSelect  ");
            if TWdgP.MultiSelect then
               TIO.Put ("True");
            else
               TIO.Put ("False");
            end if;
            TIO.New_Line;
            TIO.Put (Sp (Id + 2) & "ListStore    ");
            if TWdgP.ListStore /= null and then
              TWdgP.ListStore.Name /= null
            then
               TIO.Put (TWdgP.ListStore.Name.all);
            end if;
            TIO.New_Line;

         when GtkCalendar =>
            TIO.Put_Line (Sp (Id + 2) & "StartDate    "
                          & Image (TWdgP.Start_Date, ISO_Date));
            TIO.Put_Line (Sp (Id + 2) & "MinDate      "
                          & Image (TWdgP.MinDate, ISO_Date));
            TIO.Put_Line (Sp (Id + 2) & "MaxDate      "
                          & Image (TWdgP.MaxDate, ISO_Date));
            if TWdgP.Format_Date /= null and then TWdgP.Format_Date.all /= ""
            then
               TIO.Put_Line (Sp (Id + 2) & "Format       "
                             & TWdgP.Format_Date.all);
            end if;
            if TWdgP.ShowUpDown then
               TIO.Put_Line (Sp (Id + 2) & "ShowUpDown   True");
            else
               TIO.Put_Line (Sp (Id + 2) & "ShowUpDown   False");
            end if;
            TIO.Put (Sp (Id + 2) & "TextBuffer   ");
            if TWdgP.Text_Buffer /= null and then
              TWdgP.Text_Buffer.Name /= null
            then
               TIO.Put (TWdgP.Text_Buffer.Name.all);
            end if;
            TIO.New_Line;

         when GtkLabel =>
            TIO.Put (Sp (Id + 2) & "BorderStyle  ");
            if TWdgP.BorderStyle /= null and then TWdgP.BorderStyle.all /= ""
            then
               TIO.Put (TWdgP.BorderStyle.all);
            end if;
            TIO.New_Line;

         when GtkButton | GtkRadioButton | GtkCheckButton | GtkToggleButton =>
            if TWdgP.Active then
               TIO.Put_Line (Sp (Id + 2) & "Active       True");
            else
               TIO.Put_Line (Sp (Id + 2) & "Active       False");
            end if;
            if TWdgP.Underline then
               TIO.Put_Line (Sp (Id + 2) & "Underline    True");
            else
               TIO.Put_Line (Sp (Id + 2) & "Underline    False");
            end if;

            case TWdgP.Widget_Type is
            when GtkButton =>
               TIO.Put (Sp (Id + 2) & "ColorButton  ");
               if TWdgP.Associated_ColorButton /= null then
                  TIO.Put (TWdgP.Associated_ColorButton.Name.all);
               end if;
               TIO.New_Line;

            when GtkCheckButton =>
               TIO.Put (Sp (Id + 2) & "CheckAlign   ");
               if TWdgP.CheckAlign /= null then
                  TIO.Put_Line (TWdgP.CheckAlign.all);
               else
                  TIO.New_Line;
               end if;
            when others => null;
            end case;

         when GtkFrame | GtkToolBar =>
            case TWdgP.Widget_Type is
               when GtkToolBar =>
                  TIO.Put_Line (Sp (Id + 2) & "TrayLocation x=" &
                                  Img (TWdgP.TrayLocation.From_Left) &
                                  ", y=" & Img (TWdgP.TrayLocation.From_Top));
                  TIO.Put (Sp (Id + 2) & "Horizontal   ");
                  if TWdgP.TB_Horiz then
                     TIO.Put_Line ("True");
                  else
                     TIO.Put_Line ("False");
                  end if;
                  TIO.Put (Sp (Id + 2) & "Show Arrows  ");
                  if TWdgP.Show_Arrows then
                     TIO.Put_Line ("True");
                  else
                     TIO.Put_Line ("False");
                  end if;
                  TIO.Put (Sp (Id + 2) & "Grip Visible ");
                  if TWdgP.Grip_Visible then
                     TIO.Put_Line ("True");
                  else
                     TIO.Put_Line ("False");
                  end if;

               when others => null;
            end case;

            TWdgChild := TWdgP.Child_List;
            while TWdgChild /= null loop
               Dump_Widget (TWdgChild, Id + 2);
               TWdgChild := TWdgChild.Next;
            end loop;

         when GtkImage => null;
         when GtkMenu => null;
         when GtkSeparatorToolItem => null;
         when ToolStripStatusLabel => null;
         when Chart =>
               TIO.Put (Sp (Id + 2) & "Anchor  ");
               if TWdgP.Anchor /= null then
                  TIO.Put (TWdgP.Anchor.all);
               end if;
               TIO.New_Line;

         end case;

         TS := TWdgP.Signal_List;
         while TS /= null loop
            TIO.Put_Line (Sp (Id + 2) & "Signal       " & TS.Name.all);
            TS := TS.Next;
         end loop;

      end Dump_Widget;
   begin
      TWin := Win_List;
      while TWin /= null loop
         Dump_Window (TWin, 2);
         TWin := TWin.Next;
      end loop;
   end Dump;

   function Parse_Resource_File (TWin           : Window_Pointer;
                                 Resx_Path      : String;
                                 Resx_File_Name : String) return Integer is
      Result : Integer;
      function Parse_Window_Properties
        (TWin           : Window_Pointer;
         Resx_Path      : String;
         Resx_File_Name : String) return Integer;
      function Parse_Window_Properties
        (TWin           : Window_Pointer;
         Resx_Path      : String;
         Resx_File_Name : String) return Integer
      is
         Idx0 : Integer;
         Idx1 : Integer;
         P0   : Pair;

      begin
         --  loop to get window properties
         TIO.Open (File => RFile,
                   Mode => TIO.In_File,
                   Name => Resx_Path & "/" & Resx_File_Name & ".resx");

         NLin := 0;
         Debug (NLin, "");
         Debug (NLin, "Parsing Resources: Window: " & Resx_File_Name);
         while not TIO.End_Of_File (RFile) loop
            TIO.Get_Line (RFile, Line, Len);
            NLin := NLin + 1;

            Idx0 := Index (Line (1 .. Len), Test2);
            if Idx0 in 1 .. Len then
               Idx1 := Index (Line (Idx0 + Test2'Length .. Len), """");
               if Idx1 in Idx0 + Test2'Length .. Len then
                  declare
                     Property : constant String :=
                       Line (Idx0 + Test2'Length .. Idx1 - 1);
                  begin
                     if Property = "ClientSize" then
                        P0 := Get_Pair (Get_String (RFile));
                        TWin.Client_Size.Horiz := P0.One;
                        TWin.Client_Size.Vert  := P0.Two;
                        Debug (NLin, "Set Window Property ClientSize H=" &
                                 Img (P0.One) & ", V=" & Img (P0.Two));
                     elsif Property = "AutoScaleDimensions" then
                        P0 := Get_Pair (Get_String (RFile));
                        TWin.AutoScaleDim.Horiz := P0.One;
                        TWin.AutoScaleDim.Vert  := P0.Two;
                        Debug (NLin, "Set Window Property AutoScaleDim H=" &
                                 Img (P0.One) & ", V=" & Img (P0.Two));
                     elsif Property = "Icon" then
                        TWin.Icon := new String'("");
                     elsif Property = "StartPosition" then
                        declare
                           Temp : constant String := Get_String (RFile);
                        begin
                           if Temp = "CenterParent" then
                              TWin.Start_Position := CenterParent;
                              Debug (NLin, "Set Window Property CenterParent");
                           end if;
                        end;
                     elsif Property = "Margin" then
                        TWin.Margins := Get_Margin_Array (RFile);
                        Debug (NLin, "Set Window Property Margins " &
                                 Img (TWin.Margins (1)) & ", " &
                                 Img (TWin.Margins (2)) & ", " &
                                 Img (TWin.Margins (3)) & ", " &
                                 Img (TWin.Margins (4)));
                     elsif Property = "Font" then
                        Get_Font (RFile, TWin.Font_Name,
                                  TWin.Font_Size, TWin.Font_Weight);
                        Debug (NLin, "Set Window Property Font "
                               & TWin.Font_Name.all & ", "
                               & TWin.Font_Size.all);
                     elsif Property = "Type" then
                        null; --  should be Form
                     elsif Property = "ToolTip" then
                        TWin.ToolTip := new String'(Get_String (RFile));
                        Debug (NLin, "Set Window Property Tooltip " &
                                 TWin.ToolTip.all);
                     elsif Property = "TrayHeight" then
                        TWin.TrayHeight := Get_Integer (Get_String (RFile));
                        Debug (NLin, "Set Window Property TrayHeight " &
                                 TWin.ToolTip.all);
                     elsif Property = "Text" then
                        TWin.Title := new String'(Get_String (RFile));
                        Debug (NLin, "Set Window Property Title " &
                                 TWin.Title.all);
                     elsif Property = "Name" then
                        if TWin.Name = null then
                           TWin.Name := new String'(Get_String (RFile));
                           Debug (NLin, "Set Window Property Name " &
                                    TWin.Name.all);
                        end if;
                     elsif Property = "RightToLeft" then
                        null; --  ignore
                     else
                        TIO.Put_Line (Resx_File_Name & ".resx"
                                      & ": Line" & NLin'Image
                                      & ": unknown window property: "
                                      & Property);
                        TIO.Close (RFile);
                        return -1;
                     end if;
                  end;
               else
                  TIO.Put_Line (Resx_File_Name & ".resx"
                                & ": Line" & NLin'Image &
                                  ": cannot parse window property");
                  TIO.Close (RFile);
                  return -1;
               end if;
            end if;
         end loop;
         TIO.Close (RFile);
         return 0;
      end Parse_Window_Properties;

      function Parse_Widget_Properties
        (TWin           : Window_Pointer;
         Resx_Path      : String;
         Resx_File_Name : String) return Integer;
      function Parse_Widget_Properties
        (TWin           : Window_Pointer;
         Resx_Path      : String;
         Resx_File_Name : String) return Integer
      is
         Idx0  : Integer;
         Idx1  : Integer;
         Idx2  : Integer;
         P0    : Pair;
         Disp0 : Integer;
      begin
         TIO.Open (File => RFile,
                   Mode => TIO.In_File,
                   Name => Resx_Path & "/" & Resx_File_Name & ".resx");
         NLin := 0;
         --  second loop to skip the header comment
         while not TIO.End_Of_File (RFile) loop
            TIO.Get_Line (RFile, Line, Len);
            NLin := NLin + 1;
            Idx0 := Index (Line (1 .. Len), "-->");
            exit when Idx0 in 1 .. Len;
         end loop;

         --  continue reading
         Debug (NLin, "");
         Debug (NLin, "Parsing Resources: Widgets");
         while not TIO.End_Of_File (RFile) loop
            TIO.Get_Line (RFile, Line, Len);
            NLin := NLin + 1;
            Idx0 := Index (Line (1 .. Len), "&gt;&gt;");
            if Idx0 in 1 .. Len then
               Line (Idx0 .. Len - 8) := Line (Idx0 + 8 .. Len);
               Len := Len - 8;
            end if;
            Idx0  := Index (Line (1 .. Len), Test5);
            Disp0 := Test5'Length;
            if not (Idx0 in 1 .. Len) then
               Idx0  := Index (Line (1 .. Len), Test1);
               Disp0 := Test1'Length;
            end if;
            if Idx0 in 1 .. Len then
               Idx1 := Index (Line (Idx0 + Disp0 .. Len), """");
               Idx2 := Index (Line (Idx0 .. Idx1), "$this.");
               if Idx1 in Idx0 + Test2'Length .. Len and then
                 not (Idx2 in Idx0 .. Idx1)
               then
                  declare
                     Property : constant String :=
                       Line (Idx0 + Disp0 .. Idx1 - 1);
                  begin
                     Idx0 := Index (Property, ".");
                     if Idx0 in Property'Range then
                        declare
                           PName   : constant String :=
                             Property (Property'First .. Idx0 - 1);
                           PAttrib : constant String :=
                             Property (Idx0 + 1 .. Property'Last);
                           WT : Widget_Pointer;
                        begin
                           if PName = "" then
                              TIO.Put_Line ("Line" & NLin'Image &
                                              ": cannot find widget name");
                              TIO.Close (RFile);
                              return -1;
                           end if;
                           if PAttrib = "" then
                              TIO.Put_Line ("Line" & NLin'Image &
                                              ": cannot find widget property");
                              TIO.Close (RFile);
                              return -1;
                           end if;
                           WT := Find_Widget (TWin.Widget_List, PName);
                           if WT = null then
                              TIO.Put_Line ("Widget " & PName
                                            & " not in Designer");
                              return -1;
                           end if;
                           if PAttrib = "RightToLeft" then
                              null; --  ignore
                           elsif PAttrib = "Image" then
                              null; --  ignore
                           elsif PAttrib = "ImageTransparentColor" then
                              null; --  ignore

                           elsif PAttrib = "Anchor" then
                              WT.Anchor := new String'(Get_String (RFile));
                              Debug (NLin, "Set Widget Property "
                                     & PName & ".Anchor "
                                     & WT.Anchor.all);
                           elsif PAttrib = "ZOrder" then
                              WT.Zorder := Get_Integer (Get_String (RFile));
                              Debug (NLin, "Set Widget Property "
                                     & PName & ".ZOrder "
                                     & Img (WT.Zorder));
                           elsif PAttrib = "Margin" then
                              WT.Margins := Get_Margin_Array (RFile);
                              Debug (NLin, "Set Widget Property " & PName
                                     & ". Margins " &
                                       Img (WT.Margins (1)) & ", " &
                                       Img (WT.Margins (2)) & ", " &
                                       Img (WT.Margins (3)) & ", " &
                                       Img (WT.Margins (4)));
                           elsif PAttrib = "TabIndex" then
                              WT.TabIndex := Get_Integer (Get_String (RFile));
                              Debug (NLin, "Set Widget Property "
                                     & PName & ".TabIndex "
                                     & Img (WT.TabIndex));
                           elsif PAttrib = "Text" then
                              WT.Text := new String'(Get_String (RFile));
                              Debug (NLin, "Set Widget Property "
                                     & PName & ".Text "
                                     & WT.Text.all);
                           elsif PAttrib = "MaxLength" then
                              WT.MaxLength := Get_Integer (Get_String (RFile));
                              Debug (NLin, "Set Widget Property "
                                     & PName & ".MaxLength "
                                     & Img (WT.MaxLength));
                           elsif PAttrib = "ToolTip" or else
                             PAttrib = "ToolTipText"
                           then
                              WT.ToolTip := new String'(Get_String (RFile));
                              Debug (NLin, "Set Widget Property "
                                     & PName & ".ToolTip "
                                     & WT.ToolTip.all);
                           elsif PAttrib = "SizeMode" then
                              WT.SizeMode := new String'(Get_String (RFile));
                              Debug (NLin, "Set Widget Property "
                                     & PName & ".SizeMode "
                                     & WT.SizeMode.all);
                           elsif PAttrib = "AutoSize" then
                              WT.AutoSize := To_TriBoolean
                                (Get_Boolean (RFile));
                              Debug (NLin, "Set Widget Property "
                                     & PName & ".Autosize "
                                     & WT.AutoSize'Image);
                           elsif PAttrib = "Enabled" then
                              WT.Enabled := Get_Boolean (RFile);
                              Debug (NLin, "Set Widget Property "
                                     & PName & ".Enabled "
                                     & WT.Enabled'Image);
                           elsif PAttrib = "Visible" then
                              WT.Visible := Get_Boolean (RFile);
                              Debug (NLin, "Set Widget Property "
                                     & PName & ".Visible "
                                     & WT.Visible'Image);
                           elsif PAttrib = "WaitOnLoad" then
                              WT.WaitOnLoad := Get_Boolean (RFile);
                              Debug (NLin, "Set Widget Property "
                                     & PName
                                     & ".WaitOnLoad " & WT.WaitOnLoad'Image);
                           elsif PAttrib = "TextAlign" then
                              WT.TextAlign := new String'(Get_String (RFile));
                              Debug (NLin, "Set Widget Property "
                                     & PName & ".TextAlign "
                                     & WT.TextAlign.all);
                           elsif PAttrib = "CheckAlign" then
                              WT.CheckAlign := new String'(Get_String (RFile));
                              Debug (NLin, "Set Widget Property "
                                     & PName & ".CheckAlign "
                                     & WT.CheckAlign.all);
                           elsif PAttrib = "Name" then
                              if PName /= Get_String (RFile) then
                                 TIO.Put_Line ("Line" & NLin'Image & ": " &
                                                 "name mistmatch");
                                 raise TIO.Data_Error;
                              end if;
                              Debug (NLin, "Set Widget Property "
                                     & PName & ".Name "
                                     & WT.Name.all);
                           elsif PAttrib = "Size" then
                              P0 := Get_Pair (Get_String (RFile));
                              WT.Size.Horiz := P0.One;
                              WT.Size.Vert  := P0.Two;
                              Debug (NLin, "Set Widget Property "
                                     & PName & ".Size H=" &
                                       Img (P0.One) & ", V=" & Img (P0.Two));
                           elsif PAttrib = "Location" then
                              P0 := Get_Pair (Get_String (RFile));
                              WT.Location.From_Top := P0.Two;
                              WT.Location.From_Left := P0.One;
                              Debug (NLin, "Set Widget Property "
                                     & PName &
                                       ".Location H=" &
                                       Img (P0.One) & ", V=" & Img (P0.Two));
                           elsif PAttrib = "Type" then
                              WT.Windows_Type :=
                                new String'(Get_Widget_Name (RFile));
                              Debug (NLin, "Set Widget Property "
                                     & PName & ".Type " &
                                       WT.Windows_Type.all);
                           elsif PAttrib = "Parent" then
                              WT.Parent_Name :=
                                new String'(Get_String (RFile));
                              Debug (NLin, "Set Widget Property "
                                     & PName &
                                       ".Parent Name " &
                                       WT.Parent_Name.all);
                           elsif PAttrib = "Font" then
                              Get_Font (RFile, WT.Font_Name,
                                        WT.Font_Size, WT.Font_Weight);
                              Debug (NLin, "Set Widget Property "
                                     & PName & ".Font "
                                     & WT.Font_Name.all
                                     & ", " & WT.Font_Size.all);
                           elsif PAttrib = "PasswordChar" then
                              WT.PasswordChar :=
                                new String'(Get_String (RFile));
                              Debug (NLin, "Set Widget Property "
                                     & PName &
                                       ".PasswordChar" & WT.PasswordChar.all);
                           elsif PAttrib = "ImeMode" then
                              WT.ImeMode := new String'(Get_String (RFile));
                              Debug (NLin, "Set Widget Property "
                                     & PName &
                                       ".ImeMode " & WT.ImeMode.all);
                           elsif PAttrib = "OpenFile" then
                              WT.OpenFileDialog :=
                                new String'(Get_String (RFile));
                              Debug (NLin, "Set Widget Property "
                                     & PName &
                                       ".OpenFile " & WT.OpenFileDialog.all);
                           elsif PAttrib = "TrayLocation" then
                              P0 := Get_Pair (Get_String (RFile));
                              WT.TrayLocation.From_Top  := P0.Two;
                              WT.TrayLocation.From_Left := P0.One;
                              Debug (NLin, "Set Widget Property "
                                     & PName &
                                       ".TrayLocation H=" &
                                       Img (P0.One) & ", V=" & Img (P0.Two));
                           elsif PAttrib = "Filter" then
                              WT.OpenFileFilter :=
                                new String'(Get_String (RFile));
                              Debug (NLin, "Set Widget Property "
                                     & PName
                                     & ".OpenFileFilter "
                                     & WT.OpenFileFilter.all);
                           elsif PAttrib = "Title" then
                              WT.OpenFileTitle :=
                                new String'(Get_String (RFile));
                              Debug (NLin, "Set Widget Property "
                                     & PName
                                     & ".OpenFileTitle "
                                     & WT.OpenFileTitle.all);
                           elsif PAttrib (PAttrib'First .. PAttrib'First + 4) =
                             "Items"
                           then
                              Debug (NLin, "Resource: Ignored Widget Property "
                                     & PName & " " & PAttrib);
                           elsif PAttrib = "HorizontalScrollbar" or
                             PAttrib = "ItemHeight"
                           then
                              Debug (NLin, "Resource: Ignored Widget Property "
                                     & PName & " " & PAttrib);
                           else
                              TIO.Put_Line (Resx_File_Name & ".resx"
                                            & ": Line" & NLin'Image &
                                              ": unknown widget property: " &
                                              Property);
                              TIO.Close (RFile);
                              return -1;
                           end if;
                        exception
                           when others =>
                              TIO.Put_Line (Resx_File_Name & ".resx"
                                            & ": Line:" & NLin'Image
                                            & ": " & Property);
                              return -1;
                        end;
                     end if;
                  end;
               end if;
            end if;
         end loop;
         return 0;
      end Parse_Widget_Properties;

   begin
      Result := Parse_Window_Properties (TWin, Resx_Path, Resx_File_Name);
      if Result /= 0 then
         return Result;
      end if;

      Result := Parse_Widget_Properties (TWin, Resx_Path, Resx_File_Name);

      return Result;
   exception
      when Constraint_Error =>
         TIO.Close (DFile);
         TIO.Put_Line (Resx_File_Name & ".resx"
                       & ": Line" & NLin'Image
                       & " " & Line (1 .. Len));
         return -1;
   end Parse_Resource_File;

   function Parse2_Designer_File (TWin           : Window_Pointer;
                                  Resx_Path      : String;
                                  Resx_File_Name : String) return Integer is
      use GNAT.Calendar.Time_IO;

      Idx0  : Integer;
      Idx1  : Integer;
      Idx2  : Integer;
      Idx3  : Integer;
      WT    : Widget_Pointer;

      procedure Get_Line;
      procedure Get_Line is
      begin
         TIO.Get_Line (DFile, Line, Len);
         NLin := NLin + 1;
         if Line (Len) = ASCII.CR then
            Len := Len - 1;
         end if;
      end Get_Line;

      procedure Process_Toolbar_Items (Parent : Widget_Pointer);
      procedure Process_Toolbar_Items (Parent : Widget_Pointer) is
         Child : Widget_Pointer;
      begin
         Idx0 := Index (Line (1 .. Len), "{");
         if Idx0 not in 1 .. Len then
            raise TIO.Data_Error;
         end if;
         loop
            Idx0 := Index (Line (Idx0 + 1 .. Len), "Me.");
            if Idx0 not in 1 .. Len then
               raise TIO.Data_Error;
            end if;
            Idx0 := Idx0 + 3;
            Idx1 := Index (Line (Idx0 .. Len), ",");
            exit when Idx1 not in Idx0 .. Len;
            Child := Find_Widget (TWin.Widget_List, Line (Idx0 .. Idx1 - 1));
            if Child /= null then
               Child.Parent_Name := Parent.Name;
               Debug (NLin, "Set Widget Property "
                      & WT.Name.all & ".Parent_Name " & Parent.Name.all);
            else
               raise TIO.Data_Error;
            end if;
            Idx0 := Idx1 + 1; -- skip semicolon
            exit when Line (Idx0) = '}';
         end loop;
      end Process_Toolbar_Items;

      procedure Process_Widget;
      procedure Process_Widget is
         function Get_Date (Idx : Integer) return Ada.Calendar.Time;
         function Get_Date (Idx : Integer) return Ada.Calendar.Time is
            Temp0 : Integer;
            Temp1 : Integer;
            Y, M, D : Integer;
            TTime : Ada.Calendar.Time;
         begin
            Temp0 := Index (Line (Idx .. Len), "New Date(");
            if Temp0 in Idx .. Len - 12 then
               Temp0 := Temp0 + 9;
               Temp1 := Index (Line (Temp0 .. Len - 12), ",");
               if Temp1 in Temp0 + 1 .. Len - 12 then
                  Y := Get_Integer (Line (Temp0 .. Temp1 - 1));
                  if Y < Ada.Calendar.Year_Number'First then
                     Y := Ada.Calendar.Year_Number'First;
                  elsif Y > Ada.Calendar.Year_Number'Last then
                     Y := Ada.Calendar.Year_Number'Last;
                  end if;
                  Temp0 := Temp1 + 1;
                  Temp1 := Index (Line (Temp0 .. Len - 12), ",");
                  if Temp1 in Temp0 + 1 .. Len - 12 then
                     M := Get_Integer (Line (Temp0 .. Temp1 - 1));
                     if M < Ada.Calendar.Month_Number'First then
                        M := Ada.Calendar.Month_Number'First;
                     elsif M > Ada.Calendar.Month_Number'Last then
                        M := Ada.Calendar.Month_Number'Last;
                     end if;
                     Temp0 := Temp1 + 1;
                     Temp1 := Index (Line (Temp0 .. Len - 12), ",");
                     if Temp1 in Temp0 + 1 .. Len - 12 then
                        D := Get_Integer (Line (Temp0 .. Temp1 - 1));
                        if D < Ada.Calendar.Day_Number'First then
                           D := Ada.Calendar.Day_Number'First;
                        elsif D > Ada.Calendar.Day_Number'Last then
                           D := Ada.Calendar.Day_Number'Last;
                        end if;
                        TTime := Ada.Calendar.Time_Of (Y, M, D);
                        return TTime;
                     end if;
                  end if;
               end if;
            end if;
            TIO.Put_Line ("Ill-formed date:" & Line (Idx .. Len));
            raise TIO.Data_Error;
         end Get_Date;

      begin
         --  enter to read line with widget name
         if TIO.End_Of_File (DFile) then
            return;
         end if;
         Get_Line;
         Idx0 := Index (Line (1 .. Len), Test9);
         Idx0 := Idx0 + Test9'Length;
         WT := Find_Widget (TWin.Widget_List, Line (Idx0 .. Len));
         if WT = null then
            if TWin.Name.all = Line (Idx0 .. Len) then
               Len := 0;
               return;
            end if;
            TIO.Put_Line (Img (NLin) & ": "
                          & Line (Idx0 + 1 .. Len) & ": widget not found");
            raise TIO.Data_Error;
         end if;
         --  skip second test9-line
         if TIO.End_Of_File (DFile) then
            return;
         end if;
         Get_Line;
         --  read attributes
         loop
            exit when TIO.End_Of_File (DFile);
            Get_Line;
            --  first test9 of following widget
            exit when Line (Line'First .. Len) = Test9;
            Idx0 := Index (Line (1 .. Len), "Me." & WT.Name.all & ".");
            if Idx0 in Line'Range then
               Idx0 := Idx0 + 3 + WT.Name.all'Length + 1;
               Idx1 := Index (Line (Idx0 .. Len), " = ");
               if Idx1 not in Idx0 .. Len then
                  if Line (Idx0 .. Idx0 + 13) = "Items.AddRange" then
                     Idx1 := Idx0 + 14;
                  end if;
               end if;
               if Idx1 in Idx0 .. Len then
                  declare
                     Attrib : constant String := Line (Idx0 .. Idx1 - 1);
                  begin
                     Idx1 := Idx1 + 3;

                     if Attrib = "Name" then
                        if WT.Name.all /= Line (Idx1 + 1 .. Len - 1) then
                           TIO.Put_Line (Img (NLin)
                                         & ": name mistmatch between "
                                         & "Designer and Resource");
                           raise TIO.Data_Error;
                        end if;

                     elsif Attrib = "DisplayStyle" then
                        if Contains (Line (Idx1 .. Len), "DisplayStyle.Text")
                        then
                           WT.DStyle := Text_Only;
                        end if;
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".DStyle Text_Only");

                     elsif Attrib = "TabStop" then
                        if Line (Idx1 .. Len) = "False" then
                           WT.TabStop := False;
                           Debug (NLin, "Set Widget Property "
                                  & WT.Name.all & ".TabStop True");
                        elsif Line (Idx1 .. Len) = "True" then
                           WT.TabStop := True;
                           Debug (NLin, "Set Widget Property "
                                  & WT.Name.all & ".TabStop False");
                        end if;

                     elsif Attrib = "BackColor" then
                        WT.BgColor := new String'(Line (Idx1 .. Len));
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".BgColor "
                               & WT.BgColor.all);

                     elsif Attrib = "BorderStyle" then
                        if WT.Widget_Type = GtkEntry then
                           Idx2 := Index (Line (Idx1 .. Len),
                                          "BorderStyle.None");
                           if Idx2 in Idx1 .. Len then
                              WT.Has_Frame := False;
                              Debug (NLin, "Set Widget Property "
                                     & WT.Name.all & ".Has_Frame False");
                           end if;

                        elsif WT.Widget_Type = GtkLabel then
                           Idx2 := Index (Line (Idx1 .. Len), "BorderStyle");
                           if Idx2 not in Idx1 .. Len then
                              raise TIO.Data_Error;
                           end if;
                           WT.BorderStyle :=
                             new String'(Line (Idx2 + 12 .. Len));
                              Debug (NLin, "Set Widget Property "
                                     & WT.Name.all & ".BorderStyle "
                                     & WT.BorderStyle.all);
                        end if;

                     elsif Attrib = "GripStyle" and then
                       WT.Widget_Type = GtkToolBar
                     then
                        if Contains (Line (Idx1 .. Len), "Hidden") then
                           WT.Grip_Visible := False;
                           Debug (NLin, "Set Widget Property "
                                  & WT.Name.all & ".Grip_Visible False");
                        elsif Contains (Line (Idx1 .. Len), "Visible") then
                           WT.Grip_Visible := True;
                           Debug (NLin, "Set Widget Property "
                                  & WT.Name.all & ".Grip_Visible True");
                        end if;

                     elsif Attrib = "Checked" and then
                       (WT.Widget_Type = GtkRadioButton or
                          WT.Widget_Type = GtkButton or
                            WT.Widget_Type = GtkCheckButton or
                              WT.Widget_Type = GtkToggleButton)
                     then
                        if Contains (Line (Idx1 .. Len), "False") then
                           WT.Active := False;
                           Debug (NLin, "Set Widget Property "
                                  & WT.Name.all & ".Active False");
                        elsif Contains (Line (Idx1 .. Len), "True") then
                           WT.Active := True;
                           Debug (NLin, "Set Widget Property "
                                  & WT.Name.all & ".Active True");
                        end if;

                     elsif Attrib = "CheckState" and then
                       (WT.Widget_Type = GtkRadioButton or
                          WT.Widget_Type = GtkButton or
                            WT.Widget_Type = GtkCheckButton or
                              WT.Widget_Type = GtkToggleButton)
                     then
                        if Contains (Line (Idx1 .. Len), "Checked") then
                           WT.Active := True;
                           Debug (NLin, "Set Widget Property "
                                  & WT.Name.all & ".Active True");
                        else
                           WT.Active := False;
                           Debug (NLin, "Set Widget Property "
                                  & WT.Name.all & ".Active False");
                        end if;

                     elsif Attrib = "AnyColor" and then
                       WT.Widget_Type = GtkColorButton
                     then
                        if Contains (Line (Idx1 .. Len), "False") then
                           WT.AnyColor := False;
                           Debug (NLin, "Set Widget Property "
                                  & WT.Name.all & ".AnyColor False");
                        elsif Contains (Line (Idx1 .. Len), "False") then
                           WT.AnyColor := True;
                           Debug (NLin, "Set Widget Property "
                                  & WT.Name.all & ".AnyColor True");
                        end if;

                     elsif Attrib = "Items.AddRange" and then
                       WT.Widget_Type = GtkToolBar
                     then
                        Process_Toolbar_Items (WT);

                     elsif Attrib = "FullOpen" and then
                       WT.Widget_Type = GtkColorButton
                     then
                        if Contains (Line (Idx1 .. Len), "False") then
                           WT.FullOpen := False;
                           Debug (NLin, "Set Widget Property "
                                  & WT.Name.all & ".FullOpen False");
                        elsif Contains (Line (Idx1 .. Len), "False") then
                           WT.FullOpen := True;
                           Debug (NLin, "Set Widget Property "
                                  & WT.Name.all & ".FullOpen True");
                        end if;

                     elsif Attrib = "UseSystemPasswordChar" and then
                       WT.Widget_Type = GtkEntry
                     then
                        if Contains (Line (Idx1 .. Len), "True") then
                           WT.PasswordChar := new String'("");
                           Debug (NLin, "Set Widget Property "
                                  & WT.Name.all & ".PasswordChar");
                        end if;

                     elsif Attrib = "ReadOnly" and then
                       (WT.Widget_Type = GtkEntry or
                          WT.Widget_Type = GtkComboBox)
                     then
                        if Contains (Line (Idx1 .. Len), "False") then
                           WT.Editable := False;
                           Debug (NLin, "Set Widget Property "
                                  & WT.Name.all & ".Editable False");
                        elsif Contains (Line (Idx1 .. Len), "False") then
                           WT.Editable := True;
                           Debug (NLin, "Set Widget Property "
                                  & WT.Name.all & ".Editable True");
                        end if;

                     elsif Attrib = "Maximum" and then
                       WT.Widget_Type = GtkSpinButton
                     then
                        Idx2 := Idx1 + Test10'Length;
                        Idx3 := Index (Line (Idx2 .. Len), ",");
                        WT.MaxValue := Get_Integer (Line (Idx2 .. Idx3 - 1));
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".Maximum "
                               & Img (WT.MaxValue));

                     elsif Attrib = "Minimum" and then
                       WT.Widget_Type = GtkSpinButton
                     then
                        Idx2 := Idx1 + Test10'Length;
                        Idx3 := Index (Line (Idx2 .. Len), ",");
                        WT.MinValue := Get_Integer (Line (Idx2 .. Idx3 - 1));
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".Minimum "
                               & Img (WT.MinValue));

                     elsif Attrib = "Value" and then
                       WT.Widget_Type = GtkSpinButton
                     then
                        Idx2 := Idx1 + Test10'Length;
                        Idx3 := Index (Line (Idx2 .. Len), ",");
                        WT.StartValue := Get_Integer (Line (Idx2 .. Idx3 - 1));
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".StartValue "
                               & Img (WT.StartValue));

                     elsif Attrib = "Format" and then
                       WT.Widget_Type = GtkCalendar
                     then
                        WT.Format_Date := new String'(Line (Idx1 .. Len));
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".Format_Date"
                               & WT.Format_Date.all);

                     elsif Attrib = "Value" and then
                       WT.Widget_Type = GtkCalendar
                     then
                        WT.Start_Date := Get_Date (Idx1);
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".Start_Date"
                               & Image (WT.Start_Date, ISO_Date));

                     elsif Attrib = "MinDate" and then
                       WT.Widget_Type = GtkCalendar
                     then
                        WT.MinDate := Get_Date (Idx1);
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".Start_Date"
                               & Image (WT.MinDate, ISO_Date));

                     elsif Attrib = "MaxDate" and then
                       WT.Widget_Type = GtkCalendar
                     then
                        WT.MaxDate := Get_Date (Idx1);
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".Start_Date"
                               & Image (WT.MaxDate, ISO_Date));

                     elsif Attrib = "LimitDatePicket" and then
                       WT.Widget_Type = GtkCalendar
                     then
                        WT.MaxDate := Get_Date (Idx1);
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".Start_Date"
                               & Image (WT.MaxDate, ISO_Date));

                     elsif Attrib = "ShowUpDown" and then
                       WT.Widget_Type = GtkCalendar
                     then
                        if Contains (Line (Idx1 .. Len), "False") then
                           WT.ShowUpDown := False;
                           Debug (NLin, "Set Widget Property "
                                  & WT.Name.all & ".ShowUpDown True");
                        elsif Contains (Line (Idx1 .. Len), "False") then
                           WT.ShowUpDown := True;
                           Debug (NLin, "Set Widget Property "
                                  & WT.Name.all & ".ShowUpDown True");
                        end if;

                     elsif Attrib = "SelectionMode" and then
                       WT.Widget_Type = GtkListBox
                     then
                        WT.MultiSelect := True;
                           Debug (NLin, "Set Widget Property "
                                  & WT.Name.all & ".MultiSelect True");

                     elsif Attrib = "ForeColor" and then
                       WT.Widget_Type = GtkEntry
                     then
                        null;
                        Debug (NLin, "Ignored Widget Property "
                               & WT.Name.all & " " & Attrib);
                     elsif Attrib = "BackColor" and then
                       WT.Widget_Type = GtkEntry
                     then
                        null;
                        Debug (NLin, "Designer: Ignored Widget Property "
                               & WT.Name.all & " " & Attrib);
                     elsif Attrib = "Cursor" or
                       Attrib = "Tag" or
                       Attrib = "FormattingEnabled" or
                       Attrib = "DropDownStyle" or
                       Attrib = "UseVisualStyleBackColor" or
                       Attrib = "DialogResult" or
                       Attrib = "AllowDrop" or
                       Attrib = "FlatStyle" or
                       Attrib = "ShowNetwork" or
                       Attrib = "UseExDialog" or
                       Attrib = "AutoCheck" or
                       Attrib = "UseWaitCursor" or
                       Attrib = "ShowNewFolderButton" or
                       Attrib = "RootFolder" or
                       Attrib = "DefaultExt" or
                       Attrib = "CheckFileExists" or
                       Attrib = "IsBalloon" or
                       Attrib = "UseEXDialog" or
                       Attrib = "AutoToolTip" or
                       Attrib = "Image"
                     then
                        Debug (NLin, "Designer: Ignored Widget Property "
                               & WT.Name.all & " " & Attrib);
                     else
                        TIO.Put_Line (Resx_File_Name & ".Designer.vb"
                                      & ": Line" & NLin'Image
                                      & ": Designer: unknown attribute: "
                                      & WT.Name.all & "." & Attrib
                                      & " = " & Line (Idx1 .. Len));
                     end if;
                  end;
               end if;
            end if;
         end loop;
      end Process_Widget;

   begin
      TIO.Open (File => DFile,
                Mode => TIO.In_File,
                Name => Resx_Path & "/" & Resx_File_Name & ".Designer.vb");

      NLin := 0;
      Debug (NLin, "");
      Debug (NLin, "Parsing Designer: Attributes");
      while not TIO.End_Of_File (DFile) loop
         Get_Line;
         Idx0 := Index (Line (1 .. Len), "Me.SuspendLayout()");
         if Idx0 in Line'Range then
            exit when TIO.End_Of_File (DFile);
            Get_Line;
            loop
               Process_Widget;
               exit when TIO.End_Of_File (DFile);
--               Get_Line;
               exit when Len <= 0;
            end loop;
         end if;
      end loop;
      TIO.Close (DFile);
      return 0;
   exception
      when Constraint_Error =>
         TIO.Close (DFile);
         TIO.Put_Line (Resx_File_Name & ".Designer.vb"
                       & ": Line" & NLin'Image
                       & " " & Line (1 .. Len));
         return -1;
   end Parse2_Designer_File;

   function Parse1_Designer_File (Resx_Path      : String;
                                  Resx_File_Name : String) return Integer is
      Idx0  : Integer;
      Idx1  : Integer;
      Idx2  : Integer;
      Idx3  : Integer;
      Found : Boolean;
      TWin  : Window_Pointer;
      WT    : Widget_Pointer;
   begin
      TIO.Open (File => DFile,
                Mode => TIO.In_File,
                Name => Resx_Path & "/" & Resx_File_Name & ".Designer.vb");

      NLin := 0;

      --  skip first line
      TIO.Get_Line (DFile, Line, Len);
      NLin := NLin + 1;

      Debug (NLin, "Parsing Designer: Window");
      while not TIO.End_Of_File (DFile) loop
         TIO.Get_Line (DFile, Line, Len);
         if Line (Len) = ASCII.CR then
            Len := Len - 1;
         end if;
         NLin := NLin + 1;
         Idx0 := Index (Line (1 .. Len), "Partial Class ");
         if Idx0 in 1 .. Len then
            TWin := new Window_Properties (GtkWindow);
            if Idx0 + 14 >= Len then
               TIO.Put_Line (Resx_File_Name & ".Designer.vb"
                             & ": Could not find Window Name in Designer");
               TIO.Close (DFile);
               return -1;
            end if;
            TWin.Name := new String'(Line (Idx0 + 14 .. Len));
            Insert_Window (TWin);
            Debug (NLin, "Created GtkWindow " & TWin.Name.all);
            Debug (NLin, "Set Window Property Name " & TWin.Name.all);
            exit;
         end if;
      end loop;

      Found := False;
      while not TIO.End_Of_File (DFile) loop
         TIO.Get_Line (DFile, Line, Len);
         NLin := NLin + 1;
         Idx0 := Index (Line (1 .. Len), "Me.");
         if Idx0 in 1 .. Len then
            Found := True;
            exit;
         end if;
      end loop;

      if not Found then
         TIO.Put_Line (Resx_File_Name & ".Designer.vb"
                       & ": Me.components not found in Designer file");
         TIO.Close (DFile);
         return -1;
      end if;

      Debug (NLin, "");
      Debug (NLin, "Parsing Designer: widgets");
      loop
         Idx0 := Index (Line (1 .. Len), "Me.");
         if Idx0 not in 1 .. Len then
            exit;
         end if;
         Idx0 := Idx0 + 3;
         Idx1 := Index (Line (Idx0 .. Len), " = New " & Test13);
         if Idx1 in Idx0 .. Len then
            Idx2 := Idx1 + Test7'Length;
         else
            Idx1 := Index (Line (Idx0 .. Len), " = New " & Test8);
            if Idx1 in Idx0 .. Len then
               Idx2 := Idx1 + Test8'Length;
            else
               Idx1 := Index (Line (Idx0 .. Len), " = New " & Test7);
               if Idx1 in Idx0 .. Len then
                  Idx2 := Idx1 + Test8'Length;
               else
                  exit;
               end if;
            end if;
         end if;
         Idx3 := Len - 3;
         WT := Find_Widget (TWin.Widget_List, Line (Idx2 .. Idx3));
         if WT /= null then
            TIO.Put_Line (Resx_File_Name & ".Designer.vb: "
                          & " Line" & NLin'Image
                          & ": Repeated Widget " & Line (Idx2 .. Idx3));
            TIO.Close (DFile);
            return -1;
         end if;
         WT := new Widget_Properties (Widget_Type =>
                                        Convert (Line (Idx2 .. Idx3)));
         if WT.Widget_Type = None then
            TIO.Put_Line (Resx_File_Name & ".Designer.vb: "
                          & " Line" & NLin'Image
                          & ": unknown Widget " & Line (Idx2 .. Idx3));
            TIO.Close (DFile);
            return -1;
         end if;
         WT.Name := new String'(Line (Idx0 .. Idx1 - 1));
         Insert_Widget (TWin, WT);
         Debug (NLin, "Created "
                & WT.Widget_Type'Image & " "
                & WT.Name.all & " from "
                & Line (Idx1 + 7 .. Idx3));
         exit when TIO.End_Of_File (DFile);
         TIO.Get_Line (DFile, Line, Len);
         NLin := NLin + 1;
      end loop;

      TIO.Close (DFile);
      return 0;
   exception
      when Constraint_Error =>
         TIO.Close (DFile);
         TIO.Put_Line (Resx_File_Name & ".Designer.vb"
                       & ": Line" & NLin'Image
                       & " " & Line (1 .. Len));
         return -1;
   end Parse1_Designer_File;

   function Parse_VB_File (Resx_Path      : String;
                           Resx_File_Name : String) return Integer is
      WT : Widget_Pointer;
      Line0 : String (1 .. 1024); --  prev line
      Len0  : Integer := 0;
      Line1 : String (1 .. 1024); --  prev prev line
      Len1  : Integer := 0;
      Line2 : String (1 .. 1024); --  prev prev prev line
      Len2  : Integer := 0;
      Line3 : String (1 .. 1024); --  prev prev prev prev line
      Len3  : Integer := 0;

      Idx0  : Integer;
      Idx1  : Integer;

      procedure Get_Line;
      procedure Get_Line is
      begin
         Len3 := Len2;
         Line3 (1 .. Len3) := Line2 (1 .. Len2);

         Len2 := Len1;
         Line2 (1 .. Len2) := Line1 (1 .. Len1);

         Len1 := Len0;
         Line1 (1 .. Len1) := Line0 (1 .. Len0);

         Len0 := Len;
         Line0 (1 .. Len0) := Line (1 .. Len);

         TIO.Get_Line (VFile, Line, Len);
         NLin := NLin + 1;
         if Line (Len) = ASCII.CR then
            Len := Len - 1;
         end if;
      end Get_Line;

      function Get_Handler return String;
      function Get_Handler return String is
      begin
         Idx1 := Index (Line (1 .. Idx0 - 1), "Sub ");
         if Idx1 not in 1 .. Idx0 - 1 then
            Idx1 := Index (Line0 (1 .. Len0), "Sub ");
            if Idx1 not in 1 .. Len0 then
               Idx1 := Index (Line1 (1 .. Len1), "Sub ");
               if Idx1 not in 1 .. Len1 then
                  Idx1 := Index (Line2 (1 .. Len2), "Sub ");
                  if Idx1 not in 1 .. Len2 then
                     Idx1 := Index (Line3 (1 .. Len3), "Sub ");
                     if Idx1 not in 1 .. Len3 then
                        TIO.Put_Line (Resx_File_Name
                                      & ": Line: " & Img (NLin)
                                      & ": could not find handler");
                        return "";
                     else
                        Len :=  Len3;
                        Line (1 .. Len) := Line3 (1 .. Len3);
                     end if;
                  else
                     Len :=  Len2;
                     Line (1 .. Len) := Line2 (1 .. Len2);
                  end if;
               else
                  Len :=  Len1;
                  Line (1 .. Len) := Line1 (1 .. Len1);
               end if;
            else
               Len := Len0;
               Line (1 .. Len) := Line0 (1 .. Len0);
            end if;
         end if;
         Idx0 := Idx1 + 4;
         Idx1 := Index (Line (Idx0 .. Len), "(");
         if Idx1 not in Idx0 .. Len then
            TIO.Put_Line (Resx_File_Name
                          & ": Line: " & Img (NLin)
                          & ": could not find handler");
            return "";
         end if;
         return Line (Idx0 .. Idx1 - 1);
      end Get_Handler;

      function Parse_Handler (Complete_Signal : String) return Integer;
      function Parse_Handler (Complete_Signal : String) return Integer is
         Idx1 : Integer;
         Ret  : Integer := 0;
      begin
         Idx1 := Index (Complete_Signal, ".");
         if Idx1 not in
           Complete_Signal'First + 1 .. Complete_Signal'Last - 1
         then
            TIO.Put_Line (Resx_File_Name
                          & ": Line: " & Img (NLin)
                          & ": cannot parse signal. Line: "
                          & Complete_Signal);
            return -1;
         end if;
         declare
            WName : constant String :=
              Complete_Signal (Complete_Signal'First .. Idx1 - 1);
            SName : constant String :=
              Complete_Signal (Idx1 + 1 .. Complete_Signal'Last);
            WS : Signal_Pointer;
         begin
            if WName = "MyBase" or WName = "Me" then
               WS := new Signal_Block;
               if SName (SName'Last) = ',' then
                  WS.Name    := new String'(SName (SName'First ..
                                              SName'Last - 1));
                  Ret := 1;
               else
                  WS.Name    := new String'(SName);
               end if;
               WS.Handler := new String'(Get_Handler);
               if WS.Handler.all = "" then
                  Free (WS.Handler);
                  return -1;
               end if;
               WS.Line    := NLin;
               Insert_Signal (Win_List, WS);
               if Ret = 1 then
                  Debug (NLin, "Created Signal "
                         & Complete_Signal (Complete_Signal'First ..
                             Complete_Signal'Last - 1));
               else
                  Debug (NLin, "Created Signal " &
                           Win_List.Name.all & "." & SName);
               end if;
            else
               WT := Find_Widget (Win_List.Widget_List, WName);
               if WT = null then
                  TIO.Put_Line (Resx_File_Name & ".vb"
                                & ": Line" & NLin'Image
                                & ": cannot find widget in signal name");
                  return -1;
               end if;
               WS := new Signal_Block;
               if SName (SName'Last) = ',' then
                  WS.Name    := new String'(SName (SName'First ..
                                              SName'Last - 1));
                  Ret := 1;
               else
                  WS.Name    := new String'(SName);
               end if;
               WS.Handler := new String'(Get_Handler);
               if WS.Handler.all = "" then
                  Free (WS.Handler);
                  return -1;
               end if;
               WS.Line    := NLin;
               Insert_Signal (WT, WS);
               if Ret = 1 then
                  Debug (NLin, "Created Signal "
                         & Complete_Signal (Complete_Signal'First ..
                             Complete_Signal'Last - 1));
               else
                  Debug (NLin, "Created Signal " & Complete_Signal);
               end if;
            end if;
         end;
         return Ret;
      end Parse_Handler;

   begin
      TIO.Open (File => VFile,
                Mode => TIO.In_File,
                Name => Resx_Path & "/" & Resx_File_Name & ".vb");

      NLin := 0;

      Debug (NLin, "");
      Debug (NLin, "Parsing vb file");
      while not TIO.End_Of_File (VFile) loop
         Get_Line;
         Idx0 := Index (Line (1 .. Len), Test11);
         if Idx0 in 1 .. Len then
            Idx0 := Idx0 + Test11'Length;
            if Trim (Line (Idx0 .. Len), Ada.Strings.Both) = "" then
               exit when TIO.End_Of_File (VFile);
               Get_Line;
               Idx0 := 1;
            end if;
            if Trim (Line (Idx0 .. Len), Ada.Strings.Both) = "_" then
               exit when TIO.End_Of_File (VFile);
               Get_Line;
               Idx0 := 1;
            end if;
            declare
               Ret  : Integer;
               More : Boolean;
            begin
               Ret := Parse_Handler (Trim (Line (Idx0 .. Len),
                                     Ada.Strings.Both));
               if Ret < 0 then
                  return Ret;
               end if;
               More := (Ret = 1);
               while More loop
                  exit when TIO.End_Of_File (VFile);
                  Get_Line;
                  Ret := Parse_Handler (Trim (Line (1 .. Len),
                                        Ada.Strings.Both));
                  if Ret < 0 then
                     return Ret;
                  end if;
                  More := (Ret = 1);
               end loop;

               --  continue reading to end sub to locate color.showdialog
               while not TIO.End_Of_File (VFile) loop
                  Get_Line;
                  Idx0 := Index (Line (1 .. Len), Test12);
                  if Idx0 in Line'Range then
                     Idx1 := Index (Source  => Line (1 .. Idx0 - 1),
                                    Pattern => " = ",
                                    Going   => Ada.Strings.Backward);
                     if Idx1 in 1 .. Idx0 - 1 then
                        declare
                           WName : constant String :=
                             Line (Idx1 + 3 .. Idx0 - 1);
                           WTCD : Widget_Pointer;
                        begin
                           WTCD := Find_Widget (Win_List.Widget_List, WName);
                           if WTCD = null then
                              TIO.Put_Line ("Warning: "
                                            & Resx_File_Name & ".vb"
                                            & ": Line" & NLin'Image
                                            & ": cannot find widget in "
                                            & "ShowDialog()");
                           else
                              if WTCD.Widget_Type = GtkColorButton then
                                 WT.Associated_ColorButton := WTCD;
                                 WTCD.Associated_Button := WT;
                                 WTCD.Location    := WT.Location;
                                 WTCD.Size        := WT.Size;
                                 WTCD.TabIndex    := WT.TabIndex;
                                 WTCD.TabStop     := WT.TabStop;
                                 WTCD.Zorder      := WT.Zorder;
                                 WTCD.Enabled     := WT.Enabled;
                                 WTCD.Visible     := WT.Visible;

                                 WTCD.TextAlign   := WT.TextAlign;
                                 WTCD.Margins     := WT.Margins;
                                 WTCD.Text        := WT.Text;
                                 WTCD.ToolTip     := WT.ToolTip;
                                 WTCD.Signal_List := WT.Signal_List;
                                 exit;
                              end if;
                           end if;
                        exception
                           when others =>
                              TIO.Put_Line (Resx_File_Name & ".vb"
                                            & ": Line" & NLin'Image
                                            & ": error finding widget in "
                                            & "ShowDialog()");
                              return -1;
                        end;
                     end if;
                  else
                     Idx0 := Index (Line (1 .. Len), "End Sub");
                     exit when Idx0 in Line'Range;
                  end if;
               end loop;
            end;
         end if;
      end loop;

      TIO.Close (VFile);
      return 0;
   end Parse_VB_File;

   function Parse_VS_File (Use_Debug      : Boolean;
                           Do_Dump        : Boolean;
                           Resx_Path      : String;
                           Resx_File_Name : String) return Integer is
      Result : Integer;
      --  TWin   : Window_Pointer;
   begin
      W2gtk_Decls.Use_Debug := Use_Debug;

      Result := Parse1_Designer_File (Resx_Path, Resx_File_Name);
      if Result /= 0 then
         return Result;
      end if;

      Result := Parse_Resource_File (Win_List, Resx_Path, Resx_File_Name);
      if Result /= 0 then
         return Result;
      end if;

      Result := Parse2_Designer_File (Win_List, Resx_Path, Resx_File_Name);
      if Result /= 0 then
         return Result;
      end if;

      Result := Parse_VB_File (Resx_Path, Resx_File_Name);
      if Result /= 0 then
         return Result;
      end if;

      Result := Adjust_To_Gtk;
      if Result /= 0 then
         return Result;
      end if;

      if Do_Dump then
         Dump;
      end if;

      return Result;
   end Parse_VS_File;

   function Generate_Glade_File (Glade_Path      : String;
                                 Glade_File_Name : String) return Integer is
      TWin  : Window_Pointer;
   begin
      if Win_List = null then
         TIO.Put_Line ("No window information");
         return -1;
      end if;
      TIO.Create (File => GFile,
                  Mode => TIO.Out_File,
                  Name => Glade_Path & "/" & Glade_File_Name & ".glade");
      Emit_GtkHeader (null, 0);
      TWin := Win_List;
      while TWin /= null loop
         case TWin.Window_Type is
            when GtkWindow =>
               Emit_GtkWindow (TWin, 2);
            when GtkFileFilter =>
               Emit_GtkFileFilter (TWin, 2);
            when GtkEntryBuffer =>
               Emit_GtkEntryBuffer (TWin, 2);
            when GtkFileChooserDialog =>
               Emit_GtkFileChooserDialog (TWin, 2);
            when GtkListStore =>
               Emit_GtkListStore (TWin, 2);
         end case;
         TWin := TWin.Next;
      end loop;
      Emit_GtkTrailer (null, 0);

      TIO.Close (GFile);
      return 0;
   end Generate_Glade_File;

end W2gtk_Pkg;
