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
with Ada.Integer_Text_IO;
with Ada.Calendar;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with GNAT.Strings;            use GNAT.Strings;
with GNAT.Calendar.Time_IO;
with GNATCOLL.Tribooleans;    use GNATCOLL.Tribooleans;
with GNATCOLL.Utils;          use GNATCOLL.Utils;
with W2gtk_Decls;             use W2gtk_Decls;
with W2gtk_Emit;              use W2gtk_Emit;
with Symbol_Tables;
with W2gtk_Backups;           use W2gtk_Backups;
with W2gtk_Version;           use W2gtk_Version;

package body W2gtk_Pkg is

   package TIO renames Ada.Text_IO;
   package DSIO is new Ada.Text_IO.Enumeration_IO (Display_Style_Enum);
   package FDIO is new Ada.Text_IO.Enumeration_IO (FlowDirection_Enum);
   package PIO is new Ada.Text_IO.Enumeration_IO (Window_Position_Enum);
   package IPIO is new Ada.Text_IO.Enumeration_IO (Image_Position_Enum);
   package TAIO is new Ada.Text_IO.Enumeration_IO (TextAlign_Enum);
   package CHHSMIO is new Ada.Text_IO.Enumeration_IO
     (ColumnHeadersHeightSizeMode_Enum);
   package RHWSMIO is new Ada.Text_IO.Enumeration_IO
     (RowHeadersWidthSizeMode_Enum);
   package RHBSIO is new Ada.Text_IO.Enumeration_IO
     (RowHeadersBorderStyle_Enum);
   package BSIO is new Ada.Text_IO.Enumeration_IO (BorderStyle_Enum);
   package SMIO is new Ada.Text_IO.Enumeration_IO (SortMode_Enum);
   package ASMIO is new Ada.Text_IO.Enumeration_IO (AutoSizeMode_Enum);
   package ASCMIO is new Ada.Text_IO.Enumeration_IO (AutoSizeColumnMode_Enum);
   package SBIO is new Ada.Text_IO.Enumeration_IO (ScrollBars_Enum);
   package DRIO is new Ada.Text_IO.Enumeration_IO (DialogResult_Enum);
   package CSIO is new Ada.Text_IO.Enumeration_IO (Cell_Style_Enum);
   package LSEIO is new Ada.Text_IO.Enumeration_IO (Layout_Style_Enum);
   package FSEIO is new Ada.Text_IO.Enumeration_IO (Frame_Shadow_Enum);
   package BEIO is new Ada.Text_IO.Enumeration_IO (Baseline_Enum);

   Max_Gen : Integer := 1;

   Test2  : constant String := "<data name=""$this.";
   Test5  : constant String := "<data name=""";
   Test1  : constant String := "<metadata name=""";
   Test9  : constant String := (1 .. 8 => ' ') & "'";
   Test10 : constant String := "New Decimal(New Integer() {";
   Test11 : constant String := "Handles";
   Test12 : constant String := ".ShowDialog()";
   Test13 : constant String := "Private Sub InitializeComponent()";
   Test8  : constant String := "Partial Class ";
   Test7  : constant String := "Me.components = New System.ComponentModel";
   Test18 : constant String := "Dim resources As System.ComponentModel";
   Test19 : constant String := "Dim DataGridViewCellStyle";
   Test20 : constant String := "DataGridViewCellStyle";

   Test3  : constant String := "Items.AddRange";
   Test4  : constant String := "DropDownItems.AddRange";
   Test6  : constant String := "Columns.AddRange";
   Test14 : constant String := "Controls.Add(Me.";
   Test15 : constant String := "Series.Add(";
   Test16 : constant String := "ChartAreas.Add(";
   Test17 : constant String := "Legends.Add(";

   --  specs
   function Adjust_To_Gtk return Integer;
   function Parse_Resource_File (TWin           : Window_Pointer;
                                 Resx_Path      : String;
                                 Resx_File_Name : String) return Integer;
   function Parse2_Designer_File (TWin           : Window_Pointer;
                                  Resx_Path      : String;
                                  Resx_File_Name : String;
                                  Icon_Path      : String) return Integer;
   function Parse1_Designer_File (Resx_Path      : String;
                                  Resx_File_Name : String) return Integer;
   function Parse_VB_File (Resx_Path      : String;
                           Resx_File_Name : String) return Integer;
   procedure Dump (Path : String; File_Name : String; Instant : String);
   --  end of specs

   -------------------------------------------------------------------------
   function Adjust_To_Gtk return Integer is separate;

   -------------------------------------------------------------------------
   procedure Dump (Path : String; File_Name : String; Instant : String) is separate;

   -------------------------------------------------------------------------
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
         Win_Prop : Window_Property_Enum;

      begin
         --  loop to get window properties
         TIO.Open (File => RFile,
                   Mode => TIO.In_File,
                   Name => Resx_Path & "/" & Resx_File_Name & ".resx");

         NLin := 0;
         Debug (-1, "");
         Debug (-1, "Parsing Resources of " & Resx_File_Name);
         Debug (0, "Window");
         while not TIO.End_Of_File (RFile) loop
            TIO.Get_Line (RFile, Line, Len);
            NLin := NLin + 1;

            Idx0 := Index (Line (1 .. Len), Test2);
            if Idx0 in 1 .. Len then
               Idx1 := Index (Line (Idx0 + Test2'Length .. Len), """");
               if Idx1 in Idx0 + Test2'Length .. Len then
                  Win_Prop := Symbol_Tables.Get_Property
                    (Line (Idx0 + Test2'Length .. Idx1 - 1));

                  case Win_Prop is
                     when Str_Accept_Button =>
                        null;
                     when Str_Cancel_Button =>
                        null;
                     when Str_ClientSize =>
                        P0 := Get_Pair (Get_String (RFile));
                        TWin.Client_Size.Horiz := P0.One;
                        TWin.Client_Size.Vert  := P0.Two;
                        Debug (NLin, Sp (3) & "Set Window Property ClientSize H=" &
                                 Img (P0.One) & ", V=" & Img (P0.Two));

                     when Str_AutoScaleDimensions =>
                        P0 := Get_Pair (Get_String (RFile));
                        TWin.AutoScaleDim.Horiz := P0.One;
                        TWin.AutoScaleDim.Vert  := P0.Two;
                        Debug (NLin, Sp (3) & "Set Window Property AutoScaleDim H=" &
                                 Img (P0.One) & ", V=" & Img (P0.Two));

                     when Str_Icon =>
                        TWin.Icon := new String'("");
                        Debug (NLin, Resx_File_Name & ".resx"
                               & ": ignored window property "
                               & "Icon");

                     when Str_StartPosition =>
                        declare
                           Temp : constant String := Get_String (RFile);
                        begin
                           if Temp = "CenterParent" then
                              TWin.Start_Position := CenterParent;
                              Debug (NLin, Sp (3) & "Set Window Property CenterParent");
                           end if;
                        end;

                     when Str_Margin =>
                        TWin.Margins := Get_Margin_Array (RFile);
                        Debug (NLin, Sp (3) & "Set Window Property Margins " &
                                 Img (TWin.Margins (1)) & ", " &
                                 Img (TWin.Margins (2)) & ", " &
                                 Img (TWin.Margins (3)) & ", " &
                                 Img (TWin.Margins (4)));

                     when Str_Font =>
                        Get_Font (RFile,
                                  TWin.Font_Name,
                                  TWin.Font_Size,
                                  TWin.Font_Weight,
                                  TWin.Font_Underline);
                        if TWin.Font_Weight /= null then
                           Have.Font_Weight := Have.Font_Weight + 1;
                        end if;
                        if TWin.Font_Weight = null then
                           Debug (NLin, Sp (3) & "Set Window Property "
                                  & TWin.Name.all & ".Font "
                                  & TWin.Font_Name.all & " "
                                  & Img (TWin.Font_Size)
                                  & (if TWin.Font_Underline then
                                     " underline" else ""));
                        else
                           Debug (NLin, Sp (3) & "Set Window Property "
                                  & TWin.Font_Name.all & " "
                                  & TWin.Name.all & ".Font "
                                  & " " & Img (TWin.Font_Size)
                                  & " " & TWin.Font_Weight.all
                                  & (if TWin.Font_Underline then
                                     " underline" else ""));
                        end if;

                     when Str_Type =>
                        null; --  should be Form

                     when Str_ToolTip =>
                        TWin.ToolTip := new String'(Get_String (RFile));
                        Debug (NLin, Sp (3) & "Set Window Property Tooltip " &
                                 TWin.ToolTip.all);

                     when Str_TrayHeight =>
                        TWin.TrayHeight := Get_Integer (Get_String (RFile));
                        Debug (NLin, "Set Window Property TrayHeight " &
                                 TWin.ToolTip.all);

                     when Str_Text =>
                        TWin.Title := new String'(Get_String (RFile));
                        Debug (NLin, Sp (3) & "Set Window Property Title " &
                                 TWin.Title.all);

                     when Str_Name =>
                        if TWin.Name = null then
                           TWin.Name := new String'(+Get_String (RFile));
                           TWin.Original_Name := new String'(TWin.Name.all);
                           Debug (NLin, Sp (3) & "Set Window Property Name "
                                  & TWin.Name.all);
                        end if;

                     when Str_RightToLeft =>
                        Debug (NLin, Resx_File_Name & ".resx"
                               & ": ignored window property "
                               & "RightToLeft");

                     when No_Property =>
                        Debug (NLin, Resx_File_Name & ".resx"
                                      & ": Line" & NLin'Image
                                      & ": unknown window property: "
                                      & Line (Idx0 + Test2'Length .. Len));
                        TIO.Close (RFile);
                        return -1;
                  end case;
               else
                  Debug (NLin, Resx_File_Name & ".resx"
                                & ": Line" & NLin'Image &
                                  ": cannot parse window property");
                  TIO.Close (RFile);
                  return -1;
               end if;
            end if;
         end loop;
         TIO.Close (RFile);
         return 0;
      exception
         when Constraint_Error =>
            TIO.Close (RFile);
            Debug (NLin, "Constraint Error");
            Debug (NLin, Resx_File_Name & ".resx"
                          & ": Line" & NLin'Image
                          & " " & Line (1 .. Len));
            return -1;
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
         Debug (-1, "");
         Debug (NLin, "Widgets");
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
            if Idx0 not in 1 .. Len then
               goto Continue_Loop;
            end if;

            Idx1 := Index (Line (Idx0 + Disp0 .. Len), """");
            Idx2 := Index (Line (Idx0 .. Idx1), "$this.");
            if not (Idx1 in Idx0 + Test2'Length .. Len and then
                      not (Idx2 in Idx0 .. Idx1))
            then
               goto Continue_Loop;
            end if;

            declare
               Property : constant String := Line (Idx0 + Disp0 .. Idx1 - 1);
            begin
               Idx0 := Index (Property, ".");
               if Idx0 not in Property'Range then
                  goto Continue_Loop;
               end if;

               declare
                  PName : constant String :=
                    +Property (Property'First .. Idx0 - 1);
                  PAttrib : constant String :=
                    Property (Idx0 + 1 .. Property'Last);
                  WT      : Widget_Pointer;
                  Attr    : Widget_Attribute_Enum;
               begin
                  if PName = "" then
                     Debug (NLin, Resx_File_Name & ".resx"
                            & ": Line" & NLin'Image
                            &  ": cannot find widget name");
                     TIO.Close (RFile);
                     return -1;
                  end if;

                  if PAttrib = "" then
                     Debug (NLin, Resx_File_Name & ".resx"
                            & ": Line" & NLin'Image
                            &  ": cannot find widget property"
                            & " for " & PName);
                     TIO.Close (RFile);
                     return -1;
                  end if;

                  WT := Find_Widget (TWin.Widget_List, PName);
                  if WT = null then
                     if Contains (Line (1 .. Len), "BindingNavigator")
                     then
                        Debug (NLin, "Resource: Ignored Widget Property "
                               & Line (1 .. Len));
                        goto Continue_Loop;
                     end if;
                     Debug (NLin, Resx_File_Name & ".resx"
                            & ": Line" & NLin'Image
                            & ": widget " & PName
                            & " not in Designer");
                     TIO.Close (RFile);
                     return -1;
                  end if;

                  if Starts_With (PAttrib, "Items") then
                     if WT.Widget_Type = GtkComboTextBox then
                        if WT.Text_Buffer = null then
                           WT.Text_Buffer := new String'(Get_String (RFile));
                        else
                           declare
                              Temp : String_Access;
                           begin
                              Temp := new String'(WT.Text_Buffer.all
                                                  & ASCII.CR
                                                  & Get_String (RFile));
                              Free (WT.Text_Buffer);
                              WT.Text_Buffer := Temp;
                           end;
                        end if;
                     else
                        Debug (NLin, "Resource: Ignored Widget Property "
                               & PName & " " & PAttrib);
                     end if;
                     goto Continue_Loop;

                  elsif Contains (Property, "BindingNavigator") then
                     Debug (NLin, "Resource: Ignored Widget Property "
                            & PName & " " & PAttrib);
                     goto Continue_Loop;
                  end if;

                  Attr := Symbol_Tables.Get_Attribute (PAttrib);

                  case Attr is
                     when Attr_Anchor =>
                        WT.Anchor := new String'(Get_String (RFile));
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName & ".Anchor "
                               & WT.Anchor.all);

                     when Attr_ZOrder =>
                        WT.Zorder := Get_Integer (Get_String (RFile));
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName & ".ZOrder "
                               & Img (WT.Zorder));

                     when Attr_Margin | Attr_Padding =>
                        WT.Margins := Get_Margin_Array (RFile);
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName
                               & ". Margins "
                               & Img (WT.Margins (1)) & ", " &
                                 Img (WT.Margins (2)) & ", " &
                                 Img (WT.Margins (3)) & ", " &
                                 Img (WT.Margins (4)));

                     when Attr_TabIndex =>
                        WT.TabIndex := Get_Integer (Get_String (RFile));
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName & ".TabIndex "
                               & Img (WT.TabIndex));

                     when Attr_Text =>
                        if WT.Widget_Type = GtkTabChild then
                           WT.Text := new String'(Trim (Get_String (RFile),
                                                  Ada.Strings.Both));
                        else
                           WT.Text := new String'(Trim (Get_String (RFile),
                                                  Ada.Strings.Both));
                        end if;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName & ".Text "
                               & WT.Text.all);

                     when Attr_MaxLength =>
                        WT.MaxLength := Get_Integer (Get_String (RFile));
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName & ".MaxLength "
                               & Img (WT.MaxLength));

                     when Attr_ToolTip =>
                        WT.ToolTip := new String'(Get_String (RFile));
                        Have.Tooltips := Have.Tooltips + 1;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName & ".ToolTip "
                               & WT.ToolTip.all);

                     when Attr_ToolTipText =>
                        WT.ToolTip := new String'(Get_String (RFile));
                        Have.Column_Tooltips := Have.Column_Tooltips + 1;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName & ".ToolTipText "
                               & WT.ToolTip.all);

                     when Attr_AutoToolTip =>
                        WT.AutoToolTip := Get_Boolean (RFile);
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName & ".AutoToolTip "
                               & WT.AutoToolTip'Image);

                     when Attr_AutoSize =>
                        WT.AutoSize := Get_Boolean (RFile);
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName & ".Autosize "
                               & WT.AutoSize'Image);

                     when Attr_Enabled =>
                        WT.Enabled := Get_Boolean (RFile);
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName & ".Enabled "
                               & WT.Enabled'Image);

                     when Attr_UserAddedColumn =>
                        WT.UserAddedColumn := Get_Boolean (RFile);
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName & ".UserAddedColumn "
                               & WT.UserAddedColumn'Image);

                     when Attr_Visible =>
                        WT.Visible := Get_Boolean (RFile);
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName & ".Visible "
                               & WT.Visible'Image);

                     when Attr_TextAlign | Attr_BoxTextAlign =>
                        TAIO.Get (Get_String (RFile), WT.TextAlign, Idx2);
                        declare
                           Temp : String (1 .. 12);
                        begin
                           TAIO.Put (Temp, WT.TextAlign);
                           Debug (NLin, Sp (3) & "Set Widget Property "
                                  & PName & ".TextAlign "
                                  & Temp);
                        end;

                     when Attr_CheckAlign =>
                        WT.CheckAlign := new String'(Get_String (RFile));
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName & ".CheckAlign "
                               & WT.CheckAlign.all);

                     when Attr_Name =>
                        if PName /= +Get_String (RFile) then
                           Debug (NLin, "Line" & NLin'Image & ": " &
                                    "name mistmatch");
                           TIO.Close (RFile);
                           return -1;
                        end if;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName & ".Name "
                               & WT.Name.all);

                     when Attr_Size =>
                        P0 := Get_Pair (Get_String (RFile));
                        WT.Size.Horiz := P0.One;
                        WT.Size.Vert  := P0.Two;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName & ".Size H=" &
                                 Img (P0.One) & ", V=" & Img (P0.Two));

                     when Attr_Location =>
                        P0 := Get_Pair (Get_String (RFile));
                        WT.Location.From_Top := P0.Two;
                        WT.Location.From_Left := P0.One;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName
                               &  ".Location H="
                               & Img (P0.One) & ", V=" & Img (P0.Two));

                     when Attr_Type =>
                        if WT.Windows_Type = null then
                           WT.Windows_Type :=
                             new String'(Get_Widget_Name (RFile));
                           Debug (NLin, Sp (3) & "Set Widget Property "
                                  & PName & ".Type "
                                  & WT.Windows_Type.all);
                        else
                           declare
                              WType : constant String :=
                                Get_Widget_Name (RFile);
                           begin
                              if WType /= WT.Windows_Type.all then
                                 Debug (NLin, "Line" & NLin'Image & ": " &
                                                 "windows type mistmatch");
                                 TIO.Close (RFile);
                                 return -1;
                              end if;
                           end;
                        end if;

                     when Attr_Parent =>
                        WT.Parent_Name := new String'(+Get_String (RFile));
                        if WT.Parent_Name.all = "_This" then
                           WT.Parent_Name.all := "$this";
                        end if;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName
                               & ".Parent Name "
                               & WT.Parent_Name.all);

                     when Attr_Font =>
                        Get_Font (RFile,
                                  WT.Font_Name,
                                  WT.Font_Size,
                                  WT.Font_Weight,
                                  WT.Font_Underline);
                        if WT.Font_Weight /= null then
                           Have.Font_Weight := Have.Font_Weight + 1;
                        end if;
                        if WT.Font_Weight /= null then
                           Debug (NLin, Sp (3) & "Set Widget Property "
                                  & PName & ".Font "
                                  & WT.Font_Name.all
                                  & " " & Img (WT.Font_Size)
                                  & " " & WT.Font_Weight.all
                                  & (if WT.Font_Underline then
                                   " underline" else ""));
                        else
                           Debug (NLin, Sp (3) & "Set Widget Property "
                                  & PName & ".Font "
                                  & WT.Font_Name.all
                                  & " " & Img (WT.Font_Size)
                                  & (if WT.Font_Underline then
                                   " underline" else ""));
                        end if;

                     when Attr_PasswordChar =>
                        WT.PasswordChar :=
                          new String'(Get_String (RFile));
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName
                               & ".PasswordChar" & WT.PasswordChar.all);

                     when Attr_OpenFile =>
                        WT.OpenFileDialog :=
                          new String'(Get_String (RFile));
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName
                               & ".OpenFile " & WT.OpenFileDialog.all);

                     when Attr_TrayLocation =>
                        P0 := Get_Pair (Get_String (RFile));
                        WT.TrayLocation.From_Top  := P0.Two;
                        WT.TrayLocation.From_Left := P0.One;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName
                               & ".TrayLocation H="
                               & Img (P0.One)
                               & ", V=" & Img (P0.Two));

                     when Attr_Filter =>
                        WT.OpenFileFilter :=
                          new String'(Get_String (RFile));
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName
                               & ".OpenFileFilter "
                               & WT.OpenFileFilter.all);

                     when Attr_Title =>
                        WT.OpenFileTitle :=
                          new String'(Get_String (RFile));
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName
                               & ".OpenFileTitle "
                               & WT.OpenFileTitle.all);

                     when Attr_ImageAlign =>
                        WT.ImageAlign := Convert (Get_String (RFile));
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".ImageAlign "
                               & WT.ImageAlign'Image);

                     when Attr_FixedWidth =>
                        WT.Fixed_Width :=
                          Get_Integer (Get_String (RFile));
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName & ".Fixed_Width "
                               & Img (WT.Fixed_Width));

                     when Attr_MinimumWidth =>
                        WT.Min_Width :=
                          Get_Integer (Get_String (RFile));
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName & ".Min_Width "
                               & Img (WT.Min_Width));

                     when Attr_MaximumWidth =>
                        WT.Max_Width :=
                          Get_Integer (Get_String (RFile));
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName & ".Max_Width "
                               & Img (WT.Max_Width));

                     when Attr_HeaderText =>
                        WT.Text := new String'(Trim (Get_String (RFile),
                                               Ada.Strings.Both));
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName & ".Text "
                               & WT.Text.all);

                     when Attr_ItemSize =>
                        P0 := Get_Pair (Get_String (RFile));
                        WT.Size.Horiz := P0.One;
                        WT.Size.Vert  := P0.Two;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & PName & ".Size H="
                               & Img (P0.One) & ", V=" & Img (P0.Two));

                     when Attr_Ignored | Attr_Image | Attr_Locked =>
                        Debug (NLin, Resx_File_Name & ".resx"
                               & ": ignored widget property "
                               & PName & " " & PAttrib);

                     when others =>
                        Debug (NLin, Resx_File_Name & ".resx"
                                      & ": Line" & NLin'Image &
                                        ": unknown widget property: " &
                                        Property);
                        TIO.Close (RFile);
                        return -1;
                  end case;
               exception
                  when Constraint_Error =>
                     TIO.Close (RFile);
                     Debug (NLin, "Constraint Error");
                     Debug (NLin, Resx_File_Name & ".resx"
                            & ": Line:" & NLin'Image
                            & ": " & Property);
                     return -1;
               end;
            end;
            << Continue_Loop >>
         end loop;
         return 0;
      exception
         when Constraint_Error =>
            TIO.Close (RFile);
            Debug (NLin, "Constraint Error");
            Debug (NLin, Resx_File_Name & ".resx"
                   & ": Line" & NLin'Image
                   & " " & Line (1 .. Len));
            return -1;
      end Parse_Widget_Properties;

   begin
      Result := Parse_Window_Properties (TWin, Resx_Path, Resx_File_Name);
      if Result /= 0 then
         return Result;
      end if;

      Result := Parse_Widget_Properties (TWin, Resx_Path, Resx_File_Name);

      Debug (-1, "End of Parsing Resources");
      return Result;
   end Parse_Resource_File;

   -------------------------------------------------------------------------
   function Parse2_Designer_File (TWin           : Window_Pointer;
                                  Resx_Path      : String;
                                  Resx_File_Name : String;
                                  Icon_Path      : String) return Integer is
      use GNAT.Calendar.Time_IO;
      package IIO renames Ada.Integer_Text_IO;

      Idx0  : Integer;
      Idx1  : Integer;
      Idx2  : Integer;
      Idx3  : Integer;
      WT    : Widget_Pointer;
      Attr  : Widget_Attribute_Enum;
      Ret   : Integer;
      Num  : Integer;
      Last : Integer;

      procedure Get_Line;
      procedure Get_Line is
      begin
         TIO.Get_Line (DFile, Line, Len);
         NLin := NLin + 1;
         if Len > 0 then
            if Line (Len) = ASCII.CR then
               Len := Len - 1;
            end if;
         end if;
      end Get_Line;

      procedure Add_Items (Parent : Widget_Pointer);
      procedure Add_Items (Parent : Widget_Pointer) is
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
            --  exit when Idx1 not in Idx0 .. Len;
            if Idx1 not in Idx0 .. Len then
               Idx1 := Index (Line (Idx0 .. Len), "}");
            end if;
            Child := Find_Widget (TWin.Widget_List, +Line (Idx0 .. Idx1 - 1));
            if Child = null then
               Debug (NLin, Line (Idx0 .. Len) & ": widget not found");
               raise Constraint_Error;
            end if;
            if Child.Parent_Name /= null then
               if Child.Parent_Name.all /= Parent.Name.all then
                  Debug (NLin, "Designer 2: " & Img (NLin) & ": "
                         & "Warning: " & Child.Name.all & ": "
                         & "Mismatch with Resource File: "
                         & Child.Parent_Name.all & " / "
                         & Parent.Name.all);
               end if;
               Free (Child.Parent_Name);
            end if;
            Child.Parent_Name := new String'(Parent.Name.all);
            Parent.Num_Children := Parent.Num_Children + 1;
            Child.Child_Number := Parent.Num_Children;
            if Parent.Widget_Type = GtkDataGridView
              and then Child.Widget_Type = ExpandableColumn
            then
               Parent.Has_Expander := True;
            end if;
            Debug (NLin, Sp (3) & "Set Widget Property "
                   & Child.Name.all
                   & ".Parent_Name "
                   & Parent.Name.all
                   & ", child number=" & Img (Child.Child_Number));
            Idx0 := Idx1 + 1; -- skip semicolon
            exit when Line (Idx0) = ')';
         end loop;
      end Add_Items;

      function Process_Widget return Integer;
      function Process_Widget return Integer is
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
            Debug (NLin, "Ill-formed date:" & Line (Idx .. Len));
            raise Constraint_Error;
         end Get_Date;

         procedure Parse_One_Member (Parent : Widget_Pointer);
         procedure Parse_One_Member (Parent : Widget_Pointer) is
            Child : Widget_Pointer;
         begin
            Idx2 := Index (Line (Idx0 .. Len), Test14);
            Idx2 := Idx2 + Test14'Length;
            Idx3 := Index (Line (Idx2 .. Len), ")") - 1;
            Child := Find_Widget (TWin.Widget_List, +Line (Idx2 .. Idx3));
            if Child = null then
               Debug (NLin, Line (Idx0 + 1 .. Len) & ": widget not found");
               raise Constraint_Error;
            end if;
            if Child.Parent_Name /= null then
               if Child.Parent_Name.all /= Parent.Name.all then
                  Debug (NLin, "Designer 2: " & Img (NLin) & ": "
                         & "Warning: " & Child.Name.all & ": "
                         & "Mismatch with Resource File: "
                         & Child.Parent_Name.all & " / "
                         & Parent.Name.all);
               end if;
               Free (Child.Parent_Name);
            end if;
            Child.Parent_Name := new String'(Parent.Name.all);
            Parent.Num_Children := Parent.Num_Children + 1;
            Child.Child_Number := Parent.Num_Children;
            Debug (NLin, Sp (3) & "Set Widget Property "
                   & Child.Name.all
                   & ".Parent_Name "
                   & WT.Name.all);
         end Parse_One_Member;

         function Check_DataGridViewCellStyle return Integer;
         function Check_DataGridViewCellStyle return Integer is
            DGVS_Num : Integer;
            Last     : Integer;
            DGVS_Attr : DGVS_Attribute_Enum;
         begin
            Idx0 := Index (Line (1 .. Len), Sp (8) & Test20);
            if Idx0 not in 1 .. Len then
               return 1; --  continue reading
            end if;
            Idx0 := Idx0 + 8; --  to deal with the leading spaces
            Idx1 := Index (Line (Idx0 + Test20'Length .. Len), ".");
            IIO.Get (Line (Idx0 + Test20'Length .. Len), DGVS_Num, Last);
            if DGVS_Num > Max_DGVS then
               Debug (NLin, Sp (3) & ": "
                             & Line (Idx0 + Test20'Length .. Idx1 - 1)
                             & ": DataGridViewStyle not found");
               return -1;
            end if;
            Idx1 := Idx1 + 1;
            Idx2 := Index (Line (Idx1 .. Len), " = ");
            DGVS_Attr := Symbol_Tables.Get_DGVS_Attribute (Line (Idx1 ..
                                                             Idx2 - 1));
            Idx2 := Idx2 + 3;

            case DGVS_Attr is
               when DGVS_Attr_Alignment =>
                  Idx3 := Index (Line (Idx2 .. Len), "Alignment");
                  if Idx3 not in Idx1 .. Len then
                     raise TIO.Data_Error;
                  end if;
                  DGVS (DGVS_Num).Alignment :=
                    new String'(Line (Idx3 + 10 .. Len));
                  DGVS (DGVS_Num).Emit := True;
                  Debug (NLin, Sp (3) & "Set DGVS property "
                         & "DataGridViewStyle" & Img (DGVS_Num)
                         & ".Alignment "
                         & DGVS (DGVS_Num).Alignment.all);

               when DGVS_Attr_BgColor =>
                  DGVS (DGVS_Num).BgColor :=
                    new String'(To_Color (Line (Idx2 .. Len)));
                  DGVS (DGVS_Num).Emit := True;
                  Debug (NLin, Sp (3) & "Set DGVS Property "
                         & "DataGridViewStyle" & Img (DGVS_Num)
                         & ".BgColor "
                         & DGVS (DGVS_Num).BgColor.all);

               when DGVS_Attr_FgColor =>
                  DGVS (DGVS_Num).FgColor :=
                    new String'(To_Color (Line (Idx2 .. Len)));
                  DGVS (DGVS_Num).Emit := True;
                  Debug (NLin, Sp (3) & "Set DGVS Property "
                         & "DataGridViewStyle" & Img (DGVS_Num)
                         & ".FgColor "
                         & DGVS (DGVS_Num).FgColor.all);

               when DGVS_Attr_SelBgColor =>
                  DGVS (DGVS_Num).SelBgColor :=
                    new String'(To_Color (Line (Idx2 .. Len)));
                  DGVS (DGVS_Num).Emit := True;
                  Debug (NLin, Sp (3) & "Set DGVS Property "
                         & "DataGridViewStyle" & Img (DGVS_Num)
                         & ".SelBgColor "
                         & DGVS (DGVS_Num).SelBgColor.all);

               when DGVS_Attr_SelFgColor =>
                  DGVS (DGVS_Num).SelFgColor :=
                    new String'(To_Color (Line (Idx2 .. Len)));
                  DGVS (DGVS_Num).Emit := True;
                  Debug (NLin, Sp (3) & "Set DGVS Property "
                         & "DataGridViewStyle" & Img (DGVS_Num)
                         & ".SelFgColor "
                         & DGVS (DGVS_Num).FgColor.all);

               when DGVS_Attr_Font =>
                  Get_Font (Data        => Line (Idx2 .. Len),
                            Font_Name   => DGVS (DGVS_Num).Font_Name,
                            Font_Size   => DGVS (DGVS_Num).Font_Size,
                            Font_Weight => DGVS (DGVS_Num).Font_Weight,
                            Font_Underline => DGVS (DGVS_Num).Font_Underline);
                  DGVS (DGVS_Num).Emit := True;
                  if DGVS (DGVS_Num).Font_Underline then
                     Have.Font_Underline := Have.Font_Underline + 1;
                  end if;
                  if DGVS (DGVS_Num).Font_Weight /= null then
                     Have.Font_Weight := Have.Font_Weight + 1;
                  end if;
                  if DGVS (DGVS_Num).Font_Weight /= null then
                     Debug (NLin, Sp (3) & "Set DGVS Property "
                            & "DataGridViewStyle" & Img (DGVS_Num)
                            & ".Font " & DGVS (DGVS_Num).Font_Name.all
                            & " " & Img (DGVS (DGVS_Num).Font_Size)
                            & " " & DGVS (DGVS_Num).Font_Weight.all
                            & (if DGVS (Num).Font_Underline then
                             " underline" else ""));
                  else
                     Debug (NLin, Sp (3) & "Set DGVS Property "
                            & "DataGridViewStyle" & Img (DGVS_Num)
                            & ".Font " & DGVS (DGVS_Num).Font_Name.all
                            & " " & Img (DGVS (DGVS_Num).Font_Size)
                            & (if DGVS (DGVS_Num).Font_Underline then
                             " underline" else ""));
                  end if;

               when DGVS_Attr_Format =>
                  DGVS (DGVS_Num).Format :=
                    To_Cell_Format_Enum (Line (Idx2 .. Len));
                  DGVS (DGVS_Num).Emit := True;
                  Debug (NLin, Sp (3) & "Set DGVS Property "
                         & "DataGridViewStyle" & Img (DGVS_Num)
                         & ".Format "
                         & DGVS (DGVS_Num).Format'Image);

               when DGVS_Attr_Padding =>
                  Idx3 := Index (Line (Idx2 .. Len), "(");
                  DGVS (DGVS_Num).Padding :=
                    Get_Margin_Array (Line (Idx3 + 1 .. Len - 1));
                  DGVS (DGVS_Num).Emit := True;
                  Debug (NLin, Sp (3) & "Set DGVS Property "
                         & "DataGridViewStyle" & Img (DGVS_Num)
                         & ". Margins "
                         & "Start " & Img (DGVS (DGVS_Num).Padding (1)) & ", "
                         & ", Top " & Img (DGVS (DGVS_Num).Padding (2)) & ", "
                         & ", End " & Img (DGVS (DGVS_Num).Padding (3)) & ", "
                         & ", Bottom " & Img (DGVS (DGVS_Num).Padding (4)));

               when DGVS_Attr_WrapMode =>
                  if Index (Line (Idx2 .. Len), "True") in Idx2 .. Len then
                     DGVS (DGVS_Num).WrapMode := True;
                     Debug (NLin, Sp (3) & "Set DGVS Property "
                            & "DataGridViewStyle" & Img (DGVS_Num)
                            & ".WrapMode "
                            & "True");
                  elsif Index (Line (Idx2 .. Len), "False") in Idx2 .. Len then
                     DGVS (DGVS_Num).WrapMode := False;
                     Debug (NLin, Sp (3) & "Set DGVS Property "
                            & "DataGridViewStyle" & Img (DGVS_Num)
                            & ".WrapMode "
                            & "False");
                  end if;

               when DGVS_Attr_NullValue =>
                  DGVS (DGVS_Num).NullValue := new String'(Line (Idx2 .. Len));
                  Debug (NLin, Sp (3) & "Set DGVS Property "
                         & "DataGridViewStyle" & Img (DGVS_Num)
                         & ".NullValue "
                         & DGVS (DGVS_Num).NullValue.all);

               when DGVS_Attr_Ignored =>
                  Debug (NLin, Resx_File_Name
                         & ".Designer.vb (2)"
                         & ": Ignored DGVS property: "
                         & "DataGridViewStyle" & Img (DGVS_Num)
                         & " " & Line (Idx0 .. Idx1 - 1));

               when DGVS_No_Attribute =>
                  Debug (NLin, Resx_File_Name
                         & ".Designer.vb (2)"
                         & ": Line" & NLin'Image
                         & ": unknown DGVS property: "
                         & Line (Idx1 .. Len));
            end case;
            return 0; --  skip to end of loop
         end Check_DataGridViewCellStyle;

         P0 : Pair;
      begin --  Process_Widget
         --  enter to read line with widget name
         if TIO.End_Of_File (DFile) then
            return 0;
         end if;
         Get_Line;
         Idx0 := Index (Line (1 .. Len), Test9);
         Idx0 := Idx0 + Test9'Length;
         WT := Find_Widget (TWin.Widget_List, +Line (Idx0 .. Len));
         if WT = null then
            if TWin.Name.all = +Line (Idx0 .. Len) then
               Len := 0;
               return 0;
            end if;
            Debug (NLin, "Designer 2: " & Img (NLin) & ": "
                   & Line (Idx0 .. Len)
                   & ": widget not found");
            return -1;
         end if;
         --  skip second test9-line
         if TIO.End_Of_File (DFile) then
            return 0;
         end if;
         Get_Line;
         --  read attributes
         loop
            exit when TIO.End_Of_File (DFile);
            Get_Line;
            --  first test9 of following widget
            exit when Line (Line'First .. Len) = Test9;
            Ret := Check_DataGridViewCellStyle;
            if Ret = 0 then
               goto Continue_Loop;
            elsif Ret = -1 then
               return -1;
            end if;
            --  Ret is 1, continue reading
            Idx0 := Index (To_Lower (Line (1 .. Len)),
                           To_Lower ("Me." & WT.Name.all & "."));
            if Idx0 not in Line'Range then
               goto Continue_Loop;
            end if;

            Idx0 := Idx0 + 3 + WT.Name.all'Length + 1;
            Idx1 := Index (Line (Idx0 .. Len), " = ");
            if Idx1 not in Idx0 .. Len then
               if Line (Idx0 .. Idx0 + Test3'Length - 1) = Test3 then
                  Idx1 := Idx0 + Test3'Length;
               elsif Line (Idx0 .. Idx0 + Test4'Length - 1) = Test4 then
                  Idx1 := Idx0 + Test4'Length;
               elsif Line (Idx0 .. Idx0 + Test6'Length - 1) = Test6 then
                  Idx1 := Idx0 + Test6'Length;
               elsif Contains (Line (Idx0 .. Len), Test14)
                 and then
                   (WT.Widget_Type = GtkFrame or
                      WT.Widget_Type = GtkTabPage or
                        WT.Widget_Type = GtkNoteBook or
                          WT.Widget_Type = GtkBox)
               then
                  Parse_One_Member (WT);
                  goto Continue_Loop;
               elsif Contains (Line (Idx0 .. Len), Test15)
                 or else
                   Contains (Line (Idx0 .. Len), Test16)
                   or else
                     Contains (Line (Idx0 .. Len), Test17)
               then
                  Debug (NLin, Resx_File_Name
                         & ".Designer.vb (2)"
                         & ": Line" & NLin'Image
                         & ": Designer: Ignored Widget Property: "
                         & WT.Name.all & " " & Line (Idx0 .. Len));
                  goto Continue_Loop;

               else
                  Debug (NLin, Resx_File_Name
                         & ".Designer.vb (2)"
                         & ": Line" & NLin'Image
                         & ": Designer: cannot parse "
                         & Trim (Line (1 .. Len), Ada.Strings.Both));
                  return -1;
               end if;
            end if;
            if Idx1 not in Idx0 .. Len then
               goto Continue_Loop;
            end if;

            Attr := Symbol_Tables.Get_Attribute (Line (Idx0 .. Idx1 - 1));
            if Attr = Attr_AutoSizeMode and then
              (WT.Widget_Type in ExpandableColumn | DataGridViewTextBoxColumn
                                     | DataGridViewCheckBoxColumn)
            then
               Attr := Attr_AutoSizeColumnMode;
            end if;
            Idx1 := Idx1 + 3;

            case Attr is
               when Attr_Text =>
                  if WT.Text /= null
                    and then WT.Text.all /= Line (Idx1 + 1 .. Len - 1)
                  then
                     Debug (NLin, ": text mistmatch between "
                            & "Designer and Resource");
                     raise Constraint_Error;
                  else
                     WT.Text := new String'(Trim (Line (Idx1 + 1 .. Len - 1),
                                            Ada.Strings.Both));
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".Text "
                            & Line (Idx1 + 1 .. Len - 1));
                  end if;

               when Attr_Name =>
                  if WT.Name.all /= +Line (Idx1 + 1 .. Len - 1) then
                     Debug (NLin, ": name mistmatch between "
                            & "Designer and Resource");
                     raise Constraint_Error;
                  end if;
                  Debug (NLin, Sp (3) & "Set Widget Property "
                         & WT.Name.all & ".Name "
                         & Line (Idx1 + 1 .. Len - 1));

               when Attr_Location =>
                  Idx2 := Index (Line (Idx1 .. Len), "(");
                  P0 := Get_Pair (Line (Idx2 + 1 .. Len));
                  WT.Location.From_Top  := P0.Two;
                  WT.Location.From_Left := P0.One;
                  Debug (NLin, Sp (3) & "Set Widget Property "
                         & WT.Name.all
                         & ".Location H="
                         & Img (P0.One) & ", V=" & Img (P0.Two));

               when Attr_Size =>
                  Idx2 := Index (Line (Idx1 .. Len), "(");
                  P0 := Get_Pair (Line (Idx2 + 1 .. Len));
                  WT.Size.Horiz := P0.Two;
                  WT.Size.Vert  := P0.One;
                  Debug (NLin, Sp (3) & "Set Widget Property "
                         & WT.Name.all
                         & ".Size H="
                         & Img (P0.One) & ", V=" & Img (P0.Two));

               when Attr_FlowDirection =>
                  if Contains (Line (Idx1 .. Len), "TopDown") then
                     WT.FlowDirection := TopDown;
                     if WT.Widget_Type = GtkBox then
                        WT.Orientation := Vertical;
                     end if;
                  elsif Contains (Line (Idx1 .. Len), "RightToLeft") then
                     WT.FlowDirection := RightToLeft;
                     if WT.Widget_Type = GtkBox then
                        WT.Orientation := Horizontal;
                     end if;
                  elsif Contains (Line (Idx1 .. Len), "LeftToRight") then
                     WT.FlowDirection := LeftToRight;
                     if WT.Widget_Type = GtkBox then
                        WT.Orientation := Horizontal;
                     end if;
                  elsif Contains (Line (Idx1 .. Len), "BottomUp") then
                     WT.FlowDirection := BottomUp;
                     if WT.Widget_Type = GtkBox then
                        WT.Orientation := Vertical;
                     end if;
                  end if;
                  Debug (NLin, Sp (3) & "Set Widget Property "
                         & WT.Name.all
                         & ".Horizontal "
                         & WT.FlowDirection'Image);

               when Attr_TabIndex =>
                  WT.TabIndex := Get_Integer (Line (Idx1 .. Len));
                  Debug (NLin, Sp (3) & "Set Widget Property "
                         & WT.Name.all
                         & ".TabIndex "
                         & Img (WT.TabIndex));

               when Attr_Items_AddRange =>
                  case WT.Widget_Type is
                     when GtkToolBar | BindingNavigator
                        | GtkMenuBar | GtkStatusBar =>
                        Add_Items (WT);
                     when GtkComboTextBox =>
                        null;
                     when others =>
                        raise Program_Error;
                  end case;

               when Attr_Columns_AddRange =>
                  case WT.Widget_Type is
                     when GtkTreeGridView | GtkDataGridView =>
                        Add_Items (WT);
                     when others =>
                        raise Program_Error;
                  end case;

               when Attr_DropDownItems_AddRange =>
                  if WT.Widget_Type = GtkMenuImageItem then
                     Add_Items (WT);
                  else
                     raise Program_Error;
                  end if;

               when Attr_DisplayStyle =>
                  WT.DStyle := Convert (Line (Idx1 .. Len));
                  Debug (NLin, Sp (3) & "Set Widget Property "
                         & WT.Name.all & ".DStyle "
                         & Line (Idx1 .. Len));

               when Attr_TabStop =>
                  if Line (Idx1 .. Len) = "False" then
                     WT.TabStop := False;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".TabStop False");
                  elsif Line (Idx1 .. Len) = "True" then
                     WT.TabStop := True;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".TabStop True");
                  end if;

               when Attr_AutoToolTip =>
                  if Line (Idx1 .. Len) = "False" then
                     WT.AutoToolTip := False;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".AutoToolTip False");
                  elsif Line (Idx1 .. Len) = "True" then
                     WT.AutoToolTip := True;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".AutoToolTip True");
                     if WT.ToolTip = null then
                        WT.ToolTip := new String'(WT.Name.all);
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".+ "
                               & WT.Name.all);
                     end if;
                  end if;

               when Attr_BorderStyle =>
                  if WT.Widget_Type = GtkEntry then
                     Idx2 := Index (Line (Idx1 .. Len),
                                    "BorderStyle.None");
                     if Idx2 in Idx1 .. Len then
                        WT.Has_Frame := False;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".Has_Frame False");
                     end if;

                  elsif WT.Widget_Type = GtkLabel then
                     Idx2 := Index (Line (Idx1 .. Len), "BorderStyle");
                     if Idx2 not in Idx1 .. Len then
                        raise TIO.Data_Error;
                     end if;
                     if Contains (Line (Idx2 + 12 .. Len), "None") then
                        WT.BorderStyle := None;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".BorderStyle "
                               & "None");
                     elsif Contains (Line (Idx2 + 12 .. Len), "FixedSingle")
                     then
                        WT.BorderStyle := FixedSingle;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".BorderStyle "
                               & "FixedSingle");
                     elsif Contains (Line (Idx2 + 12 .. Len), "Fixed3D") then
                        WT.BorderStyle := Fixed3D;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".BorderStyle "
                               & "Fixed3D");
                     else
                        raise TIO.Data_Error;
                     end if;
                  end if;

               when Attr_Alignment =>
                  Idx2 := Index (Line (Idx1 .. Len), "Alignment");
                  if Idx2 not in Idx1 .. Len then
                     raise TIO.Data_Error;
                  end if;
                  TAIO.Get (Line (Idx2 + 10 .. Len), WT.TextAlign, Idx0);
                  declare
                     Temp : String (1 .. 12);
                  begin
                     TAIO.Put (Temp, WT.TextAlign);
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".TextAlign "
                            & Temp);
                  end;

               when Attr_ImageScalingSize =>
                  Idx2 := Index (Line (Idx1 .. Len), "(");
                  WT.ImageScalingSize := Get_Pair (Line (Idx2 + 1 .. Len - 1));
                  Debug (NLin, Sp (3) & "Set Widget Property "
                         & WT.Name.all & ".ImageScalingSize "
                         & Img (WT.ImageScalingSize.One)
                         & ", " & Img (WT.ImageScalingSize.Two));

               when Attr_GripStyle =>
                  if WT.Widget_Type = GtkToolBar then
                     if Contains (Line (Idx1 .. Len), "Hidden") then
                        WT.Grip_Visible := False;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".Grip_Visible False");
                     elsif Contains (Line (Idx1 .. Len), "Visible") then
                        WT.Grip_Visible := True;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".Grip_Visible True");
                     end if;
                  end if;

               when Attr_Checked =>
                  if (WT.Widget_Type = GtkRadioButton or
                        WT.Widget_Type = GtkButton or
                          WT.Widget_Type = GtkCheckButton or
                            WT.Widget_Type = GtkToggleButton)
                  then
                     if Contains (Line (Idx1 .. Len), "False") then
                        WT.Active := False;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".Active False");
                     elsif Contains (Line (Idx1 .. Len), "True") then
                        WT.Active := True;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".Active True");
                     end if;
                  end if;

               when Attr_CheckState =>
                  if (WT.Widget_Type = GtkRadioButton or
                        WT.Widget_Type = GtkButton or
                          WT.Widget_Type = GtkCheckButton or
                            WT.Widget_Type = GtkToggleButton)
                  then
                     if Contains (Line (Idx1 .. Len), "Checked") then
                        WT.Active := True;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".Active True");
                     else
                        WT.Active := False;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".Active False");
                     end if;
                  end if;

               when Attr_AnyColor =>
                  if WT.Widget_Type = GtkColorButton then
                     if Contains (Line (Idx1 .. Len), "False") then
                        WT.AnyColor := False;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".AnyColor False");
                     elsif Contains (Line (Idx1 .. Len), "True") then
                        WT.AnyColor := True;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".AnyColor True");
                     end if;
                  end if;

               when Attr_FullOpen =>
                  if WT.Widget_Type = GtkColorButton then
                     if Contains (Line (Idx1 .. Len), "False") then
                        WT.FullOpen := False;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".FullOpen False");
                     elsif Contains (Line (Idx1 .. Len), "True") then
                        WT.FullOpen := True;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".FullOpen True");
                     end if;
                  end if;

               when Attr_UseSystemPasswordChar =>
                  if  WT.Widget_Type = GtkEntry then
                     if Contains (Line (Idx1 .. Len), "True") then
                        WT.PasswordChar := new String'("");
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".PasswordChar");
                     end if;
                  end if;

               when Attr_ReadOnly =>
                  case WT.Widget_Type is
                     when GtkEntry | GtkComboTextBox =>
                        if Contains (Line (Idx1 .. Len), "False") then
                           WT.Editable := True;
                           Debug (NLin, Sp (3) & "Set Widget Property "
                                  & WT.Name.all & ".Editable False");
                        elsif Contains (Line (Idx1 .. Len), "True") then
                           WT.Editable := False;
                           Debug (NLin, Sp (3) & "Set Widget Property "
                                  & WT.Name.all & ".Editable True");
                        end if;

                     when GtkDataGridView | GtkTreeGridView
                        | ExpandableColumn | DataGridViewTextBoxColumn
                        | DataGridViewCheckBoxColumn  =>
                        if Contains (Line (Idx1 .. Len), "False") then
                           WT.ReadOnly := False;
                           Debug (NLin, Sp (3) & "Set Widget Property "
                                  & WT.Name.all & ".ReadOnly False");
                        elsif Contains (Line (Idx1 .. Len), "True") then
                           WT.ReadOnly := True;
                           Debug (NLin, Sp (3) & "Set Widget Property "
                                  & WT.Name.all & ".ReadOnly True");
                        end if;
                     when others => null;
                  end case;

               when Attr_Sorted =>
                  if WT.Widget_Type = GtkComboTextBox then
                     if Contains (Line (Idx1 .. Len), "False") then
                        WT.Sorted := False;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".Sorted False");
                     elsif Contains (Line (Idx1 .. Len), "True") then
                        WT.Sorted := True;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".Sorted True");
                     end if;
                  end if;

               when Attr_Maximum =>
                  if WT.Widget_Type = GtkSpinButton then
                     Idx2 := Idx1 + Test10'Length;
                     Idx3 := Index (Line (Idx2 .. Len), ",");
                     WT.MaxValue := Get_Integer (Line (Idx2 .. Idx3 - 1));
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".Maximum "
                            & Img (WT.MaxValue));
                  end if;

               when Attr_Minimum =>
                  if WT.Widget_Type = GtkSpinButton then
                     Idx2 := Idx1 + Test10'Length;
                     Idx3 := Index (Line (Idx2 .. Len), ",");
                     WT.MinValue := Get_Integer (Line (Idx2 .. Idx3 - 1));
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".Minimum "
                            & Img (WT.MinValue));
                  end if;

               when Attr_Value =>
                  if WT.Widget_Type = GtkSpinButton then
                     Idx2 := Idx1 + Test10'Length;
                     Idx3 := Index (Line (Idx2 .. Len), ",");
                     WT.StartValue := Get_Integer (Line (Idx2 .. Idx3 - 1));
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".StartValue "
                            & Img (WT.StartValue));
                  elsif WT.Widget_Type = GtkCalendar then
                     WT.Start_Date := Get_Date (Idx1);
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".Start_Date "
                            & Image (WT.Start_Date, ISO_Date));
                  end if;

               when Attr_Format =>
                  if WT.Widget_Type = GtkCalendar then
                     WT.Format_Date := new String'(Line (Idx1 .. Len));
                     if Index (Line (Idx1 .. Len), "Format.Time") > 0 then
                        WT.Is_DatePicker := False;
                     end if;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".Format_Date "
                            & WT.Format_Date.all);
                  end if;

               when Attr_MinDate =>
                  if WT.Widget_Type = GtkCalendar then
                     WT.MinDate := Get_Date (Idx1);
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".Min_Date "
                            & Image (WT.MinDate, ISO_Date));
                  end if;

               when Attr_MaxDate =>
                  if WT.Widget_Type = GtkCalendar then
                     WT.MaxDate := Get_Date (Idx1);
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".Max_Date "
                            & Image (WT.MaxDate, ISO_Date));
                  end if;

               when Attr_LimitDatePicket =>
                  if WT.Widget_Type = GtkCalendar then
                     WT.MaxDate := Get_Date (Idx1);
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".Limit_Date "
                            & Image (WT.MaxDate, ISO_Date));
                  end if;

               when Attr_Image =>
                  case WT.Widget_Type is
                     when GtkButton | GtkRadioButton
                        | GtkCheckButton | GtkToggleButton =>
                        WT.ImagePath :=
                          new String'(Icon_Path & "/"
                                      & Icon_Name (Line (Idx1 .. Len)));
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".ImagePath "
                               & WT.ImagePath.all);
                     when GtkImage =>
                        WT.Image :=
                          new String'(Icon_Path & "/"
                                      & Icon_Name (Line (Idx1 .. Len)));
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".Image "
                               & WT.Image.all);
                     when GtkMenuImageItem =>
                        WT.ImageMenu :=
                          new String'(Icon_Path & "/"
                                      & Icon_Name (Line (Idx1 .. Len)));
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".ImageMenu "
                               & WT.ImageMenu.all);

                     when others => null;
                  end case;

               when Attr_ImageAlign =>
                  if (WT.Widget_Type = GtkButton or
                        WT.Widget_Type = GtkRadioButton or
                          WT.Widget_Type = GtkCheckButton or
                            WT.Widget_Type = GtkToggleButton)
                  then
                     WT.ImageAlign := Convert (Line (Idx1 .. Len));
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".ImageAlign "
                            & Line (Idx1 .. Len));
                  end if;

               when Attr_ShowUpDown =>
                  if WT.Widget_Type = GtkCalendar then
                     if Contains (Line (Idx1 .. Len), "False") then
                        WT.ShowUpDown := False;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".ShowUpDown False");
                     elsif Contains (Line (Idx1 .. Len), "True") then
                        WT.ShowUpDown := True;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".ShowUpDown True");
                     end if;
                  end if;

               when Attr_SelectionMode =>
                  if WT.Widget_Type = GtkListBox then
                     WT.MultiSelect := True;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".MultiSelect True");
                  end if;

               when Attr_ForeColor =>
                  WT.FgColor :=
                    new String'(To_Color (Line (Idx1 .. Len)));
                  Debug (NLin, Sp (3) & "Set Widget Property "
                         & WT.Name.all & "." & "FgColor "
                         & WT.FgColor.all);

               when Attr_BackColor =>
                  WT.BgColor :=
                    new String'(To_Color (Line (Idx1 .. Len)));
                  Debug (NLin, Sp (3) & "Set Widget Property "
                         & WT.Name.all & "." & "BgColor "
                         & WT.BgColor.all);

               when Attr_Level =>
                  IIO.Get (Line (Idx1 .. Len), Num, Last);
                  WT.Level := Num;
                  Debug (NLin, Sp (3) & "Set Widget Property "
                         & WT.Name.all
                         & ".Level "
                         & Img (Num));

               when Attr_ScrollBars =>
                  Idx2 := Index (Line (Idx1 .. Len), ".",
                                 Ada.Strings.Backward);
                  if Contains (Line (Idx2 + 1 .. Len), "None") then
                     WT.ScrollBars := None;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".ScrollBars "
                            & "None");
                  elsif Contains (Line (Idx2 + 1 .. Len), "Both") then
                     WT.ScrollBars := Both;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".ScrollBars "
                            & "Vertical");
                  elsif Contains (Line (Idx2 + 1 .. Len), "Both") then
                     WT.ScrollBars := Both;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".ScrollBars "
                            & "Horizontal");
                  elsif Contains (Line (Idx2 + 1 .. Len), "Both") then
                     WT.ScrollBars := Both;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".ScrollBars "
                            & "Both");
                  else
                     raise TIO.Data_Error;
                  end if;

               when Attr_RowHeadersBorderStyle =>
                  Idx2 := Index (Line (Idx1 .. Len), ".",
                                 Ada.Strings.Backward);
                  if Contains (Line (Idx2 + 1 .. Len), "None") then
                     WT.RowHeadersBorderStyle := None;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".RowHeadersBorderStyle "
                            & "None");
                  elsif Contains (Line (Idx2 + 1 .. Len), "Sunken") then
                     WT.RowHeadersBorderStyle := Sunken;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".RowHeadersBorderStyle "
                            & "Sunken");
                  elsif Contains (Line (Idx2 + 1 .. Len), "Raised") then
                     WT.RowHeadersBorderStyle := Raised;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".RowHeadersBorderStyle "
                            & "Raised");
                  elsif Contains (Line (Idx2 + 1 .. Len), "Single") then
                     WT.RowHeadersBorderStyle := Single;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".RowHeadersBorderStyle "
                            & "Single");
                  elsif Contains (Line (Idx2 + 1 .. Len), "Custom") then
                     WT.RowHeadersBorderStyle := Custom;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".RowHeadersBorderStyle "
                            & "Custom");
                  else
                     raise TIO.Data_Error;
                  end if;

               when Attr_Frozen =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.Frozen := False;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all
                            & ".Frozen False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.Frozen := True;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all
                            & ".Frozen True");
                  end if;

               when Attr_WorkerReportsProgress =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.WorkerReportsProgress := False;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all
                            & ".WorkerReportsProgress False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.WorkerReportsProgress := True;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all
                            & ".WorkerReportsProgress True");
                  end if;

               when Attr_WorkerSupportsCancellation =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.WorkerSupportsCancellation := False;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all
                            & ".WorkerSupportsCancellation False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.WorkerSupportsCancellation := True;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all
                            & ".WorkerSupportsCancellation True");
                  end if;

               when Attr_EnableMetric =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.EnableMetric := False;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all
                            & ".EnableMetric False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.EnableMetric := True;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all
                            & ".EnableMetric True");
                  end if;

               when Attr_MinMargins =>
                  Idx3 := Index (Line (Idx1 .. Len), "(");
                  WT.MinMargins :=
                    Get_Margin_Array (Line (Idx3 + 1 .. Len - 1));
                  Debug (NLin, Sp (3) & "Set Widget Property "
                         & WT.Name.all
                         & ".MinMargins "
                         & Img (WT.MinMargins (1)) & ", "
                         & Img (WT.MinMargins (2)) & ", "
                         & Img (WT.MinMargins (3)) & ", "
                         & Img (WT.MinMargins (4)));

               when Attr_SelectedIndex =>
                  IIO.Get (Line (Idx1 .. Len), Num, Last);
                  WT.SelectedIndex := Num;
                  Debug (NLin, Sp (3) & "Set Widget Property "
                         & WT.Name.all
                         & ".SelectedIndex "
                         & Img (Num));

               when Attr_Padding =>
                  Idx2 := Index (Line (Idx1 .. Len), "(");
                  IIO.Get (Line (Idx2 + 1 .. Len), Num, Last);
                  WT.Padding := Num;
                  Debug (NLin, Sp (3) & "Set Widget Property "
                         & WT.Name.all
                         & ".Padding "
                         & Img (Num));

               when Attr_PaddingX =>
                  IIO.Get (Line (Idx1 .. Len), Num, Last);
                  WT.PaddingX := Num;
                  Debug (NLin, Sp (3) & "Set Widget Property "
                         & WT.Name.all
                         & ".PaddingX "
                         & Img (Num));

               when Attr_PaddingY =>
                  IIO.Get (Line (Idx1 .. Len), Num, Last);
                  WT.PaddingY := Num;
                  Debug (NLin, Sp (3) & "Set Widget Property "
                         & WT.Name.all
                         & ".PaddingY "
                         & Img (Num));

               when Attr_UseVisualStyleBackColor =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.UseVisualStyleBackColor := False;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all
                            & ".UseVisualStyleBackColor False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.UseVisualStyleBackColor := True;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all
                            & ".UseVisualStyleBackColor True");
                  end if;

               when Attr_EditModeProgramatically =>
                  WT.EditModeProgramatically := True;
                  Debug (NLin, Sp (3) & "Set Widget Property "
                         & WT.Name.all
                         & ".EditModeProgramatically True");

               when Attr_Resizable => null;
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.Resizable := False;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".Resizable False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.Resizable := True;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".Resizable True");
                  end if;

               when Attr_SortMode =>
                  Idx2 := Index (Line (Idx1 .. Len), ".",
                                 Ada.Strings.Backward);
                  if Contains (Line (Idx2 + 1 .. Len), "NotSortable") then
                     WT.SortMode := NotSortable;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".SortMode "
                            & "NotSortable");
                  elsif Contains (Line (Idx2 + 1 .. Len), "Automatic") then
                     WT.SortMode := Automatic;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".SortMode "
                            & "Automatic");
                  elsif Contains (Line (Idx2 + 1 .. Len), "Programmatic") then
                     WT.SortMode := Programmatic;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".SortMode "
                            & "Programmatic");
                  else
                     raise TIO.Data_Error;
                  end if;

               when Attr_AutoSizeMode =>
                  Idx2 := Index (Line (Idx1 .. Len), ".",
                                 Ada.Strings.Backward);
                  if Contains (Line (Idx2 + 1 .. Len), "GrowAndShrink") then
                     WT.AutoSizeMode := GrowAndShrink;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".AutoSizeMode "
                            & "GrowAndShrink");
                  elsif Contains (Line (Idx2 + 1 .. Len), "GrowOnly") then
                     WT.AutoSizeMode := GrowOnly;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".AutoSizeMode "
                            & "GrowOnly");
                  else
                     raise TIO.Data_Error;
                  end if;

               when Attr_AutoSizeColumnMode =>
                  WT.AutoSizeColumnMode :=
                    To_AutoSizeColumnMode (Line (1 .. Len));
                  declare
                     Temp : String (1 .. 26);
                  begin
                     ASCMIO.Put (Temp, WT.AutoSizeColumnMode);
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".AutoSizeColumnMode "
                            & Temp);
                  end;

               when Attr_AllowUserToAddRows =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.AllowUserToAddRows := False;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".AllowUserToAddRows False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.AllowUserToAddRows := True;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".AllowUserToAddRows True");
                  end if;

               when Attr_AllowUserToDeleteRows =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.AllowUserToDeleteRows := False;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".AllowUserToDeleteRows False");
                  elsif Contains (Line (Idx1 .. Len), "False") then
                     WT.AllowUserToAddRows := True;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".AllowUserToDeleteRows True");
                  end if;

               when Attr_EnableHeadersVisualStyles =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.EnableHeadersVisualStyles := False;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all
                            & ".EnableHeadersVisualStyles False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.EnableHeadersVisualStyles := True;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all
                            & ".EnableHeadersVisualStyles True");
                  end if;

               when Attr_MultiSelect =>
                  if (WT.Widget_Type = GtkTreeGridView or
                        WT.Widget_Type = GtkDataGridView)
                  then
                     if Contains (Line (Idx1 .. Len), "False") then
                        WT.RowMultiSelect := False;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".RowMultiSelect False");
                     elsif Contains (Line (Idx1 .. Len), "True") then
                        WT.RowMultiSelect := True;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".RowMultiSelect True");
                     end if;
                  elsif WT.Widget_Type = GtkListBox then
                     if Contains (Line (Idx1 .. Len), "False") then
                        WT.MultiSelect := False;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".MultiSelect False");
                     elsif Contains (Line (Idx1 .. Len), "True") then
                        WT.MultiSelect := True;
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all & ".MultiSelect True");
                     end if;
                  end if;

               when Attr_ColumnHeadersVisible =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.ColumnHeadersVisible := False;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".ColumnHeadersVisible False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.ColumnHeadersVisible := True;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".ColumnHeadersVisible True");
                  end if;

               when Attr_RowHeadersVisible =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.RowHeadersVisible := False;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".RowHeadersVisible False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.RowHeadersVisible := True;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".RowHeadersVisible True");
                  end if;

               when Attr_AllowUserToResizeRows =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.AllowUserToResizeRows := False;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".AllowUserToResizeRows False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.AllowUserToAddRows := True;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".AllowUserToResizeRows True");
                  end if;

               when Attr_AllowUserToOrderColumns =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.AllowUserToOrderColumns := False;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".AllowUserToOrderColumns False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.AllowUserToOrderColumns := True;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".AllowUserToOrderColumns True");
                  end if;

               when Attr_DefaultCellStyle =>
                  Idx2 := Index (Line (Idx1 .. Len), Test20);
                  if Idx2 in Idx1 .. Len then
                     IIO.Get (Line (Idx1 + Test20'Length .. Len),
                              Num, Last);
                     if Num not in DGVS'Range then
                        Debug (NLin, Resx_File_Name
                               & "Designer.vb (2:) "
                               & "No datagridviewcellstyle"
                               & Img (Num)
                               & " at line" & NLin'Image);
                        raise TIO.Data_Error;
                     end if;
                     if DGVS (Num).Name /= null then
                        Debug (NLin, Resx_File_Name
                               & ".Designer.vb (2)"
                               & ": Line" & NLin'Image
                               & ": Designer: repeated: "
                               & Line (Idx0 .. Len));
                        raise TIO.Data_Error;
                     end if;
                     WT.DefaultCellStyle := Num;
                     case WT.Widget_Type is
                        when ExpandableColumn | DataGridViewTextBoxColumn =>
                           DGVS (Num).Name :=
                             new String'("CRT_" & WT.Name.all);
                           DGVS (Num).Style_For := For_TextCell;
                        when DataGridViewCheckBoxColumn =>
                           DGVS (Num).Name :=
                             new String'("CRTG_" & WT.Name.all);
                           DGVS (Num).Style_For := For_ToggleCell;
                        when GtkDataGridView | GtkTreeGridView =>
                           DGVS (Num).Name :=
                             new String'("DEF_" & WT.Name.all);
                           DGVS (Num).Style_For := For_TreeGridView;
                        when others =>
                           null;
                     end case;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all
                            & ".DefaultCellStyle "
                            & Test20 & Img (Num));
                  else
                     raise TIO.Data_Error;
                  end if;

               when Attr_AlternatingRowsDefaultCellStyle =>
                  Idx2 := Index (Line (Idx1 .. Len), Test20);
                  if Idx2 in Idx1 .. Len then
                     IIO.Get (Line (Idx1 + Test20'Length .. Len),
                              Num, Last);
                     if Num not in DGVS'Range then
                        Debug (NLin, Resx_File_Name
                               & "Designer.vb (2:) "
                               & "No datagridviewcellstyle"
                               & Img (Num)
                               & " at line" & NLin'Image);
                        raise TIO.Data_Error;
                     end if;
                     if DGVS (Num).Name /= null then
                        Debug (NLin, Resx_File_Name
                               & ".Designer.vb (2)"
                               & ": Line" & NLin'Image
                               & ": Designer: repeated: "
                               & Line (Idx0 .. Len));
                        raise TIO.Data_Error;
                     end if;
                     WT.AlternatingRowsDefaultCellStyle := Num;
                     DGVS (Num).Name := new String'("ALT_" & WT.Name.all);
                     DGVS (Num).Style_For := For_TreeGridView;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all
                            & ".AlternatingRowsDefaultCellStyle "
                            & Test20 & Img (Num));
                  else
                     raise TIO.Data_Error;
                  end if;

               when Attr_ColumnHeadersDefaultCellStyle =>
                  Idx2 := Index (Line (Idx1 .. Len), Test20);
                  if Idx2 in Idx1 .. Len then
                     declare
                        Num  : Integer;
                        Last : Integer;
                     begin
                        IIO.Get (Line (Idx1 + Test20'Length .. Len),
                                 Num, Last);
                        if Num not in DGVS'Range then
                           Debug (NLin, Resx_File_Name
                                  & "Designer.vb (2:) "
                                  & "No datagridviewcellstyle"
                                  & Img (Num)
                                  & " at line" & NLin'Image);
                           raise TIO.Data_Error;
                        end if;
                        if DGVS (Num).Name /= null then
                           Debug (NLin, Resx_File_Name
                                  & ".Designer.vb (2)"
                                  & ": Line" & NLin'Image
                                  & ": Designer: repeated: "
                                  & Line (Idx0 .. Len));
                           raise TIO.Data_Error;
                        end if;
                        Have.HDR_CellRenderers := Have.HDR_CellRenderers + 1;
                        WT.ColumnHeadersDefaultCellStyle := Num;
                        DGVS (Num).Style_For := For_Column_Header;
                        DGVS (Num).Name := new String'("HDR_" & WT.Name.all);
                        Debug (NLin, Sp (3) & "Set Widget Property "
                               & WT.Name.all
                               & ".ColumnHeadersDefaultCellStyle "
                               & Test20 & Img (Num));
                     end;
                  else
                     raise TIO.Data_Error;
                  end if;

               when Attr_RowHeadersDefaultCellStyle =>
                  Idx2 := Index (Line (Idx1 .. Len), Test20);
                  if Idx2 in Idx1 .. Len then
                     IIO.Get (Line (Idx1 + Test20'Length .. Len),
                              Num, Last);
                     if Num not in DGVS'Range then
                        Debug (NLin, Resx_File_Name
                               & "Designer.vb (2:) "
                               & "No datagridviewcellstyle"
                               & Img (Num)
                               & " at line" & NLin'Image);
                        raise TIO.Data_Error;
                     end if;
                     if DGVS (Num).Name /= null then
                        Debug (NLin, Resx_File_Name
                               & ".Designer.vb (2)"
                               & ": Line" & NLin'Image
                               & ": Designer: repeated: "
                               & Line (Idx0 .. Len));
                        raise TIO.Data_Error;
                     end if;
                     WT.RowHeadersDefaultCellStyle := Num;
                     DGVS (Num).Style_For := For_Row_Header;
                     DGVS (Num).Name := new String'("ROW_" & WT.Name.all);
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all
                            & ".RowHeadersDefaultCellStyle "
                            & Test20 & Img (Num));
                  else
                     raise TIO.Data_Error;
                  end if;

               when Attr_CloseButtonOnTabsInactiveVisible =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.CloseButtonOnTabsInactiveVisible := False;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all
                            & ".CloseButtonOnTabsInactiveVisible False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.CloseButtonOnTabsInactiveVisible := True;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all
                            & ".CloseButtonOnTabsInactiveVisible True");
                  end if;

               when Attr_RowHeadersWidthSizeMode =>
                  Idx2 := Index (Line (Idx1 .. Len), ".",
                                 Ada.Strings.Backward);
                  if Contains (Line (Idx2 + 1 .. Len), "EnableResizing") then
                     WT.RowHeadersWidthSizeMode := EnableResizing;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".RowDeadersWidthSizeMode "
                            & "EnableResizing");
                  elsif Contains (Line (Idx2 + 1 .. Len), "DisableResizing")
                  then
                     WT.RowHeadersWidthSizeMode := DisableResizing;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".RowDeadersWidthSizeMode "
                            & "DisableResizing");
                  elsif Contains (Line (Idx2 + 1 .. Len),
                                  "AutoSizeToAllHeaders")
                  then
                     WT.RowHeadersWidthSizeMode := AutoSizeToAllHeaders;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".RowDeadersWidthSizeMode "
                            & "AutoSizeToAllHeaders");
                  elsif Contains (Line (Idx2 + 1 .. Len),
                                  "AutoSizeToDisplayedHeaders")
                  then
                     WT.RowHeadersWidthSizeMode := AutoSizeToDisplayedHeaders;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".RowDeadersWidthSizeMode "
                            & "AutoSizeToDisplayedHeaders");
                  elsif Contains (Line (Idx2 + 1 .. Len),
                                  "AutoSizeToFirstHeader")
                  then
                     WT.RowHeadersWidthSizeMode := AutoSizeToFirstHeader;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".RowDeadersWidthSizeMode "
                            & "AutoSizeToFirstHeader");
                  else
                     raise TIO.Data_Error;
                  end if;

               when Attr_ImageList | Attr_DefaultNodeImage =>
                  Debug (NLin, Sp (3) & "Set Widget Property "
                         & WT.Name.all
                         & ".ImageList null");

               when Attr_ColumnHeadersHeightSizeMode =>
                  if Contains (Line (Idx1 .. Len), "AutoSize") then
                     WT.ColumnHeadersHeightSizeMode := AutomaticSize;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".ColumnHeadersHeightSizeMode"
                            & " AutoSize");
                  elsif Contains (Line (Idx1 .. Len), "EnableResizing") then
                     WT.ColumnHeadersHeightSizeMode := EnableResizing;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".ColumnHeadersHeightSizeMode"
                            & " EnableResizing");
                  elsif Contains (Line (Idx1 .. Len), "DisableResizing") then
                     WT.ColumnHeadersHeightSizeMode := DisableResizing;
                     Debug (NLin, Sp (3) & "Set Widget Property "
                            & WT.Name.all & ".ColumnHeadersHeightSizeMode"
                            & " DisableResizing");
                  else
                     raise TIO.Data_Error;
                  end if;

               when Attr_AddNewItem | Attr_CountItem
                  | Attr_DeleteItem | Attr_MoveFirstItem
                  | Attr_MoveLastItem | Attr_MoveNextItem
                  | Attr_MovePreviousItem | Attr_PositionItem
                  =>
                  null;

               when Attr_Ignored =>
                  Debug (NLin, Resx_File_Name
                         & ".Designer.vb (2)"
                         & ": ignored property: "
                         & Line (Idx0 .. Len));

               when No_Attribute =>
                  Debug (NLin, Resx_File_Name
                         & ".Designer.vb (2)"
                         & ": Line" & NLin'Image
                         & ": Designer: unknown property: "
                         & Line (Idx0 .. Len));

               when others  =>
                  Debug (NLin, Resx_File_Name
                         & ".Designer.vb (2)"
                         & ": Line" & NLin'Image
                         & ": Designer: property not processed: "
                         & Line (Idx0 .. Len));

            end case;
            << Continue_Loop >>
         end loop;
         return 0;
      end Process_Widget;

   begin --  Parse2_Designer_File
      TIO.Open (File => DFile,
                Mode => TIO.In_File,
                Name => Resx_Path & "/" & Resx_File_Name & ".Designer.vb");

      NLin := 0;
      Debug (-1, "");
      Debug (-1, "Parsing (2) Designer");
      Debug (NLin, "Attributes");
      Ret := 0;
      while not TIO.End_Of_File (DFile) loop
         Get_Line;
         Idx0 := Index (Line (1 .. Len), "Me.SuspendLayout()");
         if Idx0 in Line'Range then
            exit when TIO.End_Of_File (DFile);
            Get_Line;
            loop
               Ret := Process_Widget;
               exit when Ret < 0;
               exit when Len <= 0;
               exit when TIO.End_Of_File (DFile);
            end loop;
         end if;
      end loop;

      Debug (-1, "End of Parsing (2) Designer");
      TIO.Close (DFile);
      return Ret;
   exception
      when Constraint_Error =>
         TIO.Close (DFile);
         Debug (NLin, "Constraint Error");
         Debug (NLin, Resx_File_Name & ".Designer.vb (2)"
                & ": Line" & NLin'Image
                & " " & Line (1 .. Len));
         return -1;
   end Parse2_Designer_File;

   -------------------------------------------------------------------------
   function Parse1_Designer_File (Resx_Path      : String;
                                  Resx_File_Name : String) return Integer is
      Idx0      : Integer;
      Idx1      : Integer;
      Idx2      : Integer;
      Idx3      : Integer;
      Found     : Boolean;
      Num       : Integer;
      Last      : Integer;
      Mark_Line : Integer;
      TWin      : Window_Pointer;
      WT        : Widget_Pointer;

      package IIO renames Ada.Integer_Text_IO;
      procedure Get_Line;
      procedure Get_Line is
      begin
         TIO.Get_Line (DFile, Line, Len);
         if Len > 0 then
            if Line (Len) = ASCII.CR then
               Len := Len - 1;
            end if;
         end if;
         NLin := NLin + 1;
      end Get_Line;

   begin
      TIO.Open (File => DFile,
                Mode => TIO.In_File,
                Name => Resx_Path & "/" & Resx_File_Name & ".Designer.vb");

      NLin := 0;

      --  skip first line
      Get_Line;

      Debug (-1, "Parsing (1) Designer");
      Found := False;
      while not TIO.End_Of_File (DFile) loop
         Get_Line;
         Idx0 := Index (Line (1 .. Len), Test8);
         if Idx0 in 1 .. Len then
            Debug (NLin, Line (1 .. Len));
            TWin := new Window_Properties (GtkWindow);
            Found := True;
            TWin.Name := new String'(+Trim (Line (Idx0 + 14 .. Len),
                                     Ada.Strings.Both));
            TWin.Original_Name := new String'(TWin.Name.all);
            TWin.Top_Level     := True;
            Insert_Window_By_Front (Win_List, TWin);
            Debug (NLin, Sp (3) & "Created GtkWindow " & TWin.Name.all);
            Debug (NLin, Sp (3) & "Set Window Property Name " & TWin.Name.all);
            exit;
         end if;
      end loop;
      if not Found then
         Debug (NLin, Resx_File_Name & ".Designer.vb (1)"
                & """" & Test8 & """" & ": not found");
         TIO.Close (DFile);
         return -1;
      end if;

      Debug (NLin, "Searching for " & Test13);
      Found := False;
      while not TIO.End_Of_File (DFile) loop
         Get_Line;
         Idx0 := Index (Line (1 .. Len), Test13);
         if Idx0 in 1 .. Len then
            Debug (NLin, Sp (3) & "Found");
            Found := True;
            exit;
         end if;
      end loop;
      if not Found then
         Debug (NLin, Resx_File_Name & ".Designer.vb (1)"
                & """" & Test13 & """" & ": not found");
         TIO.Close (DFile);
         return -1;
      end if;

      Debug (NLin, "Searching for " & Test7);
      Get_Line;
      Idx0 := Index (Line (1 .. Len), Test7);
      if Idx0 in 1 .. Len then
         Debug (NLin, Sp (3) & "Found");
         Get_Line;
      else
         Debug (NLin, Sp (3) & "Not Found");
      end if;

      Debug (NLin, "Searching for " & Test18);
      Idx0 := Index (Line (1 .. Len), Test18);
      if Idx0 in 1 .. Len then
         Debug (NLin, Sp (3) & "Found");
         Get_Line;
      else
         Debug (NLin, Sp (3) & "Not Found");
      end if;

      Debug (NLin, "Searching for " & Test19);
      Found := False;
      Mark_Line := NLin;
      loop
         Idx0 := Index (Line (1 .. Len), Test19);
         if Idx0 not in 1 .. Len - 1 then
            Debug (NLin, Sp (3) & "Not More Found");
            exit;
         end if;
         Idx0 := Idx0 + Test19'Length;
         IIO.Get (Line (Idx0 .. Len), Num, Last);
         Debug (NLin, Sp (3) & "Found " & Test19 & Img (Num));
         Max_DGVS := Integer'Max (Max_DGVS, Num);
         exit when TIO.End_Of_File (DFile);
         Get_Line;
      end loop;

      if Max_DGVS > 0 then
         DGVS := new DGVS_Array (1 .. Max_DGVS);
         TIO.Close (DFile);
         TIO.Open (File => DFile,
                   Mode => TIO.In_File,
                   Name => Resx_Path & "/" & Resx_File_Name & ".Designer.vb");
         for I in 1 .. Mark_Line loop
            TIO.Get_Line (DFile, Line, Len);
         end loop;
         NLin  := Mark_Line;
         loop
            Idx0 := Index (Line (1 .. Len), Test19);
            if Idx0 not in 1 .. Len - 1 then
               exit;
            end if;
            Idx0 := Idx0 + Test19'Length;
            IIO.Get (Line (Idx0 .. Len), Num, Last);
            DGVS (Num).Num := Num;
            Debug (NLin, Sp (3) & "Created DataGridViewCellStyle"
                   & DGVS (Num).Num'Image);
            exit when TIO.End_Of_File (DFile);
            Get_Line;
         end loop;
      end if;

      Found := False;
      loop
         Idx0 := Index (Line (1 .. Len), "Me.");
         if Idx0 in 1 .. Len then
            Found := True;
            exit;
         end if;
         exit when TIO.End_Of_File (DFile);
         Get_Line;
      end loop;
      if not Found then
         TIO.Close (DFile);
         Debug (NLin, Resx_File_Name & ".Designer.vb (1)"
                & ": Me.<widget> not found");
         return -1;
      end if;

      Debug (NLin, "Widgets");
      loop
         Idx0 := Index (Line (1 .. Len), "Me.");
         if Idx0 not in 1 .. Len then
            Debug (NLin, "unknown line " & Line (1 .. Len));
            exit;
         end if;
         Idx0 := Idx0 + 3; --  skip "Me."
         Idx1 := Index (Line (Idx0 .. Len), " = New ");
         if Idx1 in Idx0 .. Len then
            Idx1 := Idx1 - 1;
         else
            exit;
         end if;
         Idx2 := Index (Line (Idx1 + 8 .. Len), "(");
         if Idx2 not in Idx1 + 8 .. Len then
            exit;
         end if;
         Len := Idx2 - 1;
         Idx2 := Index (Line (Idx1 + 8 .. Len), ".", Ada.Strings.Backward);
         if Idx2 not in Idx1 + 8 .. Len then
            exit;
         end if;
         Idx2 := Idx2 + 1; --  skip "."
         Idx3 := Len;  --  don't count final ()
         WT := Find_Widget (TWin.Widget_List, +Line (Idx2 .. Idx3));
         if WT /= null then
            TIO.Close (DFile);
            Debug (NLin, Sp (3) & Resx_File_Name & ".Designer.vb (1): "
                   & ": Repeated Widget " & Line (Idx2 .. Idx3));
            return -1;
         end if;
         WT := new Widget_Properties
           (Widget_Type => Symbol_Tables.Get_Type (Line (Idx2 .. Idx3)));
         if WT.Widget_Type = No_Widget then
            TIO.Close (DFile);
            Debug (NLin, Sp (3) & Resx_File_Name & ".Designer.vb (1): "
                   & ": unknown Widget " & Line (Idx2 .. Idx3));
            Debug (NLin, Sp (3) & Line (1 .. Len));
            return -1;
         end if;

         WT.Name := new String'(+Line (Idx0 .. Idx1));
         if WT.Widget_Type = BindingNavigator then
            WT.Windows_Type :=
              new String'("System.Windows.Forms.BindingNavigator");
         elsif Starts_With (WT.Name.all, "Bindingnavigator")
           and then
             WT.Widget_Type = GtkButton
         then
            WT.Windows_Type :=
              new String'("System.Windows.Forms.ToolStripButton");
         elsif WT.Widget_Type = GtkBox then
            WT.Windows_Type :=
              new String'("System.Windows.Forms.FlowLayoutPanel");
         elsif WT.Widget_Type = GtkToolBar then
            WT.Windows_Type :=
              new String'("System.Windows.Forms.ToolStrip");
         elsif WT.Widget_Type = GtkButton then
            TWin.Has_Buttons := True;
         end if;

         Insert_Widget_By_Tail (TWin, WT);

         Debug (NLin, Sp (3) & "Created "
                & WT.Widget_Type'Image & " "
                & WT.Name.all & " from"
                & Line (Idx1 + 7 .. Idx3));

         exit when TIO.End_Of_File (DFile);
         Get_Line;
      end loop;

      Debug (NLin, "Resizable and [Window or Dialog]");
      while not TIO.End_Of_File (DFile) loop
         Get_Line;
         if Contains (Line (1 .. Len), "FormBorderStyle.FixedToolWindow") then
            TWin.Resizable := False;
            Debug (NLin, Sp (3) & "Set Resizable False");
         elsif Contains (Line (1 .. Len), "FormBorderStyle.SizableToolWindow") then
            TWin.Resizable := True;
            Debug (NLin, Sp (3) & "Set Resizable True");
         elsif Contains (Line (1 .. Len), "AcceptButton") then
            Idx0 :=  Index (Line (1 .. Len), " = Me.");
            if Idx0 not in 1 .. Len then
               raise TIO.Data_Error;
            end if;
            Idx0 := Idx0 + 6;
            WT := Find_Widget (TWin.Widget_List, +Line (Idx0 .. Len));
            if WT = null then
               raise TIO.Data_Error;
            end if;
            if WT.Widget_Type /= GtkButton then
               raise TIO.Data_Error;
            end if;
            TWin.Action_Buttons (OK_Response) := WT;
            TWin.Is_Dialog := True;
            WT.Dialog_Result := OK_Response;
            WT.Text := new String'("Accept");
            Debug (NLin, Sp (3) & "Set Accept Button");
            Debug (NLin, Sp (3) & "Set Dialog");
         elsif Contains (Line (1 .. Len), "CancelButton") then
            Idx0 :=  Index (Line (1 .. Len), " = Me.");
            if Idx0 not in 1 .. Len then
               raise TIO.Data_Error;
            end if;
            Idx0 := Idx0 + 6;
            WT := Find_Widget (TWin.Widget_List, +Line (Idx0 .. Len));
            if WT = null then
               raise TIO.Data_Error;
            end if;
            if WT.Widget_Type /= GtkButton then
               raise TIO.Data_Error;
            end if;
            TWin.Action_Buttons (Cancel_Response) := WT;
            TWin.Is_Dialog := True;
            WT.Dialog_Result := Cancel_Response;
            WT.Text := new String'("Cancel");
            Debug (NLin, Sp (3) & "Set Cancel Button");
            Debug (NLin, Sp (3) & "Set Dialog");
         end if;
      end loop;
      if (not TWin.Resizable) and TWin.Is_Dialog and TWin.Has_Buttons then
         TWin.Modal := True;
         Debug (NLin, Sp (3) & "Set Modal True");
      end if;

      Debug (-1, "End of Parsing (1) Designer");
      TIO.Close (DFile);

      return 0;
   exception
      when TIO.Data_Error =>
         TIO.Close (DFile);
         Debug (NLin, Sp (3) & "Data Error");
         Debug (NLin, Sp (3) & Resx_File_Name & ".Designer.vb (1)"
                & ": Line" & NLin'Image
                & " " & Line (1 .. Len));
         return -1;
      when Constraint_Error =>
         TIO.Close (DFile);
         Debug (NLin, Sp (3) & "Constraint Error");
         Debug (NLin, Sp (3) & Resx_File_Name & ".Designer.vb (1)"
                & ": Line" & NLin'Image
                & " " & Line (1 .. Len));
         return -1;
   end Parse1_Designer_File;

   -------------------------------------------------------------------------
   function Parse_VB_File (Resx_Path      : String;
                           Resx_File_Name : String) return Integer is
      WT    : Widget_Pointer;
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
         if Len > 0 then
            if Line (Len) = ASCII.CR then
               Len := Len - 1;
            end if;
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
                        Debug (NLin, Resx_File_Name & ".vb"
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
            Idx1 := Index (Line (Idx0 .. Len), " _");
            if Idx1 not in Idx0 .. Len then
               Debug (NLin, Resx_File_Name
                      & ": Line: " & Img (NLin)
                      & ": could not find handler");
               return "";
            end if;
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
            Debug (NLin, Resx_File_Name & ".vb"
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
            B  : Boolean;
         begin
            if WName = "MyBase" or WName = "Me" then
               WS := new Signal_Block;
               if SName (SName'Last) = ',' then
                  WS.Name := new String'(SName (SName'First ..
                                                SName'Last - 1));
                  Ret := 1;
               else
                  WS.Name := new String'(SName);
               end if;
               declare
                  Handler : constant String := +Get_Handler;
               begin
                  if Handler = "" then
                     return -1;
                  elsif Starts_With (Handler, "On_") then
                     WS.Handler := new String'(Handler);
                  else
                     WS.Handler := new String'("On_" & Handler);
                  end if;
               end;
               WS.Line := NLin;
               B := Insert_Signal (Win_List, WS);
               if B then
                  if Ret = 1 then
                     Debug (NLin, Sp (3) & "Created Signal "
                            & Complete_Signal (Complete_Signal'First ..
                                Complete_Signal'Last - 1)
                            & " [Gtk " & WS.GtkName.all & "]"
                            & " => " & WS.Handler.all);
                  else
                     Debug (NLin, Sp (3) & "Created Signal " &
                              Win_List.Name.all & "." & SName
                            & " [Gtk " & WS.GtkName.all & "]"
                            & " => " & WS.Handler.all);
                  end if;
               end if;
            else
               WT := Find_Widget (Win_List.Widget_List, +WName);
               if WT = null then
                  Debug (NLin, Resx_File_Name & ".vb"
                         & ": Line" & NLin'Image
                         & ": cannot find widget in signal name");
                  return -1;
               end if;
               WS := new Signal_Block;
               if SName (SName'Last) = ',' then
                  WS.Name := new String'(SName (SName'First ..
                                                SName'Last - 1));
                  Ret := 1;
               else
                  WS.Name := new String'(SName);
               end if;
               declare
                  Handler : constant String := +Get_Handler;
               begin
                  if Handler = "" then
                     return -1;
                  elsif Starts_With (Handler, "Tgv_") then
                     WS.Handler :=
                       new String'("On_" & (+WName) & "_"
                                   & Handler
                                     (Handler'First + 4 .. Handler'Last));
                  elsif Starts_With (Handler, "Dgv_") then
                     WS.Handler :=
                       new String'("On_" & (+WName) & "_"
                                   & Handler
                                     (Handler'First + 4 .. Handler'Last));
                  elsif Starts_With (Handler, "On_") then
                     WS.Handler := new String'(Handler);
                  else
                     WS.Handler := new String'("On_" & Handler);
                  end if;
               end;
               WS.Line := NLin;
               if WT.Widget_Type = GtkNoteBook
                 and then WS.Name.all = "Selected"
               then
                  WS.After := True;
               end if;
               if WT.Widget_Type = GtkCalendar then
                  Debug (NLin, "Ignoring Signal " & WS.Name.all);
               else
                  B := Insert_Signal (WT, WS);
                  if B then
                     if Ret = 1 then
                        Debug (NLin, Sp (3) & "Created Signal "
                               & Complete_Signal (Complete_Signal'First ..
                                   Complete_Signal'Last - 1)
                               & " [Gtk " & WS.GtkName.all & "]"
                               & " => " & WS.Handler.all);
                     else
                        Debug (NLin, Sp (3) & "Created Signal "
                               & Complete_Signal
                               & " [Gtk " & WS.GtkName.all & "]"
                               & " => " & WS.Handler.all);
                     end if;
                  end if;
               end if;
            end if;
         end;
         return Ret;
      exception
         when others =>
            Ada.Text_IO.Put_Line ("exception");
            return -1;
      end Parse_Handler;

   begin
      TIO.Open (File => VFile,
                Mode => TIO.In_File,
                Name => Resx_Path & "/" & Resx_File_Name & ".vb");

      NLin := 0;

      Debug (-1, "");
      Debug (-1, "Parsing vb file");
      Debug (0, "Parsing signals");
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
                     TIO.Close (VFile);
                     return Ret;
                  end if;
                  More := (Ret = 1);
               end loop;

               --  continue reading to end sub to locate .showdialog
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
                             +Line (Idx1 + 3 .. Idx0 - 1);
                           WTCD : Widget_Pointer;
                        begin
                           WTCD := Find_Widget (Win_List.Widget_List, WName);
                           if WTCD = null then
                              Debug (NLin, "Warning: "
                                     & Resx_File_Name & ".vb"
                                     & ": Line" & NLin'Image
                                     & ": cannot find widget "
                                     & WName & "." & "ShowDialog()."
                                     & " Could be an external widget.");
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
                              TIO.Close (VFile);
                              Debug (NLin, Resx_File_Name & ".vb"
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

      Debug (0, "Creating synthetic signals for Date and Time pickers");
      declare
         TWin : Window_Pointer;
         TS   : Signal_Pointer;
         B    : Boolean;
      begin
         TWin := Win_List;
         while TWin /= null loop
            WT := TWin.Widget_List;
            while WT /= null loop
               case WT.Widget_Type is
               when GtkCalendar =>
                  if WT.Is_DatePicker then
                     TS := new Signal_Block;
                     TS.Name := new String'("NextMonth");
                     TS.Handler := new String'("On_Next_Month_"
                                               & WT.Name.all);
                     B := Insert_Signal (WT, TS);
                     if B then
                        Debug (0, Sp (3) & "Created synthetic Signal "
                               & WT.Name.all & ".NextMonth");
                     end if;
                     TS := new Signal_Block;
                     TS.Name := new String'("PrevMonth");
                     TS.Handler := new String'("On_Prev_Month_"
                                               & WT.Name.all);
                     B := Insert_Signal (WT, TS);
                     if B then
                        Debug (0, Sp (3) & "Created synthetic Signal "
                               & WT.Name.all & ".PrevMonth");
                     end if;
                     TS := new Signal_Block;
                     TS.Name := new String'("NextYear");
                     TS.Handler := new String'("On_Next_Year_"
                                               & WT.Name.all);
                     B := Insert_Signal (WT, TS);
                     if B then
                        Debug (0, Sp (3) & "Created synthetic Signal "
                               & WT.Name.all & ".NextYear");
                     end if;
                     TS := new Signal_Block;
                     TS.Name := new String'("PrevYear");
                     TS.Handler := new String'("On_Prev_Year_"
                                               & WT.Name.all);
                     B := Insert_Signal (WT, TS);
                     if B then
                        Debug (0, Sp (3) & "Created  syntheticSignal "
                               & WT.Name.all & ".PrevYear");
                     end if;
                     TS := new Signal_Block;
                     TS.Name := new String'("DaySelectedDoubleClick");
                     TS.Handler := new String'("On_Day_Selected_Double_Click_"
                                               & WT.Name.all);
                     B := Insert_Signal (WT, TS);
                     if B then
                        Debug (0, Sp (3) & "Created  syntheticSignal "
                               & WT.Name.all & ".day_selected_double_click");
                     end if;
                     TS := new Signal_Block;
                     TS.Name := new String'("DaySelected");
                     TS.Handler := new String'("On_Day_Selected_"
                                               & WT.Name.all);
                     B := Insert_Signal (WT, TS);
                     if B then
                        Debug (0, Sp (3) & "Created  syntheticSignal "
                            & WT.Name.all & ".day_selected");
                     end if;
                     TS := new Signal_Block;
                     TS.Name := new String'("Click");
                     TS.Handler := new String'("On_Button_Clicked_"
                                               & WT.Name.all);
                     B := Insert_Signal (WT, TS);
                     if B then
                        Debug (0, Sp (3) & "Created  syntheticSignal "
                               & WT.Name.all & ".clicked");
                     end if;
                     TS := new Signal_Block;
                     TS.Name := new String'("Activate");
                     TS.Handler := new String'("On_Entry_Year_Activate_"
                                               & WT.Name.all);
                     B := Insert_Signal (WT, TS);
                     if B then
                        Debug (0, Sp (3) & "Created  syntheticSignal "
                               & WT.Name.all
                               & ".Year_Activate");
                     end if;
                     TS := new Signal_Block;
                     TS.Name := new String'("LeaveFocus");
                     TS.Handler := new String'("On_Entry_Year_Leavefocus_"
                                               & WT.Name.all);
                     B := Insert_Signal (WT, TS);
                     if B then
                        Debug (0, Sp (3) & "Created  syntheticSignal "
                               & WT.Name.all
                               & ".Year_Activate");
                     end if;
                     TS := new Signal_Block;
                     TS.Name := new String'("Activate");
                     TS.Handler := new String'("On_Entry_Month_Activate_"
                                               & WT.Name.all);
                     B := Insert_Signal (WT, TS);
                     if B then
                        Debug (0, Sp (3) & "Created  syntheticSignal "
                               & WT.Name.all
                               & ".Month_Activate");
                     end if;
                     TS := new Signal_Block;
                     TS.Name := new String'("LeaveFocus");
                     TS.Handler := new String'("On_Entry_Month_Leavefocus_"
                                               & WT.Name.all);
                     B := Insert_Signal (WT, TS);
                     if B then
                        Debug (0, Sp (3) & "Created  syntheticSignal "
                               & WT.Name.all
                               & ".Month_Activate");
                     end if;
                     TS := new Signal_Block;
                     TS.Name := new String'("Activate");
                     TS.Handler := new String'("On_Entry_Day_Activate_"
                                               & WT.Name.all);
                     B := Insert_Signal (WT, TS);
                     if B then
                        Debug (0, Sp (3) & "Created synthetic Signal "
                               & WT.Name.all
                               & ".Day_Activate");
                     end if;
                     TS := new Signal_Block;
                     TS.Name := new String'("LeaveFocus");
                     TS.Handler := new String'("On_Entry_Day_Leavefocus_"
                                               & WT.Name.all);
                     B := Insert_Signal (WT, TS);
                     if B then
                        Debug (0, Sp (3) & "Created synthetic Signal "
                               & WT.Name.all
                               & ".Day_Activate");

                     end if;
                  else
                     TS := new Signal_Block;
                     TS.Name := new String'("Click");
                     TS.Handler := new String'("On_Sec_Button_Up_Clicked_"
                                               & WT.Name.all);
                     B := Insert_Signal (WT, TS);
                     if B then
                        Debug (0, Sp (3) & "Created synthetic Signal "
                               & WT.Name.all
                               & ".Sec_Clicked");
                     end if;
                     TS := new Signal_Block;
                     TS.Name := new String'("Click");
                     TS.Handler := new String'("On_Sec_Button_Down_Clicked_"
                                               & WT.Name.all);
                     B := Insert_Signal (WT, TS);
                     if B then
                        Debug (0, Sp (3) & "Created synthetic Signal "
                               & WT.Name.all
                            & ".Sec_Clicked");
                     end if;
                     TS := new Signal_Block;
                     TS.Name := new String'("Click");
                     TS.Handler := new String'("On_Min_Button_Up_Clicked_"
                                               & WT.Name.all);
                     B := Insert_Signal (WT, TS);
                     if B then
                        Debug (0, Sp (3) & "Created synthetic Signal "
                               & WT.Name.all
                               & ".Min_Clicked");
                     end if;
                     TS := new Signal_Block;
                     TS.Name := new String'("Click");
                     TS.Handler := new String'("On_Min_Button_Down_Clicked_"
                                               & WT.Name.all);
                     B := Insert_Signal (WT, TS);
                     if B then
                        Debug (0, Sp (3) & "Created synthetic Signal "
                               & WT.Name.all
                               & ".Min_Clicked");
                     end if;
                     TS := new Signal_Block;
                     TS.Name := new String'("Click");
                     TS.Handler := new String'("On_Hour_Button_Up_Clicked_"
                                               & WT.Name.all);
                     B := Insert_Signal (WT, TS);
                     if B then
                        Debug (0, Sp (3) & "Created synthetic Signal "
                               & WT.Name.all
                               & ".Hour_Clicked");
                     end if;
                     TS := new Signal_Block;
                     TS.Name := new String'("Click");
                     TS.Handler := new String'("On_Hour_Button_Down_Clicked_"
                                               & WT.Name.all);
                     B := Insert_Signal (WT, TS);
                     if B then
                        Debug (0, Sp (3) & "Created synthetic Signal "
                               & WT.Name.all
                               & ".Hour_Clicked");
                     end if;
                     TS := new Signal_Block;
                     TS.Name := new String'("Activate");
                     TS.Handler := new String'("On_Entry_Hour_Activate_"
                                               & WT.Name.all);
                     B := Insert_Signal (WT, TS);
                     if B then
                        Debug (0, Sp (3) & "Created synthetic Signal "
                               & WT.Name.all
                               & ".Hour_Activate");
                     end if;
                     TS := new Signal_Block;
                     TS.Name := new String'("LeaveFocus");
                     TS.Handler := new String'("On_Entry_Hour_Leavefocus_"
                                               & WT.Name.all);
                     B := Insert_Signal (WT, TS);
                     if B then
                        Debug (0, Sp (3) & "Created synthetic Signal "
                               & WT.Name.all
                               & ".Hour_Activate");
                     end if;
                     TS := new Signal_Block;
                     TS.Name := new String'("Activate");
                     TS.Handler := new String'("On_Entry_Min_Activate_"
                                               & WT.Name.all);
                     B := Insert_Signal (WT, TS);
                     if B then
                        Debug (0, Sp (3) & "Created synthetic Signal "
                               & WT.Name.all
                               & ".Min_Activate");
                     end if;
                     TS := new Signal_Block;
                     TS.Name := new String'("LeaveFocus");
                     TS.Handler := new String'("On_Entry_Min_Leavefocus_"
                                               & WT.Name.all);
                     B := Insert_Signal (WT, TS);
                     if B then
                        Debug (0, Sp (3) & "Created synthetic Signal "
                               & WT.Name.all
                               & ".Min_Activate");
                     end if;
                     TS := new Signal_Block;
                     TS.Name := new String'("Activate");
                     TS.Handler := new String'("On_Entry_Sec_Activate_"
                                               & WT.Name.all);
                     B := Insert_Signal (WT, TS);
                     if B then
                        Debug (0, Sp (3) & "Created synthetic Signal "
                               & WT.Name.all
                               & ".Sec_Activate");
                     end if;
                     TS := new Signal_Block;
                     TS.Name := new String'("LeaveFocus");
                     TS.Handler := new String'("On_Entry_Sec_Leavefocus_"
                                               & WT.Name.all);
                     B := Insert_Signal (WT, TS);
                     if B then
                        Debug (0, Sp (3) & "Created synthetic Signal "
                               & WT.Name.all
                               & ".Sec_Activate");
                     end if;
                  end if;

               when others => null;
               end case;
               WT := WT.Next;
            end loop;
            TWin := Next_Window (Win_List, TWin);
         end loop;
      end;

      Debug (0, "Creating Load Handler for Windows with Date or Time pickers");
      declare
         TWin : Window_Pointer;
         TS   : Signal_Pointer;
         B    : Boolean;
      begin
         TWin := Win_List;
         while TWin /= null loop
            if TWin.Window_Type = GtkWindow then
               if Have.Date_Pickers + Have.Time_Pickers > 0 then
                  TS := new Signal_Block;
                  TS.Name := new String'("Load");
                  TS.Handler := new String'("On_Load_"
                                            & TWin.Name.all);
                  B := Insert_Signal (TWin, TS);
                  if B then
                     Debug (0, Sp (3) & "Created synthetic Signal "
                            & TWin.Name.all & ".Load");
                  else
                     Free (TS.Name);
                     Free (TS);
                     Debug (0, Sp (3) & "Unable to create synthetic Signal "
                            & "for "
                            & TWin.Name.all & ".Load: "
                            & " repeated handler "
                            & "On_Load_" & TWin.Name.all);
                  end if;
               end if;
            end if;
            TWin := Next_Window (Win_List, TWin);
         end loop;
      end;

      Debug (-1, "End of Parsing vb file");
      TIO.Close (VFile);
      return 0;
   end Parse_VB_File;

   -------------------------------------------------------------------------
   procedure Generate_Backup (Result    : out Integer;
                              The_Path  : String;
                              Filename  : String;
                              Use_Log   : Boolean := True);
   procedure Generate_Backup (Result    : out Integer;
                              The_Path  : String;
                              Filename  : String;
                              Use_Log   : Boolean := True) is

   begin
      Get_Max_Gen (The_Path & "/" & Filename, Max_Gen);
      Make_Backup (Result, The_Path & "/" & Filename, Max_Gen, Use_Log);
   exception
      when others =>
         Debug (-1, "Could not generate backup for " & Filename);
         Result := -1;
   end Generate_Backup;

   -------------------------------------------------------------------------
   function Parse_VS_File (Ada_Switch     : Boolean;
                           Log_Switch     : Boolean;
                           Do_Dump        : Boolean;
                           Glade_Switch   : Boolean;
                           Resx_Path      : String;
                           Resx_File_Name : String;
                           Glade_Path     : String;
                           Icon_Path      : String;
                           Ada_Path       : String) return Integer is
      Result : Integer;
      Instant : constant String := GNAT.Calendar.Time_IO.Image
        (Ada.Calendar.Clock, GNAT.Calendar.Time_IO.ISO_Time);
   begin
      W2gtk_Decls.Log := Log_Switch;

      if Log_Switch then
         Generate_Backup (Result,
                          Glade_Path,
                          Resx_File_Name & ".log",
                          Use_Log => False);
         TIO.Create (File => Log_File,
                     Mode => TIO.Out_File,
                     Name => Glade_Path & "/" & Resx_File_Name & ".log");
         Debug (-1, "w2gtk -log"
                & (if Ada_Switch then " -ada " else "")
                & (if Do_Dump then " -dump " else "")
                & (if Glade_Switch then " -glade " else "")
                & " -rp " & Resx_Path
                & " -rf " & Resx_File_Name
                & " -gp " & Glade_Path
                & " -ip " & Icon_Path
                & " -ap " & Ada_Path);
         Debug (-1, "");
         Debug (-1, "This is w2gtk " & Version & " - " & Instant);
         Debug (-1, "Processing " & Resx_Path & Resx_File_Name);
         Debug (-1, "");
         Debug (-1, "Generated log backup");
         Debug (-1, Glade_Path & "/" & Resx_File_Name & ".log"
                   & " renamed to "
                & Glade_Path & "/" & Resx_File_Name & ".log" & "~" & Img (Max_Gen));
         Debug (-1, "");
      end if;

      Result := Parse1_Designer_File (Resx_Path,
                                      Resx_File_Name);
      if Result /= 0 then
         return Result;
      end if;

      Result := Parse_Resource_File (Win_List,
                                     Resx_Path,
                                     Resx_File_Name);
      if Result /= 0 then
         return Result;
      end if;

      Result := Parse2_Designer_File (Win_List,
                                      Resx_Path,
                                      Resx_File_Name,
                                      Icon_Path);
      if Result /= 0 then
         return Result;
      end if;

      Result := Parse_VB_File (Resx_Path,
                               Resx_File_Name);
      if Result /= 0 then
         return Result;
      end if;

      Result := Adjust_To_Gtk;
      if Result /= 0 then
         return Result;
      end if;

      if Do_Dump then
         Debug (-1, "");
         Debug (-1, "Generating Dump backup" & " - " & Instant);

         Generate_Backup (Result, Glade_Path,
                          Resx_File_Name & ".dump");
         if Result < 0 then
            return Result;
         end if;
         Dump (Glade_Path, Resx_File_Name, Instant);
      end if;

      return Result;
   end Parse_VS_File;

   -------------------------------------------------------------------------
   function Generate_Glade_File (Glade_Path : String;
                                 FileName   : String) return Integer is
      TWin : Window_Pointer;
      Result : Integer;
   begin
      Debug (-1, "");
      Debug (-1, "Generating Glade backup");
      Generate_Backup (Result, Glade_Path,
                       FileName & ".glade");
      if Result < 0 then
         return Result;
      end if;

      Debug (-1, "");
      Debug (-1, "Generating Glade " & Glade_Path & "/" & FileName & ".glade");

      if Win_List = null then
         TIO.Put_Line ("No window information");
         return -1;
      end if;
      TIO.Create (File => GFile,
                  Mode => TIO.Out_File,
                  Name => Glade_Path & "/" & FileName & ".glade");
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
            when GtkImage =>
               Emit_GtkImage (TWin, 2);
            when GtkTreeStore =>
               Emit_GtkTreeStore (TWin, 2);
            when GtkModelFilter =>
               Emit_GtkModelFilter (TWin, 2);
            when GtkModelSort =>
               Emit_GtkModelSort (TWin, 2);
         end case;
         TWin := Next_Window (Win_List, TWin);
      end loop;
      Emit_GtkTrailer (null, 0);

      TIO.Close (GFile);
      Debug (-1, "End of generating Glade");
      return 0;
   end Generate_Glade_File;

end W2gtk_Pkg;
