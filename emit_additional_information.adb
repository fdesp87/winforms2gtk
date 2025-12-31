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
-- with this program; see the file COPYING3.                                --
-- If not, see <http://www.gnu.org/licenses/>.                              --
------------------------------------------------------------------------------
with W2gtk_Decls;    use W2gtk_Decls;
with Emit_Tools;     use Emit_Tools;
with GNAT.Strings;   use GNAT.Strings;

package body Emit_Additional_Information is

   procedure Emit_Columns;
   procedure Emit_Columns is
   begin
      Emit_Line (Sp (4) & "<columns>");
      Emit_Line (Sp (6) & "<!-- column-name Column_Name -->");
      Emit_Line (Sp (6) & "<column type=""gchararray""/>");
      Emit_Line (Sp (6) & "<!-- column-name tooltip -->");
      Emit_Line (Sp (6) & "<column type=""gchararray""/>");
      Emit_Line (Sp (6) & "<!-- column-name xalign -->");
      Emit_Line (Sp (6) & "<column type=""gfloat""/>");
      Emit_Line (Sp (6) & "<!-- column-name yalign -->");
      Emit_Line (Sp (6) & "<column type=""gfloat""/>");
      Emit_Line (Sp (6) & "<!-- column-name xpad -->");
      Emit_Line (Sp (6) & "<column type=""gint""/>");
      Emit_Line (Sp (6) & "<!-- column-name ypad -->");
      Emit_Line (Sp (6) & "<column type=""gint""/>");
      Emit_Line (Sp (6) & "<!-- column-name foreground -->");
      Emit_Line (Sp (6) & "<column type=""gchararray""/>");
      Emit_Line (Sp (6) & "<!-- column-name background -->");
      Emit_Line (Sp (6) & "<column type=""gchararray""/>");
      Emit_Line (Sp (6) & "<!-- column-name selforeground -->");
      Emit_Line (Sp (6) & "<column type=""gchararray""/>");
      Emit_Line (Sp (6) & "<!-- column-name selbackground -->");
      Emit_Line (Sp (6) & "<column type=""gchararray""/>");
      Emit_Line (Sp (6) & "<!-- column-name underline -->");
      Emit_Line (Sp (6) & "<column type=""gboolean""/>");
      Emit_Line (Sp (6) & "<!-- column-name font_name -->");
      Emit_Line (Sp (6) & "<column type=""gchararray""/>");
      Emit_Line (Sp (6) & "<!-- column-name font_weight -->");
      Emit_Line (Sp (6) & "<column type=""gchararray""/>");
      Emit_Line (Sp (6) & "<!-- column-name font_size -->");
      Emit_Line (Sp (6) & "<column type=""gint""/>");
      Emit_Line (Sp (4) & "</columns>");
   end Emit_Columns;

   --------------------------------------------------------------------------
   procedure Emit_Row (Row_Name       : String;
                       Tooltip        : String_Access := null;
                       Xalign         : Float  := 0.0;
                       Yalign         : Float  := 0.5;
                       Xpad           : Integer := 0;
                       Ypad           : Integer := 0;
                       Foreground     : String_Access := null;
                       Background     : String_Access := null;
                       SelForeground  : String_Access := null;
                       SelBackground  : String_Access := null;
                       Underline      : Boolean := False;
                       Font_Name      : String_Access := null;
                       Font_Weight    : String_Access := null;
                       Font_Size      : Integer := 9);
   procedure Emit_Row (Row_Name       : String;
                       Tooltip        : String_Access := null;
                       Xalign         : Float  := 0.0;
                       Yalign         : Float  := 0.5;
                       Xpad           : Integer := 0;
                       Ypad           : Integer := 0;
                       Foreground     : String_Access := null;
                       Background     : String_Access := null;
                       SelForeground  : String_Access := null;
                       SelBackground  : String_Access := null;
                       Underline      : Boolean := False;
                       Font_Name      : String_Access := null;
                       Font_Weight    : String_Access := null;
                       Font_Size      : Integer := 9) is
   begin
      Emit_Line (Sp (8) & "<row>");
      Emit_Line (Sp (10) & "<col id=""0"">" & Row_Name & "</col>");
      if Tooltip = null then
         Emit_Line (Sp (10) & "<col id=""1"" translatable=""yes""></col>");
      elsif Tooltip.all = "" then
         Emit_Line (Sp (10) & "<col id=""1"" translatable=""yes""></col>");
      else
         Emit_Line (Sp (10) & "<col id=""1"" translatable=""yes"">"
                    & Tooltip.all & "</col>");
      end if;
      Emit_Line (Sp (10) & "<col id=""2"">" & Img (Xalign, 0) & "</col>");
      Emit_Line (Sp (10) & "<col id=""3"">" & Img (Yalign, 0) & "</col>");
      Emit_Line (Sp (10) & "<col id=""4"">" & Img (Xpad) & "</col>");
      Emit_Line (Sp (10) & "<col id=""5"">" & Img (Ypad) & "</col>");
      if Foreground = null then
         Emit_Line (Sp (10) & "<col id=""6"" translatable=""yes"">"
                    & "Black" & "</col>");
      else
         Emit_Line (Sp (10) & "<col id=""6"" translatable=""yes"">"
                    & Foreground.all & "</col>");
      end if;
      if Background = null then
         if Row_Name = "ALT_BG" then
            Emit_Line (Sp (10) & "<col id=""7"" translatable=""yes"">"
                       & Default_Alt_Color & "</col>");
         else
            Emit_Line (Sp (10) & "<col id=""7"" translatable=""yes"">"
                       & "White" & "</col>");
         end if;
      else
         Emit_Line (Sp (10) & "<col id=""7"" translatable=""yes"">"
                    & Background.all & "</col>");
      end if;
      if SelForeground = null then
         Emit_Line (Sp (10) & "<col id=""8"" translatable=""yes"">"
                    & "White" & "</col>");
      else
         Emit_Line (Sp (10) & "<col id=""8"" translatable=""yes"">"
                    & SelForeground.all & "</col>");
      end if;
      if SelBackground = null then
         Emit_Line (Sp (10) & "<col id=""9"" translatable=""yes"">"
                    & "Blue" & "</col>");
      else
         Emit_Line (Sp (10) & "<col id=""9"" translatable=""yes"">"
                    & SelBackground.all & "</col>");
      end if;
      if Underline then
         Emit_Line (Sp (10) & "<col id=""10"">True</col>");
      else
         Emit_Line (Sp (10) & "<col id=""10"">False</col>");
      end if;

      if Font_Name = null then
         Emit_Line (Sp (10) & "<col id=""11"" translatable=""Yes"">"
                    & Default_Font_Name & "</col>");
      else
         Emit_Line (Sp (10) & "<col id=""11"" translatable=""Yes"">"
                    & Font_Name.all & "</col>");
      end if;
      if Font_Weight = null then
         Emit_Line (Sp (10) & "<col id=""12"" translatable=""yes"">"
                    & "Normal" & "</col>");
      else
         Emit_Line (Sp (10) & "<col id=""12"" translatable=""yes"">"
                    & Font_Weight.all & "</col>");
      end if;
      Emit_Line (Sp (10) & "<col id=""13"">" & Img (Font_Size) & "</col>");
      Emit_Line (Sp (8) & "</row>");
   end Emit_Row;

   --------------------------------------------------------------------------
   procedure Process_Column_Header (TWdg : Widget_Pointer);
   procedure Process_Column_Header (TWdg : Widget_Pointer) is
      HS : DataGridViewStyle;
      Alt_BG_Color : String_Access;
   begin
      if TWdg.Widget_Type in GtkDataGridView | GtkTreeGridView
        and then TWdg.ColumnHeadersDefaultCellStyle in DGVS'Range
      then
         if TWdg.AlternatingRowsDefaultCellStyle in DGVS'Range then
            HS := DGVS (TWdg.AlternatingRowsDefaultCellStyle);
            Alt_BG_Color := HS.BgColor;
         end if;
         if TWdg.ColumnHeadersDefaultCellStyle in DGVS'Range then
            HS := DGVS (TWdg.ColumnHeadersDefaultCellStyle);
            Emit_Row (Row_Name      => "ALT_BG",
                      Tooltip       => Alt_BG_Color,
                      Background    => HS.BgColor,
                      Foreground    => HS.FgColor,
                      SelBackground => HS.SelBgColor,
                      SelForeground => HS.SelFgColor,
                      Font_Name     => HS.Font_Name,
                      Font_Weight   => HS.Font_Weight,
                      Font_Size     => HS.Font_Size);
         else
            Emit_Row (Row_Name   => "ALT_BG",
                      Tooltip    => Alt_BG_Color);
         end if;
      end if;
   end Process_Column_Header;

   --------------------------------------------------------------------------
   procedure Process_Columns (TWdg : Widget_Pointer);
   procedure Process_Columns (TWdg : Widget_Pointer) is
      Col    : Widget_Pointer;
      I      : Integer;
      Xalign : Float := 0.0;
      Yalign : Float := 0.5;
      Last   : Integer;
   begin
      Col := TWdg.Child_List;
      while Col /= null loop
         case Col.Widget_Type is
            when ExpandableColumn | DataGridViewTextBoxColumn
               | DataGridViewCheckBoxColumn =>
               if Col.DefaultCellStyle in DGVS.all'Range then
                  I := Col.DefaultCellStyle;
                  case DGVS (I).Style_For is
                     when For_TextCell | For_ToggleCell =>
                        if DGVS (I).Alignment /= null then
                           case To_TextAlign (DGVS (I).Alignment.all) is
                              when None => null;
                              when TopLeft =>
                                 Xalign := 0.0;
                                 Yalign := 0.0;
                              when TopCenter =>
                                 Xalign := 0.5;
                                 Yalign := 0.0;
                              when TopRight =>
                                 Xalign := 1.0;
                                 Yalign := 0.0;
                              when MiddleLeft =>
                                 Xalign := 0.0;
                                 Yalign := 0.5;
                              when MiddleCenter =>
                                 Xalign := 0.5;
                                 Yalign := 0.5;
                              when MiddleRight =>
                                 Xalign := 1.0;
                                 Yalign := 0.5;
                              when BottomLeft =>
                                 Xalign := 0.1;
                                 Yalign := 1.0;
                              when BottomCenter =>
                                 Xalign := 0.5;
                                 Yalign := 1.0;
                              when BottomRight =>
                                 Xalign := 1.0;
                                 Yalign := 1.0;
                              when Left =>
                                 Xalign := 0.0;
                                 Yalign := 0.5;
                              when Right =>
                                 Xalign := 1.0;
                                 Yalign := 0.5;
                              when Center =>
                                 Xalign := 0.5;
                                 Yalign := 0.5;
                              when Top =>
                                 Xalign := 0.5;
                                 Yalign := 0.0;
                              when Bottom =>
                                 Xalign := 0.5;
                                 Yalign := 1.0;
                           end case;
                        end if;
                        Last := DGVS (I).Name.all'Last;
                        Emit_Row
                          (Row_Name      => DGVS (I).Name.all (5 .. Last),
                           Tooltip       => Col.ToolTip,
                           Xalign        => Xalign,
                           Yalign        => Yalign,
                           Xpad          => DGVS (I).Padding (1),
                           Ypad          => DGVS (I).Padding (2),
                           Foreground    => DGVS (I).FgColor,
                           Background    => DGVS (I).BgColor,
                           SelForeground => DGVS (I).SelFgColor,
                           SelBackground => DGVS (I).SelBgColor,
                           Underline     => DGVS (I).Font_Underline,
                           Font_Name     => DGVS (I).Font_Name,
                           Font_Weight   => DGVS (I).Font_Weight,
                           Font_Size     => DGVS (I).Font_Size);

                     when others => null;
                  end case;
               end if;
            when others => null;
         end case;
         Col := Col.Next;
      end loop;
   end Process_Columns;

   --------------------------------------------------------------------------
   procedure Emit_Data (TWdg : Widget_Pointer);
   procedure Emit_Data (TWdg : Widget_Pointer) is
   begin
      Emit_Line (Sp (4) & "<data>");
      Process_Columns (TWdg);
      Process_Column_Header (TWdg);
      Emit_Line (Sp (4) & "</data>");
   end Emit_Data;

   --------------------------------------------------------------------------
   procedure Emit_Columns_And_Data (TWdg : Widget_Pointer);
   procedure Emit_Columns_And_Data (TWdg : Widget_Pointer) is
   begin
      Emit_Columns;
      Emit_Data (TWdg);
   end Emit_Columns_And_Data;

   --------------------------------------------------------------------------
   procedure Emit_Focus_Column_And_Data (TWin : Window_Pointer);
   procedure Emit_Focus_Column_And_Data (TWin : Window_Pointer) is
      TWdg : Widget_Pointer := TWin.TabFocusList;
   begin
      Emit_Line (Sp (4) & "<columns>");
      Emit_Line (Sp (6) & "<!-- column-name Widget -->");
      Emit_Line (Sp (6) & "<column type=""gchararray""/>");
      Emit_Line (Sp (4) & "</columns>");
      Emit_Line (Sp (4) & "<data>");
      loop
         case TWdg.Widget_Type is
            when GtkEntry | GtkComboTextBox | GtkButton |
                 GtkRadioButton | GtkCheckButton | GtkToggleButton |
                 GtkSpinButton | GtkFileChooserButton | GtkColorButton |
                 GtkCalendar | GtkListBox | GtkTabPage
               =>
               Emit_Line (Sp (8) & "<row>");
               Emit_Line (Sp (10) & "<col id=""0"">" & TWdg.Name.all & "</col>");
               Emit_Line (Sp (8) & "</row>");
            when others => null;
         end case;
         TWdg := TWdg.Next_Focus;
         exit when TWdg = TWin.TabFocusList;
      end loop;
      Emit_Line (Sp (4) & "</data>");
   end Emit_Focus_Column_And_Data;

   --------------------------------------------------------------------------
   procedure Run is
      TWin : Window_Pointer;
   begin
      TWin := Win_List;
      while TWin /= null loop
         case TWin.Window_Type is
            when GtkListStore | GtkTreeStore =>
               if TWin.Associated_Widget.Child_List /= null then
                  if TWin.Name /= null and then TWin.Name.all /= "" then
                     Emit_Object (null, 2, "GtkListStore",
                                  TWin.Name.all & "_Additional_Information");
                     Emit_Columns_And_Data (TWin.Associated_Widget);
                     Emit_Line ("  " & "</object>");
                  end if;
               end if;
            when GtkWindow =>
               if TWin.TabFocusList /= null then
                  if TWin.Name /= null and then TWin.Name.all /= "" then
                     Emit_Object (null, 2, "GtkListStore",
                                  TWin.Name.all & "_Focus_List");
                     Emit_Focus_Column_And_Data (TWin);
                     Emit_Line ("  " & "</object>");
                  end if;
               end if;
            when others => null;
         end case;
         TWin := Next_Window (Win_List, TWin);
      end loop;
   end Run;

end Emit_Additional_Information;
