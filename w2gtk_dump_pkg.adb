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
with W2gtk_Decls;             use W2gtk_Decls;
with GNAT.Calendar.Time_IO;   use GNAT.Calendar.Time_IO;
with GNAT.Strings;            use GNAT.Strings;
with GNATCOLL.Tribooleans;    use GNATCOLL.Tribooleans;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with W2gtk_Version;           use W2gtk_Version;

package body W2gtk_Dump_Pkg is

   TWin : Window_Pointer;

   ----------------------------------------------------------------
   procedure Dump_DGVS;
   procedure Dump_Window (TWin  : Window_Pointer; Id : Integer);
   procedure Dump_Widget (TWdgP : Widget_Pointer; Id : Integer);

   ----------------------------------------------------------------
   procedure Put_Property (PName : String);
   procedure Put_Property (PName : String) is
   begin
      TIO.Put (LFile, Sp (3)
               & PName
               & Sp (24 - PName'Length));
   end Put_Property;

   ----------------------------------------------------------------
   procedure Put_Integer (N : Integer);
   procedure Put_Integer (N : Integer) is
   begin
      TIO.Put (LFile, Img (N));
      TIO.New_Line (LFile);
   end Put_Integer;

   ----------------------------------------------------------------
   procedure Put_String_Access (SA : String_Access;
                                Quoted : Boolean := False);
   procedure Put_String_Access (SA : String_Access;
                                Quoted : Boolean := False) is
   begin
      if SA /= null then
         if Quoted then
            TIO.Put (LFile, """" & SA.all & """");
         else
            TIO.Put (LFile, SA.all);
         end if;
      end if;
      TIO.New_Line (LFile);
   end Put_String_Access;

   ----------------------------------------------------------------
   procedure Put_Boolean (B : Boolean);
   procedure Put_Boolean (B : Boolean) is
   begin
      if B then
         TIO.Put (LFile, "True");
      else
         TIO.Put (LFile, "False");
      end if;
      TIO.New_Line (LFile);
   end Put_Boolean;

   ----------------------------------------------------------------
   procedure Put_Triboolean (B : Triboolean);
   procedure Put_Triboolean (B : Triboolean) is
   begin
      if GNATCOLL.Tribooleans."=" (B, True) then
         TIO.Put (LFile, "True");
      elsif GNATCOLL.Tribooleans."=" (B, False) then
         TIO.Put (LFile, "False");
      else
         TIO.Put (LFile, "Indeterminate");
      end if;
      TIO.New_Line (LFile);
   end Put_Triboolean;

   ----------------------------------------------------------------
   procedure Dump_DGVS is
   begin
      if DGVS = null then
         return;
      end if;
      for I in DGVS.all'Range loop
         if I /= DGVS.all'First then
            TIO.New_Line (LFile);
         end if;

         TIO.Put_Line (LFile, "Gtk_Cell_Renderer");

         Put_Property ("Name");
         Put_String_Access (DGVS (I).Name);

         Put_Property ("Num");
         Put_Integer (DGVS (I).Num);

         Put_Property ("Style_For");
         TIO.Put_Line (LFile, DGVS (I).Style_For'Image);

         Put_Property ("Alignment");
         Put_String_Access (DGVS (I).Alignment);

         Put_Property ("BgColor");
         Put_String_Access (DGVS (I).BgColor);

         Put_Property ("FgColor");
         Put_String_Access (DGVS (I).FgColor);

         Put_Property ("SelBgColor");
         Put_String_Access (DGVS (I).SelBgColor);

         Put_Property ("SelFgColor");
         Put_String_Access (DGVS (I).SelFgColor);

         Put_Property ("Font");
         if DGVS (I).Font_Name /= null then
            TIO.Put (LFile, DGVS (I).Font_Name.all);
            TIO.Put (LFile, " " & Img (DGVS (I).Font_Size));
            if DGVS (I).Font_Weight /= null then
               TIO.Put (LFile, " " & "+" (DGVS (I).Font_Weight.all));
            end if;
            if DGVS (I).Font_Underline then
               TIO.Put (LFile, " underline");
            end if;
         end if;
         TIO.New_Line (LFile);

         Put_Property ("Format");
         TIO.Put_Line (LFile, DGVS (I).Format'Image);

         Put_Property ("Padding");
         TIO.Put (LFile, "Start " & Img (DGVS (I).Padding (1))
                  & ", Top " & Img (DGVS (I).Padding (2))
                  & ", End " & Img (DGVS (I).Padding (3))
                  & ", Bottom " & Img (DGVS (I).Padding (4)));
         TIO.New_Line (LFile);

         Put_Property ("WrapMode");
         Put_Boolean (DGVS (I).WrapMode);

         Put_Property ("NullValue");
         Put_String_Access (DGVS (I).NullValue);

      end loop;
   end Dump_DGVS;

   ----------------------------------------------------------------
   procedure Dump_Window (TWin  : Window_Pointer; Id : Integer) is

      procedure Put_Property (PName : String);
      procedure Put_Property (PName : String) is
      begin
         TIO.Put (LFile, Sp (3)
                  & PName
                  & Sp (24 - PName'Length));
      end Put_Property;

      procedure Dump_Signal_List;
      procedure Dump_Signal_List is
         TS : Signal_Pointer;
      begin
         TS := TWin.Signal_List;
         while TS /= null loop
            if TS.Glade then
               if TS.After then
                  Put_Property ("Glade Signal (after)");
               else
                  Put_Property ("Glade Signal (before)");
               end if;
            else
               if TS.After then
                  Put_Property ("Signal (after)");
               else
                  Put_Property ("Signal (before)");
               end if;
            end if;
            TIO.Put (LFile, TS.Name.all
                     & " [Gtk " & TS.GtkName.all & "] ");
            if TS.Handler /= null then
               TIO.Put (LFile, " => " & TS.Name.all);
            end if;
            if TS.Line <= 0 then
               TIO.Put (LFile, ", synthetic");
            end if;
            TIO.New_Line (LFile);
            TS := TS.Next;
         end loop;
      end Dump_Signal_List;

      procedure Put_Widget_Pointer_Name (WP : Widget_Pointer);
      procedure Put_Widget_Pointer_Name (WP : Widget_Pointer) is
      begin
         if WP /= null and then WP.Name /= null then
            TIO.Put (LFile, WP.Name.all);
         end if;
         TIO.New_Line (LFile);
      end Put_Widget_Pointer_Name;

      procedure Dump_Children;
      procedure Dump_Children is
         TWdg : Widget_Pointer;
      begin
         TWdg := TWin.Widget_List;
         while TWdg /= null loop
            Dump_Widget (TWdg, Id + 3);
            TWdg := TWdg.Next;
         end loop;
      end Dump_Children;

      procedure Dump_Associated_Widget;
      procedure Dump_Associated_Widget is
      begin
         Put_Property ("Associated Widget");
         Put_Widget_Pointer_Name (TWin.Associated_Widget);
      end Dump_Associated_Widget;

   begin --  dump window
      TIO.New_Line (LFile);
      TIO.Put (LFile, To_Gtk (TWin));
      TIO.New_Line (LFile);

      Put_Property ("Name");
      Put_String_Access (TWin.Name);
      Put_Property ("Original Name");
      Put_String_Access (TWin.Original_Name);

      case TWin.Window_Type is
         when GtkWindow =>
            Put_Property ("Title");
            Put_String_Access (TWin.Title, True);

            Put_Property ("Number of Children");
            Put_Integer (TWin.Num_Children);

            Put_Property ("Resizable");
            Put_Boolean (TWin.Resizable);

            Put_Property ("Has Buttons");
            Put_Boolean (TWin.Has_Buttons);

            Put_Property ("Modal");
            Put_Boolean (TWin.Modal);

            Put_Property ("Font");
            if TWin.Font_Name /= null then
               TIO.Put (LFile, TWin.Font_Name.all);
               TIO.Put (LFile, " " & Img (TWin.Font_Size));
               if TWin.Font_Weight /= null then
                  TIO.Put (LFile, " " & "+" (TWin.Font_Weight.all));
               end if;
               if TWin.Font_Underline then
                  TIO.Put (LFile, " underline");
               end if;
            end if;
            TIO.New_Line (LFile);

            Put_Property ("Icon");
            Put_String_Access (TWin.Icon);

            Put_Property ("ToolTip");
            Put_String_Access (TWin.ToolTip, True);

            Put_Property ("Position");
            TIO.Put_Line (LFile, TWin.Start_Position'Image);

            Put_Property ("Size");
            TIO.Put (LFile, "H=" & Img (TWin.Client_Size.Horiz));
            TIO.Put (LFile, ", V=" & Img (TWin.Client_Size.Vert));
            TIO.New_Line (LFile);

            Put_Property ("Margin");
            TIO.Put (LFile, "Top " & Img (TWin.Margins (2))
                     & ", Bottom " & Img (TWin.Margins (4))
                     & ", Start " & Img (TWin.Margins (1))
                     & ", End " & Img (TWin.Margins (3)));
            TIO.New_Line (LFile);

            Put_Property ("AutoScale");
            TIO.Put (LFile, "Horiz " & Img (TWin.AutoScaleDim.Horiz));
            TIO.Put (LFile, ", Vert " & Img (TWin.AutoScaleDim.Vert));
            TIO.New_Line (LFile);

            Put_Property ("TryHeight");
            Put_Integer (TWin.TrayHeight);

            Put_Property ("Max Tab Index");
            Put_Integer (TWin.MaxTabIndex);

            Put_Property ("BGColor");
            Put_String_Access (TWin.BgColor);

            Put_Property ("FGColor");
            Put_String_Access (TWin.FgColor);

            Put_Property ("Accept Button");
            Put_Widget_Pointer_Name (TWin.Action_Buttons (OK_Response));

            Put_Property ("Cancel Button");
            Put_Widget_Pointer_Name (TWin.Action_Buttons (Cancel_Response));

            Dump_Signal_List;
            Dump_Children;

         when GtkFileChooserDialog =>
            Put_Property ("Title");
            Put_String_Access (TWin.Title, True);

            Put_Property ("Filter Name");
            Put_String_Access (TWin.FilterName);

            Put_Property ("Transient for");
            Put_String_Access (TWin.Transient_For.Name);

            Put_Property ("Attached to");
            Put_String_Access (TWin.Attached_To.Name);

            Dump_Signal_List;

         when GtkFileFilter =>
            Put_Property ("Title");
            Put_String_Access (TWin.Title, True);

            Put_Property ("Filter String");
            if TWin.FilterString /= null then
               TIO.Put (LFile, TWin.FilterString.all);
            end if;
            TIO.New_Line (LFile);

            Dump_Signal_List;

         when GtkEntryBuffer =>
            Dump_Associated_Widget;

            case TWin.Associated_Widget.Widget_Type is
               when GtkCalendar =>
                  Put_Property ("Min_Date");
                  TIO.Put_Line (LFile,
                                Image (TWin.Associated_Widget.MinDate,
                                  ISO_Date));

                  Put_Property ("Max_Date");
                  TIO.Put_Line (LFile,
                                Image (TWin.Associated_Widget.MaxDate,
                                  ISO_Date));

               when GtkEntry | GtkComboTextBox =>
                  declare
                     Idx0 : Integer;
                     Idx1 : Integer;
                     Txt  : constant String :=
                       TWin.Associated_Widget.Text_Buffer.all;
                  begin
                     Idx0 := Index (Txt, (1 .. 1 => ASCII.CR));
                     if Idx0 not in Txt'Range then
                        Put_Property ("Text Buffer");
                        TIO.Put_Line (LFile, Txt);
                     else
                        Put_Property ("Text Buffer");
                        TIO.Put_Line (LFile, Txt (Txt'First .. Idx0 - 1));
                        loop
                           Idx0 := Idx0 + 1; -- skip CR
                           Idx1 := Index (Txt (Idx0 .. Txt'Last),
                                          (1 .. 1 => ASCII.CR));
                           exit when Idx1 not in Idx0 .. Txt'Last;
                           Put_Property ("");
                           TIO.Put_Line (LFile, Txt (Idx0 .. Idx1 - 1));
                           Idx0 := Idx1; -- idx0 now is CR
                        end loop;
                        if Idx0 < Txt'Last then
                           Put_Property ("");
                           TIO.Put_Line (LFile, Txt (Idx0 .. Txt'Last));
                        end if;
                     end if;
                  end;
               when others => null;
            end case;
            Dump_Signal_List;

         when GtkModelFilter =>
            Put_Property ("Underlying Model");
            Put_String_Access (TWin.Underlaying_Model.Name);

            Dump_Associated_Widget;
            Dump_Signal_List;

         when GtkModelSort =>
            Put_Property ("Underlying Model");
            Put_String_Access (TWin.Underlaying_Model.Name);

            Dump_Associated_Widget;
            Dump_Signal_List;

         when GtkListStore =>
            Put_Property ("Num. Elements");
            Put_Integer (TWin.Num_Elements);

            Dump_Associated_Widget;
            Dump_Signal_List;

         when GtkTreeStore =>
            Put_Property ("Num. Elements");
            Put_Integer (TWin.Num_Elements);

            Dump_Associated_Widget;
            Dump_Signal_List;

         when GtkImage =>
            Dump_Associated_Widget;
            Put_Property ("Image");
            case TWin.Associated_Widget.Widget_Type is
               when GtkButton | GtkRadioButton
                  | GtkCheckButton | GtkToggleButton =>
                  TIO.Put_Line (LFile,
                                TWin.Associated_Widget.ImagePath.all);
               when GtkMenuNormalItem | GtkMenuImageItem
                  | GtkMenuRadioItem | GtkMenuCheckItem =>
                  TIO.Put_Line (LFile,
                                TWin.Associated_Widget.ImageMenu.all);
               when others => null;
            end case;
            Dump_Signal_List;

      end case;
   end Dump_Window;

   ----------------------------------------------------------------
   procedure Dump_Widget (TWdgP : Widget_Pointer; Id : Integer) is

      procedure Put_Property (PName : String);
      procedure Put_Property (PName : String) is
      begin
         TIO.Put (LFile, Sp (Id + 3)
                  & PName
                  & Sp (24 - PName'Length));
      end Put_Property;

      procedure Put_Window_Pointer_Name (WP : Window_Pointer);
      procedure Put_Window_Pointer_Name (WP : Window_Pointer) is
      begin
         if WP /= null and then WP.Name /= null then
            TIO.Put (LFile, WP.Name.all);
         end if;
         TIO.New_Line (LFile);
      end Put_Window_Pointer_Name;

      procedure Put_Widget_Pointer_Name (WP : Widget_Pointer);
      procedure Put_Widget_Pointer_Name (WP : Widget_Pointer) is
      begin
         if WP /= null and then WP.Name /= null then
            TIO.Put (LFile, WP.Name.all);
         end if;
         TIO.New_Line (LFile);
      end Put_Widget_Pointer_Name;

      procedure Dump_Children;
      procedure Dump_Children is
         TWdgChild : Widget_Pointer;
      begin
         TWdgChild := TWdgP.Child_List;
         while TWdgChild /= null loop
            Dump_Widget (TWdgChild, Id + 3);
            TWdgChild := TWdgChild.Next;
         end loop;
      end Dump_Children;

      procedure Dump_Signals;
      procedure Dump_Signals is
         TS : Signal_Pointer;
      begin
         TS := TWdgP.Signal_List;
         while TS /= null loop
            if TS.Glade then
               if TS.After then
                  Put_Property ("Glade Signal (after)");
               else
                  Put_Property ("Glade Signal (before)");
               end if;
            else
               if TS.After then
                  Put_Property ("Signal (after)");
               else
                  Put_Property ("Signal (before)");
               end if;
            end if;
            TIO.Put (LFile, TS.Name.all
                     & " [Gtk " & TS.GtkName.all & "] ");
            if TS.Handler /= null then
               if TS.GAda then
                  if TS.Proc then
                     TIO.Put (LFile, " => procedure " & TS.Handler.all);
                  else
                     TIO.Put (LFile, " => function " & TS.Handler.all);
                  end if;
               else
                  TIO.Put (LFile, " => No generated code");
               end if;
            end if;
            TIO.New_Line (LFile);
            TS := TS.Next;
         end loop;
      end Dump_Signals;

   begin
      TIO.New_Line (LFile);

      --  common fields
      TIO.Put_Line (LFile, Sp (Id) & To_Gtk (TWdgP));

      Put_Property ("Name");
      Put_String_Access (TWdgP.Name);

      if TWdgP.Widget_Type /= GtkTabChild then
         Put_Property ("Windows Form");
         Put_String_Access (TWdgP.Windows_Type);
      end if;

      Put_Property ("Number of Children");
      Put_Integer (TWdgP.Num_Children);

      Put_Property ("Parent Name");
      if TWdgP.Parent_Name /= null then
         Put_String_Access (TWdgP.Parent_Name);
      elsif TWdgP.Wdg_Parent /= null
        and then TWdgP.Wdg_Parent.Windows_Type /= null
      then
         TIO.Put_Line (LFile, To_Gtk (TWdgP.Wdg_Parent));
      elsif TWdgP.Win_Parent /= null then
         TIO.Put_Line (LFile, To_Gtk (TWdgP.Win_Parent));
      end if;

      Put_Property ("Child Number");
      Put_Integer (TWdgP.Child_Number);

      if TWdgP.Widget_Type /= GtkTabChild then
         Put_Property ("Location");
         TIO.Put (LFile, "x=" & Img (TWdgP.Location.From_Left));
         TIO.Put_Line (LFile, ", y=" & Img (TWdgP.Location.From_Top));

         Put_Property ("Size");
         TIO.Put (LFile, "H=" & Img (TWdgP.Size.Horiz));
         TIO.Put_Line (LFile, ", V=" & Img (TWdgP.Size.Vert));

         Put_Property ("TabIndex");
         Put_Integer (TWdgP.TabIndex);

         Put_Property ("TabStop");
         Put_Triboolean (TWdgP.TabStop);

         Put_Property ("Has Focus");
         Put_Boolean (TWdgP.Has_Focus);

         Put_Property ("Zorder");
         Put_Integer (TWdgP.Zorder);

         Put_Property ("Enabled");
         Put_Boolean (TWdgP.Enabled);

         Put_Property ("Visible");
         Put_Boolean (TWdgP.Visible);

         Put_Property ("Text");
         Put_String_Access (TWdgP.Text, True);

         Put_Property ("TextAlign");
         TIO.Put_Line (LFile, TWdgP.TextAlign'Image);

         Put_Property ("AutoSize");
         Put_Boolean (TWdgP.AutoSize);

         Put_Property ("AutoSizeMode");
         TIO.Put_Line (LFile, TWdgP.AutoSizeMode'Image);

         Put_Property ("HScrollBar");
         TIO.Put_Line (LFile, TWdgP.H_ScrollBar'Image);

         Put_Property ("VScrollBar");
         TIO.Put_Line (LFile, TWdgP.V_ScrollBar'Image);

         Put_Property ("Font");
         if TWdgP.Font_Name /= null then
            TIO.Put (LFile, TWdgP.Font_Name.all);
            TIO.Put (LFile, " " & Img (TWdgP.Font_Size));
            if TWdgP.Font_Weight /= null then
               TIO.Put (LFile, " " & "+" (TWdgP.Font_Weight.all));
            end if;
            if TWdgP.Font_Underline then
               TIO.Put (LFile, " underline");
            end if;
         end if;
         TIO.New_Line (LFile);

         Put_Property ("Margin");
         TIO.Put_Line (LFile, "Top " & Img (TWdgP.Margins (2))
                       & ", Bottom " & Img (TWdgP.Margins (4))
                       & ", Start " & Img (TWdgP.Margins (1))
                       & ", End " & Img (TWdgP.Margins (3)));

         Put_Property ("Padding");
         Put_Integer (TWdgP.Padding);

         Put_Property ("DisplayStyle");
         TIO.Put_Line (LFile, TWdgP.DStyle'Image);

         Put_Property ("MaxLength");
         Put_Integer (TWdgP.MaxLength);

         Put_Property ("AutoToolTip");
         Put_Boolean (TWdgP.AutoToolTip);

         Put_Property ("ToolTip");
         Put_String_Access (TWdgP.ToolTip, True);

         Put_Property ("UseVisualStyleBackColor");
         Put_Boolean (TWdgP.UseVisualStyleBackColor);

         Put_Property ("BGColor");
         Put_String_Access (TWdgP.BgColor);

         Put_Property ("FGColor");
         Put_String_Access (TWdgP.FgColor);

         Put_Property ("UlColor");
         Put_String_Access (TWdgP.UlColor);

         Put_Property ("Flow Direction");
         TIO.Put_Line (LFile, TWdgP.FlowDirection'Image);
      end if;

      case TWdgP.Widget_Type is
         when No_Widget =>
            null;

         when GtkAlignment =>
            Put_Property ("Halign");
            TIO.Put_Line (LFile, Img (TWdgP.Halign, 0));
            Put_Property ("Valign");
            TIO.Put_Line (LFile, Img (TWdgP.Valign, 0));
            Put_Property ("HScale");
            TIO.Put_Line (LFile, Img (TWdgP.HScale, 0));
            Put_Property ("VScale");
            TIO.Put_Line (LFile, Img (TWdgP.VScale, 0));
            Put_Property ("Padding");
            TIO.Put (LFile, "Start " & Img (TWdgP.Paddings (1))
                     & ", Top " & Img (TWdgP.Paddings (2))
                     & ", End " & Img (TWdgP.Paddings (3))
                     & ", Bottom " & Img (TWdgP.Paddings (4)));
            TIO.New_Line (LFile);

         when GtkAspectFrame =>
            Put_Property ("H_Alignment");
            TIO.Put_Line (LFile, Img (TWdgP.H_Alignment, 0));
            Put_Property ("V_Alignment");
            TIO.Put_Line (LFile, Img (TWdgP.V_Alignment, 0));
            Put_Property ("Ratio");
            TIO.Put_Line (LFile, Img (TWdgP.Ratio_If_No_Obey, 0));
            Put_Property ("Obey");
            Put_Boolean (TWdgP.Obey);
            Put_Property ("Label_Xalign");
            TIO.Put_Line (LFile, Img (TWdgP.Label_Xalign, 0));
            Put_Property ("Label_Yalign");
            TIO.Put_Line (LFile, Img (TWdgP.Label_Yalign, 0));
            Put_Property ("Frame_Shadow");
            TIO.Put_Line (LFile, TWdgP.Frame_Shadow'Image);

         when GtkFrame =>
            Put_Property ("Label_Xalign");
            TIO.Put_Line (LFile, Img (TWdgP.Label_Xalign, 0));
            Put_Property ("Label_Yalign");
            TIO.Put_Line (LFile, Img (TWdgP.Label_Yalign, 0));
            Put_Property ("Frame_Shadow");
            TIO.Put_Line (LFile, TWdgP.Frame_Shadow'Image);

         when GtkScrolledWindow =>
            Put_Property ("Frame_Shadow");
            TIO.Put (LFile, TWdgP.Frame_Shadow'Image);

         when GtkMenuItem | GtkSubMenu => null;

         when GtkSeparatorMenuItem => null;

         when GtkMenuNormalItem | GtkMenuImageItem
            | GtkMenuRadioItem | GtkMenuCheckItem =>
            Put_Property ("ImageMenu Window");
            Put_Window_Pointer_Name (TWdgP.ImageMenuWin);

            Put_Property ("ImageMenu");
            Put_String_Access (TWdgP.ImageMenu);

         when GtkDataGridView | GtkTreeGridView
            | ExpandableColumn | DataGridViewTextBoxColumn
            | DataGridViewCheckBoxColumn
            =>
            Put_Property ("Default CellStyle");
            if TWdgP.DefaultCellStyle in DGVS.all'Range then
               TIO.Put_Line (LFile,
                             DGVS (TWdgP.DefaultCellStyle).Name.all);
            else
               TIO.New_Line (LFile);
            end if;

            Put_Property ("ReadOnly");
            Put_Boolean (TWdgP.ReadOnly);

            case TWdgP.Widget_Type is
               when GtkDataGridView | GtkTreeGridView =>
                  case TWdgP.Widget_Type is
                     when GtkDataGridView =>
                        Put_Property ("Has Expander");
                        Put_Boolean (TWdgP.Has_Expander);
                     when others => null;
                  end case;

                  Put_Property ("ColumnHeadersVisible");
                  Put_Boolean (TWdgP.ColumnHeadersVisible);

                  Put_Property ("Alt. R. Def. CellStyle");
                  if TWdgP.AlternatingRowsDefaultCellStyle in DGVS.all'Range
                  then
                     TIO.Put_Line
                       (LFile,
                        DGVS
                          (TWdgP.AlternatingRowsDefaultCellStyle).Name.all);
                  else
                     TIO.New_Line (LFile);
                  end if;

                  Put_Property ("Col. H. Def. CellStyle");
                  if TWdgP.ColumnHeadersDefaultCellStyle in DGVS.all'Range
                  then
                     TIO.Put_Line
                       (LFile,
                        DGVS (TWdgP.ColumnHeadersDefaultCellStyle).Name.all);
                  else
                     TIO.New_Line (LFile);
                  end if;

                  Put_Property ("Row H. Def. CellStyle");
                  if TWdgP.RowHeadersDefaultCellStyle in DGVS.all'Range
                  then
                     TIO.Put_Line
                       (LFile,
                        DGVS (TWdgP.RowHeadersDefaultCellStyle).Name.all);
                  else
                     TIO.New_Line (LFile);
                  end if;

                  Put_Property ("Col. H. HeightSizeMode");
                  TIO.Put_Line (LFile, TWdgP.ColumnHeadersHeightSizeMode'Image);

                  Put_Property ("AllowUserToAddRows");
                  Put_Boolean (TWdgP.AllowUserToAddRows);

                  Put_Property ("AllowUserToDeleteRows");
                  Put_Boolean (TWdgP.AllowUserToDeleteRows);

                  Put_Property ("AllowUserToDeleteRows");
                  Put_Boolean (TWdgP.AllowUserToDeleteRows);

                  Put_Property ("EnableHdersVisualStyles");
                  Put_Boolean (TWdgP.EnableHeadersVisualStyles);

                  Put_Property ("RowMultiSelect");
                  Put_Boolean (TWdgP.RowMultiSelect);

                  Put_Property ("RowHeadersVisible");
                  Put_Boolean (TWdgP.RowHeadersVisible);

                  Put_Property ("EditModeProgramatically");
                  Put_Boolean (TWdgP.EditModeProgramatically);

                  Put_Property ("ImageList");
                  Put_Widget_Pointer_Name (TWdgP.ImageList);

                  Put_Property ("RowHeadersWidthSizeMode");
                  TIO.Put_Line (LFile, TWdgP.RowHeadersWidthSizeMode'Image);

                  Put_Property ("RowHeadersBorderStyle");
                  TIO.Put (LFile, TWdgP.RowHeadersBorderStyle'Image);

                  Put_Property ("Model");
                  Put_Window_Pointer_Name (TWdgP.Model);

               when ExpandableColumn | DataGridViewTextBoxColumn
                  | DataGridViewCheckBoxColumn
                  =>
                  Put_Property ("Fixed Width");
                  Put_Integer (TWdgP.Fixed_Width);

                  Put_Property ("Min Width");
                  Put_Integer (TWdgP.Min_Width);

                  Put_Property ("Max Width");
                  Put_Integer (TWdgP.Max_Width);

                  Put_Property ("UserAddedColumn");
                  Put_Boolean (TWdgP.UserAddedColumn);

                  Put_Property ("Level");
                  Put_Integer (TWdgP.Level);

                  Put_Property ("PaddingX");
                  Put_Integer (TWdgP.PaddingX);

                  Put_Property ("PaddingY");
                  Put_Integer (TWdgP.PaddingY);

                  Put_Property ("Resizable");
                  Put_Boolean (TWdgP.Resizable);

                  Put_Property ("SortMode");
                  TIO.Put_Line (LFile, TWdgP.SortMode'Image);

                  Put_Property ("AutoSizeColumnMode");
                  TIO.Put_Line (LFile, TWdgP.AutoSizeColumnMode'Image);

                  Put_Property ("DefaultNodeImage");
                  Put_Widget_Pointer_Name (TWdgP.DefaultNodeImage);

                  Put_Property ("Frozen");
                  Put_Boolean (TWdgP.Frozen);

                  case TWdgP.Widget_Type is
                     when DataGridViewCheckBoxColumn =>
                        Put_Property ("Active Column");
                        Put_Integer
                          (TWdgP.CheckBox_Col_Properties.Active_Column);

                        Put_Property ("Activatable Column");
                        Put_Integer
                          (TWdgP.CheckBox_Col_Properties.Activatable_Column);
                     when others => null;
                  end case;

               when others => null;
            end case;

         when GtkNoteBook =>
            Put_Property ("Scrollable");
            Put_Boolean (TWdgP.Scrollable);

            Put_Property ("Enable Popup");
            Put_Boolean (TWdgP.Enable_Popups);

            Put_Property ("Show Tabs");
            Put_Boolean (TWdgP.Show_Tabs);

            Put_Property ("Show Borders");
            Put_Boolean (TWdgP.Show_Border);

            Put_Property ("Pack Start");
            Put_Boolean (TWdgP.Pack_Start);

            Put_Property ("Tab CloseButton Visible");
            Put_Boolean (TWdgP.CloseButtonOnTabsInactiveVisible);

            Put_Property ("Selected Index");
            Put_Integer (TWdgP.SelectedIndex);

         when GtkTabChild =>
            null;

         when GtkTabPage =>
            Put_Property ("Label");
            Put_Widget_Pointer_Name (TWdgP.The_Label);

            Put_Property ("Button");
            Put_Widget_Pointer_Name (TWdgP.The_Button);

         when GtkEntry | GtkComboTextBox | GtkCalendar =>
            Put_Property ("Buffer");
            Put_Window_Pointer_Name (TWdgP.Buffer);

            Put_Property ("Text Buffer");
            Put_String_Access (TWdgP.Text_Buffer);

            case TWdgP.Widget_Type is
               when GtkEntry | GtkComboTextBox =>
                  Put_Property ("Editable");
                  Put_Boolean (TWdgP.Editable);

                  Put_Property ("Has Frame");
                  Put_Boolean (TWdgP.Has_Frame);

                  Put_Property ("PasswordChar");
                  Put_String_Access (TWdgP.PasswordChar);

                  if TWdgP.Widget_Type = GtkComboTextBox then
                     Put_Property ("Sorted");
                     Put_Boolean (TWdgP.Editable);
                  end if;

               when GtkCalendar =>
                  Put_Property ("StartDate");
                  TIO.Put_Line (LFile, Image (TWdgP.Start_Date, ISO_Date));

                  Put_Property ("MinDate");
                  TIO.Put_Line (LFile, Image (TWdgP.MinDate, ISO_Date));

                  Put_Property ("MaxDate");
                  TIO.Put_Line (LFile, Image (TWdgP.MaxDate, ISO_Date));

                  Put_Property ("Format Date");
                  Put_String_Access (TWdgP.Format_Date);

                  Put_Property ("ShowUpDown");
                  Put_Boolean (TWdgP.ShowUpDown);

               when others => null;
            end case;

         when GtkSpinButton =>
            Put_Property ("Start");
            Put_Integer (TWdgP.StartValue);

            Put_Property ("MaxValue");
            Put_Integer (TWdgP.MaxValue);

            Put_Property ("MinValue");
            Put_Integer (TWdgP.MinValue);

            Put_Property ("Step");
            Put_Integer (TWdgP.Step);

         when GtkFileChooserButton
            | PrintDocument | PrintDialog | PageSetupDialog
            | FolderBrowserDialog | GtkToolTip | GtkColorButton
            | GtkStatusBar | GtkToolBar | GtkMenuBar | BackgroundWorker
            | BindingNavigator
            =>

            Put_Property ("TrayLocation");
            TIO.Put (LFile, "x=" & Img (TWdgP.TrayLocation.From_Left));
            TIO.Put (LFile, ", y=" & Img (TWdgP.TrayLocation.From_Top));
            TIO.New_Line (LFile);

            case TWdgP.Widget_Type is
               when GtkFileChooserButton =>
                  Put_Property ("OpenFileDialog");
                  Put_String_Access (TWdgP.OpenFileDialog);

                  Put_Property ("OpenFileFilter");
                  Put_String_Access (TWdgP.OpenFileFilter);

                  Put_Property ("OpenFileTitle");
                  Put_String_Access (TWdgP.OpenFileTitle);

               when GtkColorButton =>
                  Put_Property ("AnyColor");
                  Put_Boolean (TWdgP.AnyColor);

                  Put_Property ("FullOpen");
                  Put_Boolean (TWdgP.FullOpen);

                  Put_Property ("Associated Button");
                  Put_Widget_Pointer_Name (TWdgP.Associated_Button);

               when GtkToolBar | GtkMenuBar | BindingNavigator =>
                  Put_Property ("Image Scaling Size");
                  TIO.Put (LFile, "H="
                           & Img (TWdgP.ImageScalingSize.One));
                  TIO.Put_Line (LFile, ", V="
                                & Img (TWdgP.ImageScalingSize.Two));

                  case TWdgP.Widget_Type is
                     when GtkToolBar | BindingNavigator =>
                        Put_Property ("Horizontal");
                        Put_Boolean (TWdgP.TB_Horiz);

                        Put_Property ("Show Arrows");
                        Put_Boolean (TWdgP.Show_Arrows);

                        Put_Property ("Grip Visible");
                        Put_Boolean (TWdgP.Grip_Visible);

                        case TWdgP.Widget_Type is
                           when BindingNavigator =>
                              Put_Property ("AddNewItem");
                              Put_Widget_Pointer_Name (TWdgP.AddNewItem);

                              Put_Property ("CountItem");
                              Put_Widget_Pointer_Name (TWdgP.CountItem);

                              Put_Property ("DeleteItem");
                              Put_Widget_Pointer_Name (TWdgP.DeleteItem);

                              Put_Property ("MoveFirstItem");
                              Put_Widget_Pointer_Name (TWdgP.MoveFirstItem);

                              Put_Property ("MoveLastItem");
                              Put_Widget_Pointer_Name (TWdgP.MoveLastItem);

                              Put_Property ("MoveNextItem");
                              Put_Widget_Pointer_Name (TWdgP.MoveNextItem);

                              Put_Property ("MovePreviousItem");
                              Put_Widget_Pointer_Name
                                (TWdgP.MovePreviousItem);

                              Put_Property ("PositionItem");
                              Put_Widget_Pointer_Name (TWdgP.PositionItem);
                           when others => null;
                        end case;
                     when others => null;
                  end case;

               when PageSetupDialog =>
                  Put_Property ("EnableMetric");
                  Put_Boolean (TWdgP.EnableMetric);

                  Put_Property ("MinMargins");
                  TIO.Put_Line (LFile,
                                "Top " & Img (TWdgP.MinMargins (2))
                                & ", Bottom " & Img (TWdgP.MinMargins (4))
                                & ", Start " & Img (TWdgP.MinMargins (1))
                                & ", End " & Img (TWdgP.MinMargins (3)));

               when BackgroundWorker =>
                  Put_Property ("Report Progress");
                  Put_Boolean (TWdgP.WorkerReportsProgress);

                  Put_Property ("Support Cancellation");
                  Put_Boolean (TWdgP.WorkerSupportsCancellation);

               when others => null;
            end case;

         when GtkListBox =>
            Put_Property ("MultiSelect");
            Put_Boolean (TWdgP.MultiSelect);

            Put_Property ("ListStore    ");
            Put_Window_Pointer_Name (TWdgP.ListStore);

         when GtkImage =>
            Put_Property ("ImagePath");
            Put_String_Access (TWdgP.Image);

         when GtkButton | GtkRadioButton
            | GtkCheckButton | GtkToggleButton
            | GtkLabel | ToolStripStatusLabel
            =>
            Put_Property ("Underline");
            Put_Boolean (TWdgP.Underline);

            case TWdgP.Widget_Type is
               when GtkLabel | ToolStripStatusLabel =>
                  Put_Property ("BorderStyle");
                  TIO.Put_Line (LFile, TWdgP.BorderStyle'Image);

               when GtkButton | GtkRadioButton
                  | GtkCheckButton | GtkToggleButton
                  =>
                  Put_Property ("Active");
                  Put_Boolean (TWdgP.Active);

                  Put_Property ("ImagePath");
                  Put_String_Access (TWdgP.ImagePath);

                  Put_Property ("ImageAlign");
                  TIO.Put_Line (LFile, TWdgP.ImageAlign'Image);

                  Put_Property ("Windows Image");
                  Put_Window_Pointer_Name (TWdgP.Win_Image);

                  case TWdgP.Widget_Type is
                     when GtkButton =>
                        Put_Property ("Dialog Result");
                        TIO.Put_Line (LFile, TWdgP.Dialog_Result'Image);

                        Put_Property ("Associated ColorButton");
                        Put_Widget_Pointer_Name
                          (TWdgP.Associated_ColorButton);

                     when GtkCheckButton =>
                        Put_Property ("CheckAlign");
                        Put_String_Access (TWdgP.CheckAlign);

                     when others => null;
                  end case;
               when others => null;
            end case;

         when Chart =>
            Put_Property ("Anchor");
            Put_String_Access (TWdgP.Anchor);

         when GtkSeparatorToolItem =>
            null;

         when GtkBox | GtkButtonBox | Internal_Child_VBox =>
            Put_Property ("Spacing");
            Put_Integer (TWdgP.Spacing);
            Put_Property ("Orientation");
            if TWdgP.Orientation = Horizontal then
               TIO.Put_Line (LFile, "Horizontal");
            else
               TIO.Put_Line (LFile, "Vertical");
            end if;

            Put_Property ("Homogeneus");
            Put_Boolean (TWdgP.Homogeneus);
            Put_Property ("Baseline");
            TIO.Put_Line (LFile, TWdgP.Baseline'Image);

            case TWdgP.Widget_Type is
               when GtkButtonBox =>
                  Put_Property ("Layout_Style");
                  TIO.Put_Line (LFile, TWdgP.Layout_Style'Image);
               when others => null;
            end case;

         when GtkFixed  => null;
         when Internal_Child_Action_Area => null;
         when Action_Widgets => null;
      end case;

      Put_Property ("----");
      TIO.New_Line (LFile);

      Dump_Signals;
      Dump_Children;

   end Dump_Widget;

   ----------------------------------------------------------------
   procedure Dump (Path : String; File_Name : String; Instant : String) is
   begin
      TIO.Create (File => LFile,
                  Mode => TIO.Out_File,
                  Name => Path & "/" & File_Name & ".dump");
      TIO.Put_Line (LFile, "Generating Dump by w2gtk " & Version &
                      " - " & Instant);
      Dump_DGVS;
      TWin := Win_List;
      while TWin /= null loop
         Dump_Window (TWin, 0);
         TWin := Next_Window (Win_List, TWin);
      end loop;

      TIO.Close (LFile);
      Debug (-1, "End of generating Dump");

   end Dump;

end W2gtk_Dump_Pkg;
