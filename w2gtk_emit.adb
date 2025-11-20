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

with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with GNAT.Strings;          use GNAT.Strings;
with GNAT.Calendar.Time_IO;
with W2gtk_Version;         use W2gtk_Version;

with Emit_Tools;            use Emit_Tools;
with Emit_Containers;       use Emit_Containers;

package body W2gtk_Emit is
   package TIO renames Ada.Text_IO;

   procedure Emit_GtkDialog (TWin : Window_Pointer;
                             Id   : Integer);
   procedure Emit_Main_GtkWindow (TWin : Window_Pointer;
                                  Id   : Integer);

   -----------------------------------------------------------------------
   --  Emit_GtkHeader
   -----------------------------------------------------------------------

   procedure Emit_GtkHeader (TWin : Window_Pointer;
                             Id   : Integer) is
      pragma Unreferenced (TWin);
   begin
      Emit_Line ("<?xml version=""1.0"" encoding=""UTF-8""?>");
      --  Emit_Line ("<!-- Generated with glade 3.40.0 -->");
      Emit_Line ("<!-- Generated with w2gtk " & Version & " -->");
      Emit_Line ("<interface>");
      Emit_Line (Sp (Id + 2) & "<requires lib=""gtk+"" version=""3.24""/>");
      Emit_Line (Sp (Id + 2) & "<!-- interface-local-resource-path / -->");
   end Emit_GtkHeader;

   procedure Emit_GtkTrailer (TWin : Window_Pointer; Id : Integer) is
      pragma Unreferenced (TWin, Id);
   begin
      Emit_Line ("</interface>");
   end Emit_GtkTrailer;


   --  procedure Emit_Alignment (TWdg : Widget_Pointer; Id : Integer) is
   --  begin
   --     Emit_Child (TWdg, Id, False);
   --     Emit_Object (TWdg, Id + 2, "GtkAlignment",
   --                  "Alignment_" & TWdg.Name.all);
   --     Emit_Name (TWdg, Id + 4);
   --     Emit_WH_Request (TWdg, Id + 4);
   --     Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
   --     Emit_Line (Sp (Id + 4)
   --                & "<property name=""left-padding"">12</property>");
   --
   --     Child := TWdg.Child_List;
   --     while Child /= null loop
   --
   --
   --
   --
   --
   --  end Emit_Alignment;
   --


   -----------------------------------------------------------------------
   --  Emit_Store
   -----------------------------------------------------------------------

   procedure Emit_Store (TWin : Window_Pointer;
                         Id   : Integer);
   procedure Emit_Store (TWin : Window_Pointer;
                         Id   : Integer) is
      Col  : Widget_Pointer;
      TWdg : constant Widget_Pointer := TWin.Associated_Widget;
   begin
      Emit_Line (Sp (Id + 2) & "<columns>");
      Col := TWdg.Child_List;
      while Col /= null loop
         Emit_Line (Sp (Id + 4) & "<!-- column-name "
                    & Col.Name.all
                    & " -->");
         case Col.Widget_Type is
            when ExpandableColumn | DataGridViewTextBoxColumn =>
               if Col.DefaultCellStyle in DGVS'Range then
                  case DGVS (Col.DefaultCellStyle).Format is
                     when Format_Boolean =>
                        Emit_Line (Sp (Id + 4)
                                   & "<column type=""gboolean""/>");
                     when Format_Integer =>
                        Emit_Line (Sp (Id + 4)
                                   & "<column type=""gchararray""/>");
                     when Format_Real =>
                        Emit_Line (Sp (Id + 4)
                                   & "<column type=""gchararray""/>");
                     when Format_Exponential =>
                        Emit_Line (Sp (Id + 4)
                                   & "<column type=""gchararray""/>");
                     when Format_Decimal =>
                        Emit_Line (Sp (Id + 4)
                                   & "<column type=""gchararray""/>");
                     when Format_Date =>
                        Emit_Line (Sp (Id + 4)
                                   & "<column type=""gchararray""/>");
                     when Format_Percent =>
                        Emit_Line (Sp (Id + 4)
                                   & "<column type=""gchararray""/>");
                     when Format_Currency =>
                        Emit_Line (Sp (Id + 4)
                                   & "<column type=""gchararray""/>");
                     when Format_String =>
                        Emit_Line (Sp (Id + 4)
                                   & "<column type=""gchararray""/>");
                  end case;
               else
                  Emit_Line (Sp (Id + 4) & "<column type=""gchararray""/>");
               end if;
            when DataGridViewCheckBoxColumn =>
               Emit_Line (Sp (Id + 4)
                          & "<column type=""GtkButtonBoxStyle""/>");
            when others => null;
         end case;
         Col := Col.Next;
      end loop;

      Col := TWdg.Child_List;
      while Col /= null loop
         case Col.Widget_Type is
            when DataGridViewCheckBoxColumn =>
               if Col.CheckBox_Col_Properties.Active_Column /= -1 then
                  Emit_Line (Sp (Id + 4) & "<!-- column-name "
                             & Col.Name.all & "_data"
                             & " -->");
                  Emit_Line (Sp (Id + 4)
                             & "<column type=""gboolean""/>");

                  if not Col.ReadOnly then
                     Emit_Line (Sp (Id + 4) & "<!-- column-name "
                                & Col.Name.all & "_activatable"
                                & " -->");
                     Emit_Line (Sp (Id + 4)
                                & "<column type=""gboolean""/>");
                  end if;
               end if;
            when ExpandableColumn | DataGridViewTextBoxColumn =>
               if Col.Text_Col_Properties.Fg_Color_Name_Column /= -1 then
                  Emit_Line (Sp (Id + 4) & "<!-- column-name "
                             & Col.Name.all & "_fg_color"
                             & " -->");
                  Emit_Line (Sp (Id + 4)
                             & "<column type=""gchararray""/>");
               end if;
            when others => null;
         end case;
         Col := Col.Next;
      end loop;

      if DGVS /= null then
         if TWdg.AlternatingRowsDefaultCellStyle in DGVS'Range then
            Emit_Line (Sp (Id + 4) & "<!-- column-name "
                       & "ALT_Bg_" & TWdg.Name.all
                       & " -->");
            Emit_Line (Sp (Id + 4) & "<column type=""gchararray""/>");
         end if;
      end if;

      Emit_Line (Sp (Id + 2) & "</columns>");
   end Emit_Store;

   -----------------------------------------------------------------------
   --  Emit_GtkModelFilter
   -----------------------------------------------------------------------

   procedure Emit_GtkModelFilter (TWin : Window_Pointer;
                                  Id   : Integer) is
   begin
      if TWin.Name /= null and then TWin.Name.all /= "" then
         Emit_Object (null, Id, "GtkTreeModelFilter", TWin.Name.all);
      else
         Emit_Object (null, Id, "GtkTreeModelFilter", "");
      end if;
      Emit_Property (Id + 2, "child-model", TWin.Underlaying_Model.Name.all);
      Emit_Line (Sp (Id) & "</object>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkModelFilter: " & TWin.Name.all);
         raise;
   end Emit_GtkModelFilter;

   -----------------------------------------------------------------------
   --  Emit_GtkModelSort
   -----------------------------------------------------------------------

   procedure Emit_GtkModelSort (TWin : Window_Pointer;
                                Id   : Integer) is
   begin
      if TWin.Name /= null and then TWin.Name.all /= "" then
         Emit_Object (null, Id, "GtkTreeModelSort", TWin.Name.all);
      else
         Emit_Object (null, Id, "GtkTreeModelSort", "");
      end if;
      Emit_Property (Id + 2, "model", TWin.Underlaying_Model.Name.all);
      Emit_Line (Sp (Id) & "</object>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkModelSort: " & TWin.Name.all);
         raise;
   end Emit_GtkModelSort;

   -----------------------------------------------------------------------
   --  Emit_GtkTreeStore
   -----------------------------------------------------------------------

   procedure Emit_GtkTreeStore (TWin : Window_Pointer;
                                Id   : Integer) is
   begin
      if TWin.Associated_Widget.Child_List /= null then
         if TWin.Name /= null and then TWin.Name.all /= "" then
            Emit_Object (null, Id, "GtkTreeStore", TWin.Name.all);
         else
            Emit_Object (null, Id, "GtkTreeStore", "");
         end if;
         Emit_Store (TWin, Id);
         Emit_Line (Sp (Id) & "</object>");
      else
         if TWin.Name /= null and then TWin.Name.all /= "" then
            Emit_Object (null, Id, "GtkTreeStore", TWin.Name.all, True);
         else
            Emit_Object (null, Id, "GtkTreeStore", "", True);
         end if;
      end if;
   exception
      when others =>
         TIO.Put_Line ("Emit GtkTreeStore: " & TWin.Name.all);
         raise;
   end Emit_GtkTreeStore;

   -----------------------------------------------------------------------
   -- Emit_GtkListStore --
   -----------------------------------------------------------------------

   procedure Emit_GtkListStore (TWin : Window_Pointer;
                                Id   : Integer) is
   begin
      if TWin.Associated_Widget.Widget_Type = GtkDataGridView
        and then TWin.Associated_Widget.Has_Expander
      then
         Emit_GtkTreeStore (TWin, Id);
      else
         if TWin.Associated_Widget.Child_List /= null then
            if TWin.Name /= null and then TWin.Name.all /= "" then
               Emit_Object (null, Id, "GtkListStore", TWin.Name.all);
            else
               Emit_Object (null, Id, "GtkListStore", "");
            end if;
            Emit_Store (TWin, Id);
            Emit_Line (Sp (Id) & "</object>");
         else
            if TWin.Name /= null and then TWin.Name.all /= "" then
               Emit_Object (null, Id, "GtkListStore", TWin.Name.all, True);
            else
               Emit_Object (null, Id, "GtkListStore", "", True);
            end if;
         end if;
      end if;
   exception
      when others =>
         TIO.Put_Line ("Emit GtkListStore: " & TWin.Name.all);
         raise;
   end Emit_GtkListStore;

   -----------------------------------------------------------------------
   --  Emit_GtkImage
   -----------------------------------------------------------------------

   procedure Emit_GtkImage (TWin : Window_Pointer;
                            Id   : Integer) is
   begin
      if TWin.Name /= null and then TWin.Name.all /= "" then
         Emit_Object (null, Id, "GtkImage", TWin.Name.all);
      else
         Emit_Object (null, Id, "GtkImage", "");
      end if;
      Emit_Name (TWin, Id + 2);
      Emit_Property (Id + 2, "visible", True);
      Emit_Property (Id + 2, "can-focus", False);
      case TWin.Associated_Widget.Widget_Type is
         when GtkMenuNormalItem | GtkMenuImageItem
            | GtkMenuRadioItem | GtkMenuCheckItem
            =>
            if TWin.Associated_Widget.ImageMenu /= null then
               Emit_Property (Id + 2, "pixbuf",
                              TWin.Associated_Widget.ImageMenu.all);
            end if;
         when GtkButton | GtkRadioButton
            | GtkCheckButton | GtkToggleButton
            =>
            if TWin.Associated_Widget.ImagePath /= null then
               Emit_Property (Id + 2, "pixbuf",
                              TWin.Associated_Widget.ImagePath.all);
            end if;
         when others => null;
      end case;
      Emit_Line (Sp (Id) & "</object>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkImage: " & TWin.Name.all);
         raise;
   end Emit_GtkImage;

   -----------------------------------------------------------------------
   -- Emit_GtkEntryBuffer --
   -----------------------------------------------------------------------

   procedure Emit_GtkEntryBuffer (TWin : Window_Pointer;
                                  Id   : Integer) is
      use GNAT.Calendar.Time_IO;
      TWdg : constant Widget_Pointer := TWin.Associated_Widget;
   begin
      if TWin.Name /= null and then TWin.Name.all /= "" then
         Emit_Object (null, Id, "GtkEntryBuffer", TWin.Name.all);
      else
         Emit_Object (null, Id, "GtkEntryBuffer", "");
      end if;
      case TWdg.Widget_Type is
         when GtkCalendar =>
            Emit_Property (Id + 2, "text",
                           Image (TWdg.MinDate, ISO_Date)
                           & ASCII.CR
                           & Image (TWdg.MaxDate, ISO_Date));
         when GtkEntry | GtkComboTextBox =>
            Emit_Property (Id + 2, "text", TWdg.Text_Buffer.all);
         when others => null;
      end case;
      Emit_Line (Sp (Id) & "</object>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkEntryBuffer: " & TWin.Name.all);
         raise;
   end Emit_GtkEntryBuffer;

   -----------------------------------------------------------------------
   --  Emit_GtkFileChooserDialog
   -----------------------------------------------------------------------

   procedure Emit_GtkFileChooserDialog (TWin : Window_Pointer;
                                        Id : Integer) is
      procedure Emit_Internal_Action_Area;
      procedure Emit_Internal_Action_Area is
      begin
         Emit_Line (Sp (Id + 6) & "<child internal-child=""action_area"">");
         Emit_Object (null, Id + 8, "GtkButtonBox", "");
         Emit_Line (Sp (Id + 10) & "<property name=""can-focus"">False"
                    & "</property>");
         Emit_Line (Sp (Id + 10) & "<property name=""layout-style"">end"
                    & "</property>");
         Emit_Line (Sp (Id + 10) & "<child>");
         Emit_Line (Sp (Id + 12) & "<placeholder/>");
         Emit_Line (Sp (Id + 10) & "</child>");
         Emit_Line (Sp (Id + 10) & "<child>");
         Emit_Line (Sp (Id + 12) & "<placeholder/>");
         Emit_Line (Sp (Id + 10) & "</child>");
         Emit_Line (Sp (Id + 8) & "</object>");
         Emit_Line (Sp (Id + 8) & "<packing>");
         Emit_Line (Sp (Id + 10) & "<property name=""expand"">False</property>");
         Emit_Line (Sp (Id + 10) & "<property name=""fill"">False</property>");
         Emit_Line (Sp (Id + 10) & "<property name=""position"">0</property>");
         Emit_Line (Sp (Id + 8) & "</packing>");
         Emit_Line (Sp (Id + 6) & "</child>");
      end Emit_Internal_Action_Area;

      procedure Emit_Internal_Vbox;
      procedure Emit_Internal_Vbox is
      begin
         Emit_Line (Sp (Id + 2) & "<child internal-child=""vbox"">");
         Emit_Object (null, Id + 4, "GtkBox", "");
         Emit_Line (Sp (Id + 6) & "<property name=""can-focus"">False"
                    & "</property>");
         Emit_Line (Sp (Id + 6) & "<property name=""orientation"">vertical"
                    & "</property>");
         Emit_Line (Sp (Id + 6) & "<property name=""spacing"">2</property>");

         Emit_Internal_Action_Area;

         Emit_Line (Sp (Id + 6) & "<child>");
         Emit_Line (Sp (Id + 8) & "<placeholder/>");
         Emit_Line (Sp (Id + 6) & "</child>");
         Emit_Line (Sp (Id + 4) & "</object>");
         Emit_Line (Sp (Id + 2) & "</child>");
      end Emit_Internal_Vbox;

   begin
      if TWin.Name /= null and then TWin.Name.all /= "" then
         Emit_Object (null, Id, "GtkFileChooserDialog", TWin.Name.all);
      else
         Emit_Object (null, Id, "GtkFileChooserDialog", "");
      end if;
      Emit_Line (Sp (Id + 2) & "<property name=""can-focus"">False"
                 & "</property>");
      Emit_Line (Sp (Id + 2) & "<property name=""title"" translatable=""yes"">"
                 & TWin.Name.all
                 & "_title</property>");
      Emit_Line (Sp (Id + 2) & "<property name=""type-hint"">dialog"
                 & "</property>");
      Emit_Line (Sp (Id + 2) & "<property name=""transient-for"">"
                 & TWin.Transient_For.Name.all & "</property>");
      Emit_Line (Sp (Id + 2) & "<property name=""attached-to"">"
                 & TWin.Attached_To.Name.all & "</property>");
      Emit_Line (Sp (Id + 2) & "<property name=""filter"">"
                 & TWin.FilterName.all & "</property>");

      Emit_Internal_Vbox;

      Emit_Line (Sp (Id) & "</object>");
   end Emit_GtkFileChooserDialog;

   -----------------------------------------------------------------------
   --  Emit_GtkFileFilter
   -----------------------------------------------------------------------

   procedure Emit_GtkFileFilter (TWin : Window_Pointer;
                                 Id   : Integer) is
      procedure Emit_Pattern (Pattern : String);
      procedure Emit_Pattern (Pattern : String) is
      begin
         Emit_Line (Sp (Id + 4) & "<pattern>"
                    & Ada.Strings.Fixed.Trim (Pattern, Ada.Strings.Both)
                    & "</pattern>");
      end Emit_Pattern;

   begin
      if TWin.Name /= null and then TWin.Name.all /= "" then
         Emit_Object (null, Id, "GtkFileFilter", TWin.Name.all);
      else
         Emit_Object (null, Id, "GtkFileFilter", "");
      end if;
      Emit_Line (Sp (Id + 2) & "<patterns>");
      if TWin.FilterString /= null and then TWin.FilterString.all /= "" then
         declare
            Filter : constant String := TWin.FilterString.all;
            Idx0 : Integer := Filter'First;
            Idx1 : Integer;
         begin
            Idx0 := Index (Filter, "|");
            if Idx0 not in Filter'Range then
               return;
            end if;
            Idx1 := Index (Filter (Idx0 + 1 .. Filter'Last), "|");
            while Idx1 in Idx0 + 1 .. Filter'Last loop
               Emit_Pattern (Filter (Idx0 .. Idx1 - 1));
               Idx0 := Idx1 + 1;
               Idx1 := Index (Filter (Idx0 .. Filter'Last), "|");
            end loop;
            if Idx0 < Filter'Last then
               Emit_Pattern (Filter (Idx0 + 1 .. Filter'Last));
            end if;
         end;
      end if;
      Emit_Line (Sp (Id + 2) & "</patterns>");
      Emit_Line (Sp (Id) & "</object>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkFileFilter: " & TWin.Name.all);
         raise;
   end Emit_GtkFileFilter;

   -----------------------------------------------------------------------
   -- Emit_GtkDialog --
   -----------------------------------------------------------------------

   procedure Emit_GtkDialog (TWin : Window_Pointer;
                             Id : Integer) is
      Num_Buttons : Integer := 0;
      Child : Widget_Pointer := TWin.Widget_List;
      --  assume TWin is a dialog
   begin
      if TWin.Name /= null and then TWin.Name.all /= "" then
         Emit_Object (null, Id, "GtkDialog", TWin.Name.all);
         Emit_Name (TWin, Id + 2);
      else
         Emit_Object (null, Id, "GtkDialog", "");
      end if;
      Emit_Property (Id + 2, "can-focus", False);
      if TWin.Title /= null and then TWin.Title.all /= "" then
         Emit_Line (Sp (Id + 2) & "<property name=""title"""
                    & " translatable=""yes"">"
                    & TWin.Title.all & "</property>");
      elsif TWin.Name /= null and then TWin.Name.all /= "" then
         Emit_Line (Sp (Id + 2) & "<property name=""title"""
                    & " translatable=""yes"">"
                    & TWin.Name.all & "</property>");
      end if;
      if not TWin.Resizable then
         Emit_Property (Id + 2, "resizable", TWin.Resizable);
      end if;
      if TWin.Modal then
         Emit_Property (Id + 2, "modal", TWin.Modal);
      end if;
      Emit_Property (Id + 2, "window-position", "center-on-parent");
      if TWin.Client_Size.Horiz /= -1 then
         Emit_Property (Id + 2, "default-width", TWin.Client_Size.Horiz);
      end if;
      if TWin.Client_Size.Vert /= -1 then
         Emit_Property (Id + 2, "default-height", TWin.Client_Size.Vert);
      end if;
      Emit_Property (Id + 2, "type-hint", "dialog");
      Emit_Property (Id + 2, "gravity", "center");
      Emit_GtkSignal (TWin, Id + 2);

      if TWin.Action_Buttons (OK_Response) /= null then
         Num_Buttons := Num_Buttons + 1;
      end if;
      if TWin.Action_Buttons (Cancel_Response) /= null then
         Num_Buttons := Num_Buttons + 1;
      end if;
      if TWin.Action_Buttons (Delete_Response) /= null then
         Num_Buttons := Num_Buttons + 1;
      end if;

      while Child /= null loop
         Emit_Widget_Child (Child, Id + 2);
         Child := Child.Next;
      end loop;

      Emit_Line (Sp (Id) & "</object>"); --  gtkdialog
   end Emit_GtkDialog;

   -----------------------------------------------------------------------
   -- Emit_Main_GtkWindow --
   -----------------------------------------------------------------------

   procedure Emit_Main_GtkWindow (TWin : Window_Pointer;
                                  Id   : Integer) is
      Child : Widget_Pointer := TWin.Widget_List;
   begin
      if TWin.Name /= null and then TWin.Name.all /= "" then
         Emit_Object (null, Id, "GtkWindow", TWin.Name.all);
      else
         Emit_Object (null, Id, "GtkWindow", "");
      end if;
      if TWin.Name /= null and then TWin.Name.all /= "" then
         Emit_Name (TWin, Id + 2);
      end if;
      if TWin.ToolTip /= null and then TWin.ToolTip.all /= "" then
         Emit_Line (Sp (Id + 2) & "<property name=""tooltip-text2"" "
                    & "translatable"
                    & "=""yes"">" & TWin.ToolTip.all & "</property>");
      end if;
      Emit_Property (Id + 2, "can-focus", False);
      if TWin.Title /= null and then TWin.Title.all /= "" then
         Emit_Line (Sp (Id + 2) & "<property name=""title"""
                    & " translatable=""yes"">"
                    & TWin.Title.all & "</property>");
      elsif TWin.Name /= null and then TWin.Name.all /= "" then
         Emit_Line (Sp (Id + 2) & "<property name=""title"""
                    & " translatable=""yes"">"
                    & TWin.Name.all & "</property>");
      end if;
      if not TWin.Resizable then
         Emit_Property (Id + 2, "resizable", TWin.Resizable);
      end if;
      if TWin.Modal then
         Emit_Property (Id + 2, "modal", TWin.Modal);
      end if;
      if TWin.Client_Size.Horiz /= -1 then
         Emit_Property (Id + 2, "default-width", TWin.Client_Size.Horiz);
      end if;
      if TWin.Client_Size.Vert /= -1 then
         Emit_Property (Id + 2, "default-height", TWin.Client_Size.Vert);
      end if;
      Emit_GtkSignal (TWin, Id + 2);

      while Child /= null loop
         Emit_Widget_Child (Child, Id + 2);
         Child := Child.Next;
      end loop;

      Emit_Line (Sp (Id) & "</object>");
   end Emit_Main_GtkWindow;

   -----------------------------------------------------------------------
   --  Emit_GtkWindow
   -----------------------------------------------------------------------

   procedure Emit_GtkWindow (TWin : Window_Pointer;
                             Id   : Integer) is
   begin
      if TWin.Is_Dialog then
         Emit_GtkDialog (TWin, Id);
      else
         Emit_Main_GtkWindow (TWin, Id);
      end if;
   end Emit_GtkWindow;

end W2gtk_Emit;
