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
with Emit_Tools;      use Emit_Tools;
with Emit_Display;    use Emit_Display;
with GNAT.Strings;    use GNAT.Strings;

package body Emit_Auxiliary is

   ---------------------------------------------------------------------------
   --  Emit_GtkNotebookTab
   ---------------------------------------------------------------------------
   procedure Emit_GtkNotebookTab (TWdg : Widget_Pointer;
                                  Id   : Integer) is
      Temp   : Widget_Pointer;
      Box    : Widget_Pointer;
      Label  : Widget_Pointer;
      Button : Widget_Pointer;
      TS     : Signal_Pointer;
   begin
      Temp := TWdg.Child_List;
      while Temp /= null loop
         if Temp.Widget_Type = GtkDataGridView then
            Emit_GtkDataGridView (Temp, Id, TWdg.Child_Number);
         elsif Temp.Widget_Type = GtkTreeGridView then
            Emit_GtkTreeView (Temp, Id, TWdg.Child_Number);
         end if;
         Temp := Temp.Next;
      end loop;

      --  tab header
      Emit_Line (Sp (Id) & "<child type=""tab"">"); --  no name

      --  gtkbox header
      Box := TWdg.Child_List.Next;
      Emit_Object (Box, Id + 2, "GtkBox", Box.Name.all);
      Emit_Visible_And_Can_Focus (Box, Id + 4, False);
      Emit_Property (Id + 4, "spacing", 2);

      --  gtklabel complete
      Label := Box.Child_List;
      Emit_Child (Label, Id + 4, False);
      Emit_Object (Label, Id + 6, "GtkLabel", Label.Name.all);
      Emit_Visible_And_Can_Focus (Label, Id + 8, False);
      Emit_Label (Label, Id + 8, UnderLine => False, Selectable => False);
      Emit_Line (Sp (Id + 6) & "</object>");
      Emit_Line (Sp (Id + 6) & "<packing>");
      Emit_Property (Id + 8, "expand", False);
      Emit_Property (Id + 8, "fill", True);
      Emit_Property (Id + 8, "position", 0);
      Emit_Line (Sp (Id + 6) & "</packing>");
      Emit_Line (Sp (Id + 4) & "</child>");

      --  gtkbutton header
      Button := Label.Next;
      Emit_Child (Button, Id + 4, False);
      Emit_Object (Button, Id + 6, "GtkButton", Button.Name.all);
      Emit_Visible_And_Can_Focus (Button, Id + 8, True);
      Emit_Property (Id + 8, "focus-on-click", False);
      Emit_Property (Id + 8, "receives-default", True);
      Emit_Property (Id + 8, "relief", "none");
      if not TWdg.GParent.CloseButtonOnTabsInactiveVisible then
         TS := TWdg.GParent.Signal_List;
         while TS /= null loop
            if TS.Name.all = "CloseButtonClick" then
               Emit_Line (Sp (Id + 8) & "<signal name="""
                          & TS.GtkName.all
                          & """ handler=""" & TS.Handler.all & """"
                          & " object=""" & Button.Name.all & """"
                          & " swapped=""no""/>");
               exit;
            end if;
            TS := TS.Next;
         end loop;
      end if;

      --  gtkimage complete
      Emit_Child (TWdg, Id + 8, False);
      Emit_Object (TWdg, Id + 10, "GtkImage", "tab_img_" & TWdg.Name.all);
      Emit_Visible_And_Can_Focus (TWdg, Id + 12, False);
      Emit_Property (Id + 12, "stock", "gtk-close");
      Emit_Line (Sp (Id + 10) & "</object>");
      Emit_Line (Sp (Id + 8) & "</child>");

      --  gtkbutton tail
      Emit_Line (Sp (Id + 6) & "</object>");
      Emit_Line (Sp (Id + 6) & "<packing>");
      Emit_Property (Id + 8, "expand", False);
      Emit_Property (Id + 8, "fill", True);
      Emit_Property (Id + 8, "position", 1);
      Emit_Line (Sp (Id + 6) & "</packing>");
      Emit_Line (Sp (Id + 4) & "</child>");

      --  gtkbox tail
      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Line (Sp (Id + 2) & "<packing>");
      Emit_Property (Id + 4, "position", 2);
      Emit_Property (Id + 4, "tab-fill", False);
      Emit_Line (Sp (Id + 2) & "</packing>");

      --  tab tail
      Emit_Line (Sp (Id) & "</child>");

   end Emit_GtkNotebookTab;

   ---------------------------------------------------------------------------
   --  Emit_GtkSubmenu
   ---------------------------------------------------------------------------
   procedure Emit_GtkSubmenu (TWdg : Widget_Pointer; Id : Integer) is
      Child : Widget_Pointer;
      Sensitive : constant Boolean := TWdg.Enabled;
   begin
      Emit_Line (Sp (Id + 4) & "<child type=""submenu"">");
      Emit_Object (TWdg, Id + 6, "GtkMenu", TWdg.Name.all & "_submenu");
      TWdg.Enabled := True;
      Emit_Visible_And_Can_Focus (TWdg, Id + 8, False);
      TWdg.Enabled := Sensitive;

      Child := TWdg.Child_List;
      while Child /= null loop
         case Child.Widget_Type is
            when GtkSeparatorMenuItem => Emit_GtkSepMenuItem (Child, Id + 8);
            when GtkMenuItem       => Emit_GtkMenuItem (Child, Id + 8);
            when GtkMenuImageItem  => Emit_GtkMenuImageItem (Child, Id + 8);
            when GtkMenuNormalItem => Emit_GtkMenuNormalItem (Child, Id + 8);
            when GtkMenuCheckItem  => null;
            when GtkMenuRadioItem  => null;
            when others => null;
         end case;
         Child := Child.Next;
      end loop;

      Emit_Line (Sp (Id + 6) & "</object>");
      Emit_Line (Sp (Id + 4) & "</child>");
   end Emit_GtkSubmenu;

   ---------------------------------------------------------------------------
   --  Emit_GtkMenuNormalItem
   ---------------------------------------------------------------------------
   procedure Emit_GtkMenuNormalItem (TWdg : Widget_Pointer; Id : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkMenuItem", TWdg.Name.all);
      Emit_Label (TWdg, Id + 4, UnderLine => False, Selectable => False);
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
      Emit_GtkSignal (TWdg, Id + 4);

      if TWdg.Child_List /= null then
         Emit_GtkSubmenu (TWdg, Id);
      end if;

      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Line (Sp (Id) & "</child>");
   end Emit_GtkMenuNormalItem;

   ---------------------------------------------------------------------------
   --  Emit_GtkMenuImageItem
   ---------------------------------------------------------------------------
   procedure Emit_GtkMenuImageItem (TWdg : Widget_Pointer; Id : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkImageMenuItem", TWdg.Name.all);
      Emit_Label (TWdg, Id + 4, UnderLine => False, Selectable => False);
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
      if TWdg.ImageMenuWin /= null
        and then TWdg.ImageMenuWin.Name /= null
      then
         Emit_Property (Id + 4, "image", TWdg.ImageMenuWin.Name.all);
      end if;
      Emit_Property (Id + 4, "use-stock", False);
      Emit_GtkSignal (TWdg, Id + 4);

      if TWdg.Child_List /= null then
         Emit_GtkSubmenu (TWdg, Id);
      end if;

      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Line (Sp (Id) & "</child>");
   end Emit_GtkMenuImageItem;

   ---------------------------------------------------------------------------
   --  Emit_GtkSepMenuItem
   ---------------------------------------------------------------------------
   procedure Emit_GtkSepMenuItem (TWdg : Widget_Pointer;
                                  Id : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkSeparatorMenuItem", "");
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
      Emit_GtkSignal (TWdg, Id + 4);
      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Line (Sp (Id) & "</child>");
   end Emit_GtkSepMenuItem;

   ---------------------------------------------------------------------------
   --  Emit_GtkSubmenu
   ---------------------------------------------------------------------------
   procedure Emit_GtkMenuItem (TWdg : Widget_Pointer; Id : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkMenuItem", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
      Emit_Label (TWdg, Id + 4, UnderLine => True, Selectable => False);
      Emit_GtkSignal (TWdg, Id + 4);

      if TWdg.Child_List /= null then
         Emit_GtkSubmenu (TWdg, Id);
      end if;

      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Line (Sp (Id) & "</child>");
   end Emit_GtkMenuItem;

   ---------------------------------------------------------------------------
   --  Emit_GtkGridViewCheckBoxColumn
   ---------------------------------------------------------------------------
   procedure Emit_GtkGridViewCheckBoxColumn (TWdg : Widget_Pointer;
                                             Id   : Integer;
                                             Num  : Integer);
   procedure Emit_GtkGridViewCheckBoxColumn (TWdg : Widget_Pointer;
                                             Id   : Integer;
                                             Num  : Integer) is
      NCol : Integer;
      Found_Signal_Toggled : Boolean;
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkTreeViewColumn", TWdg.Name.all);
      if TWdg.Resizable then
         Emit_Property (Id + 4, "resizable", TWdg.Resizable);
      end if;
      Emit_Property (Id + 4, "sizing", "fixed");
      if TWdg.Min_Width > 0 then
         Emit_Property (Id + 4, "min-width", TWdg.Min_Width);
      end if;
      if TWdg.Max_Width > 0 then
         Emit_Property (Id + 4, "max-width", TWdg.Max_Width);
      end if;
      Emit_Property (Id + 4, "title", TWdg.Text.all, True);
      if TWdg.SortMode /= NotSortable then
         Emit_Property (Id + 4, "sort-indicator", True);
         Emit_Property (Id + 4, "sort-column-id", Num);
      end if;
      Emit_GtkSignal (TWdg, Id + 4, Except => "toggled");

      Emit_Child (TWdg, Id + 4, False);
      Found_Signal_Toggled := Signal_Exists (TWdg, "toggled");
      Emit_Object (TWdg, Id + 6,
                   Wdg    => "GtkCellRendererToggle",
                   WId    => "CRTG_" & TWdg.Name.all,
                   Finish => not Found_Signal_Toggled);
      if Found_Signal_Toggled then
         Emit_GtkSignal (TWdg, Id + 8, Only_For => "toggled");
      end if;
      if Found_Signal_Toggled then
         Emit_Line (Sp (Id + 6) & "</object>");
      end if;
      Emit_Line (Sp (Id + 6) & "<attributes>");
      if TWdg.GParent.AlternatingRowsDefaultCellStyle in DGVS'Range then
         NCol := TWdg.GParent.Model.Num_Elements - 1;
         if NCol /= -1 then
            Emit_Line (Sp (Id + 8) & "<attribute name=""cell-background"">"
                       & Img (NCol) & "</attribute>");
         end if;
      end if;
      if TWdg.CheckBox_Col_Properties.Activatable_Column >= 0 then
         Emit_Line (Sp (Id + 8) & "<attribute name=""activatable"">"
                    & Img (TWdg.CheckBox_Col_Properties.Activatable_Column)
                    & "</attribute>");
      end if;
      if TWdg.CheckBox_Col_Properties.Active_Column >= 0 then
         Emit_Line (Sp (Id + 8) & "<attribute name=""active"">"
                    & Img (TWdg.CheckBox_Col_Properties.Active_Column)
                    & "</attribute>");
      end if;
      Emit_Line (Sp (Id + 6) & "</attributes>");
      Emit_Line (Sp (Id + 4) & "</child>");

      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Line (Sp (Id) & "</child>");
   end Emit_GtkGridViewCheckBoxColumn;

   ---------------------------------------------------------------------------
   --  Emit_GtkGridViewTextBoxColumn
   ---------------------------------------------------------------------------
   procedure Emit_GtkGridViewTextBoxColumn (TWdg : Widget_Pointer;
                                            Id   : Integer;
                                            Num  : Integer);
   procedure Emit_GtkGridViewTextBoxColumn (TWdg : Widget_Pointer;
                                            Id   : Integer;
                                            Num  : Integer) is
      NCol : Integer;
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkTreeViewColumn", TWdg.Name.all);
      if TWdg.Resizable then
         Emit_Property (Id + 4, "resizable", TWdg.Resizable);
      end if;
      Emit_Property (Id + 4, "sizing", "fixed");
      if TWdg.Min_Width > 0 then
         Emit_Property (Id + 4, "min-width", TWdg.Min_Width);
      end if;
      if TWdg.Max_Width > 0 then
         Emit_Property (Id + 4, "max-width", TWdg.Max_Width);
      end if;
      if TWdg.AutoSizeColumnMode = Fill then
         Emit_Property (Id + 4, "expand", True);
      end if;
      Emit_Property (Id + 4, "title", TWdg.Text.all, True);
      if TWdg.DefaultCellStyle in DGVS'Range
        and then DGVS (TWdg.DefaultCellStyle).Alignment /= null
      then
         case To_TextAlign (DGVS (TWdg.DefaultCellStyle).Alignment.all) is
            when None | Top | Bottom => null;
            when TopLeft   | MiddleLeft   | BottomLeft   | Left   =>
               --  default Emit_Property (Id + 4, "alignment", 0.0);
               null;
            when TopCenter | MiddleCenter | BottomCenter | Center =>
               Emit_Property (Id + 4, "alignment", 0.5);
            when TopRight  | MiddleRight  | BottomRight  | Right  =>
               Emit_Property (Id + 4, "alignment", 1.0);
         end case;
      end if;
      if TWdg.SortMode /= NotSortable then
         Emit_Property (Id + 4, "sort-indicator", True);
         Emit_Property (Id + 4, "sort-column-id", Num);
      end if;
      Emit_GtkSignal (TWdg, Id + 4);

      Emit_Child (TWdg, Id + 4, False);
      Emit_Object (TWdg, Id + 6,
                   Wdg    => "GtkCellRendererText",
                   WId    => "CRT_" & TWdg.Name.all,
                   Finish => True);
      Emit_Line (Sp (Id + 6) & "<attributes>");

      if TWdg.GParent.AlternatingRowsDefaultCellStyle in DGVS'Range then
         NCol := TWdg.GParent.Model.Num_Elements - 1;
         if NCol /= -1 then
            Emit_Line (Sp (Id + 8) & "<attribute name=""background"">"
                       & Img (NCol) & "</attribute>");
         end if;
      end if;
      if TWdg.Text_Col_Properties.Fg_Color_Name_Column /= -1 then
         NCol := TWdg.Text_Col_Properties.Fg_Color_Name_Column;
         Emit_Line (Sp (Id + 8) & "<attribute name=""foreground"">"
                    & Img (NCol) & "</attribute>");
      end if;

      Emit_Line (Sp (Id + 8) & "<attribute name=""text"">"
                 & Img (Num) & "</attribute>");
      Emit_Line (Sp (Id + 6) & "</attributes>");
      Emit_Line (Sp (Id + 4) & "</child>");

      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Line (Sp (Id) & "</child>");
   end Emit_GtkGridViewTextBoxColumn;

   ---------------------------------------------------------------------------
   --  Emit_GtkGridView
   ---------------------------------------------------------------------------
   procedure Emit_GtkGridView (TWdg : Widget_Pointer; Id : Integer) is
      Child : Widget_Pointer;
      Num   : Integer;
      TS    : Signal_Pointer;
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkTreeView", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, True);
      Emit_Property (Id + 4, "model", TWdg.Model.Name.all);
      if not TWdg.ColumnHeadersVisible then
         Emit_Property (Id + 4, "headers-visible", TWdg.ColumnHeadersVisible);
         Emit_Property (Id + 4, "headers-clickable", True);
      end if;
      Child := TWdg.Child_List;
      while Child /= null loop
         if Child.Widget_Type = ExpandableColumn then
            Emit_Property (Id + 4, "expander-column", Child.Name.all);
            exit;
         end if;
         Child := Child.Next;
      end loop;
      Emit_Property (Id + 4, "rules-hint", True);
      if TWdg.AllowUserToOrderColumns then
         Emit_Property (Id + 4, "reorderable", TWdg.AllowUserToOrderColumns);
      end if;
      Emit_Property (Id + 4, "fixed-height-mode", True);
      Emit_Property (Id + 4, "ubuntu-almost-fixed-height-mode", True);
      Emit_Property (Id + 4, "level-indentation", 3);
      Emit_Property (Id + 4, "rubber-banding", True);
      Emit_Property (Id + 4, "enable-grid-lines", "both");
      Emit_Property (Id + 4, "activate-on-single-click", True);
      Emit_GtkSignal (TWdg, Id + 4, Except => "SelectedIndexChanged");

      Emit_Line (Sp (Id + 4) & "<child internal-child=""selection"">");
      Emit_Object (TWdg, Id + 6, "GtkTreeSelection",
                   TWdg.Name.all & "_Selection");
      if TWdg.RowMultiSelect then
         Emit_Property (Id + 8, "mode", "multiple");
      else
         Emit_Property (Id + 8, "mode", "browse");
      end if;
      TS := TWdg.Signal_List;
      while TS /= null loop
         if TS.GtkName.all = "changed" then
            Emit_One_GtkSignal (TS, Id + 8, TWdg.Name.all);
            exit;
         end if;
         TS := TS.Next;
      end loop;
      Emit_Line (Sp (Id + 6) & "</object>");
      Emit_Line (Sp (Id + 4) & "</child>");

      Child := TWdg.Child_List;
      Num := 0;
      while Child /= null loop
         case Child.Widget_Type is
            when ExpandableColumn | DataGridViewTextBoxColumn =>
               Emit_GtkGridViewTextBoxColumn (Child, Id + 4, Num);
            when DataGridViewCheckBoxColumn =>
               Emit_GtkGridViewCheckBoxColumn (Child, Id + 4, Num);
            when others => null;
         end case;
         Num := Num + 1;
         Child := Child.Next;
      end loop;

      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Line (Sp (Id) & "</child>");
   end Emit_GtkGridView;
end Emit_Auxiliary;
