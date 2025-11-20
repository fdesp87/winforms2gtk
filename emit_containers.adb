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
with Emit_Tools;                use Emit_Tools;
with GNAT.Strings;              use GNAT.Strings;

with Emit_Display;              use Emit_Display;
with Emit_Auxiliary;            use Emit_Auxiliary;
with Emit_Controls;             use Emit_Controls;
with Emit_Internals;            use Emit_Internals;

package body Emit_Containers is

   ----------------------------------------------------------------------
--  Emit Widget Child
   ----------------------------------------------------------------------
   procedure Emit_Widget_Child (Child : Widget_Pointer;
                                Id    : Integer;
                                From_ButtonBox : Boolean := False;
                                Omit_Child     : Boolean := False) is
   begin
      case Child.Widget_Type is
         when No_Widget =>
            null;

         when Action_Widgets =>
            Emit_Action_Widgets (Child.WParent, Id);

         when BackgroundWorker =>
            null;

         when BindingNavigator =>
            Emit_GtkToolBar (Child, Id);

         when GtkAspectFrame =>
            Emit_GtkAspectFrame (Child, Id);

         when GtkButton =>
            if From_ButtonBox then
               Emit_GtkButton (Child, Id, "GtkButton",
                               Position    => Child.Child_Number,
                               Has_Default => Child.Dialog_Result = OK_Response,
                               XY          => False,
                               Homog       => False);
            else
               Emit_GtkButton (Child, Id, "GtkButton",
                               Position    => -1,
                               Has_Default => Child.Dialog_Result = OK_Response,
                               XY          => True,
                               Homog       => False);
            end if;

         when GtkBox =>
            Emit_GtkBox (Child, Id, Omit_Child);

         when GtkButtonBox =>
            Emit_GtkButtonBox (Child, Id);

         when GtkCalendar =>
            if Child.Is_DatePicker then
               Emit_DatePicker (Child, Id);
            else
               Emit_TimePicker (Child, Id);
            end if;

         when GtkCheckButton =>
            Emit_GtkCheckButton (Child, Id);

         when Chart =>
            null;

         when GtkComboTextBox =>
            Emit_GtkComboTextBox (Child, Id,
                                  Packing => True);
         when GtkColorButton =>
            Emit_GtkColorButton (Child, Id);

         when GtkDataGridView =>
            Emit_GtkDataGridView (Child, Id, 0);

         when GtkEntry =>
            Emit_GtkEntry (Child, Id);

         when GtkFixed =>
            Emit_GtkFixed (Child, Id);

         when GtkFrame =>
            Emit_GtkFrame (Child, Id);

         when GtkFileChooserButton =>
            Emit_GtkFileChooserButton (Child, Id);

         when GtkImage =>
            Emit_GtkImage (Child, Id);

         when GtkLabel =>
            Emit_GtkLabel (Child, Id, Packing => True);

         when GtkListBox =>
            Emit_GtkListBox (Child, Id);

         when GtkMenuBar =>
            Emit_GtkMenuBar (Child, Id);
         when GtkSeparatorMenuItem =>
            null; --  processed in Emit_GtkMenuBar
         when GtkMenuItem =>
            null; --  processed in Emit_GtkMenuBar
         when GtkSubMenu =>
            null; --  processed in Emit_GtkMenuBar
         when GtkMenuNormalItem =>
            null; --  processed in Emit_GtkMenuBar
         when GtkMenuImageItem =>
            null; --  processed in Emit_GtkMenuBar
         when GtkMenuRadioItem =>
            null; --  processed in Emit_GtkMenuBar
         when GtkMenuCheckItem =>
            null; --  processed in Emit_GtkMenuBar

         when GtkNoteBook =>
            Emit_GtkNoteBook (Child, Id);

         when GtkRadioButton =>
            Emit_GtkRadioButton (Child, Id, "GtkRadioButton",
                                 Underline => Child.Underline,
                                 XY => True, Homog => False);

         when GtkSeparatorToolItem =>
            null;

         when GtkStatusBar =>
            Emit_GtkStatusBar (Child, Id, 0);

         when GtkSpinButton =>
            Emit_GtkSpinButton (Child, Id);

         when GtkTabChild =>
            null;

         when GtkTabPage =>
            null;  --  processed in GtkNotebook

         when GtkToggleButton =>
            Emit_GtkToggleButton (Child, Id, "GtkToggleButton",
                                  Underline => Child.Underline,
                                  XY => True, Homog => False);

         when GtkToolBar =>
            Emit_GtkToolBar (Child, Id);

         when GtkToolTip =>
            null;

         when GtkTreeGridView =>
            Emit_GtkTreeView (Child, Id, 0);
         when ExpandableColumn =>
            null; --  processed in Emit_GtkTreeView
         when DataGridViewTextBoxColumn =>
            null; --  processed in Emit_GtkTreeView
         when DataGridViewCheckBoxColumn =>
            null; --  processed in Emit_GtkTreeView

         when FolderBrowserDialog =>
            null;

         when Internal_Child_VBox =>
            Emit_Internal_Child_VBox (Child, Id);

         when Internal_Child_Action_Area =>
            Emit_Internal_Child_Action_Area (Child, Id);

         when PageSetupDialog =>
            null;

         when PrintDocument =>
            null;

         when PrintDialog =>
            null;

         when ToolStripStatusLabel =>
            Emit_GtkLabel (Child, Id, Packing => True);

      end case;
   end Emit_Widget_Child;

   ---------------------------------------------------------------------------
   --  Emit_GtkBox
   ---------------------------------------------------------------------------
   procedure Emit_GtkBox (TWdg : Widget_Pointer;
                          Id   : Integer;
                          Omit_Child : Boolean := False) is
      Child : Widget_Pointer;
      Pack_Start : Boolean;
   begin
      if not Omit_Child then
         Emit_Child (TWdg, Id, False);
      end if;
      if TWdg.Name /= null and then TWdg.Name.all /= "" then
         Emit_Object (TWdg, Id + 2, "GtkBox", TWdg.Name.all);
      else
         Emit_Object (TWdg, Id + 2, "GtkBox", "");
      end if;
      Emit_Name (TWdg, Id + 4);
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
      if TWdg.Orientation = Vertical then
         Emit_Property (Id + 4, "orientation", "vertical");
      end if;
      if TWdg.Spacing > 0 then
         Emit_Property (Id + 4, "spacing", TWdg.Spacing);
      end if;

      Child := TWdg.Child_List;
      while Child /= null loop
         Emit_Widget_Child (Child, Id + 4);
         Child := Child.Next;
      end loop;

      Emit_Line (Sp (Id + 2) & "</object>");
      if TWdg.GParent /= null and then --  box attached to other widget
        TWdg.GParent.Widget_Type /= Internal_Child_VBox
      then
         case TWdg.FlowDirection is
            when LeftToRight => Pack_Start := True;
            when RightToLeft => Pack_Start := False;
            when TopDown     => Pack_Start := True;
            when BottomUp    => Pack_Start := False;
         end case;
         Emit_Packing (Id + 2,
                       Position   => TWdg.Child_Number,
                       Expand     => False,
                       Fill       => True,
                       Padding    => TWdg.Padding,
                       Pack_Start => Pack_Start);
      end if;
      if not Omit_Child then
         Emit_Line (Sp (Id) & "</child>");
      end if;
   end Emit_GtkBox;

   ---------------------------------------------------------------------------
   -- Emit_GtkNoteBook --
   ---------------------------------------------------------------------------
   procedure Emit_GtkNoteBook (TWdg : Widget_Pointer;
                               Id   : Integer) is
      Child : Widget_Pointer;
      Position : Integer := 0;
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkNotebook", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, True);
      Emit_Align (TWdg, Id + 4, Numeric => True);
      Emit_Margin (TWdg, Id + 4);
      Emit_ToolTip (TWdg, Id + 4);
      if not TWdg.Show_Tabs then
         Emit_Property (Id + 4, "show-tabs", TWdg.Show_Tabs);
      end if;
      if not TWdg.Show_Border then
         Emit_Property (Id + 4, "show-border", TWdg.Show_Border);
      end if;
      if TWdg.Scrollable then
         Emit_Property (Id + 4, "scrollable", TWdg.Scrollable);
      end if;
      if TWdg.Enable_Popups then
         Emit_Property (Id + 4, "enable-popup", TWdg.Enable_Popups);
      end if;
      if TWdg.CloseButtonOnTabsInactiveVisible then
         Emit_GtkSignal (TWdg, Id + 4);
      else
         Emit_GtkSignal (TWdg, Id + 4, Except => "CloseButtonClick");
      end if;

      Child := TWdg.Child_List;
      while Child /= null loop
         if Child.Widget_Type /= GtkTabPage then
            raise Program_Error;
         end if;
         Emit_GtkNotebookTab (Child, Id + 4);
         Position := Position + 1;
         Child := Child.Next;
      end loop;

      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing (Id + 2, TWdg.Child_Number,
                    True, True, TWdg.Padding, True);
      Emit_Line (Sp (Id) & "</child>");
   end Emit_GtkNoteBook;

   ---------------------------------------------------------------------------
   --  Emit_GtkFrame
   ---------------------------------------------------------------------------
   procedure Emit_GtkFrame (TWdg : Widget_Pointer;
                            Id   : Integer) is
      procedure Emit_Frame_Label (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_Frame_Label (TWdg : Widget_Pointer; Id : Integer) is
      begin
         Emit_Child (TWdg, Id, True);
         Emit_Object (TWdg, Id + 2, "GtkLabel", "label_" & TWdg.Name.all);
         Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
         Emit_Label (TWdg, Id + 4, UnderLine => False, Selectable => True);
         Emit_Attributes (TWdg, Id + 4);
         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Packing_Child (TWdg, Id,
                             Packing => False,
                             XY => True,
                             Homog => False);
         Emit_Line (Sp (Id) & "</child>");
      end Emit_Frame_Label;

      Child : Widget_Pointer;
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkFrame", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
      Emit_Line (Sp (Id + 4) &
                      "<property name=""border-width"">0</property>");
      Emit_Line (Sp (Id + 4) &
                      "<property name=""label-xalign"">0</property>");
      Emit_Line (Sp (Id + 4) &
                      "<property name=""label-yalign"">0</property>");
      Emit_Line (Sp (Id + 4) &
                      "<property name=""shadow-type"">in</property>");
      Emit_ToolTip (TWdg, Id + 4);
      Emit_Margin (TWdg, Id + 4);
      Emit_GtkSignal (TWdg, Id + 4);

      Emit_Alignment (TWdg, Id + 4);

      Emit_GtkFixed (TWdg, Id + 8);
      Child := TWdg.Child_List;
      while Child /= null loop
         Emit_Widget_Child (Child, Id + 8);
         Child := Child.Next;
      end loop;

      Emit_Line (Sp (Id + 6) & "</object>");  --  gtkalignment

      Emit_Packing_Child (TWdg, Id + 4,
                          Packing => False,
                          XY => True,
                          Homog => False);
      Emit_Line (Sp (Id + 4) & "</child>");

      Emit_Frame_Label (TWdg, Id + 4);

      Emit_Line (Sp (Id + 2) & "</object>");  --  gtkframe
      Emit_Packing_Child (TWdg, Id,
                          Packing => True,
                          XY => True,
                          Homog => False);
      Emit_Line (Sp (Id) & "</child>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkFrame: " & TWdg.Name.all);
         raise;
   end Emit_GtkFrame;

   ---------------------------------------------------------------------------
   --  Emit_AspectFrame
   ---------------------------------------------------------------------------
   procedure Emit_GtkAspectFrame (TWdg : Widget_Pointer;
                                  Id   : Integer) is
      Child : Widget_Pointer;
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkAspectFrame",
                   TWdg.Name.all & "_AspectFrame");
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
      Emit_Line (Sp (Id + 4) & "<property name=""label-xalign"">0"
                 & "</property>");
      Emit_Line (Sp (Id + 4) & "<property name=""shadow-type"">in"
                 & "</property>");

      Child := TWdg.Child_List;
      while Child /= null loop
         Emit_Widget_Child (Child, Id);
         Child := Child.Next;
      end loop;

      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing_Child (TWdg, Id,
                          Packing => True,
                          XY      => True,
                          Homog   => False);
      Emit_Line (Sp (Id) & "</child>");
   end Emit_GtkAspectFrame;

   ---------------------------------------------------------------------------
   --  Emit_GtkListBox
   ---------------------------------------------------------------------------
   procedure Emit_GtkListBox (TWdg : Widget_Pointer;
                              Id   : Integer) is

      procedure Emit_Internal_GtkSelection (Id : Integer);
      procedure Emit_Internal_GtkSelection (Id : Integer) is
         TS : Signal_Pointer;
      begin
         Emit_Line (Sp (Id) & "<child internal-child=""selection"">");
         Emit_Object (TWdg, Id + 2, "GtkTreeSelection",
                      "Selection_" & TWdg.Name.all);
         if TWdg.MultiSelect then
            Emit_Line (Sp (Id + 4) & "<property name=""mode"">multiple"
                       & "</property>");
         else
            Emit_Line (Sp (Id + 4) & "<property name=""mode"">browse"
                       & "</property>");
         end if;
         TS := TWdg.Signal_List;
         while TS /= null loop
            if TS.GtkName.all = "changed" then
               Emit_One_GtkSignal (TS, Id + 4, TWdg.Name.all);
               exit;
            end if;
            TS := TS.Next;
         end loop;

         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Packing_Child (TWdg, Id,
                                        Packing => False,
                                        XY => True,
                                        Homog => False);
         Emit_Line (Sp (Id) & "</child>");
      end Emit_Internal_GtkSelection;

      Child : Widget_Pointer;
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkTreeView", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
      Emit_ToolTip (TWdg, Id + 4);
      Emit_Margin (TWdg, Id + 4);
      Emit_GtkSignal (TWdg, Id + 4, Except => "SelectedIndexChanged");

      Emit_Internal_GtkSelection (Id + 4);

      Child := TWdg.Child_List;
      while Child /= null loop
         Emit_Widget_Child (Child, Id + 4);
         Child := Child.Next;
      end loop;


      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing_Child (TWdg, Id,
                                     Packing => True,
                                     XY => True,
                                     Homog => False);
      Emit_Line (Sp (Id) & "</child>");
   end Emit_GtkListBox;

   ---------------------------------------------------------------------------
   procedure Emit_GtkFlowBox is
   begin
      null;
   end Emit_GtkFlowBox;

   ---------------------------------------------------------------------------
   procedure Emit_GtkOverlay is
   begin
      null;
   end Emit_GtkOverlay;

   ---------------------------------------------------------------------------
   --  Emit_GtkMenuBar
   ---------------------------------------------------------------------------
   procedure Emit_GtkMenuBar (TWdg : Widget_Pointer;
                              Id   : Integer) is
      Child : Widget_Pointer;
   begin
      Emit_Child (TWdg, Id, False);
      if TWdg.Name /= null and then TWdg.Name.all /= "" then
         Emit_Object (TWdg, Id + 2, "GtkMenuBar", TWdg.Name.all);
      else
         Emit_Object (TWdg, Id + 2, "GtkMenuBar", "");
      end if;
      Emit_Name (TWdg, Id + 4);
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
      Emit_GtkSignal (TWdg, Id + 4);

      Child := TWdg.Child_List;
      while Child /= null loop
         case Child.Widget_Type is
            when GtkMenuImageItem  => Emit_GtkMenuImageItem (Child, Id + 4);
            when GtkMenuNormalItem => Emit_GtkMenuNormalItem (Child, Id + 4);
            when GtkMenuCheckItem  => null; --  pending
            when GtkMenuRadioItem  => null;
            when GtkSeparatorMenuItem => Emit_GtkSepMenuItem (Child, Id + 4);
            when GtkMenuItem       => Emit_GtkMenuItem (Child, Id + 4);
            when GtkSubMenu        => Emit_GtkMenuItem (Child, Id + 4);
            when others => null;
         end case;
         Child := Child.Next;
      end loop;

      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing (Id + 2, 0, False, True, TWdg.Padding, True);
      Emit_Line (Sp (Id) & "</child>");
   end Emit_GtkMenuBar;

   ---------------------------------------------------------------------------
   -- Emit_GtkToolBar --
   ---------------------------------------------------------------------------
   procedure Emit_GtkToolBar (TWdg : Widget_Pointer;
                              Id   : Integer) is
      procedure Emit_SeparatorToolItem (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_SeparatorToolItem (TWdg : Widget_Pointer; Id : Integer) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkSeparatorToolItem", TWdg.Name.all);
         Emit_Name (TWdg, Id + 4);
         Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
         Emit_ToolTip (TWdg, Id + 4);
         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Packing_Child (TWdg, Id,
                             Packing => True,
                             XY      => False,
                             Homog   => False);
         Emit_Line (Sp (Id) & "</child>");
      end Emit_SeparatorToolItem;

      procedure Emit_MenuToolButton (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_MenuToolButton (TWdg : Widget_Pointer; Id : Integer) is
      begin
         null;
      end Emit_MenuToolButton;

      procedure Emit_LabelItem (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_LabelItem (TWdg : Widget_Pointer; Id : Integer) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkLabel", TWdg.Name.all);
         Emit_Name (TWdg, Id + 4);
         Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
         Emit_Margin (TWdg, Id + 4);
         Emit_ToolTip (TWdg, Id + 4);
         Emit_Label (TWdg, Id + 4, TWdg.Underline, Selectable => True);
         if TWdg.MaxLength > 0 then
            Emit_Property (Id + 4, "width-chars", TWdg.MaxLength);
            Emit_Property (Id + 4, "max-width-chars", TWdg.MaxLength);
         end if;
         if TWdg.GParent.Widget_Type = BindingNavigator then
            Emit_Property (Id + 4, "xalign", 0.5);
         else
            Emit_Align (TWdg, Id + 4, Numeric => True);
         end if;
         Emit_Attributes (TWdg, Id + 4);
         Emit_GtkSignal (TWdg, Id + 4);
         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Packing_Child (TWdg, Id,
                             Packing => False,
                             XY      => False,
                             Homog   => False);
         Emit_Line (Sp (Id) & "</child>");
      end Emit_LabelItem;

      procedure Emit_ComboboxTextItem (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_ComboboxTextItem (TWdg : Widget_Pointer; Id : Integer) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkComboBoxText", TWdg.Name.all);
         Emit_Name (TWdg, Id + 4);
         Emit_Visible_And_Can_Focus (TWdg, Id + 4, True);
         Emit_Property (Id + 4, "has-focus", TWdg.Has_Focus);
         Emit_Margin (TWdg, Id + 4);
         Emit_ToolTip (TWdg, Id + 4);
         Emit_GtkSignal (TWdg, Id + 4);
         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Packing_Child (TWdg, Id,
                                        Packing => False,
                                        XY      => False,
                                        Homog   => False);
         Emit_Line (Sp (Id) & "</child>");
      end Emit_ComboboxTextItem;

      procedure Emit_ButtonItem (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_ButtonItem (TWdg : Widget_Pointer; Id : Integer) is
         Sensitive : constant Boolean := TWdg.Enabled;
      begin
         if TWdg.Associated_ColorButton /= null then
            return;
         end if;
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkButton", TWdg.Name.all);
         Emit_Name (TWdg, Id + 4);
         Emit_Visible_And_Can_Focus (TWdg, Id + 4, True);
         Emit_Property (Id + 4, "has-focus", TWdg.Has_Focus);
         Emit_Property (Id + 4, "receives-default", True);
         Emit_ToolTip (TWdg, Id + 4);
         Emit_Align (TWdg, Id + 4, Numeric => False);
         Emit_Margin (TWdg, Id + 4);
         Emit_GtkSignal (TWdg, Id + 4);

         Emit_Child (TWdg, Id + 4, False);
         Emit_Object (TWdg, Id + 6, "GtkImage", "");
         TWdg.Enabled := True;
         Emit_Visible_And_Can_Focus (TWdg, Id + 8, False);
         TWdg.Enabled := Sensitive;
         if TWdg.ImagePath /= null then
            Emit_Property (Id + 8, "pixbuf", TWdg.ImagePath.all);
         end if;
         Emit_Line (Sp (Id + 6) & "</object>");
         Emit_Packing_Child (TWdg, Id + 4,
                                        Packing => False,
                                        XY      => False,
                                        Homog   => False);
         Emit_Line (Sp (Id + 4) & "</child>");
         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Packing_Child (TWdg, Id,
                                        Packing => False,
                                        XY      => False,
                                        Homog   => False);
         Emit_Line (Sp (Id) & "</child>");
      end Emit_ButtonItem;

      procedure Emit_EntryItem (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_EntryItem (TWdg : Widget_Pointer; Id : Integer) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkEntry", TWdg.Name.all);
         Emit_Name (TWdg, Id + 4);
         Emit_Visible_And_Can_Focus (TWdg, Id + 4, TWdg.Editable);
         Emit_Property (Id + 4, "has-focus", TWdg.Has_Focus);
         Emit_Align (TWdg, Id + 4, Numeric => False);
         Emit_Margin (TWdg, Id + 4);
         Emit_ToolTip (TWdg, Id + 4);
         if not TWdg.Editable then
            Emit_Property (Id + 4, "editable", False);
         end if;
         if TWdg.Text /= null and then TWdg.Text.all /= "" then
            Emit_Line (Sp (Id + 4) & "<property name=""text"" " &
                         "translatable=""yes"">" &
                         TWdg.Text.all & "</property>");
         elsif TWdg.Buffer /= null and then TWdg.Text_Buffer /= null then
            Emit_Property (Id + 4, "buffer", TWdg.Buffer.Name.all);
         end if;
         if TWdg.MaxLength > 0 then
            Emit_Property (Id + 4, "max-length", TWdg.MaxLength);
            Emit_Property (Id + 4, "width-chars", TWdg.MaxLength);
         end if;
         Emit_Has_Frame (TWdg, Id + 4);
         Emit_GtkSignal (TWdg, Id + 4);
         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Packing_Child (TWdg, Id,
                                        Packing => False,
                                        XY      => False,
                                        Homog   => False);
         Emit_Line (Sp (Id) & "</child>");
      end Emit_EntryItem;
      procedure Emit_ToolItem (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_ToolItem (TWdg : Widget_Pointer; Id : Integer) is
         Sensitive : constant Boolean := TWdg.Enabled;
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkToolItem",
                      "toolitem_" & TWdg.Name.all);
         TWdg.Enabled := True;
         Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
         TWdg.Enabled := Sensitive;

         case TWdg.Widget_Type is
            when GtkLabel =>
               Emit_LabelItem (TWdg, Id + 4);

            when GtkComboTextBox =>
               Emit_ComboboxTextItem (TWdg, Id + 4);

            when GtkButton | GtkRadioButton | GtkToggleButton =>
               Emit_ButtonItem (TWdg, Id + 4);

            when GtkEntry =>
               Emit_EntryItem (TWdg, Id + 4);

            when others => null;
         end case;

         Emit_Line (Sp (Id + 2) & "</object>");
         if TWdg.Widget_Type = GtkLabel
           and then TWdg.GParent.Widget_Type = BindingNavigator
         then
            Emit_Line (Sp (Id + 2) & "<packing>");
            Emit_Property (Id + 4, "expand", True);
            Emit_Property (Id + 4, "homogeneous", True);
            Emit_Line (Sp (Id + 2) & "</packing>");
         else
            Emit_Packing_Child (TWdg, Id,
                                Packing => True,
                                XY      => False,
                                Homog   => True);
         end if;
         Emit_Line (Sp (Id) & "</child>");
      end Emit_ToolItem;

      Child : Widget_Pointer;
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkToolbar", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
      Emit_Align (TWdg, Id + 4, Numeric => False);
      Emit_ToolTip (TWdg, Id + 4);
      Emit_Margin (TWdg, Id + 4);
      if not TWdg.TB_Horiz then
         Emit_Line (Sp (Id + 4) & "<property name=""orientation"">"
                    & "vertical" & "</property>");
      end if;
      case TWdg.DStyle is
         when Unset => null;
         when Icons_Only =>
            Emit_Line (Sp (Id + 4) & "<property name=""toolbar-style"">"
                       & "icons" & "</property>");
         when Text_Only =>
            Emit_Line (Sp (Id + 4) & "<property name=""toolbar-style"">"
                       & "text" & "</property>");
         when Text_Beside_Icons =>
            Emit_Line (Sp (Id + 4) & "<property name=""toolbar-style"">"
                       & "both-horiz" & "</property>");
         when Text_Below_Icons =>
            Emit_Line (Sp (Id + 4) & "<property name=""toolbar-style"">"
                       & "both" & "</property>");
      end case;
      if not TWdg.Show_Arrows then
         Emit_Line (Sp (Id + 4) & "<property name=""show-arrow"">"
                      & "False" & "</property>");
      end if;
      Emit_GtkSignal (TWdg, Id + 4);

      Child := TWdg.Child_List;
      while Child /= null loop
         case Child.Widget_Type is
            when GtkButton | GtkRadioButton | GtkToggleButton
               | GtkLabel | GtkComboTextBox | GtkEntry =>
               Emit_ToolItem (Child, Id + 4);

            when GtkSeparatorToolItem =>
               Emit_SeparatorToolItem (Child, Id + 4);

            when GtkMenuBar =>
               Emit_MenuToolButton (Child, Id + 4);

            when others => raise Program_Error;
         end case;
         Child := Child.Next;
      end loop;

      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing (Id + 2,
                    TWdg.Child_Number, False, True, TWdg.Padding, True);
      Emit_Line (Sp (Id) & "</child>");
   end Emit_GtkToolBar;

   ---------------------------------------------------------------------------
   --  Emit_GtkStatusBar
   ---------------------------------------------------------------------------
   procedure Emit_GtkStatusBar (TWdg : Widget_Pointer;
                                Id   : Integer;
                                Pos  : Integer) is
      Child : Widget_Pointer;
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkStatusbar", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      if Pos = -1 then
         Emit_WH_Request (TWdg, Id + 4);
      end if;
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
      Emit_Align (TWdg, Id + 4, Numeric => False);
      Emit_Margin (TWdg, Id + 4);
      Emit_ToolTip (TWdg, Id + 4);
      Emit_Property (Id + 4, "hexpand", False);
      Emit_Property (Id + 4, "orientation", "vertical");
      Emit_Property (Id + 4, "spacing", 2);
      --  Emit_Attributes (TWdg, Id + 4);
      Emit_GtkSignal (TWdg, Id + 4);

      Child := TWdg.Child_List;
      while Child /= null loop
         Emit_Widget_Child (Child, Id + 2);
         Child := Child.Next;
      end loop;

      Emit_Line (Sp (Id + 2) & "</object>");
      if Pos = -1 then
         Emit_Packing_Child (TWdg, Id,
                                        Packing => True,
                                        XY => True,
                                        Homog => False);
      else
         Emit_Packing (Id + 2, Pos, False, True, TWdg.Padding, True);
      end if;
      Emit_Line (Sp (Id) & "</child>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkStatusBar: " & TWdg.Name.all);
         raise;
   end Emit_GtkStatusBar;

   ---------------------------------------------------------------------------
   procedure Emit_GtkToolPalette is
   begin
      null;
   end Emit_GtkToolPalette;

   ---------------------------------------------------------------------------
   procedure Emit_GtkPaned is
   begin
      null;
   end Emit_GtkPaned;

   ---------------------------------------------------------------------------
   --  Emit_GtkButtonBox
   ---------------------------------------------------------------------------
   procedure Emit_GtkButtonBox (TWdg : Widget_Pointer;
                                Id   : Integer) is
      Child   : Widget_Pointer;
   begin
      if TWdg.Name = null then
         Emit_Object (TWdg, Id + 2, "GtkButtonBox", "");
      else
         Emit_Object (TWdg, Id + 2, "GtkButtonBox", TWdg.Name.all);
      end if;
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
      case TWdg.Layout_Style is
         when Edge => null;
         when Spread => Emit_Property (Id + 4, "layout-style", "spread");
         when Start_Row => Emit_Property (Id + 4, "layout-style", "start");
         when End_Row => Emit_Property (Id + 4, "layout-style", "end");
         when Center => Emit_Property (Id + 4, "layout-style", "center");
         when Expand => Emit_Property (Id + 4, "layout-style", "expand");
      end case;

      Child := TWdg.Child_List;
      while Child /= null loop
         Emit_Widget_Child (Child, Id + 4,
                            From_ButtonBox => True);
         Child := Child.Next;
      end loop;

      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing (Id + 2, 0, False, False, 0, True);
   end Emit_GtkButtonBox;

   ---------------------------------------------------------------------------
   procedure Emit_GtkLayout is
   begin
      null;
   end Emit_GtkLayout;

   ---------------------------------------------------------------------------
   --  Emit_GtkFixed
   ---------------------------------------------------------------------------
   procedure Emit_GtkFixed (TWdg : Widget_Pointer;
                            Id   : Integer) is
      Child : Widget_Pointer;
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkFixed", "GtkFixed_" & TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
      Emit_Margin (TWdg, Id + 4);

      Child := TWdg.Child_List;
      while Child /= null loop
         Emit_Widget_Child (Child, Id + 4);
         Child := Child.Next;
      end loop;

      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing (Id + 2, 0, True, True, 0, True);
      Emit_Line (Sp (Id) & "</child>");
   end Emit_GtkFixed;

   ---------------------------------------------------------------------------
   procedure Emit_GtkEventBox is
   begin
      null;
   end Emit_GtkEventBox;

   ---------------------------------------------------------------------------
   procedure Emit_GtkExpander is
   begin
      null;
   end Emit_GtkExpander;

   ---------------------------------------------------------------------------
   procedure Emit_GtkViewport is
   begin
      null;
   end Emit_GtkViewport;

   ---------------------------------------------------------------------------
   --  Emit_ScrolledWindow
   ---------------------------------------------------------------------------
   procedure Emit_ScrolledWindow (TWdg : Widget_Pointer;
                                  Id   : Integer) is
      Child : Widget_Pointer;
      Pos   : constant Integer := 1;
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkScrolledWindow",
                   "GtkScrolledWindow_" & TWdg.Name.all);
      Emit_Property (Id + 4, "name", "GtkScrolledWindow_" & TWdg.Name.all);
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, True);
      case TWdg.ScrollBars is
         when None => raise Program_Error;
         when Vertical =>
            Emit_Property (Id + 4, "hscrollbar-policy", "never");
         when Horizontal =>
            Emit_Property (Id + 4, "vscrollbar-policy", "never");
         when Both =>
            null;
      end case;
      Emit_Property (Id + 4, "shadow-type", "in");

      Child := TWdg.Child_List;
      while Child /= null loop
         Emit_Widget_Child (Child, Id + 2);
         Child := Child.Next;
      end loop;

      Emit_Line (Sp (Id + 2) & "</object>");
      if Pos > 0 then
         Emit_Line (Sp (Id + 2) & "<packing>");
         Emit_Property (Id + 4, "position", Pos);
         Emit_Line (Sp (Id + 2) & "</packing>");

      end if;
      Emit_Line (Sp (Id) & "</child>");
   end Emit_ScrolledWindow;

   ---------------------------------------------------------------------------
   --  Emit_Alignment
   ---------------------------------------------------------------------------
   procedure Emit_Alignment (TWdg : Widget_Pointer;
                             Id   : Integer) is
      Child : Widget_Pointer;
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkAlignment",
                   "Alignment_" & TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
      Emit_Line (Sp (Id + 4)
                 & "<property name=""left-padding"">12</property>");

      Child := TWdg.Child_List;
      while Child /= null loop
         Emit_Widget_Child (Child, Id + 2);
         Child := Child.Next;
      end loop;

      Emit_Line (Sp (Id + 6) & "</object>");  --  gtkalignment
      Emit_Packing_Child (TWdg, Id + 4,
                          Packing => False,
                          XY => True,
                          Homog => False);
      Emit_Line (Sp (Id + 4) & "</child>");
   end Emit_Alignment;

   --------------------------------------------------------------------------
   procedure Emit_GtkRevealer is
   begin
      null;
   end Emit_GtkRevealer;
   --------------------------------------------------------------------------
   procedure Emit_GtkSearchBar is
   begin
      null;
   end Emit_GtkSearchBar;
   --------------------------------------------------------------------------
   procedure Emit_GtkHeaderBar is
   begin
      null;
   end Emit_GtkHeaderBar;
   --------------------------------------------------------------------------
   procedure Emit_GtkStack is
   begin
      null;
   end Emit_GtkStack;
   --------------------------------------------------------------------------
   procedure Emit_GtkPopover is
   begin
      null;
   end Emit_GtkPopover;
   --------------------------------------------------------------------------
   procedure Emit_GtkPopoverMenu is
   begin
      null;
   end Emit_GtkPopoverMenu;
   --------------------------------------------------------------------------
   procedure Emit_GtkActionBar is
   begin
      null;
   end Emit_GtkActionBar;

end Emit_Containers;
