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
with GNAT.Strings;          use GNAT.Strings;
with Emit_Tools;            use Emit_Tools;
with Emit_Containers;       use Emit_Containers;

package body Emit_Controls is

   ---------------------------------------------------------------------------
   --  Emit_GtkButton
   ---------------------------------------------------------------------------
   procedure Emit_GtkButton (Me          : Widget_Pointer;
                             Id          : Integer;
                             Object      : String;
                             Has_Default : Boolean;
                             XY          : Boolean) is
      Child : Widget_Pointer;
      Underline : constant Boolean := Me.Underline;

      procedure Emit_Button_Label;
      procedure Emit_Button_Label is
      begin
         if Me.Text /= null and then Me.Text.all /= "" then
            Emit_Line (Sp (Id + 4) & "<property name=""label"" " &
                         "translatable=""yes"">" &
                         Me.Text.all & "</property>");
            if Underline then
               Emit_Property (Id + 4, "use-underline", Underline);
            end if;
         elsif Me.Name /= null and then Me.Name.all /= "" then
            Emit_Line (Sp (Id + 4) & "<property name=""label"" " &
                         "translatable=""yes"">" &
                         Me.Name.all & "</property>");
            if Underline then
               Emit_Property (Id + 4, "use-underline", Underline);
            end if;
         end if;
      end Emit_Button_Label;
   begin
      if Me.Associated_ColorButton /= null then
         return;
      end if;
      Emit_Child (Me, Id, False);
      Emit_Object (Me, Id + 2, Object, Me.Name.all);
      if Me.Child_List /= null and then Me.Child_List.Stock then
         null;
      else
         if Me.Wdg_Parent /= null
           and then Me.Wdg_Parent.Widget_Type /= BindingNavigator
         then
            Emit_Button_Label;
         elsif Me.Win_Parent /= null then
            Emit_Button_Label;
         end if;
      end if;

      Emit_Name (Me, Id + 4);
      Emit_WH_Request (Me, Id + 4);
      Emit_Visible_And_Can_Focus (Me, Id + 4, True);
      Emit_Property (Id + 4, "has-focus", Me.Has_Focus);
      if Has_Default then
         Emit_Property (Id + 4, "can-default", True);
         Emit_Property (Id + 4, "has-default", True);
      end if;
      if Me.Child_List /= null and then Me.Child_List.Stock then
         Emit_Property (Id + 4, "focus-on-click", False);
      end if;
      Emit_Property (Id + 4, "receives-default", True);
      if Me.Child_List /= null and then not Me.Child_List.Stock then
         Emit_Button_Image (Me, Id + 4,
                            Icon_Widget => (Object = "GtkToolButton"));
      end if;
      Emit_Align (Me, Id + 4, Numeric => False);
      Emit_Margin (Me, Id + 4);
      Emit_ToolTip (Me, Id + 4);
      if Me.Child_List /= null and then Me.Child_List.Stock then
         Emit_Property (Id + 4, "relief", "none");
      end if;
      Emit_GtkSignal (Me, Id + 4);

      Child := Me.Child_List;
      while Child /= null loop
         Emit_Widget_Child (Child, Id + 4);
         Child := Child.Next;
      end loop;

      Emit_Line (Sp (Id + 2) & "</object>");
      if Me.Wdg_Parent /= null and then
        ((Me.Wdg_Parent.Widget_Type = GtkTabChild)
        or (Me.Wdg_Parent.Widget_Type = GtkButtonBox))
      then
         Emit_Packing (Id + 2,
                       Position   => Me.Child_Number,
                       Expand     => False,
                       Fill       => True,
                       Padding    => Me.Padding,
                       Pack_Start => True,
                       Force      => True);
      else
         Emit_Packing_Child (Me, Id,
                             Packing => True,
                             XY      => XY,
                             Homog   => False);
      end if;
      Emit_Line (Sp (Id) & "</child>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkButton: " & Me.Name.all);
         raise;
   end Emit_GtkButton;

   ---------------------------------------------------------------------------
   --  Emit_GtkEntry
   ---------------------------------------------------------------------------
   procedure Emit_GtkEntry (Me : Widget_Pointer;
                            Id : Integer) is
   begin
      Emit_Child (Me, Id, False);
      Emit_Object (Me, Id + 2, "GtkEntry", Me.Name.all);
      Emit_Name (Me, Id + 4);
      Emit_WH_Request (Me, Id + 4);
      Emit_Visible_And_Can_Focus (Me, Id + 4, Me.Editable);
      Emit_Property (Id + 4, "has-focus", Me.Has_Focus);
      Emit_Password (Me, Id + 4);
      Emit_ToolTip (Me, Id + 4);
      if Me.Activates_Default then
         Emit_Property (Id + 4, "activates-default", True);
      end if;
      Emit_Has_Frame (Me, Id + 4);
      if Me.MaxLength > 0 then
         Emit_Property (Id + 4, "width-chars", Me.MaxLength);
         Emit_Property (Id + 4, "max-width-chars", Me.MaxLength);
      end if;
      if Me.Text /= null and then Me.Text.all /= "" then
         Emit_Line (Sp (Id + 4) & "<property name=""text"" " &
                      "translatable=""yes"">" &
                      Me.Text.all & "</property>");
      elsif Me.Buffer /= null and then Me.Text_Buffer /= null then
         Emit_Property (Id + 4, "buffer", Me.Buffer.Name.all);
      end if;
      Emit_Margin (Me, Id + 4);
      if not Me.Editable then
         Emit_Property (Id + 4, "editable", False);
      end if;
      Emit_Align (Me, Id + 4, Numeric => True);
      Emit_GtkSignal (Me, Id + 4);
      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing_Child (Me, Id,
                          Packing => True,
                          XY => True,
                          Homog => False);
      Emit_Line (Sp (Id) & "</child>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkEntry: " & Me.Name.all);
         raise;
   end Emit_GtkEntry;

   ---------------------------------------------------------------------------
   --  Emit_GtkSpinButton
   ---------------------------------------------------------------------------
   procedure Emit_GtkSpinButton (Me : Widget_Pointer;
                                 Id : Integer) is
   begin
      Emit_Child (Me, Id, False);
      Emit_Object (Me, Id + 2, "GtkSpinButton", Me.Name.all);
      Emit_Label (Me, Id + 4,
                  UnderLine  => Me.Underline,
                  Selectable => False);
      Emit_Name (Me, Id + 4);
      Emit_WH_Request (Me, Id + 4);
      Emit_Visible_And_Can_Focus (Me, Id + 4, True);
      if Me.Text /= null and then Me.Text.all /= "" then
         Emit_Line (Sp (Id + 4) & "<property name=""text"" " &
                      "translatable=""yes"">" &
                      Me.Text.all & "</property>");
      end if;
      Emit_Align (Me, Id + 4, Numeric => False);
      Emit_Margin (Me, Id + 4);
      Emit_ToolTip (Me, Id + 4);
      Emit_Property (Id + 4, "numeric", True);
      Emit_Property (Id + 4, "wrap", True);
      Emit_GtkSignal (Me, Id + 4);
      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing_Child (Me, Id,
                          Packing => True,
                          XY => True,
                          Homog => False);
      Emit_Line (Sp (Id) & "</child>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkSpinButton: " & Me.Name.all);
         raise;
   end Emit_GtkSpinButton;

   ---------------------------------------------------------------------------
   --  Emit_GtkToggleButton
   ---------------------------------------------------------------------------
   procedure Emit_GtkToggleButton (Me     : Widget_Pointer;
                                   Id     : Integer;
                                   Object : String;
                                   XY     : Boolean;
                                   Homog  : Boolean) is
   begin
      Emit_Child (Me, Id, False);
      Emit_Object (Me, Id + 2, Object, Me.Name.all);
      Emit_Label (Me, Id + 4,
                  UnderLine  => Me.Underline,
                  Selectable => False);
      Emit_Name (Me, Id + 4);
      Emit_WH_Request (Me, Id + 4);
      Emit_Visible_And_Can_Focus (Me, Id + 4, True);
      Emit_Property (Id + 4, "has-focus", Me.Has_Focus);
      Emit_Property (Id + 4, "receives-default", True);
      Emit_Button_Image (Me, Id + 4,
                         Icon_Widget => (Object = "GtkToggleToolButton"));
      Emit_Align (Me, Id + 4, Numeric => False);
      Emit_Margin (Me, Id + 4);
      Emit_ToolTip (Me, Id + 4);
      if Me.Active then
         Emit_Property (Id + 4, "active", True);
      end if;
      Emit_Property (Id + 4, "draw-indicator", True);
      --  Emit_Attributes (TWdg, Id + 4);
      Emit_GtkSignal (Me, Id + 4);
      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing_Child (Me, Id,
                                     Packing => True,
                                     XY      => XY,
                                     Homog   => Homog);
      Emit_Line (Sp (Id) & "</child>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkToggleButton: " & Me.Name.all);
         raise;
   end Emit_GtkToggleButton;

   ---------------------------------------------------------------------------
   --  Emit_GtkCheckButton
   ---------------------------------------------------------------------------
   procedure Emit_GtkCheckButton (Me : Widget_Pointer;
                                  Id : Integer) is
   begin
      Emit_Child (Me, Id, False);
      Emit_Object (Me, Id + 2, "GtkCheckButton", Me.Name.all);
      Emit_Label (Me, Id + 4,
                  UnderLine => False,
                  Selectable => False);
      Emit_Name (Me, Id + 4);
      Emit_WH_Request (Me, Id + 4);
      Emit_Visible_And_Can_Focus (Me, Id + 4, True);
      Emit_Property (Id + 4, "has-focus", Me.Has_Focus);
      Emit_Property (Id + 4, "receives-default", True);
      Emit_Button_Image (Me, Id + 4, Icon_Widget => False);
      Emit_Align (Me, Id + 4, Numeric => False);
      Emit_Margin (Me, Id + 4);
      Emit_ToolTip (Me, Id + 4);
      Emit_CheckAlign (Me, Id + 4);
      Emit_Property (Id + 4, "draw-indicator", True);
      Emit_GtkSignal (Me, Id + 4);
      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing_Child (Me, Id,
                                     Packing => True,
                                     XY => True,
                                     Homog => False);
      Emit_Line (Sp (Id) & "</child>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkCheckButton: " & Me.Name.all);
         raise;
   end Emit_GtkCheckButton;

   ---------------------------------------------------------------------------
   --  Emit_GtkRadioButton
   ---------------------------------------------------------------------------
   procedure Emit_GtkRadioButton (Me     : Widget_Pointer;
                                  Id     : Integer;
                                  Object : String;
                                  XY     : Boolean;
                                  Homog  : Boolean) is
   begin
      Emit_Child (Me, Id, False);
      Emit_Object (Me, Id + 2, Object, Me.Name.all);
      Emit_Label (Me, Id + 4,
                  UnderLine => Me.Underline,
                  Selectable => False);
      Emit_Name (Me, Id + 4);
      Emit_WH_Request (Me, Id + 4);
      Emit_Visible_And_Can_Focus (Me, Id + 4, True);
      Emit_Property (Id + 4, "has-focus", Me.Has_Focus);
      Emit_Property (Id + 4, "receives-default", True);
      Emit_Button_Image (Me, Id + 4,
                         Icon_Widget => (Object = "GtkToolButton"));
      Emit_Align (Me, Id + 4, Numeric => False);
      Emit_Margin (Me, Id + 4);
      Emit_ToolTip (Me, Id + 4);
      if Me.Active then
         Emit_Property (Id + 4, "active", True);
      end if;
      Emit_Property (Id + 4, "draw-indicator", True);
      if Me.Wdg_Parent /= null and then
        Me.Wdg_Parent.Widget_Type = GtkFixed
      then
         if Me.Wdg_Parent.First_RadioButton = null then
            Me.Wdg_Parent.First_RadioButton := Me;
         end if;
         Emit_Property (Id + 4, "group",
                        Me.Wdg_Parent.First_RadioButton.Name.all);
      end if;
      Emit_GtkSignal (Me, Id + 4);
      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing_Child (Me, Id,
                          Packing => True,
                          XY      => XY,
                          Homog   => Homog);
      Emit_Line (Sp (Id) & "</child>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkRadioButton: " & Me.Name.all);
         raise;
   end Emit_GtkRadioButton;

   ---------------------------------------------------------------------------
   --  Emit_GtkComboTextBox
   ---------------------------------------------------------------------------
   procedure Emit_GtkComboTextBox (Me : Widget_Pointer;
                                   Id : Integer) is
      procedure Emit_Internal_GtkEntry (Id : Integer);
      procedure Emit_Internal_GtkEntry (Id : Integer) is
      begin
         Emit_Line (Sp (Id) & "<child internal-child=""entry"">");
         Emit_Object (Me, Id + 2, "GtkEntry", Me.Name.all & "_TextEntry");
         Emit_Property (Id + 4, "can-focus", Me.Editable);
         if Me.ToolTip /= null and then Me.ToolTip.all /= "" then
            Emit_Line (Sp (Id + 4) & "<property name=""tooltip-text"" "
                       & "translatable=""yes"">" &
                         Me.ToolTip.all & "</property>");
         end if;
         if Me.Buffer /= null and then Me.Text_Buffer /= null then
            Emit_Property (Id + 4, "buffer", Me.Buffer.Name.all);
         end if;
         Emit_Has_Frame (Me, Id + 4);
         if not Me.Editable then
            Emit_Property (Id + 4, "editable", False);
         end if;
         Emit_Align (Me, Id + 4, Numeric => False);
         Emit_Property (Id + 4, "activates-default", True);
         if Me.MaxLength > 0 then
            Emit_Property (Id + 4, "width-chars", Me.MaxLength);
            Emit_Property (Id + 4, "max-width-chars", Me.MaxLength);
         end if;
         if Me.ToolTip /= null and then Me.ToolTip.all /= "" then
            Emit_Line (Sp (Id + 4) & "<property name=""primary-icon-"
                       & "tooltip-text"" "
                       & "translatable=""yes"">" &
                         Me.ToolTip.all & "</property>");
         end if;
         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Line (Sp (Id) & "</child>");
      end Emit_Internal_GtkEntry;
   begin
      Emit_Child (Me, Id, False);
      Emit_Object (Me, Id + 2, "GtkComboBoxText", Me.Name.all);
      Emit_Name (Me, Id + 4);
      Emit_WH_Request (Me, Id + 4);
      Emit_Visible_And_Can_Focus (Me, Id + 4, True);
      Emit_Property (Id + 4, "has-focus", Me.Has_Focus);
      Emit_Margin (Me, Id + 4);
      Emit_ToolTip (Me, Id + 4);
      Emit_Property (Id + 4, "has-entry", True);
      Emit_GtkSignal (Me, Id + 4);

      Emit_Internal_GtkEntry (Id + 4);

      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing_Child (Me, Id,
                          Packing => True,
                          XY      => True,
                          Homog   => False);
      Emit_Line (Sp (Id) & "</child>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkComboTextbox: " & Me.Name.all);
         raise;
   end Emit_GtkComboTextBox;

   ---------------------------------------------------------------------------
   -- Emit_GtkFileChooserButton --
   ---------------------------------------------------------------------------
   procedure Emit_GtkFileChooserButton (Me : Widget_Pointer;
                                        Id : Integer) is
   begin
      Emit_Child (Me, Id, False);
      Emit_Object (Me, Id + 2, "GtkFileChooserButton", Me.Name.all);
      Emit_Name (Me, Id + 4);
      Emit_WH_Request (Me, Id + 4);
      Emit_Visible_And_Can_Focus (Me, Id + 4, False);
      Emit_ToolTip (Me, Id + 4);
      if Me.OpenFileFilter /= null and then Me.OpenFileFilter.all /= ""
      then
         Emit_Property (Id + 4, "filter", Me.OpenFileFilter.all);
      end if;
      if Me.OpenFileDialog /= null and then Me.OpenFileDialog.all /= ""
      then
         Emit_Property (Id + 4, "dialog", Me.OpenFileDialog.all);
      end if;
      if Me.OpenFileTitle /= null and then Me.OpenFileTitle.all /= ""
      then
         Emit_Line (Sp (Id + 4) & "<property name=""title"""
                    & " translatable=""yes"">" & Me.OpenFileTitle.all
                    & "</property>");
      end if;
      Emit_GtkSignal (Me, Id + 4);
      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing_Child (Me, Id,
                          Packing => True,
                          XY => True,
                          Homog => False);
      Emit_Line (Sp (Id) & "</child>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkFileChooserButton: " & Me.Name.all);
         raise;
   end Emit_GtkFileChooserButton;

   ---------------------------------------------------------------------------
   --  Emit_GtkColorButton
   ---------------------------------------------------------------------------
   procedure Emit_GtkColorButton (Me : Widget_Pointer;
                                  Id : Integer) is
   begin
      Emit_Child (Me, Id, False);
      Emit_Object (Me, Id + 2, "GtkColorButton", Me.Name.all);
      Emit_Name (Me, Id + 4);
      Emit_WH_Request (Me, Id + 4);
      Emit_Visible_And_Can_Focus (Me, Id + 4, True);
      Emit_Line (Sp (Id + 4) & "<property name=""receives-default"">" &
                   "True" & "</property>");
      if Me.AnyColor then
         Emit_Line (Sp (Id + 4) & "<property name=""use-alpha"">" &
                      "True" & "</property>");
      end if;
      if Me.FullOpen then
         Emit_Line (Sp (Id + 4) & "<property name=""show-editor"">" &
                      "True" & "</property>");
      end if;
      Emit_Align (Me, Id + 4, Numeric => False);
      Emit_Margin (Me, Id + 4);
      Emit_ToolTip (Me, Id + 4);
      Emit_GtkSignal (Me, Id + 4);
      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing_Child (Me, Id,
                                     Packing => True,
                                     XY      => True,
                                     Homog   => False);
      Emit_Line (Sp (Id) & "</child>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkColorButon: " & Me.Name.all);
         raise;
   end Emit_GtkColorButton;

end Emit_Controls;
