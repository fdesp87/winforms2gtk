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

package body Emit_Controls is

   ---------------------------------------------------------------------------
   --  Emit_GtkButton
   ---------------------------------------------------------------------------
   procedure Emit_GtkButton (TWdg        : Widget_Pointer;
                             Id          : Integer;
                             Object      : String;
                             Position    : Integer;
                             Has_Default : Boolean;
                             XY          : Boolean;
                             Homog       : Boolean) is
      Underline : constant Boolean := TWdg.Underline;

      procedure Emit_Button_Label;
      procedure Emit_Button_Label is
      begin
         if TWdg.Text /= null and then TWdg.Text.all /= "" then
            Emit_Line (Sp (Id + 4) & "<property name=""label"" " &
                         "translatable=""yes"">" &
                         TWdg.Text.all & "</property>");
            if Underline then
               Emit_Property (Id + 4, "use-underline", Underline);
            end if;
         elsif TWdg.Name /= null and then TWdg.Name.all /= "" then
            Emit_Line (Sp (Id + 4) & "<property name=""label"" " &
                         "translatable=""yes"">" &
                         TWdg.Name.all & "</property>");
            if Underline then
               Emit_Property (Id + 4, "use-underline", Underline);
            end if;
         end if;
      end Emit_Button_Label;
   begin
      if TWdg.Associated_ColorButton /= null then
         return;
      end if;
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, Object, TWdg.Name.all);
      if TWdg.GParent /= null
        and then TWdg.GParent.Widget_Type /= BindingNavigator
      then
         Emit_Button_Label;
      elsif TWdg.WParent /= null then
         Emit_Button_Label;
      end if;
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, True);
      Emit_Property (Id + 4, "has-focus", TWdg.Has_Focus);
      if Has_Default then
         Emit_Property (Id + 4, "can-default", True);
         Emit_Property (Id + 4, "has-default", True);
      end if;
      Emit_Property (Id + 4, "receives-default", True);
      Emit_Button_Image (TWdg, Id + 4,
                         Icon_Widget => (Object = "GtkToolButton"));
      Emit_Align (TWdg, Id + 4, Numeric => False);
      Emit_Margin (TWdg, Id + 4);
      Emit_ToolTip (TWdg, Id + 4);
      Emit_GtkSignal (TWdg, Id + 4);
      Emit_Line (Sp (Id + 2) & "</object>");
      if Position < 0 then
         Emit_Packing_Child (TWdg, Id,
                             Packing => True,
                             XY => XY,
                             Homog => Homog);
      else
         Emit_Packing (Id + 2, Position, False, True, 0, True);
      end if;
      Emit_Line (Sp (Id) & "</child>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkButton: " & TWdg.Name.all);
         raise;
   end Emit_GtkButton;

   ---------------------------------------------------------------------------
   --  Emit_GtkEntry
   ---------------------------------------------------------------------------
   procedure Emit_GtkEntry (TWdg : Widget_Pointer;
                            Id   : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkEntry", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, TWdg.Editable);
      Emit_Property (Id + 4, "has-focus", TWdg.Has_Focus);
      Emit_Password (TWdg, Id + 4);
      if TWdg.Activates_Default then
         Emit_Property (Id + 4, "activates-default", True);
      end if;
      Emit_Has_Frame (TWdg, Id + 4);
      if TWdg.MaxLength > 0 then
         Emit_Property (Id + 4, "width-chars", TWdg.MaxLength);
         Emit_Property (Id + 4, "max-width-chars", TWdg.MaxLength);
      end if;
      if TWdg.Text /= null and then TWdg.Text.all /= "" then
         Emit_Line (Sp (Id + 4) & "<property name=""text"" " &
                      "translatable=""yes"">" &
                      TWdg.Text.all & "</property>");
      elsif TWdg.Buffer /= null and then TWdg.Text_Buffer /= null then
         Emit_Property (Id + 4, "buffer", TWdg.Buffer.Name.all);
      end if;
      Emit_Margin (TWdg, Id + 4);
      if not TWdg.Editable then
         Emit_Property (Id + 4, "editable", False);
      end if;
      Emit_ToolTip (TWdg, Id + 4);
      Emit_Align (TWdg, Id + 4, Numeric => True);
      Emit_GtkSignal (TWdg, Id + 4);
      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing_Child (TWdg, Id,
                          Packing => True,
                          XY => True,
                          Homog => False);
      Emit_Line (Sp (Id) & "</child>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkEntry: " & TWdg.Name.all);
         raise;
   end Emit_GtkEntry;

   ---------------------------------------------------------------------------
   --  Emit_GtkSpinButton
   ---------------------------------------------------------------------------
   procedure Emit_GtkSpinButton (TWdg : Widget_Pointer; Id : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkSpinButton", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, True);
      if TWdg.Text /= null and then TWdg.Text.all /= "" then
         Emit_Line (Sp (Id + 4) & "<property name=""text"" " &
                      "translatable=""yes"">" &
                      TWdg.Text.all & "</property>");
      end if;
      Emit_Align (TWdg, Id + 4, Numeric => False);
      Emit_Margin (TWdg, Id + 4);
      Emit_ToolTip (TWdg, Id + 4);
      Emit_Property (Id + 4, "numeric", True);
      Emit_Property (Id + 4, "wrap", True);
      Emit_GtkSignal (TWdg, Id + 4);
      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing_Child (TWdg, Id,
                          Packing => True,
                          XY => True,
                          Homog => False);
      Emit_Line (Sp (Id) & "</child>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkSpinButton: " & TWdg.Name.all);
         raise;
   end Emit_GtkSpinButton;

   ---------------------------------------------------------------------------
   --  Emit_GtkToggleButton
   ---------------------------------------------------------------------------
   procedure Emit_GtkToggleButton (TWdg      : Widget_Pointer;
                                   Id        : Integer;
                                   Object    : String;
                                   Underline : Boolean;
                                   XY        : Boolean;
                                   Homog     : Boolean) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, Object, TWdg.Name.all);
      Emit_Label (TWdg, Id + 4, UnderLine => Underline, Selectable => False);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, True);
      Emit_Property (Id + 4, "has-focus", TWdg.Has_Focus);
      Emit_Property (Id + 4, "receives-default", True);
      Emit_Button_Image (TWdg, Id + 4,
                         Icon_Widget => (Object = "GtkToggleToolButton"));
      Emit_Align (TWdg, Id + 4, Numeric => False);
      Emit_Margin (TWdg, Id + 4);
      Emit_ToolTip (TWdg, Id + 4);
      if TWdg.Active then
         Emit_Property (Id + 4, "active", True);
      end if;
      Emit_Property (Id + 4, "draw-indicator", True);
      --  Emit_Attributes (TWdg, Id + 4);
      Emit_GtkSignal (TWdg, Id + 4);
      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing_Child (TWdg, Id,
                                     Packing => True,
                                     XY      => XY,
                                     Homog   => Homog);
      Emit_Line (Sp (Id) & "</child>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkToggleButton: " & TWdg.Name.all);
         raise;
   end Emit_GtkToggleButton;

   ---------------------------------------------------------------------------
   --  Emit_GtkCheckButton
   ---------------------------------------------------------------------------
   procedure Emit_GtkCheckButton (TWdg : Widget_Pointer; Id : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkCheckButton", TWdg.Name.all);
      Emit_Label (TWdg, Id + 4, UnderLine => False, Selectable => False);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, True);
      Emit_Property (Id + 4, "has-focus", TWdg.Has_Focus);
      Emit_Property (Id + 4, "receives-default", True);
      Emit_Button_Image (TWdg, Id + 4, Icon_Widget => False);
      Emit_Align (TWdg, Id + 4, Numeric => False);
      Emit_Margin (TWdg, Id + 4);
      Emit_ToolTip (TWdg, Id + 4);
      Emit_CheckAlign (TWdg, Id + 4);
      Emit_Property (Id + 4, "draw-indicator", True);
      Emit_GtkSignal (TWdg, Id + 4);
      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing_Child (TWdg, Id,
                                     Packing => True,
                                     XY => True,
                                     Homog => False);
      Emit_Line (Sp (Id) & "</child>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkCheckButton: " & TWdg.Name.all);
         raise;
   end Emit_GtkCheckButton;

   ---------------------------------------------------------------------------
   --  Emit_GtkRadioButton
   ---------------------------------------------------------------------------
   procedure Emit_GtkRadioButton (TWdg      : Widget_Pointer;
                                  Id        : Integer;
                                  Object    : String;
                                  Underline : Boolean;
                                  XY        : Boolean;
                                  Homog     : Boolean) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, Object, TWdg.Name.all);
      Emit_Label (TWdg, Id + 4, UnderLine => Underline, Selectable => False);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, True);
      Emit_Property (Id + 4, "has-focus", TWdg.Has_Focus);
      Emit_Property (Id + 4, "receives-default", True);
      Emit_Button_Image (TWdg, Id + 4,
                         Icon_Widget => (Object = "GtkToolButton"));
      Emit_Align (TWdg, Id + 4, Numeric => False);
      Emit_Margin (TWdg, Id + 4);
      Emit_ToolTip (TWdg, Id + 4);
      if TWdg.Active then
         Emit_Property (Id + 4, "active", True);
      end if;
      Emit_Property (Id + 4, "draw-indicator", True);
      --  Emit_Attributes (TWdg, Id + 4);
      Emit_GtkSignal (TWdg, Id + 4);
      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing_Child (TWdg, Id,
                                     Packing => True,
                                     XY      => XY,
                                     Homog   => Homog);
      Emit_Line (Sp (Id) & "</child>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkRadioButton: " & TWdg.Name.all);
         raise;
   end Emit_GtkRadioButton;

   ---------------------------------------------------------------------------
   --  Emit_GtkComboTextBox
   ---------------------------------------------------------------------------
   procedure Emit_GtkComboTextBox (TWdg    : Widget_Pointer;
                                   Id      : Integer;
                                   Packing : Boolean) is
      procedure Emit_Internal_GtkEntry (Id : Integer);
      procedure Emit_Internal_GtkEntry (Id : Integer) is
      begin
         Emit_Line (Sp (Id) & "<child internal-child=""entry"">");
         Emit_Object (TWdg, Id + 2, "GtkEntry", TWdg.Name.all & "_textentry");
         Emit_Property (Id + 4, "can-focus", TWdg.Editable);
         if TWdg.ToolTip /= null and then TWdg.ToolTip.all /= "" then
            Emit_Line (Sp (Id + 4) & "<property name=""tooltip-text"" "
                       & "translatable=""yes"">" &
                         TWdg.ToolTip.all & "</property>");
         end if;
         if TWdg.Buffer /= null and then TWdg.Text_Buffer /= null then
            Emit_Property (Id + 4, "buffer", TWdg.Buffer.Name.all);
         end if;
         Emit_Has_Frame (TWdg, Id + 4);
         if not TWdg.Editable then
            Emit_Property (Id + 4, "editable", False);
         end if;
         Emit_Align (TWdg, Id + 4, Numeric => False);
         Emit_Property (Id + 4, "activates-default", True);
         if TWdg.MaxLength > 0 then
            Emit_Property (Id + 4, "width-chars", TWdg.MaxLength);
            Emit_Property (Id + 4, "max-width-chars", TWdg.MaxLength);
         end if;
         if TWdg.ToolTip /= null and then TWdg.ToolTip.all /= "" then
            Emit_Line (Sp (Id + 4) & "<property name=""primary-icon-"
                       & "tooltip-text"" "
                       & "translatable=""yes"">" &
                         TWdg.ToolTip.all & "</property>");
         end if;
         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Line (Sp (Id) & "</child>");
      end Emit_Internal_GtkEntry;
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkComboBoxText", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, True);
      Emit_Property (Id + 4, "has-focus", TWdg.Has_Focus);
      Emit_Margin (TWdg, Id + 4);
      Emit_ToolTip (TWdg, Id + 4);
      Emit_Property (Id + 4, "has-entry", True);
      Emit_GtkSignal (TWdg, Id + 4);

      Emit_Internal_GtkEntry (Id + 4);

      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing_Child (TWdg, Id,
                          Packing => Packing,
                          XY      => True,
                          Homog   => False);
      Emit_Line (Sp (Id) & "</child>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkComboTextbox: " & TWdg.Name.all);
         raise;
   end Emit_GtkComboTextBox;

   ---------------------------------------------------------------------------
   -- Emit_GtkFileChooserButton --
   ---------------------------------------------------------------------------
   procedure Emit_GtkFileChooserButton (TWdg : Widget_Pointer; Id : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkFileChooserButton", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
      Emit_ToolTip (TWdg, Id + 4);
      if TWdg.OpenFileFilter /= null and then TWdg.OpenFileFilter.all /= ""
      then
         Emit_Property (Id + 4, "filter", TWdg.OpenFileFilter.all);
      end if;
      if TWdg.OpenFileDialog /= null and then TWdg.OpenFileDialog.all /= ""
      then
         Emit_Property (Id + 4, "dialog", TWdg.OpenFileDialog.all);
      end if;
      if TWdg.OpenFileTitle /= null and then TWdg.OpenFileTitle.all /= ""
      then
         Emit_Line (Sp (Id + 4) & "<property name=""title"""
                    & " translatable=""yes"">" & TWdg.OpenFileTitle.all
                    & "</property>");
      end if;
      Emit_GtkSignal (TWdg, Id + 4);
      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing_Child (TWdg, Id,
                          Packing => True,
                          XY => True,
                          Homog => False);
      Emit_Line (Sp (Id) & "</child>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkFileChooserButton: " & TWdg.Name.all);
         raise;
   end Emit_GtkFileChooserButton;

   ---------------------------------------------------------------------------
   --  Emit_GtkColorButton
   ---------------------------------------------------------------------------
   procedure Emit_GtkColorButton (TWdg : Widget_Pointer; Id : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkColorButton", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, True);
      Emit_Line (Sp (Id + 4) & "<property name=""receives-default"">" &
                   "True" & "</property>");
      if TWdg.AnyColor then
         Emit_Line (Sp (Id + 4) & "<property name=""use-alpha"">" &
                      "True" & "</property>");
      end if;
      if TWdg.FullOpen then
         Emit_Line (Sp (Id + 4) & "<property name=""show-editor"">" &
                      "True" & "</property>");
      end if;
      Emit_Align (TWdg, Id + 4, Numeric => False);
      Emit_Margin (TWdg, Id + 4);
      Emit_ToolTip (TWdg, Id + 4);
      Emit_GtkSignal (TWdg, Id + 4);
      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing_Child (TWdg, Id,
                                     Packing => True,
                                     XY      => True,
                                     Homog   => False);
      Emit_Line (Sp (Id) & "</child>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkColorButon: " & TWdg.Name.all);
         raise;
   end Emit_GtkColorButton;

end Emit_Controls;
