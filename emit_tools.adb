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
with Symbol_Tables;           use Symbol_Tables;
with Gdk.RGBA;
with Glib;                    use Glib;
with GNAT.Strings;            use GNAT.Strings;
with Strings_Edit.Integers;
with Ada.Characters.Handling;

package body Emit_Tools is
   package SEI renames Strings_Edit.Integers;

   ----------------------------------------------------------------------
   function To_Hex (XWindows_Colorname : String) return String;
   function To_Hex (XWindows_Colorname : String) return String is
      H : Gdk.RGBA.Gdk_RGBA;
      Success : Boolean;

      function Hex (X : Gdouble) return String;
      function Hex (X : Gdouble) return String is
         I : Integer;
         R : String (1 .. 4);
         P : Integer := R'First;
      begin
         if X <= 1.0 then  -- %
            I := Integer (X * 2.0**16) - 1;
         elsif X <= 256.0 then
            I := Integer (X * 2.0**8);
         elsif X <= 2.0**16 then
            I := Integer (X);
         else
            I := 0;
         end if;
         SEI.Put (Destination => R,
                  Pointer     => P,
                  Value       => I,
                  Base        => 16,
                  Field       => 0,
                  Justify     => Strings_Edit.Right,
                  Fill        => ' ');

         return Ada.Characters.Handling.To_Lower (R);
      exception
         when others => return "0000";
      end Hex;
   begin
      Gdk.RGBA.Parse (H, XWindows_Colorname, Success);
      if Success then
         return Hex (H.Red) & Hex (H.Green) & Hex (H.Blue);
      else
         return XWindows_Colorname;
      end if;
   end To_Hex;

   ----------------------------------------------------------------------
   procedure Emit_Placeholder (Id : Integer) is
   begin
      Emit_Line (Sp (Id) & "<child>");
      Emit_Line (Sp (Id + 2) & "<placeholder/>");
      Emit_Line (Sp (Id) & "</child>");
   end Emit_Placeholder;

   ----------------------------------------------------------------------
   procedure Emit_Property (Id     : Integer;
                            PName  : String;
                            PValue : String;
                            Translatable : Boolean := False) is
   begin
      if Translatable then
         if PValue /= "" then
            Emit_Line (Sp (Id) & "<property name="
                       & """" & PName & """"
                       & " translatable=""yes"">"
                       & PValue
                       & "</property>");
         end if;
      else
         Emit_Line (Sp (Id) & "<property name="
                    & """" & PName & """>"
                    & PValue
                    & "</property>");
      end if;
   end Emit_Property;

   ----------------------------------------------------------------------
   procedure Emit_Property (Id     : Integer;
                            PName  : String;
                            PValue : Boolean) is
   begin
      if PValue then
         Emit_Property (Id, PName, "True");
      elsif PName /= "has-focus" and PName /= "visible" then
         Emit_Property (Id, PName, "False");
      end if;
   end Emit_Property;

   ----------------------------------------------------------------------
   procedure Emit_Property (Id     : Integer;
                            PName  : String;
                            PValue : Integer) is
   begin
      Emit_Property (Id, PName, Img (PValue));
   end Emit_Property;

   ----------------------------------------------------------------------
   procedure Emit_Property (Id     : Integer;
                            PName  : String;
                            PValue : Float) is
   begin
      if PValue = 0.0 then
         Emit_Property (Id, PName, "0");
      else
         Emit_Property (Id, PName, Img (PValue, 0));
      end if;
   end Emit_Property;

   ----------------------------------------------------------------------
   procedure Emit_Line (Text : String) is
   begin
      TIO.Put_Line (GFile, Text);
   end Emit_Line;

   ----------------------------------------------------------------------
   procedure Emit_GtkSignals (Me : Window_Pointer;
                              Id : Integer) is
      TS : Signal_Pointer := Me.Signal_List;
   begin
      while TS /= null loop
         if TS.Glade then
            if TS.After then
               Emit_Line (Sp (Id) & "<signal name="""
                          & TS.GtkName.all
                          & """ handler="""
                          & TS.Handler.all
                          & " after=""yes"""
                          & """ swapped=""no""/>");
            else
               Emit_Line (Sp (Id) & "<signal name="""
                          & TS.GtkName.all
                          & """ handler="""
                          & TS.Handler.all
                          & """ swapped=""no""/>");
            end if;
         end if;
         TS := TS.Next;
      end loop;
   exception
      when Unknown_Signal =>
         TIO.Put_Line ("Line" & TS.Line'Image
                       & ": Convert to Glade: Unknown signal: "
                       & TS.Name.all
                       & " (Window " & Me.Name.all & ")");
   end Emit_GtkSignals;

   ----------------------------------------------------------------------
   procedure Emit_One_GtkSignal (TS   : Signal_Pointer;
                                 Id   : Integer;
                                 Name : String) is
   begin
      if TS.After then
         Emit_Line (Sp (Id) & "<signal name="""
                    & TS.GtkName.all
                    & """ handler=""" & TS.Handler.all & """"
                    & " object=""" & Name & """"
                    & " after=""yes"""
                    & " swapped=""no""/>");
      else
         Emit_Line (Sp (Id) & "<signal name="""
                    & TS.GtkName.all
                    & """ handler=""" & TS.Handler.all & """"
                    & " object=""" & Name & """"
                    & " swapped=""no""/>");
      end if;
   end Emit_One_GtkSignal;

   ----------------------------------------------------------------------
   procedure Emit_GtkSignal (Me       : Widget_Pointer;
                             Id       : Integer;
                             Except   : String := "";
                             Only_For : String := "") is
      TS   : Signal_Pointer := Me.Signal_List;
      Emit : Boolean;
   begin
      if Except /= "" and then Only_For /= "" then
         TIO.Put_Line ("Emit_GtkSignal "
                       & "Except and Only_For cannot be both /= """"");
         raise Program_Error;
      end if;
      if Me.Name = null or else Me.Name.all = "" then
         TIO.Put_Line ("Emit_GtkSignal: "
                       & "Widget has no ID (name)");
         raise Program_Error;
      end if;
      while TS /= null loop
         if TS.Glade then
            if Except = "" and then Only_For = "" then
               Emit := True;
            elsif Except /= "" then
               Emit := (Except /= TS.Name.all) and (Except /= TS.GtkName.all);
            elsif Only_For /= "" then
               Emit := (Only_For = TS.Name.all) or else (Only_For = TS.GtkName.all);
            else
               Emit := False;
            end if;
            if Emit then
               if TS.After then
                  Emit_Line (Sp (Id) & "<signal name="""
                             & TS.GtkName.all
                             & """ handler=""" & TS.Handler.all & """"
                             & " object=""" & Me.Name.all & """"
                             & " after=""yes"""
                             & " swapped=""no""/>");
               else
                  Emit_Line (Sp (Id) & "<signal name="""
                             & TS.GtkName.all
                             & """ handler=""" & TS.Handler.all & """"
                             & " object=""" & Me.Name.all & """"
                             & " swapped=""no""/>");
               end if;
            end if;
         end if;
         TS := TS.Next;
      end loop;
   exception
      when Unknown_Signal =>
         TIO.Put_Line ("Line " & Img (TS.Line)
                       & ": Convert to Glade: Unknown signal: "
                       & TS.Name.all
                       & " (Widget " & Me.Name.all & ")");
   end Emit_GtkSignal;

   ----------------------------------------------------------------------
   procedure Emit_Has_Frame (Me : Widget_Pointer;
                             Id : Integer) is
   begin
      if Me.Widget_Type = GtkEntry or
        Me.Widget_Type = GtkComboTextBox
      then
         if not Me.Has_Frame then
            Emit_Property (Id, "has-frame", False);
         end if;
      end if;
   end Emit_Has_Frame;

   ----------------------------------------------------------------------
   procedure Emit_Margin (Me : Widget_Pointer;
                          Id : Integer) is
   begin
      if Me.Margins (1) > 0 then
         Emit_Property (Id, "margin-start", Me.Margins (1));
      end if;
      if Me.Margins (3) > 0 then
         Emit_Property (Id, "margin-end", Me.Margins (3));
      end if;
      if Me.Margins (2) > 0 then
         Emit_Property (Id, "margin-top", Me.Margins (2));
      end if;
      if Me.Margins (4) > 0 then
         Emit_Property (Id, "margin-bottom", Me.Margins (4));
      end if;
   end Emit_Margin;

   ----------------------------------------------------------------------
   procedure Emit_Padding (Me : Widget_Pointer;
                           Id : Integer) is
   begin
      if Me.Paddings (1) > 0 then
         Emit_Property (Id, "left-padding", Me.Paddings (1));
      end if;
      if Me.Paddings (3) > 0 then
         Emit_Property (Id, "right-padding", Me.Paddings (3));
      end if;
      if Me.Paddings (2) > 0 then
         Emit_Property (Id, "top-padding", Me.Paddings (2));
      end if;
      if Me.Paddings (4) > 0 then
         Emit_Property (Id, "bottom-padding", Me.Paddings (4));
      end if;
   end Emit_Padding;

   ----------------------------------------------------------------------
   procedure Emit_Child (Me : Widget_Pointer;
                         Id : Integer;
                         Emit_Type_Label : Boolean) is
      pragma Unreferenced (Me);
   begin
      if Emit_Type_Label then
         Emit_Line (Sp (Id) & "<child type=""label"">");
      else
         Emit_Line (Sp (Id) & "<child>");
      end if;
   end Emit_Child;

   ----------------------------------------------------------------------
   procedure Emit_Object (Me     : Widget_Pointer;
                          Id     : Integer; --  identation
                          Wdg    : String;  --  gtk widget
                          WId    : String;  --  Widget_Id
                          Finish : Boolean := False) is
      pragma Unreferenced (Me);
   begin
      if WId /= "" then
         Emit_Line (Sp (Id) & "<object class="""
                    & Wdg
                    & """ id="""
                    & WId
                    & (if Finish then """/>" else """>"));
      else
         Emit_Line (Sp (Id) & "<object class=""" & Wdg & """>");
      end if;
      Debug (-1, Sp (Id) & Wdg & " " & WId);
   end Emit_Object;

   ----------------------------------------------------------------------
   procedure Emit_Name (Me : Window_Pointer;
                        Id : Integer) is
   begin
      if Me.Name /= null and then Me.Name.all /= "" then
         Emit_Property (Id, "name", Me.Name.all);
      end if;
   end Emit_Name;

   ----------------------------------------------------------------------
   procedure Emit_Name (Name : String;
                        Id : Integer) is
   begin
      Emit_Property (Id, "name", Name);
   end Emit_Name;

   ----------------------------------------------------------------------
   procedure Emit_Name (Me : Widget_Pointer;
                        Id : Integer) is
   begin
      if Me.Name /= null then
         Emit_Property (Id, "name", Me.Name.all);
      end if;
   end Emit_Name;

   ----------------------------------------------------------------------
   procedure Emit_WH_Request (Me : Widget_Pointer;
                              Id : Integer) is
   begin
      if Me.Size.Horiz > 0 then
         Emit_Property (Id, "width-request", Me.Size.Horiz);
      end if;
      if Me.Size.Vert > 0 then
         Emit_Property (Id, "height-request", Me.Size.Vert);
      end if;
   end Emit_WH_Request;

   ----------------------------------------------------------------------
   procedure Emit_Visible_And_Can_Focus (Me    : Widget_Pointer;
                                         Id    : Integer;
                                         Focus : Boolean) is
   begin
      Emit_Property (Id, "visible", Me.Visible);
      if not Me.Enabled then
         Emit_Property (Id, "sensitive", False);
      end if;
      Emit_Property (Id, "can-focus", Focus);
   end Emit_Visible_And_Can_Focus;

   ----------------------------------------------------------------------
   procedure Emit_Button_Image (Me        : Widget_Pointer;
                                Id          : Integer;
                                Icon_Widget : Boolean) is
   begin
      if Me.Win_Image /= null then
         if Icon_Widget then
            Emit_Property (Id, "icon-widget", Me.Win_Image.Name.all);
         else
            Emit_Property (Id, "image", Me.Win_Image.Name.all);
            Emit_Property (Id, "always-show-image", True);
         end if;
      end if;
   end Emit_Button_Image;

   ----------------------------------------------------------------------
   procedure Emit_Password (Me : Widget_Pointer;
                            Id : Integer) is
   begin
      if Me.PasswordChar /= null then
         Emit_Property (Id, "visibility", False);
         if Me.PasswordChar.all /= "" then
            Emit_Line (Sp (Id) & "<property name=""invisible-char"">"
                       & Me.PasswordChar.all (Me.PasswordChar.all'First)
                       & "</property>");
         end if;
      end if;
   end Emit_Password;

   ----------------------------------------------------------------------
   procedure Emit_Label (Me         : Widget_Pointer;
                         Id         : Integer;
                         UnderLine  : Boolean;
                         Selectable : Boolean) is
   begin
      if Me.Text /= null and then Me.Text.all /= "" then
         Emit_Property (Id, "label", Me.Text.all, True);
         if UnderLine then
            Emit_Property (Id, "visible", True);
            Emit_Property (Id, "use-underline", True);
         end if;
         if Selectable then
            Emit_Property (Id, "selectable", True);
         end if;
      end if;
   end Emit_Label;

   ----------------------------------------------------------------------
   procedure Emit_Packing (Id         : Integer;
                           Position   : Integer;
                           Expand     : Boolean;
                           Fill       : Boolean;
                           Padding    : Integer;
                           Pack_Start : Boolean;
                           Force      : Boolean := False) is
   begin
      if Expand
        or (not Fill)
        or (Padding > 0)
        or (not Pack_Start)
        or (Position > 0)
        or Force
      then
         Emit_Line (Sp (Id) & "<packing>");
         if Expand or Force then
            Emit_Property (Id + 2, "expand", Expand);
         end if;
         if (not Fill) or Force then
            Emit_Property (Id + 2, "fill", Fill);
         end if;
         if Padding > 0 then
            Emit_Property (Id + 2, "padding", Padding);
         end if;
         if not Pack_Start then
            Emit_Property (Id + 2, "pack-type", "end");
         end if;
         if (Position > 0) or Force then
            Emit_Property (Id + 2, "position", Position);
         end if;
         Emit_Line (Sp (Id) & "</packing>");
      end if;
   end Emit_Packing;

   ----------------------------------------------------------------------
   procedure Emit_Packing_Child (Me      : Widget_Pointer;
                                 Id      : Integer;
                                 Packing : Boolean;
                                 XY      : Boolean;
                                 Homog   : Boolean) is
      procedure Emit_Packing (Id    : Integer;
                              XY    : Boolean;
                              Homog : Boolean);
      procedure Emit_Packing (Id    : Integer;
                              XY    : Boolean;
                              Homog : Boolean) is
      begin
         case Me.Widget_Type is
            when GtkFileChooserButton =>
               if Me.TrayLocation.From_Left <= 0 and then
                 Me.TrayLocation.From_Top + Me.Win_Parent.TrayHeight <= 0
               then
                  return;
               end if;
               Emit_Line (Sp (Id) & "<packing>");
               Emit_Property (Id + 2, "x", Me.TrayLocation.From_Left);
               Emit_Property (Id + 2, "y", Me.TrayLocation.From_Top +
                                Me.Win_Parent.TrayHeight);
               Emit_Line (Sp (Id) & "</packing>");
            when others =>
               if XY then
                  if Me.Location.From_Left <= 0 and then
                    Me.Location.From_Top <= 0
                  then
                     return;
                  end if;
                  Emit_Line (Sp (Id) & "<packing>");
                  if Me.Location.From_Left /= 0 then
                     Emit_Property (Id + 2, "x", Me.Location.From_Left);
                  end if;
                  if Me.Location.From_Top /= 0 then
                     Emit_Property (Id + 2, "y", Me.Location.From_Top);
                  end if;
                  Emit_Line (Sp (Id) & "</packing>");
               elsif Homog then
                  Emit_Line (Sp (Id) & "<packing>");
                  Emit_Property (Id + 2, "expand", False);
                  Emit_Property (Id + 2, "homogeneous", True);
                  Emit_Line (Sp (Id) & "</packing>");
               else
                  Emit_Line (Sp (Id) & "<packing>");
                  Emit_Property (Id + 2, "expand", False);
                  Emit_Property (Id + 2, "homogeneous", False);
                  Emit_Line (Sp (Id) & "</packing>");
               end if;
         end case;
      end Emit_Packing;
   begin
      if Packing then
         Emit_Packing (Id + 2, XY, Homog);
      end if;
   end Emit_Packing_Child;

   ----------------------------------------------------------------------
   procedure Emit_Packing_Tabchild (Id       : Integer;
                                    Position : Integer;
                                    Tab_Fill : Boolean) is
   begin
      Emit_Line (Sp (Id) & "<packing>");
      if Position > 0 then
         Emit_Property (Id + 2, "position", Position);
      end if;
      Emit_Property (Id + 2, "tab-fill", Tab_Fill);
      Emit_Line (Sp (Id) & "</packing>");
   end Emit_Packing_Tabchild;

   ----------------------------------------------------------------------
   procedure Emit_Align (Me      : Widget_Pointer;
                         Id      : Integer;
                         Numeric : Boolean) is
   begin
      case Me.TextAlign is
         when TopLeft =>
            if Numeric then
               Emit_Property (Id, "xalign", 0.0);
               if Me.Widget_Type /= GtkEntry then
                  Emit_Property (Id, "yalign", 0.0);
               end if;
            else
               Emit_Property (Id, "halign", "start");
               Emit_Property (Id, "valign", "start");
            end if;
         when TopCenter =>
            if Numeric then
               Emit_Property (Id, "xalign", 0.5);
               if Me.Widget_Type /= GtkEntry then
                  Emit_Property (Id, "yalign", 0.0);
               end if;
            else
               Emit_Property (Id, "halign", "center");
               Emit_Property (Id, "valign", "start");
            end if;
         when TopRight =>
            if Numeric then
               Emit_Property (Id, "xalign", 1.0);
               if Me.Widget_Type /= GtkEntry then
                  Emit_Property (Id, "yalign", 0.0);
               end if;
            else
               Emit_Property (Id, "halign", "end");
               Emit_Property (Id, "valign", "start");
            end if;
         when MiddleLeft =>
            if Numeric then
               Emit_Property (Id, "xalign", 0.0);
               if Me.Widget_Type /= GtkEntry then
                  Emit_Property (Id, "yalign", 0.5);
               end if;
            else
               Emit_Property (Id, "halign", "start");
               Emit_Property (Id, "valign", "center");
            end if;
         when MiddleCenter =>
            if Numeric then
               Emit_Property (Id, "xalign", 0.5);
               if Me.Widget_Type /= GtkEntry then
                  Emit_Property (Id, "yalign", 0.5);
               end if;
            else
               Emit_Property (Id, "halign", "center");
               Emit_Property (Id, "valign", "center");
            end if;
         when MiddleRight =>
            if Numeric then
               Emit_Property (Id, "xalign", 1.0);
               if Me.Widget_Type /= GtkEntry then
                  Emit_Property (Id, "yalign", 0.5);
               end if;
            else
               Emit_Property (Id, "halign", "end");
               Emit_Property (Id, "valign", "center");
            end if;
         when BottomLeft =>
            if Numeric then
               Emit_Property (Id, "xalign", 0.0);
               if Me.Widget_Type /= GtkEntry then
                  Emit_Property (Id, "yalign", 1.0);
               end if;
            else
               Emit_Property (Id, "halign", "start");
               Emit_Property (Id, "valign", "end");
            end if;
         when BottomCenter =>
            if Numeric then
               Emit_Property (Id, "xalign", 0.5);
               if Me.Widget_Type /= GtkEntry then
                  Emit_Property (Id, "yalign", 1.0);
               end if;
            else
               Emit_Property (Id, "halign", "center");
               Emit_Property (Id, "valign", "end");
            end if;
         when BottomRight =>
            if Numeric then
               Emit_Property (Id, "xalign", 1.0);
               if Me.Widget_Type /= GtkEntry then
                  Emit_Property (Id, "yalign", 1.0);
               end if;
            else
               Emit_Property (Id, "halign", "end");
               Emit_Property (Id, "valign", "end");
            end if;
         when Right =>
            if Numeric then
               Emit_Property (Id, "xalign", 1.0);
            else
               Emit_Property (Id, "halign", "end");
            end if;
         when Left =>
            if Numeric then
               Emit_Property (Id, "xalign", 0.0);
            else
               Emit_Property (Id, "halign", "start");
            end if;
         when Top =>
            if Numeric then
               Emit_Property (Id, "yalign", 0.0);
            else
               Emit_Property (Id, "valign", "start");
            end if;
         when Bottom =>
            if Numeric then
               Emit_Property (Id, "yalign", 1.0);
            else
               Emit_Property (Id, "valign", "end");
            end if;
         when Center =>
            if Numeric then
               Emit_Property (Id, "xalign", 0.5);
               if Me.Widget_Type /= GtkEntry then
                  Emit_Property (Id, "yalign", 0.5);
               end if;
            else
               Emit_Property (Id, "halign", "center");
               Emit_Property (Id, "valign", "center");
            end if;
         when None => null;
      end case;
   end Emit_Align;

   ----------------------------------------------------------------------
   procedure Emit_CheckAlign (Me : Widget_Pointer;
                              Id : Integer) is
   begin
      if Me.CheckAlign /= null then
         if Me.CheckAlign.all = "TopLeft" then
            Emit_Property (Id, "xalign", 0.0);
            Emit_Property (Id, "yalign", 0.0);
         elsif Me.CheckAlign.all = "TopCenter" then
            Emit_Property (Id, "xalign", 0.5);
            Emit_Property (Id, "yalign", 0.0);
         elsif Me.CheckAlign.all = "TopRight" then
            Emit_Property (Id, "xalign", 1.0);
            Emit_Property (Id, "yalign", 0.0);
         elsif Me.CheckAlign.all = "MiddleLeft" then
            Emit_Property (Id, "xalign", 0.0);
            Emit_Property (Id, "yalign", 0.5);
         elsif Me.CheckAlign.all = "MiddleCenter" then
            Emit_Property (Id, "xalign", 0.5);
            Emit_Property (Id, "yalign", 0.5);
         elsif Me.CheckAlign.all = "MiddleRight" then
            Emit_Property (Id, "xalign", 1.0);
            Emit_Property (Id, "yalign", 0.5);
         elsif Me.CheckAlign.all = "BottomLeft" then
            Emit_Property (Id, "xalign", 0.0);
            Emit_Property (Id, "yalign", 1.0);
         elsif Me.CheckAlign.all = "BottomCenter" then
            Emit_Property (Id, "xalign", 0.5);
            Emit_Property (Id, "yalign", 1.0);
         elsif Me.CheckAlign.all = "BottomRight" then
            Emit_Property (Id, "xalign", 1.0);
            Emit_Property (Id, "yalign", 1.0);
         end if;
      end if;
   end Emit_CheckAlign;

   ----------------------------------------------------------------------
   procedure Emit_Scrollbars_Policy (Me : Widget_Pointer;
                                     Id : Integer) is
   begin
      case Me.H_ScrollBar is
         when Always =>
            Emit_Property (Id, "hscrollbar-policy", "always");
         when Automatic =>
            null;
         when Never =>
            Emit_Property (Id, "hscrollbar-policy", "never");
         when External =>
            null;
      end case;
      case Me.V_ScrollBar is
         when Always =>
            Emit_Property (Id, "vscrollbar-policy", "always");
         when Automatic =>
            null;
         when Never =>
            Emit_Property (Id, "vscrollbar-policy", "never");
         when External =>
            null;
      end case;
   end Emit_Scrollbars_Policy;

   ----------------------------------------------------------------------
   procedure Emit_ToolTip (Me : Widget_Pointer;
                           Id : Integer) is
   begin
      if Me.ToolTip /= null and then Me.ToolTip.all /= "" then
         case Me.Widget_Type is
            when GtkLabel | GtkEntry | GtkImage | GtkButton |
                 GtkCheckButton | GtkRadioButton | GtkFrame =>
               Emit_Property (Id, "tooltip-text", Me.ToolTip.all, True);
            when GtkSpinButton =>
               Emit_Property (Id, "primary-icon-tooltip-text",
                              Me.ToolTip.all, True);
            when others => null;
         end case;
      end if;
   end Emit_ToolTip;

   ----------------------------------------------------------------------
   procedure Emit_Attributes (Me : Widget_Pointer;
                              Id : Integer) is
   begin
      if (Me.Font_Name = null or else Me.Font_Name.all = "") and
        (Me.BgColor = null or else Me.BgColor.all = "") and
        (Me.FgColor = null or else Me.FgColor.all = "") and
        (Me.UlColor = null or else Me.UlColor.all = "")
      then
         return;
      end if;
      Emit_Line (Sp (Id) & "<attributes>");

      if Me.Font_Name /= null and then Me.Font_Name.all /= "" then
         Emit_Line (Sp (Id + 2)
                    & "<attribute name=""font-desc"" value="""
                    & Me.Font_Name.all & " "
                    & Img (Me.Font_Size)
                    & """/>");
         if Me.Font_Weight /= null
           and then (Me.Font_Weight.all /= ""
                     and then Me.Font_Weight.all /= "none")
         then
            Emit_Line (Sp (Id + 2)
                       & "<attribute name=""weight"" value="""
                       & Me.Font_Weight.all & """/>");
         end if;
      end if;

      if Me.FgColor /= null and then Me.FgColor.all /= "" then
         Emit_Line (Sp (Id + 2)
                    & "<attribute name=""foreground"" value="""
                    & "#" & To_Hex (Me.FgColor.all)
                    & """/>");
      end if;

      if not Me.UseVisualStyleBackColor then
         if Me.BgColor /= null and then Me.BgColor.all /= "" then
            Emit_Line (Sp (Id + 2)
                       & "<attribute name=""background"" value="""
                       & "#" & To_Hex (Me.BgColor.all)
                       & """/>");
         end if;
      end if;

      if Me.Font_Underline then
         Emit_Line (Sp (Id + 2)
                    & "<attribute name=""underline"" value="""
                    & "True"
                    & """/>");
      end if;

      if Me.UlColor /= null and then Me.UlColor.all /= "" then
         Emit_Line (Sp (Id + 2)
                    & "<attribute name=""underline-color"" value="""
                    & "#" & To_Hex (Me.UlColor.all)
                    & """/>");
      end if;

      Emit_Line (Sp (Id) & "</attributes>");
   end Emit_Attributes;

end Emit_Tools;
