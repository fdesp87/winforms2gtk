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
   procedure Emit_GtkSignal (TWin : Window_Pointer; Id : Integer) is
      TS : Signal_Pointer := TWin.Signal_List;
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
                       & " (Window " & TWin.Name.all & ")");
   end Emit_GtkSignal;

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
   procedure Emit_GtkSignal (TWdg     : Widget_Pointer;
                             Id       : Integer;
                             Except   : String := "";
                             Only_For : String := "") is
      TS   : Signal_Pointer := TWdg.Signal_List;
      Emit : Boolean;
   begin
      if Except /= "" and then Only_For /= "" then
         TIO.Put_Line ("Emit_GtkSignal "
                       & "Except and Only_For cannot be both /= """"");
         raise Program_Error;
      end if;
      if TWdg.Name = null or else TWdg.Name.all = "" then
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
                             & " object=""" & TWdg.Name.all & """"
                             & " after=""yes"""
                             & " swapped=""no""/>");
               else
                  Emit_Line (Sp (Id) & "<signal name="""
                             & TS.GtkName.all
                             & """ handler=""" & TS.Handler.all & """"
                             & " object=""" & TWdg.Name.all & """"
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
                       & " (Widget " & TWdg.Name.all & ")");
   end Emit_GtkSignal;

   ----------------------------------------------------------------------
   procedure Emit_Has_Frame (TWdg : Widget_Pointer; Id : Integer) is
   begin
      if TWdg.Widget_Type = GtkEntry or
        TWdg.Widget_Type = GtkComboTextBox
      then
         if not TWdg.Has_Frame then
            Emit_Property (Id, "has-frame", False);
         end if;
      end if;
   end Emit_Has_Frame;

   ----------------------------------------------------------------------
   procedure Emit_Margin (TWdg : Widget_Pointer; Id : Integer) is
   begin
      if TWdg.Margins (1) > 0 then
         Emit_Property (Id, "margin-start", TWdg.Margins (1));
      end if;
      if TWdg.Margins (3) > 0 then
         Emit_Property (Id, "margin-end", TWdg.Margins (3));
      end if;
      if TWdg.Margins (2) > 0 then
         Emit_Property (Id, "margin-top", TWdg.Margins (2));
      end if;
      if TWdg.Margins (4) > 0 then
         Emit_Property (Id, "margin-bottom", TWdg.Margins (4));
      end if;
   end Emit_Margin;
   --  procedure Emit_Margin (TWin : Window_Pointer; Id : Integer) is
   --  begin
   --     if TWin.Margins (2) /= -1 then
   --        Emit_Property (Id, "margin-start", TWin.Margins (1));
   --     end if;
   --     if TWin.Margins (4) /= -1 then
   --        Emit_Property (Id, "margin-end", TWin.Margins (2));
   --     end if;
   --     if TWin.Margins (1) /= -1 then
   --        Emit_Property (Id, "margin-top", TWin.Margins (3));
   --     end if;
   --     if TWin.Margins (3) /= -1 then
   --        Emit_Property (Id, "margin-bottom", TWin.Margins (4));
   --     end if;
   --  end Emit_Margin;

   ----------------------------------------------------------------------
   procedure Emit_Child (TWdg : Widget_Pointer;
                         Id     : Integer;
                         Emit_Type_Label : Boolean) is
      pragma Unreferenced (TWdg);
   begin
      if Emit_Type_Label then
         Emit_Line (Sp (Id) & "<child type=""label"">");
      else
         Emit_Line (Sp (Id) & "<child>");
      end if;
   end Emit_Child;

   ----------------------------------------------------------------------
   procedure Emit_Object (TWdg   : Widget_Pointer;
                          Id     : Integer; --  identation
                          Wdg    : String;  --  gtk widget
                          WId    : String;  --  Widget_Id
                          Finish : Boolean := False) is
      pragma Unreferenced (TWdg);
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
   procedure Emit_Name (TWin : Window_Pointer; Id : Integer) is
   begin
      if TWin.Name /= null then
         Emit_Property (Id, "name", TWin.Name.all);
      end if;
   end Emit_Name;

   ----------------------------------------------------------------------
   procedure Emit_Name (Name : String; Id : Integer) is
   begin
      Emit_Property (Id, "name", Name);
   end Emit_Name;

   ----------------------------------------------------------------------
   procedure Emit_Name (TWdg : Widget_Pointer; Id : Integer) is
   begin
      if TWdg.Name /= null then
         Emit_Property (Id, "name", TWdg.Name.all);
      end if;
   end Emit_Name;

   ----------------------------------------------------------------------
   procedure Emit_WH_Request (TWdg : Widget_Pointer;
                              Id   : Integer) is
   begin
      if TWdg.Size.Horiz > 0 then
         Emit_Property (Id, "width-request", TWdg.Size.Horiz);
      end if;
      if TWdg.Size.Vert > 0 then
         Emit_Property (Id, "height-request", TWdg.Size.Vert);
      end if;
   end Emit_WH_Request;

   ----------------------------------------------------------------------
   procedure Emit_Visible_And_Can_Focus (TWdg  : Widget_Pointer;
                                         Id    : Integer;
                                         Focus : Boolean) is
   begin
      Emit_Property (Id, "visible", TWdg.Visible);
      if not TWdg.Enabled then
         Emit_Property (Id, "sensitive", False);
      end if;
      Emit_Property (Id, "can-focus", Focus);
   end Emit_Visible_And_Can_Focus;

   ----------------------------------------------------------------------
   procedure Emit_Button_Image (TWdg        : Widget_Pointer;
                                Id          : Integer;
                                Icon_Widget : Boolean) is
   begin
      if TWdg.Win_Image /= null then
         if Icon_Widget then
            Emit_Property (Id, "icon-widget", TWdg.Win_Image.Name.all);
         else
            Emit_Property (Id, "image", TWdg.Win_Image.Name.all);
            Emit_Property (Id, "always-show-image", True);
         end if;
      end if;
   end Emit_Button_Image;

   ----------------------------------------------------------------------
   procedure Emit_Password (TWdg : Widget_Pointer; Id : Integer) is
   begin
      if TWdg.PasswordChar /= null then
         Emit_Property (Id, "visibility", False);
         if TWdg.PasswordChar.all /= "" then
            Emit_Line (Sp (Id) & "<property name=""invisible-char"">"
                       & TWdg.PasswordChar.all (TWdg.PasswordChar.all'First)
                       & "</property>");
         end if;
      end if;
   end Emit_Password;

   ----------------------------------------------------------------------
   procedure Emit_Label (TWdg       : Widget_Pointer;
                         Id         : Integer;
                         UnderLine  : Boolean;
                         Selectable : Boolean) is
   begin
      if TWdg.Text /= null and then TWdg.Text.all /= "" then
         Emit_Property (Id, "label", TWdg.Text.all, True);
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
                           Pack_Start : Boolean) is
   begin
      Emit_Line (Sp (Id) & "<packing>");
      Emit_Property (Id + 2, "expand", Expand);
      Emit_Property (Id + 2, "fill", Fill);
      if Padding > 0 then
         Emit_Property (Id + 2, "padding", Padding);
      end if;
      if not Pack_Start then
         Emit_Property (Id + 2, "pack-type", "end");
      end if;
      Emit_Property (Id + 2, "position", Position);
      Emit_Line (Sp (Id) & "</packing>");
   end Emit_Packing;

   ----------------------------------------------------------------------
   procedure Emit_Packing_Child (TWdg    : Widget_Pointer;
                                 Id      : Integer;
                                 Packing : Boolean;
                                 XY      : Boolean;
                                 Homog   : Boolean) is
      procedure Emit_Packing (TWdg  : Widget_Pointer;
                              Id    : Integer;
                              XY    : Boolean;
                              Homog : Boolean);
      procedure Emit_Packing (TWdg  : Widget_Pointer;
                              Id    : Integer;
                              XY    : Boolean;
                              Homog : Boolean) is
      begin
         case TWdg.Widget_Type is
            when GtkFileChooserButton =>
               if TWdg.TrayLocation.From_Left <= 0 and then
                 TWdg.TrayLocation.From_Top + TWdg.WParent.TrayHeight <= 0
               then
                  return;
               end if;
               Emit_Line (Sp (Id) & "<packing>");
               Emit_Property (Id + 2, "x", TWdg.TrayLocation.From_Left);
               Emit_Property (Id + 2, "y", TWdg.TrayLocation.From_Top +
                                TWdg.WParent.TrayHeight);
               Emit_Line (Sp (Id) & "</packing>");
            when others =>
               if XY then
                  if TWdg.Location.From_Left <= 0 and then
                    TWdg.Location.From_Top <= 0
                  then
                     return;
                  end if;
                  Emit_Line (Sp (Id) & "<packing>");
                  Emit_Property (Id + 2, "x", TWdg.Location.From_Left);
                  Emit_Property (Id + 2, "y", TWdg.Location.From_Top);
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
         Emit_Packing (TWdg, Id + 2, XY, Homog);
      end if;
   end Emit_Packing_Child;

   ----------------------------------------------------------------------
   procedure Emit_Align (TWdg    : Widget_Pointer;
                         Id      : Integer;
                         Numeric : Boolean) is
   begin
      case TWdg.TextAlign is
         when TopLeft =>
            if Numeric then
               Emit_Property (Id, "xalign", 0.0);
               if TWdg.Widget_Type /= GtkEntry then
                  Emit_Property (Id, "yalign", 0.0);
               end if;
            else
               Emit_Property (Id, "halign", "start");
               Emit_Property (Id, "valign", "start");
            end if;
         when TopCenter =>
            if Numeric then
               Emit_Property (Id, "xalign", 0.5);
               if TWdg.Widget_Type /= GtkEntry then
                  Emit_Property (Id, "yalign", 0.0);
               end if;
            else
               Emit_Property (Id, "halign", "center");
               Emit_Property (Id, "valign", "start");
            end if;
         when TopRight =>
            if Numeric then
               Emit_Property (Id, "xalign", 1.0);
               if TWdg.Widget_Type /= GtkEntry then
                  Emit_Property (Id, "yalign", 0.0);
               end if;
            else
               Emit_Property (Id, "halign", "end");
               Emit_Property (Id, "valign", "start");
            end if;
         when MiddleLeft =>
            if Numeric then
               Emit_Property (Id, "xalign", 0.0);
               if TWdg.Widget_Type /= GtkEntry then
                  Emit_Property (Id, "yalign", 0.5);
               end if;
            else
               Emit_Property (Id, "halign", "start");
               Emit_Property (Id, "valign", "center");
            end if;
         when MiddleCenter =>
            if Numeric then
               Emit_Property (Id, "xalign", 0.5);
               if TWdg.Widget_Type /= GtkEntry then
                  Emit_Property (Id, "yalign", 0.5);
               end if;
            else
               Emit_Property (Id, "halign", "center");
               Emit_Property (Id, "valign", "center");
            end if;
         when MiddleRight =>
            if Numeric then
               Emit_Property (Id, "xalign", 1.0);
               if TWdg.Widget_Type /= GtkEntry then
                  Emit_Property (Id, "yalign", 0.5);
               end if;
            else
               Emit_Property (Id, "halign", "end");
               Emit_Property (Id, "valign", "center");
            end if;
         when BottomLeft =>
            if Numeric then
               Emit_Property (Id, "xalign", 0.0);
               if TWdg.Widget_Type /= GtkEntry then
                  Emit_Property (Id, "yalign", 1.0);
               end if;
            else
               Emit_Property (Id, "halign", "start");
               Emit_Property (Id, "valign", "end");
            end if;
         when BottomCenter =>
            if Numeric then
               Emit_Property (Id, "xalign", 0.5);
               if TWdg.Widget_Type /= GtkEntry then
                  Emit_Property (Id, "yalign", 1.0);
               end if;
            else
               Emit_Property (Id, "halign", "center");
               Emit_Property (Id, "valign", "end");
            end if;
         when BottomRight =>
            if Numeric then
               Emit_Property (Id, "xalign", 1.0);
               if TWdg.Widget_Type /= GtkEntry then
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
               if TWdg.Widget_Type /= GtkEntry then
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
   procedure Emit_CheckAlign (TWdg : Widget_Pointer; Id : Integer) is
   begin
      if TWdg.CheckAlign /= null then
         if TWdg.CheckAlign.all = "TopLeft" then
            Emit_Property (Id, "xalign", 0.0);
            Emit_Property (Id, "yalign", 0.0);
         elsif TWdg.CheckAlign.all = "TopCenter" then
            Emit_Property (Id, "xalign", 0.5);
            Emit_Property (Id, "yalign", 0.0);
         elsif TWdg.CheckAlign.all = "TopRight" then
            Emit_Property (Id, "xalign", 1.0);
            Emit_Property (Id, "yalign", 0.0);
         elsif TWdg.CheckAlign.all = "MiddleLeft" then
            Emit_Property (Id, "xalign", 0.0);
            Emit_Property (Id, "yalign", 0.5);
         elsif TWdg.CheckAlign.all = "MiddleCenter" then
            Emit_Property (Id, "xalign", 0.5);
            Emit_Property (Id, "yalign", 0.5);
         elsif TWdg.CheckAlign.all = "MiddleRight" then
            Emit_Property (Id, "xalign", 1.0);
            Emit_Property (Id, "yalign", 0.5);
         elsif TWdg.CheckAlign.all = "BottomLeft" then
            Emit_Property (Id, "xalign", 0.0);
            Emit_Property (Id, "yalign", 1.0);
         elsif TWdg.CheckAlign.all = "BottomCenter" then
            Emit_Property (Id, "xalign", 0.5);
            Emit_Property (Id, "yalign", 1.0);
         elsif TWdg.CheckAlign.all = "BottomRight" then
            Emit_Property (Id, "xalign", 1.0);
            Emit_Property (Id, "yalign", 1.0);
         end if;
      end if;
   end Emit_CheckAlign;

   ----------------------------------------------------------------------
   procedure Emit_ToolTip (TWdg : Widget_Pointer; Id : Integer) is
   begin
      if TWdg.ToolTip /= null and then TWdg.ToolTip.all /= "" then
         case TWdg.Widget_Type is
            when GtkLabel | GtkEntry | GtkImage | GtkButton |
                 GtkCheckButton | GtkRadioButton | GtkFrame =>
               Emit_Property (Id, "tooltip-text", TWdg.ToolTip.all, True);
            when GtkSpinButton =>
               Emit_Property (Id, "primary-icon-tooltip-text",
                              TWdg.ToolTip.all, True);
            when others => null;
         end case;
      end if;
   end Emit_ToolTip;

   ----------------------------------------------------------------------
   procedure Emit_Attributes (TWdg : Widget_Pointer; Id : Integer) is
   begin
      if (TWdg.Font_Name = null or else TWdg.Font_Name.all = "") and
        (TWdg.BgColor = null or else TWdg.BgColor.all = "") and
        (TWdg.FgColor = null or else TWdg.FgColor.all = "") and
        (TWdg.UlColor = null or else TWdg.UlColor.all = "")
      then
         return;
      end if;
      Emit_Line (Sp (Id) & "<attributes>");

      if TWdg.Font_Name /= null and then TWdg.Font_Name.all /= "" then
         Emit_Line (Sp (Id + 2)
                    & "<attribute name=""font-desc"" value="""
                    & TWdg.Font_Name.all & " "
                    & Img (TWdg.Font_Size)
                    & """/>");
         if TWdg.Font_Weight /= null
           and then (TWdg.Font_Weight.all /= ""
                     and then TWdg.Font_Weight.all /= "none")
         then
            Emit_Line (Sp (Id + 2)
                       & "<attribute name=""weight"" value="""
                       & TWdg.Font_Weight.all & """/>");
         end if;
      end if;

      if TWdg.FgColor /= null and then TWdg.FgColor.all /= "" then
         Emit_Line (Sp (Id + 2)
                    & "<attribute name=""foreground"" value="""
                    & "#" & To_Hex (TWdg.FgColor.all)
                    & """/>");
      end if;

      if not TWdg.UseVisualStyleBackColor then
         if TWdg.BgColor /= null and then TWdg.BgColor.all /= "" then
            Emit_Line (Sp (Id + 2)
                       & "<attribute name=""background"" value="""
                       & "#" & To_Hex (TWdg.BgColor.all)
                       & """/>");
         end if;
      end if;

      if TWdg.Font_Underline then
         Emit_Line (Sp (Id + 2)
                    & "<attribute name=""underline"" value="""
                    & "True"
                    & """/>");
      end if;

      if TWdg.UlColor /= null and then TWdg.UlColor.all /= "" then
         Emit_Line (Sp (Id + 2)
                    & "<attribute name=""underline-color"" value="""
                    & "#" & To_Hex (TWdg.UlColor.all)
                    & """/>");
      end if;

      Emit_Line (Sp (Id) & "</attributes>");
   end Emit_Attributes;

end Emit_Tools;
