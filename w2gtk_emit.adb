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
with Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.Strings;      use GNAT.Strings;
with GNAT.Calendar.Time_IO;

package body W2gtk_Emit is
   Unknown_Signal : exception;

   procedure Emit_GtkLabel (TWdg    : Widget_Pointer;
                            Id      : Integer;
                            Packing : Boolean);
   procedure Emit_GtkCalendar (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_GtkImage (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_GtkFrame (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_GtkListBox (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_GtkEntry (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_GtkSpinButton (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_GtkComboBox (TWdg    : Widget_Pointer;
                               Id      : Integer;
                               Packing : Boolean := True);

   procedure Emit_GtkButton (TWdg      : Widget_Pointer;
                             Id        : Integer;
                             Object    : String;
                             Underline : Boolean;
                             XY        : Boolean;
                             Homog     : Boolean);
   procedure Emit_GtkColorButton (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_GtkToggleButton (TWdg      : Widget_Pointer;
                                   Id        : Integer;
                                   Object    : String;
                                   Underline : Boolean;
                                   XY        : Boolean;
                                  Homog     : Boolean);
   procedure Emit_GtkRadioButton (TWdg      : Widget_Pointer;
                                  Id        : Integer;
                                  Object    : String;
                                  Underline : Boolean;
                                  XY        : Boolean;
                                  Homog     : Boolean);
   procedure Emit_GtkCheckButton (TWdg : Widget_Pointer; Id : Integer);

   procedure Emit_GtkStatusBar (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_GtkFileChooserButton (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_GtkToolBar (TWdg : Widget_Pointer; Id : Integer);

   ------------------
   --  tools specs --
   ------------------

   procedure Emit_Property (Id : Integer; PName : String; PValue : String);
   procedure Emit_Property (Id : Integer; PName : String; PValue : Boolean);
   procedure Emit_Property (Id : Integer; PName : String; PValue : Integer);
   procedure Emit_Property (Id : Integer; PName : String; PValue : Float);

   procedure Emit_Align (TWdg    : Widget_Pointer;
                         Id      : Integer;
                         Numeric : Boolean);
   procedure Emit_CheckAlign (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_Has_Frame (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_Child (TWdg : Widget_Pointer;
                         Id   : Integer;
                         Emit_Type_Label : Boolean);
   procedure Emit_End_Object_Packing_Child (TWdg    : Widget_Pointer;
                                            Id      : Integer;
                                            Packing : Boolean;
                                            XY      : Boolean;
                                            Homog   : Boolean);
   function Convert_Signal_To_Gtk (Windows_Signal : String) return String;
   procedure Emit_GtkSignal (TWin : Window_Pointer; Id : Integer);
   procedure Emit_GtkSignal (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_Attributes (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_Label (TWdg      : Widget_Pointer;
                         Id        : Integer;
                         UnderLine : Boolean);
   procedure Emit_Line (Text : String);
   procedure Emit_Margin (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_Margin (TWin : Window_Pointer; Id : Integer);
   procedure Emit_Name (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_Name (TWin : Window_Pointer; Id : Integer);
   procedure Emit_Object (TWdg : Widget_Pointer;
                          Id   : Integer;
                          Wdg  : String;
                          WId  : String);
   procedure Emit_Button_Image (TWdg        : Widget_Pointer;
                                Id          : Integer;
                                Icon_Widget : Boolean);
   procedure Emit_Password (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_ToolTip (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_Visible_And_Focus (TWdg  : Widget_Pointer;
                                     Id    : Integer;
                                     Focus : Boolean := False);
   procedure Emit_Visible_And_Focus (TWin  : Window_Pointer;
                                     Id    : Integer;
                                     Focus : Boolean := False);
   procedure Emit_WH_Request (TWdg : Widget_Pointer; Id : Integer);

   -------------------
   --  tools bodies --
   -------------------

   procedure Emit_Property (Id : Integer; PName : String; PValue : String) is
   begin
      Emit_Line (Sp (Id) & "<property name="""
                 & PName
                 & """>"
                 & PValue
                 & "</property>");
   end Emit_Property;

   procedure Emit_Property (Id : Integer; PName : String; PValue : Boolean) is
   begin
      if PValue then
         Emit_Property (Id, PName, "True");
      else
         Emit_Property (Id, PName, "False");
      end if;
   end Emit_Property;

   procedure Emit_Property (Id : Integer; PName : String; PValue : Integer) is
   begin
      Emit_Property (Id, PName, Img (PValue));
   end Emit_Property;

   procedure Emit_Property (Id : Integer; PName : String; PValue : Float) is
   begin
      Emit_Property (Id, PName, Img (PValue));
   end Emit_Property;

   function Convert_Signal_To_Gtk (Windows_Signal : String) return String is
   begin
      if Windows_Signal = "Load" then
         return "realize";
      end if;
      if Windows_Signal = "FormClosing" then
         return "destroy";
      end if;
      if Windows_Signal = "TextChanged" then
         return "changed"; --  gtkentry gtkcombobox
      end if;
      if Windows_Signal = "SelectedIndexChanged" then
         return "changed";  --  combobox
      end if;
      if Windows_Signal = "ValueChanged" then
         return "value-changed";  --  spin
      end if;
      if Windows_Signal = "SelectedValueChanged" then
         return "changed";  --  combobox
      end if;
      if Windows_Signal = "CheckedChanged" then
         return "toggled";  --  checkbutton
      end if;
      if Windows_Signal = "Checked" then
         return "toggled";  --  radiobutton
      end if;
      if Windows_Signal = "Click" then
         return "clicked";
      end if;
      if Windows_Signal = "BeginPrint" then
         return "begin-print";
      end if;
      if Windows_Signal = "PrintPage" then
         return "print-page";
      end if;
      if Windows_Signal = "MouseClick" then
         return "clicked";
      end if;
      if Windows_Signal = "Leave" then
         return "leave-notify-event";
      end if;
      if Windows_Signal = "FileOk" then
         return "file-set";
      end if;
      raise Unknown_Signal;
   end Convert_Signal_To_Gtk;

   procedure Emit_Line (Text : String) is
   begin
      TIO.Put_Line (GFile, Text);
   end Emit_Line;

   procedure Emit_GtkSignal (TWin : Window_Pointer; Id : Integer) is
      TS : Signal_Pointer := TWin.Signal_List;
   begin
      while TS /= null loop
         Emit_Line (Sp (Id) & "<signal name="""
                    & Convert_Signal_To_Gtk (TS.Name.all)
                    & """ handler="""
                    & TS.Handler.all
                    & """ swapped=""no""/>");
         TS := TS.Next;
      end loop;
   exception
      when Unknown_Signal =>
         TIO.Put_Line ("Line" & TS.Line'Image
                       & ": Convert to Glade: Unknown signal: "
                       & TS.Name.all
                       & " (Window " & TWin.Name.all & ")");
   end Emit_GtkSignal;

   procedure Emit_GtkSignal (TWdg : Widget_Pointer; Id : Integer) is
      TS : Signal_Pointer := TWdg.Signal_List;
   begin
      while TS /= null loop
         Emit_Line (Sp (Id) & "<signal name="""
                    & Convert_Signal_To_Gtk (TS.Name.all)
                    & """ handler="""
                    & TS.Handler.all
                    & """ swapped=""no""/>");
         TS := TS.Next;
      end loop;
   exception
      when Unknown_Signal =>
         TIO.Put_Line ("Line" & TS.Line'Image
                       & ": Convert to Glade: Unknown signal: "
                       & TS.Name.all
                       & " (Widget " & TWdg.Name.all & ")");
   end Emit_GtkSignal;

   procedure Emit_GtkHeader (TWin : Window_Pointer; Id : Integer) is
      pragma Unreferenced (TWin);
   begin
      Emit_Line ("<?xml version=""1.0"" encoding=""UTF-8""?>");
      Emit_Line ("<!-- Generated with glade 3.38.2 -->");
      Emit_Line ("<interface>");
      Emit_Line (Sp (Id + 2) & "<requires lib=""gtk+"" version=""3.24""/>");
   end Emit_GtkHeader;

   procedure Emit_GtkTrailer (TWin : Window_Pointer; Id : Integer) is
      pragma Unreferenced (TWin, Id);
   begin
      Emit_Line ("</interface>");
   end Emit_GtkTrailer;

   procedure Emit_Has_Frame (TWdg : Widget_Pointer; Id : Integer) is
   begin
      if TWdg.Widget_Type = GtkEntry or TWdg.Widget_Type = GtkComboBox then
         if not TWdg.Has_Frame then
            Emit_Property (Id, "has-frame", False);
         end if;
      end if;
   end Emit_Has_Frame;

   procedure Emit_Margin (TWdg : Widget_Pointer; Id : Integer) is
   begin
      if TWdg.Margins (1) /= -1 then
         Emit_Property (Id, "margin-top", TWdg.Margins (3));
      end if;
      if TWdg.Margins (2) /= -1 then
         Emit_Property (Id, "margin-start", TWdg.Margins (1));
      end if;
      if TWdg.Margins (3) /= -1 then
         Emit_Property (Id, "margin-bottom", TWdg.Margins (4));
      end if;
      if TWdg.Margins (4) /= -1 then
         Emit_Property (Id, "margin-end", TWdg.Margins (2));
      end if;
   end Emit_Margin;
   procedure Emit_Margin (TWin : Window_Pointer; Id : Integer) is
   begin
      if TWin.Margins (1) /= -1 then
         Emit_Property (Id, "margin-top", TWin.Margins (3));
      end if;
      if TWin.Margins (2) /= -1 then
         Emit_Property (Id, "margin-start", TWin.Margins (1));
      end if;
      if TWin.Margins (3) /= -1 then
         Emit_Property (Id, "margin-bottom", TWin.Margins (4));
      end if;
      if TWin.Margins (4) /= -1 then
         Emit_Property (Id, "margin-end", TWin.Margins (2));
      end if;
   end Emit_Margin;

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
   procedure Emit_Object (TWdg : Widget_Pointer;
                          Id     : Integer;
                          Wdg    : String;
                          WId    : String) is
      pragma Unreferenced (TWdg);
   begin
      if WId /= "" then
         Emit_Line (Sp (Id + 2) & "<object class=""" & Wdg &
                      """ id=""" & WId & """>");
      else
         Emit_Line (Sp (Id + 2) & "<object class=""" & Wdg & """>");
      end if;
   end Emit_Object;

   procedure Emit_Name (TWin : Window_Pointer; Id : Integer) is
   begin
      Emit_Property (Id, "name", TWin.Name.all);
   end Emit_Name;

   procedure Emit_Name (TWdg : Widget_Pointer; Id : Integer) is
   begin
      Emit_Property (Id, "name", TWdg.Name.all);
   end Emit_Name;

   procedure Emit_WH_Request (TWdg : Widget_Pointer; Id : Integer) is
   begin
      Emit_Property (Id, "width-request", TWdg.Size.Horiz);
      Emit_Property (Id, "height-request", TWdg.Size.Vert);
   end Emit_WH_Request;

   procedure Emit_Visible_And_Focus (TWdg  : Widget_Pointer;
                                     Id    : Integer;
                                     Focus : Boolean := False) is
   begin
      Emit_Property (Id, "visible", TWdg.Visible);
      if not TWdg.Enabled then
         Emit_Property (Id, "sensitive", False);
      end if;
      Emit_Property (Id, "can-focus", Focus);
   end Emit_Visible_And_Focus;

   procedure Emit_Visible_And_Focus (TWin  : Window_Pointer;
                                     Id    : Integer;
                                     Focus : Boolean := False) is
      pragma Unreferenced (TWin);
   begin
      Emit_Property (Id + 2, "visible", True);
      Emit_Property (Id + 2, "can-focus", Focus);
   end Emit_Visible_And_Focus;

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

   procedure Emit_Label (TWdg      : Widget_Pointer;
                         Id        : Integer;
                         UnderLine : Boolean) is
   begin
      if TWdg.Text /= null and then TWdg.Text.all /= "" then
         Emit_Line (Sp (Id) & "<property name=""label"" " &
                      "translatable=""yes"">" &
                      TWdg.Text.all & "</property>");
         if UnderLine then
            Emit_Property (Id, "visible", True);
            Emit_Property (Id, "use-underline", True);
         end if;
         Emit_Property (Id, "selectable", True);
      end if;
   end Emit_Label;

   procedure Emit_End_Object_Packing_Child (TWdg    : Widget_Pointer;
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
         Emit_Line (Sp (Id) & "<packing>");
         case TWdg.Widget_Type is
         when GtkFileChooserButton =>
            Emit_Property (Id + 2, "x", TWdg.TrayLocation.From_Left);
            Emit_Property (Id + 2, "y", TWdg.TrayLocation.From_Top +
                           TWdg.WParent.TrayHeight);
         when others =>
            if XY then
               Emit_Property (Id + 2, "x", TWdg.Location.From_Left);
               Emit_Property (Id + 2, "y", TWdg.Location.From_Top);
            elsif Homog then
               Emit_Property (Id + 2, "expand", False);
               Emit_Property (Id + 2, "homogeneous", True);
            else
               Emit_Property (Id + 2, "expand", False);
               Emit_Property (Id + 2, "homogeneous", False);
            end if;
         end case;
         Emit_Line (Sp (Id) & "</packing>");
      end Emit_Packing;
   begin
      Emit_Line (Sp (Id + 2) & "</object>");
      if Packing then
         Emit_Packing (TWdg, Id + 2, XY, Homog);
      end if;
      Emit_Line (Sp (Id) & "</child>");

   end Emit_End_Object_Packing_Child;

   procedure Emit_Align (TWdg    : Widget_Pointer;
                         Id      : Integer;
                         Numeric : Boolean) is
   begin
      if TWdg.TextAlign /= null then
         if Numeric then
            if TWdg.TextAlign.all = "TopLeft" then
               Emit_Property (Id, "xalign", 0.0);
               Emit_Property (Id, "yalign", 0.0);
            elsif TWdg.TextAlign.all = "TopCenter" then
               Emit_Property (Id, "xalign", 0.5);
               Emit_Property (Id, "yalign", 0.0);
            elsif TWdg.TextAlign.all = "TopRight" then
               Emit_Property (Id, "halign", "end");
               Emit_Property (Id, "valign", "start");
            elsif TWdg.TextAlign.all = "MiddleLeft" then
               Emit_Property (Id, "xalign", 0.0);
               Emit_Property (Id, "yalign", 0.5);
            elsif TWdg.TextAlign.all = "MiddleCenter" then
               Emit_Property (Id, "xalign", 0.5);
               Emit_Property (Id, "yalign", 0.5);
            elsif TWdg.TextAlign.all = "MiddleRight" then
               Emit_Property (Id, "xalign", 1.0);
               Emit_Property (Id, "yalign", 0.5);
            elsif TWdg.TextAlign.all = "BottomLeft" then
               Emit_Property (Id, "xalign", 0.0);
               Emit_Property (Id, "yalign", 1.0);
            elsif TWdg.TextAlign.all = "BottomCenter" then
               Emit_Property (Id, "xalign", 0.5);
               Emit_Property (Id, "yalign", 1.0);
            elsif TWdg.TextAlign.all = "BottomRight" then
               Emit_Property (Id, "xalign", 1.0);
               Emit_Property (Id, "yalign", 1.0);
            end if;
         else
            if TWdg.TextAlign.all = "TopLeft" then
               Emit_Property (Id, "halign", "start");
               Emit_Property (Id, "valign", "start");
            elsif TWdg.TextAlign.all = "TopCenter" then
               Emit_Property (Id, "halign", "center");
               Emit_Property (Id, "valign", "start");
            elsif TWdg.TextAlign.all = "TopRight" then
               Emit_Property (Id, "halign", "end");
               Emit_Property (Id, "valign", "start");
            elsif TWdg.TextAlign.all = "MiddleLeft" then
               Emit_Property (Id, "halign", "start");
               Emit_Property (Id, "valign", "center");
            elsif TWdg.TextAlign.all = "MiddleCenter" then
               Emit_Property (Id, "halign", "center");
               Emit_Property (Id, "valign", "center");
            elsif TWdg.TextAlign.all = "MiddleRight" then
               Emit_Property (Id, "halign", "end");
               Emit_Property (Id, "valign", "center");
            elsif TWdg.TextAlign.all = "BottomLeft" then
               Emit_Property (Id, "halign", "start");
               Emit_Property (Id, "valign", "end");
            elsif TWdg.TextAlign.all = "BottomCenter" then
               Emit_Property (Id, "halign", "center");
               Emit_Property (Id, "valign", "end");
            elsif TWdg.TextAlign.all = "BottomRight" then
               Emit_Property (Id, "halign", "end");
               Emit_Property (Id, "valign", "end");
            end if;
         end if;
      end if;
   end Emit_Align;

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

   procedure Emit_ToolTip (TWdg : Widget_Pointer; Id : Integer) is
   begin
      if TWdg.ToolTip /= null and then TWdg.ToolTip.all /= "" then
         case TWdg.Widget_Type is
         when GtkLabel | GtkEntry | GtkImage | GtkButton |
              GtkCheckButton | GtkRadioButton | GtkFrame =>
            Emit_Line (Sp (Id) &
                         "<property name=""tooltip-text""" &
                         " translatable=""yes"">" & TWdg.ToolTip.all &
                         "</property>");
         when GtkSpinButton =>
            Emit_Line (Sp (Id) &
                         "<property name=""primary-icon-tooltip-text""" &
                         " translatable=""yes"">" & TWdg.ToolTip.all &
                         "</property>");
         when others => null;
         end case;
      end if;
   end Emit_ToolTip;

   procedure Emit_Attributes (TWdg : Widget_Pointer; Id : Integer) is
   begin
      if (TWdg.Font_Name = null or else TWdg.Font_Name.all = "") and
        (TWdg.BgColor = null or else TWdg.BgColor.all = "") and
        (TWdg.FgColor = null or else TWdg.FgColor.all = "")
      then
         return;
      end if;
      Emit_Line (Sp (Id) & "<attributes>");

      if TWdg.Font_Name /= null and then TWdg.Font_Name.all /= "" then
         Emit_Line (Sp (Id + 2)
                    & "<attribute name=""font-desc"" value="""
                    & TWdg.Font_Name.all & " "
                    & TWdg.Font_Size.all
                    & """/>");
         if TWdg.Font_Weight /= null and then TWdg.Font_Weight.all /= "" then
            Emit_Line (Sp (Id + 2)
                       & "<attribute name=""weight"" value="""
                       & TWdg.Font_Weight.all & """/>");
         end if;
      end if;

      if TWdg.FgColor /= null and then TWdg.FgColor.all /= "" then
         Emit_Line (Sp (Id + 2)
                    & "<attribute name=""foreground"" value="""
                    & TWdg.FgColor.all
                    & """/>");
      end if;

      if TWdg.BgColor /= null and then TWdg.BgColor.all /= "" then
         Emit_Line (Sp (Id + 2)
                    & "<attribute name=""background"" value="""
                    & TWdg.BgColor.all
                    & """/>");
      end if;

      if TWdg.UlColor /= null and then TWdg.UlColor.all /= "" then
         Emit_Line (Sp (Id + 2)
                    & "<attribute name=""underline-color"" value="""
                    & TWdg.UlColor.all
                    & """/>");
      end if;

      Emit_Line (Sp (Id) & "</attributes>");
   end Emit_Attributes;

   -------------------
   -- Emit_GtkLabel --
   -------------------

   procedure Emit_GtkLabel (TWdg    : Widget_Pointer;
                            Id      : Integer;
                            Packing : Boolean) is
   begin
      if TWdg.BorderStyle /= null and then TWdg.BorderStyle.all /= "" then
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id, "GtkAspectFrame", TWdg.Name.all
                      & "_aspectframe");
         Emit_Name (TWdg, Id + 4);
         Emit_WH_Request (TWdg, Id + 4);
         Emit_Visible_And_Focus (TWdg, Id + 4);
         Emit_Line (Sp (Id + 4) & "<property name=""label-xalign"">0"
                    & "</property>");
         Emit_Line (Sp (Id + 4) & "<property name=""shadow-type"">in"
                    & "</property>");

         Emit_Child (TWdg, Id + 4, False);
         Emit_Object (TWdg, Id + 4, "GtkLabel", TWdg.Name.all);
         Emit_Name (TWdg, Id + 8);
         Emit_Visible_And_Focus (TWdg, Id + 8);
         Emit_Label (TWdg, Id + 8, UnderLine => False);
         Emit_Attributes (TWdg, Id + 8);
         Emit_GtkSignal (TWdg, Id + 8);
         Emit_End_Object_Packing_Child (TWdg, Id + 4,
                                        Packing => False,
                                        XY => True,
                                        Homog => False);
         --  end for the aspect frame
         Emit_End_Object_Packing_Child (TWdg, Id,
                                        Packing => Packing,
                                        XY => True,
                                        Homog => False);
      else
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id, "GtkLabel", TWdg.Name.all);
         Emit_Name (TWdg, Id + 4);
         Emit_WH_Request (TWdg, Id + 4);
         Emit_Visible_And_Focus (TWdg, Id + 4);
         Emit_Align (TWdg, Id + 4, Numeric => False);
         Emit_Margin (TWdg, Id + 4);
         Emit_ToolTip (TWdg, Id + 4);
         Emit_Label (TWdg, Id + 4, UnderLine => False);
         Emit_Attributes (TWdg, Id + 4);
         Emit_GtkSignal (TWdg, Id + 4);
         Emit_End_Object_Packing_Child (TWdg, Id,
                                        Packing => Packing,
                                        XY => True,
                                        Homog => False);
      end if;
   exception
      when others =>
         TIO.Put_Line ("Emit GtkLabel: " & TWdg.Name.all);
         raise;
   end Emit_GtkLabel;

   -------------------
   -- Emit_GtkImage --
   -------------------

   procedure Emit_GtkImage (TWdg : Widget_Pointer;
                            Id   : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id, "GtkImage", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Focus (TWdg, Id + 4);
      Emit_Line (Sp (Id + 4) & "<property name=""pixbuf"">"
                 & TWdg.Image.all
                 & "</property>");
      Emit_Align (TWdg, Id + 4, Numeric => True);
      Emit_Margin (TWdg, Id + 4);
      Emit_ToolTip (TWdg, Id + 4);
      Emit_GtkSignal (TWdg, Id + 4);
      Emit_End_Object_Packing_Child (TWdg, Id,
                                     Packing => True,
                                     XY => True,
                                     Homog => False);
   exception
      when others =>
         TIO.Put_Line ("Emit GtkImage: " & TWdg.Name.all);
         raise;
   end Emit_GtkImage;

   -------------------
   -- Emit_GtkImage --
   -------------------
   procedure Emit_GtkImage (TWin : Window_Pointer;
                            Id   : Integer) is
   begin
      Emit_Object (null, Id - 2, "GtkImage", TWin.Name.all);
      Emit_Name (TWin, Id + 2);
      Emit_Visible_And_Focus (TWin, Id + 2);
      if TWin.Associated_Widget.ImagePath /= null then
         Emit_Property (Id + 1, "pixbuf",
                        TWin.Associated_Widget.ImagePath.all);
      end if;
      Emit_Line (Sp (Id) & "</object>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkImage: " & TWin.Name.all);
         raise;
   end Emit_GtkImage;

   --------------------
   -- Emit_GtkButton --
   --------------------

   procedure Emit_GtkButton (TWdg      : Widget_Pointer;
                             Id        : Integer;
                             Object    : String;
                             Underline : Boolean;
                             XY        : Boolean;
                             Homog     : Boolean) is
   begin
      if TWdg.Associated_ColorButton /= null then
         return;
      end if;
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id, Object, TWdg.Name.all);
      if TWdg.Text /= null and then TWdg.Text.all /= "" then
         Emit_Line (Sp (Id + 4) & "<property name=""label"" " &
                      "translatable=""yes"">" &
                      TWdg.Text.all & "</property>");
      elsif TWdg.Name /= null and then TWdg.Name.all /= "" then
         Emit_Line (Sp (Id + 4) & "<property name=""label"" " &
                      "translatable=""yes"">" &
                      TWdg.Name.all & "</property>");
      end if;
      if Underline then
         Emit_Property (Id + 4, "use-underline", Underline);
      end if;
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Focus (TWdg, Id + 4);
      Emit_Property (Id + 4, "receives-default", True);
      Emit_Button_Image (TWdg, Id + 4,
                         Icon_Widget => (Object = "GtkToolButton"));
      Emit_Align (TWdg, Id + 4, Numeric => False);
      Emit_Margin (TWdg, Id + 4);
      Emit_ToolTip (TWdg, Id + 4);
      Emit_GtkSignal (TWdg, Id + 4);
      Emit_Attributes (TWdg, Id + 4);
      Emit_End_Object_Packing_Child (TWdg, Id,
                                     Packing => True,
                                     XY => XY,
                                     Homog => Homog);
   exception
      when others =>
         TIO.Put_Line ("Emit GtkButton: " & TWdg.Name.all);
         raise;
   end Emit_GtkButton;

   -------------------
   -- Emit_GtkEntry --
   -------------------

   procedure Emit_GtkEntry (TWdg : Widget_Pointer; Id : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id, "GtkEntry", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Focus (TWdg, Id + 4, Focus => TWdg.Editable);
      Emit_Password (TWdg, Id + 4);
      Emit_Align (TWdg, Id + 4, Numeric => False);
      if not TWdg.Editable then
         Emit_Property (Id + 4, "editable", False);
      end if;
      if TWdg.Text /= null and then TWdg.Text.all /= "" then
         Emit_Line (Sp (Id + 4) & "<property name=""text"" " &
                      "translatable=""yes"">" &
                      TWdg.Text.all & "</property>");
      end if;
      Emit_Margin (TWdg, Id + 4);
      Emit_ToolTip (TWdg, Id + 4);
      if TWdg.MaxLength > 0 then
         Emit_Property (Id + 4, "max-length", TWdg.MaxLength);
         Emit_Line (Sp (Id + 4)
                    & "<property name=""max-length"">"
                    & Img (TWdg.MaxLength) & "</property>");
      end if;
      Emit_Has_Frame (TWdg, Id + 4);
      Emit_GtkSignal (TWdg, Id + 4);
      Emit_Attributes (TWdg, Id + 4);
      Emit_End_Object_Packing_Child (TWdg, Id,
                                     Packing => True,
                                     XY => True,
                                     Homog => False);
   exception
      when others =>
         TIO.Put_Line ("Emit GtkEntry: " & TWdg.Name.all);
         raise;
   end Emit_GtkEntry;

   ------------------------
   -- Emit_GtkSpinButton --
   ------------------------

   procedure Emit_GtkSpinButton (TWdg : Widget_Pointer; Id : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id, "GtkSpinButton", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Focus (TWdg, Id + 4);
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
      Emit_Attributes (TWdg, Id + 4);
      Emit_End_Object_Packing_Child (TWdg, Id,
                                     Packing => True,
                                     XY => True,
                                     Homog => False);
   exception
      when others =>
         TIO.Put_Line ("Emit GtkSpinButton: " & TWdg.Name.all);
         raise;
   end Emit_GtkSpinButton;

   ----------------------
   -- Emit_GtkComboBox --
   ----------------------

   procedure Emit_GtkComboBox (TWdg    : Widget_Pointer;
                               Id      : Integer;
                               Packing : Boolean := True) is
      procedure Emit_Internal_GtkEntry (Id : Integer);
      procedure Emit_Internal_GtkEntry (Id : Integer) is
      begin
         Emit_Line (Sp (Id) & "<child internal-child=""entry"">");
         Emit_Line (Sp (Id + 2) & "<object class=""GtkEntry"" id="""
                    & TWdg.Name.all & "_textentry" & """>");
         Emit_Property (Id + 2, "can-focus", False);
         if TWdg.ToolTip /= null and then TWdg.ToolTip.all /= "" then
            Emit_Line (Sp (Id + 4) & "<property name=""tooltip-text"" "
                       & "translatable=""Yes"">" &
                         TWdg.ToolTip.all & "</property>");
         end if;
         Emit_Has_Frame (TWdg, Id + 4);
         if not TWdg.Editable then
            Emit_Property (Id + 4, "editable", False);
         end if;
         if TWdg.MaxLength > 0 then
            Emit_Property (Id + 4, "max-length", TWdg.MaxLength);
         end if;
         if TWdg.ToolTip /= null and then TWdg.ToolTip.all /= "" then
            Emit_Line (Sp (Id + 4) & "<property name=""primary-icon-"
                       & "tooltip-text"" "
                       & "translatable=""Yes"">" &
                         TWdg.ToolTip.all & "</property>");
         end if;
         Emit_Attributes (TWdg, Id + 4);
         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Line (Sp (Id) & "</child>");
      end Emit_Internal_GtkEntry;
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id, "GtkComboBox", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Focus (TWdg, Id + 4);
      Emit_Align (TWdg, Id + 4, Numeric => False);
      Emit_Margin (TWdg, Id + 4);
      Emit_ToolTip (TWdg, Id + 4);
      Emit_Attributes (TWdg, Id + 4);
      Emit_GtkSignal (TWdg, Id + 4);
      Emit_Property (Id + 4, "has-entry", True);
      Emit_Property (Id + 4, "entry-text-column", 1);
      Emit_Internal_GtkEntry (Id + 4);
      Emit_End_Object_Packing_Child (TWdg, Id,
                                     Packing => Packing,
                                     XY => True,
                                     Homog => False);
   exception
      when others =>
         TIO.Put_Line ("Emit GtkCombobox: " & TWdg.Name.all);
         raise;
   end Emit_GtkComboBox;

   --------------------------
   -- Emit_GtkToggleButton --
   --------------------------

   procedure Emit_GtkToggleButton (TWdg      : Widget_Pointer;
                                   Id        : Integer;
                                   Object    : String;
                                   Underline : Boolean;
                                   XY        : Boolean;
                                   Homog     : Boolean) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id, Object, TWdg.Name.all);
      Emit_Label (TWdg, Id + 4, Underline);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Focus (TWdg, Id + 4);
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
      Emit_Attributes (TWdg, Id + 4);
      Emit_GtkSignal (TWdg, Id + 4);
      Emit_End_Object_Packing_Child (TWdg, Id,
                                     Packing => True,
                                     XY      => XY,
                                     Homog   => Homog);
   exception
      when others =>
         TIO.Put_Line ("Emit GtkToggleButton: " & TWdg.Name.all);
         raise;
   end Emit_GtkToggleButton;

   -------------------------
   -- Emit_GtkRadioButton --
   -------------------------

   procedure Emit_GtkRadioButton (TWdg      : Widget_Pointer;
                                  Id        : Integer;
                                  Object    : String;
                                  Underline : Boolean;
                                  XY        : Boolean;
                                  Homog     : Boolean) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id, Object, TWdg.Name.all);
      Emit_Label (TWdg, Id + 4, Underline);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Focus (TWdg, Id + 4);
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
      Emit_Attributes (TWdg, Id + 4);
      Emit_GtkSignal (TWdg, Id + 4);
      Emit_End_Object_Packing_Child (TWdg, Id,
                                     Packing => True,
                                     XY      => XY,
                                     Homog   => Homog);
   exception
      when others =>
         TIO.Put_Line ("Emit GtkRadioButton: " & TWdg.Name.all);
         raise;
   end Emit_GtkRadioButton;

   -------------------------
   -- Emit_GtkCheckButton --
   -------------------------

   procedure Emit_GtkCheckButton (TWdg : Widget_Pointer; Id : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id, "GtkCheckButton", TWdg.Name.all);
      Emit_Label (TWdg, Id + 4, UnderLine => False);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Focus (TWdg, Id + 4);
      Emit_Property (Id + 4, "receives-default", True);
      Emit_Button_Image (TWdg, Id + 4, Icon_Widget => False);
      Emit_Align (TWdg, Id + 4, Numeric => False);
      Emit_Margin (TWdg, Id + 4);
      Emit_ToolTip (TWdg, Id + 4);
      Emit_CheckAlign (TWdg, Id + 4);
      Emit_Property (Id + 4, "draw-indicator", True);
      Emit_Attributes (TWdg, Id + 4);
      Emit_GtkSignal (TWdg, Id + 4);
      Emit_End_Object_Packing_Child (TWdg, Id,
                                     Packing => True,
                                     XY => True,
                                     Homog => False);
   exception
      when others =>
         TIO.Put_Line ("Emit GtkCheckButton: " & TWdg.Name.all);
         raise;
   end Emit_GtkCheckButton;

   -----------------------
   -- Emit_GtkStatusBar --
   -----------------------

   procedure Emit_GtkStatusBar (TWdg : Widget_Pointer; Id : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id, "GtkStatusbar", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Focus (TWdg, Id + 4);
      Emit_Align (TWdg, Id + 4, Numeric => False);
      Emit_Margin (TWdg, Id + 4);
      Emit_ToolTip (TWdg, Id + 4);
      Emit_Property (Id + 4, "hexpand", False);
      Emit_Property (Id + 4, "orientation", "vertical");
      Emit_Property (Id + 4, "spacing", 2);
      Emit_Attributes (TWdg, Id + 4);
      Emit_GtkSignal (TWdg, Id + 4);
      Emit_End_Object_Packing_Child (TWdg, Id,
                                     Packing => True,
                                     XY => True,
                                     Homog => False);
   exception
      when others =>
         TIO.Put_Line ("Emit GtkStatusBar: " & TWdg.Name.all);
         raise;
   end Emit_GtkStatusBar;

   -------------------------------
   -- Emit_GtkFileChooserDialog --
   -------------------------------

   procedure Emit_GtkFileChooserButton (TWdg : Widget_Pointer; Id : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id, "GtkFileChooserButton", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Focus (TWdg, Id + 4);
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
      Emit_End_Object_Packing_Child (TWdg, Id,
                                     Packing => True,
                                     XY => True,
                                     Homog => False);
   exception
      when others =>
         TIO.Put_Line ("Emit GtkFileChooserButton: " & TWdg.Name.all);
         raise;
   end Emit_GtkFileChooserButton;

   ----------------------
   -- Emit_GtkCalendar --
   ----------------------

   procedure Emit_GtkCalendar (TWdg : Widget_Pointer; Id : Integer) is
      procedure Emit_WH (Id : Integer; W, H : Integer);
      procedure Emit_Image (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_Button (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_Packing (Id : Integer; Pos : Integer);
      procedure Emit_Entry (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_Box (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_AspectFrame (TWdg : Widget_Pointer; Id : Integer);

      procedure Emit_WH (Id : Integer; W, H : Integer) is
      begin
         Emit_Line (Sp (Id) & "<property name=""width-request"">" &
                      Img (W) & "</property>");
         Emit_Line (Sp (Id) & "<property name=""height-request"">" &
                      Img (H) & "</property>");
      end Emit_WH;

      procedure Emit_Packing (Id : Integer; Pos : Integer) is
      begin
         Emit_Line (Sp (Id) & "<packing>");
         Emit_Line (Sp (Id + 2) & "<property name=""expand"">"
                    & "False</property>");
         Emit_Line (Sp (Id + 2) & "<property name=""fill"">True</property>");
         Emit_Line (Sp (Id + 2) & "<property name=""position"">"
                      & Img (Pos) & "</property>");
         Emit_Line (Sp (Id) & "</packing>");
      end Emit_Packing;

      procedure Emit_Image (TWdg : Widget_Pointer; Id : Integer) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id, "GtkImage", "Img_" & TWdg.Name.all);
         Emit_Name (TWdg, Id + 4);
         Emit_Visible_And_Focus (TWdg, Id + 4);
         Emit_Line (Sp (Id + 4) & "<property name=""icon-name"">"
                    & "object-flip-vertical-symbolic</property>");
         Emit_End_Object_Packing_Child (TWdg, Id,
                                        Packing => False,
                                        XY => True,
                                        Homog => False);
      end Emit_Image;

      procedure Emit_Button (TWdg : Widget_Pointer; Id : Integer) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id, "GtkButton", "Button_" & TWdg.Name.all);
         Emit_Name (TWdg, Id + 4);
         Emit_Visible_And_Focus (TWdg, Id + 4);
         Emit_ToolTip (TWdg, Id + 4);
         Emit_Line (Sp (Id + 4) & "<signal name=""clicked"" handler="""
                    & "on_button_clicked_" & TWdg.Name.all
                    & """ swapped=""no""/>");
         Emit_Image (TWdg, Id + 4);
         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Packing (Id + 2, 1);
         Emit_Line (Sp (Id) & "</child>");
      end Emit_Button;

      procedure Emit_Entry (TWdg : Widget_Pointer; Id : Integer) is
         S : constant Integer := Integer'Min (TWdg.Size.Horiz, TWdg.Size.Vert);
         use GNAT.Calendar.Time_IO;
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkEntry", "Entry_" & TWdg.Name.all);
         Emit_Name (TWdg, Id + 4);
         if TWdg.ShowUpDown then
            Emit_WH (Id + 4, TWdg.Size.Horiz - S, TWdg.Size.Vert);
         else
            Emit_WH (Id + 4, TWdg.Size.Horiz, TWdg.Size.Vert);
         end if;
         Emit_Visible_And_Focus (TWdg, Id + 4, Focus => True);
         Emit_ToolTip (TWdg, Id + 4);
         Emit_Line (Sp (Id + 4) & "<property name=""buffer"">"
                    & TWdg.Text_Buffer.Name.all
                    & "</property>");
         Emit_Line (Sp (Id + 4) & "<property name=""max-length"">"
                    & "10" & "</property>");
         Emit_Line (Sp (Id + 4) & "<property name=""text"" "
                    & "translatable=""yes"">"
                    & Image (TWdg.Start_Date, "%d/%m/%Y")
                    & "</property>");
         Emit_Line (Sp (Id + 4) & "<property name=""xalign"">0.5</property>");
         Emit_Line (Sp (Id + 4) & "<signal name=""delete-text"" handler="""
                    & "on_entry_delete_text_" & TWdg.Name.all
                    & """ swapped=""no""/>");
         Emit_Line (Sp (Id + 4) & "<signal name=""insert-text"" handler="""
                    & "on_entry_insert_text_" & TWdg.Name.all
                    & """ swapped=""no""/>");
         Emit_Attributes (TWdg, Id + 4);
         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Packing (Id + 2, 0);
         Emit_Line (Sp (Id) & "</child>");
      end Emit_Entry;

      procedure Emit_Box (TWdg : Widget_Pointer; Id : Integer) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id, "GtkBox", "Box_" & TWdg.Name.all);
         Emit_Name (TWdg, Id + 4);
         Emit_Visible_And_Focus (TWdg, Id + 4);
         Emit_Entry (TWdg, Id + 4);
         if TWdg.ShowUpDown then
            Emit_Button (TWdg, Id + 4);
         end if;
         Emit_End_Object_Packing_Child (TWdg, Id,
                                        Packing => False,
                                        XY => True,
                                        Homog => False);
      end Emit_Box;

      procedure Emit_AspectFrame (TWdg : Widget_Pointer; Id : Integer) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id, "GtkAspectFrame",
                      "AspectFrame_" & TWdg.Name.all);
         Emit_Name (TWdg, Id + 4);
         Emit_WH_Request (TWdg, Id + 4);
         Emit_Visible_And_Focus (TWdg, Id + 4);
         Emit_Line (Sp (Id + 4) & "<property name=""label-xalign"">0"
                    & "</property>");
         Emit_Line (Sp (Id + 4) & "<property name=""shadow-type"">in"
                    & "</property>");

         Emit_Box (TWdg, Id + 4);

         Emit_End_Object_Packing_Child (TWdg, Id,
                                        Packing => True,
                                        XY => True,
                                        Homog => False);
      end Emit_AspectFrame;
   begin
      Emit_AspectFrame (TWdg, Id);
   exception
      when others =>
         TIO.Put_Line ("Emit GtkAspectFrame: " & TWdg.Name.all);
         raise;
   end Emit_GtkCalendar;

   ---------------------
   -- Emit_GtkListBox --
   ---------------------
   procedure Emit_GtkListBox (TWdg : Widget_Pointer; Id : Integer) is
      procedure Emit_Internal_GtkSelection (Id : Integer);
      procedure Emit_Internal_GtkSelection (Id : Integer) is
      begin
         Emit_Line (Sp (Id) & "<child internal-child=""selection"">");
         Emit_Object (TWdg, Id, "GtkTreeSelection",
                      "Selection_" & TWdg.Name.all);
         if TWdg.MultiSelect then
            Emit_Line (Sp (Id + 4) & "<property name=""mode"">multiple"
                       & "</property>");
         end if;
         Emit_End_Object_Packing_Child (TWdg, Id,
                                        Packing => False,
                                        XY => True,
                                        Homog => False);
      end Emit_Internal_GtkSelection;
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id, "GtkTreeView", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Focus (TWdg, Id + 4);
      Emit_ToolTip (TWdg, Id + 4);
      Emit_Margin (TWdg, Id + 4);
      Emit_GtkSignal (TWdg, Id + 4);
      Emit_Internal_GtkSelection (Id + 4);
      Emit_End_Object_Packing_Child (TWdg, Id,
                                     Packing => True,
                                     XY => True,
                                     Homog => False);
   end Emit_GtkListBox;

   -------------------
   -- Emit_GtkFrame --
   -------------------

   procedure Emit_GtkFrame (TWdg : Widget_Pointer; Id : Integer) is
      procedure Emit_Alignment (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_Alignment (TWdg : Widget_Pointer; Id : Integer) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id, "GtkAlignment", "Alignment_" & TWdg.Name.all);
         Emit_Name (TWdg, Id + 4);
         Emit_WH_Request (TWdg, Id + 4);
         Emit_Visible_And_Focus (TWdg, Id + 4);
         Emit_Line (Sp (Id + 4)
                    & "<property name=""left-padding"">12</property>");
      end Emit_Alignment;

      procedure Emit_Fixed (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_Fixed (TWdg : Widget_Pointer; Id : Integer) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id, "GtkFixed", "Fixed_" & TWdg.Name.all);
         Emit_Visible_And_Focus (TWdg, Id + 4);
      end Emit_Fixed;

      procedure Emit_Frame_Label (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_Frame_Label (TWdg : Widget_Pointer; Id : Integer) is
      begin
         Emit_Child (TWdg, Id, True);
         Emit_Object (TWdg, Id, "GtkLabel", "label_" & TWdg.Name.all);
         Emit_Visible_And_Focus (TWdg, Id + 4);
         Emit_Label (TWdg, Id + 4, UnderLine => False);
         Emit_Attributes (TWdg, Id + 4);
         Emit_End_Object_Packing_Child (TWdg, Id,
                                        Packing => False,
                                        XY => True,
                                        Homog => False);
      end Emit_Frame_Label;

      Temp : Widget_Pointer;
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id, "GtkFrame", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Focus (TWdg, Id + 4);
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
      Emit_Fixed (TWdg, Id + 8);
      Temp := TWdg.Child_List;
      while Temp /= null loop
         case Temp.Widget_Type is
            when GtkLabel =>
               Emit_GtkLabel (Temp, Id + 12, Packing => True);
            when GtkImage =>
               Emit_GtkImage (Temp, Id + 12);
            when GtkEntry =>
               Emit_GtkEntry (Temp, Id + 12);
            when GtkComboBox =>
               Emit_GtkComboBox (Temp, Id + 12);
            when GtkButton =>
               Emit_GtkButton (Temp, Id + 12, "GtkButton",
                               Underline => Temp.Underline,
                               XY => True, Homog => False);
            when GtkRadioButton =>
               Emit_GtkRadioButton (Temp, Id + 12, "GtkRadioButton",
                                    Underline => Temp.Underline,
                                    XY => True, Homog => False);
            when GtkCheckButton =>
               Emit_GtkCheckButton (Temp, Id + 12);
            when GtkSpinButton =>
               Emit_GtkSpinButton (Temp, Id + 12);
            when GtkColorButton =>
               Emit_GtkColorButton (Temp, Id + 12);
            when GtkToolBar =>
               Emit_GtkToolBar (Temp, Id + 12);
            when GtkListBox =>
               Emit_GtkListBox (Temp, Id + 12);
            when GtkCalendar =>
               Emit_GtkCalendar (Temp, Id + 12);
            when GtkFrame =>
               Emit_GtkFrame (Temp, Id + 12);
            when GtkStatusBar =>
               Emit_GtkStatusBar (Temp, Id + 12);
            when ToolStripStatusLabel =>
               Emit_GtkLabel (Temp, Id + 12, Packing => True);
            when GtkFileChooserButton =>
               Emit_GtkFileChooserButton (Temp, Id + 12);
            when GtkToggleButton =>
               Emit_GtkToggleButton (Temp, Id + 12, "GtkToggleButton",
                                     Underline => Temp.Underline,
                                     XY => True, Homog => False);
            when None                 => null;
            when PrintDocument        => null;
            when PrintDialog          => null;
            when Chart                => null;
            when FolderBrowserDialog  => null;
            when GtkToolTip           => null;
            when GtkSeparatorToolItem => null;
            when GtkMenu              => null;
         end case;
         Temp := Temp.Next;
      end loop;
      --  gtkfixed
      Emit_End_Object_Packing_Child (TWdg, Id + 8,
                                     Packing => False,
                                     XY => True,
                                     Homog => False);
      --  gtkalignment
      Emit_End_Object_Packing_Child (TWdg, Id + 4,
                                     Packing => False,
                                     XY => True,
                                     Homog => False);
      Emit_Frame_Label (TWdg, Id + 4);
      --  gtkframe
      Emit_End_Object_Packing_Child (TWdg, Id,
                                     Packing => True,
                                     XY => True,
                                     Homog => False);
   exception
      when others =>
         TIO.Put_Line ("Emit GtkFrame: " & TWdg.Name.all);
         raise;
   end Emit_GtkFrame;

   -----------------------
   -- Emit_GtkListStore --
   -----------------------

   procedure Emit_GtkListStore (TWin : Window_Pointer; Id : Integer) is
   begin
      Emit_Object (null, Id - 2, "GtkListStore", TWin.Name.all);
      Emit_Line (Sp (Id + 2) & "<columns>");
      Emit_Line (Sp (Id + 2) & "<column type=""gchar""/>");
      Emit_Line (Sp (Id + 2) & "</columns>");
      Emit_Line (Sp (Id) & "</object>");
   end Emit_GtkListStore;

   -------------------------
   -- Emit_GtkEntryBuffer --
   -------------------------

   procedure Emit_GtkEntryBuffer (TWin : Window_Pointer; Id : Integer) is
      use GNAT.Calendar.Time_IO;
      TWdg : constant Widget_Pointer := TWin.Associated_Widget;
   begin
      Emit_Object (null, Id - 2, "GtkEntryBuffer", TWin.Name.all);
      Emit_Property (Id + 2, "text",
                     Image (TWdg.MinDate, ISO_Date)
                     & ASCII.CR
                     & Image (TWdg.MaxDate, ISO_Date));
      Emit_Line (Sp (Id) & "</object>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkEntryBuffer: " & TWin.Name.all);
         raise;
   end Emit_GtkEntryBuffer;

   ------------------------
   -- Emit_GtkFileFilter --
   ------------------------

   procedure Emit_GtkFileFilter (TWin : Window_Pointer; Id : Integer) is
      procedure Emit_Pattern (Pattern : String);
      procedure Emit_Pattern (Pattern : String) is
      begin
         Emit_Line (Sp (Id + 4) & "<pattern>"
                    & Ada.Strings.Fixed.Trim (Pattern, Ada.Strings.Both)
                    & "</pattern>");
      end Emit_Pattern;

   begin
      Emit_Object (null, Id - 2, "GtkFileFilter", TWin.Name.all);
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

   -------------------------------
   -- Emit_GtkFileChooserDialog --
   -------------------------------

   procedure Emit_GtkFileChooserDialog (TWin : Window_Pointer; Id : Integer) is
   begin
      Emit_Object (null, Id - 2, "GtkFileChooserDialog", TWin.Name.all);
      Emit_Line (Sp (Id + 2) & "<property name=""Can-Focus"">False"
                 & "</property>");
      Emit_Line (Sp (Id + 2) & "<property name=""Title"" translatable=""Yes"">"
                 & TWin.Name.all
                 & "_title</property>");
      Emit_Line (Sp (Id + 2) & "<property name=""type-Hint"">dialog"
                 & "</property>");
      Emit_Line (Sp (Id + 2) & "<property name=""transient-for"">"
                 & TWin.Transient_For.Name.all & "</property>");
      Emit_Line (Sp (Id + 2) & "<property name=""attached-To"">"
                 & TWin.Attached_To.Name.all & "</property>");
      Emit_Line (Sp (Id + 2) & "<property name=""filter"">"
                 & TWin.FilterName.all & "</property>");
      Emit_Line (Sp (Id + 2) & "<child internal-child=""Vbox"">");
      Emit_Line (Sp (Id + 6) & "<object class=""GtkBox"">");
      Emit_Line (Sp (Id + 8) & "<property name=""can-focus"">False"
                 & "</property>");
      Emit_Line (Sp (Id + 8) & "<property name=""orientation"">vertical"
                 & "</property>");
      Emit_Line (Sp (Id + 8) & "<property name=""spacing"">2</property>");
      Emit_Line (Sp (Id + 8) & "<child internal-child=""action_area"">");
      Emit_Line (Sp (Id + 10) & "<object class=""GtkButtonBox"">");
      Emit_Line (Sp (Id + 12) & "<property name=""can-focus"">False"
                 & "</property>");
      Emit_Line (Sp (Id + 12) & "<property name=""layout-style"">end"
                 & "</property>");
      Emit_Line (Sp (Id + 12) & "<child>");
      Emit_Line (Sp (Id + 14) & "<placeholder/>");
      Emit_Line (Sp (Id + 12) & "</child>");
      Emit_Line (Sp (Id + 12) & "<child>");
      Emit_Line (Sp (Id + 14) & "<placeholder/>");
      Emit_Line (Sp (Id + 12) & "</child>");
      Emit_Line (Sp (Id + 8) & "</object>");
      Emit_Line (Sp (Id + 8) & "<packing>");
      Emit_Line (Sp (Id + 10) & "<property name=""expand"">False</property>");
      Emit_Line (Sp (Id + 10) & "<property name=""fill"">False</property>");
      Emit_Line (Sp (Id + 10) & "<property name=""position"">0</property>");
      Emit_Line (Sp (Id + 8) & " </packing>");
      Emit_Line (Sp (Id + 6) & "</child>");
      Emit_Line (Sp (Id + 6) & "<child>");
      Emit_Line (Sp (Id + 8) & "<placeholder/>");
      Emit_Line (Sp (Id + 6) & "</child>");
      Emit_Line (Sp (Id + 4) & "</object>");
      Emit_Line (Sp (Id + 2) & "</child>");

      Emit_Line (Sp (Id) & "</object>");
   end Emit_GtkFileChooserDialog;

   -----------------------------
   -- Emit_GtkFileColorButton --
   -----------------------------

   procedure Emit_GtkColorButton (TWdg : Widget_Pointer; Id : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id, "GtkColorButton", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Focus (TWdg, Id + 4, True);
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
      Emit_End_Object_Packing_Child (TWdg, Id,
                                     Packing => True,
                                     XY      => True,
                                     Homog   => False);
   exception
      when others =>
         TIO.Put_Line ("Emit GtkColorButon: " & TWdg.Name.all);
         raise;
   end Emit_GtkColorButton;

   ---------------------
   -- Emit_GtkToolBar --
   ---------------------

   procedure Emit_GtkToolBar (TWdg : Widget_Pointer; Id : Integer) is
      procedure Emit_SeparatorToolItem (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_SeparatorToolItem (TWdg : Widget_Pointer; Id : Integer) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id, "GtkSeparatorToolItem", TWdg.Name.all);
         Emit_Name (TWdg, Id + 4);
         Emit_Visible_And_Focus (TWdg, Id + 4, True);
         Emit_ToolTip (TWdg, Id + 4);
         Emit_End_Object_Packing_Child (TWdg, Id,
                                        Packing => True,
                                        XY      => False,
                                        Homog   => False);
      end Emit_SeparatorToolItem;
      procedure Emit_MenuToolButton (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_MenuToolButton (TWdg : Widget_Pointer; Id : Integer) is
      begin
         null;
      end Emit_MenuToolButton;
      procedure Emit_ToolItem (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_ToolItem (TWdg : Widget_Pointer; Id : Integer) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id, "GtkToolItem", "toolitem_" & TWdg.Name.all);
         Emit_Visible_And_Focus (TWdg, Id + 4, Focus => False);
         Emit_ToolTip (TWdg, Id + 4);
         if TWdg.Widget_Type = GtkLabel then
            Emit_GtkLabel (TWdg, Id + 4, Packing => False);
         elsif TWdg.Widget_Type = GtkComboBox then
            Emit_GtkComboBox (TWdg, Id + 4, Packing => False);
         end if;
         Emit_End_Object_Packing_Child (TWdg, Id,
                                        Packing => True,
                                        XY      => False,
                                        Homog   => False);
      end Emit_ToolItem;

      Temp : Widget_Pointer;
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id, "GtkToolbar", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Focus (TWdg, Id + 4, Focus => False);
      Emit_Align (TWdg, Id + 4, Numeric => False);
      Emit_Margin (TWdg, Id + 4);
      Emit_ToolTip (TWdg, Id + 4);
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

      Temp := TWdg.Child_List;
      while Temp /= null loop
         case Temp.Widget_Type is
            when GtkButton =>
               Emit_GtkButton (Temp, Id + 4, "GtkToolButton",
                               Underline => Temp.Underline,
                               XY        => False,
                               Homog     => False);
            when GtkRadioButton =>
               Emit_GtkRadioButton (Temp, Id + 4, "GtkToolButton",
                                    Underline => Temp.Underline,
                                    XY        => False,
                                    Homog     => False);
            when GtkToggleButton =>
               Emit_GtkToggleButton (Temp, Id + 4, "GtkToggleToolButton",
                                     Underline => Temp.Underline,
                                     XY        => False,
                                     Homog     => False);
            when GtkSeparatorToolItem =>
               Emit_SeparatorToolItem (Temp, Id + 4);
            when GtkMenu =>
               Emit_MenuToolButton (Temp, Id + 4);
            when GtkLabel =>
               Emit_ToolItem (Temp, Id + 4);
            when GtkComboBox =>
               Emit_ToolItem (Temp, Id + 4);

            when GtkImage             => null;
            when GtkEntry             => null;
            when GtkCheckButton       => null;
            when GtkSpinButton        => null;
            when GtkColorButton       => null;
            when GtkToolBar           => null;
            when GtkListBox           => null;
            when GtkCalendar          => null;
            when GtkFrame             => null;
            when GtkStatusBar         => null;
            when ToolStripStatusLabel => null;
            when GtkFileChooserButton => null;
            when None                 => null;
            when PrintDocument        => null;
            when PrintDialog          => null;
            when Chart                => null;
            when FolderBrowserDialog  => null;
            when GtkToolTip           => null;
         end case;
         Temp := Temp.Next;
      end loop;

      Emit_End_Object_Packing_Child (TWdg, Id,
                                     Packing => True,
                                     XY      => True,
                                     Homog   => False);
   end Emit_GtkToolBar;

   --------------------
   -- Emit_GtkWindow --
   --------------------

   procedure Emit_GtkWindow (TWin : Window_Pointer; Id : Integer) is
      TWdg : Widget_Pointer;

      procedure Emit_Fixed (TWin : Window_Pointer; Id : Integer);
      procedure Emit_Fixed (TWin : Window_Pointer; Id : Integer) is
      begin
         Emit_Line (Sp (Id) & "<child>");
         if TWin.Name /= null and then TWin.Name.all /= "" then
            Emit_Line (Sp (Id + 2) & "<object class=""GtkFixed"" "
                       & "id=""GtkFixed_"
                       & TWin.Name.all & """>");
            Emit_Name (TWin, Id + 4);
         else
            Emit_Line (Sp (Id + 4) & "<object class=""GtkFixed"">");
         end if;
         Emit_Property (Id + 4, "visible", True);
         Emit_Property (Id + 4, "can-focus", False);
         Emit_Margin (TWin, Id + 4);

         TWdg := TWin.Widget_List;
         while TWdg /= null loop
            case TWdg.Widget_Type is
               when GtkLabel =>
                  Emit_GtkLabel (TWdg, Id + 4, Packing => True);
               when GtkImage =>
                  Emit_GtkImage (TWdg, Id + 4);
               when GtkEntry =>
                  Emit_GtkEntry (TWdg, Id + 4);
               when GtkComboBox =>
                  Emit_GtkComboBox (TWdg, Id + 4);
               when GtkButton =>
                  Emit_GtkButton (TWdg, Id + 4, "GtkButton",
                                  Underline => TWdg.Underline,
                                  XY        => True,
                                  Homog     => False);
               when GtkRadioButton =>
                  Emit_GtkRadioButton (TWdg, Id + 4, "GtkRadioButton",
                                       Underline => TWdg.Underline,
                                       XY => True, Homog => False);
               when GtkCheckButton =>
                  Emit_GtkCheckButton (TWdg, Id + 4);
               when GtkSpinButton =>
                  Emit_GtkSpinButton (TWdg, Id + 4);
               when GtkColorButton =>
                  Emit_GtkColorButton (TWdg, Id + 4);
               when GtkToolBar =>
                  Emit_GtkToolBar (TWdg, Id + 4);
               when GtkListBox =>
                  Emit_GtkListBox (TWdg, Id + 4);
               when GtkCalendar =>
                  Emit_GtkCalendar (TWdg, Id + 4);
               when GtkFrame =>
                  Emit_GtkFrame (TWdg, Id + 4);
               when GtkStatusBar =>
                  Emit_GtkStatusBar (TWdg, Id + 4);
               when ToolStripStatusLabel =>
                  Emit_GtkLabel (TWdg, Id + 4, Packing => True);
               when GtkFileChooserButton =>
                  Emit_GtkFileChooserButton (TWdg, Id + 4);
               when GtkToggleButton =>
                  Emit_GtkToggleButton (TWdg, Id + 4, "GtkToggleButton",
                                        Underline => TWdg.Underline,
                                        XY => True, Homog => False);

               when None                 => null;
               when PrintDocument        => null;
               when PrintDialog          => null;
               when Chart                => null;
               when FolderBrowserDialog  => null;
               when GtkToolTip           => null;
               when GtkSeparatorToolItem => null;
               when GtkMenu              => null;
            end case;
            TWdg := TWdg.Next;
         end loop;
         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Line (Sp (Id) & "</child>");
      exception
         when others =>
            if TWin.Name /= null and then TWin.Name.all /= "" then
               TIO.Put_Line ("Emit_Fixed: " & TWin.Name.all);
            else
               TIO.Put_Line ("Emit_Fixed");
            end if;
            TIO.Put_Line ("Emit Widget: " & TWdg.Name.all);
            raise;
      end Emit_Fixed;
   begin
      if TWin.Name /= null and then TWin.Name.all /= "" then
         Emit_Line (Sp (Id) & "<object class=""GtkWindow"""
                    & " id=""" & TWin.Name.all & """>");
      else
         Emit_Line (Sp (Id) & "<object class=""GtkWindow"">");
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
      if TWin.Name /= null and then TWin.Name.all /= "" then
         Emit_Name (TWin, Id + 2);
      end if;
      Emit_Property (Id + 2, "resizable", False);
      if TWin.Client_Size.Horiz /= -1 then
         Emit_Property (Id + 2, "default-width", TWin.Client_Size.Horiz);
      end if;
      if TWin.Client_Size.Vert /= -1 then
         Emit_Property (Id + 2, "default-height", TWin.Client_Size.Vert);
      end if;
      Emit_Property (Id + 2, "type-hint", "Utility");
      Emit_GtkSignal (TWin, Id + 2);

      Emit_Fixed (TWin, Id + 2);
      Emit_Line (Sp (Id) & "</object>");
   end Emit_GtkWindow;

end W2gtk_Emit;
