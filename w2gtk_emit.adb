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
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with GNAT.Strings;          use GNAT.Strings;
with GNAT.Calendar.Time_IO;
with Symbol_Tables;         use Symbol_Tables;

package body W2gtk_Emit is

   -------------------
   --  widget specs --
   -------------------

   procedure Emit_GtkBox (TWdg : Widget_Pointer;
                          Id   : Integer;
                          Pos  : Integer);
   -----------
   procedure Emit_GtkToolBar (TWdg : Widget_Pointer;
                              Id   : Integer;
                              Pos  : Integer);
   -----------
   procedure Emit_GtkGridViewCheckBoxColumn (TWdg : Widget_Pointer;
                                             Id   : Integer;
                                             Num  : Integer);
   procedure Emit_GtkGridViewTextBoxColumn (TWdg : Widget_Pointer;
                                            Id   : Integer;
                                            Num  : Integer);
   procedure Emit_GtkDataGridView (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_GtkTreeGridView (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_GtkGridView (TWdg : Widget_Pointer; Id : Integer);
   -----------
   procedure Emit_GtkNoteBook (TWdg : Widget_Pointer;
                               Id   : Integer;
                               Pos  : Integer);
   procedure Emit_GtkNotebookTab (TWdg : Widget_Pointer; Id : Integer);
   -----------
   procedure Emit_GtkMenuBar (TWdg : Widget_Pointer;
                              Id   : Integer;
                              Pos  : Integer);
   procedure Emit_GtkMenuNormalItem (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_GtkMenuImageItem (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_GtkSepMenuItem (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_GtkMenuItem (TWdg : Widget_Pointer; Id : Integer);
   -----------
   procedure Emit_GtkLabel (TWdg    : Widget_Pointer;
                            Id      : Integer;
                            Packing : Boolean);

   procedure Emit_DatePicker (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_TimePicker (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_GtkImage (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_GtkFrame (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_GtkListBox (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_GtkEntry (TWdg : Widget_Pointer;
                            Id   : Integer;
                            Activate_Default : Boolean);
   procedure Emit_GtkSpinButton (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_GtkComboTextBox (TWdg    : Widget_Pointer;
                                   Id      : Integer;
                                   Packing : Boolean;
                                   Activate_Default : Boolean);
   -----------
   procedure Emit_GtkButton (TWdg        : Widget_Pointer;
                             Id          : Integer;
                             Object      : String;
                             Position    : Integer;
                             Has_Default : Boolean;
                             XY          : Boolean;
                             Homog       : Boolean);
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
   -----------
   procedure Emit_GtkStatusBar (TWdg : Widget_Pointer;
                                Id   : Integer;
                                Pos  : Integer);
   procedure Emit_GtkFileChooserButton (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_GtkDialog_Body (TWin : Window_Pointer;
                                  Id   : Integer;
                                  Activate_Default : Boolean);
   procedure Emit_Main_GtkWindow_Body (TWin : Window_Pointer; Id : Integer);
   procedure Emit_GtkDialog (TWin : Window_Pointer; Id : Integer);
   procedure Emit_Main_GtkWindow (TWin : Window_Pointer; Id : Integer);

   -----------------
   --  tool specs --
   -----------------
   package Emit_Tools is
      procedure Emit_Property (Id     : Integer;
                               PName  : String;
                               PValue : String;
                               Translatable : Boolean := False);
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
      procedure Emit_Packing (Id         : Integer;
                              Position   : Integer;
                              Expand     : Boolean;
                              Fill       : Boolean;
                              Padding    : Integer;
                              Pack_Start : Boolean);
      procedure Emit_Packing_Child (TWdg    : Widget_Pointer;
                                    Id      : Integer;
                                    Packing : Boolean;
                                    XY      : Boolean;
                                    Homog   : Boolean);
      procedure Emit_GtkSignal (TWin : Window_Pointer; Id : Integer);
      procedure Emit_GtkSignal (TWdg     : Widget_Pointer;
                                Id       : Integer;
                                Except   : String := "";
                                Only_For : String := "");
      procedure Emit_Attributes (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_Label (TWdg       : Widget_Pointer;
                            Id         : Integer;
                            UnderLine  : Boolean;
                            Selectable : Boolean);
      procedure Emit_Line (Text : String);
      procedure Emit_Margin (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_Margin (TWin : Window_Pointer; Id : Integer);
      procedure Emit_Name (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_Name (Name : String; Id : Integer);
      procedure Emit_Name (TWin : Window_Pointer; Id : Integer);
      procedure Emit_Object (TWdg   : Widget_Pointer;
                             Id     : Integer; --  identation
                             Wdg    : String;  --  gtk widget
                             WId    : String;  --  Widget_Id
                             Finish : Boolean := False);
      procedure Emit_Button_Image (TWdg        : Widget_Pointer;
                                   Id          : Integer;
                                   Icon_Widget : Boolean);
      procedure Emit_Password (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_ToolTip (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_Visible_And_Focus (TWdg  : Widget_Pointer;
                                        Id    : Integer;
                                        Focus : Boolean);
      procedure Emit_Visible_And_Focus (TWin  : Window_Pointer;
                                        Id    : Integer;
                                        Focus : Boolean);
      procedure Emit_WH_Request (TWdg : Widget_Pointer;
                                 Id   : Integer);
   end Emit_Tools;

   ------------------
   --  tool bodies --
   ------------------
   package body Emit_Tools is
      procedure Emit_Property (Id     : Integer;
                               PName  : String;
                               PValue : String;
                               Translatable : Boolean := False) is
      begin
         if Translatable then
            Emit_Line (Sp (Id) & "<property name="
                       & """" & PName & """"
                       & " translatable=""yes"">"
                       & PValue
                       & "</property>");
         else
            Emit_Line (Sp (Id) & "<property name="
                       & """" & PName & """>"
                       & PValue
                       & "</property>");
         end if;
      end Emit_Property;

      procedure Emit_Property (Id     : Integer;
                               PName  : String;
                               PValue : Boolean) is
      begin
         if PValue then
            Emit_Property (Id, PName, "True");
         else
            Emit_Property (Id, PName, "False");
         end if;
      end Emit_Property;

      procedure Emit_Property (Id     : Integer;
                               PName  : String;
                               PValue : Integer) is
      begin
         Emit_Property (Id, PName, Img (PValue));
      end Emit_Property;

      procedure Emit_Property (Id     : Integer;
                               PName  : String;
                               PValue : Float) is
      begin
         Emit_Property (Id, PName, Img (PValue));
      end Emit_Property;

      procedure Emit_Line (Text : String) is
      begin
         TIO.Put_Line (GFile, Text);
      end Emit_Line;

      procedure Emit_GtkSignal (TWin : Window_Pointer; Id : Integer) is
         TS : Signal_Pointer := TWin.Signal_List;
      begin
         while TS /= null loop
            if TS.Glade then
               if TS.After then
                  Emit_Line (Sp (Id) & "<signal name="""
                             & Convert_Signal_To_Gtk (TWin, TS)
                             & """ handler="""
                             & TS.Handler.all
                             & " after=""yes"""
                             & """ swapped=""no""/>");
               else
                  Emit_Line (Sp (Id) & "<signal name="""
                             & Convert_Signal_To_Gtk (TWin, TS)
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
                  Emit := Except /= TS.Name.all;
               elsif Only_For /= "" then
                  Emit := Only_For = TS.Name.all;
               else
                  Emit := False;
               end if;
               if Emit then
                  if TS.After then
                     Emit_Line (Sp (Id) & "<signal name="""
                                & Convert_Signal_To_Gtk (TWdg, TS)
                                & """ handler=""" & TS.Handler.all & """"
                                & " object=""" & TWdg.Name.all & """"
                                & " after=""yes"""
                                & " swapped=""no""/>");
                  else
                     Emit_Line (Sp (Id) & "<signal name="""
                                & Convert_Signal_To_Gtk (TWdg, TS)
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

      procedure Emit_Margin (TWdg : Widget_Pointer; Id : Integer) is
      begin
         if TWdg.Margins (2) /= -1 then
            Emit_Property (Id, "margin-start", TWdg.Margins (1));
         end if;
         if TWdg.Margins (4) /= -1 then
            Emit_Property (Id, "margin-end", TWdg.Margins (2));
         end if;
         if TWdg.Margins (1) /= -1 then
            Emit_Property (Id, "margin-top", TWdg.Margins (3));
         end if;
         if TWdg.Margins (3) /= -1 then
            Emit_Property (Id, "margin-bottom", TWdg.Margins (4));
         end if;
      end Emit_Margin;
      procedure Emit_Margin (TWin : Window_Pointer; Id : Integer) is
      begin
         if TWin.Margins (2) /= -1 then
            Emit_Property (Id, "margin-start", TWin.Margins (1));
         end if;
         if TWin.Margins (4) /= -1 then
            Emit_Property (Id, "margin-end", TWin.Margins (2));
         end if;
         if TWin.Margins (1) /= -1 then
            Emit_Property (Id, "margin-top", TWin.Margins (3));
         end if;
         if TWin.Margins (3) /= -1 then
            Emit_Property (Id, "margin-bottom", TWin.Margins (4));
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
      end Emit_Object;

      procedure Emit_Name (TWin : Window_Pointer; Id : Integer) is
      begin
         Emit_Property (Id, "name", TWin.Name.all);
      end Emit_Name;

      procedure Emit_Name (Name : String; Id : Integer) is
      begin
         Emit_Property (Id, "name", Name);
      end Emit_Name;

      procedure Emit_Name (TWdg : Widget_Pointer; Id : Integer) is
      begin
         Emit_Property (Id, "name", TWdg.Name.all);
      end Emit_Name;

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

      procedure Emit_Visible_And_Focus (TWdg  : Widget_Pointer;
                                        Id    : Integer;
                                        Focus : Boolean) is
      begin
         Emit_Property (Id, "visible", TWdg.Visible);
         if not TWdg.Enabled then
            Emit_Property (Id, "sensitive", False);
         end if;
         Emit_Property (Id, "can-focus", Focus);
      end Emit_Visible_And_Focus;

      procedure Emit_Visible_And_Focus (TWin  : Window_Pointer;
                                        Id    : Integer;
                                        Focus : Boolean) is
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
         Emit_Property (Id + 2, "position", Position);
         if Padding > 0 then
            Emit_Property (Id + 2, "padding", Padding);
         end if;
         if not Pack_Start then
            Emit_Property (Id + 2, "pack-type", "end");
         end if;
         Emit_Line (Sp (Id) & "</packing>");
      end Emit_Packing;

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
         if Packing then
            Emit_Packing (TWdg, Id + 2, XY, Homog);
         end if;
      end Emit_Packing_Child;

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
               Emit_Property (Id, "tooltip-text", TWdg.ToolTip.all, True);
            when GtkSpinButton =>
               Emit_Property (Id, "primary-icon-tooltip-text",
                              TWdg.ToolTip.all, True);
            when others => null;
            end case;
         end if;
      end Emit_ToolTip;

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
                       & TWdg.FgColor.all
                       & """/>");
         end if;

         if not TWdg.UseVisualStyleBackColor then
            if TWdg.BgColor /= null and then TWdg.BgColor.all /= "" then
               Emit_Line (Sp (Id + 2)
                          & "<attribute name=""background"" value="""
                          & TWdg.BgColor.all
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
                       & TWdg.UlColor.all
                       & """/>");
         end if;

         Emit_Line (Sp (Id) & "</attributes>");
      end Emit_Attributes;
   end Emit_Tools;
   use Emit_Tools;

   -------------------------------------------------------------
   --                       widget bodies                     --
   -------------------------------------------------------------

   procedure Emit_GtkHeader (TWin : Window_Pointer; Id : Integer) is
      pragma Unreferenced (TWin);
   begin
      Emit_Line ("<?xml version=""1.0"" encoding=""UTF-8""?>");
      Emit_Line ("<!-- Generated with glade 3.38.2 -->");
      Emit_Line ("<interface>");
      Emit_Line (Sp (Id + 2) & "<requires lib=""gtk+"" version=""3.24""/>");
      Emit_Line (Sp (Id + 2) & "<!-- interface-local-resource-path / -->");
   end Emit_GtkHeader;

   procedure Emit_GtkTrailer (TWin : Window_Pointer; Id : Integer) is
      pragma Unreferenced (TWin, Id);
   begin
      Emit_Line ("</interface>");
   end Emit_GtkTrailer;

   ------------------------------------
   -- Emit_GtkGridViewCheckBoxColumn --
   --------------------.---------------

   procedure Emit_GtkGridViewCheckBoxColumn (TWdg : Widget_Pointer;
                                             Id   : Integer;
                                             Num  : Integer) is
      NCol : Integer;
      Found_Signal_Toggled : Boolean;
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkTreeViewColumn", TWdg.Name.all);
      Emit_Property (Id + 4, "resizable", TWdg.Resizable);
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
      Emit_GtkSignal (TWdg, Id + 4, Except => "Toggled");

      Emit_Child (TWdg, Id + 4, False);
      Found_Signal_Toggled := Signal_Exists (TWdg, "Toggled");
      Emit_Object (TWdg, Id + 6,
                   Wdg    => "GtkCellRendererToggle",
                   WId    => "CRTG_" & TWdg.Name.all,
                   Finish => not Found_Signal_Toggled);
      if Found_Signal_Toggled then
         Emit_GtkSignal (TWdg, Id + 8, Only_For => "Toggled");
      end if;
      if Found_Signal_Toggled then
         Emit_Line (Sp (Id + 6) & "</object>");
      end if;
      Emit_Line (Sp (Id + 6) & "<attributes>");
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
      NCol := TWdg.GParent.Model.Num_Elements - 1;
      if TWdg.GParent.AlternatingRowsDefaultCellStyle in DGVS'Range then
         Emit_Line (Sp (Id + 8) & "<attribute name=""cell-background"">"
                       & Img (NCol) & "</attribute>");
      end if;
      Emit_Line (Sp (Id + 6) & "</attributes>");
      Emit_Line (Sp (Id + 4) & "</child>");

      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Line (Sp (Id) & "</child>");
   end Emit_GtkGridViewCheckBoxColumn;

   -----------------------------------
   -- Emit_GtkGridViewTextBoxColumn --
   -----------------------------------

   procedure Emit_GtkGridViewTextBoxColumn (TWdg : Widget_Pointer;
                                            Id   : Integer;
                                            Num  : Integer) is
      NCol : Integer;
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkTreeViewColumn", TWdg.Name.all);
      Emit_Property (Id + 4, "resizable", TWdg.Resizable);
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
               Emit_Property (Id + 4, "alignment", 0.0);
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
      Emit_Line (Sp (Id + 8) & "<attributes>");
      Emit_Line (Sp (Id + 10) & "<attribute name=""text"">"
                 & Img (Num) & "</attribute>");

      if TWdg.Text_Col_Properties.Fg_Color_Name_Column /= -1 then
         NCol := TWdg.Text_Col_Properties.Fg_Color_Name_Column;
         Emit_Line (Sp (Id + 10) & "<attribute name=""foreground"">"
                    & Img (NCol) & "</attribute>");
      end if;

      NCol := TWdg.GParent.Model.Num_Elements - 1;
      if TWdg.GParent.AlternatingRowsDefaultCellStyle in DGVS'Range then
         Emit_Line (Sp (Id + 10) & "<attribute name=""background"">"
                    & Img (NCol) & "</attribute>");
      end if;
      Emit_Line (Sp (Id + 8) & "</attributes>");
      Emit_Line (Sp (Id + 4) & "</child>");

      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Line (Sp (Id) & "</child>");
   end Emit_GtkGridViewTextBoxColumn;

   --------------------------------------------
   -- Emit_GtkTreeGridView / GtkDataGridView --
   --------------------------------------------

   procedure Emit_GtkDataGridView (TWdg : Widget_Pointer; Id : Integer) is
   begin
      Emit_GtkTreeGridView (TWdg, Id);
   end Emit_GtkDataGridView;

   procedure Emit_GtkTreeGridView (TWdg : Widget_Pointer; Id : Integer) is
   begin
      if TWdg.ScrollBars /= None then
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkScrolledWindow",
                      "GtkScrolledWindow_" & TWdg.Name.all);
         Emit_Name (TWdg, Id + 4);
         Emit_Property (Id + 4, "name", "GtkScrolledWindow_" & TWdg.Name.all);
         Emit_Visible_And_Focus (TWdg, Id + 4, True);
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

         Emit_GtkGridView (TWdg, Id + 4);

         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Line (Sp (Id) & "</child>");

      else
         Emit_GtkGridView (TWdg, Id + 4);
      end if;
   end Emit_GtkTreeGridView;

   --------------------------------------------
   procedure Emit_GtkGridView (TWdg : Widget_Pointer; Id : Integer) is
      Child : Widget_Pointer;
      Num   : Integer;
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkTreeView", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_Visible_And_Focus (TWdg, Id + 4, True);
      Emit_Property (Id + 4, "model", TWdg.Model.Name.all);
      Emit_Property (Id + 4, "headers-visible", TWdg.ColumnHeadersVisible);
      if TWdg.ColumnHeadersVisible then
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
      Emit_Property (Id + 4, "reorderable", TWdg.AllowUserToOrderColumns);
      Emit_Property (Id + 4, "fixed-height-mode", True);
      Emit_Property (Id + 4, "ubuntu-almost-fixed-height-mode", True);
      Emit_Property (Id + 4, "level-indentation", 3);
      Emit_Property (Id + 4, "rubber-banding", True);
      Emit_Property (Id + 4, "enable-grid-lines", "both");
      Emit_Property (Id + 4, "activate-on-single-click", True);
      Emit_GtkSignal (TWdg, Id + 4);
      Emit_Line (Sp (Id + 4) & "<child internal-child=""selection"">");
      Emit_Object (TWdg, Id + 6, "GtkTreeSelection",
                   TWdg.Name.all & "_Selection");
      if TWdg.RowMultiSelect then
         Emit_Property (Id + 8, "mode", "multiple");
      else
         Emit_Property (Id + 8, "mode", "browse");
      end if;
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

   ----------------------
   -- Emit_GtkNoteBook --
   ----------------------

   procedure Emit_GtkNoteBook (TWdg : Widget_Pointer;
                               Id   : Integer;
                               Pos  : Integer) is
      Temp : Widget_Pointer;
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkNotebook", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      if Pos = -1 then
         Emit_WH_Request (TWdg, Id + 4);
      end if;
      Emit_Visible_And_Focus (TWdg, Id + 4, True);
      Emit_Align (TWdg, Id + 4, Numeric => True);
      Emit_Margin (TWdg, Id + 4);
      Emit_ToolTip (TWdg, Id + 4);
      Emit_Property (Id + 4, "show-tabs", TWdg.Show_Tabs);
      Emit_Property (Id + 4, "show-border", TWdg.Show_Border);
      Emit_Property (Id + 4, "scrollable", TWdg.Scrollable);
      Emit_Property (Id + 4, "enable-popup", TWdg.Enable_Popups);
      if TWdg.CloseButtonOnTabsInactiveVisible then
         Emit_GtkSignal (TWdg, Id + 4);
      else
         Emit_GtkSignal (TWdg, Id + 4, Except => "CloseButtonClick");
      end if;

      Temp := TWdg.Child_List;
      while Temp /= null loop
         if Temp.Widget_Type /= GtkTabPage then
            raise Program_Error;
         end if;
         Emit_GtkNotebookTab (Temp, Id + 4);
         Temp := Temp.Next;
      end loop;

      Emit_Line (Sp (Id + 2) & "</object>");
      if Pos = -1 then
         Emit_Packing_Child (TWdg, Id,
                                        Packing => False,
                                        XY      => False,
                                        Homog   => False);
      else
         Emit_Packing (Id + 2, Pos, (Pos /= -1), True, TWdg.Padding, True);
      end if;
      Emit_Line (Sp (Id) & "</child>");
   end Emit_GtkNoteBook;

   -------------------------
   -- Emit_GtkNotebookTab --
   -------------------------

   procedure Emit_GtkNotebookTab (TWdg : Widget_Pointer; Id : Integer) is
      Temp   : Widget_Pointer;
      Box    : Widget_Pointer;
      Label  : Widget_Pointer;
      Button : Widget_Pointer;
      TS     : Signal_Pointer;
   begin
      Temp := TWdg.Child_List;
      while Temp /= null loop
         if Temp.Widget_Type = GtkDataGridView then
            Emit_GtkDataGridView (Temp, Id);
         elsif Temp.Widget_Type = GtkTreeGridView then
            Emit_GtkTreeGridView (Temp, Id);
         end if;
         Temp := Temp.Next;
      end loop;

      --  tab header
      Emit_Line (Sp (Id) & "<child type=""tab"">"); --  no name

      --  gtkbox header
      Box := TWdg.Child_List.Next;
      Emit_Object (Box, Id + 2, "GtkBox", Box.Name.all);
      Emit_Visible_And_Focus (Box, Id + 4, False);
      Emit_Property (Id + 4, "spacing", 2);

      --  gtklabel complete
      Label := Box.Child_List;
      Emit_Child (Label, Id + 4, False);
      Emit_Object (Label, Id + 6, "GtkLabel", Label.Name.all);
      Emit_Visible_And_Focus (Label, Id + 8, False);
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
      Emit_Visible_And_Focus (Button, Id + 8, True);
      Emit_Property (Id + 8, "focus-on-click", False);
      Emit_Property (Id + 8, "receives-default", True);
      Emit_Property (Id + 8, "relief", "none");
      if not TWdg.GParent.CloseButtonOnTabsInactiveVisible then
         TS := TWdg.GParent.Signal_List;
         while TS /= null loop
            if TS.Name.all = "CloseButtonClick" then
               Emit_Line (Sp (Id + 8) & "<signal name="""
                          & Convert_Signal_To_Gtk (Button, TS)
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
      Emit_Visible_And_Focus (TWdg, Id + 12, False);
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
      Emit_Property (Id + 4, "position", Box.Child_Num);
      Emit_Property (Id + 4, "tab-fill", False);
      Emit_Line (Sp (Id + 2) & "</packing>");

      --  tab tail
      Emit_Line (Sp (Id) & "</child>");

   end Emit_GtkNotebookTab;
   ---------------------------------
   -- Emit_GtkMenu and associated --
   ---------------------------------
   procedure Emit_GtkSubmenu (TWdg : Widget_Pointer; Id : Integer);
   procedure Emit_GtkSubmenu (TWdg : Widget_Pointer; Id : Integer) is
      Child : Widget_Pointer;
      Sensitive : constant Boolean := TWdg.Enabled;
   begin
      Emit_Line (Sp (Id + 4) & "<child type=""submenu"">");
      Emit_Object (TWdg, Id + 6, "GtkMenu", TWdg.Name.all & "_submenu");
      TWdg.Enabled := True;
      Emit_Visible_And_Focus (TWdg, Id + 8, False);
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

   procedure Emit_GtkMenuNormalItem (TWdg : Widget_Pointer; Id : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkMenuItem", TWdg.Name.all);
      Emit_Label (TWdg, Id + 4, UnderLine => False, Selectable => False);
      Emit_Visible_And_Focus (TWdg, Id + 4, False);
      Emit_GtkSignal (TWdg, Id + 4);

      if TWdg.Child_List /= null then
         Emit_GtkSubmenu (TWdg, Id);
      end if;

      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Line (Sp (Id) & "</child>");
   end Emit_GtkMenuNormalItem;

   procedure Emit_GtkMenuImageItem (TWdg : Widget_Pointer; Id : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkImageMenuItem", TWdg.Name.all);
      Emit_Label (TWdg, Id + 4, UnderLine => False, Selectable => False);
      Emit_Visible_And_Focus (TWdg, Id + 4, False);
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

   procedure Emit_GtkSepMenuItem (TWdg : Widget_Pointer;
                                  Id : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkSeparatorMenuItem", "");
      Emit_Visible_And_Focus (TWdg, Id + 4, False);
      Emit_GtkSignal (TWdg, Id + 4);
      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Line (Sp (Id) & "</child>");
   end Emit_GtkSepMenuItem;

   procedure Emit_GtkMenuItem (TWdg : Widget_Pointer; Id : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkMenuItem", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_Visible_And_Focus (TWdg, Id + 4, False);
      Emit_Label (TWdg, Id + 4, UnderLine => True, Selectable => False);
      Emit_GtkSignal (TWdg, Id + 4);

      if TWdg.Child_List /= null then
         Emit_GtkSubmenu (TWdg, Id);
      end if;

      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Line (Sp (Id) & "</child>");
   end Emit_GtkMenuItem;

   procedure Emit_GtkMenuBar (TWdg : Widget_Pointer;
                              Id   : Integer;
                              Pos : Integer) is
      Child : Widget_Pointer;
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkMenuBar", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      if Pos = -1 then
         Emit_WH_Request (TWdg, Id + 4);
      end if;
      Emit_Visible_And_Focus (TWdg, Id + 4, False);
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
            when others => null;
         end case;
         Child := Child.Next;
      end loop;

      Emit_Line (Sp (Id + 2) & "</object>");
      if Pos = -1 then
         Emit_Packing_Child (TWdg, Id,
                             Packing => False,
                             XY      => True,
                             Homog   => False);
      else
         Emit_Packing (Id + 2, Pos, False, True, TWdg.Padding, True);
      end if;
      Emit_Line (Sp (Id) & "</child>");
   end Emit_GtkMenuBar;

   -------------------
   -- Emit_GtkLabel --
   -------------------

   procedure Emit_GtkLabel (TWdg    : Widget_Pointer;
                            Id      : Integer;
                            Packing : Boolean) is
   begin
      if TWdg.BorderStyle /= None then
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkAspectFrame",
                      TWdg.Name.all & "_aspectframe");
         Emit_Name (TWdg, Id + 4);
         Emit_WH_Request (TWdg, Id + 4);
         Emit_Visible_And_Focus (TWdg, Id + 4, False);
         Emit_Line (Sp (Id + 4) & "<property name=""label-xalign"">0"
                    & "</property>");
         Emit_Line (Sp (Id + 4) & "<property name=""shadow-type"">in"
                    & "</property>");

         Emit_Child (TWdg, Id + 4, False);
         Emit_Object (TWdg, Id + 6, "GtkLabel", TWdg.Name.all);
         Emit_Name (TWdg, Id + 8);
         Emit_Visible_And_Focus (TWdg, Id + 8, False);
         Emit_Property (Id + 8, "has-focus", TWdg.Has_Focus);
         Emit_Label (TWdg, Id + 8, UnderLine => False, Selectable => True);
         Emit_Align (TWdg, Id + 8, Numeric => False);
         if TWdg.MaxLength > 0 then
            Emit_Property (Id + 8, "width-chars", TWdg.MaxLength);
            Emit_Property (Id + 8, "max-width-chars", TWdg.MaxLength);
         end if;
         Emit_Attributes (TWdg, Id + 8);
         Emit_GtkSignal (TWdg, Id + 8);

         Emit_Line (Sp (Id + 6) & "</object>");
         Emit_Packing_Child (TWdg, Id + 4,
                                        Packing => False,
                                        XY      => True,
                                        Homog   => False);
         Emit_Line (Sp (Id + 4) & "</child>");

         --  end for the aspect frame
         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Packing_Child (TWdg, Id,
                             Packing => Packing,
                             XY      => True,
                             Homog   => False);
         Emit_Line (Sp (Id) & "</child>");
      else
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkLabel", TWdg.Name.all);
         Emit_Name (TWdg, Id + 4);
         Emit_WH_Request (TWdg, Id + 4);
         Emit_Visible_And_Focus (TWdg, Id + 4, False);
         Emit_Property (Id + 4, "has-focus", TWdg.Has_Focus);
         Emit_Align (TWdg, Id + 4, Numeric => True);
         Emit_Margin (TWdg, Id + 4);
         Emit_ToolTip (TWdg, Id + 4);
         Emit_Label (TWdg, Id + 4, UnderLine => False, Selectable => True);
         Emit_Attributes (TWdg, Id + 4);
         Emit_GtkSignal (TWdg, Id + 4);
         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Packing_Child (TWdg, Id,
                             Packing => Packing,
                             XY => True,
                             Homog => False);
         Emit_Line (Sp (Id) & "</child>");
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
      Emit_Object (TWdg, Id + 2, "GtkImage", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Focus (TWdg, Id + 4, False);
      Emit_Line (Sp (Id + 4) & "<property name=""pixbuf"">"
                 & TWdg.Image.all
                 & "</property>");
      Emit_Align (TWdg, Id + 4, Numeric => True);
      Emit_Margin (TWdg, Id + 4);
      Emit_ToolTip (TWdg, Id + 4);
      Emit_GtkSignal (TWdg, Id + 4);
      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing_Child (TWdg, Id,
                                     Packing => True,
                                     XY => True,
                                     Homog => False);
      Emit_Line (Sp (Id) & "</child>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkImage: " & TWdg.Name.all);
         raise;
   end Emit_GtkImage;

   --------------------
   -- Emit_GtkButton --
   --------------------

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
      Emit_Visible_And_Focus (TWdg, Id + 4, True);
      Emit_Property (Id + 4, "has-focus", TWdg.Has_Focus);
      Emit_Property (Id + 4, "receives-default", True);
      if Has_Default then
         Emit_Property (Id + 4, "can-default", True);
         Emit_Property (Id + 4, "has-default", True);
      end if;
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

   -------------------
   -- Emit_GtkEntry --
   -------------------

   procedure Emit_GtkEntry (TWdg : Widget_Pointer;
                            Id   : Integer;
                            Activate_Default : Boolean) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkEntry", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Focus (TWdg, Id + 4, TWdg.Editable);
      Emit_Property (Id + 4, "has-focus", TWdg.Has_Focus);
      Emit_Password (TWdg, Id + 4);
      Emit_Align (TWdg, Id + 4, Numeric => True);
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

      Emit_Margin (TWdg, Id + 4);
      Emit_ToolTip (TWdg, Id + 4);
      if Activate_Default then
         Emit_Property (Id + 4, "activates-default", True);
      end if;
      if TWdg.MaxLength > 0 then
         Emit_Property (Id + 4, "width-chars", TWdg.MaxLength);
         Emit_Property (Id + 4, "max-width-chars", TWdg.MaxLength);
      end if;
      Emit_Has_Frame (TWdg, Id + 4);
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

   ------------------------
   -- Emit_GtkSpinButton --
   ------------------------

   procedure Emit_GtkSpinButton (TWdg : Widget_Pointer; Id : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkSpinButton", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Focus (TWdg, Id + 4, True);
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

   --------------------------
   -- Emit_GtkComboTextBox --
   --------------------------

   procedure Emit_GtkComboTextBox (TWdg    : Widget_Pointer;
                                   Id      : Integer;
                                   Packing : Boolean;
                                   Activate_Default : Boolean) is
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
         if Activate_Default then
            Emit_Property (Id + 4, "activates-default", True);
         end if;
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
      Emit_Visible_And_Focus (TWdg, Id + 4, True);
      Emit_Property (Id + 4, "has-focus", TWdg.Has_Focus);
      Emit_Margin (TWdg, Id + 4);
      Emit_ToolTip (TWdg, Id + 4);
      Emit_Property (Id + 4, "has-entry", True);
      Emit_GtkSignal (TWdg, Id + 4);
      --  Emit_Property (Id + 4, "entry-text-column", 1);
      Emit_Internal_GtkEntry (Id + 4);
      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing_Child (TWdg, Id,
                          Packing => Packing,
                          XY => True,
                          Homog => False);
      Emit_Line (Sp (Id) & "</child>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkComboTextbox: " & TWdg.Name.all);
         raise;
   end Emit_GtkComboTextBox;

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
      Emit_Object (TWdg, Id + 2, Object, TWdg.Name.all);
      Emit_Label (TWdg, Id + 4, UnderLine => Underline, Selectable => False);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Focus (TWdg, Id + 4, True);
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
      Emit_Object (TWdg, Id + 2, Object, TWdg.Name.all);
      Emit_Label (TWdg, Id + 4, UnderLine => Underline, Selectable => False);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Focus (TWdg, Id + 4, True);
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

   -------------------------
   -- Emit_GtkCheckButton --
   -------------------------

   procedure Emit_GtkCheckButton (TWdg : Widget_Pointer; Id : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkCheckButton", TWdg.Name.all);
      Emit_Label (TWdg, Id + 4, UnderLine => False, Selectable => False);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Focus (TWdg, Id + 4, True);
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

   -----------------------
   -- Emit_GtkStatusBar --
   -----------------------

   procedure Emit_GtkStatusBar (TWdg : Widget_Pointer;
                                Id : Integer;
                                Pos : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkStatusbar", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      if Pos = -1 then
         Emit_WH_Request (TWdg, Id + 4);
      end if;
      Emit_Visible_And_Focus (TWdg, Id + 4, False);
      Emit_Align (TWdg, Id + 4, Numeric => False);
      Emit_Margin (TWdg, Id + 4);
      Emit_ToolTip (TWdg, Id + 4);
      Emit_Property (Id + 4, "hexpand", False);
      Emit_Property (Id + 4, "orientation", "vertical");
      Emit_Property (Id + 4, "spacing", 2);
      --  Emit_Attributes (TWdg, Id + 4);
      Emit_GtkSignal (TWdg, Id + 4);

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

   -------------------------------
   -- Emit_GtkFileChooserButton --
   -------------------------------

   procedure Emit_GtkFileChooserButton (TWdg : Widget_Pointer; Id : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkFileChooserButton", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Focus (TWdg, Id + 4, False);
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

   ---------------------
   -- Emit_TimePicker --
   ---------------------
   procedure Emit_TimePicker (TWdg : Widget_Pointer; Id : Integer) is
      procedure Emit_Packing (Id : Integer; Pos : Integer);
      procedure Emit_Packing (Id : Integer; Pos : Integer) is
      begin
         Emit_Line (Sp (Id) & "<packing>");
         Emit_Property (Id + 2, "expand", False);
         Emit_Property (Id + 2, "fill", False);
         Emit_Property (Id + 2, "position", Pos);
         Emit_Line (Sp (Id) & "</packing>");
      end Emit_Packing;

      procedure Emit_Image (TWdg : Widget_Pointer;
                            Id   : Integer;
                            Name : String;
                            Up   : Boolean);
      procedure Emit_Image (TWdg : Widget_Pointer;
                            Id   : Integer;
                            Name : String;
                            Up   : Boolean) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkImage",
                      TWdg.Name.all & Name & "_Img");
         Emit_Name (TWdg.Name.all & Name & "_Img", Id + 4);
         Emit_Visible_And_Focus (TWdg, Id + 4, False);
         Emit_ToolTip (TWdg, Id + 4);
         if Up then
            Emit_Line (Sp (Id + 4) & "<property name=""stock"">"
                       & "gtk-go-up"
                       & "</property>");
         else
            Emit_Line (Sp (Id + 4) & "<property name=""stock"">"
                       & "gtk-go-down"
                       & "</property>");
         end if;
         Emit_Property (Id + 4, "icon_size", 1);
         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Line (Sp (Id) & "</child>");
      exception
         when others =>
            TIO.Put_Line ("Emit TimePicker.Image: " & TWdg.Name.all);
            raise;
      end Emit_Image;

      procedure Emit_Button (TWdg : Widget_Pointer;
                             Id   : Integer;
                             Pos  : Integer;
                             Name : String);
      procedure Emit_Button (TWdg : Widget_Pointer;
                             Id   : Integer;
                             Pos  : Integer;
                             Name : String) is
      begin
         Emit_Child (TWdg, Id, False);
         if Pos = 0 then
            Emit_Object (TWdg, Id + 2, "GtkButton",
                         TWdg.Name.all & Name & "_Button_Up");
            Emit_Name (TWdg.Name.all & Name & "_Button_Up", Id + 4);
         else
            Emit_Object (TWdg, Id + 2, "GtkButton",
                         TWdg.Name.all & Name & "_Button_Down");
            Emit_Name (TWdg.Name.all & Name & "_Button_Down", Id + 4);
         end if;
         Emit_Property (Id + 4, "width-request", 11);
         Emit_Property (Id + 4, "height-request", 11);
         Emit_Visible_And_Focus (TWdg, Id + 4, True);
         Emit_Property (Id + 4, "receives-default", True);
         Emit_ToolTip (TWdg, Id + 4);
         if Pos = 0 then
            Emit_Line (Sp (Id + 4) & "<signal name=""clicked"" handler="""
                       & "On" & Name & "_Button_Up_Clicked_" & TWdg.Name.all
                       & """ object=""" & TWdg.Name.all & Name & "_Entry"
                       & """ swapped=""no""/>");
            Emit_Image (TWdg, Id + 4, Name & "_Up", True);
         else
            Emit_Line (Sp (Id + 4) & "<signal name=""clicked"" handler="""
                       & "On" & Name & "_Button_Down_Clicked_" & TWdg.Name.all
                       & """ object=""" & TWdg.Name.all & Name & "_Entry"
                       & """ swapped=""no""/>");
            Emit_Image (TWdg, Id + 4, Name & "_Down", False);
         end if;

         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Packing (Id + 2, Pos);
         Emit_Line (Sp (Id) & "</child>");
      exception
         when others =>
            TIO.Put_Line ("Emit TimePicker.Button: " & TWdg.Name.all);
            raise;
      end Emit_Button;

      procedure Emit_Entry (TWdg   : Widget_Pointer;
                            Id     : Integer;
                            Horiz  : Integer;
                            Pos    : Integer;
                            Len    : Integer;
                            PlaceH : String;
                            Name   : String);
      procedure Emit_Entry (TWdg   : Widget_Pointer;
                            Id     : Integer;
                            Horiz  : Integer;
                            Pos    : Integer;
                            Len    : Integer;
                            PlaceH : String;
                            Name   : String) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkEntry",
                      TWdg.Name.all & Name & "_Entry");
         Emit_Name (TWdg.Name.all & Name & "_Entry", Id + 4);
         Emit_Property (Id + 4, "width-request", Horiz);
         Emit_Property (Id + 4, "height-request", 11);
         Emit_Visible_And_Focus (TWdg, Id + 4, True);
         Emit_Property (Id + 4, "hexpand", True);
         Emit_Property (Id + 4, "max-length", Len);
         Emit_Property (Id + 4, "width-chars", Len);
         Emit_Property (Id + 4, "max-width-chars", Len);
         Emit_Line (Sp (Id + 4) & "<property name=""xalign"">0.5</property>");
         Emit_Property (Id + 4,  "overwrite-mode", True);
         Emit_Line (Sp (Id + 4) & "<property name=""placeholder-text"" "
                    & "translatable=""yes"">"
                    & PlaceH
                    & "</property>");
         Emit_Line (Sp (Id + 4) & "<signal name=""activate"" handler="""
                    & "On_Entry" & Name & "_Activate_" & TWdg.Name.all
                    & """ object=""" & TWdg.Name.all & Name & "_Button_Up"
                    & """ swapped=""no""/>");
         Emit_Line (Sp (Id + 4) & "<signal name=""focus-out-event"" handler="""
                    & "On_Entry" & Name & "_Leavefocus_" & TWdg.Name.all
                    & """ object=""" & TWdg.Name.all & Name & "_Button_Up"
                    & """ swapped=""no""/>");
         Emit_ToolTip (TWdg, Id + 4);
         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Packing (Id + 2, Pos);
         Emit_Line (Sp (Id) & "</child>");
      exception
         when others =>
            TIO.Put_Line ("Emit TimePicker.Entry: " & TWdg.Name.all);
            raise;
      end Emit_Entry;

      procedure Emit_VBox (TWdg : Widget_Pointer;
                           Id   : Integer;
                           Pos  : Integer;
                           Name : String);
      procedure Emit_VBox (TWdg : Widget_Pointer;
                           Id   : Integer;
                           Pos  : Integer;
                           Name : String) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkBox",
                      TWdg.Name.all & Name & "_Vbox");
         Emit_Name (TWdg.Name.all & Name & "_Vbox", Id + 4);
         Emit_Visible_And_Focus (TWdg, Id + 4, False);
         Emit_Property (Id + 4, "orientation", "vertical");

         Emit_Button (TWdg, Id + 4, 0, Name);
         Emit_Button (TWdg, Id + 4, 1, Name);

         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Packing (Id + 2, Pos);
         Emit_Line (Sp (Id) & "</child>");
      exception
         when others =>
            TIO.Put_Line ("Emit TimePicker.VBox: " & TWdg.Name.all);
            raise;
      end Emit_VBox;

      procedure Emit_HBox (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_HBox (TWdg : Widget_Pointer; Id : Integer) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkBox", TWdg.Name.all & "_Hbox");
         Emit_Name (TWdg.Name.all & "_Hbox", Id + 4);
         Emit_Visible_And_Focus (TWdg, Id + 4, False);
         Emit_Property (Id + 4, "orientation", "horizontal");

         Emit_Entry (TWdg, Id + 4, 20, 0, 2, "00", "_Hour");
         Emit_VBox (TWdg, Id + 4, 1, "_Hour");
         Emit_Entry (TWdg, Id + 4, 20, 2, 2, "00", "_Min");
         Emit_VBox (TWdg, Id + 4, 3, "_Min");
         Emit_Entry (TWdg, Id + 4, 20, 4, 2, "00", "_Sec");
         Emit_VBox (TWdg, Id + 4, 5, "_Sec");

         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Line (Sp (Id) & "</child>");
      exception
         when others =>
            TIO.Put_Line ("Emit TimePicker.HBox: " & TWdg.Name.all);
            raise;
      end Emit_HBox;

      procedure Emit_Aspectframe (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_Aspectframe (TWdg : Widget_Pointer; Id : Integer) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkAspectFrame",
                      TWdg.Name.all & "_Aspectframe");
         Emit_Name (TWdg.Name.all & "_Aspectframe", Id + 4);
         Emit_Visible_And_Focus (TWdg, Id + 4, False);
         Emit_Line (Sp (Id + 4) & "<property name=""label-xalign"">0"
                    & "</property>");
         Emit_Line (Sp (Id + 4) & "<property name=""shadow-type"">in"
                    & "</property>");
         Emit_HBox (TWdg, Id + 4);
         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Packing_Child (TWdg, Id,
                             Packing => True,
                             XY      => True,
                             Homog   => False);
         Emit_Line (Sp (Id) & "</child>");
      exception
         when others =>
            TIO.Put_Line ("Emit TimePicker.Aspectframe: " & TWdg.Name.all);
            raise;
      end Emit_Aspectframe;

   begin
      Emit_Aspectframe (TWdg, Id);
   exception
      when others =>
         TIO.Put_Line ("Emit TimePicker: " & TWdg.Name.all);
         raise;
   end Emit_TimePicker;

   ---------------------
   -- Emit_DatePicker --
   ---------------------
   procedure Emit_DatePicker (TWdg : Widget_Pointer; Id : Integer) is
      procedure Emit_Packing (Id     : Integer;
                              Expand : Boolean;
                              Fill   : Boolean;
                              Pos    : Integer);
      procedure Emit_Packing (Id     : Integer;
                              Expand : Boolean;
                              Fill   : Boolean;
                              Pos    : Integer) is
      begin
         Emit_Line (Sp (Id) & "<packing>");
         Emit_Property (Id + 2, "expand", Expand);
         Emit_Property (Id + 2, "fill", Fill);
         Emit_Property (Id + 2, "position", Pos);
         Emit_Line (Sp (Id) & "</packing>");
      end Emit_Packing;

      procedure Emit_Entry (TWdg   : Widget_Pointer;
                            Id     : Integer;
                            Pos    : Integer;
                            WR     : Integer;
                            MaxC   : Integer;
                            PlaceH : String;
                            Name   : String);
      procedure Emit_Entry (TWdg   : Widget_Pointer;
                            Id     : Integer;
                            Pos    : Integer;
                            WR     : Integer;
                            MaxC   : Integer;
                            PlaceH : String;
                            Name   : String) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkEntry", TWdg.Name.all
                      & Name & "_Entry");
         Emit_Name (TWdg.Name.all & Name & "_Entry", Id + 4);
         Emit_Line (Sp (Id + 4) & "<property name=""width-request"">"
                    & Img (WR) & "</property>");
         Emit_Visible_And_Focus (TWdg, Id + 4, True);
         Emit_ToolTip (TWdg, Id + 4);
         Emit_Line (Sp (Id + 4) & "<property name=""max-length"">"
                    & Img (MaxC) & "</property>");
         Emit_Line (Sp (Id + 4) & "<property name=""width-chars"">"
                    & Img (MaxC) & "</property>");
         Emit_Line (Sp (Id + 4) & "<property name=""max-width-chars"">"
                    & Img (MaxC) & "</property>");
         Emit_Line (Sp (Id + 4) & "<property name=""xalign"">0.5</property>");
         Emit_Property (Id + 4,  "overwrite-mode", True);
         Emit_Line (Sp (Id + 4) & "<property name=""placeholder-text"" "
                    & "translatable=""yes"">"
                    & PlaceH
                    & "</property>");
         Emit_Line (Sp (Id + 4)
                    & "<property name=""input-purpose"">digits</property>");
         Emit_Line (Sp (Id + 4) & "<signal name=""activate"" handler="""
                    & "On_Entry" & Name & "_Activate_" & TWdg.Name.all
                    & """ object=""" & TWdg.Name.all & "_Calendar"
                    & """ swapped=""no""/>");
         Emit_Line (Sp (Id + 4) & "<signal name=""focus-out-event"" handler="""
                    & "On_Entry" & Name & "_Leavefocus_" & TWdg.Name.all
                    & """ object=""" & TWdg.Name.all & "_Calendar"
                    & """ swapped=""no""/>");
         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Packing (Id + 2, False, False, Pos);
         Emit_Line (Sp (Id) & "</child>");
      exception
         when others =>
            TIO.Put_Line ("Emit DatePicker.Entry: " & TWdg.Name.all);
            raise;
      end Emit_Entry;

      procedure Emit_Image (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_Image (TWdg : Widget_Pointer; Id : Integer) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkImage", TWdg.Name.all & "_Img");
         Emit_Name (TWdg.Name.all & "_Img", Id + 4);
         Emit_Visible_And_Focus (TWdg, Id + 4, False);
         Emit_ToolTip (TWdg, Id + 4);
         Emit_Line (Sp (Id + 4) & "<property name=""icon-name"">"
                    & "office-calendar"
                    & "</property>");
         Emit_Line (Sp (Id + 2) & "</object>");
         --  Emit_Packing_Child (TWdg, Id,
         --                      Packing => False,
         --                      XY => True,
         --                      Homog => False);
         Emit_Line (Sp (Id) & "</child>");
      exception
         when others =>
            TIO.Put_Line ("Emit DatePicker.Image: " & TWdg.Name.all);
            raise;
      end Emit_Image;

      procedure Emit_Button (TWdg : Widget_Pointer;
                             Id   : Integer;
                             Pos  : Integer);
      procedure Emit_Button (TWdg : Widget_Pointer;
                             Id   : Integer;
                             Pos  : Integer) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkButton", TWdg.Name.all & "_Button");
         Emit_Name (TWdg.Name.all & "_Button", Id + 4);
         Emit_Visible_And_Focus (TWdg, Id + 4, True);
         Emit_Property (Id + 4, "receives-default", True);
         Emit_ToolTip (TWdg, Id + 4);
         Emit_Line (Sp (Id + 4) & "<signal name=""clicked"" handler="""
                    & "On_Button_Clicked_" & TWdg.Name.all
                    & """ object=""" & TWdg.Name.all & "_Calendar"
                    & """ swapped=""no""/>");

         Emit_Image (TWdg, Id + 4);

         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Packing (Id + 2, True, True, Pos);
         Emit_Line (Sp (Id) & "</child>");
      exception
         when others =>
            TIO.Put_Line ("Emit DatePicker.Button: " & TWdg.Name.all);
            raise;
      end Emit_Button;

      procedure Emit_Calendar (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_Calendar (TWdg : Widget_Pointer; Id : Integer) is
         Temp : constant Boolean := TWdg.Visible;
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkCalendar", TWdg.Name.all & "_Calendar");
         Emit_Name (TWdg.Name.all & "_Calendar", Id + 4);
         TWdg.Visible := False;
         Emit_Visible_And_Focus (TWdg, Id + 4, False);
         TWdg.Visible := Temp;
         Emit_ToolTip (TWdg, Id + 4);
         Emit_Line (Sp (Id + 4) & "<signal name=""day-selected"" handler="""
                    & "On_Day_Selected_" & TWdg.Name.all
                    & """ object=""" & TWdg.Name.all & "_Button"
                    & """ swapped=""no""/>");
         Emit_Line (Sp (Id + 4)
                    & "<signal name=""day-selected-double-click"" handler="""
                    & "On_Day_Selected_Double_Click_" & TWdg.Name.all
                    & """ object=""" & TWdg.Name.all & "_Button"
                    & """ swapped=""no""/>");
         Emit_Line (Sp (Id + 4)
                    & "<signal name=""next-month"" handler="""
                    & "On_Next_Month_" & TWdg.Name.all
                    & """ object=""" & TWdg.Name.all & "_Button"
                    & """ swapped=""no""/>");
         Emit_Line (Sp (Id + 4)
                    & "<signal name=""prev-month"" handler="""
                    & "On_Prev_Month_" & TWdg.Name.all
                    & """ object=""" & TWdg.Name.all & "_Button"
                    & """ swapped=""no""/>");
         Emit_Line (Sp (Id + 4)
                    & "<signal name=""next-year"" handler="""
                    & "On_Next_Year_" & TWdg.Name.all
                    & """ object=""" & TWdg.Name.all & "_Button"
                    & """ swapped=""no""/>");
         Emit_Line (Sp (Id + 4)
                    & "<signal name=""prev-year"" handler="""
                    & "On_Prev_Year_" & TWdg.Name.all
                    & """ object=""" & TWdg.Name.all & "_Button"
                    & """ swapped=""no""/>");
         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Packing (Id => Id + 2, Expand => False, Fill => True, Pos => 1);
         Emit_Line (Sp (Id) & "</child>");
      exception
         when others =>
            TIO.Put_Line ("Emit DatePicker.Calendar: " & TWdg.Name.all);
            raise;
      end Emit_Calendar;

      procedure Emit_HBox (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_HBox (TWdg : Widget_Pointer; Id : Integer) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkBox", TWdg.Name.all & "_Hbox");
         Emit_Name (TWdg.Name.all & "_Hbox", Id + 4);
         Emit_Visible_And_Focus (TWdg, Id + 4, False);
         Emit_Property (Id + 4, "orientation", "horizontal");

         Emit_Entry (TWdg, Id + 4, 0, 50, 4, "2020", "_Year");
         Emit_Entry (TWdg, Id + 4, 1, 20, 2, "01", "_Month");
         Emit_Entry (TWdg, Id + 4, 2, 20, 2, "01", "_Day");
         if TWdg.ShowUpDown then
            Emit_Button (TWdg, Id + 3, Pos => 4);
         end if;

         Emit_Line (Sp (Id + 2) & "</object>");
         --  Emit_Packing (Id + 2, 0);
         Emit_Line (Sp (Id) & "</child>");
      exception
         when others =>
            TIO.Put_Line ("Emit DatePicker.HBox: " & TWdg.Name.all);
            raise;
      end Emit_HBox;

      procedure Emit_Aspectframe (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_Aspectframe (TWdg : Widget_Pointer; Id : Integer) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkAspectFrame",
                      TWdg.Name.all & "_Aspectframe");
         Emit_Name (TWdg.Name.all & "_Aspectframe", Id + 4);
         Emit_Visible_And_Focus (TWdg, Id + 4, False);
         Emit_Line (Sp (Id + 4) & "<property name=""label-xalign"">0"
                    & "</property>");
         Emit_Line (Sp (Id + 4) & "<property name=""shadow-type"">in"
                    & "</property>");
         Emit_Property (Id + 4, "xalign", 0.0);
         Emit_HBox (TWdg, Id + 4);
         Emit_Line (Sp (Id + 2) & "</object>");
         --  Emit_Packing (Id + 2, False, False, 0);
         Emit_Line (Sp (Id) & "</child>");
      exception
         when others =>
            TIO.Put_Line ("Emit DatePicker.Aspectframe: " & TWdg.Name.all);
            raise;
      end Emit_Aspectframe;

      procedure Emit_VBox (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_VBox (TWdg : Widget_Pointer; Id : Integer) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkBox", TWdg.Name.all & "_Vbox");
         Emit_Name (TWdg.Name.all & "_Vbox", Id + 4);
         Emit_Visible_And_Focus (TWdg, Id + 4, False);
         Emit_Property (Id + 4, "orientation", "vertical");

         Emit_Aspectframe (TWdg, Id + 4);
         Emit_Calendar (TWdg, Id + 4);

         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Packing_Child (TWdg, Id,
                             Packing => True,
                             XY      => True,
                             Homog   => False);
         Emit_Line (Sp (Id) & "</child>");
      exception
         when others =>
            TIO.Put_Line ("Emit DatePicker.VBox: " & TWdg.Name.all);
            raise;
      end Emit_VBox;

   begin
      Emit_VBox (TWdg, Id);
   exception
      when others =>
         TIO.Put_Line ("Emit DatePicker: " & TWdg.Name.all);
         raise;
   end Emit_DatePicker;


   ---------------------
   -- Emit_GtkListBox --
   ---------------------
   procedure Emit_GtkListBox (TWdg : Widget_Pointer; Id : Integer) is
      procedure Emit_Internal_GtkSelection (Id : Integer);
      procedure Emit_Internal_GtkSelection (Id : Integer) is
      begin
         Emit_Line (Sp (Id) & "<child internal-child=""selection"">");
         Emit_Object (TWdg, Id + 2, "GtkTreeSelection",
                      "Selection_" & TWdg.Name.all);
         if TWdg.MultiSelect then
            Emit_Line (Sp (Id + 4) & "<property name=""mode"">multiple"
                       & "</property>");
         end if;
         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Packing_Child (TWdg, Id,
                                        Packing => False,
                                        XY => True,
                                        Homog => False);
         Emit_Line (Sp (Id) & "</child>");
      end Emit_Internal_GtkSelection;
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkTreeView", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Focus (TWdg, Id + 4, False);
      Emit_ToolTip (TWdg, Id + 4);
      Emit_Margin (TWdg, Id + 4);
      Emit_GtkSignal (TWdg, Id + 4);
      Emit_Internal_GtkSelection (Id + 4);
      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Packing_Child (TWdg, Id,
                                     Packing => True,
                                     XY => True,
                                     Homog => False);
      Emit_Line (Sp (Id) & "</child>");
   end Emit_GtkListBox;

   -------------------
   -- Emit_GtkFrame --
   -------------------

   procedure Emit_GtkFrame (TWdg : Widget_Pointer; Id : Integer) is
      procedure Emit_Alignment (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_Alignment (TWdg : Widget_Pointer; Id : Integer) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkAlignment",
                      "Alignment_" & TWdg.Name.all);
         Emit_Name (TWdg, Id + 4);
         Emit_WH_Request (TWdg, Id + 4);
         Emit_Visible_And_Focus (TWdg, Id + 4, False);
         Emit_Line (Sp (Id + 4)
                    & "<property name=""left-padding"">12</property>");
      end Emit_Alignment;

      procedure Emit_Fixed (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_Fixed (TWdg : Widget_Pointer; Id : Integer) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkFixed", "Fixed_" & TWdg.Name.all);
         Emit_Visible_And_Focus (TWdg, Id + 4, False);
      end Emit_Fixed;

      procedure Emit_Frame_Label (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_Frame_Label (TWdg : Widget_Pointer; Id : Integer) is
      begin
         Emit_Child (TWdg, Id, True);
         Emit_Object (TWdg, Id + 2, "GtkLabel", "label_" & TWdg.Name.all);
         Emit_Visible_And_Focus (TWdg, Id + 4, False);
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
      Emit_Visible_And_Focus (TWdg, Id + 4, False);
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
      Child := TWdg.Child_List;
      while Child /= null loop
         case Child.Widget_Type is
            when No_Widget =>
               null;
            when GtkBox =>
               null;
            when GtkMenuBar =>
               null;
            when GtkMenuItem | GtkSubMenu =>
               null;
            when GtkSeparatorMenuItem =>
               null;
            when GtkMenuNormalItem =>
               null;
            when GtkMenuImageItem =>
               null;
            when GtkMenuRadioItem =>
               null;
            when GtkMenuCheckItem =>
               null;
            when GtkNoteBook =>
               null;
            when GtkTabChild => null;
            when GtkTabPage => null;
            when GtkDataGridView =>
               null;
            when GtkTreeGridView =>
               null;
            when ExpandableColumn | DataGridViewTextBoxColumn =>
               null;
            when DataGridViewCheckBoxColumn =>
               null;
            when BackgroundWorker =>
               null;
            when GtkLabel =>
               Emit_GtkLabel (Child, Id + 12, Packing => True);
            when GtkImage =>
               Emit_GtkImage (Child, Id + 12);
            when GtkEntry =>
               Emit_GtkEntry (Child, Id + 12, False);
            when GtkComboTextBox =>
               Emit_GtkComboTextBox (Child, Id + 12,
                                     Packing => True,
                                     Activate_Default => False);
            when GtkButton =>
               Emit_GtkButton (Child, Id + 12, "GtkButton",
                               Position    => -1,
                               Has_Default => False,
                               XY          => True,
                               Homog       => False);
            when GtkRadioButton =>
               Emit_GtkRadioButton (Child, Id + 12, "GtkRadioButton",
                                    Underline => Child.Underline,
                                    XY => True, Homog => False);
            when GtkCheckButton =>
               Emit_GtkCheckButton (Child, Id + 12);
            when GtkSpinButton =>
               Emit_GtkSpinButton (Child, Id + 12);
            when GtkColorButton =>
               Emit_GtkColorButton (Child, Id + 12);
            when GtkToolBar =>
               Emit_GtkToolBar (Child, Id + 12, 0);
            when GtkListBox =>
               Emit_GtkListBox (Child, Id + 12);
            when GtkCalendar =>
               if Child.Is_DatePicker then
                  Emit_DatePicker (Child, Id + 12);
               else
                  Emit_TimePicker (Child, Id + 12);
               end if;
            when GtkFrame =>
               Emit_GtkFrame (Child, Id + 12);
            when GtkStatusBar =>
               Emit_GtkStatusBar (Child, Id + 12, 0);
            when ToolStripStatusLabel =>
               Emit_GtkLabel (Child, Id + 12, Packing => True);
            when GtkFileChooserButton =>
               Emit_GtkFileChooserButton (Child, Id + 12);
            when GtkToggleButton =>
               Emit_GtkToggleButton (Child, Id + 12, "GtkToggleButton",
                                     Underline => Child.Underline,
                                     XY => True, Homog => False);
            when PrintDocument        => null;
            when PrintDialog          => null;
            when PageSetupDialog      => null;
            when Chart                => null;
            when FolderBrowserDialog  => null;
            when GtkToolTip           => null;
            when GtkSeparatorToolItem => null;
            when BindingNavigator =>
               Emit_GtkToolBar (Child, Id + 4, 0);
         end case;
         Child := Child.Next;
      end loop;
      --  gtkfixed
      Emit_Line (Sp (Id + 10) & "</object>");
      Emit_Packing_Child (TWdg, Id + 8,
                                     Packing => False,
                                     XY => True,
                                     Homog => False);
      Emit_Line (Sp (Id + 8) & "</child>");
      --  gtkalignment
      Emit_Line (Sp (Id + 6) & "</object>");
      Emit_Packing_Child (TWdg, Id + 4,
                                     Packing => False,
                                     XY => True,
                                     Homog => False);
      Emit_Line (Sp (Id + 4) & "</child>");
      Emit_Frame_Label (TWdg, Id + 4);
      --  gtkframe
      Emit_Line (Sp (Id + 2) & "</object>");
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

   -----------------------------
   -- Emit_GtkFileColorButton --
   -----------------------------

   procedure Emit_GtkColorButton (TWdg : Widget_Pointer; Id : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkColorButton", TWdg.Name.all);
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

   -----------------
   -- Emit_GtkBox --
   -----------------
   procedure Emit_GtkBox (TWdg : Widget_Pointer;
                          Id   : Integer;
                          Pos  : Integer) is
      Child : Widget_Pointer;
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkBox", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      if Pos = -1 then
         Emit_WH_Request (TWdg, Id + 4);
      end if;
      Emit_Visible_And_Focus (TWdg, Id + 4, False);
      if TWdg.FlowDirection = LeftToRight
        or else
          TWdg.FlowDirection = RightToLeft
      then
         Emit_Property (Id + 4, "orientation", "horizontal");
      else
         Emit_Property (Id + 4, "orientation", "vertical");
      end if;

      Child := TWdg.Child_List;
      while Child /= null loop
         case Child.Widget_Type is
            when No_Widget => null;

            when GtkBox =>
               Emit_GtkBox (Child, Id + 4, Child.Child_Num - 1);
            when BindingNavigator =>
               Emit_GtkToolBar (Child, Id + 4, Child.Child_Num - 1);
            when GtkMenuBar =>
               Emit_GtkMenuBar (Child, Id + 4, Child.Child_Num - 1);
            when GtkNoteBook =>
               Emit_GtkNoteBook (Child, Id + 4, Child.Child_Num - 1);
            when GtkStatusBar =>
               Emit_GtkStatusBar (Child, Id + 4, Child.Child_Num - 1);
            when GtkToolBar =>
               Emit_GtkToolBar (Child, Id + 4, Child.Child_Num - 1);

            when GtkSeparatorMenuItem => null;
            when GtkMenuItem | GtkSubMenu => null;
            when GtkMenuNormalItem => null;
            when GtkMenuImageItem => null;
            when GtkMenuRadioItem => null;
            when GtkMenuCheckItem => null;
            when GtkDataGridView => null;
            when GtkTreeGridView => null;
            when ExpandableColumn | DataGridViewTextBoxColumn =>  null;
            when DataGridViewCheckBoxColumn => null;
            when GtkTabChild => null;
            when GtkTabPage => null;

            when GtkEntry =>
               Emit_GtkEntry (Child, Id + 4, False);
            when GtkComboTextBox =>
               Emit_GtkComboTextBox (Child, Id + 4,
                                     Packing => True,
                                     Activate_Default => False);
            when GtkCalendar =>
               if Child.Is_DatePicker then
                  Emit_DatePicker (Child, Id + 4);
               else
                  Emit_TimePicker (Child, Id + 4);
               end if;
            when GtkSpinButton =>
               Emit_GtkSpinButton (Child, Id + 4);
            when GtkFileChooserButton =>
               Emit_GtkFileChooserButton (Child, Id + 4);

            when PrintDocument        => null;
            when PrintDialog          => null;
            when PageSetupDialog      => null;
            when FolderBrowserDialog  => null;
            when GtkToolTip           => null;

            when GtkColorButton =>
               Emit_GtkColorButton (Child, Id + 4);
            when GtkListBox =>
               Emit_GtkListBox (Child, Id + 4);
            when GtkLabel =>
               Emit_GtkLabel (Child, Id + 4, Packing => True);
            when ToolStripStatusLabel =>
               Emit_GtkLabel (Child, Id + 4, Packing => True);
            when GtkImage =>
               Emit_GtkImage (Child, Id + 4);
            when GtkButton =>
               Emit_GtkButton (Child, Id + 4, "GtkButton",
                               Position    => -1,
                               Has_Default => False,
                               XY          => True,
                               Homog       => False);
            when GtkRadioButton =>
               Emit_GtkRadioButton (Child, Id + 4, "GtkRadioButton",
                                    Underline => Child.Underline,
                                    XY        => True,
                                    Homog     => False);
            when GtkCheckButton =>
               Emit_GtkCheckButton (Child, Id + 4);
            when GtkToggleButton =>
               Emit_GtkToggleButton (Child, Id + 4, "GtkToggleButton",
                                     Underline => Child.Underline,
                                     XY        => True,
                                     Homog     => False);
            when GtkFrame =>
               Emit_GtkFrame (Child, Id + 4);

            when Chart                => null;
            when GtkSeparatorToolItem => null;
            when BackgroundWorker     => null;
         end case;
         Child := Child.Next;
      end loop;

      Emit_Line (Sp (Id + 2) & "</object>");
      if Pos = -1 then --  called from fixed window
         Emit_Packing_Child (TWdg, Id,
                             Packing => True,
                             XY      => True,
                             Homog   => False);
      elsif Pos = -2 then --  called from main window
         null;
      else
         if TWdg.FlowDirection = BottomUp
           or else
             TWdg.FlowDirection = RightToLeft
         then
            Emit_Packing (Id + 2, Pos, False, True, TWdg.Padding, False);
         else
            Emit_Packing (Id + 2, Pos, False, True, TWdg.Padding, True);
         end if;
      end if;
      Emit_Line (Sp (Id) & "</child>");
   end Emit_GtkBox;

   ---------------------
   -- Emit_GtkToolBar --
   ---------------------

   procedure Emit_GtkToolBar (TWdg : Widget_Pointer;
                              Id   : Integer;
                              Pos  : Integer) is
      procedure Emit_SeparatorToolItem (TWdg : Widget_Pointer; Id : Integer);
      procedure Emit_SeparatorToolItem (TWdg : Widget_Pointer; Id : Integer) is
      begin
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkSeparatorToolItem", TWdg.Name.all);
         Emit_Name (TWdg, Id + 4);
         Emit_Visible_And_Focus (TWdg, Id + 4, False);
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
         Emit_Visible_And_Focus (TWdg, Id + 4, False);
         Emit_Align (TWdg, Id + 4, Numeric => True);
         Emit_Margin (TWdg, Id + 4);
         Emit_ToolTip (TWdg, Id + 4);
         Emit_Label (TWdg, Id + 4, TWdg.Underline, Selectable => True);
         if TWdg.GParent.Widget_Type = BindingNavigator then
            Emit_Property (Id + 4, "xalign", 0.01);
         end if;
         if TWdg.MaxLength > 0 then
            Emit_Property (Id + 4, "width-chars", TWdg.MaxLength);
            Emit_Property (Id + 4, "max-width-chars", TWdg.MaxLength);
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
         Emit_Visible_And_Focus (TWdg, Id + 4, True);
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
         Emit_Visible_And_Focus (TWdg, Id + 4, True);
         Emit_Property (Id + 4, "has-focus", TWdg.Has_Focus);
         Emit_Property (Id + 4, "receives-default", True);
         Emit_Align (TWdg, Id + 4, Numeric => False);
         Emit_Margin (TWdg, Id + 4);
         Emit_ToolTip (TWdg, Id + 4);
         Emit_GtkSignal (TWdg, Id + 4);

         Emit_Child (TWdg, Id + 4, False);
         Emit_Object (TWdg, Id + 6, "GtkImage", "");
         TWdg.Enabled := True;
         Emit_Visible_And_Focus (TWdg, Id + 8, False);
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
         Emit_Visible_And_Focus (TWdg, Id + 4, TWdg.Editable);
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
         Emit_Visible_And_Focus (TWdg, Id + 4, False);
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

      Temp : Widget_Pointer;
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkToolbar", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      if Pos = -1 then
         Emit_WH_Request (TWdg, Id + 4);
      end if;
      Emit_Visible_And_Focus (TWdg, Id + 4, False);
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
            when GtkButton | GtkRadioButton | GtkToggleButton
               | GtkLabel | GtkComboTextBox | GtkEntry =>
               Emit_ToolItem (Temp, Id + 4);

            when GtkSeparatorToolItem =>
               Emit_SeparatorToolItem (Temp, Id + 4);

            when GtkMenuBar =>
               Emit_MenuToolButton (Temp, Id + 4);

            when others => null;
         end case;
         Temp := Temp.Next;
      end loop;

      Emit_Line (Sp (Id + 2) & "</object>");
      if Pos = -1 then
         Emit_Packing_Child (TWdg, Id,
                                        Packing => True,
                                        XY      => True,
                                        Homog   => False);
      else
         Emit_Packing (Id + 2, Pos, False, True, TWdg.Padding, True);
      end if;
      Emit_Line (Sp (Id) & "</child>");
   end Emit_GtkToolBar;

   ----------------------------------------------------
   --             G T K   W I N D O W S              --
   ----------------------------------------------------

   ----------------
   -- Emit_Store --
   ----------------

   procedure Emit_Store (TWin : Window_Pointer; Id : Integer);
   procedure Emit_Store (TWin : Window_Pointer; Id : Integer) is
      Col  : Widget_Pointer;
      TWdg : constant Widget_Pointer := TWin.Associated_Widget;
   begin
      Emit_Line (Sp (Id + 2) & "<columns>");
      Col := TWdg.Child_List;
      while Col /= null loop
         Emit_Line (Sp (Id + 2) & "<!-- column-name "
                    & Col.Name.all
                    & " -->");
         case Col.Widget_Type is
            when ExpandableColumn | DataGridViewTextBoxColumn =>
               if Col.DefaultCellStyle in DGVS'Range then
                  case DGVS (Col.DefaultCellStyle).Format is
                     when Format_Boolean =>
                        Emit_Line (Sp (Id + 2)
                                   & "<column type=""gboolean""/>");
                     when Format_Integer =>
                        Emit_Line (Sp (Id + 2)
                                   & "<column type=""gchararray""/>");
                     when Format_Real =>
                        Emit_Line (Sp (Id + 2)
                                   & "<column type=""gchararray""/>");
                     when Format_Exponential =>
                        Emit_Line (Sp (Id + 2)
                                   & "<column type=""gchararray""/>");
                     when Format_Decimal =>
                        Emit_Line (Sp (Id + 2)
                                   & "<column type=""gchararray""/>");
                     when Format_Date =>
                        Emit_Line (Sp (Id + 2)
                                   & "<column type=""gchararray""/>");
                     when Format_Percent =>
                        Emit_Line (Sp (Id + 2)
                                   & "<column type=""gchararray""/>");
                     when Format_Currency =>
                        Emit_Line (Sp (Id + 2)
                                   & "<column type=""gchararray""/>");
                     when Format_String =>
                        Emit_Line (Sp (Id + 2)
                                   & "<column type=""gchararray""/>");
                  end case;
               else
                  Emit_Line (Sp (Id + 2) & "<column type=""gchararray""/>");
               end if;
            when DataGridViewCheckBoxColumn =>
               Emit_Line (Sp (Id + 2)
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
                  Emit_Line (Sp (Id + 2) & "<!-- column-name "
                             & Col.Name.all & "_data"
                             & " -->");
                  Emit_Line (Sp (Id + 2)
                             & "<column type=""gboolean""/>");

                  if not Col.ReadOnly then
                     Emit_Line (Sp (Id + 2) & "<!-- column-name "
                                & Col.Name.all & "_activatable"
                                & " -->");
                     Emit_Line (Sp (Id + 2)
                                & "<column type=""gboolean""/>");
                  end if;
               end if;
            when ExpandableColumn | DataGridViewTextBoxColumn =>
               if Col.Text_Col_Properties.Fg_Color_Name_Column /= -1 then
                  Emit_Line (Sp (Id + 2) & "<!-- column-name "
                             & Col.Name.all & "_fg_color"
                             & " -->");
                  Emit_Line (Sp (Id + 2)
                             & "<column type=""gchararray""/>");
               end if;
            when others => null;
         end case;
         Col := Col.Next;
      end loop;

      if TWdg.AlternatingRowsDefaultCellStyle in DGVS'Range then
         Emit_Line (Sp (Id + 2) & "<!-- column-name "
                    & "ALT_Bg_" & TWdg.Name.all
                    & " -->");
         Emit_Line (Sp (Id + 2) & "<column type=""gchararray""/>");
      end if;

      Emit_Line (Sp (Id + 2) & "</columns>");
   end Emit_Store;

   -------------------------
   -- Emit_GtkModelFilter --
   -------------------------

   procedure Emit_GtkModelFilter (TWin : Window_Pointer; Id : Integer) is
   begin
      Emit_Object (null, Id, "GtkTreeModelFilter", TWin.Name.all);
      Emit_Property (Id + 2, "child-model", TWin.Underlying_Model.Name.all);
      Emit_Line (Sp (Id) & "</object>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkModelFilter: " & TWin.Name.all);
         raise;
   end Emit_GtkModelFilter;

   -----------------------
   -- Emit_GtkModelSort --
   -----------------------

   procedure Emit_GtkModelSort (TWin : Window_Pointer; Id : Integer) is
   begin
      Emit_Object (null, Id, "GtkTreeModelSort", TWin.Name.all);
      Emit_Property (Id + 2, "model", TWin.Underlying_Model.Name.all);
      Emit_Line (Sp (Id) & "</object>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkModelSort: " & TWin.Name.all);
         raise;
   end Emit_GtkModelSort;

   -----------------------
   -- Emit_GtkTreeStore --
   -----------------------

   procedure Emit_GtkTreeStore (TWin : Window_Pointer; Id : Integer) is
   begin
      Emit_Object (null, Id, "GtkTreeStore", TWin.Name.all);
      Emit_Store (TWin, Id);
      Emit_Line (Sp (Id) & "</object>");
   exception
      when others =>
         TIO.Put_Line ("Emit GtkTreeStore: " & TWin.Name.all);
         raise;
   end Emit_GtkTreeStore;

   -----------------------
   -- Emit_GtkListStore --
   -----------------------

   procedure Emit_GtkListStore (TWin : Window_Pointer; Id : Integer) is
   begin
      if TWin.Associated_Widget.Widget_Type = GtkDataGridView
        and then TWin.Associated_Widget.Has_Expander
      then
         Emit_GtkTreeStore (TWin, Id);
      else
         Emit_Object (null, Id, "GtkListStore", TWin.Name.all);
         Emit_Store (TWin, Id);
         Emit_Line (Sp (Id) & "</object>");
      end if;
   exception
      when others =>
         TIO.Put_Line ("Emit GtkListStore: " & TWin.Name.all);
         raise;
   end Emit_GtkListStore;

   -------------------
   -- Emit_GtkImage --
   -------------------

   procedure Emit_GtkImage (TWin : Window_Pointer;
                            Id   : Integer) is
   begin
      Emit_Object (null, Id, "GtkImage", TWin.Name.all);
      Emit_Name (TWin, Id + 2);
      Emit_Visible_And_Focus (TWin, Id, False);
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

   -------------------------
   -- Emit_GtkEntryBuffer --
   -------------------------

   procedure Emit_GtkEntryBuffer (TWin : Window_Pointer; Id : Integer) is
      use GNAT.Calendar.Time_IO;
      TWdg : constant Widget_Pointer := TWin.Associated_Widget;
   begin
      Emit_Object (null, Id, "GtkEntryBuffer", TWin.Name.all);
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

   -------------------------------
   -- Emit_GtkFileChooserDialog --
   -------------------------------

   procedure Emit_GtkFileChooserDialog (TWin : Window_Pointer; Id : Integer) is
   begin
      Emit_Object (null, Id, "GtkFileChooserDialog", TWin.Name.all);
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
      Emit_Line (Sp (Id + 2) & "<child internal-child=""vbox"">");
      Emit_Object (null, Id + 4, "GtkBox", "");
      Emit_Line (Sp (Id + 6) & "<property name=""can-focus"">False"
                 & "</property>");
      Emit_Line (Sp (Id + 6) & "<property name=""orientation"">vertical"
                 & "</property>");
      Emit_Line (Sp (Id + 6) & "<property name=""spacing"">2</property>");
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
      Emit_Line (Sp (Id + 6) & "<child>");
      Emit_Line (Sp (Id + 8) & "<placeholder/>");
      Emit_Line (Sp (Id + 6) & "</child>");
      Emit_Line (Sp (Id + 4) & "</object>");
      Emit_Line (Sp (Id + 2) & "</child>");

      Emit_Line (Sp (Id) & "</object>");
   end Emit_GtkFileChooserDialog;

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
      Emit_Object (null, Id, "GtkFileFilter", TWin.Name.all);
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

   --------------------
   -- Emit_GtkDialog --
   --------------------

   procedure Emit_GtkDialog_Body (TWin : Window_Pointer;
                                  Id   : Integer;
                                  Activate_Default : Boolean) is
      TWdg : Widget_Pointer;
   begin
      if not TWin.Resizable then
         Emit_Line (Sp (Id) & "<child>");
         if TWin.Name /= null and then TWin.Name.all /= "" then
            Emit_Object (null, Id + 2, "GtkFixed", "GtkFixed_" & TWin.Name.all);
            Emit_Name (TWin, Id + 4);
         else
            Emit_Object (null, Id + 2, "GtkFixed", "");
         end if;
         Emit_Property (Id + 4, "visible", True);
         Emit_Property (Id + 4, "can-focus", False);
         Emit_Margin (TWin, Id + 4);
      end if;

      TWdg := TWin.Widget_List;
      while TWdg /= null loop
         case TWdg.Widget_Type is
            when No_Widget =>
               null;
            when GtkBox =>
               Emit_GtkBox (TWdg, Id + 4, -1);

            when GtkMenuBar =>
               null;
            when GtkSeparatorMenuItem =>
               null;
            when GtkMenuItem | GtkSubMenu =>
               null;
            when GtkMenuNormalItem =>
               null;
            when GtkMenuImageItem =>
               null;
            when GtkMenuRadioItem =>
               null;
            when GtkMenuCheckItem =>
               null;

            when GtkDataGridView =>
               Emit_GtkDataGridView (TWdg, Id + 4);
            when GtkTreeGridView =>
               Emit_GtkTreeGridView (TWdg, Id + 4);
            when ExpandableColumn | DataGridViewTextBoxColumn =>
               null; --  handled within GtkData/TreeGridView
            when DataGridViewCheckBoxColumn =>
               null; --  handled within GtkData/TreeGridView

            when GtkNoteBook =>
               Emit_GtkNoteBook (TWdg, Id + 4, 0);

            when GtkTabChild => null;
            when GtkTabPage => null;

            when GtkEntry =>
               Emit_GtkEntry (TWdg, Id + 4, Activate_Default);
            when GtkComboTextBox =>
               Emit_GtkComboTextBox (TWdg, Id + 4,
                                     Packing => True,
                                     Activate_Default => Activate_Default);
            when GtkCalendar =>
               if TWdg.Is_DatePicker then
                  Emit_DatePicker (TWdg, Id + 4);
               else
                  Emit_TimePicker (TWdg, Id + 4);
               end if;
            when GtkSpinButton =>
               Emit_GtkSpinButton (TWdg, Id + 4);
            when GtkFileChooserButton =>
               Emit_GtkFileChooserButton (TWdg, Id + 4);
            when PrintDocument        => null;
            when PrintDialog          => null;
            when PageSetupDialog      => null;
            when FolderBrowserDialog  => null;
            when GtkToolTip           => null;
            when GtkColorButton =>
               Emit_GtkColorButton (TWdg, Id + 4);
            when GtkStatusBar =>
               Emit_GtkStatusBar (TWdg, Id + 4, 0);
            when GtkToolBar =>
               Emit_GtkToolBar (TWdg, Id + 4, 0);
            when BackgroundWorker     => null;
            when GtkListBox =>
               Emit_GtkListBox (TWdg, Id + 4);
            when GtkLabel =>
               Emit_GtkLabel (TWdg, Id + 4, Packing => True);
            when ToolStripStatusLabel =>
               Emit_GtkLabel (TWdg, Id + 4, Packing => True);
            when GtkImage =>
               Emit_GtkImage (TWdg, Id + 4);
            when GtkButton =>
               if (TWin.Accept_Button /= TWdg
                   and TWin.Cancel_Button /= TWdg)
               then
                  Emit_GtkButton (TWdg, Id + 4, "GtkButton",
                                  Position    => -1,
                                  Has_Default => False,
                                  XY          => True,
                                  Homog       => False);
               end if;
            when GtkRadioButton =>
               Emit_GtkRadioButton (TWdg, Id + 4, "GtkRadioButton",
                                    Underline => TWdg.Underline,
                                    XY => True, Homog => False);
            when GtkCheckButton =>
               Emit_GtkCheckButton (TWdg, Id + 4);
            when GtkToggleButton =>
               Emit_GtkToggleButton (TWdg, Id + 4, "GtkToggleButton",
                                     Underline => TWdg.Underline,
                                     XY => True, Homog => False);
            when GtkFrame =>
               Emit_GtkFrame (TWdg, Id + 4);
            when Chart                => null;
            when GtkSeparatorToolItem => null;
            when BindingNavigator =>
               Emit_GtkToolBar (TWdg, Id + 4, 0);
         end case;
         TWdg := TWdg.Next;
      end loop;
      if not TWin.Resizable then
         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Packing (Id + 2, 0, True, True, 0, True);
         Emit_Line (Sp (Id) & "</child>");
      end if;
   exception
      when others =>
         if TWin.Name /= null and then TWin.Name.all /= "" then
            TIO.Put_Line ("Emit_Fixed_GtkWindow: " & TWin.Name.all);
         else
            TIO.Put_Line ("Emit_Fixed_GtkWindow");
         end if;
         TIO.Put_Line ("Emit Widget_GtkWindow: " & TWdg.Name.all);
         raise;
   end Emit_GtkDialog_Body;

   procedure Emit_GtkDialog (TWin : Window_Pointer; Id : Integer) is
      Spacing : Integer := 0;
   begin
      if TWin.Name /= null and then TWin.Name.all /= "" then
         Emit_Object (null, Id, "GtkDialog", TWin.Name.all);
      else
         Emit_Object (null, Id, "GtkDialog", "");
      end if;
      if TWin.Name /= null and then TWin.Name.all /= "" then
         Emit_Name (TWin, Id + 2);
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
      Emit_Property (Id + 2, "resizable", TWin.Resizable);
      Emit_Property (Id + 2, "modal", TWin.Modal);
      if TWin.Client_Size.Horiz /= -1 then
         Emit_Property (Id + 2, "default-width", TWin.Client_Size.Horiz);
      end if;
      if TWin.Client_Size.Vert /= -1 then
         Emit_Property (Id + 2, "default-height", TWin.Client_Size.Vert);
      end if;
      if TWin.Is_Dialog then
         Emit_Property (Id + 2, "window-position", "center-on-parent");
         Emit_Property (Id + 2, "type-hint", "dialog");
         Emit_Property (Id + 2, "gravity", "center");
      end if;
      Emit_GtkSignal (TWin, Id + 2);

      if TWin.Accept_Button /= null then
         Spacing := Spacing + 1;
      end if;
      if TWin.Cancel_Button /= null then
         Spacing := Spacing + 1;
      end if;

      Emit_Line (Sp (Id + 2) & "<child internal-child=""vbox"">");
      Emit_Object (null, Id + 4, "GtkBox", "");
      Emit_Property (Id + 6, "can-focus", False);
      Emit_Property (Id + 6, "orientation", "vertical");
      if Spacing > 0 then
         Emit_Property (Id + 6, "spacing", Spacing);
         Emit_Line (Sp (Id + 6) & "<child internal-child=""action_area"">");
         Emit_Object (null, Id + 8, "GtkButtonBox", "");
         Emit_Property (Id + 10, "can-focus", False);
         Emit_Line (Sp (Id + 10) & "<property name=""layout-style"">"
                    & "spread</property>");
         if TWin.Cancel_Button /= null then
            Emit_GtkButton (TWin.Cancel_Button, Id + 10, "GtkButton",
                            Position    => 0,
                            Has_Default => (TWin.Accept_Button = null),
                            XY          => False,
                            Homog       => False);
         end if;
         if TWin.Accept_Button /= null then
            Emit_GtkButton (TWin.Accept_Button, Id + 10, "GtkButton",
                            Position    =>
                              (if TWin.Cancel_Button = null then 0 else 1),
                            Has_Default => True,
                            XY          => False,
                            Homog       => False);
         end if;
         Emit_Line (Sp (Id + 8) & "</object>");
         Emit_Packing (Id + 8, Expand => False, Fill => False,
                       Padding => 0,
                       Position => 0, Pack_Start => True);
         Emit_Line (Sp (Id + 6) & "</child>");

         Emit_GtkDialog_Body (TWin, Id + 4, True);

      else
         Emit_GtkDialog_Body (TWin, Id + 4, False);
      end if;
      Emit_Line (Sp (Id + 4) & "</object>");
      Emit_Line (Sp (Id + 2) & "</child>");

      if Spacing > 0 then
         Emit_Line (Sp (Id + 2) & "<action-widgets>");
         if TWin.Cancel_Button /= null then
            Emit_Line (Sp (Id + 4) & "<action-widget response="""
                       & To_Gtk (TWin.Cancel_Button.Dialog_Result)
                       & """>"
                       & TWin.Cancel_Button.Name.all
                       & "</action-widget>");
         end if;
         if TWin.Accept_Button /= null then
            Emit_Line (Sp (Id + 4) & "<action-widget response="""
                       & To_Gtk (TWin.Accept_Button.Dialog_Result)
                       & """>"
                       & TWin.Accept_Button.Name.all
                       & "</action-widget>");
         end if;
         Emit_Line (Sp (Id + 2) & "</action-widgets>");
      end if;
      Emit_Line (Sp (Id) & "</object>");
   end Emit_GtkDialog;

   -------------------------
   -- Emit_Main_GtkWindow --
   -------------------------

   procedure Emit_Main_GtkWindow_Body (TWin : Window_Pointer; Id : Integer) is
      TWdg : Widget_Pointer;
   begin
      TWdg := TWin.Widget_List;
      while TWdg /= null loop
         case TWdg.Widget_Type is
            when No_Widget =>
               null;

            when GtkBox =>
               Emit_GtkBox (TWdg, Id, -2);

            when GtkMenuBar =>
               Emit_GtkMenuBar (TWdg, Id, -1);
            when GtkSeparatorMenuItem =>
               null;
            when GtkMenuItem | GtkSubMenu =>
               null;
            when GtkMenuNormalItem =>
               null;
            when GtkMenuImageItem =>
               null;
            when GtkMenuRadioItem =>
               null;
            when GtkMenuCheckItem =>
               null;

            when GtkDataGridView =>
               Emit_GtkDataGridView (TWdg, Id);
            when GtkTreeGridView =>
               Emit_GtkTreeGridView (TWdg, Id);
            when ExpandableColumn | DataGridViewTextBoxColumn =>
               null;
            when DataGridViewCheckBoxColumn =>
               null;

            when GtkNoteBook =>
               Emit_GtkNoteBook (TWdg, Id, 0);

            when GtkTabChild => null;
            when GtkTabPage => null;

            when GtkEntry =>
               Emit_GtkEntry (TWdg, Id, False);
            when GtkComboTextBox =>
               Emit_GtkComboTextBox (TWdg, Id,
                                     Packing => True,
                                     Activate_Default => False);
            when GtkCalendar =>
               if TWdg.Is_DatePicker then
                  Emit_DatePicker (TWdg, Id);
               else
                  Emit_TimePicker (TWdg, Id);
               end if;
            when GtkSpinButton =>
               Emit_GtkSpinButton (TWdg, Id);
            when GtkFileChooserButton =>
               Emit_GtkFileChooserButton (TWdg, Id);
            when PrintDocument        => null;
            when PrintDialog          => null;
            when PageSetupDialog      => null;
            when FolderBrowserDialog  => null;
            when GtkToolTip           => null;
            when GtkColorButton =>
               Emit_GtkColorButton (TWdg, Id);
            when GtkStatusBar =>
               Emit_GtkStatusBar (TWdg, Id, 0);
            when GtkToolBar =>
               Emit_GtkToolBar (TWdg, Id, 0);
            when BackgroundWorker     => null;
            when GtkListBox =>
               Emit_GtkListBox (TWdg, Id);
            when GtkLabel =>
               Emit_GtkLabel (TWdg, Id, Packing => True);
            when ToolStripStatusLabel =>
               Emit_GtkLabel (TWdg, Id, Packing => True);
            when GtkImage =>
               Emit_GtkImage (TWdg, Id);
            when GtkButton =>
               Emit_GtkButton (TWdg, Id, "GtkButton",
                               Position    => -1,
                               Has_Default => False,
                               XY          => True,
                               Homog       => False);
            when GtkRadioButton =>
               Emit_GtkRadioButton (TWdg, Id, "GtkRadioButton",
                                    Underline => TWdg.Underline,
                                    XY => True, Homog => False);
            when GtkCheckButton =>
               Emit_GtkCheckButton (TWdg, Id);
            when GtkToggleButton =>
               Emit_GtkToggleButton (TWdg, Id, "GtkToggleButton",
                                     Underline => TWdg.Underline,
                                     XY => True, Homog => False);
            when GtkFrame =>
               Emit_GtkFrame (TWdg, Id);
            when Chart                => null;
            when GtkSeparatorToolItem => null;
            when BindingNavigator =>
               Emit_GtkToolBar (TWdg, Id, 0);
         end case;
         TWdg := TWdg.Next;
      end loop;
   end Emit_Main_GtkWindow_Body;

   procedure Emit_Main_GtkWindow (TWin : Window_Pointer; Id : Integer) is
   begin
      if TWin.Name /= null and then TWin.Name.all /= "" then
         Emit_Object (null, Id, "GtkWindow", TWin.Name.all);
      else
         Emit_Object (null, Id, "GtkWindow", "");
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
      Emit_Property (Id + 2, "resizable", TWin.Resizable);
      Emit_Property (Id + 2, "modal", TWin.Modal);
      if TWin.Client_Size.Horiz /= -1 then
         Emit_Property (Id + 2, "default-width", TWin.Client_Size.Horiz);
      end if;
      if TWin.Client_Size.Vert /= -1 then
         Emit_Property (Id + 2, "default-height", TWin.Client_Size.Vert);
      end if;
      Emit_GtkSignal (TWin, Id + 2);

      Emit_Main_GtkWindow_Body (TWin, Id + 2);
      Emit_Line (Sp (Id) & "</object>");
   end Emit_Main_GtkWindow;

   --------------------
   -- Emit_GtkWindow --
   --------------------
   procedure Emit_GtkWindow (TWin : Window_Pointer; Id : Integer) is
   begin
      if TWin.Is_Dialog then
         Emit_GtkDialog (TWin, Id);
      else
         Emit_Main_GtkWindow (TWin, Id + 2);
      end if;
   end Emit_GtkWindow;

end W2gtk_Emit;
