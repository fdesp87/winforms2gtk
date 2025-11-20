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
with Emit_Tools;       use Emit_Tools;
with Emit_Auxiliary;   use Emit_Auxiliary;

package body Emit_Display is

   ---------------------------------------------------------------------------
   -- Emit_GtkLabel --
   ---------------------------------------------------------------------------
   procedure Emit_GtkLabel (TWdg    : Widget_Pointer;
                            Id      : Integer;
                            Packing : Boolean) is
   begin
      if TWdg.BorderStyle /= None then

         Emit_Child (TWdg, Id + 4, False);
         Emit_Object (TWdg, Id + 6, "GtkLabel", TWdg.Name.all);
         Emit_Name (TWdg, Id + 8);
         Emit_Visible_And_Can_Focus (TWdg, Id + 8, False);
         Emit_Property (Id + 8, "has-focus", TWdg.Has_Focus);
         Emit_Align (TWdg, Id + 8, Numeric => False);
         Emit_Label (TWdg, Id + 8, UnderLine => False, Selectable => True);
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

      else
         Emit_Child (TWdg, Id, False);
         Emit_Object (TWdg, Id + 2, "GtkLabel", TWdg.Name.all);
         Emit_Name (TWdg, Id + 4);
         Emit_WH_Request (TWdg, Id + 4);
         Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
         Emit_Property (Id + 4, "has-focus", TWdg.Has_Focus);
         Emit_Margin (TWdg, Id + 4);
         Emit_ToolTip (TWdg, Id + 4);
         Emit_Label (TWdg, Id + 4, UnderLine => False, Selectable => True);
         Emit_Align (TWdg, Id + 4, Numeric => True);
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

   ---------------------------------------------------------------------------
   -- Emit_GtkImage --
   ---------------------------------------------------------------------------
   procedure Emit_GtkImage (TWdg : Widget_Pointer;
                            Id   : Integer) is
   begin
      Emit_Child (TWdg, Id, False);
      Emit_Object (TWdg, Id + 2, "GtkImage", TWdg.Name.all);
      Emit_Name (TWdg, Id + 4);
      Emit_WH_Request (TWdg, Id + 4);
      Emit_Visible_And_Can_Focus (TWdg, Id + 4, Focus => False);
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

   ---------------------------------------------------------------------------
   --  Emit_GtkTreeView
   ---------------------------------------------------------------------------
   procedure Emit_GtkTreeView (TWdg : Widget_Pointer;
                               Id   : Integer;
                               Pos  : Integer) is
   begin
      if TWdg.ScrollBars /= None then
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

         Emit_GtkGridView (TWdg, Id + 4);

         Emit_Line (Sp (Id + 2) & "</object>");
         if Pos > 0 then
            Emit_Line (Sp (Id + 2) & "<packing>");
            Emit_Property (Id + 4, "position", Pos);
            Emit_Line (Sp (Id + 2) & "</packing>");

         end if;
         Emit_Line (Sp (Id) & "</child>");
      else
         Emit_GtkGridView (TWdg, Id + 4);
      end if;
   end Emit_GtkTreeView;

   ---------------------------------------------------------------------------
   --  Emit_GtkDataGridView
   ---------------------------------------------------------------------------
   procedure Emit_GtkDataGridView (TWdg : Widget_Pointer;
                                   Id   : Integer;
                                   Pos  : Integer) is
   begin
      Emit_GtkTreeView (TWdg, Id, Pos);
   end Emit_GtkDataGridView;

   ---------------------------------------------------------------------------
   -- Emit_DatePicker --
   ---------------------------------------------------------------------------
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
         Emit_Visible_And_Can_Focus (TWdg, Id + 4, True);
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
         Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
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
         Emit_Visible_And_Can_Focus (TWdg, Id + 4, True);
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
         Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
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
                    & "<signal name=""next-year"" handler="""
                    & "On_Next_Year_" & TWdg.Name.all
                    & """ object=""" & TWdg.Name.all & "_Button"
                    & """ swapped=""no""/>");
         Emit_Line (Sp (Id + 4)
                    & "<signal name=""prev-month"" handler="""
                    & "On_Prev_Month_" & TWdg.Name.all
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
         Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
         --  Emit_Property (Id + 4, "orientation", "horizontal"); --  DEFAULT

         Emit_Entry (TWdg, Id + 4, 0, 50, 4, "2020", "_Year");
         Emit_Entry (TWdg, Id + 4, 1, 20, 2, "01", "_Month");
         Emit_Entry (TWdg, Id + 4, 2, 20, 2, "01", "_Day");
         if TWdg.ShowUpDown then
            Emit_Button (TWdg, Id + 4, Pos => 4);
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
         Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
         Emit_Line (Sp (Id + 4) & "<property name=""label-xalign"">0"
                    & "</property>");
         Emit_Line (Sp (Id + 4) & "<property name=""shadow-type"">in"
                    & "</property>");
         Emit_Property (Id + 4, "xalign", 0.0);
         Emit_HBox (TWdg, Id + 4);
         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Packing (Id + 2, False, True, 0);
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
         Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
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

   ---------------------------------------------------------------------------
   -- Emit_TimePicker --
   ---------------------------------------------------------------------------
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
         Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
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
         Emit_Visible_And_Can_Focus (TWdg, Id + 4, True);
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
         Emit_Visible_And_Can_Focus (TWdg, Id + 4, True);
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
         Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
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
         Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
         --  Emit_Property (Id + 4, "orientation", "horizontal"); --  DEFAULT

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
         Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
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

end Emit_Display;
