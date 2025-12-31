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
   procedure Emit_GtkLabel (Me         : Widget_Pointer;
                            Id         : Integer;
                            Packing    : Boolean;
                            Selectable : Boolean) is
   begin
      if Me.BorderStyle /= None then

         Emit_Child (Me, Id + 4, False);
         Emit_Object (Me, Id + 6, "GtkLabel", Me.Name.all);
         Emit_Name (Me, Id + 8);
         Emit_Visible_And_Can_Focus (Me, Id + 8, False);
         Emit_Property (Id + 8, "has-focus", Me.Has_Focus);
         Emit_Align (Me, Id + 8, Numeric => False);
         Emit_Label (Me, Id + 8,
                     UnderLine  => Me.Underline,
                     Selectable => Selectable);
         if Me.MaxLength > 0 then
            Emit_Property (Id + 8, "width-chars", Me.MaxLength);
            Emit_Property (Id + 8, "max-width-chars", Me.MaxLength);
         end if;
         Emit_Attributes (Me, Id + 8);
         Emit_GtkSignal (Me, Id + 8);

         Emit_Line (Sp (Id + 6) & "</object>");
         Emit_Packing_Child (Me, Id + 4,
                             Packing => False,
                             XY      => True,
                             Homog   => False);
         Emit_Line (Sp (Id + 4) & "</child>");

      else
         Emit_Child (Me, Id, False);
         Emit_Object (Me, Id + 2, "GtkLabel", Me.Name.all);
         Emit_Name (Me, Id + 4);
         Emit_WH_Request (Me, Id + 4);
         Emit_Visible_And_Can_Focus (Me, Id + 4, False);
         Emit_Property (Id + 4, "has-focus", Me.Has_Focus);
         Emit_Margin (Me, Id + 4);
         Emit_ToolTip (Me, Id + 4);
         Emit_Label (Me, Id + 4,
                     UnderLine => Me.Underline,
                     Selectable => Selectable);
         Emit_Align (Me, Id + 4, Numeric => True);
         Emit_Attributes (Me, Id + 4);
         Emit_GtkSignal (Me, Id + 4);
         Emit_Line (Sp (Id + 2) & "</object>");
         if Me.Wdg_Parent /= null and then
           Me.Wdg_Parent.Widget_Type = GtkTabChild
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
                                Packing => Packing,
                                XY      => True,
                                Homog   => False);
         end if;
         Emit_Line (Sp (Id) & "</child>");
      end if;
   exception
      when others =>
         TIO.Put_Line ("Emit GtkLabel: " & Me.Name.all);
         raise;
   end Emit_GtkLabel;

   ---------------------------------------------------------------------------
   -- Emit_GtkImage --
   ---------------------------------------------------------------------------
   procedure Emit_GtkImage (Me : Widget_Pointer;
                            Id : Integer) is
   begin
      Emit_Child (Me, Id, False);
      Emit_Object (Me, Id + 2, "GtkImage", Me.Name.all);
      Emit_Name (Me, Id + 4);
      Emit_WH_Request (Me, Id + 4);
      Emit_Visible_And_Can_Focus (Me, Id + 4, Focus => False);
      if Me.Stock then
         Emit_Line (Sp (Id + 4) & "<property name=""stock"">"
                    & Me.Image.all
                    & "</property>");
      else
         Emit_Line (Sp (Id + 4) & "<property name=""pixbuf"">"
                    & Me.Image.all
                    & "</property>");
      end if;
      Emit_Align (Me, Id + 4, Numeric => True);
      Emit_Margin (Me, Id + 4);
      Emit_ToolTip (Me, Id + 4);
      Emit_GtkSignal (Me, Id + 4);
      Emit_Line (Sp (Id + 2) & "</object>");
      Emit_Line (Sp (Id) & "</child>");
      --  if Me.Wdg_Parent.Widget_Type = GtkButton then
      --     Emit_Packing (Id + 2,
      --                   Position   => Me.Child_Number,
      --                   Expand     => False,
      --                   Fill       => True,
      --                   Padding    => Me.Padding,
      --                   Pack_Start => True,
      --                   Force      => True);
      --  else
      Emit_Packing_Child (Me, Id,
                          Packing => True,
                          XY      => True,
                          Homog   => False);
      --  end if;
   exception
      when others =>
         TIO.Put_Line ("Emit GtkImage: " & Me.Name.all);
         raise;
   end Emit_GtkImage;

   ---------------------------------------------------------------------------
   --  Emit_GtkTreeView
   ---------------------------------------------------------------------------
   procedure Emit_GtkTreeView (Me : Widget_Pointer;
                               Id : Integer) is
   begin
      Emit_GtkGridView (Me, Id);
   end Emit_GtkTreeView;

   ---------------------------------------------------------------------------
   --  Emit_GtkDataGridView
   ---------------------------------------------------------------------------
   procedure Emit_GtkDataGridView (Me : Widget_Pointer;
                                   Id : Integer) is
   begin
      Emit_GtkGridView (Me, Id);
   end Emit_GtkDataGridView;

   ---------------------------------------------------------------------------
   -- Emit_DatePicker --
   ---------------------------------------------------------------------------
   procedure Emit_DatePicker (Me : Widget_Pointer;
                              Id : Integer) is

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
         Emit_Packing (Id + 2, Pos, False, False, 0, True);
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
         Emit_Packing (Id + 2, Pos, True, True, 0, True);
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
         Emit_Packing (Id + 2, 1, False, False, 0, True);
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
                      TWdg.Name.all & "_AspectFrame");
         Emit_Name (TWdg.Name.all & "_AspectFrame", Id + 4);
         Emit_Visible_And_Can_Focus (TWdg, Id + 4, False);
         Emit_Line (Sp (Id + 4) & "<property name=""label-xalign"">0"
                    & "</property>");
         Emit_Line (Sp (Id + 4) & "<property name=""shadow-type"">in"
                    & "</property>");
         Emit_Property (Id + 4, "xalign", 0.0);
         Emit_HBox (TWdg, Id + 4);
         Emit_Line (Sp (Id + 2) & "</object>");
         Emit_Packing (Id + 2, 0, False, True, 0, True);
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
      Emit_VBox (Me, Id);
   exception
      when others =>
         TIO.Put_Line ("Emit DatePicker: " & Me.Name.all);
         raise;
   end Emit_DatePicker;

   ---------------------------------------------------------------------------
   -- Emit_TimePicker --
   ---------------------------------------------------------------------------
   procedure Emit_TimePicker (Me : Widget_Pointer;
                              Id : Integer) is

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
         Emit_Packing (Id + 2, Pos, False, False, 0, True);
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
         Emit_Packing (Id + 2, Pos, False, False, 0, True);
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
         Emit_Packing (Id + 2, Pos, False, False, 0, True);
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
                      TWdg.Name.all & "_AspectFrame");
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
      Emit_Aspectframe (Me, Id);
   exception
      when others =>
         TIO.Put_Line ("Emit TimePicker: " & Me.Name.all);
         raise;
   end Emit_TimePicker;

end Emit_Display;
