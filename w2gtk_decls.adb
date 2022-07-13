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
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body W2gtk_Decls is
   package ITIO renames Ada.Integer_Text_IO;
   package FTIO renames Ada.Float_Text_IO;

   Test3  : constant String := "<value>";
   Test4  : constant String := "</value>";
   Test6  : constant String := "<value />";

   --------
   -- sp --
   --------

   function Sp (N : Integer) return String is
   begin
      return (1 .. N => ' ');
   end Sp;

   -----------
   -- Debug --
   -----------

   procedure Debug (NLin : Integer; Msg : String) is
   begin
      if Use_Debug then
         ITIO.Put (NLin, Width => 5);
         TIO.Put_Line (": " & Msg);
      end if;
   end Debug;

   ---------
   -- Img --
   ---------

   function Img (X : Integer) return String is
      Z : constant String := X'Image;
   begin
      if X >= 0 then
         return Z (Z'First + 1 .. Z'Last);
      else
         return Z;
      end if;
   end Img;

   function Img (X : Float) return String is
      Z : constant String := X'Image;
   begin
      if X >= 0.0 then
         return Z (Z'First + 1 .. Z'Last);
      else
         return Z;
      end if;
   end Img;

   -------------
   -- Convert --
   -------------

   function Convert (WType : String) return Widget_Enum is
   begin
      if Contains (WType, "Label") then
         return GtkLabel;
      elsif Contains (WType, "PictureBox") then
         return GtkImage;
      elsif Contains (WType, "NumericUpDown") then
         return GtkSpinButton;
      elsif Contains (WType, "ToolTip") then
         return GtkToolTip;
      elsif Contains (WType, "GroupBox") then
         return GtkFrame;
      elsif Contains (WType, "ListBox") then
         return GtkListBox;
      elsif Contains (WType, "TextBox") then
         return GtkEntry;
      elsif Contains (WType, "ComboBox") then
         return GtkComboBox;
      elsif Contains (WType, "RadioButton") then
         return GtkRadioButton;
      elsif Contains (WType, "ToggleButton") then
         return GtkToggleButton;
      elsif Contains (WType, "ToolStripButton") then
         return GtkButton;
      elsif Contains (WType, "ColorDialog") then
         return GtkColorButton;
      elsif Contains (WType, "Button") then --  must be after other buttons
         return GtkButton;
      elsif Contains (WType, "CheckBox") then
         return GtkCheckButton;
      elsif Contains (WType, "Forms.Panel") then
         return GtkFrame;
      elsif Contains (WType, "Forms.DateTimePicker") then
         return GtkCalendar;
      elsif Contains (WType, "PrintDocument") then
         return PrintDocument;
      elsif Contains (WType, "PrintDialog") then
         return PrintDialog;
      elsif Contains (WType, "OpenFileDialog") then
         return GtkFileChooserButton;
      elsif Contains (WType, "FolderBrowserDialog") then
         return FolderBrowserDialog;
      elsif Contains (WType, "StatusStrip") then
         return GtkStatusBar;
      elsif Contains (WType, "ToolStripStatusLabel") then
         return ToolStripStatusLabel;
      elsif Contains (WType, "ToolStripSeparator") then
         return GtkSeparatorToolItem;
      elsif Contains (WType, "ToolStrip") then
         return GtkToolBar;
      elsif Contains (WType, "Chart") then
         return Chart;
      else
         return None;
      end if;
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (WStyle : String) return Display_Style is
   begin
      if Contains (WStyle, "DisplayStyle.Text") then
         return Text_Only;
      elsif Contains (WStyle, "DisplayStyle.Image") then
         return Icons_Only;
      elsif Contains (WStyle, "DisplayStyle.ImageAndText") then
         return Text_Below_Icons;
      else
         return Unset;
      end if;
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (IAlign : String) return Image_Position_Enum is
   begin
      if Contains (IAlign, "BottomCenter") then
         return Bottom;
      elsif Contains (IAlign, "BottomLeft") then
         return Left;
      elsif Contains (IAlign, "BottomRight") then
         return Right;
      elsif Contains (IAlign, "MiddleCenter") then
         return Top;
      elsif Contains (IAlign, "MiddleLeft") then
         return Left;
      elsif Contains (IAlign, "MiddleRight") then
         return Right;
      elsif Contains (IAlign, "TopCenter") then
         return Top;
      elsif Contains (IAlign, "TopLeft") then
         return Left;
      elsif Contains (IAlign, "TopRight") then
         return Right;
      else
         return Top;
      end if;
   end Convert;

   --------------
   -- To_Color --
   --------------

   function To_Color (WColor : String) return String is
      Idx0 : Integer;
   begin
      Idx0 := Index (Source  => WColor,
                     Pattern => ".",
                     Going   => Ada.Strings.Backward);
      if Idx0 in WColor'Range then
         if WColor (Idx0 + 1 .. WColor'Last) = "Control" then
            return "white";
         else
            return WColor (Idx0 + 1 .. WColor'Last);
         end if;
      else
         return "black";
      end if;
   end To_Color;

   ---------------
   -- Icon_Name --
   ---------------

   function Icon_Name (Src : String) return String is
      Idx0 : Integer;
   begin
      Idx0 := Index (Source  => Src,
                     Pattern => ".",
                     Going   => Ada.Strings.Backward);
      if Idx0 in Src'Range then
         return Src (Idx0 + 1 .. Src'Last) & ".png";
      else
         return "";
      end if;
   end Icon_Name;

   -----------------
   -- Find_Widget --
   -----------------
   --  do not call this procedure after Reorder_Widgets

   function Find_Widget (Start : Widget_Pointer;
                         WType : Widget_Enum) return Widget_Pointer is
      WT : Widget_Pointer := Start;
   begin
      while WT /= null loop
         if WT.Widget_Type = WType then
            return WT;
         end if;
         WT := WT.Next;
      end loop;
      return null;
   end Find_Widget;
   function Find_Widget (Start : Widget_Pointer;
                         Name  : String) return Widget_Pointer is
      WT : Widget_Pointer := Start;
   begin
      while WT /= null loop
         if WT.Name /= null and then WT.Name.all = Name then
            return WT;
         end if;
         WT := WT.Next;
      end loop;
      return null;
   end Find_Widget;

   -------------------
   -- Insert_Widget --
   -------------------

   procedure Insert_Widget (Parent : Window_Pointer;
                            WT     : Widget_Pointer) is
      Temp : Widget_Pointer;
   begin
      if Parent.Widget_List = null then
         Parent.Widget_List := WT;
         WT.Next := null;
         WT.Prev := null;
         return;
      end if;
      Temp := Parent.Widget_List;
      loop
         exit when Temp.Next = null;
         Temp := Temp.Next;
      end loop;
      WT.Prev   := Temp;
      Temp.Next := WT;
      WT.Next := null;
   end Insert_Widget;

   procedure Insert_Widget (Parent : Widget_Pointer;
                            WT     : Widget_Pointer) is
      Temp : Widget_Pointer;
   begin
      if Parent.Child_List = null then
         Parent.Child_List := WT;
         WT.Next := null;
         return;
      end if;
      Temp := Parent.Child_List;
      loop
         exit when Temp.Next = null;
         Temp := Temp.Next;
      end loop;
      Temp.Next := WT;
      WT.Next := null;
   end Insert_Widget;

   -------------------
   -- Insert_Signal --
   -------------------
   procedure Insert_Signal (TWin : Window_Pointer;
                            TS   : Signal_Pointer) is
   begin
      if TWin.Signal_List = null then
         TS.Next := null;
         TWin.Signal_List := TS;
      else
         TS.Next := TWin.Signal_List;
         TWin.Signal_List := TS;
      end if;
   end Insert_Signal;

   procedure Insert_Signal (TWdg : Widget_Pointer;
                            TS   : Signal_Pointer) is
   begin
      if TWdg.Signal_List = null then
         TS.Next := null;
         TWdg.Signal_List := TS;
      else
         TS.Next := TWdg.Signal_List;
         TWdg.Signal_List := TS;
      end if;
   end Insert_Signal;

   -------------------
   -- Insert_Window --
   -------------------

   procedure Insert_Window (TWin : Window_Pointer) is
      Temp : Window_Pointer;
   begin
      if Win_List = null then
         Win_List := TWin;
         TWin.Next := null;
         return;
      end if;
      Temp := Win_List;
      loop
         exit when Temp.Next = null;
         Temp := Temp.Next;
      end loop;
      Temp.Next := TWin;
      TWin.Next := null;
   end Insert_Window;

   procedure Insert_Front_Window (TWin : Window_Pointer) is
   begin
      TWin.Next := Win_List;
      Win_List  := TWin;
   end Insert_Front_Window;

   --------------------
   -- Extract_Widget --
   --------------------

   procedure Extract_Widget (TWin : Window_Pointer;
                             WT   : Widget_Pointer);
   procedure Extract_Widget (TWin : Window_Pointer;
                             WT   : Widget_Pointer) is
   begin
      if TWin.Widget_List = WT then       --  first item
         if WT.Next = null then           --  and last item
            TWin.Widget_List := null;
         else                             --  first but more items
            WT.Next.Prev := null;
            TWin.Widget_List := WT.Next;
         end if;
      elsif WT.Next = null then           --  last item (cannot be first)
         WT.Prev.Next := null;
      else                                --  midle item
         WT.Prev.Next := WT.Next;
         WT.Next.Prev := WT.Prev;
      end if;
      WT.Next := null;
      WT.Prev := null;
   end Extract_Widget;

   ---------------------
   -- Reparent_Widget --
   ---------------------

   procedure Relink_To_Containers (TWin : Window_Pointer) is
      TWdg : Widget_Pointer := TWin.Widget_List;
      Temp : Widget_Pointer;
   begin
      while TWdg /= null loop
         if TWdg.GParent /= null then
            Temp := TWdg;
            TWdg := TWdg.Next;
            Extract_Widget (TWin, Temp);
            Insert_Widget (Temp.GParent, Temp);
            Debug (NLin, "Widget " & Temp.Name.all
                   & " linked to Container " & Temp.GParent.Name.all);
         else
            TWdg := TWdg.Next;
         end if;
      end loop;
   end Relink_To_Containers;

   -------------------------
   -- Process_Inheritable --
   -------------------------

   procedure Process_Inheritable (TWin : Window_Pointer) is
      TWdg  : Widget_Pointer;
      --  TWdg1 : Widget_Pointer;
   begin
      --  inherits font
      TWdg := TWin.Widget_List;
      while TWdg /= null loop
         case TWdg.Widget_Type is
            when GtkImage =>
               null;
            when others =>
               if TWdg.Font_Name = null and then TWin.Font_Name /= null
               then
                  TWdg.Font_Name := new String'(TWin.Font_Name.all);
               end if;
               if TWdg.Font_Size = null and then TWin.Font_Size /= null
               then
                  TWdg.Font_Size := new String'(TWin.Font_Size.all);
               end if;
               if TWdg.Font_Weight = null and then TWin.Font_Weight /= null
               then
                  TWdg.Font_Weight := new String'(TWin.Font_Weight.all);
               end if;
               Debug (NLin, "Set Inherited Property "
                      & TWdg.Name.all & ".Font="""
                      & TWdg.Font_Name.all & """");
         end case;
         TWdg := TWdg.Next;
      end loop;
   end Process_Inheritable;

   --------------
   -- Contains --
   --------------

   function Contains (Source : String; Pattern : String) return Boolean is
      Idx0  : Integer;
   begin
      Idx0 := Index (Source, Pattern);
      return (Idx0 in Source'Range);
   end Contains;

   -----------------
   -- Get_Boolean --
   -----------------

   function Get_Boolean (F : TIO.File_Type) return Boolean is
      Data : constant String := Get_String (F);
   begin
      if Contains (Data, "True") then
         return True;
      elsif Contains (Data, "False") then
         return False;
      else
         TIO.Put_Line ("Line" & NLin'Image &
                         ": wrong boolean: " & Data);
         raise TIO.Data_Error;
      end if;
   end Get_Boolean;

   -----------------
   -- Get_Integer --
   -----------------

   function Get_Integer (Data : String) return Integer is
      Last : Integer;
      Num  : Integer;
   begin
      ITIO.Get (Data, Num, Last);
      return Num;
   exception
      when TIO.Data_Error =>
         TIO.Put_Line ("Line" & NLin'Image &
                         ": wrong first number: " & Data);
         raise;
   end Get_Integer;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (F : TIO.File_Type) return String is
      Idx0  : Integer;
      Idx1  : Integer;
   begin
      TIO.Get_Line (F, Line, Len);
      NLin := NLin + 1;
      Idx0 := Index (Line (1 .. Len), Test3);
      if Idx0 in 1 .. Len then
         Idx1 := Index (Line (Idx0 + Test3'Length .. Len), Test4);
         if Idx1 in Idx0 + Test3'Length .. Len then
            return Line (Idx0 + Test3'Length .. Idx1 - 1);
         else
            TIO.Put_Line ("Line" & NLin'Image & ": missing " & Test4);
            raise TIO.Data_Error;
         end if;
      end if;
      Idx0 := Index (Line (1 .. Len), Test6);
      if Idx0 in 1 .. Len then
         return "";
      end if;
      TIO.Put_Line ("Line" & NLin'Image & ": unable to parse it");
      raise TIO.Data_Error;
   end Get_String;

   ---------------------
   -- Get_Widget_Name --
   ---------------------

   function Get_Widget_Name (F : TIO.File_Type) return String is
      Idx0 : Integer;
      Data : constant String := Get_String (F);
   begin
      Idx0 := Index (Data, ",");
      if Idx0 in Data'Range then
         return Data (Data'First .. Idx0 - 1);
      end if;
      TIO.Put_Line ("Line" & NLin'Image & ": unknown widget name");
      raise TIO.Data_Error;
   end Get_Widget_Name;

   --------------
   -- Get_Pair --
   --------------

   function Get_Pair (Data : String) return Pair is
      P    : Pair;
      Last : Integer;
   begin
      if Data = "" then
         TIO.Put_Line ("Line" & NLin'Image & ": empty first number");
         raise TIO.Data_Error;
      end if;
      begin
         ITIO.Get (Data, P.One, Last);
      exception
         when TIO.Data_Error =>
            TIO.Put_Line ("Line" & NLin'Image &
                            ": wrong first number: " & Data);
            raise;
      end;
      begin
         ITIO.Get (Data (Last + 2 .. Data'Last), P.Two, Last);
      exception
         when TIO.Data_Error =>
            TIO.Put_Line ("Line" & NLin'Image &
                            ": wrong second number: " &
                            Data (Last + 1 .. Data'Last));
            raise;
      end;
      return P;
   end Get_Pair;

   ----------------------
   -- Get_Margin_Array --
   ----------------------

   function Get_Margin_Array (F : TIO.File_Type) return Margin_Array is
      Data : constant String := Get_String (F);
      P    : Pair;
      Q    : Pair;
      Idx0  : Integer;
      Idx1  : Integer;
   begin
      P    := Get_Pair (Data);
      Idx0 := Index (Data, ",");
      Idx1 := Index (Data (Idx0 + 1 .. Data'Last), ",");
      if not (Idx1 in Idx0 + 1 .. Data'Last) then
         TIO.Put_Line ("Line" & NLin'Image &
                         ": wrong other numbers: " &
                         Data (Idx1 + 1 .. Data'Last));
         raise TIO.Data_Error;
      end if;
      Q := Get_Pair (Data (Idx1 + 1 .. Data'Last));
      --  1 => top, 2 => start or left, 3 => bottom, 4 => end or right
      return Margin_Array'(1 => P.One, 2 => P.Two,
                           3 => Q.One, 4 => Q.Two);
   end Get_Margin_Array;

   --------------
   -- Get_Font --
   --------------

   procedure Get_Font (F           : TIO.File_Type;
                       Font_Name   : in out String_Access;
                       Font_Size   : in out String_Access;
                       Font_Weight : in out String_Access) is
      Data  : constant String := Get_String (F);
      Idx0  : Integer;
      Idx1  : Integer;
      Idx2  : Integer;
      Last  : Integer;
      Num   : Float;
      FSize : Integer;
   begin
      Idx0 := Index (Data, ",");
      if not (Idx0 in Data'Range)
        or else Idx0 = Data'First
      then
         raise TIO.Data_Error;
      end if;
      if Data (Data'First .. Idx0 - 1) = "Calibri" then
         Font_Name := new String'("Sans");
      else
         Font_Name := new String'(Data (Data'First .. Idx0 - 1));
      end if;

      Idx1 := Index (Data (Idx0 + 1 .. Data'Last), "pt");
      if not (Idx1 in Idx0 + 1 .. Data'Last) then
         raise TIO.Data_Error;
      end if;
      FTIO.Get (Data (Idx0 + 1 .. Idx1 - 1), Num, Last);
      FSize := Integer (Num);
      Font_Size := new String'(Img (FSize));

      Idx2 := Index (Data (Idx1 + 1 .. Data'Last), ", style=");
      if Idx2 in Idx1 + 1 .. Data'Last - 1 then
         Font_Weight := new String'(To_Lower (Data (Idx2 + 8 .. Data'Last)));
      else
         Font_Weight := null;
      end if;
   end Get_Font;

end W2gtk_Decls;
