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
with Ada.Text_IO;
with Ada.Calendar;
with GNAT.Strings; use GNAT.Strings;
with GNATCOLL.Tribooleans;  use GNATCOLL.Tribooleans;

package W2gtk_Decls is
   package TIO renames Ada.Text_IO;
   package AC  renames Ada.Calendar;

   type Margin_Array is array (Integer range 1 .. 4) of Integer;
   Null_Margin : constant Margin_Array := (others => -1);
   type Location_Pair is record
      From_Top  : Integer := -1;
      From_Left : Integer := -1;
   end record;
   type Size_Pair is record
      Horiz : Integer := -1;
      Vert  : Integer := -1;
   end record;
   type Pair is record
      One : Integer;
      Two : Integer;
   end record;
   type Window_Position_Enum is (None, CenterParent);
   type Image_Position_Enum is (Left, Right, Top, Bottom);

   type Display_Style is (Unset,
                          Icons_Only,         --  icons
                          Text_Only,          --  text
                          Text_Beside_Icons,  --  both horiz
                          Text_Below_Icons);  --  both

   type Signal_Block;
   type Signal_Pointer is access all Signal_Block;
   type Signal_Block is record
      Next    : Signal_Pointer;
      Name    : String_Access := null;
      Handler : String_Access := null;
      Line    : Integer;
   end record;

   type Window_Enum is (GtkWindow,
                        GtkFileChooserDialog,
                        GtkFileFilter,
                        GtkEntryBuffer,
                        GtkListStore,
                        GtkImage);
   type Window_Properties;
   type Window_Pointer is access all Window_Properties;
   type Widget_Properties;
   type Widget_Pointer is access all Widget_Properties;

   type Window_Properties (Window_Type : Window_Enum) is record
      Next           : Window_Pointer;
      Name           : String_Access := null; --  it is id
      Title          : String_Access := null;
      Signal_List    : Signal_Pointer;
      case Window_Type is
         when GtkWindow =>
            Font_Name      : String_Access := null;
            Font_Size      : String_Access := null;
            Font_Weight    : String_Access := null;
            Icon           : String_Access := null;
            ToolTip        : String_Access := null;
            Start_Position : Window_Position_Enum := None;
            Client_Size    : Size_Pair;
            Margins        : Margin_Array  := Null_Margin;
            AutoScaleDim   : Size_Pair;
            TrayHeight     : Integer := 0;
            BgColor        : String_Access := null;
            FgColor        : String_Access := null;
            Widget_List    : Widget_Pointer;
         when GtkFileChooserDialog =>
            FilterName     : String_Access := null;
            Transient_For  : Window_Pointer;
            Attached_To    : Widget_Pointer;
         when GtkFileFilter =>
            FilterString   : String_Access := null;
         when GtkEntryBuffer | GtkListStore | GtkImage =>
            Associated_Widget :  Widget_Pointer;
      end case;
   end record;
   Win_List : Window_Pointer := null;
   Num_Aux_Widgets : Integer := 0;

   type Widget_Enum is (None, GtkLabel, GtkEntry, GtkComboBox,
                        GtkButton, GtkRadioButton, GtkCheckButton,
                        GtkImage, GtkSpinButton, GtkFrame,
                        GtkStatusBar, GtkFileChooserButton,
                        GtkToolTip, GtkCalendar, GtkColorButton,
                        GtkListBox, GtkMenu,
                        GtkToolBar,
                        GtkSeparatorToolItem,
                        GtkToggleButton,
                        PrintDocument, PrintDialog, Chart,
                        FolderBrowserDialog,
                        ToolStripStatusLabel);

   type Widget_Properties (Widget_Type  : Widget_Enum) is record
      Next         : Widget_Pointer := null;
      Prev         : Widget_Pointer := null;
      Child_List   : Widget_Pointer := null; --  only for containers

      WParent      : Window_Pointer := null; --  will be adjusted later
      Parent_Name  : String_Access  := null;

      GParent      : Widget_Pointer := null; --  parent is gtkframe/gtktoolbar

      Windows_Type : String_Access  := null; --  this is the Windows type

      Name         : String_Access  := null; --  this is the gtk id

      Location     : Location_Pair;
      Size         : Size_Pair;
      TabIndex     : Integer        := -1;
      TabStop      : Boolean        := True;
      Zorder       : Integer        := -1;

      Enabled      : Boolean        := True;  --  sensitive
      Visible      : Boolean        := True;
      Text         : String_Access  := null;
      TextAlign    : String_Access  := null;
      AutoSize     : Triboolean     := Indeterminate;
      Font_Name    : String_Access  := null;
      Font_Size    : String_Access  := null;
      Font_Weight  : String_Access  := null;
      Margins      : Margin_Array   := Null_Margin;
      DStyle       : Display_Style  := Unset; --  labels and buttons
      MaxLength    : Integer        := -1;
      AutoToolTip  : Boolean        := False;
      ToolTip      : String_Access  := null;
      BgColor      : String_Access  := null;
      FgColor      : String_Access  := null;
      UlColor      : String_Access  := null;
      Signal_List  : Signal_Pointer;

      case Widget_Type is
         when GtkEntry | GtkComboBox =>
            Editable     : Boolean        := True;
            Has_Frame    : Boolean        := True;
            PasswordChar : String_Access  := null;

         when GtkSpinButton =>
            StartValue   : Integer := 0;
            MaxValue     : Integer := 0;
            MinValue     : Integer := 0;
            Step         : Integer := 1;

         when GtkFileChooserButton
            | PrintDocument
            | PrintDialog
            | FolderBrowserDialog
            | GtkToolTip
            | GtkColorButton
            | GtkStatusBar
            | GtkToolBar =>

            TrayLocation   : Location_Pair;
            case Widget_Type is
               when GtkFileChooserButton =>
                  OpenFileDialog : String_Access := null;
                  OpenFileFilter : String_Access := null;
                  OpenFileTitle  : String_Access := null;

               when GtkColorButton =>
                  AnyColor : Boolean := False;
                  FullOpen : Boolean := False;
                  Associated_Button : Widget_Pointer;

               when GtkToolBar =>
                  TB_Horiz      : Boolean       := True;
                  Show_Arrows   : Boolean       := True;
                  Grip_Visible  : Boolean       := True;
               when others =>
                  null;
            end case;

         when GtkListBox =>
            MultiSelect : Boolean := False;
            ListStore   : Window_Pointer;

         when GtkCalendar =>
            Start_Date  : Ada.Calendar.Time := Ada.Calendar.Clock;
            MinDate     : Ada.Calendar.Time := AC.Time_Of (1901, 12, 30);
            MaxDate     : Ada.Calendar.Time := AC.Time_Of (2399, 12, 30);
            Format_Date : String_Access     := null;
            ShowUpDown  : Boolean           := True;
            Text_Buffer : Window_Pointer;

         when GtkLabel =>
            BorderStyle : String_Access  := null;

         when GtkImage =>
            Image : String_Access := null;

         when GtkButton | GtkRadioButton | GtkCheckButton | GtkToggleButton =>
            Active      : Boolean             := True;
            Underline   : Boolean             := False;
            ImagePath   : String_Access       := null;
            ImageAlign  : Image_Position_Enum := Left;
            Win_Image   : Window_Pointer      := null;

            case Widget_Type is
               when GtkButton =>
                  Associated_ColorButton : Widget_Pointer;

               when GtkCheckButton =>
                  CheckAlign : String_Access  := null;

               when others => null;
            end case;

         when Chart =>
            Anchor : String_Access := null; --  temporary
         when None => null;
         when GtkFrame => null;
         when GtkMenu => null;
         when GtkSeparatorToolItem => null;
         when ToolStripStatusLabel => null;
      end case;
   end record;

   Use_Debug : Boolean := True;

   function Sp (N : Integer) return String;

   procedure Debug (NLin : Integer; Msg : String);

   function Img (X : Integer) return String;
   function Img (X : Float) return String;

   function Find_Widget (Start : Widget_Pointer;
                         WType : Widget_Enum) return Widget_Pointer;
   function Find_Widget (Start : Widget_Pointer;
                         Name  : String) return Widget_Pointer;
   --  do not call this procedure after Reparent_Widgets

   procedure Relink_To_Containers (TWin : Window_Pointer);
   procedure Process_Inheritable (TWin : Window_Pointer);

   procedure Insert_Widget (Parent : Window_Pointer;
                            WT     : Widget_Pointer);
   procedure Insert_Widget (Parent : Widget_Pointer;
                            WT     : Widget_Pointer);

   procedure Insert_Signal (TWin : Window_Pointer;
                            TS   : Signal_Pointer);
   procedure Insert_Signal (TWdg : Widget_Pointer;
                            TS   : Signal_Pointer);

   procedure Insert_Window (TWin : Window_Pointer);
   procedure Insert_Front_Window (TWin : Window_Pointer);

   function Contains (Source : String; Pattern : String) return Boolean;
   function Get_Integer (Data : String) return Integer;
   function Get_Pair (Data : String) return Pair;
   --  functions specific to RFile
   function Get_String (F : TIO.File_Type) return String;
   function Convert (WType : String) return Widget_Enum;
   function Convert (WStyle : String) return Display_Style;
   function Convert (IAlign : String) return Image_Position_Enum;
   function To_Color (WColor : String) return String;
   function Icon_Name (Src : String) return String;
   function Get_Widget_Name (F : TIO.File_Type) return String;
   function Get_Margin_Array (F : TIO.File_Type) return Margin_Array;
   function Get_Boolean (F : TIO.File_Type) return Boolean;
   procedure Get_Font (F           : TIO.File_Type;
                       Font_Name   : in out String_Access;
                       Font_Size   : in out String_Access;
                       Font_Weight : in out String_Access);


   RFile : TIO.File_Type;
   DFile : TIO.File_Type;
   GFile : TIO.File_Type;
   VFile : TIO.File_Type;

   Line   : String (1 .. 1024);
   Len    : Natural;
   NLin   : Integer;
end W2gtk_Decls;
