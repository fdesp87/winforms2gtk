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
with W2gtk_Decls; use W2gtk_Decls;

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
   procedure Emit_One_GtkSignal (TS   : Signal_Pointer;
                                 Id   : Integer;
                                 Name : String);
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
   --  procedure Emit_Margin (TWin : Window_Pointer; Id : Integer);
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
   procedure Emit_Visible_And_Can_Focus (TWdg  : Widget_Pointer;
                                         Id    : Integer;
                                         Focus : Boolean);
   procedure Emit_WH_Request (TWdg : Widget_Pointer;
                              Id   : Integer);

end Emit_Tools;
