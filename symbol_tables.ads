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
with W2gtk_Decls;         use W2gtk_Decls;
package Symbol_Tables is
   Unknown_Signal : exception;

   procedure Initialize;

   function Get_Type (Wdg_Type : String) return Widget_Enum;
   function Get_Property (Win_Prop : String) return Window_Property_Enum;
   function Get_Attribute (Wdg_Attr : String) return Widget_Attribute_Enum;
   function Get_DGVS_Attribute (DGVS_Attr : String) return DGVS_Attribute_Enum;
   function Get_Gtk_Signal (WSignal : String) return String;
   function Convert_Signal_To_Gtk (TWin    : Window_Pointer;
                                   WSignal : String) return String;
   function Convert_Signal_To_Gtk (TWdg    : Widget_Pointer;
                                   WSignal : String) return String;

end Symbol_Tables;
