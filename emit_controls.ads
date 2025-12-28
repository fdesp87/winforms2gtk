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
with W2gtk_Decls;     use W2gtk_Decls;

package Emit_Controls is

   procedure Emit_GtkEntry (Me : Widget_Pointer;
                            Id : Integer);
   procedure Emit_GtkButton (Me          : Widget_Pointer;
                             Id          : Integer;
                             Object      : String;
                             Has_Default : Boolean;
                             XY          : Boolean);
   procedure Emit_GtkSpinButton (Me : Widget_Pointer;
                                 Id : Integer);
   procedure Emit_GtkToggleButton (Me     : Widget_Pointer;
                                   Id     : Integer;
                                   Object : String;
                                   XY     : Boolean;
                                   Homog  : Boolean);
   procedure Emit_GtkCheckButton (Me : Widget_Pointer;
                                  Id : Integer);
   procedure Emit_GtkRadioButton (Me     : Widget_Pointer;
                                  Id     : Integer;
                                  Object : String;
                                  XY     : Boolean;
                                  Homog  : Boolean);
   procedure Emit_GtkColorButton (Me : Widget_Pointer;
                                  Id : Integer);
   procedure Emit_GtkComboTextBox (Me : Widget_Pointer;
                                   Id : Integer);
   procedure Emit_GtkFileChooserButton (Me : Widget_Pointer;
                                        Id : Integer);

end Emit_Controls;
