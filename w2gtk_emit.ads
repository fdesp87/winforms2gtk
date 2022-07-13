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
with W2gtk_Decls; use W2gtk_Decls;

package W2gtk_Emit is
   package TIO renames Ada.Text_IO;

   procedure Emit_GtkHeader (TWin : Window_Pointer; Id : Integer);
   procedure Emit_GtkWindow (TWin : Window_Pointer; Id : Integer);
   procedure Emit_GtkFileFilter (TWin : Window_Pointer; Id : Integer);
   procedure Emit_GtkEntryBuffer (TWin : Window_Pointer; Id : Integer);
   procedure Emit_GtkListStore (TWin : Window_Pointer; Id : Integer);
   procedure Emit_GtkFileChooserDialog (TWin : Window_Pointer; Id : Integer);
   procedure Emit_GtkImage (TWin : Window_Pointer; Id : Integer);
   procedure Emit_GtkTrailer (TWin : Window_Pointer; Id : Integer);

end W2gtk_Emit;
