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

package Emit_Containers is

   procedure Emit_Widget_Child (Child : Widget_Pointer;
                                Id    : Integer;
                                From_ButtonBox : Boolean := False;
                                Omit_Child     : Boolean := False);
   procedure Emit_GtkBox (TWdg : Widget_Pointer;
                          Id   : Integer;
                          Omit_Child : Boolean := False);
   procedure Emit_GtkNoteBook (TWdg : Widget_Pointer;
                               Id   : Integer);
   procedure Emit_GtkFrame (TWdg : Widget_Pointer;
                            Id   : Integer);
   procedure Emit_GtkAspectFrame (TWdg : Widget_Pointer;
                                  Id : Integer);
   procedure Emit_GtkListBox (TWdg : Widget_Pointer;
                              Id   : Integer);
   procedure Emit_GtkFlowBox;
   procedure Emit_GtkOverlay;
   procedure Emit_GtkMenuBar (TWdg : Widget_Pointer;
                              Id   : Integer);
   procedure Emit_GtkToolBar (TWdg : Widget_Pointer;
                              Id   : Integer);
   procedure Emit_GtkStatusBar (TWdg : Widget_Pointer;
                                Id   : Integer;
                                Pos  : Integer);
   procedure Emit_GtkToolPalette;
   procedure Emit_GtkPaned;
   procedure Emit_GtkButtonBox (TWdg : Widget_Pointer;
                                Id   : Integer);
   procedure Emit_GtkLayout;
   procedure Emit_GtkFixed (TWdg : Widget_Pointer;
                            Id   : Integer);
   procedure Emit_GtkEventBox;
   procedure Emit_GtkExpander;
   procedure Emit_GtkViewport;
   procedure Emit_ScrolledWindow (TWdg : Widget_Pointer;
                                  Id   : Integer);
   procedure Emit_Alignment (TWdg : Widget_Pointer;
                             Id   : Integer);
   procedure Emit_GtkRevealer;
   procedure Emit_GtkSearchBar;
   procedure Emit_GtkHeaderBar;
   procedure Emit_GtkStack;
   procedure Emit_GtkPopover;
   procedure Emit_GtkPopoverMenu;
   procedure Emit_GtkActionBar;
end Emit_Containers;
