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
with Emit_Tools;           use Emit_Tools;
with Emit_Containers;      use Emit_Containers;

package body Emit_Internals is

   ------------------------------
   -- Emit_Internal_Child_VBox --
   ------------------------------

   procedure Emit_Internal_Child_VBox (Me : Widget_Pointer;
                                       Id : Integer) is
      Child : Widget_Pointer;
   begin
      Emit_Line (Sp (Id) & "<child internal-child=""vbox"">");
      Debug (-1, Sp (Id) & "internal-child=""vbox""");

      Child := Me.Child_List;
      while Child /= null loop
         Emit_Widget_Child (Child, Id, Omit_Child => True);
         Child := Child.Next;
      end loop;

      Emit_Line (Sp (Id) & "</child>");  --  vbox
   end Emit_Internal_Child_VBox;

   -----------------------------
   -- Emit_Child_Action_Area  --
   -----------------------------

   procedure Emit_Internal_Child_Action_Area (Me : Widget_Pointer;
                                              Id  : Integer) is
      Child : Widget_Pointer := Me.Child_List;
   begin
      Emit_Line (Sp (Id) & "<child internal-child=""action_area"">");
      Debug (-1, Sp (Id + 2) & "internal-child=""action_area""");

      while Child /= null loop
         Emit_Widget_Child (Child, Id);
         Child := Child.Next;
      end loop;

      Emit_Line (Sp (Id) & "</child>");  --  action area
   end Emit_Internal_Child_Action_Area;

   -------------------------
   -- Emit_Action_Widgets --
   -------------------------

   procedure Emit_Action_Widgets (Me : Window_Pointer;
                                  Id : Integer) is
      TWdg : Widget_Pointer;
   begin
      Emit_Line (Sp (Id) & "<action-widgets>");
      Debug (-1, Sp (Id) & "action-widgets");

      for Bt in DialogResult_Enum loop
         if Me.Action_Buttons (Bt) /= null then
            TWdg := Me.Action_Buttons (Bt);
            Emit_Line (Sp (Id + 2) & "<action-widget response="""
                       & To_Gtk (TWdg.Dialog_Result)
                       & """>"
                       & TWdg.Name.all
                       & "</action-widget>");
            Debug (-1, Sp (Id + 2) & "action-widget response for "
                   & TWdg.Name.all & " Button");
         end if;
      end loop;

      Emit_Line (Sp (Id) & "</action-widgets>");
   end Emit_Action_Widgets;

end Emit_Internals;
