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
with W2gtk_Decls;             use W2gtk_Decls;
with GNAT.Strings;            use GNAT.Strings;
with GNATCOLL.Tribooleans;    use GNATCOLL.Tribooleans;
with GNATCOLL.Utils;          use GNATCOLL.Utils;

package body W2gtk_Adjust_To_Gtk_Pkg is

   TWin    : Window_Pointer;
   TWdg    : Widget_Pointer;
   TWdg0   : Widget_Pointer;
   Temp    : Widget_Pointer;
   NWin0   : Window_Pointer;
   NWin1   : Window_Pointer;
   NWin2   : Window_Pointer;
   NCol    : Widget_Pointer;
   TS      : Signal_Pointer;
   Found   : Boolean;
   Counter : Integer;
   Result  : Integer;
   Win_List_Aux : Window_Pointer := null;
   B : Boolean;
   From_Top : Integer;

   -------------------------------------------------------
   --  Initially each widget list of a window is plain  --
   -------------------------------------------------------

   -----------------------------------------------------------------------
   procedure Visit_Widget_For_Close_Button_Signal_For_Gtknotebooks
     (This : Widget_Pointer);
   procedure Visit_Widget_For_Close_Button_Signal_For_Gtknotebooks
     (This : Widget_Pointer) is
      NTB    : Widget_Pointer;
      TS     : Signal_Pointer;
      WS     : Signal_Pointer;
      Found  : Boolean := False;
   begin
      if This = null then
         return;
      end if;

      if This.Widget_Type = GtkImage
        and then This.Stock
        and then (This.Wdg_Parent /= null
                  and then
                  This.Wdg_Parent.Widget_Type = GtkButton)
        and then (This.Wdg_Parent.Wdg_Parent /= null
                  and then
                  This.Wdg_Parent.Wdg_Parent.Widget_Type = GtkTabChild)
        and then (This.Wdg_Parent.Wdg_Parent.Wdg_Parent /= null
                  and then
                  This.Wdg_Parent.Wdg_Parent.Wdg_Parent.Widget_Type = GtkTabPage)
        and then (This.Wdg_Parent.Wdg_Parent.Wdg_Parent.Wdg_Parent /= null
                  and then
                  This.Wdg_Parent.Wdg_Parent.Wdg_Parent.Wdg_Parent.Widget_Type
                  = GtkNoteBook)
      then
         NTB := This.Wdg_Parent.Wdg_Parent.Wdg_Parent.Wdg_Parent;
         TS := NTB.Signal_List;
         while TS /= null loop
            if TS.Name.all = "CloseButtonClick" then
               WS := new Signal_Block;
               WS.Name := New_String (TS.Name);
               WS.Handler := New_String (TS.Handler);
               B := Insert_Signal (This.Wdg_Parent, WS, True);  -- ignore duplicates
               Counter := Counter + 1;
               Debug (NLin, Sp (3) & Counter'Image
                      & " Created Synthetic Signal "
                      & WS.Name.all
                      & " [Gtk " & WS.GtkName.all & "]"
                      & " => " & WS.Handler.all
                      & " for widget " & This.Wdg_Parent.Name.all
                      & " [" & This.Widget_Type'Image & "]");
               Found := True;
               exit;
            end if;
            TS := TS.Next;
         end loop;
         if not Found then
            TS := new Signal_Block;
            TS.Name := new String'("CloseButtonClick");
            TS.Handler := new String'("On_" & NTB.Name.all
                                      & "Hide_Tab by_Closebutton");
            B := Insert_Signal (NTB, TS);
            if B then
               Counter := Counter + 1;
               Debug (NLin, Sp (3) & Counter'Image
                      & "Created Synthetic Signal "
                      & NTB.Name.all
                      & " [Gtk " & TS.GtkName.all & "]"
                      & " => " & TS.Handler.all
                      & " for widget " & NTB.Name.all
                      & " [" & NTB.Widget_Type'Image & "]");
               WS := new Signal_Block;
               WS.Name := New_String (TS.Name);
               WS.Handler := New_String (TS.Handler);
               B := Insert_Signal (This.Wdg_Parent, WS, True);  -- ignore duplicates
               Debug (NLin, Sp (3) & Counter'Image
                      & " Created Synthetic Signal "
                      & WS.Name.all
                      & " [Gtk " & WS.GtkName.all & "]"
                      & " => " & WS.Handler.all
                      & " for widget " & This.Name.all
                      & " [" & This.Wdg_Parent.Widget_Type'Image & "]");
            else
               Debug (NLin, Sp (3) & "Unable to create Synthetic Signal "
                      & NTB.Name.all
                      & " [Gtk " & TS.GtkName.all & "]"
                      & " => " & TS.Handler.all
                      & " for widget " & NTB.Name.all
                      & " [" & NTB.Widget_Type'Image & "]");
               raise Program_Error;
            end if;
         end if;
      end if;

      Visit_Widget_For_Close_Button_Signal_For_Gtknotebooks (This.Next);
      Visit_Widget_For_Close_Button_Signal_For_Gtknotebooks (This.Child_List);
   end Visit_Widget_For_Close_Button_Signal_For_Gtknotebooks;

   -----------------------------------------------------------------------
   procedure Visit_GtkTree_Widget_For_GtkBox
     (Parent : in out Widget_Pointer);
   procedure Visit_GtkTree_Widget_For_GtkBox
     (Parent : in out Widget_Pointer) is
      Child : Widget_Pointer;
   begin
      if Parent = null then
         return;
      end if;

      if Parent.Widget_Type = GtkBox
        and then Parent.Num_Children = 1
      then
         Child := Parent.Child_List; --  which has only one element
         if Child.Next /= null or else Child.Prev /= null then
            raise Program_Error;
         end if;
         case Child.Widget_Type is
            when GtkMenuBar | GtkBox | GtkToolBar
               | GtkNoteBook | BindingNavigator
               =>
               if Child.TextAlign /= Right then
                  Debug (0, Sp (3) & Parent.Name.all
                         & ": replaced by "
                         & Child.Name.all);
                  Replace_Parent_By_Child (Parent, Child);
               end if;
            when others => null;
         end case;
      end if;

      Visit_GtkTree_Widget_For_GtkBox (Parent.Next);
      Visit_GtkTree_Widget_For_GtkBox (Parent.Child_List);
   end Visit_GtkTree_Widget_For_GtkBox;

   -----------------------------------------------------------------------
   procedure Visit_GtkTree_Widget_For_Columns (TWdg : Widget_Pointer);
   procedure Visit_GtkTree_Widget_For_Columns (TWdg : Widget_Pointer) is
      Num : Integer;
      B   : Boolean;
   begin
      if TWdg = null then
         return;
      end if;

      if TWdg.Widget_Type = GtkDataGridView
        or else
          TWdg.Widget_Type = GtkTreeGridView
      then
         Num  := Num_Children (TWdg);
         if Num > 0 then
            Temp := TWdg.Child_List;
            while Temp /= null loop
               case Temp.Widget_Type is
                  when DataGridViewCheckBoxColumn =>
                     Temp.CheckBox_Col_Properties.Active_Column := Num;
                     Debug (0, Sp (3) & Temp.Name.all
                            & ": active Column => "
                            & Img (Num));
                     Num := Num + 1;
                     if not Temp.ReadOnly then
                        Temp.CheckBox_Col_Properties.Activatable_Column
                          := Num;
                        Debug (0, Sp (3) & Temp.Name.all
                               & ": activatable Column => "
                               & Img (Num));
                        Num := Num + 1;
                        Found := Signal_Exists (Temp, "Toggled");
                        if not Found then
                           TS := new Signal_Block;
                           TS.Name := new String'("Toggled");
                           TS.Handler :=
                             new String'("On_"
                                         & Temp.Name.all
                                         & "_Toggled");
                           B := Insert_Signal (Temp, TS); --  Toggled
                           if B then
                              Debug (0, Sp (3) & Temp.Name.all
                                     & ": generated "
                                     & TS.Handler.all);
                           else
                              Debug (0, "Warning"
                                     & ": repeated handler " & TS.Handler.all
                                     & ": No Glade, No Ada will be generated "
                                     & "for this signal");
                              Free (TS.Name);
                              Free (TS.Handler);
                              Free (TS.GtkName);
                              Free (TS);
                           end if;
                        end if;
                     end if;
                  when ExpandableColumn | DataGridViewTextBoxColumn =>
                     if Temp.DefaultCellStyle in DGVS'Range and then
                       (DGVS (Temp.DefaultCellStyle).Format /= Format_String
                        and DGVS (Temp.DefaultCellStyle).Format /= Format_Boolean
                        and DGVS (Temp.DefaultCellStyle).Format /= Format_Date)
                     then
                        Temp.Text_Col_Properties.Fg_Color_Name_Column := Num;
                        Debug (0, Sp (3) & Temp.Name.all
                               & ": Fg Color Column => "
                               & Img (Num));
                        Num := Num + 1;
                     end if;
                  when others => null;
               end case;
               Temp := Temp.Next;
            end loop;
         end if;
      end if;

      Visit_GtkTree_Widget_For_Columns (TWdg.Next);
      Visit_GtkTree_Widget_For_Columns (TWdg.Child_List);
   end Visit_GtkTree_Widget_For_Columns;

   -----------------------------------------------------------------------
   procedure Recast_To_GtkNormalMenuItem (TWdg : in out Widget_Pointer);
   procedure Recast_To_GtkNormalMenuItem (TWdg : in out Widget_Pointer) is
      Temp   : Widget_Pointer;
      Parent : constant Widget_Pointer := TWdg.Wdg_Parent;
   begin
      Temp := new Widget_Properties (GtkMenuNormalItem);
      Copy_Common_Attributes (From => TWdg, To => Temp);
      Temp.ImageMenu := TWdg.ImageMenu;
      Replace (Parent, TWdg, Temp);
      Release (TWdg);
      TWdg := Temp;
      Have.MenuNormalItems := Have.MenuNormalItems + 1;
      Debug (0, Sp (3) & "GtkMenuItemImage " & TWdg.Name.all
             & " => GtkMenuNormalItem");
   end Recast_To_GtkNormalMenuItem;

   -----------------------------------------------------------------------
   procedure Visit_GtkMenuImageItem_Widget (TWdg : in out Widget_Pointer);
   procedure Visit_GtkMenuImageItem_Widget (TWdg : in out Widget_Pointer) is
   begin
      if TWdg = null then
         return;
      end if;

      if TWdg.Child_List = null then
         if TWdg.Widget_Type = GtkMenuImageItem then
            if TWdg.ImageMenu = null then
               Recast_To_GtkNormalMenuItem (TWdg);
               Have.MenuImageItems := Have.MenuImageItems - 1;
            end if;
         end if;
      end if;

      Visit_GtkMenuImageItem_Widget (TWdg.Next);
      Visit_GtkMenuImageItem_Widget (TWdg.Child_List);
   end Visit_GtkMenuImageItem_Widget;

   -----------------------------------------------------------------------
   procedure Visit_Use_Sort (TWdg : in Widget_Pointer);
   procedure Visit_Use_Sort (TWdg : in Widget_Pointer) is
   begin
      if TWdg = null then
         return;
      end if;

      if TWdg.Widget_Type in ExpandableColumn | DataGridViewTextBoxColumn
        | DataGridViewCheckBoxColumn
      then
         if TWdg.SortMode /= NotSortable then
            TWdg.Wdg_Parent.Use_Sort := True;
            Found := Signal_Exists (TWdg, "Click");
            if not Found then
               TS := new Signal_Block;
               TS.Name := new String'("Click");
               TS.Handler := new String'("On_"
                                         & TWdg.Name.all
                                         & "_Clicked");
               B := Insert_Signal (TWdg, TS); --  Click
                  if B then
                     Debug (0, Sp (3) & "Created Synthetic Signal "
                            & TWdg.Name.all
                            & ".clicked");
                  else
                     Debug (0, "WARNING"
                            & ": repeated handler " & TS.Handler.all
                            & ": No Glade, No Ada will be generated "
                            & "for this signal");
                     Free (TS.Name);
                     Free (TS.Handler);
                     Free (TS.GtkName);
                     Free (TS);
                  end if;
            end if;
         end if;
      end if;

      Visit_Use_Sort (TWdg.Next);
      Visit_Use_Sort (TWdg.Child_List);
   end Visit_Use_Sort;

   -----------------------------------------------------------------------
   procedure Visit_Renumber_Children (TWdg : in Widget_Pointer);
   procedure Visit_Renumber_Children (TWdg : in Widget_Pointer) is
      TWdg0 : Widget_Pointer;
   begin
      if TWdg = null then
         return;
      end if;

      TWdg.Num_Children := 0;
      TWdg0 := TWdg.Child_List;
      while TWdg0 /= null loop
         TWdg0.Child_Number := TWdg.Num_Children;
         TWdg.Num_Children  := TWdg.Num_Children + 1;
         if TWdg0.Name /= null then
            Debug (0, Sp (3) & TWdg0.Name.all
                   & " renumbered to child num." & TWdg0.Child_Number'Image);
         else
            Debug (0, Sp (3) & TWdg0.Widget_Type'Image
                   & " renumbered to child num." & TWdg0.Child_Number'Image);
         end if;
         TWdg0 := TWdg0.Next;
      end loop;
      Visit_Renumber_Children (TWdg.Child_List);
      Visit_Renumber_Children (TWdg.Next);
   end Visit_Renumber_Children;

   -----------------------------------------------------------------------
   -----------------------------------------------------------------------
   procedure Generate_Auxiliary_Windows;
   procedure Generate_Auxiliary_Windows is
   begin
      Debug (-1, "");
      Debug (0, "Generating auxiliary windows");
      Counter := 0;
      TWin := Win_List;
      while TWin /= null loop
         TWdg := TWin.Widget_List;
         while TWdg /= null loop
            case TWdg.Widget_Type is
               when GtkFileChooserButton =>
                  Num_Aux_Widgets := Num_Aux_Widgets + 1;
                  if TWdg.OpenFileFilter /= null
                    and then TWdg.OpenFileFilter.all /= ""
                  then
                     NWin1 := new Window_Properties (GtkFileFilter);
                     NWin1.Name := new String'("GtkFileFilter_"
                                               & TWdg.Name.all);
                     NWin1.Original_Name := New_String (TWdg.Name);
                     NWin1.Title := TWdg.OpenFileTitle;
                     NWin1.FilterString := new String'(TWdg.OpenFileFilter.all);
                     Free (TWdg.OpenFileFilter);
                     TWdg.OpenFileFilter := NWin1.Name;
                  end if;

                  NWin0 := new Window_Properties (GtkFileChooserDialog);
                  NWin0.Name := new String'("GtkFileChooserDialog_"
                                            & TWdg.Name.all);
                  NWin0.Original_Name := New_String (TWdg.Name);
                  NWin0.Title := TWdg.OpenFileTitle;
                  NWin0.FilterName := New_String (NWin1.Name);
                  NWin0.Transient_For := TWin;
                  NWin0.Attached_To   := TWdg;
                  NWin0.Top_Level     := True;

                  if TWdg.OpenFileDialog = null then
                     TWdg.OpenFileDialog := New_String (NWin0.Name);
                  end if;
                  Insert_Window_By_Front (Win_List_Aux, NWin0);
                  Counter := Counter + 1;
                  Debug (0, Sp (3) & Counter'Image & " "
                         & "Generated filechooserdialog for "
                         & NWin0.Name.all
                         & " for " & TWdg.Name.all
                         & " (" & TWdg.Widget_Type'Image & ")");

                  Insert_Window_By_Front (Win_List_Aux, NWin1);
                  Counter := Counter + 1;
                  Debug (0, Sp (3) & Counter'Image & " "
                         & NWin1.Name.all
                         & " for " & TWdg.Name.all
                         & " (" & TWdg.Widget_Type'Image & ")");

               when GtkEntry | GtkComboTextBox =>
                  if TWdg.Text_Buffer /= null
                    and then TWdg.Text_Buffer.all /= ""
                  then
                     Num_Aux_Widgets := Num_Aux_Widgets + 1;
                     NWin1 := new Window_Properties (GtkEntryBuffer);
                     NWin1.Name := new String'("GtkEntryBuffer_"
                                               & TWdg.Name.all);
                     NWin1.Original_Name := New_String (TWdg.Name);
                     NWin1.Associated_Widget := TWdg;
                     TWdg.Buffer := NWin1;
                     Insert_Window_By_Front (Win_List_Aux, NWin1);
                     Counter := Counter + 1;
                     Debug (0, Sp (3) & Counter'Image & " "
                            & "Generated entrybuffer for "
                            & NWin1.Name.all
                            & " for " & TWdg.Name.all
                            & " (" & TWdg.Widget_Type'Image & ")");
                  end if;

               when GtkListBox =>
                  Num_Aux_Widgets := Num_Aux_Widgets + 1;
                  NWin1 := new Window_Properties (GtkListStore);
                  NWin1.Name := new String'("GtkListStore_"
                                            & TWdg.Name.all);
                  NWin1.Original_Name := New_String (TWdg.Name);
                  NWin1.Associated_Widget := TWdg;
                  TWdg.ListStore := NWin1;
                  Insert_Window_By_Front (Win_List_Aux, NWin1);
                  Counter := Counter + 1;
                  Debug (0, Sp (3) & Counter'Image & " "
                         & "Generated liststore for "
                         & NWin1.Name.all
                         & " for " & TWdg.Name.all
                         & " (" & TWdg.Widget_Type'Image & ")");

               when GtkButton | GtkRadioButton
                  | GtkCheckButton | GtkToggleButton =>
                  if TWdg.ImagePath /= null then
                     Num_Aux_Widgets := Num_Aux_Widgets + 1;
                     NWin1 := new Window_Properties (GtkImage);
                     NWin1.Name := new String'("GtkImage_"
                                               & TWdg.Name.all);
                     NWin1.Original_Name := New_String (TWdg.Name);
                     NWin1.Associated_Widget := TWdg;
                     TWdg.Win_Image := NWin1;
                     Insert_Window_By_Front (Win_List_Aux, NWin1);
                     Counter := Counter + 1;
                     Debug (0, Sp (3) & Counter'Image & " "
                            & "Generated gtkimage for "
                            & NWin1.Name.all
                            & " for " & TWdg.Name.all
                            & " (" & TWdg.Widget_Type'Image & ")");
                  end if;

               when GtkMenuImageItem =>
                  if TWdg.ImageMenu /= null then
                     Num_Aux_Widgets := Num_Aux_Widgets + 1;
                     NWin1 := new Window_Properties (GtkImage);
                     NWin1.Name := new String'("GtkImage_"
                                               & TWdg.Name.all);
                     NWin1.Original_Name := New_String (TWdg.Name);
                     NWin1.Associated_Widget := TWdg;
                     TWdg.ImageMenuWin := NWin1;
                     Insert_Window_By_Front (Win_List_Aux, NWin1);
                     Counter := Counter + 1;
                     Debug (0, Sp (3) & Counter'Image & " "
                            & "Generated gtkimage for "
                            & NWin1.Name.all
                            & " for " & TWdg.Name.all
                            & " (" & TWdg.Widget_Type'Image & ")");
                  end if;

               when GtkDataGridView  | GtkTreeGridView =>
                  Num_Aux_Widgets := Num_Aux_Widgets + 1;
                  if TWdg.Widget_Type = GtkDataGridView
                    and then
                      not TWdg.Has_Expander
                  then
                     NWin0 := new Window_Properties (GtkListStore);
                     NWin0.Name := new String'("GtkListStore_"
                                               & Normalize_Name (TWdg));
                     NWin0.Original_Name := New_String (TWdg.Name);
                     NWin0.Associated_Widget := TWdg;
                     Counter := Counter + 1;
                     Debug (0, Sp (3) & Counter'Image & " "
                            & "Generated gtkliststore for "
                            & NWin0.Name.all
                            & " for " & Normalize_Name (TWdg)
                            & " (" & TWdg.Widget_Type'Image & ")");
                  else
                     Num_Aux_Widgets := Num_Aux_Widgets + 1;
                     NWin0 := new Window_Properties (GtkTreeStore);
                     --  NWin0.Name := new String'("GtkTreeStore_"
                     NWin0.Name := new String'("GtkListStore_"
                                               & Normalize_Name (TWdg));
                     NWin0.Original_Name := New_String (TWdg.Name);
                     NWin0.Associated_Widget := TWdg;
                     Counter := Counter + 1;
                     Debug (0, Sp (3) & Counter'Image & " "
                            --  & "Generated gtktreestore for "
                            & "Generated gtktreestore for "
                            & NWin0.Name.all
                            & " for " & Normalize_Name (TWdg)
                            & " (" & TWdg.Widget_Type'Image & ")");
                  end if;
                  --  generate the filter model
                  Num_Aux_Widgets := Num_Aux_Widgets + 1;
                  NWin1 := new Window_Properties (GtkModelFilter);
                  NWin1.Name := new String'("GtkModelFilter_"
                                            & Normalize_Name (TWdg));
                  NWin1.Original_Name := New_String (TWdg.Name);
                  NWin1.Associated_Widget := TWdg;
                  NWin1.Underlaying_Model := NWin0;
                  Counter := Counter + 1;
                  Debug (0, Sp (3) & Counter'Image & " "
                         & "Generated gtkmodelfilter for "
                         & NWin1.Name.all
                         & " for " & NWin0.Name.all
                         & " (" & TWdg.Widget_Type'Image & ")");
                  --  generate the sort model
                  Num_Aux_Widgets := Num_Aux_Widgets + 1;
                  NWin2 := new Window_Properties (GtkModelSort);
                  NWin2.Name := new String'("GtkModelSort_"
                                            & Normalize_Name (TWdg));
                  NWin2.Original_Name := New_String (TWdg.Name);
                  NWin2.Associated_Widget := TWdg;
                  NWin2.Underlaying_Model := NWin1;
                  Counter := Counter + 1;
                  Debug (0, Sp (3) & Counter'Image & " "
                         & "Generated gtkmodelsort for "
                         & NWin2.Name.all
                         & " for " & NWin1.Name.all
                         & " (" & TWdg.Widget_Type'Image & ")");

                  --   grab the model (sort)
                  TWdg.Model := NWin2;

                  --  insert the objects
                  Insert_Window_By_Front (Win_List_Aux, NWin2);
                  Insert_Window_By_Front (Win_List_Aux, NWin1);
                  Insert_Window_By_Front (Win_List_Aux, NWin0);

               when others => null;
            end case;
            TWdg := TWdg.Next;
         end loop;
         TWin := Next_Window (Win_List, TWin);
      end loop;
   end Generate_Auxiliary_Windows;

   -----------------------------------------------------------------------
   procedure Generate_Auxiliary_Widgets;
   procedure Generate_Auxiliary_Widgets is
      Wdg0 : Widget_Pointer;
      Wdg1 : Widget_Pointer;
      Wdg2 : Widget_Pointer;
      Wdg3 : Widget_Pointer;
   begin
      Debug (-1, "");
      Debug (0, "Generating auxiliary widgets");

      --  scrolled windows
      Counter := 0;
      TWin := Win_List;
      while TWin /= null loop
         TWdg := TWin.Widget_List;
         while TWdg /= null loop
            case TWdg.Widget_Type is
               when GtkDataGridView  | GtkTreeGridView =>
                  --  generate the scrolled window if necessary
                  if TWdg.H_ScrollBar = Always or TWdg.H_ScrollBar = Automatic
                    or TWdg.V_ScrollBar = Always or TWdg.V_ScrollBar = Automatic
                  then
                     Num_Aux_Widgets := Num_Aux_Widgets + 1;
                     Wdg0 := new Widget_Properties (GtkScrolledWindow);
                     Wdg0.Name := new String'(TWdg.Name.all & "_ScrolledWindow");
                     Wdg0.Parent_Name := New_String (TWdg.Parent_Name);
                     Wdg0.Wdg_Parent  := TWdg.Wdg_Parent;
                     Wdg0.Win_Parent  := TWdg.Win_Parent;
                     TWdg.Wdg_Parent  := Wdg0;

                     Insert_Widget_By_Tail (TWin, Wdg0);
                     --  name may have changed
                     TWdg.Parent_Name := New_String (Wdg0.Name);

                     Counter := Counter + 1;
                     Debug (0, Sp (3) & Counter'Image & " "
                            & "Generated scrolled window "
                            & Wdg0.Name.all
                            & " for " & TWdg.Name.all
                            & " [" & TWdg.Widget_Type'Image & "]");
                  end if;

               when others => null;
            end case;
            TWdg := TWdg.Next;
         end loop;
         TWin := Next_Window (Win_List, TWin);
      end loop;

      --  notebook tabpages
      Counter := 0;
      TWin := Win_List;
      while TWin /= null loop
         TWdg := TWin.Widget_List;
         while TWdg /= null loop
            case TWdg.Widget_Type is
               when GtkTabPage =>
                  Num_Aux_Widgets := Num_Aux_Widgets + 1;
                  --  create the tab box
                  Wdg0 := new Widget_Properties (GtkTabChild);
                  Wdg0.Name := new String'(TWdg.Name.all & "_Box");
                  Wdg0.Parent_Name := New_String (TWdg.Name);

                  Insert_Widget_By_Tail (TWin, Wdg0);

                  Counter := Counter + 1;
                  Debug (0, Sp (3) & Counter'Image & " "
                         & "Generated tab box "
                         & Wdg0.Name.all
                         & " for " & TWdg.Name.all
                         & " [" & TWdg.Widget_Type'Image & "]");

                  --  create the label
                  Wdg1 := new Widget_Properties (GtkLabel);
                  Wdg1.Name := new String'(TWdg.Name.all & "_Label");
                  Wdg1.Windows_Type := new String'("System.Windows.Forms.Label");
                  Wdg1.Parent_Name := new String'(Wdg0.Name.all);
                  Wdg1.Text := new String'(TWdg.Text.all);

                  Insert_Widget_By_Tail (TWin, Wdg1);

                  TWdg.The_Label := Wdg1;
                  Debug (0, Sp (3) & Counter'Image & " "
                         & "Generated gtklabel "
                         & Wdg1.Name.all & " for "
                         & Wdg0.Name.all
                         & " [" & Wdg0.Widget_Type'Image & "]");

                  --  create the button
                  Wdg2 := new Widget_Properties (GtkButton);
                  Wdg2.Name := new String'(TWdg.Name.all & "_Button");
                  Wdg2.Parent_Name := new String'(Wdg0.Name.all);

                  Insert_Widget_By_Tail (TWin, Wdg2);

                  TWdg.The_Button := Wdg1;
                  Debug (0, Sp (3) & Counter'Image & " "
                         & "Generated gtkbutton "
                         & Wdg2.Name.all & " for "
                         & Wdg0.Name.all
                         & " [" & Wdg0.Widget_Type'Image & "]");

                  --  Note: the signal for this button is included later

                  --  create the image within the button
                  Wdg3 := new Widget_Properties (GtkImage);
                  Wdg3.Name := new String'("Tab_Img_" & TWdg.Name.all);
                  Wdg3.Parent_Name := new String'(Wdg2.Name.all);
                  Wdg3.Stock := True;
                  Wdg3.Image := new String'("gtk-close");

                  Insert_Widget_By_Tail (TWin, Wdg3);

                  Debug (0, Sp (3) & Counter'Image & " "
                         & "Generated stock image "
                         & Wdg3.Name.all & " for "
                         & Wdg2.Name.all
                         & " [" & Wdg2.Widget_Type'Image & "]");

                  TWdg.The_Button := Wdg1;
                  Debug (0, Sp (3) & Counter'Image & " "
                         & "Generated gtkbutton "
                         & Wdg2.Name.all & " for "
                         & Wdg0.Name.all
                         & " [" & Wdg0.Widget_Type'Image & "]");

               when others => null;
            end case;
            TWdg := TWdg.Next;
         end loop;
         TWin := Next_Window (Win_List, TWin);
      end loop;

      --  frames
      Counter := 0;
      TWin := Win_List;
      while TWin /= null loop
         TWdg := TWin.Widget_List;
         while TWdg /= null loop
            case TWdg.Widget_Type is
               when GtkFrame =>
                  Num_Aux_Widgets := Num_Aux_Widgets + 1;
                  --  create the alignment
                  Wdg0 := new Widget_Properties (GtkAlignment);
                  Wdg0.Name := new String'(TWdg.Name.all & "_Alignment");
                  Wdg0.Parent_Name := New_String (TWdg.Name);

                  Insert_Widget_By_Tail (TWin, Wdg0);

                  Counter := Counter + 1;
                  Debug (0, Sp (3) & Counter'Image & " "
                         & "Generated gtkalignment "
                         & Wdg0.Name.all
                         & " for " & TWdg.Name.all
                         & " [" & TWdg.Widget_Type'Image & "]");

                  --  create the gtkfixed
                  Wdg1 := new Widget_Properties (GtkFixed);
                  Wdg1.Name := new String'(TWdg.Name.all & "_Fixed");
                  Wdg1.Parent_Name := new String'(Wdg0.Name.all);

                  Insert_Widget_By_Tail (TWin, Wdg1);
                  Debug (0, Sp (3) & Counter'Image & " "
                         & "Generated gtkfixed "
                         & Wdg1.Name.all
                         & " for " & Wdg0.Name.all
                         & " [" & Wdg0.Widget_Type'Image & "]");

                  --  move the children of TWdg to Wdg1
                  Wdg3 := TWin.Widget_List;
                  while Wdg3 /= null loop
                     if Wdg3 = Wdg0 then
                        null;
                     elsif Wdg3.Parent_Name /= null
                       and then Wdg3.Parent_Name.all = TWdg.Name.all
                     then
                        Free (Wdg3.Parent_Name);
                        Wdg3.Parent_Name := New_String (Wdg1.Name);
                        Debug (0, Sp (3) & Counter'Image & " "
                               & "Moved "
                               & Wdg3.Name.all
                               & " to parent " & Wdg1.Name.all
                               & " [" & Wdg1.Widget_Type'Image & "]");
                     end if;
                     Wdg3 := Wdg3.Next;
                  end loop;

               when others => null;
            end case;
            TWdg := TWdg.Next;
         end loop;
         TWin := Next_Window (Win_List, TWin);
      end loop;
   end Generate_Auxiliary_Widgets;

   -----------------------------------------------------------------------
   procedure Move_Windows_To_Auxiliary_List;
   procedure Move_Windows_To_Auxiliary_List is
   begin
      Debug (-1, "");
      Debug (0, "Move windows to auxiliary list");
      Counter := 0;
      TWin := Win_List;
      loop
         TWin := Extract_First_Window (Win_List);
         exit when TWin = null;
         Insert_Window_By_Front (Win_List_Aux, TWin);
         Counter := Counter + 1;
         Debug (0, Sp (3) & Counter'Image & " " & TWin.Name.all);
      end loop;
   end Move_Windows_To_Auxiliary_List;

   -----------------------------------------------------------------------
   procedure Reordering_Windows_In_Separate_Lists;
   procedure Reordering_Windows_In_Separate_Lists is
   begin
      Debug (-1, "");
      Debug (0, "Reordering Windows in separate lists");
      NWin0   := null;
      Counter := 0;
      loop
         TWin := Extract_First_Window (Win_List_Aux);
         exit when TWin = null;
         Counter := Counter + 1;
         if TWin.Top_Level then
            Insert_Window_By_Order (NWin0, TWin);
            Debug (0, Sp (3) & Counter'Image & " "
                   & "Inserting Top-Level Window by order " & TWin.Name.all);
         else
            Insert_Window_By_Order (Win_List, TWin);
            Debug (0, Sp (3) & Counter'Image & " "
                   & "Inserting no-Top-Level Window by order " & TWin.Name.all);
         end if;
      end loop;
      --  here win_list_aux is empty
      if Win_List_Aux /= null then
         raise Program_Error;
      end if;
   end Reordering_Windows_In_Separate_Lists;

   -----------------------------------------------------------------------
   procedure Set_Some_Properties_Of_Toolstripstatuslabel;
   procedure Set_Some_Properties_Of_Toolstripstatuslabel is
   begin
      Debug (-1, "");
      Debug (0, "Set some properties of toolstripstatuslabel");
      Counter := 0;
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            TWdg := TWin.Widget_List;
            while TWdg /= null loop
               if TWdg.Name /= null then
                  if Contains (TWdg.Name.all, "ToolStripStatusLabel") then
                     Temp := Find_Widget (TWin.Widget_List, GtkStatusBar);
                     if Temp /= null then
                        TWdg.Location.From_Top :=
                          Integer'Max (0, TWdg.Location.From_Top) +
                          Temp.Location.From_Top;
                        TWdg.Location.From_Left :=
                          Integer'Max (0, TWdg.Location.From_Left) +
                          Temp.Location.From_Left;
                        Counter := Counter + 1;
                        Debug (0, Sp (3) & Counter'Image & " "
                               & "ToolStripStatusLabel "
                               & TWdg.Name.all
                               & ": Location adjusted");
                     end if;
                  end if;
               end if;
               TWdg := TWdg.Next;
            end loop;
         end if;
         TWin := Next_Window (Win_List, TWin);
      end loop;
   end Set_Some_Properties_Of_Toolstripstatuslabel;

   -----------------------------------------------------------------------
   procedure Reordering_Sort_Models;
   procedure Reordering_Sort_Models is
   begin
      Debug (-1, "");
      Debug (0, "Reordering Sort Models");
      Counter := 0;
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkListStore or TWin.Window_Type = GtkTreeStore then
            NWin0 := Find_Window (Win_List, "GtkModelSort_"
                                  & TWin.Original_Name.all);
            if NWin0 /= null then
               Extract_Window (Win_List, NWin0);
               Insert_Window (Root => Win_List, After => TWin, TWin => NWin0);
               Counter := Counter + 1;
               Debug (0, Sp (3) & Counter'Image & " "
                      & NWin0.Name.all);
            end if;
         end if;
         TWin := Next_Window (Win_List, TWin);
      end loop;
   end Reordering_Sort_Models;

   -----------------------------------------------------------------------
   procedure Reordering_Filter_Models;
   procedure Reordering_Filter_Models is
   begin
      Debug (-1, "");
      Debug (0, "Reordering Filter Models");
      Counter := 0;
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkListStore or TWin.Window_Type = GtkTreeStore then
            NWin0 := Find_Window (Win_List, "GtkModelFilter_"
                                  & TWin.Original_Name.all);
            if NWin0 /= null then
               Extract_Window (Win_List, NWin0);
               Insert_Window (Root => Win_List, After => TWin, TWin => NWin0);
               Counter := Counter + 1;
               Debug (0, Sp (3) & Counter'Image & " "
                      & NWin0.Name.all);
            end if;
         end if;
         TWin := Next_Window (Win_List, TWin);
      end loop;
   end Reordering_Filter_Models;

   -----------------------------------------------------------------------
   procedure Set_Have_Windows_And_Widgets;
   procedure Set_Have_Windows_And_Widgets is
   begin
      Debug (-1, "");
      Debug (0, "Set Have Windows and Widgets");
      TWin := Win_List;
      while TWin /= null loop
         Set_Have (TWin);
         if TWin.Window_Type = GtkWindow then
            TWdg := TWin.Widget_List;
            while TWdg /= null loop
               Set_Have (TWdg);
               TWdg := TWdg.Next;
            end loop;
         end if;
         TWin := Next_Window (Win_List, TWin);
      end loop;

      Debug (0, Sp (3) & "ListStores" & Have.ListStores'Image);
      Debug (0, Sp (3) & "TreeStores" & Have.TreeStores'Image);
      Debug (0, Sp (3) & "FileFilters" & Have.FileFilters'Image);
      Debug (0, Sp (3) & "Filechooserdialogs" & Have.Filechooserdialogs'Image);
      Debug (0, Sp (3) & "Entrybuffers" & Have.Entrybuffers'Image);
      Debug (0, Sp (3) & "Date_Pickers" & Have.Date_Pickers'Image);
      Debug (0, Sp (3) & "Time_Pickers" & Have.Time_Pickers'Image);
      Debug (0, Sp (3) & "Radio_Buttons" & Have.Radio_Buttons'Image);
      Debug (0, Sp (3) & "Check_Buttons" & Have.Check_Buttons'Image);
      Debug (0, Sp (3) & "Frames" & Have.Frames'Image);
      Debug (0, Sp (3) & "Tooltips" & Have.Tooltips'Image);
      Debug (0, Sp (3) & "Tree Columns Tooltips" & Have.Column_Tooltips'Image);
      Debug (0, Sp (3) & "Font_Underline" & Have.Font_Underline'Image);
      Debug (0, Sp (3) & "Font_Weight" & Have.Font_Weight'Image);
      Debug (0, Sp (3) & "Buttons" & Have.Buttons'Image);
      Debug (0, Sp (3) & "Labels" & Have.Labels'Image);
      Debug (0, Sp (3) & "Menus" & Have.Menus'Image);
      Debug (0, Sp (3) & "MenuImageItems" & Have.MenuImageItems'Image);
      Debug (0, Sp (3) & "MenuNormalItems" & Have.MenuNormalItems'Image);
      Debug (0, Sp (3) & "MenuSeparators" & Have.MenuSeparators'Image);
      Debug (0, Sp (3) & "Toolbars" & Have.Toolbars'Image);
      Debug (0, Sp (3) & "ToolSeparators" & Have.ToolSeparators'Image);
      Debug (0, Sp (3) & "Images" & Have.Images'Image);
      Debug (0, Sp (3) & "Notebooks" & Have.Notebooks'Image);
      Debug (0, Sp (3) & "TreeViews" & Have.TreeViews'Image);
      Debug (0, Sp (3) & "TreeViewColumns" & Have.TreeViewColumns'Image);
      Debug (0, Sp (3) & "TreeViewToggles" & Have.TreeViewToggles'Image);
      Debug (0, Sp (3) & "HDR_CellRenderers" & Have.HDR_CellRenderers'Image);
      Debug (0, Sp (3) & "Entries" & Have.Entries'Image);
      Debug (0, Sp (3) & "ComboTextBoxes" & Have.ComboTextBoxes'Image);
      Debug (0, Sp (3) & "Boxes" & Have.Boxes'Image);
      Debug (0, Sp (3) & "FileChooserButtons" & Have.FileChooserButtons'Image);
   end Set_Have_Windows_And_Widgets;

   -----------------------------------------------------------------------
   function Reparenting_Widgets_From_Parent_Name return Integer;
   function Reparenting_Widgets_From_Parent_Name return Integer is
   begin
      Debug (-1, "");
      Debug (0, "Reparenting widgets from parent name");
      Counter := 0;
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            TWdg := TWin.Widget_List;
            while TWdg /= null loop
               if TWdg.Parent_Name /= null then
                  if TWdg.Parent_Name.all = "$this" then
                     TWdg.Win_Parent := TWin;
                     Free (TWdg.Parent_Name);
                     TWdg.Parent_Name := new String'(TWdg.Win_Parent.Name.all);
                     Counter := Counter + 1;
                     Debug (0, Sp (3) & Counter'Image & " "
                            & TWdg.Name.all
                            & " [" & TWdg.Widget_Type'Image & "]"
                            & " reparented to Window "
                            & TWdg.Win_Parent.Name.all
                            & " [" & TWdg.Win_Parent.Window_Type'Image & "]");
                  else
                     TWdg0 := Find_Widget (TWin.Widget_List,
                                           TWdg.Parent_Name.all);
                     if TWdg0 = null then
                        Debug (0, Sp (3)
                               & "WARNING: Widget " & TWdg.Name.all
                               & " [" & TWdg.Widget_Type'Image & "]"
                               & " without parent, "
                               & " tried " & TWdg.Parent_Name.all);
                        return -1;
                     end if;
                     TWdg.Wdg_Parent := TWdg0;
                     Counter := Counter + 1;
                     Debug (0, Sp (3) & Counter'Image & " "
                            & TWdg.Name.all
                            & " [" & TWdg.Widget_Type'Image & "]"
                            & " reparented to widget "
                            & TWdg.Wdg_Parent.Name.all
                            & " [" & TWdg.Wdg_Parent.Widget_Type'Image & "]");
                  end if;
               else
                  TWdg.Win_Parent := TWin;
                  TWdg.Parent_Name := new String'(TWin.Name.all);
                  Counter := Counter + 1;
                  Debug (0, Sp (3) & Counter'Image & " "
                         & TWdg.Name.all
                         & " [" & TWdg.Widget_Type'Image & "]"
                         & " reparented to window " & TWdg.Win_Parent.Name.all
                         & " [" & TWdg.Win_Parent.Window_Type'Image & "]");
               end if;
               TWdg := TWdg.Next;
            end loop;
         end if;
         TWin := Next_Window (Win_List, TWin);
      end loop;
      return 0;
   end Reparenting_Widgets_From_Parent_Name;

   -----------------------------------------------------------------------
   procedure Set_Maxlength_For_Non_Editable;
   procedure Set_Maxlength_For_Non_Editable is
   begin
      Debug (-1, "");
      Debug (0, "Set maxlength for non-editable gtkentries, "
             & "comboboxes and labels");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            TWdg := TWin.Widget_List;
            while TWdg /= null loop
               if TWdg.AutoSize then
                  case TWdg.Widget_Type is
                  when GtkEntry =>
                     if not TWdg.Editable then
                        if TWdg.MaxLength < 1
                          and then TWdg.Text /= null
                        then
                           TWdg.MaxLength := TWdg.Text.all'Length;
                           Debug (0, Sp (3) & TWdg.Name.all
                                  & ".Maxlength = "
                                  & Img (TWdg.MaxLength));
                        end if;
                     else
                        TWdg.MaxLength := TWdg.Size.Horiz / 8;
                     end if;
                  when GtkComboTextBox =>
                     TWdg.MaxLength := TWdg.Size.Horiz / 10;
                     Debug (0, Sp (3) & TWdg.Name.all
                            & ".Maxlength = "
                            & Img (TWdg.MaxLength));
                  when GtkLabel =>
                     if TWdg.MaxLength < 1
                       and then TWdg.Text /= null
                     then
                        TWdg.MaxLength := TWdg.Text.all'Length;
                        Debug (0, Sp (3) & TWdg.Name.all
                               & ".Maxlength = "
                               & Img (TWdg.MaxLength));
                     end if;
                  when others => null;
                  end case;
               end if;
               TWdg := TWdg.Next;
            end loop;
         end if;
         TWin := Next_Window (Win_List, TWin);
      end loop;
   end Set_Maxlength_For_Non_Editable;

   -----------------------------------------------------------------------
   procedure Recasting_Menus_Separator;
   procedure Recasting_Menus_Separator is
   begin
      Debug (-1, "");
      Debug (0, "Recast menus: "
             & "GtkSeparatorToolItem in Menus => GtkSeparatorMenuItem");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            TWdg := TWin.Widget_List;
            while TWdg /= null loop
               if TWdg.Widget_Type = GtkSeparatorToolItem then
                  if TWdg.Wdg_Parent.Widget_Type = GtkMenuBar or else
                    TWdg.Wdg_Parent.Widget_Type = GtkMenuItem or else
                    TWdg.Wdg_Parent.Widget_Type = GtkMenuImageItem
                  then
                     Temp := new Widget_Properties (GtkSeparatorMenuItem);
                     Copy_Common_Attributes (From => TWdg, To => Temp);
                     Replace (TWin, TWdg, Temp);
                     Release (TWdg);
                     TWdg := Temp;
                     Have.MenuSeparators := Have.MenuSeparators + 1;
                     Have.ToolSeparators := Have.ToolSeparators - 1;
                     Debug (0, Sp (3) & "GtkSeparatorToolItem " & TWdg.Name.all
                            & " => GtkSeparatorMenuItem");
                  end if;
               end if;
               TWdg := TWdg.Next;
            end loop;
         end if;
         TWin := Next_Window (Win_List, TWin);
      end loop;
   end Recasting_Menus_Separator;

   -----------------------------------------------------------------------
   procedure Setting_Maxtabindex;
   procedure Setting_Maxtabindex is
   begin
      Debug (-1, "");
      Debug (0, "Setting maxtabindex");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            TWdg := TWin.Widget_List;
            while TWdg /= null loop
               if TWdg.TabIndex > TWin.MaxTabIndex then
                  TWin.MaxTabIndex := TWdg.TabIndex;
               end if;
               TWdg := TWdg.Next;
            end loop;
            Debug (0, Sp (3) & "Set Window Property " & TWin.Name.all
                   & ".MaxTabIndex " & Image (TWin.MaxTabIndex, 0));
            TWin.MinTabIndex := TWin.MaxTabIndex;
         end if;
         TWin := Next_Window (Win_List, TWin);
      end loop;
   end Setting_Maxtabindex;

   -----------------------------------------------------------------------
   procedure Processing_Tabindex_And_Tabstop;
   procedure Processing_Tabindex_And_Tabstop is
   begin
      Debug (-1, "");
      Debug (0, "Processing tabindex and tabstop");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            TWdg := TWin.Widget_List;
            while TWdg /= null loop
               if TWdg.TabStop = Indeterminate then
                  case TWdg.Widget_Type is
                  when GtkLabel =>
                     TWdg.TabStop := False;
                     Debug (0, Sp (3) & "Set Widget Property "
                            & TWdg.Name.all
                            & ".TabStop False");
                  when
                       GtkEntry | GtkComboTextBox
                     | GtkButton | GtkRadioButton
                     | GtkCheckButton | GtkToggleButton
                     =>
                     if TWdg.Widget_Type in GtkEntry | GtkComboTextBox then
                        TWdg.TabStop := To_TriBoolean (TWdg.Editable);
                     else
                        TWdg.TabStop := True;
                     end if;
                     if To_Boolean (TWdg.TabStop) then
                        Debug (0, Sp (3) & "Set Widget Property "
                               & TWdg.Name.all
                               & ".TabStop True");
                     end if;
                     if TWdg.TabIndex < 0 then
                        TWin.MaxTabIndex := @ + 1;
                        TWdg.TabIndex := TWin.MaxTabIndex;
                        Debug (0, Sp (3) & "Set Widget Property "
                               & TWdg.Name.all
                               & ".TabIndex "
                               & Image (TWdg.TabIndex, 0));
                     end if;
                  when others => null;
                  end case;
               end if;
               TWdg := TWdg.Next;
            end loop;
         end if;
         TWin := Next_Window (Win_List, TWin);
      end loop;
   end Processing_Tabindex_And_Tabstop;

   -----------------------------------------------------------------------
   procedure Processing_Focus_Handler;
   procedure Processing_Focus_Handler is
   begin
      Debug (-1, "");
      Debug (0, "Inserting focus handler (only for dialogs)");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow and then TWin.Is_Dialog then
            TWdg := TWin.Widget_List;
            while TWdg /= null loop
               if GNATCOLL.Tribooleans."=" (TWdg.TabStop, True) then
                  case TWdg.Widget_Type is
                     when GtkLabel | GtkEntry | GtkComboTextBox
                        | GtkButton | GtkRadioButton
                        | GtkCheckButton | GtkToggleButton
                        =>
                        Found := Signal_Exists (TWdg, "Leave");
                        if not Found then
                           TS := new Signal_Block;
                           TS.Name := new String'("Leave");
                           TS.Handler := new String'("On_"
                                                     & TWdg.Name.all
                                                     & "_Leave");
                           B := Insert_Signal (TWdg, TS);
                           if B then
                              Debug (0, Sp (3) & "Created Synthetic Signal "
                                     & ".leave"
                                     & " for widget "
                                     & TWdg.Name.all
                                     & " [" & TWdg.Widget_Type'Image & "]");
                           else
                              Debug (0, "Warning"
                                     & ": repeated handler " & TS.Handler.all
                                     & ": No Glade, No Ada will be generated "
                                     & "for this signal");
                              Free (TS.Name);
                              Free (TS.Handler);
                              Free (TS.GtkName);
                              Free (TS);
                           end if;
                        end if;
                     when others => null;
                  end case;
               end if;
               TWdg := TWdg.Next;
            end loop;
         end if;
         TWin := Next_Window (Win_List, TWin);
      end loop;
   end Processing_Focus_Handler;

   -----------------------------------------------------------------------
   procedure Selecting_The_Has_Focus_Widget;
   procedure Selecting_The_Has_Focus_Widget is
   begin
      Debug (-1, "");
      Debug (0, "Selecting the has-focus widget");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            TWdg := TWin.Widget_List;
            while TWdg /= null loop
               if GNATCOLL.Tribooleans."=" (TWdg.TabStop, True) then
                  case TWdg.Widget_Type is
                  when GtkLabel | GtkEntry | GtkComboTextBox
                     | GtkButton | GtkRadioButton
                     | GtkCheckButton | GtkToggleButton
                     =>
                     if TWdg.TabIndex >= 0
                       and then TWdg.TabIndex < TWin.MinTabIndex
                     then
                        TWin.Has_Focus_Widget := TWdg;
                        TWin.MinTabIndex := TWdg.TabIndex;
                     end if;
                  when others => null;
                  end case;
               end if;
               TWdg := TWdg.Next;
            end loop;
            if TWin.Has_Focus_Widget /= null then
               TWin.Has_Focus_Widget.Has_Focus := True;
               Debug (0, Sp (3) & "Set Widget Property "
                      & TWin.Has_Focus_Widget.Name.all
                      & ".Has_Focus True");
            end if;
         end if;
         TWin := Next_Window (Win_List, TWin);
      end loop;
   end Selecting_The_Has_Focus_Widget;

   -----------------------------------------------------------------------
   procedure Setting_The_Focus_Chain;
   procedure Setting_The_Focus_Chain is
   begin
      Debug (-1, "");
      Debug (0, "Setting the focus chain");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            TWdg := TWin.Widget_List;
            while TWdg /= null loop
               if To_Boolean (TWdg.TabStop) then
                  Debug (0, Sp (3) & "Inserting "
                         & TWdg.Name.all
                         & " with TabIndex " & TWdg.TabIndex'Image);
                  Insert_Focus (Into => TWin, Focus => TWdg);
               end if;
               TWdg := TWdg.Next;
            end loop;
            TWdg := TWin.TabFocusList;
            if TWdg /= null then
               Debug (0, "");
               Debug (0, "Focus chain for " & TWin.Name.all);
               loop
                  Debug (0, Sp (3) & TWdg.Name.all
                         & ".TabIndex "
                         & TWdg.TabIndex'Image
                         & " => " & TWdg.Next_Focus.Name.all);
                  TWdg := TWdg.Next_Focus;
                  exit when TWdg = TWin.TabFocusList;
               end loop;
            end if;
         end if;
         TWin := Next_Window (Win_List, TWin);
      end loop;
   end Setting_The_Focus_Chain;

   -----------------------------------------------------------------------
   procedure Inheritable_Attributes;
   procedure Inheritable_Attributes is
   begin
      Debug (-1, "");
      Debug (0, "Inheritable attributes");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            Process_Inheritable (TWin);
         end if;
         TWin := Next_Window (Win_List, TWin);
      end loop;
   end Inheritable_Attributes;

   -----------------------------------------------------------------------
   procedure Processing_Labels_With_Aspect_Frame;
   procedure Processing_Labels_With_Aspect_Frame is
   begin
      Debug (-1, "");
      Debug (0, "Processing Labels With Aspect Frame");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            TWdg := TWin.Widget_List;
            while TWdg /= null loop
               if TWdg.Widget_Type = GtkLabel and then TWdg.BorderStyle /= None
               then
                  TWdg0 := new Widget_Properties (GtkAspectFrame);
                  TWdg0.Frame_Shadow := In_Shadow;
                  TWdg0.Name := new String'(TWdg.Name.all & "_AspectFrame");
                  TWdg0.Parent_Name  := new String'(TWdg.Name.all);
                  TWdg0.Wdg_Parent   := TWdg.Wdg_Parent;
                  TWdg.Wdg_Parent    := TWdg0;
                  TWdg0.Size         := TWdg.Size;
                  TWdg0.Location     := TWdg.Location;
                  TWdg0.Child_Number := TWdg.Child_Number;
                  TWdg.Size          := (-1, -1);

                  Insert_Widget_By_Tail (TWin, TWdg0);

                  Debug (0, Sp (3) & "Created widget AspectFrame for label "
                         & TWdg.Name.all);
               end if;
               TWdg := TWdg.Next;
            end loop;
         end if;
         TWin := Next_Window (Win_List, TWin);
      end loop;
   end Processing_Labels_With_Aspect_Frame;

   -----------------------------------------------------------------------
   procedure Preparing_Action_Area;
   procedure Preparing_Action_Area is
   begin
      Debug (-1, "");
      Debug (0, "Preparing action area");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            if TWin.Action_Buttons (OK_Response) /= null or
              TWin.Action_Buttons (Cancel_Response) /= null
            then
               if TWin.Action_Buttons (OK_Response) /= null then
                  From_Top := TWin.Action_Buttons (OK_Response).Location.From_Top;
               elsif TWin.Action_Buttons (Cancel_Response) /= null then
                  From_Top := TWin.Action_Buttons (Cancel_Response).Location.From_Top;
               else
                  Debug (0, Sp (3)
                         & "OK and Cancel Buttons not found "
                         & "for action area, exiting");
                  exit;
               end if;
               TWdg := TWin.Widget_List;
               while TWdg /= null loop
                  if TWdg.Widget_Type = GtkButton then
                     if TWdg.Location.From_Top = From_Top then
                        if TWdg.Text.all = "Delete" then
                           TWin.Action_Buttons (Delete_Response) := TWdg;
                           if TWdg.Dialog_Result = None_Response then
                              TWdg.Dialog_Result := Delete_Response;
                           end if;
                           Debug (0, Sp (3)
                                  & "Delete button included in action area");
                           exit;
                        end if;
                     end if;
                  end if;
                  TWdg := TWdg.Next;
               end loop;
            end if;
         end if;
         TWin := Next_Window (Win_List, TWin);
      end loop;
   end Preparing_Action_Area;

   -----------------------------------------------------------------------
   procedure Preparing_Dialogs;
   procedure Preparing_Dialogs is
      WP_Internal_Child_Vbox        : Widget_Pointer;
      WP_GtkBox                     : Widget_Pointer;
      WP_Internal_Child_Action_Area : Widget_Pointer;
      WP_GtkButtonBox               : Widget_Pointer;
      WP_GtkFixed                   : Widget_Pointer;
      WP_Action_Widgets             : Widget_Pointer;
   begin
      Debug (-1, "");
      Debug (0, "Preparing_Dialogs");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow and then TWin.Is_Dialog then
            WP_Internal_Child_Vbox := new Widget_Properties
              (Widget_Type => Internal_Child_VBox);
            WP_Internal_Child_Vbox.Wdg_Parent := null;
            WP_Internal_Child_Vbox.Parent_Name := New_String (TWin.Name);
            WP_Internal_Child_Vbox.Orientation := Vertical;
            WP_Internal_Child_Vbox.Spacing := 2;
            WP_Internal_Child_Vbox.Visible := False;
            WP_Internal_Child_Vbox.Name := new String'("WP_Internal_Child_Vbox");
            Debug (0, Sp (3) & "Created widget "
                   & WP_Internal_Child_Vbox.Widget_Type'Image);

            WP_GtkBox := new Widget_Properties (GtkBox);
            WP_GtkBox.Wdg_Parent := WP_Internal_Child_Vbox;
            WP_GtkBox.Parent_Name := New_String (WP_Internal_Child_Vbox.Name);
            WP_GtkBox.Orientation := Vertical;
            WP_GtkBox.Spacing := 2;
            WP_GtkBox.Visible := False;
            WP_GtkBox.Name := new String'("WP_GtkBox");
            Debug (0, Sp (3) & "Created widget "
                   & WP_GtkBox.Widget_Type'Image);

            WP_Internal_Child_Action_Area :=
              new Widget_Properties (Internal_Child_Action_Area);
            WP_Internal_Child_Action_Area.Wdg_Parent := WP_GtkBox;
            WP_Internal_Child_Action_Area.Parent_Name :=
              New_String (WP_GtkBox.Name);
            WP_Internal_Child_Action_Area.Name :=
              new String'("WP_Internal_Child_Action_Area");
            Debug (0, Sp (3) & "Created widget "
                   & WP_Internal_Child_Action_Area.Widget_Type'Image);

            WP_GtkButtonBox := new Widget_Properties (GtkButtonBox);
            WP_GtkButtonBox.Wdg_Parent := WP_Internal_Child_Action_Area;
            WP_GtkButtonBox.Parent_Name :=
              New_String (WP_Internal_Child_Action_Area.Name);
            WP_GtkButtonBox.Layout_Style := Spread;
            WP_GtkButtonBox.Orientation := Vertical;
            WP_GtkButtonBox.Spacing := 0;
            WP_GtkButtonBox.Visible := False;
            WP_GtkButtonBox.Name := new String'("WP_GtkButtonBox");
            Debug (0, Sp (3) & "Created widget "
                   & WP_GtkButtonBox.Widget_Type'Image);

            WP_GtkFixed := new Widget_Properties (GtkFixed);
            WP_GtkFixed.Name := new String'(TWin.Name.all & "_Fixed");
            WP_GtkFixed.Wdg_Parent := WP_GtkBox;
            WP_GtkFixed.Parent_Name := New_String (WP_GtkBox.Name);
            WP_GtkFixed.Visible := True;
            WP_GtkFixed.Margins := TWin.Margins;
            Debug (0, Sp (3) & "Created widget "
                   & WP_GtkFixed.Widget_Type'Image);

            TWdg := TWin.Widget_List;
            while TWdg /= null loop
               if TWdg.Wdg_Parent = null then
                  TWdg.Wdg_Parent := WP_GtkFixed;
                  Free (TWdg.Parent_Name);
                  TWdg.Parent_Name := new String'(WP_GtkFixed.Name.all);
               end if;
               TWdg := TWdg. Next;
            end loop;

            WP_Action_Widgets := new Widget_Properties (Action_Widgets);
            WP_Action_Widgets.Win_Parent := TWin;
            WP_Action_Widgets.Wdg_Parent := null;
            WP_Action_Widgets.Parent_Name := New_String (TWin.Name);
            WP_Action_Widgets.Name := new String'("WP_Action_Widgets");
            Debug (0, Sp (3) & "Created widget "
                   & WP_Internal_Child_Vbox.Widget_Type'Image);

            if TWin.Action_Buttons (Delete_Response) /= null then
               Unlink_Widget (TWin, TWin.Action_Buttons (Delete_Response));
               TWin.Action_Buttons (Delete_Response).Wdg_Parent := WP_GtkButtonBox;
               Insert_Widget_By_Tail (TWin, TWin.Action_Buttons (Delete_Response));
               Debug (0, Sp (3) & "Attached Delete Button to action Area");
            end if;

            if TWin.Action_Buttons (Cancel_Response) /= null then
               Unlink_Widget (TWin, TWin.Action_Buttons (Cancel_Response));
               TWin.Action_Buttons (Cancel_Response).Wdg_Parent := WP_GtkButtonBox;
               Insert_Widget_By_Tail (TWin, TWin.Action_Buttons (Cancel_Response));
               Debug (0, Sp (3) & "Attached Cancel Button to action Area");
            end if;

            if TWin.Action_Buttons (OK_Response) /= null then
               Unlink_Widget (TWin, TWin.Action_Buttons (OK_Response));
               TWin.Action_Buttons (OK_Response).Wdg_Parent := WP_GtkButtonBox;
               Insert_Widget_By_Tail (TWin, TWin.Action_Buttons (OK_Response));
               Debug (0, Sp (3) & "Attached OK Button to action Area");
            end if;

            TWin.Num_Children := 0;
            Insert_Widget_By_Tail (TWin, WP_Internal_Child_Vbox);
            Insert_Widget_By_Tail (TWin, WP_GtkBox);
            Free (WP_GtkBox.Name);
            Insert_Widget_By_Tail (TWin, WP_Internal_Child_Action_Area);
            Insert_Widget_By_Tail (TWin, WP_GtkButtonBox);
            Free (WP_GtkButtonBox.Name);
            Insert_Widget_By_Tail (TWin, WP_GtkFixed);
            Insert_Widget_By_Tail (TWin, WP_Action_Widgets);

         end if;
         TWin := Next_Window (Win_List, TWin);
      end loop;
   end Preparing_Dialogs;

   ---------------------------------------------------------
   --  until now, each gtkwindow had a linear widget list --
   ---------------------------------------------------------

   -----------------------------------------------------------------------
   procedure Relinking_To_The_Correct_Parent_List;
   procedure Relinking_To_The_Correct_Parent_List is
   begin
      Debug (-1, "");
      Debug (0, "Relinking to the correct parent list");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            Relink_Children_To_Parent (TWin);
         end if;
         TWin := Next_Window (Win_List, TWin);
      end loop;
   end Relinking_To_The_Correct_Parent_List;

   --------------------------------------------------------------
   --  now each gtkwindow is a tree with its widgets and so on --
   --------------------------------------------------------------

   -----------------------------------------------------------------------
   procedure Removing_Gtkbox_With_Only_One_Child_Container;
   procedure Removing_Gtkbox_With_Only_One_Child_Container is
   begin
      Debug (-1, "");
      Debug (0, "Removing gtkbox with only one child container");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            Visit_GtkTree_Widget_For_GtkBox (TWin.Widget_List);
         end if;
         TWin := Next_Window (Win_List, TWin);
      end loop;
   end Removing_Gtkbox_With_Only_One_Child_Container;

   -----------------------------------------------------------------------
   procedure Generating_Format_Columns;
   procedure Generating_Format_Columns is
   begin
      Debug (-1, "");
      Debug (0, "Generating Format Columns");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            Visit_GtkTree_Widget_For_Columns (TWin.Widget_List);
         end if;
         TWin := Next_Window (Win_List, TWin);
      end loop;
   end Generating_Format_Columns;

   procedure Compute_Num_Element_For_Stores;
   procedure Compute_Num_Element_For_Stores is
   begin
      --  Must be after adjust format columns
      Debug (-1, "");
      Debug (0, "Compute number of elements for Stores");
      TWin := Win_List;
      while TWin /= null loop
         case TWin.Window_Type is
            when GtkListStore | GtkTreeStore =>
               TWdg := TWin.Associated_Widget;
               NCol := TWdg.Child_List;
               while NCol /= null loop
                  TWin.Num_Elements := TWin.Num_Elements + 1;
                  case NCol.Widget_Type is
                     when DataGridViewCheckBoxColumn =>
                        TWin.Num_Elements := TWin.Num_Elements + 1;
                        if not NCol.ReadOnly then
                           TWin.Num_Elements := TWin.Num_Elements + 1;
                        end if;
                     when ExpandableColumn | DataGridViewTextBoxColumn =>
                        if NCol.Text_Col_Properties.Fg_Color_Name_Column /= -1
                        then
                           TWin.Num_Elements := TWin.Num_Elements + 1;
                        end if;
                     when others => null;
                  end case;
                  NCol := NCol.Next;
               end loop;
               if DGVS /= null then
                  if TWdg.AlternatingRowsDefaultCellStyle in DGVS'Range then
                     TWin.Num_Elements := TWin.Num_Elements + 1;
                  end if;
               end if;
               Debug (0, Sp (3) & TWin.Name.all
                      & " =>" & TWin.Num_Elements'Image);
            when GtkModelFilter =>
               TWin.Num_Elements := TWin.Underlaying_Model.Num_Elements;
               Debug (0, Sp (3) & TWin.Name.all
                      & " =>" & TWin.Num_Elements'Image);
            when GtkModelSort =>
               TWin.Num_Elements := TWin.Underlaying_Model.Num_Elements;
               Debug (0, Sp (3) & TWin.Name.all
                      & " =>" & TWin.Num_Elements'Image);
            when others => null;
         end case;
         TWin := Next_Window (Win_List, TWin);
      end loop;
   end Compute_Num_Element_For_Stores;

   -----------------------------------------------------------------------
   procedure Recasting_Menus_Gtkmenuimageitem_With_No_Image;
   procedure Recasting_Menus_Gtkmenuimageitem_With_No_Image is
   begin
      Debug (-1, "");
      Debug (0, "Recast menus: "
             & "gtkmenuimageitem with no image => gtknormalmenuitem and");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            Visit_GtkMenuImageItem_Widget (TWin.Widget_List);
         end if;
         TWin := Next_Window (Win_List, TWin);
      end loop;
   end Recasting_Menus_Gtkmenuimageitem_With_No_Image;

   -----------------------------------------------------------------------
   procedure Use_Sort_For_Datagrids_And_Treegrids;
   procedure Use_Sort_For_Datagrids_And_Treegrids is
   begin
      Debug (-1, "");
      Debug (0, "Use Sort in data/treeviews");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            Visit_Use_Sort (TWin.Widget_List);
         end if;
         TWin := Next_Window (Win_List, TWin);
      end loop;
   end Use_Sort_For_Datagrids_And_Treegrids;

   -----------------------------------------------------------------------
   procedure Renumber_All;
   procedure Renumber_All is
   begin
      Debug (-1, "");
      Debug (0, "Renumbering");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            TWin.Num_Children := 0;
            TWdg := TWin.Widget_List;
            while TWdg /= null loop
               TWdg.Child_Number := TWin.Num_Children;
               if TWdg.Name /= null then
                  Debug (0, Sp (3) & TWdg.Name.all
                         & " renumbered to child num." & TWdg.Child_Number'Image);
               else
                  Debug (0, TWdg.Widget_Type'Image
                         & " renumbered to child num." & TWdg.Child_Number'Image);
               end if;
               TWin.Num_Children := TWin.Num_Children + 1;
               Visit_Renumber_Children (TWin.Widget_List);
               TWdg := TWdg.Next;
            end loop;
         end if;
         TWin := Next_Window (Win_List, TWin);
      end loop;
   end Renumber_All;

   -----------------------------------------------------------------------
   procedure Insert_Close_Button_Signal_For_Gtknotebooks;
   procedure Insert_Close_Button_Signal_For_Gtknotebooks is
   begin
      Debug (-1, "");
      Debug (0, "Insert_Close_Button_Signal_For_Gtknotebooks");

      Counter := 0;
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            TWdg := TWin.Widget_List;
            Visit_Widget_For_Close_Button_Signal_For_Gtknotebooks (TWdg);
         end if;
         TWin := Next_Window (Win_List, TWin);
      end loop;

   end Insert_Close_Button_Signal_For_Gtknotebooks;


   -----------------------------------------------------------------------
   function Adjust_To_Gtk return Integer is
   begin
      if Win_List = null then
         return -1; --  no windows
      end if;
      Debug (-1, "");
      Debug (-1, "Adjusting to GTK");

      -------------------------------------------------------
      --  Initially each widget list of a window is plain  --
      -------------------------------------------------------

      Generate_Auxiliary_Windows;

      Generate_Auxiliary_Widgets;

      Move_Windows_To_Auxiliary_List;

      Reordering_Windows_In_Separate_Lists;


      Join_Roots (Win_List, NWin0);
      Debug (0, "Joined both windows list");

      Set_Some_Properties_Of_Toolstripstatuslabel;

      Reordering_Sort_Models;

      Reordering_Filter_Models;

      Set_Have_Windows_And_Widgets;

      Result := Reparenting_Widgets_From_Parent_Name;
      if Result < 0 then
         return Result;
      end if;

      Set_Maxlength_For_Non_Editable;

      Recasting_Menus_Separator;

      Setting_Maxtabindex;

      Processing_Tabindex_And_Tabstop;

      Processing_Focus_Handler;

      Selecting_The_Has_Focus_Widget;

      Setting_The_Focus_Chain;

      Inheritable_Attributes;

      Processing_Labels_With_Aspect_Frame;

      Preparing_Action_Area;

      Preparing_Dialogs;

      ---------------------------------------------------------
      --  until now, each gtkwindow had a linear widget list --
      ---------------------------------------------------------

      Relinking_To_The_Correct_Parent_List;

      --------------------------------------------------------------
      --  now each gtkwindow is a tree with its widgets and so on --
      --------------------------------------------------------------

      Removing_Gtkbox_With_Only_One_Child_Container;

      Generating_Format_Columns;

      Compute_Num_Element_For_Stores;

      Recasting_Menus_Gtkmenuimageitem_With_No_Image;

      Use_Sort_For_Datagrids_And_Treegrids;

      Renumber_All;

      Insert_Close_Button_Signal_For_Gtknotebooks;

      Debug (-1, "End of Adjusting to GTK");
      return 0;
   end Adjust_To_Gtk;

end W2gtk_Adjust_To_Gtk_Pkg;
