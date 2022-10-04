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
with Ada.Integer_Text_IO;
with Ada.Calendar;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with GNAT.Strings;            use GNAT.Strings;
with GNAT.Calendar.Time_IO;
with GNATCOLL.Tribooleans;    use GNATCOLL.Tribooleans;
with GNATCOLL.Utils;          use GNATCOLL.Utils;
with W2gtk_Decls;             use W2gtk_Decls;
with W2gtk_Emit;              use W2gtk_Emit;
with Symbol_Tables;           use Symbol_Tables;

package body W2gtk_Pkg is
   package TIO renames Ada.Text_IO;
   --  package WinIO is new Ada.Text_IO.Enumeration_IO (Window_Enum);
   --  package WdgIO is new Ada.Text_IO.Enumeration_IO (Widget_Enum);
   package DSIO is new Ada.Text_IO.Enumeration_IO (Display_Style_Enum);
   package FDIO is new Ada.Text_IO.Enumeration_IO (FlowDirection_Enum);
   package PIO is new Ada.Text_IO.Enumeration_IO (Window_Position_Enum);
   package IPIO is new Ada.Text_IO.Enumeration_IO (Image_Position_Enum);
   package TAIO is new Ada.Text_IO.Enumeration_IO (TextAlign_Enum);
   package CHHSMIO is new Ada.Text_IO.Enumeration_IO
     (ColumnHeadersHeightSizeMode_Enum);
   package RHWSMIO is new Ada.Text_IO.Enumeration_IO
     (RowHeadersWidthSizeMode_Enum);
   package RHBSIO is new Ada.Text_IO.Enumeration_IO
     (RowHeadersBorderStyle_Enum);
   package BSIO is new Ada.Text_IO.Enumeration_IO (BorderStyle_Enum);
   package SMIO is new Ada.Text_IO.Enumeration_IO (SortMode_Enum);
   package ASMIO is new Ada.Text_IO.Enumeration_IO (AutoSizeMode_Enum);
   package ASCMIO is new Ada.Text_IO.Enumeration_IO (AutoSizeColumnMode_Enum);
   package SBIO is new Ada.Text_IO.Enumeration_IO (ScrollBars_Enum);
   package DRIO is new Ada.Text_IO.Enumeration_IO (DialogResult_Enum);
   package CSIO is new Ada.Text_IO.Enumeration_IO (Cell_Style_Enum);

   Test2  : constant String := "<data name=""$this.";
   Test5  : constant String := "<data name=""";
   Test1  : constant String := "<metadata name=""";
   Test9  : constant String := (1 .. 8 => ' ') & "'";
   Test10 : constant String := "New Decimal(New Integer() {";
   Test11 : constant String := "Handles";
   Test12 : constant String := ".ShowDialog()";
   Test13 : constant String := "Private Sub InitializeComponent()";
   Test8  : constant String := "Partial Class ";
   Test7  : constant String := "Me.components = New System.ComponentModel";
   Test18 : constant String := "Dim resources As System.ComponentModel";
   Test19 : constant String := "Dim DataGridViewCellStyle";
   Test20 : constant String := "DataGridViewCellStyle";

   Test3  : constant String := "Items.AddRange";
   Test4  : constant String := "DropDownItems.AddRange";
   Test6  : constant String := "Columns.AddRange";
   Test14 : constant String := "Controls.Add(Me.";
   Test15 : constant String := "Series.Add(";
   Test16 : constant String := "ChartAreas.Add(";
   Test17 : constant String := "Legends.Add(";
   --  specs
   function Adjust_To_Gtk return Integer;
   function Parse_Resource_File (TWin           : Window_Pointer;
                                 Resx_Path      : String;
                                 Resx_File_Name : String) return Integer;
   function Parse2_Designer_File (TWin           : Window_Pointer;
                                  Resx_Path      : String;
                                  Resx_File_Name : String;
                                  Icon_Path      : String) return Integer;
   function Parse1_Designer_File (Resx_Path      : String;
                                  Resx_File_Name : String) return Integer;
   function Parse_VB_File (Resx_Path      : String;
                           Resx_File_Name : String) return Integer;
   procedure Dump (Path : String; File_Name : String);
   --  end of specs

   function Adjust_To_Gtk return Integer is
      TWin  : Window_Pointer;
      TWdg  : Widget_Pointer;
      TWdgP : Widget_Pointer;
      Temp  : Widget_Pointer;
      NWin0 : Window_Pointer;
      NWin1 : Window_Pointer;
      NWin2 : Window_Pointer;
      NCol  : Widget_Pointer;
      TS    : Signal_Pointer;
      Found : Boolean;

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
                     Debug (0, Parent.Name.all
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

      procedure Visit_GtkTree_Widget_For_Toggle_Buttons
        (TWdg : Widget_Pointer);
      procedure Visit_GtkTree_Widget_For_Toggle_Buttons
        (TWdg : Widget_Pointer)
      is
         Num : Integer;
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
                  if Temp.Widget_Type = DataGridViewCheckBoxColumn then
                     Have.TreeViewToggles := Have.TreeViewToggles + 1;
                     Temp.CheckBox_Col_Properties.Active_Column := Num;
                     Num := Num + 1;
                     if not Temp.ReadOnly then
                        Temp.CheckBox_Col_Properties.Activatable_Column := Num;
                        Debug (0, Temp.Name.all
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
                           TS.Line    := -1;
                           Insert_Signal (Temp, TS); --  Toggled
                           Debug (0, Temp.Name.all
                                  & ": generated "
                                  & TS.Handler.all);
                        end if;
                     end if;
                  end if;
                  Temp := Temp.Next;
               end loop;
            end if;
         end if;

         Visit_GtkTree_Widget_For_Toggle_Buttons (TWdg.Next);
         Visit_GtkTree_Widget_For_Toggle_Buttons (TWdg.Child_List);
      end Visit_GtkTree_Widget_For_Toggle_Buttons;

      procedure Recast_To_GtkNormalMenuItem (TWdg : in out Widget_Pointer);
      procedure Recast_To_GtkNormalMenuItem (TWdg : in out Widget_Pointer) is
         Temp   : Widget_Pointer;
         Parent : constant Widget_Pointer := TWdg.GParent;
      begin
         Temp := new Widget_Properties (GtkMenuNormalItem);
         Copy_Common_Attributes (From => TWdg, To => Temp);
         Temp.ImageMenu := TWdg.ImageMenu;
         Replace (Parent, TWdg, Temp);
         Release (TWdg);
         TWdg := Temp;
         Have.MenuNormalItems := Have.MenuNormalItems + 1;
         Debug (0, "GtkMenuItemImage " & TWdg.Name.all
                & " GtkMenuNormalItem");
      end Recast_To_GtkNormalMenuItem;

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
               TWdg.GParent.Use_Sort := True;
               Found := Signal_Exists (TWdg, "Click");
               if not Found then
                  TS := new Signal_Block;
                  TS.Name := new String'("Click");
                  TS.Handler := new String'("On_"
                                            & TWdg.Name.all
                                            & "_Clicked");
                  Insert_Signal (TWdg, TS); --  Click
                  Debug (0, "Created Signal "
                         & TWdg.Name.all
                         & ".clicked");
               end if;
            end if;
         end if;

         Visit_Use_Sort (TWdg.Next);
         Visit_Use_Sort (TWdg.Child_List);
      end Visit_Use_Sort;

   begin
      if Win_List = null then
         return -1; --  no windows
      end if;

      -------------------------------------------------------
      --  Initially each widget list of a window is plain  --
      -------------------------------------------------------

      --  set some properties of toolstripstatuslabel
      Debug (0, "");
      Debug (0, "Adjusting to GTK: "
             & "set some properties of toolstripstatuslabel");
      TWin := Win_List;
      while TWin /= null loop
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
                     Debug (0,
                            "ToolStripStatusLabel "
                            & TWdg.Name.all
                            & ": Location adjusted");
                  end if;
               end if;
            end if;
            TWdg := TWdg.Next;
         end loop;
         TWin := TWin.Next;
      end loop;

      Debug (0, "");
      Debug (0, "Adjusting to GTK: GtkWindows or GtkDialog");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Resizable and then not TWin.Modal then
            TWin.Is_Dialog := False;
         else
            TWin.Is_Dialog := True;
         end if;
         TWin := TWin.Next;
      end loop;

      --  generate auxiliary elements
      Debug (0, "");
      Debug (0, "Adjusting to GTK: generate auxiliary windows");
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
                     NWin1.Name := new String'("Filefilter"
                                               & "_" & TWdg.Name.all
                                              );
                     NWin1.Title := TWdg.OpenFileTitle;
                     NWin1.FilterString := new
                       String'(TWdg.OpenFileFilter.all);
                     Free (TWdg.OpenFileFilter);
                     TWdg.OpenFileFilter := NWin1.Name;
                  end if;

                  NWin0 := new Window_Properties (GtkFileChooserDialog);
                  NWin0.Name := new String'("Filechooserdialog"
                                            & "_" & TWdg.Name.all
                                           );
                  NWin0.Title := TWdg.OpenFileTitle;
                  NWin0.FilterName := NWin1.Name;
                  NWin0.Transient_For := TWin;
                  NWin0.Attached_To   := TWdg;

                  if TWdg.OpenFileDialog = null then
                     TWdg.OpenFileDialog := NWin0.Name;
                  end if;
                  Insert_Window_By_Front (NWin0);
                  Debug (0, "Generated filechooserdialog for "
                         & NWin0.Name.all
                         & " for " & TWdg.Name.all
                         & " (" & TWdg.Widget_Type'Image & ")");

                  Insert_Window_By_Front (NWin1);
                  Debug (0, "Generated filefilter "
                         & NWin1.Name.all
                         & " for " & TWdg.Name.all
                         & " (" & TWdg.Widget_Type'Image & ")");

               when GtkEntry | GtkComboTextBox =>
                  if TWdg.Text_Buffer /= null
                    and then TWdg.Text_Buffer.all /= ""
                  then
                     Num_Aux_Widgets := Num_Aux_Widgets + 1;
                     NWin1 := new Window_Properties (GtkEntryBuffer);
                     NWin1.Name := new String'("Fntrybuffer"
                                               & "_" & TWdg.Name.all
                                              );
                     NWin1.Associated_Widget := TWdg;
                     TWdg.Buffer := NWin1;
                     Insert_Window_By_Front (NWin1);
                     Debug (0, "Generated entrybuffer for "
                            & NWin1.Name.all
                            & " for " & TWdg.Name.all
                            & " (" & TWdg.Widget_Type'Image & ")");
                  end if;

               when GtkCalendar =>
                  Num_Aux_Widgets := Num_Aux_Widgets + 1;
                  NWin1 := new Window_Properties (GtkEntryBuffer);
                  NWin1.Name := new String'("Fntrybuffer"
                                            & "_" & TWdg.Name.all
                                           );
                  NWin1.Associated_Widget := TWdg;
                  TWdg.Buffer := NWin1;
                  Insert_Window_By_Front (NWin1);
                  Debug (0, "Generated entrybuffer for "
                         & NWin1.Name.all
                         & " for " & TWdg.Name.all
                         & " (" & TWdg.Widget_Type'Image & ")");

               when GtkListBox =>
                  Num_Aux_Widgets := Num_Aux_Widgets + 1;
                  NWin1 := new Window_Properties (GtkListStore);
                  NWin1.Name := new String'("Liststore"
                                            & "_" & TWdg.Name.all
                                           );
                  NWin1.Associated_Widget := TWdg;
                  TWdg.ListStore := NWin1;
                  Insert_Window_By_Front (NWin1);
                  Debug (0, "Generated liststore for "
                         & NWin1.Name.all
                         & " for " & TWdg.Name.all
                         & " (" & TWdg.Widget_Type'Image & ")");

               when GtkButton | GtkRadioButton
                  | GtkCheckButton | GtkToggleButton =>
                  if TWdg.ImagePath /= null then
                     Num_Aux_Widgets := Num_Aux_Widgets + 1;
                     NWin1 := new Window_Properties (GtkImage);
                     NWin1.Name := new String'("Gtkimage"
                                               & "_" & TWdg.Name.all
                                              );
                     NWin1.Associated_Widget := TWdg;
                     TWdg.Win_Image := NWin1;
                     Insert_Window_By_Front (NWin1);
                     Debug (0, "Generated gtkimage for "
                            & NWin1.Name.all
                            & " for " & TWdg.Name.all
                            & " (" & TWdg.Widget_Type'Image & ")");
                  end if;

               when GtkMenuImageItem =>
                  if TWdg.ImageMenu /= null then
                     Num_Aux_Widgets := Num_Aux_Widgets + 1;
                     NWin1 := new Window_Properties (GtkImage);
                     NWin1.Name := new String'("Gtkimage"
                                               & "_" & TWdg.Name.all
                                              );
                     NWin1.Associated_Widget := TWdg;
                     TWdg.ImageMenuWin := NWin1;
                     Insert_Window_By_Front (NWin1);
                     Debug (0, "Generated gtkimage for "
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
                     NWin0.Name := new String'("Gtkliststore"
                                               & "_"
                                               & Normalize_Name (TWdg)
                                              );
                     NWin0.Associated_Widget := TWdg;
                     Debug (0, "Generated gtkliststore for "
                            & NWin0.Name.all
                            & " for " & Normalize_Name (TWdg)
                            & " (" & TWdg.Widget_Type'Image & ")");
                  else
                     Num_Aux_Widgets := Num_Aux_Widgets + 1;
                     NWin0 := new Window_Properties (GtkTreeStore);
                     NWin0.Name := new String'("Gtktreestore"
                                               & "_"
                                               & Normalize_Name (TWdg)
                                              );
                     NWin0.Associated_Widget := TWdg;
                     Debug (0, "Generated gtktreestore for "
                            & NWin0.Name.all
                            & " for " & Normalize_Name (TWdg)
                            & " (" & TWdg.Widget_Type'Image & ")");
                  end if;
                  --  generate the filter model
                  Num_Aux_Widgets := Num_Aux_Widgets + 1;
                  NWin1 := new Window_Properties (GtkModelFilter);
                  NWin1.Name := new String'("Gtkmodelfilter"
                                            & "_"
                                            & Normalize_Name (TWdg)
                                           );
                  NWin1.Associated_Widget := TWdg;
                  NWin1.Underlying_Model  := NWin0;
                  Debug (0, "Generated gtkmodelfilter for "
                         & NWin1.Name.all
                         & " for " & NWin0.Name.all
                         & " (" & TWdg.Widget_Type'Image & ")");
                  --  generate the sort model
                  Num_Aux_Widgets := Num_Aux_Widgets + 1;
                  NWin2 := new Window_Properties (GtkModelSort);
                  NWin2.Name := new String'("Gtkmodelsort"
                                            & "_"
                                            & Normalize_Name (TWdg)
                                           );
                  NWin2.Associated_Widget := TWdg;
                  NWin2.Underlying_Model  := NWin1;
                  Debug (0, "Generated gtkmodelsort for "
                         & NWin2.Name.all
                         & " for " & NWin1.Name.all
                         & " (" & TWdg.Widget_Type'Image & ")");

                  --   grab the model (sort)
                  TWdg.Model := NWin2;

                  --  insert the objects
                  Insert_Window_By_Front (NWin2);
                  Insert_Window_By_Front (NWin1);
                  Insert_Window_By_Front (NWin0);

               when GtkTabPage =>
                  Num_Aux_Widgets := Num_Aux_Widgets + 1;
                  declare
                     Wdg1 : Widget_Pointer;
                     Wdg0 : Widget_Pointer;
                  begin
                     --  create the tab box
                     Wdg0 := new Widget_Properties (GtkTabChild);
                     Wdg0.Name := new String'(TWdg.Name.all & "_Box");
                     Wdg0.Parent_Name := new String'(TWdg.Name.all);
                     Insert_Widget_By_Tail (TWin, Wdg0);
                     TWdg.Num_Children := TWdg.Num_Children + 1;
                     Wdg0.Child_Num := TWdg.Num_Children;
                     Debug (0, "Generated tab box "
                            & Wdg0.Name.all
                            & " for " & TWdg.Name.all);

                     --  create the label
                     Wdg1 := new Widget_Properties (GtkLabel);
                     Wdg1.Name := new String'(TWdg.Name.all & "_Label");
                     Wdg1.Windows_Type :=
                       new String'("System.Windows.Forms.Label");
                     Wdg1.Parent_Name := new String'(Wdg0.Name.all);
                     Wdg1.Text := new String'(TWdg.Text.all);
                     Insert_Widget_By_Tail (TWin, Wdg1);
                     Wdg0.Num_Children := Wdg0.Num_Children + 1;
                     Wdg1.Child_Num := Wdg0.Num_Children;
                     TWdg.The_Label := Wdg1;
                     Debug (0, "Generated gtklabel for "
                            & Wdg1.Name.all & " from "
                            & TWdg.Name.all);

                     --  create the button
                     Wdg1 := new Widget_Properties (GtkButton);
                     Wdg1.Name := new String'(TWdg.Name.all & "_Button");
                     Wdg1.Parent_Name := new String'(Wdg0.Name.all);
                     Insert_Widget_By_Tail (TWin, Wdg1);
                     Wdg0.Num_Children := Wdg0.Num_Children + 1;
                     Wdg1.Child_Num := Wdg0.Num_Children;
                     TWdg.The_Button := Wdg1;
                     Debug (0, "Generated gtkbutton for "
                            & Wdg1.Name.all & " from "
                            & TWdg.Name.all);
                  end;

               when others => null;
            end case;
            TWdg := TWdg.Next;
         end loop;
         TWin := TWin.Next;
      end loop;

      --  set correct parent from parent name and set Gparent
      Debug (0, "");
      Debug (0, "Adjusting to GTK: reparenting from parent name");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            TWdg := TWin.Widget_List;
            while TWdg /= null loop
               if TWdg.Parent_Name /= null then
                  if TWdg.Parent_Name.all = "$this" then
                     TWdg.WParent := TWin;
                     Free (TWdg.Parent_Name);
                     TWdg.Parent_Name := new String'(TWdg.WParent.Name.all);
                     Debug (0, "Reparenting Widget " & TWdg.Name.all
                            & " to Window " & TWdg.WParent.Name.all);
                  else
                     TWdgP := Find_Widget (TWin.Widget_List,
                                           TWdg.Parent_Name.all);
                     if TWdgP = null then
                        TIO.Put_Line ("Widget " & TWdg.Name.all
                                      & " without parent, "
                                      & " tried " & TWdg.Parent_Name.all);
                        return -1;
                     end if;
                     TWdg.GParent := TWdgP;
                     Debug (0, "Reparenting Widget "
                            & TWdg.Name.all
                            & " to "
                            & TWdg.GParent.Name.all);
                  end if;
               else
                  TWdg.WParent := TWin;
                  TWdg.Parent_Name := new String'(TWin.Name.all);
                  Debug (0, "Reparenting Widget " & TWdg.Name.all
                         & " to Window " & TWdg.WParent.Name.all);
               end if;
               TWdg := TWdg.Next;
            end loop;
         end if;
         TWin := TWin.Next;
      end loop;

      Debug (0, "");
      Debug (0, "Adjusting to GTK: set maxlength for non-editable gtkentries, "
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
                              Debug (0, TWdg.Name.all
                                     & ".Maxlength = "
                                     & Img (TWdg.MaxLength));
                           end if;
                        else
                           TWdg.MaxLength := TWdg.Size.Horiz / 8;
                        end if;
                     when GtkComboTextBox =>
                        TWdg.MaxLength := TWdg.Size.Horiz / 10;
                     when GtkLabel =>
                        if TWdg.MaxLength < 1
                          and then TWdg.Text /= null
                        then
                           TWdg.MaxLength := TWdg.Text.all'Length;
                           Debug (0, TWdg.Name.all
                                  & ".Maxlength = "
                                  & Img (TWdg.MaxLength));
                        end if;
                     when others => null;
                  end case;
               end if;
               TWdg := TWdg.Next;
            end loop;
         end if;
         TWin := TWin.Next;
      end loop;

      Debug (0, "");
      Debug (0, "Adjusting to GTK: recast menus: "
             & "GtkSeparatorToolItem in Menus => GtkSeparatorMenuItem");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            TWdg := TWin.Widget_List;
            while TWdg /= null loop
               if TWdg.Widget_Type = GtkSeparatorToolItem then
                  if TWdg.GParent.Widget_Type = GtkMenuBar or else
                    TWdg.GParent.Widget_Type = GtkMenuItem or else
                    TWdg.GParent.Widget_Type = GtkMenuImageItem
                  then
                     Temp := new Widget_Properties (GtkSeparatorMenuItem);
                     Copy_Common_Attributes (From => TWdg, To => Temp);
                     Replace (TWin, TWdg, Temp);
                     Release (TWdg);
                     TWdg := Temp;
                     Have.MenuSeparators := Have.MenuSeparators + 1;
                     Have.ToolSeparators := Have.ToolSeparators - 1;
                     Debug (0, "GtkSeparatorToolItem " & TWdg.Name.all
                            & " => GtkSeparatorMenuItem");
                  end if;
               end if;
               TWdg := TWdg.Next;
            end loop;
         end if;
         TWin := TWin.Next;
      end loop;

      Debug (0, "");
      Debug (0, "Adjusting to GTK: setting maxtabindex");
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
            Debug (0, "Set Window Property " & TWin.Name.all
                   & ".MaxTabIndex " & Image (TWin.MaxTabIndex, 0));
            TWin.MinTabIndex := TWin.MaxTabIndex;
         end if;
         TWin := TWin.Next;
      end loop;

      Debug (0, "");
      Debug (0, "Adjusting to GTK: process tabindex and tabstop");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            TWdg := TWin.Widget_List;
            while TWdg /= null loop
               if TWdg.TabStop = Indeterminate then
                  case TWdg.Widget_Type is
                     when GtkLabel =>
                        TWdg.TabStop := False;
                        Debug (0, "Set Widget Property "
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
                           Debug (0, "Set Widget Property "
                                  & TWdg.Name.all
                                  & ".TabStop True");
                        end if;
                        if TWdg.TabIndex < 0 then
                           TWin.MaxTabIndex := @ + 1;
                           TWdg.TabIndex := TWin.MaxTabIndex;
                           Debug (0, "Set Widget Property "
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
         TWin := TWin.Next;
      end loop;

      Debug (0, "");
      Debug (0, "Adjusting to GTK: inserting focus handler "
             & "(only for dialogs)");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            if TWin.Is_Dialog then
               TWdg := TWin.Widget_List;
               while TWdg /= null loop
                  if GNATCOLL.Tribooleans."=" (TWdg.TabStop, True) then
                     case TWdg.Widget_Type is
                        when GtkLabel | GtkEntry | GtkComboTextBox
                           | GtkButton | GtkRadioButton
                           | GtkCheckButton | GtkToggleButton
                           =>
                           --  search for a signal named "leave"
                           Found := Signal_Exists (TWdg, "LeaveFocus");
                           if not Found then
                              TS := new Signal_Block;
                              TS.Name := new String'("LeaveFocus");
                              TS.Handler := new String'("On_"
                                                        & TWdg.Name.all
                                                        & "_Focus");
                              Insert_Signal (TWdg, TS); --  LeaveFocus
                              Debug (0, "Created Signal "
                                     & TWdg.Name.all
                                     & ".leave");
                           end if;
                        when others => null;
                     end case;
                  end if;
                  TWdg := TWdg.Next;
               end loop;
            end if;
         end if;
         TWin := TWin.Next;
      end loop;

      Debug (0, "");
      Debug (0, "Adjusting to GTK: selecting the has-focus widget");
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
               Debug (0, "Set Widget Property "
                      & TWin.Has_Focus_Widget.Name.all
                      & ".Has_Focus True");
            end if;
         end if;
         TWin := TWin.Next;
      end loop;

      Debug (0, "");
      Debug (0, "Adjusting to GTK: setting the focus chain");
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
               Debug (0, "Focus chain for " & TWin.Name.all);
               loop
                  Debug (0, Sp (3) & TWdg.Name.all
                         & ".TabIndex " & TWdg.TabIndex'Image);
                  TWdg := TWdg.Next_Focus;
                  exit when TWdg = TWin.TabFocusList;
               end loop;
            end if;
         end if;
         TWin := TWin.Next;
      end loop;

      --  process inheritable attributes (font, others?)
      Debug (0, "");
      Debug (0, "Adjusting to GTK: inheritable attributes");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            Process_Inheritable (TWin);
         end if;
         TWin := TWin.Next;
      end loop;

      ---------------------------------------------------------
      --  until now, each gtkwindow had a linear widget list --
      ---------------------------------------------------------

      --  reorder the widgets placing each widget in the correct parent list
      Debug (0, "");
      Debug (0, "Adjusting to GTK: relinking to the correct parent list");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            Relink_Children_To_Parent (TWin);
         end if;
         TWin := TWin.Next;
      end loop;

      --  compute num_elements for stores
      Debug (0, "");
      Debug (0, "Adjusting to GTK: compute num. elements for models");
      TWin := Win_List;
      while TWin /= null loop
         case TWin.Window_Type is
            when GtkListStore | GtkTreeStore =>
               TWdg := TWin.Associated_Widget;
               NCol := TWdg.Child_List;
               while NCol /= null loop
                  TWin.Num_Elements := TWin.Num_Elements + 1;
                  if NCol.Widget_Type = DataGridViewCheckBoxColumn then
                     TWin.Num_Elements := TWin.Num_Elements + 1;
                     if not NCol.ReadOnly then
                        TWin.Num_Elements := TWin.Num_Elements + 1;
                     end if;
                  end if;
                  NCol := NCol.Next;
               end loop;
               if TWdg.AlternatingRowsDefaultCellStyle in DGVS'Range then
                  TWin.Num_Elements := TWin.Num_Elements + 1;
               end if;
            when GtkModelFilter =>
               TWin.Num_Elements := TWin.Underlying_Model.Num_Elements;
            when GtkModelSort =>
               TWin.Num_Elements := TWin.Underlying_Model.Num_Elements;
            when others => null;
         end case;
         TWin := TWin.Next;
      end loop;

      --  remove a gtkbox parent which contains only one child container
      Debug (0, "");
      Debug (0, "Removing gtkbox with only one child container");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            Visit_GtkTree_Widget_For_GtkBox (TWin.Widget_List);
         end if;
         TWin := TWin.Next;
      end loop;

      --  generating format columns for toggle buttons
      Debug (0, "");
      Debug (0, "Preparing toggle buttons");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            Visit_GtkTree_Widget_For_Toggle_Buttons (TWin.Widget_List);
         end if;
         TWin := TWin.Next;
      end loop;

      --  recast menus: gtkmenuimageitem with no image => gtknormalmenuitem
      Debug (0, "");
      Debug (0, "Adjusting to GTK: recast menus:"
             & "gtkmenuimageitem with no image => gtknormalmenuitem and");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            Visit_GtkMenuImageItem_Widget (TWin.Widget_List);
         end if;
         TWin := TWin.Next;
      end loop;

      --  setting use_sort for data/treegrids
      Debug (0, "");
      Debug (0, "Adjusting to GTK: Use Sort in data/treeviews");
      TWin := Win_List;
      while TWin /= null loop
         if TWin.Window_Type = GtkWindow then
            Visit_Use_Sort (TWin.Widget_List);
         end if;
         TWin := TWin.Next;
      end loop;
      Debug (0, "End of Adjust");
      return 0;
   end Adjust_To_Gtk;

   procedure Dump (Path : String; File_Name : String) is
      TWin  : Window_Pointer;

      use GNAT.Calendar.Time_IO;
      procedure Dump_DGVS;
      procedure Dump_Window (TWin  : Window_Pointer; Id : Integer);
      procedure Dump_Widget (TWdgP : Widget_Pointer; Id : Integer);

      procedure Put_Property (PName : String);
      procedure Put_Property (PName : String) is
      begin
         TIO.Put (LFile, Sp (3)
                  & PName
                  & Sp (24 - PName'Length));
      end Put_Property;

      procedure Put_Integer (N : Integer);
      procedure Put_Integer (N : Integer) is
      begin
         TIO.Put (LFile, Img (N));
         TIO.New_Line (LFile);
      end Put_Integer;

      procedure Put_String_Access (SA : String_Access;
                                   Quoted : Boolean := False);
      procedure Put_String_Access (SA : String_Access;
                                   Quoted : Boolean := False) is
      begin
         if SA /= null then
            if Quoted then
               TIO.Put (LFile, """" & SA.all & """");
            else
               TIO.Put (LFile, SA.all);
            end if;
         end if;
         TIO.New_Line (LFile);
      end Put_String_Access;

      procedure Put_Boolean (B : Boolean);
      procedure Put_Boolean (B : Boolean) is
      begin
         if B then
            TIO.Put (LFile, "True");
         else
            TIO.Put (LFile, "False");
         end if;
         TIO.New_Line (LFile);
      end Put_Boolean;

      procedure Put_Triboolean (B : Triboolean);
      procedure Put_Triboolean (B : Triboolean) is
      begin
         if GNATCOLL.Tribooleans."=" (B, True) then
            TIO.Put (LFile, "True");
         elsif GNATCOLL.Tribooleans."=" (B, False) then
            TIO.Put (LFile, "False");
         else
            TIO.Put (LFile, "Indeterminate");
         end if;
         TIO.New_Line (LFile);
      end Put_Triboolean;

      procedure Dump_DGVS is
      begin
         if DGVS = null then
            return;
         end if;
         for I in DGVS.all'Range loop
            if I /= DGVS.all'First then
               TIO.New_Line (LFile);
            end if;

            TIO.Put_Line (LFile, "Gtk_Cell_Renderer");

            Put_Property ("Name");
            Put_String_Access (DGVS (I).Name);

            Put_Property ("Num");
            Put_Integer (DGVS (I).Num);

            Put_Property ("Style_For");
            CSIO.Put (LFile, DGVS (I).Style_For);
            TIO.New_Line (LFile);

            Put_Property ("Alignment");
            Put_String_Access (DGVS (I).Alignment);

            Put_Property ("BgColor");
            Put_String_Access (DGVS (I).BgColor);

            Put_Property ("FgColor");
            Put_String_Access (DGVS (I).FgColor);

            Put_Property ("SelBgColor");
            Put_String_Access (DGVS (I).SelBgColor);

            Put_Property ("SelFgColor");
            Put_String_Access (DGVS (I).SelFgColor);

            Put_Property ("Font");
            if DGVS (I).Font_Name /= null then
               TIO.Put (LFile, DGVS (I).Font_Name.all);
               TIO.Put (LFile, " " & Img (DGVS (I).Font_Size));
               if DGVS (I).Font_Weight /= null then
                  TIO.Put (LFile, " " & "+" (DGVS (I).Font_Weight.all));
               end if;
               if DGVS (I).Font_Underline then
                  TIO.Put (LFile, " underline");
               end if;
            end if;
            TIO.New_Line (LFile);

            Put_Property ("Format");
            TIO.Put_Line (LFile, DGVS (I).Format'Image);

            Put_Property ("Padding");
            TIO.Put (LFile, "Top " & Img (DGVS (I).Padding (1))
                     & ", Bottom " & Img (DGVS (I).Padding (3))
                     & ", Start " & Img (DGVS (I).Padding (2))
                     & ", End " & Img (DGVS (I).Padding (4)));
            TIO.New_Line (LFile);

            Put_Property ("WrapMode");
            Put_Boolean (DGVS (I).WrapMode);

            Put_Property ("NullValue");
            Put_String_Access (DGVS (I).NullValue);

         end loop;
      end Dump_DGVS;

      procedure Dump_Window (TWin  : Window_Pointer; Id : Integer) is

         procedure Put_Property (PName : String);
         procedure Put_Property (PName : String) is
         begin
            TIO.Put (LFile, Sp (3)
                     & PName
                     & Sp (24 - PName'Length));
         end Put_Property;

         procedure Dump_Signal_List;
         procedure Dump_Signal_List is
            TS : Signal_Pointer;
         begin
            TS := TWin.Signal_List;
            while TS /= null loop
               Put_Property ("Signal");
               TIO.Put (LFile, TS.Name.all
                        & " ("
                        & Convert_Signal_To_Gtk (TWin, TS.Name.all)
                        & ") ");
               if TS.Handler /= null then
                  TIO.Put (LFile, " => " & TS.Name.all);
               end if;
               TIO.New_Line (LFile);
               TS := TS.Next;
            end loop;
         end Dump_Signal_List;

         procedure Put_Widget_Pointer_Name (WP : Widget_Pointer);
         procedure Put_Widget_Pointer_Name (WP : Widget_Pointer) is
         begin
            if WP /= null and then WP.Name /= null then
               TIO.Put (LFile, WP.Name.all);
            end if;
            TIO.New_Line (LFile);
         end Put_Widget_Pointer_Name;

         procedure Dump_Children;
         procedure Dump_Children is
            TWdg : Widget_Pointer;
         begin
            TWdg := TWin.Widget_List;
            while TWdg /= null loop
               Dump_Widget (TWdg, Id + 3);
               TWdg := TWdg.Next;
            end loop;
         end Dump_Children;

         procedure Dump_Associated_Widget;
         procedure Dump_Associated_Widget is
         begin
            Put_Property ("Associated Widget");
            Put_Widget_Pointer_Name (TWin.Associated_Widget);
         end Dump_Associated_Widget;

      begin --  dump window
         TIO.New_Line (LFile);
         TIO.Put (LFile, To_Gtk (TWin));
         TIO.New_Line (LFile);

         Put_Property ("Name");
         Put_String_Access (TWin.Name);

         case TWin.Window_Type is
            when GtkWindow =>
               Put_Property ("Title");
               Put_String_Access (TWin.Title, True);

               Put_Property ("Resizable");
               Put_Boolean (TWin.Resizable);

               Put_Property ("Modal");
               Put_Boolean (TWin.Modal);

               Put_Property ("Font");
               if TWin.Font_Name /= null then
                  TIO.Put (LFile, TWin.Font_Name.all);
                  TIO.Put (LFile, " " & Img (TWin.Font_Size));
                  if TWin.Font_Weight /= null then
                     TIO.Put (LFile, " " & "+" (TWin.Font_Weight.all));
                  end if;
                  if TWin.Font_Underline then
                     TIO.Put (LFile, " underline");
                  end if;
               end if;
               TIO.New_Line (LFile);

               Put_Property ("Icon");
               Put_String_Access (TWin.Icon);

               Put_Property ("ToolTip");
               Put_String_Access (TWin.ToolTip, True);

               Put_Property ("Position");
               PIO.Put (LFile, TWin.Start_Position);
               TIO.New_Line (LFile);

               Put_Property ("Size");
               TIO.Put (LFile, "H=" & Img (TWin.Client_Size.Horiz));
               TIO.Put (LFile, ", V=" & Img (TWin.Client_Size.Vert));
               TIO.New_Line (LFile);

               Put_Property ("Margin");
               TIO.Put (LFile, "Top " & Img (TWin.Margins (1))
                        & ", Bottom " & Img (TWin.Margins (3))
                        & ", Start " & Img (TWin.Margins (2))
                        & ", End " & Img (TWin.Margins (4)));
               TIO.New_Line (LFile);

               Put_Property ("AutoScale");
               TIO.Put (LFile, "Horiz " & Img (TWin.AutoScaleDim.Horiz));
               TIO.Put (LFile, ", Vert " & Img (TWin.AutoScaleDim.Vert));
               TIO.New_Line (LFile);

               Put_Property ("TryHeight");
               Put_Integer (TWin.TrayHeight);

               Put_Property ("Max Tab Index");
               Put_Integer (TWin.MaxTabIndex);

               Put_Property ("BGColor");
               Put_String_Access (TWin.BgColor);

               Put_Property ("FGColor");
               Put_String_Access (TWin.FgColor);

               Put_Property ("Accept Button");
               Put_Widget_Pointer_Name (TWin.Accept_Button);

               Put_Property ("Cancel Button");
               Put_Widget_Pointer_Name (TWin.Cancel_Button);

               Dump_Signal_List;
               Dump_Children;

            when GtkFileChooserDialog =>
               Put_Property ("Title");
               Put_String_Access (TWin.Title, True);

               Put_Property ("Filter Name");
               Put_String_Access (TWin.FilterName);

               Put_Property ("Transient for");
               Put_String_Access (TWin.Transient_For.Name);

               Put_Property ("Attached to");
               Put_String_Access (TWin.Attached_To.Name);

               Dump_Signal_List;

            when GtkFileFilter =>
               Put_Property ("Title");
               Put_String_Access (TWin.Title, True);

               Put_Property ("Filter String");
               if TWin.FilterString /= null then
                  TIO.Put (LFile, TWin.FilterString.all);
               end if;
               TIO.New_Line (LFile);

               Dump_Signal_List;

            when GtkEntryBuffer =>
               Dump_Associated_Widget;

               case TWin.Associated_Widget.Widget_Type is
                  when GtkCalendar =>
                     Put_Property ("Min_Date");
                     TIO.Put_Line (LFile,
                                   Image (TWin.Associated_Widget.MinDate,
                                   ISO_Date));

                     Put_Property ("Max_Date");
                     TIO.Put_Line (LFile,
                                   Image (TWin.Associated_Widget.MaxDate,
                                     ISO_Date));

                  when GtkEntry | GtkComboTextBox =>
                     declare
                        Idx0 : Integer;
                        Idx1 : Integer;
                        Txt  : constant String :=
                          TWin.Associated_Widget.Text_Buffer.all;
                     begin
                        Idx0 := Index (Txt, (1 .. 1 => ASCII.CR));
                        if Idx0 not in Txt'Range then
                           Put_Property ("Text Buffer");
                           TIO.Put_Line (LFile, Txt);
                        else
                           Put_Property ("Text Buffer");
                           TIO.Put_Line (LFile, Txt (Txt'First .. Idx0 - 1));
                           loop
                              Idx0 := Idx0 + 1; -- skip CR
                              Idx1 := Index (Txt (Idx0 .. Txt'Last),
                                             (1 .. 1 => ASCII.CR));
                              exit when Idx1 not in Idx0 .. Txt'Last;
                              Put_Property ("");
                              TIO.Put_Line (LFile, Txt (Idx0 .. Idx1 - 1));
                              Idx0 := Idx1; -- idx0 now is CR
                           end loop;
                           if Idx0 < Txt'Last then
                              Put_Property ("");
                              TIO.Put_Line (LFile, Txt (Idx0 .. Txt'Last));
                           end if;
                        end if;
                     end;
                  when others => null;
               end case;
               Dump_Signal_List;

            when GtkModelFilter =>
               Put_Property ("Underlying Model");
               Put_String_Access (TWin.Underlying_Model.Name);

               Dump_Associated_Widget;
               Dump_Signal_List;

            when GtkModelSort =>
               Put_Property ("Underlying Model");
               Put_String_Access (TWin.Underlying_Model.Name);

               Dump_Associated_Widget;
               Dump_Signal_List;

            when GtkListStore =>
               Put_Property ("Num. Elements");
               Put_Integer (TWin.Num_Elements);

               Dump_Associated_Widget;
               Dump_Signal_List;

            when GtkTreeStore =>
               Put_Property ("Num. Elements");
               Put_Integer (TWin.Num_Elements);

               Dump_Associated_Widget;
               Dump_Signal_List;

            when GtkImage =>
               Dump_Associated_Widget;
               Put_Property ("Image");
               case TWin.Associated_Widget.Widget_Type is
                  when GtkButton | GtkRadioButton
                     | GtkCheckButton | GtkToggleButton =>
                     TIO.Put_Line (LFile,
                                   TWin.Associated_Widget.ImagePath.all);
                  when GtkMenuNormalItem | GtkMenuImageItem
                     | GtkMenuRadioItem | GtkMenuCheckItem =>
                     TIO.Put_Line (LFile,
                                   TWin.Associated_Widget.ImageMenu.all);
                  when others => null;
               end case;
               Dump_Signal_List;

         end case;
      end Dump_Window;

      procedure Dump_Widget (TWdgP : Widget_Pointer; Id : Integer) is

         procedure Put_Property (PName : String);
         procedure Put_Property (PName : String) is
         begin
            TIO.Put (LFile, Sp (Id + 3)
                     & PName
                     & Sp (24 - PName'Length));
         end Put_Property;

         procedure Put_Window_Pointer_Name (WP : Window_Pointer);
         procedure Put_Window_Pointer_Name (WP : Window_Pointer) is
         begin
            if WP /= null and then WP.Name /= null then
               TIO.Put (LFile, WP.Name.all);
            end if;
            TIO.New_Line (LFile);
         end Put_Window_Pointer_Name;

         procedure Put_Widget_Pointer_Name (WP : Widget_Pointer);
         procedure Put_Widget_Pointer_Name (WP : Widget_Pointer) is
         begin
            if WP /= null and then WP.Name /= null then
               TIO.Put (LFile, WP.Name.all);
            end if;
            TIO.New_Line (LFile);
         end Put_Widget_Pointer_Name;

         procedure Dump_Children;
         procedure Dump_Children is
            TWdgChild : Widget_Pointer;
         begin
            TWdgChild := TWdgP.Child_List;
            while TWdgChild /= null loop
               Dump_Widget (TWdgChild, Id + 3);
               TWdgChild := TWdgChild.Next;
            end loop;
         end Dump_Children;

         procedure Dump_Signals;
         procedure Dump_Signals is
            TS : Signal_Pointer;
         begin
            TS := TWdgP.Signal_List;
            while TS /= null loop
               if TS.After then
                  Put_Property ("Signal (after)");
               else
                  Put_Property ("Signal (before)");
               end if;
               TIO.Put (LFile, TS.Name.all
                        & " ("
                        & Convert_Signal_To_Gtk (TWdgP, TS.Name.all)
                        & ") ");
               if TS.Handler /= null then
                  if TS.Proc then
                     TIO.Put (LFile, " => procedure " & TS.Handler.all);
                  else
                     TIO.Put (LFile, " => function " & TS.Handler.all);
                  end if;
               end if;
               TIO.New_Line (LFile);
               TS := TS.Next;
            end loop;
         end Dump_Signals;

      begin
         TIO.New_Line (LFile);

         --  common fields
         TIO.Put (LFile, Sp (Id) & To_Gtk (TWdgP));
         TIO.New_Line (LFile);

         Put_Property ("Name");
         Put_String_Access (TWdgP.Name);

         if TWdgP.Widget_Type /= GtkTabChild then
            Put_Property ("Windows Form");
            Put_String_Access (TWdgP.Windows_Type);
         end if;

         Put_Property ("Number of Children");
         Put_Integer (TWdgP.Num_Children);

         Put_Property ("Parent");
         if TWdgP.Parent_Name /= null then
            Put_String_Access (TWdgP.Parent_Name);
         elsif TWdgP.GParent /= null
           and then TWdgP.GParent.Windows_Type /= null
         then
            TIO.Put (LFile, To_Gtk (TWdgP.GParent));
            TIO.New_Line (LFile);
         elsif TWdgP.WParent /= null then
            --  WinIO.Put (LFile, TWdgP.WParent.Window_Type);
            TIO.Put (LFile, To_Gtk (TWdgP.WParent));
            TIO.New_Line (LFile);
         end if;

            Put_Property ("Child Number");
            Put_Integer (TWdgP.Child_Num);

         if TWdgP.Widget_Type /= GtkTabChild then
            Put_Property ("Location");
            TIO.Put (LFile, "x=" & Img (TWdgP.Location.From_Left));
            TIO.Put_Line (LFile, ", y=" & Img (TWdgP.Location.From_Top));

            Put_Property ("Size");
            TIO.Put (LFile, "H=" & Img (TWdgP.Size.Horiz));
            TIO.Put_Line (LFile, ", V=" & Img (TWdgP.Size.Vert));

            Put_Property ("TabIndex");
            Put_Integer (TWdgP.TabIndex);

            Put_Property ("TabStop");
            Put_Triboolean (TWdgP.TabStop);

            Put_Property ("Has Focus");
            Put_Boolean (TWdgP.Has_Focus);

            Put_Property ("Zorder");
            Put_Integer (TWdgP.Zorder);

            Put_Property ("Enabled");
            Put_Boolean (TWdgP.Enabled);

            Put_Property ("Visible");
            Put_Boolean (TWdgP.Visible);

            Put_Property ("Text");
            Put_String_Access (TWdgP.Text, True);

            Put_Property ("TextAlign");
            TAIO.Put (LFile, TWdgP.TextAlign);
            TIO.New_Line (LFile);

            Put_Property ("AutoSize");
            Put_Boolean (TWdgP.AutoSize);

            Put_Property ("AutoSizeMode");
            ASMIO.Put (LFile, TWdgP.AutoSizeMode);
            TIO.New_Line (LFile);

            Put_Property ("Font");
            if TWdgP.Font_Name /= null then
               TIO.Put (LFile, TWdgP.Font_Name.all);
               TIO.Put (LFile, " " & Img (TWdgP.Font_Size));
               if TWdgP.Font_Weight /= null then
                  TIO.Put (LFile, " " & "+" (TWdgP.Font_Weight.all));
               end if;
               if TWdgP.Font_Underline then
                  TIO.Put (LFile, " underline");
               end if;
            end if;
            TIO.New_Line (LFile);

            Put_Property ("Margin");
            TIO.Put_Line (LFile, "Top " & Img (TWdgP.Margins (1))
                          & ", Bottom " & Img (TWdgP.Margins (3))
                          & ", Start " & Img (TWdgP.Margins (2))
                          & ", End " & Img (TWdgP.Margins (4)));

            Put_Property ("Padding");
            Put_Integer (TWdgP.Padding);

            Put_Property ("DisplayStyle");
            DSIO.Put (LFile, TWdgP.DStyle);
            TIO.New_Line (LFile);

            Put_Property ("MaxLength");
            Put_Integer (TWdgP.MaxLength);

            Put_Property ("AutoToolTip");
            Put_Boolean (TWdgP.AutoToolTip);

            Put_Property ("ToolTip");
            Put_String_Access (TWdgP.ToolTip, True);

            Put_Property ("UseVisualStyleBackColor");
            Put_Boolean (TWdgP.UseVisualStyleBackColor);

            Put_Property ("BGColor");
            Put_String_Access (TWdgP.BgColor);

            Put_Property ("FGColor");
            Put_String_Access (TWdgP.FgColor);

            Put_Property ("UlColor");
            Put_String_Access (TWdgP.UlColor);

            Put_Property ("Flow Direction");
            FDIO.Put (LFile, TWdgP.FlowDirection);
            TIO.New_Line (LFile);
         end if;

         Put_Property ("----");
         TIO.New_Line (LFile);

         case TWdgP.Widget_Type is
            when No_Widget =>
               null;

            when GtkMenuItem | GtkSubMenu => null;

            when GtkSeparatorMenuItem => null;

            when GtkMenuNormalItem | GtkMenuImageItem
               | GtkMenuRadioItem | GtkMenuCheckItem =>
               Put_Property ("ImageMenu Window");
               Put_Window_Pointer_Name (TWdgP.ImageMenuWin);

               Put_Property ("ImageMenu");
               Put_String_Access (TWdgP.ImageMenu);

            when GtkDataGridView | GtkTreeGridView
               | ExpandableColumn | DataGridViewTextBoxColumn
               | DataGridViewCheckBoxColumn
               =>
               Put_Property ("Default CellStyle");
               if TWdgP.DefaultCellStyle in DGVS.all'Range then
                  TIO.Put_Line (LFile,
                                DGVS (TWdgP.DefaultCellStyle).Name.all);
               else
                  TIO.New_Line (LFile);
               end if;

               Put_Property ("ReadOnly");
               Put_Boolean (TWdgP.ReadOnly);

               case TWdgP.Widget_Type is
                  when GtkDataGridView | GtkTreeGridView =>
                     case TWdgP.Widget_Type is
                        when GtkDataGridView =>
                           Put_Property ("Has Expander");
                           Put_Boolean (TWdgP.Has_Expander);
                        when others => null;
                     end case;

                     Put_Property ("ColumnHeadersVisible");
                     Put_Boolean (TWdgP.ColumnHeadersVisible);

                     Put_Property ("Alt. R. Def. CellStyle");
                     if TWdgP.AlternatingRowsDefaultCellStyle in DGVS.all'Range
                     then
                        TIO.Put_Line
                          (LFile,
                           DGVS
                             (TWdgP.AlternatingRowsDefaultCellStyle).Name.all);
                     else
                        TIO.New_Line (LFile);
                     end if;

                     Put_Property ("Col. H. Def. CellStyle");
                     if TWdgP.ColumnHeadersDefaultCellStyle in DGVS.all'Range
                     then
                        TIO.Put_Line
                          (LFile,
                           DGVS (TWdgP.ColumnHeadersDefaultCellStyle).Name.all);
                     else
                        TIO.New_Line (LFile);
                     end if;

                     Put_Property ("Row H. Def. CellStyle");
                     if TWdgP.RowHeadersDefaultCellStyle in DGVS.all'Range
                     then
                        TIO.Put_Line
                          (LFile,
                           DGVS (TWdgP.RowHeadersDefaultCellStyle).Name.all);
                     else
                        TIO.New_Line (LFile);
                     end if;

                     Put_Property ("Col. H. HeightSizeMode");
                     CHHSMIO.Put (LFile, TWdgP.ColumnHeadersHeightSizeMode);
                     TIO.New_Line (LFile);

                     Put_Property ("AllowUserToAddRows");
                     Put_Boolean (TWdgP.AllowUserToAddRows);

                     Put_Property ("AllowUserToDeleteRows");
                     Put_Boolean (TWdgP.AllowUserToDeleteRows);

                     Put_Property ("AllowUserToDeleteRows");
                     Put_Boolean (TWdgP.AllowUserToDeleteRows);

                     Put_Property ("EnableHeadersVisualStyles");
                     Put_Boolean (TWdgP.EnableHeadersVisualStyles);

                     Put_Property ("RowMultiSelect");
                     Put_Boolean (TWdgP.RowMultiSelect);

                     Put_Property ("RowHeadersVisible");
                     Put_Boolean (TWdgP.RowHeadersVisible);

                     Put_Property ("EditModeProgramatically");
                     Put_Boolean (TWdgP.EditModeProgramatically);

                     Put_Property ("ImageList");
                     Put_Widget_Pointer_Name (TWdgP.ImageList);

                     Put_Property ("RowHeadersWidthSizeMode");
                     RHWSMIO.Put (LFile, TWdgP.RowHeadersWidthSizeMode);
                     TIO.New_Line (LFile);

                     Put_Property ("RowHeadersBorderStyle");
                     RHBSIO.Put (LFile, TWdgP.RowHeadersBorderStyle);
                     TIO.New_Line (LFile);

                     Put_Property ("ScrollBars");
                     SBIO.Put (LFile, TWdgP.ScrollBars);
                     TIO.New_Line (LFile);

                     Put_Property ("Model");
                     Put_Window_Pointer_Name (TWdgP.Model);

                  when ExpandableColumn | DataGridViewTextBoxColumn
                     | DataGridViewCheckBoxColumn
                     =>
                     Put_Property ("Fixed Width");
                     Put_Integer (TWdgP.Fixed_Width);

                     Put_Property ("Min Width");
                     Put_Integer (TWdgP.Min_Width);

                     Put_Property ("Max Width");
                     Put_Integer (TWdgP.Max_Width);

                     Put_Property ("UserAddedColumn");
                     Put_Boolean (TWdgP.UserAddedColumn);

                     Put_Property ("Level");
                     Put_Integer (TWdgP.Level);

                     Put_Property ("PaddingX");
                     Put_Integer (TWdgP.PaddingX);

                     Put_Property ("PaddingY");
                     Put_Integer (TWdgP.PaddingY);

                     Put_Property ("Resizable");
                     Put_Boolean (TWdgP.Resizable);

                     Put_Property ("SortMode");
                     SMIO.Put (LFile, TWdgP.SortMode);
                     TIO.New_Line (LFile);

                     Put_Property ("AutoSizeColumnMode");
                     ASCMIO.Put (LFile, TWdgP.AutoSizeColumnMode);
                     TIO.New_Line (LFile);

                     Put_Property ("DefaultNodeImage");
                     Put_Widget_Pointer_Name (TWdgP.DefaultNodeImage);

                     Put_Property ("Frozen");
                     Put_Boolean (TWdgP.Frozen);

                     case TWdgP.Widget_Type is
                        when DataGridViewCheckBoxColumn =>
                           Put_Property ("Active Column");
                           Put_Integer
                             (TWdgP.CheckBox_Col_Properties.Active_Column);

                           Put_Property ("Activatable Column");
                           Put_Integer
                             (TWdgP.CheckBox_Col_Properties.Activatable_Column);
                        when others => null;
                     end case;

                  when others => null;
               end case;

            when GtkNoteBook =>
               Put_Property ("Scrollable");
               Put_Boolean (TWdgP.Scrollable);

               Put_Property ("Enable Popup");
               Put_Boolean (TWdgP.Enable_Popups);

               Put_Property ("Show Tabs");
               Put_Boolean (TWdgP.Show_Tabs);

               Put_Property ("Show Borders");
               Put_Boolean (TWdgP.Show_Border);

               Put_Property ("Pack Start");
               Put_Boolean (TWdgP.Pack_Start);

               Put_Property ("Tab CloseButton Visible");
               Put_Boolean (TWdgP.CloseButtonOnTabsInactiveVisible);

               Put_Property ("Selected Index");
               Put_Integer (TWdgP.SelectedIndex);

            when GtkTabChild =>
               null;

            when GtkTabPage =>
               Put_Property ("Label");
               Put_Widget_Pointer_Name (TWdgP.The_Label);

               Put_Property ("Button");
               Put_Widget_Pointer_Name (TWdgP.The_Button);

            when GtkEntry | GtkComboTextBox | GtkCalendar =>
               Put_Property ("Buffer");
               Put_Window_Pointer_Name (TWdgP.Buffer);

               Put_Property ("Text Buffer");
               Put_String_Access (TWdgP.Text_Buffer);

               case TWdgP.Widget_Type is
                  when GtkEntry | GtkComboTextBox =>
                     Put_Property ("Editable");
                     Put_Boolean (TWdgP.Editable);

                     Put_Property ("Has Frame");
                     Put_Boolean (TWdgP.Has_Frame);

                     Put_Property ("PasswordChar");
                     Put_String_Access (TWdgP.PasswordChar);

                     if TWdgP.Widget_Type = GtkComboTextBox then
                        Put_Property ("Sorted");
                        Put_Boolean (TWdgP.Editable);
                     end if;

                  when GtkCalendar =>
                     Put_Property ("StartDate");
                     TIO.Put_Line (Image (TWdgP.Start_Date, ISO_Date));

                     Put_Property ("MinDate");
                     TIO.Put_Line (Image (TWdgP.MinDate, ISO_Date));

                     Put_Property ("MaxDate");
                     TIO.Put_Line (Image (TWdgP.MaxDate, ISO_Date));

                     Put_Property ("Format Date");
                     Put_String_Access (TWdgP.Format_Date);

                     Put_Property ("ShowUpDown");
                     Put_Boolean (TWdgP.ShowUpDown);

                  when others => null;
               end case;

            when GtkSpinButton =>
               Put_Property ("Start");
               Put_Integer (TWdgP.StartValue);

               Put_Property ("MaxValue");
               Put_Integer (TWdgP.MaxValue);

               Put_Property ("MinValue");
               Put_Integer (TWdgP.MinValue);

               Put_Property ("Step");
               Put_Integer (TWdgP.Step);

            when GtkFileChooserButton
               | PrintDocument | PrintDialog | PageSetupDialog
               | FolderBrowserDialog | GtkToolTip | GtkColorButton
               | GtkStatusBar | GtkToolBar | GtkMenuBar | BackgroundWorker
               | BindingNavigator
               =>

               Put_Property ("TrayLocation");
               TIO.Put (LFile, "x=" & Img (TWdgP.TrayLocation.From_Left));
               TIO.Put (LFile, ", y=" & Img (TWdgP.TrayLocation.From_Top));
               TIO.New_Line (LFile);

               case TWdgP.Widget_Type is
                  when GtkFileChooserButton =>
                     Put_Property ("OpenFileDialog");
                     Put_String_Access (TWdgP.OpenFileDialog);

                     Put_Property ("OpenFileFilter");
                     Put_String_Access (TWdgP.OpenFileFilter);

                     Put_Property ("OpenFileTitle");
                     Put_String_Access (TWdgP.OpenFileTitle);

                  when GtkColorButton =>
                     Put_Property ("AnyColor");
                     Put_Boolean (TWdgP.AnyColor);

                     Put_Property ("FullOpen");
                     Put_Boolean (TWdgP.FullOpen);

                     Put_Property ("Associated Button");
                     Put_Widget_Pointer_Name (TWdgP.Associated_Button);

                  when GtkToolBar | GtkMenuBar | BindingNavigator =>
                     Put_Property ("Image Scaling Size");
                     TIO.Put (LFile, "H="
                              & Img (TWdgP.ImageScalingSize.One));
                     TIO.Put_Line (LFile, ", V="
                              & Img (TWdgP.ImageScalingSize.Two));

                     case TWdgP.Widget_Type is
                        when GtkToolBar | BindingNavigator =>
                           Put_Property ("Horizontal");
                           Put_Boolean (TWdgP.TB_Horiz);

                           Put_Property ("Show Arrows");
                           Put_Boolean (TWdgP.Show_Arrows);

                           Put_Property ("Grip Visible");
                           Put_Boolean (TWdgP.Grip_Visible);

                           case TWdgP.Widget_Type is
                              when BindingNavigator =>
                                 Put_Property ("AddNewItem");
                                 Put_Widget_Pointer_Name (TWdgP.AddNewItem);

                                 Put_Property ("CountItem");
                                 Put_Widget_Pointer_Name (TWdgP.CountItem);

                                 Put_Property ("DeleteItem");
                                 Put_Widget_Pointer_Name (TWdgP.DeleteItem);

                                 Put_Property ("MoveFirstItem");
                                 Put_Widget_Pointer_Name (TWdgP.MoveFirstItem);

                                 Put_Property ("MoveLastItem");
                                 Put_Widget_Pointer_Name (TWdgP.MoveLastItem);

                                 Put_Property ("MoveNextItem");
                                 Put_Widget_Pointer_Name (TWdgP.MoveNextItem);

                                 Put_Property ("MovePreviousItem");
                                 Put_Widget_Pointer_Name
                                   (TWdgP.MovePreviousItem);

                                 Put_Property ("PositionItem");
                                 Put_Widget_Pointer_Name (TWdgP.PositionItem);
                              when others => null;
                           end case;
                        when others => null;
                     end case;

                  when PageSetupDialog =>
                     Put_Property ("EnableMetric");
                     Put_Boolean (TWdgP.EnableMetric);

                     Put_Property ("MinMargins");
                     TIO.Put_Line (LFile,
                              "Top " & Img (TWdgP.MinMargins (1))
                              & ", Bottom " & Img (TWdgP.MinMargins (3))
                              & ", Start " & Img (TWdgP.MinMargins (2))
                              & ", End " & Img (TWdgP.MinMargins (4)));

               when BackgroundWorker =>
                     Put_Property ("Report Progress");
                     Put_Boolean (TWdgP.WorkerReportsProgress);

                     Put_Property ("Support Cancellation");
                     Put_Boolean (TWdgP.WorkerSupportsCancellation);

                  when others => null;
               end case;

            when GtkListBox =>
               Put_Property ("MultiSelect");
               Put_Boolean (TWdgP.MultiSelect);

               Put_Property ("ListStore    ");
               Put_Window_Pointer_Name (TWdgP.ListStore);

            when GtkImage =>
               Put_Property ("ImagePath");
               Put_String_Access (TWdgP.Image);

            when GtkButton | GtkRadioButton
               | GtkCheckButton | GtkToggleButton
               | GtkLabel | ToolStripStatusLabel
               =>
               Put_Property ("Underline");
               Put_Boolean (TWdgP.Underline);

               case TWdgP.Widget_Type is
                  when GtkLabel | ToolStripStatusLabel =>
                     Put_Property ("BorderStyle");
                     BSIO.Put (LFile, TWdgP.BorderStyle);
                     TIO.New_Line (LFile);

                  when GtkButton | GtkRadioButton
                     | GtkCheckButton | GtkToggleButton
                     =>
                     Put_Property ("Active");
                     Put_Boolean (TWdgP.Active);

                     Put_Property ("ImagePath");
                     Put_String_Access (TWdgP.ImagePath);

                     Put_Property ("ImageAlign");
                     IPIO.Put (LFile, TWdgP.ImageAlign);
                     TIO.New_Line (LFile);

                     Put_Property ("Windows Image");
                     Put_Window_Pointer_Name (TWdgP.Win_Image);

                     case TWdgP.Widget_Type is
                        when GtkButton =>
                           Put_Property ("Dialog Result");
                           DRIO.Put (LFile, TWdgP.Dialog_Result);
                           TIO.New_Line (LFile);

                           Put_Property ("Associated ColorButton");
                           Put_Widget_Pointer_Name
                             (TWdgP.Associated_ColorButton);

                        when GtkCheckButton =>
                           Put_Property ("CheckAlign");
                           Put_String_Access (TWdgP.CheckAlign);

                        when others => null;
                     end case;
                  when others => null;
               end case;

            when Chart =>
               Put_Property ("Anchor");
               Put_String_Access (TWdgP.Anchor);

            when GtkFrame =>
               null;

            when GtkBox =>
               null;

            when GtkSeparatorToolItem =>
               null;

         end case;

         Dump_Signals;
         Dump_Children;

      end Dump_Widget;
   begin
      Debug (0, "Generating Dump");
      TIO.Create (File => LFile,
                  Mode => TIO.Out_File,
                  Name => Path & "/" & File_Name & ".dump");
      Dump_DGVS;
      TWin := Win_List;
      while TWin /= null loop
         Dump_Window (TWin, 0);
         TWin := TWin.Next;
      end loop;
      TIO.Close (LFile);
      Debug (0, "End of generating Dump");
   end Dump;

   function Parse_Resource_File (TWin           : Window_Pointer;
                                 Resx_Path      : String;
                                 Resx_File_Name : String) return Integer is
      Result : Integer;
      function Parse_Window_Properties
        (TWin           : Window_Pointer;
         Resx_Path      : String;
         Resx_File_Name : String) return Integer;
      function Parse_Window_Properties
        (TWin           : Window_Pointer;
         Resx_Path      : String;
         Resx_File_Name : String) return Integer
      is
         Idx0 : Integer;
         Idx1 : Integer;
         P0   : Pair;
         Win_Prop : Window_Property_Enum;

      begin
         --  loop to get window properties
         TIO.Open (File => RFile,
                   Mode => TIO.In_File,
                   Name => Resx_Path & "/" & Resx_File_Name & ".resx");

         NLin := 0;
         Debug (NLin, "");
         Debug (NLin, "Parsing Resources: Window: " & Resx_File_Name);
         while not TIO.End_Of_File (RFile) loop
            TIO.Get_Line (RFile, Line, Len);
            NLin := NLin + 1;

            Idx0 := Index (Line (1 .. Len), Test2);
            if Idx0 in 1 .. Len then
               Idx1 := Index (Line (Idx0 + Test2'Length .. Len), """");
               if Idx1 in Idx0 + Test2'Length .. Len then
                  Win_Prop := Symbol_Tables.Get_Property
                    (Line (Idx0 + Test2'Length .. Idx1 - 1));

                  case Win_Prop is
                     when Str_Accept_Button =>
                        null;
                     when Str_Cancel_Button =>
                        null;
                     when Str_ClientSize =>
                        P0 := Get_Pair (Get_String (RFile));
                        TWin.Client_Size.Horiz := P0.One;
                        TWin.Client_Size.Vert  := P0.Two;
                        Debug (NLin, "Set Window Property ClientSize H=" &
                                 Img (P0.One) & ", V=" & Img (P0.Two));

                     when Str_AutoScaleDimensions =>
                        P0 := Get_Pair (Get_String (RFile));
                        TWin.AutoScaleDim.Horiz := P0.One;
                        TWin.AutoScaleDim.Vert  := P0.Two;
                        Debug (NLin, "Set Window Property AutoScaleDim H=" &
                                 Img (P0.One) & ", V=" & Img (P0.Two));

                     when Str_Icon =>
                        TWin.Icon := new String'("");
                        Debug (NLin, Resx_File_Name & ".resx"
                               & ": ignored window property "
                               & "Icon");

                     when Str_StartPosition =>
                        declare
                           Temp : constant String := Get_String (RFile);
                        begin
                           if Temp = "CenterParent" then
                              TWin.Start_Position := CenterParent;
                              Debug (NLin, "Set Window Property CenterParent");
                           end if;
                        end;

                     when Str_Margin =>
                        TWin.Margins := Get_Margin_Array (RFile);
                        Debug (NLin, "Set Window Property Margins " &
                                 Img (TWin.Margins (1)) & ", " &
                                 Img (TWin.Margins (2)) & ", " &
                                 Img (TWin.Margins (3)) & ", " &
                                 Img (TWin.Margins (4)));

                     when Str_Font =>
                        Get_Font (RFile,
                                  TWin.Font_Name,
                                  TWin.Font_Size,
                                  TWin.Font_Weight,
                                  TWin.Font_Underline);
                        if TWin.Font_Weight /= null then
                           Have.Font_Weight := Have.Font_Weight + 1;
                        end if;
                        if TWin.Font_Weight = null then
                           Debug (NLin, "Set Window Property "
                                  & TWin.Name.all & ".Font "
                                  & TWin.Font_Name.all & " "
                                  & Img (TWin.Font_Size)
                                  & (if TWin.Font_Underline then
                                     " underline" else ""));
                        else
                           Debug (NLin, "Set Window Property "
                                  & TWin.Font_Name.all & " "
                                  & TWin.Name.all & ".Font "
                                  & " " & Img (TWin.Font_Size)
                                  & " " & TWin.Font_Weight.all
                                  & (if TWin.Font_Underline then
                                     " underline" else ""));
                        end if;

                     when Str_Type =>
                        null; --  should be Form

                     when Str_ToolTip =>
                        TWin.ToolTip := new String'(Get_String (RFile));
                        Debug (NLin, "Set Window Property Tooltip " &
                                 TWin.ToolTip.all);

                     when Str_TrayHeight =>
                        TWin.TrayHeight := Get_Integer (Get_String (RFile));
                        Debug (NLin, "Set Window Property TrayHeight " &
                                 TWin.ToolTip.all);

                     when Str_Text =>
                        TWin.Title := new String'(Get_String (RFile));
                        Debug (NLin, "Set Window Property Title " &
                                 TWin.Title.all);

                     when Str_Name =>
                        if TWin.Name = null then
                           TWin.Name := new String'(+Get_String (RFile));
                           Debug (NLin, "Set Window Property Name "
                                  & TWin.Name.all);
                        end if;

                     when Str_RightToLeft =>
                        Debug (NLin, Resx_File_Name & ".resx"
                               & ": ignored window property "
                               & "RightToLeft");

                     when No_Property =>
                        TIO.Put_Line (Resx_File_Name & ".resx"
                                      & ": Line" & NLin'Image
                                      & ": unknown window property: "
                                      & Line (Idx0 + Test2'Length .. Len));
                        TIO.Close (RFile);
                        return -1;
                  end case;
               else
                  TIO.Put_Line (Resx_File_Name & ".resx"
                                & ": Line" & NLin'Image &
                                  ": cannot parse window property");
                  TIO.Close (RFile);
                  return -1;
               end if;
            end if;
         end loop;
         TIO.Close (RFile);
         return 0;
      exception
         when Constraint_Error =>
            TIO.Close (RFile);
            TIO.Put_Line ("Constraint Error");
            TIO.Put_Line (Resx_File_Name & ".resx"
                          & ": Line" & NLin'Image
                          & " " & Line (1 .. Len));
            return -1;
      end Parse_Window_Properties;

      function Parse_Widget_Properties
        (TWin           : Window_Pointer;
         Resx_Path      : String;
         Resx_File_Name : String) return Integer;
      function Parse_Widget_Properties
        (TWin           : Window_Pointer;
         Resx_Path      : String;
         Resx_File_Name : String) return Integer
      is
         Idx0  : Integer;
         Idx1  : Integer;
         Idx2  : Integer;
         P0    : Pair;
         Disp0 : Integer;
      begin
         TIO.Open (File => RFile,
                   Mode => TIO.In_File,
                   Name => Resx_Path & "/" & Resx_File_Name & ".resx");
         NLin := 0;
         --  second loop to skip the header comment
         while not TIO.End_Of_File (RFile) loop
            TIO.Get_Line (RFile, Line, Len);
            NLin := NLin + 1;
            Idx0 := Index (Line (1 .. Len), "-->");
            exit when Idx0 in 1 .. Len;
         end loop;

         --  continue reading
         Debug (NLin, "");
         Debug (NLin, "Parsing Resources: Widgets");
         while not TIO.End_Of_File (RFile) loop
            TIO.Get_Line (RFile, Line, Len);
            NLin := NLin + 1;
            Idx0 := Index (Line (1 .. Len), "&gt;&gt;");
            if Idx0 in 1 .. Len then
               Line (Idx0 .. Len - 8) := Line (Idx0 + 8 .. Len);
               Len := Len - 8;
            end if;
            Idx0  := Index (Line (1 .. Len), Test5);
            Disp0 := Test5'Length;
            if not (Idx0 in 1 .. Len) then
               Idx0  := Index (Line (1 .. Len), Test1);
               Disp0 := Test1'Length;
            end if;
            if Idx0 not in 1 .. Len then
               goto Continue_Loop;
            end if;

            Idx1 := Index (Line (Idx0 + Disp0 .. Len), """");
            Idx2 := Index (Line (Idx0 .. Idx1), "$this.");
            if not (Idx1 in Idx0 + Test2'Length .. Len and then
                      not (Idx2 in Idx0 .. Idx1))
            then
               goto Continue_Loop;
            end if;

            declare
               Property : constant String := Line (Idx0 + Disp0 .. Idx1 - 1);
            begin
               Idx0 := Index (Property, ".");
               if Idx0 not in Property'Range then
                  goto Continue_Loop;
               end if;

               declare
                  PName : constant String :=
                    +Property (Property'First .. Idx0 - 1);
                  PAttrib : constant String :=
                    Property (Idx0 + 1 .. Property'Last);
                  WT      : Widget_Pointer;
                  Attr    : Widget_Attribute_Enum;
               begin
                  if PName = "" then
                     TIO.Put_Line (Resx_File_Name & ".resx"
                                   & ": Line" & NLin'Image
                                   &  ": cannot find widget name");
                     TIO.Close (RFile);
                     return -1;
                  end if;

                  if PAttrib = "" then
                     TIO.Put_Line (Resx_File_Name & ".resx"
                                   & ": Line" & NLin'Image
                                   &  ": cannot find widget property"
                                   & " for " & PName);
                     TIO.Close (RFile);
                     return -1;
                  end if;

                  WT := Find_Widget (TWin.Widget_List, PName);
                  if WT = null then
                     if Contains (Line (1 .. Len), "BindingNavigator")
                     then
                        Debug (NLin, "Resource: Ignored Widget Property "
                               & Line (1 .. Len));
                        goto Continue_Loop;
                     end if;
                     TIO.Put_Line (Resx_File_Name & ".resx"
                                   & ": Line" & NLin'Image
                                   & ": widget " & PName
                                   & " not in Designer");
                     TIO.Close (RFile);
                     return -1;
                  end if;

                  if Starts_With (PAttrib, "Items") then
                     if WT.Widget_Type = GtkComboTextBox then
                        if WT.Text_Buffer = null then
                           WT.Text_Buffer := new String'(Get_String (RFile));
                        else
                           declare
                              Temp : String_Access;
                           begin
                              Temp := new String'(WT.Text_Buffer.all
                                                  & ASCII.CR
                                                  & Get_String (RFile));
                              Free (WT.Text_Buffer);
                              WT.Text_Buffer := Temp;
                           end;
                        end if;
                     else
                        Debug (NLin, "Resource: Ignored Widget Property "
                               & PName & " " & PAttrib);
                     end if;
                     goto Continue_Loop;

                  elsif Contains (Property, "BindingNavigator") then
                     Debug (NLin, "Resource: Ignored Widget Property "
                            & PName & " " & PAttrib);
                     goto Continue_Loop;
                  end if;

                  Attr := Symbol_Tables.Get_Attribute (PAttrib);

                  case Attr is
                     when Attr_Anchor =>
                        WT.Anchor := new String'(Get_String (RFile));
                        Debug (NLin, "Set Widget Property "
                               & PName & ".Anchor "
                               & WT.Anchor.all);

                     when Attr_ZOrder =>
                        WT.Zorder := Get_Integer (Get_String (RFile));
                        Debug (NLin, "Set Widget Property "
                               & PName & ".ZOrder "
                               & Img (WT.Zorder));

                     when Attr_Margin | Attr_Padding =>
                        WT.Margins := Get_Margin_Array (RFile);
                        Debug (NLin, "Set Widget Property "
                               & PName
                               & ". Margins "
                               & Img (WT.Margins (1)) & ", " &
                                 Img (WT.Margins (2)) & ", " &
                                 Img (WT.Margins (3)) & ", " &
                                 Img (WT.Margins (4)));

                     when Attr_TabIndex =>
                        WT.TabIndex := Get_Integer (Get_String (RFile));
                        Debug (NLin, "Set Widget Property "
                               & PName & ".TabIndex "
                               & Img (WT.TabIndex));

                     when Attr_Text =>
                        if WT.Widget_Type = GtkTabChild then
                           WT.Text := new String'(Trim (Get_String (RFile),
                                                  Ada.Strings.Both));
                        else
                           WT.Text := new String'(Trim (Get_String (RFile),
                                                  Ada.Strings.Both));
                        end if;
                        Debug (NLin, "Set Widget Property "
                               & PName & ".Text "
                               & WT.Text.all);

                     when Attr_MaxLength =>
                        WT.MaxLength := Get_Integer (Get_String (RFile));
                        Debug (NLin, "Set Widget Property "
                               & PName & ".MaxLength "
                               & Img (WT.MaxLength));

                     when Attr_ToolTip | Attr_ToolTipText =>
                        WT.ToolTip := new String'(Get_String (RFile));
                        Debug (NLin, "Set Widget Property "
                               & PName & ".ToolTip "
                               & WT.ToolTip.all);

                     when Attr_AutoToolTip =>
                        WT.AutoToolTip := Get_Boolean (RFile);
                        Debug (NLin, "Set Widget Property "
                               & PName & ".AutoToolTip "
                               & WT.AutoToolTip'Image);


                     when Attr_AutoSize =>
                        WT.AutoSize := Get_Boolean (RFile);
                        Debug (NLin, "Set Widget Property "
                               & PName & ".Autosize "
                               & WT.AutoSize'Image);

                     when Attr_Enabled =>
                        WT.Enabled := Get_Boolean (RFile);
                        Debug (NLin, "Set Widget Property "
                               & PName & ".Enabled "
                               & WT.Enabled'Image);

                     when Attr_UserAddedColumn =>
                        WT.UserAddedColumn := Get_Boolean (RFile);
                        Debug (NLin, "Set Widget Property "
                               & PName & ".UserAddedColumn "
                               & WT.UserAddedColumn'Image);

                     when Attr_Visible =>
                        WT.Visible := Get_Boolean (RFile);
                        Debug (NLin, "Set Widget Property "
                               & PName & ".Visible "
                               & WT.Visible'Image);

                     when Attr_TextAlign | Attr_BoxTextAlign =>
                        TAIO.Get (Get_String (RFile), WT.TextAlign, Idx2);
                        declare
                           Temp : String (1 .. 12);
                        begin
                           TAIO.Put (Temp, WT.TextAlign);
                           Debug (NLin, "Set Widget Property "
                                  & PName & ".TextAlign "
                                  & Temp);
                        end;

                     when Attr_CheckAlign =>
                        WT.CheckAlign := new String'(Get_String (RFile));
                        Debug (NLin, "Set Widget Property "
                               & PName & ".CheckAlign "
                               & WT.CheckAlign.all);

                     when Attr_Name =>
                        if PName /= +Get_String (RFile) then
                           TIO.Put_Line ("Line" & NLin'Image & ": " &
                                           "name mistmatch");
                           TIO.Close (RFile);
                           return -1;
                        end if;
                        Debug (NLin, "Set Widget Property "
                               & PName & ".Name "
                               & WT.Name.all);

                     when Attr_Size =>
                        P0 := Get_Pair (Get_String (RFile));
                        WT.Size.Horiz := P0.One;
                        WT.Size.Vert  := P0.Two;
                        Debug (NLin, "Set Widget Property "
                               & PName & ".Size H=" &
                                 Img (P0.One) & ", V=" & Img (P0.Two));

                     when Attr_Location =>
                        P0 := Get_Pair (Get_String (RFile));
                        WT.Location.From_Top := P0.Two;
                        WT.Location.From_Left := P0.One;
                        Debug (NLin, "Set Widget Property "
                               & PName
                               &  ".Location H="
                               & Img (P0.One) & ", V=" & Img (P0.Two));

                     when Attr_Type =>
                        if WT.Windows_Type = null then
                           WT.Windows_Type :=
                             new String'(Get_Widget_Name (RFile));
                           Debug (NLin, "Set Widget Property "
                                  & PName & ".Type "
                                  & WT.Windows_Type.all);
                        else
                           declare
                              WType : constant String :=
                                Get_Widget_Name (RFile);
                           begin
                              if WType /= WT.Windows_Type.all then
                                 TIO.Put_Line ("Line" & NLin'Image & ": " &
                                                 "windows type mistmatch");
                                 TIO.Close (RFile);
                                 return -1;
                              end if;
                           end;
                        end if;

                     when Attr_Parent =>
                        WT.Parent_Name :=
                          new String'(+Get_String (RFile));
                        if WT.Parent_Name.all = "_This" then
                           WT.Parent_Name.all := "$this";
                        end if;
                        Debug (NLin, "Set Widget Property "
                               & PName
                               & ".Parent Name "
                               & WT.Parent_Name.all);

                     when Attr_Font =>
                        Get_Font (RFile,
                                  WT.Font_Name,
                                  WT.Font_Size,
                                  WT.Font_Weight,
                                  WT.Font_Underline);
                        if WT.Font_Weight /= null then
                           Have.Font_Weight := Have.Font_Weight + 1;
                        end if;
                        if WT.Font_Weight /= null then
                           Debug (NLin, "Set Widget Property "
                                  & PName & ".Font "
                                  & WT.Font_Name.all
                                  & " " & Img (WT.Font_Size)
                                  & " " & WT.Font_Weight.all
                                  & (if WT.Font_Underline then
                                   " underline" else ""));
                        else
                           Debug (NLin, "Set Widget Property "
                                  & PName & ".Font "
                                  & WT.Font_Name.all
                                  & " " & Img (WT.Font_Size)
                                  & (if WT.Font_Underline then
                                   " underline" else ""));
                        end if;

                     when Attr_PasswordChar =>
                        WT.PasswordChar :=
                          new String'(Get_String (RFile));
                        Debug (NLin, "Set Widget Property "
                               & PName
                               & ".PasswordChar" & WT.PasswordChar.all);

                     when Attr_OpenFile =>
                        WT.OpenFileDialog :=
                          new String'(Get_String (RFile));
                        Debug (NLin, "Set Widget Property "
                               & PName
                               & ".OpenFile " & WT.OpenFileDialog.all);

                     when Attr_TrayLocation =>
                        P0 := Get_Pair (Get_String (RFile));
                        WT.TrayLocation.From_Top  := P0.Two;
                        WT.TrayLocation.From_Left := P0.One;
                        Debug (NLin, "Set Widget Property "
                               & PName
                               & ".TrayLocation H="
                               & Img (P0.One)
                               & ", V=" & Img (P0.Two));

                     when Attr_Filter =>
                        WT.OpenFileFilter :=
                          new String'(Get_String (RFile));
                        Debug (NLin, "Set Widget Property "
                               & PName
                               & ".OpenFileFilter "
                               & WT.OpenFileFilter.all);

                     when Attr_Title =>
                        WT.OpenFileTitle :=
                          new String'(Get_String (RFile));
                        Debug (NLin, "Set Widget Property "
                               & PName
                               & ".OpenFileTitle "
                               & WT.OpenFileTitle.all);

                     when Attr_ImageAlign =>
                        WT.ImageAlign := Convert (Get_String (RFile));
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".ImageAlign "
                               & WT.ImageAlign'Image);

                     when Attr_FixedWidth =>
                        WT.Fixed_Width :=
                          Get_Integer (Get_String (RFile));
                        Debug (NLin, "Set Widget Property "
                               & PName & ".Fixed_Width "
                               & Img (WT.Fixed_Width));

                     when Attr_MinimumWidth =>
                        WT.Min_Width :=
                          Get_Integer (Get_String (RFile));
                        Debug (NLin, "Set Widget Property "
                               & PName & ".Min_Width "
                               & Img (WT.Min_Width));

                     when Attr_MaximumWidth =>
                        WT.Max_Width :=
                          Get_Integer (Get_String (RFile));
                        Debug (NLin, "Set Widget Property "
                               & PName & ".Max_Width "
                               & Img (WT.Max_Width));

                     when Attr_HeaderText =>
                        WT.Text := new String'(Trim (Get_String (RFile),
                                               Ada.Strings.Both));
                        Debug (NLin, "Set Widget Property "
                               & PName & ".Text "
                               & WT.Text.all);

                     when Attr_ItemSize =>
                        P0 := Get_Pair (Get_String (RFile));
                        WT.Size.Horiz := P0.One;
                        WT.Size.Vert  := P0.Two;
                        Debug (NLin, "Set Widget Property "
                               & PName & ".Size H="
                               & Img (P0.One) & ", V=" & Img (P0.Two));

                     when Attr_Ignored | Attr_Image =>
                        Debug (NLin, Resx_File_Name & ".resx"
                               & ": ignored widget property "
                               & PName & " " & PAttrib);

                     when others =>
                        TIO.Put_Line (Resx_File_Name & ".resx"
                                      & ": Line" & NLin'Image &
                                        ": unknown widget property: " &
                                        Property);
                        TIO.Close (RFile);
                        return -1;
                  end case;
               exception
                  when Constraint_Error =>
                     TIO.Close (RFile);
                     TIO.Put_Line ("Constraint Error");
                     TIO.Put_Line (Resx_File_Name & ".resx"
                                   & ": Line:" & NLin'Image
                                   & ": " & Property);
                     return -1;
               end;
            end;
            << Continue_Loop >>
         end loop;
         return 0;
      exception
         when Constraint_Error =>
            TIO.Close (RFile);
            TIO.Put_Line ("Constraint Error");
            TIO.Put_Line (Resx_File_Name & ".resx"
                          & ": Line" & NLin'Image
                          & " " & Line (1 .. Len));
            return -1;
      end Parse_Widget_Properties;

   begin
      Result := Parse_Window_Properties (TWin, Resx_Path, Resx_File_Name);
      if Result /= 0 then
         return Result;
      end if;

      Result := Parse_Widget_Properties (TWin, Resx_Path, Resx_File_Name);

      Debug (0, "End of Parsing Resources");
      return Result;
   end Parse_Resource_File;

   function Parse2_Designer_File (TWin           : Window_Pointer;
                                  Resx_Path      : String;
                                  Resx_File_Name : String;
                                  Icon_Path      : String) return Integer is
      use GNAT.Calendar.Time_IO;
      package IIO renames Ada.Integer_Text_IO;

      Idx0  : Integer;
      Idx1  : Integer;
      Idx2  : Integer;
      Idx3  : Integer;
      WT    : Widget_Pointer;
      Attr  : Widget_Attribute_Enum;
      Ret   : Integer;
      Num  : Integer;
      Last : Integer;

      procedure Get_Line;
      procedure Get_Line is
      begin
         TIO.Get_Line (DFile, Line, Len);
         NLin := NLin + 1;
         if Line (Len) = ASCII.CR then
            Len := Len - 1;
         end if;
      end Get_Line;

      procedure Add_Items (Parent : Widget_Pointer);
      procedure Add_Items (Parent : Widget_Pointer) is
         Child : Widget_Pointer;
      begin
         Idx0 := Index (Line (1 .. Len), "{");
         if Idx0 not in 1 .. Len then
            raise TIO.Data_Error;
         end if;
         loop
            Idx0 := Index (Line (Idx0 + 1 .. Len), "Me.");
            if Idx0 not in 1 .. Len then
               raise TIO.Data_Error;
            end if;
            Idx0 := Idx0 + 3;
            Idx1 := Index (Line (Idx0 .. Len), ",");
            --  exit when Idx1 not in Idx0 .. Len;
            if Idx1 not in Idx0 .. Len then
               Idx1 := Index (Line (Idx0 .. Len), "}");
            end if;
            Child := Find_Widget (TWin.Widget_List, +Line (Idx0 .. Idx1 - 1));
            if Child = null then
               TIO.Put_Line (Img (NLin) & ": "
                             & Line (Idx0 .. Len) & ": widget not found");
               raise Constraint_Error;
            end if;
            if Child.Parent_Name /= null then
               if Child.Parent_Name.all /= Parent.Name.all then
                  TIO.Put_Line ("Designer 2: " & Img (NLin) & ": "
                                & "Warning: " & Child.Name.all & ": "
                                & "Mismatch with Resource File: "
                                & Child.Parent_Name.all & " / "
                                & Parent.Name.all);
               end if;
               Free (Child.Parent_Name);
            end if;
            Child.Parent_Name := new String'(Parent.Name.all);
            Parent.Num_Children := Parent.Num_Children + 1;
            Child.Child_Num := Parent.Num_Children;
            if Parent.Widget_Type = GtkDataGridView
              and then Child.Widget_Type = ExpandableColumn
            then
               Parent.Has_Expander := True;
            end if;
            Debug (NLin, "Set Widget Property "
                   & Child.Name.all
                   & ".Parent_Name "
                   & Parent.Name.all
                   & ", child number=" & Img (Child.Child_Num));
            Idx0 := Idx1 + 1; -- skip semicolon
            exit when Line (Idx0) = ')';
         end loop;
      end Add_Items;

      function Process_Widget return Integer;
      function Process_Widget return Integer is
         function Get_Date (Idx : Integer) return Ada.Calendar.Time;
         function Get_Date (Idx : Integer) return Ada.Calendar.Time is
            Temp0 : Integer;
            Temp1 : Integer;
            Y, M, D : Integer;
            TTime : Ada.Calendar.Time;
         begin
            Temp0 := Index (Line (Idx .. Len), "New Date(");
            if Temp0 in Idx .. Len - 12 then
               Temp0 := Temp0 + 9;
               Temp1 := Index (Line (Temp0 .. Len - 12), ",");
               if Temp1 in Temp0 + 1 .. Len - 12 then
                  Y := Get_Integer (Line (Temp0 .. Temp1 - 1));
                  if Y < Ada.Calendar.Year_Number'First then
                     Y := Ada.Calendar.Year_Number'First;
                  elsif Y > Ada.Calendar.Year_Number'Last then
                     Y := Ada.Calendar.Year_Number'Last;
                  end if;
                  Temp0 := Temp1 + 1;
                  Temp1 := Index (Line (Temp0 .. Len - 12), ",");
                  if Temp1 in Temp0 + 1 .. Len - 12 then
                     M := Get_Integer (Line (Temp0 .. Temp1 - 1));
                     if M < Ada.Calendar.Month_Number'First then
                        M := Ada.Calendar.Month_Number'First;
                     elsif M > Ada.Calendar.Month_Number'Last then
                        M := Ada.Calendar.Month_Number'Last;
                     end if;
                     Temp0 := Temp1 + 1;
                     Temp1 := Index (Line (Temp0 .. Len - 12), ",");
                     if Temp1 in Temp0 + 1 .. Len - 12 then
                        D := Get_Integer (Line (Temp0 .. Temp1 - 1));
                        if D < Ada.Calendar.Day_Number'First then
                           D := Ada.Calendar.Day_Number'First;
                        elsif D > Ada.Calendar.Day_Number'Last then
                           D := Ada.Calendar.Day_Number'Last;
                        end if;
                        TTime := Ada.Calendar.Time_Of (Y, M, D);
                        return TTime;
                     end if;
                  end if;
               end if;
            end if;
            TIO.Put_Line ("Ill-formed date:" & Line (Idx .. Len));
            raise Constraint_Error;
         end Get_Date;

         procedure Parse_One_Member (Parent : Widget_Pointer);
         procedure Parse_One_Member (Parent : Widget_Pointer) is
            Child : Widget_Pointer;
         begin
            Idx2 := Index (Line (Idx0 .. Len), Test14);
            Idx2 := Idx2 + Test14'Length;
            Idx3 := Index (Line (Idx2 .. Len), ")") - 1;
            Child := Find_Widget (TWin.Widget_List, +Line (Idx2 .. Idx3));
            if Child = null then
               TIO.Put_Line (Img (NLin) & ": "
                             & Line (Idx0 + 1 .. Len) & ": widget not found");
               raise Constraint_Error;
            end if;
            if Child.Parent_Name /= null then
               if Child.Parent_Name.all /= Parent.Name.all then
                  TIO.Put_Line ("Designer 2: " & Img (NLin) & ": "
                                & "Warning: " & Child.Name.all & ": "
                                & "Mismatch with Resource File: "
                                & Child.Parent_Name.all & " / "
                                & Parent.Name.all);
               end if;
               Free (Child.Parent_Name);
            end if;
            Child.Parent_Name := new String'(Parent.Name.all);
            Parent.Num_Children := Parent.Num_Children + 1;
            Child.Child_Num := Parent.Num_Children;
            Debug (NLin, "Set Widget Property "
                   & Child.Name.all
                   & ".Parent_Name "
                   & WT.Name.all);
         end Parse_One_Member;

         function Check_DataGridViewCellStyle return Integer;
         function Check_DataGridViewCellStyle return Integer is
            DGVS_Num : Integer;
            Last     : Integer;
            DGVS_Attr : DGVS_Attribute_Enum;
         begin
            Idx0 := Index (Line (1 .. Len), Sp (8) & Test20);
            if Idx0 not in 1 .. Len then
               return 1; --  continue reading
            end if;
            Idx0 := Idx0 + 8; --  to deal with the leading spaces
            Idx1 := Index (Line (Idx0 + Test20'Length .. Len), ".");
            IIO.Get (Line (Idx0 + Test20'Length .. Len), DGVS_Num, Last);
            if DGVS_Num > Max_DGVS then
               TIO.Put_Line (Img (NLin) & ": "
                             & Line (Idx0 + Test20'Length .. Idx1 - 1)
                             & ": DataGridViewStyle not found");
               return -1;
            end if;
            Idx1 := Idx1 + 1;
            Idx2 := Index (Line (Idx1 .. Len), " = ");
            DGVS_Attr := Symbol_Tables.Get_DGVS_Attribute (Line (Idx1 ..
                                                             Idx2 - 1));
            Idx2 := Idx2 + 3;

            case DGVS_Attr is
               when DGVS_Attr_Alignment =>
                  Idx3 := Index (Line (Idx2 .. Len), "Alignment");
                  if Idx3 not in Idx1 .. Len then
                     raise TIO.Data_Error;
                  end if;
                  DGVS (DGVS_Num).Alignment :=
                    new String'(Line (Idx3 + 10 .. Len));
                  DGVS (DGVS_Num).Emit := True;
                  Debug (NLin, "Set DGVS property "
                         & "DataGridViewStyle" & Img (DGVS_Num)
                         & ".Alignment "
                         & DGVS (DGVS_Num).Alignment.all);

               when DGVS_Attr_BgColor =>
                  DGVS (DGVS_Num).BgColor :=
                    new String'(To_Color (Line (Idx2 .. Len)));
                  DGVS (DGVS_Num).Emit := True;
                  Debug (NLin, "Set DGVS Property "
                         & "DataGridViewStyle" & Img (DGVS_Num)
                         & ".BgColor "
                         & DGVS (DGVS_Num).BgColor.all);

               when DGVS_Attr_FgColor =>
                  DGVS (DGVS_Num).FgColor :=
                    new String'(To_Color (Line (Idx2 .. Len)));
                  DGVS (DGVS_Num).Emit := True;
                  Debug (NLin, "Set DGVS Property "
                         & "DataGridViewStyle" & Img (DGVS_Num)
                         & ".FgColor "
                         & DGVS (DGVS_Num).FgColor.all);

               when DGVS_Attr_SelBgColor =>
                  DGVS (DGVS_Num).SelBgColor :=
                    new String'(To_Color (Line (Idx2 .. Len)));
                  DGVS (DGVS_Num).Emit := True;
                  Debug (NLin, "Set DGVS Property "
                         & "DataGridViewStyle" & Img (DGVS_Num)
                         & ".SelBgColor "
                         & DGVS (DGVS_Num).SelBgColor.all);

               when DGVS_Attr_SelFgColor =>
                  DGVS (DGVS_Num).SelFgColor :=
                    new String'(To_Color (Line (Idx2 .. Len)));
                  DGVS (DGVS_Num).Emit := True;
                  Debug (NLin, "Set DGVS Property "
                         & "DataGridViewStyle" & Img (DGVS_Num)
                         & ".SelFgColor "
                         & DGVS (DGVS_Num).FgColor.all);

               when DGVS_Attr_Font =>
                  Get_Font (Data        => Line (Idx2 .. Len),
                            Font_Name   => DGVS (DGVS_Num).Font_Name,
                            Font_Size   => DGVS (DGVS_Num).Font_Size,
                            Font_Weight => DGVS (DGVS_Num).Font_Weight,
                            Font_Underline => DGVS (DGVS_Num).Font_Underline);
                  DGVS (DGVS_Num).Emit := True;
                  if DGVS (DGVS_Num).Font_Underline then
                     Have.Font_Underline := Have.Font_Underline + 1;
                  end if;
                  if DGVS (DGVS_Num).Font_Weight /= null then
                     Have.Font_Weight := Have.Font_Weight + 1;
                  end if;
                  if DGVS (DGVS_Num).Font_Weight /= null then
                     Debug (NLin, "Set DGVS Property "
                            & "DataGridViewStyle" & Img (DGVS_Num)
                            & ".Font " & DGVS (DGVS_Num).Font_Name.all
                            & " " & Img (DGVS (DGVS_Num).Font_Size)
                            & " " & DGVS (DGVS_Num).Font_Weight.all
                            & (if DGVS (Num).Font_Underline then
                             " underline" else ""));
                  else
                     Debug (NLin, "Set DGVS Property "
                            & "DataGridViewStyle" & Img (DGVS_Num)
                            & ".Font " & DGVS (DGVS_Num).Font_Name.all
                            & " " & Img (DGVS (DGVS_Num).Font_Size)
                            & (if DGVS (DGVS_Num).Font_Underline then
                             " underline" else ""));
                  end if;

               when DGVS_Attr_Format =>
                  DGVS (DGVS_Num).Format :=
                    To_Cell_Format_Enum (Line (Idx2 .. Len));
                  DGVS (DGVS_Num).Emit := True;
                  Debug (NLin, "Set DGVS Property "
                         & "DataGridViewStyle" & Img (DGVS_Num)
                         & ".Format "
                         & DGVS (DGVS_Num).Format'Image);

               when DGVS_Attr_Padding =>
                  Idx3 := Index (Line (Idx2 .. Len), "(");
                  DGVS (DGVS_Num).Padding :=
                    Get_Margin_Array (Line (Idx3 + 1 .. Len - 1));
                  DGVS (DGVS_Num).Emit := True;
                  Debug (NLin, "Set DGVS Property "
                         & "DataGridViewStyle" & Img (DGVS_Num)
                         & ". Margins "
                         & Img (DGVS (DGVS_Num).Padding (1)) & ", "
                         & Img (DGVS (DGVS_Num).Padding (2)) & ", "
                         & Img (DGVS (DGVS_Num).Padding (3)) & ", "
                         & Img (DGVS (DGVS_Num).Padding (4)));

               when DGVS_Attr_WrapMode =>
                  if Index (Line (Idx2 .. Len), "True") in Idx2 .. Len then
                     DGVS (DGVS_Num).WrapMode := True;
                     Debug (NLin, "Set DGVS Property "
                            & "DataGridViewStyle" & Img (DGVS_Num)
                            & ".WrapMode "
                            & "True");
                  elsif Index (Line (Idx2 .. Len), "False") in Idx2 .. Len then
                     DGVS (DGVS_Num).WrapMode := False;
                     Debug (NLin, "Set DGVS Property "
                            & "DataGridViewStyle" & Img (DGVS_Num)
                            & ".WrapMode "
                            & "False");
                  end if;

               when DGVS_Attr_NullValue =>
                  DGVS (DGVS_Num).NullValue := new String'(Line (Idx2 .. Len));
                  Debug (NLin, "Set DGVS Property "
                         & "DataGridViewStyle" & Img (DGVS_Num)
                         & ".NullValue "
                         & DGVS (DGVS_Num).NullValue.all);

               when DGVS_Attr_Ignored =>
                  Debug (NLin, Resx_File_Name
                         & ".Designer.vb (2)"
                         & ": Ignored DGVS property: "
                         & "DataGridViewStyle" & Img (DGVS_Num)
                         & " " & Line (Idx0 .. Idx1 - 1));

               when DGVS_No_Attribute =>
                  TIO.Put_Line (Resx_File_Name
                                & ".Designer.vb (2)"
                                & ": Line" & NLin'Image
                                & ": unknown DGVS property: "
                                & Line (Idx1 .. Len));
            end case;
            return 0; --  skip to end of loop
         end Check_DataGridViewCellStyle;

         P0 : Pair;
      begin --  Process_Widget
         --  enter to read line with widget name
         if TIO.End_Of_File (DFile) then
            return 0;
         end if;
         Get_Line;
         Idx0 := Index (Line (1 .. Len), Test9);
         Idx0 := Idx0 + Test9'Length;
         WT := Find_Widget (TWin.Widget_List, +Line (Idx0 .. Len));
         if WT = null then
            if TWin.Name.all = +Line (Idx0 .. Len) then
               Len := 0;
               return 0;
            end if;
            TIO.Put_Line ("Designer 2: " & Img (NLin) & ": "
                          & Line (Idx0 .. Len)
                          & ": widget not found");
            return -1;
         end if;
         --  skip second test9-line
         if TIO.End_Of_File (DFile) then
            return 0;
         end if;
         Get_Line;
         --  read attributes
         loop
            exit when TIO.End_Of_File (DFile);
            Get_Line;
            --  first test9 of following widget
            exit when Line (Line'First .. Len) = Test9;
            Ret := Check_DataGridViewCellStyle;
            if Ret = 0 then
               goto Continue_Loop;
            elsif Ret = -1 then
               return -1;
            end if;
            --  Ret is 1, continue reading
            Idx0 := Index (To_Lower (Line (1 .. Len)),
                           To_Lower ("Me." & WT.Name.all & "."));
            if Idx0 not in Line'Range then
               goto Continue_Loop;
            end if;

            Idx0 := Idx0 + 3 + WT.Name.all'Length + 1;
            Idx1 := Index (Line (Idx0 .. Len), " = ");
            if Idx1 not in Idx0 .. Len then
               if Line (Idx0 .. Idx0 + Test3'Length - 1) = Test3 then
                  Idx1 := Idx0 + Test3'Length;
               elsif Line (Idx0 .. Idx0 + Test4'Length - 1) = Test4 then
                  Idx1 := Idx0 + Test4'Length;
               elsif Line (Idx0 .. Idx0 + Test6'Length - 1) = Test6 then
                  Idx1 := Idx0 + Test6'Length;
               elsif Contains (Line (Idx0 .. Len), Test14)
                 and then
                   (WT.Widget_Type = GtkFrame or
                      WT.Widget_Type = GtkTabPage or
                        WT.Widget_Type = GtkNoteBook or
                          WT.Widget_Type = GtkBox)
               then
                  Parse_One_Member (WT);
                  goto Continue_Loop;
               elsif Contains (Line (Idx0 .. Len), Test15)
                 or else
                   Contains (Line (Idx0 .. Len), Test16)
                   or else
                     Contains (Line (Idx0 .. Len), Test17)
               then
                  Debug (NLin, Resx_File_Name
                         & ".Designer.vb (2)"
                         & ": Line" & NLin'Image
                         & ": Designer: Ignored Widget Property: "
                         & WT.Name.all & " " & Line (Idx0 .. Len));
                  goto Continue_Loop;

               else
                  TIO.Put_Line (Resx_File_Name
                                & ".Designer.vb (2)"
                                & ": Line" & NLin'Image
                                & ": Designer: cannot parse "
                                & Trim (Line (1 .. Len), Ada.Strings.Both));
                  return -1;
               end if;
            end if;
            if Idx1 not in Idx0 .. Len then
               goto Continue_Loop;
            end if;

            Attr := Symbol_Tables.Get_Attribute (Line (Idx0 .. Idx1 - 1));
            if Attr = Attr_AutoSizeMode and then
              (WT.Widget_Type in ExpandableColumn | DataGridViewTextBoxColumn
                                     | DataGridViewCheckBoxColumn)
            then
               Attr := Attr_AutoSizeColumnMode;
            end if;
            Idx1 := Idx1 + 3;

            case Attr is
               when Attr_Text =>
                  if WT.Text /= null
                    and then WT.Text.all /= Line (Idx1 + 1 .. Len - 1)
                  then
                     TIO.Put_Line (Img (NLin)
                                   & ": text mistmatch between "
                                   & "Designer and Resource");
                     raise Constraint_Error;
                  else
                     WT.Text := new String'(Trim (Line (Idx1 + 1 .. Len - 1),
                                            Ada.Strings.Both));
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".Text "
                            & Line (Idx1 + 1 .. Len - 1));
                  end if;

               when Attr_Name =>
                  if WT.Name.all /= +Line (Idx1 + 1 .. Len - 1) then
                     TIO.Put_Line (Img (NLin)
                                   & ": name mistmatch between "
                                   & "Designer and Resource");
                     raise Constraint_Error;
                  end if;
                  Debug (NLin, "Set Widget Property "
                         & WT.Name.all & ".Name "
                         & Line (Idx1 + 1 .. Len - 1));

               when Attr_Location =>
                  Idx2 := Index (Line (Idx1 .. Len), "(");
                  P0 := Get_Pair (Line (Idx2 + 1 .. Len));
                  WT.Location.From_Top  := P0.Two;
                  WT.Location.From_Left := P0.One;
                  Debug (NLin, "Set Widget Property "
                         & WT.Name.all
                         & ".Location H="
                         & Img (P0.One) & ", V=" & Img (P0.Two));

               when Attr_Size =>
                  Idx2 := Index (Line (Idx1 .. Len), "(");
                  P0 := Get_Pair (Line (Idx2 + 1 .. Len));
                  WT.Size.Horiz := P0.Two;
                  WT.Size.Vert  := P0.One;
                  Debug (NLin, "Set Widget Property "
                         & WT.Name.all
                         & ".Size H="
                         & Img (P0.One) & ", V=" & Img (P0.Two));

               when Attr_FlowDirection =>
                  if Contains (Line (Idx1 .. Len), "TopDown") then
                     WT.FlowDirection := TopDown;
                  elsif Contains (Line (Idx1 .. Len), "RightToLeft") then
                     WT.FlowDirection := RightToLeft;
                  elsif Contains (Line (Idx1 .. Len), "LeftToRight") then
                     WT.FlowDirection := LeftToRight;
                  elsif Contains (Line (Idx1 .. Len), "BottomUp") then
                     WT.FlowDirection := BottomUp;
                  end if;
                  Debug (NLin, "Set Widget Property "
                         & WT.Name.all
                         & ".Horizontal "
                         & WT.FlowDirection'Image);

               when Attr_TabIndex =>
                  WT.TabIndex := Get_Integer (Line (Idx1 .. Len));
                  Debug (NLin, "Set Widget Property "
                         & WT.Name.all
                         & ".TabIndex "
                         & Img (WT.TabIndex));

               when Attr_Items_AddRange =>
                  case WT.Widget_Type is
                     when GtkToolBar | BindingNavigator
                        | GtkMenuBar | GtkStatusBar =>
                        Add_Items (WT);
                     when GtkComboTextBox =>
                        null;
                     when others =>
                        raise Program_Error;
                  end case;

               when Attr_Columns_AddRange =>
                  case WT.Widget_Type is
                     when GtkTreeGridView | GtkDataGridView =>
                        Add_Items (WT);
                     when others =>
                        raise Program_Error;
                  end case;

               when Attr_DropDownItems_AddRange =>
                  if WT.Widget_Type = GtkMenuImageItem then
                     Add_Items (WT);
                  else
                     raise Program_Error;
                  end if;

               when Attr_DisplayStyle =>
                  WT.DStyle := Convert (Line (Idx1 .. Len));
                  Debug (NLin, "Set Widget Property "
                         & WT.Name.all & ".DStyle "
                         & Line (Idx1 .. Len));

               when Attr_TabStop =>
                  if Line (Idx1 .. Len) = "False" then
                     WT.TabStop := False;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".TabStop False");
                  elsif Line (Idx1 .. Len) = "True" then
                     WT.TabStop := True;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".TabStop True");
                  end if;

               when Attr_AutoToolTip =>
                  if Line (Idx1 .. Len) = "False" then
                     WT.AutoToolTip := False;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".AutoToolTip False");
                  elsif Line (Idx1 .. Len) = "True" then
                     WT.AutoToolTip := True;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".AutoToolTip True");
                     if WT.ToolTip = null then
                        WT.ToolTip := new String'(WT.Name.all);
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".ToolTip "
                               & WT.Name.all);
                     end if;
                  end if;

               when Attr_BorderStyle =>
                  if WT.Widget_Type = GtkEntry then
                     Idx2 := Index (Line (Idx1 .. Len),
                                    "BorderStyle.None");
                     if Idx2 in Idx1 .. Len then
                        WT.Has_Frame := False;
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".Has_Frame False");
                     end if;

                  elsif WT.Widget_Type = GtkLabel then
                     Idx2 := Index (Line (Idx1 .. Len), "BorderStyle");
                     if Idx2 not in Idx1 .. Len then
                        raise TIO.Data_Error;
                     end if;
                     if Contains (Line (Idx2 + 12 .. Len), "None") then
                        WT.BorderStyle := None;
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".BorderStyle "
                               & "None");
                     elsif Contains (Line (Idx2 + 12 .. Len), "FixedSingle")
                     then
                        WT.BorderStyle := FixedSingle;
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".BorderStyle "
                               & "FixedSingle");
                     elsif Contains (Line (Idx2 + 12 .. Len), "Fixed3D") then
                        WT.BorderStyle := Fixed3D;
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".BorderStyle "
                               & "Fixed3D");
                     else
                        raise TIO.Data_Error;
                     end if;
                  end if;

               when Attr_Alignment =>
                  Idx2 := Index (Line (Idx1 .. Len), "Alignment");
                  if Idx2 not in Idx1 .. Len then
                     raise TIO.Data_Error;
                  end if;
                  TAIO.Get (Line (Idx2 + 10 .. Len), WT.TextAlign, Idx0);
                  declare
                     Temp : String (1 .. 12);
                  begin
                     TAIO.Put (Temp, WT.TextAlign);
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".TextAlign "
                            & Temp);
                  end;

               when Attr_ImageScalingSize =>
                  Idx2 := Index (Line (Idx1 .. Len), "(");
                  WT.ImageScalingSize := Get_Pair (Line (Idx2 + 1 .. Len - 1));
                  Debug (NLin, "Set Widget Property "
                         & WT.Name.all & ".ImageScalingSize "
                         & Img (WT.ImageScalingSize.One)
                         & ", " & Img (WT.ImageScalingSize.Two));

               when Attr_GripStyle =>
                  if WT.Widget_Type = GtkToolBar then
                     if Contains (Line (Idx1 .. Len), "Hidden") then
                        WT.Grip_Visible := False;
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".Grip_Visible False");
                     elsif Contains (Line (Idx1 .. Len), "Visible") then
                        WT.Grip_Visible := True;
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".Grip_Visible True");
                     end if;
                  end if;

               when Attr_Checked =>
                  if (WT.Widget_Type = GtkRadioButton or
                        WT.Widget_Type = GtkButton or
                          WT.Widget_Type = GtkCheckButton or
                            WT.Widget_Type = GtkToggleButton)
                  then
                     if Contains (Line (Idx1 .. Len), "False") then
                        WT.Active := False;
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".Active False");
                     elsif Contains (Line (Idx1 .. Len), "True") then
                        WT.Active := True;
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".Active True");
                     end if;
                  end if;

               when Attr_CheckState =>
                  if (WT.Widget_Type = GtkRadioButton or
                        WT.Widget_Type = GtkButton or
                          WT.Widget_Type = GtkCheckButton or
                            WT.Widget_Type = GtkToggleButton)
                  then
                     if Contains (Line (Idx1 .. Len), "Checked") then
                        WT.Active := True;
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".Active True");
                     else
                        WT.Active := False;
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".Active False");
                     end if;
                  end if;

               when Attr_AnyColor =>
                  if WT.Widget_Type = GtkColorButton then
                     if Contains (Line (Idx1 .. Len), "False") then
                        WT.AnyColor := False;
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".AnyColor False");
                     elsif Contains (Line (Idx1 .. Len), "True") then
                        WT.AnyColor := True;
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".AnyColor True");
                     end if;
                  end if;

               when Attr_FullOpen =>
                  if WT.Widget_Type = GtkColorButton then
                     if Contains (Line (Idx1 .. Len), "False") then
                        WT.FullOpen := False;
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".FullOpen False");
                     elsif Contains (Line (Idx1 .. Len), "True") then
                        WT.FullOpen := True;
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".FullOpen True");
                     end if;
                  end if;

               when Attr_UseSystemPasswordChar =>
                  if  WT.Widget_Type = GtkEntry then
                     if Contains (Line (Idx1 .. Len), "True") then
                        WT.PasswordChar := new String'("");
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".PasswordChar");
                     end if;
                  end if;

               when Attr_ReadOnly =>
                  case WT.Widget_Type is
                     when GtkEntry | GtkComboTextBox =>
                        if Contains (Line (Idx1 .. Len), "False") then
                           WT.Editable := True;
                           Debug (NLin, "Set Widget Property "
                                  & WT.Name.all & ".Editable False");
                        elsif Contains (Line (Idx1 .. Len), "True") then
                           WT.Editable := False;
                           Debug (NLin, "Set Widget Property "
                                  & WT.Name.all & ".Editable True");
                        end if;

                     when GtkDataGridView | GtkTreeGridView
                        | ExpandableColumn | DataGridViewTextBoxColumn
                        | DataGridViewCheckBoxColumn  =>
                        if Contains (Line (Idx1 .. Len), "False") then
                           WT.ReadOnly := False;
                           Debug (NLin, "Set Widget Property "
                                  & WT.Name.all & ".ReadOnly False");
                        elsif Contains (Line (Idx1 .. Len), "True") then
                           WT.ReadOnly := True;
                           Debug (NLin, "Set Widget Property "
                                  & WT.Name.all & ".ReadOnly True");
                        end if;
                     when others => null;
                  end case;

               when Attr_Sorted =>
                  if WT.Widget_Type = GtkComboTextBox then
                     if Contains (Line (Idx1 .. Len), "False") then
                        WT.Sorted := False;
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".Sorted False");
                     elsif Contains (Line (Idx1 .. Len), "True") then
                        WT.Sorted := True;
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".Sorted True");
                     end if;
                  end if;

               when Attr_Maximum =>
                  if WT.Widget_Type = GtkSpinButton then
                     Idx2 := Idx1 + Test10'Length;
                     Idx3 := Index (Line (Idx2 .. Len), ",");
                     WT.MaxValue := Get_Integer (Line (Idx2 .. Idx3 - 1));
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".Maximum "
                            & Img (WT.MaxValue));
                  end if;

               when Attr_Minimum =>
                  if WT.Widget_Type = GtkSpinButton then
                     Idx2 := Idx1 + Test10'Length;
                     Idx3 := Index (Line (Idx2 .. Len), ",");
                     WT.MinValue := Get_Integer (Line (Idx2 .. Idx3 - 1));
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".Minimum "
                            & Img (WT.MinValue));
                  end if;

               when Attr_Value =>
                  if WT.Widget_Type = GtkSpinButton then
                     Idx2 := Idx1 + Test10'Length;
                     Idx3 := Index (Line (Idx2 .. Len), ",");
                     WT.StartValue := Get_Integer (Line (Idx2 .. Idx3 - 1));
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".StartValue "
                            & Img (WT.StartValue));
                  elsif WT.Widget_Type = GtkCalendar then
                     WT.Start_Date := Get_Date (Idx1);
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".Start_Date"
                            & Image (WT.Start_Date, ISO_Date));
                  end if;

               when Attr_Format =>
                  if WT.Widget_Type = GtkCalendar then
                     WT.Format_Date := new String'(Line (Idx1 .. Len));
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".Format_Date"
                            & WT.Format_Date.all);
                  end if;

               when Attr_MinDate =>
                  if WT.Widget_Type = GtkCalendar then
                     WT.MinDate := Get_Date (Idx1);
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".Start_Date"
                            & Image (WT.MinDate, ISO_Date));
                  end if;

               when Attr_MaxDate =>
                  if WT.Widget_Type = GtkCalendar then
                     WT.MaxDate := Get_Date (Idx1);
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".Start_Date"
                            & Image (WT.MaxDate, ISO_Date));
                  end if;

               when Attr_LimitDatePicket =>
                  if WT.Widget_Type = GtkCalendar then
                     WT.MaxDate := Get_Date (Idx1);
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".Start_Date"
                            & Image (WT.MaxDate, ISO_Date));
                  end if;

               when Attr_Image =>
                  case WT.Widget_Type is
                     when GtkButton | GtkRadioButton
                        | GtkCheckButton | GtkToggleButton =>
                        WT.ImagePath :=
                          new String'(Icon_Path & "/"
                                      & Icon_Name (Line (Idx1 .. Len)));
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".ImagePath "
                               & WT.ImagePath.all);
                     when GtkImage =>
                        WT.Image :=
                          new String'(Icon_Path & "/"
                                      & Icon_Name (Line (Idx1 .. Len)));
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".Image "
                               & WT.Image.all);
                     when GtkMenuImageItem =>
                        WT.ImageMenu :=
                          new String'(Icon_Path & "/"
                                      & Icon_Name (Line (Idx1 .. Len)));
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".ImageMenu "
                               & WT.ImageMenu.all);

                     when others => null;
                  end case;

               when Attr_ImageAlign =>
                  if (WT.Widget_Type = GtkButton or
                        WT.Widget_Type = GtkRadioButton or
                          WT.Widget_Type = GtkCheckButton or
                            WT.Widget_Type = GtkToggleButton)
                  then
                     WT.ImageAlign := Convert (Line (Idx1 .. Len));
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".ImageAlign "
                            & Line (Idx1 .. Len));
                  end if;

               when Attr_ShowUpDown =>
                  if WT.Widget_Type = GtkCalendar then
                     if Contains (Line (Idx1 .. Len), "False") then
                        WT.ShowUpDown := False;
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".ShowUpDown False");
                     elsif Contains (Line (Idx1 .. Len), "True") then
                        WT.ShowUpDown := True;
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".ShowUpDown True");
                     end if;
                  end if;

               when Attr_SelectionMode =>
                  if WT.Widget_Type = GtkListBox then
                     WT.MultiSelect := True;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".MultiSelect True");
                  end if;

               when Attr_ForeColor =>
                  WT.FgColor :=
                    new String'(To_Color (Line (Idx1 .. Len)));
                  Debug (NLin, "Set Widget Property "
                         & WT.Name.all & "." & "FgColor "
                         & WT.FgColor.all);

               when Attr_BackColor =>
                  WT.BgColor :=
                    new String'(To_Color (Line (Idx1 .. Len)));
                  Debug (NLin, "Set Widget Property "
                         & WT.Name.all & "." & "BgColor "
                         & WT.BgColor.all);

               when Attr_Level =>
                  IIO.Get (Line (Idx1 .. Len), Num, Last);
                  WT.Level := Num;
                  Debug (NLin, "Set Widget Property "
                         & WT.Name.all
                         & ".Level "
                         & Img (Num));

               when Attr_ScrollBars =>
                  Idx2 := Index (Line (Idx1 .. Len), ".",
                                 Ada.Strings.Backward);
                  if Contains (Line (Idx2 + 1 .. Len), "None") then
                     WT.ScrollBars := None;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".ScrollBars "
                            & "None");
                  elsif Contains (Line (Idx2 + 1 .. Len), "Both") then
                     WT.ScrollBars := Both;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".ScrollBars "
                            & "Vertical");
                  elsif Contains (Line (Idx2 + 1 .. Len), "Both") then
                     WT.ScrollBars := Both;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".ScrollBars "
                            & "Horizontal");
                  elsif Contains (Line (Idx2 + 1 .. Len), "Both") then
                     WT.ScrollBars := Both;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".ScrollBars "
                            & "Both");
                  else
                     raise TIO.Data_Error;
                  end if;

               when Attr_RowHeadersBorderStyle =>
                  Idx2 := Index (Line (Idx1 .. Len), ".",
                                 Ada.Strings.Backward);
                  if Contains (Line (Idx2 + 1 .. Len), "None") then
                     WT.RowHeadersBorderStyle := None;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".RowHeadersBorderStyle "
                            & "None");
                  elsif Contains (Line (Idx2 + 1 .. Len), "Sunken") then
                     WT.RowHeadersBorderStyle := Sunken;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".RowHeadersBorderStyle "
                            & "Sunken");
                  elsif Contains (Line (Idx2 + 1 .. Len), "Raised") then
                     WT.RowHeadersBorderStyle := Raised;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".RowHeadersBorderStyle "
                            & "Raised");
                  elsif Contains (Line (Idx2 + 1 .. Len), "Single") then
                     WT.RowHeadersBorderStyle := Single;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".RowHeadersBorderStyle "
                            & "Single");
                  elsif Contains (Line (Idx2 + 1 .. Len), "Custom") then
                     WT.RowHeadersBorderStyle := Custom;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".RowHeadersBorderStyle "
                            & "Custom");
                  else
                     raise TIO.Data_Error;
                  end if;

               when Attr_Frozen =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.Frozen := False;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all
                            & ".Frozen False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.Frozen := True;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all
                            & ".Frozen True");
                  end if;

               when Attr_WorkerReportsProgress =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.WorkerReportsProgress := False;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all
                            & ".WorkerReportsProgress False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.WorkerReportsProgress := True;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all
                            & ".WorkerReportsProgress True");
                  end if;

               when Attr_WorkerSupportsCancellation =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.WorkerSupportsCancellation := False;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all
                            & ".WorkerSupportsCancellation False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.WorkerSupportsCancellation := True;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all
                            & ".WorkerSupportsCancellation True");
                  end if;

               when Attr_EnableMetric =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.EnableMetric := False;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all
                            & ".EnableMetric False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.EnableMetric := True;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all
                            & ".EnableMetric True");
                  end if;

               when Attr_MinMargins =>
                  Idx3 := Index (Line (Idx1 .. Len), "(");
                  WT.MinMargins :=
                    Get_Margin_Array (Line (Idx3 + 1 .. Len - 1));
                  Debug (NLin, "Set Widget Property "
                         & WT.Name.all
                         & ".MinMargins "
                         & Img (WT.MinMargins (1)) & ", "
                         & Img (WT.MinMargins (2)) & ", "
                         & Img (WT.MinMargins (3)) & ", "
                         & Img (WT.MinMargins (4)));

               when Attr_SelectedIndex =>
                  IIO.Get (Line (Idx1 .. Len), Num, Last);
                  WT.SelectedIndex := Num;
                  Debug (NLin, "Set Widget Property "
                         & WT.Name.all
                         & ".SelectedIndex "
                         & Img (Num));

               when Attr_Padding =>
                  Idx2 := Index (Line (Idx1 .. Len), "(");
                  IIO.Get (Line (Idx2 + 1 .. Len), Num, Last);
                  WT.Padding := Num;
                  Debug (NLin, "Set Widget Property "
                         & WT.Name.all
                         & ".Padding "
                         & Img (Num));

               when Attr_PaddingX =>
                  IIO.Get (Line (Idx1 .. Len), Num, Last);
                  WT.PaddingX := Num;
                  Debug (NLin, "Set Widget Property "
                         & WT.Name.all
                         & ".PaddingX "
                         & Img (Num));

               when Attr_PaddingY =>
                  IIO.Get (Line (Idx1 .. Len), Num, Last);
                  WT.PaddingY := Num;
                  Debug (NLin, "Set Widget Property "
                         & WT.Name.all
                         & ".PaddingY "
                         & Img (Num));

               when Attr_UseVisualStyleBackColor =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.UseVisualStyleBackColor := False;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all
                            & ".UseVisualStyleBackColor False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.UseVisualStyleBackColor := True;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all
                            & ".UseVisualStyleBackColor True");
                  end if;

               when Attr_EditModeProgramatically =>
                  WT.EditModeProgramatically := True;
                  Debug (NLin, "Set Widget Property "
                         & WT.Name.all
                         & ".EditModeProgramatically True");

               when Attr_Resizable => null;
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.Resizable := False;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".Resizable False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.Resizable := True;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".Resizable True");
                  end if;

               when Attr_SortMode =>
                  Idx2 := Index (Line (Idx1 .. Len), ".",
                                 Ada.Strings.Backward);
                  if Contains (Line (Idx2 + 1 .. Len), "NotSortable") then
                     WT.SortMode := NotSortable;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".SortMode "
                            & "NotSortable");
                  elsif Contains (Line (Idx2 + 1 .. Len), "Automatic") then
                     WT.SortMode := Automatic;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".SortMode "
                            & "Automatic");
                  elsif Contains (Line (Idx2 + 1 .. Len), "Programmatic") then
                     WT.SortMode := Programmatic;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".SortMode "
                            & "Programmatic");
                  else
                     raise TIO.Data_Error;
                  end if;

               when Attr_AutoSizeMode =>
                  Idx2 := Index (Line (Idx1 .. Len), ".",
                                 Ada.Strings.Backward);
                  if Contains (Line (Idx2 + 1 .. Len), "GrowAndShrink") then
                     WT.AutoSizeMode := GrowAndShrink;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".AutoSizeMode "
                            & "GrowAndShrink");
                  elsif Contains (Line (Idx2 + 1 .. Len), "GrowOnly") then
                     WT.AutoSizeMode := GrowOnly;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".AutoSizeMode "
                            & "GrowOnly");
                  else
                     raise TIO.Data_Error;
                  end if;

               when Attr_AutoSizeColumnMode =>
                  WT.AutoSizeColumnMode :=
                    To_AutoSizeColumnMode (Line (1 .. Len));
                  declare
                     Temp : String (1 .. 26);
                  begin
                     ASCMIO.Put (Temp, WT.AutoSizeColumnMode);
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".AutoSizeColumnMode "
                            & Temp);
                  end;

               when Attr_AllowUserToAddRows =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.AllowUserToAddRows := False;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".AllowUserToAddRows False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.AllowUserToAddRows := True;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".AllowUserToAddRows True");
                  end if;

               when Attr_AllowUserToDeleteRows =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.AllowUserToDeleteRows := False;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".AllowUserToDeleteRows False");
                  elsif Contains (Line (Idx1 .. Len), "False") then
                     WT.AllowUserToAddRows := True;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".AllowUserToDeleteRows True");
                  end if;

               when Attr_EnableHeadersVisualStyles =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.EnableHeadersVisualStyles := False;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all
                            & ".EnableHeadersVisualStyles False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.EnableHeadersVisualStyles := True;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all
                            & ".EnableHeadersVisualStyles True");
                  end if;

               when Attr_MultiSelect =>
                  if (WT.Widget_Type = GtkTreeGridView or
                        WT.Widget_Type = GtkDataGridView)
                  then
                     if Contains (Line (Idx1 .. Len), "False") then
                        WT.RowMultiSelect := False;
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".RowMultiSelect False");
                     elsif Contains (Line (Idx1 .. Len), "True") then
                        WT.RowMultiSelect := True;
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".RowMultiSelect True");
                     end if;
                  elsif WT.Widget_Type = GtkListBox then
                     if Contains (Line (Idx1 .. Len), "False") then
                        WT.MultiSelect := False;
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".MultiSelect False");
                     elsif Contains (Line (Idx1 .. Len), "True") then
                        WT.MultiSelect := True;
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all & ".MultiSelect True");
                     end if;
                  end if;

               when Attr_ColumnHeadersVisible =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.ColumnHeadersVisible := False;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".ColumnHeadersVisible False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.ColumnHeadersVisible := True;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".ColumnHeadersVisible True");
                  end if;

               when Attr_RowHeadersVisible =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.RowHeadersVisible := False;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".RowHeadersVisible False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.RowHeadersVisible := True;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".RowHeadersVisible True");
                  end if;

               when Attr_AllowUserToResizeRows =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.AllowUserToResizeRows := False;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".AllowUserToResizeRows False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.AllowUserToAddRows := True;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".AllowUserToResizeRows True");
                  end if;

               when Attr_AllowUserToOrderColumns =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.AllowUserToOrderColumns := False;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".AllowUserToOrderColumns False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.AllowUserToOrderColumns := True;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".AllowUserToOrderColumns True");
                  end if;

               when Attr_DefaultCellStyle =>
                  Idx2 := Index (Line (Idx1 .. Len), Test20);
                  if Idx2 in Idx1 .. Len then
                     IIO.Get (Line (Idx1 + Test20'Length .. Len),
                              Num, Last);
                     if Num not in DGVS'Range then
                        TIO.Put_Line (Resx_File_Name
                                      & "Designer.vb (2:) "
                                      & "No datagridviewcellstyle"
                                      & Img (Num)
                                      & " at line" & NLin'Image);
                        raise TIO.Data_Error;
                     end if;
                     if DGVS (Num).Name /= null then
                        TIO.Put_Line (Resx_File_Name
                                      & ".Designer.vb (2)"
                                      & ": Line" & NLin'Image
                                      & ": Designer: repeated: "
                                      & Line (Idx0 .. Len));
                        raise TIO.Data_Error;
                     end if;
                     WT.DefaultCellStyle := Num;
                     case WT.Widget_Type is
                        when ExpandableColumn | DataGridViewTextBoxColumn =>
                           DGVS (Num).Name :=
                             new String'("CRT_" & WT.Name.all);
                           DGVS (Num).Style_For := For_TextCell;
                        when DataGridViewCheckBoxColumn =>
                           DGVS (Num).Name :=
                             new String'("CRTG_" & WT.Name.all);
                           DGVS (Num).Style_For := For_ToggleCell;
                        when GtkDataGridView | GtkTreeGridView =>
                           DGVS (Num).Name :=
                             new String'("DEF_" & WT.Name.all);
                           DGVS (Num).Style_For := For_TreeGridView;
                        when others =>
                           null;
                     end case;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all
                            & ".DefaultCellStyle "
                            & Test20 & Img (Num));
                  else
                     raise TIO.Data_Error;
                  end if;

               when Attr_AlternatingRowsDefaultCellStyle =>
                  Idx2 := Index (Line (Idx1 .. Len), Test20);
                  if Idx2 in Idx1 .. Len then
                     IIO.Get (Line (Idx1 + Test20'Length .. Len),
                              Num, Last);
                     if Num not in DGVS'Range then
                        TIO.Put_Line (Resx_File_Name
                                      & "Designer.vb (2:) "
                                      & "No datagridviewcellstyle"
                                      & Img (Num)
                                      & " at line" & NLin'Image);
                        raise TIO.Data_Error;
                     end if;
                     if DGVS (Num).Name /= null then
                        TIO.Put_Line (Resx_File_Name
                                      & ".Designer.vb (2)"
                                      & ": Line" & NLin'Image
                                      & ": Designer: repeated: "
                                      & Line (Idx0 .. Len));
                        raise TIO.Data_Error;
                     end if;
                     WT.AlternatingRowsDefaultCellStyle := Num;
                     DGVS (Num).Name := new String'("ALT_" & WT.Name.all);
                     DGVS (Num).Style_For := For_TreeGridView;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all
                            & ".AlternatingRowsDefaultCellStyle "
                            & Test20 & Img (Num));
                  else
                     raise TIO.Data_Error;
                  end if;

               when Attr_ColumnHeadersDefaultCellStyle =>
                  Idx2 := Index (Line (Idx1 .. Len), Test20);
                  if Idx2 in Idx1 .. Len then
                     declare
                        Num  : Integer;
                        Last : Integer;
                     begin
                        IIO.Get (Line (Idx1 + Test20'Length .. Len),
                                 Num, Last);
                        if Num not in DGVS'Range then
                           TIO.Put_Line (Resx_File_Name
                                         & "Designer.vb (2:) "
                                         & "No datagridviewcellstyle"
                                         & Img (Num)
                                         & " at line" & NLin'Image);
                           raise TIO.Data_Error;
                        end if;
                        if DGVS (Num).Name /= null then
                           TIO.Put_Line (Resx_File_Name
                                         & ".Designer.vb (2)"
                                         & ": Line" & NLin'Image
                                         & ": Designer: repeated: "
                                         & Line (Idx0 .. Len));
                           raise TIO.Data_Error;
                        end if;
                        Have.HDR_CellRenderers := Have.HDR_CellRenderers + 1;
                        WT.ColumnHeadersDefaultCellStyle := Num;
                        DGVS (Num).Style_For := For_Column_Header;
                        DGVS (Num).Name := new String'("HDR_" & WT.Name.all);
                        Debug (NLin, "Set Widget Property "
                               & WT.Name.all
                               & ".ColumnHeadersDefaultCellStyle "
                               & Test20 & Img (Num));
                     end;
                  else
                     raise TIO.Data_Error;
                  end if;

               when Attr_RowHeadersDefaultCellStyle =>
                  Idx2 := Index (Line (Idx1 .. Len), Test20);
                  if Idx2 in Idx1 .. Len then
                     IIO.Get (Line (Idx1 + Test20'Length .. Len),
                              Num, Last);
                     if Num not in DGVS'Range then
                        TIO.Put_Line (Resx_File_Name
                                      & "Designer.vb (2:) "
                                      & "No datagridviewcellstyle"
                                      & Img (Num)
                                      & " at line" & NLin'Image);
                        raise TIO.Data_Error;
                     end if;
                     if DGVS (Num).Name /= null then
                        TIO.Put_Line (Resx_File_Name
                                      & ".Designer.vb (2)"
                                      & ": Line" & NLin'Image
                                      & ": Designer: repeated: "
                                      & Line (Idx0 .. Len));
                        raise TIO.Data_Error;
                     end if;
                     WT.RowHeadersDefaultCellStyle := Num;
                     DGVS (Num).Style_For := For_Row_Header;
                     DGVS (Num).Name := new String'("ROW_" & WT.Name.all);
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all
                            & ".RowHeadersDefaultCellStyle "
                            & Test20 & Img (Num));
                  else
                     raise TIO.Data_Error;
                  end if;

               when Attr_CloseButtonOnTabsInactiveVisible =>
                  if Contains (Line (Idx1 .. Len), "False") then
                     WT.CloseButtonOnTabsInactiveVisible := False;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all
                            & ".CloseButtonOnTabsInactiveVisible False");
                  elsif Contains (Line (Idx1 .. Len), "True") then
                     WT.CloseButtonOnTabsInactiveVisible := True;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all
                            & ".CloseButtonOnTabsInactiveVisible True");
                  end if;

               when Attr_RowHeadersWidthSizeMode =>
                  Idx2 := Index (Line (Idx1 .. Len), ".",
                                 Ada.Strings.Backward);
                  if Contains (Line (Idx2 + 1 .. Len), "EnableResizing") then
                     WT.RowHeadersWidthSizeMode := EnableResizing;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".RowDeadersWidthSizeMode "
                            & "EnableResizing");
                  elsif Contains (Line (Idx2 + 1 .. Len), "DisableResizing")
                  then
                     WT.RowHeadersWidthSizeMode := DisableResizing;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".RowDeadersWidthSizeMode "
                            & "DisableResizing");
                  elsif Contains (Line (Idx2 + 1 .. Len),
                                  "AutoSizeToAllHeaders")
                  then
                     WT.RowHeadersWidthSizeMode := AutoSizeToAllHeaders;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".RowDeadersWidthSizeMode "
                            & "AutoSizeToAllHeaders");
                  elsif Contains (Line (Idx2 + 1 .. Len),
                                  "AutoSizeToDisplayedHeaders")
                  then
                     WT.RowHeadersWidthSizeMode := AutoSizeToDisplayedHeaders;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".RowDeadersWidthSizeMode "
                            & "AutoSizeToDisplayedHeaders");
                  elsif Contains (Line (Idx2 + 1 .. Len),
                                  "AutoSizeToFirstHeader")
                  then
                     WT.RowHeadersWidthSizeMode := AutoSizeToFirstHeader;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".RowDeadersWidthSizeMode "
                            & "AutoSizeToFirstHeader");
                  else
                     raise TIO.Data_Error;
                  end if;

               when Attr_ImageList | Attr_DefaultNodeImage =>
                  Debug (NLin, "Set Widget Property "
                         & WT.Name.all
                         & ".ImageList null");

               when Attr_ColumnHeadersHeightSizeMode =>
                  if Contains (Line (Idx1 .. Len), "AutoSize") then
                     WT.ColumnHeadersHeightSizeMode := AutomaticSize;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".ColumnHeadersHeightSizeMode"
                            & " AutoSize");
                  elsif Contains (Line (Idx1 .. Len), "EnableResizing") then
                     WT.ColumnHeadersHeightSizeMode := EnableResizing;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".ColumnHeadersHeightSizeMode"
                            & " EnableResizing");
                  elsif Contains (Line (Idx1 .. Len), "DisableResizing") then
                     WT.ColumnHeadersHeightSizeMode := DisableResizing;
                     Debug (NLin, "Set Widget Property "
                            & WT.Name.all & ".ColumnHeadersHeightSizeMode"
                            & " DisableResizing");
                  else
                     raise TIO.Data_Error;
                  end if;

               when Attr_AddNewItem | Attr_CountItem
                  | Attr_DeleteItem | Attr_MoveFirstItem
                  | Attr_MoveLastItem | Attr_MoveNextItem
                  | Attr_MovePreviousItem | Attr_PositionItem
                  =>
                  null;

               when Attr_Ignored =>
                  Debug (NLin, Resx_File_Name
                         & ".Designer.vb (2)"
                         & ": ignored property: "
                         & Line (Idx0 .. Len));

               when No_Attribute =>
                  TIO.Put_Line (Resx_File_Name
                                & ".Designer.vb (2)"
                                & ": Line" & NLin'Image
                                & ": Designer: unknown property: "
                                & Line (Idx0 .. Len));

               when others  =>
                  TIO.Put_Line (Resx_File_Name
                                & ".Designer.vb (2)"
                                & ": Line" & NLin'Image
                                & ": Designer: property not processed: "
                                & Line (Idx0 .. Len));

            end case;
            << Continue_Loop >>
         end loop;
         return 0;
      end Process_Widget;

   begin --  Parse2_Designer_File
      TIO.Open (File => DFile,
                Mode => TIO.In_File,
                Name => Resx_Path & "/" & Resx_File_Name & ".Designer.vb");

      NLin := 0;
      Debug (NLin, "");
      Debug (NLin, "Parsing (2) Designer: Attributes");
      Ret := 0;
      while not TIO.End_Of_File (DFile) loop
         Get_Line;
         Idx0 := Index (Line (1 .. Len), "Me.SuspendLayout()");
         if Idx0 in Line'Range then
            exit when TIO.End_Of_File (DFile);
            Get_Line;
            loop
               Ret := Process_Widget;
               exit when Ret < 0;
               exit when Len <= 0;
               exit when TIO.End_Of_File (DFile);
            end loop;
         end if;
      end loop;

      Debug (0, "End of Designer 2");
      TIO.Close (DFile);
      return Ret;
   exception
      when Constraint_Error =>
         TIO.Close (DFile);
         TIO.Put_Line ("Constraint Error");
         TIO.Put_Line (Resx_File_Name & ".Designer.vb (2)"
                       & ": Line" & NLin'Image
                       & " " & Line (1 .. Len));
         return -1;
   end Parse2_Designer_File;

   function Parse1_Designer_File (Resx_Path      : String;
                                  Resx_File_Name : String) return Integer is
      Idx0      : Integer;
      Idx1      : Integer;
      Idx2      : Integer;
      Idx3      : Integer;
      Found     : Boolean;
      Num       : Integer;
      Last      : Integer;
      Mark_Line : Integer;
      TWin      : Window_Pointer;
      WT        : Widget_Pointer;

      package IIO renames Ada.Integer_Text_IO;
      procedure Get_Line;
      procedure Get_Line is
      begin
         TIO.Get_Line (DFile, Line, Len);
         if Line (Len) = ASCII.CR then
            Len := Len - 1;
         end if;
         NLin := NLin + 1;
      end Get_Line;

   begin
      TIO.Open (File => DFile,
                Mode => TIO.In_File,
                Name => Resx_Path & "/" & Resx_File_Name & ".Designer.vb");

      NLin := 0;

      --  skip first line
      Get_Line;

      Debug (NLin, "Parsing (1) Designer: " & Test8);
      Found := False;
      while not TIO.End_Of_File (DFile) loop
         Get_Line;
         Idx0 := Index (Line (1 .. Len), Test8);
         if Idx0 in 1 .. Len then
            TWin := new Window_Properties (GtkWindow);
            Found := True;
            TWin.Name := new String'(+Trim (Line (Idx0 + 14 .. Len),
                                     Ada.Strings.Both));
            Insert_Window_By_Tail (TWin);
            Debug (NLin, "Created GtkWindow " & TWin.Name.all);
            Debug (NLin, "Set Window Property Name " & TWin.Name.all);
            exit;
         end if;
      end loop;
      if not Found then
         TIO.Put_Line (Resx_File_Name & ".Designer.vb (1)"
                       & """" & Test8 & """" & ": not found");
         TIO.Close (DFile);
         return -1;
      end if;

      Debug (NLin, "Parsing (1) Designer: " & Test13);
      Found := False;
      while not TIO.End_Of_File (DFile) loop
         Get_Line;
         Idx0 := Index (Line (1 .. Len), Test13);
         if Idx0 in 1 .. Len then
            Found := True;
            exit;
         end if;
      end loop;
      if not Found then
         TIO.Put_Line (Resx_File_Name & ".Designer.vb (1)"
                       & """" & Test13 & """" & ": not found");
         TIO.Close (DFile);
         return -1;
      end if;

      Debug (NLin, "Parsing (1) Designer: " & Test7);
      Get_Line;
      Idx0 := Index (Line (1 .. Len), Test7);
      if Idx0 in 1 .. Len then
         Debug (NLin, "Parsing (1) Designer: " & Test18);
         Get_Line;
         Idx0 := Index (Line (1 .. Len), Test18);
         if Idx0 in 1 .. Len then
            Get_Line;
         end if;
      end if;

      Debug (NLin, "Parsing (1) Designer: " & Test19);
      Found := False;
      Mark_Line := NLin;
      loop
         Idx0 := Index (Line (1 .. Len), Test19);
         if Idx0 not in 1 .. Len - 1 then
            exit;
         end if;
         Idx0 := Idx0 + Test19'Length;
         IIO.Get (Line (Idx0 .. Len), Num, Last);
         Max_DGVS := Integer'Max (Max_DGVS, Num);
         exit when TIO.End_Of_File (DFile);
         Get_Line;
      end loop;

      if Max_DGVS > 0 then
         DGVS := new DGVS_Array (1 .. Max_DGVS);
         TIO.Close (DFile);
         TIO.Open (File => DFile,
                   Mode => TIO.In_File,
                   Name => Resx_Path & "/" & Resx_File_Name & ".Designer.vb");
         for I in 1 .. Mark_Line loop
            TIO.Get_Line (DFile, Line, Len);
         end loop;
         NLin  := Mark_Line;
         loop
            Idx0 := Index (Line (1 .. Len), Test19);
            if Idx0 not in 1 .. Len - 1 then
               exit;
            end if;
            Idx0 := Idx0 + Test19'Length;
            IIO.Get (Line (Idx0 .. Len), Num, Last);
            DGVS (Num).Num := Num;
            Debug (NLin, "Created DataGridViewCellStyle"
                   & DGVS (Num).Num'Image);
            exit when TIO.End_Of_File (DFile);
            Get_Line;
         end loop;
      end if;

      Found := False;
      loop
         Idx0 := Index (Line (1 .. Len), "Me.");
         if Idx0 in 1 .. Len then
            Found := True;
            exit;
         end if;
         exit when TIO.End_Of_File (DFile);
         Get_Line;
      end loop;
      if not Found then
         TIO.Close (DFile);
         TIO.Put_Line (Resx_File_Name & ".Designer.vb (1)"
                       & ": Me.<widget> not found");
         return -1;
      end if;

      Debug (NLin, "Parsing Designer (1): widgets");
      loop
         Idx0 := Index (Line (1 .. Len), "Me.");
         if Idx0 not in 1 .. Len then
            TIO.Put_Line (Line (1 .. Len));
            exit;
         end if;
         Idx0 := Idx0 + 3; --  skip "Me."
         Idx1 := Index (Line (Idx0 .. Len), " = New ");
         if Idx1 in Idx0 .. Len then
            Idx1 := Idx1 - 1;
         else
            exit;
         end if;
         Idx2 := Index (Line (Idx1 + 8 .. Len), "(");
         if Idx2 not in Idx1 + 8 .. Len then
            exit;
         end if;
         Len := Idx2 - 1;
         Idx2 := Index (Line (Idx1 + 8 .. Len), ".", Ada.Strings.Backward);
         if Idx2 not in Idx1 + 8 .. Len then
            exit;
         end if;
         Idx2 := Idx2 + 1; --  skip "."
         Idx3 := Len;  --  don't count final ()
         WT := Find_Widget (TWin.Widget_List, +Line (Idx2 .. Idx3));
         if WT /= null then
            TIO.Close (DFile);
            TIO.Put_Line (Resx_File_Name & ".Designer.vb (1): "
                          & " Line" & NLin'Image
                          & ": Repeated Widget " & Line (Idx2 .. Idx3));
            return -1;
         end if;
         WT := new Widget_Properties
           (Widget_Type => Symbol_Tables.Get_Type (Line (Idx2 .. Idx3)));
         if WT.Widget_Type = No_Widget then
            TIO.Close (DFile);
            TIO.Put_Line (Resx_File_Name & ".Designer.vb (1): "
                          & " Line" & NLin'Image
                          & ": unknown Widget " & Line (Idx2 .. Idx3));
            TIO.Put_Line (Line (1 .. Len));
            return -1;
         end if;

         WT.Name := new String'(+Line (Idx0 .. Idx1));
         if WT.Widget_Type = BindingNavigator then
            WT.Windows_Type :=
              new String'("System.Windows.Forms.BindingNavigator");
         elsif Starts_With (WT.Name.all, "Bindingnavigator")
           and then
             WT.Widget_Type = GtkButton
         then
            WT.Windows_Type :=
              new String'("System.Windows.Forms.ToolStripButton");
         elsif WT.Widget_Type = GtkBox then
            WT.Windows_Type :=
              new String'("System.Windows.Forms.FlowLayoutPanel");
         elsif WT.Widget_Type = GtkToolBar then
            WT.Windows_Type :=
              new String'("System.Windows.Forms.ToolStrip");
         end if;
         Insert_Widget_By_Tail (TWin, WT);
         Debug (NLin, "Created "
                & WT.Widget_Type'Image & " "
                & WT.Name.all & " from"
                & Line (Idx1 + 7 .. Idx3));

         exit when TIO.End_Of_File (DFile);
         TIO.Get_Line (DFile, Line, Len);
         if Line (Len) = ASCII.CR then
            Len := Len - 1;
         end if;
         NLin := NLin + 1;
      end loop;

      while not TIO.End_Of_File (DFile) loop
         TIO.Get_Line (DFile, Line, Len);
         if Line (Len) = ASCII.CR then
            Len := Len - 1;
         end if;
         NLin := NLin + 1;

         if Contains (Line (1 .. Len), "FormBorderStyle.FixedToolWindow") then
            TWin.Resizable := False;
            TWin.Modal     := True;
            exit;
         elsif Contains (Line (1 .. Len), "AcceptButton") then
            Idx0 :=  Index (Line (1 .. Len), " = Me.");
            if Idx0 not in 1 .. Len then
               raise TIO.Data_Error;
            end if;
            Idx0 := Idx0 + 6;
            WT := Find_Widget (TWin.Widget_List, +Line (Idx0 .. Len));
            if WT = null then
               raise TIO.Data_Error;
            end if;
            if WT.Widget_Type /= GtkButton then
               raise TIO.Data_Error;
            end if;
            TWin.Accept_Button := WT;
            WT.Dialog_Result := OK;
            WT.Text := new String'("Accept");
         elsif Contains (Line (1 .. Len), "CancelButton") then
            Idx0 :=  Index (Line (1 .. Len), " = Me.");
            if Idx0 not in 1 .. Len then
               raise TIO.Data_Error;
            end if;
            Idx0 := Idx0 + 6;
            WT := Find_Widget (TWin.Widget_List, +Line (Idx0 .. Len));
            if WT = null then
               raise TIO.Data_Error;
            end if;
            if WT.Widget_Type /= GtkButton then
               raise TIO.Data_Error;
            end if;
            TWin.Cancel_Button := WT;
            WT.Dialog_Result := Cancel;
            WT.Text := new String'("Cancel");
         end if;

      end loop;

      Debug (0, "End of Parsing (1) Designer");
      TIO.Close (DFile);

      return 0;
   exception
      when Constraint_Error =>
         TIO.Close (DFile);
         TIO.Put_Line ("Constraint Error");
         TIO.Put_Line (Resx_File_Name & ".Designer.vb (1)"
                       & ": Line" & NLin'Image
                       & " " & Line (1 .. Len));
         return -1;
   end Parse1_Designer_File;

   function Parse_VB_File (Resx_Path      : String;
                           Resx_File_Name : String) return Integer is
      WT : Widget_Pointer;
      Line0 : String (1 .. 1024); --  prev line
      Len0  : Integer := 0;
      Line1 : String (1 .. 1024); --  prev prev line
      Len1  : Integer := 0;
      Line2 : String (1 .. 1024); --  prev prev prev line
      Len2  : Integer := 0;
      Line3 : String (1 .. 1024); --  prev prev prev prev line
      Len3  : Integer := 0;

      Idx0  : Integer;
      Idx1  : Integer;

      procedure Get_Line;
      procedure Get_Line is
      begin
         Len3 := Len2;
         Line3 (1 .. Len3) := Line2 (1 .. Len2);

         Len2 := Len1;
         Line2 (1 .. Len2) := Line1 (1 .. Len1);

         Len1 := Len0;
         Line1 (1 .. Len1) := Line0 (1 .. Len0);

         Len0 := Len;
         Line0 (1 .. Len0) := Line (1 .. Len);

         TIO.Get_Line (VFile, Line, Len);
         NLin := NLin + 1;
         if Line (Len) = ASCII.CR then
            Len := Len - 1;
         end if;
      end Get_Line;

      function Get_Handler return String;
      function Get_Handler return String is
      begin
         Idx1 := Index (Line (1 .. Idx0 - 1), "Sub ");
         if Idx1 not in 1 .. Idx0 - 1 then
            Idx1 := Index (Line0 (1 .. Len0), "Sub ");
            if Idx1 not in 1 .. Len0 then
               Idx1 := Index (Line1 (1 .. Len1), "Sub ");
               if Idx1 not in 1 .. Len1 then
                  Idx1 := Index (Line2 (1 .. Len2), "Sub ");
                  if Idx1 not in 1 .. Len2 then
                     Idx1 := Index (Line3 (1 .. Len3), "Sub ");
                     if Idx1 not in 1 .. Len3 then
                        TIO.Put_Line (Resx_File_Name & ".vb"
                                      & ": Line: " & Img (NLin)
                                      & ": could not find handler");
                        return "";
                     else
                        Len :=  Len3;
                        Line (1 .. Len) := Line3 (1 .. Len3);
                     end if;
                  else
                     Len :=  Len2;
                     Line (1 .. Len) := Line2 (1 .. Len2);
                  end if;
               else
                  Len :=  Len1;
                  Line (1 .. Len) := Line1 (1 .. Len1);
               end if;
            else
               Len := Len0;
               Line (1 .. Len) := Line0 (1 .. Len0);
            end if;
         end if;
         Idx0 := Idx1 + 4;
         Idx1 := Index (Line (Idx0 .. Len), "(");
         if Idx1 not in Idx0 .. Len then
            Idx1 := Index (Line (Idx0 .. Len), " _");
            if Idx1 not in Idx0 .. Len then
               TIO.Put_Line (Resx_File_Name
                             & ": Line: " & Img (NLin)
                             & ": could not find handler");
               return "";
            end if;
         end if;
         return Line (Idx0 .. Idx1 - 1);
      end Get_Handler;

      function Parse_Handler (Complete_Signal : String) return Integer;
      function Parse_Handler (Complete_Signal : String) return Integer is
         Idx1 : Integer;
         Ret  : Integer := 0;
      begin
         Idx1 := Index (Complete_Signal, ".");
         if Idx1 not in
           Complete_Signal'First + 1 .. Complete_Signal'Last - 1
         then
            TIO.Put_Line (Resx_File_Name & ".vb"
                          & ": Line: " & Img (NLin)
                          & ": cannot parse signal. Line: "
                          & Complete_Signal);
            return -1;
         end if;
         declare
            WName : constant String :=
              Complete_Signal (Complete_Signal'First .. Idx1 - 1);
            SName : constant String :=
              Complete_Signal (Idx1 + 1 .. Complete_Signal'Last);
            WS : Signal_Pointer;
         begin
            if WName = "MyBase" or WName = "Me" then
               WS := new Signal_Block;
               if SName (SName'Last) = ',' then
                  WS.Name := new String'(SName (SName'First ..
                                                SName'Last - 1));
                  Ret := 1;
               else
                  WS.Name := new String'(SName);
               end if;
               declare
                  Handler : constant String := +Get_Handler;
               begin
                  if Handler = "" then
                     return -1;
                  elsif Starts_With (Handler, "On_") then
                     WS.Handler := new String'(Handler);
                  else
                     WS.Handler := new String'("On_" & Handler);
                  end if;
               end;
               WS.Line := NLin;
               Insert_Signal (Win_List, WS);
               if Ret = 1 then
                  Debug (NLin, "Created Signal "
                         & Complete_Signal (Complete_Signal'First ..
                             Complete_Signal'Last - 1));
               else
                  Debug (NLin, "Created Signal " &
                           Win_List.Name.all & "." & SName);
               end if;
            else
               WT := Find_Widget (Win_List.Widget_List, +WName);
               if WT = null then
                  TIO.Put_Line (Resx_File_Name & ".vb"
                                & ": Line" & NLin'Image
                                & ": cannot find widget in signal name");
                  return -1;
               end if;
               WS := new Signal_Block;
               if SName (SName'Last) = ',' then
                  WS.Name := new String'(SName (SName'First ..
                                                SName'Last - 1));
                  Ret := 1;
               else
                  WS.Name := new String'(SName);
               end if;
               declare
                  Handler : constant String := +Get_Handler;
               begin
                  if Handler = "" then
                     return -1;
                  elsif Starts_With (Handler, "Tgv_") then
                     WS.Handler :=
                       new String'("On_" & (+WName) & "_"
                                   & Handler
                                     (Handler'First + 4 .. Handler'Last));
                  elsif Starts_With (Handler, "Dgv_") then
                     WS.Handler :=
                       new String'("On_" & (+WName) & "_"
                                   & Handler
                                     (Handler'First + 4 .. Handler'Last));
                  elsif Starts_With (Handler, "On_") then
                     WS.Handler := new String'(Handler);
                  else
                     WS.Handler := new String'("On_" & Handler);
                  end if;
               end;
               WS.Line := NLin;
               if WT.Widget_Type = GtkNoteBook
                 and then WS.Name.all = "Selected"
               then
                  WS.After := True;
               end if;
               Insert_Signal (WT, WS);
               if Ret = 1 then
                  Debug (NLin, "Created Signal "
                         & Complete_Signal (Complete_Signal'First ..
                             Complete_Signal'Last - 1));
               else
                  Debug (NLin, "Created Signal " & Complete_Signal);
               end if;
            end if;
         end;
         return Ret;
      end Parse_Handler;

   begin
      TIO.Open (File => VFile,
                Mode => TIO.In_File,
                Name => Resx_Path & "/" & Resx_File_Name & ".vb");

      NLin := 0;

      Debug (NLin, "");
      Debug (NLin, "Parsing vb file");
      while not TIO.End_Of_File (VFile) loop
         Get_Line;
         Idx0 := Index (Line (1 .. Len), Test11);
         if Idx0 in 1 .. Len then
            Idx0 := Idx0 + Test11'Length;
            if Trim (Line (Idx0 .. Len), Ada.Strings.Both) = "" then
               exit when TIO.End_Of_File (VFile);
               Get_Line;
               Idx0 := 1;
            end if;
            if Trim (Line (Idx0 .. Len), Ada.Strings.Both) = "_" then
               exit when TIO.End_Of_File (VFile);
               Get_Line;
               Idx0 := 1;
            end if;
            declare
               Ret  : Integer;
               More : Boolean;
            begin
               Ret := Parse_Handler (Trim (Line (Idx0 .. Len),
                                     Ada.Strings.Both));
               if Ret < 0 then
                  return Ret;
               end if;
               More := (Ret = 1);
               while More loop
                  exit when TIO.End_Of_File (VFile);
                  Get_Line;
                  Ret := Parse_Handler (Trim (Line (1 .. Len),
                                        Ada.Strings.Both));
                  if Ret < 0 then
                     TIO.Close (VFile);
                     return Ret;
                  end if;
                  More := (Ret = 1);
               end loop;

               --  continue reading to end sub to locate color.showdialog
               while not TIO.End_Of_File (VFile) loop
                  Get_Line;
                  Idx0 := Index (Line (1 .. Len), Test12);
                  if Idx0 in Line'Range then
                     Idx1 := Index (Source  => Line (1 .. Idx0 - 1),
                                    Pattern => " = ",
                                    Going   => Ada.Strings.Backward);
                     if Idx1 in 1 .. Idx0 - 1 then
                        declare
                           WName : constant String :=
                             +Line (Idx1 + 3 .. Idx0 - 1);
                           WTCD : Widget_Pointer;
                        begin
                           WTCD := Find_Widget (Win_List.Widget_List, WName);
                           if WTCD = null then
                              TIO.Put_Line ("Warning: "
                                            & Resx_File_Name & ".vb"
                                            & ": Line" & NLin'Image
                                            & ": cannot find widget "
                                            & WName & "." & "ShowDialog()."
                                            & " Could be an external widget.");
                           else
                              if WTCD.Widget_Type = GtkColorButton then
                                 WT.Associated_ColorButton := WTCD;
                                 WTCD.Associated_Button := WT;
                                 WTCD.Location    := WT.Location;
                                 WTCD.Size        := WT.Size;
                                 WTCD.TabIndex    := WT.TabIndex;
                                 WTCD.TabStop     := WT.TabStop;
                                 WTCD.Zorder      := WT.Zorder;
                                 WTCD.Enabled     := WT.Enabled;
                                 WTCD.Visible     := WT.Visible;

                                 WTCD.TextAlign   := WT.TextAlign;
                                 WTCD.Margins     := WT.Margins;
                                 WTCD.Text        := WT.Text;
                                 WTCD.ToolTip     := WT.ToolTip;
                                 WTCD.Signal_List := WT.Signal_List;
                                 exit;
                              end if;
                           end if;
                        exception
                           when others =>
                              TIO.Close (VFile);
                              TIO.Put_Line (Resx_File_Name & ".vb"
                                            & ": Line" & NLin'Image
                                            & ": error finding widget in "
                                            & "ShowDialog()");
                              return -1;
                        end;
                     end if;
                  else
                     Idx0 := Index (Line (1 .. Len), "End Sub");
                     exit when Idx0 in Line'Range;
                  end if;
               end loop;
            end;
         end if;
      end loop;

      Debug (0, "End of Parsing vb file");
      TIO.Close (VFile);
      return 0;
   end Parse_VB_File;

   function Parse_VS_File (Use_Debug      : Boolean;
                           Do_Dump        : Boolean;
                           Resx_Path      : String;
                           Resx_File_Name : String;
                           Glade_Path     : String;
                           Icon_Path      : String) return Integer is
      Result : Integer;
   begin
      W2gtk_Decls.Use_Debug := Use_Debug;

      Result := Parse1_Designer_File (Resx_Path, Resx_File_Name);
      if Result /= 0 then
         return Result;
      end if;

      Result := Parse_Resource_File (Win_List, Resx_Path, Resx_File_Name);
      if Result /= 0 then
         return Result;
      end if;

      Result := Parse2_Designer_File (Win_List,
                                      Resx_Path, Resx_File_Name,
                                      Icon_Path);
      if Result /= 0 then
         return Result;
      end if;

      Result := Parse_VB_File (Resx_Path, Resx_File_Name);
      if Result /= 0 then
         return Result;
      end if;

      Result := Adjust_To_Gtk;
      if Result /= 0 then
         return Result;
      end if;

      if Do_Dump then
         Dump (Glade_Path, Resx_File_Name);
      end if;

      return Result;
   end Parse_VS_File;

   function Generate_Glade_File (Glade_Path      : String;
                                 Glade_File_Name : String) return Integer is
      TWin  : Window_Pointer;
   begin
      Debug (0, "Generating Glade");

      if Win_List = null then
         TIO.Put_Line ("No window information");
         return -1;
      end if;
      TIO.Create (File => GFile,
                  Mode => TIO.Out_File,
                  Name => Glade_Path & "/" & Glade_File_Name & ".glade");
      Emit_GtkHeader (null, 0);
      TWin := Win_List;
      while TWin /= null loop
         case TWin.Window_Type is
            when GtkWindow =>
               Emit_GtkWindow (TWin, 2);
            when GtkFileFilter =>
               Emit_GtkFileFilter (TWin, 2);
            when GtkEntryBuffer =>
               Emit_GtkEntryBuffer (TWin, 2);
            when GtkFileChooserDialog =>
               Emit_GtkFileChooserDialog (TWin, 2);
            when GtkListStore =>
               Emit_GtkListStore (TWin, 2);
            when GtkImage =>
               Emit_GtkImage (TWin, 2);
            when GtkTreeStore =>
               Emit_GtkTreeStore (TWin, 2);
            when GtkModelFilter =>
               Emit_GtkModelFilter (TWin, 2);
            when GtkModelSort =>
               Emit_GtkModelSort (TWin, 2);
         end case;
         TWin := TWin.Next;
      end loop;
      Emit_GtkTrailer (null, 0);

      TIO.Close (GFile);
      return 0;
   end Generate_Glade_File;

end W2gtk_Pkg;
