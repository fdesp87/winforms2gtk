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
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Symbol_Tables;

package body W2gtk_Decls is
   package ITIO renames Ada.Integer_Text_IO;
   package FTIO renames Ada.Float_Text_IO;

   Test3  : constant String := "<value>";
   Test4  : constant String := "</value>";
   Test6  : constant String := "<value />";

   -------------------
   -- id_fcontainer --
   -------------------

   function Is_Container (TWdg : Widget_Pointer) return Boolean is
   begin
      if TWdg = null then
         return True; --  this impedes the generation of a gtkfixed
      end if;
      case TWdg.Widget_Type is
         when GtkBox | GtkButtonBox | GtkFixed | GtkFrame
            | GtkNoteBook | GtkScrolledWindow | GtkStatusBar
            | GtkToolBar | GtkTreeGridView | Internal_Child_VBox =>
            return True;
         when others => return False;
      end case;
   end Is_Container;

   --------
   -- sp --
   --------

   function Sp (N : Integer) return String is
   begin
      if N > 0 then
         return (1 .. N => ' ');
      else
         return "";
      end if;
   end Sp;

   -----------
   -- Debug --
   -----------

   procedure Debug (NLin : Integer; Msg : String) is
   begin
      if Log then
         if NLin >= 0 then
            ITIO.Put (Log_File, NLin, Width => 5);
            TIO.Put (Log_File, ": ");
         end if;
         TIO.Put_Line (Log_File, Msg);
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

   function Img (X : Float; Exp : Integer) return String is
      Str : String (1 .. 20) := (others => ' ');
      I   : constant Integer := Integer (X);
   begin
      if Float (I) = X then
         return Img (I);
      end if;
      FTIO.Put (Str, X, Aft => 0, Exp => Exp);
      return Ada.Strings.Fixed.Trim (Str, Ada.Strings.Both);
   end Img;

   -------------
   -- Convert --
   -------------

   function Convert (WStyle : String) return Display_Style_Enum is
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

   -------------------------
   -- To_Cell_Format_Enum --
   -------------------------

   function To_Cell_Format_Enum (Str : String) return Cell_Format_Enum is
   begin
      if Str = "" then
         return Format_String;
      end if;
      if To_Lower (Str (Str'First + 1)) = 'r' then    --  integer
         return Format_Integer;
      elsif To_Lower (Str (Str'First + 1)) = 'n' then --  real
         return Format_Real;
      elsif To_Lower (Str (Str'First + 1)) = 'p' then --  percent
         return Format_Percent;
      elsif To_Lower (Str (Str'First + 1)) = 'd' then --  date
         return Format_Date;
      elsif To_Lower (Str (Str'First + 1)) = 'c' then --  currency
         return Format_Currency;
      elsif To_Lower (Str (Str'First + 1)) = 'e' then --  exponential
         return Format_Exponential;
      elsif To_Lower (Str (Str'First + 1)) = 'f' then --  fixed point decimal
         return Format_Decimal;
      elsif To_Lower (Str (Str'First + 1)) = 'g' then --  date
         return Format_String;
      elsif To_Lower (Str (Str'First + 1)) = '0' then --  integer
         return Format_Integer;
      elsif To_Lower (Str (Str'First + 1)) = 'b' then --  boolean
         return Format_Boolean;
      else
         return Format_String;
      end if;
   end To_Cell_Format_Enum;

   ------------------
   -- To TextAlign --
   ------------------

   function To_TextAlign (IAlign : String) return TextAlign_Enum is
   begin
      if Contains (IAlign, "BottomCenter") then
         return BottomCenter;
      elsif Contains (IAlign, "BottomLeft") then
         return BottomLeft;
      elsif Contains (IAlign, "BottomRight") then
         return BottomRight;
      elsif Contains (IAlign, "MiddleCenter") then
         return MiddleCenter;
      elsif Contains (IAlign, "MiddleLeft") then
         return MiddleLeft;
      elsif Contains (IAlign, "MiddleRight") then
         return MiddleRight;
      elsif Contains (IAlign, "TopCenter") then
         return TopCenter;
      elsif Contains (IAlign, "TopLeft") then
         return TopLeft;
      elsif Contains (IAlign, "TopRight") then
         return TopRight;
      elsif Contains (IAlign, "Left") then
         return Left;
      elsif Contains (IAlign, "Right") then
         return Right;
      elsif Contains (IAlign, "Top") then
         return Top;
      elsif Contains (IAlign, "Bottom") then
         return Bottom;
      else
         return Center;
      end if;
   end To_TextAlign;

   ---------------------------
   -- To_AutoSizeColumnMode --
   ---------------------------

   function To_AutoSizeColumnMode (WLine : String)
                                    return AutoSizeColumnMode_Enum is
      Idx0 : Integer;
   begin
      Idx0 := Index (Source  => WLine,
                     Pattern => ".",
                     Going   => Ada.Strings.Backward);
      if Idx0 not in WLine'Range then
         raise TIO.Data_Error;
      end if;

      declare
         WMode : constant String := WLine (Idx0 + 1 .. WLine'Last);
      begin
         if WMode = "NotSet" then
            return NotSet;
         elsif WMode = "None" then
            return None;
         elsif WMode = "ColumnHeader" then
            return ColumnHeader;
         elsif WMode = "AllCellsExceptHeader" then
            return AllCellsExceptHeader;
         elsif WMode = "AllCells" then
            return AllCells;
         elsif WMode = "DisplayedCellsExceptHeader" then
            return DisplayedCellsExceptHeader;
         elsif WMode = "DisplayedCells" then
            return DisplayedCells;
         elsif WMode = "Fill" then
            return Fill;
         else
            raise TIO.Data_Error;
         end if;
      end;
   end To_AutoSizeColumnMode;

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
         elsif WColor (Idx0 + 1 .. WColor'Last) = "Highlight" then
            return "DodgerBlue1";  --  "white"; --  3399FF
         elsif WColor (Idx0 + 1 .. WColor'Last) = "Window" then
            return "grey81";
         elsif WColor (Idx0 + 1 .. WColor'Last) = "ButtonFace" then
            return "grey81";
         elsif WColor (Idx0 + 1 .. WColor'Last) = "ControlText" then
            return "black";
         elsif WColor (Idx0 + 1 .. WColor'Last) = "WindowText" then
            return "black";
         elsif WColor (Idx0 + 1 .. WColor'Last) = "HighlightText" then
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

   ----------------
   -- New_String --
   ----------------
   function New_String (K : String_Access) return String_Access is
   begin
      if K /= null then
         return new String'(K.all);
      else
         return null;
      end if;
   end New_String;

   ----------------------------
   -- Copy_Common_Attributes --
   ----------------------------

   procedure Copy_Common_Attributes (From : Widget_Pointer;
                                     To   : Widget_Pointer) is
   begin
      To.Next := From.Next;
      To.Prev := From.Next;
      To.Child_List := From.Child_List;
      To.Num_Children := From.Num_Children;

      To.Parent_Name  := New_String (From.Parent_Name);
      To.Win_Parent      := From.Win_Parent;
      To.Wdg_Parent      := From.Wdg_Parent;
      To.Child_Number := From.Child_Number;

      To.Windows_Type := New_String (From.Windows_Type);

      To.Name := New_String (From.Name);

      To.Location := From.Location;
      To.Size := From.Size;
      To.TabIndex := From.TabIndex;
      To.TabStop := From.TabStop;
      To.Has_Focus := From.Has_Focus;
      To.Next_Focus := From.Next_Focus;
      To.Prev_Focus := From.Prev_Focus;
      To.Zorder := From.Zorder;

      To.Enabled := From.Enabled;
      To.Visible := From.Visible;
      To.Text := New_String (From.Text);
      To.TextAlign := From.TextAlign;
      To.AutoSize := From.AutoSize;
      To.AutoSizeMode := From.AutoSizeMode;
      To.Font_Name := New_String (From.Font_Name);
      To.Font_Size := From.Font_Size;
      To.Font_Weight := New_String (From.Font_Weight);
      To.Margins := From.Margins;
      To.DStyle := From.DStyle;
      To.MaxLength := From.MaxLength;
      To.AutoToolTip := From.AutoToolTip;
      To.ToolTip := New_String (From.ToolTip);
      To.BgColor := New_String (From.BgColor);
      To.FgColor := New_String (From.FgColor);
      To.UlColor := New_String (From.UlColor);
      To.FlowDirection := From.FlowDirection;

      To.Signal_List   := From.Signal_List;
      From.Signal_List := null;

      To.Child_List   := From.Child_List;
      From.Child_List := null;

   end Copy_Common_Attributes;

   -------------
   -- Release --
   -------------

   procedure Release (WT : in out Widget_Pointer) is
   begin
      if WT = null then
         return;
      end if;
      if WT.Next /= null
        or else WT.Prev /= null
        or else WT.Child_List /= null
        or else WT.Signal_List /= null
      then
         raise Program_Error;
      end if;

      Free (WT.Parent_Name);
      Free (WT.Name);
      Free (WT.Text);
      Free (WT.Font_Name);
      Free (WT.Font_Weight);
      Free (WT.ToolTip);
      Free (WT.BgColor);
      Free (WT.FgColor);
      Free (WT.UlColor);

      case WT.Widget_Type is
         when No_Widget =>
            null;

         when GtkAlignment => null;
         when GtkAspectFrame => null;
         when GtkScrolledWindow => null;


         when GtkMenuItem | GtkSubMenu => null;
         when GtkSeparatorMenuItem => null;

         when GtkMenuNormalItem | GtkMenuImageItem
            | GtkMenuRadioItem | GtkMenuCheckItem
            =>
            Free (WT.ImageMenu);

         when GtkDataGridView | GtkTreeGridView
            | ExpandableColumn | DataGridViewTextBoxColumn
            | DataGridViewCheckBoxColumn =>

            case WT.Widget_Type is
               when GtkDataGridView | GtkTreeGridView =>
                  null;

               when ExpandableColumn | DataGridViewTextBoxColumn
                  | DataGridViewCheckBoxColumn
                  =>
                  null;

                  case WT.Widget_Type is
                     when DataGridViewCheckBoxColumn =>
                        null;
                     when others => null;
                  end case;

               when others => null;
            end case;

         when GtkNoteBook =>
            null;

         when GtkTabChild =>
            null;

         when GtkTabPage =>
            Free (WT.The_Label);
            Free (WT.The_Button);

         when GtkEntry | GtkComboTextBox | GtkCalendar =>
            Free (WT.Text_Buffer);
            case WT.Widget_Type is
               when GtkEntry | GtkComboTextBox =>
                  Free (WT.PasswordChar);
                  case WT.Widget_Type is
                     when GtkComboTextBox =>
                        null;
                     when others => null;
                  end case;
               when GtkCalendar =>
                  null;
               when others => null;
            end case;

         when GtkSpinButton =>
            null;

         when GtkFileChooserButton
            | PrintDocument | PrintDialog | PageSetupDialog
            | FolderBrowserDialog | GtkToolTip | GtkColorButton
            | GtkStatusBar | GtkToolBar | GtkMenuBar | BackgroundWorker
            | BindingNavigator =>

            case WT.Widget_Type is
               when GtkFileChooserButton =>
                  Free (WT.OpenFileDialog);
                  Free (WT.OpenFileFilter);
                  Free (WT.OpenFileTitle);

               when GtkColorButton =>
                  null;

               when GtkToolBar | GtkMenuBar | BindingNavigator =>
                  case WT.Widget_Type is
                     when GtkToolBar | BindingNavigator =>
                        case WT.Widget_Type is
                           when BindingNavigator =>
                              null;
                           when others =>
                              null;
                        end case;
                     when others => null;
                  end case;

               when PageSetupDialog =>
                  null;

               when BackgroundWorker =>
                  null;

               when others =>
                  null;
            end case;

         when GtkListBox =>
            null;

         when GtkImage =>
            Free (WT.Image);

         when GtkButton | GtkRadioButton | GtkCheckButton | GtkToggleButton
              | GtkLabel | ToolStripStatusLabel
            =>
            case WT.Widget_Type is
               when GtkLabel | ToolStripStatusLabel =>
                  null;

               when GtkButton | GtkRadioButton
                  | GtkCheckButton | GtkToggleButton
                  =>
                  Free (WT.ImagePath);

                  case WT.Widget_Type is
                     when GtkButton =>
                        null;

                     when GtkCheckButton =>
                        Free (WT.CheckAlign);

                     when others => null;
                  end case;
               when others => null;
            end case;

         when GtkFrame =>
            null;

         when GtkBox =>
            null;

         when Chart =>
            Free (WT.Anchor);

         when GtkSeparatorToolItem => null;

         when GtkFixed  => null;
         when GtkButtonBox => null;
         when Internal_Child_VBox => null;
         when Internal_Child_Action_Area => null;
         when Action_Widgets => null;
      end case;
      Free (WT);
   end Release;

   -------------
   -- Unlink  --
   -------------

   procedure Unlink_Widget (Parent : Window_Pointer;
                            WT     : Widget_Pointer) is
   begin
      if Parent.Widget_List = WT then     --  first item
         if WT.Next = null then           --  and last item
            Parent.Widget_List := null;
         else                             --  first but more items
            WT.Next.Prev := null;
            Parent.Widget_List := WT.Next;
            Parent.Widget_List.Prev := null;
         end if;
      elsif WT.Next = null then           --  last item (cannot be first)
         WT.Prev.Next := null;
      else                                --  midle item
         WT.Prev.Next := WT.Next;
         WT.Next.Prev := WT.Prev;
      end if;
      WT.Next := null;
      WT.Prev := null;
   end Unlink_Widget;

   -------------
   -- Unlink  --
   -------------

   procedure Unlink_Widget (TWdg : Widget_Pointer;
                            WT   : Widget_Pointer) is
   begin
      if TWdg.Child_List = WT then        --  first item
         if WT.Next = null then           --  and last item
            TWdg.Child_List := null;
         else                             --  first but more items
            WT.Next.Prev := null;
            TWdg.Child_List := WT.Next;
            TWdg.Child_List.Prev := null;
         end if;
      elsif WT.Next = null then           --  last item (cannot be first)
         WT.Prev.Next := null;
      else                                --  midle item
         WT.Prev.Next := WT.Next;
         WT.Next.Prev := WT.Prev;
      end if;
      WT.Next := null;
      WT.Prev := null;
   end Unlink_Widget;

   --------------------------
   --  insert widget by order
   --------------------------

   procedure Insert_Widget_By_Order (Parent : Widget_Pointer;
                                     WT     : Widget_Pointer);
   procedure Insert_Widget_By_Order (Parent : Widget_Pointer;
                                     WT     : Widget_Pointer) is
      Temp : Widget_Pointer;
   begin
      --  if child_list is empty, just link it
      if Parent.Child_List = null then
         Parent.Child_List := WT;
         WT.Next := null;
         WT.Prev := null;
         return;
      end if;

      --  check if WT should be in front
      if Parent.Child_List.Child_Number > WT.Child_Number then
         WT.Next := Parent.Child_List;
         WT.Prev := null;
         Parent.Child_List.Prev := WT;
         Parent.Child_List := WT;
         return;
      end if;

      --  check if WT sould be inserted by the tail
      Temp := Parent.Child_List;
      loop
         exit when Temp.Next = null;
         Temp := Temp.Next;
      end loop;
      if Temp.Child_Number < WT.Child_Number then
         Temp.Next := WT;
         WT.Prev := Temp;
         WT.Next := null;
         return;
      end if;

      --  WT in the midle. Detect it
      Temp := Parent.Child_List;
      loop
         exit when Temp.Child_Number > WT.Child_Number;
         Temp := Temp.Next;
      end loop;
      WT.Next := Temp;
      WT.Prev := Temp.Prev;
      Temp.Prev.Next := WT;
      Temp.Prev := WT;
   end Insert_Widget_By_Order;

   -----------------------------
   -- Replace_Parent_By_Child --
   ------------------------------

   procedure Replace_Parent_By_Child (Parent : in out Widget_Pointer;
                                      Child  : Widget_Pointer) is
      GParent : constant Widget_Pointer := Parent.Wdg_Parent;
   begin
      Child.Wdg_Parent       := GParent;
      Child.Parent_Name   := new String'(Parent.Parent_Name.all);
      Child.FlowDirection := Parent.FlowDirection;
      Child.Child_Number  := Parent.Child_Number;

      if Parent.Wdg_Parent = null then  --  first level widget
         Unlink_Widget (Parent.Win_Parent, Parent);
      else
         Unlink_Widget (GParent, Parent);
         Insert_Widget_By_Order (GParent, Child);
      end if;
      Parent.Child_List := null;
      Release (Parent);
      Parent := Child;
   end Replace_Parent_By_Child;

   --------------------
   -- Replace widget --
   --------------------

   procedure Replace (TWin   : Window_Pointer;
                      OldWdg : Widget_Pointer;
                      NewWdg : Widget_Pointer) is
   begin
      if TWin.Widget_List = OldWdg then  --  in front
         TWin.Widget_List := NewWdg;
         if OldWdg.Next /= null then     --  more items
            OldWdg.Next.Prev := NewWdg;
         end if;
      elsif OldWdg.Next = null then      --  last (cannot be first)
         OldWdg.Prev := NewWdg;
      else
         OldWdg.Prev.Next := NewWdg;
         OldWdg.Next.Prev := NewWdg;
      end if;
      OldWdg.Next := null;
      OldWdg.Prev := null;
   end Replace;

   ----------------- --
   -- Replace widget --
   ----------------- --

   procedure Replace (Parent : Widget_Pointer;
                      OldWdg : Widget_Pointer;
                      NewWdg : Widget_Pointer) is
   begin
      if Parent.Child_List = OldWdg then  --  first
         Parent.Child_List := NewWdg;
         if OldWdg.Next /= null then      --  not last
            OldWdg.Next.Prev := NewWdg;
         end if;
      elsif OldWdg.Next = null then       --  last (cannot be first)
         OldWdg.Prev := NewWdg;
      else                                --  middle
         OldWdg.Prev.Next := NewWdg;
         OldWdg.Next.Prev := NewWdg;
      end if;
      OldWdg.Next := null;
      OldWdg.Prev := null;
   end Replace;

   --------------
   -- Set Have --
   --------------

   procedure Set_Have (WP : Window_Pointer) is
   begin
      case WP.Window_Type is
         when GtkTreeStore =>
            Have.TreeStores := Have.TreeStores + 1;
         when GtkImage =>
            Have.Images     := Have.Images + 1;
         when GtkListStore =>
            Have.ListStores := Have.ListStores + 1;
         when GtkFileFilter =>
            Have.FileFilters := Have.FileFilters + 1;
         when GtkFileChooserDialog =>
            Have.Filechooserdialogs := Have.Filechooserdialogs + 1;
         when GtkEntryBuffer =>
            Have.Entrybuffers := Have.Entrybuffers + 1;
         when others => null;
      end case;
   end Set_Have;

   procedure Set_Have (WT : Widget_Pointer) is
   begin
      case WT.Widget_Type is
         when GtkButton =>
            Have.Buttons := Have.Buttons + 1;
            if WT.ImagePath /= null or else WT.Win_Image /= null then
               Have.Images := Have.Images + 1;
            end if;
         when GtkLabel =>
            Have.Labels := Have.Labels + 1;
         when GtkMenuBar =>
            Have.Menus :=  Have.Menus + 1;
         when GtkMenuNormalItem =>
            Have.MenuNormalItems := Have.MenuNormalItems + 1;
         when GtkMenuImageItem =>
            Have.MenuImageItems := Have.MenuImageItems + 1;
            if WT.ImageMenu /= null then
               Have.Images := Have.Images + 1;
            end if;
         when GtkSeparatorMenuItem =>
            Have.MenuSeparators := Have.MenuSeparators + 1;
         when GtkToolBar =>
            Have.Toolbars := Have.Toolbars + 1;
         when GtkSeparatorToolItem =>
            Have.ToolSeparators := Have.ToolSeparators + 1;
         when GtkImage =>
            Have.Images := Have.Images + 1;
         when GtkNoteBook =>
            Have.Notebooks := Have.Notebooks + 1;
         when GtkDataGridView | GtkTreeGridView =>
            Have.TreeViews := Have.TreeViews + 1;
         when ExpandableColumn | DataGridViewTextBoxColumn =>
            Have.TreeViewColumns := Have.TreeViewColumns + 1;
         when DataGridViewCheckBoxColumn =>
            Have.TreeViewToggles := Have.TreeViewToggles + 1;
         when GtkEntry =>
            Have.Entries := Have.Entries + 1;
         when GtkComboTextBox =>
            Have.ComboTextBoxes := Have.ComboTextBoxes + 1;
         when GtkBox =>
            Have.Boxes := Have.Boxes + 1;
         when GtkFileChooserButton =>
            Have.FileChooserButtons := Have.FileChooserButtons + 1;
         when GtkToolTip =>
            Have.Tooltips := Have.Tooltips + 1;
         when GtkCalendar =>
            if WT.Is_DatePicker then
               Have.Date_Pickers := Have.Date_Pickers + 1;
               Have.Entries := Have.Entries + 3;
               Have.Buttons := Have.Buttons + 1;
            else
               Have.Time_Pickers := Have.Time_Pickers + 1;
               Have.Entries := Have.Entries + 3;
               Have.Buttons := Have.Buttons + 6;
            end if;
         when GtkRadioButton =>
            Have.Radio_Buttons := Have.Radio_Buttons + 1;
         when GtkCheckButton =>
            Have.Check_Buttons := Have.Check_Buttons + 1;
         when GtkFrame =>
            Have.Frames := Have.Frames + 1;
         when others => null;
      end case;
   end Set_Have;

   -------------------
   -- Insert_Widget --
   -------------------
   --  insert by the tail, by the order found when parsing Designer step 1
   --  except date_pickers which are inserted at the end and by reverse order
   procedure Insert_Widget_By_Tail (Parent : Window_Pointer;
                                    WT     : Widget_Pointer) is
      Temp0 : Widget_Pointer; --  the last widget in the list
      Temp1 : Widget_Pointer; --  the first date picker in the list
      Count : Integer := 0;
   begin
      if WT.Name = null or else WT.Name.all = "" then
         raise Widget_Name_Not_Assigned;
      else
         declare
            Name  : constant String := WT.Name.all;
         begin
            loop
               Temp0 := Find_Widget (Parent.Widget_List, WT.Name.all);
               exit when Temp0 = null;
               if Count = Integer'Last then
                  raise Duplicated_Widget_Name;
               end if;
               Free (WT.Name);
               Count := Count + 1;
               WT.Name := new String'(Name & Img (Count));
            end loop;
            if Count /= 0 then
               Debug (0, Sp (3) & "WARNING "
                      & "widget " & Name & " renamed to "
                      & WT.Name.all);
            end if;
         end;
      end if;

      WT.Child_Number := Parent.Num_Children;
      Parent.Num_Children := Parent.Num_Children + 1;
      WT.Win_Parent := Parent;

      --  if list if empty, insert WT
      if Parent.Widget_List = null then
         Parent.Widget_List := WT;
         WT.Next := null;
         WT.Prev := null;
         return;
      end if;

      --  set Temp0 to the last widget and set Temp1 to the first date_picker
      Temp0 := Parent.Widget_List;
      Temp1 := null;
      loop
         if Temp1 = null
           and then Temp0.Widget_Type = GtkCalendar
           --  and then Temp0.Is_DatePicker
         then
            Temp1 := Temp0;
         end if;
         exit when Temp0.Next = null;
         Temp0 := Temp0.Next;
      end loop;

      if Temp1 = null then    --  no calendars in the list
         Temp0.Next := WT;    --  insert WT by the end
         WT.Next    := null;
         WT.Prev    := Temp0;
         return;
      end if;

      --  Temp1 is a date picker
      if Temp1 = Parent.Widget_List then  --  temp1 is the first widget
         WT.Next := Temp1;                --  insert WT by the front
         WT.Prev := null;
         Temp1.Prev := WT;
         Parent.Widget_List := WT;
         return;
      end if;

      --  insert WT before Temp1
      WT.Next := Temp1;
      WT.Prev := Temp1.Prev;
      Temp1.Prev.Next := WT;
      Temp1.Prev := WT;
   end Insert_Widget_By_Tail;

   --------------------
   -- Not_Duplicated --
   --------------------

   --  ensure no duplicated Ada signal handlers
   function Not_Duplicated (TS : Signal_Pointer;
                           Ignore_Dup : Boolean) return Boolean;
   function Not_Duplicated (TS : Signal_Pointer;
                           Ignore_Dup : Boolean) return Boolean is
      B : Boolean;
   begin
      B := Symbol_Tables.Insert_In_Handler_Map (TS);
      if not B and then not Ignore_Dup then
         TS.GAda  := False;
         TS.Glade := False;
      end if;
      return B;
   end Not_Duplicated;

   -------------------
   -- Insert_Signal --
   -------------------
   --  insert in alphabetic order
   function Insert_Signal (TWin : Window_Pointer;
                           TS   : Signal_Pointer;
                           Ignore_Dup : Boolean := False) return Boolean is
      Temp, Temp_Prev : Signal_Pointer;
   begin
      TS.GtkName := new String'(Symbol_Tables.Convert_Signal_To_Gtk (TWin, TS));
      if TWin.Signal_List = null then
         --  insert by the front
         TS.Next := null;
         TWin.Signal_List := TS;
         return Not_Duplicated (TS, Ignore_Dup);
      end if;

      if TS.GtkName.all <= TWin.Signal_List.GtkName.all then
         --  insert by the front
         TS.Next := TWin.Signal_List;
         TWin.Signal_List := TS;
         return Not_Duplicated (TS, Ignore_Dup);
      end if;

      Temp_Prev := TWin.Signal_List;
      Temp      := Temp_Prev.Next;
      while Temp /= null loop
         if TS.GtkName.all <= Temp.GtkName.all then
            TS.Next := Temp;
            Temp_Prev.Next := TS;
            return Not_Duplicated (TS, Ignore_Dup);
         end if;
         Temp_Prev := Temp;
         Temp := Temp.Next;
      end loop;
      --  insert at the end
      TS.Next   := null;
      Temp_Prev.Next := TS;
      return Not_Duplicated (TS, Ignore_Dup);
   exception
      when Symbol_Tables.Unknown_Signal => return False;
   end Insert_Signal;

   -------------------
   -- Insert_Signal --
   -------------------
   --  insert in alphabetic order
   function  Insert_Signal (TWdg : Widget_Pointer;
                            TS   : Signal_Pointer;
                            Ignore_Dup : Boolean := False) return Boolean is
      Temp, Temp_Prev : Signal_Pointer;
   begin
      TS.GtkName := new String '(Symbol_Tables.Convert_Signal_To_Gtk (TWdg, TS));
      if TWdg.Signal_List = null  then
         --  insert by the front
         TS.Next := null;
         TWdg.Signal_List := TS;
         return Not_Duplicated (TS, Ignore_Dup);
      end if;

      if TS.GtkName.all <= TWdg.Signal_List.GtkName.all then
         --  insert by the front
         TS.Next := TWdg.Signal_List;
         TWdg.Signal_List := TS;
         return Not_Duplicated (TS, Ignore_Dup);
      end if;

      Temp_Prev := TWdg.Signal_List;
      Temp      := Temp_Prev.Next;
      while Temp /= null loop
         if TS.GtkName.all <= Temp.GtkName.all then
            TS.Next := Temp;
            Temp_Prev.Next := TS;
            return Not_Duplicated (TS, Ignore_Dup);
         end if;
         Temp_Prev := Temp;
         Temp := Temp.Next;
      end loop;
      --  insert at the end
      TS.Next   := null;
      Temp_Prev.Next := TS;
      return Not_Duplicated (TS, Ignore_Dup);
   exception
      when Symbol_Tables.Unknown_Signal => return False;
   end Insert_Signal;

   ------------------
   -- Signal Exists--
   ------------------

   function Signal_Exists (TWin : Window_Pointer;
                           Signal_Name : String) return Boolean is
      TS : Signal_Pointer;
   begin
      TS := TWin.Signal_List;
      while TS /= null loop
         if TS.Name.all = Signal_Name then
            return True;
         end if;
         TS := TS.Next;
      end loop;
      return False;
   end Signal_Exists;

   function Signal_Exists (TWdg        : Widget_Pointer;
                           Signal_Name : String) return Boolean is
      TS : Signal_Pointer;
   begin
      TS := TWdg.Signal_List;
      while TS /= null loop
         if TS.Name.all = Signal_Name then
            return True;
         end if;
         if TS.GtkName.all = Signal_Name then
            return True;
         end if;
         TS := TS.Next;
      end loop;
      return False;
   end Signal_Exists;

   -----------------
   -- Insert_Focus --
   ------------------
   --  Inserted with TabIndex increasing
   --  Last widget in the list points to the first one (i.e., circular list)
   procedure Insert_Focus (Into : Window_Pointer; Focus : Widget_Pointer) is
      Temp : Widget_Pointer;

      procedure TabIndex_Repeated (W1 : Widget_Pointer; W2 : Widget_Pointer);
      procedure TabIndex_Repeated (W1 : Widget_Pointer; W2 : Widget_Pointer) is
      begin
         TIO.Put_Line (W1.Name.all & ".TabIndex" & W1.TabIndex'Image);
         TIO.Put_Line (W2.Name.all & ".TabIndex" & W2.TabIndex'Image);
      end TabIndex_Repeated;
   begin
      if Into.TabFocusList = null then
         --  empty list
         Into.TabFocusList := Focus;
         Focus.Next_Focus  := Focus;
         Focus.Prev_Focus  := Focus;
         return;
      end if;
      if Into.TabFocusList.Next_Focus = Into.TabFocusList then
         --  list with just one element
         if Into.TabFocusList.TabIndex < Focus.TabIndex then
            Into.TabFocusList.Next_Focus := Focus;
            Into.TabFocusList.Prev_Focus := Focus;
            Focus.Next_Focus := Into.TabFocusList;
            Focus.Prev_Focus := Into.TabFocusList;
         elsif Into.TabFocusList.TabIndex > Focus.TabIndex then
            Focus.Next_Focus := Into.TabFocusList;
            Focus.Prev_Focus := Into.TabFocusList;
            Into.TabFocusList.Next_Focus := Focus;
            Into.TabFocusList.Prev_Focus := Focus;
            Into.TabFocusList := Focus;
         else
            TabIndex_Repeated (Into.TabFocusList, Focus);
            raise TIO.Data_Error;
         end if;
         return;
      end if;
      --  here the list has two or more elements
      --  check that tabindex is not repeated
      Temp := Into.TabFocusList;
      loop
         if Temp.TabIndex = Focus.TabIndex then
            TabIndex_Repeated (Temp, Focus);
            raise TIO.Data_Error;
         end if;
         Temp := Temp.Next_Focus;
         exit when Temp = Into.TabFocusList;
      end loop;

      --  check if must be inserted in the front
      Temp := Into.TabFocusList;
      if Temp.TabIndex > Focus.TabIndex then
         Focus.Next_Focus := Temp;
         Temp.Prev_Focus.Next_Focus := Focus;
         Focus.Prev_Focus := Temp.Prev_Focus;
         Temp.Prev_Focus := Focus;
         Into.TabFocusList := Focus;
         return;
      end if;

      --  check if must be inserted in the end
      Temp := Into.TabFocusList.Prev_Focus;
      if Temp.TabIndex < Focus.TabIndex
--        and then Into.TabFocusList.TabIndex < Focus.TabIndex
      then
         Focus.Next_Focus := Into.TabFocusList;
         Focus.Prev_Focus := Temp;
         Temp.Next_Focus := Focus;
         Into.TabFocusList.Prev_Focus := Focus;
         return;
      end if;

      --  insert by tabindex in the middle
      Temp := Into.TabFocusList;
      loop
         if Temp.TabIndex < Focus.TabIndex
           and then Temp.Next_Focus.TabIndex > Focus.TabIndex
         then
            Focus.Next_Focus := Temp.Next_Focus;
            Focus.Prev_Focus := Temp;
            Temp.Next_Focus.Prev_Focus := Focus;
            Temp.Next_Focus := Focus;
            return;
         end if;
         Temp := Temp.Next_Focus;
         exit when Temp = Into.TabFocusList;
      end loop;

      raise Program_Error;
   end Insert_Focus;

   -----------------
   -- Next_Window --
   -----------------

   function Next_Window (Root : Window_Pointer;
                         TWin : Window_Pointer) return Window_Pointer is
   begin
      if Root = null then
         return null;
      end if;
      if TWin = null then
         return null;
      end if;
      if TWin = Root then
         if TWin.Next = Root then
            return null; -- there was only one
         else
            return TWin.Next;
         end if;
      end if;
      if TWin.Next = Root then
         return null; -- twin was last
      else
         return TWin.Next;
      end if;
   end Next_Window;

   -----------------
   -- Find_Window --
   -----------------

   function Find_Window (Root : Window_Pointer;
                         Name : String) return Window_Pointer is
      Temp : Window_Pointer := Root;
   begin
      if Root = null then
         return null;
      end if;
      while Temp /= null  loop
         if Name = Temp.Name.all then
            return Temp;
         end if;
         Temp := Next_Window (Root, Temp);
      end loop;
      return null;
   end Find_Window;

   ------------------
   -- Is_Duplicate --
   ------------------

   --  ensure no duplicated gtk windows
   function Is_Duplicate (Root : Window_Pointer;
                          TWin : Window_Pointer) return Boolean;
   function Is_Duplicate (Root : Window_Pointer;
                          TWin : Window_Pointer) return Boolean is
      Temp : constant Window_Pointer := Find_Window (Root, TWin.Name.all);
   begin
      return Temp /= null;
   end Is_Duplicate;

   ---------------------------
   -- Insert_Window_By_Tail --
   ---------------------------
   --  insert by the tail
   procedure Insert_Window_By_Tail (Root : in out Window_Pointer;
                                    TWin : Window_Pointer);
   procedure Insert_Window_By_Tail (Root : in out Window_Pointer;
                                    TWin : Window_Pointer) is
      Last : Window_Pointer;
   begin
      if Is_Duplicate (Root, TWin) then
         raise Duplicated_Window_Name;
      end if;
      if Root = null then
         Root := TWin;
         TWin.Prev := TWin;
         TWin.Next := TWin;
      else
         Last := Root.Prev;
         TWin.Next := Root;
         TWin.Prev := Last;
         Last.Next := TWin;
         Root.Prev := TWin;
      end if;
   end Insert_Window_By_Tail;

   ----------------------------
   -- Insert_Window_By_Order --
   ----------------------------

   --  insert by order
   procedure Insert_Window_By_Order (Root : in out Window_Pointer;
                                     TWin : Window_Pointer) is
      Temp : Window_Pointer;
   begin
      if Is_Duplicate (Root, TWin) then
         raise Duplicated_Window_Name;
      end if;
      if Root = null then --  list empty
         Insert_Window_By_Front (Root, TWin);
         return;
      end if;

      if TWin.Name.all <= Root.Name.all then --  less than first
         Insert_Window_By_Front (Root, TWin);
         return;
      end if;

      if TWin.Name.all > Root.Prev.Name.all then --  after last
         Insert_Window_By_Tail (Root, TWin);
         return;
      end if;

      Temp := Root.Next;
      while Temp /= Root loop
         if TWin.Name.all <= Temp.Name.all then
            TWin.Next := Temp;
            TWin.Prev := Temp.Prev;
            Temp.Prev.Next := TWin;
            Temp.Prev := TWin;
            return;
         end if;
         Temp := Temp.Next;
      end loop;
      raise Program_Error;
   end Insert_Window_By_Order;

   ----------------------------
   -- Insert_Window_By_Front --
   ----------------------------

   procedure Insert_Window_By_Front (Root : in out Window_Pointer;
                                     TWin : Window_Pointer) is
   begin
      if TWin.Name = null or else TWin.Name.all = "" then
         raise Window_Name_Not_Assigned;
      end if;
      if Is_Duplicate (Root, TWin) then
         Debug (0, "Duplicated Window Name " & TWin.Name.all);
         raise Duplicated_Window_Name;
      end if;
      if Root = null then
         Root := TWin;
         TWin.Prev := TWin;
         TWin.Next := TWin;
      else
         TWin.Next := Root;
         TWin.Prev := Root.Prev;
         Root.Prev.Next := TWin;
         Root.Prev := TWin;
         Root      := TWin;
      end if;
   end Insert_Window_By_Front;

   --------------------------
   -- Extract_First_Window --
   --------------------------

   function Extract_First_Window (Root : in out Window_Pointer)
                                  return Window_Pointer is
      TWin : Window_Pointer;
   begin
      if Root = null then
         return null;
      end if;
      TWin := Root;
      if Root.Next = Root then
         --  only one
         TWin.Next := null;
         TWin.Prev := null;
         Root := null;
      else
         Root.Prev.Next := Root.Next;
         Root.Next.Prev := Root.Prev;
         Root := Root.Next;
         TWin.Next := null;
         TWin.Prev := null;
      end if;
      return TWin;
   end Extract_First_Window;

   --------------------
   -- Extract_Window --
   --------------------

   procedure Extract_Window (Root : in out Window_Pointer;
                             TWin : Window_Pointer) is
   begin
      if Root = null then
         raise Program_Error;
      end if;
      if Root.Next = Root then --  only one
         if Root = TWin then
            Root := null;
            TWin.Next := null;
            TWin.Prev := null;
            return;
         else
            raise Program_Error;
         end if;
      end if;
      if TWin = Root then --  extract the first
         Root.Prev.Next := Root.Next;
         Root.Next.Prev := Root.Prev;
         Root := Root.Next;
         TWin.Next := null;
         TWin.Prev := null;
         return;
      end if;
      if TWin = Root.Prev then --  extract the last
         Root.Prev.Prev := Root;
         Root.Prev := Root.Prev.Prev;
         TWin.Next := null;
         TWin.Prev := null;
         return;
      end if;
      --  extract in the midle
      TWin.Prev.Next := TWin.Next;
      TWin.Next.Prev := TWin.Prev;
      TWin.Next := null;
      TWin.Prev := null;
   end Extract_Window;

   -------------------
   -- Insert_Window --
   -------------------

   procedure Insert_Window (Root  : in out Window_Pointer;
                            After : Window_Pointer;
                            TWin  : Window_Pointer) is
   begin
      if Root = null then
         Insert_Window_By_Tail (Root, TWin);
         return;
      end if;

      if After.Next = Root then --  after is last
         Insert_Window_By_Tail (Root, TWin);
      else
         TWin.Next := After.Next;
         TWin.Prev := After;
         After.Next.Prev := TWin;
         After.Next := TWin;
      end if;
   end Insert_Window;

   ----------------
   -- Join_Roots --
   ----------------

   procedure Join_Roots (Root1 : in out Window_Pointer;
                         Root2 : in out Window_Pointer) is
      Temp : Window_Pointer;
   begin
      if Root2 = null then
         return;
      end if;
      if Root1 = null then
         Root1 := Root2;
         return;
      end if;

      Root1.Prev.Next := Root2;
      Temp := Root1.Prev;
      Root1.Prev      := Root2.Prev;

      Root2.Prev.Next := Root1;
      Root2.Prev      := Temp;
      Root2 := null;
   end Join_Roots;

   -------------------------------
   -- Relink_Children_To_Parent --
   -------------------------------
   --  On entry, all widgets belonging to a windows are in the window's
   --  widget list, ordered as its first appearence in the Designer
   --  On exit, widgets are linked in the child_list of parent, by order
   --  as indicated in widget's child_num

   procedure Relink_Children_To_Parent (TWin : Window_Pointer) is
      TWdg : Widget_Pointer := TWin.Widget_List;
      Temp : Widget_Pointer;
      Counter : Integer := 0;
   begin
      --  upon start, all widgets in a window are linked in just the
      --  window's widget_list, as their first appearence in the Designer
      while TWdg /= null loop
         if TWdg.Wdg_Parent /= null then
            Temp := TWdg;
            TWdg := TWdg.Next;
            Unlink_Widget (TWin, Temp);
            Insert_Widget_By_Order (Temp.Wdg_Parent, Temp);
            Counter := Counter + 1;
            if Temp.Name /= null then
               if Temp.Wdg_Parent.Name /= null then
                  Debug (0, Sp (3) & Counter'Image & " "
                         & Temp.Name.all
                         & " [" & Temp.Widget_Type'Image & "]"
                         & " linked to parent widget "
                         & Temp.Wdg_Parent.Name.all
                         & " [" & Temp.Wdg_Parent.Widget_Type'Image & "]");
               else
                  Debug (0, Sp (3) & Counter'Image & " "
                         & Temp.Name.all
                         & " linked to parent widget "
                         & Temp.Wdg_Parent.Widget_Type'Image
                         & " [" & Temp.Wdg_Parent.Widget_Type'Image & "]");
               end if;
            else
               if Temp.Wdg_Parent.Name /= null then
                  Debug (0, "WARNING:" & Sp (3) & "Widget "
                         & "NO NAME" & Counter'Image & " "
                         & " [" & Temp.Widget_Type'Image & "]"
                         & " linked to parent widget "
                         & Temp.Wdg_Parent.Name.all
                         & " [" & Temp.Wdg_Parent.Widget_Type'Image & "]");
               else
                  Debug (0, "WARNING:" & Sp (3) & "Widget "
                         & "NO NAME" & Counter'Image & " "
                         & " [" & Temp.Widget_Type'Image & "]"
                         & " linked to parent window or widget "
                         & Temp.Wdg_Parent.Widget_Type'Image
                         & " [" & Temp.Wdg_Parent.Widget_Type'Image & "]");
               end if;
            end if;
         else
            Counter := Counter + 1;
            if TWdg /= null and then TWdg.Name /= null then
               Debug (0, Sp (3) & Counter'Image & " "
                      & TWdg.Name.all
                      & " [" & TWdg.Widget_Type'Image & "]"
                      & " linked to parent Window "
                      & TWin.Name.all
                      & " [" & TWin.Window_Type'Image & "]");
            else
               Debug (0, "WARNING:" & Sp (3) & "Widget "
                      & "NO NAME" & Counter'Image & " "
                      & " [" & TWdg.Widget_Type'Image & "]"
                      & " linked to parent Window "
                      & TWin.Name.all
                      & " [" & TWin.Window_Type'Image & "]");
            end if;
            TWdg := TWdg.Next;
         end if;
      end loop;
   end Relink_Children_To_Parent;

   -------------------------
   -- Process_Inheritable --
   -------------------------

   procedure Process_Inheritable (TWin : Window_Pointer) is
      TWdg : Widget_Pointer;
      Changed : Boolean;
   begin
      --  inherits font
      TWdg := TWin.Widget_List;
      while TWdg /= null loop
         Changed := False;
         if TWdg.Font_Name = null and then TWin.Font_Name /= null then
            TWdg.Font_Name := new String'(TWin.Font_Name.all);
            Changed := True;
         end if;
         if TWdg.Font_Size = Default_Font_Size then
            TWdg.Font_Size := TWin.Font_Size;
            Changed := True;
         end if;
         if TWdg.Font_Weight = null and then TWin.Font_Weight /= null then
            TWdg.Font_Weight := new String'(TWin.Font_Weight.all);
            Changed := True;
         end if;
         if not TWdg.Font_Underline and then TWin.Font_Underline then
            TWdg.Font_Underline := True;
         end if;
         if Changed then
            Debug (0, Sp (3) & "Set Inherited Property " & TWdg.Name.all & ".Font="""
                   & TWdg.Font_Name.all
                   & ", " & Img (TWdg.Font_Size)
                   & (if TWdg.Font_Weight /= null then
                        ", " & TWdg.Font_Weight.all else "") & """"
                   & (if TWdg.Font_Underline then "underline" else ""));
         end if;
         TWdg := TWdg.Next;
      end loop;

      for I in 1 .. Max_DGVS loop
         Changed := False;
         if DGVS (I).Font_Name = null and then TWin.Font_Name /= null then
            DGVS (I).Font_Name := new String'(TWin.Font_Name.all);
            Changed := True;
         end if;
         if DGVS (I).Font_Size = Default_Font_Size then
            DGVS (I).Font_Size := TWin.Font_Size;
            Changed := True;
         end if;
         if DGVS (I).Font_Weight = null and then TWin.Font_Weight /= null
         then
            DGVS (I).Font_Weight := new String'(TWin.Font_Weight.all);
            Changed := True;
         end if;
         if not DGVS (I).Font_Underline and then TWin.Font_Underline then
            DGVS (I).Font_Underline := True;
            Changed := True;
         end if;
         if Changed then
            if DGVS (I).Name /= null then
               Debug (0, Sp (3) & "Set Inherited Property Gtk_Cell_Renderer "
                      & "(" & Img (I) & ") "
                      & DGVS (I).Name.all
                      & ".Font=""" & DGVS (I).Font_Name.all
                      & ", " & Img (DGVS (I).Font_Size)
                      & (if DGVS (I).Font_Weight /= null then
                           ", " & DGVS (I).Font_Weight.all else "") & """"
                      & (if DGVS (I).Font_Underline then "underline" else ""));
            else
               Debug (0, Sp (3) & "Set Inherited Property Gtk_Cell_Renderer "
                      & "(" & Img (I) & ")"
                      & ".Font=""" & DGVS (I).Font_Name.all
                      & ", " & Img (DGVS (I).Font_Size)
                      & (if DGVS (I).Font_Weight /= null then
                           ", " & DGVS (I).Font_Weight.all else "") & """"
                      & (if DGVS (I).Font_Underline then "underline" else ""));
            end if;
         end if;
      end loop;
   end Process_Inheritable;

   --------------
   -- Contains --
   --------------

   function Get_DialogResult_Enum (Source : String) return DialogResult_Enum is
   begin
      if Source = "None" then
         return None_Response;
      elsif Source = "OK" then
         return OK_Response;
      elsif Source = "Cancel" then
         return Cancel_Response;
      elsif Source = "Abort" then
         return Reject_Response;
      elsif Source = "Retry" then
         return Retry_Response;
      elsif Source = "Ignore" then
         return Ignore_Response;
      elsif Source = "Yes" then
         return Yes_Response;
      elsif Source = "No" then
         return No_Response;
      elsif Source = "TryAgain" then
         return TryAgain_Response;
      else
         return Pending_Response;
      end if;
   end Get_DialogResult_Enum;

   --------------
   -- Contains --
   --------------

   function Contains (Source : String; Pattern : String) return Boolean is
      Idx0 : Integer;
      LC_Source  : constant String := To_Lower (Source);
      LC_Pattern : constant String := To_Lower (Pattern);
   begin
      Idx0 := Index (Source  => LC_Source,
                     Pattern => LC_Pattern);
      return (Idx0 in LC_Source'Range);
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

   function Get_Boolean (Data : String) return Boolean is
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

   function Get_Margin_Array (Data : String) return Margin_Array is
      P    : Pair;
      Q    : Pair;
      Idx0  : Integer;
      Idx1  : Integer;
   begin
      --  1 => start or left, 2 => top, 3 => end or right, 4 => bottom
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
      return Margin_Array'(1 => P.One, 2 => P.Two,
                           3 => Q.One, 4 => Q.Two);
   end Get_Margin_Array;

   ----------------------
   -- Get_Margin_Array --
   ----------------------

   function Get_Margin_Array (F : TIO.File_Type) return Margin_Array is
      Data : constant String := Get_String (F);
   begin
      return Get_Margin_Array (Data);
   end Get_Margin_Array;

   --------------
   -- Get_Font --
   --------------

   procedure Get_Font (Data           : in String;
                       Font_Name      : in out String_Access;
                       Font_Size      : in out Integer;
                       Font_Weight    : in out String_Access;
                       Font_Underline : in out Boolean) is
      Idx0  : Integer;
      Idx1  : Integer;
      Idx2  : Integer;
      Idx3  : Integer;
      Last  : Integer;
      Num   : Float;
   begin
      Idx0 := Index (Data, """");
      if Idx0 not in Data'Range then
         raise TIO.Data_Error;
      end if;
      Idx0 := Idx0 + 1;
      Idx1 := Index (Data (Idx0 + 1 .. Data'Last), """");
      if Idx1 not in Idx0 + 1 .. Data'Last then
         raise TIO.Data_Error;
      end if;
      Idx1 := Idx1 - 1;

      --  font name
      if Data (Idx0 .. Idx1) = "Calibri" then
         Font_Name := new String'("Sans");
      else
         Font_Name := new String'(Data (Idx0 .. Idx1));
      end if;

      --  font size
      Idx2 := Index (Data (Idx1 + 1 .. Data'Last), """, ");
      if not (Idx2 in Idx1 + 1 .. Data'Last) then
         raise TIO.Data_Error;
      end if;
      Idx2 := Idx2 + 3;
      FTIO.Get (Data (Idx2 .. Data'Last), Num, Last);
      Font_Size := Integer (Num);

      --  optional font weitht
      Idx3 := Index (Data (Idx2 + 1 .. Data'Last), ", style=");
      if Idx3 in Idx2 + 1 .. Data'Last - 1 then
         Font_Weight := new String'(To_Lower
                                    (Data (Idx3 + 8 .. Data'Last - 1)));
      else
         Font_Weight := null;
      end if;

      --  optional font underline
      Idx3 :=  Index (Data,  "System.Drawing.FontStyle.Underline");
      Font_Underline := Idx3 in Data'Range;
   end Get_Font;

   --------------
   -- Get_Font --
   --------------

   procedure Get_Font (F              : TIO.File_Type;
                       Font_Name      : in out String_Access;
                       Font_Size      : in out Integer;
                       Font_Weight    : in out String_Access;
                       Font_Underline : in out Boolean) is
      Data  : constant String := Get_String (F);
      Idx0  : Integer;
      Idx1  : Integer;
      Idx2  : Integer;
      Last  : Integer;
      Num   : Float;
   begin
      Idx0 := Index (Data, ",");
      if not (Idx0 in Data'Range)
        or else Idx0 = Data'First
      then
         raise TIO.Data_Error;
      end if;

      --  font name
      if Data (Data'First .. Idx0 - 1) = "Calibri" then
         Font_Name := new String'("Sans");
      else
         Font_Name := new String'(Data (Data'First .. Idx0 - 1));
      end if;

      --  font size
      Idx1 := Index (Data (Idx0 + 1 .. Data'Last), "pt");
      if not (Idx1 in Idx0 + 1 .. Data'Last) then
         raise TIO.Data_Error;
      end if;
      FTIO.Get (Data (Idx0 + 1 .. Idx1 - 1), Num, Last);
      Font_Size := Integer (Num);

      --  font weight
      Idx2 := Index (Data (Idx1 + 1 .. Data'Last), ", style=");
      if Idx2 in Idx1 + 1 .. Data'Last - 1 then
         Font_Weight := new String'(To_Lower (Data (Idx2 + 8 .. Data'Last)));
      else
         Font_Weight := null;
      end if;

      --  optional font underline
      Idx2 :=  Index (Data,  "System.Drawing.FontStyle.Underline");
      Font_Underline := Idx2 in Data'Range;
   end Get_Font;

   ------------------
   -- Num_Children --
   ------------------

   function Num_Children (TWdg : Widget_Pointer) return Integer is
      Temp : Widget_Pointer;
      Num  : Integer := 0;
   begin
      if TWdg = null then
         return -1;
      end if;
      Temp := TWdg.Child_List;
      while Temp /= null loop
         Num := Num + 1;
         Temp := Temp.Next;
      end loop;
      if Num /= TWdg.Num_Children then
         raise Program_Error;
      end if;
      return Num;
   end Num_Children;

   --------------------
   -- Normalize Name --
   --------------------

   function Normalize_Name (TWdg : Widget_Pointer) return String is
      Idx0 : Integer;
   begin
      case TWdg.Widget_Type is
         when GtkDataGridView =>
            Idx0 := Index (TWdg.Name.all, "DataGridView_");
            if Idx0 in TWdg.Name.all'Range then
               return TWdg.Name (Idx0 + 13 .. TWdg.Name'Last);
            end if;
         when GtkTreeGridView =>
            Idx0 := Index (TWdg.Name.all, "TreeGridView_");
            if Idx0 in TWdg.Name.all'Range then
               return TWdg.Name (Idx0 + 13 .. TWdg.Name'Last);
            end if;
         when others => null;
      end case;
      return TWdg.Name.all;
   end Normalize_Name;

   --------------
   --  To_Gtk  --
   --------------

   function To_Gtk (T : Window_Pointer) return String is
   begin
      case T.Window_Type is
         when GtkWindow =>
            if T.Is_Dialog then
               return "Gtk_Dialog";
            else
               return "Gtk_Window";
            end if;
         when GtkFileChooserDialog => return "Gtk_File_Chooser_Dialog";
         when GtkFileFilter        => return "Gtk_File_Filter";
         when GtkEntryBuffer       => return "Gtk_Entry_Buffer";
         when GtkListStore         => return "Gtk_List_Store";
         when GtkTreeStore         => return "Gtk_Tree_Store";
         when GtkModelSort         => return "Gtk_Tree_Model_Sort";
         when GtkModelFilter       => return "Gtk_Tree_Model_Filter";
         when GtkImage             => return "Gtk_Image";
      end case;
   end To_Gtk;

   function To_Gtk (T       : Widget_Pointer;
                    For_Ada : Boolean := False) return String is
   begin
      if T.Widget_Type = GtkTabPage and then For_Ada then
         return "Gtk_Widget";
      end if;

      case T.Widget_Type is
         when No_Widget => return "No Widget";
         when GtkAspectFrame => return "Gtk_AspectFrame";
         when GtkAlignment => return "Gtk_Alignment";
         when GtkLabel => return "Gtk_Label";
         when GtkNoteBook => return "Gtk_Notebook";
         when GtkTabPage => return "tab";
         when GtkTabChild => return "Gtk_Box";
         when GtkDataGridView => return "Gtk_Tree_View";
         when GtkTreeGridView => return "Gtk_Tree_View";
         when ExpandableColumn => return "Gtk_Tree_View_Column";
         when DataGridViewCheckBoxColumn => return "Gtk_Tree_View_Column";
         when DataGridViewTextBoxColumn => return "Gtk_Tree_View_Column";
         when GtkListBox => return "Gtk_List_Box";
         when GtkEntry => return "Gtk_Entry";
         when GtkComboTextBox => return "Gtk_Combo_Box_Text";
         when GtkToolBar => return "Gtk_Toolbar";
         when GtkButton => return "Gtk_Button";
         when GtkRadioButton => return "Gtk_Radio_Button";
         when GtkCheckButton => return "Gtk_Check_Button";
         when GtkToggleButton => return "Gtk_Toggle_Button";
         when GtkSeparatorToolItem => return "Gtk_Separator_Tool_Item";
         when GtkMenuBar => return "Gtk_Menu_Bar";
         when GtkMenuItem => return "Gtk_Menu_Item";
         when GtkMenuNormalItem => return "Gtk_Menu_Item";
         when GtkMenuImageItem => return "Gtk_Image_Menu_Item";
         when GtkMenuRadioItem => return "Gtk_Radio_Menu_Item";
         when GtkMenuCheckItem => return "Gtk_Check_Menu_Item";
         when GtkSubMenu => return "Gtk_SubMenu";
         when GtkSeparatorMenuItem => return "Gtk_Separator_Menu_Item";
         when GtkColorButton => return "Gtk_Color_Button";
         when GtkCalendar => return "Gtk_Calendar";
         when GtkToolTip => return "Gtk_Tooltip";
         when GtkScrolledWindow => return "Gtk_ScrolledWindow";
         when GtkFileChooserButton => return "Gtk_File_Chooser_Button";
         when GtkStatusBar => return "Gtk_Status_Bar";
         when GtkBox => return "Gtk_Box";
         when GtkFrame => return "Gtk_Frame";
         when GtkSpinButton => return "Gtk_Spin_Button";
         when GtkImage => return "Gtk_Image";
         when ToolStripStatusLabel => return "Gtk_Label";

         when BindingNavigator => return "Gtk_Toolbar";
         when BackgroundWorker => return "Integer";

         when PrintDocument => return "Integer";
         when PrintDialog => return "Integer";
         when Chart => return "Integer";
         when FolderBrowserDialog => return "Integer";
         when PageSetupDialog => return "Integer";
         when Internal_Child_VBox => return "Internal_Child_VBox";
         when Internal_Child_Action_Area => return "Internal_Child_Action_Area";
         when Action_Widgets => return "Action_Widgets";
         when GtkFixed => return "Gtk_Fixed";
         when GtkButtonBox => return "Gtk_ButtonBox";
      end case;
   end To_Gtk;

   function To_Gtk (D : DialogResult_Enum) return String is
   begin
      case D is
         when None_Response         => return "-1";
         when Reject_Response       => return "-2";
         when Accept_Response       => return "-3";
         when Delete_Event_Response => return "-4";
         when OK_Response           => return "-5";
         when Cancel_Response       => return "-6";
         when Close_Response        => return "-7";
         when Yes_Response          => return "-8";
         when No_Response           => return "-9";
         when Apply_Response        => return "-10";
         when Help_Response         => return "-11";
         when User1_Response        => return "-91";   --  invented
         when User2_Response        => return "-92";   --  invented
         when User3_Response        => return "-93";   --  invented
         when User4_Response        => return "-94";   --  invented
         when User5_Response        => return "-95";   --  invented
         when Delete_Response       => return "-96";   --  invented
         when Retry_Response        => return "-97";   --  invented
         when Ignore_Response       => return "-98";   --  invented
         when TryAgain_Response     => return "-99";   --  invented
         when others                => return "-1000"; --  invented
      end case;
   end To_Gtk;

   --------------------
   --  Iterate       --
   --------------------
   procedure Iterate
     (TWdg     : Widget_Pointer;
      Callback : access procedure (TWdg : Widget_Pointer)) is
   begin
      if TWdg = null then
         return;
      end if;

      Callback (TWdg);

      Iterate (TWdg.Child_List, Callback);
      Iterate (TWdg.Next, Callback);
   end Iterate;

   procedure Iterate
     (TWin : Window_Pointer;
      Callback : access procedure (TWdg : Widget_Pointer)) is
      Temp_Win : Window_Pointer := TWin;
   begin
      while Temp_Win /= null loop
         if Temp_Win.Window_Type = GtkWindow then
            Iterate (Temp_Win.Widget_List, Callback);
         end if;
         Temp_Win := Next_Window (Win_List, Temp_Win);
      end loop;
   end Iterate;

end W2gtk_Decls;
