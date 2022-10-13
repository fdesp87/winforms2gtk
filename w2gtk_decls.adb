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
with Unchecked_Deallocation;

package body W2gtk_Decls is
   package ITIO renames Ada.Integer_Text_IO;
   package FTIO renames Ada.Float_Text_IO;

   Test3  : constant String := "<value>";
   Test4  : constant String := "</value>";
   Test6  : constant String := "<value />";

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
      if Use_Debug then
         ITIO.Put (NLin, Width => 5);
         TIO.Put_Line (": " & Msg);
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

   function Img (X : Float) return String is
      Z : constant String := X'Image;
   begin
      if X >= 0.0 then
         return Z (Z'First + 1 .. Z'Last);
      else
         return Z;
      end if;
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

   ----------------------------
   -- Copy_Common_Attributes --
   ----------------------------

   procedure Copy_Common_Attributes (From : Widget_Pointer;
                                     To   : Widget_Pointer) is
      function New_String (K : String_Access) return String_Access;
      function New_String (K : String_Access) return String_Access is
      begin
         if K /= null then
            return new String'(K.all);
         else
            return null;
         end if;
      end New_String;

   begin
      To.Next := From.Next;
      To.Prev := From.Next;
      To.Child_List := From.Child_List;
      To.Num_Children := From.Num_Children;

      To.Parent_Name := New_String (From.Parent_Name);
      To.WParent     := From.WParent;
      To.GParent     := From.GParent;
      To.Child_Num   := From.Child_Num;

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

   ----------
   -- Free --
   ----------

   procedure Free is new Unchecked_Deallocation (Widget_Properties,
                                                 Widget_Pointer);
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
      end case;
      Free (WT);
   end Release;

   -------------
   -- Unlink  --
   -------------

   procedure Unlink_Widget (TWin : Window_Pointer;
                            WT   : Widget_Pointer);
   procedure Unlink_Widget (TWin : Window_Pointer;
                            WT   : Widget_Pointer) is
   begin
      if TWin.Widget_List = WT then       --  first item
         if WT.Next = null then           --  and last item
            TWin.Widget_List := null;
         else                             --  first but more items
            WT.Next.Prev := null;
            TWin.Widget_List := WT.Next;
            TWin.Widget_List.Prev := null;
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
                            WT   : Widget_Pointer);
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
      if Parent.Child_List.Child_Num > WT.Child_Num then
         WT.Next := Parent.Child_List;
         WT.Prev := null;
         Parent.Child_List.Prev := WT;
         Parent.Child_List := WT;
         return;
      end if;

      --  check if WT should be the current last
      Temp := Parent.Child_List;
      loop
         exit when Temp.Next = null;
         Temp := Temp.Next;
      end loop;
      if Temp.Child_Num < WT.Child_Num then
         Temp.Next := WT;
         WT.Prev := Temp;
         WT.Next := null;
         return;
      end if;

      --  WT in the midle. Detect it
      Temp := Parent.Child_List;
      loop
         exit when Temp.Child_Num > WT.Child_Num;
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
      GParent : constant Widget_Pointer := Parent.GParent;
   begin
      Child.GParent       := GParent;
      Child.Parent_Name   := new String'(Parent.Parent_Name.all);
      Child.FlowDirection := Parent.FlowDirection;
      Child.Child_Num     := Parent.Child_Num;

      if Parent.GParent = null then  --  first level widget



         Unlink_Widget (Parent.WParent, Parent);


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
   procedure Set_Have (WP : Window_Pointer);
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

   procedure Set_Have (WT : Widget_Pointer);
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
         when others => null;
      end case;
   end Set_Have;

   -------------------
   -- Insert_Widget --
   -------------------
   --  insert by the tail, by the order found when parsing Designer step 1
   procedure Insert_Widget_By_Tail (Parent : Window_Pointer;
                                    WT     : Widget_Pointer) is
      Temp : Widget_Pointer;
   begin
      if Parent.Widget_List = null then
         Parent.Widget_List := WT;
         WT.Next := null;
         WT.Prev := null;
         return;
      end if;
      Temp := Parent.Widget_List;
      loop
         exit when Temp.Next = null;
         Temp := Temp.Next;
      end loop;
      Temp.Next := WT;
      WT.Next   := null;
      WT.Prev   := Temp;
      Set_Have (WT);
   end Insert_Widget_By_Tail;

   -------------------
   -- Insert_Signal --
   -------------------
   --  insert by the front

   procedure Insert_Signal (TWin : Window_Pointer;
                            TS   : Signal_Pointer) is
   begin
      if TWin.Signal_List = null then
         TS.Next := null;
         TWin.Signal_List := TS;
      else
         TS.Next := TWin.Signal_List;
         TWin.Signal_List := TS;
      end if;
   end Insert_Signal;

   procedure Insert_Signal (TWdg : Widget_Pointer;
                            TS   : Signal_Pointer) is
   begin
      if TWdg.Signal_List = null then
         TS.Next := null;
         TWdg.Signal_List := TS;
      else
         TS.Next := TWdg.Signal_List;
         TWdg.Signal_List := TS;
      end if;
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

   -------------------
   -- Insert_Window --
   -------------------
   --  insert by the tail
   procedure Insert_Window_By_Tail (TWin : Window_Pointer) is
      Temp : Window_Pointer;
   begin
      if Win_List = null then
         Win_List := TWin;
         TWin.Next := null;
         return;
      end if;
      Temp := Win_List;
      loop
         exit when Temp.Next = null;
         Temp := Temp.Next;
      end loop;
      Temp.Next := TWin;
      TWin.Next := null;
      Set_Have (TWin);
   end Insert_Window_By_Tail;

   -------------------
   -- Insert_Window --
   -------------------
   --  insert by the tail
   procedure Insert_Window_By_Front (TWin : Window_Pointer) is
   begin
      TWin.Next := Win_List;
      Win_List  := TWin;
      Set_Have (TWin);
   end Insert_Window_By_Front;

   -------------------------------
   -- Relink_Children_To_Parent --
   -------------------------------
   --  On entry, all widgets belonging to a windows are in the window's
   --  widget list, ordered as its first appearence in the Designer
   --  On exit, widgets are linked in the child_list of parent, by order
   --  as indicated in widget's child_num

   procedure Relink_Children_To_Parent (TWin : Window_Pointer) is

      -----------------------------------------
      TWdg : Widget_Pointer := TWin.Widget_List;
      Temp : Widget_Pointer;
   begin
      --  upon start, all widgets in a window are linked in just the
      --  window's widget_list, as their first appearence in the Designer
      while TWdg /= null loop
         if TWdg.GParent /= null then
            Temp := TWdg;
            TWdg := TWdg.Next;
            Unlink_Widget (TWin, Temp);
            Insert_Widget_By_Order (Temp.GParent, Temp);
            Debug (0, "Widget "
                   & Temp.Name.all
                   & " (Child Number " & Img (Temp.Child_Num) & ")"
                   & " linked to Parent " & Temp.GParent.Name.all);
         else
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
            Debug (0, "Set Inherited Property " & TWdg.Name.all & ".Font="""
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
               Debug (0, "Set Inherited Property Gtk_Cell_Renderer "
                      & "(" & Img (I) & ")"
                      & DGVS (I).Name.all
                      & ".Font=""" & DGVS (I).Font_Name.all
                      & ", " & Img (DGVS (I).Font_Size)
                      & (if DGVS (I).Font_Weight /= null then
                           ", " & DGVS (I).Font_Weight.all else "") & """"
                      & (if DGVS (I).Font_Underline then "underline" else ""));
            else
               Debug (0, "Set Inherited Property Gtk_Cell_Renderer "
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
      --  1 => start or left, 2 => top, 3 => end or right, 4 => bottom
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
         when GtkSeparatorMenuItem => return "Gtk_Separator_Menu_Item";
         when GtkColorButton => return "Gtk_Color_Button";
         when GtkCalendar => return "Gtk_Calendar";
         when GtkToolTip => return "Gtk_Tool_Tip";
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
         when others => return "";
      end case;
   end To_Gtk;

   function To_Gtk (D : DialogResult_Enum) return String is
   begin
      case D is
         when None     => return "-1";  --  None
         when OK       => return "-5";  --  OK
         when Cancel   => return "-6";  --  Cancel
         when Aborted  => return "-7";  --  Close
         when Retry    => return "-97"; --  invented
         when Ignore   => return "-98"; --  invented
         when Yes      => return "-8";  --  Yes
         when No       => return "-9";  --  No
         when TryAgain => return "-99"; --  invented
         when Continue => return "-10"; --  Apply
      end case;
      --  others in gtk: Reject=-2, Accept=-3, Delete (from titlebar) = -3
      --                 Help = -11
   end To_Gtk;
end W2gtk_Decls;
