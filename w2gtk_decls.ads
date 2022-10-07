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
with Ada.Calendar;
with GNAT.Strings; use GNAT.Strings;
with GNATCOLL.Tribooleans;  use GNATCOLL.Tribooleans;
with GNATCOLL.Utils;

package W2gtk_Decls is
   package TIO renames Ada.Text_IO;
   package AC  renames Ada.Calendar;

   Default_Font_Size : constant := -9;
   Default_Font_Name : constant String := "Calibri";

   type DialogResult_Enum is (None, OK, Cancel, Aborted, Retry, Ignore,
                              Yes, No, TryAgain, Continue);

   type Margin_Array is array (Integer range 1 .. 4) of Integer;
   Null_Margin : constant Margin_Array := (others => -1);

   type Location_Pair is record
      From_Top  : Integer := -1;
      From_Left : Integer := -1;
   end record;

   type Size_Pair is record
      Horiz : Integer := -1;
      Vert  : Integer := -1;
   end record;

   type Pair is record
      One : Integer;
      Two : Integer;
   end record;

   type FlowDirection_Enum is (LeftToRight, RightToLeft, TopDown, BottomUp);

   type Window_Position_Enum is (None, CenterParent);

   type Image_Position_Enum is (Left, Right, Top, Bottom);

   type Display_Style_Enum is (Unset,
                               Icons_Only,         --  icons
                               Text_Only,          --  text
                               Text_Beside_Icons,  --  both horiz
                               Text_Below_Icons);  --  both

   type TextAlign_Enum is (None,
                           TopLeft, TopCenter, TopRight,
                           MiddleLeft, MiddleCenter, MiddleRight,
                           BottomLeft, BottomCenter, BottomRight,
                           Left, Right, Center, Top, Bottom);

   type ColumnHeadersHeightSizeMode_Enum is (EnableResizing,
                                             DisableResizing,
                                             AutomaticSize);

   type RowHeadersWidthSizeMode_Enum is (EnableResizing,
                                         DisableResizing,
                                         AutoSizeToAllHeaders,
                                         AutoSizeToDisplayedHeaders,
                                         AutoSizeToFirstHeader);

   type RowHeadersBorderStyle_Enum is (Custom,
                                       Single,
                                       Raised,
                                       Sunken,
                                       None);

   type BorderStyle_Enum is (None, FixedSingle, Fixed3D);

   type SortMode_Enum is (NotSortable, Automatic, Programmatic);

   type AutoSizeMode_Enum is (GrowAndShrink, GrowOnly);

   type AutoSizeColumnMode_Enum is (NotSet, None, ColumnHeader,
                                    AllCellsExceptHeader, AllCells,
                                    DisplayedCellsExceptHeader, DisplayedCells,
                                    Fill);
   type ScrollBars_Enum is (None, Horizontal, Vertical, Both);

   --  signals
   type Signal_Block;
   type Signal_Pointer is access all Signal_Block;
   type Signal_Block is record
      Next    : Signal_Pointer;
      Name    : String_Access := null; --  original signal name
      Handler : String_Access := null; --  handler name
      Line    : Integer;
      Proc    : Boolean := True;       --  false means function
      After   : Boolean := False;      --  emit After if true
      Glade   : Boolean := True;       --  generate glade signal
      GAda    : Boolean := True;       --  generate Ada signal handler
   end record;

   --  datagridview style
   type DGVS_Attribute_Enum is (DGVS_No_Attribute, DGVS_Attr_Ignored,
                                DGVS_Attr_Alignment,
                                DGVS_Attr_BgColor,
                                DGVS_Attr_FgColor,
                                DGVS_Attr_SelBgColor,
                                DGVS_Attr_SelFgColor,
                                DGVS_Attr_Font,
                                DGVS_Attr_Format,
                                DGVS_Attr_Padding,
                                DGVS_Attr_WrapMode,
                                DGVS_Attr_NullValue);
   type Cell_Style_Enum is (None,
                            For_TextCell,
                            For_ToggleCell,
                            For_Column_Header,
                            For_Row_Header,
                            For_TreeGridView);
   type Cell_Format_Enum is (Format_Bool,
                             Format_Int,
                             Format_Real,
                             Format_String);
   type DataGridViewStyle is record
      Num            : Integer          := 0;
      Name           : String_Access    := null;
      Style_For      : Cell_Style_Enum  := None;
      Emit           : Boolean          := False;
      Alignment      : String_Access    := null;
      BgColor        : String_Access    := null;
      FgColor        : String_Access    := null;
      SelBgColor     : String_Access    := null;
      SelFgColor     : String_Access    := null;
      Font_Name      : String_Access    := null;
      Font_Size      : Integer          := Default_Font_Size;
      Font_Weight    : String_Access    := null;
      Font_Underline : Boolean          := False;
      Format         : Cell_Format_Enum := Format_String;
      Padding        : Margin_Array     := Null_Margin;
      WrapMode       : Boolean          := False;
      NullValue      : String_Access    := null;
   end record;
   type DGVS_Array is array (Integer range <>) of DataGridViewStyle;
   type DGVS_Array_Pointer is access all DGVS_Array;
   Max_DGVS : Integer := -1;
   DGVS     : DGVS_Array_Pointer;

   type Text_Renderer_Properties is record
      Text_Column               : Integer := -1;
      Markup_Column             : Integer := -1;
      SingleParagraph_Column    : Integer := -1;
      Width_Chars_Column        : Integer := -1;
      Max_Width_Chars_Column    : Integer := -1;
      Wrap_Width_Column         : Integer := -1;
      Alignment_Column          : Integer := -1;
      Placeholder_Text_Column   : Integer := -1;
      bg_Color_Name_Column      : Integer := -1;
      Fg_Color_Name_Column      : Integer := -1;
      Font_Column               : Integer := -1;
      Font_Family_Column        : Integer := -1;
      Font_Variant_Column       : Integer := -1;
      Font_Weigt_Column         : Integer := -1;
      Font_Stretch_Column       : Integer := -1;
      Font_Size_Column          : Integer := -1;
      Font_Points_Column        : Integer := -1;
      Editable_Column           : Integer := -1;
      Strikethrough_Column      : Integer := -1;
      Underline_Column          : Integer := -1;
      Rise_Column               : Integer := -1;
      Language_Column           : Integer := -1;
      Ellipsize_Column          : Integer := -1;
      Wrap_Mode_Column          : Integer := -1;
      --
      Visible_Column            : Integer := -1;
      Active_Column             : Integer := -1; --  also known as sensitive
      HAlign_Column             : Integer := -1;
      VAlign_Column             : Integer := -1;
      HPadding_Column           : Integer := -1;
      VPadding_Column           : Integer := -1;
      Width_Column              : Integer := -1;
      Height_Column             : Integer := -1;
      Cell_Bg_Color_Name_Column : Integer := -1;
   end record;
   type Toggle_Renderer_Properties is record
      Activatable_Column        : Integer := -1;
      Toggle_State_Column       : Integer := -1;
      Radio_State_Column        : Integer := -1;
      Inconsisten_State_Column  : Integer := -1;
      Indicator_Size_Column     : Integer := -1;
      --
      Visible_Column            : Integer := -1;
      Active_Column             : Integer := -1; --  also known as sensitive
      HAlign_Column             : Integer := -1;
      VAlign_Column             : Integer := -1;
      HPadding_Column           : Integer := -1;
      VPadding_Column           : Integer := -1;
      Cell_Bg_Color_Name_Column : Integer := -1;
   end record;

   --  windows and widgets
   type Have_Block is record
      --  cellrenderers
      Font_Underline     : Integer := 0;
      Font_Weight        : Integer := 0;
      --  widgets
      Buttons            : Integer := 0;
      Labels             : Integer := 0;
      Menus              : Integer := 0;
      MenuImageItems     : Integer := 0;
      MenuNormalItems    : Integer := 0;
      MenuSeparators     : Integer := 0;
      Toolbars           : Integer := 0;
      ToolSeparators     : Integer := 0;
      Images             : Integer := 0;
      Notebooks          : Integer := 0;
      TreeViews          : Integer := 0;
      TreeViewColumns    : Integer := 0;
      TreeViewToggles    : Integer := 0;
      HDR_CellRenderers  : Integer := 0;
      Entries            : Integer := 0;
      ComboTextBoxes     : Integer := 0;
      Boxes              : Integer := 0;
      FileChooserButtons : Integer := 0;
      --  windows
      ListStores         : Integer := 0;
      TreeStores         : Integer := 0;
      FileFilters        : Integer := 0;
      Filechooserdialogs : Integer := 0;
      Entrybuffers       : Integer := 0;
   end record;
   Have : Have_Block;

   --  windows
   type Window_Enum is (GtkWindow,
                        GtkFileChooserDialog,
                        GtkFileFilter,
                        GtkEntryBuffer,
                        GtkListStore,
                        GtkTreeStore,
                        GtkModelSort,
                        GtkModelFilter,
                        GtkImage);
   type Window_Properties;
   type Window_Pointer is access all Window_Properties;
   type Widget_Properties;
   type Widget_Pointer is access all Widget_Properties;

   type Window_Property_Enum is (No_Property,
                                 Str_ClientSize,
                                 Str_AutoScaleDimensions,
                                 Str_Icon,
                                 Str_Margin,
                                 Str_Font,
                                 Str_Type,
                                 Str_ToolTip,
                                 Str_StartPosition,
                                 Str_TrayHeight,
                                 Str_Text,
                                 Str_Name,
                                 Str_RightToLeft,
                                 Str_Accept_Button,
                                 Str_Cancel_Button);

   type Window_Properties (Window_Type : Window_Enum) is record
      Next           : Window_Pointer;
      Name           : String_Access := null; --  it is id
      Title          : String_Access := null;
      Signal_List    : Signal_Pointer;
      case Window_Type is
         when GtkWindow =>
            Resizable         : Boolean       := True;
            Modal             : Boolean       := False;
            Is_Dialog         : Boolean       := False;
            Font_Name         : String_Access := null;
            Font_Size         : Integer       := Default_Font_Size;
            Font_Weight       : String_Access := null;
            Font_Underline    : Boolean       := False;
            Icon              : String_Access := null;
            ToolTip           : String_Access := null;
            Start_Position    : Window_Position_Enum := None;
            Client_Size       : Size_Pair;
            Margins           : Margin_Array  := Null_Margin;
            AutoScaleDim      : Size_Pair;
            TrayHeight        : Integer := 0;
            MaxTabIndex       : Integer := 0;
            MinTabIndex       : Integer := -1;
            Has_Focus_Widget  : Widget_Pointer := null;
            BgColor           : String_Access := null;
            FgColor           : String_Access := null;
            Widget_List       : Widget_Pointer := null;
            Accept_Button     : Widget_Pointer := null;
            Cancel_Button     : Widget_Pointer := null;
            TabFocusList      : Widget_Pointer := null;
         when GtkFileChooserDialog =>
            FilterName        : String_Access := null;
            Transient_For     : Window_Pointer;
            Attached_To       : Widget_Pointer;
         when GtkFileFilter =>
            FilterString      : String_Access := null;
         when GtkEntryBuffer | GtkImage |
              GtkListStore | GtkTreeStore |
              GtkModelSort | GtkModelFilter =>
            Associated_Widget  :  Widget_Pointer;
            Num_Elements : Integer := 0;
            case Window_Type is
               when GtkModelSort | GtkModelFilter =>
                  Underlying_Model : Window_Pointer;
               when others => null;
            end case;
      end case;
   end record;
   Win_List : Window_Pointer := null;
   Num_Aux_Widgets : Integer := 0;

   --  widgets
   type Widget_Enum is
     (No_Widget,
      --
      GtkLabel, GtkEntry, GtkComboTextBox,
      --
      GtkButton, GtkRadioButton, GtkCheckButton, GtkToggleButton,
      --
      GtkImage, GtkSpinButton, GtkFrame, GtkBox,
      GtkStatusBar, GtkFileChooserButton,
      --
      GtkToolTip, GtkCalendar, GtkColorButton,
      GtkListBox,
      --
      GtkMenuBar, GtkSubMenu,
      GtkMenuItem, GtkMenuNormalItem, GtkMenuImageItem,
      GtkMenuRadioItem, GtkMenuCheckItem,
      GtkSeparatorMenuItem,
      --
      GtkToolBar, GtkSeparatorToolItem,
      --
      GtkDataGridView, GtkTreeGridView,
      ExpandableColumn, DataGridViewTextBoxColumn,
      DataGridViewCheckBoxColumn,
      --
      GtkNoteBook, GtkTabChild, GtkTabPage,
      --
      PrintDocument, PrintDialog, PageSetupDialog,
      Chart, BackgroundWorker,
      FolderBrowserDialog,
      ToolStripStatusLabel,
      BindingNavigator);

   type Widget_Attribute_Enum is (No_Attribute, Attr_Ignored,
                                  Attr_Anchor, Attr_ZOrder,
                                  Attr_Margin, Attr_Padding,
                                  Attr_TabIndex, Attr_Text,
                                  Attr_MaxLength, Attr_ToolTip,
                                  Attr_ToolTipText, Attr_AutoToolTip,
                                  Attr_AutoSize, Attr_AutoSizeMode,
                                  Attr_Enabled,
                                  Attr_Visible, Attr_TextAlign,
                                  Attr_BoxTextAlign, Attr_CheckAlign,
                                  Attr_Name, Attr_Size,
                                  Attr_Location, Attr_Type,
                                  Attr_Parent, Attr_Font,
                                  Attr_PasswordChar, Attr_OpenFile,
                                  Attr_TrayLocation, Attr_Filter,
                                  Attr_Title, Attr_ImageAlign,
                                  Attr_FixedWidth,
                                  Attr_MinimumWidth, Attr_MaximumWidth,
                                  Attr_HeaderText, Attr_ItemSize,

                                  Attr_DisplayStyle, Attr_TabStop,
                                  Attr_BorderStyle, Attr_Alignment,
                                  Attr_GripStyle, Attr_Checked,
                                  Attr_CheckState, Attr_AnyColor,
                                  Attr_Items_AddRange, Attr_Columns_AddRange,
                                  Attr_DropDownItems_AddRange,
                                  Attr_FullOpen, Attr_UseSystemPasswordChar,
                                  Attr_ReadOnly, Attr_Sorted,
                                  Attr_Maximum, Attr_Minimum,
                                  Attr_Value, Attr_Format,
                                  Attr_MinDate,  Attr_MaxDate,
                                  Attr_LimitDatePicket, Attr_Image,
                                  Attr_ShowUpDown, Attr_SelectionMode,
                                  Attr_ForeColor, Attr_BackColor,
                                  Attr_ColumnHeadersVisible,
                                  Attr_ColumnHeadersDefaultCellStyle,
                                  Attr_RowHeadersDefaultCellStyle,
                                  Attr_DefaultCellStyle,
                                  Attr_AlternatingRowsDefaultCellStyle,
                                  Attr_ColumnHeadersHeightSizeMode,
                                  Attr_UserAddedColumn,
                                  Attr_AllowUserToAddRows,
                                  Attr_AllowUserToDeleteRows,
                                  Attr_AllowUserToResizeRows,
                                  Attr_AllowUserToOrderColumns,
                                  Attr_EnableHeadersVisualStyles,
                                  Attr_MultiSelect,
                                  Attr_RowHeadersVisible,
                                  Attr_ScrollBars,
                                  Attr_Level,
                                  Attr_PaddingX,
                                  Attr_PaddingY,
                                  Attr_Resizable,
                                  Attr_SortMode,
                                  Attr_AutoSizeColumnMode,
                                  Attr_UseVisualStyleBackColor,
                                  Attr_EditModeProgramatically,
                                  Attr_ImageScalingSize,
                                  Attr_CloseButtonOnTabsInactiveVisible,
                                  Attr_DefaultNodeImage,
                                  Attr_RowHeadersWidthSizeMode,
                                  Attr_ImageList,
                                  Attr_SelectedIndex,
                                  Attr_RowHeadersBorderStyle,
                                  Attr_Frozen,
                                  Attr_EnableMetric,
                                  Attr_MinMargins,
                                  Attr_WorkerReportsProgress,
                                  Attr_WorkerSupportsCancellation,
                                  Attr_AddNewItem,
                                  Attr_CountItem,
                                  Attr_DeleteItem,
                                  Attr_MoveFirstItem,
                                  Attr_MoveLastItem,
                                  Attr_MoveNextItem,
                                  Attr_MovePreviousItem,
                                  Attr_PositionItem,
                                  Attr_FlowDirection);

   type Widget_Properties (Widget_Type  : Widget_Enum) is record
      Next           : Widget_Pointer := null;
      Prev           : Widget_Pointer := null;
      Child_List     : Widget_Pointer := null; --  only for containers
      Num_Children   : Integer        := 0;    --  only for containers

      Parent_Name    : String_Access  := null;
      WParent        : Window_Pointer := null; --  parent is a window
      GParent        : Widget_Pointer := null; --  parent is a widget
      Child_Num      : Integer        := 0;    --  order in parent's child list

      Windows_Type   : String_Access  := null; --  this is the Windows type

      Name           : String_Access  := null; --  this is the gtk id

      Location       : Location_Pair;
      Size           : Size_Pair;
      TabIndex       : Integer        := -1;
      TabStop        : Triboolean     := Indeterminate;
      Next_Focus     : Widget_Pointer := null;
      Prev_Focus     : Widget_Pointer := null;
      Has_Focus      : Boolean        := False;
      Zorder         : Integer        := -1;

      Enabled        : Boolean        := True;  --  sensitive
      Visible        : Boolean        := True;
      Text           : String_Access  := null;
      TextAlign      : TextAlign_Enum := None;
      AutoSize       : Boolean        := True;
      AutoSizeMode   : AutoSizeMode_Enum := GrowOnly;

      Font_Name      : String_Access  := null;
      Font_Size      : Integer        := Default_Font_Size;
      Font_Weight    : String_Access  := null;
      Font_Underline : Boolean        := False;

      Margins        : Margin_Array   := Null_Margin;
      Padding        : Integer        := 0;
      DStyle         : Display_Style_Enum := Unset; --  labels and buttons
      MaxLength      : Integer        := -1;
      AutoToolTip    : Boolean        := False;
      ToolTip        : String_Access  := null;
      UseVisualStyleBackColor : Boolean := True;
      BgColor        : String_Access  := null;
      FgColor        : String_Access  := null;
      UlColor        : String_Access  := null;
      FlowDirection  : FlowDirection_Enum := LeftToRight;
      Signal_List    : Signal_Pointer;

      case Widget_Type is
         when No_Widget =>
            null;

         when GtkMenuItem | GtkSubMenu => null;
         when GtkSeparatorMenuItem => null;

         when GtkMenuNormalItem | GtkMenuImageItem
            | GtkMenuRadioItem | GtkMenuCheckItem
            =>
            ImageMenuWin : Window_Pointer := null;
            ImageMenu    : String_Access  := null;

         when GtkDataGridView | GtkTreeGridView
            | ExpandableColumn | DataGridViewTextBoxColumn
            | DataGridViewCheckBoxColumn =>
            DefaultCellStyle : Integer := -1;
            ReadOnly         : Boolean := False;

            case Widget_Type is
               when GtkDataGridView | GtkTreeGridView =>
                  ColumnHeadersVisible            : Boolean := True;
                  ColumnHeadersDefaultCellStyle   : Integer := -1;
                  RowHeadersDefaultCellStyle      : Integer := -1;
                  AlternatingRowsDefaultCellStyle : Integer := -1;
                  ColumnHeadersHeightSizeMode
                         : ColumnHeadersHeightSizeMode_Enum := EnableResizing;
                  AllowUserToAddRows            : Boolean := True;
                  AllowUserToDeleteRows         : Boolean := True;
                  AllowUserToResizeRows         : Boolean := True;
                  AllowUserToOrderColumns       : Boolean := False;
                  EnableHeadersVisualStyles     : Boolean := False;
                  RowMultiSelect                : Boolean := True;
                  RowHeadersVisible             : Boolean := True;
                  EditModeProgramatically       : Boolean := False;
                  ImageList                     : Widget_Pointer := null;
                  RowHeadersWidthSizeMode
                              : RowHeadersWidthSizeMode_Enum := EnableResizing;
                  RowHeadersBorderStyle : RowHeadersBorderStyle_Enum := None;
                  ScrollBars : ScrollBars_Enum := Both;
                  Use_Sort   : Boolean := False;
                  Model      : Window_Pointer  := null;

                  case Widget_Type is
                     when GtkDataGridView =>
                        Has_Expander : Boolean := False;
                     when others => null;
                  end case;


               when ExpandableColumn | DataGridViewTextBoxColumn
                  | DataGridViewCheckBoxColumn
                  =>
                  Fixed_Width        : Integer := 100; --  windows default
                  Min_Width          : Integer := 13;  --  windows default 5
                  Max_Width          : Integer := -1;
                  UserAddedColumn    : Boolean := True;
                  Level              : Integer := -1;
                  PaddingX           : Integer := -1;
                  PaddingY           : Integer := -1;
                  Resizable          : Boolean := True;
                  SortMode           : SortMode_Enum := NotSortable;
                  AutoSizeColumnMode : AutoSizeColumnMode_Enum := NotSet;
                  DefaultNodeImage   : Widget_Pointer := null;
                  Frozen             : Boolean := False; --  not horiz. scroll

                  case Widget_Type is
                     when DataGridViewCheckBoxColumn =>
                        CheckBox_Col_Properties : Toggle_Renderer_Properties;
                        Activatable_Column : Integer := -1;
                     when ExpandableColumn | DataGridViewTextBoxColumn =>
                        Text_Col_Properties : Text_Renderer_Properties;
                     when others => null;
                  end case;

               when others => null;
            end case;

         when GtkNoteBook =>
            Scrollable    : Boolean := True;
            Enable_Popups : Boolean := True;
            Show_Tabs     : Boolean := True;
            Show_Border   : Boolean := True;
            Pack_Start    : Boolean := True; -- else pack end
            CloseButtonOnTabsInactiveVisible : Boolean := True;
            SelectedIndex : Integer := -1;

         when GtkTabChild =>
            null;

         when GtkTabPage =>
            --  will be a hbox with label and close button
            The_Label  : Widget_Pointer;
            The_Button : Widget_Pointer;

         when GtkEntry | GtkComboTextBox | GtkCalendar =>
            Buffer      : Window_Pointer;
            Text_Buffer : String_Access := null;
            case Widget_Type is
               when GtkEntry | GtkComboTextBox =>
                  Editable     : Boolean        := True;
                  Has_Frame    : Boolean        := True;
                  PasswordChar : String_Access  := null;
                  case Widget_Type is
                     when GtkComboTextBox =>
                        Sorted : Boolean := False;
                     when others => null;
                  end case;
               when GtkCalendar =>
                  Start_Date  : Ada.Calendar.Time := Ada.Calendar.Clock;
                  MinDate     : Ada.Calendar.Time := AC.Time_Of (1901, 12, 30);
                  MaxDate     : Ada.Calendar.Time := AC.Time_Of (2399, 12, 30);
                  Format_Date : String_Access     := null;
                  ShowUpDown  : Boolean           := True;
               when others => null;
            end case;

         when GtkSpinButton =>
            StartValue   : Integer := 0;
            MaxValue     : Integer := 0;
            MinValue     : Integer := 0;
            Step         : Integer := 1;

         when GtkFileChooserButton
            | PrintDocument | PrintDialog | PageSetupDialog
            | FolderBrowserDialog | GtkToolTip | GtkColorButton
            | GtkStatusBar | GtkToolBar | GtkMenuBar | BackgroundWorker
            | BindingNavigator =>

            TrayLocation   : Location_Pair;
            case Widget_Type is
               when GtkFileChooserButton =>
                  OpenFileDialog : String_Access := null;
                  OpenFileFilter : String_Access := null;
                  OpenFileTitle  : String_Access := null;

               when GtkColorButton =>
                  AnyColor : Boolean := False;
                  FullOpen : Boolean := False;
                  Associated_Button : Widget_Pointer;

               when GtkToolBar | GtkMenuBar | BindingNavigator =>
                  ImageScalingSize : Pair;
                  case Widget_Type is
                     when GtkToolBar | BindingNavigator =>
                        TB_Horiz         : Boolean       := True;
                        Show_Arrows      : Boolean       := True;
                        Grip_Visible     : Boolean       := True;
                        case Widget_Type is
                           when BindingNavigator =>
                              AddNewItem       : Widget_Pointer := null;
                              CountItem        : Widget_Pointer := null;
                              DeleteItem       : Widget_Pointer := null;
                              MoveFirstItem    : Widget_Pointer := null;
                              MoveLastItem     : Widget_Pointer := null;
                              MoveNextItem     : Widget_Pointer := null;
                              MovePreviousItem : Widget_Pointer := null;
                              PositionItem     : Widget_Pointer := null;
                           when others =>
                              null;
                        end case;
                     when others => null;
                  end case;

               when PageSetupDialog =>
                  EnableMetric : Boolean := False;
                  MinMargins   : Margin_Array;

               when BackgroundWorker =>
                  WorkerReportsProgress      : Boolean := False;
                  WorkerSupportsCancellation : Boolean := False;

               when others =>
                  null;
            end case;

         when GtkListBox =>
            MultiSelect : Boolean := False;
            ListStore   : Window_Pointer := null;

         when GtkImage =>
            Image : String_Access := null;

         when GtkButton | GtkRadioButton | GtkCheckButton | GtkToggleButton
              | GtkLabel | ToolStripStatusLabel
            =>
            Underline   : Boolean := False;

            case Widget_Type is
               when GtkLabel | ToolStripStatusLabel =>
                  BorderStyle : BorderStyle_Enum := None;

               when GtkButton | GtkRadioButton
                  | GtkCheckButton | GtkToggleButton
                  =>
                  Active      : Boolean             := True;
                  ImagePath   : String_Access       := null;
                  ImageAlign  : Image_Position_Enum := Left;
                  Win_Image   : Window_Pointer      := null;

                  case Widget_Type is
                     when GtkButton =>
                        Dialog_Result : DialogResult_Enum := None;
                        Associated_ColorButton : Widget_Pointer;

                     when GtkCheckButton =>
                        CheckAlign : String_Access  := null;

                     when others => null;
                  end case;
               when others => null;
            end case;

         when GtkFrame => null;

         when GtkBox => null;

         when Chart =>
            Anchor : String_Access := null;

         when GtkSeparatorToolItem => null;

      end case;
   end record;

   Use_Debug : Boolean := True;

   function Sp (N : Integer) return String;

   procedure Debug (NLin : Integer; Msg : String);

   function Img (X : Integer) return String;
   function Img (X : Float) return String;

   function Find_Widget (Start : Widget_Pointer;
                         WType : Widget_Enum) return Widget_Pointer;
   function Find_Widget (Start : Widget_Pointer;
                         Name  : String) return Widget_Pointer;
   --  do not call this procedure after Reparent_Widgets

   procedure Release (WT : in out Widget_Pointer);
   procedure Replace_Parent_By_Child (Parent : in out Widget_Pointer;
                                      Child  : Widget_Pointer);
   procedure Relink_Children_To_Parent (TWin : Window_Pointer);
   procedure Process_Inheritable (TWin : Window_Pointer);

   procedure Copy_Common_Attributes (From : Widget_Pointer;
                                     To   : Widget_Pointer);
   procedure Replace (TWin   : Window_Pointer;
                      OldWdg : Widget_Pointer;
                      NewWdg : Widget_Pointer);
   procedure Replace (Parent : Widget_Pointer; --  parent widget
                      OldWdg : Widget_Pointer;
                      NewWdg : Widget_Pointer);
   procedure Insert_Widget_By_Tail (Parent : Window_Pointer;
                                    WT     : Widget_Pointer);
   procedure Insert_Signal (TWin : Window_Pointer;
                            TS   : Signal_Pointer);
   procedure Insert_Signal (TWdg : Widget_Pointer;
                            TS   : Signal_Pointer);
   function Signal_Exists (TWin : Window_Pointer;
                           Signal_Name : String) return Boolean;
   function Signal_Exists (TWdg        : Widget_Pointer;
                           Signal_Name : String) return Boolean;
   procedure Insert_Focus (Into : Window_Pointer; Focus : Widget_Pointer);

   procedure Insert_Window_By_Tail (TWin : Window_Pointer);
   procedure Insert_Window_By_Front (TWin : Window_Pointer);

   function Contains (Source : String; Pattern : String) return Boolean;
   function Get_Integer (Data : String) return Integer;
   function Get_Pair (Data : String) return Pair;
   function Get_String (F : TIO.File_Type) return String;
   function Convert (WStyle : String) return Display_Style_Enum;
   function Convert (IAlign : String) return Image_Position_Enum;
   function To_Cell_Format_Enum (Str : String) return Cell_Format_Enum;
   function To_TextAlign (IAlign : String) return TextAlign_Enum;
   function To_Color (WColor : String) return String;
   function To_AutoSizeColumnMode (WLine : String)
                                   return AutoSizeColumnMode_Enum;
   function Icon_Name (Src : String) return String;
   function Get_Widget_Name (F : TIO.File_Type) return String;
   function Get_Margin_Array (Data : String) return Margin_Array;
   function Get_Margin_Array (F : TIO.File_Type) return Margin_Array;
   function Get_Boolean (Data : String) return Boolean;
   function Get_Boolean (F : TIO.File_Type) return Boolean;
   procedure Get_Font (Data           : in String;
                       Font_Name      : in out String_Access;
                       Font_Size      : in out Integer;
                       Font_Weight    : in out String_Access;
                       Font_Underline : in out Boolean);
   procedure Get_Font (F              : TIO.File_Type;
                       Font_Name      : in out String_Access;
                       Font_Size      : in out Integer;
                       Font_Weight    : in out String_Access;
                       Font_Underline : in out Boolean);
   function Num_Children (TWdg : Widget_Pointer) return Integer;
   function Normalize_Name (TWdg : Widget_Pointer) return String;
   function To_Gtk (T : Window_Pointer) return String;
   function To_Gtk (T       : Widget_Pointer;
                    For_Ada : Boolean := False) return String;
   function To_Gtk (D : DialogResult_Enum) return String;
   function "+" (Str : String) return String is
      (GNATCOLL.Utils.Capitalize (Str));

   RFile : TIO.File_Type; --  resource     (in)
   DFile : TIO.File_Type; --  designer     (in)
   VFile : TIO.File_Type; --  visual basic (in)
   GFile : TIO.File_Type; --  glade        (out)
   LFile : TIO.File_Type; --  dump         (out)

   Line   : String (1 .. 1024);
   Len    : Natural;
   NLin   : Integer;
end W2gtk_Decls;
