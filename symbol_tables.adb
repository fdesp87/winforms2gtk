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
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package body Symbol_Tables is

   ------------
   -- Common --
   ------------

   function Hash (Str : String) return Ada.Containers.Hash_Type;
   function Hash (Str : String) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (Str);
   end Hash;
   function Key_Equal (Key1, Key2 : String) return Boolean;
   function Key_Equal (Key1, Key2 : String) return Boolean is
   begin
      return Key1 = Key2;
   end Key_Equal;

   -----------------
   -- GTK Signals --
   -----------------

   package Signal_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => String,
      Hash            => Hash,
      Equivalent_Keys => Key_Equal,
      "="             => "=");

   Signal_Symbol_Table : Signal_Map.Map;

   procedure Initialize_Signal_Symbol_Table;
   procedure Initialize_Signal_Symbol_Table is
      Wst : Signal_Map.Map renames Signal_Symbol_Table;
   begin
      Wst.Insert ("Load", "realize");
      Wst.Insert ("FormClosing", "destroy");
      Wst.Insert ("TextChanged", "changed");          --  gtkentry gtkcombobox
      Wst.Insert ("SelectedIndexChanged", "changed"); --  combobox
      Wst.Insert ("ValueChanged", "value-changed");   --  spin
      Wst.Insert ("SelectedValueChanged", "changed"); --  combobox
      Wst.Insert ("CheckedChanged", "toggled");       --  checkbutton
      Wst.Insert ("Checked", "toggled");              --  radiobutton
      Wst.Insert ("Click", "clicked");
      Wst.Insert ("BeginPrint", "begin-print");
      Wst.Insert ("PrintPage", "print-page");
      Wst.Insert ("MouseClick", "clicked");
      Wst.Insert ("Leave", "leave-notify-event");
      Wst.Insert ("FileOk", "file-set");
      Wst.Insert ("MouseDoubleClick", "clicked");
      Wst.Insert ("CloseButtonClick", "clicked");
      Wst.Insert ("Selected", "clicked");
      Wst.Insert ("CellMouseEnter", "clicked");
      Wst.Insert ("CellMouseLeave", "clicked");
      Wst.Insert ("CellMouseClick", "clicked");
      Wst.Insert ("toggled", "toggled");
      Wst.Insert ("leave", "focus");
      Wst.Insert ("DoWork", "DoWork");
      Wst.Insert ("ProgressChanged", "ProgressChanged");
      Wst.Insert ("RunWorkerCompleted", "RunWorkerCompleted");
   end Initialize_Signal_Symbol_Table;

   function Get_Gtk_Signal (WSignal : String) return String
   is
      use Signal_Map;
      Crs : Cursor;
   begin
      Crs := Signal_Symbol_Table.Find (WSignal);
      if Crs = No_Element then
         return "";
      else
         return Element (Crs);
      end if;
   end Get_Gtk_Signal;

   function Convert_Signal_To_Gtk (TWin    : Window_Pointer;
                                   WSignal : String) return String is
      pragma Unreferenced (TWin);
      Gtk_Signal : constant String := Get_Gtk_Signal (WSignal);
   begin
      if Gtk_Signal = "" then
         raise Unknown_Signal;
      end if;
      return Gtk_Signal;
   end Convert_Signal_To_Gtk;

   function Convert_Signal_To_Gtk (TWdg    : Widget_Pointer;
                                   WSignal : String) return String is
      Gtk_Signal : constant String := Get_Gtk_Signal (WSignal);
   begin
      if Gtk_Signal = "" then
         raise Unknown_Signal;
      end if;
      case TWdg.Widget_Type is
         when GtkDataGridView | GtkTreeGridView =>
            if WSignal = "CellMouseClick" then
               return "cursor-changed";
            elsif WSignal = "CellMouseEnter" then
               return "cursor-changed";
            elsif WSignal = "CellMouseLeave" then
               return "cursor-changed";
            end if;
         when DataGridViewCheckBoxColumn =>
            if WSignal = "toggled" then
               return "clicked";
            end if;
         when GtkNoteBook =>
            if WSignal = "Selected" then
               return "switch-page";
            elsif WSignal = "MouseDoubleClick" then
               return "switch-page";
            elsif WSignal = "CloseButtonClick" then
               return "switch-page";
            end if;
         when GtkMenuBar =>
            if WSignal = "Click" then
               return "activate-current";
            end if;
         when GtkMenuItem | GtkSeparatorMenuItem
            | GtkMenuNormalItem | GtkMenuImageItem
            | GtkMenuRadioItem | GtkMenuCheckItem =>
            if WSignal = "Click" then
               return "activate";
            end if;
         when others => null;
      end case;
      return Gtk_Signal;
   end Convert_Signal_To_Gtk;

   ---------------------
   -- DGVS Attributes --
   ---------------------

   package DGVS_Attributes_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => DGVS_Attribute_Enum,
      Hash            => Hash,
      Equivalent_Keys => Key_Equal,
      "="             => "=");

   DGVS_Attributes_Symbol_Table : DGVS_Attributes_Map.Map;

   procedure Initialize_DGVS_Attributes_Symbol_Table;
   procedure Initialize_DGVS_Attributes_Symbol_Table is
      Wst : DGVS_Attributes_Map.Map renames DGVS_Attributes_Symbol_Table;
   begin
      Wst.Insert ("Alignment", DGVS_Attr_Alignment);
      Wst.Insert ("BackColor", DGVS_Attr_BgColor);
      Wst.Insert ("ForeColor", DGVS_Attr_FgColor);
      Wst.Insert ("SelectionBackColor", DGVS_Attr_SelBgColor);
      Wst.Insert ("SelectionForeColor", DGVS_Attr_SelFgColor);
      Wst.Insert ("Font", DGVS_Attr_Font);
      Wst.Insert ("Format", DGVS_Attr_Format);
      Wst.Insert ("Padding", DGVS_Attr_Padding);
      Wst.Insert ("WrapMode", DGVS_Attr_WrapMode);
      Wst.Insert ("NullValue", DGVS_Attr_NullValue);
   end Initialize_DGVS_Attributes_Symbol_Table;

   function Get_DGVS_Attribute (DGVS_Attr : String) return DGVS_Attribute_Enum
   is
      use DGVS_Attributes_Map;
      Crs : Cursor;
   begin
      Crs := DGVS_Attributes_Symbol_Table.Find (DGVS_Attr);
      if Crs = No_Element then
         return DGVS_No_Attribute;
      else
         return Element (Crs);
      end if;
   end Get_DGVS_Attribute;

   -----------------------
   -- Widget Attributes --
   -----------------------

   package Widgets_Attributes_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Widget_Attribute_Enum,
      Hash            => Hash,
      Equivalent_Keys => Key_Equal,
      "="             => "=");

   Widgets_Attributes_Symbol_Table : Widgets_Attributes_Map.Map;

   procedure Initialize_Widgets_Attributes_Symbol_Table;
   procedure Initialize_Widgets_Attributes_Symbol_Table is
      Wst : Widgets_Attributes_Map.Map renames Widgets_Attributes_Symbol_Table;
   begin
      Wst.Insert ("Anchor", Attr_Anchor);
      Wst.Insert ("ZOrder", Attr_ZOrder);
      Wst.Insert ("Margin", Attr_Margin);
      Wst.Insert ("Padding", Attr_Padding);
      Wst.Insert ("TabIndex", Attr_TabIndex);
      Wst.Insert ("Text", Attr_Text);
      Wst.Insert ("MaxLength", Attr_MaxLength);
      Wst.Insert ("ToolTip", Attr_ToolTip);
      Wst.Insert ("ToolTipText", Attr_ToolTipText);
      Wst.Insert ("AutoToolTip", Attr_AutoToolTip);
      Wst.Insert ("AutoSize", Attr_AutoSize);
      Wst.Insert ("Enabled", Attr_Enabled);
      Wst.Insert ("Visible", Attr_Visible);
      Wst.Insert ("TextAlign", Attr_TextAlign);
      Wst.Insert ("TextBoxTextAlign", Attr_BoxTextAlign);
      Wst.Insert ("CheckAlign", Attr_CheckAlign);
      Wst.Insert ("Name", Attr_Name);
      Wst.Insert ("Size", Attr_Size);
      Wst.Insert ("Location", Attr_Location);
      Wst.Insert ("Type", Attr_Type);
      Wst.Insert ("Parent", Attr_Parent);
      Wst.Insert ("Font", Attr_Font);
      Wst.Insert ("PasswordChar", Attr_PasswordChar);
      Wst.Insert ("OpenFile", Attr_OpenFile);
      Wst.Insert ("TrayLocation", Attr_TrayLocation);
      Wst.Insert ("Filter", Attr_Filter);
      Wst.Insert ("Title", Attr_Title);
      Wst.Insert ("ImageAlign", Attr_ImageAlign);
      Wst.Insert ("Width", Attr_FixedWidth);
      Wst.Insert ("MinimumWidth", Attr_MinimumWidth);
      Wst.Insert ("MaximumWidth", Attr_MaximumWidth);
      Wst.Insert ("HeaderText", Attr_HeaderText);
      Wst.Insert ("FlowDirection", Attr_FlowDirection);
      Wst.Insert ("ItemSize", Attr_ItemSize);
      Wst.Insert ("DisplayStyle", Attr_DisplayStyle);
      Wst.Insert ("TabStop", Attr_TabStop);
      Wst.Insert ("BorderStyle", Attr_BorderStyle);
      Wst.Insert ("Alignment", Attr_Alignment);
      Wst.Insert ("GripStyle", Attr_GripStyle);
      Wst.Insert ("Checked", Attr_Checked);
      Wst.Insert ("CheckState", Attr_CheckState);
      Wst.Insert ("AnyColor", Attr_AnyColor);
      Wst.Insert ("Items.AddRange", Attr_Items_AddRange);
      Wst.Insert ("Columns.AddRange", Attr_Columns_AddRange);
      Wst.Insert ("DropDownItems.AddRange", Attr_DropDownItems_AddRange);
      Wst.Insert ("FullOpen", Attr_FullOpen);
      Wst.Insert ("UseSystemPasswordChar", Attr_UseSystemPasswordChar);
      Wst.Insert ("Sorted", Attr_Sorted);
      Wst.Insert ("Maximum", Attr_Maximum);
      Wst.Insert ("Minimum", Attr_Minimum);
      Wst.Insert ("Value", Attr_Value);
      Wst.Insert ("Format", Attr_Format);
      Wst.Insert ("MinDate", Attr_MinDate);
      Wst.Insert ("MaxDate", Attr_MaxDate);
      Wst.Insert ("LimitDatePicket", Attr_LimitDatePicket);
      Wst.Insert ("Image", Attr_Image);
      Wst.Insert ("ShowUpDown", Attr_ShowUpDown);
      Wst.Insert ("SelectionMode", Attr_SelectionMode);
      Wst.Insert ("ForeColor", Attr_ForeColor);
      Wst.Insert ("BackColor", Attr_BackColor);
      Wst.Insert ("UserAddedColumn", Attr_UserAddedColumn);
      Wst.Insert ("AllowUserToAddRows", Attr_AllowUserToAddRows);
      Wst.Insert ("AllowUserToDeleteRows", Attr_AllowUserToDeleteRows);
      Wst.Insert ("AllowUserToResizeRows", Attr_AllowUserToResizeRows);
      Wst.Insert ("AllowUserToOrderColumns", Attr_AllowUserToOrderColumns);
      Wst.Insert ("RowHeadersDefaultCellStyle",
                  Attr_RowHeadersDefaultCellStyle);
      Wst.Insert ("ColumnHeadersDefaultCellStyle",
                  Attr_ColumnHeadersDefaultCellStyle);
      Wst.Insert ("ColumnHeadersHeightSizeMode",
                  Attr_ColumnHeadersHeightSizeMode);
      Wst.Insert ("DefaultCellStyle", Attr_DefaultCellStyle);
      Wst.Insert ("EnableHeadersVisualStyles", Attr_EnableHeadersVisualStyles);
      Wst.Insert ("MultiSelect", Attr_MultiSelect);
      Wst.Insert ("ScrollBars", Attr_ScrollBars);
      Wst.Insert ("ReadOnly", Attr_ReadOnly);
      Wst.Insert ("ColumnHeadersVisible", Attr_ColumnHeadersVisible);
      Wst.Insert ("RowHeadersVisible", Attr_RowHeadersVisible);
      Wst.Insert ("Level", Attr_Level);
      Wst.Insert ("PaddingX", Attr_PaddingX);
      Wst.Insert ("PaddingY", Attr_PaddingY);
      Wst.Insert ("Resizable", Attr_Resizable);
      Wst.Insert ("SortMode", Attr_SortMode);
      Wst.Insert ("AutoSizeMode", Attr_AutoSizeMode);
      Wst.Insert ("AutoSizeColumnMode", Attr_AutoSizeMode);
      Wst.Insert ("UseVisualStyleBackColor", Attr_UseVisualStyleBackColor);
      Wst.Insert ("EditMode", Attr_EditModeProgramatically);
      Wst.Insert ("ImageScalingSize", Attr_ImageScalingSize);
      Wst.Insert ("CloseButtonOnTabsInactiveVisible",
                  Attr_CloseButtonOnTabsInactiveVisible);
      Wst.Insert ("DefaultNodeImage", Attr_DefaultNodeImage);
      Wst.Insert ("RowHeadersWidthSizeMode", Attr_RowHeadersWidthSizeMode);
      Wst.Insert ("ImageList", Attr_ImageList);
      Wst.Insert ("SelectedIndex", Attr_SelectedIndex);
      Wst.Insert ("RowHeadersBorderStyle", Attr_RowHeadersBorderStyle);
      Wst.Insert ("Frozen", Attr_Frozen);
      Wst.Insert ("EnableMetric", Attr_EnableMetric);
      Wst.Insert ("MinMargins", Attr_MinMargins);
      Wst.Insert ("WorkerReportsProgress", Attr_WorkerReportsProgress);
      Wst.Insert ("WorkerSupportsCancellation",
                  Attr_WorkerSupportsCancellation);
      Wst.Insert ("AddNewItem", Attr_AddNewItem);
      Wst.Insert ("CountItem", Attr_CountItem);
      Wst.Insert ("DeleteItem", Attr_DeleteItem);
      Wst.Insert ("MoveFirstItem", Attr_MoveFirstItem);
      Wst.Insert ("MoveLastItem", Attr_MoveLastItem);
      Wst.Insert ("MoveNextItem", Attr_MoveNextItem);
      Wst.Insert ("MovePreviousItem", Attr_MovePreviousItem);
      Wst.Insert ("PositionItem", Attr_PositionItem);

      Wst.Insert ("HorizontalScrollbar", Attr_Ignored);
      Wst.Insert ("ItemHeight", Attr_Ignored);
      Wst.Insert ("Dock", Attr_Ignored);
      Wst.Insert ("ImeMode", Attr_Ignored);
      Wst.Insert ("WaitOnLoad", Attr_Ignored);
      Wst.Insert ("SizeMode", Attr_Ignored);
      Wst.Insert ("RightToLeft", Attr_Ignored);
      Wst.Insert ("TextImageRelation", Attr_Ignored);
      Wst.Insert ("ImageTransparentColor", Attr_Ignored);
      Wst.Insert ("BackgroundImageLayout", Attr_Ignored);

      Wst.Insert ("Cursor", Attr_Ignored);
      Wst.Insert ("Tag", Attr_Ignored);
      Wst.Insert ("FormattingEnabled", Attr_Ignored);
      Wst.Insert ("DropDownStyle", Attr_Ignored);
      Wst.Insert ("DialogResult", Attr_Ignored);
      Wst.Insert ("AllowDrop", Attr_Ignored);
      Wst.Insert ("FlatStyle", Attr_Ignored);
      Wst.Insert ("ShowNetwork", Attr_Ignored);
      Wst.Insert ("UseExDialog", Attr_Ignored);
      Wst.Insert ("AutoCheck", Attr_Ignored);
      Wst.Insert ("UseWaitCursor", Attr_Ignored);
      Wst.Insert ("ShowNewFolderButton", Attr_Ignored);
      Wst.Insert ("RootFolder", Attr_Ignored);
      Wst.Insert ("DefaultExt", Attr_Ignored);
      Wst.Insert ("CheckFileExists", Attr_Ignored);
      Wst.Insert ("IsBalloon", Attr_Ignored);
      Wst.Insert ("UseEXDialog", Attr_Ignored);
   end Initialize_Widgets_Attributes_Symbol_Table;

   function Get_Attribute (Wdg_Attr : String) return Widget_Attribute_Enum is
      use Widgets_Attributes_Map;
      Crs : Cursor;
   begin
      Crs := Widgets_Attributes_Symbol_Table.Find (Wdg_Attr);
      if Crs = No_Element then
         return No_Attribute;
      else
         return Element (Crs);
      end if;
   end Get_Attribute;

   ------------------------
   -- Windows Properties --
   ------------------------

   package Windows_Properties_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Window_Property_Enum,
      Hash            => Hash,
      Equivalent_Keys => Key_Equal,
      "="             => "=");

   Windows_Properties_Symbol_Table : Windows_Properties_Map.Map;

   procedure Initialize_Windows_Properties_Symbol_Table;
   procedure Initialize_Windows_Properties_Symbol_Table is
      Wst : Windows_Properties_Map.Map renames Windows_Properties_Symbol_Table;
   begin
      Wst.Insert ("ClientSize", Str_ClientSize);
      Wst.Insert ("AutoScaleDimensions", Str_AutoScaleDimensions);
      Wst.Insert ("Icon", Str_Icon);
      Wst.Insert ("Margin", Str_Margin);
      Wst.Insert ("Font", Str_Font);
      Wst.Insert ("Type", Str_Type);
      Wst.Insert ("ToolTip", Str_ToolTip);
      Wst.Insert ("StartPosition", Str_StartPosition);
      Wst.Insert ("TrayHeight", Str_TrayHeight);
      Wst.Insert ("Text", Str_Text);
      Wst.Insert ("Name", Str_Name);
      Wst.Insert ("RightToLeft", Str_RightToLeft);
      Wst.Insert ("AcceptButton", Str_Accept_Button);
      Wst.Insert ("CancelButton", Str_Cancel_Button);
   end Initialize_Windows_Properties_Symbol_Table;

   function Get_Property (Win_Prop : String) return Window_Property_Enum is
      use Windows_Properties_Map;
      Crs : Cursor;
   begin
      Crs := Windows_Properties_Symbol_Table.Find (Win_Prop);
      if Crs = No_Element then
         return No_Property;
      else
         return Element (Crs);
      end if;
   end Get_Property;

   ----------------
   -- Widget Map --
   ----------------

   package Widget_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Widget_Enum,
      Hash            => Hash,
      Equivalent_Keys => Key_Equal,
      "="             => "=");

   Widget_Symbol_Table : Widget_Map.Map;

   procedure Initialize_Widget_Symbol_Table;
   procedure Initialize_Widget_Symbol_Table is
      Wst : Widget_Map.Map renames Widget_Symbol_Table;
   begin
      Wst.Insert ("TreeGridView", GtkTreeGridView);
      Wst.Insert ("ExpandableColumn", ExpandableColumn);
      Wst.Insert ("DataGridViewTextBoxColumn", DataGridViewTextBoxColumn);
      Wst.Insert ("DataGridViewCheckBoxColumn", DataGridViewCheckBoxColumn);
      Wst.Insert ("DataGridView", GtkDataGridView);
      Wst.Insert ("TreeGridColumn", ExpandableColumn);
      Wst.Insert ("Label", GtkLabel);
      Wst.Insert ("ToolStripLabel", GtkLabel);
      Wst.Insert ("PictureBox", GtkImage);
      Wst.Insert ("NumericUpDown", GtkSpinButton);
      Wst.Insert ("ToolTip", GtkToolTip);
      Wst.Insert ("GroupBox", GtkFrame);
      Wst.Insert ("ListBox", GtkListBox);
      Wst.Insert ("TextBox", GtkEntry);
      Wst.Insert ("ToolStripTextBox", GtkEntry);
      Wst.Insert ("ComboBox", GtkComboBox);
      Wst.Insert ("ToolStripComboBox", GtkComboBox);
      Wst.Insert ("RadioButton", GtkRadioButton);
      Wst.Insert ("ToggleButton", GtkToggleButton);
      Wst.Insert ("ToolStripButton", GtkButton);
      Wst.Insert ("ColorDialog", GtkColorButton);
      Wst.Insert ("Button", GtkButton);
      Wst.Insert ("CheckBox", GtkCheckButton);
      Wst.Insert ("Panel", GtkFrame);
      Wst.Insert ("DateTimePicker", GtkCalendar);
      Wst.Insert ("PrintDocument", PrintDocument);
      Wst.Insert ("PrintDialog", PrintDialog);
      Wst.Insert ("PageSetupDialog", PageSetupDialog);
      Wst.Insert ("OpenFileDialog", GtkFileChooserButton);
      Wst.Insert ("FolderBrowserDialog", FolderBrowserDialog);
      Wst.Insert ("StatusStrip", GtkStatusBar);
      Wst.Insert ("ToolStripStatusLabel", ToolStripStatusLabel);
      Wst.Insert ("ToolStripSeparator", GtkSeparatorToolItem);
      Wst.Insert ("Chart", Chart);
      Wst.Insert ("MenuStrip", GtkMenuBar);
      Wst.Insert ("MenuStripItem", GtkMenuItem);
      Wst.Insert ("ToolStripMenuItem", GtkMenuImageItem);
      Wst.Insert ("ToolStrip", GtkToolBar);
      Wst.Insert ("TabControl", GtkNoteBook);
      Wst.Insert ("TabControlEx", GtkNoteBook);
      Wst.Insert ("TabPage", GtkTabChild);
      Wst.Insert ("FlowLayoutPanel", GtkBox);
      Wst.Insert ("BackgroundWorker", BackgroundWorker);
      Wst.Insert ("BindingNavigator", BindingNavigator);
   end Initialize_Widget_Symbol_Table;

   function Get_Type (Wdg_Type : String) return Widget_Enum is
      use Widget_Map;
      Crs : Cursor;
   begin
      Crs := Widget_Symbol_Table.Find (Wdg_Type);
      if Crs = No_Element then
         return No_Widget;
      else
         return Element (Crs);
      end if;
   end Get_Type;

   -----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Initialize_Signal_Symbol_Table;
      Initialize_Widget_Symbol_Table;
      Initialize_Windows_Properties_Symbol_Table;
      Initialize_Widgets_Attributes_Symbol_Table;
      Initialize_DGVS_Attributes_Symbol_Table;
   end Initialize;

end Symbol_Tables;
