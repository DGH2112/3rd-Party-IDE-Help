(**

  This module contains a frame for presenting the 3rd Part Help that is installed in the IDE.

  @Author  David Hoyle
  @Version 1.098
  @Date    29 May 2020

  @license

    3rd Party Help is a RAD Studio IDE plug-in that allows you to easily install
    additional 3rd Party HTML Help files into the IDE to provide context
    sensitive help for libraries and components.

    Copyright (C) 2019  David Hoyle (https://github.com/DGH2112/Delphi-IDE-Explorer)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

**)
Unit TPIDEHelp.IDEOptionsFrame;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ImgList,
  TPIDEHelp.Interfaces, System.ImageList;

Type
  (** A frame to contain the plug-ins options in the IDE options dialogue. **)
  TframeTPIDEHelpOptions = Class(TFrame, ITPHelpOptionsFrame)
    btnAdd: TButton;
    btnEdit: TButton;
    btnDelete: TButton;
    ilButtons: TImageList;
    lvHelp: TListView;
    procedure lvHelpSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure lvHelpCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
  Strict Private
  Strict Private
    FHelpList : ITPHCustomHelpList;
  Strict Protected
    // ITPHelpOptionsFrame
    Procedure InitialiseFrame;
    Procedure FinaliseFrame;
    // General Methods
    Procedure PopulateListView;
  Public
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
  End;

Implementation

{$R *.dfm}

Uses
  ToolsAPI,
  TPIDEHelp.HelpEntryForm,
  TPIDEHelp.ToolsAPIFunctions,
  TPIDEHelp.CustomHelpList;

(**

  This is an on click event handler for the Add button.

  @precon  None.
  @postcon Promts the user to add a CHM file to the list of files.

  @param   Sender as a TObject

**)
procedure TframeTPIDEHelpOptions.btnAddClick(Sender: TObject);

Var
  strName, strFileName : String;

begin
  strName := '';
  strFileName := '';
  If TfrmHelpEntry.Execute(strName, strFileName, FHelpList.UsedHelpNames) Then
    Begin
      FHelpList.Add(strName, strFileName);
      PopulateListView;
    End;
end;

(**

  This is an on click event handler for the Delete button.

  @precon  None.
  @postcon Deletes the selected CHM file from the list.

  @param   Sender as a TObject

**)
Procedure TframeTPIDEHelpOptions.btnDeleteClick(Sender: TObject);

Begin
  FHelpList.Delete(lvHelp.Selected.Caption);
  PopulateListView;
End;

(**

  This is an on click event handler for the Edit button.

  @precon  None.
  @postcon Allows the user to edit the selected CHM file.

  @param   Sender as a TObject

**)
Procedure TframeTPIDEHelpOptions.btnEditClick(Sender: TObject);

Var
  strOldName, strNewName, strFileName : String;

Begin
  strOldName := lvHelp.Selected.Caption;
  strNewName := strOldName;
  strFileName := FHelpList.Filename[strOldName];
  If TfrmHelpEntry.Execute(strNewName, strFileName, FHelpList.UsedHelpNames) Then
    Begin
      FHelpList.Update(strOldName, strNewName, strFileName);
      PopulateListView;
    End;
End;

(**

  A constructor for the TframeTPIDEHelpOptions class.

  @precon  None.
  @postcon Creates a generic collection

  @nocheck MissingCONSTInParam

  @param   AOwner as a TComponent

**)
Constructor TframeTPIDEHelpOptions.Create(AOwner : TComponent);

Begin
  Inherited Create(AOwner);
  FHelpList := TTPHCustomHelpList.Create;
End;

(**

  A destructor for the TframeTPHelpOptions class.

  @precon  None.
  @postcon Frees the generic collection.

**)
Destructor TframeTPIDEHelpOptions.Destroy;

Begin
  FHelpList := Nil;
  Inherited Destroy;
End;

(**

  This method makes the changes that have been asked for in the dialogue to the Help information
  in the registry.

  @precon  None.
  @postcon The RAD Studio registry is updated with the list of custom help information.

**)
Procedure TframeTPIDEHelpOptions.FinaliseFrame;

Begin
  FHelpList.SaveListChanges;
End;

(**

  This method initialises the frame with 3rd party help items from the RAD Studio regsitry.

  @precon  None.
  @postcon The list view is populates with help items.

**)
Procedure TframeTPIDEHelpOptions.InitialiseFrame;

Begin
  PopulateListView;
  lvHelpSelectItem(lvHelp, Nil, False);
End;

(**

  This is an on CustomDraweItem for the Help listview.

  @precon  None.
  @postcon Rendered and Help files that cannot be found in GrayText.

  @param   Sender      as a TCustomListView
  @param   Item        as a TListItem
  @param   State       as a TCustomDrawState
  @param   DefaultDraw as a Boolean as a reference

**)
Procedure TframeTPIDEHelpOptions.lvHelpCustomDrawItem(Sender: TCustomListView; Item: TListItem;
  State: TCustomDrawState; Var DefaultDraw: Boolean);

Begin
  Sender.Canvas.Font.Color := TTPHelpToolsAPIFunctions.ThemeColour(clWindowText);
  If Not FileExists(Item.SubItems[0]) Then
    Sender.Canvas.Font.Color := TTPHelpToolsAPIFunctions.ThemeColour(clGrayText);
End;

(**

  This is an on select item event handler for the listview.

  @precon  None.
  @postcon Updates the enalbed proprty of the Edit and Delete buttons.

  @param   Sender   as a TObject
  @param   Item     as a TListItem
  @param   Selected as a Boolean

**)
Procedure TframeTPIDEHelpOptions.lvHelpSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);

Begin
  btnEdit.Enabled := lvHelp.ItemIndex > -1;
  btnDelete.Enabled := btnEdit.Enabled;
End;

(**

  This method populates the list view with the information in the generic collection.

  @precon  None.
  @postcon the listview is populated with the contents of the generic collection.

**)
Procedure TframeTPIDEHelpOptions.PopulateListView;

Var
  iHelp: Integer;
  Item: TListItem;
  iSelectedIndex : Integer;

Begin
  lvHelp.Items.BeginUpdate;
  Try
    iSelectedIndex := lvHelp.ItemIndex;
    lvHelp.Clear;
    For iHelp := 0 To FHelpList.Count - 1 Do
      Begin
        Item := lvHelp.Items.Add;
        Item.Caption := FHelpList.Name[iHelp];
        Item.SubItems.Add(FHelpList.Filename[Item.Caption]);
      End;
    If iSelectedIndex >= lvHelp.items.Count Then
      iSelectedIndex := lvHelp.Items.Count - 1;
    lvHelp.ItemIndex := iSelectedIndex;
  Finally
    lvHelp.Items.EndUpdate;
  End;
End;

End.
