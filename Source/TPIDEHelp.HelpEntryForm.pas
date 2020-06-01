(**
  
  This module contains a form class for querying the user for the name and file path of a CHM help
  file to be include in the IDEs list of help file.

  @Author  David Hoyle
  @Version 1.097
  @Date    29 May 2020

  @license

    3rd Party Help is a RAD Studio IDE plug-in that allows you to easily install
    additional 3rd Party HTML Help files into the IDE to provide context
    sensitive help for libraries and components.

    Copyright (C) 2020  David Hoyle (https://github.com/DGH2112/Delphi-IDE-Explorer)

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
Unit TPIDEHelp.HelpEntryForm;

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
  Vcl.ImgList,
  Vcl.ExtCtrls, System.ImageList;

Type
  (** A form class to allow the editing of a CHM file. **)
  TfrmHelpEntry = Class(TForm)
    lblName: TLabel;
    edtName: TEdit;
    lblFilename: TLabel;
    edtFilename: TEdit;
    btnBrowse: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    ilButtons: TImageList;
    pnlThemeFudge: TPanel;
    dlgFileOpen: TFileOpenDialog;
    procedure btnOKClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
  Strict Private
    FIDENames : TStringList;
  Strict Protected
  Public
    Class Function Execute(Var strName, strFileName : String; Const slIDENames : TStringList) : Boolean;
  End;

Implementation

{$R *.dfm}

Uses
  ToolsAPI,
  TPIDEHelp.ToolsAPIFunctions;

(**

  This is an on click event handler for the Browse button.

  @precon  None.
  @postcon Allows the user to browse for a CHM file.

  @param   Sender as a TObject

**)
Procedure TfrmHelpEntry.btnBrowseClick(Sender: TObject);

Var
  strPath: String;


Begin
  strPath := ExtractFilePath(edtFilename.Text);
  While (Length(strPath) > 0) and Not DirectoryExists(strPath)  Do
    Begin
      If (Length(strPath) > 0) And (strPath[Length(strPath)] = '\') Then
        Delete(strPath, Length(strPath), 1);
      strPath := ExtractFilePath(strPath);
    End;
 dlgFileOpen.DefaultFolder := strPath;
  If dlgFileOpen.Execute Then
    Begin
      edtFilename.Text := dlgFileOpen.FileName;
      edtName.Text := ChangeFileExt(ExtractFileName(dlgFileOpen.FileName), '');
    End;
End;

(**

  This is an on click event handler for the OK button.

  @precon  None.
  @postcon Validate the contents of the dialogue.

  @param   Sender as a TObject

**)
Procedure TfrmHelpEntry.btnOKClick(Sender: TObject);

ResourceString
  strEmptyName = 'You must specify a name for the help file!';
  strCHMNotFound = 'The help file "%s" was not found!';
  strEbmtName = 'The name "%s" is already used by an existing Embracadero IDE Help file.';

Begin
  If edtName.Text = '' Then
    Begin
      MessageDlg(strEmptyName, mtError, [mbOK], 0);
      ModalResult := mrNone;
    End;
  If Not FileExists(edtFilename.Text) Then
    Begin
      MessageDlg(Format(strCHMNotFound, [edtFilename.Text]), mtError, [mbOK], 0);
      ModalResult := mrNone;
    End;
  If FIDENames.IndexOf(edtName.Text) > -1 Then
    Begin
      MessageDlg(Format(strEbmtName, [edtName.Text]), mtError, [mbOK], 0);
      ModalResult := mrNone;
    End;
End;

(**

  This method is the main way to invoke the dialogue.

  @precon  None.
  @postcon Displays a dialogue for editing the name and filename of a CHM file in the IDE.

  @param   strName     as a String as a reference
  @param   strFileName as a String as a reference
  @param   slIDENames  as a TStringList as a constant
  @return  a Boolean

**)
Class Function TfrmHelpEntry.Execute(Var strName, strFileName : String;
  Const slIDENames : TStringList) : Boolean;

Var
  F : TfrmHelpEntry;
  
Begin
  Result := False;
  TTPHelpToolsAPIFunctions.RegisterFormForTheming(TfrmHelpEntry);
  F := TfrmHelpEntry.Create(Application.MainForm);
  Try
    TTPHelpToolsAPIFunctions.ApplyThemeToComponent(F);
    F.edtName.Text := strName;
    F.edtFilename.Text := strFileName;
    F.FIDENames := slIDENames;
    If F.ShowModal = mrOk Then
      Begin
        strName := F.edtName.Text;
        strFileName := F.edtFilename.Text;
        Result := True;
      End;
  Finally
    F.Free;
  End;
End;

End.
