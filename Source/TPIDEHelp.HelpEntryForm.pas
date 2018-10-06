(**
  
  This module contains a form class for querying the user for the name and file path of a CHM help
  file to be include in the IDEs list of help file.

  @Author  David Hoyle
  @Version 1.0
  @Date    06 Oct 2018
  
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
  System.ImageList,
  Vcl.ImgList, Vcl.ExtCtrls;

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
