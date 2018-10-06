(**

  This module contains a frame for presenting the 3rd Part Help that is installed in the IDE.

  @Author  David Hoyle
  @Version 1.0
  @Date    06 Oct 2018

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
  System.ImageList,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ImgList,
  TPIDEHelp.Interfaces;

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
    Type
      (** An enumerate for identifying the type of help item. **)
      TTPHHelpType = (htEbmt, ht3rdParty, htDelete);
      (** A record to describe the help items in the registry. **)
      THelpRecord = Record
        FOldName  : String;
        FNewName  : String;
        FFilename : String;
        FHelpType : TTPHHelpType;
        Constructor Create(Const strName, strFilename : String; Const eHelpType : TTPHHelpType);
      End;
  Strict Private
    FHelp : TList<THelpRecord>;
  Strict Protected
    // ITPHelpOptionsFrame
    Procedure InitialiseFrame;
    Procedure FinaliseFrame;
    // General Methods
    Procedure PopulateListView;
    Function  GetIDEVersionNum(Const strBDSDir : String) : String;
    Function  GetIDERegPoint() : String;
  Public
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
  End;

Implementation

{$R *.dfm}

Uses
  ToolsAPI,
  System.Win.Registry,
  System.RegularExpressions,
  TPIDEHelp.HelpEntryForm, TPIDEHelp.ToolsAPIFunctions;

Const
  (** A constant for the registry location of the IDEs 3rd party help. **)
  strHelpRegKey = 'Software\Embarcadero\%s\%s\Help';
  (** A constant for the custom 2td party HTML file entries. **)
  strHTMLHelpFiles = 'HTMLHelp1Files';
  (** A constant for the RAD Studio IDE base directory environment variable. **)
  strBDSEnviroVar = 'BDS';

(**

  A constructor for the THelpRecord class.

  @precon  None.
  @postcon Initalises the record.

  @param   strName     as a String as a constant
  @param   strFilename as a String as a constant
  @param   eHelpType   as a TTPHHelpType as a constant

**)
Constructor TframeTPIDEHelpOptions.THelpRecord.Create(Const strName, strFilename : String;
  Const eHelpType : TTPHHelpType);

Begin
  FOldName  := strName;
  FNewName  := strName;
  FFilename := strFilename;
  FHelpType := eHelpType;
End;

(**

  This is an on click event handler for the Add button.

  @precon  None.
  @postcon Promts the user to add a CHM file to the list of files.

  @param   Sender as a TObject

**)
procedure TframeTPIDEHelpOptions.btnAddClick(Sender: TObject);

Var
  strName, strFileName : String;
  R: THelpRecord;
  slIDENames: TStringList;
  iHelp: Integer;

begin
  strName := '';
  strFileName := '';
  slIDENames := TStringList.Create;
  Try
    For iHelp := 0 To FHelp.Count - 1 Do
      If FHelp[iHelp].FHelpType = htEbmt Then
        slIDENames.Add(fHelp[iHelp].FOldName);
    If TfrmHelpEntry.Execute(strName, strFileName, slIDENames) Then
      Begin
        R.Create(strName, strFileName, ht3rdParty);
        FHelp.Add(R);
        PopulateListView;
      End;
  Finally
    slIDENames.Free;
  End;
end;

(**

  This is an on click event handler for the Delete button.

  @precon  None.
  @postcon Deletes the selected CHM file from the list.

  @param   Sender as a TObject

**)
Procedure TframeTPIDEHelpOptions.btnDeleteClick(Sender: TObject);

Var
  iIndex: Integer;
  R: THelpRecord;

Begin
  iIndex := Integer(lvHelp.Selected.Data);
  R := FHelp[iIndex];
  R.FHelpType := htDelete;
  FHelp[iIndex] := R;
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
  strName, strFileName : String;
  iIndex: Integer;
  R: THelpRecord;
  slIDENames: TStringList;
  iHelp: Integer;

Begin
  iIndex := Integer(lvHelp.Selected.Data);
  strName := FHelp[iIndex].FNewName;
  strFileName := FHelp[iIndex].FFilename;
  slIDENames := TStringList.Create;
  Try
    For iHelp := 0 To FHelp.Count - 1 Do
      If FHelp[iHelp].FHelpType = htEbmt Then
        slIDENames.Add(fHelp[iHelp].FOldName);
    If TfrmHelpEntry.Execute(strName, strFileName, slIDENames) Then
      Begin
        R := FHelp[iIndex];
        R.FNewName := strName;
        R.FFilename := strFileName;
        FHelp[iIndex] := R;
        PopulateListView;
      End;
  Finally
    slIDENames.Free;
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
  FHelp := TList<THelpRecord>.Create;
End;

(**

  A destructor for the TframeTPHelpOptions class.

  @precon  None.
  @postcon Frees the generic collection.

**)
Destructor TframeTPIDEHelpOptions.Destroy;

Begin
  FHelp.Free;
  Inherited Destroy;
End;

(**

  This method makes the changes that have been asked for in the dialogue to the Help information
  in the registry.

  @precon  None.
  @postcon The RAD Studio registry is updated with the list of custom help information.

**)
Procedure TframeTPIDEHelpOptions.FinaliseFrame;

Var
  iHelp: Integer;
  strBDSDir: String;
  R: TRegIniFile;

Begin
  strBDSDir := System.SysUtils.GetEnvironmentVariable(strBDSEnviroVar);
  R := TRegIniFile.Create(Format(strHelpRegKey, [GetIDERegPoint(), GetIDEVersionNum(strBDSDir)]));
  Try
    For iHelp := 0 To FHelp.Count - 1 Do
      Case FHelp[iHelp].FHelpType Of
        htDelete: R.DeleteKey(strHTMLHelpFiles, FHelp[iHelp].FOldName);
        ht3rdParty:
          If CompareText(FHelp[iHelp].FOldName, FHelp[iHelp].FNewName) <> 0 Then
            Begin
              R.DeleteKey(strHTMLHelpFiles, FHelp[iHelp].FOldName);
              R.WriteString(strHTMLHelpFiles, FHelp[iHelp].FNewName, FHelp[iHelp].FFilename);
            End Else
          If CompareText(FHelp[iHelp].FFilename,
            R.ReadString(strHTMLHelpFiles, FHelp[iHelp].FOldName, '')) <> 0 Then
            R.WriteString(strHTMLHelpFiles, FHelp[iHelp].FOldName, FHelp[iHelp].FFilename);
      End;
  Finally
    R.Free;
  End;
End;

(**

  This method searches the IDEs command line parameters for an alternate registration point (-rXxxxx)
  and returns that alternate point instead of the standard BDS if found.

  @precon  None.
  @postcon Returns the activty IDEs registration point.

  @return  a String

**)
Function TframeTPIDEHelpOptions.GetIDERegPoint: String;

Const
  strDefaultRegPoint = 'BDS';
  iSwitchLen = 2;

Var
  iParam: Integer;

Begin
  Result := strDefaultRegPoint;
  For iParam := 1 To ParamCount Do
    If CompareText(Copy(ParamStr(iParam), 1, iSwitchLen), '-r') = 0 Then
      Begin
        Result := ParamStr(iParam);
        Delete(Result, 1, iSwitchLen);
        Break;
      End;
End;

(**

  This method returns the IDEs version number from the end of the BDS environment variable passed.

  @precon  None.
  @postcon the version number is returned.

  @param   strBDSDir as a String as a constant
  @return  a String

**)
Function TframeTPIDEHelpOptions.GetIDEVersionNum(Const strBDSDir: String): String;

Var
  RegEx : TRegEx;
  M: TMatch;

Begin
  Result := '0.0';
  RegEx := TRegEx.Create('\d+\.\d', [roIgnoreCase, roCompiled, roSingleLine]);
  M := RegEx.Match(strBDSDir);
  If M.Success Then
    Result := M.Value;
End;

(**

  This method initialises the frame with 3rd party help items from the RAD Studio regsitry.

  @precon  None.
  @postcon The list view is populates with help items.

**)
Procedure TframeTPIDEHelpOptions.InitialiseFrame;

Var
  R : TRegIniFile;
  slHelp: TStringList;
  iHelp: Integer;
  strBDSDir: String;
  strFileName: String;
  eHelpType: TTPHHelpType;

Begin
  strBDSDir := System.SysUtils.GetEnvironmentVariable(strBDSEnviroVar);
  R := TRegIniFile.Create(Format(strHelpRegKey, [GetIDERegPoint(), GetIDEVersionNum(strBDSDir)]));
  Try
    slHelp := TStringList.Create;
    Try
      R.ReadSection(strHTMLHelpFiles, slHelp);
      For iHelp := 0 To slHelp.Count - 1 Do
        Begin
          strFileName := R.ReadString(strHTMLHelpFiles, slHelp[iHelp], '');
          eHelpType := ht3rdParty;
          If CompareText(Copy(strFilename, 1, Length(strBDSDir)), strBDSDir) = 0 Then
            eHelpType := htEbmt;
          FHelp.Add(THelpRecord.Create(slHelp[iHelp], strFileName, eHelpType));
        End;
    Finally
      slHelp.Free;
    End;
  Finally
    R.Free;
  End;
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
    For iHelp := 0 To FHelp.Count - 1 Do
      If FHelp[iHelp].FHelpType In [ht3rdParty] Then
        Begin
          Item := lvHelp.Items.Add;
          Item.Caption := FHelp[iHelp].FNewName;
          Item.SubItems.Add(FHelp[iHelp].FFilename);
          Item.Data := Pointer(iHelp);
        End;
    If iSelectedIndex >= lvHelp.items.Count Then
      iSelectedIndex := lvHelp.Items.Count - 1;
    lvHelp.ItemIndex := iSelectedIndex;
  Finally
    lvHelp.Items.EndUpdate;
  End;
End;

End.
