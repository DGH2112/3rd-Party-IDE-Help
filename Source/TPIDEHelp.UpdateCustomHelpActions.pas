(**
  
  This module contains code that manages the actions and menu items for the custom help files.

  @Author  David Hoyle
  @Version 1.0
  @Date    06 Oct 2018
  
**)
Unit TPIDEHelp.UpdateCustomHelpActions;

Interface

Uses
  System.Classes,
  System.Contnrs,
  System.Generics.Collections,
  VCL.Menus,
  TPIDEHelp.Interfaces;

Type
  (** A class which implements the ITPHelpUpdateJHelpActions interface. **)
  TTPHelpUpdateCustomHelpActions = Class(TInterfacedObject, ITPHelpUpdateHelpAction)
  Strict Private
    FCustomHelpActions   : TObjectList;
    FCustomHelpMenus     : TObjectList;
    FCustomHelp          : ITPHCustomHelpList;
    FCustomHelpFileNames : TStringList;
    FMenuHook            : TMenuItem;
  Strict Protected
    // ITPHelpUpdateHelpActions
    Procedure CreateCustomHelpActions;
    // General Methods
    Procedure CreateActions;
    Procedure CreateMenus;
    Procedure ShowCustomHelpFile(Sender : TObject);
  Public
    Constructor Create;
    Destructor Destroy; Override;
  End;

Implementation

Uses
  ToolsAPI,
  System.SysUtils,
  VCL.ActnList,
  VCL.Dialogs,
  VCL.Forms,
  WinAPI.Windows,
  WinAPI.ShellAPI,
  TPIDEHelp.CustomHelpList;

(**

  A constructor for the TTPHelpUpdateCustomHelpActions class.

  @precon  None.
  @postcon Creates object lists for the actiosn and menus.

**)
Constructor TTPHelpUpdateCustomHelpActions.Create;

Begin
  Inherited Create;
  FCustomHelp := TTPHCustomHelpList.Create;
  FCustomHelpActions := TObjectList.Create(True);
  FCustomHelpMenus := TObjectList.Create(True);
  FCustomHelpFileNames := TStringList.Create;
  FMenuHook := Nil;
  CreateCustomHelpActions;
End;

(**

  This method creates an action in the IDE for each third party custom help file.

  @precon  None.
  @postcon Actions are created (ine the IDE) for third party help files.

**)
Procedure TTPHelpUpdateCustomHelpActions.CreateActions;

ResourceString
  strTPHHelpActions = 'Third Party Custom Help';

Var
  NS : INTAServices;
  A: TAction;
  iThirdPartyHelp: Integer;
  
Begin
  If Supports(BorlandIDEServices, INTAServices, NS) Then
    For iThirdPartyHelp := 0 To FCustomHelp.Count - 1 Do
      Begin
        A := TAction.Create(NS.ActionList);
        A.ActionList := NS.ActionList;
        A.Caption := FCustomHelp.Name[iThirdPartyHelp];
        A.OnExecute := ShowCustomHelpFile;
        A.Category := strTPHHelpActions;
        A.Tag := FCustomHelpFileNames.Add(FCustomHelp.Filename[A.Caption]);
        FCustomHelpActions.Add(A);
      End;
End;

(**

  This method creates (or re-creates if they already exist) the actioans and menus for the custom Help
  file under the main Help menu above the about item.

  @precon  None.
  @postcon The custom help items are created.

**)
Procedure TTPHelpUpdateCustomHelpActions.CreateCustomHelpActions;

Begin
  FCustomHelpMenus.Clear;
  FCustomHelpActions.Clear;
  FCustomHelpFileNames.Clear;
  FCustomHelp := TTPHCustomHelpList.Create;
  CreateActions;
  CreateMenus;
  FCustomHelp := Nil;
End;

(**

  Thie method creates menu items for the third party help actions.

  @precon  None.
  @postcon The menu items are created.

**)
Procedure TTPHelpUpdateCustomHelpActions.CreateMenus;

  (**

    This method attempts to find the given meun name in the IDEs main menu.

    @precon  MainMenu must be a a valid instance.
    @postcon The reference to the named menu is returned if found else nil is returned.

    @param   MainMenu as a TMainMenu as a constant
    @param   strName  as a String as a constant
    @return  a TMenuItem

  **)
  Function FindMenu(Const MainMenu : TMainMenu; Const strName : String) : TMenuItem; Overload;

  Var
    i: Integer;

  Begin
    Result := Nil;
    For i := 0 To MainMenu.Items.Count - 1 Do
      If CompareText(MainMenu.Items[i].Name, strName) = 0 Then
        Result := MainMenu.Items[i];
  End;

Const
  strHelpMenu = 'HelpMenu';

Var
  NS : INTAServices;
  HelpMenu: TMenuItem;
  iAction: Integer;
  MI: TMenuItem;
  iIndex: Integer;

Begin
  If Supports(BorlandIDEServices, INTAServices, NS) Then
    Begin
      HelpMenu := FindMenu(NS.MainMenu, strHelpMenu);
      If Assigned(HelpMenu) Then
        Begin
          If Not Assigned(FMenuHook) Then
            Begin
              FMenuHook := TMenuItem.Create(HelpMenu);
              FMenuHook.Caption := '-';
              HelpMenu.Add(FMenuHook);
            End;
          iIndex := FMenuHook.MenuIndex;
          For iAction := 0 To FCustomHelpActions.Count - 1 Do
            Begin
              Inc(iIndex);
              MI := TMenuItem.Create(HelpMenu);
              MI.Action := FCustomHelpActions[iAction] As TAction;
              HelpMenu.Insert(iIndex, MI);
              FCustomHelpMenus.Add(MI);
            End
        End;
    End;
End;

(**

  A destructor for the TTPHelpUpdateCustomHelpActions class.

  @precon  None.
  @postcon Frees the memory used by the actions and menus.

**)
Destructor TTPHelpUpdateCustomHelpActions.Destroy;

Begin
  FCustomHelpFileNames.Free;
  FCustomHelpMenus.Free;
  FMenuHook.Free;
  FCustomHelpActions.Free;
  Inherited Destroy;
End;

(**

  This method opens the CHM HTML Help file for the given action using the Shell.

  @precon  None.
  @postcon The Help file is displayed.

  @param   Sender as a TObject

**)
Procedure TTPHelpUpdateCustomHelpActions.ShowCustomHelpFile(Sender: TObject);

Const
  strShellOpenCmd = 'open';

Var
  A: TAction;

Begin
  If Sender Is TAction Then
    Begin
      A := Sender As TAction;
      ShellExecute(Application.Handle, strShellOpenCmd, PChar(FCustomHelpFileNames[A.Tag]), '',
        PChar(GetCurrentDir), SW_NORMAL);
    End;
End;

End.
