(**
  
  This module contains code for adding and remove an about box entry in the RAD Studio IDE.

  @Author  David Hoyle
  @Version 1.0
  @Date    03 Oct 2018
  
**)
Unit TPIDEHelp.AboutBox;

Interface

Type
  (** A record to encapsulate the about box functionality. **)
  TTPHelpAboutBox = Record
    Class Function  AddAboutBox() : Integer; Static;
    Class Procedure RemoveAboutBox(Const iAboutBoxIndex : Integer); Static;
  End;

Implementation

Uses
  ToolsAPI,
  System.SysUtils,
  VCL.Forms,
  WinAPI.Windows,
  TPIDEHelp.Functions,
  TPIDEHelp.ResourceStrings;

(**

  This method adds an about box entry to the RAD Studio IDE.

  @precon  None.
  @postcon The About Box entry is added to the IDE and its index is returned.

  @return  an Integer

**)
Class Function TTPHelpAboutBox.AddAboutBox: Integer;

Const
  strTPHelpSplashScreen = 'TPHelpSplashScreen48x48';

ResourceString
  strAboutBoxDescription = 'A RAD Studio plug-in to manage and access Third Party help.';

Var
  ABS : IOTAAboutBoxServices;
  bmAboutBox : HBITMAP;
  recVersionInfo: TTPHVersionInfo;
  
Begin
  Result := -1;
  bmAboutBox := LoadBitmap(hInstance, strTPHelpSplashScreen);
  TTPHelpFunction.BuildNumber(recVersionInfo);
  If Supports(BorlandIDEServices, IOTAAboutBoxServices, ABS) Then
    Result := ABS.AddPluginInfo(
      Format(str3rdPartyIDEHelpFor, [recVersionInfo.FMajor, recVersionInfo.FMinor,
        strRevisions[Succ(recVersionInfo.FBugFix)], Application.Title]),
      strAboutBoxDescription,
      bmAboutBox,
      {$IFDEF DEBUG} True {$ELSE} False {$ENDIF},
      Format(strLicenseStatus, [recVersionInfo.FMajor, recVersionInfo.FMinor, recVersionInfo.FBugFix,
        recVersionInfo.FBuild])
    );
End;

(**

  This method removes the About Box entry from the IDE.

  @precon  None.
  @postcon The About Box entry with the given index is removed from the IDE.

  @param   iAboutBoxIndex as an Integer as a constant

**)
Class Procedure TTPHelpAboutBox.RemoveAboutBox(Const iAboutBoxIndex : Integer);

Var
  ABS : IOTAAboutBoxServices;

Begin
  If iAboutBoxIndex > -1 Then
    If Supports(BorlandIDEServices, IOTAAboutBoxServices, ABS) Then
      ABS.RemovePluginInfo(iAboutBoxIndex);
End;

End.
