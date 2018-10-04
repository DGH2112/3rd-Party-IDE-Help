(**
  
  This module contains code to create a splash screen entry on the RAD Studio IDE splash screen.

  @Author  David Hoyle
  @Version 1.0
  @Date    03 Oct 2018
  
**)
Unit TPIDEHelp.SplashScreen;

Interface

Type
  (** A record to encapsulate all of the splash screen methods. **)
  TTPHelpSplashScreen = Record
    Class Procedure AddSplashScreenItem(); Static;
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

  This method adds a splash screen entry to the RAD Studio IDE splash screen.

  @precon  None.
  @postcon The entry is added to the splashs screen.

**)
Class Procedure TTPHelpSplashScreen.AddSplashScreenItem();

Const
  {$IFDEF D2007}
  strTPHelpSplashScreen = 'TPHelpSplashScreen24x24';
  {$ELSE}
  strTPHelpSplashScreen = 'TPHelpSplashScreen48x48';
  {$ENDIF}

Var
  SSS : IOTASplashScreenServices;
  bmSplashScreen : HBITMAP;
  recVersionInfo : TTPHVersionInfo;

Begin
  bmSplashScreen := LoadBitmap(hInstance, strTPHelpSplashScreen);
  TTPHelpFunction.BuildNumber(recVersionInfo);
  If Supports(SplashScreenServices, IOTASplashScreenServices, SSS) Then
    SSS.AddPluginBitmap(
      Format(str3rdPartyIDEHelpFor, [recVersionInfo.FMajor, recVersionInfo.FMinor,
        strRevisions[Succ(recVersionInfo.FBugFix)], Application.Title]),
      bmSplashScreen,
      {$IFDEF DEBUG} True {$ELSE} False {$ENDIF},
      Format(strLicenseStatus, [recVersionInfo.FMajor, recVersionInfo.FMinor, recVersionInfo.FBugFix,
        recVersionInfo.FBuild])
    );
End;

End.
