(**
  
  This module contains Open Tools API utility functions for use throughout this application.

  @Author  David Hoyle
  @Version 1.0
  @Date    06 Oct 2018
  
**)
Unit TPIDEHelp.ToolsAPIFunctions;

Interface

{$INCLUDE CompilerDefinitions.inc}


Uses
  System.Classes,
  VCL.Graphics,
  VCL.Forms;

Type
  (** A record to encapsulate all the ToolsAPI utility functions. **)
  TTPHelpToolsAPIFunctions = Record
    Class Procedure RegisterFormForTheming(Const FormCls: TCustomFormClass); Static;
    Class Procedure ApplyThemeToComponent(Const AComponent: TComponent); Static;
    Class Function  ThemeColour(Const iColour : TColor) : TColor; Static;
  End;

Implementation

Uses
  ToolsAPI,
  System.SysUtils;

(**

  This method applies an IDE Theme to the given component IF theming is enabled and available in the IDE.

  @precon  AComponent must be a valid instance.
  @postcon The component and all its child components are themed if theming is enabled and available.

  @param   AComponent as a TComponent as a constant

**)
Class Procedure TTPHelpToolsAPIFunctions.ApplyThemeToComponent(Const AComponent: TComponent);

{$IFDEF DXE102}
Var
  ITS: IOTAIDEThemingServices250;
{$ENDIF}

Begin
  {$IFDEF DXE102}
  If Supports(BorlandIDEServices, IOTAIDEThemingServices250, ITS) Then
    If ITS.IDEThemingEnabled Then
      ITS.ApplyTheme(AComponent);
  {$ENDIF}
End;

(**

  This method registers the given form with the IDE form theming is theming is available and enabled.

  @precon  None.
  @postcon The given form class is regsitered with the IDE for theming if themign is available and
           enabled.

  @param   FormCls as a TCustomFormClass as a constant

**)
Class Procedure TTPHelpToolsAPIFunctions.RegisterFormForTheming(Const FormCls: TCustomFormClass);

{$IFDEF DXE102}
Var
  ITS: IOTAIDEThemingServices250;
{$ENDIF}

Begin
  {$IFDEF DXE102}
  If Supports(BorlandIDEServices, IOTAIDEThemingServices250, ITS) Then
    If ITS.IDEThemingEnabled Then
      ITS.RegisterFormClass(FormCls);
  {$ENDIF}
End;

(**

  This method returns the themed system colour for the given colour IF theming is avaiulable and enabled.

  @precon  None.
  @postcon The themd colour is returned if theming is available and enabled.

  @param   iColour as a TColor as a constant
  @return  a TColor

**)
Class Function TTPHelpToolsAPIFunctions.ThemeColour(Const iColour: TColor): TColor;

{$IFDEF DXE102}
Var
  ITS: IOTAIDEThemingServices250;
{$ENDIF}

Begin
  Result := iColour;
  {$IFDEF DXE102}
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
    If ITS.IDEThemingEnabled Then
      Result := ITS.StyleServices.GetSystemColor(iColour);
  {$ENDIF}
End;

End.
