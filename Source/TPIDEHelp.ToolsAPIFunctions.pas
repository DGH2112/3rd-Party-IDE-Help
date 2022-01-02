(**
  
  This module contains Open Tools API utility functions for use throughout this application.

  @Author  David Hoyle
  @Version 1.102
  @Date    31 Dec 2021

  @license

    3rd Party Help is a RAD Studio IDE plug-in that allows you to easily install
    additional 3rd Party HTML Help files into the IDE to provide context
    sensitive help for libraries and components.

    Copyright (C) 2020  David Hoyle (https://github.com/DGH2112/3rd-Party-IDE-Help)

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

{$IFDEF RS102}
Var
  ITS: IOTAIDEThemingServices250;
{$ENDIF RS102}

Begin
  {$IFDEF RS102}
  If Supports(BorlandIDEServices, IOTAIDEThemingServices250, ITS) Then
    If ITS.IDEThemingEnabled Then
      ITS.ApplyTheme(AComponent);
  {$ENDIF RS102}
End;

(**

  This method registers the given form with the IDE form theming is theming is available and enabled.

  @precon  None.
  @postcon The given form class is registered with the IDE for theming if theming is available and
           enabled.

  @param   FormCls as a TCustomFormClass as a constant

**)
Class Procedure TTPHelpToolsAPIFunctions.RegisterFormForTheming(Const FormCls: TCustomFormClass);

{$IFDEF RS102}
Var
  ITS: IOTAIDEThemingServices250;
{$ENDIF RS102}

Begin
  {$IFDEF RS102}
  If Supports(BorlandIDEServices, IOTAIDEThemingServices250, ITS) Then
    If ITS.IDEThemingEnabled Then
      ITS.RegisterFormClass(FormCls);
  {$ENDIF RS102}
End;

(**

  This method returns the themed system colour for the given colour IF theming is available and enabled.

  @precon  None.
  @postcon The themed colour is returned if theming is available and enabled.

  @param   iColour as a TColor as a constant
  @return  a TColor

**)
Class Function TTPHelpToolsAPIFunctions.ThemeColour(Const iColour: TColor): TColor;

{$IFDEF RS102}
Var
  ITS: IOTAIDEThemingServices250;
{$ENDIF RS102}

Begin
  Result := iColour;
  {$IFDEF RS102}
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
    If ITS.IDEThemingEnabled Then
      Result := ITS.StyleServices.GetSystemColor(iColour);
  {$ENDIF RS102}
End;

End.
