(**

  This module defines an IOTAWizard class for the RAD Studio plug-in.

  @Author  David Hoyle
  @Version 1.099
  @Date    30 May 2020

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
Unit TPIDEHelp.Wizard;

Interface

Uses
  ToolsAPI,
  TPIDEHelp.Interfaces;

Type
  (** A class that implements the IOTAWizard interface for registering a wizard plugin with the IDE. **)
  TTPIDEHelpWizard = Class(TInterfacedObject, IUnknown, IOTANotifier, IOTAWizard)
  Strict Private
    FAboutBox                : Integer;
    FAddinOptions            : INTAAddinOptions;
    FUpdateCustomHelpActions : ITPHelpUpdateHelpAction;
  Strict Protected
    Procedure Execute;
    Function  GetIDString: String;
    Function  GetName: String;
    Function  GetState: TWizardState;
    Procedure AfterSave;
    Procedure BeforeSave;
    Procedure Destroyed;
    Procedure Modified;
  Public
    Constructor Create;
    Destructor Destroy; Override;
  End;

  Function InitWizard(Const BorlandIDEServices : IBorlandIDEServices;
    RegisterProc : TWizardRegisterProc;
    var Terminate: TWizardTerminateProc) : Boolean; StdCall;

Exports
  InitWizard Name WizardEntryPoint;

Implementation


Uses
  System.SysUtils,
  TPIDEHelp.SplashScreen,
  TPIDEHelp.AboutBox, 
  TPIDEHelp.AddinOptions,
  TPIDEHelp.UpdateCustomHelpActions;

(**

  This function registers the p[lug-in DLL with the IDE as the IDE looks for this when loading each
  plug-in DLL.

  @precon  None.
  @postcon Creates an nistance of the 3rd Party Help plug-in wizard.

  @nocheck MissingCONSTInParam
  @nohint  Terminate 

  @param   BorlandIDEServices as an IBorlandIDEServices as a constant
  @param   RegisterProc       as a TWizardRegisterProc
  @param   Terminate          as a TWizardTerminateProc as a reference
  @return  a Boolean

**)
Function InitWizard(Const BorlandIDEServices : IBorlandIDEServices;
  RegisterProc : TWizardRegisterProc;
  var Terminate: TWizardTerminateProc) : Boolean; StdCall;

Begin
  Result := Assigned(BorlandIDEServices);
  If Result Then
    RegisterProc(TTPIDEHelpWizard.Create);
End;
(**

  This is an AfterSave method for the IOTAWizard interface.

  @precon  None.
  @postcon Does nothing / Not Used.

  @nocheck EmptyMethod

**)
Procedure TTPIDEHelpWizard.AfterSave;

Begin
End;

(**

  This is an BeforeSave method for the IOTAWizard interface.

  @precon  None.
  @postcon Does nothing / Not Used.

  @nocheck EmptyMethod

**)
Procedure TTPIDEHelpWizard.BeforeSave;

Begin
End;

(**

  A constructor for the TTPHelpWizard class.

  @precon  None.
  @postcon Adds a splash screen entry to the IDE.

**)
Constructor TTPIDEHelpWizard.Create;

Var
  EOS : INTAEnvironmentOptionsServices;

Begin
  TTPHelpSplashScreen.AddSplashScreenItem;
  FAboutBox := TTPHelpAboutBox.AddAboutBox;
  FUpdateCustomHelpActions := TTPHelpUpdateCustomHelpActions.Create;
  FAddinOptions := TTPHelpAddinOptions.Create(FUpdateCustomHelpActions);
  If Supports(BorlandIDEServices, INTAEnvironmentOptionsServices, EOS) Then
    EOS.RegisterAddInOptions(FAddinOptions);
End;

(**

  A destructor for the TTPIDEHelpWizard class.

  @precon  None.
  @postcon Frees memory used by the plug-in.

**)
Destructor TTPIDEHelpWizard.Destroy;

Var
  EOS : INTAEnvironmentOptionsServices;

Begin
  If Supports(BorlandIDEServices, INTAEnvironmentOptionsServices, EOS) Then
    EOS.UnregisterAddInOptions(FAddinOptions);
  FAddinOptions := Nil;
  TTPHelpAboutBox.RemoveAboutBox(FAboutBox);
  Inherited Destroy;
End;

(**

  This is an Destroyed method for the IOTAWizard interface.

  @precon  None.
  @postcon Does nothing / Not Used.

  @nocheck EmptyMethod

**)
Procedure TTPIDEHelpWizard.Destroyed;

Begin
End;

(**

  This is an Execute method for the IOTAWizard interface.

  @precon  None.
  @postcon Does nothing / Not Used.

  @nocheck EmptyMethod

**)
Procedure TTPIDEHelpWizard.Execute;

Begin
End;

(**

  This is a getter method for the ISString property.

  @precon  None.
  @postcon Returns a unqiue string identifier for the plug-in.

  @return  a String

**)
Function TTPIDEHelpWizard.GetIDString: String;

ResourceString
  strDGHSeasonFallThirdPartyHelp = 'DGH.Season''s Fall.Third Party Help';

Begin
  Result := strDGHSeasonFallThirdPartyHelp;
End;

(**

  This is a getter method for the Name property.

  @precon  None.
  @postcon Returns the name of the plug-in.

  @return  a String

**)
Function TTPIDEHelpWizard.GetName: String;

ResourceString
  strThirdPartyHelp = 'Third Party Help';

Begin
  Result := strThirdPartyHelp;
End;

(**

  This is a getter method for the State property.

  @precon  None.
  @postcon Returns a set containing wsEnabled for the plug-in.

  @return  a TWizardState

**)
Function TTPIDEHelpWizard.GetState: TWizardState;

Begin
  Result := [wsEnabled];
End;

(**

  This is an Modified method for the IOTAWizard interface.

  @precon  None.
  @postcon Does nothing / Not Used.

  @nocheck EmptyMethod

**)
Procedure TTPIDEHelpWizard.Modified;

Begin
End;

End.
