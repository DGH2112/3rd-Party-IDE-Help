(**
  
  This module contains resource string to be used throughout the application.

  @Author  David Hoyle
  @Version 1.137
  @Date    15 Nov 2023

  @license

    3rd Party Help is a RAD Studio IDE plug-in that allows you to easily install
    additional 3rd Party HTML Help files into the IDE to provide context
    sensitive help for libraries and components.

    Copyright (C) 2023  David Hoyle (https://github.com/DGH2112/3rd-Party-IDE-Help)

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
Unit TPIDEHelp.ResourceStrings;

Interface

ResourceString
  (** A resource string to the name of the a plug-in. **)
  str3rdPartyIDEHelpFor = '3rd Party IDE Help %d.%d%s for %s';
  {$IFDEF DEBUG}
  (** A resource string for the build information on the splash screen **)
  strSplashScreenBuild = 'David Hoyle (c) 2023 License GNU GPL3 (DEBUG Build %d.%d.%d.%d)';
  {$ELSE}
  (** A resource string for the build information on the splash screen **)
  strSplashScreenBuild = 'David Hoyle (c) 2023 License GNU GPL3 (Build %d.%d.%d.%d)';
  {$ENDIF DEBUG}

Implementation

End.
