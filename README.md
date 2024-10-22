# RSS Reborn Installer

Welcome to the [RSS Reborn](https://github.com/RSS-Reborn/RSS-Reborn) Installer! This installer is designed to help you easily set up Ballisticfox's RSS Reborn.

![RSS Reborn Installer](https://github.com/drobie22/RSS-Reborn-Installer/blob/main/Images/V1-1-0wizard.png)

## Features

- Downloads and installs the RSS Reborn mod and its dependencies to a game directory you choose. 
- Provides options to select different resolutions for textures.
- Optionally can include Blackrack's Volumetric Clouds (EVE and Scatterer, which you must still download from Patreon).
  - If not selected, it will default to a previously working RSSVE. If this is your case, Please make sure RSSVE works before using the installer!

## Requirements

- [Kerbal Space Program](https://www.kerbalspaceprogram.com/) Version 1.12.x
- [Real Solar System Modpack](https://github.com/KSP-RO/RealSolarSystem) (must be installed and launched at least once)
	- RSS Reborn is a custom fork of RSS. That means RSS Reborn is compatible with RP-1, Realism Overhaul, etc.
- [7-Zip](https://www.7-zip.org/download.html) or [WinRAR](https://www.win-rar.com/) (for extracting files)
- 50 GB free storage space
- (Optional) [Blackrack's Volumetric Clouds](https://www.patreon.com/blackrack) If you wish to include this, you must have his EVE and Scatterer zips downloaded to your downloads folder so the installer can grab them. 
    - This is in place to ensure you have obtained the files through Patreon. 
    - RSS Reborn Installer will never circumvent support for mod developers. 

## How to Download and Install

### Step 1: Backup your GameData!

1. Go to your game directory and backup the GameData folder. This way you can always revert in case of an issue.

### Step 2: Download the Installer

1. Go to the [Releases](https://github.com/drobie22/RSS-Reborn-Installer/releases) page.
2. Download the latest version of the `RSS-Reborn-Installer.exe`.
   - If the download does not work, try the zip file.

### Step 3: Run the Installer

1. Run the downloaded `RSS-Reborn-Installer.exe`.
2. Follow the on-screen instructions:
   - Confirm that you have successfully launched Real Solar System at least once.
   - Optionally confirm that you have downloaded Blackrack's Raymarched Volumetrics from Patreon.
       - If downloaded, select the file when prompted.
   - Select the desired resolutions for textures.
   - Specify a game directory (defaults to steam install). 
3. The installer will download, extract, merge, and move the necessary files into your gamedata.

### Step 4: Launch KSP

1. After the installation is complete, launch Kerbal Space Program.
2. Enjoy your enhanced RSS Reborn experience!

Note: If any you encounter any issues, please submit an [issue](https://github.com/drobie22/RSS-Reborn-Installer/issues).

## Troubleshooting

- ***All textures are showing "none"!*** 
    - Your GitHub token may have expired. Create a new token and run the installer again.
- ***Parallax Error*** Unable to locate a critical part of the Parallax Installation: ParallaxTerrain.cfg is missing.
    - Completely normal. You can ignore.
    - OR enable the "Remove Parallax Texture Warning" checkbox in "Recommended Community Visual Settings"
- ***7-Zip executable not found! Please ensure 7-Zip is installed.***
    - Ensure that 7-Zip or WinRAR is installed in the default location.
- Check to make sure your Real Solar System installation can successfully run before using this installer. If that doesnt work, RSS Reborn will obviously not work. 
- If you load the game but get stuck on an infinite black loading screen with spinning planets, check to see if you have the stock Squad folders in GameData. Otherwise, submit an [issue](https://github.com/drobie22/RSS-Reborn-Installer/issues).
- GitHub naturally doesn't like programs fetching lots of data at once. Usually this is capped at 60 calls per hour. The installer will run 20-40 requests to GitHub in a run depending on resolutions selected. If you see an error related to GitHub's rate limit, please wait an hour before trying again. 
    - Alternatively, there is a method to set up an access token on your machine which the installer will recognize and use to increase the limit to 5000 calls per hour. (Not sure why you need that, but you do you). 
    - Instructions are at the bottom of this page.
    - Installer will always grab the latest files from GitHub.
- Help! No resolutions showing for a body! 
    - Most likely the latest release of that body was empty. Before you start installation, manually download the previous release.
- Downloads are failing, why?
    - Try connecting to another network or switching between wifi and ethernet. 
    - Windows Defender could be blocking some downloads.
 
#### If you encounter a problem not listed:

Please submit an [issue](https://github.com/drobie22/RSS-Reborn-Installer/issues) with your latest log (found in your KSP directory inside `\Logs\RSSRebornInstaller`. 
The log will look something like `Setup Log 2024-07-15 #001.txt`.

Alternatively, the log can be found in the directory: `C:\Users\yourusername\AppData\Local\Temp`, where yourusername is the name of your windows user. 
The log will still look something like `Setup Log 2024-07-15 #001.txt`.

## Future Updates

- Support for downloading deferred rendering, user-downloaded and automated installation. 
- Support for retrieving files from other releases, not just the latest.
- Support for downloading MTE.
- Installer will automatically back up your GameData folder and compress it in case of failure.
- Local caching of files to speed up installation time

## Credits

Big thank you to Ballisticfox, Techo, and VaNnadin for creating and maintaining RSS Reborn, and to Blackrack for his amazing Volumetric Clouds!
Additional thanks to kyrianelson413 for their GitHub expertise and debugging help!

## License

This project is licensed under the MIT License. See the [LICENSE](https://github.com/drobie22/RSS-Reborn-Installer/blob/main/Licenses/LICENSE) file for details.

## Setting Up a GitHub Environment Variable

Follow these steps to set up an environment access token to increase GitHub API calls.

### Step 1: Sign in to GitHub
1. Go to [github.com](https://github.com).
2. Sign in with your GitHub credentials.

### Step 2: Click on your Profile
1. Click on your profile icon in the upper-right corner.
2. Select "Developer Settings" from the menu.
(You may also have to go to normal settings first, then go to Developer Settings)

### Step 3: Personal Access Tokens
1. In Developer settings, click on "Personal Access Tokens".
2. Click on "Tokens (classic)".

### Step 4: Generate New Token
1. Click on the "Generate new token" drop down.
2. Select "Generate new token (classic)".
3. You may be requred to confirm access using the GitHub mobile app.

### Step 5: Token Details
1. In the "Note" field, name the token whatever you want.
2. Set the expiration, a week should be fine.
3. Check the first "repo" box, that is enough.
4. At the botom, click the "Generate Token" button.

### Step 6: Save Environment Token
1. Copy the given personal access token. It will not be shown again.

### Step 7: Command Line
1. Open windows command line
2. **Type:** setx MY_ACCESS_TOKEN "YOUR_TOKEN"

### Step 8: Run Installer Again
1. The installer will automatically recognize MY_ACCESS_TOKEN, and your GitHub API limit will be increased!

