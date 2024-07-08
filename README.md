# RSS Reborn Installer

Welcome to the [RSS Reborn](https://github.com/RSS-Reborn/RSS-Reborn) Installer! This installer is designed to help you easily set up Ballisticfox's RSS Reborn mod for RP-1 (RSS/RO).

![RSS Reborn Installer](https://github.com/drobie22/RSS-Reborn-Installer/blob/main/Images/wizard.png?raw=true)

## Features

- Downloads and installs the RSS Reborn mod and its dependencies to a game directory you choose. 
- Provides options to select different resolutions for textures.
- Optionally can include Blackrack's Volumetric Clouds (EVE and Scatterer, which you must still download from Patreon).

## Requirements

- [Kerbal Space Program](https://www.kerbalspaceprogram.com/)
- [RP-1 RSS/RO Modpack](https://github.com/KSP-RO/RP-0) (must be installed and launched at least once)
- [7-Zip](https://www.7-zip.org/download.html) (for extracting files)
- (Optional) [Blackrack's Volumetric Clouds](https://www.patreon.com/blackrack) If you wish to include this, you must have his EVE and Scatterer zips downloaded to your downloads folder so the installer can grab them. 
    - This is in place to ensure you have obtained the files through Patreon. 
    - RSS Reborn Installer will never circumvent support for mod developers. 

## How to Download and Install

### Step 1: Download the Installer

1. Go to the [Releases](https://github.com/drobie22/RSS-Reborn/releases) page.
2. Download the latest version of the `RSS-Reborn-Installer.exe`.

### Step 2: Run the Installer

1. Run the downloaded `RSS-Reborn-Installer.exe`.
2. Follow the on-screen instructions:
   - Confirm that you have successfully launched RP-1 at least once.
   - Optionally confirm that you have downloaded Blackrack's EVE and Scatterer enhancements.
   - Select the desired resolutions for textures.
   - Specify a game directory (defaults to steam install). 
3. The installer will download, extract, merge, and move the necessary files into your gamedata.

### Step 3: Launch KSP

1. After the installation is complete, launch Kerbal Space Program.
2. Enjoy your enhanced RSS Reborn experience!

Note: If any you encounter any issues, please submit an [issue](https://github.com/drobie22/RSS-Reborn-Installer/issues).

## Troubleshooting

- Ensure that 7-Zip is installed in the default location.
- Check to make sure RP-1 (RSS/RO) can successfully run before using this installer. If that doesnt work, RSS Reborn will obviously not work. 
- If you load the game but get stuck on an infinite black loading screen with spinning planets, check to see if you have the stock Squad folders in GameData. Otherwise, submit an [issue](https://github.com/drobie22/RSS-Reborn-Installer/issues).
- GitHub naturally doesn't like programs fetching lots of data at once. Usually this is capped at 60 calls per hour. The installer will run 20-40 requests to GitHub in a run depending on resolutions selected. If you see an error related to GitHub's rate limit, please wait an hour before trying again. 
    - Alternatively, there is a method to set up an access token on your machine which the installer will recognize and use to increase the limit to 5000 calls per hour. (Not sure why you need that, but you do you). 
    - Instructions will be implimented soon. 
    - Installer will always grab the latest files from GitHub
- Help! No resolutions showing for a body! 
    - Most likely the latest release of that body was empty. Before you start installation, manually download the previous release. 

## Future Updates

- Support for downloading deferred rendering, user-downloaded and automated installation. 
- Support for retrieving files from other releases, not just the latest. 

## Credits

Big thank you to Ballisticfox, Techo, and VaNnadin for creating and maintaining RSS Reborn, and to Blackrack for his amazing Volumetric Clouds!

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.
