; Inno Setup Script

#define MyAppName "RSS-Reborn"
#define MyAppVersion "0.1"
#define MyAppPublisher "DRobie22"
#define MyAppURL "https://github.com/RSS-Reborn/RSS-Reborn"
#define MyAppExeName "RSS-Reborn-Installer.exe"

#include "it_download.iss"

[Setup]
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
CreateAppDir=no
OutputBaseFilename=RSSRebornInstaller
Compression=lzma
SolidCompression=yes
WizardStyle=modern
WizardSmallImageFile=icon.bmp
BackColor=$cccccc
WizardImageFile=backgroundmoon.bmp
WizardImageStretch=no
DisableWelcomePage=no
SetupLogging=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Messages]
WelcomeLabel2=This will install [name/ver] on your computer.%n%nMod created and maintained by ballisticfox, Techo, and VaNnadin.%nInstaller created by DRobie22.

[Files]
Source: "C:\Program Files\7-Zip\7z.exe"; DestDir: "{tmp}"; Flags: dontcopy
Source: "itdownload.dll"; DestDir: "{tmp}"; Flags: dontcopy
Source: "7za.exe"; DestDir: "{tmp}"; Flags: dontcopy

[Code]
const
  KSP_DIR = 'C:\Program Files (x86)\Steam\steamapps\common\Kerbal Space Program\GameData';
  GitHubAPI = 'https://api.github.com/repos/';
  RSSConfigsRepo = 'RSS-Reborn/RSS-Configs';
  RSSTexturesRepo = 'RSS-Reborn/RSS-Terrain';
  URLMON_DLL = 'urlmon.dll';
  S_OK = 0;
  MAX_PATH = 260;
	MyAccessToken = '';
  
var
  BodyRepos: array[0..11] of string;
  ResolutionCombos: array[0..11] of TComboBox;
  DownloadsDir: string;
  HasRP1, HasEVEAndScatterer: Boolean;
  EVEAndScattererPage: TInputOptionWizardPage;
  RP1Checkbox: TNewCheckBox;
  ComboBox: TComboBox;
  I, J: Integer;
  JSON: string;
  Resolution: string;
  DownloadList: TStringList;
  ScattererDownloaded: Boolean;
  EVEdownloaded: string;
  
	
procedure InitializeGitHubAPI;
var
  AccessToken: string;
begin
  AccessToken := GetEnv('MY_ACCESS_TOKEN'); // Get token from environment variable
  if AccessToken = '' then
  begin
    MsgBox('GitHub Access Token is not set.', mbError, MB_OK);
    Exit;
  end;
  // Use AccessToken in your API calls
end;
	
function IsDirectoryEmpty(DirPath: string): Boolean;
var
  FindRec: TFindRec;
begin
  Log('Checking if directory is empty: ' + DirPath);
  Result := not FindFirst(AddBackslash(DirPath) + '*', FindRec);
  try
    FindClose(FindRec);
  except
    Result := False;
  end;
  if Result then
    Log('Directory is empty: True')
  else
    Log('Directory is empty: False');
end;


procedure InitializeBodyRepos;
begin
  Log('Initializing BodyRepos array');
  BodyRepos[11] := 'RSS-Reborn/RSS-Sol';
  BodyRepos[10] := 'RSS-Reborn/RSS-Mercury';
  BodyRepos[9] := 'RSS-Reborn/RSS-Venus';
  BodyRepos[8] := 'RSS-Reborn/RSS-Earth';
  BodyRepos[7] := 'RSS-Reborn/RSS-Luna';
  BodyRepos[6] := 'RSS-Reborn/RSS-Mars';
  BodyRepos[5] := 'RSS-Reborn/RSS-Jupiter';
  BodyRepos[4] := 'RSS-Reborn/RSS-Saturn';
  BodyRepos[3] := 'RSS-Reborn/RSS-Uranus';
  BodyRepos[2] := 'RSS-Reborn/RSS-Neptune';
  BodyRepos[1] := 'RSS-Reborn/RSS-AsteroidBelt';
  BodyRepos[0] := 'RSS-Reborn/RSS-KuiperBelt';
  Log('BodyRepos array initialized');
end;

function Extract7Zip(ArchivePath, DestDir: string): Boolean;
var
  ZipPath: string;
  ResultCode: Integer;
begin
  Log(Format('Extracting archive %s to directory %s', [ArchivePath, DestDir]));
  
  // Set the path to 7-Zip executable
  ZipPath := ExpandConstant('{tmp}\7za.exe');
  
  // Check if 7za.exe exists
  if not FileExists(ZipPath) then
  begin
    Log('7-Zip executable not found!');
    MsgBox('7-Zip executable not found!', mbError, MB_OK);
    Result := False;
    Exit;
  end;
  
  // Run 7-Zip to extract the archive
  if not Exec(ZipPath, 'x "' + ArchivePath + '" -o"' + DestDir + '" -y', '', SW_HIDE, ewWaitUntilTerminated, ResultCode) then
  begin
    Log(Format('Failed to extract %s, error code: %d', [ArchivePath, ResultCode]));
    MsgBox('Failed to extract ' + ArchivePath, mbError, MB_OK);
    Result := False;
    Exit;
  end;
  
  // Check if extraction was successful
  if ResultCode <> 0 then
  begin
    Log(Format('7-Zip returned error code %d while extracting %s', [ResultCode, ArchivePath]));
    MsgBox('7-Zip returned error code ' + IntToStr(ResultCode) + ' while extracting ' + ArchivePath, mbError, MB_OK);
    Result := False;
    Exit;
  end;

  Log('Extraction successful');
  Result := True;
end;

function GetSubstringPosition(const SubStr, Str: string; StartPos: Integer): Integer;
var
  TempStr: string;
begin
  Log(Format('Finding position of substring "%s" in string "%s" starting from position %d', [SubStr, Str, StartPos]));
  TempStr := Copy(Str, StartPos, Length(Str) - StartPos + 1);
  Result := Pos(SubStr, TempStr);
  if Result <> 0 then
    Result := Result + StartPos - 1;
  Log(Format('Substring position found: %d', [Result]));
end;

function GetLatestReleaseAssets(Repo: string): string;
var
  HttpCli: Variant;
  JSON, AssetsURL: string;
  I, J: Integer;
  ErrorMessage: string;
  
begin
  Log('Getting latest release assets for repository: ' + Repo);
  Result := '';
  try
    HttpCli := CreateOleObject('WinHttp.WinHttpRequest.5.1');
    HttpCli.Open('GET', GitHubAPI + Repo + '/releases/latest', False);
    HttpCli.SetRequestHeader('User-Agent', 'InnoSetup');
    HttpCli.SetRequestHeader('Authorization', 'token ' + MyAccessToken); // Add authorization header
    HttpCli.Send;
    
    if HttpCli.Status = 200 then
    begin
      Log('Request successful, status: ' + IntToStr(HttpCli.Status));
      JSON := HttpCli.ResponseText;
      I := GetSubstringPosition('"assets_url":"', JSON, 1);
      if I > 0 then
      begin
        I := I + 14;
        J := GetSubstringPosition('"', JSON, I);
        AssetsURL := Copy(JSON, I, J - I);
        Result := AssetsURL;
        Log('Assets URL found: ' + Result);
      end;
    end
    else
    begin
      Log('Failed to get the latest release assets for ' + Repo + '. Status: ' + IntToStr(HttpCli.Status));
      MsgBox('Failed to get the latest release assets for ' + Repo + '. Status: ' + IntToStr(HttpCli.Status), mbError, MB_OK);
    end;
  except
    begin
      ErrorMessage := 'Exception during HTTP request for latest release assets of ' + Repo + ': ' + GetExceptionMessage;
      Log(ErrorMessage);
      MsgBox(ErrorMessage, mbError, MB_OK);
    end;
  end;
end;

function LatestReleaseHasFiles(URL: string): Boolean;
var
  WinHttpReq: Variant;
begin
  Result := False;
  
  try
    WinHttpReq := CreateOleObject('WinHttp.WinHttpRequest.5.1');
    WinHttpReq.Open('HEAD', URL, False);
    WinHttpReq.Send;
    
    if WinHttpReq.Status = 200 then
    begin
      if WinHttpReq.GetResponseHeader('Content-Length') > 0 then
        Result := True;
    end;
  except
    MsgBox('Failed to check if latest release has files. Exception: ' + GetExceptionMessage, mbError, MB_OK);
  end;
end;

function FindNextQuote(const JSON: string; StartIndex: Integer): Integer;
var
  I: Integer;
begin
  I := StartIndex;
  while (I <= Length(JSON)) do
  begin
    if JSON[I] = '"' then
    begin
      Result := I;
      Exit;
    end;
    Inc(I);
  end;
  Result := -1; // Return -1 if not found
end;

function LastCharPos(const Substr: string; const S: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := Length(S) downto 1 do
  begin
    if Substr = S[i] then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function ExtractResolution(AssetName: String): String;
var
  UnderscorePos, DashPos, DelimiterPos: Integer;
  Resolution: String;
begin
  Resolution := '';

  // Find the position where the resolution starts (right after the last '_' or '-')
  UnderscorePos := LastCharPos('_', AssetName);
  DashPos := LastCharPos('-', AssetName);

  // Determine the correct delimiter position
  if UnderscorePos > DashPos then
    DelimiterPos := UnderscorePos
  else
    DelimiterPos := DashPos;

  if DelimiterPos > 0 then
  begin
    // Extract numeric characters from the filename starting after the delimiter
    Inc(DelimiterPos); // Move past the delimiter
    while (DelimiterPos <= Length(AssetName)) do
    begin
      if (AssetName[DelimiterPos] >= '0') and (AssetName[DelimiterPos] <= '9') or (AssetName[DelimiterPos] = 'k') then
      begin
        Resolution := Resolution + AssetName[DelimiterPos];
        Inc(DelimiterPos);
      end
      else
        Break; // Stop loop if character is not a digit or 'k'
    end;
  end;

  // Return the extracted resolution
  Result := Resolution;
end;

procedure PopulateResolutions(ComboBox: TComboBox; Repo: string);
var
  HttpCli: Variant;
  JSON, AssetName: string;
  I, J: Integer;
  AddedResolutions: TStringList;
begin
  AddedResolutions := TStringList.Create;
  try
    Log('Populating resolutions for ' + Repo);
    
    // Clear any existing items in the ComboBox and the list of added resolutions
    ComboBox.Items.Clear;
    AddedResolutions.Clear;

    HttpCli := CreateOleObject('WinHttp.WinHttpRequest.5.1');
    HttpCli.Open('GET', GetLatestReleaseAssets(Repo), False);
    HttpCli.SetRequestHeader('Authorization', 'token ' + MyAccessToken);
    HttpCli.Send;

    if HttpCli.Status = 200 then
    begin
      Log('Received HTTP 200 OK response for ' + Repo);
      
      JSON := HttpCli.ResponseText;
      I := 1;
      while I > 0 do
      begin
        I := GetSubstringPosition('"name":"', JSON, I);
        if I > 0 then
        begin
          I := I + Length('"name":"');
          J := GetSubstringPosition('"', JSON, I);
          if J > 0 then
          begin
            AssetName := Copy(JSON, I, J - I);
            Log('Found asset: ' + AssetName);
            // Extract resolution from the asset name
            Resolution := ExtractResolution(AssetName);
            Log('Extracted resolution: ' + Resolution);

            // Check if this resolution has already been added
            if (Resolution <> '') and (AddedResolutions.IndexOf(Resolution) = -1) then
            begin
              Log('Adding resolution to ComboBox: ' + Resolution);
              ComboBox.Items.Add(Resolution); // Add item to ComboBox
              AddedResolutions.Add(Resolution); // Add resolution to the list of added resolutions
            end;

            I := J + 1;
          end;
        end;
      end;

      if ComboBox.Items.Count > 0 then
        Log('Setting default item in ComboBox');
        ComboBox.ItemIndex := 0; // Default to the first item
    end
    else
    begin
      Log('Failed to retrieve assets for ' + Repo + '. Status: ' + IntToStr(HttpCli.Status));
      MsgBox('Failed to retrieve assets for ' + Repo + '. Status: ' + IntToStr(HttpCli.Status), mbError, MB_OK);
    end;

  except
    Log('Exception occurred while populating resolutions for ' + Repo + ': ' + GetExceptionMessage);
    MsgBox('Failed to populate resolutions for ' + Repo + '. Exception: ' + GetExceptionMessage, mbError, MB_OK);
  end;

  AddedResolutions.Free;
end;


function CheckRP1Confirmation: Boolean;
begin
  Log('Checking RP-1 confirmation');
  Result := True;
  if not RP1Checkbox.Checked then
  begin
    Log('RP-1 confirmation not checked');
    MsgBox('Please confirm that you have installed and launched RP-1.', mbError, MB_OK);
    Result := False; // Prevents proceeding to the next wizard page
  end
  else
  begin
    Log('RP-1 confirmation checked');
  end;
end;

procedure InitializeWizard;
var
  i: Integer;
  ComboBox: TComboBox;
  ResolutionPages: array of TWizardPage;
  ResolutionCombos: array of TComboBox;
  EVEAndScattererPage: TInputOptionWizardPage;
begin

  Log('Initializing wizard');
  
  ITD_Init();
  
  // Initialize DownloadsDir
  DownloadsDir := ExpandConstant('{userdocs}\Downloads');
  Log('Downloads directory initialized: ' + DownloadsDir);
  
  // Initialize BodyRepos array
  InitializeBodyRepos;
  Log('Body repositories initialized');
  
  // Set options if needed, for example, set the download timeout to 60 seconds:
  ITD_SetOption('Timeout', '60');

  // Specify that the download page should appear after the "Ready to Install" page
  ITD_DownloadAfter(wpReady);

  // Create RP-1 installation confirmation checkbox
  RP1Checkbox := TNewCheckBox.Create(WizardForm);
  RP1Checkbox.Parent := WizardForm.WelcomePage;
  RP1Checkbox.Left := ScaleX(18); // Adjust left position
  RP1Checkbox.Top := ScaleY(180); // Adjust top position
  RP1Checkbox.Width := WizardForm.ClientWidth - ScaleX(36);
  RP1Checkbox.Caption := 'I confirm that I have successfully installed and launched RP-1.';
  RP1Checkbox.Checked := False; // Default to unchecked
  Log('RP-1 installation confirmation checkbox created');
  
  // Create custom page for asking about EVE and Scatterer
  EVEAndScattererPage := CreateInputOptionPage(
    wpWelcome,
    'EVE and Scatterer',
    'Please select if you have downloaded and installed Blackrack''s EVE and Scatterer.',
    '',
    True, False);
  EVEAndScattererPage.Add('Yes, I have downloaded Blackrack''s zips for EVE and Scatterer.');
  EVEAndScattererPage.Add('No, I wish to use RSSVE.');
  EVEAndScattererPage.SelectedValueIndex := 0; // Set default selection to 'Yes'
  Log('EVE and Scatterer page created');
  
    // Create custom pages for selecting resolutions
    SetArrayLength(ResolutionPages, Length(BodyRepos));
    SetArrayLength(ResolutionCombos, Length(BodyRepos));
    
    for i := 0 to High(BodyRepos) do
    begin
      Log('Creating resolution page for ' + BodyRepos[i]);
      ResolutionPages[i] := CreateCustomPage(EVEAndScattererPage.ID, 'Select Resolution for ' + Copy(BodyRepos[i], 11, Length(BodyRepos[i]) - 10), 'Select the desired resolution for ' + Copy(BodyRepos[i], 11, Length(BodyRepos[i]) - 10));
      ComboBox := TComboBox.Create(ResolutionPages[i]);
      ComboBox.Parent := ResolutionPages[i].Surface;
      ComboBox.Left := ScaleX(18);
      ComboBox.Top := ScaleY(180);
      ComboBox.Width := ResolutionPages[i].SurfaceWidth - ScaleX(36);
      PopulateResolutions(ComboBox, BodyRepos[i]);
      ComboBox.ItemIndex := 0; // Default to the first item
      ResolutionCombos[i] := ComboBox;
      Log('Resolution page created for ' + BodyRepos[i]);
    end;
    
    Log('Wizard initialization completed');
end;

function NextButtonClick(CurPageID: Integer): Boolean;
begin
  Result := True; // Allow navigation by default
  
  // Validate RP-1 checkbox before proceeding
  if (CurPageID = wpWelcome) and (not RP1Checkbox.Checked) then
  begin
    MsgBox('You must have RP-1 installed and launched at least once before proceeding.', mbError, MB_OK);
    Result := False; // Prevent navigation
  end;
end;

procedure MoveDownloadedEVEAndScatterer;
begin
  try
    Log('Moving downloaded EVE and Scatterer files');

    if FileExists(DownloadsDir + '\EnvironmentalVisualEnhancements.zip') then
    begin
      Log('Extracting EnvironmentalVisualEnhancements.zip');
      Extract7Zip(DownloadsDir + '\EnvironmentalVisualEnhancements.zip', KSP_DIR);
    end
    else
    begin
      Log('EnvironmentalVisualEnhancements.zip not found');
    end;

    if FileExists(DownloadsDir + '\Scatterer.zip') then
    begin
      Log('Extracting Scatterer.zip');
      Extract7Zip(DownloadsDir + '\Scatterer.zip', KSP_DIR);
    end
    else
    begin
      Log('Scatterer.zip not found');
    end;

    Log('Extraction completed');
  except
    MsgBox('Failed to move downloaded EVE and Scatterer files. Exception: ' + GetExceptionMessage, mbError, MB_OK);
  end;
end;

function DirectoryExists(Dir: string): Boolean;
var
  FindRec: TFindRec;
begin
  Result := FindFirst(AddBackslash(Dir) + '*.*', FindRec);
  FindClose(FindRec);
end;

function GetLatestReleaseVersion(Repo: string): string;
var
  HttpCli: Variant;
  JSON, TagName: string;
  I, J: Integer;
begin
  Log('Getting latest release version for repository: ' + Repo);
  Result := '';
  try
    HttpCli := CreateOleObject('WinHttp.WinHttpRequest.5.1');
    HttpCli.Open('GET', GitHubAPI + Repo + '/releases/latest', False);
    HttpCli.SetRequestHeader('User-Agent', 'InnoSetup');
    HttpCli.SetRequestHeader('Authorization', 'token ' + MyAccessToken); // Add authorization header
    HttpCli.Send;
    
    if HttpCli.Status = 200 then
    begin
      Log('Request successful, status: ' + IntToStr(HttpCli.Status));
      JSON := HttpCli.ResponseText;
      I := Pos('"tag_name":"', JSON);
      if I > 0 then
      begin
        I := I + Length('"tag_name":"');
        JSON := Copy(JSON, I, Length(JSON) - I + 1);
        J := Pos('"', JSON);
        TagName := Copy(JSON, 1, J - 1);
        Result := TagName;
        Log('Latest release version found: ' + Result);
      end;
    end
    else
    begin
      Log('Failed to get the latest release version for ' + Repo + '. Status: ' + IntToStr(HttpCli.Status));
      MsgBox('Failed to get the latest release version for ' + Repo + '. Status: ' + IntToStr(HttpCli.Status), mbError, MB_OK);
    end;
  except
    Log('Exception during HTTP request for latest release version of ' + Repo);
    MsgBox('Exception during HTTP request for latest release version of ' + Repo, mbError, MB_OK);
  end;
end;

procedure InitializeDownloadsDir;
begin
  DownloadsDir := ExpandConstant('{userdocs}\Downloads');
  Log('Downloads directory initialized: ' + DownloadsDir);
end;

procedure AddToDownloadList(RepoName, TempFileName: string);
var
  LatestReleaseURL: string;
begin
  LatestReleaseURL := GetLatestReleaseAssets(RepoName);
  if LatestReleaseURL <> '' then
    DownloadList.Add(LatestReleaseURL + ExtractFileName(TempFileName) + '=' + TempFileName);
end;


// Function to get the latest release URL
function GetRepoDownloadURL(Repo: string): string; begin
  Result := GetLatestReleaseAssets(Repo);
end;

procedure InitializeDownloadList;
var
  LatestVersion: string;
  ParallaxURL, ParallaxScatterTexturesURL: string;
  LatestReleaseURL: string;
begin
  DownloadList := TStringList.Create;

  // RSS-Configs
  LatestVersion := GetLatestReleaseVersion('RSS-Reborn/RSS-Configs');
  if LatestVersion <> '' then
  begin
    LatestReleaseURL := 'https://github.com/RSS-Reborn/RSS-Configs/releases/download/' + LatestVersion + '/RSS_Configs.7z';
    AddToDownloadList(LatestReleaseURL, ExpandConstant('{tmp}\RSS_Configs.7z'));
  end;

  // RSS-Terrain
  LatestVersion := GetLatestReleaseVersion('RSS-Reborn/RSS-Terrain');
  if LatestVersion <> '' then
  begin
    LatestReleaseURL := 'https://github.com/RSS-Reborn/RSS-Terrain/releases/download/' + LatestVersion + '/RSS_Terrain.7z';
    AddToDownloadList(LatestReleaseURL, ExpandConstant('{tmp}\RSS_Terrain.7z'));
  end;

  // Planetary textures at user-selected resolutions
  for i := 0 to High(BodyRepos) do
  begin
    Resolution := ResolutionCombos[i].Text;
    LatestReleaseURL := GetLatestReleaseAssets(BodyRepos[i]);
    AddToDownloadList(LatestReleaseURL, ExpandConstant('{tmp}\') + BodyRepos[i] + '_' + Resolution + '.7z');
  end;

  // RSSVE-Configs (if EVE and Scatterer are installed)
  if EVEAndScattererPage.SelectedValueIndex = 0 then
  begin
    LatestVersion := GetLatestReleaseVersion('RSS-Reborn/RSSVE-Configs');
    if LatestVersion <> '' then
    begin
      LatestReleaseURL := 'https://github.com/RSS-Reborn/RSSVE-Configs/releases/download/' + LatestVersion + '/RSSVE_Configs.7z';
      AddToDownloadList(LatestReleaseURL, ExpandConstant('{tmp}\RSSVE_Configs.7z'));
    end;
  end;

  // RSSVE-Textures
  LatestVersion := GetLatestReleaseVersion('RSS-Reborn/RSSVE-Textures');
  if LatestVersion <> '' then
  begin
    LatestReleaseURL := 'https://github.com/RSS-Reborn/RSSVE-Textures/releases/download/' + LatestVersion + '/RSSVE_Textures.7z';
    AddToDownloadList(LatestReleaseURL, ExpandConstant('{tmp}\RSSVE_Textures.7z'));
  end;

  // Scatterer (if not already downloaded by user)
  if not ScattererDownloaded then
  begin
    LatestVersion := GetLatestReleaseVersion('LGhassen/Scatterer');
    if LatestVersion <> '' then
    begin
      LatestReleaseURL := 'https://github.com/LGhassen/Scatterer/releases/download/' + LatestVersion + '/Scatterer.zip';
      AddToDownloadList(LatestReleaseURL, ExpandConstant('{tmp}\Scatterer.zip'));
    end;
  end;

  // EVE (if not already downloaded by user)
  if not EVEDownloaded then
  begin
    LatestVersion := GetLatestReleaseVersion('LGhassen/EnvironmentalVisualEnhancements');
    if LatestVersion <> '' then
    begin
      LatestReleaseURL := 'https://github.com/LGhassen/EnvironmentalVisualEnhancements/releases/download/' + LatestVersion + '/Environmental_Visual_Enhancements_Redux-' + LatestVersion + '.zip';
      AddToDownloadList(LatestReleaseURL, ExpandConstant('{tmp}\EVE-' + LatestVersion + '.zip'));
    end;
  end;

  // Download Parallax and Parallax_ScatterTextures
  LatestVersion := GetLatestReleaseVersion('Gameslinx/Tessellation');
  if LatestVersion <> '' then
  begin
    ParallaxURL := 'https://github.com/Gameslinx/Tessellation/releases/download/' + LatestVersion + '/Parallax-' + LatestVersion + '.zip';
    ParallaxScatterTexturesURL := 'https://github.com/Gameslinx/Tessellation/releases/download/' + LatestVersion + '/Parallax_ScatterTextures-' + LatestVersion + '.zip';

    AddToDownloadList(ParallaxURL, ExpandConstant('{tmp}\Parallax-' + LatestVersion + '.zip'));
    AddToDownloadList(ParallaxScatterTexturesURL, ExpandConstant('{tmp}\Parallax_ScatterTextures-' + LatestVersion + '.zip'));
  end;
end;

procedure DownloadAndExtractFiles;
var
  I: Integer;
  DownloadItem: string;
  URL, TempFile: string;
  DownloadResult: Integer;
begin
  for I := 0 to DownloadList.Count - 1 do
  begin
    // Get the full item from DownloadList
    DownloadItem := DownloadList[I];

    // Extract URL and TempFile from DownloadItem
    URL := ExtractFilePath(DownloadItem);
    TempFile := ExtractFileName(DownloadItem);

    Log('Downloading ' + URL + ' to ' + TempFile);

    // Add the file to download queue
    ITD_AddFile(URL, TempFile);

    // Show download progress screen after page wpReady
    ITD_DownloadAfter(wpReady);

    // Initiate download (assuming synchronous download for simplicity)
    DownloadResult := ITD_DownloadFile(URL, TempFile);

    // Check download result
    if DownloadResult = 0 then
    begin
      Log('Downloaded ' + URL + '. Extracting...');

      // Extract the downloaded file (assuming 7zip extraction)
      if Extract7Zip(TempFile, ExpandConstant('{tmp}')) then
      begin
        Log('Extraction completed for ' + URL);
        Log('Installation completed for ' + URL);
      end
      else
      begin
        Log('Failed to extract ' + URL);
        MsgBox('Failed to extract ' + URL + '.', mbError, MB_OK);
      end;
    end
    else
    begin
      Log('Failed to download ' + URL + '. Error code: ' + IntToStr(DownloadResult));
      MsgBox('Failed to download ' + URL + '.', mbError, MB_OK);
    end;
  end;
end;


procedure InitializeAndDownload;
begin
  InitializeDownloadList;
  DownloadAndExtractFiles;

  // Clean up download list
  DownloadList.Free;
end;

procedure VerifyDownloadAndExtraction;
var
  I: Integer;
  Entry, URL, TempFile: string;
  DelimiterPos: Integer;
begin
  Log('Verifying downloaded and extracted files...');

  for I := 0 to DownloadList.Count - 1 do
  begin
    // Extract the entire entry from DownloadList
    Entry := DownloadList[I];

    // Find the position of the '=' delimiter
    DelimiterPos := Pos('=', Entry);

    // Ensure the delimiter is found and extract URL and TempFile
    if DelimiterPos > 0 then
    begin
      URL := Trim(Copy(Entry, 1, DelimiterPos - 1));
      TempFile := Trim(Copy(Entry, DelimiterPos + 1, Length(Entry) - DelimiterPos));
    end
    else
    begin
      Log('Invalid entry in DownloadList: ' + Entry);
      MsgBox('Invalid entry in DownloadList: ' + Entry, mbError, MB_OK);
      Exit; // Exit procedure early if any entry is invalid
    end;

    // Expand any constants in TempFile path
    TempFile := ExpandConstant(TempFile);

    // Check if the file exists
    if not FileExists(TempFile) then
    begin
      Log('Error: File not found after extraction: ' + TempFile);
      MsgBox('Error: File not found after extraction: ' + TempFile, mbError, MB_OK);
      Exit; // Exit procedure early if any file is missing
    end;
  end;

  Log('All files verified to be downloaded and extracted successfully.');
end;

function LastDelimiter(const Delimiters, S: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Length(S) downto 1 do
    if Pos(S[I], Delimiters) > 0 then
    begin
      Result := I;
      Exit;
    end;
end;

function ExtractFileNameWithoutExt(const FileName: string): string;
var
  I: Integer;
begin
  Result := ExtractFileName(FileName);
  I := LastDelimiter('.', Result);
  if I > 0 then
    SetLength(Result, I - 1);
end;

procedure MoveGameData(SourceDir, DestDir: string);
var
  ResultCode: Integer;
begin
  // Ensure the destination directory exists
  if not DirExists(DestDir) then
  begin
    if not CreateDir(DestDir) then
    begin
      Log(Format('Failed to create directory: %s', [DestDir]));
      Exit;
    end;
  end;

  // Move all files and directories from SourceDir to DestDir
  if not Exec('cmd.exe', '/C move "' + SourceDir + '\*" "' + DestDir + '"', '', SW_HIDE, ewWaitUntilTerminated, ResultCode) then
  begin
    Log(Format('Failed to move files from %s to %s', [SourceDir, DestDir]));
  end
  else if ResultCode <> 0 then
  begin
    Log(Format('Move command failed with code %d', [ResultCode]));
  end;
end;

procedure MergeGameDataFolders;
var
  I: Integer;
  SourceDir, DestDir: string;
begin
  DestDir := ExpandConstant('{userdesktop}\MergedGameData');
  
  for I := 0 to DownloadList.Count - 1 do
  begin
    SourceDir := ExpandConstant('{tmp}') + '\' + ExtractFileNameWithoutExt(DownloadList[I]) + '\GameData';
    if DirExists(SourceDir) then
    begin
      MoveGameData(SourceDir, DestDir);
    end
    else
    begin
      Log(Format('GameData folder not found in %s', [SourceDir]));
    end;
  end;
end;

procedure CurStepChanged(CurStep: TSetupStep);
var
  TempZip, LatestReleaseURL, DownloadURL, Resolution: string;
  i: Integer;
  LatestVersion: string;
  ParallaxZipFileName: string;
  ParallaxScatterTexturesZipFileName: string;
  ParallaxURL, ParallaxScatterTexturesURL: string;

begin
  if CurStep = ssInstall then
  ITD_Init;
  begin
  
    Log('Starting RSS Reborn installation process.');
    
    // Clear the download directory
    InitializeDownloadsDir;
    //ClearDownloadDirectory;
    
    // Check RP-1 installation confirmation
    if not RP1Checkbox.Checked then
    begin
      Log('RP-1 installation confirmation failed. Aborting installation.');
      MsgBox('You must have RP-1 installed and launched at least once before proceeding.', mbError, MB_OK);
      WizardForm.Close; // Close the wizard to terminate installation
      Exit;
    end;
    
    // Remove specified folders
    Log('Removing obsolete folders.');

    // Remove Kopernicus if it exists
    if DirectoryExists(KSP_DIR + '\Kopernicus') then
    begin
      if not DelTree(KSP_DIR + '\Kopernicus', True, True, True) then
      begin
        Log('Failed to delete Kopernicus directory.');
      end
      else
      begin
        Log('Kopernicus directory deleted.');
      end;
    end
    else
    begin
      Log('Kopernicus directory does not exist.');
    end;

    // Remove Parallax if it exists
    if DirectoryExists(KSP_DIR + '\Parallax') then
    begin
      if not DelTree(KSP_DIR + '\Parallax', True, True, True) then
      begin
        Log('Failed to delete Parallax directory.');
      end
      else
      begin
        Log('Parallax directory deleted.');
      end;
    end
    else
    begin
      Log('Parallax directory does not exist.');
    end;

    // Remove Parallax_StockTextures if it exists
    if DirectoryExists(KSP_DIR + '\Parallax_StockTextures') then
    begin
      if not DelTree(KSP_DIR + '\Parallax_StockTextures', True, True, True) then
      begin
        Log('Failed to delete Parallax_StockTextures directory.');
      end
      else
      begin
        Log('Parallax_StockTextures directory deleted.');
      end;
    end
    else
    begin
      Log('Parallax_StockTextures directory does not exist.');
    end;

    // Remove RSS-Textures if it exists
    if DirectoryExists(KSP_DIR + '\RSS-Textures') then
    begin
      if not DelTree(KSP_DIR + '\RSS-Textures', True, True, True) then
      begin
        Log('Failed to delete RSS-Textures directory.');
      end
      else
      begin
        Log('RSS-Textures directory deleted.');
      end;
    end
    else
    begin
      Log('RSS-Textures directory does not exist.');
    end;

    // Remove RSSVE if it exists
    if DirectoryExists(KSP_DIR + '\RSSVE') then
    begin
      if not DelTree(KSP_DIR + '\RSSVE', True, True, True) then
      begin
        Log('Failed to delete RSSVE directory.');
      end
      else
      begin
        Log('RSSVE directory deleted.');
      end;
    end
    else
    begin
      Log('RSSVE directory does not exist.');
    end;

    // Remove RealSolarSystem if it exists
    if DirectoryExists(KSP_DIR + '\RealSolarSystem') then
    begin
      if not DelTree(KSP_DIR + '\RealSolarSystem', True, True, True) then
      begin
        Log('Failed to delete RealSolarSystem directory.');
      end
      else
      begin
        Log('RealSolarSystem directory deleted.');
      end;
    end
    else
    begin
      Log('RealSolarSystem directory does not exist.');
    end;

    Log('Folders removal completed');

    // Initialize and start downloading
    Log('Starting download process.');
    InitializeAndDownload;
    Log('Finished downloading.');
    
    // Download  Parallax
    LatestVersion := GetLatestReleaseVersion('Gameslinx/Tessellation');
    if LatestVersion = '' then
    begin
      Log('Failed to retrieve the latest release version.');
      MsgBox('Failed to retrieve the latest release version.', mbError, MB_OK);
      Exit;
    end;

    // Construct URLs for downloading Parallax and Parallax_ScatterTextures
    ITD_AddFile('https://api.github.com/repos/Gameslinx/Tessellation/releases/tags/' + LatestVersion + '/Parallax-' + LatestVersion + '.zip', ExpandConstant('{tmp}\Parallax.zip'));
    ITD_AddFile('https://api.github.com/repos/Gameslinx/Tessellation/releases/tags/' + LatestVersion + '/Parallax_ScatterTextures-' + LatestVersion + '.zip', ExpandConstant('{tmp}\Parallax_ScatterTextures.zip'));


    // Construct URLs for downloading Parallax and Parallax_ScatterTextures
    ParallaxURL := 'https://github.com/Gameslinx/Tessellation/releases/download/' + LatestVersion + '/Parallax-' + LatestVersion + '.zip';
    ParallaxScatterTexturesURL := 'https://github.com/Gameslinx/Tessellation/releases/download/' + LatestVersion + '/Parallax_ScatterTextures-' + LatestVersion + '.zip';

    // Download Parallax_ScatterTextures
    Log('Downloading ' + ParallaxScatterTexturesURL + ' to ' + ExpandConstant('{tmp}\') + 'Parallax_ScatterTextures-' + LatestVersion + '.zip');
    ITD_AddFile(ParallaxScatterTexturesURL, ExpandConstant('{tmp}\') + 'Parallax_ScatterTextures-' + LatestVersion + '.zip');
    ITD_DownloadAfter(wpReady); // Show download progress screen after page wpReady

  end;

  // Download and extract all files in the download list
  DownloadAndExtractFiles;
  
  // Merge the GameData folders
  MergeGameDataFolders;
  Log('GameData folders merged successfully.');
  MsgBox('GameData folders merged successfully!', mbInformation, MB_OK);
 

  Log('RSS Reborn installation process completed successfully.');
  MsgBox('RSS Reborn has been installed successfully!', mbInformation, MB_OK);
  
end;