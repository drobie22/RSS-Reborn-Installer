; Inno Setup Script
; David Robie (DRobie22)
; This installer follows RSS Reborn's GitHub Instructions

#define MyAppName "RSS-Reborn"
#define MyAppVersion "0.1"
#define MyAppPublisher "DRobie22"
#define MyAppURL "https://github.com/RSS-Reborn/RSS-Reborn"
#define MyAppExeName "RSS-Reborn-Installer.exe"

#include "it_download.iss"

[Setup]
AppName={#MyAppName}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
AppVersion={#MyAppVersion}
BackColor=$cccccc
Compression=lzma
CreateAppDir=no
DisableWelcomePage=no
OutputBaseFilename=RSSRebornInstaller
SetupLogging=yes
SolidCompression=yes
WizardImageFile=backgroundinstaller.bmp
WizardImageStretch=no
WizardSmallImageFile=icon.bmp
WizardStyle=modern

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
  GitHubAPI = 'https://api.github.com/repos/';
  KSP_DIR = 'C:\Program Files (x86)\Steam\steamapps\common\Kerbal Space Program\GameData';
  MAX_PATH = 260;
  RSSConfigsRepo = 'RSS-Reborn/RSS-Configs';
  RSSTexturesRepo = 'RSS-Reborn/RSS-Terrain';
  S_OK = 0;
  URLMON_DLL = 'urlmon.dll';
  
var
  BodyRepos: array[0..11] of string;
  DownloadList: TStringList;
  DownloadsDir: string;
  EVEdownloaded: Boolean;
  I: Integer;
	MyAccessToken: string;
  Resolution: string;
  ResolutionCombos: array of TComboBox;
  RP1Checkbox: TNewCheckBox;
	EVEAndScattererCheckbox: TNewCheckBox;
  ScattererDownloaded: Boolean;
	
type
  TResolutionPages = array of TWizardPage;
  TResolutionCombos = array of TComboBox;
	
procedure InitializeVariables;
begin
  EVEDownloaded := False;
  ScattererDownloaded := False;
	DownloadList := TStringList.Create;
end;
	
procedure DeinitializeVariables;
begin
  DownloadList.Free;
end;	
	
procedure InitializeGitHubAPI;
// Used to allow downloads on GitHub without being blocked
begin
  MyAccessToken := GetEnv('MY_ACCESS_TOKEN'); // Get token from environment variable
  if MyAccessToken = '' then
  begin
    MsgBox('GitHub Access Token is not set.', mbError, MB_OK);
    Exit;
  end;
  Log('GitHub Access Token retrieved: ' + MyAccessToken); // Log token retrieval for debugging purposes
end;

function DirectoryExists(Dir: string): Boolean;
// Helper function
var
  FindRec: TFindRec;
begin
  Result := FindFirst(AddBackslash(Dir) + '*.*', FindRec);
  FindClose(FindRec);
end;
	
function IsDirectoryEmpty(DirPath: string): Boolean;
// Helper function
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

function LastDelimiter(const Delimiters, S: string): Integer;
// Helper function for string manipulation
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
// Helper function for string manipulation
var
  I: Integer;
begin
  Result := ExtractFileName(FileName);
  I := LastDelimiter('.', Result);
  if I > 0 then
    SetLength(Result, I - 1);
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
// Helper function for string manipulation
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

function GetSubstringPosition(const SubStr, Str: string; StartPos: Integer): Integer;
// Helper function for string manipulation
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

function PosEx(const SubStr, S: string; Offset: Integer): Integer;
var
  TempStr: string;
begin
  TempStr := Copy(S, Offset, Length(S) - Offset + 1);
  Result := Pos(SubStr, TempStr);
  if Result <> 0 then
    Result := Result + Offset - 1;
end;

procedure InitializeBodyRepos;
// Identifies all planetary bodies (and the sun/moon too)
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
// Uses 7 Zip to extract files. User will need to have 7 Zip. 7Za.exe is included with installer
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

function GetLatestReleaseAssets(Repo, Resolution: string): string;
var
  HttpCli: Variant;
  JSON, TagName, AssetsURL, AssetsJSON, AssetName, BrowserDownloadURL: string;
  AccessToken: string;
  AssetURLs: TStringList;
  I, J, StartPos: Integer;
begin
  Result := '';
  AssetURLs := TStringList.Create;

  Log('GetLatestReleaseAssets called with Repo: ' + Repo + ' and Resolution: ' + Resolution);

  // Check if the Repo string is already a full URL
  if Pos('https://', Repo) = 1 then
  begin
    Log('Repo is a full URL, returning directly: ' + Repo);
    Result := Repo;
    AssetURLs.Free;
    Exit;
  end;

  try
    // Retrieve the access token from the environment variable
    AccessToken := GetEnv('MY_ACCESS_TOKEN');
    if AccessToken = '' then
    begin
      Log('No GitHub Access Token found, proceeding without it.');
    end
    else
    begin
      Log('GitHub Access Token retrieved.');
    end;

    HttpCli := CreateOleObject('WinHttp.WinHttpRequest.5.1');
    HttpCli.Open('GET', GitHubAPI + Repo + '/releases/latest', False);
    HttpCli.SetRequestHeader('User-Agent', 'RSS-Reborn-Installer');
    
    if AccessToken <> '' then
    begin
      HttpCli.SetRequestHeader('Authorization', 'token ' + AccessToken); // Add authorization header if token is present
      Log('Authorization header set with token.');
    end;

    Log('Sending request to: ' + GitHubAPI + Repo + '/releases/latest');
    HttpCli.Send;

    if HttpCli.Status = 200 then
    begin
      Log('Received HTTP 200 OK response.');
      JSON := HttpCli.ResponseText;
      Log('Latest release JSON: ' + JSON);

      // Extract tag name
      I := Pos('"tag_name":"', JSON);
      if I > 0 then
      begin
        I := I + Length('"tag_name":"');
        J := FindNextQuote(JSON, I);
        if J > 0 then
        begin
          TagName := Copy(JSON, I, J - I);
          Log('Extracted Tag Name: ' + TagName);
        end
        else
        begin
          Log('Failed to find the end of tag name.');
        end;
      end
      else
      begin
        Log('Failed to find tag name in the JSON.');
      end;

      // Extract assets_url
      I := Pos('"assets_url":"', JSON);
      if I > 0 then
      begin
        I := I + Length('"assets_url":"');
        J := FindNextQuote(JSON, I);
        if J > 0 then
        begin
          AssetsURL := Copy(JSON, I, J - I);
          Log('Extracted Assets URL: ' + AssetsURL);

          // Request the assets JSON
          HttpCli.Open('GET', AssetsURL, False);
          HttpCli.SetRequestHeader('User-Agent', 'RSS-Reborn-Installer');
          if AccessToken <> '' then
          begin
            HttpCli.SetRequestHeader('Authorization', 'token ' + AccessToken); // Add authorization header if token is present
            Log('Authorization header set with token for assets URL.');
          end;

          Log('Sending request to assets URL: ' + AssetsURL);
          HttpCli.Send;

          if HttpCli.Status = 200 then
          begin
            Log('Received HTTP 200 OK response for assets URL.');
            AssetsJSON := HttpCli.ResponseText;
            Log('Assets JSON: ' + AssetsJSON);

            // Extract and filter asset names based on the selected resolution
            StartPos := 1;
            while StartPos > 0 do
            begin
              I := PosEx('"name":"', AssetsJSON, StartPos);
              if I > 0 then
              begin
                I := I + Length('"name":"');
                J := FindNextQuote(AssetsJSON, I);
                if J > 0 then
                begin
                  AssetName := Copy(AssetsJSON, I, J - I);
                  Log('Found asset: ' + AssetName);

                  // Check if the asset name contains the selected resolution
                  if (Resolution = '') or (Pos(Resolution, AssetName) > 0) then
                  begin
                    // Extract the browser download URL
                    I := PosEx('"browser_download_url":"', AssetsJSON, J);
                    if I > 0 then
                    begin
                      I := I + Length('"browser_download_url":"');
                      J := FindNextQuote(AssetsJSON, I);
                      if J > 0 then
                      begin
                        BrowserDownloadURL := Copy(AssetsJSON, I, J - I);
                        Log('Adding asset URL: ' + BrowserDownloadURL);
                        AssetURLs.Add(BrowserDownloadURL);
                      end
                      else
                      begin
                        Log('Failed to find the end of browser download URL.');
                      end;
                    end
                    else
                    begin
                      Log('Failed to find browser download URL for asset: ' + AssetName);
                    end;
                  end;

                  StartPos := J + 1;  // Move past the current asset name
                end
                else
                begin
                  Log('Failed to find the end of asset name.');
                end;
              end
              else
                Break;  // Exit the loop if no more assets are found
            end;
          end
          else if HttpCli.Status = 404 then
          begin
            Log('Assets not found for ' + Repo + '. Status: 404');
            MsgBox('Assets not found for ' + Repo + '. Status: 404', mbError, MB_OK);
          end
          else
          begin
            Log('Failed to retrieve assets for ' + Repo + '. Status: ' + IntToStr(HttpCli.Status));
            MsgBox('Failed to retrieve assets for ' + Repo + '. Status: ' + IntToStr(HttpCli.Status), mbError, MB_OK);
          end;
        end
        else
        begin
          Log('Failed to find the end of assets URL.');
        end;
      end
      else
      begin
        Log('Failed to find assets URL in the JSON.');
      end;
    end
    else
    begin
      Log('Failed to get the latest release for ' + Repo + '. Status: ' + IntToStr(HttpCli.Status));
      MsgBox('Failed to get the latest release for ' + Repo + '. Status: ' + IntToStr(HttpCli.Status), mbError, MB_OK);
    end;
  except
    Log('Exception during HTTP request for latest release assets of ' + Repo + ': ' + GetExceptionMessage);
    MsgBox('Exception during HTTP request for latest release assets of ' + Repo + ': ' + GetExceptionMessage, mbError, MB_OK);
  end;

  // Now that we have the URLs, we proceed to download using itdownload.dll
  for I := 0 to AssetURLs.Count - 1 do
  begin
    try
      Log('Downloading asset: ' + AssetURLs[I]);
      ITD_AddFile(PChar(AssetURLs[I]), PChar(ExpandConstant('{tmp}\') + ExtractFileName(AssetURLs[I])));
      ITD_DownloadFiles;  // Call the procedure without checking return value
    except
      Log('Exception while downloading asset: ' + AssetURLs[I] + ': ' + GetExceptionMessage);
      MsgBox('Exception while downloading asset: ' + AssetURLs[I] + ': ' + GetExceptionMessage, mbError, MB_OK);
    end;
  end;

	// If there are any URLs, concatenate them into a single string
	if AssetURLs.Count > 0 then
	begin
		for I := 0 to AssetURLs.Count - 1 do
		begin
			if I > 0 then
				Result := Result + #13#10; // Add a line break between URLs
			Result := Result + AssetURLs[I];
		end;
	end;

  AssetURLs.Free;
end;

function LatestReleaseHasFiles(URL: string): Boolean;
// Need to check if a GitHub release is legit
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

function ExtractResolution(AssetName: String): String;
// Identifies available resolutions in GitHub Releases of Bodies
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
  AssetURLs: TStringList;
  AssetName, Resolution: string;
  I: Integer;
  AddedResolutions: TStringList;
  URLs: string;
begin
  AddedResolutions := TStringList.Create;
  AssetURLs := TStringList.Create;
  try
    Log('Populating resolutions for ' + Repo);

    // Clear any existing items in the ComboBox and the list of added resolutions
    ComboBox.Items.Clear;
    AddedResolutions.Clear;

    // Get the list of asset URLs as a single string, then split it into a TStringList
    URLs := GetLatestReleaseAssets(Repo, '');
    AssetURLs.Text := URLs;

    if AssetURLs.Count = 0 then
    begin
      Log('Failed to get assets for ' + Repo);
      Exit;
    end;

    // Process each asset URL to extract resolutions
    for I := 0 to AssetURLs.Count - 1 do
    begin
      AssetName := ExtractFileName(AssetURLs[I]);
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
    end;

    // Set default item in ComboBox if resolutions were added
    if ComboBox.Items.Count > 0 then
    begin
      Log('Setting default item in ComboBox');
      ComboBox.ItemIndex := 0; // Default to the first item
    end;
  except
    Log('Exception occurred while populating resolutions for ' + Repo + ': ' + GetExceptionMessage);
    MsgBox('Failed to populate resolutions for ' + Repo + '. Exception: ' + GetExceptionMessage, mbError, MB_OK);
  end;

  AddedResolutions.Free;
  AssetURLs.Free;
end;

function CheckRP1Confirmation: Boolean;
// User is required to have RP-1 RSS/RO Modpack installed and working before installing RSS Reborn
begin
  Log('Checking RP-1 confirmation and EVE/Scatterer download confirmation');
  Result := True;
  
  if not RP1Checkbox.Checked then
  begin
    Log('RP-1 confirmation not checked');
    MsgBox('Please confirm that you have installed and launched RP-1 at least once. RSS Reborn will not work if RP-1 does not work.', mbError, MB_OK);
    Result := False; // Prevents proceeding to the next wizard page
  end
  else
  begin
    Log('RP-1 confirmation checked');
  end;
  
  if EVEAndScattererCheckbox.Checked then
  begin
    Log('EVE and Scatterer download confirmation checked');
    MsgBox('Please ensure that Blackrack''s Patreon EVE and Scatterer zip folders are in your downloads.', mbInformation, MB_OK);
  end;
end;


procedure InitializeWizard;
var
  i: Integer;
  ComboBox: TComboBox;
  ResolutionPages: array of TWizardPage;
begin
  Log('Initializing wizard');

  // BodyRepos can be initialized here as it doesn't involve any network operations
  InitializeBodyRepos;
  InitializeVariables;

  ITD_Init();

  // Initialize DownloadsDir
  DownloadsDir := ExpandConstant('{userdocs}\Downloads');
  Log('Downloads directory initialized: ' + DownloadsDir);

  // Set options if needed, for example, set the download timeout to 60 seconds:
  ITD_SetOption('Timeout', '60');

  // Specify that the download page should appear after the "Ready to Install" page
  ITD_DownloadAfter(wpReady);

  // Create RP-1 installation confirmation checkbox
  RP1Checkbox := TNewCheckBox.Create(WizardForm);
  RP1Checkbox.Parent := WizardForm.WelcomePage;
  RP1Checkbox.Left := ScaleX(18); // Adjust left position
  RP1Checkbox.Top := ScaleY(220); // Adjust top position
  RP1Checkbox.Width := WizardForm.ClientWidth - ScaleX(36);
  RP1Checkbox.Height := ScaleY(40); // Adjust height for text wrapping
  RP1Checkbox.Caption := 'I confirm that I have successfully run RP-1 once.';
  RP1Checkbox.Checked := False; // Default to unchecked
  Log('RP-1 installation confirmation checkbox created');

  // Create EVE and Scatterer download confirmation checkbox
  EVEAndScattererCheckbox := TNewCheckBox.Create(WizardForm);
  EVEAndScattererCheckbox.Parent := WizardForm.WelcomePage;
  EVEAndScattererCheckbox.Left := ScaleX(18); // Adjust left position
  EVEAndScattererCheckbox.Top := ScaleY(260); // Adjust top position
  EVEAndScattererCheckbox.Width := WizardForm.ClientWidth - ScaleX(36);
  EVEAndScattererCheckbox.Height := ScaleY(40); // Adjust height for text wrapping
  EVEAndScattererCheckbox.Caption := '(Optional) I am using Blackrack''s EVE and Scatterer.';
  EVEAndScattererCheckbox.Checked := False; // Default to unchecked
  Log('EVE and Scatterer download confirmation checkbox created');
	
  // Create custom pages for selecting resolutions
  SetLength(ResolutionPages, Length(BodyRepos));
  SetLength(ResolutionCombos, Length(BodyRepos));

  for i := 0 to High(BodyRepos) do
  begin
    Log('Creating resolution page for ' + BodyRepos[i]);
    ResolutionPages[i] := CreateCustomPage(wpWelcome, 'Select Resolution for ' + Copy(BodyRepos[i], 11, Length(BodyRepos[i]) - 10), 'Select the desired resolution for ' + Copy(BodyRepos[i], 11, Length(BodyRepos[i]) - 10));
    ComboBox := TComboBox.Create(ResolutionPages[i]);
    ComboBox.Parent := ResolutionPages[i].Surface;
    ComboBox.Left := ScaleX(18);
    ComboBox.Top := ScaleY(180);
    ComboBox.Width := ResolutionPages[i].SurfaceWidth - ScaleX(36);
    ResolutionCombos[i] := ComboBox;
    Log('Resolution page created for ' + BodyRepos[i]);
  end;

  Log('Wizard initialization completed');
end;

procedure MoveDownloadedEVEAndScatterer;
// If user has paid for Volumentric Clouds, installer takes the zips from the downloads folder
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

function GetLatestReleaseVersion(Repo: string): string;
// Retrieves the latest version of GitHub releases for assets, as some require them in file name
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
    if MyAccessToken <> '' then
    begin
      HttpCli.SetRequestHeader('Authorization', 'token ' + MyAccessToken); // Add authorization header
      Log('Authorization header set with token.');
    end
    else
    begin
      Log('GitHub Access Token is empty.');
    end;
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
    Log('Exception during HTTP request for latest release version of ' + Repo + ': ' + GetExceptionMessage);
    MsgBox('Exception during HTTP request for latest release version of ' + Repo + ': ' + GetExceptionMessage, mbError, MB_OK);
  end;
end;

procedure InitializeDownloadsDir;
// Installer needs a place to download from, temp location is on desktop
begin
  DownloadsDir := ExpandConstant('{userdocs}\Downloads');
  Log('Downloads directory initialized: ' + DownloadsDir);
end;

procedure AddToDownloadList(RepoName, Resolution, TempFileName: string);
var
  LatestReleaseURLs: string;
  URL: string;
  URLList: TStringList;
  I: Integer;
begin
  URLList := TStringList.Create;
  try
    LatestReleaseURLs := GetLatestReleaseAssets(RepoName, Resolution);
    
    // Split the returned URLs string into the TStringList
    URLList.Text := LatestReleaseURLs;
    
    if URLList.Count > 0 then
    begin
      for I := 0 to URLList.Count - 1 do
      begin
        URL := URLList[I];
        DownloadList.Add(URL + '=' + TempFileName);
      end;
    end;
  finally
    URLList.Free;
  end;
end;

function GetRepoDownloadURL(Repo, Resolution: string): string;
var
  URLs: TStringList;
begin
  URLs := TStringList.Create;
  try
    URLs.Text := GetLatestReleaseAssets(Repo, Resolution);
    if URLs.Count > 0 then
      Result := URLs.Text
    else
      Result := '';
  finally
    URLs.Free;
  end;
end;

procedure InitializeDownloadList;
// Includes everything required for RSS Reborn
var
  LatestVersion: string;
  ParallaxURL, ParallaxScatterTexturesURL: string;
  LatestReleaseURL: string;
begin
  ITD_ClearFiles;

  // RSS-Configs
  LatestVersion := GetLatestReleaseVersion('RSS-Reborn/RSS-Configs');
  if LatestVersion <> '' then
  begin
    LatestReleaseURL := 'https://api.github.com/RSS-Reborn/RSS-Configs/releases/download/' + LatestVersion + '/RSS_Configs.7z';
    AddToDownloadList(LatestReleaseURL, '', ExpandConstant('{tmp}\RSS_Configs.7z'));
  end;
  
  // RSS-Terrain
  LatestVersion := GetLatestReleaseVersion('RSS-Reborn/RSS-Terrain');
  if LatestVersion <> '' then
  begin
    LatestReleaseURL := 'https://api.github.com/RSS-Reborn/RSS-Terrain/releases/download/' + LatestVersion + '/RSS_Terrain.7z';
    AddToDownloadList(LatestReleaseURL, '', ExpandConstant('{tmp}\RSS_Terrain.7z'));
  end;

  // Planetary textures at user-selected resolutions
  for i := 0 to High(BodyRepos) do
  begin
    Resolution := ResolutionCombos[i].Text;
    LatestReleaseURL := GetRepoDownloadURL(BodyRepos[i], Resolution);
    if LatestReleaseURL <> '' then
      AddToDownloadList(LatestReleaseURL, Resolution, ExpandConstant('{tmp}\') + BodyRepos[i] + '_' + Resolution + '.7z');
  end;

  // RSSVE-Configs (if EVE and Scatterer are not installed)
  if not EVEAndScattererCheckbox.Checked then
  begin
    LatestVersion := GetLatestReleaseVersion('RSS-Reborn/RSSVE-Configs');
    if LatestVersion <> '' then
    begin
      LatestReleaseURL := 'https://github.com/RSS-Reborn/RSSVE-Configs/releases/download/' + LatestVersion + '/RSSVE_Configs.7z';
      AddToDownloadList(LatestReleaseURL, '', ExpandConstant('{tmp}\RSSVE_Configs.7z'));
    end;
  end;

  // RSSVE-Textures
  LatestVersion := GetLatestReleaseVersion('RSS-Reborn/RSSVE-Textures');
  if LatestVersion <> '' then
  begin
    LatestReleaseURL := 'https://github.com/RSS-Reborn/RSSVE-Textures/releases/download/' + LatestVersion + '/RSSVE_Textures.7z';
    AddToDownloadList(LatestReleaseURL, '', ExpandConstant('{tmp}\RSSVE_Textures.7z'));
  end;

  // Scatterer (if not already downloaded by user)
  if not ScattererDownloaded then
  begin
    LatestVersion := GetLatestReleaseVersion('LGhassen/Scatterer');
    if LatestVersion <> '' then
    begin
      LatestReleaseURL := 'https://github.com/LGhassen/Scatterer/releases/download/' + LatestVersion + '/Scatterer.zip';
      AddToDownloadList(LatestReleaseURL, '', ExpandConstant('{tmp}\Scatterer.zip'));
    end;
  end;

  // EVE (if not already downloaded by user)
  if not EVEDownloaded then
  begin
    LatestVersion := GetLatestReleaseVersion('LGhassen/EnvironmentalVisualEnhancements');
    if LatestVersion <> '' then
    begin
      LatestReleaseURL := 'https://github.com/LGhassen/EnvironmentalVisualEnhancements/releases/download/' + LatestVersion + '/Environmental_Visual_Enhancements_Redux-' + LatestVersion + '.zip';
      AddToDownloadList(LatestReleaseURL, '', ExpandConstant('{tmp}\EVE-' + LatestVersion + '.zip'));
    end;
  end;

  // Download Parallax and Parallax_ScatterTextures
  LatestVersion := GetLatestReleaseVersion('Gameslinx/Tessellation');
  if LatestVersion <> '' then
  begin
    ParallaxURL := 'https://github.com/Gameslinx/Tessellation/releases/download/' + LatestVersion + '/Parallax-' + LatestVersion + '.zip';
    ParallaxScatterTexturesURL := 'https://github.com/Gameslinx/Tessellation/releases/download/' + LatestVersion + '/Parallax_ScatterTextures-' + LatestVersion + '.zip';

    AddToDownloadList(ParallaxURL, '', ExpandConstant('{tmp}\Parallax-' + LatestVersion + '.zip'));
    AddToDownloadList(ParallaxScatterTexturesURL, '', ExpandConstant('{tmp}\Parallax_ScatterTextures-' + LatestVersion + '.zip'));
  end;
end;

procedure DownloadAndExtractFiles;
var
  I: Integer;
  DownloadItem, URL, TempFile: string;
  DelimiterPos: Integer;
  DownloadResult: Integer;
begin
  for I := 0 to DownloadList.Count - 1 do
  begin
    // Get the full item from DownloadList
    DownloadItem := DownloadList[I];

    // Find the position of the '=' delimiter
    DelimiterPos := Pos('=', DownloadItem);

    // Ensure the delimiter is found and extract URL and TempFile
    if DelimiterPos > 0 then
    begin
      URL := Trim(Copy(DownloadItem, 1, DelimiterPos - 1));
      TempFile := Trim(Copy(DownloadItem, DelimiterPos + 1, Length(DownloadItem) - DelimiterPos));
    end
    else
    begin
      Log('Invalid entry in DownloadList: ' + DownloadItem);
      MsgBox('Invalid entry in DownloadList: ' + DownloadItem, mbError, MB_OK);
      Continue; // Skip this item if it's invalid
    end;

    Log('Downloading ' + URL + ' to ' + TempFile);

    // Add the file to download queue
    ITD_AddFile(URL, TempFile);

    // Show download progress screen after page wpReady
    ITD_DownloadAfter(wpReady);

    // Initiate download
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
// Simple procedure to run init and download one after another
begin
  InitializeDownloadList;
  DownloadAndExtractFiles;

  // Clean up download list
  DownloadList.Free;
end;

procedure VerifyDownloadAndExtraction;
// Essential to check that downloads and extractions were successful
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

procedure MoveGameData(SourceDir, DestDir: string);
// All extracted folders need to be merged and moved together
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
// All extracted folders need to be merged and moved together
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

procedure CleanupTemporaryFiles;
var
  TempDir: string;
begin
  TempDir := ExpandConstant('{tmp}');
  if DirExists(TempDir) then
  begin
    Log('Cleaning up temporary files...');
    if not DelTree(TempDir, True, True, True) then
      Log('Failed to clean up temporary files in ' + TempDir)
    else
      Log('Temporary files cleaned up successfully.');
  end;
end;

procedure DeinitializeSetup();
begin
  Log('Deinitializing Setup.');
  CleanupTemporaryFiles; // Clean up temporary files
  DeinitializeVariables; // Free allocated resources
end;

procedure RemoveObsoleteFolders;
begin
  if DirectoryExists(KSP_DIR + '\Kopernicus') then
    if not DelTree(KSP_DIR + '\Kopernicus', True, True, True) then
      Log('Failed to delete Kopernicus directory.')
    else
      Log('Kopernicus directory deleted.')
  else
    Log('Kopernicus directory does not exist.');

  if DirectoryExists(KSP_DIR + '\Parallax') then
    if not DelTree(KSP_DIR + '\Parallax', True, True, True) then
      Log('Failed to delete Parallax directory.')
    else
      Log('Parallax directory deleted.')
  else
    Log('Parallax directory does not exist.');

  if DirectoryExists(KSP_DIR + '\Parallax_StockTextures') then
    if not DelTree(KSP_DIR + '\Parallax_StockTextures', True, True, True) then
      Log('Failed to delete Parallax_StockTextures directory.')
    else
      Log('Parallax_StockTextures directory deleted.')
  else
    Log('Parallax_StockTextures directory does not exist.');

  if DirectoryExists(KSP_DIR + '\RSS-Textures') then
    if not DelTree(KSP_DIR + '\RSS-Textures', True, True, True) then
      Log('Failed to delete RSS-Textures directory.')
    else
      Log('RSS-Textures directory deleted.')
  else
    Log('RSS-Textures directory does not exist.');

  if DirectoryExists(KSP_DIR + '\RSSVE') then
    if not DelTree(KSP_DIR + '\RSSVE', True, True, True) then
      Log('Failed to delete RSSVE directory.')
    else
      Log('RSSVE directory deleted.')
  else
    Log('RSSVE directory does not exist.');

  if DirectoryExists(KSP_DIR + '\RealSolarSystem') then
    if not DelTree(KSP_DIR + '\RealSolarSystem', True, True, True) then
      Log('Failed to delete RealSolarSystem directory.')
    else
      Log('RealSolarSystem directory deleted.')
  else
    Log('RealSolarSystem directory does not exist.');

  Log('Folders removal completed.');
end;

procedure DownloadParallaxTextures;
var
  LatestVersion: string;
  ParallaxURL, ParallaxScatterTexturesURL: string;
begin
  LatestVersion := GetLatestReleaseVersion('Gameslinx/Tessellation');
  if LatestVersion = '' then
  begin
    Log('Failed to retrieve the latest release version.');
    MsgBox('Failed to retrieve the latest release version.', mbError, MB_OK);
    Exit;
  end;

  ParallaxURL := 'https://github.com/Gameslinx/Tessellation/releases/download/' + LatestVersion + '/Parallax-' + LatestVersion + '.zip';
  ParallaxScatterTexturesURL := 'https://github.com/Gameslinx/Tessellation/releases/download/' + LatestVersion + '/Parallax_ScatterTextures-' + LatestVersion + '.zip';

  Log('Downloading ' + ParallaxURL + ' to ' + ExpandConstant('{tmp}\Parallax-' + LatestVersion + '.zip'));
  ITD_AddFile(ParallaxURL, ExpandConstant('{tmp}\Parallax-' + LatestVersion + '.zip'));

  Log('Downloading ' + ParallaxScatterTexturesURL + ' to ' + ExpandConstant('{tmp}\Parallax_ScatterTextures-' + LatestVersion + '.zip'));
  ITD_AddFile(ParallaxScatterTexturesURL, ExpandConstant('{tmp}\Parallax_ScatterTextures-' + LatestVersion + '.zip'));

  ITD_DownloadAfter(wpReady);
end;

procedure StartInstallation;
// This procedure handles the initialization and downloading steps
begin
  Log('Starting RSS Reborn installation process.');

  // Clear the download directory
  InitializeDownloadsDir;

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
  RemoveObsoleteFolders;

  // Initialize and start downloading
  Log('Starting download process.');
  InitializeDownloadList;
  DownloadAndExtractFiles;
  Log('Finished downloading.');

  // Merge the GameData folders
  MergeGameDataFolders;
  Log('GameData folders merged successfully.');
  MsgBox('GameData folders merged successfully!', mbInformation, MB_OK);

  Log('RSS Reborn installation process completed successfully.');
end;

function NextButtonClick(CurPageID: Integer): Boolean;
begin
  Result := True; // Allow navigation by default

  // Validate RP-1 checkbox before proceeding
  if (CurPageID = wpWelcome) and (not CheckRP1Confirmation) then
  begin
    Result := False; // Prevent navigation
  end
  else if (CurPageID = wpWelcome) then
  begin
    // Initialize GitHub API and start the installation process after the welcome page
    InitializeGitHubAPI;
    StartInstallation;
  end;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
  begin
    // Show final confirmation page
    if MsgBox('RSS Reborn has been installed successfully! Would you like to close the installer now?', mbConfirmation, MB_YESNO) = IDYES then
    begin
      DeinitializeSetup;
      WizardForm.Close;
    end;

    MsgBox('Thank you for installing RSS Reborn!', mbInformation, MB_OK);
  end;
end;
