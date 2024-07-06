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
Source: "license.txt"; DestDir: "{app}"; Flags: dontcopy
Source: "lgpl-3.0.txt"; DestDir: "{app}"; Flags: dontcopy
Source: {#emit ReadReg(HKEY_LOCAL_MACHINE,'Software\Sherlock Software\InnoTools\Downloader','InstallPath','')}\itdownload.dll; Flags: dontcopy; DestDir: {tmp}

[Code]
const
  GitHubAPI = 'https://api.github.com/repos/';
  KSP_DIR = 'C:\Program Files (x86)\Steam\steamapps\common\Kerbal Space Program\GameData';
  MAX_PATH = 260;
  RSSConfigsRepo = 'RSS-Reborn/RSS-Configs';
  RSSTexturesRepo = 'RSS-Reborn/RSS-Terrain';
  S_OK = 0;
  URLMON_DLL = 'urlmon.dll';
	WM_SETTEXT = $000C;
  
var
  AssetDataList: array of TStringList;
  BodyRepos: array[0..11] of string;
  BodySizes: array of string;	
	BodyVersions: array of string;
  DownloadList: TStringList;
  DownloadsDir: string;
	EVEAndScattererCheckbox: TNewCheckBox;
  EVEdownloaded: Boolean;
  I: Integer;
	LatestReleaseAssetsJSON: string;
	LatestReleaseJSON: string;
  LatestReleaseVersion: string;
	MyAccessToken: string;
  Resolution: string;
  ResolutionCombos: array of TComboBox;
	ResolutionPages: array of TWizardPage;
  RP1Checkbox: TNewCheckBox;
	Sizes: array of Int64;
  SizesList: array of TStringList;
	SizeLabelList: array of TLabel;
  ScattererDownloaded: Boolean;
	
type
  TResolutionPages = array of TWizardPage;
  TResolutionCombos = array of TComboBox;

procedure InitializeVariables;
// Initializes global variables to their default states.
begin
  EVEDownloaded := False;
  ScattererDownloaded := False;
	DownloadList := TStringList.Create;
end;

procedure InitializeArrayLengths;
// Sets the lengths of arrays to prepare for storing data.
begin
  // Set the lengths of the arrays
  SetLength(Sizes, 12);
  SetLength(SizeLabelList, 12);
end;

function AddFileSize(CurrentSize, NewSize: Int64): Int64;
// Adds two file sizes together to accumulate the total size of files.
begin
  Result := CurrentSize + NewSize;
end;
	
procedure DeinitializeVariables;
// Frees allocated resources. Prevents memory leaks by releasing resources.
begin
  DownloadList.Free;
end;	

procedure LogSizeError(Body: String; Size: Int64);
// Logs an error message for file size overflow.
begin
  Log(Format('Overflow detected for %s with size: %d', [Body, Size]));
end;
	
procedure InitializeGitHubAPI;
// Retrieves GitHub access token from environment variable. 
// Allows authenticated requests to GitHub API.
begin
  MyAccessToken := GetEnv('MY_ACCESS_TOKEN'); // Get token from environment variable
  if MyAccessToken = '' then
  begin
    Log('GitHub Access Token is not set.');
    Exit;
  end;
  Log('GitHub Access Token retrieved: ' + MyAccessToken); // Log token retrieval for debugging purposes
end;

function SendMessage(hWnd: LongInt; Msg: LongInt; wParam: LongInt; lParam: LongInt): LongInt;
// Helper function
  external 'SendMessageA@user32.dll stdcall';

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
// Helper function
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
// Helper function
var
  I: Integer;
begin
  Result := ExtractFileName(FileName);
  I := LastDelimiter('.', Result);
  if I > 0 then
    SetLength(Result, I - 1);
end;

function FindNextQuote(const JSON: string; StartIndex: Integer): Integer;
// Helper function
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
// Helper function
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
// Helper function
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
// Helper function
var
  TempStr: string;
begin
  TempStr := Copy(S, Offset, Length(S) - Offset + 1);
  Result := Pos(SubStr, TempStr);
  if Result <> 0 then
    Result := Result + Offset - 1;
end;

procedure InitializeBodyRepos;
// Initializes the array with GitHub repositories for planetary bodies.
// Provides the list of repositories to fetch assets from.
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
// Extracts archives using 7-Zip.
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
    Result := False;
    Exit;
  end;
  
  // Run 7-Zip to extract the archive
  if not Exec(ZipPath, 'x "' + ArchivePath + '" -o"' + DestDir + '" -y', '', SW_HIDE, ewWaitUntilTerminated, ResultCode) then
  begin
    Log(Format('Failed to extract %s, error code: %d', [ArchivePath, ResultCode]));
    Result := False;
    Exit;
  end;
  
  // Check if extraction was successful
  if ResultCode <> 0 then
  begin
    Log(Format('7-Zip returned error code %d while extracting %s', [ResultCode, ArchivePath]));
    Result := False;
    Exit;
  end;

  Log('Extraction successful');
  Result := True;
end;

function FormatSize(SizeInBytes: Integer): string;
// Converts file sizes from bytes to MB.
begin
  Result := IntToStr(Round(SizeInBytes / 1048576)) + ' MB'; // Convert bytes to MB and format
end;

function GetFileSizeForLatestReleaseFromAssets(AssetsJSON: string): string;
// Calculates the total size of assets from the JSON response.
var
  I, J, Size, TotalSize: Int64;
begin
  Result := 'Unknown';
  TotalSize := 0;

  I := Pos('"size":', AssetsJSON);
  while I > 0 do
  begin
    I := I + Length('"size":');
    J := PosEx(',', AssetsJSON, I);
    Size := StrToInt64Def(Copy(AssetsJSON, I, J - I), -1);
    if Size <> -1 then
      TotalSize := TotalSize + Size;
    I := PosEx('"size":', AssetsJSON, J);
  end;

  if TotalSize > 0 then
    Result := FormatSize(TotalSize); // Ensure this formats to MB
end;

procedure GetLatestReleaseHTTPInfo(Repo: string);
// Fetches the latest release information from GitHub.
var
  HttpCli: Variant;
  I, J: Integer;
  AccessToken: string;
  AssetsURL: string;
begin
  Log('Fetching latest release info for ' + Repo);
  LatestReleaseJSON := '';
  LatestReleaseAssetsJSON := '';
  LatestReleaseVersion := '';

  try
    AccessToken := GetEnv('MY_ACCESS_TOKEN');
    HttpCli := CreateOleObject('WinHttp.WinHttpRequest.5.1');
    HttpCli.Open('GET', GitHubAPI + Repo + '/releases/latest', False);
    HttpCli.SetRequestHeader('User-Agent', 'RSS-Reborn-Installer');
    if AccessToken <> '' then
      HttpCli.SetRequestHeader('Authorization', 'token ' + AccessToken);

    HttpCli.Send;

    if HttpCli.Status = 200 then
    begin
      LatestReleaseJSON := HttpCli.ResponseText;
      if LatestReleaseJSON = '' then
      begin
        Log('Empty response for latest release info');
        Exit;
      end;

      I := Pos('"tag_name":"', LatestReleaseJSON);
      if I > 0 then
      begin
        I := I + Length('"tag_name":"');
        J := FindNextQuote(LatestReleaseJSON, I);
        if J > 0 then
          LatestReleaseVersion := Copy(LatestReleaseJSON, I, J - I);
      end;

      I := Pos('"assets_url":"', LatestReleaseJSON);
      if I > 0 then
      begin
        I := I + Length('"assets_url":"');
        J := FindNextQuote(LatestReleaseJSON, I);
        if J > 0 then
        begin
          AssetsURL := Copy(LatestReleaseJSON, I, J - I);
          HttpCli.Open('GET', AssetsURL, False);
          if AccessToken <> '' then
            HttpCli.SetRequestHeader('Authorization', 'token ' + AccessToken);

          HttpCli.Send;
          if HttpCli.Status = 200 then
          begin
            LatestReleaseAssetsJSON := HttpCli.ResponseText;
            if LatestReleaseAssetsJSON = '' then
              Log('No assets found for the latest release');
          end
          else
            Log('Failed to fetch assets info, status: ' + IntToStr(HttpCli.Status));
        end;
      end;
    end
    else
    begin
      Log('Failed to fetch latest release info, status: ' + IntToStr(HttpCli.Status));
    end;
  except
  end;
end;

function GetLatestReleaseAssets(Repo, Resolution: string; var Version: string; var Size: string): string;
// Retrieves download URLs for assets matching a specific resolution.
var
  AssetName, BrowserDownloadURL: string;
  I, J, StartPos: Integer;
  AssetURLs: TStringList;
begin
  Result := '';
  Version := LatestReleaseVersion; // Use the version stored by GetLatestReleaseHTTPInfo
  Size := GetFileSizeForLatestReleaseFromAssets(LatestReleaseAssetsJSON); // Use the size calculation function
  AssetURLs := TStringList.Create;

  StartPos := 1;
  while StartPos > 0 do
  begin
    I := PosEx('"name":"', LatestReleaseAssetsJSON, StartPos);
    if I > 0 then
    begin
      I := I + Length('"name":"');
      J := FindNextQuote(LatestReleaseAssetsJSON, I);
      if J > 0 then
      begin
        AssetName := Copy(LatestReleaseAssetsJSON, I, J - I);
        if (Resolution = '') or (Pos(Resolution, AssetName) > 0) then
        begin
          I := PosEx('"browser_download_url":"', LatestReleaseAssetsJSON, J);
          if I > 0 then
          begin
            I := I + Length('"browser_download_url":"');
            J := FindNextQuote(LatestReleaseAssetsJSON, I);
            if J > 0 then
            begin
              BrowserDownloadURL := Copy(LatestReleaseAssetsJSON, I, J - I);
              AssetURLs.Add(BrowserDownloadURL);
            end;
          end;
        end;
        StartPos := J + 1;
      end;
    end
    else
      Break;
  end;

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
// Checks if a GitHub release has associated files.
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
    Log('Failed to check if latest release has files. Exception: ' + GetExceptionMessage);
  end;
end;

function ExtractResolution(AssetName: String): String;
// Extracts resolution information from asset names.
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

function GetLatestReleaseVersion(Repo: string): string;
// Fetches the latest release version from GitHub.
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
    end;
  except
    Log('Exception during HTTP request for latest release version of ' + Repo + ': ' + GetExceptionMessage);
  end;
end;

procedure RetrieveBodyInfo;
// Retrieves version and size information for each body repository.
var
  i: Integer;
  Size: string;
begin
  Log('Retrieving body info');
  SetLength(BodyVersions, Length(BodyRepos));
  SetLength(BodySizes, Length(BodyRepos));

  for i := 0 to High(BodyRepos) do
  begin
    GetLatestReleaseHTTPInfo(BodyRepos[i]);
    BodyVersions[i] := LatestReleaseVersion;
    BodySizes[i] := GetFileSizeForLatestReleaseFromAssets(LatestReleaseAssetsJSON);
  end;
end;

function GetAssetSizeFromRepo(Repo: string; AssetName: string): Integer;
// Fetches the size of a specific asset from a GitHub repository.
var
  HttpCli: Variant;
  JSON: string;
  I, J: Integer;
begin
  Result := -1;
  try
    HttpCli := CreateOleObject('WinHttp.WinHttpRequest.5.1');
    HttpCli.Open('GET', GitHubAPI + Repo + '/releases/latest', False);
    HttpCli.SetRequestHeader('User-Agent', 'InnoSetup');
    if MyAccessToken <> '' then
    begin
      HttpCli.SetRequestHeader('Authorization', 'token ' + MyAccessToken);
      Log('Authorization header set with token.');
    end
    else
    begin
      Log('GitHub Access Token is empty.');
    end;
    HttpCli.Send;

    if HttpCli.Status = 200 then
    begin
      JSON := HttpCli.ResponseText;
      I := Pos('"name":"' + AssetName + '"', JSON);
      if I > 0 then
      begin
        I := PosEx('"size":', JSON, I);
        if I > 0 then
        begin
          I := I + Length('"size":');
          J := PosEx(',', JSON, I);
          Result := StrToIntDef(Copy(JSON, I, J - I), -1);
        end;
      end;
    end
    else
    begin
      Log('Failed to get asset size from ' + Repo + '. Status: ' + IntToStr(HttpCli.Status));
    end;
  except
    Log('Exception during HTTP request for asset size of ' + AssetName + ' in ' + Repo + ': ' + GetExceptionMessage);
  end;
end;

procedure UpdateSizeLabel(ComboBoxTag: Integer);
// Updates the label showing the total size of selected resolutions.
begin
  if ComboBoxTag < Length(SizeLabelList) then
  begin
    if Assigned(SizeLabelList[ComboBoxTag]) then
    begin
      SizeLabelList[ComboBoxTag].Caption := Format('Total Size: %d MB', [Sizes[ComboBoxTag] div (1024 * 1024)]);
    end
    else
    begin
      Log(Format('SizeLabelList[%d] is not assigned (nil).', [ComboBoxTag]));
    end;
  end;
end;

procedure PopulateResolutions(ComboBox: TComboBox; Repo: string; var Sizes: TStringList);
// Populates the resolution dropdowns for each body.
var
  i: Integer;
  Body: String;
  AssetSize: Int64;
  AssetName, Resolution: string;
  J, StartPos, Size: Int64;
  AddedResolutions, ResolutionSizes: TStringList;
begin
  InitializeArrayLengths;

  AddedResolutions := TStringList.Create;
  ResolutionSizes := TStringList.Create;
  try
    Log('Populating resolutions for ' + Repo);
    ComboBox.Items.Clear;
    Sizes.Clear;

    GetLatestReleaseHTTPInfo(Repo);

    StartPos := 1;
    while StartPos > 0 do
    begin
      I := PosEx('"name":"', LatestReleaseAssetsJSON, StartPos);
      if I > 0 then
      begin
        I := I + Length('"name":"');
        J := FindNextQuote(LatestReleaseAssetsJSON, I);
        if J > 0 then
        begin
          AssetName := Copy(LatestReleaseAssetsJSON, I, J - I);
          Resolution := ExtractResolution(AssetName);
          Log('Found asset: ' + AssetName + ' with resolution: ' + Resolution);

          Size := 0;
          I := PosEx('"size":', LatestReleaseAssetsJSON, J);
          if I > 0 then
          begin
            I := I + Length('"size":');
            StartPos := PosEx(',', LatestReleaseAssetsJSON, I);
            Size := StrToInt64Def(Copy(LatestReleaseAssetsJSON, I, StartPos - I), 0);
            Log('Size for asset ' + AssetName + ': ' + IntToStr(Size));

            // Accumulate sizes for each resolution, including multi-part files
            if Resolution <> '' then
            begin
              if AddedResolutions.IndexOf(Resolution) = -1 then
              begin
                ComboBox.Items.Add(Resolution);
                AddedResolutions.Add(Resolution);
                ResolutionSizes.Add(IntToStr(Size));
                Log('Added resolution: ' + Resolution + ' with size: ' + IntToStr(Size));
              end
              else
              begin
                ResolutionSizes[AddedResolutions.IndexOf(Resolution)] :=
                  IntToStr(StrToInt64(ResolutionSizes[AddedResolutions.IndexOf(Resolution)]) + Size);
                Log('Updated resolution: ' + Resolution + ' with new size: ' + ResolutionSizes[AddedResolutions.IndexOf(Resolution)]);
              end;
            end;

            // Check for multi-part files and accumulate their sizes
            while PosEx('.00', AssetName, 1) > 0 do
            begin
              I := PosEx('"size":', LatestReleaseAssetsJSON, StartPos + 1);
              if I > 0 then
              begin
                I := I + Length('"size":');
                StartPos := PosEx(',', LatestReleaseAssetsJSON, I);
                Size := StrToInt64Def(Copy(LatestReleaseAssetsJSON, I, StartPos - I), 0);
                Log('Found multi-part file with size: ' + IntToStr(Size));
                if AddedResolutions.IndexOf(Resolution) <> -1 then
                begin
                  ResolutionSizes[AddedResolutions.IndexOf(Resolution)] :=
                    IntToStr(StrToInt64(ResolutionSizes[AddedResolutions.IndexOf(Resolution)]) + Size);
                  Log('Updated resolution: ' + Resolution + ' with new size: ' + ResolutionSizes[AddedResolutions.IndexOf(Resolution)]);
                end;
              end
              else
                Break;
            end;
          end;
          StartPos := J + 1;
        end;
      end
      else
        Break;
    end;

    // Store accumulated sizes
    for I := 0 to AddedResolutions.Count - 1 do
    begin
      Sizes.Add(ResolutionSizes[I]);
      Log('Final size for resolution ' + AddedResolutions[I] + ': ' + ResolutionSizes[I]);
    end;

    if ComboBox.Items.Count > 0 then
    begin
      ComboBox.ItemIndex := 0;
      try
        // Ensure that the initial size label is updated correctly
        Log('Attempting to update initial size label for ' + Repo + ' with ComboBox.Tag: ' + IntToStr(ComboBox.Tag));
        Log('Length of SizeLabelList: ' + IntToStr(Length(SizeLabelList)));
        Log('Length of Sizes: ' + IntToStr(Sizes.Count));

        if (ComboBox.Tag >= 0) and (ComboBox.Tag < Length(SizeLabelList)) then
        begin
          if Assigned(SizeLabelList[ComboBox.Tag]) then
          begin
            if (ComboBox.ItemIndex >= 0) and (ComboBox.ItemIndex < Sizes.Count) then
            begin
              SizeLabelList[ComboBox.Tag].Caption := 'Total Size: ' + IntToStr(StrToInt64Def(Sizes[ComboBox.ItemIndex], 0) div 1048576) + ' MB';
              Log('Initial size for default selection: ' + SizeLabelList[ComboBox.Tag].Caption);
            end
            else
            begin
              Log('Invalid ComboBox.ItemIndex: ' + IntToStr(ComboBox.ItemIndex));
            end;
          end
          else
          begin
            Log('SizeLabelList[' + IntToStr(ComboBox.Tag) + '] is not assigned (nil).');
          end;
        end
        else
        begin
          Log('Invalid ComboBox.Tag value: ' + IntToStr(ComboBox.Tag));
        end;
      except
        Log('Exception updating initial size label for ' + Repo + ': ' + GetExceptionMessage);
      end;
    end;
  except
    Log('Exception occurred while populating resolutions for ' + Repo + ': ' + GetExceptionMessage);
  end;
  AddedResolutions.Free;
  ResolutionSizes.Free;
end;

procedure ComboBoxChange(Sender: TObject);
// Handles changes in resolution selection.
var
  ComboBox: TComboBox;
  Index: Integer;
  SizeStr: string;
  SizeInBytes: Int64;
begin
  ComboBox := TComboBox(Sender);
  Index := ComboBox.Tag;
  
  Log('ComboBoxChange called for ComboBox with Tag: ' + IntToStr(Index));
  Log('Length of SizeLabelList: ' + IntToStr(Length(SizeLabelList)));
  Log('Length of SizesList: ' + IntToStr(Length(SizesList)));
  if ComboBox.ItemIndex >= 0 then
  begin
    try
      if (Index >= 0) and (Index < Length(SizesList)) then
      begin
        if (ComboBox.ItemIndex >= 0) and (ComboBox.ItemIndex < SizesList[Index].Count) then
        begin
          SizeInBytes := StrToInt64Def(SizesList[Index][ComboBox.ItemIndex], 0);
          SizeStr := 'Total Size: ' + IntToStr(SizeInBytes div 1048576) + ' MB';
          if (Index >= 0) and (Index < Length(SizeLabelList)) then
          begin
            SizeLabelList[Index].Caption := SizeStr;
            Log('SizeLabel updated to: ' + SizeStr);
          end
          else
          begin
            Log('Invalid SizeLabelList index: ' + IntToStr(Index));
          end;
        end
        else
        begin
          Log('Invalid ComboBox.ItemIndex: ' + IntToStr(ComboBox.ItemIndex));
        end;
      end
      else
      begin
        Log('Invalid SizesList index: ' + IntToStr(Index));
      end;
    except
      Log('Exception updating size label: ' + GetExceptionMessage);
    end;
  end;
end;

function CheckRP1Confirmation: Boolean;
// Ensures the user has installed RP-1 before proceeding.
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

procedure SetITDOptions;
// Sets options for the InnoTools Downloader.
begin
  ITD_SetOption('ShowDetailsButton', 'false'); // Hide the Details button
  ITD_SetOption('CustomUI', 'true'); // Enable custom UI settings if supported
  // Additional customization options can be set here if needed
end;

procedure InitializeWizard;
// Initializes the installation wizard, including UI elements and variables.
// Sets up the installer’s user interface and prepares variables.
var
  i: Integer;
  ComboBox: TComboBox;
  BodyLabel, VersionLabel, SizeLabel: TLabel;
  Page: TWizardPage;
  PageHeight: Integer;
begin
  Log('Initializing wizard');

  InitializeBodyRepos;
  InitializeVariables;

  RetrieveBodyInfo;

  DownloadsDir := ExpandConstant('{userdocs}\Downloads');
  Log('Downloads directory initialized: ' + DownloadsDir);

  SetITDOptions;

  RP1Checkbox := TNewCheckBox.Create(WizardForm);
  RP1Checkbox.Parent := WizardForm.WelcomePage;
  RP1Checkbox.Left := ScaleX(18);
  RP1Checkbox.Top := ScaleY(150);
  RP1Checkbox.Width := WizardForm.ClientWidth - ScaleX(36);
  RP1Checkbox.Height := ScaleY(40);
  RP1Checkbox.Caption := 'I confirm that I have successfully run RP-1 once.';
  RP1Checkbox.Checked := False;
  Log('RP-1 installation confirmation checkbox created');

  EVEAndScattererCheckbox := TNewCheckBox.Create(WizardForm);
  EVEAndScattererCheckbox.Parent := WizardForm.WelcomePage;
  EVEAndScattererCheckbox.Left := ScaleX(18);
  EVEAndScattererCheckbox.Top := ScaleY(200);
  EVEAndScattererCheckbox.Width := WizardForm.ClientWidth - ScaleX(36);
  EVEAndScattererCheckbox.Height := ScaleY(40);
  EVEAndScattererCheckbox.Caption := '(Optional) I am using Blackrack''s EVE and Scatterer.';
  EVEAndScattererCheckbox.Checked := False;
  Log('EVE and Scatterer download confirmation checkbox created');

  Page := CreateCustomPage(wpWelcome, 'Select Resolutions', 'Select the desired resolution for each body');

  WizardForm.ClientHeight := WizardForm.ClientHeight + ScaleY(400);
  WizardForm.ClientWidth := WizardForm.ClientWidth + ScaleX(300);

  SetLength(ResolutionCombos, Length(BodyRepos));
  SetLength(AssetDataList, Length(BodyRepos));
  SetLength(SizesList, Length(BodyRepos));
  SetLength(SizeLabelList, Length(BodyRepos));

  PageHeight := 10;

  for i := 0 to High(BodyRepos) do
  begin
    BodyLabel := TLabel.Create(Page);
    BodyLabel.Parent := Page.Surface;
    BodyLabel.Left := ScaleX(8);
    BodyLabel.Top := ScaleY(PageHeight);
    BodyLabel.Caption := Copy(BodyRepos[i], 11, Length(BodyRepos[i]) - 10);

    ComboBox := TComboBox.Create(Page);
    ComboBox.Parent := Page.Surface;
    ComboBox.Left := ScaleX(150);
    ComboBox.Top := ScaleY(PageHeight);
    ComboBox.Width := ScaleX(50);
    ComboBox.OnChange := @ComboBoxChange;
    ComboBox.Tag := i;
    ResolutionCombos[i] := ComboBox;

    AssetDataList[i] := TStringList.Create;
    SizesList[i] := TStringList.Create;
    PopulateResolutions(ComboBox, BodyRepos[i], SizesList[i]);

    Log('Dropdown for ' + BodyRepos[i] + ' created');

    VersionLabel := TLabel.Create(Page);
    VersionLabel.Parent := Page.Surface;
    VersionLabel.Left := ScaleX(320);
    VersionLabel.Top := ScaleY(PageHeight);
    if i < Length(BodyVersions) then
      VersionLabel.Caption := 'Version: ' + BodyVersions[i];

    SizeLabel := TLabel.Create(Page);
    SizeLabel.Parent := Page.Surface;
    SizeLabel.Left := ScaleX(420);
    SizeLabel.Top := ScaleY(PageHeight);
    if SizesList[i].Count > 0 then
      SizeLabel.Caption := 'Total Size: ' + IntToStr(StrToIntDef(SizesList[i][0], 0) div 1048576) + ' MB'
    else
      SizeLabel.Caption := 'Total Size: Unknown';
    SizeLabelList[i] := SizeLabel;

    Log('Size label for ' + BodyRepos[i] + ' initialized: ' + SizeLabel.Caption);

    PageHeight := PageHeight + 25;
  end;

  try
    ITD_Internal_InitUI(WizardForm.Handle);
    Log('ITD_Internal_InitUI successful');
  except
    Log('ITD_Internal_InitUI failed: ' + GetExceptionMessage);
    Exit;
  end;

  WizardForm.Repaint;
  Page.Surface.Repaint;

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
    Log('Failed to move downloaded EVE and Scatterer files. Exception: ' + GetExceptionMessage);
  end;
end;

procedure AddToDownloadList(RepoName, Resolution, TempFileName: string);
// Adds URLs to the download list based on selected resolutions.
var
  URL: string;
  AssetName, BrowserDownloadURL: string;
  I, J, StartPos: Integer;
begin
  GetLatestReleaseHTTPInfo(RepoName);

  StartPos := 1;
  while StartPos > 0 do
  begin
    I := PosEx('"name":"', LatestReleaseAssetsJSON, StartPos);
    if I > 0 then
    begin
      I := I + Length('"name":"');
      J := FindNextQuote(LatestReleaseAssetsJSON, I);
      if J > 0 then
      begin
        AssetName := Copy(LatestReleaseAssetsJSON, I, J - I);
        if (Resolution = '') or (Pos(Resolution, AssetName) > 0) then
        begin
          I := PosEx('"browser_download_url":"', LatestReleaseAssetsJSON, J);
          if I > 0 then
          begin
            I := I + Length('"browser_download_url":"');
            J := FindNextQuote(LatestReleaseAssetsJSON, I);
            if J > 0 then
            begin
              BrowserDownloadURL := Copy(LatestReleaseAssetsJSON, I, J - I);
              DownloadList.Add(BrowserDownloadURL + '=' + TempFileName);
            end;
          end;
        end;
        StartPos := J + 1;
      end;
    end
    else
      Break;
  end;
end;

function GetRepoDownloadURL(Repo, Resolution: string): string;
// Retrieves the download URL for a specific repository and resolution.
var
  AssetName, BrowserDownloadURL: string;
  I, J, StartPos: Integer;
begin
  GetLatestReleaseHTTPInfo(Repo);
  Result := '';

  StartPos := 1;
  while StartPos > 0 do
  begin
    I := PosEx('"name":"', LatestReleaseAssetsJSON, StartPos);
    if I > 0 then
    begin
      I := I + Length('"name":"');
      J := FindNextQuote(LatestReleaseAssetsJSON, I);
      if J > 0 then
      begin
        AssetName := Copy(LatestReleaseAssetsJSON, I, J - I);
        if (Resolution = '') or (Pos(Resolution, AssetName) > 0) then
        begin
          I := PosEx('"browser_download_url":"', LatestReleaseAssetsJSON, J);
          if I > 0 then
          begin
            I := I + Length('"browser_download_url":"');
            J := FindNextQuote(LatestReleaseAssetsJSON, I);
            if J > 0 then
            begin
              BrowserDownloadURL := Copy(LatestReleaseAssetsJSON, I, J - I);
              Result := BrowserDownloadURL;
              Exit;
            end;
          end;
        end;
        StartPos := J + 1;
      end;
    end
    else
      Break;
  end;
end;

procedure InitializeDownloadList;
// Initializes the list of files to be downloaded.
var
  LatestVersion: string;
  ParallaxURL, ParallaxScatterTexturesURL: string;
  LatestReleaseURL: string;
begin
  Log('Initializing download list');
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
// Downloads and extracts the files listed in the download list.
// Ensures all required files are available and extracted.
var
  I: Integer;
  ResultStr: String;
begin
  Log('Starting download process for all assets');

  // Initiate download for all files added to the queue
  ITD_DownloadFiles;

  // Get the download result
  ResultStr := ITD_GetString(ITDS_TitleCaption);  // Or any other appropriate method to get the result

  // Check download result
  if ResultStr = '0' then  // Assuming '0' indicates success
  begin
    Log('All assets downloaded successfully. Extracting files...');
    for I := 0 to DownloadList.Count - 1 do
    begin
      // Extract the downloaded file (assuming 7zip extraction)
      if Extract7Zip(ExpandConstant('{tmp}\') + ExtractFileName(DownloadList[I]), ExpandConstant('{tmp}')) then
      begin
        Log('Extraction completed for ' + DownloadList[I]);
      end
      else
      begin
        Log('Failed to extract ' + DownloadList[I]);
      end;
    end;
    Log('All files extracted successfully.');
  end
  else
  begin
    Log('Failed to download assets. Error: ' + ResultStr);
  end;
end;

procedure InitializeAndDownload;
// Runs initialization and download procedures in sequence.
begin
  InitializeDownloadList;
  DownloadAndExtractFiles;

  // Clean up download list
  DownloadList.Free;
end;

procedure VerifyDownloadAndExtraction;
// Verifies that all files have been downloaded and extracted successfully.
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
      Exit; // Exit procedure early if any entry is invalid
    end;

    // Expand any constants in TempFile path
    TempFile := ExpandConstant(TempFile);

    // Check if the file exists
    if not FileExists(TempFile) then
    begin
      Log('Error: File not found after extraction: ' + TempFile);
      Exit; // Exit procedure early if any file is missing
    end;
  end;

  Log('All files verified to be downloaded and extracted successfully.');
end;

procedure MoveGameData(SourceDir, DestDir: string);
// Moves game data files from the temporary directory to the game directory.
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
	
	Log(Format('Moving game data from %s to %s', [SourceDir, DestDir]));

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
// Merges multiple game data folders into one.
var
  I: Integer;
  SourceDir, DestDir: string;
begin

  Log('Merging GameData folders');
	
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
// Deletes temporary files created during the installation process.
// Keeps the system clean and frees up disk space.
var
  TempDir: string;
begin
  TempDir := ExpandConstant('{tmp}');
  Log('Cleaning up temporary files in ' + TempDir);  // Add more descriptive logging
  if DirExists(TempDir) then
  begin
    if not DelTree(TempDir, True, True, True) then
      Log('Failed to clean up temporary files in ' + TempDir)
    else
      Log('Temporary files cleaned up successfully.');
  end
  else
    Log('No temporary files found to clean up in ' + TempDir);
end;

procedure DeinitializeSetup;
// Cleans up resources and temporary files after installation.
begin
  Log('Deinitializing setup and cleaning up resources');  // Ensure complete logging
  CleanupTemporaryFiles;
  DeinitializeVariables;
end;

procedure RemoveObsoleteFolders;
// Deletes old or obsolete directories from the game installation.
// Prevents conflicts and ensures only relevant files remain.
begin
  Log('Removing obsolete folders');
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
// Downloads specific textures for the Parallax mod.
var
  LatestVersion: string;
  ParallaxURL, ParallaxScatterTexturesURL: string;
begin
  Log('Downloading Parallax textures(procedure)');
  LatestVersion := GetLatestReleaseVersion('Gameslinx/Tessellation');
  if LatestVersion = '' then
  begin
    Log('Failed to retrieve the latest release version.');
    //MsgBox('Failed to retrieve the latest release version.', mbError, MB_OK);
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

procedure InitializeDownloadsDir;
// Sets the directory for downloading files.
begin
  DownloadsDir := ExpandConstant('{userdocs}\Desktop');
  Log('Downloads directory initialized: ' + DownloadsDir);
end;

procedure StartInstallation;
// Manages the entire installation process.
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
end;

function NextButtonClick(CurPageID: Integer): Boolean;
// Handles the actions to perform when the Next button is clicked.
// Guides the installation flow based on user input.
var
  i: Integer;
begin
  Log('Next button clicked, CurPageID: ' + IntToStr(CurPageID));
  Result := True; // Allow navigation by default

  // Validate RP-1 checkbox before proceeding from the welcome page
  if (CurPageID = wpWelcome) then
  begin
    if not CheckRP1Confirmation then
    begin
      Result := False; // Prevent navigation if RP-1 confirmation is not checked
      Exit;
    end
    else
    begin
      // Initialize GitHub API after all validations
      InitializeGitHubAPI;
    end;
  end;

  // If user is on the resolutions page, start the installation process
  if (CurPageID = wpReady) then
  begin
    StartInstallation;
  end;
end;
	
procedure CurPageChanged(CurPageID: Integer);
// Logs and manages actions when the current page of the wizard changes.
// Prepares the installer for subsequent steps.
begin
  Log('Current page changed, CurPageID: ' + IntToStr(CurPageID));
  if CurPageID = wpReady then
  begin
    Log('Preparing to start the installation process.');
    // Additional logic to prepare the installation, if any, can be added here
  end;
end;

procedure CurStepChanged(CurStep: TSetupStep);
// Manages transitions between installation steps.
begin
  Log('Current step changed, CurStep: ' + IntToStr(Ord(CurStep)));
  if CurStep = ssInstall then
  begin
    Log('Starting the installation process.');
    StartInstallation;
  end
end;