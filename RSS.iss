; Inno Setup Script
; David Robie (DRobie22)
; This installer follows RSS Reborn's GitHub Instructions

#define MyAppName "RSS Reborn Installer"
#define MyAppVersion "0.5"
#define MyAppPublisher "DRobie22"
#define MyAppURL "https://github.com/RSS-Reborn/RSS-Reborn"
#define MyAppExeName "RSS-Reborn-Installer.exe"

[Setup]
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={autopf}\My Program
DefaultGroupName=My Program
OutputBaseFilename=RSSRebornInstaller
SetupLogging=yes
Compression=lzma
SolidCompression=yes
WizardImageFile=images\backgroundearth.bmp
WizardImageStretch=no
WizardSmallImageFile=images\icon.bmp
WizardStyle=modern
DisableWelcomePage=no
PrivilegesRequired=admin

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Messages]
WelcomeLabel2=This will install RSS Reborn into your default KSP directory.%n%nMod created and maintained by Ballisticfox, Techo, and VaNnadin.%n[name/ver] created by DRobie22.

[Files]
Source: "C:\Program Files\7-Zip\7z.exe"; DestDir: "{tmp}"; Flags: dontcopy;
Source: "Licenses\license.txt"; DestDir: "{app}"; Flags: dontcopy;
Source: "Licenses\lgpl-3.0.txt"; DestDir: "{app}"; Flags: dontcopy;

[Code]
const
  GitHubAPI = 'https://api.github.com/repos/';
  MAX_PATH = 260;
	RequiredSpace = 50000000000;
  RSSConfigsRepo = 'RSS-Reborn/RSS-Configs';
  RSSTexturesRepo = 'RSS-Reborn/RSS-Terrain';
  S_OK = 0;
  URLMON_DLL = 'urlmon.dll';
	
var
  AssetDataList: array of TStringList;
  BodyRepos: array[0..11] of string;
  BodySizes: array of string;	
	BodyVersions: array of string;
	CachedReleaseInfo: TStringList;
	CurrentFileLabel: TNewStaticText;
  DownloadList: TStringList;
  DownloadsDir: string;
	EVEAndScattererCheckbox: TNewCheckBox;
  EVEdownloaded: Boolean;
	KSP_DIR: string;
  KSPDirPage: TInputDirWizardPage;
	DownloadPage: TOutputProgressWizardPage;
	LatestReleaseAssetsJSON: string;
	LatestReleaseJSON: string;
  LatestReleaseVersion: string;
	MyAccessToken: string;
  ResolutionCombos: array of TComboBox;
  RP1Checkbox: TNewCheckBox;
	Sizes: array of Int64;
  SizesList: array of TStringList;
	SizeLabelList: array of TLabel;
  ScattererDownloaded: Boolean;
	UserCanceled: Boolean;
  wpSelectResolutions: Integer;
	
type
  TResolutionPages = array of TWizardPage;
  TResolutionCombos = array of TComboBox;

procedure InitializeVariables;
// Initializes global variables to their default states.
begin
  Log('Initializing variables...');
  EVEDownloaded := False;
  ScattererDownloaded := False;
  DownloadList := TStringList.Create;
	CachedReleaseInfo := TStringList.Create;
  UserCanceled := False;
  Log('Variables initialized.');
end;

procedure InitializeArrayLengths;
// Sets the lengths of arrays to prepare for storing data.
begin
  // Set the lengths of the arrays
  SetLength(Sizes, 12);
  SetLength(SizeLabelList, 12);
  SetLength(SizesList, 12);
end;

function Is7ZipInstalled: Boolean;
begin
  Result := FileExists('C:\Program Files\7-Zip\7z.exe') or FileExists('C:\Program Files (x86)\7-Zip\7z.exe');
  if Result then
    Log('7-Zip is installed.')
  else
    Log('7-Zip is not installed.');
end;

function GetDiskFreeSpaceEx(
// Uses windows api to check available space
  lpDirectoryName: string;
  var lpFreeBytesAvailableToCaller, lpTotalNumberOfBytes, lpTotalNumberOfFreeBytes: Int64): BOOL;
  external 'GetDiskFreeSpaceExW@kernel32.dll stdcall';

function GetFreeSpace(Drive: String): Int64;
// Uses windows api to check available space
var
  FreeAvailable, TotalSpace, TotalFree: Int64;
begin
  Result := -1;
  if GetDiskFreeSpaceEx(Drive, FreeAvailable, TotalSpace, TotalFree) then
  begin
    Result := FreeAvailable;
  end;
end;

function IsEnoughDiskSpaceAvailable: Boolean;
// Uses windows api to check available space (need 50 GB to be safe)
var
  FreeSpace: Int64;
begin
  FreeSpace := GetFreeSpace(ExpandConstant('{sd}'));
  Log('Free disk space: ' + IntToStr(FreeSpace div (1024 * 1024 * 1024)) + ' GB');
  Result := FreeSpace >= RequiredSpace;
end;

function InitializeSetup: Boolean;
begin
  Result := True;
  if not IsEnoughDiskSpaceAvailable then
  begin
    MsgBox('You need at least 50 GB of free disk space to install this application.', mbError, MB_OK);
    Result := False;
    Log('Not enough disk space available.');
  end
  else if not Is7ZipInstalled then
  begin
    MsgBox('7-Zip is not installed. Please install 7-Zip to continue.', mbError, MB_OK);
    Result := False;
    Log('7-Zip is not installed.');
  end
  else
    Log('Setup initialization successful.');
end;

procedure InitializeDownloadsDir;
// Sets the directory for downloading files.
begin
  DownloadsDir := ExpandConstant('{userappdata}\RSSRebornDownloads');
  if not DirExists(DownloadsDir) then
  begin
    CreateDir(DownloadsDir);
    Log('Created download directory: ' + DownloadsDir);
  end;
  Log('Downloads directory initialized: ' + DownloadsDir);
end;

procedure LogDownloadListDetails;
var
  I: Integer;
  Entry, URL, FileName: string;
begin
  Log('Listing all files to be downloaded and their URLs:');
  for I := 0 to DownloadList.Count - 1 do
  begin
    Entry := DownloadList[I];
    URL := Copy(Entry, 1, Pos('=', Entry) - 1);
    FileName := Copy(Entry, Pos('=', Entry) + 1, Length(Entry));
    Log('File: ' + FileName + ' URL: ' + URL);
  end;
  Log('Files will be downloaded to: ' + DownloadsDir);
end;

function ReadGitHubAccessToken: string;
// Reads the GitHub access token from the environment variable if it exists.
// Allows more downloads per hour
begin
  Result := GetEnv('MY_ACCESS_TOKEN');
  if Result <> '' then
    Log('GitHub access token found in environment variable.')
  else
    Log('GitHub access token not found in environment variable.');
end;
	
procedure DeinitializeVariables;
// Frees allocated resources. Prevents memory leaks by releasing resources.
begin
  DownloadList.Free;
  CachedReleaseInfo.Free; // Free cache
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

function ExtractBodyName(Repo: string): string;
// Helper Function
var
  LastSlashPos: Integer;
  BodyName: string;
begin
  LastSlashPos := LastDelimiter('/', Repo);
  if LastSlashPos > 0 then
    BodyName := Copy(Repo, LastSlashPos + 1, Length(Repo) - LastSlashPos)
  else
    BodyName := Repo;  // Fallback in case there is no slash
  
  // Remove the 'RSS-' prefix if it exists
  if Pos('RSS-', BodyName) = 1 then
    Delete(BodyName, 1, Length('RSS-'));
  
  Result := BodyName;
end;

procedure InitializeBodyRepos;
// Initializes the array with GitHub repositories for planetary bodies.
// Provides the list of repositories to fetch assets from.
begin
  Log('Initializing BodyRepos array');
  BodyRepos[0] := 'RSS-Reborn/RSS-Sol';
  BodyRepos[1] := 'RSS-Reborn/RSS-Mercury';
  BodyRepos[2] := 'RSS-Reborn/RSS-Venus';
  BodyRepos[3] := 'RSS-Reborn/RSS-Earth';
  BodyRepos[4] := 'RSS-Reborn/RSS-Luna';
  BodyRepos[5] := 'RSS-Reborn/RSS-Mars';
  BodyRepos[6] := 'RSS-Reborn/RSS-Jupiter';
  BodyRepos[7] := 'RSS-Reborn/RSS-Saturn';
  BodyRepos[8] := 'RSS-Reborn/RSS-Uranus';
  BodyRepos[9] := 'RSS-Reborn/RSS-Neptune';
  BodyRepos[10] := 'RSS-Reborn/RSS-AsteroidBelt';
  BodyRepos[11] := 'RSS-Reborn/RSS-KuiperBelt';
  Log('BodyRepos array initialized');
end;

function Extract7Zip(ArchivePath, DestDir: string): Boolean;
var
  ZipPath: string;
  ResultCode: Integer;
  CommandLine: string;
begin
  Log(Format('Extracting archive %s to directory %s', [ArchivePath, DestDir]));
  
  if FileExists('C:\Program Files\7-Zip\7z.exe') then
    ZipPath := 'C:\Program Files\7-Zip\7z.exe'
  else if FileExists('C:\Program Files (x86)\7-Zip\7z.exe') then
    ZipPath := 'C:\Program Files (x86)\7-Zip\7z.exe'
  else
  begin
    Log('7-Zip executable not found!');
    MsgBox('7-Zip executable not found! Please ensure 7-Zip is installed.', mbError, MB_OK);
    Result := False;
    Exit;
  end;
  
  if not FileExists(ArchivePath) then
  begin
    Log('Archive file not found: ' + ArchivePath);
    Result := False;
    Exit;
  end;
  
  CommandLine := Format('"%s" x "%s" -o"%s" -y', [ZipPath, ArchivePath, DestDir]);
  Log('Running command: ' + CommandLine);
  
  if not Exec(ZipPath, 'x "' + ArchivePath + '" -o"' + DestDir + '" -y', '', SW_HIDE, ewWaitUntilTerminated, ResultCode) then
  begin
    Log(Format('Failed to execute 7-Zip for %s, error code: %d', [ArchivePath, ResultCode]));
    Result := False;
    Exit;
  end;
  
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

function GetCachedJSONForRepo(Repo: string; var ReleaseJSON: string; var AssetsJSON: string): Boolean;
var
  i: Integer;
  KeyValue: TStringList;
  CachedData: string;
begin
  Result := False;
  KeyValue := TStringList.Create;
  try
    for i := 0 to CachedReleaseInfo.Count - 1 do
    begin
      KeyValue.DelimitedText := CachedReleaseInfo[i];
      if SameText(KeyValue[0], Repo) then
      begin
        CachedData := KeyValue[1];
        ReleaseJSON := Copy(CachedData, 1, Pos('|', CachedData) - 1);
        AssetsJSON := Copy(CachedData, Pos('|', CachedData) + 1, Length(CachedData));
        Result := True;
        Exit;
      end;
    end;
  finally
    KeyValue.Free;
  end;
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
var
  HttpCli: Variant;
  I, J: Integer;
  AssetsURL: string;
  CombinedResponse, CachedData: string;
begin
  if GetCachedJSONForRepo(Repo, LatestReleaseJSON, LatestReleaseAssetsJSON) then
  begin
    Log('Using cached release info for ' + Repo);
    Exit;
  end;

  Log('Fetching latest release info for ' + Repo);
  LatestReleaseJSON := '';
  LatestReleaseAssetsJSON := '';
  LatestReleaseVersion := '';

  try
    HttpCli := CreateOleObject('WinHttp.WinHttpRequest.5.1');
    HttpCli.Open('GET', GitHubAPI + Repo + '/releases/latest', False);
    HttpCli.SetRequestHeader('User-Agent', 'RSS-Reborn-Installer');
    MyAccessToken := ReadGitHubAccessToken;
    if MyAccessToken <> '' then
      HttpCli.SetRequestHeader('Authorization', 'token ' + MyAccessToken);

    HttpCli.Send;

    if HttpCli.Status = 200 then
    begin
		  Log('GitHub Call');
      CombinedResponse := HttpCli.ResponseText;
      if CombinedResponse = '' then
      begin
        Log('Empty response for latest release info');
        Exit;
      end;

      LatestReleaseJSON := CombinedResponse;
      
      // Parse the JSON response to extract necessary fields and assets
      I := Pos('"tag_name":"', CombinedResponse);
      if I > 0 then
      begin
        I := I + Length('"tag_name":"');
        J := FindNextQuote(CombinedResponse, I);
        if J > 0 then
          LatestReleaseVersion := Copy(CombinedResponse, I, J - I);
      end;

      I := Pos('"assets":[', CombinedResponse);
      if I > 0 then
      begin
        J := PosEx(']', CombinedResponse, I) + 1;
        LatestReleaseAssetsJSON := Copy(CombinedResponse, I, J - I);
      end;

      CachedData := Repo + '=' + LatestReleaseJSON + '|' + LatestReleaseAssetsJSON;
      CachedReleaseInfo.Add(CachedData); // Cache the combined JSON responses
    end
    else
    begin
      Log('Failed to fetch latest release info, status: ' + IntToStr(HttpCli.Status));
      if HttpCli.Status = 403 then
      begin
        Log('HTTP 403 Forbidden error. Possible rate limit exceeded.');
        if MsgBox('GitHub download rate limit exceeded. Please wait a moment before retrying. Click OK to retry now, or Cancel to exit.', mbInformation, MB_OKCANCEL) = IDOK then
        begin
          Log('User acknowledged rate limit message. Retrying...');
          GetLatestReleaseHTTPInfo(Repo);
        end
        else
        begin
          Log('User canceled retry. Exiting installer.');
          UserCanceled := True;
          Exit;
        end;
      end;
    end;
  except
    Log('Exception occurred while fetching latest release info: ' + GetExceptionMessage);
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
    HttpCli.SetRequestHeader('User-Agent', 'RSS-Reborn-Installer');

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
var
  I: Integer;
  ReleaseJSON, AssetsJSON: string;
begin
  Log('Retrieving body info');
  SetLength(BodyVersions, Length(BodyRepos));
  SetLength(BodySizes, Length(BodyRepos));
  SetLength(AssetDataList, Length(BodyRepos)); // Ensure AssetDataList is properly initialized

  for I := 0 to High(BodyRepos) do
  begin
    if UserCanceled then
    begin
      Log('Installation canceled by user. Exiting body info retrieval loop.');
      Exit;
    end;

    if GetCachedJSONForRepo(BodyRepos[I], ReleaseJSON, AssetsJSON) then
    begin
      LatestReleaseJSON := ReleaseJSON;
      LatestReleaseAssetsJSON := AssetsJSON;
    end
    else
    begin
      GetLatestReleaseHTTPInfo(BodyRepos[I]);
      ReleaseJSON := LatestReleaseJSON;
      AssetsJSON := LatestReleaseAssetsJSON;
    end;

    BodyVersions[I] := LatestReleaseVersion;
    BodySizes[I] := GetFileSizeForLatestReleaseFromAssets(AssetsJSON);

    if Assigned(AssetDataList[I]) then
      AssetDataList[I].Free; // Free previously assigned TStringList

    AssetDataList[I] := TStringList.Create;
    AssetDataList[I].Text := AssetsJSON;
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

procedure PopulateResolutions(ComboBox: TComboBox; RepoIndex: Integer; var Sizes: TStringList);
var
  I, J, StartPos: Integer;
  AssetName, Resolution: string;
  Size, TotalSize: Int64;
  AddedResolutions: TStringList;
begin
  AddedResolutions := TStringList.Create;
  try
    Log('Populating resolutions for ' + BodyRepos[RepoIndex]);
    ComboBox.Items.Clear;
    Sizes.Clear;

    // Ensure that the RepoIndex is within the bounds of AssetDataList
    if (RepoIndex < 0) or (RepoIndex >= Length(AssetDataList)) then
    begin
      Log('Error: RepoIndex ' + IntToStr(RepoIndex) + ' is out of range.');
      Exit;
    end;

    // Use cached data from RetrieveBodyInfo
    LatestReleaseAssetsJSON := AssetDataList[RepoIndex].Text;

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

            if Resolution <> '' then
            begin
              if AddedResolutions.IndexOf(Resolution) = -1 then
              begin
                ComboBox.Items.Add(Resolution);
                AddedResolutions.Add(Resolution);
                Sizes.Add(IntToStr(Size));
                Log('Added resolution: ' + Resolution + ' with size: ' + IntToStr(Size));
              end
              else
              begin
                TotalSize := StrToInt64(Sizes[AddedResolutions.IndexOf(Resolution)]) + Size;
                Sizes[AddedResolutions.IndexOf(Resolution)] := IntToStr(TotalSize);
                Log('Updated resolution: ' + Resolution + ' with new size: ' + IntToStr(TotalSize));
              end
            end
          end;
          StartPos := J + 1;
        end
      end
      else
        Break;
    end;

    if ComboBox.Items.Count > 0 then
    begin
      ComboBox.ItemIndex := 0;
      UpdateSizeLabel(ComboBox.Tag);
    end;
  finally
    AddedResolutions.Free;
  end;
end;

procedure ComboBoxChange(Sender: TObject);
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

procedure InitializeWizard;
var
  I: Integer;
  ComboBox: TComboBox;
  BodyLabel, VersionLabel, SizeLabel: TLabel;
  Page: TWizardPage;
  PageHeight: Integer;
begin
  Log('Initializing wizard');

  InitializeBodyRepos;
  InitializeVariables;
	InitializeArrayLengths;
  
  // Read GitHub access token from the registry
  MyAccessToken := ReadGitHubAccessToken;

  RetrieveBodyInfo;

  RP1Checkbox := TNewCheckBox.Create(WizardForm);
  RP1Checkbox.Parent := WizardForm.WelcomePage;
  RP1Checkbox.Left := ScaleX(18);
  RP1Checkbox.Top := ScaleY(175);
  RP1Checkbox.Width := WizardForm.ClientWidth - ScaleX(36);
  RP1Checkbox.Height := ScaleY(40);
  RP1Checkbox.Caption := 'I confirm that I have successfully run RP-1 once.';
  RP1Checkbox.Checked := False;
  Log('RP-1 installation confirmation checkbox created');

  EVEAndScattererCheckbox := TNewCheckBox.Create(WizardForm);
  EVEAndScattererCheckbox.Parent := WizardForm.WelcomePage;
  EVEAndScattererCheckbox.Left := ScaleX(18);
  EVEAndScattererCheckbox.Top := ScaleY(215);
  EVEAndScattererCheckbox.Width := WizardForm.ClientWidth - ScaleX(36);
  EVEAndScattererCheckbox.Height := ScaleY(40);
  EVEAndScattererCheckbox.Caption := '(Optional) I am using Blackrack''s EVE and Scatterer.';
  EVEAndScattererCheckbox.Checked := False;
  Log('EVE and Scatterer download confirmation checkbox created');
  
  // Create and configure the KSP directory input page
  KSPDirPage := CreateInputDirPage(wpWelcome, 'KSP Directory', 'Select the KSP directory', 'Please select the directory where Kerbal Space Program is installed.', False, '');
  KSPDirPage.Add('');
  KSPDirPage.Values[0] := 'C:\Program Files (x86)\Steam\steamapps\common\Kerbal Space Program\GameData';  // Set the default directory

  Page := CreateCustomPage(wpWelcome, 'Select Resolutions', 'Select the desired resolution for each body');
  wpSelectResolutions := Page.ID;

  DownloadPage := CreateOutputProgressPage('Downloading Files', 'This may take a while, but I''m sure you''re used to long KSP loading times by now.');
  CurrentFileLabel := TNewStaticText.Create(DownloadPage);
  CurrentFileLabel.Parent := DownloadPage.Surface;
  CurrentFileLabel.Left := ScaleX(8);
  CurrentFileLabel.Top := ScaleY(70);
  CurrentFileLabel.Width := DownloadPage.SurfaceWidth - ScaleX(16);
  CurrentFileLabel.Caption := 'Initializing download...';
	
  WizardForm.ClientHeight := WizardForm.ClientHeight + ScaleY(0);
  WizardForm.ClientWidth := WizardForm.ClientWidth + ScaleX(0);

  SetLength(ResolutionCombos, Length(BodyRepos));
  SetLength(SizesList, Length(BodyRepos));
  SetLength(SizeLabelList, Length(BodyRepos));

  PageHeight := 0;

  for I := 0 to High(BodyRepos) do
  begin
    BodyLabel := TLabel.Create(Page);
    BodyLabel.Parent := Page.Surface;
    BodyLabel.Left := ScaleX(8);
    BodyLabel.Top := ScaleY(PageHeight);
    BodyLabel.Caption := ExtractBodyName(BodyRepos[I]);

    ComboBox := TComboBox.Create(Page);
    ComboBox.Parent := Page.Surface;
    ComboBox.Left := ScaleX(150);
    ComboBox.Top := ScaleY(PageHeight);
    ComboBox.Width := ScaleX(50);
    ComboBox.OnChange := @ComboBoxChange;
    ComboBox.Tag := I;
    ResolutionCombos[I] := ComboBox;

    SizesList[I] := TStringList.Create;
    PopulateResolutions(ComboBox, I, SizesList[I]);

    Log('Dropdown for ' + BodyRepos[I] + ' created');

    VersionLabel := TLabel.Create(Page);
    VersionLabel.Parent := Page.Surface;
    VersionLabel.Left := ScaleX(275);
    VersionLabel.Top := ScaleY(PageHeight);
    if I < Length(BodyVersions) then
      VersionLabel.Caption := 'Version: ' + BodyVersions[I];

    SizeLabel := TLabel.Create(Page);
    SizeLabel.Parent := Page.Surface;
    SizeLabel.Left := ScaleX(420);
    SizeLabel.Top := ScaleY(PageHeight);
    if SizesList[I].Count > 0 then
      SizeLabel.Caption := 'Total Size: ' + IntToStr(StrToIntDef(SizesList[I][0], 0) div 1048576) + ' MB'
    else
      SizeLabel.Caption := 'Total Size: Unknown';
    SizeLabelList[I] := SizeLabel;

    Log('Size label for ' + BodyRepos[I] + ' initialized: ' + SizeLabel.Caption);

    PageHeight := PageHeight + 25;
  end;

  WizardForm.Repaint;
  Page.Surface.Repaint;

  Log('Wizard initialization completed');
end;



function GetRepoDownloadURLs(Repo, Resolution: string): TStringList;
var
  AssetName, BrowserDownloadURL: string;
  I, J, StartPos: Integer;
  ReleaseJSON, AssetsJSON: string;
begin
  Result := TStringList.Create;
  if GetCachedJSONForRepo(Repo, ReleaseJSON, AssetsJSON) then
  begin
    LatestReleaseAssetsJSON := AssetsJSON;
    LatestReleaseJSON := ReleaseJSON;
  end
  else
  begin
    GetLatestReleaseHTTPInfo(Repo);
    LatestReleaseAssetsJSON := LatestReleaseAssetsJSON;
  end;

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
              Result.Add(BrowserDownloadURL);
              // Check for multi-part files and add them to the list
              while FileExists(AssetName + '.7z.' + Format('%.3d', [Result.Count + 1])) do
              begin
                Result.Add(BrowserDownloadURL + '.7z.' + Format('%.3d', [Result.Count + 1]));
              end;
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

procedure AddToDownloadList(RepoName, Resolution, DestFilePath: string);
var
  DownloadURLs: TStringList;
  BrowserDownloadURL: string;
  I, J: Integer;
  URLExists: Boolean;
begin
  DownloadURLs := GetRepoDownloadURLs(RepoName, Resolution);

  for I := 0 to DownloadURLs.Count - 1 do
  begin
    BrowserDownloadURL := DownloadURLs[I];
    
    // Check if the file is already in the download list
    URLExists := False;
    for J := 0 to DownloadList.Count - 1 do
    begin
      if Pos(BrowserDownloadURL, DownloadList[J]) > 0 then
      begin
        URLExists := True;
        Break;
      end;
    end;

    if not URLExists then
    begin
      Log('Adding to download list: ' + BrowserDownloadURL + ' as ' + DestFilePath);
      DownloadList.Add(BrowserDownloadURL + '=' + DestFilePath);
    end
    else
    begin
      Log('Skipping already added URL: ' + BrowserDownloadURL);
    end;
  end;

  DownloadURLs.Free;
end;

procedure InitializeDownloadList;
var
  LatestVersion: string;
  ParallaxURL, ParallaxScatterTexturesURL: string;
  Resolution: string;
  I: Integer;
begin
  DownloadList := TStringList.Create;

  // RSS-Terrain
  AddToDownloadList('RSS-Reborn/RSS-Terrain', '', ExpandConstant('{tmp}\RSS_Terrain.7z'));

  // RSS-Configs
  AddToDownloadList('RSS-Reborn/RSS-Configs', '', ExpandConstant('{tmp}\RSS_Configs.7z'));

  // Planetary textures at user-selected resolutions
  for I := 0 to High(BodyRepos) do
  begin
    Resolution := ResolutionCombos[I].Text;
    AddToDownloadList(BodyRepos[I], Resolution, ExpandConstant('{tmp}\') + ExtractBodyName(BodyRepos[I]) + '_' + Resolution + '.7z');
  end;

  // RSSVE-Configs (if EVE and Scatterer are installed)
  if not EVEAndScattererCheckbox.Checked then
    AddToDownloadList('RSS-Reborn/RSSVE-Configs', '', ExpandConstant('{tmp}\RSSVE_Configs.7z'));

  // RSSVE-Textures
  if not EVEAndScattererCheckbox.Checked then
  AddToDownloadList('RSS-Reborn/RSSVE-Textures', '', ExpandConstant('{tmp}\RSSVE_Textures.7z'));

  // Scatterer (if not already downloaded by user)
  if not ScattererDownloaded then
    AddToDownloadList('LGhassen/Scatterer', '', ExpandConstant('{tmp}\Scatterer.zip'));

  // EVE (if not already downloaded by user)
  if not EVEDownloaded then
    AddToDownloadList('LGhassen/EnvironmentalVisualEnhancements', '', ExpandConstant('{tmp}\EVE.zip'));

  // Download Parallax and Parallax_ScatterTextures
  LatestVersion := GetLatestReleaseVersion('Gameslinx/Tessellation');
  if LatestVersion <> '' then
  begin
    ParallaxURL := 'https://github.com/Gameslinx/Tessellation/releases/download/' + LatestVersion + '/Parallax-' + LatestVersion + '.zip';
    ParallaxScatterTexturesURL := 'https://github.com/Gameslinx/Tessellation/releases/download/' + LatestVersion + '/Parallax_ScatterTextures-' + LatestVersion + '.zip';
    AddToDownloadList('Gameslinx/Tessellation', '', ExpandConstant('{tmp}\Parallax-' + LatestVersion + '.zip'));
    AddToDownloadList('Gameslinx/Tessellation', '', ExpandConstant('{tmp}\Parallax_ScatterTextures-' + LatestVersion + '.zip'));
  end;
end;

procedure MergeGameDataFolders;
var
  I: Integer;
  SourceDir, DestDir: string;
begin
  Log('Starting MergeGameDataFolders');
  if DownloadList = nil then
  begin
    Log('Error: DownloadList is not initialized.');
    Exit;
  end;

  Log('DownloadList.Count: ' + IntToStr(DownloadList.Count));
  if DownloadList.Count = 0 then
  begin
    Log('Warning: DownloadList is empty. Nothing to merge.');
    Exit;
  end;

  Log('Merging GameData folders');
  DestDir := ExpandConstant('{userdesktop}\MergedGameData');

  for I := 0 to DownloadList.Count - 1 do
  begin
    SourceDir := ExpandConstant('{tmp}') + '\' + ExtractFileNameWithoutExt(DownloadList[I]) + '\GameData';
    Log('Checking if directory exists: ' + SourceDir);
    if DirExists(SourceDir) then
    begin
      Log('Directory exists: ' + SourceDir + '. Moving to ' + DestDir);
    end
    else
    begin
      Log(Format('GameData folder not found in %s', [SourceDir]));
    end;
  end;
  Log('Completed MergeGameDataFolders');
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

procedure ExtractProc;
var
  I: Integer;
  Entry, TempFile: string;
begin
  for I := 0 to DownloadList.Count - 1 do
  begin
    Entry := DownloadList[I];
    TempFile := Trim(Copy(Entry, Pos('=', Entry) + 1, Length(Entry)));
    if Extract7Zip(ExpandConstant('{tmp}\') + ExtractFileName(TempFile), ExpandConstant('{app}')) then
    begin
      Log('Extraction completed for ' + TempFile);
    end
    else
    begin
      Log('Failed to extract ' + TempFile);
    end;
  end;
end;

procedure VerifyDownloadCompletion;
var
  I: Integer;
  Entry, TempFile: string;
  AllFilesDownloaded: Boolean;
begin
  AllFilesDownloaded := True;
  for I := 0 to DownloadList.Count - 1 do
  begin
    Entry := DownloadList[I];
    TempFile := Trim(Copy(Entry, Pos('=', Entry) + 1, Length(Entry)));
    if not FileExists(ExpandConstant('{tmp}\') + ExtractFileName(TempFile)) then
    begin
      Log('Error: File not downloaded: ' + TempFile);
      AllFilesDownloaded := False;
    end;
  end;

  if not AllFilesDownloaded then
  begin
    MsgBox('One or more files failed to download. Please check the logs for details.', mbError, MB_OK);
    Exit;
  end;

  Log('All files downloaded successfully.');
end;

procedure VerifyDownloadAndExtraction;
var
  I: Integer;
  Entry, TempFile: string;
  SkippedFiles: TStringList;
  SkippedFilesMessage: string;
begin
  SkippedFiles := TStringList.Create;
  try
    for I := 0 to DownloadList.Count - 1 do
    begin
      Entry := DownloadList[I];
      TempFile := Trim(Copy(Entry, Pos('=', Entry) + 1, Length(Entry)));
      TempFile := ExpandConstant(TempFile);

      if not FileExists(TempFile) then
      begin
        Log('Error: File not found after extraction: ' + TempFile);
        SkippedFiles.Add(TempFile);
      end;
    end;

    if SkippedFiles.Count > 0 then
    begin
      Log('The following files were skipped:');
      SkippedFilesMessage := 'The following files were skipped during the download and extraction process:'#13#10;
      for I := 0 to SkippedFiles.Count - 1 do
      begin
        Log(SkippedFiles[I]);
        SkippedFilesMessage := SkippedFilesMessage + SkippedFiles[I] + #13#10;
      end;
      MsgBox(SkippedFilesMessage, mbError, MB_OK);
    end
    else
    begin
      Log('All files verified to be downloaded and extracted successfully.');
    end;
  finally
    SkippedFiles.Free;
  end;
end;

procedure OnDownloadComplete;
begin
  try
    VerifyDownloadCompletion;
    VerifyDownloadAndExtraction;
    Log('Download and extraction process completed');
  except
    Log('Post-download steps failed: Unexpected error occurred.');
    MsgBox('Post-download steps failed. Please check the logs for details.', mbError, MB_OK);
    Exit;
  end;
end;

procedure StartInstallation;
begin
  Log('Starting RSS Reborn installation process.');
  InitializeDownloadsDir;
  if not RP1Checkbox.Checked then
  begin
    Log('RP-1 installation confirmation failed. Aborting installation.');
    MsgBox('You must have RP-1 installed and launched at least once before proceeding.', mbError, MB_OK);
    WizardForm.Close;
    Exit;
  end;
  RemoveObsoleteFolders;
end;

function NextButtonClick(CurPageID: Integer): Boolean;
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
    end;

    // Validate and set the KSP directory from the input page
    KSP_DIR := KSPDirPage.Values[0];
    if KSP_DIR = '' then
    begin
      Log('Error: KSP directory is empty.');
      MsgBox('Failed to set the KSP directory. Please select a valid directory.', mbError, MB_OK);
      Result := False;
      Exit;
    end;
    Log('KSP directory set to: ' + KSP_DIR);
  end
end;

procedure ClearDownloadDirectory;
begin;
	if DirectoryExists(DownloadsDir) then
    if not DelTree(DownloadsDir, True, True, True) then
      Log('Failed to delete Downloads directory.')
    else
      Log('DownloadsDir directory deleted.')
  else
    Log('DownloadsDir directory does not exist.');
end;

function URLDownloadToFile(Caller: Integer; URL: PAnsiChar; FileName: PAnsiChar; Reserved: Integer; StatusCB: Integer): Integer;
  external 'URLDownloadToFileA@urlmon.dll stdcall';

procedure DownloadAllFiles;
var
  URL, Dest, FileName, DownloadItem: String;
  I: Integer;
  DownloadResult: HRESULT;
begin
	DownloadPage.SetProgress(0, DownloadList.Count);
  for I := 0 to DownloadList.Count - 1 do
  begin
    DownloadPage.SetProgress(I, DownloadList.Count);
    // Extract URL and TempFile from DownloadList
    DownloadItem := DownloadList[I];
    URL := Copy(DownloadItem, 1, Pos('=', DownloadItem) - 1);
    FileName := Copy(DownloadItem, Pos('=', DownloadItem) + 1, Length(DownloadItem));

    // Ensure the correct destination path
    Dest := DownloadsDir + '\' + ExtractFileName(FileName);

    CurrentFileLabel.Caption := 'Downloading: ' + Dest;
    Log('Downloading ' + URL + ' to ' + Dest);

    DownloadResult := URLDownloadToFile(0, PAnsiChar(URL), PAnsiChar(Dest), 0, 0);
    Log('GitHub Call');
    if DownloadResult = S_OK then
    begin
      Log('Download successful. File saved to: ' + Dest);
    end
    else
    begin
      Log('Failed to download ' + URL + ' with error code: ' + IntToStr(DownloadResult));
      MsgBox('Failed to download ' + URL + ' with error code: ' + IntToStr(DownloadResult), mbError, MB_OK);
      Exit; // Exit on first failure
    end;
  end;
  DownloadPage.SetProgress(DownloadList.Count, DownloadList.Count);
end;

function InitializeDownloads: Boolean;
begin
  Result := True;
  try
    InitializeDownloadsDir;
    InitializeDownloadList;
    LogDownloadListDetails;
  except
    Log('Error in InitializeDownloads: ' + GetExceptionMessage);
    Result := False;
  end;
end;

function ExtractFiles: Boolean;
begin
  Result := True;
  try
    ExtractProc;
    Log('Download and extraction process completed');
  except
    Log('ExtractFiles failed: ' + GetExceptionMessage);
    Result := False;
  end;
end;

function MergeGameData: Boolean;
begin
  Result := True;
  try
    MergeGameDataFolders;
    Log('GameData folders merged successfully.');
  except
    Log('MergeGameData failed: ' + GetExceptionMessage);
    Result := False;
  end;
end;

procedure DeinitializeSetup;
// Cleans up resources and temporary files after installation.
begin
  Log('Deinitializing setup and cleaning up resources');  // Ensure complete logging
  CleanupTemporaryFiles;
	ClearDownloadDirectory;
  DeinitializeVariables;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
  begin
    Log('Install step reached. Starting installation process.');
    DownloadPage.Show;
    try
      // Call modular functions to perform tasks
      if not InitializeDownloads then
      begin
        Log('Failed to initialize downloads.');
        MsgBox('Failed to initialize downloads. Please check the logs for details.', mbError, MB_OK);
        Exit;
      end;

      DownloadAllFiles;

      if not ExtractFiles then
      begin
        Log('Extract files step failed.');
        MsgBox('Extract files step failed. Please check the logs for details.', mbError, MB_OK);
        Exit;
      end;

      if not MergeGameData then
      begin
        Log('Merge game data step failed.');
        MsgBox('Merge game data step failed. Please check the logs for details.', mbError, MB_OK);
        Exit;
      end;

      Log('Installation process completed successfully, cleaning up files now');
			DeinitializeSetup;
    finally
      DownloadPage.Hide;
    end;
  end;
end;
