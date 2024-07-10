; Inno Setup Script
; David Robie (DRobie22)
; This installer follows RSS Reborn's GitHub Instructions

; Please note: This is my first time using inno setup
; Some sections could be done better, more efficiently, and be overall less complex
; Feel free to contribute, or offer constructive criticism 

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
	SECONDS_IN_A_DAY = 86400;
  SECONDS_IN_AN_HOUR = 3600;
  SECONDS_IN_A_MINUTE = 60;
  MOVEFILE_COPY_ALLOWED = $2;
  MOVEFILE_REPLACE_EXISTING = $1;	
	
var
  AssetDataList: array of TStringList;
  BodyRepos: array[0..11] of string;
  BodySizes: array of string;	
	BodyVersions: array of string;
	CachedReleaseInfo: TStringList;
	CurrentFileLabel: TNewStaticText;
	CurrentFileLabelE: TNewStaticText;
  DownloadList: TStringList;
  DownloadsDir: string;
	EVEAndScattererCheckbox: TNewCheckBox;
  EVEdownloaded: Boolean;
	KSP_DIR: string;
  KSPDirPage: TInputDirWizardPage;
	DownloadPage: TOutputProgressWizardPage;
	ExtractPage: TOutputProgressWizardPage;
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
// Function to assist 7Zip  Init
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
// Begin the sequence by checking for space and 7 zip
begin
  Result := True;
  if not IsEnoughDiskSpaceAvailable then
  begin
    MsgBox('You need at least 50 GB of free disk space to install RSS-Reborn without issues.', mbError, MB_OK);
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
// Optional Logging for listing downloads before execution 
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
// Reads the GitHub access token from the environment variable if it exists
// Allows users to have downloads per hour
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
  CachedReleaseInfo.Free; 
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

function FormatSize(SizeInBytes: Integer): string;
// Converts file sizes from bytes to MB.
begin
  Result := IntToStr(Round(SizeInBytes / 1048576)) + ' MB'; 
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

function ReplaceSubstring(const S, OldPattern, NewPattern: string): string;
// Helper function
var
  PosStart: Integer;
begin
  Result := S;
  PosStart := Pos(OldPattern, Result);
  while PosStart > 0 do
  begin
    Delete(Result, PosStart, Length(OldPattern));
    Insert(NewPattern, Result, PosStart);
    PosStart := Pos(OldPattern, Result); 
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

function CustomExtractFileName(DownloadURL: string): string;
// Helper function
var
  FileNameStartPos: Integer;
begin
  // Find the last '/' in the URL
  FileNameStartPos := LastDelimiter('/', DownloadURL) + 1;
  
  // Extract the filename from the URL, including everything after the last '/'
  Result := Copy(DownloadURL, FileNameStartPos, Length(DownloadURL) - FileNameStartPos + 1);
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
  Result := -1; 
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
    BodyName := Repo;  
  
  // Remove the 'RSS-' prefix if it exists
  if Pos('RSS-', BodyName) = 1 then
    Delete(BodyName, 1, Length('RSS-'));
  
  Result := BodyName;
end;

function DateToDays(Year, Month, Day: Integer): Int64;
// Helper Function
var
  A, Y, M: Int64;
begin
  A := (14 - Month) div 12;
  Y := Year + 4800 - A;
  M := Month + 12 * A - 3;
  Result := Day + ((153 * M + 2) div 5) + (365 * Y) + (Y div 4) - (Y div 100) + (Y div 400) - 32045;
end;

function GetCurrentUnixTime: Int64;
// Helper Function
var
  DateTimeStr: string;
  Year, Month, Day, Hour, Minute, Second: Integer;
  DaysSinceUnixEpoch: Int64;
  SecondsInCurrentDay: Int64;
begin
  DateTimeStr := GetDateTimeString('yyyy-mm-dd hh:nn:ss', #0, #0);
  // Extract year, month, day, hour, minute, second
  Year := StrToInt(Copy(DateTimeStr, 1, 4));
  Month := StrToInt(Copy(DateTimeStr, 6, 2));
  Day := StrToInt(Copy(DateTimeStr, 9, 2));
  Hour := StrToInt(Copy(DateTimeStr, 12, 2));
  Minute := StrToInt(Copy(DateTimeStr, 15, 2));
  Second := StrToInt(Copy(DateTimeStr, 18, 2));

  // Calculate days since Unix epoch (January 1, 1970)
  DaysSinceUnixEpoch := DateToDays(Year, Month, Day) - DateToDays(1970, 1, 1);
  // Calculate seconds in the current day
  SecondsInCurrentDay := (Hour * SECONDS_IN_AN_HOUR) + (Minute * SECONDS_IN_A_MINUTE) + Second;

  // Calculate total Unix time
  Result := (DaysSinceUnixEpoch * SECONDS_IN_A_DAY) + SecondsInCurrentDay;
end;

function SecondsToTimeStr(Seconds: Int64): string;
// Helper Function 
var
  Days, Hours, Minutes: Int64;
begin
  Days := Seconds div SECONDS_IN_A_DAY;
  Hours := (Seconds mod SECONDS_IN_A_DAY) div SECONDS_IN_AN_HOUR;
  Minutes := (Seconds mod SECONDS_IN_AN_HOUR) div SECONDS_IN_A_MINUTE;
  Result := Format('%d days, %d hours, %d minutes', [Days, Hours, Minutes]);
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
// Use user's 7zip to unzip files, including multi-volume archives
var
  ZipPath: string;
  ResultCode: Integer;
  CommandLine: string;
  IsMultiVolume: Boolean;
  FirstPartArchivePath: string;
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

  // Check if the archive is a multi-volume archive by looking for ".001" extension
  IsMultiVolume := ExtractFileExt(ArchivePath) = '.001';
  if IsMultiVolume then
  begin
    Log('Multi-volume archive detected');
    FirstPartArchivePath := ArchivePath;
    Log('Using first part of multi-volume archive: ' + FirstPartArchivePath);
    CommandLine := Format('"%s" x "%s" -o"%s" -y', [ZipPath, FirstPartArchivePath, DestDir]);
  end
  else
  begin
    CommandLine := Format('"%s" x "%s" -o"%s" -y', [ZipPath, ArchivePath, DestDir]);
  end;

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

function GetCachedJSONForRepo(Repo: string; var ReleaseJSON: string; var AssetsJSON: string): Boolean;
// Assists call to github for caching info locally to minimize http calls
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

procedure CheckRateLimit;
var
  HttpCli: Variant;
  Response, CoreResource, ResetTimeStr: string;
  ResetTime, CurrentTime, TimeRemaining: Int64;
  I, J: Integer;
begin
  try
    HttpCli := CreateOleObject('WinHttp.WinHttpRequest.5.1');
    HttpCli.Open('GET', 'https://api.github.com/rate_limit', False);
    HttpCli.SetRequestHeader('User-Agent', 'RSS-Reborn-Installer');
    MyAccessToken := ReadGitHubAccessToken;
    if MyAccessToken <> '' then
      HttpCli.SetRequestHeader('Authorization', 'token ' + MyAccessToken);

    HttpCli.Send;

    if HttpCli.Status = 200 then
    begin
      Response := HttpCli.ResponseText;
      I := Pos('"core":{', Response);
      if I > 0 then
      begin
        CoreResource := Copy(Response, I, Length(Response) - I + 1);
        I := Pos('"reset":', CoreResource);
        if I > 0 then
        begin
          I := I + Length('"reset":');
          J := PosEx(',', CoreResource, I);
          ResetTimeStr := Copy(CoreResource, I, J - I);
          ResetTime := StrToInt64(ResetTimeStr);
          CurrentTime := GetCurrentUnixTime;
          TimeRemaining := ResetTime - CurrentTime;

          if TimeRemaining > 0 then
            MsgBox('Rate limit will be reset in: ' + SecondsToTimeStr(TimeRemaining), mbInformation, MB_OK)
          else
            MsgBox('Rate limit has already been reset.', mbInformation, MB_OK);
        end
        else
          Log('Failed to find reset time in response.');
      end
      else
        Log('Failed to find core resource in response.');
    end
    else
      Log('Failed to fetch rate limit info, status: ' + IntToStr(HttpCli.Status));
  except
    Log('Exception occurred while checking rate limit: ' + GetExceptionMessage);
  end;
end;

procedure GetLatestReleaseHTTPInfo(Repo: string);
// Actual call to get information to cache
var
  HttpCli: Variant;
  I, J: Integer;
  CombinedResponse, CachedData: string;
begin
  // Check if cached info already exists
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

			// Cache the combined JSON responses
      CachedData := Repo + '=' + LatestReleaseJSON + '|' + LatestReleaseAssetsJSON;
      CachedReleaseInfo.Add(CachedData); 
    end
    else
    begin
      Log('Failed to fetch latest release info, status: ' + IntToStr(HttpCli.Status));
      if HttpCli.Status = 403 then
      begin
        Log('HTTP 403 Forbidden error. Possible rate limit exceeded.');
        if MsgBox('GitHub download rate limit exceeded. Please wait a moment before retrying. Click OK to retry now, or Cancel to exit.', mbInformation, MB_OKCANCEL) = IDOK then
        begin
				  CheckRateLimit;
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
  Version := LatestReleaseVersion; 
  Size := GetFileSizeForLatestReleaseFromAssets(LatestReleaseAssetsJSON); 
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
        Result := Result + #13#10; 
      Result := Result + AssetURLs[I];
    end;
  end;

  AssetURLs.Free;
end;

function LatestReleaseHasFiles(Repo: string): Boolean;
var
  ReleaseJSON, AssetsJSON: string;
begin
  Log('Checking if latest release has files for repository: ' + Repo);
  Result := False;
  
  if GetCachedJSONForRepo(Repo, ReleaseJSON, AssetsJSON) then
  begin
    LatestReleaseAssetsJSON := AssetsJSON;
    Log('Using cached release info for ' + Repo);
    
    if Pos('"assets":[', AssetsJSON) > 0 then
    begin
      Result := True;
      Log('Release has associated files.');
    end
    else
    begin
      Log('No assets found in cached release info.');
    end;
  end
  else
  begin
    Log('Cached data not found for repository: ' + Repo);
  end;
end;

function ExtractResolution(AssetName: String): String;
// Extracts resolution information from asset names.
var
  UnderscorePos, DashPos, DotPos, DelimiterPos: Integer;
  Resolution: String;
  ContainsDigit: Boolean;
  I: Integer;
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

  // Find the position of the last dot before the file extension
  DotPos := LastDelimiter('.', AssetName);
  if DotPos > 0 then
  begin
    // Check for multi-volume archives
    if Copy(AssetName, DotPos - 3, 4) = '.7z.' then
      DotPos := DotPos - 4;

    if DelimiterPos > 0 then
    begin
      // Extract the substring between the delimiter and the last dot before the extension
      Resolution := Copy(AssetName, DelimiterPos + 1, DotPos - DelimiterPos - 1);
    end;
  end;

  // Exclude any resolution with "scale"
  if Pos('scale', LowerCase(Resolution)) > 0 then
    Resolution := '';

  // Ensure the resolution contains at least one digit or "NoModels"
  ContainsDigit := False;
  for I := 1 to Length(Resolution) do
  begin
    if (Resolution[I] >= '0') and (Resolution[I] <= '9') then
    begin
      ContainsDigit := True;
      Break;
    end;
  end;
  
  if not ContainsDigit and (Pos('NoModels', Resolution) = 0) then
    Resolution := '';

  // Append "k" to numeric resolutions if not present
  if (Resolution <> '') and (Resolution[Length(Resolution)] <> 'k') and ContainsDigit then
    Resolution := Resolution + 'k';

  // Return the extracted resolution
  Result := Resolution;
end;

function GetLatestReleaseVersion(Repo: string): string;
//Extracts release information from cached data
var
  ReleaseJSON, AssetsJSON: string;
  I, J: Integer;
begin
  Log('Getting latest release version for repository: ' + Repo);
  Result := '';
  
  if GetCachedJSONForRepo(Repo, ReleaseJSON, AssetsJSON) then
  begin
    LatestReleaseJSON := ReleaseJSON;
    Log('Using cached release info for ' + Repo);
    
    I := Pos('"tag_name":"', ReleaseJSON);
    if I > 0 then
    begin
      I := I + Length('"tag_name":"');
      J := FindNextQuote(ReleaseJSON, I);
      if J > 0 then
      begin
        Result := Copy(ReleaseJSON, I, J - I);
        Log('Latest release version found: ' + Result);
      end;
    end
    else
    begin
      Log('No tag_name found in cached release info.');
    end;
  end
  else
  begin
    Log('Cached data not found for repository: ' + Repo);
  end;
end;

procedure RetrieveBodyInfo;
// Combined proc to call version size and asset functions
var
  I: Integer;
  ReleaseJSON, AssetsJSON: string;
begin
  Log('Retrieving body info');
  SetLength(BodyVersions, Length(BodyRepos));
  SetLength(BodySizes, Length(BodyRepos));
  SetLength(AssetDataList, Length(BodyRepos)); 

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
      AssetDataList[I].Free; 

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
// Takes cached data and extracts information for the UI
var
  I, J, StartPos: Integer;
  AssetName, Resolution: string;
  Size, TotalSize: Int64;
  AddedResolutions: TStringList;
  ResolutionIndex: Integer;
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

          // Exclude files with "scale" in the name
          if (Pos('scale', LowerCase(AssetName)) > 0) then
          begin
            Log('Skipping asset: ' + AssetName + ' due to exclusion criteria.');
            StartPos := J + 1;
            Continue;
          end;

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
              // Check if the exact Resolution exists in AddedResolutions
              ResolutionIndex := AddedResolutions.IndexOf(Resolution);
              if ResolutionIndex = -1 then
              begin
                ComboBox.Items.Add(Resolution);
                AddedResolutions.Add(Resolution);
                Sizes.Add(IntToStr(Size));
                Log('Added resolution: ' + Resolution + ' with size: ' + IntToStr(Size));
              end
              else
              begin
                TotalSize := StrToInt64(Sizes[ResolutionIndex]) + Size;
                Sizes[ResolutionIndex] := IntToStr(TotalSize);
                Log('Updated resolution: ' + Resolution + ' with new size: ' + IntToStr(TotalSize));
              end;
            end;
          end;
          StartPos := J + 1;
        end;
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
// Proc to assist in instant UI updates based on user input
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
    Result := False; 
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
// Initialize the UI
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
  
  // Read GitHub access token from the registry if it exists
  MyAccessToken := ReadGitHubAccessToken;

	//Get information ready for the UI
  RetrieveBodyInfo;

	// Checkbox on welcome page
  RP1Checkbox := TNewCheckBox.Create(WizardForm);
  RP1Checkbox.Parent := WizardForm.WelcomePage;
  RP1Checkbox.Left := ScaleX(18);
  RP1Checkbox.Top := ScaleY(175);
  RP1Checkbox.Width := WizardForm.ClientWidth - ScaleX(36);
  RP1Checkbox.Height := ScaleY(40);
  RP1Checkbox.Caption := 'I confirm that I have successfully run RP-1 once.';
  RP1Checkbox.Checked := False;
  Log('RP-1 installation confirmation checkbox created');

	// Checkbox on welcome page
  EVEAndScattererCheckbox := TNewCheckBox.Create(WizardForm);
  EVEAndScattererCheckbox.Parent := WizardForm.WelcomePage;
  EVEAndScattererCheckbox.Left := ScaleX(18);
  EVEAndScattererCheckbox.Top := ScaleY(215);
  EVEAndScattererCheckbox.Width := WizardForm.ClientWidth - ScaleX(36);
  EVEAndScattererCheckbox.Height := ScaleY(40);
  EVEAndScattererCheckbox.Caption := '(Optional) I am using Blackrack''s EVE and Scatterer.';
  EVEAndScattererCheckbox.Checked := False;
  Log('EVE and Scatterer download confirmation checkbox created');
  
  // KSP directory input page
  KSPDirPage := CreateInputDirPage(wpWelcome, 'KSP Directory', 'Select the KSP directory', 'Please select the directory where Kerbal Space Program is installed.', False, '');
  KSPDirPage.Add('');
  KSPDirPage.Values[0] := 'C:\Program Files (x86)\Steam\steamapps\common\Kerbal Space Program\GameData';  // Set the default directory

	// Resolution input page
  Page := CreateCustomPage(wpWelcome, 'Select Resolutions', 'Select the desired resolution for each body');
  wpSelectResolutions := Page.ID;

	// Download Progress Bar Page
  DownloadPage := CreateOutputProgressPage('Downloading Files', 'This may take a while, but I''m sure you''re used to long KSP loading times by now.');
  CurrentFileLabel := TNewStaticText.Create(DownloadPage);
  CurrentFileLabel.Parent := DownloadPage.Surface;
  CurrentFileLabel.Left := ScaleX(8);
  CurrentFileLabel.Top := ScaleY(70);
  CurrentFileLabel.Width := DownloadPage.SurfaceWidth - ScaleX(16);
  CurrentFileLabel.Caption := 'Initializing download...';
	
	// Extraction Progress Bar Page 
	ExtractPage := CreateOutputProgressPage('Extracting Files', 'This may take a while, but I''m sure you''re used to long KSP loading times by now.');
  CurrentFileLabelE := TNewStaticText.Create(ExtractPage);
  CurrentFileLabelE.Parent := ExtractPage.Surface;
  CurrentFileLabelE.Left := ScaleX(8);
  CurrentFileLabelE.Top := ScaleY(70);
  CurrentFileLabelE.Width := ExtractPage.SurfaceWidth - ScaleX(16);
  CurrentFileLabelE.Caption := 'Initializing Extraction...';
	
  WizardForm.ClientHeight := WizardForm.ClientHeight + ScaleY(0);
  WizardForm.ClientWidth := WizardForm.ClientWidth + ScaleX(0);

  SetLength(ResolutionCombos, Length(BodyRepos));
  SetLength(SizesList, Length(BodyRepos));
  SetLength(SizeLabelList, Length(BodyRepos));

  PageHeight := 0;

	//Processing for Resolutions Page
  for I := 0 to High(BodyRepos) do
  begin
		// Body Name
    BodyLabel := TLabel.Create(Page);
    BodyLabel.Parent := Page.Surface;
    BodyLabel.Left := ScaleX(8);
    BodyLabel.Top := ScaleY(PageHeight);
    BodyLabel.Caption := ExtractBodyName(BodyRepos[I]);

		// Resolution Drop Down
    ComboBox := TComboBox.Create(Page);
    ComboBox.Parent := Page.Surface;
    ComboBox.Left := ScaleX(100);
    ComboBox.Top := ScaleY(PageHeight);
    ComboBox.Width := ScaleX(100);
    ComboBox.OnChange := @ComboBoxChange;
    ComboBox.Tag := I;
    ResolutionCombos[I] := ComboBox;

    SizesList[I] := TStringList.Create;
    PopulateResolutions(ComboBox, I, SizesList[I]);

    Log('Dropdown for ' + BodyRepos[I] + ' created');

		// Texture Version No.
    VersionLabel := TLabel.Create(Page);
    VersionLabel.Parent := Page.Surface;
    VersionLabel.Left := ScaleX(275);
    VersionLabel.Top := ScaleY(PageHeight);
    if I < Length(BodyVersions) then
      VersionLabel.Caption := 'Version: ' + BodyVersions[I];

		// Approx size of download, sum of all assets in Resolution Pack
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
// Pulls direct download links from a repo's cache
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
// Proc to add direct download URL to a queue, executed later
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
      Log('Add to downloads: ' + BrowserDownloadURL + ' as ' + DestFilePath);
      DownloadList.Add(BrowserDownloadURL + '=' + DestFilePath);
    end
    else
    begin
      Log('Skip duplicate URL: ' + BrowserDownloadURL);
    end;
  end;

  DownloadURLs.Free;
end;

procedure InitializeDownloadList;
// Adds non-body repos to download list
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

  // Scatterer (if not using Blackrack's)
  if not ScattererDownloaded then
    AddToDownloadList('LGhassen/Scatterer', '', ExpandConstant('{tmp}\Scatterer.zip'));

  // EVE (if not using Blackrack's)
  if not EVEDownloaded then
    AddToDownloadList('LGhassen/EnvironmentalVisualEnhancements', '', ExpandConstant('{tmp}\EVE.zip'));

  // Download Parallax and Parallax_ScatterTextures
		LatestVersion := GetLatestReleaseVersion('Gameslinx/Tessellation');
    ParallaxURL := 'https://github.com/Gameslinx/Tessellation/releases/download/' + LatestVersion + '/Parallax-' + LatestVersion + '.zip';
    ParallaxScatterTexturesURL := 'https://github.com/Gameslinx/Tessellation/releases/download/' + LatestVersion + '/Parallax_ScatterTextures-' + LatestVersion + '.zip';
    AddToDownloadList('Gameslinx/Tessellation', '', ExpandConstant('{tmp}\Parallax-' + LatestVersion + '.zip'));
    AddToDownloadList('Gameslinx/Tessellation', '', ExpandConstant('{tmp}\Parallax_ScatterTextures-' + LatestVersion + '.zip'));
end;

function MoveFolder(const SourcePath, DestPath: String): Boolean;
var
  FindRec: TFindRec;
  SourceFile, DestFile: String;
begin
  Result := True;
  if FindFirst(AddBackslash(SourcePath) + '*', FindRec) then
  begin
    try
        SourceFile := AddBackslash(SourcePath) + FindRec.Name;
        DestFile := AddBackslash(DestPath) + FindRec.Name;

        if (FindRec.Attributes and FILE_ATTRIBUTE_DIRECTORY) = FILE_ATTRIBUTE_DIRECTORY then
        begin
          if (FindRec.Name <> '.') and (FindRec.Name <> '..') then
          begin
            if not DirExists(DestFile) then
            begin
              if not CreateDir(DestFile) then
              begin
                Log('Failed to create destination: ' + DestFile);
                MsgBox('Failed to create destination: ' + DestFile, mbError, MB_OK);
                Result := False;
                Exit;
              end;
            end;
            if not MoveFolder(SourceFile, DestFile) then
            begin
              Result := False;
              Exit;
            end;
          end;
        end
        else
        begin
          if not RenameFile(SourceFile, DestFile) then
          begin
            Log('Failed to move file: ' + SourceFile + ' to ' + DestFile);
            MsgBox('Failed to move file: ' + SourceFile + ' to ' + DestFile, mbError, MB_OK);
            Result := False;
            Exit;
          end;
        end;
    finally
      FindClose(FindRec);
    end;
  end
  else
  begin
    Log('Source does not exist: ' + SourcePath);
    MsgBox('Source does not exist: ' + SourcePath, mbError, MB_OK);
    Result := False;
  end;
end;

procedure MoveGameDataFolders(SourceDir, DestDir: string);
var
  FindRec: TFindRec;
  SourcePath, GameDataPath: string;
begin
  SourcePath := AddBackslash(SourceDir);

  if FindFirst(SourcePath + '*', FindRec) then
  begin
    try
      repeat
        if (FindRec.Attributes and FILE_ATTRIBUTE_DIRECTORY <> 0) and (FindRec.Name <> '.') and (FindRec.Name <> '..') then
        begin
          GameDataPath := AddBackslash(SourcePath) + FindRec.Name + '\GameData';
          if DirExists(GameDataPath) then
          begin
            if not MoveFolder(GameDataPath, DestDir) then
            begin
              Log(Format('Failed to move from %s to %s', [GameDataPath, DestDir]));
              MsgBox(Format('Failed to move from %s to %s', [GameDataPath, DestDir]), mbError, MB_OK);
            end
            else
              Log(Format('Moved GameData from %s to %s', [GameDataPath, DestDir]));
          end
          else
            Log(Format('GameData folder not found in %s', [SourcePath]));
        end;
      until not FindNext(FindRec);
    finally
      FindClose(FindRec);
    end;
  end
  else
  begin
    Log('Nothing found in ' + SourcePath);
    MsgBox('Nothing found in ' + SourcePath, mbError, MB_OK);
  end;
end;

procedure MoveSpecialFolders(SourceDir, DestDir: string);
var
  ParallaxVersion, ParallaxSource, ParallaxDest, ParallaxScatterVersion, ParallaxScatterSource, ParallaxScatterDest: string;
begin
  Log('Starting MoveSpecialFolders');

  // Get the latest version for Parallax and Parallax_ScatterTextures from cache
  ParallaxVersion := GetLatestReleaseVersion('Gameslinx/Tessellation');
  ParallaxScatterVersion := GetLatestReleaseVersion('Gameslinx/Tessellation');

  // Define the source and destination paths for Parallax
  ParallaxSource := AddBackslash(SourceDir) + 'Parallax-' + ParallaxVersion;
  ParallaxDest := AddBackslash(DestDir) + 'GameData\Parallax';
  if DirExists(ParallaxSource) then
  begin
    if not DirExists(ParallaxDest) then
    begin
      if CreateDir(ParallaxDest) then
        Log('Created directory: ' + ParallaxDest)
      else
        Log('Failed to create directory: ' + ParallaxDest);
    end;

    if MoveFolder(ParallaxSource, ParallaxDest) then
      Log('Moved Parallax folder to ' + ParallaxDest)
    else
      Log('Failed to move Parallax folder to ' + ParallaxDest);
  end
  else
  begin
    Log('Parallax folder does not exist in ' + ParallaxSource);
  end;

  // Define the source and destination paths for Parallax_ScatterTextures
  ParallaxScatterSource := AddBackslash(SourceDir) + 'Parallax_ScatterTextures-' + ParallaxScatterVersion;
  ParallaxScatterDest := AddBackslash(DestDir) + 'GameData\Parallax_StockTextures';
  if DirExists(ParallaxScatterSource) then
  begin
    if not DirExists(ParallaxScatterDest) then
    begin
      if CreateDir(ParallaxScatterDest) then
        Log('Created directory: ' + ParallaxScatterDest)
      else
        Log('Failed to create directory: ' + ParallaxScatterDest);
    end;

    if MoveFolder(ParallaxScatterSource, ParallaxScatterDest) then
      Log('Moved Parallax_ScatterTextures folder to ' + ParallaxScatterDest)
    else
      Log('Failed to move Parallax_ScatterTextures folder to ' + ParallaxScatterDest);
  end
  else
  begin
    Log('Parallax_StockTextures folder does not exist in ' + ParallaxScatterSource);
  end;

  Log('Completed MoveSpecialFolders');
end;

procedure MergeGameDataFolders;
var
  DownloadsDir, DestDir: string;
begin
  Log('Starting MergeGameDataFolders');

  // Define the Downloads directory
  DownloadsDir := ExpandConstant('{userappdata}\RSSRebornDownloads');

  // Define the destination GameData directory
  DestDir := DownloadsDir + '\GameData';
  if not DirExists(DestDir) then
  begin
    if CreateDir(DestDir) then
      Log('Created: ' + DestDir)
    else
      Log('Failed to create: ' + DestDir);
  end;

  // Move GameData folders from each extracted directory to GameData
  MoveGameDataFolders(DownloadsDir, DestDir);
	MoveSpecialFolders(DownloadsDir, DestDir);

  Log('Completed MergeGameDataFolders');
end;

procedure CleanupTemporaryFiles;
// Deletes temporary files created during the installation process.
// Keeps the system clean and frees up disk space.
var
  TempDir: string;
begin
  TempDir := ExpandConstant('{tmp}');
  Log('Cleaning up temporary files in ' + TempDir);  
  if DirExists(TempDir) then
  begin
    if not DelTree(TempDir, True, True, True) then
      Log('Failed to clean up ' + TempDir)
    else
      Log('Cleaned up successfully.');
  end
  else
    Log('Nothing in ' + TempDir);
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
// The proc that calls upon 7 zip
var
  I, PartCount: Integer;
  FileName, CurrentLoc, URL, DownloadItem, Dest, EndDest: string;
  IsMultiPart, ExtractionSuccessful: Boolean;
begin
  ExtractPage.SetProgress(0, DownloadList.Count);
  for I := 0 to DownloadList.Count - 1 do
  begin
    ExtractPage.SetProgress(I, DownloadList.Count);
    DownloadItem := DownloadList[I];
    URL := Copy(DownloadItem, 1, Pos('=', DownloadItem) - 1);
    FileName := CustomExtractFileName(URL);
    CurrentLoc := DownloadsDir + '\' + FileName;
    
    // Determine destination directory name
    if Pos('.7z.001', FileName) > 0 then
      Dest := ReplaceSubstring(FileName, '.7z.001', '')
    else if Pos('.7z', FileName) > 0 then
      Dest := ReplaceSubstring(FileName, '.7z', '')
    else if Pos('.zip', FileName) > 0 then
      Dest := ReplaceSubstring(FileName, '.zip', '');
      
    EndDest := DownloadsDir + '\' + Dest;
    
    CurrentFileLabelE.Caption := 'Extracting: ' + FileName;
    WizardForm.Update;

    IsMultiPart := Pos('.7z.001', FileName) > 0;
    ExtractionSuccessful := False;
    
    // Check if the file is a multi-volume archive part (only .001) or a single archive
    if IsMultiPart or (Pos('.7z', FileName) > 0) or (Pos('.zip', FileName) > 0) then
    begin
      if Extract7Zip(CurrentLoc, EndDest) then
      begin
        Log('Extraction complete:' + CurrentLoc);
        ExtractionSuccessful := True;
        
        // If it's a multi-part archive, check that all parts were extracted
        if IsMultiPart then
        begin
          PartCount := 1;
          while FileExists(DownloadsDir + '\' + Dest + '.7z.' + Format('%.3d', [PartCount])) do
            Inc(PartCount);
            
          Dec(PartCount); // Subtract one because the loop increments an extra time
          
          // Check that all parts are extracted
          if DirectoryExists(EndDest) then
          begin
            Log('Multi-part archive ' + Dest + ' extracted successfully with ' + IntToStr(PartCount) + ' parts.');
          end
          else
          begin
            Log('Failed to verify extraction of multi-part archive ' + Dest);
            ExtractionSuccessful := False;
          end;

          // Skip the remaining parts
          while (I + 1 < DownloadList.Count) and (Pos(Dest, DownloadList[I + 1]) > 0) do
            Inc(I);
        end;
      end
      else
      begin
        Log('Failed to extract ' + CurrentLoc);
      end;
    end;
    
    // Handle extraction failure
    if not ExtractionSuccessful then
    begin
      Log('Extraction process encountered issues for ' + CurrentLoc);
      MsgBox('Extraction process encountered issues for ' + CurrentLoc, mbError, MB_OK);
    end;
  end;
  ExtractPage.SetProgress(DownloadList.Count, DownloadList.Count);
end;

procedure VerifyDownloadCompletion;
// Checks that everything downloaded okay
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
    if not FileExists(DownloadsDir + '\' + ExtractFileName(TempFile)) then
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

  // We don't want stock textures, duh
  if DirectoryExists(DownloadsDir + '\Parallax_StockTextures') then
  begin
    if not DelTree(DownloadsDir + '\Parallax_StockTextures', True, True, True) then
      Log('Failed to delete Parallax_StockTextures directory.')
    else
      Log('Parallax_StockTextures directory deleted.')
  end
  else
    Log('Parallax_StockTextures directory does not exist.');

  Log('All files downloaded successfully.');
end;

procedure VerifyExtraction;
// Checks that everything extracted using 7-Zip okay
var
  I: Integer;
  Entry, TempFile, ExtractedPath: string;
  AllFilesExtracted: Boolean;
begin
  AllFilesExtracted := True;
  for I := 0 to DownloadList.Count - 1 do
  begin
    Entry := DownloadList[I];
    TempFile := Trim(Copy(Entry, Pos('=', Entry) + 1, Length(Entry)));
    ExtractedPath := ChangeFileExt(DownloadsDir + '\' + ExtractFileName(TempFile), '');

    if not DirectoryExists(ExtractedPath) then
    begin
      Log('Error: Extraction failed for: ' + ExtractedPath);
      AllFilesExtracted := False;
    end;
  end;

  if not AllFilesExtracted then
  begin
    MsgBox('One or more files failed to extract. Please check the logs for details.', mbError, MB_OK);
    Exit;
  end;

  Log('All files extracted successfully.');
end;

procedure OnDownloadComplete;
// Calls checks to be completed after download and extraction
// Can probably just call verify extraction if needed
begin
  try
    VerifyDownloadCompletion;
    Log('Download process completed');
  except
    Log('Post-download steps failed: Unexpected error occurred.');
    MsgBox('Post-download steps failed. Please check the logs for details.', mbError, MB_OK);
    Exit;
  end;
end;

procedure StartInstallation;
// Ensures RP-1 checkbox was checked by user before removing folders
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
// When user changes the UI page, checks are made
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
// Frees up space and prevents conflicts
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
// The full procedure that executes the download list
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
    FileName := CustomExtractFileName(URL);

    // Ensure the correct destination path
    Dest := DownloadsDir + '\' + FileName;
		
		CurrentFileLabel.Caption := 'Downloading: ' + FileName;
		WizardForm.Update;
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
// Helper function to call proc, returns error if it cannot execute 
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
// Helper function to call proc, returns error if it cannot execute 
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
// Helper function to call proc, returns error if it cannot execute 
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
  Log('Deinitializing setup and cleaning up resources');  
  CleanupTemporaryFiles;
	//ClearDownloadDirectory;
  DeinitializeVariables;
end;

procedure CurStepChanged(CurStep: TSetupStep);
// Actual process steps for installation
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
		finally
			OnDownloadComplete;
		  DownloadPage.Hide;
			ExtractPage.Show;
		end

		try
      if not ExtractFiles then
      begin
        Log('Extract files step failed.');
        MsgBox('Extract files step failed. Please check the logs for details.', mbError, MB_OK);
        Exit;
      end;
		finally
		  VerifyExtraction;
		  ExtractPage.Hide;
		end;

      if not MergeGameData then
      begin
        Log('Merge game data step failed.');
        MsgBox('Merge game data step failed. Please check the logs for details.', mbError, MB_OK);
        Exit;
      end;

      Log('Installation process completed successfully, cleaning up files now');
			DeinitializeSetup;

  end;
end;
