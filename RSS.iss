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
	BodiesWithNoAssets: TStringList;
	CachedReleaseInfo: TStringList;
	CurrentFileLabel: TNewStaticText;
	CurrentFileLabelE: TNewStaticText;
	CurrentFileLabelM: TNewStaticText;
  DownloadList: TStringList;
  DownloadsDir: string;
	DownloadLogs: TStringList;
	ExtractionLogs: TStringList;
	EVEAndScattererCheckbox: TNewCheckBox;
  EVEdownloaded: Boolean;
	GitHubCount: TStringList;
	KSP_DIR: string;
  KSPDirPage: TInputDirWizardPage;
	DownloadPage: TOutputProgressWizardPage;
	ExtractPage: TOutputProgressWizardPage;
	MergePage: TOutputProgressWizardPage;
	LatestReleaseAssetsJSON: string;
	LatestReleaseJSON: string;
  LatestReleaseVersion: string;
	MyAccessToken: string;
	NoAssetsFound: Boolean;
	ParallaxVersion: string;
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
  
  if Assigned(DownloadList) then
    DownloadList.Free;
  DownloadList := TStringList.Create;
  
  if Assigned(CachedReleaseInfo) then
    CachedReleaseInfo.Free;
  CachedReleaseInfo := TStringList.Create;
  
  if Assigned(ExtractionLogs) then
    ExtractionLogs.Free;
  ExtractionLogs := TStringList.Create;
  
  if Assigned(DownloadLogs) then
    DownloadLogs.Free;
  DownloadLogs := TStringList.Create;
  
  if Assigned(GitHubCount) then
    GitHubCount.Free;
  GitHubCount := TStringList.Create;
	
	if Assigned(BodiesWithNoAssets) then
    BodiesWithNoAssets.Free;
  BodiesWithNoAssets := TStringList.Create;

  UserCanceled := False;
  Log('Variables initialized.');
  Log('========================================================');
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

function DirectoryExists(Dir: string): Boolean;
// Helper function
var
  FindRec: TFindRec;
begin
  Result := FindFirst(AddBackslash(Dir) + '*.*', FindRec);
  FindClose(FindRec);
end;

procedure InitializeDownloadsDir;
// Sets the directory for downloading files.
begin
  DownloadsDir := ExpandConstant('{userappdata}\RSSRebornDownloads');
  if not DirExists(DownloadsDir) then
  begin
    if CreateDir(DownloadsDir) then
      Log('Created download directory: ' + DownloadsDir)
    else
      Log('Failed to create download directory: ' + DownloadsDir);
  end;
  Log('Downloads directory initialized: ' + DownloadsDir);
end;

procedure ClearDownloadDirectory;
// Frees up space and prevents conflicts
begin;
	DownloadsDir := ExpandConstant('{userappdata}\RSSRebornDownloads');
	if DirectoryExists(DownloadsDir) then
    if not DelTree(DownloadsDir, True, True, True) then
      Log('Failed to delete Downloads directory.')
    else
      Log('DownloadsDir directory deleted.')
  else
    Log('DownloadsDir directory does not exist.');
end;

procedure ReadGitHubAccessTokenOnce;
// Reads the GitHub access token from the environment variable if it exists
// Allows users to have downloads per hour
var
ResultOne: String;
begin
  ResultOne := GetEnv('MY_ACCESS_TOKEN');
  if ResultOne <> '' then
    Log('GitHub access token found in environment variable.')
  else
    Log('GitHub access token not found in environment variable.');
end;

function ReadGitHubAccessToken: string;
// Reads the GitHub access token from the environment variable if it exists
// Allows users to have downloads per hour
begin
  Result := GetEnv('MY_ACCESS_TOKEN');
end;

function InitializeSetup: Boolean;
// Begin the sequence by checking for space and 7 zip
begin
  Result := True;
	ClearDownloadDirectory;
	ReadGitHubAccessTokenOnce;
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
	
procedure DeinitializeVariables;
// Frees allocated resources. Prevents memory leaks by releasing resources.
begin
  DownloadList.Free;
  CachedReleaseInfo.Free; 
	ExtractionLogs.Free;
	DownloadLogs.Free;
	GitHubCount.Free;
end;

function SendMessage(hWnd: LongInt; Msg: LongInt; wParam: LongInt; lParam: LongInt): LongInt;
// Helper function
  external 'SendMessageA@user32.dll stdcall';

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

function ExtractDirName(const FileName: string): string;
var
  TempResult: string;
begin
  TempResult := ExtractFileName(FileName);
  
  // Remove .001 if present
  if Length(TempResult) > 4 then
    if Copy(TempResult, Length(TempResult) - 3, 4) = '.001' then
      TempResult := Copy(TempResult, 1, Length(TempResult) - 4);
  
  // Remove other extensions
  Result := ExtractFileNameWithoutExt(TempResult);
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

function ExtractJSONField(const JSON, FieldName: string): string;
var
  StartPos, EndPos: Integer;
begin
  Result := '';
  StartPos := Pos(FieldName, JSON);
  if StartPos > 0 then
  begin
    StartPos := StartPos + Length(FieldName);
    EndPos := PosEx('"', JSON, StartPos + 1);
    if EndPos > 0 then
      Result := Copy(JSON, StartPos + 1, EndPos - StartPos - 1);
  end;
end;

function ExtractJSONArray(const JSON, ArrayName: string): string;
var
  StartPos, EndPos: Integer;
begin
  Result := '';
  StartPos := Pos(ArrayName, JSON);
  if StartPos > 0 then
  begin
    StartPos := StartPos + Length(ArrayName);
    EndPos := PosEx(']', JSON, StartPos);
    if EndPos > 0 then
      Result := Copy(JSON, StartPos, EndPos - StartPos + 1);
  end;
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
  IsMultiVolume: Boolean;
  FirstPartArchivePath: string;
begin
  // Determine the 7-Zip executable path
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

  // Verify the archive file exists
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
    FirstPartArchivePath := ArchivePath;
    Log('Multi-volume archive detected, using first part: ' + FirstPartArchivePath);
    CommandLine := Format('"%s" x "%s" -o"%s" -y', [ZipPath, FirstPartArchivePath, DestDir]);
  end
  else
  begin
    // Skip if it's a part of a multi-volume archive but not the first part
    if (Pos('.7z.', ArchivePath) > 0) and not IsMultiVolume then
    begin
      Log('Skipping part of multi-volume archive: ' + ArchivePath);
      Result := True;
      Exit;
    end;
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

  Log('Extraction successful for ' + ArchivePath);
  Result := True;
end;

procedure CacheReleaseInfo(Repo, Resolution, ReleaseJSON, AssetsJSON: string);
var
  CacheKey: string;
  CachedData: string;
  I: Integer;
begin
  CacheKey := Repo + ':' + Resolution;
  CachedData := ReleaseJSON + '|' + AssetsJSON;

  // Check if the key already exists
  for I := 0 to CachedReleaseInfo.Count - 1 do
  begin
    if Pos(CacheKey + '=', CachedReleaseInfo[I]) = 1 then
    begin
      // Update existing entry
      CachedReleaseInfo[I] := CacheKey + '=' + CachedData;
      Log('Updated cached data for ' + CacheKey);
      Exit;
    end;
  end;

  // Add new entry if not found
  CachedReleaseInfo.Add(CacheKey + '=' + CachedData);
  Log('Cached new data for ' + CacheKey);
end;

function GetCachedJSONForRepo(Repo, Resolution: string; var ReleaseJSON: string; var AssetsJSON: string): Boolean;
var
  CacheKey, CachedData: string;
  I, DelimPos: Integer;
begin
  Result := False;
  CacheKey := Repo + ':' + Resolution;

  for I := 0 to CachedReleaseInfo.Count - 1 do
  begin
    if Pos(CacheKey + '=', CachedReleaseInfo[I]) = 1 then
    begin
      CachedData := Copy(CachedReleaseInfo[I], Length(CacheKey) + 2, MaxInt); // +2 to skip '='
      DelimPos := Pos('|', CachedData);
      if DelimPos > 0 then
      begin
        ReleaseJSON := Copy(CachedData, 1, DelimPos - 1);
        AssetsJSON := Copy(CachedData, DelimPos + 1, Length(CachedData) - DelimPos);
        Result := True;
        Log('Cached data found for ' + CacheKey);
        Exit;
      end
      else
        Log('Invalid cached data format for ' + CacheKey);
    end;
  end;

  Log('No cached data found for ' + CacheKey);
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
var
  HttpCli: Variant;
  CombinedResponse: string;
begin
  if GetCachedJSONForRepo(Repo, '', LatestReleaseJSON, LatestReleaseAssetsJSON) then
  begin
    Log('Using cached release info for ' + Repo);
    LatestReleaseVersion := ExtractJSONField(LatestReleaseJSON, '"tag_name":"');
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
    GitHubCount.Add('GitHub call');

    if HttpCli.Status = 200 then
    begin
      CombinedResponse := HttpCli.ResponseText;
      if CombinedResponse = '' then
      begin
        Log('Empty response for latest release info');
        Exit;
      end;

      LatestReleaseJSON := CombinedResponse;

      // Parse the JSON response to extract necessary fields and assets
      LatestReleaseVersion := ExtractJSONField(CombinedResponse, '"tag_name":"');
      LatestReleaseAssetsJSON := ExtractJSONArray(CombinedResponse, '"assets":[');

      // Cache the combined JSON responses
      CacheReleaseInfo(Repo, '', LatestReleaseJSON, LatestReleaseAssetsJSON);
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
      end
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
  
  if GetCachedJSONForRepo(Repo, '', ReleaseJSON, AssetsJSON) then
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

  // Append "k" to numeric resolutions if not present and not containing "NoModels"
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
  
  if GetCachedJSONForRepo(BodyRepos[I], '', ReleaseJSON, AssetsJSON) then
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

function ExtractVersion(const URL: string): string;
// For URL input
var
  StartPos, EndPos: Integer;
begin
  StartPos := Pos('releases/download/', URL);
  if StartPos > 0 then
  begin
    StartPos := StartPos + Length('releases/download/');
    EndPos := PosEx('/', URL, StartPos);
    if EndPos > 0 then
      Result := Copy(URL, StartPos, EndPos - StartPos)
    else
      Result := Copy(URL, StartPos, Length(URL) - StartPos + 1);  
  end
  else
    Result := '';
end;

procedure RetrieveBodyInfo;
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

    if GetCachedJSONForRepo(BodyRepos[I], '', ReleaseJSON, AssetsJSON) then
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
var
  I, J, StartPos: Integer;
  AssetName, ReleaseJSON, AssetsJSON, Resolution: string;
  Size, TotalSize: Int64;
  AddedResolutions: TStringList;
  ResolutionIndex: Integer;
begin
  NoAssetsFound := False;
  AddedResolutions := TStringList.Create;
  try
    Log('========================================================');
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
								
								// Cache the resolution-specific data
								if (BodyRepos[RepoIndex] <> '') and (Resolution <> '') then
								begin
									ReleaseJSON := LatestReleaseJSON;
									AssetsJSON := LatestReleaseAssetsJSON;
									CachedReleaseInfo.Add(BodyRepos[RepoIndex] + ':' + Resolution + '=' + ReleaseJSON + '|' + AssetsJSON);
									Log('Cached resolution-specific data for repo: ' + BodyRepos[RepoIndex] + ' with resolution: ' + Resolution);
								end;
								
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
		
		// Add "None" option to the ComboBox
    ComboBox.Items.Add('None');
    Sizes.Add('0');

    // Check if the ComboBox is empty except for "None"
    if ComboBox.Items.Count = 1 then
    begin
      ComboBox.ItemIndex := 0;
      Log('No assets found for ' + BodyRepos[RepoIndex] + '. Added "None" option.');
      NoAssetsFound := True;
			BodiesWithNoAssets.Add(ExtractBodyName(BodyRepos[RepoIndex]));
    end
    else
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
          if (Index >= 0) and (Index < Length(SizeLabelList)) and Assigned(SizeLabelList[Index]) then
          begin
            SizeLabelList[Index].Caption := SizeStr;
            Log('SizeLabel updated to: ' + SizeStr);
          end
          else
          begin
            Log('Invalid or unassigned SizeLabelList index: ' + IntToStr(Index));
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
	Log('========================================================');
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
	
	// Merge Progress Bar Page 
	MergePage := CreateOutputProgressPage('Merging Files', 'This may take a while, but I''m sure you''re used to long KSP loading times by now.');
  CurrentFileLabelM := TNewStaticText.Create(MergePage);
  CurrentFileLabelM.Parent := MergePage.Surface;
  CurrentFileLabelM.Left := ScaleX(8);
  CurrentFileLabelM.Top := ScaleY(70);
  CurrentFileLabelM.Width := MergePage.SurfaceWidth - ScaleX(16);
  CurrentFileLabelM.Caption := 'Initializing Merging...';
	
  WizardForm.ClientHeight := WizardForm.ClientHeight + ScaleY(0);
  WizardForm.ClientWidth := WizardForm.ClientWidth + ScaleX(0);

  SetLength(ResolutionCombos, Length(BodyRepos));
  SetLength(SizesList, Length(BodyRepos));
  SetLength(SizeLabelList, Length(BodyRepos));

  PageHeight := 0;

	Log('========================================================');
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

		Log('========================================================');
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
	Log('========================================================');
end;

function GetRepoDownloadURLs(Repo, Resolution: string): TStringList;
var
  AssetName, BrowserDownloadURL: string;
  I, J, StartPos: Integer;
  ReleaseJSON, AssetsJSON: string;
begin
  Result := TStringList.Create;
  Log('Attempting to retrieve cached data for repository: ' + Repo + ' with resolution: ' + Resolution);

  if GetCachedJSONForRepo(Repo, Resolution, ReleaseJSON, AssetsJSON) then
  begin
    LatestReleaseAssetsJSON := AssetsJSON;
    LatestReleaseJSON := ReleaseJSON;
    Log('Using cached release info for ' + Repo + ' with resolution: ' + Resolution);
  end
  else
  begin
    Log('No cached data found for ' + Repo + ' with resolution: ' + Resolution + '. Fetching latest release info.');
    GetLatestReleaseHTTPInfo(Repo);
    LatestReleaseAssetsJSON := LatestReleaseAssetsJSON;
    LatestReleaseJSON := LatestReleaseJSON;
    // Cache the resolution-specific data
    CachedReleaseInfo.Add(Repo + ':' + Resolution + '=' + LatestReleaseJSON + '|' + LatestReleaseAssetsJSON);
    Log('Fetched latest release info for ' + Repo + ' with resolution: ' + Resolution);
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
        Log('Found asset in GetRepoDownloadURLs: ' + AssetName);
        // Skip "NoModels" assets unless explicitly selected
        if (Pos('NoModels', AssetName) > 0) and (Resolution <> 'NoModels') then
        begin
          Log('Skipping NoModels asset: ' + AssetName);
          StartPos := J + 1;
          Continue;
        end;
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
              if Result.IndexOf(BrowserDownloadURL) = -1 then
              begin
                Result.Add(BrowserDownloadURL);
                Log('Added download URL in GetRepoDownloadURLs: ' + BrowserDownloadURL);
              end
              else
              begin
                Log('Skipped duplicate URL in GetRepoDownloadURLs: ' + BrowserDownloadURL);
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

  Log('Completed retrieving download URLs for repository: ' + Repo + ' with resolution: ' + Resolution);
end;

procedure AddToDownloadList(RepoName, Resolution, DestFilePath: string);
var
  DownloadURLs: TStringList;
  BrowserDownloadURL: string;
  I, J: Integer;
  URLExists: Boolean;
  ReleaseJSON, AssetsJSON: string;
begin
  Log('========================================================');
  Log('AddToDownloadList called for Repo: ' + RepoName + ' with Resolution: ' + Resolution);

  if Resolution = 'None' then
  begin
    Log('Skipping download for ' + RepoName + ' as "None" is selected.');
    Exit;
  end;

  if not GetCachedJSONForRepo(RepoName, Resolution, ReleaseJSON, AssetsJSON) then
  begin
    GetLatestReleaseHTTPInfo(RepoName);
    ReleaseJSON := LatestReleaseJSON;
    AssetsJSON := LatestReleaseAssetsJSON;
    // Cache the resolution-specific data
    CachedReleaseInfo.Add(RepoName + ':' + Resolution + '=' + ReleaseJSON + '|' + AssetsJSON);
  end;

  // Get the download URLs for the specified resolution
  DownloadURLs := GetRepoDownloadURLs(RepoName, Resolution);

  for I := 0 to DownloadURLs.Count - 1 do
  begin
    BrowserDownloadURL := DownloadURLs[I];

    // Exclude the specific URL
    if Pos('Parallax_StockTextures', BrowserDownloadURL) > 0 then
    begin
      Log('Excluded URL: ' + BrowserDownloadURL);
      Continue;
    end;

    // Check if the file is already in the download list
    URLExists := False;
    for J := 0 to DownloadList.Count - 1 do
    begin
      if Pos(BrowserDownloadURL, DownloadList[J]) > 0 then
      begin
        URLExists := True;
        Log('Duplicate URL found in AddToDownloadList: ' + BrowserDownloadURL);
        Break;
      end;
    end;

    if not URLExists then
    begin
      Log('Add to downloads in AddToDownloadList: ' + BrowserDownloadURL + ' as ' + DestFilePath);
      DownloadList.Add(BrowserDownloadURL + '=' + DestFilePath);
    end
    else
    begin
      Log('Skip duplicate URL in AddToDownloadList: ' + BrowserDownloadURL);
    end;
  end;

  DownloadURLs.Free;
end;

procedure InitializeDownloadList;
// Adds non-body repos to download list
var
  Resolution, ParallaxURL: string;
  I: Integer;
begin
  Log('InitializeDownloadList called');

  // RSS-Terrain
  AddToDownloadList('RSS-Reborn/RSS-Terrain', '', ExpandConstant('{tmp}\RSS_Terrain.7z'));

  // RSS-Configs
  AddToDownloadList('RSS-Reborn/RSS-Configs', '', ExpandConstant('{tmp}\RSS_Configs.7z'));

  // Planetary textures at user-selected resolutions
  for I := 0 to High(BodyRepos) do
  begin
    Resolution := ResolutionCombos[I].Text;
		Log('========================================================');
		Log('Adding to download list: ' + BodyRepos[I] + ' with resolution: ' + Resolution);
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

  // Download Parallax
		ParallaxVersion := GetLatestReleaseVersion('Gameslinx/Tessellation');
    ParallaxURL := 'https://github.com/Gameslinx/Tessellation/releases/download/' + ParallaxVersion + '/Parallax-' + ParallaxVersion + '.zip';
    AddToDownloadList('Gameslinx/Tessellation', '', ExpandConstant('{tmp}\Parallax-' + ParallaxVersion + '.zip'));
end;

procedure CopyFileAndDelete(const SourceFile, DestFile: string);
begin
  //Log('Copying file: ' + SourceFile + ' to ' + DestFile);
  if not FileCopy(SourceFile, DestFile, True) then
  begin
    Log('Failed to copy file: ' + SourceFile + ' to ' + DestFile);
    RaiseException('Failed to copy file: ' + SourceFile);
  end
  else
  begin
    //Log('Copy successful: ' + SourceFile);
    // Delete the original file after copying
    if not DeleteFile(SourceFile) then
    begin
      Log('Failed to delete original file: ' + SourceFile);
      RaiseException('Failed to delete original file: ' + SourceFile);
    end
    else
    begin
      //Log('Successfully deleted original file: ' + SourceFile);
    end;
  end;
end;

function ForceRemoveDir(const DirPath: string): Boolean;
var
  Command: string;
  ResultCode: Integer;
begin
  Command := 'cmd.exe /C rd /S /Q "' + DirPath + '"';
  Result := Exec(Command, '', '', SW_HIDE, ewWaitUntilTerminated, ResultCode);
  if ResultCode = 0 then
    Result := True
  else
    Result := False;
end;

function TryRemoveDir(const DirPath: string): Boolean;
var
  RetryCount: Integer;
begin
  RetryCount := 0;
  Result := False;
  while (RetryCount < 5) and (not Result) do
  begin
    Result := RemoveDir(DirPath);
    if not Result then
    begin
      Log('Failed to delete directory: ' + DirPath + ', retrying...');
      Sleep(1000); // Wait for 1 second before retrying
    end;
    Inc(RetryCount);
  end;
  if not Result then
  begin
    Log('Standard delete failed, attempting forceful delete...');
    Result := ForceRemoveDir(DirPath);
  end;
  if Result then
    //Log('Successfully deleted directory: ' + DirPath)
  else
    Log('Failed to delete directory after retries: ' + DirPath);
end;

procedure CopyFileAndRenameIfNeeded(const SourceFile, DestFile: string);
var
  DestFileNew: string;
  Count: Integer;
begin
  Count := 0;
  DestFileNew := DestFile;
  while FileExists(DestFileNew) do
  begin
    Inc(Count);
    DestFileNew := ChangeFileExt(DestFile, '') + '_' + IntToStr(Count) + ExtractFileExt(DestFile);
  end;

  if not FileCopy(SourceFile, DestFileNew, True) then
  begin
    Log('Failed to copy file: ' + SourceFile + ' to ' + DestFileNew);
    RaiseException('Failed to copy file: ' + SourceFile);
  end
  else
  begin
    if not DeleteFile(SourceFile) then
    begin
      Log('Failed to delete original file: ' + SourceFile);
    end;
  end;
end;

procedure CopyDirectory(const SourceDir, DestDir: string);
var
  FindRec: TFindRec;
  SourceFile, DestFile: string;
begin
  if not DirExists(DestDir) then
  begin
    if not CreateDir(DestDir) then
    begin
      Log('Failed to create directory: ' + DestDir);
      RaiseException('Failed to create directory: ' + DestDir);
    end;
  end;

  if FindFirst(SourceDir + '\*', FindRec) then
  begin
    try
      repeat
        SourceFile := SourceDir + '\' + FindRec.Name;
        DestFile := DestDir + '\' + FindRec.Name;

        if FindRec.Attributes and FILE_ATTRIBUTE_DIRECTORY <> 0 then
        begin
          if (FindRec.Name <> '.') and (FindRec.Name <> '..') then
          begin
            CopyDirectory(SourceFile, DestDir + '\' + FindRec.Name);
          end;
        end
        else
        begin
          CopyFileAndRenameIfNeeded(SourceFile, DestFile);
        end;
      until not FindNext(FindRec);
    finally
      FindClose(FindRec);
    end;
  end;

  // Attempt to delete the source directory after copying
  if not TryRemoveDir(SourceDir) then
  begin
    Log('Failed to delete original directory: ' + SourceDir);
  end;
end;

procedure MergeGameDataFolders;
var
  DownloadsDir, GameDataMerged: string;
  FindRec: TFindRec;
  SourceDir: string;
  ProgressCounter: Integer;
begin
  // Define the source and destination directories
  DownloadsDir := ExpandConstant('{userappdata}\RSSRebornDownloads');
  GameDataMerged := DownloadsDir + '\GameDataMerged';
	
  // Ensure the GameDataMerged directory exists
  if not DirExists(GameDataMerged) then
  begin
    Log('GameDataMerged directory does not exist. Creating: ' + GameDataMerged);
    if not CreateDir(GameDataMerged) then
    begin
      Log('Failed to create GameDataMerged directory: ' + GameDataMerged);
      MsgBox('Failed to create GameDataMerged directory: ' + GameDataMerged, mbError, MB_OK);
      RaiseException('Failed to create GameDataMerged directory: ' + GameDataMerged);
      Exit; 
    end
    else
    begin
      //Log('Successfully created GameDataMerged directory: ' + GameDataMerged);
    end;
  end;

  ProgressCounter := 0;

  if FindFirst(DownloadsDir + '\*', FindRec) then
  begin
    try
      repeat
        SourceDir := DownloadsDir + '\' + FindRec.Name;

        // Skip if it's not a directory or if it's the GameDataMerged directory itself or if it's a zip file
        if (FindRec.Attributes and FILE_ATTRIBUTE_DIRECTORY <> 0) and
           (FindRec.Name <> '.') and (FindRec.Name <> '..') and
           (SourceDir <> GameDataMerged) and
           (ExtractFileExt(FindRec.Name) <> '.zip') then
        begin
          Log('Moving directory: ' + SourceDir + ' to ' + GameDataMerged);
          CopyDirectory(SourceDir, GameDataMerged);
        end;
        
        // Update progress
        Inc(ProgressCounter);
        CurrentFileLabelM.Caption := 'Merging: ' + SourceDir;
        MergePage.SetProgress(ProgressCounter, 50);
        WizardForm.Update;
                
      until not FindNext(FindRec);
    finally
      FindClose(FindRec);
    end;
  end;
  MergePage.SetProgress(50, 50);
  WizardForm.Update;
  MergePage.SetProgress(DownloadList.Count, DownloadList.Count);
end;

procedure MoveParallaxDirectories;
var
  SourceDir, DestDir: string;
begin
  Log('Moving Parallax and Parallax_StockTextures to GameData directory.');
  
  // Define the source and destination directories
  SourceDir := ExpandConstant('{userappdata}\RSSRebornDownloads\GameDataMerged');
  DestDir := SourceDir + '\GameData';

  // Move Parallax directory
  if DirExists(SourceDir + '\Parallax') then
  begin
    Log('Moving directory: ' + SourceDir + '\Parallax to ' + DestDir);
    CopyDirectory(SourceDir + '\Parallax', DestDir + '\Parallax');
    if not TryRemoveDir(SourceDir + '\Parallax') then
    begin
      Log('Failed to delete original Parallax directory.');
    end;
  end;

  // Move Parallax_StockTextures directory
  if DirExists(SourceDir + '\Parallax_StockTextures') then
  begin
    Log('Moving directory: ' + SourceDir + '\Parallax_StockTextures to ' + DestDir);
    CopyDirectory(SourceDir + '\Parallax_StockTextures', DestDir + '\Parallax_StockTextures');
    if not TryRemoveDir(SourceDir + '\Parallax_StockTextures') then
    begin
      Log('Failed to delete original Parallax_StockTextures directory.');
    end;
  end;
  
  Log('Successfully moved Parallax and Parallax_StockTextures directories.');
  Log('Completed merging GameData folders');
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
  I, PartCount: Integer;
  FileName, CurrentLoc, URL, DownloadItem, Dest, EndDest: string;
  IsMultiPart, ExtractionSuccessful: Boolean;
begin
  ExtractPage.SetProgress(0, DownloadList.Count);
  for I := 0 to DownloadList.Count - 1 do
  begin
    WizardForm.Update;
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

procedure VerifyDownloads;
// Checks that everything downloaded okay
var
  I, GitHubCallCount: Integer;
  AllFilesDownloaded: Boolean;
begin
  AllFilesDownloaded := True;
  
  // We don't want stock textures, duh
  if DirectoryExists(DownloadsDir + '\Parallax_StockTextures' + ParallaxVersion) then
  begin
    if not DelTree(DownloadsDir + '\Parallax_StockTextures' + ParallaxVersion, True, True, True) then
      DownloadLogs.Add('Failed to delete Parallax_StockTextures directory.')
    else
      DownloadLogs.Add('Parallax_StockTextures directory deleted.')
  end
  else
    DownloadLogs.Add('Parallax_StockTextures directory does not exist.');

  // Check for any failure messages in DownloadLogs
  for I := 0 to DownloadLogs.Count - 1 do
  begin
    Log(DownloadLogs[I]);
    if Pos('Error: ', DownloadLogs[I]) > 0 then
    begin
      AllFilesDownloaded := False;
    end;
  end;
	
	// Write out all the log entries collected during download
	GitHubCallCount := GitHubCount.Count;
	Log('========================================================');
	Log('GitHub Calls ' + IntToStr(GitHubCallCount));
	Log('========================================================');

  if not AllFilesDownloaded then
  begin
    MsgBox('One or more files failed to download. Please check the logs for details.', mbError, MB_OK);
    Exit;
  end;

  Log('All files downloaded successfully.');
end;

procedure VerifyExtraction;
// Checks that everything extracted using 7-Zip okay
var
  I: Integer;
  AllFilesExtracted: Boolean;
begin
  AllFilesExtracted := True;

  // Write out all the log entries collected during extraction
  for I := 0 to ExtractionLogs.Count - 1 do
  begin
    Log(ExtractionLogs[I]);
    if Pos('Error: ', ExtractionLogs[I]) > 0 then
    begin
      AllFilesExtracted := False;
    end;
  end;

  if not AllFilesExtracted then
  begin
    MsgBox('One or more files failed to extract. Please check the logs for details.', mbError, MB_OK);
  end
  else
  begin
    Log('All files extracted successfully.');
		Log('========================================================');
  end;
end;

procedure OnDownloadComplete;
// Calls checks to be completed after download and extraction
// Can probably just call verify extraction if needed
begin
  try
    VerifyDownloads;
    Log('Download process completed');
		Log('========================================================');
  except
    Log('Post-download steps failed: Unexpected error occurred.');
    MsgBox('Post-download steps failed. Please check the logs for details.', mbError, MB_OK);
		Log('========================================================');
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
  
    if NoAssetsFound then
    begin
      MsgBox('No assets found for the latest release of one or more bodies. They will not be downloaded.', mbInformation, MB_OK);
    end;
  end;
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
  Log('========================================================');
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
    DownloadLogs.Add('Downloading ' + URL + ' to ' + Dest);

    DownloadResult := URLDownloadToFile(0, PAnsiChar(URL), PAnsiChar(Dest), 0, 0);
    GitHubCount.Add('Github Call');
    if not DownloadResult = S_OK then
    begin
      DownloadLogs.Add('Failed to download ' + URL + ' with error code: ' + IntToStr(DownloadResult));
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
		Log('========================================================');
    Result := False;
  end;
end;

function MergeGameData: Boolean;
// Helper function to call proc, returns error if it cannot execute 
begin
  Result := True;
  try
    MergeGameDataFolders;
		MoveParallaxDirectories;
    Log('GameData folders merged successfully.');
		Log('========================================================');
  except
    Log('MergeGameData failed: ' + GetExceptionMessage);
		Log('========================================================');
    Result := False;
  end;
end;

procedure DeinitializeSetup;
// Cleans up resources and temporary files after installation.
begin
  Log('Deinitializing setup and cleaning up resources');  
	//ClearDownloadDirectory;
  DeinitializeVariables;
end;

procedure CurStepChanged(CurStep: TSetupStep);
// Actual process steps for installation
begin
  if CurStep = ssInstall then
  begin
	  Log('========================================================');
    Log('Install step reached. Starting installation process.');
    DownloadPage.Show;
    try
      // Call modular functions to perform tasks
			begin
				if not InitializeDownloads then
				begin
					Log('Failed to initialize downloads.');
					MsgBox('Failed to initialize downloads. Please check the logs for details.', mbError, MB_OK);
					Exit;
				end
				else
				begin
					Log('CurStepChanged');
				end;
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
			MergePage.Show;
		end;

		try
      if not MergeGameData then
      begin
        Log('Merge game data step failed.');
        MsgBox('Merge game data step failed. Please check the logs for details.', mbError, MB_OK);
        Exit;
      end;
		finally;
			MergePage.hide;
		end;

      Log('Installation process completed successfully, cleaning up files now');
			DeinitializeSetup;

  end;
end;
