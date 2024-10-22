; Inno Setup Script
; David Robie (DRobie22)
; This installer follows RSS Reborn's GitHub Instructions

; Please note: This is my first time using inno setup
; Some sections could be done better, more efficiently, and be overall less complex
; Feel free to contribute, or offer constructive criticism 

; The entire system for storing api JSON data should be redone
; It is very much held together with duct tape and prayers
; ...but it works

#define MyAppName "RSS Reborn Installer"
#define MyAppVersion "1.2.0"
#define MyAppPublisher "DRobie22"
#define MyAppURL "drobie22/RSS-Reborn-Installer"
#define MyAppExeName "RSS-Reborn-Installer.exe"

[Setup]
AppName={#MyAppName}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
AppVersion={#MyAppVersion}
Compression=lzma
DefaultDirName={autopf}\RSS-Reborn-Installer
DefaultGroupName=RSS-Reborn-Installer
DisableDirPage=yes 
DisableWelcomePage=no
OutputBaseFilename=RSSRebornInstaller
PrivilegesRequired=admin
SetupLogging=yes
SolidCompression=yes
WizardImageFile=images\Mars.bmp
WizardImageStretch=no
WizardSmallImageFile=images\icon.bmp
WizardStyle=modern

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

#include ReadReg(HKLM, 'Software\WOW6432Node\Mitrich Software\Inno Download Plugin', 'InstallDir') + '\idp.iss'

[Messages]
WelcomeLabel2=This will install RSS Reborn into your KSP directory.%n%nMod created and maintained by Ballisticfox, Techo, and VaNnadin.%n[name/ver] created by DRobie22.

[Files]
Source: "Licenses\license.txt"; DestDir: "{app}"; Flags: dontcopy;
Source: "Licenses\lgpl-3.0.txt"; DestDir: "{app}"; Flags: dontcopy;
Source: "idp.dll"; DestDir: "{tmp}"; Flags: dontcopy;

[Code]
const
  GitHubAPI = 'https://api.github.com/repos/';
  MAX_PATH = 260;
  MOVEFILE_COPY_ALLOWED = $2;
  MOVEFILE_REPLACE_EXISTING = $1; 
  RequiredSpace = 50000000000;
  RSSConfigsRepo = 'RSS-Reborn/RSS-Configs';
  RSSTexturesRepo = 'RSS-Reborn/RSS-Terrain';
  S_OK = 0;
  SECONDS_IN_A_DAY = 86400;
  SECONDS_IN_AN_HOUR = 3600;
  SECONDS_IN_A_MINUTE = 60;
  URLMON_DLL = 'urlmon.dll';
  IDP_DOWNLOADING = 1;
  IDP_DOWNLOADCOMPLETE = 2;
  IDP_DOWNLOADERROR = 3;
	
var
  AAinKSPCheckbox: TCheckBox;
  AddTUFXCheckbox: TCheckBox;
  AppExeName: String;
  AppName: String;
  AppPublisher: String;
  AppURL: String;
  AppVersion: String;
  AssetDataList: array of TStringList;
  AssetsStore: TStringList;
  BodiesWithNoAssets: TStringList;
  BodyRepos: array[0..12] of string;
  BodySizes: array of string;
  BodyVersions: array of string;
  CommunitySettings: TCheckBox;
  CurrentFileLabelE: TNewStaticText;
  CurrentFileLabelM: TNewStaticText;
  DownloadList: TStringList;
  DownloadLogs: TStringList;
  //DownloadPage: TOutputProgressWizardPage;
  DownloadsDir: string;
  EVEdownloaded: Boolean;
  ExtractPage: TOutputProgressWizardPage;
  ExtractionLogs: TStringList;
  FileBrowseButton: TButton;
  FileEdit: TEdit;
  FileLabel: TLabel;
  GitHubCount: TStringList;
  HQCloudsCheckbox: TCheckBox;
  KSPDirPage: TInputDirWizardPage;
  KSP_DIR: string;
  LatestReleaseAssetsJSON: string;
  LatestReleaseJSON: string;
  LatestReleaseVersion: string;
  LineSeparator: TBevel;
  MergePage: TOutputProgressWizardPage;
  MyAccessToken: string;
  NoAssetsFound: Boolean;
  Note1: TLabel;
  Note2: TLabel;
  ParallaxVersion: string;
  RAYVOL_DIR: string;
  RP1Checkbox: TNewCheckBox;
  RaymarchedVolumetricsCheckbox: TNewCheckBox;
  ReflectionQualityCheckbox: TCheckBox;
  ReflectionTextureCheckbox: TCheckBox;
  ReleaseStore: TStringList;
  ResolutionCombos: array of TComboBox;
  ReverseRobocopyCommands: TStringList;
  RobocopyCommands: TStringList;
  ScaledAssetsAvailable: array of Boolean;
  ScaledCheckboxes: array of TCheckBox;
  ScattererDownloaded: Boolean;
  SevenZipPath: string;
  SizeLabelList: array of TLabel;
  Sizes: array of Int64;
  SizesList: array of TStringList;
  StoredReleaseInfo: TStringList;
  UserCanceled: Boolean;
  WinRARPath: string;
  wpSelectResolutions: Integer;

type
  TResolutionPages = array of TWizardPage;
  TResolutionCombos = array of TComboBox;
  TProgressCallback = procedure(Position, TotalSize: Integer);
  TDownloadCallback = function(Progress: Integer; ProgressMax: Integer; Status: Integer; Param: Integer): Integer;
  TDownloadProgressCallback = function(Progress: Integer; ProgressMax: Integer; Status: Integer; Param: Integer): Integer;

procedure InitializeConstants;
begin
  AppName := '{#MyAppName}';
  AppVersion := '{#MyAppVersion}';
  AppPublisher := '{#MyAppPublisher}';
  AppURL := '{#MyAppURL}';
  AppExeName := '{#MyAppExeName}';
end;

procedure InitializeDownloadsDir;
// Sets the directory for downloading files.
begin
  DownloadsDir := ExpandConstant(KSP_DIR + '\RSSRebornDownloads');
  if not DirExists(DownloadsDir) then
  begin
    if CreateDir(DownloadsDir) then
      Log('Created download directory: ' + DownloadsDir)
    else
      Log('Failed to create download directory: ' + DownloadsDir);
  end;
  Log('Downloads directory initialized: ' + DownloadsDir); 
end;

procedure InitializeVariables;
// Initializes global variables to their default states.
begin
  Log('Initializing variables...');
  EVEDownloaded := False;
  ScattererDownloaded := False;
  
  if Assigned(DownloadList) then
    DownloadList.Free;
  DownloadList := TStringList.Create;
  
  if Assigned(StoredReleaseInfo) then
    StoredReleaseInfo.Free;
  StoredReleaseInfo := TStringList.Create;
  
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
  DownloadsDir := ExpandConstant('{userappdata}\RSSRebornDownloads');
  Log('Variables initialized.');
  Log('========================================================');
end;

procedure InitializeArrayLengths;
// Sets the lengths of arrays to prepare for storing data.
var
  I: Integer;

begin
  // Set the lengths of the arrays
  SetLength(Sizes, 13);
  SetLength(SizeLabelList, 13);
  SetLength(SizesList, 13);
  for I := 0 to High(SizesList) do
    SizesList[I] := TStringList.Create;
end;

procedure InitializeRobocopyLogs;
begin
  RobocopyCommands := TStringList.Create;
  ReverseRobocopyCommands := TStringList.Create;
end;

procedure LogRobocopyCommand(const Command, ReverseCommand: string);
begin
  RobocopyCommands.Add(Command);
  ReverseRobocopyCommands.Add(ReverseCommand);
  Log(' [ROBOCOPY] Executed robocopy command: ' + Command);
  Log('Reverse robocopy command: ' + ReverseCommand);
end;

function GetDiskFreeSpaceEx(
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

function IsEnoughDiskSpaceAvailable(Drive: String): Boolean;
// Uses windows api to check available space (need 50 GB to be safe)
var
  FreeSpace: Int64;
begin
  FreeSpace := GetFreeSpace(Drive);
  Log('Free disk space on ' + Drive + ': ' + IntToStr(FreeSpace div (1024 * 1024 * 1024)) + ' GB');
  Result := FreeSpace >= RequiredSpace;
end;

function DirectoryExists(Dir: string): Boolean;
// Checks to see if a requested folder path exists (yes or no) 
var
  FindRec: TFindRec;
begin
  Result := FindFirst(AddBackslash(Dir) + '*.*', FindRec);
  FindClose(FindRec);
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

function GetDriveList: TStringList;
// For users who play on a different drive
var
  Drives: TStringList;
  DriveLetter: Char;
begin
  Drives := TStringList.Create;
  DriveLetter := 'A';
  while Ord(DriveLetter) <= Ord('Z') do
  begin
    if DirExists(DriveLetter + ':\') then
    begin
      Drives.Add(DriveLetter + ':\');
    end;
    DriveLetter := Chr(Ord(DriveLetter) + 1);
  end;
  Result := Drives;
end;

function FindProgram(ProgramFolder, ProgramExe: string): string;
// Searches for a specified program in a path 
var
  Drives: TStringList;
  I: Integer;
  ProgramPath: string;
begin
  Result := '';
  Drives := GetDriveList;
  try
    for I := 0 to Drives.Count - 1 do
    begin
      ProgramPath := Drives[I] + 'Program Files\' + ProgramFolder + '\' + ProgramExe;
      if FileExists(ProgramPath) then
      begin
        Result := ProgramPath;
        Exit;
      end;
      ProgramPath := Drives[I] + 'Program Files (x86)\' + ProgramFolder + '\' + ProgramExe;
      if FileExists(ProgramPath) then
      begin
        Result := ProgramPath;
        Exit;
      end;
    end;
  finally
    Drives.Free;
  end;
end;

function InitializeSetup: Boolean;
// Begin the sequence by checking for space and required programs
begin
  Log('Initializing setup...');
  ReleaseStore := TStringList.Create;
  AssetsStore := TStringList.Create;
  Result := True;
  ReadGitHubAccessTokenOnce;
  InitializeRobocopyLogs;
	
	SevenZipPath := FindProgram('7-Zip', '7z.exe');
	WinRARPath := FindProgram('WinRAR', 'WinRAR.exe');

	if SevenZipPath = '' then
	begin
		if WinRARPath = '' then
		begin
			MsgBox('Neither 7-Zip nor WinRAR is installed. Please install either 7-Zip or WinRAR to continue.', mbError, MB_OK);
			Result := False;
			Log('Neither 7-Zip nor WinRAR is installed.');
		end
		else
		begin
			Log('WinRAR found at ' + WinRARPath);
		end;
	end
	else
	begin
		Log('7-Zip found at ' + SevenZipPath);
	end;

  if Result then
    Log('Setup initialization successful.');
end;

procedure DeinitializeVariables;
// Frees allocated resources. Prevents memory leaks by releasing resources.
begin
  DownloadList.Free;
  StoredReleaseInfo.Free; 
	ExtractionLogs.Free;
	DownloadLogs.Free;
	GitHubCount.Free;
end;

function SendMessage(hWnd: LongInt; Msg: LongInt; wParam: LongInt; lParam: LongInt): LongInt;
  external 'SendMessageA@user32.dll stdcall';

function FormatSize(SizeInBytes: Integer): string;
// Converts file sizes from bytes to MB.
begin
  Result := IntToStr(Round(SizeInBytes / 1048576)) + ' MB'; 
end;

function ParseSize(SizeStr: String): Int64;
var
  SizeValue: Double;
  UnitStr: String;
  PosSpace: Integer;
begin
  // Remove prefix if present
  if Pos('Total Size: ', SizeStr) = 1 then
    Delete(SizeStr, 1, Length('Total Size: '));

  PosSpace := Pos(' ', SizeStr);
  if PosSpace > 0 then
  begin
    try
      SizeValue := StrToFloat(Trim(Copy(SizeStr, 1, PosSpace - 1)));
    except
      // Handle all exceptions here
      Result := 0;
      Log('Error converting size value for size string: ' + SizeStr);
      Exit;
    end;
    
    UnitStr := Trim(Copy(SizeStr, PosSpace + 1, Length(SizeStr) - PosSpace));
    
    if UnitStr = 'Bytes' then
      Result := Round(SizeValue)
    else if UnitStr = 'KB' then
      Result := Round(SizeValue * 1024)
    else if UnitStr = 'MB' then
      Result := Round(SizeValue * 1048576)
    else
    begin
      Log('Unknown unit: ' + UnitStr + ' for size string: ' + SizeStr);
      Result := 0; // Unknown unit
    end;
  end
  else
  begin
    Log('Invalid size string: ' + SizeStr);
    Result := 0; // Invalid size string
  end;
end;

function IsDirectoryEmpty(DirPath: string): Boolean;
// Checks to see if the given direcory contains files
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
// String manipulation, gives position of last given character
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
// String manipulation to substitute text
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
// Specifically used to remove suffix from file name
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
// Specifically used to get file name from a URL string
var
  FileNameStartPos: Integer;
begin
  // Find the last '/' in the URL
  FileNameStartPos := LastDelimiter('/', DownloadURL) + 1;
  
  // Extract the filename from the URL, including everything after the last '/'
  Result := Copy(DownloadURL, FileNameStartPos, Length(DownloadURL) - FileNameStartPos + 1);
end;

function FindNextQuote(const JSON: string; StartIndex: Integer): Integer;
// String manipulation to get position of next "
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
// String manipulation to get length of string
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
// String manipulation for body repos to get only the name for the UI
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
// Logic for GitHub limit time interpretation 
var
  A, Y, M: Int64;
begin
  A := (14 - Month) div 12;
  Y := Year + 4800 - A;
  M := Month + 12 * A - 3;
  Result := Day + ((153 * M + 2) div 5) + (365 * Y) + (Y div 4) - (Y div 100) + (Y div 400) - 32045;
end;

function GetCurrentUnixTime: Int64;
// Logic for GitHub limit time interpretation 
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
// Logic for GitHub limit time interpretation 
var
  Days, Hours, Minutes: Int64;
begin
  Days := Seconds div SECONDS_IN_A_DAY;
  Hours := (Seconds mod SECONDS_IN_A_DAY) div SECONDS_IN_AN_HOUR;
  Minutes := (Seconds mod SECONDS_IN_AN_HOUR) div SECONDS_IN_A_MINUTE;
  Result := Format('%d days, %d hours, %d minutes', [Days, Hours, Minutes]);
end;

function ExtractJSONField(const JSON, FieldName: string): string;
// String manipulation
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
// String manipulation
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
  BodyRepos[6] := 'RSS-Reborn/RSS-MarsMoons'
  BodyRepos[7] := 'RSS-Reborn/RSS-Jupiter';
  BodyRepos[8] := 'RSS-Reborn/RSS-Saturn';
  BodyRepos[9] := 'RSS-Reborn/RSS-Uranus';
  BodyRepos[10] := 'RSS-Reborn/RSS-Neptune';
  BodyRepos[11] := 'RSS-Reborn/RSS-AsteroidBelt';
  BodyRepos[12] := 'RSS-Reborn/RSS-KuiperBelt';
  Log('BodyRepos array initialized');
end;

function ExtractProgram(ArchivePath, DestDir: string): Boolean;
//Uses 7 zip or winrar to extract a compressed file 
var
  ResultCode: Integer;
  CommandLine: string;
  IsMultiVolume: Boolean;
  FirstPartArchivePath: string;
begin
  Result := False;

  // Verify the archive file exists
  if not FileExists(ArchivePath) then
  begin
    Log('Archive file not found: ' + ArchivePath);
    Exit;
  end;

  // Check if the archive is a multi-volume archive by looking for ".001" extension
  IsMultiVolume := ExtractFileExt(ArchivePath) = '.001';
  if IsMultiVolume then
  begin
    FirstPartArchivePath := ArchivePath;
    Log('Multi-volume archive detected, using first part: ' + FirstPartArchivePath);
    CommandLine := Format('"%s" x "%s" -o"%s" -y', [SevenZipPath, FirstPartArchivePath, DestDir]);
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
    CommandLine := Format('"%s" x "%s" -o"%s" -y', [SevenZipPath, ArchivePath, DestDir]);
  end;

  Log('[EXTRACTING] Running command: ' + CommandLine);

  // Use 7-Zip to extract if available
  if SevenZipPath <> '' then
  begin
		if not Exec(SevenZipPath, 'x "' + ArchivePath + '" -o"' + DestDir + '" -y', '', SW_HIDE, ewWaitUntilTerminated, ResultCode) then
    begin
      Log(Format('Failed to execute 7-Zip for %s, error code: %d', [ArchivePath, ResultCode]));
      Exit;
    end;
  end
	
  // Use WinRAR to extract if 7-Zip is not available
  else if WinRARPath <> '' then
  begin
    CommandLine := Format('x -ibck -y "%s" "%s\"', [ArchivePath, DestDir]);
    Log(Format('Running command: "%s" %s', [WinRARPath, CommandLine]));

    if not Exec(WinRARPath, CommandLine, '', SW_HIDE, ewWaitUntilTerminated, ResultCode) then
    begin
      Log(Format('Failed to execute WinRAR for %s, error code: %d', [ArchivePath, ResultCode]));
      Result := False;
      Exit;
    end;

    if ResultCode <> 0 then
    begin
      Log(Format('WinRAR returned error code %d while extracting %s', [ResultCode, ArchivePath]));
      Result := False;
      Exit;
    end
    else
    begin
      Log(Format('Extraction successful for %s', [ArchivePath]));
    end;
  end

  else
  begin
    Log('No extraction program found!');
    MsgBox('No extraction program found! Please ensure 7-Zip or WinRAR is installed.', mbError, MB_OK);
    Exit;
  end;

  if ResultCode <> 0 then
  begin
    Log(Format('Extraction returned error code %d while extracting %s', [ResultCode, ArchivePath]));
    Exit;
  end;

  Log('Extraction successful for ' + ArchivePath);
  Result := True;
end;

procedure StoreReleaseInfo(Repo, Resolution, ReleaseJSON, AssetsJSON: string);
// Adds info from called http into storage 
var
  StoreKey: string;
begin
  StoreKey := Repo + '_' + Resolution;
  ReleaseStore.Add(StoreKey + '=' + ReleaseJSON);
  AssetsStore.Add(StoreKey + '=' + AssetsJSON);
end;

function GetStoredJSONForRepo(Repo, Resolution: string; var ReleaseJSON, AssetsJSON: string): Boolean;
// Retrieves info from storage when needed based on body and resolution 
var
  StoreKey, Line: string;
  i: Integer;
begin
  StoreKey := Repo + '_' + Resolution;
  ReleaseJSON := '';
  AssetsJSON := '';
  Result := False;

  // Loop through ReleaseStore to find the StoreKey
  for i := 0 to ReleaseStore.Count - 1 do
  begin
    Line := ReleaseStore[i];
    if Pos(StoreKey + '=', Line) = 1 then
    begin
      ReleaseJSON := Copy(Line, Length(StoreKey) + 2, MaxInt);
      Break;
    end;
  end;

  // Loop through AssetsStore to find the StoreKey
  for i := 0 to AssetsStore.Count - 1 do
  begin
    Line := AssetsStore[i];
    if Pos(StoreKey + '=', Line) = 1 then
    begin
      AssetsJSON := Copy(Line, Length(StoreKey) + 2, MaxInt);
      Break;
    end;
  end;

  Result := (ReleaseJSON <> '') and (AssetsJSON <> '');
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
// Checks GitHub's api rate limit
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
// Main call to GitHub API
var
  HttpCli: Variant;
  CombinedResponse: string;
begin
  if GetStoredJSONForRepo(Repo, '', LatestReleaseJSON, LatestReleaseAssetsJSON) then
  begin
    Log('Using Stored release info for ' + Repo);
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

      // Validate and log the response length
      Log('Response received. Length: ' + IntToStr(Length(CombinedResponse)));
      if CombinedResponse = '' then
      begin
        Log('Empty response for latest release info');
        Exit;
      end;

      // Store the full JSON response
      LatestReleaseJSON := CombinedResponse;
      //Log('Full JSON Response: ' + LatestReleaseJSON);

      // Extract the tag name (version) from the response
      LatestReleaseVersion := ExtractJSONField(CombinedResponse, '"tag_name":"');
      if LatestReleaseVersion = '' then
        Log('Warning: Could not extract "tag_name" from JSON.');

      // Extract the assets array from the response
      LatestReleaseAssetsJSON := ExtractJSONArray(CombinedResponse, '"assets":[');
      if LatestReleaseAssetsJSON = '' then
        Log('Warning: Could not extract "assets" array from JSON.');

      // Store the combined JSON responses for future use
      StoreReleaseInfo(Repo, '', LatestReleaseJSON, LatestReleaseAssetsJSON);
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
  
  try
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
              end
              else
                Log('Failed to find closing quote for browser download URL');
            end
            else
              Log('Failed to find browser download URL for asset: ' + AssetName);
          end;
          StartPos := J + 1;
        end
        else
          Log('Failed to find closing quote for asset name');
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
    end
    else
      Log('No assets found for resolution: ' + Resolution);

  finally
    AssetURLs.Free;
  end;
end;

function LatestReleaseHasFiles(Repo: string): Boolean;
// Checks Store to see if release has assets
var
  ReleaseJSON, AssetsJSON: string;
begin
  Log('Checking if latest release has files for repository: ' + Repo);
  Result := False;
  
  if GetStoredJSONForRepo(Repo, '', ReleaseJSON, AssetsJSON) then
  begin
    LatestReleaseAssetsJSON := AssetsJSON;
    Log('Using Stored release info for ' + Repo);
    
    if Pos('"assets":[', AssetsJSON) > 0 then
    begin
      Result := True;
      Log('Release has associated files.');
    end
    else
    begin
      Log('No assets found in Stored release info.');
    end;
  end
  else
  begin
    Log('Stored data not found for repository: ' + Repo);
  end;
end;

function ExtractResolution(AssetName: String): String;
// Extracts resolution information from asset names.
var
  UnderscorePos, DashPos, DotPos, DelimiterPos, NoModelsPos: Integer;
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
		
  // Handle NoModels case
  NoModelsPos := Pos('NoModels', AssetName);
  if NoModelsPos > 0 then
  begin
    // Include the 4 characters before NoModels
    if NoModelsPos > 4 then
      Resolution := Copy(AssetName, NoModelsPos - 4, 4) + ' NoModels'
    else
      Resolution := 'NoModels';
  end;

  // Handle the case with only one asset without resolution number
  if Resolution = '' then
  begin
    // Extract the part after the first underscore or dash
    if (UnderscorePos > 0) or (DashPos > 0) then
    begin
      if UnderscorePos > 0 then
        Resolution := Copy(AssetName, UnderscorePos + 1, DotPos - UnderscorePos - 1)
      else if DashPos > 0 then
        Resolution := Copy(AssetName, DashPos + 1, DotPos - DashPos - 1);
    end
    else
    begin
      // If no underscore or dash, use the entire asset name before the extension
      Resolution := Copy(AssetName, 1, DotPos - 1);
    end;
  end;

  // Return the extracted resolution
  Result := Resolution;
end;

function GetLatestReleaseVersion(Repo: string): string;
//Extracts release information from Stored data
var
  ReleaseJSON, AssetsJSON: string;
  I, J: Integer;
begin
  Log('Getting latest release version for repository: ' + Repo);
  Result := '';
  
  if GetStoredJSONForRepo(BodyRepos[I], '', ReleaseJSON, AssetsJSON) then
  begin
    LatestReleaseJSON := ReleaseJSON;
    Log('Using Stored release info for ' + Repo);
    
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
      Log('No tag_name found in Stored release info.');
    end;
  end
  else
  begin
    Log('Stored data not found for repository: ' + Repo);
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
// Gets all information from a body's repo
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

    if GetStoredJSONForRepo(BodyRepos[I], '', ReleaseJSON, AssetsJSON) then
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
var
  SizeInBytes: Int64;
  SizeStr: string;
begin
  if ComboBoxTag < Length(SizeLabelList) then
  begin
    if Assigned(SizeLabelList[ComboBoxTag]) then
    begin
      SizeInBytes := StrToInt64Def(SizesList[ComboBoxTag][0], 0);
      if SizeInBytes < 1024 then
        SizeStr := Format('%d Bytes', [SizeInBytes])
      else if SizeInBytes < 1048576 then
        SizeStr := Format('%.2f KB', [SizeInBytes / 1024.0])
      else
        SizeStr := Format('%.2f MB', [SizeInBytes / 1048576.0]);
      SizeLabelList[ComboBoxTag].Caption := 'Total Size: ' + SizeStr;
    end;
  end;
end;

function IsNewVersionAvailable: Boolean;
var
  HttpCli: Variant;
  CombinedResponse, FullReleaseName, LatestVersion: string;
  VersionStartPos: Integer;
begin
  Result := False;
	Log('========================================================');
  Log('Checking for new version from ' + AppURL);
  try
    HttpCli := CreateOleObject('WinHttp.WinHttpRequest.5.1');
    HttpCli.Open('GET', 'https://api.github.com/repos/' + AppURL + '/releases/latest', False);
    HttpCli.SetRequestHeader('User-Agent', 'RSS-Reborn-Installer');

    MyAccessToken := ReadGitHubAccessToken;
    if MyAccessToken <> '' then
      HttpCli.SetRequestHeader('Authorization', 'token ' + MyAccessToken);

    HttpCli.Send;

    if HttpCli.Status = 200 then
    begin
      CombinedResponse := HttpCli.ResponseText;
      FullReleaseName := ExtractJSONField(CombinedResponse, '"name":"');

      // Extract the version number from FullReleaseName
      if FullReleaseName <> '' then
      begin
        VersionStartPos := Pos('v', FullReleaseName);
        if VersionStartPos > 0 then
          LatestVersion := Copy(FullReleaseName, VersionStartPos + 1, Length(FullReleaseName) - VersionStartPos);
        
        Log('Fetched latest release version: ' + LatestVersion);
        Log('Current running version: ' + '{#MyAppVersion}');
        Result := CompareStr(LatestVersion, '{#MyAppVersion}') > 0;
        Log('Is new version available: ' + LatestVersion + ' > ' + '{#MyAppVersion}');
        Log('========================================================');
      end
      else
      begin
        Log('Failed to extract latest version from release name: ' + FullReleaseName);
      end;
    end
    else
    begin
      Log('Failed to fetch latest release info, status: ' + IntToStr(HttpCli.Status));
    end;
  except
    Log('Exception occurred while checking for new version: ' + GetExceptionMessage);
  end;
end;

procedure PopulateResolutions(ComboBox: TComboBox; RepoIndex: Integer; var Sizes: TStringList);
// Populates drop down elements in UI with all available resolution packs
var
  I, J, StartPos: Integer;
  AssetName, ReleaseJSON, AssetsJSON, Resolution: string;
  Size, TotalSize: Int64;
  AddedResolutions: TStringList;
  ResolutionIndex: Integer;
  ScaledAvailable: Boolean;
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

    // Use Stored data from RetrieveBodyInfo
    LatestReleaseAssetsJSON := AssetDataList[RepoIndex].Text;
  
    ScaledAvailable := False;
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
          Log('Populate Resolutions found asset: ' + AssetName + ' with resolution: ' + Resolution);

          // Assets with "scale" in the name
          if (Pos('scale', LowerCase(AssetName)) > 0) then
          begin
            Log('Scaled Asset found: ' + AssetName);
            ScaledAvailable := True;
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
								
								// Store the resolution-specific data
								if (BodyRepos[RepoIndex] <> '') and (Resolution <> '') then
								begin
									ReleaseJSON := LatestReleaseJSON;
									AssetsJSON := LatestReleaseAssetsJSON;
									StoredReleaseInfo.Add(BodyRepos[RepoIndex] + ':' + Resolution + '=' + ReleaseJSON + '|' + AssetsJSON);
									Log('Stored resolution-specific data for repo: ' + BodyRepos[RepoIndex] + ' with resolution: ' + Resolution);
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
        end
        else
          Break;
      end
      else
        Break;
    end;
		
		ComboBox.Items.Add('None');
    Sizes.Add('0');

    ComboBox.ItemIndex := 0;
    UpdateSizeLabel(ComboBox.Tag);

    // Show a checkbox if scaled assets are available
    if ScaledAvailable then
    begin
      ScaledCheckboxes[RepoIndex].Visible := True;
      ScaledAssetsAvailable[RepoIndex] := True;
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
          if SizeInBytes < 1024 then
            SizeStr := 'Total Size: ' + Format('%d Bytes', [SizeInBytes])
          else if SizeInBytes < 1048576 then
            SizeStr := 'Total Size: ' + Format('%.2f KB', [SizeInBytes / 1024.0])
          else
            SizeStr := 'Total Size: ' + Format('%.2f MB', [SizeInBytes / 1048576.0]);
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
  Log('Checking RSS confirmation and EVE/Scatterer download confirmation');
  Result := True;
  
  if not RP1Checkbox.Checked then
  begin
    Log('RSS confirmation not checked');
    MsgBox('Please confirm that you have installed and launched Real Solar System at least once, and confirm your GameData is backed up. RSS Reborn will not work if RSS does not work.', mbError, MB_OK);
    Result := False; 
  end
  else
  begin
    Log('RSS confirmation checked');
  end;
end;

procedure RaymarchedVolumetricsCheckboxClick(Sender: TObject);
begin
  FileEdit.Visible := RaymarchedVolumetricsCheckbox.Checked;
  FileBrowseButton.Visible := RaymarchedVolumetricsCheckbox.Checked;
  FileLabel.Visible := RaymarchedVolumetricsCheckbox.Checked;
  HQCloudsCheckbox.Visible := RaymarchedVolumetricsCheckbox.Checked;
  HQCloudsCheckbox.Checked := False;
end;

procedure BrowseForFile(Sender: TObject);
var
  FileName: String;
begin
  FileName := '';
  if GetOpenFileName('Select KSP Executable', FileName, '', 'Zip Files (*.zip)|*.zip|7z Files (*.7z)|*.7z|All Files (*.*)|*.*', 'zip') then
  begin
    FileEdit.Text := FileName;
  end;
end;

// Event handler for CommunitySettings checkbox
procedure CommunitySettingsClick(Sender: TObject);
begin
  AddTUFXCheckbox.Enabled := CommunitySettings.Checked;
  Note1.Enabled := CommunitySettings.Checked;
  Note2.Enabled := CommunitySettings.Checked;
  ReflectionQualityCheckbox.Enabled := CommunitySettings.Checked;
  ReflectionTextureCheckbox.Enabled := CommunitySettings.Checked;
  AAinKSPCheckbox.Enabled := CommunitySettings.Checked;
  HQCloudsCheckbox.Enabled := CommunitySettings.Checked;

  if not CommunitySettings.Checked then
  begin
    AddTUFXCheckbox.Checked := False; 
    ReflectionQualityCheckbox.Checked := False; 
    ReflectionTextureCheckbox.Checked := False; 
    AAinKSPCheckbox.Checked := False; 
    HQCloudsCheckbox.Checked := False; 
    Log('Community Settings turned off - all related checkboxes unchecked');
  end
  else
  begin
    Log('Community Settings turned on');
  end;
end;

procedure WizardFormResize(Sender: TObject);
begin
  FileEdit.Width := WizardForm.ClientWidth - FileBrowseButton.Width - FileEdit.Left - 90; 
  FileBrowseButton.Left := FileEdit.Left + FileEdit.Width + 10;
  LineSeparator.Width := WizardForm.ClientWidth - KSPDirPage.Edits[0].Left * 2;
  CommunitySettings.Width := WizardForm.ClientWidth - KSPDirPage.Edits[0].Left * 2;
end;

procedure InitializeWizard;
// Initialize the UI
var
  I: Integer;
  ComboBox: TComboBox;
  BodyLabel, VersionLabel, SizeLabel: TLabel;
  Page: TWizardPage;
  PageHeight, PageWidth: Integer;
begin
  Log('Initializing wizard');

  InitializeConstants;
  InitializeVariables;
	InitializeArrayLengths;
  InitializeBodyRepos;

  // Read GitHub access token from the registry if it exists
  MyAccessToken := ReadGitHubAccessToken;

  // Check for new version
  if IsNewVersionAvailable then
  begin
    MsgBox('A new version of the installer is available. Please download the latest version from GitHub.', mbInformation, MB_OK);
    WizardForm.Close;
  end;

	//Get information ready for the UI
  RetrieveBodyInfo;

	// Checkbox on welcome page
  RP1Checkbox := TNewCheckBox.Create(WizardForm);
  RP1Checkbox.Parent := WizardForm.WelcomePage;
  RP1Checkbox.Left := ScaleX(175);
  RP1Checkbox.Top := ScaleY(175);
  RP1Checkbox.Width := WizardForm.ClientWidth - ScaleX(36);
  RP1Checkbox.Height := ScaleY(40);
  RP1Checkbox.Caption := 'I have run RSS once and have backed up my GameData.';
  RP1Checkbox.Checked := False;
	Log('========================================================');
  Log('RSS installation confirmation checkbox created');

	// Checkbox on welcome page
  RaymarchedVolumetricsCheckbox := TNewCheckBox.Create(WizardForm);
  RaymarchedVolumetricsCheckbox.Parent := WizardForm.WelcomePage;
  RaymarchedVolumetricsCheckbox.Left := ScaleX(175);
  RaymarchedVolumetricsCheckbox.Top := ScaleY(215);
  RaymarchedVolumetricsCheckbox.Width := WizardForm.ClientWidth - ScaleX(36);
  RaymarchedVolumetricsCheckbox.Height := ScaleY(40);
  RaymarchedVolumetricsCheckbox.Caption := '(Optional) I am using Blackrack''s Volumetric Clouds.';
  RaymarchedVolumetricsCheckbox.Checked := False;
  Log('EVE and Scatterer download confirmation checkbox created');

	// Resolution input page
  Page := CreateCustomPage(wpWelcome, 'Select Resolutions', 'Select the desired resolution for each body');
  wpSelectResolutions := Page.ID;

  // Create the KSP directory input page
  KSPDirPage := CreateInputDirPage(wpWelcome, 'KSP Settings', 'Select the KSP directory, and choose Settings', 'Please select the directory where Kerbal Space Program is installed.', False, '');
  KSPDirPage.Add('KSP Directory');
  KSPDirPage.Values[0] := 'C:\Program Files (x86)\Steam\steamapps\common\Kerbal Space Program';

  // Create the label for the file selection control
  FileLabel := TLabel.Create(WizardForm);
  FileLabel.Parent := KSPDirPage.Surface;
  FileLabel.Top := KSPDirPage.Edits[0].Top + KSPDirPage.Edits[0].Height + 10;
  FileLabel.Left := KSPDirPage.Edits[0].Left;
  FileLabel.Caption := 'Select Raymarched Volumetrics File:';
  FileLabel.Visible := False;

  // Create the file selection edit control
  FileEdit := TEdit.Create(WizardForm);
  FileEdit.Parent := KSPDirPage.Surface;
  FileEdit.Top := FileLabel.Top + FileLabel.Height + 5;
  FileEdit.Left := FileLabel.Left;
  FileEdit.Width := WizardForm.ClientWidth - FileEdit.Left - 20;
  FileEdit.Visible := False;

  // Create the file browse button
  FileBrowseButton := TButton.Create(WizardForm);
  FileBrowseButton.Parent := KSPDirPage.Surface;
  FileBrowseButton.Top := FileEdit.Top;
  FileBrowseButton.Left := FileEdit.Left + FileEdit.Width + 10;
  FileBrowseButton.Caption := 'Browse...';
  FileBrowseButton.OnClick := @BrowseForFile;
  FileBrowseButton.Visible := False;

  FileBrowseButton.OnClick := @BrowseForFile;
  RaymarchedVolumetricsCheckbox.OnClick := @RaymarchedVolumetricsCheckboxClick;
  WizardForm.OnResize := @WizardFormResize;

  // Create the line separator
  LineSeparator := TBevel.Create(WizardForm);
  LineSeparator.Parent := KSPDirPage.Surface;
  LineSeparator.Shape := bsTopLine;
  LineSeparator.Left := KSPDirPage.Edits[0].Left;
  LineSeparator.Top := FileBrowseButton.Top + FileBrowseButton.Height + 10;
  LineSeparator.Width := WizardForm.ClientWidth - KSPDirPage.Edits[0].Left * 2;
  LineSeparator.Height := 2;

  // Create and configure the first checkbox
  CommunitySettings := TCheckBox.Create(WizardForm);
  CommunitySettings.Parent := KSPDirPage.Surface;
  CommunitySettings.Left := KSPDirPage.Edits[0].Left;
  CommunitySettings.Top := LineSeparator.Top + LineSeparator.Height + 10;
  CommunitySettings.Width := WizardForm.ClientWidth - KSPDirPage.Edits[0].Left * 2;
  CommunitySettings.Caption := 'Enable Recommended Community Visual Settings';
  CommunitySettings.Checked := False; 
  CommunitySettings.OnClick := @CommunitySettingsClick;

  AddTUFXCheckbox := TCheckBox.Create(WizardForm);
  AddTUFXCheckbox.Parent := KSPDirPage.Surface;
  AddTUFXCheckbox.Left := CommunitySettings.Left + 25;
  AddTUFXCheckbox.Top := CommunitySettings.Top + CommunitySettings.Height + 10;
  AddTUFXCheckbox.Width := CommunitySettings.Width;
  AddTUFXCheckbox.Caption := 'Add TUFX to installation';
  AddTUFXCheckbox.Enabled := False; 

  Note1 := TLabel.Create(WizardForm);
  Note1.Parent := KSPDirPage.Surface;
  Note1.Left := AddTUFXCheckbox.Left + 20;
  Note1.Top := AddTUFXCheckbox.Top + AddTUFXCheckbox.Height + 5;
  Note1.Caption := '    - Needed for Blackrack''s profile and (easy) anti aliasing.';
  Note1.AutoSize := True;
  Note1.Enabled := False;

  Note2 := TLabel.Create(WizardForm);
  Note2.Parent := KSPDirPage.Surface;
  Note2.Left := AddTUFXCheckbox.Left + 20;
  Note2.Top := Note1.Top + Note1.Height + 5;
  Note2.Caption := '    - SMAA enabled.';
  Note2.AutoSize := True;
  Note2.Enabled := False;

  ReflectionQualityCheckbox := TCheckBox.Create(WizardForm);
  ReflectionQualityCheckbox.Parent := KSPDirPage.Surface;
  ReflectionQualityCheckbox.Left := AddTUFXCheckbox.Left;
  ReflectionQualityCheckbox.Top := Note2.Top + Note2.Height + 10;
  ReflectionQualityCheckbox.Width := AddTUFXCheckbox.Width;
  ReflectionQualityCheckbox.Caption := 'Reflection Quality -> Low';
  ReflectionQualityCheckbox.Enabled := False;

  ReflectionTextureCheckbox := TCheckBox.Create(WizardForm);
  ReflectionTextureCheckbox.Parent := KSPDirPage.Surface;
  ReflectionTextureCheckbox.Left := ReflectionQualityCheckbox.Left;
  ReflectionTextureCheckbox.Top := ReflectionQualityCheckbox.Top + ReflectionQualityCheckbox.Height + 10;
  ReflectionTextureCheckbox.Width := ReflectionQualityCheckbox.Width;
  ReflectionTextureCheckbox.Caption := 'Reflection Texture -> 128';
  ReflectionTextureCheckbox.Enabled := False;

  AAinKSPCheckbox := TCheckBox.Create(WizardForm);
  AAinKSPCheckbox.Parent := KSPDirPage.Surface;
  AAinKSPCheckbox.Left := ReflectionTextureCheckbox.Left;
  AAinKSPCheckbox.Top := ReflectionTextureCheckbox.Top + ReflectionTextureCheckbox.Height + 10;
  AAinKSPCheckbox.Width := ReflectionTextureCheckbox.Width;
  AAinKSPCheckbox.Caption := 'AA in KSP -> off';
  AAinKSPCheckbox.Enabled := False; 

  HQCloudsCheckbox := TCheckBox.Create(WizardForm);
  HQCloudsCheckbox.Parent := KSPDirPage.Surface;
  HQCloudsCheckbox.Left := AAinKSPCheckbox.Left;
  HQCloudsCheckbox.Top := AAinKSPCheckbox.Top + AAinKSPCheckbox.Height + 10;
  HQCloudsCheckbox.Width := AAinKSPCheckbox.Width;
  HQCloudsCheckbox.Caption := 'Remove HQ Volumetric Clouds Config (if present)';
  HQCloudsCheckbox.Enabled := False; 
  HQCloudsCheckbox.Visible := RaymarchedVolumetricsCheckbox.Checked;
	
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
	
  // Wizard Window Dimensions
  WizardForm.ClientHeight := WizardForm.ClientHeight + ScaleY(25);
  WizardForm.ClientWidth := WizardForm.ClientWidth + ScaleX(15);

  SetLength(ResolutionCombos, Length(BodyRepos));
  SetLength(SizesList, Length(BodyRepos));
  SetLength(SizeLabelList, Length(BodyRepos));
  SetLength(ScaledCheckboxes, Length(BodyRepos));
  SetLength(ScaledAssetsAvailable, Length(BodyRepos));

  PageHeight := 0;
  PageWidth := 0;

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
    
    // Scaled Asset Checkbox
    ScaledCheckboxes[I] := TNewCheckBox.Create(Page);
    ScaledCheckboxes[I].Parent := Page.Surface;
    ScaledCheckboxes[I].Left := ScaleX(220);
    ScaledCheckboxes[I].Top := ScaleY(PageHeight);
    ScaledCheckboxes[I].Caption := 'Include Scaled';
    ScaledCheckboxes[I].Visible := False;

    SizesList[I] := TStringList.Create;
    PopulateResolutions(ComboBox, I, SizesList[I]);

    Log('Dropdown for ' + BodyRepos[I] + ' created');

		// Texture Version No.
    VersionLabel := TLabel.Create(Page);
    VersionLabel.Parent := Page.Surface;
    VersionLabel.Left := ScaleX(325);
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
    UpdateSizeLabel(I);

    Log('Size label for ' + BodyRepos[I] + ' initialized: ' + SizeLabel.Caption);

    PageHeight := PageHeight + 25;
    PageWidth := PageWidth + 200;
  end;

  WizardForm.Repaint;
  Page.Surface.Repaint;

  Log('Wizard initialization completed');
	Log('========================================================');
end;

procedure SetKSPDir;
var
  DriveLetter: String;
begin
  KSP_DIR := KSPDirPage.Values[0];
  if KSP_DIR = '' then
  begin
    Log('Error: KSP directory is empty.');
    MsgBox('Failed to set the KSP directory. Please select a valid directory.', mbError, MB_OK);
    WizardForm.Close;
    Exit;
  end;

  // Extract the drive letter from the KSP_DIR
  DriveLetter := Copy(KSP_DIR, 1, 2); 

  // Check for available disk space (50 GB required)
  if not IsEnoughDiskSpaceAvailable(DriveLetter) then
  begin
    Log('Warning: Not enough disk space available on drive ' + DriveLetter + '.');
    MsgBox('Warning: Not enough disk space on the selected drive (' + DriveLetter + '). You need at least 50GB of free space.', mbError, MB_OK);
    KSPDirPage.Values[0] := ''; 
    Exit;
  end;

  Log('KSP directory set to: ' + KSP_DIR);

// Handle the optional directory if the checkbox is checked
  if RaymarchedVolumetricsCheckbox.Checked then
  begin
    RAYVOL_DIR := FileEdit.Text;
    Log('Raymarched Volumetrics File set to: ' + RAYVOL_DIR);
  end;
end;

procedure UpdateConfigFile;
var
  ConfigFilePath: string;
  Lines: TStringList;
  i: Integer;
  HQCloudsFilePath: string;
begin
  // Define the path to the settings.cfg file in the KSP directory
  ConfigFilePath := KSP_DIR + '\settings.cfg';
  HQCloudsFilePath := KSP_DIR + '\GameData\HQ_volumetricClouds.cfg';

  // Log the attempt to update the settings.cfg file
  Log('Attempting to update settings.cfg at: ' + ConfigFilePath);

  // Check if the file exists
  if FileExists(ConfigFilePath) then
  begin
    // Create a TStringList to load and modify the file
    Lines := TStringList.Create;
    try
      // Load the file into the TStringList
      Lines.LoadFromFile(ConfigFilePath);
      Log('Loaded settings.cfg successfully.');

      // Iterate through each line to find the target settings and update them
      for i := 0 to Lines.Count - 1 do
      begin
        if ReflectionQualityCheckbox.Checked and (Pos('REFLECTION_PROBE_REFRESH_MODE', Lines[i]) = 1) then
        begin
          Lines[i] := 'REFLECTION_PROBE_REFRESH_MODE = 1';
          Log('Updated REFLECTION_PROBE_REFRESH_MODE to 1');
        end;

        if ReflectionTextureCheckbox.Checked and (Pos('REFLECTION_PROBE_TEXTURE_RESOLUTION', Lines[i]) = 1) then
        begin
          Lines[i] := 'REFLECTION_PROBE_TEXTURE_RESOLUTION = 0';
          Log('Updated REFLECTION_PROBE_TEXTURE_RESOLUTION to 0');
        end;

        if AAinKSPCheckbox.Checked and (Pos('ANTI_ALIASING', Lines[i]) = 1) then
        begin
          Lines[i] := 'ANTI_ALIASING = 0';
          Log('Updated ANTI_ALIASING to 0');
        end;
      end;

      // Save the updated lines back to the file
      Lines.SaveToFile(ConfigFilePath);
      Log('Saved changes to settings.cfg successfully.');
    finally
      Lines.Free;  // Free the TStringList after use
      Log('Freed memory allocated for TStringList.');
    end;
  end
  else
  begin
    // Handle the case where the file does not exist
    Log('The settings.cfg file was not found at: ' + ConfigFilePath);
    MsgBox('The default KSP settings.cfg file was not found! Your game may not run.', mbError, MB_OK);
  end;

  // Delete HQ_volumetricClouds.cfg if the HQCloudsCheckbox is checked
  if HQCloudsCheckbox.Checked then
  begin
    if FileExists(HQCloudsFilePath) then
    begin
      if DeleteFile(HQCloudsFilePath) then
      begin
        Log('Deleted HQ_volumetricClouds.cfg successfully.');
      end
      else
      begin
        Log('Failed to delete HQ_volumetricClouds.cfg.');
      end;
    end
    else
    begin
      Log('HQ_volumetricClouds.cfg was not found at: ' + HQCloudsFilePath);
    end;
  end;
end;

procedure ClearDownloadDirectory;
// Frees up space and prevents conflicts
begin;
	if DirectoryExists(DownloadsDir) then
    if not DelTree(DownloadsDir, True, True, True) then
      Log('Failed to delete Downloads directory:' + DownloadsDir)
    else
      Log('DownloadsDir directory deleted:' + DownloadsDir)
  else
    Log('DownloadsDir directory does not exist:' + DownloadsDir);
end;

function ExtractJSONString(const JSON: string; var CurrentPos: Integer): string;
var
  StartPos, EndPos: Integer;
begin
  Result := '';
  // Move CurrentPos to the start of the string (skip any whitespace or quotes)
  while (CurrentPos <= Length(JSON)) and (JSON[CurrentPos] in [' ', '"']) do
    Inc(CurrentPos);
  
  StartPos := CurrentPos;
  
  // Find the end of the string
  while (CurrentPos <= Length(JSON)) and (JSON[CurrentPos] <> '"') do
    Inc(CurrentPos);
  
  EndPos := CurrentPos;
  
  // Extract the string
  if EndPos > StartPos then
    Result := Copy(JSON, StartPos, EndPos - StartPos);
  
  Inc(CurrentPos);  // Move past the closing quote
end;

function GetRepoDownloadURLs(Repo, Resolution: String; IsScaled: Boolean): TStringList;
var
  JSONData, Key, AssetName, BrowserDownloadURL: String;
  IsScaledStr: String;
  AssetSectionStart, NameStart, NameEnd, UrlStart, UrlEnd: Integer;
begin
  Result := TStringList.Create;
  
  // Convert IsScaled boolean to string for logging
  if IsScaled then
    IsScaledStr := 'True'
  else
    IsScaledStr := 'False';

  // Retrieve stored JSON for the repo if available, otherwise fetch from the web
  if GetStoredJSONForRepo(Repo, Resolution, JSONData, Key) then
  begin
    Log('Using stored release info for repository: ' + Repo);
  end
  else
  begin
    Log('No stored data found for repository: ' + Repo + '. Fetching latest release info from the web.');
    GetLatestReleaseHTTPInfo(Repo);
    Log('Fetched latest release info. Storing data...');
    StoreReleaseInfo(Repo, Resolution, JSONData, Key);
  end; 

  // Start JSON Parsing
  Log('Starting JSON parsing.');

  // Find the start of the assets section
  AssetSectionStart := Pos('"assets":[', JSONData);
  if AssetSectionStart = 0 then
  begin
    Log('Error: "assets" section not found in the JSON.');
    Exit;
  end;
  
  // Process each asset in the assets section
  while True do
  begin
    // Find the start of the next asset object
    NameStart := PosEx('"name":"', JSONData, AssetSectionStart);
    if NameStart = 0 then
    begin
      Log('No more assets found in the JSON.');
      Break;
    end;
    
    NameStart := NameStart + Length('"name":"');
    NameEnd := PosEx('"', JSONData, NameStart);
    AssetName := Copy(JSONData, NameStart, NameEnd - NameStart);
    Log('Found asset: ' + AssetName);

    // Check if the asset matches the criteria
    if (Resolution = '') or (ExtractResolution(AssetName) = Resolution) or 
       (IsScaled and (Pos('scaled', LowerCase(AssetName)) > 0)) then
    begin
      //Log('Asset matches criteria (Resolution: ' + Resolution + ', IsScaled: ' + IsScaledStr + ').');

      // Look for the "browser_download_url"
      UrlStart := PosEx('"browser_download_url":"', JSONData, NameEnd);
      if UrlStart > 0 then
      begin
        UrlStart := UrlStart + Length('"browser_download_url":"');
        UrlEnd := PosEx('"', JSONData, UrlStart);
        BrowserDownloadURL := Copy(JSONData, UrlStart, UrlEnd - UrlStart);

        Log('Valid download URL found: ' + BrowserDownloadURL);
        Result.Add(BrowserDownloadURL);
      end
      else
      begin
        Log('Error: "browser_download_url" not found for asset: ' + AssetName);
      end;
    end
    else
    begin
      Log('Asset does not match criteria and will be skipped.');
    end;
    
    // Move to the next asset
    AssetSectionStart := NameEnd;
  end;

  if Result.Count = 0 then
  begin
    Log('No valid URLs were found or matched the criteria.');
  end
  else
  begin
    Log('Total URLs found: ' + IntToStr(Result.Count));
  end;
end;

procedure AddToDownloadList(Repo, Resolution, DestFilePath: string; IncludeScaled: Boolean);
var
  DownloadURLs: TStringList;
  I, J: Integer;
  URLExists: Boolean;
  AssetName: String;
begin
  Log('========================================================');
  Log('AddToDownloadList called for Repo: ' + Repo + ' with Resolution: ' + Resolution);

  if not GetStoredJSONForRepo(Repo, Resolution, LatestReleaseJSON, LatestReleaseAssetsJSON) then
  begin
    GetLatestReleaseHTTPInfo(Repo);
    StoreReleaseInfo(Repo, Resolution, LatestReleaseJSON, LatestReleaseAssetsJSON);
  end;

  DownloadURLs := GetRepoDownloadURLs(Repo, Resolution, IncludeScaled);
  
  try
    for I := 0 to DownloadURLs.Count - 1 do
    begin
      URLExists := False;
      AssetName := CustomExtractFileName(DownloadURLs[I]);

      // Skip URLs containing "parallax_stocktextures"
      if Pos('parallax_stocktextures', LowerCase(DownloadURLs[I])) > 0 then
      begin
        Log('Skipped URL containing parallax_stocktextures: ' + DownloadURLs[I]);
        Continue;
      end;

      // Ensure only RSS_Configs.7z is added for RSS-Configs repository
      if (Repo = 'RSS-Reborn/RSS-Configs') and (Pos('OldBiomes', AssetName) > 0) then
      begin
        Log('Skipping asset: ' + AssetName);
        Continue;
      end;

      for J := 0 to DownloadList.Count - 1 do
      begin
        if Pos(DownloadURLs[I], DownloadList[J]) > 0 then
        begin
          URLExists := True;
          Break;
        end;
      end;

      if not URLExists then
      begin
        Log('Adding download URL to list: ' + DownloadURLs[I]);
        DownloadList.Add(DownloadURLs[I] + '=' + DestFilePath);
      end
      else
      begin
        Log('Duplicate URL found and skipped: ' + DownloadURLs[I]);
      end;
    end;

  finally
    DownloadURLs.Free;
  end;
end;

procedure InitializeDownloadList;
// Adds non-body repos to download list
var
  Resolution, ParallaxURL: string;
  I: Integer;
  IncludeScaled: Boolean;
begin
  Log('InitializeDownloadList called');
  
  // RSS-Configs
  AddToDownloadList('RSS-Reborn/RSS-Configs', '', (DownloadsDir + '\RSS_Configs.7z'), False);

  // RSS-Terrain
  AddToDownloadList('RSS-Reborn/RSS-Terrain', '', (DownloadsDir + '\RSS_Terrain.7z'), False);

  // Kopernicus
  AddToDownloadList('ballisticfox/Kopernicus', '', (DownloadsDir + '\Kopernicus.7z'), False);

  // Planetary textures at user-selected resolutions
  for I := 0 to High(BodyRepos) do
  begin
    if (ResolutionCombos[I] <> nil) and (ResolutionCombos[I].ItemIndex >= 0) then
    begin
      Resolution := ResolutionCombos[I].Text;
      IncludeScaled := ScaledCheckboxes[I].Checked; // Check if scaled textures are to be added
      AddToDownloadList(BodyRepos[I], Resolution, (DownloadsDir + '\' + ExtractBodyName(BodyRepos[I]) + '_' + Resolution + '.7z'), IncludeScaled);
    end
    else
    begin
      Log('ResolutionCombos[' + IntToStr(I) + '] is not initialized or has no selected item.');
    end;
  end;

  // RSSVE-Configs (if using blackrack's)
  if RaymarchedVolumetricsCheckbox.Checked then
  begin
  AddToDownloadList('RSS-Reborn/RSSVE-Configs', '', (DownloadsDir + '\RSSVE_Configs.7z'), False);
  end
  else
  begin
    Log('Skipping RSSVE Configs');
  end;

  // RSSVE-Textures (if using blackrack's)
  if RaymarchedVolumetricsCheckbox.Checked then
  begin
  AddToDownloadList('RSS-Reborn/RSSVE-Textures', '', (DownloadsDir + '\RSSVE_Textures.7z'), False);
  end
  else
  begin
    Log('Skipping RSSVE Textures');
  end;

  // Scatterer (if not using Blackrack's)
  if not RaymarchedVolumetricsCheckbox.Checked then
  begin
    AddToDownloadList('LGhassen/Scatterer', '', (DownloadsDir + '\Scatterer.zip'), False);
  end
  else
  begin
    Log('Skipping, using Blackrack''s Volumetrics');
  end;

  // EVE (if not using Blackrack's)
  if not RaymarchedVolumetricsCheckbox.Checked then
  begin
    AddToDownloadList('LGhassen/EnvironmentalVisualEnhancements', '', (DownloadsDir + '\EVE.zip'), False);
  end
  else
  begin
    Log('Skipping, using Blackrack''s Volumetrics');
  end;

  // TUFX
  if AddTUFXCheckbox.Checked then
  begin
    AddToDownloadList('KSPModStewards/TUFX', '', (DownloadsDir + '\TUFX.zip'), False);
    Log('TUFX will be downloaded and installed.');
  end
  else
  begin
    Log('Skipping TUFX Option as it is not selected.');
  end;

  // Download Parallax
  ParallaxVersion := GetLatestReleaseVersion('Gameslinx/Tessellation');
  ParallaxURL := 'https://github.com/Gameslinx/Tessellation/releases/download/' + ParallaxVersion + '/Parallax-' + ParallaxVersion + '.zip';
  AddToDownloadList('Gameslinx/Tessellation', '', (DownloadsDir + '\Parallax-' + ParallaxVersion + '.zip'), False);
end;

procedure CopyFileAndDelete(const SourceFile, DestFile: string);
// Simple proc to move files
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

procedure DeleteDirectory(const DirPath: string);
// Simple proc to delete a directory using command line
var
  Command: string;
  ResultCode: Integer;
begin
  Command := Format('cmd.exe /C rd /s /q "%s"', [DirPath]);
  Log('Executing delete command: ' + Command);
  if Exec(Command, '', '', SW_HIDE, ewWaitUntilTerminated, ResultCode) then
  begin
    if ResultCode = 0 then
      Log('Successfully deleted directory: ' + DirPath)
    else
      Log('Failed to delete directory: ' + DirPath + ' with error code: ' + IntToStr(ResultCode));
  end
  else
    Log('Failed to execute delete command for directory: ' + DirPath);
end;

procedure MoveDirectory(const SourceDir, DestDir: string);
var
  ResultCode: Integer;
  MoveCommand, ReverseCommand: string;
begin
  // Ensure the destination directory exists
  if not DirectoryExists(DestDir) then
  begin
    if not CreateDir(DestDir) then
    begin
      Log('Failed to create destination directory: ' + DestDir);
      Exit;
    end;
  end;

  // Use robocopy to move the directory and its contents
  MoveCommand := Format('robocopy "%s" "%s" /MOVE /E /MT:16 /R:2 /W:5 /COPY:DAT', [SourceDir, DestDir]);
  ReverseCommand := Format('robocopy "%s" "%s" /MOVE /E /MT:16 /R:2 /W:5 /COPY:DAT', [DestDir, SourceDir]);

  // Check if SourceDir is empty 
  if SourceDir = '' then
  begin
    Log('Error: SourceDir is empty. Aborting installation.');
    MsgBox('Error: SourceDir is not set. Aborting installation.', mbError, MB_OK);
    Abort; 
  end;

  // Check if DestDir is empty 
  if DestDir = '' then
  begin
    Log('Error: DestDir is empty. Aborting installation.');
    MsgBox('Error: DestDir is not set. Aborting installation.', mbError, MB_OK);
    Abort; 
  end;

  if Exec('cmd.exe', '/C ' + MoveCommand, '', SW_HIDE, ewWaitUntilTerminated, ResultCode) then
  begin
    if ResultCode < 8 then
    begin
      Log('Successfully moved directory: ' + SourceDir + ' to ' + DestDir);
      LogRobocopyCommand(MoveCommand, ReverseCommand);
    end
    else
    begin
      Log('Failed to move directory: ' + SourceDir + ' to ' + DestDir + ' with error code: ' + IntToStr(ResultCode));
      MsgBox('Failed to move directory: ' + SourceDir + ' to ' + DestDir + ' with error code: ' + IntToStr(ResultCode), mbError, MB_OK);
    end;
  end
  else
  begin
    Log('Failed to execute move command for directory: ' + SourceDir + ' to ' + DestDir);
    MsgBox('Failed to execute move command for directory: ' + SourceDir + ' to ' + DestDir, mbError, MB_OK);
  end;
end;

procedure MoveRaymarchedVolumetrics;
// Moves only the specific directories from RaymarchedVolumetrics to GameDataMerged
var
  SourceDir, DestDir: string;
begin
  SourceDir := (DownloadsDir + '\RaymarchedVolumetrics\GameData');
  DestDir := (DownloadsDir + '\GameDataMerged\GameData');

  // Ensure the destination directory exists
  if not DirExists(DestDir) then
  begin
    Log('GameData directory does not exist in GameDataMerged. Creating: ' + DestDir);
    if not CreateDir(DestDir) then
    begin
      Log('Failed to create GameData directory in GameDataMerged: ' + DestDir);
      MsgBox('Failed to create GameData directory in GameDataMerged: ' + DestDir, mbError, MB_OK);
      RaiseException('Failed to create GameData directory in GameDataMerged: ' + DestDir);
      Exit;
    end
    else
    begin
      Log('Successfully created GameData directory in GameDataMerged: ' + DestDir);
    end;
  end;

  // Move EnvironmentalVisualEnhancements directory
  if DirExists(SourceDir + '\EnvironmentalVisualEnhancements') then
  begin
    Log('Moving EnvironmentalVisualEnhancements from ' + SourceDir + ' to ' + DestDir);
    MoveDirectory(SourceDir + '\EnvironmentalVisualEnhancements', DestDir + '\EnvironmentalVisualEnhancements');
  end;

  // Move Scatterer directory
  if DirExists(SourceDir + '\Scatterer') then
  begin
    Log('Moving Scatterer from ' + SourceDir + ' to ' + DestDir);
    MoveDirectory(SourceDir + '\Scatterer', DestDir + '\Scatterer');
  end;

  Log('RaymarchedVolumetrics specific directories moved successfully.');
end;

procedure MoveParallaxDirectories;
var
  SourceDir, DestDir: string;
begin
  Log('Moving Parallax and Parallax_StockTextures to GameData directory.');
  
  // Define the source and destination directories
  SourceDir := (DownloadsDir + '\GameDataMerged');
  DestDir := SourceDir + '\GameData';

  // Move Parallax directory
  if DirExists(SourceDir + '\Parallax') then
  begin
    Log('Moving directory: ' + SourceDir + '\Parallax to ' + DestDir);
    MoveDirectory(SourceDir + '\Parallax', DestDir + '\Parallax');
  end;

  // Move Parallax_StockTextures directory
  if DirExists(SourceDir + '\Parallax_StockTextures') then
  begin
    Log('Moving directory: ' + SourceDir + '\Parallax_StockTextures to ' + DestDir);
    MoveDirectory(SourceDir + '\Parallax_StockTextures', DestDir + '\Parallax_StockTextures');
  end;
  
  Log('Successfully moved Parallax and Parallax_StockTextures directories.');
end;

procedure MoveScattererGameData;
var
  DestDir, ScattererSourceDir, FindRecName: string;
  FindRec: TFindRec;
begin
  Log('Moving Scatterer to GameData directory.');
  
  // Define the source and destination directories
  DestDir := DownloadsDir + '\GameDataMerged\GameData';

  // Ensure the destination directory exists
  if not DirExists(DestDir) then
  begin
    if not CreateDir(DestDir) then
    begin
      Log('Failed to create destination directory: ' + DestDir);
      Exit;
    end;
  end;

  // Search for directories starting with "scatterer" in DownloadsDir
  if FindFirst(DownloadsDir + '\Scatterer*', FindRec) then
  begin
    try
      repeat
        FindRecName := FindRec.Name;
        ScattererSourceDir := DownloadsDir + '\' + FindRecName + '\GameData\Scatterer';
        if DirExists(ScattererSourceDir) then
        begin
          Log('Moving directory: ' + ScattererSourceDir + ' to ' + DestDir + '\Scatterer');
          MoveDirectory(ScattererSourceDir, DestDir + '\Scatterer');
        end
        else
        begin
          Log('Scatterer directory not found in ' + ScattererSourceDir);
        end;
      until not FindNext(FindRec);
    finally
      FindClose(FindRec);
    end;
  end
  else
  begin
    Log('No scatterer directories found in ' + DownloadsDir);
  end;

  Log('Successfully moved Scatterer directories.');
end;

function CheckForGameDataMerged: Boolean;
var
  FindRec: TFindRec;
  DownloadsDirMerged: string;
begin
  Result := False;
  DownloadsDirMerged := KSP_DIR + '\RSSRebornDownloads';

  // Ensure DownloadsDirMerged exists
  if not DirExists(DownloadsDirMerged) then
  begin
    Log('Error: DownloadsDirMerged does not exist.');
    MsgBox('Error: DownloadsDirMerged does not exist. Aborting installation.', mbError, MB_OK);
    Abort;
    Exit;
  end;

  // List the contents of DownloadsDirMerged
  if FindFirst(DownloadsDirMerged + '\*', FindRec) then
  begin
    try
      repeat
        Log('Found: ' + FindRec.Name);
        if (FindRec.Attributes and FILE_ATTRIBUTE_DIRECTORY <> 0) and (FindRec.Name = 'GameDataMerged') then
        begin
          Result := True;
          Break;
        end;
      until not FindNext(FindRec);
    finally
      FindClose(FindRec);
    end;
  end;

  if not Result then
  begin
    Log('Error: GameDataMerged folder not found in ' + DownloadsDirMerged);
    MsgBox('Error: GameDataMerged folder not found. Aborting installation.', mbError, MB_OK);
    Abort;
  end;
end;

function CheckForKopernicus: Boolean;
var
  FindRec: TFindRec;
  DownloadsDirMerged: string;
begin
  Result := False;
  DownloadsDirMerged := KSP_DIR + '\RSSRebornDownloads';

  // Ensure DownloadsDirMerged exists
  if not DirExists(DownloadsDirMerged) then
  begin
    Log('Error: DownloadsDirMerged does not exist.');
    MsgBox('Error: DownloadsDirMerged does not exist. Aborting installation.', mbError, MB_OK);
    Abort;
    Exit;
  end;

  // List the contents of DownloadsDirMerged
  if FindFirst(DownloadsDirMerged + '\*', FindRec) then
  begin
    try
      repeat
        //Log('Found: ' + FindRec.Name);
        if (FindRec.Attributes and FILE_ATTRIBUTE_DIRECTORY <> 0) and (Pos('Kopernicus', FindRec.Name) = 1) then
        begin
          Result := True;
          Break;
        end;
      until not FindNext(FindRec);
    finally
      FindClose(FindRec);
    end;
  end;

  if not Result then
  begin
    Log('Error: Kopernicus folder not found in ' + DownloadsDirMerged);
    MsgBox('Error: Kopernicus folder not found. Aborting installation.', mbError, MB_OK);
    Abort;
  end;
end;

procedure MergeGameDataFolders;
// Takes contents from downloads and puts them together before moving to KSP directory
var
  DownloadsDirMerged, GameDataMerged, SourceDir, GameDataDir: string;
  FindRec: TFindRec;
  ProgressCounter: Integer;
begin
  // Define the source and destination directories
  GameDataMerged := KSP_DIR + '\RSSRebornDownloads\GameDataMerged';
  DownloadsDirMerged := KSP_DIR + '\RSSRebornDownloads';
	
  // Ensure the GameDataMerged directory exists
  if not DirExists(GameDataMerged) then
  begin
    Log('GameDataMerged directory does not exist. Creating: ' + GameDataMerged);
    if not CreateDir(GameDataMerged) then
    begin
      Log('Failed to create GameDataMerged directory: ' + GameDataMerged);
      MsgBox('Failed to create GameDataMerged directory: ' + GameDataMerged, mbError, MB_OK);
      RaiseException('Failed to create GameDataMerged directory: ' + GameDataMerged);
      Abort;
      Exit; 
    end
    else
    begin
      Log('Successfully created GameDataMerged directory: ' + GameDataMerged);
    end;
  end;

  ProgressCounter := 0;
  WizardForm.Update;

  Log('Downloads directory is: ' + DownloadsDir);
  Log('Downloads directory merged initialized: ' + DownloadsDirMerged);

 // Check if DownloadsDir is empty
  if DownloadsDir = '' then
  begin
    Log('Error: DownloadsDir is empty. Aborting installation.');
    MsgBox('Error: Downloads directory is not set. Aborting installation.', mbError, MB_OK);
    Abort; 
  end;

   // Check if DownloadsDirMerged is empty
  if DownloadsDirMerged = '' then
  begin
    Log('Error: DownloadsDirMerged is empty. Aborting installation.');
    MsgBox('Error: Downloads directory is not set. Aborting installation.', mbError, MB_OK);
    Abort; 
  end;

  if not CheckForGameDataMerged then
      Abort;

  if not CheckForKopernicus then
      Abort;

   // Check if KSP_DIR is empty
  if KSP_DIR = '' then
  begin
    Log('Error: KSP_DIR is empty. Aborting installation.');
    MsgBox('Error: KSP directory is not set. Aborting installation.', mbError, MB_OK);
    Abort; 
  end;

  if FindFirst(DownloadsDirMerged + '\*', FindRec) then
  begin
    try
      repeat
        SourceDir := DownloadsDirMerged + '\' + FindRec.Name;
        GameDataDir := SourceDir;

        WizardForm.Update;
        CurrentFileLabelM.Caption := 'Merging: ' + GameDataDir;
        WizardForm.Update;

        // Skip if it's not a directory, if it's the GameDataMerged directory itself,
        // if it's RaymarchedVolumetrics, or if the directory name starts with "scatterer"
        if ((FindRec.Attributes and FILE_ATTRIBUTE_DIRECTORY) <> 0) and
           (FindRec.Name <> '.') and (FindRec.Name <> '..') and
           (SourceDir <> GameDataMerged) and
           (FindRec.Name <> 'RaymarchedVolumetrics') and
           (Pos('Scatterer', FindRec.Name) = 0) then
        begin
          Log('Moving contents of GameData directory: ' + GameDataDir + ' to ' + GameDataMerged);
          MoveDirectory(GameDataDir, GameDataMerged);
        end
        else
        begin
          Log('Skipping directory: ' + FindRec.Name);
        end;
        
        // Update progress
        Inc(ProgressCounter);
        MergePage.SetProgress(ProgressCounter, 50);
        WizardForm.Update;
                
      until not FindNext(FindRec);
    finally
      FindClose(FindRec);
    end;
  end;
	
	Inc(ProgressCounter);
	MoveParallaxDirectories;
	CurrentFileLabelM.Caption := 'Merging: Parallax';
  MergePage.SetProgress(ProgressCounter, 50);
	WizardForm.Update;
	
	if RaymarchedVolumetricsCheckbox.Checked then
	begin
	  Inc(ProgressCounter);
		MoveRaymarchedVolumetrics;
		CurrentFileLabelM.Caption := 'Merging: Raymarched Volumetrics';
		MergePage.SetProgress(ProgressCounter, 50);
		WizardForm.Update;
	end
	else
	begin
	  Inc(ProgressCounter);
		MoveScattererGameData;
		CurrentFileLabelM.Caption := 'Merging: Scatterer';
		MergePage.SetProgress(ProgressCounter, 50);
		WizardForm.Update;
	end;

  CurrentFileLabelM.Caption := 'Wrapping Up...';
  WizardForm.Update;

  MergePage.SetProgress(50, 50);
  WizardForm.Update;
  MergePage.SetProgress(DownloadList.Count, DownloadList.Count);
end;

procedure ExtractRaymarchedVolumetrics;
var
  SourceFile, DestDir: string;
begin
  if RaymarchedVolumetricsCheckbox.Checked then
  begin
    SourceFile := RAYVOL_DIR;
    DestDir := DownloadsDir + '\RaymarchedVolumetrics';

    if not ExtractProgram(SourceFile, DestDir) then
    begin
      Log('Failed to extract ' + SourceFile + ' to ' + DestDir);
      MsgBox('Failed to extract ' + SourceFile + '. Please check the logs for details.', mbError, MB_OK);
    end;
  end;
end;

procedure ExtractProc;
// Process to use 7 zip on each folder in downloads directory
var
  I, PartCount, Count: Integer;
  FileName, CurrentLoc, URL, DownloadItem, Dest, EndDest: string;
  IsMultiPart, ExtractionSuccessful: Boolean;
begin
	
	// First move and extract Raymarched Volumetrics if checked 
	if not RaymarchedVolumetricsCheckbox.Checked then
  begin
	  Count := DownloadList.Count;
    Log('Not moving/extracting Blackrack''s Volumetrics');
  	ExtractPage.SetProgress(0, Count);
  end
  else
  begin
	  Count := DownloadList.Count + 1;
		ExtractPage.SetProgress(0, Count);
		Log('Moving/extracting Blackrack''s Volumetrics');
		CurrentFileLabelE.Caption := 'Extracting: Raymarched Volumetrics';
		WizardForm.Update;
		ExtractRaymarchedVolumetrics;
	  ExtractPage.SetProgress(1, Count);
		WizardForm.Update;
  end;
	
  for I := 0 to DownloadList.Count - 1 do
  begin
    WizardForm.Update;
    ExtractPage.SetProgress(I, Count);
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
      if ExtractProgram(CurrentLoc, EndDest) then
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
  ExtractPage.SetProgress(DownloadList.Count, Count);
end;

procedure VerifyDownloads;
// Checks that everything downloaded okay
var
  I, GitHubCallCount: Integer;
  AllFilesDownloaded: Boolean;
begin
  AllFilesDownloaded := True;
  
  // We don't want stock textures, duh
  if DirectoryExists(DownloadsDir + '\Parallax_StockTextures-' + ParallaxVersion) then
  begin
    if not DelTree(DownloadsDir + '\Parallax_StockTextures-' + ParallaxVersion, True, True, True) then
      DownloadLogs.Add('Failed to delete Parallax_StockTextures directory.')
    else
      DownloadLogs.Add('Parallax_StockTextures directory deleted.')
  end
  else
    DownloadLogs.Add('Parallax_StockTextures directory does not exist.');

  // Check for any failure messages in DownloadLogs
  for I := 0 to DownloadLogs.Count - 1 do
  begin
    //Log(DownloadLogs[I]); //Debugging
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

procedure RemoveObsoleteFolders;
// Deletes old or obsolete directories from the game installation.
// Prevents conflicts and ensures only relevant files remain.
begin
  Log('Removing obsolete folders');
  if DirectoryExists(KSP_DIR + '\GameData\Kopernicus') then
    if not DelTree(KSP_DIR + '\GameData\Kopernicus', True, True, True) then
      Log('Failed to delete Kopernicus directory.')
    else
      Log('Kopernicus directory deleted.')
  else
    Log('Kopernicus directory does not exist.');

  if DirectoryExists(KSP_DIR + '\GameData\Parallax') then
    if not DelTree(KSP_DIR + '\GameData\Parallax', True, True, True) then
      Log('Failed to delete Parallax directory.')
    else
      Log('Parallax directory deleted.')
  else
    Log('Parallax directory does not exist.');

  if DirectoryExists(KSP_DIR + '\GameData\Parallax_StockTextures') then
    if not DelTree(KSP_DIR + '\GameData\Parallax_StockTextures', True, True, True) then
      Log('Failed to delete Parallax_StockTextures directory.')
    else
      Log('Parallax_StockTextures directory deleted.')
  else
    Log('Parallax_StockTextures directory does not exist.');

  if RaymarchedVolumetricsCheckbox.Checked then
  begin
    if DirectoryExists(KSP_DIR + '\GameData\RSSVE') then
      if not DelTree(KSP_DIR + '\GameData\RSSVE', True, True, True) then
        Log('Failed to delete RSSVE directory.')
      else
        Log('RSSVE directory deleted.')
    else
      Log('RSSVE directory does not exist.');
  end
  else
  begin
    Log('RSSVE directory will remain, no raymarched volumetrics.');
  end;

  if AddTUFXCheckbox.Checked then
  begin
    if DirectoryExists(KSP_DIR + 'KSPModStewards/TUFX') then
      if not DelTree(KSP_DIR + 'KSPModStewards/TUFX', True, True, True) then
        Log('Failed to delete TUFX directory.')
      else
        Log('TUFX directory deleted.')
    else
      Log('TUFX directory does not exist.');
  end
  else
  begin
    Log('TUFX directory will remain, not selected by user.');
  end;

  if DirectoryExists(KSP_DIR + '\GameData\RealSolarSystem') then
    if not DelTree(KSP_DIR + '\GameData\RealSolarSystem', True, True, True) then
      Log('Failed to delete RealSolarSystem directory.')
    else
      Log('RealSolarSystem directory deleted.')
  else
    Log('RealSolarSystem directory does not exist.');
		
  if DirectoryExists(KSP_DIR + '\GameData\EnvironmentalVisualEnhancements') then
    if not DelTree(KSP_DIR + '\GameData\EnvironmentalVisualEnhancements', True, True, True) then
      Log('Failed to delete EnvironmentalVisualEnhancements directory.')
    else
      Log('EnvironmentalVisualEnhancements directory deleted.')
  else
    Log('EnvironmentalVisualEnhancements directory does not exist.');
		
  if DirectoryExists(KSP_DIR + '\GameData\RSS-Configs') then
    if not DelTree(KSP_DIR + '\GameData\RSS-Configs', True, True, True) then
      Log('Failed to delete RSS-Configs directory.')
    else
      Log('RSS-Configs directory deleted.')
  else
    Log('RSS-Configs directory does not exist.');
		
  if DirectoryExists(KSP_DIR + '\GameData\RSS-Terrain') then
    if not DelTree(KSP_DIR + '\GameData\RSS-Terrain', True, True, True) then
      Log('Failed to delete RSS-Terrain directory.')
    else
      Log('RSS-Terrain directory deleted.')
  else
    Log('RSS-Terrain directory does not exist.');
		
  if DirectoryExists(KSP_DIR + '\GameData\RSSVE-Configs') then
    if not DelTree(KSP_DIR + '\GameData\RSSVE-Configs', True, True, True) then
      Log('Failed to delete RSSVE-Configs directory.')
    else
      Log('RSSVE-Configs directory deleted.')
  else
    Log('RSSVE-Configs directory does not exist.');
		
  if DirectoryExists(KSP_DIR + '\GameData\RSSVE-Textures') then
    if not DelTree(KSP_DIR + '\GameData\RSSVE-Textures', True, True, True) then
      Log('Failed to delete RSSVE-Textures directory.')
    else
      Log('RSSVE-Textures directory deleted.')
  else
    Log('RSSVE-Textures directory does not exist.');
		
  if DirectoryExists(KSP_DIR + '\GameData\Scatterer') then
    if not DelTree(KSP_DIR + '\GameData\Scatterer', True, True, True) then
      Log('Failed to delete Scatterer directory.')
    else
      Log('Scatterer directory deleted.')
  else
    Log('Scatterer directory does not exist.');

  if DirectoryExists(KSP_DIR + '\GameData\StockScattererConfigs') then
    if not DelTree(KSP_DIR + '\GameData\StockScattererConfigs', True, True, True) then
      Log('Failed to delete StockScattererConfigs directory.')
    else
      Log('StockScattererConfigs directory deleted.')
  else
    Log('ScatteStockScattererConfigsrer directory does not exist.');

    if DirectoryExists(KSP_DIR + '\GameData\StockVolumetricClouds') then
    if not DelTree(KSP_DIR + '\GameData\StockVolumetricClouds', True, True, True) then
      Log('Failed to delete StockVolumetricClouds directory.')
    else
      Log('StockVolumetricClouds directory deleted.')
  else
    Log('StockVolumetricClouds directory does not exist.');
    
  Log('Folders removal completed.');
end;

procedure StartInstallation;
// Ensures RP-1 checkbox was checked by user before removing folders
begin
  Log('Starting RSS Reborn installation process by clearing old folders.');
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
  
    if NoAssetsFound then
    begin
      MsgBox('No assets found for the latest release of one or more bodies. They will not be downloaded.', mbInformation, MB_OK);
    end;
  end
  else if (CurPageID = KSPDirPage.ID) then
  begin
    SetKSPDir;
    if KSPDirPage.Values[0] = '' then
    begin
      Result := False; 
      Exit;
    end;
  end;

  if (CurPageID = KSPDirPage.ID) and (RaymarchedVolumetricsCheckbox.Checked) then
  begin
    if (FileEdit.Text = '') or 
      ((LowerCase(ExtractFileExt(FileEdit.Text)) <> '.zip') and 
        (LowerCase(ExtractFileExt(FileEdit.Text)) <> '.7z')) or 
      (Pos('RaymarchedVolumetrics', ExtractFileName(FileEdit.Text)) = 0) then
    begin
      MsgBox('Please select a valid Raymarched Volumetrics compressed file.', mbError, MB_OK);
      Log('Selected file for Raymarched Volumetrics:' + FileEdit.Text)
      Result := False;
      Exit;
    end;
  end;
end;

procedure DownloadAllFiles;
var
  URL, Dest, FileName, DownloadItem: String;
  I: Integer;
begin
  Log('========================================================');
  //DownloadPage.SetProgress(0, DownloadList.Count);
  //IndividualProgressBar.Position := 0;
  for I := 0 to DownloadList.Count - 1 do
  begin
    Log(DownloadList[I]);
  end;
  if DownloadList.Count = 0 then
  begin
    Log('No downloads found!');
    MsgBox('Failed to queue URLs for download. Please submit an issue on GitHub with your log.', mbError, MB_OK);
    abort;
  end;

  idpSetOption('DetailedMode',   '1');

  for I := 0 to DownloadList.Count - 1 do
  begin
    DownloadItem := DownloadList[I];
    URL := Copy(DownloadItem, 1, Pos('=', DownloadItem) - 1);
    FileName := CustomExtractFileName(URL);
    Dest := DownloadsDir + '\' + FileName;
    WizardForm.Update;
    Log('Queueing download for URL: ' + URL);
    Log('URL queued to: ' + expandconstant(Dest));
    idpAddFile(URL, ExpandConstant(Dest));
  end;

  idpDownloadAfter(wpPreparing);
  WizardForm.Update;
end;

procedure MoveGameDataToKSPDir;
// Final move to KSP installation 
var
  SourceDir, DestDir: string;
begin
  // Define the source and destination directories
  SourceDir := (DownloadsDir + '\GameDataMerged\GameData');
  DestDir := KSP_DIR + '\GameData';
  
  // Log the action
  Log('Moving GameData from ' + SourceDir + ' to ' + DestDir);
  
  // Perform the copy and log any errors
  try
    MoveDirectory(SourceDir, DestDir);
    Log('Successfully moved GameData to ' + DestDir);
  except
    Log('Failed to move GameData to ' + DestDir);
    MsgBox('Failed to move GameData to ' + DestDir + '. Please check the logs for details.', mbError, MB_OK);
  end;
end;

//To be added
//procedure BackupGameDataFolder;
//
//var
//  GameDataPath, BackupPath, PowerShellPath, CommandLine: string;
//  ResultCode: Integer;
//begin
//  GameDataPath := KSP_DIR + '\GameData';
//  BackupPath := ExpandConstant('{userdesktop}') + '\GameDataBackup.zip';
//
//  // Check if PowerShell is available
//  if FileExists('C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe') then
//    PowerShellPath := 'C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe'
//  else
//  begin
//    Log('PowerShell executable not found!');
//    MsgBox('PowerShell executable not found! Please ensure PowerShell is available.', mbError, MB_OK);
//    Exit;
//  end;
//
//  // Build the command line for PowerShell to create the zip file
//  CommandLine := Format('-Command "Compress-Archive -Path ''%s\*'' -DestinationPath ''%s'' -Force"', [GameDataPath, BackupPath]);
//
//  // Log the paths for verification
//  Log('GameDataPath: ' + GameDataPath);
//  Log('BackupPath: ' + BackupPath);
//
//  // Log the command line
//  Log('Executing command: "' + PowerShellPath + '" ' + CommandLine);
//
//  // Execute the command
//  if Exec(PowerShellPath, CommandLine, '', SW_HIDE, ewWaitUntilTerminated, ResultCode) then
//  begin
//    if ResultCode = 0 then
//    begin
//      Log('GameData folder successfully backed up.');
//    end
//    else
//    begin
//      Log('Error compressing GameData folder. Backup failed. Error code: ' + IntToStr(ResultCode));
//      MsgBox('Error compressing GameData folder. Backup failed. Error code: ' + IntToStr(ResultCode), mbError, mb_OK);
//    end;
//  end
//  else
//  begin
//    Log('Error starting PowerShell process for backup. Backup failed.');
//    MsgBox('Error starting PowerShell process for backup. Backup failed.', mbError, mb_OK);
//  end;
//
//  // Verify if the backup file exists and log the result
//  if FileExists(BackupPath) then
//  begin
//    Log('Backup file created successfully: ' + BackupPath);
//  end
//  else
//  begin
//    Log('Backup file not found: ' + BackupPath);
//    MsgBox('Backup file not found: ' + BackupPath, mbError, MB_OK);
//  end;
//end;

procedure LogReverseCommandsAtEnd;
var
  I: Integer;
begin
  Log('========================================================');
  Log('FOR REFERENCE ONLY - ROBOCOMMANDS:');
  for I := 0 to ReverseRobocopyCommands.Count - 1 do
  begin
    Log(RobocopyCommands[I]);
    Log(ReverseRobocopyCommands[I]);
  end;
  Log('========================================================');
end;

function InitializeDownloads: Boolean;
// Helper function to call proc, returns error if it cannot execute 
begin
  Result := True;
  try
    ClearDownloadDirectory;
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
var
  LogFilePathName, LogFileName, NewFilePathName, NewFilePathNameLogs, NewFilePathNameRSS: string;
begin
  Log('Deinitializing setup and cleaning up resources');  
	ClearDownloadDirectory;

  // Get the full path of the log file
  LogFilePathName := ExpandConstant('{log}');
  LogFileName := ExtractFileName(LogFilePathName);
  
  // Set the new target path
  NewFilePathName := ExpandConstant(KSP_DIR + '\Logs\RSSRebornInstaller\') + LogFileName;
  Log('Destination file path ' + NewFilePathName);
  NewFilePathNameLogs := ExpandConstant(KSP_DIR + '\Logs');
  Log('KSP Log file path ' + NewFilePathNameLogs);
  NewFilePathNameRSS := ExpandConstant(KSP_DIR + '\Logs\RSSRebornInstaller');
  Log('RSS Log file path ' + NewFilePathNameRSS);

  // Ensure the directory exists
  if not DirExists(NewFilePathNameLogs) then
    CreateDir(NewFilePathNameLogs);

  if not DirExists(ExpandConstant(NewFilePathNameRSS)) then
    CreateDir(ExpandConstant(NewFilePathNameRSS));
  
  // Copy the log file to the new location
  if FileExists(LogFilePathName) then
  begin
    FileCopy(LogFilePathName, NewFilePathName, False);
    Log('Log Copied to ' + NewFilePathName);
  end
  else
  begin
    MsgBox('Log file not found.', mbError, MB_OK)
  end;

  Log('Installer has completed.');  
end;

procedure CheckAndAddFolder(const FolderName: string; ModList, MissingMods: TStringList; GameDataPath: string);
begin
  if not DirExists(GameDataPath + '\' + FolderName) then
  begin
    MissingMods.Add(FolderName);
  end
  else
  begin
    ModList.Add(FolderName);
  end;
end;

procedure ListGameDataFolders;
var
  GameDataPath, TexturesDir: string;
  ModList, MissingMods: TStringList;
  MsgText: string;
  i: Integer;
begin
  // Set the path to the GameData folder
  GameDataPath := KSP_DIR + '\GameData';
  TexturesDIr := GameDataPath + '\RSS-Textures\PluginData'

  // Initialize the string lists to store mod folder names and missing mods
  ModList := TStringList.Create;
  MissingMods := TStringList.Create;
  try
    // List of required mods
    CheckAndAddFolder('Kopernicus', ModList, MissingMods, GameDataPath);
    CheckAndAddFolder('RSS-Configs', ModList, MissingMods, GameDataPath);
    CheckAndAddFolder('RSS-Terrain', ModList, MissingMods, GameDataPath);
    CheckAndAddFolder('RSS-Textures', ModList, MissingMods, GameDataPath);
    CheckAndAddFolder('Scatterer', ModList, MissingMods, GameDataPath);
    CheckAndAddFolder('EnvironmentalVisualEnhancements', ModList, MissingMods, GameDataPath);
    CheckAndAddFolder('00_Sun', ModList, MissingMods, TexturesDIr);
    CheckAndAddFolder('01_Mercury', ModList, MissingMods, TexturesDIr);
    CheckAndAddFolder('02_Venus', ModList, MissingMods, TexturesDIr);
    CheckAndAddFolder('03_Earth', ModList, MissingMods, TexturesDIr);
    CheckAndAddFolder('03-01_Moon', ModList, MissingMods, TexturesDIr);
    CheckAndAddFolder('04_Mars', ModList, MissingMods, TexturesDIr);
    CheckAndAddFolder('05_Jupiter', ModList, MissingMods, TexturesDIr);
    CheckAndAddFolder('06_Saturn', ModList, MissingMods, TexturesDIr);
    CheckAndAddFolder('07_Uranus', ModList, MissingMods, TexturesDIr);
    CheckAndAddFolder('08_Neptune', ModList, MissingMods, TexturesDIr);
    CheckAndAddFolder('09-01_Vesta', ModList, MissingMods, TexturesDIr);
    CheckAndAddFolder('09-02_Ceres', ModList, MissingMods, TexturesDIr);
    CheckAndAddFolder('10-01_Pluto', ModList, MissingMods, TexturesDIr);

    if RaymarchedVolumetricsCheckbox.Checked then
    begin
      CheckAndAddFolder('RSSVE-Configs', ModList, MissingMods, GameDataPath);
      CheckAndAddFolder('RSSVE-Textures', ModList, MissingMods, GameDataPath);
    end
    else
    begin
      CheckAndAddFolder('RSSVE', ModList, MissingMods, GameDataPath);
    end;

    if AddTUFXCheckbox.Checked then
    begin
      CheckAndAddFolder('TUFX', ModList, MissingMods, GameDataPath);
    end;

    // Log the complete mod list
    Log('Installed mods in GameData folder:');
    for i := 0 to ModList.Count - 1 do
    begin
      Log(' - ' + ModList[i]);
    end;

    // If there are missing mods, show a message to the user
    if MissingMods.Count > 0 then
    begin
      MsgText := 'The following mods are missing from your GameData folder: ' + MissingMods.CommaText + ' ';
      MsgBox(MsgText, mbError, MB_OK);
    end;
  finally
    ModList.Free;
    MissingMods.Free;
  end;
end;

procedure CurPageChanged(CurPageID: Integer);
begin
  if CurPageID = wpPreparing then
  begin
    Log('wpPrep page reached. Running custom code before ssInstall.');
    Log('========================================================');
    Log('Install step reached. Starting installation process.');

		// Set KSP directory based on user input, and immediately back it up in case of failure
		SetKSPDir;
		//BackupGameDataFolder;
		
    try
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
			StartInstallation;
			DownloadAllFiles;
      UpdateConfigFile;
		finally
			OnDownloadComplete;
			ExtractPage.Show;
		end
  end;
end;

procedure CurStepChanged(CurStep: TSetupStep);
// Actual process steps for installation
begin
  if CurStep = ssPostInstall then
  begin
		try
      ExtractPage.Show;
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
		  MoveGameDataToKSPDir;
			MergePage.hide;
      LogReverseCommandsAtEnd;
		end;

      // List all folders in the GameData directory at the end and check for missing mods
      ListGameDataFolders;

      Log('Installation process completed successfully, cleaning up files now');
			DeinitializeSetup;
  end;
end;