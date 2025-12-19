unit uPGConfig; // Manajemen Konfigurasi Aplikasi

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  TPGConfig = record
    Enabled: Boolean;
    Host: string;
    Port: Integer;
    Database: string;
    Username: string;
    Password: string;
  end;

  TSyncConfig = record
    AutoSync: Boolean;
    SyncInterval: Integer;  // dalam detik
    MaxRetry: Integer;
    QueueSize: Integer;
  end;

  TAppConfig = record
    PGConfig: TPGConfig;
    SyncConfig: TSyncConfig;
  end;

function LoadAppConfig: TAppConfig;
procedure SaveAppConfig(const AConfig: TAppConfig);
function GetDefaultConfig: TAppConfig;

implementation

function GetDefaultConfig: TAppConfig;
begin
  // PostgreSQL defaults
  Result.PGConfig.Enabled := True;
  Result.PGConfig.Host := 'localhost';
  Result.PGConfig.Port := 5432;
  Result.PGConfig.Database := 'kasir_db';
  Result.PGConfig.Username := 'kasir_user';
  Result.PGConfig.Password := '';

  // Sync defaults
  Result.SyncConfig.AutoSync := True;
  Result.SyncConfig.SyncInterval := 300;  // 5 menit
  Result.SyncConfig.MaxRetry := 5;
  Result.SyncConfig.QueueSize := 1000;
end;

function LoadAppConfig: TAppConfig;
var
  Ini: TIniFile;
  IniPath: string;
begin
  // Get default first
  Result := GetDefaultConfig;

  IniPath := ExtractFilePath(ParamStr(0)) + 'config.ini';

  if not FileExists(IniPath) then
  begin
    // Create default config file
    SaveAppConfig(Result);
    Exit;
  end;

  Ini := TIniFile.Create(IniPath);
  try
    // PostgreSQL section
    Result.PGConfig.Enabled := (LowerCase(Ini.ReadString('PostgreSQL', 'Enabled', 'true')) = 'true');
    Result.PGConfig.Host := Ini.ReadString('PostgreSQL', 'Host', 'localhost');
    Result.PGConfig.Port := Ini.ReadInteger('PostgreSQL', 'Port', 5432);
    Result.PGConfig.Database := Ini.ReadString('PostgreSQL', 'Database', 'kasir_db');
    Result.PGConfig.Username := Ini.ReadString('PostgreSQL', 'Username', 'kasir_user');
    Result.PGConfig.Password := Ini.ReadString('PostgreSQL', 'Password', '');

    // Sync section
    Result.SyncConfig.AutoSync := Ini.ReadBool('Sync', 'AutoSync', True);
    Result.SyncConfig.SyncInterval := Ini.ReadInteger('Sync', 'SyncInterval', 300);
    Result.SyncConfig.MaxRetry := Ini.ReadInteger('Sync', 'MaxRetry', 5);
    Result.SyncConfig.QueueSize := Ini.ReadInteger('Sync', 'QueueSize', 1000);
  finally
    Ini.Free;
  end;
end;

procedure SaveAppConfig(const AConfig: TAppConfig);
var
  Ini: TIniFile;
  IniPath: string;
begin
  IniPath := ExtractFilePath(ParamStr(0)) + 'config.ini';
  Ini := TIniFile.Create(IniPath);
  try
    // PostgreSQL section
    Ini.WriteBool('PostgreSQL', 'Enabled', AConfig.PGConfig.Enabled);
    Ini.WriteString('PostgreSQL', 'Host', AConfig.PGConfig.Host);
    Ini.WriteInteger('PostgreSQL', 'Port', AConfig.PGConfig.Port);
    Ini.WriteString('PostgreSQL', 'Database', AConfig.PGConfig.Database);
    Ini.WriteString('PostgreSQL', 'Username', AConfig.PGConfig.Username);
    Ini.WriteString('PostgreSQL', 'Password', AConfig.PGConfig.Password);

    // Sync section
    Ini.WriteBool('Sync', 'AutoSync', AConfig.SyncConfig.AutoSync);
    Ini.WriteInteger('Sync', 'SyncInterval', AConfig.SyncConfig.SyncInterval);
    Ini.WriteInteger('Sync', 'MaxRetry', AConfig.SyncConfig.MaxRetry);
    Ini.WriteInteger('Sync', 'QueueSize', AConfig.SyncConfig.QueueSize);
  finally
    Ini.Free;
  end;
end;

end.
