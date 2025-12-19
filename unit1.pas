unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, fpjson, jsonparser, SQLDB, SQLite3Conn, DB, Math,
  Forms, Controls, Graphics, Dialogs, StdCtrls, DBGrids, LCLType, ExtCtrls,
  IniFiles, StrUtils,
  FormLogin, FormModalAwal, FormDebit, FormSearchName, FormReturn, uTypes,
  uPostgreSQL, uSyncQueue, uPGConfig, uSyncWorker;

const
  // Terminal configuration
  CONFIG_FILE = 'terminal.ini';

  // Keyboard shortcuts
  KEY_CLEAR_CART = VK_DELETE;
  KEY_SEARCH_NAME = VK_F2;
  KEY_DISC_PERCENT = VK_F6;
  KEY_DISC_NOMINAL = VK_F7;
  KEY_RETURN = VK_F9;
  KEY_EOD = VK_F10;
  KEY_PAY_DEBIT = VK_F11;
  KEY_PAY_CASH = VK_F12;
  KEY_CLEAR_INPUT = VK_SPACE;
  KEY_SET_QTY = VK_END;
  KEY_SCAN = VK_RETURN;
  KEY_FOCUS_INPUT = VK_ESCAPE;

  // Product code length ranges
  MIN_CODE_LENGTH = 5;
  MAX_SHORT_CODE = 8;
  MIN_BARCODE_LENGTH = 9;
  MAX_BARCODE_LENGTH = 13;

type
  { TForm1 }
  TForm1 = class(TForm)
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    edtMainInput: TEdit;
    GroupBox1: TGroupBox;
    lblDriveStatus: TLabel;
    lQtyInfo: TLabel;
    lSubTtl: TLabel;
    pnlStatus: TPanel;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    TimerDateTime: TTimer;
    TimerDriveCheck: TTimer;

    lblAppTitle: TLabel;
    lblDateTime: TLabel;
    lblSubtotalLabel: TLabel;
    lblInputLabel: TLabel;
    lblShortcuts: TLabel;
    pnlMain: TPanel;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    pnlSubtotal: TPanel;
    pnlInput: TPanel;

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtMainInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtMainInputKeyPress(Sender: TObject; var Key: char);
    procedure TimerDateTimeTimer(Sender: TObject);
    procedure TimerDriveCheckTimer(Sender: TObject);

  private
    FPendingQty: Integer;
    FQtyMode: Boolean;
    FDriveOnline: Boolean;
    FDataDrivePath: string;
    FProcessingReturn: Boolean;
    FCurrentDrawerID: Integer;
    FCurrentUser: TUserInfo;
    FLoginHistoryID: Integer;

    // PostgreSQL & Sync managers
    FPostgreSQLManager: TPostgreSQLManager;
    FSyncQueueManager: TSyncQueueManager;
    FSyncWorker: TSyncWorker;
    FAppConfig: TAppConfig;

    procedure LoadTerminalConfig;
    procedure ValidateTerminalConfig;

    procedure DoLogin;
    procedure DoLogout;
    procedure CreateUserTables;
    procedure UpdateUserInfoDisplay;

    procedure CheckAndOpenCashDrawer;
    function IsCashDrawerOpen: Boolean;
    procedure CreateCashDrawerTable;
    procedure UpdateCashDrawerSales(Amount: Double; IsReturn: Boolean);

    procedure CheckDriveStatus;
    procedure UpdateStatusIndicator(IsOnline: Boolean);
    procedure SetupGridColumnWidths;

    procedure CreateAllTables;
    procedure LoadCartTemp;
    procedure UpdateSubtotal;
    procedure ClearCartWithLog;
    procedure ProcessPayment(PayType: string; Provider: string; Last4: string; PayInput: string);
    function SearchProduct(const ACode: string): TProductInfo;
    function ReadFileToString(const FileName: string): string;
    function GenerateCancelNo: string;
    function GenerateTransactionNo: string;
    procedure AddToCart(const Info: TProductInfo);
    procedure ProcessInputWithQty(const ACode: string);
    procedure AddToCartWithQty(const Info: TProductInfo; AQty: Integer);
    procedure ApplyDiscountToLastItem(Value: Double; IsPercent: Boolean);
    procedure HandleSearchByName;
    procedure HandleDiscountPercent;
    procedure HandleDiscountNominal;
    procedure HandleDebitPayment;
    procedure HandleCashPayment;
    function ValidatePaymentAmount(const PayInput: string; SubTotal: Double; out PayAmount: Double): Boolean;

    procedure HandleReturn;
    procedure ProcessReturn(const OrigTrx: TTransactionRecord; const Items: array of TReturnItem);
    function GenerateReturnNo: string;

    function GetPriceByQty(const Info: TProductInfo; AQty: Double): Double;

    procedure InitializePostgreSQL;
    procedure SyncUsersFromPostgreSQL;
    procedure TrySyncTransaction(const ATrxID: string; const ATrxData: TJSONObject);

    procedure HandleEndOfDay;
    procedure ProcessEndOfDay;
    procedure SyncCashDrawerEOD;
    procedure SyncProductsFromDBF;
    function SyncAllPendingData: Integer;
    function ReadDBFProducts(const DBFPath: string): Integer;
  public
  end;

var
  Form1: TForm1;
  TERMINAL_ID: string = '';
  TERMINAL_NAME: string = '';
  TERMINAL_LOCATION: string = '';

implementation

{$R *.lfm}

{ =============================================================================}
{ POSTGRESQL INITIALIZATION                                               }
{ =============================================================================}

procedure TForm1.InitializePostgreSQL;
begin
  if not FAppConfig.PGConfig.Enabled then
    Exit;

  try
    if FPostgreSQLManager.Connect(
      FAppConfig.PGConfig.Host,
      FAppConfig.PGConfig.Database,
      FAppConfig.PGConfig.Username,
      FAppConfig.PGConfig.Password,
      FAppConfig.PGConfig.Port
    ) then
    begin
      // Online - Register terminal
      try
        FPostgreSQLManager.RegisterTerminal(TERMINAL_NAME, TERMINAL_LOCATION);
      except
        // Silent
      end;

      // Start sync worker
      try
        FSyncWorker.Start;
      except
        // Silent
      end;

      UpdateStatusIndicator(FDriveOnline);
    end
    else
    begin
      // OFFLINE MODE - Tidak perlu error message
      // Status indicator sudah show OFFLINE
      UpdateStatusIndicator(FDriveOnline);
    end;
  except
    on E: Exception do
    begin
      // Silent fail - status indicator akan show OFFLINE
      UpdateStatusIndicator(FDriveOnline);
    end;
  end;
end;

procedure TForm1.TrySyncTransaction(const ATrxID: string; const ATrxData: TJSONObject);
var
  Success: Boolean;
begin
  Success := False;

  // Try direct sync if online
  if Assigned(FPostgreSQLManager) and FPostgreSQLManager.IsConnected then
  begin
    try
      Success := FPostgreSQLManager.SyncTransaction(ATrxID, ATrxData);
    except
      Success := False;
    end;
  end;

  // If failed, add to queue
  if not Success and Assigned(FSyncQueueManager) then
  begin
    try
      FSyncQueueManager.QueueTransaction(ATrxID, ATrxData);
    except
      // Silent fail
    end;
  end;
end;

{ --------------------------------------------------------------- }
{ SYNC USERS FROM POSTGRESQL }
{ --------------------------------------------------------------- }
procedure TForm1.SyncUsersFromPostgreSQL;
var
  Users: TJSONArray;
  UserObj: TJSONObject;
  i: Integer;
  UserLevel: string;
  SyncCount: Integer;
begin
  Users := nil;

  if not Assigned(FPostgreSQLManager) or not FPostgreSQLManager.IsConnected then
    raise Exception.Create('Tidak terhubung ke PostgreSQL');

  try
    Users := FPostgreSQLManager.FetchAllUsers;

    if Users.Count = 0 then
      raise Exception.Create('Tidak ada user di PostgreSQL');

    SQLQuery1.SQL.Text := 'DELETE FROM users WHERE is_default = 0';
    SQLQuery1.ExecSQL;

    SyncCount := 0;

    for i := 0 to Users.Count - 1 do
    begin
      UserObj := Users.Objects[i];
      UserLevel := UserObj.Get('user_level', 'kasir');

      SQLQuery1.SQL.Text :=
        'INSERT OR REPLACE INTO users (id, cashier_code, password, name, level, active, is_default, updated_at) ' +
        'VALUES (:id, :code, :pass, :name, :level, 1, 0, :upd)';

      SQLQuery1.ParamByName('id').AsInteger := UserObj.Get('user_id', 0);
      SQLQuery1.ParamByName('code').AsString := UserObj.Get('username', '');
      SQLQuery1.ParamByName('pass').AsString := '000000';
      SQLQuery1.ParamByName('name').AsString := UserObj.Get('full_name', '');
      SQLQuery1.ParamByName('level').AsString := UserLevel;
      SQLQuery1.ParamByName('upd').AsString := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
      SQLQuery1.ExecSQL;

      Inc(SyncCount);
    end;

    if SQLTransaction1.Active then
      SQLTransaction1.Commit;

  except
    on E: Exception do
    begin
      if SQLTransaction1.Active then
        SQLTransaction1.Rollback;

      if Assigned(Users) then
        Users.Free;

      raise; // Re-raise exception untuk ditangkap di DoLogin
    end;
  end;

  if Assigned(Users) then
    Users.Free;
end;

{ --------------------------------------------------------------- }
{ SYNC PRODUCTS FROM DBF }
{ --------------------------------------------------------------- }
procedure TForm1.SyncProductsFromDBF;
var
  DBFPath: string;
  SyncCount: Integer;
begin
  DBFPath := 'F:\wpi\dat\produk.dbf';

  if not FileExists(DBFPath) then
    raise Exception.Create('File produk.dbf tidak ditemukan: ' + DBFPath);

  try
    SyncCount := ReadDBFProducts(DBFPath);

    if SyncCount = 0 then
      raise Exception.Create('Tidak ada data produk yang berhasil disinkron');

  except
    on E: Exception do
      raise; // Re-raise exception
  end;
end;

function TForm1.ReadDBFProducts(const DBFPath: string): Integer;
var
  Proc: TProcess;
  BasePath, JsonPath, ExecPath: string;
  JSON: TJSONData;
  Arr: TJSONArray;
  Obj: TJSONObject;
  S: string;
  i: Integer;
  ProdCode, AltCode, ProdName: string;  // ← Tambahkan AltCode
  Price, PriceA, PriceB, QtyA, QtyB: Double;
  Boom, QtyMeth: Integer;
  SyncCount: Integer;
begin
  Result := 0;

  if not FileExists(DBFPath) then
  begin
    ShowMessage('File DBF tidak ditemukan!' + sLineBreak +
                'Path: ' + DBFPath);
    Exit;
  end;

  try
    BasePath := IncludeTrailingPathDelimiter(
      ExtractFilePath(Application.ExeName) + 'hb30'
    );

    JsonPath := BasePath + 'products.json';
    ExecPath := BasePath + 'syncprod.exe';

    if FileExists(JsonPath) then
      DeleteFile(JsonPath);

    if FileExists(ExecPath) then
    begin
      Proc := TProcess.Create(nil);
      try
        Proc.Executable := ExecPath;
        Proc.Parameters.Add(DBFPath);
        Proc.Parameters.Add(JsonPath);
        Proc.CurrentDirectory := BasePath;
        Proc.Options := [poWaitOnExit, poNoConsole];
        Proc.Execute;
      finally
        Proc.Free;
      end;
    end
    else
    begin
      ShowMessage('syncprod.exe tidak ditemukan!' + sLineBreak +
                  'Path: ' + ExecPath);
      Exit;
    end;

    Sleep(500);

    if not FileExists(JsonPath) then
    begin
      ShowMessage('Gagal membuat file JSON dari DBF!');
      Exit;
    end;

    S := ReadFileToString(JsonPath);
    if Trim(S) = '' then
    begin
      ShowMessage('File JSON kosong!');
      Exit;
    end;

    JSON := nil;
    SyncCount := 0;

    try
      JSON := GetJSON(S);

      if JSON.JSONType = jtArray then
      begin
        Arr := TJSONArray(JSON);

        SQLQuery1.SQL.Text := 'DELETE FROM products';
        SQLQuery1.ExecSQL;

        for i := 0 to Arr.Count - 1 do
        begin
          Obj := Arr.Objects[i];

          ProdCode := Obj.Get('code', '');
          AltCode := Obj.Get('altcode', '');  // ← Ambil altcode
          ProdName := Obj.Get('name', '');
          Price := Obj.Get('price', 0.0);
          PriceA := Obj.Get('price_a', 0.0);
          QtyA := Obj.Get('qty_a', 0.0);
          PriceB := Obj.Get('price_b', 0.0);
          QtyB := Obj.Get('qty_b', 0.0);
          Boom := Obj.Get('boom', 0);
          QtyMeth := Obj.Get('qtymeth', 0);

          if Trim(ProdCode) = '' then
            Continue;

          SQLQuery1.SQL.Text :=
            'INSERT OR REPLACE INTO products ' +
            '(product_code, altcode, product_name, price, price_a, qty_a, price_b, qty_b, boom, qtymeth, is_active, updated_at) ' +
            'VALUES (:code, :altcode, :name, :price, :pricea, :qtya, :priceb, :qtyb, :boom, :qtymeth, 1, :upd)';

          SQLQuery1.ParamByName('code').AsString := ProdCode;
          SQLQuery1.ParamByName('altcode').AsString := AltCode;  // ← Insert altcode
          SQLQuery1.ParamByName('name').AsString := ProdName;
          SQLQuery1.ParamByName('price').AsFloat := Price;
          SQLQuery1.ParamByName('pricea').AsFloat := PriceA;
          SQLQuery1.ParamByName('qtya').AsFloat := QtyA;
          SQLQuery1.ParamByName('priceb').AsFloat := PriceB;
          SQLQuery1.ParamByName('qtyb').AsFloat := QtyB;
          SQLQuery1.ParamByName('boom').AsInteger := Boom;
          SQLQuery1.ParamByName('qtymeth').AsInteger := QtyMeth;
          SQLQuery1.ParamByName('upd').AsString := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
          SQLQuery1.ExecSQL;

          Inc(SyncCount);
        end;

        if SQLTransaction1.Active then
          SQLTransaction1.Commit;

        Result := SyncCount;
      end;

    finally
      if Assigned(JSON) then
        JSON.Free;
      if FileExists(JsonPath) then
        DeleteFile(JsonPath);
    end;

  except
    on E: Exception do
    begin
      if SQLTransaction1.Active then
        SQLTransaction1.Rollback;
      ShowMessage('Error sync products: ' + E.Message);
    end;
  end;
end;


{ --------------------------------------------------------------- }
{ DRIVE STATUS CHECK }
{ --------------------------------------------------------------- }
procedure TForm1.CheckDriveStatus;
var
  DriveAvailable: Boolean;
begin
  DriveAvailable := DirectoryExists(FDataDrivePath);

  if DriveAvailable <> FDriveOnline then
  begin
    FDriveOnline := DriveAvailable;
    UpdateStatusIndicator(FDriveOnline);
  end;
end;

procedure TForm1.UpdateStatusIndicator(IsOnline: Boolean);
var
  StatusText: string;
begin
  if IsOnline then
  begin
    if Assigned(FPostgreSQLManager) and FPostgreSQLManager.IsConnected then
    begin
      StatusText := '● ONLINE + SYNC';
      lblDriveStatus.Font.Color := clWhite;
      pnlStatus.Color := $0050B000;  // Dark Green
    end
    else
    begin
      StatusText := '● ONLINE (LOCAL)';
      lblDriveStatus.Font.Color := clWhite;
      pnlStatus.Color := $00808000;  // Dark Yellow (warning)
    end;
  end
  else
  begin
    StatusText := '● OFFLINE';
    lblDriveStatus.Font.Color := clWhite;
    pnlStatus.Color := $002020B0;  // Dark Red
  end;

  lblDriveStatus.Caption := StatusText;
  lblDriveStatus.Refresh;
  pnlStatus.Refresh;
end;

procedure TForm1.TimerDriveCheckTimer(Sender: TObject);
begin
  CheckDriveStatus;
end;

{ --------------------------------------------------------------- }
{ GRID AUTO-RESIZE }
{ --------------------------------------------------------------- }
procedure TForm1.SetupGridColumnWidths;
const
  W_CODE = 90;
  W_QTY = 80;
  W_PRICE = 120;
  W_DISC_VALUE = 90;
  W_DISC_TYPE = 120;
  W_TOTAL = 200;
  W_MARGIN = 20;
  MIN_NAME_WIDTH = 260;
var
  FixedTotal, NameWidth: Integer;
begin
  if DBGrid1.Columns.Count < 7 then Exit;

  FixedTotal := W_CODE + W_QTY + W_PRICE + W_DISC_VALUE + W_DISC_TYPE + W_TOTAL + W_MARGIN;
  NameWidth := DBGrid1.ClientWidth - FixedTotal;

  if NameWidth < MIN_NAME_WIDTH then
    NameWidth := MIN_NAME_WIDTH;

  DBGrid1.Columns[0].Width := W_CODE;
  DBGrid1.Columns[1].Width := NameWidth;
  DBGrid1.Columns[2].Width := W_QTY;
  DBGrid1.Columns[3].Width := W_PRICE;
  DBGrid1.Columns[4].Width := W_DISC_VALUE;
  DBGrid1.Columns[5].Width := W_DISC_TYPE;
  DBGrid1.Columns[6].Width := W_TOTAL;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  SetupGridColumnWidths;
end;

{ --------------------------------------------------------------- }
{ FORM CREATE }
{ --------------------------------------------------------------- }
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Setting monitor
  if Screen.MonitorCount >= 2 then
  begin
    SetBounds(
      Screen.Monitors[1].Left,
      Screen.Monitors[1].Top,
      Screen.Monitors[1].Width,
      Screen.Monitors[1].Height
    );
  end;

  LoadTerminalConfig;
  ValidateTerminalConfig;
  FAppConfig := LoadAppConfig;

  FPendingQty := 0;
  FQtyMode := False;
  FDriveOnline := False;
  FProcessingReturn := False;
  FCurrentDrawerID := 0;
  FLoginHistoryID := 0;
  FDataDrivePath := 'F:\wpi\dat';

  FCurrentUser.ID := 0;
  FCurrentUser.CashierCode := '';
  FCurrentUser.Name := '';
  FCurrentUser.Level := ulKasir;

  SQLite3Connection1.DatabaseName := 'kasir.db';
  SQLite3Connection1.Params.Add('pfDisableParsers=1');
  SQLTransaction1.Action := caCommit;
  SQLite3Connection1.Transaction := SQLTransaction1;
  SQLTransaction1.Database := SQLite3Connection1;
  SQLQuery1.Database := SQLite3Connection1;
  SQLQuery1.Transaction := SQLTransaction1;

  try
    SQLite3Connection1.Connected := True;
    CreateAllTables;

    FSyncQueueManager := TSyncQueueManager.Create(SQLite3Connection1, SQLTransaction1, SQLQuery1);
    FPostgreSQLManager := TPostgreSQLManager.Create(TERMINAL_ID);
    FSyncWorker := TSyncWorker.Create(FPostgreSQLManager, FSyncQueueManager);

    DataSource1.DataSet := SQLQuery1;
    DBGrid1.DataSource := DataSource1;

    CheckDriveStatus;

    // ========== INITIALIZE POSTGRESQL DULU (SEBELUM LOGIN) ==========
    InitializePostgreSQL;
    // ========== END ==========

    // Login (akan cek FPostgreSQLManager.IsConnected)
    DoLogin;

    CheckAndOpenCashDrawer;
    LoadCartTemp;

  except
    on E: Exception do
      ShowMessage('Error inisialisasi database: ' + E.Message);
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;

  // Stop and free sync managers
  if Assigned(FSyncWorker) then
  begin
    FSyncWorker.Stop;
    FSyncWorker.Free;
  end;
  if Assigned(FPostgreSQLManager) then
    FPostgreSQLManager.Free;
  if Assigned(FSyncQueueManager) then
    FSyncQueueManager.Free;

  DoLogout;
end;

procedure TForm1.edtMainInputKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9', #8, #13]) then
  begin
    Key := #0;
    Exit;
  end;

  if (Length(edtMainInput.Text) >= 13) and (Key <> #8) then
  begin
    Key := #0;
    Exit;
  end;
end;

procedure TForm1.TimerDateTimeTimer(Sender: TObject);
begin
  lblDateTime.Caption := FormatDateTime('hh:nn:ss  |  dd/mm/yyyy  ', Now);
end;

{ --------------------------------------------------------------- }
{ TERMINAL CONFIGURATION }
{ --------------------------------------------------------------- }
procedure TForm1.LoadTerminalConfig;
var
  IniFile: TIniFile;
  ConfigPath: string;
begin
  ConfigPath := ExtractFilePath(Application.ExeName) + CONFIG_FILE;

  if not FileExists(ConfigPath) then
  begin
    ShowMessage('ERROR: File terminal.ini tidak ditemukan!' + sLineBreak +
                sLineBreak +
                'File: ' + ConfigPath + sLineBreak +
                sLineBreak +
                'Aplikasi tidak dapat berjalan tanpa konfigurasi terminal.' + sLineBreak +
                'Silakan buat file terminal.ini terlebih dahulu.');
    Application.Terminate;
    Exit;
  end;

  IniFile := TIniFile.Create(ConfigPath);
  try
    TERMINAL_ID := Trim(IniFile.ReadString('TERMINAL', 'ID', ''));
    TERMINAL_NAME := Trim(IniFile.ReadString('TERMINAL', 'NAME', 'Unknown'));
    TERMINAL_LOCATION := Trim(IniFile.ReadString('TERMINAL', 'LOCATION', ''));

    if TERMINAL_ID = '' then
    begin
      ShowMessage('ERROR: Terminal ID belum di-set!' + sLineBreak +
                  sLineBreak +
                  'Silakan isi [TERMINAL] ID di file terminal.ini' + sLineBreak +
                  'Contoh: ID=001');
      Application.Terminate;
      Exit;
    end;

    if Length(TERMINAL_ID) <> 3 then
    begin
      ShowMessage('ERROR: Terminal ID harus 3 digit!' + sLineBreak +
                  sLineBreak +
                  'Format yang benar: 001, 002, 003, dst' + sLineBreak +
                  'Saat ini: ' + TERMINAL_ID);
      Application.Terminate;
      Exit;
    end;

    try
      StrToInt(TERMINAL_ID);
    except
      ShowMessage('ERROR: Terminal ID harus angka!' + sLineBreak +
                  sLineBreak +
                  'Format yang benar: 001, 002, 003, dst' + sLineBreak +
                  'Saat ini: ' + TERMINAL_ID);
      Application.Terminate;
      Exit;
    end;

  finally
    IniFile.Free;
  end;
end;

procedure TForm1.ValidateTerminalConfig;
begin
  if TERMINAL_ID = '' then
  begin
    ShowMessage('CRITICAL ERROR: Terminal ID tidak terdeteksi!' + sLineBreak +
                'Aplikasi akan ditutup.');
    Application.Terminate;
  end;
end;

{ --------------------------------------------------------------- }
{ CREATE TABLES }
{ --------------------------------------------------------------- }
procedure TForm1.CreateAllTables;
begin
  SQLQuery1.Close;
  SQLQuery1.SQL.Text :=
    'CREATE TABLE IF NOT EXISTS cart_temp ('
    + ' id INTEGER PRIMARY KEY AUTOINCREMENT,'
    + ' product_code VARCHAR(50),'
    + ' product_name VARCHAR(100),'
    + ' price REAL,'
    + ' qty REAL,'
    + ' discount_value REAL,'
    + ' discount_type VARCHAR(50),'
    + ' total_price REAL'
    + ');';
  SQLQuery1.ExecSQL;

  SQLQuery1.SQL.Text :=
    'CREATE TABLE IF NOT EXISTS transactions ('
    + ' id INTEGER PRIMARY KEY AUTOINCREMENT,'
    + ' transaction_no VARCHAR(30),'
    + ' transaction_date VARCHAR(20),'
    + ' subtotal REAL,'
    + ' payment_status VARCHAR(20),'
    + ' discount REAL,'
    + ' total REAL,'
    + ' pay_amount REAL,'
    + ' change_amount REAL,'
    + ' payment_type VARCHAR(20),'
    + ' payment_provider VARCHAR(20),'
    + ' card_last4 VARCHAR(10),'
    + ' transaction_type VARCHAR(20) DEFAULT ''sale'','
    + ' return_from_transaction_id INTEGER,'
    + ' user_id INTEGER,'
    + ' cashier_code VARCHAR(6),'
    + ' terminal_id VARCHAR(10),'
    + ' deleted_at VARCHAR(20)'
    + ');';
  SQLQuery1.ExecSQL;

  SQLQuery1.SQL.Text :=
    'CREATE TABLE IF NOT EXISTS transaction_items ('
    + ' id INTEGER PRIMARY KEY AUTOINCREMENT,'
    + ' transaction_id INTEGER,'
    + ' product_code VARCHAR(50),'
    + ' product_name VARCHAR(100),'
    + ' price REAL,'
    + ' qty REAL,'
    + ' discount_value REAL,'
    + ' discount_type VARCHAR(50),'
    + ' total_price REAL,'
    + ' deleted_at VARCHAR(20)'
    + ');';
  SQLQuery1.ExecSQL;

  SQLQuery1.SQL.Text :=
    'CREATE TABLE IF NOT EXISTS products (' +
    ' product_code VARCHAR(13) PRIMARY KEY,' +
    ' altcode VARCHAR(13),' +
    ' product_name VARCHAR(200) NOT NULL,' +
    ' price REAL NOT NULL,' +
    ' price_a REAL DEFAULT 0,' +
    ' qty_a REAL DEFAULT 0,' +
    ' price_b REAL DEFAULT 0,' +
    ' qty_b REAL DEFAULT 0,' +
    ' boom INTEGER DEFAULT 0,' +
    ' qtymeth INTEGER DEFAULT 0,' +
    ' is_active INTEGER DEFAULT 1,' +
    ' updated_at VARCHAR(20)' +
    ');';
  SQLQuery1.ExecSQL;

  SQLQuery1.SQL.Text :=
    'CREATE INDEX IF NOT EXISTS idx_products_altcode ON products(altcode)';
  SQLQuery1.ExecSQL;

  CreateUserTables;
  CreateCashDrawerTable;

  SQLTransaction1.Commit;
end;

procedure TForm1.CreateUserTables;
begin
  // Tabel users
  SQLQuery1.SQL.Text :=
    'CREATE TABLE IF NOT EXISTS users (' +
    ' id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    ' cashier_code VARCHAR(6) NOT NULL UNIQUE,' +
    ' password VARCHAR(6) NOT NULL,' +
    ' name VARCHAR(100) NOT NULL,' +
    ' level VARCHAR(10) NOT NULL,' +
    ' active INTEGER DEFAULT 1,' +
    ' is_default INTEGER DEFAULT 0,' +
    ' created_at VARCHAR(20),' +
    ' updated_at VARCHAR(20)' +
    ');';
  SQLQuery1.ExecSQL;

  // Insert default users jika belum ada
  SQLQuery1.SQL.Text :=
    'INSERT OR IGNORE INTO users (id, cashier_code, password, name, level, is_default, created_at) ' +
    'VALUES (999, ''999999'', ''919191'', ''ADMIN SETUP'', ''admin'', 1, datetime(''now''))';
  SQLQuery1.ExecSQL;

  // Tabel login_history
  SQLQuery1.SQL.Text :=
    'CREATE TABLE IF NOT EXISTS login_history (' +
    ' id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    ' user_id INTEGER,' +
    ' cashier_code VARCHAR(6),' +
    ' login_time VARCHAR(20),' +
    ' logout_time VARCHAR(20),' +
    ' status VARCHAR(20)' +
    ');';
  SQLQuery1.ExecSQL;

end;

procedure TForm1.DoLogin;
var
  IsAdminSetup: Boolean;
  UserCount, ProductCount: Integer;
  ProgressForm: TForm;
  ProgressLabel: TLabel;
  Success: Boolean;
begin
  if FrmLogin = nil then
    FrmLogin := TFrmLogin.Create(Self);

  FrmLogin.Connection := SQLite3Connection1;
  FrmLogin.Transaction := SQLTransaction1;

  if FrmLogin.ShowModal = mrOK then
  begin
    FCurrentUser := FrmLogin.UserInfo;
    FLoginHistoryID := FrmLogin.LoginHistoryID;
    UpdateUserInfoDisplay;

    IsAdminSetup := (FCurrentUser.CashierCode = '999999');

    if IsAdminSetup then
    begin
      // ========== ADMIN SETUP - SYNC USER & PRODUK ==========

      // CEK KONEKSI POSTGRESQL
      if not Assigned(FPostgreSQLManager) or not FPostgreSQLManager.IsConnected then
      begin
        ShowMessage('ERROR!' + sLineBreak + sLineBreak +
                    'Tidak terhubung ke PostgreSQL.' + sLineBreak +
                    'Aplikasi akan ditutup.');
        Application.Terminate;
        Exit;
      end;

      ShowMessage('LOGIN SEBAGAI ADMIN SETUP' + sLineBreak + sLineBreak +
                  'User ini hanya untuk sinkronisasi data.' + sLineBreak +
                  'Proses sinkronisasi akan dimulai...');

      // BUAT PROGRESS FORM
      UserCount := 0;
      ProductCount := 0;
      Success := True;

      ProgressForm := TForm.Create(nil);
      try
        ProgressForm.BorderStyle := bsNone;
        ProgressForm.Width := 500;
        ProgressForm.Height := 180;
        ProgressForm.Position := poScreenCenter;
        ProgressForm.Color := clWhite;

        ProgressLabel := TLabel.Create(ProgressForm);
        ProgressLabel.Parent := ProgressForm;
        ProgressLabel.Align := alClient;
        ProgressLabel.Alignment := taCenter;
        ProgressLabel.Layout := tlCenter;
        ProgressLabel.Font.Size := 14;
        ProgressLabel.Font.Color := clBlack;

        ProgressForm.Show;
        Application.ProcessMessages;

        try
          // ========== 1. SYNC USERS DARI POSTGRESQL ==========
          ProgressLabel.Caption := 'SYNC USER DARI SERVER' + sLineBreak + sLineBreak +
                                   'Mohon tunggu...';
          ProgressLabel.Font.Color := clBlack;
          Application.ProcessMessages;

          try
            SyncUsersFromPostgreSQL;

            // Get user count
            SQLQuery1.SQL.Text := 'SELECT COUNT(*) as cnt FROM users WHERE is_default = 0';
            SQLQuery1.Open;
            UserCount := SQLQuery1.FieldByName('cnt').AsInteger;
            SQLQuery1.Close;

            ProgressLabel.Caption := 'SYNC USER BERHASIL!' + sLineBreak + sLineBreak +
                                     'Total: ' + IntToStr(UserCount) + ' user';
            ProgressLabel.Font.Color := clGreen;
            Application.ProcessMessages;
            Sleep(1000);
          except
            on E: Exception do
            begin
              ProgressLabel.Caption := 'SYNC USER GAGAL!' + sLineBreak + sLineBreak +
                                       E.Message;
              ProgressLabel.Font.Color := clRed;
              Application.ProcessMessages;
              Sleep(2000);
              Success := False;
            end;
          end;

          // ========== 2. SYNC PRODUCTS DARI DBF ==========
          ProgressLabel.Caption := 'SYNC MASTER PRODUK DARI DBF' + sLineBreak + sLineBreak +
                                   'Mohon tunggu...';
          ProgressLabel.Font.Color := clBlack;
          Application.ProcessMessages;

          try
            ProductCount := ReadDBFProducts(FDataDrivePath + '\produk.dbf');

            if ProductCount > 0 then
            begin
              ProgressLabel.Caption := 'SYNC PRODUK BERHASIL!' + sLineBreak + sLineBreak +
                                       'Total: ' + IntToStr(ProductCount) + ' produk';
              ProgressLabel.Font.Color := clGreen;
              Application.ProcessMessages;
              Sleep(1000);
            end
            else
            begin
              ProgressLabel.Caption := 'SYNC PRODUK GAGAL!' + sLineBreak + sLineBreak +
                                       'Tidak ada data produk';
              ProgressLabel.Font.Color := clRed;
              Application.ProcessMessages;
              Sleep(2000);
              Success := False;
            end;
          except
            on E: Exception do
            begin
              ProgressLabel.Caption := 'SYNC PRODUK GAGAL!' + sLineBreak + sLineBreak +
                                       E.Message;
              ProgressLabel.Font.Color := clRed;
              Application.ProcessMessages;
              Sleep(2000);
              Success := False;
            end;
          end;

          // ========== 3. SUMMARY ==========
          if Success then
          begin
            ProgressLabel.Caption := 'SETUP SELESAI!' + sLineBreak + sLineBreak +
                                     'User: ' + IntToStr(UserCount) + sLineBreak +
                                     'Produk: ' + IntToStr(ProductCount);
            ProgressLabel.Font.Color := clGreen;
          end
          else
          begin
            ProgressLabel.Caption := 'SETUP SELESAI DENGAN ERROR!' + sLineBreak + sLineBreak +
                                     'Cek pesan error di atas';
            ProgressLabel.Font.Color := clRed;
          end;
          Application.ProcessMessages;
          Sleep(2000);

        finally
          ProgressForm.Close;
        end;

      finally
        ProgressForm.Free;
      end;

      // SHOW FINAL SUMMARY
      if Success then
      begin
        ShowMessage('SETUP SELESAI!' + sLineBreak + sLineBreak +
                    'User tersinkron: ' + IntToStr(UserCount) + sLineBreak +
                    'Produk tersinkron: ' + IntToStr(ProductCount) + sLineBreak + sLineBreak +
                    'Aplikasi akan ditutup.' + sLineBreak +
                    'Login kembali dengan user normal untuk transaksi.');
      end
      else
      begin
        ShowMessage('SETUP SELESAI DENGAN ERROR!' + sLineBreak + sLineBreak +
                    'User tersinkron: ' + IntToStr(UserCount) + sLineBreak +
                    'Produk tersinkron: ' + IntToStr(ProductCount) + sLineBreak + sLineBreak +
                    'Periksa koneksi dan coba lagi.');
      end;

      Application.Terminate;
      Halt;
      // ========== END ADMIN SETUP ==========
    end
    else
    begin
      // ========== USER NORMAL - INFO MODE ==========
      if Assigned(FPostgreSQLManager) and FPostgreSQLManager.IsConnected then
      begin
        // ONLINE MODE
        ShowMessage('Selamat datang, ' + FCurrentUser.Name + '!' + sLineBreak + sLineBreak +
                    'Mode: ONLINE + SYNC' + sLineBreak +
                    'Data akan tersinkron ke server.');
      end
      else
      begin
        // OFFLINE MODE
        ShowMessage('Selamat datang, ' + FCurrentUser.Name + '!' + sLineBreak + sLineBreak +
                    'Mode: OFFLINE (LOCAL ONLY)' + sLineBreak +
                    'Server tidak terhubung.' + sLineBreak +
                    'Transaksi akan disimpan lokal dan disinkron saat online.');
      end;
      // ========== END INFO ==========
    end;
  end
  else
  begin
    Application.Terminate;
  end;
end;

procedure TForm1.DoLogout;
var
  TimeStr: string;
begin
  if FLoginHistoryID > 0 then
  begin
    TimeStr := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

    try
      SQLQuery1.Close;
      SQLQuery1.SQL.Text :=
        'UPDATE login_history SET ' +
        'logout_time = :time, status = ''logout'' ' +
        'WHERE id = :id';
      SQLQuery1.ParamByName('time').AsString := TimeStr;
      SQLQuery1.ParamByName('id').AsInteger := FLoginHistoryID;
      SQLQuery1.ExecSQL;

      if SQLTransaction1.Active then
        SQLTransaction1.Commit;
    except
      on E: Exception do
        ShowMessage('Error saving logout: ' + E.Message);
    end;
  end;
end;

procedure TForm1.UpdateUserInfoDisplay;
var
  LevelStr: string;
begin
  if FCurrentUser.Level = ulAdmin then
    LevelStr := 'ADMIN'
  else
    LevelStr := 'KASIR';

  // Update status bar dengan info user
  lblAppTitle.Caption := 'Toserba Jadi Baru - ' + FCurrentUser.Name +
                         ' [' + LevelStr + '] | Terminal ' + TERMINAL_ID;
end;

procedure TForm1.CreateCashDrawerTable;
begin
  SQLQuery1.SQL.Text :=
    'CREATE TABLE IF NOT EXISTS cash_drawer (' +
    ' id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    ' date VARCHAR(10),' +
    ' opening_cash REAL,' +
    ' opening_time VARCHAR(20),' +
    ' closing_cash REAL,' +
    ' closing_time VARCHAR(20),' +
    ' expected_cash REAL,' +
    ' cash_sales REAL,' +
    ' cash_returns REAL,' +
    ' difference REAL,' +
    ' notes TEXT,' +
    ' status VARCHAR(20),' +
    ' user_id INTEGER,' +
    ' cashier_code VARCHAR(6),' +
    ' terminal_id VARCHAR(10)' +
    ');';
  SQLQuery1.ExecSQL;
end;

function TForm1.IsCashDrawerOpen: Boolean;
var
  TodayStr: string;
begin
  Result := False;
  TodayStr := FormatDateTime('yyyy-mm-dd', Now);

  try
    SQLQuery1.Close;
    SQLQuery1.SQL.Text :=
      'SELECT id FROM cash_drawer ' +
      'WHERE date = :dt AND terminal_id = :term AND status = ''open''';
    SQLQuery1.ParamByName('dt').AsString := TodayStr;
    SQLQuery1.ParamByName('term').AsString := TERMINAL_ID;

    SQLQuery1.Open;

    if not SQLQuery1.EOF then
    begin
      Result := True;
      FCurrentDrawerID := SQLQuery1.FieldByName('id').AsInteger;
    end
    else
    begin
      FCurrentDrawerID := 0;
    end;

    SQLQuery1.Close;
  except
    on E: Exception do
      ShowMessage('Error checking cash drawer: ' + E.Message);
  end;
end;

procedure TForm1.CheckAndOpenCashDrawer;
begin
  if not IsCashDrawerOpen then
  begin
    if FrmModalAwal = nil then
      FrmModalAwal := TFrmModalAwal.Create(Self);

    FrmModalAwal.Connection := SQLite3Connection1;
    FrmModalAwal.Transaction := SQLTransaction1;

    if FrmModalAwal.ShowModal = mrOK then
    begin
      try
        SQLQuery1.Close;
        SQLQuery1.SQL.Text :=
          'UPDATE cash_drawer SET user_id = :uid, cashier_code = :code, terminal_id = :term ' +
          'WHERE id = (SELECT MAX(id) FROM cash_drawer)';
        SQLQuery1.ParamByName('uid').AsInteger := FCurrentUser.ID;
        SQLQuery1.ParamByName('code').AsString := FCurrentUser.CashierCode;
        SQLQuery1.ParamByName('term').AsString := TERMINAL_ID;
        SQLQuery1.ExecSQL;

        if SQLTransaction1.Active then
          SQLTransaction1.Commit;
      except
        // Silent fail
      end;

      IsCashDrawerOpen;
      ShowMessage('Modal awal berhasil dibuka!' + sLineBreak +
                  'Terminal: ' + TERMINAL_ID + ' - ' + TERMINAL_NAME + sLineBreak +
                  'Rp ' + FormatFloat('#,##0', FrmModalAwal.OpeningCash));
    end
    else
    begin
      ShowMessage('Aplikasi akan ditutup karena modal belum dibuka.');
      Application.Terminate;
    end;
  end;
end;

procedure TForm1.UpdateCashDrawerSales(Amount: Double; IsReturn: Boolean);
var
  FieldName: string;
begin
  if FCurrentDrawerID = 0 then
    Exit;

  try
    if IsReturn then
      FieldName := 'cash_returns'
    else
      FieldName := 'cash_sales';

    SQLQuery1.Close;
    SQLQuery1.SQL.Text :=
      'UPDATE cash_drawer SET ' +
      FieldName + ' = ' + FieldName + ' + :amt, ' +
      'expected_cash = opening_cash + cash_sales - cash_returns ' +
      'WHERE id = :id';
    SQLQuery1.ParamByName('amt').AsFloat := Amount;
    SQLQuery1.ParamByName('id').AsInteger := FCurrentDrawerID;
    SQLQuery1.ExecSQL;

    if SQLTransaction1.Active then
      SQLTransaction1.Commit;
  except
    on E: Exception do
      ShowMessage('Error updating cash drawer: ' + E.Message);
  end;
end;


{ --------------------------------------------------------------- }
{ LOAD CART }
{ --------------------------------------------------------------- }
procedure TForm1.LoadCartTemp;
begin
  try
    SQLQuery1.Close;
    SQLQuery1.SQL.Text :=
      'SELECT product_code, product_name, price, qty, discount_value, discount_type, total_price '
      + 'FROM cart_temp ORDER BY id';
    SQLQuery1.Open;

    DBGrid1.Refresh;

    UpdateSubtotal;
  except
    on E: Exception do
      ShowMessage('Error loading cart: ' + E.Message);
  end;
end;

{ --------------------------------------------------------------- }
{ UPDATE SUBTOTAL }
{ --------------------------------------------------------------- }
procedure TForm1.UpdateSubtotal;
var
  TempQuery: TSQLQuery;
  Ttl: Double;
begin
  TempQuery := TSQLQuery.Create(nil);
  try
    TempQuery.Database := SQLite3Connection1;
    TempQuery.SQL.Text := 'SELECT COALESCE(SUM(total_price), 0) AS ttl FROM cart_temp';
    TempQuery.Open;
    Ttl := TempQuery.FieldByName('ttl').AsFloat;
    TempQuery.Close;

    lSubTtl.Caption := 'Rp ' + FormatFloat('#,##0', Ttl);
  finally
    TempQuery.Free;
  end;
end;

{ --------------------------------------------------------------- }
{ UTILITY FUNCTIONS }
{ --------------------------------------------------------------- }
function TForm1.ReadFileToString(const FileName: string): string;
var
  F: TFileStream;
  S: TStringStream;
begin
  Result := '';
  if not FileExists(FileName) then Exit;

  F := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  S := TStringStream.Create('');
  try
    S.CopyFrom(F, F.Size);
    Result := S.DataString;
  finally
    F.Free;
    S.Free;
  end;
end;

function TForm1.GenerateCancelNo: string;
begin
  Result := 'CANCEL-' + FormatDateTime('yyyymmddhhnnss', Now);
end;

function TForm1.GenerateTransactionNo: string;
var
  MaxNo, NoUrut: Integer;
  TodayStr: string;
begin
  TodayStr := FormatDateTime('yyyymmdd', Now);

  SQLQuery1.Close;
  SQLQuery1.SQL.Text :=
    'SELECT MAX(CAST(substr(transaction_no, 18, 4) AS INTEGER)) AS max_no ' +
    'FROM transactions ' +
    'WHERE substr(transaction_no, 1, 3) = ''TRX'' ' +
    '  AND substr(transaction_no, 5, 8) = :tgl ' +
    '  AND substr(transaction_no, 14, 3) = :term ' +
    '  AND deleted_at IS NULL';
  SQLQuery1.ParamByName('tgl').AsString := TodayStr;
  SQLQuery1.ParamByName('term').AsString := TERMINAL_ID;
  SQLQuery1.Open;

  if SQLQuery1.FieldByName('max_no').IsNull then
    MaxNo := 0
  else
    MaxNo := SQLQuery1.FieldByName('max_no').AsInteger;

  SQLQuery1.Close;

  NoUrut := MaxNo + 1;

  Result := 'TRX-' + TodayStr + '-' + TERMINAL_ID + '-' + FormatFloat('0000', NoUrut);
end;

{ --------------------------------------------------------------- }
{ CLEAR CART WITH LOG }
{ --------------------------------------------------------------- }
procedure TForm1.ClearCartWithLog;
var
  CancelID: Integer;
  SubTotal: Double;
  DelTime: string;
begin
  try
    SQLQuery1.Close;
    SQLQuery1.SQL.Text := 'SELECT COALESCE(SUM(total_price), 0) AS ttl FROM cart_temp';
    SQLQuery1.Open;
    SubTotal := SQLQuery1.FieldByName('ttl').AsFloat;
    SQLQuery1.Close;

    if SubTotal = 0 then begin
      LoadCartTemp;
      Exit;
    end;

    DelTime := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

    if not SQLTransaction1.Active then
      SQLTransaction1.StartTransaction;

    try
      SQLQuery1.SQL.Text :=
        'INSERT INTO transactions (transaction_no, transaction_date, subtotal, payment_status, discount, total, deleted_at) '
        + 'VALUES (:no, :dt, :sttl, "void", 0, :ttl, :del)';
      SQLQuery1.ParamByName('no').AsString := GenerateCancelNo;
      SQLQuery1.ParamByName('dt').AsString := DelTime;
      SQLQuery1.ParamByName('sttl').AsFloat := SubTotal;
      SQLQuery1.ParamByName('ttl').AsFloat := SubTotal;
      SQLQuery1.ParamByName('del').AsString := DelTime;
      SQLQuery1.ExecSQL;

      SQLQuery1.SQL.Text := 'SELECT last_insert_rowid() AS id';
      SQLQuery1.Open;
      CancelID := SQLQuery1.FieldByName('id').AsInteger;
      SQLQuery1.Close;

      SQLQuery1.SQL.Text :=
        'INSERT INTO transaction_items '
        + '(transaction_id, product_code, product_name, price, qty, discount_value, discount_type, total_price, deleted_at) '
        + 'SELECT :id, product_code, product_name, price, qty, discount_value, discount_type, total_price, :del FROM cart_temp';
      SQLQuery1.ParamByName('id').AsInteger := CancelID;
      SQLQuery1.ParamByName('del').AsString := DelTime;
      SQLQuery1.ExecSQL;

      SQLQuery1.SQL.Text := 'DELETE FROM cart_temp';
      SQLQuery1.ExecSQL;

      SQLTransaction1.Commit;
      LoadCartTemp;
    except
      on E: Exception do begin
        if SQLTransaction1.Active then
          SQLTransaction1.Rollback;
        LoadCartTemp;
        ShowMessage('Error clearing cart: ' + E.Message);
      end;
    end;
  except
    on E: Exception do begin
      LoadCartTemp;
      ShowMessage('Error: ' + E.Message);
    end;
  end;
end;

{ --------------------------------------------------------------- }
{ PRODUCT SEARCH }
{ --------------------------------------------------------------- }
function TForm1.SearchProduct(const ACode: string): TProductInfo;
var
  Proc: TProcess;
  Act: string;
  JsonPath, BasePath, ExecPath: string;
  JSON: TJSONData;
  Obj: TJSONObject;
  Arr: TJSONArray;
  S: string;
  CodeLen: Integer;
  TempQuery: TSQLQuery;
  FoundInSQLite: Boolean;
begin
  Result.Found := False;
  Result.Boom := False;
  Result.QtyMeth := False;
  Result.QtyA := 0;
  Result.PriceA := 0;
  Result.QtyB := 0;
  Result.PriceB := 0;
  Result.Code := '';
  Result.Name := '';
  Result.Price := 0;

  if Trim(ACode) = '' then Exit;

  FoundInSQLite := False;

  try
    // ========== PRIORITAS 1: CEK SQLITE LOKAL ==========
    TempQuery := TSQLQuery.Create(nil);
    try
      TempQuery.Database := SQLite3Connection1;
      TempQuery.Transaction := SQLTransaction1;

      TempQuery.SQL.Text :=
        'SELECT product_code, altcode, product_name, price, ' +
        '       price_a, qty_a, price_b, qty_b, boom, qtymeth ' +
        'FROM products ' +
        'WHERE (product_code = :code OR altcode = :code) ' +
        '  AND is_active = 1 ' +
        'LIMIT 1';
      TempQuery.ParamByName('code').AsString := UpperCase(ACode);

      try
        TempQuery.Open;

        if not TempQuery.EOF then
        begin
          // Found in SQLite!
          Result.Found := True;
          Result.Code := TempQuery.FieldByName('product_code').AsString;
          Result.Name := TempQuery.FieldByName('product_name').AsString;
          Result.Price := TempQuery.FieldByName('price').AsFloat;
          Result.Boom := (TempQuery.FieldByName('boom').AsInteger = 1);
          Result.QtyMeth := (TempQuery.FieldByName('qtymeth').AsInteger = 1);
          Result.QtyA := TempQuery.FieldByName('qty_a').AsFloat;
          Result.PriceA := TempQuery.FieldByName('price_a').AsFloat;
          Result.QtyB := TempQuery.FieldByName('qty_b').AsFloat;
          Result.PriceB := TempQuery.FieldByName('price_b').AsFloat;

          FoundInSQLite := True;  // ← SET FLAG, jangan FREE dan EXIT di sini
        end;

        if TempQuery.Active then
          TempQuery.Close;

      except
        on E: Exception do
        begin
          // Error SQLite - lanjut ke DBF
          if TempQuery.Active then
            TempQuery.Close;
        end;
      end;

    finally
      TempQuery.Free;  // ← FREE di sini (1x aja)
    end;
    // ========== END SQLITE CHECK ==========

    // Jika sudah ketemu di SQLite, return
    if FoundInSQLite then
      Exit;

    // ========== PRIORITAS 2: CEK DBF (FALLBACK) ==========
    if not FDriveOnline then
    begin
      ShowMessage('PRODUK TIDAK DITEMUKAN!' + sLineBreak + sLineBreak +
                  'Produk tidak ada di database lokal.' + sLineBreak +
                  'Drive data offline, tidak bisa cari di DBF.');
      Exit;
    end;

    CodeLen := Length(ACode);

    if (CodeLen >= MIN_CODE_LENGTH) and (CodeLen <= MAX_SHORT_CODE) then
      Act := 'c'
    else if (CodeLen >= MIN_BARCODE_LENGTH) and (CodeLen <= MAX_BARCODE_LENGTH) then
      Act := 'b'
    else
      Act := '';

    BasePath := IncludeTrailingPathDelimiter(
      ExtractFilePath(Application.ExeName) + 'hb30'
    );

    JsonPath := BasePath + UpperCase(ACode) + '.json';
    ExecPath := BasePath + 'seekprod.exe';

    if FileExists(ExecPath) then
    begin
      Proc := TProcess.Create(nil);
      try
        Proc.Executable := ExecPath;
        if Act <> '' then
          Proc.Parameters.Add(Act);
        Proc.Parameters.Add(ACode);
        Proc.CurrentDirectory := BasePath;
        Proc.Options := [poWaitOnExit, poNoConsole];
        Proc.Execute;
      finally
        Proc.Free;
      end;
    end;

    if not FileExists(JsonPath) then Exit;

    S := ReadFileToString(JsonPath);
    if Trim(S) = '' then Exit;

    JSON := nil;
    try
      JSON := GetJSON(S);

      case JSON.JSONType of
        jtObject: begin
          Obj := TJSONObject(JSON);
          Result.Found := True;
          Result.Code := Obj.Get('code', '');
          Result.Name := Obj.Get('desc', Obj.Get('name', ''));
          Result.Price := Obj.Get('price', 0.0);
          Result.Boom := Obj.Get('boom', False);
          Result.QtyMeth := Obj.Get('qtymeth', False);
          Result.QtyA := Obj.Get('qty_a', 0.0);
          Result.PriceA := Obj.Get('price_a', 0.0);
          Result.QtyB := Obj.Get('qty_b', 0.0);
          Result.PriceB := Obj.Get('price_b', 0.0);
        end;

        jtArray: begin
          Arr := TJSONArray(JSON);
          if Arr.Count > 0 then
          begin
            Obj := Arr.Objects[0];
            Result.Found := True;
            Result.Code := Obj.Get('code', '');
            Result.Name := Obj.Get('desc', Obj.Get('name', ''));
            Result.Price := Obj.Get('price', 0.0);
            Result.Boom := Obj.Get('boom', False);
            Result.QtyMeth := Obj.Get('qtymeth', False);
            Result.QtyA := Obj.Get('qty_a', 0.0);
            Result.PriceA := Obj.Get('price_a', 0.0);
            Result.QtyB := Obj.Get('qty_b', 0.0);
            Result.PriceB := Obj.Get('price_b', 0.0);
          end;
        end;
      end;
    finally
      if Assigned(JSON) then
        JSON.Free;
      if FileExists(JsonPath) then
        DeleteFile(JsonPath);
    end;

  except
    on E: Exception do
    begin
      Result.Found := False;
      ShowMessage('Error searching product: ' + E.Message);
    end;
  end;
end;

function TForm1.GetPriceByQty(const Info: TProductInfo; AQty: Double): Double;
begin
  Result := Info.Price; // Default harga normal

  // Validasi input
  if AQty <= 0 then
    Exit;

  if Info.Price < 0 then
  begin
    Result := 0;
    Exit;
  end;

  // Cek apakah produk memiliki harga grosir
  if Info.Boom and Info.QtyMeth then
  begin
    // Cek qty_b
    if (Info.QtyB > 0) and (AQty > Info.QtyB) and (Info.PriceB > 0) then
      Result := Info.PriceB
    // Cek qty_a
    else if (Info.QtyA > 0) and (AQty > Info.QtyA) and (Info.PriceA > 0) then
      Result := Info.PriceA;
  end;
end;

{ --------------------------------------------------------------- }
{ ADD TO CART }
{ --------------------------------------------------------------- }
procedure TForm1.AddToCart(const Info: TProductInfo);
begin
  AddToCartWithQty(Info, 1);
end;

procedure TForm1.AddToCartWithQty(const Info: TProductInfo; AQty: Integer);
var
  OldQty, NewQty, Total, ActualPrice: Double;
begin
  // Validasi input
  if not Info.Found then
  begin
    ShowMessage('Produk tidak valid!');
    Exit;
  end;

  if Trim(Info.Code) = '' then
  begin
    ShowMessage('Kode produk kosong!');
    Exit;
  end;

  if AQty <= 0 then
  begin
    ShowMessage('Quantity harus lebih dari 0!');
    Exit;
  end;

  if Info.Price < 0 then
  begin
    ShowMessage('Harga produk tidak valid!');
    Exit;
  end;

  try
    SQLQuery1.Close;
    SQLQuery1.SQL.Text := 'SELECT id, qty FROM cart_temp WHERE product_code = :c';
    SQLQuery1.ParamByName('c').AsString := Info.Code;
    SQLQuery1.Open;

    if not SQLQuery1.EOF then
    begin
      // Update existing item
      OldQty := SQLQuery1.FieldByName('qty').AsFloat;
      NewQty := OldQty + AQty;

      // Hitung harga berdasarkan qty total
      ActualPrice := GetPriceByQty(Info, NewQty);

      if ActualPrice < 0 then
        ActualPrice := Info.Price;

      Total := NewQty * ActualPrice;

      SQLQuery1.Close;

      SQLQuery1.SQL.Text := 'DELETE FROM cart_temp WHERE product_code = :c';
      SQLQuery1.ParamByName('c').AsString := Info.Code;
      SQLQuery1.ExecSQL;

      SQLQuery1.SQL.Text :=
        'INSERT INTO cart_temp (product_code, product_name, price, qty, discount_value, discount_type, total_price) '
        + 'VALUES (:c, :n, :p, :q, 0, '''', :t)';
      SQLQuery1.ParamByName('c').AsString := Info.Code;
      SQLQuery1.ParamByName('n').AsString := Info.Name;
      SQLQuery1.ParamByName('p').AsFloat := ActualPrice;
      SQLQuery1.ParamByName('q').AsFloat := NewQty;
      SQLQuery1.ParamByName('t').AsFloat := Total;
      SQLQuery1.ExecSQL;
    end
    else
    begin
      // Add new item
      SQLQuery1.Close;

      ActualPrice := GetPriceByQty(Info, AQty);

      if ActualPrice < 0 then
        ActualPrice := Info.Price;

      SQLQuery1.SQL.Text :=
        'INSERT INTO cart_temp (product_code, product_name, price, qty, discount_value, discount_type, total_price) '
        + 'VALUES (:c, :n, :p, :q, 0, '''', :t)';
      SQLQuery1.ParamByName('c').AsString := Info.Code;
      SQLQuery1.ParamByName('n').AsString := Info.Name;
      SQLQuery1.ParamByName('p').AsFloat := ActualPrice;
      SQLQuery1.ParamByName('q').AsFloat := AQty;
      SQLQuery1.ParamByName('t').AsFloat := ActualPrice * AQty;
      SQLQuery1.ExecSQL;
    end;

    LoadCartTemp;

    if SQLQuery1.Active and (SQLQuery1.RecordCount > 0) then
    begin
      SQLQuery1.Last;
      DBGrid1.Refresh;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Error adding to cart: ' + E.Message);
      LoadCartTemp; // Reload cart untuk safety
    end;
  end;
end;

procedure TForm1.ProcessInputWithQty(const ACode: string);
var
  Info: TProductInfo;
  QtyFinal: Integer;
begin
  Info := SearchProduct(ACode);

  if not Info.Found then begin
    ShowMessage('Produk tidak ditemukan!');
    Exit;
  end;

  if FQtyMode then
    QtyFinal := FPendingQty
  else
    QtyFinal := 1;

  AddToCartWithQty(Info, QtyFinal);
end;

{ --------------------------------------------------------------- }
{ DISCOUNT }
{ --------------------------------------------------------------- }
procedure TForm1.ApplyDiscountToLastItem(Value: Double; IsPercent: Boolean);
var
  ID: Integer;
  Qty, Price, SubTotal, Diskon, NewTotal: Double;
  DiscType: string;
begin
  try
    SQLQuery1.Close;
    SQLQuery1.SQL.Text := 'SELECT id, price, qty FROM cart_temp ORDER BY id DESC LIMIT 1';
    SQLQuery1.Open;

    if SQLQuery1.EOF then begin
      ShowMessage('Tidak ada item untuk diskon.');
      Exit;
    end;

    ID := SQLQuery1.FieldByName('id').AsInteger;
    Price := SQLQuery1.FieldByName('price').AsFloat;
    Qty := SQLQuery1.FieldByName('qty').AsFloat;
    SubTotal := Price * Qty;

    if IsPercent then
    begin
      if (Value >= 0) and (Value <= 100) then
      begin
        Diskon := SubTotal * (Value / 100);
        DiscType := FloatToStr(Value) + '%';
      end
      else
      begin
        ShowMessage('Nominal diskon persen tidak sesuai!');
        LoadCartTemp;
        Exit;
      end;
    end
    else
    begin
      if (Value >= 0) and (Value <= SubTotal) then
      begin
        Diskon := Value;
        DiscType := 'Rp ' + FormatFloat('#,##0', (Value / Qty));
      end
      else
      begin
        ShowMessage('Nominal diskon rupiah tidak sesuai!');
        LoadCartTemp;
        Exit;
      end;
    end;

    NewTotal := SubTotal - Diskon;

    SQLQuery1.Close;
    SQLQuery1.SQL.Text :=
      'UPDATE cart_temp SET discount_value = :d, discount_type = :t, total_price = :tot WHERE id = :id';
    SQLQuery1.ParamByName('d').AsFloat := Diskon;
    SQLQuery1.ParamByName('t').AsString := DiscType;
    SQLQuery1.ParamByName('tot').AsFloat := NewTotal;
    SQLQuery1.ParamByName('id').AsInteger := ID;
    SQLQuery1.ExecSQL;

    LoadCartTemp;
  except
    on E: Exception do
      ShowMessage('Error applying discount: ' + E.Message);
  end;
end;

{ --------------------------------------------------------------- }
{ PAYMENT VALIDATION }
{ --------------------------------------------------------------- }
function TForm1.ValidatePaymentAmount(const PayInput: string; SubTotal: Double; out PayAmount: Double): Boolean;
var
  CleanInput: string;
begin
  Result := False;

  if Trim(PayInput) = '' then begin
    PayAmount := SubTotal;
    Result := True;
    Exit;
  end;

  CleanInput := StringReplace(PayInput, '.', '', [rfReplaceAll]);
  CleanInput := StringReplace(CleanInput, ',', '.', [rfReplaceAll]);

  if not TryStrToFloat(CleanInput, PayAmount) then begin
    ShowMessage('Format nominal tidak valid!');
    Exit;
  end;

  if PayAmount < SubTotal then begin
    ShowMessage('Nominal pembayaran kurang dari subtotal!');
    Exit;
  end;

  Result := True;
end;

{ --------------------------------------------------------------- }
{ PAYMENT PROCESS }
{ --------------------------------------------------------------- }
procedure TForm1.ProcessPayment(PayType, Provider, Last4, PayInput: string);
var
  PayAmount, SubTotal, Change: Double;
  TID: Integer;
  TrNo, TrDate: string;
  TrxData: TJSONObject;
  ItemsArray: TJSONArray;
  ItemQuery: TSQLQuery;
  ItemObj: TJSONObject;
begin
  try
    if SQLQuery1.Active then
      SQLQuery1.Close;

    SQLQuery1.SQL.Text := 'SELECT COALESCE(SUM(total_price), 0) AS ttl FROM cart_temp';
    SQLQuery1.Open;
    try
      SubTotal := SQLQuery1.FieldByName('ttl').AsFloat;
    finally
      SQLQuery1.Close;
    end;

    if SubTotal <= 0 then begin
      ShowMessage('Keranjang kosong.');
      LoadCartTemp;
      Exit;
    end;

    if PayType = 'cash' then begin
      if not ValidatePaymentAmount(PayInput, SubTotal, PayAmount) then begin
        LoadCartTemp;
        Exit;
      end;
      Change := PayAmount - SubTotal;
    end else begin
      PayAmount := SubTotal;
      Change := 0;
    end;

    TrNo := GenerateTransactionNo;
    TrDate := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

    if not SQLTransaction1.Active then
       SQLTransaction1.StartTransaction;
    try
      SQLQuery1.SQL.Text :=
        'INSERT INTO transactions (transaction_no, transaction_date, subtotal, discount, total, pay_amount, ' +
        'change_amount, payment_status, payment_type, payment_provider, card_last4, ' +
        'user_id, cashier_code, terminal_id, deleted_at) ' +
        'VALUES (:no, :dt, :sub, 0, :tot, :pay, :chg, :pysts, :ptype, :prov, :card, :uid, :code, :term, NULL)';

      SQLQuery1.ParamByName('no').AsString := TrNo;
      SQLQuery1.ParamByName('dt').AsString := TrDate;
      SQLQuery1.ParamByName('sub').AsFloat := SubTotal;
      SQLQuery1.ParamByName('tot').AsFloat := SubTotal;
      SQLQuery1.ParamByName('pay').AsFloat := PayAmount;
      SQLQuery1.ParamByName('chg').AsFloat := Change;
      SQLQuery1.ParamByName('pysts').AsString := 'sale';
      SQLQuery1.ParamByName('ptype').AsString := PayType;
      SQLQuery1.ParamByName('prov').AsString := Provider;
      SQLQuery1.ParamByName('card').AsString := Last4;
      SQLQuery1.ParamByName('uid').AsInteger := FCurrentUser.ID;
      SQLQuery1.ParamByName('code').AsString := FCurrentUser.CashierCode;
      SQLQuery1.ParamByName('term').AsString := TERMINAL_ID;
      SQLQuery1.ExecSQL;

      SQLQuery1.SQL.Text := 'SELECT last_insert_rowid() AS id';
      SQLQuery1.Open;
      try
        TID := SQLQuery1.FieldByName('id').AsInteger;
      finally
        SQLQuery1.Close;
      end;

      SQLQuery1.SQL.Text :=
        'INSERT INTO transaction_items ' +
        '(transaction_id, product_code, product_name, price, qty, discount_value, discount_type, total_price, deleted_at) ' +
        'SELECT :id, product_code, product_name, price, qty, discount_value, discount_type, total_price, NULL FROM cart_temp';
      SQLQuery1.ParamByName('id').AsInteger := TID;
      SQLQuery1.ExecSQL;

      SQLQuery1.SQL.Text := 'DELETE FROM cart_temp';
      SQLQuery1.ExecSQL;

      SQLTransaction1.Commit;

      // ========== SYNC TO POSTGRESQL ==========
      try
        if Assigned(FPostgreSQLManager) and FPostgreSQLManager.IsConnected then
        begin
          TrxData := TJSONObject.Create;
          ItemsArray := TJSONArray.Create;
          ItemQuery := TSQLQuery.Create(nil);
          try
            ItemQuery.Database := SQLite3Connection1;
            ItemQuery.Transaction := SQLTransaction1;

            // Get items dari transaction yang baru disimpan
            ItemQuery.SQL.Text :=
              'SELECT product_code, product_name, price, qty, discount_value, discount_type, total_price ' +
              'FROM transaction_items WHERE transaction_id = :tid';
            ItemQuery.ParamByName('tid').AsInteger := TID;
            ItemQuery.Open;

            while not ItemQuery.EOF do
            begin
              ItemObj := TJSONObject.Create;
              ItemObj.Add('product_code', ItemQuery.FieldByName('product_code').AsString);
              ItemObj.Add('product_name', ItemQuery.FieldByName('product_name').AsString);
              ItemObj.Add('qty', ItemQuery.FieldByName('qty').AsInteger);
              ItemObj.Add('price', ItemQuery.FieldByName('price').AsFloat);
              ItemObj.Add('discount_value', ItemQuery.FieldByName('discount_value').AsFloat);
              ItemObj.Add('discount_type', ItemQuery.FieldByName('discount_type').AsString);
              ItemObj.Add('total_price', ItemQuery.FieldByName('total_price').AsFloat);

              ItemsArray.Add(ItemObj);
              ItemQuery.Next;
            end;

            ItemQuery.Close;

            // Build transaction data
            TrxData.Add('user_id', FCurrentUser.ID);
            TrxData.Add('total_amount', SubTotal);
            TrxData.Add('payment_type', PayType);
            TrxData.Add('payment_bank', Provider);
            TrxData.Add('payment_last4', Last4);
            TrxData.Add('payment_amount', PayAmount);
            TrxData.Add('change_amount', Change);
            TrxData.Add('is_return', False);
            TrxData.Add('items', ItemsArray);

            // Try sync immediately
            TrySyncTransaction(TrNo, TrxData);

          finally
            ItemQuery.Free;
            TrxData.Free;
          end;
        end;
      except
        // Silent - sync failure tidak boleh block transaksi
      end;
      // ========== END SYNC ==========

      // Update cash drawer jika pembayaran cash
      if PayType = 'cash' then
        UpdateCashDrawerSales(SubTotal, False);

      LoadCartTemp;
      edtMainInput.Clear;
      lQtyInfo.Caption := '';

      if (PayType = 'cash') and (Change > 0) then
        ShowMessage('Pembayaran berhasil!' + sLineBreak + 'Kembalian: Rp ' + FormatFloat('#,##0', Change))
      else
        ShowMessage('Pembayaran berhasil!' + sLineBreak + 'No. Transaksi: ' + TrNo);

    except
      on E: Exception do
      begin
        if SQLTransaction1.Active then
          SQLTransaction1.Rollback;
        LoadCartTemp;
        ShowMessage('Error processing payment: ' + E.Message);
      end;
    end;
  except
    on E: Exception do
    begin
      LoadCartTemp;
      ShowMessage('Error: ' + E.Message);
    end;
  end;
end;

{ --------------------------------------------------------------- }
{ KEYBOARD HANDLERS }
{ --------------------------------------------------------------- }
procedure TForm1.HandleSearchByName;
begin
  if FrmSearchName = nil then
    FrmSearchName := TFrmSearchName.Create(Self);

  // Pass SQLite connection
  FrmSearchName.Connection := SQLite3Connection1;
  FrmSearchName.Transaction := SQLTransaction1;

  if FrmSearchName.ShowModal = mrOK then
  begin
    if FrmSearchName.SelectedProduct.Found then
      AddToCart(FrmSearchName.SelectedProduct);
  end;
end;

procedure TForm1.HandleDiscountPercent;
var
  Value: Double;
begin
  Value := StrToFloatDef(edtMainInput.Text, 0);
  if Value > 0 then
    ApplyDiscountToLastItem(Value, True);
  edtMainInput.Clear;
end;

procedure TForm1.HandleDiscountNominal;
var
  Value: Double;
begin
  Value := StrToFloatDef(edtMainInput.Text, 0);
  if Value > 0 then
    ApplyDiscountToLastItem(Value, False);
  edtMainInput.Clear;
end;

procedure TForm1.HandleDebitPayment;
var
  Bank, Last4: string;
  PayAmount: Int64;
  SubTotal: Double;
begin
  try
    SQLQuery1.Close;
    SQLQuery1.SQL.Text := 'SELECT COALESCE(SUM(total_price), 0) AS ttl FROM cart_temp';
    SQLQuery1.Open;
    SubTotal := SQLQuery1.FieldByName('ttl').AsFloat;
    SQLQuery1.Close;

    if SubTotal <= 0 then begin
      ShowMessage('Keranjang masih kosong!');
      LoadCartTemp;
      Exit;
    end;

    if FrmDebit = nil then
      FrmDebit := TFrmDebit.Create(Self);

    FrmDebit.SetTotal(SubTotal);

    if FrmDebit.GetPaymentInfo(Bank, Last4, PayAmount) then
      ProcessPayment('debit', Bank, Last4, IntToStr(PayAmount))
    else
      LoadCartTemp;
  except
    on E: Exception do begin
      LoadCartTemp;
      ShowMessage('Error: ' + E.Message);
    end;
  end;
end;

procedure TForm1.HandleCashPayment;
begin
  ProcessPayment('cash', '', '', edtMainInput.Text);
end;

{ --------------------------------------------------------------- }
{ FORM KEYDOWN }
{ --------------------------------------------------------------- }
procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // Block semua shortcut untuk Admin Setup
  if FCurrentUser.CashierCode = '999999' then
  begin
    ShowMessage('ADMIN SETUP tidak bisa melakukan transaksi!' + sLineBreak + sLineBreak +
                'Silakan logout dan login dengan user normal.');
    Exit;
  end;

  case Key of
    KEY_CLEAR_CART: begin
      if MessageDlg('Hapus semua item keranjang?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        ClearCartWithLog;
      Exit;
    end;

    KEY_SEARCH_NAME: begin
      HandleSearchByName;
      Exit;
    end;

    KEY_DISC_PERCENT: begin
      HandleDiscountPercent;
      Exit;
    end;

    KEY_DISC_NOMINAL: begin
      HandleDiscountNominal;
      Exit;
    end;

    KEY_PAY_DEBIT: begin
      HandleDebitPayment;
      Exit;
    end;

    KEY_PAY_CASH: begin
      HandleCashPayment;
      Exit;
    end;

    KEY_FOCUS_INPUT: begin
      edtMainInput.SetFocus;
    end;

    KEY_RETURN: begin
      HandleReturn;
      Exit;
    end;

    KEY_EOD: begin
      HandleEndOfDay;
      Exit;
    end;

  end;
end;

{ --------------------------------------------------------------- }
{ INPUT KEYDOWN }
{ --------------------------------------------------------------- }
procedure TForm1.edtMainInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    KEY_CLEAR_INPUT: begin
      edtMainInput.Clear;
      Key := 0;
      Exit;
    end;

    KEY_SET_QTY: begin
      if edtMainInput.Text <> '' then begin
        FPendingQty := StrToIntDef(edtMainInput.Text, 1);
        FQtyMode := True;
        edtMainInput.Clear;
        lQtyInfo.Caption := 'Qty: ' + IntToStr(FPendingQty);
      end;
      Exit;
    end;

    KEY_SCAN: begin
      if Trim(edtMainInput.Text) <> '' then
        ProcessInputWithQty(edtMainInput.Text);

      edtMainInput.Clear;
      FQtyMode := False;
      FPendingQty := 0;
      lQtyInfo.Caption := '';
      Exit;
    end;
  end;
end;

function TForm1.GenerateReturnNo: string;
var
  MaxNo, NoUrut: Integer;
  TodayStr: string;
begin
  TodayStr := FormatDateTime('yyyymmdd', Now);

  SQLQuery1.Close;
  SQLQuery1.SQL.Text :=
    'SELECT MAX(CAST(substr(transaction_no, 18, 4) AS INTEGER)) AS max_no ' +
    'FROM transactions ' +
    'WHERE substr(transaction_no, 1, 3) = ''RET'' ' +
    '  AND substr(transaction_no, 5, 8) = :tgl ' +
    '  AND substr(transaction_no, 14, 3) = :term ' +
    '  AND deleted_at IS NULL';
  SQLQuery1.ParamByName('tgl').AsString := TodayStr;
  SQLQuery1.ParamByName('term').AsString := TERMINAL_ID;
  SQLQuery1.Open;

  if SQLQuery1.FieldByName('max_no').IsNull then
    MaxNo := 0
  else
    MaxNo := SQLQuery1.FieldByName('max_no').AsInteger;

  SQLQuery1.Close;

  NoUrut := MaxNo + 1;

  Result := 'RET-' + TodayStr + '-' + TERMINAL_ID + '-' + FormatFloat('0000', NoUrut);
end;

procedure TForm1.HandleReturn;
var
  RetItems: array[0..99] of TReturnItem;
  OrigTrx: TTransactionRecord;
  i: Integer;
begin
  if FProcessingReturn then begin
    ShowMessage('Sedang memproses retur, mohon tunggu...');
    Exit;
  end;

  // Initialize array
  for i := 0 to High(RetItems) do begin
    RetItems[i].ItemID := 0;
    RetItems[i].ProductCode := '';
    RetItems[i].ProductName := '';
    RetItems[i].Price := 0;
    RetItems[i].OriginalQty := 0;
    RetItems[i].ReturnQty := 0;
    RetItems[i].DiscountValue := 0;
    RetItems[i].DiscountType := '';
    RetItems[i].TotalPrice := 0;
  end;

  if FrmReturn = nil then
    FrmReturn := TFrmReturn.Create(Self);

  try
    FrmReturn.Connection := SQLite3Connection1;
    FrmReturn.Transaction := SQLTransaction1;

    if FrmReturn.ShowModal = mrOK then begin
      OrigTrx := FrmReturn.GetOriginalTransaction;

      if FrmReturn.GetReturnItems(RetItems) then begin
        FProcessingReturn := True;
        try
          ProcessReturn(OrigTrx, RetItems);
        finally
          FProcessingReturn := False;
        end;
      end;

      FrmReturn.ResetForm;
    end;
  finally
    LoadCartTemp;
  end;
end;

procedure TForm1.ProcessReturn(const OrigTrx: TTransactionRecord; const Items: array of TReturnItem);
var
  i: Integer;
  ReturnTID: Integer;
  ReturnNo, ReturnDate: string;
  ReturnTotal, ItemTotal, DiscountProportion, CurrentDiscountValue: Double;
  HasValidItems: Boolean;
  ItemCount: Integer;
  TrxData: TJSONObject;
  ItemsArray: TJSONArray;
  ItemObj: TJSONObject;
  ItemQuery: TSQLQuery;
begin
  try
    ReturnTotal := 0;
    HasValidItems := False;
    ItemCount := 0;

    // Hitung jumlah item yang valid
    for i := 0 to High(Items) do begin
      if (Items[i].ProductCode <> '') and (Items[i].ReturnQty > 0) then
        Inc(ItemCount);
    end;

    if ItemCount = 0 then begin
      ShowMessage('Tidak ada item untuk diretur!');
      LoadCartTemp;
      Exit;
    end;

    // Hitung total retur dengan validasi ketat
    for i := 0 to High(Items) do begin
      try
        if Trim(Items[i].ProductCode) = '' then
          Continue;

        if IsNaN(Items[i].ReturnQty) or IsInfinite(Items[i].ReturnQty) then
          Continue;
        if IsNaN(Items[i].OriginalQty) or IsInfinite(Items[i].OriginalQty) then
          Continue;
        if IsNaN(Items[i].Price) or IsInfinite(Items[i].Price) then
          Continue;

        CurrentDiscountValue := Items[i].DiscountValue;
        if IsNaN(CurrentDiscountValue) or IsInfinite(CurrentDiscountValue) then
          CurrentDiscountValue := 0;

        if Items[i].ReturnQty <= 0 then
          Continue;

        if Items[i].OriginalQty <= 0 then
          Continue;

        if Items[i].Price < 0 then
          Continue;

        DiscountProportion := 0;
        if CurrentDiscountValue > 0 then begin
          DiscountProportion := CurrentDiscountValue * (Items[i].ReturnQty / Items[i].OriginalQty);
        end;

        ItemTotal := (Items[i].Price * Items[i].ReturnQty) - DiscountProportion;

        if IsNaN(ItemTotal) or IsInfinite(ItemTotal) then
          Continue;

        if ItemTotal < 0 then
          ItemTotal := 0;

        ReturnTotal := ReturnTotal + ItemTotal;
        HasValidItems := True;

      except
        on E: Exception do begin
          Continue;
        end;
      end;
    end;

    if not HasValidItems then begin
      ShowMessage('Tidak ada item valid untuk diretur!');
      LoadCartTemp;
      Exit;
    end;

    if IsNaN(ReturnTotal) or IsInfinite(ReturnTotal) then begin
      ShowMessage('Total retur tidak valid (NaN/Infinite)!');
      LoadCartTemp;
      Exit;
    end;

    if ReturnTotal <= 0 then begin
      ShowMessage('Total retur harus lebih dari 0!');
      LoadCartTemp;
      Exit;
    end;

    ReturnNo := GenerateReturnNo;
    ReturnDate := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

    if not SQLTransaction1.Active then
      SQLTransaction1.StartTransaction;

    try
      // Insert ke transactions dengan transaction_type = 'return'
      SQLQuery1.SQL.Text :=
        'INSERT INTO transactions (transaction_no, transaction_date, subtotal, discount, total, ' +
        'pay_amount, change_amount, payment_status, payment_type, payment_provider, card_last4, ' +
        'transaction_type, return_from_transaction_id, user_id, cashier_code, terminal_id, deleted_at) ' +
        'VALUES (:no, :dt, :sub, 0, :tot, :pay, 0, ''return'', :ptype, :prov, :card, ''return'', :orig_id, :uid, :code, :term, NULL)';

      SQLQuery1.ParamByName('no').AsString := ReturnNo;
      SQLQuery1.ParamByName('dt').AsString := ReturnDate;
      SQLQuery1.ParamByName('sub').AsFloat := -ReturnTotal;
      SQLQuery1.ParamByName('tot').AsFloat := -ReturnTotal;
      SQLQuery1.ParamByName('pay').AsFloat := -ReturnTotal;
      SQLQuery1.ParamByName('ptype').AsString := OrigTrx.PaymentType;
      SQLQuery1.ParamByName('prov').AsString := OrigTrx.PaymentProvider;
      SQLQuery1.ParamByName('card').AsString := OrigTrx.CardLast4;
      SQLQuery1.ParamByName('orig_id').AsInteger := OrigTrx.ID;
      SQLQuery1.ParamByName('uid').AsInteger := FCurrentUser.ID;
      SQLQuery1.ParamByName('code').AsString := FCurrentUser.CashierCode;
      SQLQuery1.ParamByName('term').AsString := TERMINAL_ID;
      SQLQuery1.ExecSQL;

      // Get return transaction ID
      SQLQuery1.SQL.Text := 'SELECT last_insert_rowid() AS id';
      SQLQuery1.Open;
      ReturnTID := SQLQuery1.FieldByName('id').AsInteger;
      SQLQuery1.Close;

      // Insert items dengan qty negatif - HANYA YANG VALID
      for i := 0 to High(Items) do begin
        try
          if Trim(Items[i].ProductCode) = '' then
            Continue;

          if IsNaN(Items[i].ReturnQty) or IsInfinite(Items[i].ReturnQty) then
            Continue;
          if IsNaN(Items[i].OriginalQty) or IsInfinite(Items[i].OriginalQty) then
            Continue;
          if IsNaN(Items[i].Price) or IsInfinite(Items[i].Price) then
            Continue;

          CurrentDiscountValue := Items[i].DiscountValue;
          if IsNaN(CurrentDiscountValue) or IsInfinite(CurrentDiscountValue) then
            CurrentDiscountValue := 0;

          if Items[i].ReturnQty <= 0 then
            Continue;

          if Items[i].OriginalQty <= 0 then
            Continue;

          if Items[i].Price < 0 then
            Continue;

          DiscountProportion := 0;
          if CurrentDiscountValue > 0 then begin
            DiscountProportion := CurrentDiscountValue * (Items[i].ReturnQty / Items[i].OriginalQty);
          end;

          ItemTotal := (Items[i].Price * Items[i].ReturnQty) - DiscountProportion;

          if IsNaN(ItemTotal) or IsInfinite(ItemTotal) then
            Continue;

          if ItemTotal < 0 then
            ItemTotal := 0;

          SQLQuery1.SQL.Text :=
            'INSERT INTO transaction_items ' +
            '(transaction_id, product_code, product_name, price, qty, discount_value, discount_type, total_price, deleted_at) ' +
            'VALUES (:tid, :code, :name, :price, :qty, :disc, :dtype, :tot, NULL)';
          SQLQuery1.ParamByName('tid').AsInteger := ReturnTID;
          SQLQuery1.ParamByName('code').AsString := Items[i].ProductCode;
          SQLQuery1.ParamByName('name').AsString := Items[i].ProductName;
          SQLQuery1.ParamByName('price').AsFloat := Items[i].Price;
          SQLQuery1.ParamByName('qty').AsFloat := -Items[i].ReturnQty;
          SQLQuery1.ParamByName('disc').AsFloat := DiscountProportion;
          SQLQuery1.ParamByName('dtype').AsString := Items[i].DiscountType;
          SQLQuery1.ParamByName('tot').AsFloat := -ItemTotal;
          SQLQuery1.ExecSQL;

        except
          on E: Exception do begin
            Continue;
          end;
        end;
      end;

      SQLTransaction1.Commit;

      // ========== SYNC TO POSTGRESQL ==========
      try
        if Assigned(FPostgreSQLManager) and FPostgreSQLManager.IsConnected then
        begin
          TrxData := TJSONObject.Create;
          ItemsArray := TJSONArray.Create;
          ItemQuery := TSQLQuery.Create(nil);
          try
            ItemQuery.Database := SQLite3Connection1;
            ItemQuery.Transaction := SQLTransaction1;

            // Get items dari return transaction yang baru disimpan
            ItemQuery.SQL.Text :=
              'SELECT product_code, product_name, price, qty, discount_value, discount_type, total_price ' +
              'FROM transaction_items WHERE transaction_id = :tid';
            ItemQuery.ParamByName('tid').AsInteger := ReturnTID;
            ItemQuery.Open;

            while not ItemQuery.EOF do
            begin
              ItemObj := TJSONObject.Create;
              ItemObj.Add('product_code', ItemQuery.FieldByName('product_code').AsString);
              ItemObj.Add('product_name', ItemQuery.FieldByName('product_name').AsString);
              ItemObj.Add('qty', ItemQuery.FieldByName('qty').AsInteger);
              ItemObj.Add('price', ItemQuery.FieldByName('price').AsFloat);
              ItemObj.Add('discount_value', ItemQuery.FieldByName('discount_value').AsFloat);
              ItemObj.Add('discount_type', ItemQuery.FieldByName('discount_type').AsString);
              ItemObj.Add('total_price', ItemQuery.FieldByName('total_price').AsFloat);

              ItemsArray.Add(ItemObj);
              ItemQuery.Next;
            end;

            ItemQuery.Close;

            // Build return transaction data
            TrxData.Add('user_id', FCurrentUser.ID);
            TrxData.Add('total_amount', -ReturnTotal);  // Negatif untuk return
            TrxData.Add('payment_type', OrigTrx.PaymentType);
            TrxData.Add('payment_bank', OrigTrx.PaymentProvider);
            TrxData.Add('payment_last4', OrigTrx.CardLast4);
            TrxData.Add('payment_amount', -ReturnTotal);
            TrxData.Add('change_amount', 0.0);
            TrxData.Add('is_return', True);  // Flag return
            TrxData.Add('original_trx_id', OrigTrx.TransactionNo);  // Reference ke transaksi asli
            TrxData.Add('items', ItemsArray);

            // Try sync immediately
            TrySyncTransaction(ReturnNo, TrxData);

          finally
            ItemQuery.Free;
            TrxData.Free;
          end;
        end;
      except
        // Silent - sync failure tidak boleh block retur
      end;
      // ========== END SYNC ==========

      // Update cash drawer jika retur dari transaksi cash
      if OrigTrx.PaymentType = 'cash' then
        UpdateCashDrawerSales(ReturnTotal, True);

      ShowMessage('Retur berhasil!' + sLineBreak +
                  'No. Retur: ' + ReturnNo + sLineBreak +
                  'Total Pengembalian: Rp ' + FormatFloat('#,##0', ReturnTotal) + sLineBreak +
                  'Metode: ' + UpperCase(OrigTrx.PaymentType));

      LoadCartTemp;

    except
      on E: Exception do begin
        if SQLTransaction1.Active then
          SQLTransaction1.Rollback;
        LoadCartTemp;
        ShowMessage('Error processing return: ' + E.Message);
      end;
    end;
  except
    on E: Exception do begin
      LoadCartTemp;
      ShowMessage('Error: ' + E.Message);
    end;
  end;
end;

{ --------------------------------------------------------------- }
{ END OF DAY }
{ --------------------------------------------------------------- }
procedure TForm1.HandleEndOfDay;
var
  TrxCount: Integer;
  TempQuery: TSQLQuery;
  DrawerStatus: string;
begin
  TrxCount := 0;
  TempQuery := TSQLQuery.Create(nil);
  try
    TempQuery.Database := SQLite3Connection1;
    TempQuery.Transaction := SQLTransaction1;

    // 1. CEK: Apakah cash drawer sudah closed hari ini?
    TempQuery.SQL.Text :=
      'SELECT status FROM cash_drawer ' +
      'WHERE DATE(date) = DATE(''now'') ' +
      'AND terminal_id = :term';
    TempQuery.ParamByName('term').AsString := TERMINAL_ID;

    try
      TempQuery.Open;
      if not TempQuery.EOF then
      begin
        DrawerStatus := TempQuery.FieldByName('status').AsString;
        if DrawerStatus = 'closed' then
        begin
          ShowMessage('END OF DAY SUDAH DILAKUKAN!' + sLineBreak + sLineBreak +
                      'EOD hari ini sudah selesai.' + sLineBreak +
                      'Tidak bisa melakukan EOD lagi di hari yang sama.');
          TempQuery.Close;
          TempQuery.Free;
          Exit;
        end;
      end;
      TempQuery.Close;
    except
      on E: Exception do
      begin
        ShowMessage('Error cek cash drawer: ' + E.Message);
        TempQuery.Free;
        Exit;
      end;
    end;

    // 2. CEK: Apakah ada transaksi hari ini?
    TempQuery.SQL.Text :=
      'SELECT COUNT(*) as cnt FROM transactions ' +
      'WHERE DATE(transaction_date) = DATE(''now'') ' +
      'AND deleted_at IS NULL ' +
      'AND payment_status = ''sale''';

    try
      TempQuery.Open;
      TrxCount := TempQuery.FieldByName('cnt').AsInteger;
      TempQuery.Close;
    except
      on E: Exception do
      begin
        ShowMessage('Error cek transaksi: ' + E.Message);
        TempQuery.Free;
        Exit;
      end;
    end;

  finally
    TempQuery.Free;
  end;

  if TrxCount = 0 then
  begin
    ShowMessage('TIDAK ADA TRANSAKSI!' + sLineBreak + sLineBreak +
                'End of Day hanya bisa dilakukan jika ada transaksi hari ini.');
    Exit;
  end;

  // 3. Konfirmasi EOD
  if MessageDlg('END OF DAY' + sLineBreak + sLineBreak +
                'Total transaksi hari ini: ' + IntToStr(TrxCount) + sLineBreak + sLineBreak +
                'Proses End of Day akan:' + sLineBreak +
                '1. Tutup cash drawer hari ini' + sLineBreak +
                '2. Sinkronisasi semua data ke server' + sLineBreak +
                '3. Tutup aplikasi' + sLineBreak + sLineBreak +
                'Lanjutkan?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    ProcessEndOfDay;
end;

procedure TForm1.SyncCashDrawerEOD;
var
  CashDrawerData: TJSONObject;
begin
  if not Assigned(FPostgreSQLManager) or not FPostgreSQLManager.IsConnected then
    Exit;

  try
    // Get cash drawer data
    SQLQuery1.SQL.Text :=
      'SELECT * FROM cash_drawer WHERE id = :id';
    SQLQuery1.ParamByName('id').AsInteger := FCurrentDrawerID;
    SQLQuery1.Open;

    if not SQLQuery1.EOF then
    begin
      CashDrawerData := TJSONObject.Create;
      try
        CashDrawerData.Add('user_id', FCurrentUser.ID);
        CashDrawerData.Add('amount', SQLQuery1.FieldByName('opening_cash').AsFloat);
        CashDrawerData.Add('balance_before', 0.0);
        CashDrawerData.Add('balance_after', SQLQuery1.FieldByName('expected_cash').AsFloat);
        CashDrawerData.Add('trx_id', '');
        CashDrawerData.Add('notes',
          'Opening: ' + FormatFloat('#,##0', SQLQuery1.FieldByName('opening_cash').AsFloat) + ' | ' +
          'Sales: ' + FormatFloat('#,##0', SQLQuery1.FieldByName('cash_sales').AsFloat) + ' | ' +
          'Returns: ' + FormatFloat('#,##0', SQLQuery1.FieldByName('cash_returns').AsFloat) + ' | ' +
          'Expected: ' + FormatFloat('#,##0', SQLQuery1.FieldByName('expected_cash').AsFloat) + ' | ' +
          'Actual: ' + FormatFloat('#,##0', SQLQuery1.FieldByName('closing_cash').AsFloat) + ' | ' +
          'Diff: ' + FormatFloat('#,##0', SQLQuery1.FieldByName('difference').AsFloat)
        );

        // Sync opening drawer
        FPostgreSQLManager.SyncCashDrawer('open', CashDrawerData);

        // Update for closing
        CashDrawerData.Delete('amount');
        CashDrawerData.Add('amount', SQLQuery1.FieldByName('closing_cash').AsFloat);
        CashDrawerData.Delete('balance_after');
        CashDrawerData.Add('balance_after', SQLQuery1.FieldByName('closing_cash').AsFloat);

        // Sync closing drawer
        FPostgreSQLManager.SyncCashDrawer('close', CashDrawerData);

      finally
        CashDrawerData.Free;
      end;
    end;

    SQLQuery1.Close;
  except
    on E: Exception do
      ShowMessage('Warning: Cash drawer sync failed - ' + E.Message);
  end;
end;

procedure TForm1.ProcessEndOfDay;
var
  TodayStr: string;
  ExpectedCash, ActualCash, Difference: Double;
  ClosingTime: string;
  Notes: string;
  ActualCashStr: string;
  SyncedCount, UserCount, ProductCount: Integer;
  ProgressForm: TForm;
  ProgressLabel: TLabel;
begin
  TodayStr := FormatDateTime('yyyy-mm-dd', Now);
  ClosingTime := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  try
    // 1. CEK APAKAH CASH DRAWER MASIH OPEN
    if not IsCashDrawerOpen then
    begin
      ShowMessage('Cash drawer sudah ditutup atau belum dibuka hari ini!');
      Exit;
    end;

    // 2. INPUT ACTUAL CASH
    ActualCashStr := '';
    if not InputQuery('END OF DAY - Hitung Uang',
                      'Masukkan jumlah uang aktual di laci:',
                      ActualCashStr) then
      Exit;

    ActualCash := StrToFloatDef(ActualCashStr, 0);

    if ActualCash < 0 then
    begin
      ShowMessage('Jumlah uang tidak valid!');
      Exit;
    end;

    // 3. GET EXPECTED CASH
    SQLQuery1.SQL.Text :=
      'SELECT expected_cash FROM cash_drawer WHERE id = :id';
    SQLQuery1.ParamByName('id').AsInteger := FCurrentDrawerID;
    SQLQuery1.Open;
    ExpectedCash := SQLQuery1.FieldByName('expected_cash').AsFloat;
    SQLQuery1.Close;

    // 4. HITUNG SELISIH
    Difference := ActualCash - ExpectedCash;

    // 5. INPUT NOTES (OPSIONAL)
    Notes := '';
    if Difference <> 0 then
    begin
      if not InputQuery('END OF DAY - Catatan',
                        'Catatan selisih (opsional):',
                        Notes) then
        Notes := '';
    end;

    // 6. UPDATE CASH DRAWER
    SQLQuery1.SQL.Text :=
      'UPDATE cash_drawer SET ' +
      'closing_cash = :closing, ' +
      'closing_time = :time, ' +
      'difference = :diff, ' +
      'notes = :notes, ' +
      'status = ''closed'' ' +
      'WHERE id = :id';
    SQLQuery1.ParamByName('closing').AsFloat := ActualCash;
    SQLQuery1.ParamByName('time').AsString := ClosingTime;
    SQLQuery1.ParamByName('diff').AsFloat := Difference;
    SQLQuery1.ParamByName('notes').AsString := Notes;
    SQLQuery1.ParamByName('id').AsInteger := FCurrentDrawerID;
    SQLQuery1.ExecSQL;

    if SQLTransaction1.Active then
      SQLTransaction1.Commit;

    // 7. TAMPILKAN SUMMARY
    ShowMessage('CLOSING CASH DRAWER' + sLineBreak + sLineBreak +
                'Expected: Rp ' + FormatFloat('#,##0', ExpectedCash) + sLineBreak +
                'Actual: Rp ' + FormatFloat('#,##0', ActualCash) + sLineBreak +
                'Selisih: Rp ' + FormatFloat('#,##0', Difference) + sLineBreak + sLineBreak +
                'Status: ' + IfThen(Difference = 0, 'PAS', IfThen(Difference > 0, 'LEBIH', 'KURANG')));

    // 8. SYNC SEMUA DATA
    ProgressForm := TForm.Create(nil);
    try
      ProgressForm.BorderStyle := bsNone;
      ProgressForm.Width := 400;
      ProgressForm.Height := 150;
      ProgressForm.Position := poScreenCenter;
      ProgressForm.Color := clWhite;

      ProgressLabel := TLabel.Create(ProgressForm);
      ProgressLabel.Parent := ProgressForm;
      ProgressLabel.Align := alClient;
      ProgressLabel.Alignment := taCenter;
      ProgressLabel.Layout := tlCenter;
      ProgressLabel.Font.Size := 14;

      ProgressForm.Show;
      Application.ProcessMessages;

      try
        // A. Sync Master Produk dari DBF
        ProgressLabel.Caption := 'Sync Master Produk dari DBF...' + sLineBreak +
                                 'Mohon tunggu...';
        Application.ProcessMessages;

        ProductCount := ReadDBFProducts(FDataDrivePath + '\produk.dbf');

        // B. Sync User dari PostgreSQL (jika online)
        if Assigned(FPostgreSQLManager) and FPostgreSQLManager.IsConnected then
        begin
          ProgressLabel.Caption := 'Sync User dari Server...' + sLineBreak +
                                   'Mohon tunggu...';
          Application.ProcessMessages;

          SyncUsersFromPostgreSQL;

          // Get user count
          SQLQuery1.SQL.Text := 'SELECT COUNT(*) as cnt FROM users WHERE is_default = 0';
          SQLQuery1.Open;
          UserCount := SQLQuery1.FieldByName('cnt').AsInteger;
          SQLQuery1.Close;

          // C. Sync Cash Drawer
          ProgressLabel.Caption := 'Sync Cash Drawer...';
          Application.ProcessMessages;
          SyncCashDrawerEOD;

          // D. Sync All Pending Transactions
          ProgressLabel.Caption := 'Sync Transaksi Pending...' + sLineBreak +
                                   'Mohon tunggu...';
          Application.ProcessMessages;
          SyncedCount := SyncAllPendingData;

          ProgressLabel.Caption := 'SINKRONISASI SELESAI!' + sLineBreak + sLineBreak +
                                   'User: ' + IntToStr(UserCount) + ' | ' +
                                   'Produk: ' + IntToStr(ProductCount) + ' | ' +
                                   'Transaksi: ' + IntToStr(SyncedCount);
          Application.ProcessMessages;
          Sleep(2000);
        end
        else
        begin
          ProgressLabel.Caption := 'SYNC SELESAI (OFFLINE MODE)' + sLineBreak + sLineBreak +
                                   'Produk: ' + IntToStr(ProductCount) + sLineBreak +
                                   'Transaksi akan disinkron saat online';
          Application.ProcessMessages;
          Sleep(2000);
        end;

      finally
        ProgressForm.Close;
      end;

    finally
      ProgressForm.Free;
    end;

    if Assigned(FPostgreSQLManager) and FPostgreSQLManager.IsConnected then
    begin
      ShowMessage('SINKRONISASI SELESAI!' + sLineBreak + sLineBreak +
                  'User: ' + IntToStr(UserCount) + sLineBreak +
                  'Produk: ' + IntToStr(ProductCount) + sLineBreak +
                  'Transaksi: ' + IntToStr(SyncedCount));
    end
    else
    begin
      ShowMessage('EOD SELESAI (OFFLINE)' + sLineBreak + sLineBreak +
                  'Produk tersinkron: ' + IntToStr(ProductCount) + sLineBreak +
                  'Data transaksi akan disinkron saat online.');
    end;

    // 9. TUTUP APLIKASI
    ShowMessage('END OF DAY SELESAI!' + sLineBreak + sLineBreak +
                'Aplikasi akan ditutup.' + sLineBreak +
                'Terima kasih!');

    Application.Terminate;

  except
    on E: Exception do
    begin
      if SQLTransaction1.Active then
        SQLTransaction1.Rollback;
      ShowMessage('ERROR END OF DAY: ' + E.Message);
    end;
  end;
end;

function TForm1.SyncAllPendingData: Integer;
var
  SyncedCount: Integer;
begin
  Result := 0;

  try
    // Sync pending queue
    if Assigned(FSyncWorker) then
    begin
      SyncedCount := FSyncWorker.ProcessPendingQueue;
      Result := SyncedCount;
    end;
  except
    on E: Exception do
      ShowMessage('WARNING: Beberapa data gagal disinkron.' + sLineBreak +
                  'Data akan disinkron otomatis nanti.' + sLineBreak + sLineBreak +
                  'Error: ' + E.Message);
  end;
end;

{ --------------------------------------------------------------- }
{ FORM SHOW }
{ --------------------------------------------------------------- }
procedure TForm1.FormShow(Sender: TObject);
begin
  SetupGridColumnWidths;
  TimerDateTimeTimer(nil);
  edtMainInput.SetFocus;
end;

end.
