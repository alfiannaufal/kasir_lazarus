unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, fpjson, jsonparser, SQLDB, SQLite3Conn, DB, Math,
  Forms, Controls, Graphics, Dialogs, StdCtrls, DBGrids, LCLType, ExtCtrls,
  IniFiles, StrUtils,
  FormLogin, FormModalAwal, FormDebit, FormSearchName, FormReturn, uTypes;

const
  // Terminal configuration
  CONFIG_FILE = 'terminal.ini';

  // Keyboard shortcuts
  KEY_CLEAR_CART = VK_DELETE;
  KEY_SEARCH_NAME = VK_F2;
  KEY_DISC_PERCENT = VK_F6;
  KEY_DISC_NOMINAL = VK_F7;
  KEY_PAY_DEBIT = VK_F11;
  KEY_PAY_CASH = VK_F12;
  KEY_CLEAR_INPUT = VK_SPACE;
  KEY_SET_QTY = VK_END;
  KEY_SCAN = VK_RETURN;
  KEY_FOCUS_INPUT = VK_ESCAPE;
  KEY_RETURN = VK_F9;

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
  public
  end;

var
  Form1: TForm1;
  TERMINAL_ID: string = '';
  TERMINAL_NAME: string = '';
  TERMINAL_LOCATION: string = '';

implementation

{$R *.lfm}

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
begin
  if IsOnline then
  begin
    lblDriveStatus.Caption := '● ONLINE';
    lblDriveStatus.Font.Color := clWhite;
    pnlStatus.Color := $0050B000;  // Dark Green
  end
  else
  begin
    lblDriveStatus.Caption := '● OFFLINE';
    lblDriveStatus.Font.Color := clWhite;
    pnlStatus.Color := $002020B0;  // Dark Red
  end;

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
    // WindowState := wsMaximized;
  end;

  // Load Terminal Config
  LoadTerminalConfig;
  ValidateTerminalConfig;

  FPendingQty := 0;
  FQtyMode := False;
  FDriveOnline := False;
  FProcessingReturn := False;
  FCurrentDrawerID := 0;
  FLoginHistoryID := 0;
  FDataDrivePath := 'F:\wpi\dat';

  // Initialize user info
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

    DataSource1.DataSet := SQLQuery1;
    DBGrid1.DataSource := DataSource1;

    DoLogin;

    CheckDriveStatus;
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
    ' created_at VARCHAR(20),' +
    ' updated_at VARCHAR(20)' +
    ');';
  SQLQuery1.ExecSQL;

  // Insert default users jika belum ada
  SQLQuery1.SQL.Text :=
    'INSERT OR IGNORE INTO users (cashier_code, password, name, level, created_at) ' +
    'VALUES (''000000'', ''000000'', ''Administrator'', ''admin'', datetime(''now''))';
  SQLQuery1.ExecSQL;

  SQLQuery1.SQL.Text :=
    'INSERT OR IGNORE INTO users (cashier_code, password, name, level, created_at) ' +
    'VALUES (''111111'', ''111111'', ''Kasir 1'', ''kasir'', datetime(''now''))';
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

    ShowMessage('Selamat datang, ' + FCurrentUser.Name + '!');
  end
  else
  begin
    // User tidak login, tutup aplikasi
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
begin
  Result.Found := False;
  Result.Boom := False;
  Result.QtyMeth := False;
  Result.QtyA := 0;
  Result.PriceA := 0;
  Result.QtyB := 0;
  Result.PriceB := 0;

  if Trim(ACode) = '' then Exit;

  try
    if not FDriveOnline then
    begin
      ShowMessage('Sistem pencarian OFFLINE!' + sLineBreak +
                  'Drive data tidak tersedia.');
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

    if FileExists(ExecPath) then begin
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

          // Data harga grosir
          Result.Boom := Obj.Get('boom', False);
          Result.QtyMeth := Obj.Get('qtymeth', False);
          Result.QtyA := Obj.Get('qty_a', 0.0);
          Result.PriceA := Obj.Get('price_a', 0.0);
          Result.QtyB := Obj.Get('qty_b', 0.0);
          Result.PriceB := Obj.Get('price_b', 0.0);
        end;

        jtArray: begin
          Arr := TJSONArray(JSON);
          if Arr.Count > 0 then begin
            Obj := Arr.Objects[0];
            Result.Found := True;
            Result.Code := Obj.Get('code', '');
            Result.Name := Obj.Get('desc', Obj.Get('name', ''));
            Result.Price := Obj.Get('price', 0.0);

            // Harga grosir
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
    on E: Exception do begin
      Result.Found := False;
      ShowMessage('Error searching product: ' + E.Message);
    end;
  end;
end;

function TForm1.GetPriceByQty(const Info: TProductInfo; AQty: Double): Double;
begin
  Result := Info.Price; // Default harga normal

  // Cek apakah produk memiliki harga grosir
  if Info.Boom and Info.QtyMeth then
  begin
    // Cek qty_b
    if (Info.QtyB > 0) and (AQty > Info.QtyB) then
      Result := Info.PriceB
    // Cek qty_a
    else if (Info.QtyA > 0) and (AQty > Info.QtyA) then
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
  try
    SQLQuery1.Close;
    SQLQuery1.SQL.Text := 'SELECT id, qty FROM cart_temp WHERE product_code = :c';
    SQLQuery1.ParamByName('c').AsString := Info.Code;
    SQLQuery1.Open;

    if not SQLQuery1.EOF then begin
      OldQty := SQLQuery1.FieldByName('qty').AsFloat;
      NewQty := OldQty + AQty;

      // Hitung harga berdasarkan qty total
      ActualPrice := GetPriceByQty(Info, NewQty);
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
    end else begin
      // Hitung harga berdasarkan qty
      ActualPrice := GetPriceByQty(Info, AQty);

      SQLQuery1.Close;
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
      ShowMessage('Error adding to cart: ' + E.Message);
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

  if FrmSearchName.ShowModal = mrOK then begin
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
        // Skip item kosong
        if Trim(Items[i].ProductCode) = '' then
          Continue;

        // Validasi NaN/Infinite di semua field
        if IsNaN(Items[i].ReturnQty) or IsInfinite(Items[i].ReturnQty) then
          Continue;
        if IsNaN(Items[i].OriginalQty) or IsInfinite(Items[i].OriginalQty) then
          Continue;
        if IsNaN(Items[i].Price) or IsInfinite(Items[i].Price) then
          Continue;

        // Copy discount value ke variable lokal
        CurrentDiscountValue := Items[i].DiscountValue;
        if IsNaN(CurrentDiscountValue) or IsInfinite(CurrentDiscountValue) then
          CurrentDiscountValue := 0;

        // Skip item dengan qty 0 atau negatif
        if Items[i].ReturnQty <= 0 then
          Continue;

        // Skip item dengan OriginalQty invalid
        if Items[i].OriginalQty <= 0 then
          Continue;

        // Skip item dengan Price invalid
        if Items[i].Price < 0 then
          Continue;

        // Hitung proporsi diskon
        DiscountProportion := 0;
        if CurrentDiscountValue > 0 then begin
          DiscountProportion := CurrentDiscountValue * (Items[i].ReturnQty / Items[i].OriginalQty);
        end;

        // Hitung item total
        ItemTotal := (Items[i].Price * Items[i].ReturnQty) - DiscountProportion;

        // Validasi ItemTotal
        if IsNaN(ItemTotal) or IsInfinite(ItemTotal) then
          Continue;

        if ItemTotal < 0 then
          ItemTotal := 0;

        ReturnTotal := ReturnTotal + ItemTotal;
        HasValidItems := True;

      except
        on E: Exception do begin
          // Skip item yang error
          Continue;
        end;
      end;
    end;

    // Validasi ada item valid
    if not HasValidItems then begin
      ShowMessage('Tidak ada item valid untuk diretur!');
      LoadCartTemp;
      Exit;
    end;

    // Validasi total retur
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
          // Skip item kosong
          if Trim(Items[i].ProductCode) = '' then
            Continue;

          // Validasi NaN/Infinite
          if IsNaN(Items[i].ReturnQty) or IsInfinite(Items[i].ReturnQty) then
            Continue;
          if IsNaN(Items[i].OriginalQty) or IsInfinite(Items[i].OriginalQty) then
            Continue;
          if IsNaN(Items[i].Price) or IsInfinite(Items[i].Price) then
            Continue;

          // Copy discount value
          CurrentDiscountValue := Items[i].DiscountValue;
          if IsNaN(CurrentDiscountValue) or IsInfinite(CurrentDiscountValue) then
            CurrentDiscountValue := 0;

          // Skip item dengan qty 0 atau negatif
          if Items[i].ReturnQty <= 0 then
            Continue;

          // Skip item dengan OriginalQty invalid
          if Items[i].OriginalQty <= 0 then
            Continue;

          // Skip item dengan Price invalid
          if Items[i].Price < 0 then
            Continue;

          // Hitung proporsi diskon
          DiscountProportion := 0;
          if CurrentDiscountValue > 0 then begin
            DiscountProportion := CurrentDiscountValue * (Items[i].ReturnQty / Items[i].OriginalQty);
          end;

          ItemTotal := (Items[i].Price * Items[i].ReturnQty) - DiscountProportion;

          // Validasi ItemTotal
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
{ FORM SHOW }
{ --------------------------------------------------------------- }
procedure TForm1.FormShow(Sender: TObject);
begin
  SetupGridColumnWidths;
  TimerDateTimeTimer(nil);
  edtMainInput.SetFocus;
end;

end.
