unit uPostgreSQL; //  Koneksi & Sinkronisasi ke PostgreSQL Server

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, DB, fpjson, jsonparser,
  {$IFDEF UNIX}
  pqconnection,
  {$ELSE}
  PQConnection,
  {$ENDIF}
  uTypes;

type
  { TPostgreSQLManager }
  TPostgreSQLManager = class
  private
    FConnection: TPQConnection;
    FTransaction: TSQLTransaction;
    FQuery: TSQLQuery;
    FIsOnline: Boolean;
    FTerminalID: string;

    function TestConnection: Boolean;
    procedure LogSyncError(const ErrorMsg: string);
  public
    constructor Create(const ATerminalID: string);
    destructor Destroy; override;

    function Connect(const AHost, ADatabase, AUser, APassword: string; APort: Integer = 5432): Boolean;
    procedure Disconnect;
    function IsConnected: Boolean;

    procedure CreateAllTables;
    function RegisterTerminal(const ATerminalName, ALocation: string): Boolean;
    function FetchAllUsers: TJSONArray;

    function SyncTransaction(const ATrxID: string; const AData: TJSONObject): Boolean;
    function SyncCashDrawer(const AAction: string; const AData: TJSONObject): Boolean;
    function SyncReturnTransaction(const AReturnData: TJSONObject): Boolean;
    function SyncPendingData: Boolean;

    property IsOnline: Boolean read FIsOnline;
  end;

implementation

{ TPostgreSQLManager }

constructor TPostgreSQLManager.Create(const ATerminalID: string);
begin
  inherited Create;
  FTerminalID := ATerminalID;
  FIsOnline := False;

  FConnection := TPQConnection.Create(nil);
  FTransaction := TSQLTransaction.Create(nil);
  FQuery := TSQLQuery.Create(nil);

  FTransaction.Database := FConnection;
  FConnection.Transaction := FTransaction;
  FQuery.Database := FConnection;
  FQuery.Transaction := FTransaction;
end;

destructor TPostgreSQLManager.Destroy;
begin
  Disconnect;

  if Assigned(FQuery) then
    FQuery.Free;
  if Assigned(FTransaction) then
    FTransaction.Free;
  if Assigned(FConnection) then
    FConnection.Free;

  inherited Destroy;
end;

function TPostgreSQLManager.Connect(const AHost, ADatabase, AUser, APassword: string; APort: Integer): Boolean;
begin
  Result := False;

  try
    FConnection.HostName := AHost;
    FConnection.DatabaseName := ADatabase;
    FConnection.UserName := AUser;
    FConnection.Password := APassword;

    FConnection.Params.Clear;
    FConnection.Params.Add('port=' + IntToStr(APort));
    FConnection.Params.Add('connect_timeout=5');

    try
      FConnection.Connected := True;
    except
      on E: Exception do
      begin
        // Connection failed - SILENT
        FIsOnline := False;
        Result := False;
        Exit; // ← Exit di sini, tidak raise exception
      end;
    end;

    FIsOnline := TestConnection;
    Result := FIsOnline;

    if FIsOnline then
    begin
      try
        CreateAllTables;
      except
        on E: Exception do
        begin
          LogSyncError('Create tables failed: ' + E.Message);
          // Continue anyway
        end;
      end;
    end;

  except
    on E: Exception do
    begin
      FIsOnline := False;
      Result := False;
      // SILENT - tidak re-raise exception
    end;
  end;
end;

procedure TPostgreSQLManager.Disconnect;
begin
  try
    if Assigned(FConnection) and FConnection.Connected then
      FConnection.Connected := False;
    FIsOnline := False;
  except
    // Silent
  end;
end;

function TPostgreSQLManager.IsConnected: Boolean;
begin
  Result := FIsOnline and Assigned(FConnection) and FConnection.Connected;
end;

function TPostgreSQLManager.TestConnection: Boolean;
begin
  Result := False;
  try
    if FConnection.Connected then
    begin
      FQuery.SQL.Text := 'SELECT 1';
      FQuery.Open;
      Result := not FQuery.EOF;
      FQuery.Close;
    end;
  except
    Result := False;
  end;
end;

procedure TPostgreSQLManager.LogSyncError(const ErrorMsg: string);
var
  LogFile: TextFile;
  LogPath: string;
  TimeStamp: string;
begin
  LogPath := ExtractFilePath(ParamStr(0)) + 'sync_errors.log';
  TimeStamp := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  try
    AssignFile(LogFile, LogPath);

    if FileExists(LogPath) then
      Append(LogFile)
    else
      Rewrite(LogFile);

    WriteLn(LogFile, TimeStamp, ' | ', FTerminalID, ' | ', ErrorMsg);
    CloseFile(LogFile);
  except
    // Silent fail
  end;
end;

procedure TPostgreSQLManager.CreateAllTables;
begin
  try
    if FTransaction.Active then
      FTransaction.Commit;

    FTransaction.StartTransaction;

    // Table: terminals
    FQuery.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS terminals (' +
      '  terminal_id VARCHAR(10) PRIMARY KEY,' +
      '  terminal_name VARCHAR(100),' +
      '  location VARCHAR(100),' +
      '  is_active BOOLEAN DEFAULT true,' +
      '  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,' +
      '  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP' +
      ')';
    FQuery.ExecSQL;

    // Table: users
    FQuery.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS users (' +
      '  user_id SERIAL PRIMARY KEY,' +
      '  username VARCHAR(50) UNIQUE NOT NULL,' +
      '  password_hash VARCHAR(255) NOT NULL,' +
      '  full_name VARCHAR(100),' +
      '  user_level VARCHAR(20),' +
      '  is_active BOOLEAN DEFAULT true,' +
      '  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,' +
      '  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP' +
      ')';
    FQuery.ExecSQL;

    // Table: transactions
    FQuery.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS transactions (' +
      '  trx_id VARCHAR(50) PRIMARY KEY,' +
      '  terminal_id VARCHAR(10) REFERENCES terminals(terminal_id),' +
      '  user_id INTEGER REFERENCES users(user_id),' +
      '  trx_date TIMESTAMP NOT NULL,' +
      '  total_amount DECIMAL(15,2) NOT NULL,' +
      '  payment_type VARCHAR(20) NOT NULL,' +
      '  payment_bank VARCHAR(50),' +
      '  payment_last4 VARCHAR(4),' +
      '  payment_amount DECIMAL(15,2),' +
      '  change_amount DECIMAL(15,2),' +
      '  is_return BOOLEAN DEFAULT false,' +
      '  original_trx_id VARCHAR(50),' +
      '  notes TEXT,' +
      '  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,' +
      '  synced_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP' +
      ')';
    FQuery.ExecSQL;

    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_trx_terminal ON transactions(terminal_id)';
    FQuery.ExecSQL;
    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_trx_date ON transactions(trx_date)';
    FQuery.ExecSQL;

    // Table: transaction_items
    FQuery.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS transaction_items (' +
      '  item_id SERIAL PRIMARY KEY,' +
      '  trx_id VARCHAR(50) REFERENCES transactions(trx_id) ON DELETE CASCADE,' +
      '  product_code VARCHAR(13) NOT NULL,' +
      '  product_name VARCHAR(200) NOT NULL,' +
      '  qty INTEGER NOT NULL,' +
      '  price DECIMAL(15,2) NOT NULL,' +
      '  discount_value DECIMAL(15,2) DEFAULT 0,' +
      '  discount_type VARCHAR(20),' +
      '  total_price DECIMAL(15,2) NOT NULL,' +
      '  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP' +
      ')';
    FQuery.ExecSQL;

    FQuery.SQL.Text := 'CREATE INDEX IF NOT EXISTS idx_trx_items_trx ON transaction_items(trx_id)';
    FQuery.ExecSQL;

    // Table: cash_drawer
    FQuery.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS cash_drawer (' +
      '  drawer_id SERIAL PRIMARY KEY,' +
      '  terminal_id VARCHAR(10) REFERENCES terminals(terminal_id),' +
      '  user_id INTEGER REFERENCES users(user_id),' +
      '  action_type VARCHAR(20) NOT NULL,' +
      '  amount DECIMAL(15,2) NOT NULL,' +
      '  balance_before DECIMAL(15,2),' +
      '  balance_after DECIMAL(15,2),' +
      '  trx_id VARCHAR(50),' +
      '  notes TEXT,' +
      '  action_date TIMESTAMP NOT NULL,' +
      '  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP' +
      ')';
    FQuery.ExecSQL;

    // Table: sync_log
    FQuery.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS sync_log (' +
      '  log_id SERIAL PRIMARY KEY,' +
      '  terminal_id VARCHAR(10),' +
      '  sync_type VARCHAR(50),' +
      '  reference_id VARCHAR(100),' +
      '  status VARCHAR(20),' +
      '  error_message TEXT,' +
      '  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP' +
      ')';
    FQuery.ExecSQL;

    FTransaction.Commit;
  except
    on E: Exception do
    begin
      if FTransaction.Active then
        FTransaction.Rollback;
      LogSyncError('Create tables failed: ' + E.Message);
      raise;
    end;
  end;
end;

function TPostgreSQLManager.RegisterTerminal(const ATerminalName, ALocation: string): Boolean;
begin
  Result := False;
  if not IsConnected then Exit;

  try
    if FTransaction.Active then
      FTransaction.Commit;

    FTransaction.StartTransaction;

    FQuery.SQL.Text :=
      'INSERT INTO terminals (terminal_id, terminal_name, location, is_active) ' +
      'VALUES (:id, :name, :loc, true) ' +
      'ON CONFLICT (terminal_id) DO UPDATE SET ' +
      '  terminal_name = EXCLUDED.terminal_name, ' +
      '  location = EXCLUDED.location, ' +
      '  updated_at = CURRENT_TIMESTAMP';

    FQuery.ParamByName('id').AsString := FTerminalID;
    FQuery.ParamByName('name').AsString := ATerminalName;
    FQuery.ParamByName('loc').AsString := ALocation;
    FQuery.ExecSQL;

    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      if FTransaction.Active then
        FTransaction.Rollback;
      LogSyncError('Register terminal failed: ' + E.Message);
    end;
  end;
end;

function TPostgreSQLManager.FetchAllUsers: TJSONArray;
var
  UserObj: TJSONObject;
begin
  Result := TJSONArray.Create;

  if not IsConnected then Exit;

  try
    FQuery.SQL.Text :=
      'SELECT user_id, username, full_name, user_level, is_active ' +
      'FROM users ' +
      'WHERE is_active = true ' +
      'ORDER BY user_id';

    FQuery.Open;

    while not FQuery.EOF do
    begin
      UserObj := TJSONObject.Create;
      UserObj.Add('user_id', FQuery.FieldByName('user_id').AsInteger);
      UserObj.Add('username', FQuery.FieldByName('username').AsString);
      UserObj.Add('full_name', FQuery.FieldByName('full_name').AsString);
      UserObj.Add('user_level', FQuery.FieldByName('user_level').AsString);
      UserObj.Add('is_active', FQuery.FieldByName('is_active').AsBoolean);

      Result.Add(UserObj);
      FQuery.Next;
    end;

    FQuery.Close;
  except
    on E: Exception do
    begin
      LogSyncError('Fetch users failed: ' + E.Message);
      Result.Clear;
    end;
  end;
end;

function TPostgreSQLManager.SyncTransaction(const ATrxID: string; const AData: TJSONObject): Boolean;
var
  Items: TJSONArray;
  Item: TJSONObject;
  i: Integer;
begin
  Result := False;
  if not IsConnected then Exit;

  try
    if FTransaction.Active then
      FTransaction.Commit;

    FTransaction.StartTransaction;

    // Insert transaction header
    FQuery.SQL.Text :=
      'INSERT INTO transactions (' +
      '  trx_id, terminal_id, user_id, trx_date, total_amount, ' +
      '  payment_type, payment_bank, payment_last4, payment_amount, ' +
      '  change_amount, is_return, original_trx_id, notes' +
      ') VALUES (' +
      '  :trx_id, :terminal_id, :user_id, :trx_date, :total_amount, ' +
      '  :payment_type, :payment_bank, :payment_last4, :payment_amount, ' +
      '  :change_amount, :is_return, :original_trx_id, :notes' +
      ') ON CONFLICT (trx_id) DO UPDATE SET ' +
      '  synced_at = CURRENT_TIMESTAMP';

    FQuery.ParamByName('trx_id').AsString := ATrxID;
    FQuery.ParamByName('terminal_id').AsString := FTerminalID;
    FQuery.ParamByName('user_id').AsInteger := AData.Get('user_id', 0);
    FQuery.ParamByName('trx_date').AsDateTime := Now;
    FQuery.ParamByName('total_amount').AsFloat := AData.Get('total_amount', 0.0);
    FQuery.ParamByName('payment_type').AsString := AData.Get('payment_type', '');
    FQuery.ParamByName('payment_bank').AsString := AData.Get('payment_bank', '');
    FQuery.ParamByName('payment_last4').AsString := AData.Get('payment_last4', '');
    FQuery.ParamByName('payment_amount').AsFloat := AData.Get('payment_amount', 0.0);
    FQuery.ParamByName('change_amount').AsFloat := AData.Get('change_amount', 0.0);
    FQuery.ParamByName('is_return').AsBoolean := AData.Get('is_return', False);
    FQuery.ParamByName('original_trx_id').AsString := AData.Get('original_trx_id', '');
    FQuery.ParamByName('notes').AsString := AData.Get('notes', '');
    FQuery.ExecSQL;

    // Insert transaction items
    Items := AData.Get('items', TJSONArray(nil));
    if Items <> nil then
    begin
      for i := 0 to Items.Count - 1 do
      begin
        Item := Items.Objects[i];

        FQuery.SQL.Text :=
          'INSERT INTO transaction_items (' +
          '  trx_id, product_code, product_name, qty, price, ' +
          '  discount_value, discount_type, total_price' +
          ') VALUES (' +
          '  :trx_id, :product_code, :product_name, :qty, :price, ' +
          '  :discount_value, :discount_type, :total_price' +
          ')';

        FQuery.ParamByName('trx_id').AsString := ATrxID;
        FQuery.ParamByName('product_code').AsString := Item.Get('product_code', '');
        FQuery.ParamByName('product_name').AsString := Item.Get('product_name', '');
        FQuery.ParamByName('qty').AsInteger := Item.Get('qty', 0);
        FQuery.ParamByName('price').AsFloat := Item.Get('price', 0.0);
        FQuery.ParamByName('discount_value').AsFloat := Item.Get('discount_value', 0.0);
        FQuery.ParamByName('discount_type').AsString := Item.Get('discount_type', '');
        FQuery.ParamByName('total_price').AsFloat := Item.Get('total_price', 0.0);
        FQuery.ExecSQL;
      end;
    end;

    // ========== LOG TO sync_log (TAMBAHKAN INI) ==========
    FQuery.SQL.Text :=
      'INSERT INTO sync_log (terminal_id, sync_type, reference_id, status) ' +
      'VALUES (:terminal_id, :sync_type, :reference_id, :status)';
    FQuery.ParamByName('terminal_id').AsString := FTerminalID;
    FQuery.ParamByName('sync_type').AsString := 'transaction';
    FQuery.ParamByName('reference_id').AsString := ATrxID;
    FQuery.ParamByName('status').AsString := 'success';
    FQuery.ExecSQL;
    // ========== END LOG ==========

    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      if FTransaction.Active then
        FTransaction.Rollback;
      LogSyncError('Sync transaction failed: ' + E.Message);

      // Log error to sync_log
      try
        if FTransaction.Active then
          FTransaction.Commit;
        FTransaction.StartTransaction;

        FQuery.SQL.Text :=
          'INSERT INTO sync_log (terminal_id, sync_type, reference_id, status, error_message) ' +
          'VALUES (:terminal_id, :sync_type, :reference_id, :status, :error)';
        FQuery.ParamByName('terminal_id').AsString := FTerminalID;
        FQuery.ParamByName('sync_type').AsString := 'transaction';
        FQuery.ParamByName('reference_id').AsString := ATrxID;
        FQuery.ParamByName('status').AsString := 'failed';
        FQuery.ParamByName('error').AsString := E.Message;
        FQuery.ExecSQL;

        FTransaction.Commit;
      except
        // Silent
      end;
    end;
  end;
end;

function TPostgreSQLManager.SyncCashDrawer(const AAction: string; const AData: TJSONObject): Boolean;
begin
  Result := False;
  if not IsConnected then Exit;

  try
    if FTransaction.Active then
      FTransaction.Commit;

    FTransaction.StartTransaction;

    FQuery.SQL.Text :=
      'INSERT INTO cash_drawer (' +
      '  terminal_id, user_id, action_type, amount, ' +
      '  balance_before, balance_after, trx_id, notes, action_date' +
      ') VALUES (' +
      '  :terminal_id, :user_id, :action_type, :amount, ' +
      '  :balance_before, :balance_after, :trx_id, :notes, :action_date' +
      ')';

    FQuery.ParamByName('terminal_id').AsString := FTerminalID;
    FQuery.ParamByName('user_id').AsInteger := AData.Get('user_id', 0);
    FQuery.ParamByName('action_type').AsString := AAction;
    FQuery.ParamByName('amount').AsFloat := AData.Get('amount', 0.0);
    FQuery.ParamByName('balance_before').AsFloat := AData.Get('balance_before', 0.0);
    FQuery.ParamByName('balance_after').AsFloat := AData.Get('balance_after', 0.0);
    FQuery.ParamByName('trx_id').AsString := AData.Get('trx_id', '');
    FQuery.ParamByName('notes').AsString := AData.Get('notes', '');
    FQuery.ParamByName('action_date').AsDateTime := Now;
    FQuery.ExecSQL;

    FQuery.SQL.Text :=
      'INSERT INTO sync_log (terminal_id, sync_type, reference_id, status) ' +
      'VALUES (:terminal_id, :sync_type, :reference_id, :status)';
    FQuery.ParamByName('terminal_id').AsString := FTerminalID;
    FQuery.ParamByName('sync_type').AsString := 'cash_drawer';
    FQuery.ParamByName('reference_id').AsString := AAction;
    FQuery.ParamByName('status').AsString := 'success';
    FQuery.ExecSQL;

    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      if FTransaction.Active then
        FTransaction.Rollback;
      LogSyncError('Sync cash drawer failed: ' + E.Message);
    end;
  end;
end;

function TPostgreSQLManager.SyncReturnTransaction(const AReturnData: TJSONObject): Boolean;
begin
  Result := SyncTransaction(AReturnData.Get('trx_id', ''), AReturnData);
end;

function TPostgreSQLManager.SyncPendingData: Boolean;
begin
  Result := True;
end;

end.
