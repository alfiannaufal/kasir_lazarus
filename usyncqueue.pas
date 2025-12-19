unit uSyncQueue; // Queue Management untuk Data yang Gagal Sync

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, DB, fpjson;

type
  TSyncQueueItem = record
    QueueID: Integer;
    SyncType: string;
    ReferenceID: string;
    JsonData: string;
    AttemptCount: Integer;
    CreatedAt: TDateTime;
    LastAttempt: TDateTime;
  end;

  TSyncQueueItemArray = array of TSyncQueueItem;  // ← Custom array type

  { TSyncQueueManager }
  TSyncQueueManager = class
  private
    FConnection: TSQLConnection;
    FTransaction: TSQLTransaction;
    FQuery: TSQLQuery;

    procedure CreateQueueTable;
  public
    constructor Create(AConnection: TSQLConnection; ATransaction: TSQLTransaction; AQuery: TSQLQuery);
    destructor Destroy; override;

    // Queue management
    function AddToQueue(const ASyncType, AReferenceID, AJsonData: string): Boolean;
    function GetPendingItems(MaxItems: Integer = 100): TSyncQueueItemArray;  // ← Use custom type
    function MarkAsSynced(QueueID: Integer): Boolean;
    function MarkAsFailed(QueueID: Integer; const ErrorMsg: string): Boolean;
    function GetQueueCount: Integer;
    function ClearSyncedItems(OlderThanDays: Integer = 7): Integer;

    // Specific queue items
    function QueueTransaction(const ATrxID: string; const AJsonData: TJSONObject): Boolean;
    function QueueCashDrawer(const AActionType: string; const AJsonData: TJSONObject): Boolean;
  end;

implementation

{ TSyncQueueManager }

constructor TSyncQueueManager.Create(AConnection: TSQLConnection;
  ATransaction: TSQLTransaction; AQuery: TSQLQuery);
begin
  inherited Create;
  FConnection := AConnection;
  FTransaction := ATransaction;
  FQuery := AQuery;
  CreateQueueTable;
end;

destructor TSyncQueueManager.Destroy;
begin
  inherited Destroy;
end;

procedure TSyncQueueManager.CreateQueueTable;
begin
  try
    FQuery.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS sync_queue (' +
      '  queue_id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  sync_type VARCHAR(50) NOT NULL,' +
      '  reference_id VARCHAR(100),' +
      '  json_data TEXT,' +
      '  attempt_count INTEGER DEFAULT 0,' +
      '  max_attempts INTEGER DEFAULT 5,' +
      '  status VARCHAR(20) DEFAULT ''pending'',' +
      '  error_message TEXT,' +
      '  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,' +
      '  last_attempt DATETIME,' +
      '  synced_at DATETIME' +
      ')';
    FQuery.ExecSQL;
    FTransaction.Commit;

    // Create index
    FQuery.SQL.Text :=
      'CREATE INDEX IF NOT EXISTS idx_sync_queue_status ON sync_queue(status, created_at)';
    FQuery.ExecSQL;
    FTransaction.Commit;
  except
    on E: Exception do
    begin
      FTransaction.Rollback;
      raise Exception.Create('Failed to create sync_queue table: ' + E.Message);
    end;
  end;
end;

function TSyncQueueManager.AddToQueue(const ASyncType, AReferenceID, AJsonData: string): Boolean;
begin
  Result := False;
  try
    FQuery.SQL.Text :=
      'INSERT INTO sync_queue (sync_type, reference_id, json_data, status) ' +
      'VALUES (:sync_type, :reference_id, :json_data, ''pending'')';

    FQuery.ParamByName('sync_type').AsString := ASyncType;
    FQuery.ParamByName('reference_id').AsString := AReferenceID;
    FQuery.ParamByName('json_data').AsString := AJsonData;
    FQuery.ExecSQL;

    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      FTransaction.Rollback;
      raise Exception.Create('Failed to add to queue: ' + E.Message);
    end;
  end;
end;

function TSyncQueueManager.GetPendingItems(MaxItems: Integer): TSyncQueueItemArray;
var
  ItemList: array of TSyncQueueItem;
  Count: Integer;
begin
  SetLength(ItemList, 0);
  Count := 0;

  try
    FQuery.SQL.Text :=
      'SELECT queue_id, sync_type, reference_id, json_data, ' +
      '       attempt_count, created_at, last_attempt ' +
      'FROM sync_queue ' +
      'WHERE status = ''pending'' AND attempt_count < max_attempts ' +
      'ORDER BY created_at ASC ' +
      'LIMIT :max_items';

    FQuery.ParamByName('max_items').AsInteger := MaxItems;
    FQuery.Open;

    while not FQuery.EOF do
    begin
      SetLength(ItemList, Count + 1);

      ItemList[Count].QueueID := FQuery.FieldByName('queue_id').AsInteger;
      ItemList[Count].SyncType := FQuery.FieldByName('sync_type').AsString;
      ItemList[Count].ReferenceID := FQuery.FieldByName('reference_id').AsString;
      ItemList[Count].JsonData := FQuery.FieldByName('json_data').AsString;
      ItemList[Count].AttemptCount := FQuery.FieldByName('attempt_count').AsInteger;
      ItemList[Count].CreatedAt := FQuery.FieldByName('created_at').AsDateTime;

      if not FQuery.FieldByName('last_attempt').IsNull then
        ItemList[Count].LastAttempt := FQuery.FieldByName('last_attempt').AsDateTime
      else
        ItemList[Count].LastAttempt := 0;

      Inc(Count);
      FQuery.Next;
    end;

    FQuery.Close;
  except
    on E: Exception do
      raise Exception.Create('Failed to get pending items: ' + E.Message);
  end;

  Result := ItemList;
end;

function TSyncQueueManager.MarkAsSynced(QueueID: Integer): Boolean;
begin
  Result := False;
  try
    FQuery.SQL.Text :=
      'UPDATE sync_queue ' +
      'SET status = ''synced'', synced_at = CURRENT_TIMESTAMP ' +
      'WHERE queue_id = :queue_id';

    FQuery.ParamByName('queue_id').AsInteger := QueueID;
    FQuery.ExecSQL;

    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      FTransaction.Rollback;
      raise Exception.Create('Failed to mark as synced: ' + E.Message);
    end;
  end;
end;

function TSyncQueueManager.MarkAsFailed(QueueID: Integer; const ErrorMsg: string): Boolean;
begin
  Result := False;
  try
    FQuery.SQL.Text :=
      'UPDATE sync_queue ' +
      'SET attempt_count = attempt_count + 1, ' +
      '    last_attempt = CURRENT_TIMESTAMP, ' +
      '    error_message = :error_msg, ' +
      '    status = CASE WHEN attempt_count + 1 >= max_attempts THEN ''failed'' ELSE ''pending'' END ' +
      'WHERE queue_id = :queue_id';

    FQuery.ParamByName('queue_id').AsInteger := QueueID;
    FQuery.ParamByName('error_msg').AsString := ErrorMsg;
    FQuery.ExecSQL;

    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      FTransaction.Rollback;
      raise Exception.Create('Failed to mark as failed: ' + E.Message);
    end;
  end;
end;

function TSyncQueueManager.GetQueueCount: Integer;
begin
  Result := 0;
  try
    FQuery.SQL.Text :=
      'SELECT COUNT(*) as total FROM sync_queue WHERE status = ''pending''';
    FQuery.Open;

    if not FQuery.EOF then
      Result := FQuery.FieldByName('total').AsInteger;

    FQuery.Close;
  except
    on E: Exception do
      Result := 0;
  end;
end;

function TSyncQueueManager.ClearSyncedItems(OlderThanDays: Integer): Integer;
begin
  Result := 0;
  try
    FQuery.SQL.Text :=
      'DELETE FROM sync_queue ' +
      'WHERE status = ''synced'' ' +
      'AND synced_at < datetime(''now'', ''-' + IntToStr(OlderThanDays) + ' days'')';

    FQuery.ExecSQL;
    Result := FQuery.RowsAffected;
    FTransaction.Commit;
  except
    on E: Exception do
    begin
      FTransaction.Rollback;
      Result := 0;
    end;
  end;
end;

function TSyncQueueManager.QueueTransaction(const ATrxID: string; const AJsonData: TJSONObject): Boolean;
var
  JsonStr: string;
begin
  JsonStr := AJsonData.AsJSON;
  Result := AddToQueue('transaction', ATrxID, JsonStr);
end;

function TSyncQueueManager.QueueCashDrawer(const AActionType: string; const AJsonData: TJSONObject): Boolean;
var
  JsonStr: string;
begin
  JsonStr := AJsonData.AsJSON;
  Result := AddToQueue('cash_drawer', AActionType, JsonStr);
end;

end.
