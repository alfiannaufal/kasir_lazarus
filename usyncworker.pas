unit uSyncWorker; // Background Worker untuk Auto Sync

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser,
  uPostgreSQL, uSyncQueue;

type
  TSyncStatus = (ssIdle, ssSyncing, ssError);

  TSyncStats = record
    TotalPending: Integer;
    TotalSynced: Integer;
    TotalFailed: Integer;
    LastSyncTime: TDateTime;
    LastError: string;
  end;

  { TSyncWorker }
  TSyncWorker = class
  private
    FPGManager: TPostgreSQLManager;
    FQueueManager: TSyncQueueManager;
    FStatus: TSyncStatus;
    FStats: TSyncStats;
    FIsRunning: Boolean;

    procedure ProcessQueueItem(const AItem: TSyncQueueItem);
    procedure UpdateStats;
  public
    constructor Create(APGManager: TPostgreSQLManager; AQueueManager: TSyncQueueManager);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
    function ProcessPendingQueue: Integer;

    property Status: TSyncStatus read FStatus;
    property Stats: TSyncStats read FStats;
    property IsRunning: Boolean read FIsRunning;
  end;

implementation

{ TSyncWorker }

constructor TSyncWorker.Create(APGManager: TPostgreSQLManager; AQueueManager: TSyncQueueManager);
begin
  inherited Create;
  FPGManager := APGManager;
  FQueueManager := AQueueManager;
  FStatus := ssIdle;
  FIsRunning := False;

  // Initialize stats
  FStats.TotalPending := 0;
  FStats.TotalSynced := 0;
  FStats.TotalFailed := 0;
  FStats.LastSyncTime := 0;
  FStats.LastError := '';
end;

destructor TSyncWorker.Destroy;
begin
  Stop;
  inherited Destroy;
end;

procedure TSyncWorker.Start;
begin
  FIsRunning := True;
  FStatus := ssIdle;
end;

procedure TSyncWorker.Stop;
begin
  FIsRunning := False;
  FStatus := ssIdle;
end;

function TSyncWorker.ProcessPendingQueue: Integer;
var
  PendingItems: TSyncQueueItemArray;  // ← Use custom type from uSyncQueue
  Item: TSyncQueueItem;
  i: Integer;
  SuccessCount: Integer;
begin
  Result := 0;

  if not FIsRunning then Exit;
  if not FPGManager.IsConnected then Exit;

  FStatus := ssSyncing;
  SuccessCount := 0;

  try
    // Get pending items from queue
    PendingItems := FQueueManager.GetPendingItems(50);  // Process max 50 items at once

    if Length(PendingItems) = 0 then
    begin
      FStatus := ssIdle;
      Exit;
    end;

    // Process each item
    for i := 0 to High(PendingItems) do
    begin
      if not FIsRunning then Break;

      Item := PendingItems[i];

      try
        ProcessQueueItem(Item);
        Inc(SuccessCount);
        Inc(FStats.TotalSynced);
      except
        on E: Exception do
        begin
          Inc(FStats.TotalFailed);
          FStats.LastError := E.Message;
          FQueueManager.MarkAsFailed(Item.QueueID, E.Message);
        end;
      end;
    end;

    Result := SuccessCount;
    FStats.LastSyncTime := Now;
    UpdateStats;

  finally
    FStatus := ssIdle;
  end;
end;

procedure TSyncWorker.ProcessQueueItem(const AItem: TSyncQueueItem);
var
  JsonData: TJSONData;
  JsonObj: TJSONObject;
  Success: Boolean;
begin
  Success := False;
  JsonData := nil;

  try
    // Parse JSON data
    JsonData := GetJSON(AItem.JsonData);

    if not (JsonData is TJSONObject) then
      raise Exception.Create('Invalid JSON data format');

    JsonObj := TJSONObject(JsonData);

    // Process based on sync type
    case AItem.SyncType of
      'transaction':
        Success := FPGManager.SyncTransaction(AItem.ReferenceID, JsonObj);

      'cash_drawer':
        Success := FPGManager.SyncCashDrawer(AItem.ReferenceID, JsonObj);

      'return':
        Success := FPGManager.SyncReturnTransaction(JsonObj);

      else
        raise Exception.Create('Unknown sync type: ' + AItem.SyncType);
    end;

    if Success then
      FQueueManager.MarkAsSynced(AItem.QueueID)
    else
      raise Exception.Create('Sync failed for unknown reason');

  finally
    if Assigned(JsonData) then
      JsonData.Free;
  end;
end;

procedure TSyncWorker.UpdateStats;
begin
  FStats.TotalPending := FQueueManager.GetQueueCount;
end;

end.
