unit FormSearchName;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Process, fpjson, jsonparser, LCLType, Grids, uTypes;

type
  { TFrmSearchName }
  TFrmSearchName = class(TForm)
    edtSearch: TEdit;
    sgList: TStringGrid;

    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure edtSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgListDblClick(Sender: TObject);
    procedure sgListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  private
    FLastResult: TJSONArray;
    FIsSearching: Boolean;
    FBasePath: string;

    procedure PerformSearch;
    procedure SelectCurrentItem;
    procedure ClearResults;
    procedure ShowLoading;
    function RunSeekName(const Keyword: string): TJSONArray;
    function CleanJSONString(const S: string): string;
    function SanitizeText(const S: string): string;
  public
    SelectedProduct: TProductInfo;
  end;

var
  FrmSearchName: TFrmSearchName;

implementation

{$R *.lfm}

{ --------------------------------------------------------------- }
{ TEXT SANITIZATION }
{ --------------------------------------------------------------- }
function TFrmSearchName.SanitizeText(const S: string): string;
var
  I: Integer;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    C := S[I];
    // Hanya terima karakter yang printable dan aman
    if (Ord(C) >= 32) and (Ord(C) < 127) then
      Result := Result + C
    else if C = #9 then  // Tab
      Result := Result + ' '
    else if C = #10 then // LF
      Result := Result + ' '
    else if C = #13 then // CR
      Result := Result + ' '
    // Karakter lain diabaikan
  end;

  // Trim multiple spaces
  while Pos('  ', Result) > 0 do
    Result := StringReplace(Result, '  ', ' ', [rfReplaceAll]);

  Result := Trim(Result);
end;

{ --------------------------------------------------------------- }
{ JSON CLEANING }
{ --------------------------------------------------------------- }
function TFrmSearchName.CleanJSONString(const S: string): string;
var
  I: Integer;
  InString: Boolean;
  C, NextC: Char;
begin
  Result := '';
  InString := False;
  I := 1;

  while I <= Length(S) do
  begin
    C := S[I];

    // Track jika sedang di dalam string
    if C = '"' then
    begin
      // Cek apakah quote ini escaped
      if (I > 1) and (S[I-1] = '\') then
        Result := Result + C
      else
      begin
        InString := not InString;
        Result := Result + C;
      end;
      Inc(I);
      Continue;
    end;

    // Handle escape sequences di dalam string
    if InString and (C = '\') then
    begin
      if I < Length(S) then
      begin
        NextC := S[I+1];
        case NextC of
          'n': begin
            Result := Result + ' ';  // Replace \n dengan space
            Inc(I, 2);
            Continue;
          end;
          'r': begin
            Result := Result + ' ';  // Replace \r dengan space
            Inc(I, 2);
            Continue;
          end;
          't': begin
            Result := Result + ' ';  // Replace \t dengan space
            Inc(I, 2);
            Continue;
          end;
          '\': begin
            Result := Result + '\\';  // Keep valid escape
            Inc(I, 2);
            Continue;
          end;
          '"': begin
            Result := Result + '\"';  // Keep valid escape
            Inc(I, 2);
            Continue;
          end;
          '/': begin
            Result := Result + '\/';  // Keep valid escape
            Inc(I, 2);
            Continue;
          end;
          'b', 'f': begin
            Result := Result + ' ';  // Replace \b \f dengan space
            Inc(I, 2);
            Continue;
          end;
          'u': begin
            // Unicode escape - keep as is jika valid
            if (I + 5 <= Length(S)) then
            begin
              Result := Result + '\u' + Copy(S, I+2, 4);
              Inc(I, 6);
              Continue;
            end;
          end;
          else begin
            // Invalid escape sequence - skip backslash
            Inc(I);
            Continue;
          end;
        end;
      end;
    end;

    // Karakter biasa
    if InString then
    begin
      // Di dalam string - hanya terima printable chars
      if (Ord(C) >= 32) and (Ord(C) < 127) then
        Result := Result + C
      else if C = #9 then
        Result := Result + ' '
      // Karakter control lain diabaikan
    end
    else
    begin
      // Di luar string - JSON structure
      Result := Result + C;
    end;

    Inc(I);
  end;
end;

{ --------------------------------------------------------------- }
{ UTILITY FUNCTION }
{ --------------------------------------------------------------- }
function TFrmSearchName.RunSeekName(const Keyword: string): TJSONArray;
var
  P: TProcess;
  JsonFile: string;
  SL: TStringList;
  JSON: TJSONData;
  Obj: TJSONObject;
  ArrRaw: TJSONArray;
  ArrCopy: TJSONArray;
  I: Integer;
  CleanedJSON: string;
begin
  Result := nil;
  if Trim(Keyword) = '' then Exit;

  JsonFile := FBasePath + UpperCase(Keyword) + '.json';

  P := TProcess.Create(nil);
  try
    P.Executable := FBasePath + 'seekname.exe';
    P.Parameters.Add(Keyword);
    P.CurrentDirectory := FBasePath;
    P.Options := [poWaitOnExit, poNoConsole];
    P.Execute;
  finally
    P.Free;
  end;

  if not FileExists(JsonFile) then Exit;

  SL := TStringList.Create;
  try
    try
      SL.LoadFromFile(JsonFile);
    except
      on E: Exception do
      begin
        // Error reading file
        Exit;
      end;
    end;

    // Cek file kosong
    if Trim(SL.Text) = '' then Exit;

    // Clean JSON string dari karakter aneh
    CleanedJSON := CleanJSONString(SL.Text);

    if Trim(CleanedJSON) = '' then Exit;

    // Parse JSON dengan error handling
    JSON := nil;
    try
      JSON := GetJSON(CleanedJSON);
    except
      on E: Exception do
      begin
        // JSON invalid - log error jika diperlukan
        // ShowMessage('JSON Parse Error: ' + E.Message);
        Exit;
      end;
    end;

    if not Assigned(JSON) then Exit;

    try
      if JSON.JSONType = jtObject then
      begin
        Obj := TJSONObject(JSON);
        if Obj.Find('products') <> nil then
        begin
          ArrRaw := Obj.Arrays['products'];
          ArrCopy := TJSONArray.Create;
          try
            for I := 0 to ArrRaw.Count - 1 do
              ArrCopy.Add(ArrRaw.Items[I].Clone);
            Result := ArrCopy;
          except
            ArrCopy.Free;
            raise;
          end;
        end;
      end;
    finally
      JSON.Free;
    end;
  finally
    SL.Free;

    // Cleanup JSON file
    if FileExists(JsonFile) then
    begin
      try
        DeleteFile(JsonFile);
      except
        // Ignore delete errors
      end;
    end;
  end;
end;

{ --------------------------------------------------------------- }
{ FORM LIFECYCLE }
{ --------------------------------------------------------------- }
procedure TFrmSearchName.FormShow(Sender: TObject);
begin
  FBasePath := ExtractFilePath(Application.ExeName) + 'hb30' + PathDelim;
  SelectedProduct.Found := False;
  FIsSearching := False;

  edtSearch.Clear;

  // Setup grid
  sgList.ColCount := 3;
  sgList.RowCount := 1;
  sgList.FixedRows := 1;
  sgList.Options := sgList.Options + [goRowSelect];

  sgList.Cells[0,0] := 'Code';
  sgList.Cells[1,0] := 'Name';
  sgList.Cells[2,0] := 'Price';

  sgList.ColWidths[0] := 100;
  sgList.ColWidths[1] := 300;
  sgList.ColWidths[2] := 100;

  FLastResult := nil;

  edtSearch.SetFocus;
end;

procedure TFrmSearchName.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FLastResult) then
    FreeAndNil(FLastResult);
end;

{ --------------------------------------------------------------- }
{ LOADING INDICATOR }
{ --------------------------------------------------------------- }
procedure TFrmSearchName.ShowLoading;
begin
  sgList.RowCount := 2;
  sgList.Cells[0, 1] := '';
  sgList.Cells[1, 1] := 'Mencari produk...';
  sgList.Cells[2, 1] := '';
  Application.ProcessMessages;
end;

{ --------------------------------------------------------------- }
{ SEARCH }
{ --------------------------------------------------------------- }
procedure TFrmSearchName.PerformSearch;
var
  Keyword: string;
  I: Integer;
  Obj: TJSONObject;
  NotFound: Boolean;
  ItemCode, ItemName: string;
begin
  if FIsSearching then Exit;

  Keyword := Trim(edtSearch.Text);

  if Keyword = '' then
  begin
    ClearResults;
    Exit;
  end;

  FIsSearching := True;
  NotFound := False;

  try
    ShowLoading;
    edtSearch.Enabled := False;
    Screen.Cursor := crHourGlass;

    if Assigned(FLastResult) then
      FreeAndNil(FLastResult);

    FLastResult := RunSeekName(Keyword);

    if (FLastResult = nil) or (FLastResult.Count = 0) then
    begin
      sgList.RowCount := 1;
      NotFound := True;
    end
    else
    begin
      sgList.RowCount := FLastResult.Count + 1;

      for I := 0 to FLastResult.Count - 1 do
      begin
        Obj := FLastResult.Objects[I];

        // Sanitize text dari karakter aneh
        ItemCode := SanitizeText(Obj.Get('code', ''));
        ItemName := SanitizeText(Obj.Get('desc', ''));

        sgList.Cells[0, I+1] := ItemCode;
        sgList.Cells[1, I+1] := ItemName;
        sgList.Cells[2, I+1] := FormatFloat('#,##0', Obj.Get('price', 0.0));
      end;

      if FLastResult.Count = 1 then
      begin
        sgList.Row := 1;
        SelectCurrentItem;
      end
      else
      begin
        sgList.Row := 1;
        sgList.Col := 0;
        sgList.SetFocus;
      end;
    end;
  finally
    FIsSearching := False;
    edtSearch.Enabled := True;
    Screen.Cursor := crDefault;
  end;

  if NotFound then
  begin
    ShowMessage('Produk tidak ditemukan!');
    edtSearch.SetFocus;
    edtSearch.SelectAll;
  end;
end;

procedure TFrmSearchName.ClearResults;
begin
  sgList.RowCount := 1;
  if Assigned(FLastResult) then
    FreeAndNil(FLastResult);
end;

{ --------------------------------------------------------------- }
{ SELECTION }
{ --------------------------------------------------------------- }
procedure TFrmSearchName.SelectCurrentItem;
var
  idx: Integer;
  Obj: TJSONObject;
begin
  idx := sgList.Row - 1;

  if (FLastResult = nil) or (idx < 0) or (idx >= FLastResult.Count) then
    Exit;

  Obj := FLastResult.Objects[idx];

  SelectedProduct.Found := True;
  SelectedProduct.Code  := SanitizeText(Obj.Get('code', ''));
  SelectedProduct.Name  := SanitizeText(Obj.Get('desc', ''));
  SelectedProduct.Price := Obj.Get('price', 0.0);

  ModalResult := mrOK;
end;

{ --------------------------------------------------------------- }
{ KEYBOARD NAVIGATION }
{ --------------------------------------------------------------- }
procedure TFrmSearchName.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    // ESC pertama → clear search, ESC kedua → close
    if Trim(edtSearch.Text) <> '' then
    begin
      edtSearch.Clear;
      ClearResults;
      edtSearch.SetFocus;
      Key := 0;
    end
    else
      ModalResult := mrCancel;
  end;
end;

procedure TFrmSearchName.edtSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_DOWN: begin
      if (sgList.RowCount > 1) then
      begin
        sgList.Row := 1;
        sgList.SetFocus;
        Key := 0;
      end;
    end;

    VK_RETURN: begin
      PerformSearch;
      Key := 0;
    end;
  end;
end;

{ --------------------------------------------------------------- }
{ GRID NAVIGATION }
{ --------------------------------------------------------------- }
procedure TFrmSearchName.sgListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN: begin
      SelectCurrentItem;
      Key := 0;
    end;

    VK_UP: begin
      if sgList.Row = 1 then
      begin
        edtSearch.SetFocus;
        edtSearch.SelStart := Length(edtSearch.Text);
        Key := 0;
      end;
    end;

    VK_ESCAPE: begin
      edtSearch.SetFocus;
      edtSearch.SelStart := Length(edtSearch.Text);
      Key := 0;
    end;
  end;
end;

procedure TFrmSearchName.sgListDblClick(Sender: TObject);
begin
  SelectCurrentItem;
end;

end.
