unit FormReturn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, DB, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Grids, LCLType, uTypes;

type
  TFrmReturn = class(TForm)
    edtTransactionNo: TEdit;
    lblTitle: TLabel;
    lblTransactionNo: TLabel;
    lblInfo: TLabel;
    lblHelp: TLabel;
    pnlTop: TPanel;
    pnlBottom: TPanel;
    sgItems: TStringGrid;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtTransactionNoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgItemsDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure sgItemsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgItemsKeyPress(Sender: TObject; var Key: char);
  private
    FTransaction: TTransactionRecord;
    FItems: array of TReturnItem;
    FConnection: TSQLConnection;
    FTransaction_: TSQLTransaction;
    FEditMode: Boolean;
    FEditBuffer: string;

    procedure SetupGrid;
    procedure ClearGrid;
    function SearchTransaction(const TrxNo: string): Boolean;
    procedure LoadTransactionItems(TrxID: Integer);
    procedure ToggleItemSelection(Row: Integer);
    procedure UpdateReturnQtyDisplay(Row: Integer);
    procedure EnterEditMode;
    procedure ExitEditMode(SaveChanges: Boolean);
    function ValidateReturn: Boolean;
    procedure ProcessReturn;
    procedure ClearData;
  public
    property Connection: TSQLConnection read FConnection write FConnection;
    property Transaction: TSQLTransaction read FTransaction_ write FTransaction_;

    function GetReturnItems(var Items: array of TReturnItem): Boolean;
    function GetOriginalTransaction: TTransactionRecord;
    procedure ResetForm;
  end;

var
  FrmReturn: TFrmReturn;

implementation

{$R *.lfm}

procedure TFrmReturn.FormCreate(Sender: TObject);
begin
  FEditMode := False;
  FEditBuffer := '';
  SetupGrid;
  ClearGrid;
  lblInfo.Caption := '';
  KeyPreview := True;
end;

procedure TFrmReturn.FormShow(Sender: TObject);
begin
  edtTransactionNo.Clear;
  ClearGrid;
  edtTransactionNo.SetFocus;
end;

procedure TFrmReturn.ClearData;
begin
  SetLength(FItems, 0);
  sgItems.RowCount := 1;
  lblInfo.Caption := '';
end;

procedure TFrmReturn.ResetForm;
begin
  edtTransactionNo.Clear;
  ClearData;
  FEditMode := False;
  FEditBuffer := '';
end;

procedure TFrmReturn.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FEditMode := False;
  FEditBuffer := '';
end;

procedure TFrmReturn.SetupGrid;
begin
  sgItems.ColCount := 6;
  sgItems.RowCount := 1;
  sgItems.FixedRows := 1;
  sgItems.FixedCols := 0;
  sgItems.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
                      goRowSelect, goFixedRowNumbering];

  sgItems.ColWidths[0] := 30;   // Checkbox
  sgItems.ColWidths[1] := 80;  // Code
  sgItems.ColWidths[2] := 350;  // Name
  sgItems.ColWidths[3] := 80;   // Qty Asli
  sgItems.ColWidths[4] := 100;  // Qty Retur
  sgItems.ColWidths[5] := 120;  // Total

  sgItems.Cells[0, 0] := '☐';
  sgItems.Cells[1, 0] := 'Kode';
  sgItems.Cells[2, 0] := 'Nama Barang';
  sgItems.Cells[3, 0] := 'Qty';
  sgItems.Cells[4, 0] := 'Qty Retur';
  sgItems.Cells[5, 0] := 'Total';
end;

procedure TFrmReturn.ClearGrid;
begin
  sgItems.RowCount := 1;
  SetLength(FItems, 0);
  lblInfo.Caption := '';
end;

function TFrmReturn.SearchTransaction(const TrxNo: string): Boolean;
var
  Q: TSQLQuery;
begin
  Result := False;

  if not Assigned(FConnection) or not FConnection.Connected then
    Exit;

  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConnection;
    Q.Transaction := FTransaction_;

    Q.SQL.Text :=
      'SELECT id, transaction_no, transaction_date, subtotal, payment_status, ' +
      'discount, total, pay_amount, change_amount, payment_type, payment_provider, ' +
      'card_last4, COALESCE(transaction_type, ''sale'') as transaction_type, ' +
      'return_from_transaction_id ' +
      'FROM transactions ' +
      'WHERE transaction_no = :trx AND deleted_at IS NULL';
    Q.ParamByName('trx').AsString := TrxNo;
    Q.Open;

    if Q.EOF then begin
      ShowMessage('Transaksi tidak ditemukan!');
      Exit;
    end;

    if Q.FieldByName('transaction_type').AsString = 'return' then begin
      ShowMessage('Ini adalah transaksi retur, tidak bisa diretur lagi!');
      Exit;
    end;

    if Q.FieldByName('payment_status').AsString = 'void' then begin
      ShowMessage('Transaksi sudah dibatalkan!');
      Exit;
    end;

    FTransaction.ID := Q.FieldByName('id').AsInteger;
    FTransaction.TransactionNo := Q.FieldByName('transaction_no').AsString;
    FTransaction.TransactionDate := Q.FieldByName('transaction_date').AsString;
    FTransaction.SubTotal := Q.FieldByName('subtotal').AsFloat;
    FTransaction.PaymentStatus := Q.FieldByName('payment_status').AsString;
    FTransaction.Discount := Q.FieldByName('discount').AsFloat;
    FTransaction.Total := Q.FieldByName('total').AsFloat;
    FTransaction.PayAmount := Q.FieldByName('pay_amount').AsFloat;
    FTransaction.ChangeAmount := Q.FieldByName('change_amount').AsFloat;
    FTransaction.PaymentType := Q.FieldByName('payment_type').AsString;
    FTransaction.PaymentProvider := Q.FieldByName('payment_provider').AsString;
    FTransaction.CardLast4 := Q.FieldByName('card_last4').AsString;
    FTransaction.TransactionType := Q.FieldByName('transaction_type').AsString;

    Result := True;
  finally
    Q.Free;
  end;
end;

procedure TFrmReturn.LoadTransactionItems(TrxID: Integer);
var
  Q, QReturn: TSQLQuery;
  Idx: Integer;
  ReturnedQty, AvailableQty: Double;
begin
  ClearGrid;

  Q := TSQLQuery.Create(nil);
  QReturn := TSQLQuery.Create(nil);
  try
    Q.Database := FConnection;
    Q.Transaction := FTransaction_;
    QReturn.Database := FConnection;
    QReturn.Transaction := FTransaction_;

    Q.SQL.Text :=
      'SELECT id, product_code, product_name, price, qty, ' +
      'discount_value, discount_type, total_price ' +
      'FROM transaction_items ' +
      'WHERE transaction_id = :tid AND deleted_at IS NULL ' +
      'ORDER BY id';
    Q.ParamByName('tid').AsInteger := TrxID;
    Q.Open;

    if Q.RecordCount = 0 then begin
      ShowMessage('Tidak ada item di transaksi ini!');
      Exit;
    end;

    SetLength(FItems, Q.RecordCount);
    sgItems.RowCount := Q.RecordCount + 1;

    Idx := 0;
    while not Q.EOF do begin
      // Cek berapa qty yang sudah diretur untuk item ini
      QReturn.Close;
      QReturn.SQL.Text :=
        'SELECT COALESCE(SUM(ABS(ti.qty)), 0) as returned_qty ' +
        'FROM transaction_items ti ' +
        'JOIN transactions t ON t.id = ti.transaction_id ' +
        'WHERE t.return_from_transaction_id = :orig_trx_id ' +
        '  AND ti.product_code = :code ' +
        '  AND t.transaction_type = ''return'' ' +
        '  AND t.deleted_at IS NULL ' +
        '  AND ti.deleted_at IS NULL';
      QReturn.ParamByName('orig_trx_id').AsInteger := TrxID;
      QReturn.ParamByName('code').AsString := Q.FieldByName('product_code').AsString;
      QReturn.Open;

      ReturnedQty := QReturn.FieldByName('returned_qty').AsFloat;
      AvailableQty := Q.FieldByName('qty').AsFloat - ReturnedQty;

      QReturn.Close;

      FItems[Idx].ItemID := Q.FieldByName('id').AsInteger;
      FItems[Idx].ProductCode := Q.FieldByName('product_code').AsString;
      FItems[Idx].ProductName := Q.FieldByName('product_name').AsString;
      FItems[Idx].Price := Q.FieldByName('price').AsFloat;
      FItems[Idx].OriginalQty := AvailableQty;  // Qty yang tersisa
      FItems[Idx].ReturnQty := 0;
      FItems[Idx].DiscountValue := Q.FieldByName('discount_value').AsFloat;
      FItems[Idx].DiscountType := Q.FieldByName('discount_type').AsString;
      FItems[Idx].TotalPrice := Q.FieldByName('total_price').AsFloat * (AvailableQty / Q.FieldByName('qty').AsFloat);

      sgItems.Cells[0, Idx + 1] := '☐';
      sgItems.Cells[1, Idx + 1] := FItems[Idx].ProductCode;

      // Tampilkan info qty tersisa
      if AvailableQty <= 0 then begin
        sgItems.Cells[2, Idx + 1] := FItems[Idx].ProductName + ' [HABIS DIRETUR]';
        sgItems.Cells[0, Idx + 1] := '✖';
      end else if ReturnedQty > 0 then begin
        sgItems.Cells[2, Idx + 1] := FItems[Idx].ProductName +
          ' [sudah diretur ' + FormatFloat('#,##0', ReturnedQty) + ']';
      end else begin
        sgItems.Cells[2, Idx + 1] := FItems[Idx].ProductName;
      end;

      sgItems.Cells[3, Idx + 1] := FormatFloat('#,##0', AvailableQty);
      sgItems.Cells[4, Idx + 1] := '0';
      sgItems.Cells[5, Idx + 1] := 'Rp ' + FormatFloat('#,##0', FItems[Idx].TotalPrice);

      Inc(Idx);
      Q.Next;
    end;

    lblInfo.Caption := Format('Transaksi: %s | Tanggal: %s | Total: Rp %s | Payment: %s',
      [FTransaction.TransactionNo,
       Copy(FTransaction.TransactionDate, 1, 10),
       FormatFloat('#,##0', FTransaction.Total),
       UpperCase(FTransaction.PaymentType)]);

    if sgItems.RowCount > 1 then begin
      sgItems.Row := 1;
      sgItems.SetFocus;
    end;

  finally
    Q.Free;
    QReturn.Free;
  end;
end;

procedure TFrmReturn.ToggleItemSelection(Row: Integer);
var
  Idx: Integer;
begin
  Idx := Row - 1;
  if (Idx < 0) or (Idx >= Length(FItems)) then Exit;

  // Cegah pilih item yang sudah habis diretur
  if FItems[Idx].OriginalQty <= 0 then begin
    ShowMessage('Item ini sudah habis diretur!');
    Exit;
  end;

  if sgItems.Cells[0, Row] = '☐' then begin
    sgItems.Cells[0, Row] := '☑';
    FItems[Idx].ReturnQty := FItems[Idx].OriginalQty;
  end else begin
    sgItems.Cells[0, Row] := '☐';
    FItems[Idx].ReturnQty := 0;
  end;

  UpdateReturnQtyDisplay(Row);
end;

procedure TFrmReturn.UpdateReturnQtyDisplay(Row: Integer);
var
  Idx: Integer;
begin
  Idx := Row - 1;
  if (Idx < 0) or (Idx >= Length(FItems)) then Exit;

  if FItems[Idx].ReturnQty > 0 then
    sgItems.Cells[4, Row] := FormatFloat('#,##0', FItems[Idx].ReturnQty)
  else
    sgItems.Cells[4, Row] := '0';
end;

procedure TFrmReturn.EnterEditMode;
begin
  FEditMode := True;
  FEditBuffer := '';
  lblHelp.Caption := '[EDIT MODE] Ketik angka qty, tekan ENTER untuk simpan, ESC untuk batal';
  lblHelp.Font.Color := clYellow;
end;

procedure TFrmReturn.ExitEditMode(SaveChanges: Boolean);
var
  Row, Idx: Integer;
  NewQty: Double;
begin
  Row := sgItems.Row;
  Idx := Row - 1;

  if SaveChanges and (Idx >= 0) and (Idx < Length(FItems)) then begin
    // Cegah edit item yang habis
    if FItems[Idx].OriginalQty <= 0 then begin
      ShowMessage('Item ini sudah habis diretur!');
      FEditMode := False;
      FEditBuffer := '';
      lblHelp.Caption := '[SPACE] toggle | [0-9] edit qty | [F12] proses | [ESC] batal';
      lblHelp.Font.Color := clLime;
      Exit;
    end;

    if TryStrToFloat(FEditBuffer, NewQty) then begin
      if (NewQty >= 0) and (NewQty <= FItems[Idx].OriginalQty) then begin
        FItems[Idx].ReturnQty := NewQty;
        if NewQty > 0 then
          sgItems.Cells[0, Row] := '☑'
        else
          sgItems.Cells[0, Row] := '☐';
        UpdateReturnQtyDisplay(Row);
      end else begin
        ShowMessage('Qty tidak valid! Max tersisa: ' + FormatFloat('#,##0', FItems[Idx].OriginalQty));
      end;
    end;
  end;

  FEditMode := False;
  FEditBuffer := '';
  lblHelp.Caption := '[SPACE] toggle | [0-9] edit qty | [F12] proses | [ESC] batal';
  lblHelp.Font.Color := clLime;
end;

procedure TFrmReturn.edtTransactionNoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN: begin
      if Trim(edtTransactionNo.Text) <> '' then begin
        if SearchTransaction(Trim(edtTransactionNo.Text)) then
          LoadTransactionItems(FTransaction.ID);
      end;
      Key := 0;
    end;

    VK_ESCAPE: begin
      ModalResult := mrCancel;
      Key := 0;
    end;
  end;
end;

procedure TFrmReturn.sgItemsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if FEditMode then begin
    case Key of
      VK_RETURN: begin
        ExitEditMode(True);
        Key := 0;
      end;

      VK_ESCAPE: begin
        ExitEditMode(False);
        Key := 0;
      end;

      VK_BACK: begin
        if Length(FEditBuffer) > 0 then
          Delete(FEditBuffer, Length(FEditBuffer), 1);
        lblHelp.Caption := '[EDIT MODE] Qty: ' + FEditBuffer + '_';
        Key := 0;
      end;
    end;
  end else begin
    case Key of
      VK_SPACE: begin
        if sgItems.Row > 0 then
          ToggleItemSelection(sgItems.Row);
        Key := 0;
      end;

      VK_ESCAPE: begin
        ModalResult := mrCancel;
        Key := 0;
      end;
    end;
  end;
end;

procedure TFrmReturn.sgItemsKeyPress(Sender: TObject; var Key: char);
begin
  if FEditMode then begin
    if Key in ['0'..'9', '.', ','] then begin
      if Key = ',' then Key := '.';
      FEditBuffer := FEditBuffer + Key;
      lblHelp.Caption := '[EDIT MODE] Qty: ' + FEditBuffer + '_';
      Key := #0;
    end;
  end else begin
    if Key in ['0'..'9'] then begin
      EnterEditMode;
      FEditBuffer := Key;
      lblHelp.Caption := '[EDIT MODE] Qty: ' + FEditBuffer + '_';
      Key := #0;
    end;
  end;
end;

procedure TFrmReturn.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_F12: begin
      if sgItems.RowCount > 1 then
        ProcessReturn;
      Key := 0;
    end;

    VK_ESCAPE: begin
      if not FEditMode then
        ModalResult := mrCancel;
      Key := 0;
    end;
  end;
end;

function TFrmReturn.ValidateReturn: Boolean;
var
  i, Count: Integer;
begin
  Result := False;
  Count := 0;

  for i := 0 to High(FItems) do begin
    if FItems[i].ReturnQty > 0 then
      Inc(Count);
  end;

  if Count = 0 then begin
    ShowMessage('Pilih minimal 1 item untuk diretur!');
    Exit;
  end;

  Result := True;
end;

procedure TFrmReturn.ProcessReturn;
begin
  if not ValidateReturn then
    Exit;

  // Cegah double click
  if ModalResult <> mrNone then
    Exit;

  if MessageDlg('Proses Retur',
    'Yakin akan memproses retur ini?' + sLineBreak +
    'Pengembalian akan diproses sesuai metode pembayaran asli.',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    ModalResult := mrOK;
  end;
end;

procedure TFrmReturn.sgItemsDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  S: string;
  TextStyle: TTextStyle;
begin
  if aRow = 0 then begin
    sgItems.Canvas.Brush.Color := $00604020;
    sgItems.Canvas.FillRect(aRect);
    sgItems.Canvas.Font.Color := clWhite;
    sgItems.Canvas.Font.Style := [fsBold];
  end else begin
    if gdSelected in aState then
      sgItems.Canvas.Brush.Color := $00506030
    else if odd(aRow) then
      sgItems.Canvas.Brush.Color := $00323232
    else
      sgItems.Canvas.Brush.Color := $00282828;
    sgItems.Canvas.FillRect(aRect);
    sgItems.Canvas.Font.Color := clWhite;
  end;

  S := sgItems.Cells[aCol, aRow];
  TextStyle := sgItems.Canvas.TextStyle;
  TextStyle.Alignment := taLeftJustify;
  TextStyle.Layout := tlCenter;

  if aCol = 0 then
    TextStyle.Alignment := taCenter
  else if aCol in [3, 4, 5] then
    TextStyle.Alignment := taRightJustify;

  sgItems.Canvas.TextRect(aRect, aRect.Left + 5, aRect.Top, S, TextStyle);
end;

function TFrmReturn.GetReturnItems(var Items: array of TReturnItem): Boolean;
var
  i, Idx: Integer;
begin
  Result := False;
  Idx := 0;

  for i := 0 to High(FItems) do begin
    if FItems[i].ReturnQty > 0 then begin
      if Idx <= High(Items) then begin
        Items[Idx] := FItems[i];
        Inc(Idx);
      end;
    end;
  end;

  Result := Idx > 0;
end;

function TFrmReturn.GetOriginalTransaction: TTransactionRecord;
begin
  Result := FTransaction;
end;

end.
