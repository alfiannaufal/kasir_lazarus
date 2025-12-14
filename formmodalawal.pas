unit FormModalAwal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LCLType, SQLDB, DB;

type
  TFrmModalAwal = class(TForm)
    edtModalAwal: TEdit;
    lblTitle: TLabel;
    lblInstruction: TLabel;
    lblAmount: TLabel;
    lblDate: TLabel;
    lblLastOpening: TLabel;
    pnlTop: TPanel;
    pnlMain: TPanel;
    pnlBottom: TPanel;
    lblHelp: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtModalAwalKeyPress(Sender: TObject; var Key: char);
    procedure edtModalAwalKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FConnection: TSQLConnection;
    FTransaction: TSQLTransaction;
    FOpeningCash: Double;
    FLastOpeningAmount: Double;

    function GetLastOpeningAmount: Double;
    function SaveOpeningCash: Boolean;
    function ParseAmount(const Input: string): Double;
  public
    property Connection: TSQLConnection read FConnection write FConnection;
    property Transaction: TSQLTransaction read FTransaction write FTransaction;
    property OpeningCash: Double read FOpeningCash;
  end;

var
  FrmModalAwal: TFrmModalAwal;

implementation

{$R *.lfm}

procedure TFrmModalAwal.FormCreate(Sender: TObject);
begin
  KeyPreview := True;
  FOpeningCash := 0;
  FLastOpeningAmount := 0;
end;

procedure TFrmModalAwal.FormShow(Sender: TObject);
begin
  lblDate.Caption := 'Tanggal: ' + FormatDateTime('dddd, dd mmmm yyyy', Now);

  // Ambil modal terakhir sebagai referensi
  FLastOpeningAmount := GetLastOpeningAmount;

  if FLastOpeningAmount > 0 then
    lblLastOpening.Caption := 'Modal kemarin: Rp ' + FormatFloat('#,##0', FLastOpeningAmount)
  else
    lblLastOpening.Caption := '';

  edtModalAwal.Clear;
  edtModalAwal.SetFocus;
end;

function TFrmModalAwal.GetLastOpeningAmount: Double;
var
  Q: TSQLQuery;
begin
  Result := 0;

  if not Assigned(FConnection) or not FConnection.Connected then
    Exit;

  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConnection;
    Q.Transaction := FTransaction;

    Q.SQL.Text :=
      'SELECT opening_cash FROM cash_drawer ' +
      'WHERE status = ''closed'' ' +
      'ORDER BY date DESC, closing_time DESC LIMIT 1';
    Q.Open;

    if not Q.EOF then
      Result := Q.FieldByName('opening_cash').AsFloat;

    Q.Close;
  finally
    Q.Free;
  end;
end;

function TFrmModalAwal.ParseAmount(const Input: string): Double;
var
  CleanInput: string;
begin
  Result := 0;

  if Trim(Input) = '' then
    Exit;

  // Remove dots (thousand separator) and replace comma with dot (decimal)
  CleanInput := StringReplace(Input, '.', '', [rfReplaceAll]);
  CleanInput := StringReplace(CleanInput, ',', '.', [rfReplaceAll]);

  if not TryStrToFloat(CleanInput, Result) then
    Result := 0;
end;

function TFrmModalAwal.SaveOpeningCash: Boolean;
var
  Q: TSQLQuery;
  TodayStr, TimeStr: string;
  Amount: Double;
begin
  Result := False;

  Amount := ParseAmount(edtModalAwal.Text);

  if Amount <= 0 then
  begin
    ShowMessage('Nominal modal harus lebih dari 0!');
    edtModalAwal.SetFocus;
    Exit;
  end;

  if not Assigned(FConnection) or not FConnection.Connected then
  begin
    ShowMessage('Database tidak terhubung!');
    Exit;
  end;

  TodayStr := FormatDateTime('yyyy-mm-dd', Now);
  TimeStr := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConnection;
    Q.Transaction := FTransaction;

    // Cek apakah sudah ada modal hari ini
    Q.SQL.Text :=
      'SELECT id FROM cash_drawer ' +
      'WHERE date = :dt AND status = ''open''';
    Q.ParamByName('dt').AsString := TodayStr;
    Q.Open;

    if not Q.EOF then
    begin
      ShowMessage('Modal hari ini sudah dibuka!');
      Q.Close;
      Exit;
    end;

    Q.Close;

    // Insert modal baru
    if not FTransaction.Active then
      FTransaction.StartTransaction;

    try
      Q.SQL.Text :=
        'INSERT INTO cash_drawer ' +
        '(date, opening_cash, opening_time, status, ' +
        'cash_sales, cash_returns, expected_cash, closing_cash, difference) ' +
        'VALUES (:dt, :amt, :time, ''open'', 0, 0, :amt, NULL, 0)';
      Q.ParamByName('dt').AsString := TodayStr;
      Q.ParamByName('amt').AsFloat := Amount;
      Q.ParamByName('time').AsString := TimeStr;
      Q.ExecSQL;

      FTransaction.Commit;

      FOpeningCash := Amount;
      Result := True;

    except
      on E: Exception do
      begin
        if FTransaction.Active then
          FTransaction.Rollback;
        ShowMessage('Error menyimpan modal: ' + E.Message);
      end;
    end;
  finally
    Q.Free;
  end;
end;

procedure TFrmModalAwal.edtModalAwalKeyPress(Sender: TObject; var Key: char);
begin
  // Hanya terima angka, backspace, dan enter
  if not (Key in ['0'..'9', #8, #13]) then
  begin
    Key := #0;
    Exit;
  end;
end;

procedure TFrmModalAwal.edtModalAwalKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN: begin
      if SaveOpeningCash then
        ModalResult := mrOK;
      Key := 0;
    end;

    VK_ESCAPE: begin
      if MessageDlg('Keluar Aplikasi',
        'Modal belum dibuka. Keluar dari aplikasi?',
        mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        ModalResult := mrCancel;
      end;
      Key := 0;
    end;
  end;
end;

procedure TFrmModalAwal.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    if MessageDlg('Keluar Aplikasi',
      'Modal belum dibuka. Keluar dari aplikasi?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      ModalResult := mrCancel;
    end;
    Key := 0;
  end;
end;

end.
