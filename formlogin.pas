unit FormLogin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LCLType, SQLDB, DB;

type
  TUserLevel = (ulKasir, ulAdmin);

  TUserInfo = record
    ID: Integer;
    CashierCode: string;
    Name: string;
    Level: TUserLevel;
  end;

  TFrmLogin = class(TForm)
    edtKodeKasir: TEdit;
    edtPassword: TEdit;
    lblTitle: TLabel;
    lblKodeKasir: TLabel;
    lblPassword: TLabel;
    lblInfo: TLabel;
    pnlTop: TPanel;
    pnlMain: TPanel;
    pnlBottom: TPanel;
    lblHelp: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtKodeKasirKeyPress(Sender: TObject; var Key: char);
    procedure edtKodeKasirKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtPasswordKeyPress(Sender: TObject; var Key: char);
    procedure edtPasswordKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FConnection: TSQLConnection;
    FTransaction: TSQLTransaction;
    FUserInfo: TUserInfo;
    FLoginHistoryID: Integer;
    FAttemptCount: Integer;

    function ValidateLogin(const Code, Pass: string): Boolean;
    function SaveLoginHistory: Integer;
  public
    property Connection: TSQLConnection read FConnection write FConnection;
    property Transaction: TSQLTransaction read FTransaction write FTransaction;
    property UserInfo: TUserInfo read FUserInfo;
    property LoginHistoryID: Integer read FLoginHistoryID;
  end;

var
  FrmLogin: TFrmLogin;

implementation

{$R *.lfm}

procedure TFrmLogin.FormCreate(Sender: TObject);
begin
  KeyPreview := True;
  FAttemptCount := 0;
  FLoginHistoryID := 0;

  FUserInfo.ID := 0;
  FUserInfo.CashierCode := '';
  FUserInfo.Name := '';
  FUserInfo.Level := ulKasir;
end;

procedure TFrmLogin.FormShow(Sender: TObject);
begin
  edtKodeKasir.Clear;
  edtPassword.Clear;
  lblInfo.Caption := '';
  edtKodeKasir.SetFocus;
end;

function TFrmLogin.ValidateLogin(const Code, Pass: string): Boolean;
var
  Q: TSQLQuery;
  Level: string;
begin
  Result := False;

  if (Length(Code) <> 6) or (Length(Pass) <> 6) then
  begin
    ShowMessage('Kode kasir dan password harus 6 digit!');
    Exit;
  end;

  if not Assigned(FConnection) or not FConnection.Connected then
  begin
    ShowMessage('Database tidak terhubung!');
    Exit;
  end;

  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConnection;
    Q.Transaction := FTransaction;

    Q.SQL.Text :=
      'SELECT id, cashier_code, name, level, active ' +
      'FROM users ' +
      'WHERE cashier_code = :code AND password = :pass';
    Q.ParamByName('code').AsString := Code;
    Q.ParamByName('pass').AsString := Pass;
    Q.Open;

    if Q.EOF then
    begin
      Inc(FAttemptCount);

      if FAttemptCount >= 3 then
      begin
        ShowMessage('Terlalu banyak percobaan gagal!' + sLineBreak +
                    'Aplikasi akan ditutup.');
        Application.Terminate;
      end
      else
      begin
        lblInfo.Caption := 'Kode atau password salah! (' +
                           IntToStr(FAttemptCount) + '/3)';
        lblInfo.Font.Color := clRed;
      end;

      Exit;
    end;

    if Q.FieldByName('active').AsInteger = 0 then
    begin
      ShowMessage('User ini sudah dinonaktifkan!');
      Exit;
    end;

    FUserInfo.ID := Q.FieldByName('id').AsInteger;
    FUserInfo.CashierCode := Q.FieldByName('cashier_code').AsString;
    FUserInfo.Name := Q.FieldByName('name').AsString;

    Level := Q.FieldByName('level').AsString;
    if Level = 'admin' then
      FUserInfo.Level := ulAdmin
    else
      FUserInfo.Level := ulKasir;

    Q.Close;

    // Save login history
    FLoginHistoryID := SaveLoginHistory;

    Result := True;

  finally
    Q.Free;
  end;
end;

function TFrmLogin.SaveLoginHistory: Integer;
var
  Q: TSQLQuery;
  TimeStr: string;
begin
  Result := 0;

  TimeStr := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConnection;
    Q.Transaction := FTransaction;

    if not FTransaction.Active then
      FTransaction.StartTransaction;

    try
      Q.SQL.Text :=
        'INSERT INTO login_history ' +
        '(user_id, cashier_code, login_time, status) ' +
        'VALUES (:uid, :code, :time, ''active'')';
      Q.ParamByName('uid').AsInteger := FUserInfo.ID;
      Q.ParamByName('code').AsString := FUserInfo.CashierCode;
      Q.ParamByName('time').AsString := TimeStr;
      Q.ExecSQL;

      Q.SQL.Text := 'SELECT last_insert_rowid() AS id';
      Q.Open;
      Result := Q.FieldByName('id').AsInteger;
      Q.Close;

      FTransaction.Commit;
    except
      on E: Exception do
      begin
        if FTransaction.Active then
          FTransaction.Rollback;
        ShowMessage('Error saving login history: ' + E.Message);
      end;
    end;
  finally
    Q.Free;
  end;
end;

procedure TFrmLogin.edtKodeKasirKeyPress(Sender: TObject; var Key: char);
begin
  // Hanya terima angka, backspace, dan enter
  if not (Key in ['0'..'9', #8, #13]) then
  begin
    Key := #0;
    Exit;
  end;

  // Max 6 digit
  if (Length(edtKodeKasir.Text) >= 6) and (Key <> #8) and (Key <> #13) then
  begin
    Key := #0;
    Exit;
  end;
end;

procedure TFrmLogin.edtKodeKasirKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN: begin
      if Length(edtKodeKasir.Text) = 6 then
      begin
        edtPassword.SetFocus;
      end
      else
      begin
        ShowMessage('Kode kasir harus 6 digit!');
      end;
      Key := 0;
    end;

    VK_ESCAPE: begin
      if MessageDlg('Keluar Aplikasi',
        'Keluar dari aplikasi?',
        mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        Application.Terminate;
      end;
      Key := 0;
    end;
  end;
end;

procedure TFrmLogin.edtPasswordKeyPress(Sender: TObject; var Key: char);
begin
  // Hanya terima angka, backspace, dan enter
  if not (Key in ['0'..'9', #8, #13]) then
  begin
    Key := #0;
    Exit;
  end;

  // Max 6 digit
  if (Length(edtPassword.Text) >= 6) and (Key <> #8) and (Key <> #13) then
  begin
    Key := #0;
    Exit;
  end;
end;

procedure TFrmLogin.edtPasswordKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN: begin
      if (Length(edtKodeKasir.Text) = 6) and (Length(edtPassword.Text) = 6) then
      begin
        if ValidateLogin(edtKodeKasir.Text, edtPassword.Text) then
        begin
          ModalResult := mrOK;
        end
        else
        begin
          edtPassword.Clear;
          edtPassword.SetFocus;
        end;
      end
      else
      begin
        ShowMessage('Kode dan password harus 6 digit!');
      end;
      Key := 0;
    end;

    VK_UP: begin
      edtKodeKasir.SetFocus;
      Key := 0;
    end;

    VK_ESCAPE: begin
      if MessageDlg('Keluar Aplikasi',
        'Keluar dari aplikasi?',
        mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        Application.Terminate;
      end;
      Key := 0;
    end;
  end;
end;

procedure TFrmLogin.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    if MessageDlg('Keluar Aplikasi',
      'Keluar dari aplikasi?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      Application.Terminate;
    end;
    Key := 0;
  end;
end;

end.
