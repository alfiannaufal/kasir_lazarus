unit FormDebit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, Dialogs, LCLType, ExtCtrls;

type
  { TFrmDebit }

  TFrmDebit = class(TForm)
    pnlTop: TPanel;
    lblTotal: TLabel;
    pnlMain: TPanel;
    lblBank: TLabel;
    cbBank: TComboBox;
    lblLast4: TLabel;
    edLast4: TEdit;
    lblPay: TLabel;
    edPay: TEdit;
    pnlBottom: TPanel;
    lblShortcuts: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cbBankKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cbBankKeyPress(Sender: TObject; var Key: char);
    procedure cbBankDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure edLast4KeyPress(Sender: TObject; var Key: char);
    procedure edLast4KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edPayKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edPayKeyPress(Sender: TObject; var Key: char);
    procedure edPayChange(Sender: TObject);

  private
    FTotal: Int64;
    PaymentOK: Boolean;
    FIsFormatting: Boolean;

    procedure ApplyDarkMode;
    function ValidateInputs: Boolean;
    function GetPayAmount: Int64;
    procedure FormatPaymentAmount;

  public
    procedure SetTotal(ATotal: Double);
    function GetPaymentInfo(out Bank, Last4: string; out PayAmount: Int64): Boolean;
  end;

var
  FrmDebit: TFrmDebit;

implementation

{$R *.lfm}

{ TFrmDebit }

procedure TFrmDebit.FormCreate(Sender: TObject);
begin
  FIsFormatting := False;
  ApplyDarkMode;
end;

procedure TFrmDebit.ApplyDarkMode;
begin
  // Dark mode colors
  Color := $2D2D30;  // Dark gray background

  // Top panel (Total)
  pnlTop.Color := $1E1E1E;  // Darker gray
  pnlTop.Font.Color := clWhite;
  lblTotal.Font.Color := $00FFB347;  // Orange for total

  // Main panel
  pnlMain.Color := $2D2D30;

  // Labels
  lblBank.Font.Color := clWhite;
  lblLast4.Font.Color := clWhite;
  lblPay.Font.Color := clWhite;

  // ComboBox - use owner draw for dark mode
  cbBank.Style := csOwnerDrawFixed;
  cbBank.Color := $3E3E42;  // Medium dark gray
  cbBank.Font.Color := clWhite;
  cbBank.OnDrawItem := @cbBankDrawItem;

  // Edit boxes
  edLast4.Color := $3E3E42;
  edLast4.Font.Color := clWhite;
  edPay.Color := $3E3E42;
  edPay.Font.Color := $00FFB347;  // Orange for amount

  // Bottom panel (Shortcuts)
  pnlBottom.Color := $1E1E1E;
  lblShortcuts.Font.Color := $00B0B0B0;  // Light gray
end;

procedure TFrmDebit.cbBankDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  combo: TComboBox;
  txt: string;
  bgColor, textColor: TColor;
begin
  combo := Control as TComboBox;

  // Set colors based on state
  if odSelected in State then
  begin
    bgColor := $5A5A5E;  // Lighter gray when selected
    textColor := clWhite;
  end
  else
  begin
    bgColor := $3E3E42;  // Normal dark gray
    textColor := clWhite;
  end;

  // Draw background
  combo.Canvas.Brush.Color := bgColor;
  combo.Canvas.FillRect(ARect);

  // Draw text
  if Index >= 0 then
  begin
    txt := combo.Items[Index];
    combo.Canvas.Font.Color := textColor;
    combo.Canvas.Font.Style := [];
    combo.Canvas.TextOut(ARect.Left + 4, ARect.Top + 4, txt);
  end;
end;

procedure TFrmDebit.SetTotal(ATotal: Double);
begin
  FTotal := Round(ATotal);
end;

procedure TFrmDebit.FormShow(Sender: TObject);
begin
  PaymentOK := False;

  // Setup bank combo
  cbBank.Items.Clear;
  cbBank.Items.AddStrings(['BCA', 'Mandiri', 'BRI', 'BNI', 'BSI', 'CIMB Niaga', 'Lainnya']);
  cbBank.ItemIndex := 0;

  // Set total & default payment
  lblTotal.Caption := 'TOTAL: Rp ' + FormatFloat('#,##0', FTotal);
  edPay.Text := FormatFloat('#,##0', FTotal);

  // Clear last 4 digits
  edLast4.Text := '';
  edLast4.CharCase := ecUpperCase;

  // Reapply dark mode to ensure it's applied
  ApplyDarkMode;

  // Focus on bank selection
  cbBank.SetFocus;
end;

procedure TFrmDebit.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // ESC to cancel
  if Key = VK_ESCAPE then
  begin
    ModalResult := mrCancel;
    Key := 0;
    Exit;
  end;

  // Ctrl+Enter to submit
  if (Key = VK_RETURN) and (ssCtrl in Shift) then
  begin
    if ValidateInputs then
    begin
      PaymentOK := True;
      ModalResult := mrOK;
    end;
    Key := 0;
  end;
end;

procedure TFrmDebit.cbBankKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    edLast4.SetFocus;
    Key := 0;
  end;
end;

procedure TFrmDebit.cbBankKeyPress(Sender: TObject; var Key: char);
begin
  // Quick selection with number keys
  case Key of
    '1': cbBank.ItemIndex := 0;  // BCA
    '2': cbBank.ItemIndex := 1;  // Mandiri
    '3': cbBank.ItemIndex := 2;  // BRI
    '4': cbBank.ItemIndex := 3;  // BNI
    '5': cbBank.ItemIndex := 4;  // BSI
    '6': cbBank.ItemIndex := 5;  // CIMB Niaga
    '7': cbBank.ItemIndex := 6;  // Lainnya
  end;
end;

procedure TFrmDebit.edLast4KeyPress(Sender: TObject; var Key: char);
begin
  // Only allow numbers, backspace, and enter
  if not (Key in ['0'..'9', #8, #13]) then
  begin
    Key := #0;
    Beep;
    Exit;
  end;

  // Limit to 4 digits
  if (Length(edLast4.Text) >= 4) and (Key <> #8) and (Key <> #13) then
  begin
    Key := #0;
    Beep;
    Exit;
  end;
end;

procedure TFrmDebit.edLast4KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    if Length(edLast4.Text) <> 4 then
    begin
      ShowMessage('Nomor kartu harus 4 digit!');
      edLast4.SelectAll;
      Exit;
    end;
    edPay.SetFocus;
    edPay.SelectAll;
    Key := 0;
  end;
end;

procedure TFrmDebit.edPayKeyPress(Sender: TObject; var Key: char);
begin
  // Allow only numbers and control keys
  if not (Key in ['0'..'9', #8, #13]) then
  begin
    Key := #0;
    Beep;
  end;
end;

procedure TFrmDebit.edPayKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    if ValidateInputs then
    begin
      PaymentOK := True;
      ModalResult := mrOK;
    end;
    Key := 0;
  end;
end;

procedure TFrmDebit.edPayChange(Sender: TObject);
begin
  if not FIsFormatting then
    FormatPaymentAmount;
end;

procedure TFrmDebit.FormatPaymentAmount;
var
  raw, cleaned: string;
  value: Int64;
  cursorPos, digitsBefore, digitsAfter, i: Integer;
begin
  FIsFormatting := True;
  try
    cursorPos := edPay.SelStart;
    raw := edPay.Text;

    // Count digits before cursor
    digitsBefore := 0;
    for i := 1 to cursorPos do
      if (i <= Length(raw)) and (raw[i] in ['0'..'9']) then
        Inc(digitsBefore);

    // Clean the string - remove all non-digits
    cleaned := '';
    for i := 1 to Length(raw) do
      if raw[i] in ['0'..'9'] then
        cleaned := cleaned + raw[i];

    if cleaned = '' then
    begin
      edPay.Text := '0';
      edPay.SelStart := 1;
      Exit;
    end;

    // Convert and format
    value := StrToInt64Def(cleaned, 0);
    edPay.Text := FormatFloat('#,##0', value);

    // Restore cursor position by counting digits
    digitsAfter := 0;
    for i := 1 to Length(edPay.Text) do
    begin
      if edPay.Text[i] in ['0'..'9'] then
        Inc(digitsAfter);
      if digitsAfter >= digitsBefore then
      begin
        edPay.SelStart := i;
        Break;
      end;
    end;

  finally
    FIsFormatting := False;
  end;
end;

function TFrmDebit.ValidateInputs: Boolean;
var
  payAmount: Int64;
begin
  Result := False;

  // Validate bank selection
  if cbBank.ItemIndex < 0 then
  begin
    ShowMessage('Pilih bank terlebih dahulu!');
    cbBank.SetFocus;
    Exit;
  end;

  // Validate card last 4 digits
  if Length(edLast4.Text) <> 4 then
  begin
    ShowMessage('Nomor kartu harus 4 digit!');
    edLast4.SelectAll;
    edLast4.SetFocus;
    Exit;
  end;

  // Validate payment amount
  payAmount := GetPayAmount;
  if payAmount <= 0 then
  begin
    ShowMessage('Nominal pembayaran tidak valid!');
    edPay.SetFocus;
    edPay.SelectAll;
    Exit;
  end;

  Result := True;
end;

function TFrmDebit.GetPayAmount: Int64;
var
  cleaned: string;
  i: Integer;
begin
  cleaned := '';
  for i := 1 to Length(edPay.Text) do
    if edPay.Text[i] in ['0'..'9'] then
      cleaned := cleaned + edPay.Text[i];

  Result := StrToInt64Def(cleaned, 0);
end;

function TFrmDebit.GetPaymentInfo(out Bank, Last4: string; out PayAmount: Int64): Boolean;
begin
  Result := False;

  if ShowModal = mrOK then
  begin
    if not PaymentOK then Exit;

    Bank := cbBank.Text;
    Last4 := edLast4.Text;
    PayAmount := GetPayAmount;

    Result := True;
  end;
end;

end.
