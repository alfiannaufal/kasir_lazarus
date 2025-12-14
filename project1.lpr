program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, FormDebit, FormSearchName, uTypes, FormReturn, FormModalAwal,
  FormLogin;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  {$PUSH}{$WARN 5044 OFF}
  Application.MainFormOnTaskbar:=True;
  {$POP}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFrmDebit, FrmDebit);
  Application.CreateForm(TFrmSearchName, FrmSearchName);
  Application.CreateForm(TFrmReturn, FrmReturn);
  Application.CreateForm(TFrmModalAwal, FrmModalAwal);
  Application.CreateForm(TFrmLogin, FrmLogin);
  Application.Run;
end.

