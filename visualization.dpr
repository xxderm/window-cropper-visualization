program visualization;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form1},
  Contour in 'Contour.pas',
  ContourFileWorker in 'ContourFileWorker.pas',
  CoordinateConvertor in 'CoordinateConvertor.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
