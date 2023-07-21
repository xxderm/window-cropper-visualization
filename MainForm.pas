unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  Contour, ContourFileWorker, CoordinateConvertor;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    Label3: TLabel;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure renderWindow(rect: TRectF);
    procedure renderContours(const contours: IContours; color: TColor);
    procedure renderContourBit(const contourBit: IContour; color: TColor);
    procedure renderContourBitEx(const contourBit: IContour; color: TColor);
    procedure renderAll();
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.renderContourBitEx(const contourBit: IContour; color: TColor);
var
  i: Integer;
  point: TPointI;
  cnt: Integer;
begin
  self.Canvas.Pen.Width := 2;
  self.Canvas.Pen.Color := color;
  i := 0;
  while i < contourBit.getContourBitCount - 1 do
  begin
    if (i = contourBit.getContourBitCount - 1)
        and contourBit.getContourBit(i).isClosed	then
    begin
      point.x := round(contourBit.getContourBit(i + 1).getPoint(0).getX);
      point.y := round(contourBit.getContourBit(i + 1).getPoint(0).getY);
      point := convertor.convert(point.x, point.y);
      self.Canvas.MoveTo(point.x, point.y);

      point.x := round(contourBit.getContourBit(0).getPoint(0).getX);
      point.y := round(contourBit.getContourBit(0).getPoint(0).getY);
      point := convertor.convert(point.x, point.y);
      self.Canvas.LineTo(point.x, point.y);
    end
    else
    begin
      point.x := round(contourBit.getContourBit(i).getPoint(0).getX);
      point.y := round(contourBit.getContourBit(i).getPoint(0).getY);
      point := convertor.convert(point.x, point.y);
      self.Canvas.MoveTo(point.x, point.y);

      point.x := round(contourBit.getContourBit(i + 1).getPoint(0).getX);
      point.y := round(contourBit.getContourBit(i + 1).getPoint(0).getY);
      point := convertor.convert(point.x, point.y);
      self.Canvas.LineTo(point.x, point.y);
    end;

    i := i + 2;
  end;

end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  self.Refresh;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  self.Edit1.Text := IntToStr(-43601);
  self.Edit2.Text := IntToStr(-200933);
  self.Edit3.Text := IntToStr(-36210);
  self.Edit4.Text := IntToStr(-196468);

  // X
  self.TrackBar1.SetTick(1000);
  self.TrackBar1.Min := -60000;
  self.TrackBar1.Max := -20000;

  // Y
  self.TrackBar2.SetTick(1000);
  self.TrackBar2.Min := -300000;
  self.TrackBar2.Max := -100000;
end;

procedure TForm1.renderContourBit(const contourBit: IContour; color: TColor);
var
  i: Integer;
  point: TPointI;
begin
  self.Canvas.Pen.Width := 2;
  self.Canvas.Pen.Color := color;
  for i := 0 to contourBit.getContourBitCount - 2 do
  begin

    point.x := round(contourBit.getContourBit(i).getPoint(0).getX);
    point.y := round(contourBit.getContourBit(i).getPoint(0).getY);
    point := convertor.convert(point.x, point.y);
    self.Canvas.MoveTo(point.x, point.y);

    point.x := round(contourBit.getContourBit(i + 1).getPoint(0).getX);
    point.y := round(contourBit.getContourBit(i + 1).getPoint(0).getY);
    point := convertor.convert(point.x, point.y);
    self.Canvas.LineTo(point.x, point.y);

    // Замкнутый контурбит соединяется с первой точкой
    if (i = contourBit.getContourBitCount - 2)
        and contourBit.getContourBit(i).isClosed	then
    begin
      point.x := round(contourBit.getContourBit(i + 1).getPoint(0).getX);
      point.y := round(contourBit.getContourBit(i + 1).getPoint(0).getY);
      point := convertor.convert(point.x, point.y);
      self.Canvas.MoveTo(point.x, point.y);

      point.x := round(contourBit.getContourBit(0).getPoint(0).getX);
      point.y := round(contourBit.getContourBit(0).getPoint(0).getY);
      point := convertor.convert(point.x, point.y);
      self.Canvas.LineTo(point.x, point.y);
    end;
  end;
end;

procedure TForm1.renderContours(const contours: IContours; color: TColor);
var
  contIdx: Integer;
  i: Integer;
begin
  for contIdx := 0 to contours.getContourCount - 1 do
  begin
    self.renderContourBit(contours.getContour(contIdx), color);
  end;
end;

procedure TForm1.renderWindow(rect: TRectF);
var
  point1: TPointI;
  point2: TPointI;
begin
  point1 := convertor.convert(round(rect.x1), round(rect.y1));
  point2 := convertor.convert(round(rect.x2), round(rect.y2));
  self.Canvas.MoveTo(point1.x, point1.y);
  self.Canvas.LineTo(point2.x, point1.y);
  self.Canvas.MoveTo(point2.x, point1.y);
  self.Canvas.LineTo(point2.x, point2.y);
  self.Canvas.MoveTo(point2.x, point2.y);
  self.Canvas.LineTo(point1.x, point2.y);
  self.Canvas.MoveTo(point1.x, point2.y);
  self.Canvas.LineTo(point1.x, point1.y);
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
var
  diff: Integer;
begin
  diff := Abs(StrToInt(self.Edit1.Text)) - Abs(StrToInt(self.Edit3.Text));

  self.Edit1.Text := IntToStr(self.TrackBar1.Position - diff);
  self.Edit3.Text := IntToStr(self.TrackBar1.Position);

  self.Refresh;
  self.renderAll;
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
var
  diff: Integer;
begin
  diff := Abs(StrToInt(self.Edit2.Text)) - Abs(StrToInt(self.Edit4.Text));

  self.Edit2.Text := IntToStr(self.TrackBar2.Position - diff);
  self.Edit4.Text := IntToStr(self.TrackBar2.Position);

  self.Refresh;
  self.renderAll;
end;

procedure TForm1.renderAll;
const
  fileCount: Integer = 7;
var
  j: Integer;
  windowRect: TRectF;
begin
  var contours: TContours := TContours.Create;

  // Загрузка файлов
  for j := 1 to fileCount do
  begin
    Loader.processFile('Контуры/Горные отводы_1#' + IntToStr(j) + '.ktr', contours);
  end;

  // Рендер отрезков
  self.renderContours(contours, clGreen);

  // Рендер рамки
  windowRect.x1	:= StrToInt(self.Edit1.Text);
  windowRect.y1 := StrToInt(self.Edit2.Text);
  windowRect.x2 := StrToInt(self.Edit3.Text);
  windowRect.y2 := StrToInt(self.Edit4.Text);
  self.Canvas.Pen.Width	:= 1;
  self.Canvas.Pen.Style := psDash;
  self.Canvas.Pen.Color := clRed;
  self.renderWindow(windowRect);

  // Рендер отрезков
  var insideContours: IContour := CutContoursByWindow(contours, windowRect);

  self.renderContourBitEx(insideContours, clRed);
  Loader.saveContour('results.ktr', insideContours);

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  self.Edit1.Text := IntToStr(-43601);
  self.Edit2.Text := IntToStr(-200933);
  self.Edit3.Text := IntToStr(-36210);
  self.Edit4.Text := IntToStr(-196468);
  self.renderAll;
end;

end.
